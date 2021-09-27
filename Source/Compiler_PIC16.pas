{Unidad con rutinas del analizador sintáctico.
}
unit Compiler_PIC16;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, lclProc,
  MisUtils, P6502utils, CPUCore, CompBase, ParserDirec, GenCodBas_PIC16,
  GenCod_PIC16, ParserDirec_PIC16, Globales, FormConfig,
  XpresElemP65 {Por diseño, FormConfig, no debería accederse desde aquí};
type
  { TCompiler_PIC16 }
  TCompiler_PIC16 = class(TParserDirec)
  private   //Funciones básicas
    procedure cInNewLine(lin: string);
  private //Compilación de secciones
    procedure EvaluateConstantDeclare;
    procedure ConstantFolding;
    procedure ScanForRegsRequired;
    procedure DoOptimize;
    procedure DoGenerateCode;
  public
    OnAfterCompile: procedure of object;   //Al finalizar la compilación.
    procedure Compile(NombArc: string; Link: boolean); override;
    procedure RAMusage(lins: TStrings; ExcUnused: boolean); override; //uso de memoria RAM
    procedure DumpCode(lins: TSTrings; AsmMode, IncVarDec, ExcUnused: boolean;
      incAdrr, incCom, incVarNam: boolean); override; //uso de la memoria Flash
    function RAMusedStr: string; override;
    procedure GetResourcesUsed(out ramUse, romUse, stkUse: single); override;
    procedure GenerateListReport(lins: TStrings); override;
  public //Inicialización
    constructor Create; override;
    destructor Destroy; override;
  end;

procedure SetLanguage;

implementation
var
  ER_DUPLIC_IDEN, ER_NOT_IMPLEM_, ER_IDEN_EXPECT, ER_INVAL_FLOAT: string;
  ER_ERR_IN_NUMB, ER_UNDEF_TYPE_, ER_EXP_VAR_IDE, ER_BIT_VAR_REF: String;
  ER_UNKNOWN_ID_, ER_DUPLIC_FUNC_, ER_IDE_TYP_EXP : String;
  ER_COMPIL_PROC, ER_CON_EXP_EXP: String;
  ER_FIL_NOFOUND, WA_UNUSED_CON_, WA_UNUSED_PRO_: String;
  MSG_RAM_USED, MSG_FLS_USED: String;
//Funciones básicas
procedure SetLanguage;
begin
  ParserDirec_PIC16.SetLanguage;
  {$I ..\_language\tra_Compiler.pas}
end;
procedure TCompiler_PIC16.cInNewLine(lin: string);
//Se pasa a una nueva _Línea en el contexto de entrada
begin
  if Config.IncComment then begin
    pic.addTopComm('    ;'+trim(lin));  //agrega _Línea al código ensmblador
  end;
end;
procedure TCompiler_PIC16.EvaluateConstantDeclare;
{Calculates final values from constant declared in CONST sections of program and
functions. Constants could be expressions:
  const
    C1 = 123;
    C2 = C1 and $FF;
}
var
  cons: TEleConsDec;
  consExpres: TEleExpress;
begin
  //Calculate values in CONST sections
  for cons in TreeElems.AllCons do begin
    {For optimziation we should evaluate only used Constant, but we evaluate alls
    because constants like the field "length" of arrays, needs to be evaluated in order
    to get the size defined, before assign RAM. }
    //if cons.nCalled > 0 then begin  //Used constant.
       TreeElems.OpenElement(cons);  //To resolve properly identifiers
       if not cons.evaluated then begin
         //If it isn't evaluated, must be an expression.
         consExpres := TEleExpress(cons.elements[0]);  //Takes the expression node.
         //Should be an expression. Need to be calculated.
         ConstantFoldExpr(consExpres);
         if HayError then exit;
         if consExpres.opType<>otConst then begin
           //The expression returned a not-constant value.
           GenError('Expected constant expression.', consExpres.srcDec);
           exit;
         end;
         if not consExpres.evaluated then begin
           GenError('Constant not evaluated.', consExpres.srcDec);
           exit;
         end;
         //Copy the value.
         cons.value := consExpres.value;
         cons.evaluated := true;
       end;
    //end;
  end;
end;
procedure TCompiler_PIC16.ConstantFolding;
{Do a fold constant optimization and evaluate constant expresion. }
var
  fun : TEleFun;
  bod: TEleBody;
begin
  compMod := cmConsEval;    //Generates code.
  pic.disableCodegen := true;  //Disable the code generation
  pic.iRam := 0;  //Clear RAM position
  //Code subroutines
  for fun in usedFuncs do begin
    ConstantFoldBody(fun.BodyNode);
    if HayError then exit;   //Puede haber error
  end;
  //Code body
  bod := TreeElems.BodyNode;  //lee Nodo del cuerpo principal
  ConstantFoldBody(bod);
end;
procedure TCompiler_PIC16.ScanForRegsRequired;
{Do an exploration to the AST, callin to the code generation (without generates
code) in order to:
- Detect Registers and Temporal variables required to evaluate expressions.
}
var
  fun : TEleFun;
  bod: TEleBody;
begin
  //Set flags to enable routines requireA(), requireH(), ...
  compMod := cmRequire;  //Mode to only detect required register or variables uses.
  pic.disableCodegen := true;  //Disable the code generation
  pic.iRam := 0;  //Clear RAM position
  //Code subroutines
  for fun in usedFuncs do begin
    if fun.IsInterrupt then continue;
    GenCodeBody(fun.BodyNode);
    if HayError then exit;   //Puede haber error
  end;
  //Code body
  bod := TreeElems.BodyNode;  //lee Nodo del cuerpo principal
  GenCodeBody(bod);
  //if HayError then exit;   //Puede haber error
end;
procedure TCompiler_PIC16.DoOptimize;
{Usa la información del árbol de sintaxis, para optimizar su estructura con
miras a la síntesis.
Se debe llamar después de llamar a DoAnalyzeProgram().}
begin
  ExprLevel := 0;
  ResetRAM;    //2ms aprox.
  ClearError;
  pic.MsjError := '';
  {Realiza una exploración al AST para detectar funciones no usadas, y así marcar
  todos sus elementos (constantes, variables, tipos) como no-usados, quitando las
  referencias (callers). }
  RefreshAllElementLists;  //Actualiza lista de elementos
//  EndCountElapsed('-- Optimized in: ');
  RemoveUnusedFunc;   //Se debe empezar con las funciones. 1ms aprox.
  RemoveUnusedVars;   //Luego las variables. 1ms aprox.
  RemoveUnusedCons;   //1ms aprox.
  RemoveUnusedTypes;  //1ms aprox.
  UpdateFunLstCalled; //Actualiza lista "lstCalled" de las funciones usadas
  if HayError then exit;
  SeparateUsedFunctions;
  EvaluateConstantDeclare;
  if HayError then exit;
  ConstantFolding;
  if HayError then exit;
  //{Realiza una primera creación de variables para que la síntesis posterior se haga de
  //la forma más parecida a como se haría. }
  //CreateVarsAndPars;
  {Realiza la simplificación del AST, realizando una primera compilación.}
 ScanForRegsRequired;
end;
procedure TCompiler_PIC16.DoGenerateCode;
{Generates the final binary code using information from the AST as input.
Must be called after DoOptimize().}
  procedure GenCodeMainBody(body: TEleBody);
  {Generates code for a Main Body element.}
  begin
    //It's the main program
    PutLabel('__main_program__');
    //Process body
    TreeElems.OpenElement(body); //Locate in the Body. Formally this won't be necessary if we are not going to solve identifiers.
    GenCodeSentences(TreeElems.curNode.elements);
    TreeElems.CloseElement;              //Close the Body.
    //Ending label
    PutLabel('__end_program__');
    //{ TODO : Considerar incluir este código de verificación. }
    //  if pic.MsjError<>'' then begin //Puede ser error al escribir la última instrucción
    //    GenError(pic.MsjError);
    //    exit;
    //  end;
  end;
  procedure GenCodeFunction(body: TEleBody);
  {Generates code for a function element.}
  var
    isInt: boolean;
    funcPar: TEleFunBase;
  begin
    PutLabel('__' + body.Parent.name);
    funcPar := TEleFunBase(body.Parent);  //Parent function
    isInt := funcPar.IsInterrupt;  //Update flag
    //Process body
    TreeElems.OpenElement(body); //Locate in the Body.
    GenCodeSentences(TreeElems.curNode.elements);
    TreeElems.CloseElement;              //Close the Body.
    //Includes the final RTS
    if OptRetProc then begin  //Optimize
      //Verifica es que ya se ha incluido exit().
      if funcPar.ObligatoryExit<>nil then begin
        //Ya tiene un exit() obligatorio y en el final (al menos eso se espera)
        //No es necesario incluir el RTS().
      end else begin
        //No hay un exit(), seguro
        codRTS(isInt);  //RTS instruction
      end;
    end else begin  //Always include
      codRTS(isInt);  //RTS instruction
    end;
  end;
var
  add, addr: word;
  fun    : TEleFun;
  i      : Integer;
  bod    : TEleBody;
  iniMain: integer;
  elem   : TxpElement;
begin
  //Verifica las constantes usadas. Solo en el nodo principal, para no sobrecargar mensajes.
  for elem in TreeElems.main.elements do if elem.idClass = eleConsDec then begin
    if elem.nCalled = 0 then begin
      GenWarn(WA_UNUSED_CON_, [elem.name], elem.srcDec);
    end;
  end;
  //Inicio de generación de código.
  pic.iRam := GeneralORG;  //Inicia puntero a RAM
  compMod := cmGenCode;    //Generates code.
  pic.disableCodegen := false;  //Enable the code generation
  if Commodore64 then begin
    //En modo Commodore 64
    if pic.iRam = $801 then begin
      //Se pide compilar en el espacio de memoria del BASIC
      PutTopComm('      ;BASIC starter code: 10 SYS 2062');
      pic.codByte($0C, true);  //Dirección de siguiente línea
      pic.codByte($08, true);
      pic.codByte($0A, true);  //Número de línea
      pic.codByte($00, true);
      pic.codByte($9e, true);  //Token de instrucción SYS
      pic.codByte($20, true);  //Espacio
      pic.codByte($32, true);  //2
      pic.codByte($30, true);  //0
      pic.codByte($36, true);  //6
      pic.codByte($32, true);  //2
      pic.codByte($00, true);  //Fin de instrucción
      pic.codByte($00, true);  //Sgte línea BASIC
      pic.codByte($00, true);  //Sgte línea BASIC
    end;
  end;
  _JMP_post(iniMain);   //Salto hasta después del espacio de variables
  //Asigna memoria a registros
  //Asigna memoria para las variables, buscando memoria libre a partir de "GeneralORG".
  CreateVarsAndPars;  //Primero a las variables locales (y parámetros) de las funciones
  //Find the next free RAM location, to write functions.
  pic.freeStart := GeneralORG;  //Start of program block
  pic.dataAddr1   := -1; {Disable. It has been already used for allocatig variables, but
                          now we just want to find a free RAM location in the program block}
  pic.GetFreeByte(addr);
  pic.iRam := addr;
  //Codifica la función INTERRUPT, si existe
  if interruptFunct<>nil then begin;
    { TODO : Revisar }
    //fun := interruptFunct;
    ////Compila la función en la dirección 0x04
    //pic.iRam := $04;
    //fun.adrr := pic.iRam;    //Actualiza la dirección final
    //fun.retType.DefineRegister;    //Asegura que se dispondrá de los WR necesarios
    //SetCtxState(fun.posCtx);  //Posiciona escáner
    //PutLabel('__'+fun.name);
    //TreeElems.OpenElement(fun.BodyNode); //Ubica el espacio de nombres, de forma similar a la pre-compilación
    //CompileSentence;
    //TreeElems.CloseElement;  //cierra el body
    //TreeElems.CloseElement;  //cierra la función
    //if HayError then exit;     //Puede haber error
  end;
  //Codifica las subrutinas usadas
  for fun in usedFuncs do begin
    if fun.IsInterrupt then continue;
//debugln('---Función usada: ' + fun.name);
    {According the method we use to add callers (See TCompilerBase.AddCallerTo() ),
    condition "fun.nCalled>0" ensures we have here the first ocurrence of a function. So
    it can be:
      - A common function/procedure in the main program.
      - A INTERFACE function.
      - A private IMPLEMENTATION function (without previous declaration).
    }
    //Compile used function in the current address.
    fun.adrr := pic.iRam;     //Actualiza la dirección final
    //Is a common function with body.
    GenCodeFunction(fun.BodyNode);
    if HayError then exit;   //Puede haber error
    fun.coded := true;       //Marca como ya codficada en memoria.
    //Verifica si hace falta completar llamadas
    if fun.nAddresPend>0 then begin
        //Hay llamadas pendientes que completar a esta función
        for i:=0 to fun.nAddresPend -1 do begin
          debugln('Completando lllamadas pendientes a %s en %d', [fun.name, fun.addrsPend[i]]);
          //Completa la instrucción JSR $0000
          add := fun.addrsPend[i];
          pic.ram[add].value   := fun.adrr and $ff;
          pic.ram[add+1].value := (fun.adrr >> 8) and $ff;
        end;
      end;
  end;
  for fun in unusedFuncs do begin
    //Esta función no se usa.
    if fun.Parent = TreeElems.main then begin
      //Genera mensaje solo para funciones del programa principal.
      GenWarn(WA_UNUSED_PRO_, [fun.name], fun.srcDec);
    end;
  end;
  //Compila cuerpo del programa principal
  _LABEL_post(iniMain);   //Termina de codificar el salto
  bod := TreeElems.BodyNode;  //lee Nodo del cuerpo principal
  if bod = nil then begin
    GenError('Body program not found.');
    exit;
  end;
  bod.adrr := pic.iRam;  //guarda la dirección de codificación
//  bod.nCalled := 1;    //Actualiza
  GenCodeMainBody(bod);
  //if HayError then exit;     //Puede haber error
  {No es necesario hacer más validaciones, porque ya se hicieron en la primera pasada}
  //_RTS();   //agrega instrucción final
end;
procedure TCompiler_PIC16.Compile(NombArc: string; Link: boolean);
//Compila el contenido de un archivo.
var
  p: SizeInt;
begin
  DefCompiler;   //Debe hacerse solo una vez al inicio
  mode := modPicPas;   //Por defecto en sintaxis nueva
  mainFile := NombArc;
  hexfile := ChangeFileExt(NombArc, '.prg');     //Obtiene nombre
  hexfile := hexFilePath;   //Expande nombre si es necesario
  //se pone en un "try" para capturar errores y para tener un punto salida de salida
  //único
  if ejecProg then begin
    GenError(ER_COMPIL_PROC);
    exit;  //sale directamente
  end;
  try
    ejecProg := true;  //marca bandera
    ClearError;
    //Genera instrucciones de inicio
    ClearContexts;       //elimina todos los Contextos de entrada
    //Compila el texto indicado
    if not OpenContextFrom(NombArc) then begin
      //No lo encuentra
      GenError(ER_FIL_NOFOUND, [NombArc]);
      exit;
    end;
    {-------------------------------------------------}
    TreeElems.Clear;
    //Asigna nombre y archivo a elemento
    TreeElems.main.name := ExtractFileName(mainFile);
    p := pos('.',TreeElems.main.name);
    if p <> 0 then TreeElems.main.name := copy(TreeElems.main.name, 1, p-1);
//    TreeElems.main.srcDec.fil := mainFile;
    TreeElems.main.srcDec := GetSrcPos;
    //Continúa con preparación
    TreeDirec.Clear;
    StartCountElapsed;  //Start timer
    CreateSystemElements;  //Crea los elementos del sistema. 3ms aprox.
    ClearMacros;           //Limpia las macros
    //Inicia PIC
    ExprLevel := 0;  //inicia
    pic.dataAddr1   := -1;  {Reset flag}
    //Compila el archivo actual como programa o como unidad
    if IsUnit then begin
      CompiledUnit := true;
      DoAnalyzeUnit(TreeElems.main);
      if HayError then exit;
      UpdateCallersToUnits;
      EndCountElapsed('** Unit analyzed in: ');
    end else begin
      //Debe ser un programa
      CompiledUnit := false;
      DoAnalyzeProgram;    //puede dar error
      if HayError then exit;
      UpdateCallersToUnits;
      EndCountElapsed('** Program analyzed in: ');
      if Link then begin  //El enlazado solo es válido para programas
//        {Compila solo los procedimientos usados, leyendo la información del árbol de sintaxis,
//        que debe haber sido actualizado en la primera pasada.}
        StartCountElapsed;
        DoOptimize;
        if HayError then exit;
        EndCountElapsed('-- Optimized in: ');
        StartCountElapsed;
        DoGenerateCode;
        EndCountElapsed('-- Synthetized in: ');
        StartCountElapsed;
        //Genera archivo hexa, en la misma ruta del programa
        pic.GenHex(hexFile, GeneralORG);
        EndCountElapsed('-- Output generated in: ');
      end;
    end;
    {-------------------------------------------------}
    //ClearAll;//es necesario por dejar limpio
  finally
    ejecProg := false;
    //Tareas de finalización
    if OnAfterCompile<>nil then OnAfterCompile;
  end;
end;
function AdrStr(absAdr: word): string;
{formatea una dirección en cadena.}
begin
  Result := '0x' + IntToHex(AbsAdr, 3);
end;
procedure TCompiler_PIC16.RAMusage(lins: TStrings; ExcUnused: boolean);
{Devuelve una cadena con información sobre el uso de la memoria.}
var
  v: TEleVarDec;
  subUsed: string;
begin
  for v in TreeElems.AllVars do begin   //Se supone que "AllVars" ya se actualizó.
      //debugln('AllVars['+IntToStr(i)+']='+v.name+','+v.Parent.name);
      if ExcUnused and (v.nCalled = 0) then continue;
      if v.nCalled = 0 then subUsed := '; <Unused>' else subUsed := '';
      if v.typ.IsByteSize then begin
        lins.Add(v.name + ' EQU ' +  AdrStr(v.addr)+ subUsed);
      end else if v.typ.IsWordSize then begin
        lins.Add(v.name+'@0' + ' EQU ' +  AdrStr(v.addrL)+ subUsed);
        lins.Add(v.name+'@1' + ' EQU ' +  AdrStr(v.addrH)+ subUsed);
      end else if v.typ.IsDWordSize then begin
        lins.Add(v.name+'@0' + ' EQU ' +  AdrStr(v.addrL)+ subUsed);
        lins.Add(v.name+'@1' + ' EQU ' +  AdrStr(v.addrH)+ subUsed);
        lins.Add(v.name+'@2' + ' EQU ' +  AdrStr(v.addrE)+ subUsed);
        lins.Add(v.name+'@3' + ' EQU ' +  AdrStr(v.addrU)+ subUsed);
      end else begin
        lins.Add('"' + v.name + '"->' +  AdrStr(v.addr) + subUsed);
      end;
  end;
  //Reporte de registros de trabajo, auxiliares
  {***** En el nuevo esquema de trabajo, no se maenjan registros auxiliares y
  los registros se manejan como simples varaibles.}
//  lins.Add(';-------------------------');
end;
procedure TCompiler_PIC16.DumpCode(lins: TSTrings; AsmMode, IncVarDec,
  ExcUnused: boolean; incAdrr, incCom, incVarNam: boolean);
var
  i: Integer;
  minUsed: integer;
begin
  if AsmMode then begin
    //Incluye encabezado
    lins.Add('    ;Code generated by P6502 compiler');
    lins.Add('    processor ' + PICName);
    if IncVarDec then begin
       lins.Add(';===RAM usage===');
       RAMusage(lins, ExcUnused);
    end;
    lins.Add(';===Blocks of Code===');
    pic.DumpCodeAsm(lins, incAdrr, incAdrr, incCom, incVarNam);
    lins.Add(';--------------------');
    lins.Add('      END');
  end else begin
    minUsed := pic.CPUMAXRAM;
    for i := 0 to pic.CPUMAXRAM-1 do begin
      if pic.ram[i].used <> ruUnused then begin
        if i<minUsed then minUsed := i;  //Calcula mínimo
        lins.Add('poke ' +  IntToStr(i) + ',' + IntToStr(pic.ram[i].value));
      end;
    end;
    lins.Add('sys ' +  IntToStr(minUsed) );
  end;
end;
function TCompiler_PIC16.RAMusedStr: string;
var
  usedRAM, totRAM: integer;
begin
  totRAM := pic.TotalMemRAM;
  if totRAM=0 then exit;  //protección
  usedRAM := pic.UsedMemRAM;
  Result := MSG_RAM_USED + IntToStr(usedRAM) +'/'+ IntToStr(totRAM) + 'B (' +
        FloatToStrF(100*usedRAM/totRAM, ffGeneral, 1, 3) + '%)';
end;
procedure TCompiler_PIC16.GetResourcesUsed(out ramUse, romUse, stkUse: single);
var
  usedRAM, totRAM: integer;
begin
  //Calcula RAM
  ramUse := 0;  //valor por defecto
  totRAM := pic.TotalMemRAM;
  if totRAM = 0 then exit;  //protección
  usedRAM := pic.UsedMemRAM;
  ramUse := usedRAM/totRAM;
  //Calcula STACK
//  nes := TreeElems.main.UpdateCalledAll;   //Debe haberse llenado TreeElems.main.lstCalled
  //No considera el anidamiento por interrupciones
  stkUse := TreeElems.main.maxNesting/STACK_SIZE;
end;
procedure TCompiler_PIC16.GenerateListReport(lins: TStrings);
{Genera un reporte detallado de la compialción}
var
  curInst, opc: TP6502Inst;
  i: word;
  OpCodeCoun: array[low(TP6502Inst)..high(TP6502Inst)] of integer;
  tmpList: TStringList;
  txt, OpCode, Times, state: String;

  fun: TEleFun;
  caller : TxpEleCaller;
  called : TxpElement;
  exitCall: TxpExitCall;
begin
  ////////////////////////////////////////////////////////////
  //////////// Reporte de uso de memeoria  ///////////
  ////////////////////////////////////////////////////////////
  lins.Add(RAMusedStr);
  ////////////////////////////////////////////////////////////
  //////////// Reporte de cuenta de instrucciones  ///////////
  ////////////////////////////////////////////////////////////
  //Limpia contadores
  for opc := low(TP6502Inst) to high(TP6502Inst) do begin
    OpCodeCoun[opc] := 0;
  end;
  //Cuenta apariciones
  for i:=0 to high(pic.ram) do begin
    if pic.ram[i].used <> ruUnused then begin
       pic.PC.W := i;
       curInst := pic.CurInstruction;
       Inc(OpCodeCoun[curInst]);  //Acumula
    end;
  end;
  //Carga en lista para ordenar
  tmpList:= TStringList.Create;
  for opc := low(TP6502Inst) to high(TP6502Inst) do begin
    tmpList.Add(Format('%.4d', [OpCodeCoun[Opc]]) + '-' + PIC16InstName[opc].name);
  end;
  tmpList.Sort;  //Ordena
  //Muestra lista ordenada
  lins.Add(';INSTRUCTION COUNTER');
  lins.Add(';===================');
  for i:=tmpList.Count-1 downto 0 do begin
    txt := tmpList[i];
    OpCode := copy(txt , 6, 10);
    Times  := copy(txt , 1, 4);
    if Times = '0000' then continue;
    lins.Add(copy(OpCode + '    ',1,7) + '->'+ Times);
  end;
  tmpList.Destroy;

  ////////////////////////////////////////////////////////////
  ////////////////// Reporte de Funciones   ///////////
  ////////////////////////////////////////////////////////////
  lins.Add('');
  lins.Add(';PROCEDURE LIST');
  lins.Add(';===================');

  lins.Add(';NAME                    USED   POSIITON IN SOURCE');
  lins.Add(';----------------------- ------ -------------------------------');
  for fun in TreeElems.AllFuncs do begin
    if fun.nCalled > 0 then begin
      if fun.nCalled = 0 then
        state := 'Unused'
      else
        state := RightStr('     '+IntToStr(fun.nCalled)+ '', 6);

      lins.Add( copy(fun.name + space(24) , 1, 24) + ' ' +
                state + ' ' +
                fun.srcDec.RowColString + ':' + ctxFile(fun.srcDec.idCtx)
      );
    end;
  end;

  ////////////////////////////////////////////////////////////
  ////////////////// Detalle de Funciones   ///////////
  ////////////////////////////////////////////////////////////
  lins.Add('');
  lins.Add(';PROCEDURE DETAIL');
  lins.Add(';===================');
  for fun in TreeElems.AllFuncs do begin
    if fun.nCalled > 0 then begin
      lins.Add('------------------------------------');
      lins.Add('----- PROCEDURE ' + fun.name);
      lins.Add('------------------------------------');
      lins.Add('  Caller Procedures:');
      if fun.lstCallers .Count = 0 then begin
        lins.Add('    <none>');
      end else begin
        for caller in fun.lstCallers do begin
          lins.Add('    - ' + caller.caller.Parent.name);
        end;
      end;
      lins.Add('');

      lins.Add('  Called Procedures:');
      if fun.lstCalled.Count = 0 then begin
        lins.Add('    <none>');
      end else begin
        for called in fun.lstCalled do begin
          lins.Add('    - ' + called.name);
        end;
      end;
      lins.Add('');

      lins.Add('  All Called Procedures:');
      if fun.lstCalledAll.Count = 0 then begin
        lins.Add('    <none>');
      end else begin
        for called in fun.lstCalledAll do begin
          lins.Add('    - ' + called.name);
        end;
      end;
      lins.Add('');

      lins.Add('  Exit Instruction Calls:');
      if fun.lstExitCalls.Count = 0 then begin
        lins.Add('    <none>');
      end else begin
        for exitCall in fun.lstExitCalls do begin
          lins.Add('    - Exit() in ' +exitCall.srcPos.RowColString);
        end;
      end;
      lins.Add('');

    end;
  end;
  //Detalles del programa principal

  lins.Add('------------------------------------');
  lins.Add('----- Main Program');
  lins.Add('------------------------------------');
  lins.Add('  Called Procedures:');
  if TreeElems.main.lstCalled.Count = 0 then begin
    lins.Add('    <none>');
  end else begin
    for called in TreeElems.main.lstCalled do begin
      lins.Add('    - ' + called.name);
    end;
  end;
  lins.Add('');
  //Muestra el máximo nivel de anidamiento.
  lins.Add('Max. Nesting = ' + IntToSTr(TreeElems.main.maxNesting));

end;
constructor TCompiler_PIC16.Create;
begin
  inherited Create;
  OnNewLine:=@cInNewLine;
  mode := modPicPas;   //Por defecto en sintaxis nueva
end;
destructor TCompiler_PIC16.Destroy;
begin
  inherited Destroy;
end;

end.

