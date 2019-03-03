{Unidad con rutinas del analizador sintáctico.
}
unit Compiler_PIC16;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, lclProc, SynEditHighlighter, types, MisUtils, XpresBas,
  XpresTypesPIC, XpresElementsPIC, P6502utils, Parser, ParserDirec,
  GenCodBas_PIC16, GenCod_PIC16, ParserDirec_PIC16, Globales,
  FormConfig {Por diseño, FormConfig, no debería accederse desde aquí};
type
 { TCompiler }

  { TCompiler_PIC16 }

  TCompiler_PIC16 = class(TParserDirec)
  private   //Funciones básicas
    function AddType(typName: string; srcPos: TSrcPos): TxpEleType;
    function AddVariable(varName: string; eleTyp: TxpEleType; srcPos: TSrcPos
      ): TxpEleVar;
    procedure ArrayDeclaration(out itemTyp: TxpEleType; out nEle: integer);
    procedure array_high(const OpPtr: pointer);
    procedure array_low(const OpPtr: pointer);
    procedure CompileTypeDeclar(IsInterface: boolean; typName: string = '');
    function GetAdicVarDeclar(out IsBit: boolean): TAdicVarDec;
    procedure cInNewLine(lin: string);
    function CompileStructBody(GenCode: boolean): boolean;
    function CompileConditionalBody: boolean;
    function CompileNoConditionBody(GenCode: boolean): boolean;
    procedure CompileFOR;
    procedure CompileLastEnd;
    procedure CompileProcHeader(out fun: TxpEleFun; ValidateDup: boolean = true);
    function GetExpressionBool: boolean;
    function getParamType: TxpEleType;
    function GetTypeVarDeclar: TxpEleType;
    function IsUnit: boolean;
    procedure array_length(const OpPtr: pointer);
    procedure ProcCommentsNoExec;
    function StartOfSection: boolean;
    procedure ResetRAM;
    procedure getListOfIdent(var itemList: TStringDynArray; out
      srcPosArray: TSrcPosArray);
    procedure CaptureDecParams(fun: TxpEleFun);
    procedure CompileIF;
    procedure CompileREPEAT;
    procedure CompileWHILE;
    procedure Tree_AddElement(elem: TxpElement);
    function VerifyEND: boolean;
  protected //Métodos OVERRIDE
    procedure ProcComments; override;
    procedure TipDefecNumber(var Op: TOperand; toknum: string); override;
    procedure TipDefecString(var Op: TOperand; tokcad: string); override;
    procedure TipDefecBoolean(var Op: TOperand; tokcad: string); override;
  private   //Rutinas para la compilación y enlace
    procedure CompileProcBody(fun: TxpEleFun);
  private //Compilación de secciones
    procedure CompileGlobalConstDeclar;
    procedure CompileVarDeclar(IsInterface: boolean = false);
    procedure CompileProcDeclar(IsImplementation: boolean);
    procedure CompileInstruction;
    procedure CompileInstructionDummy;
    function OpenContextFrom(filePath: string): boolean;
    procedure CompileCurBlock;
    procedure CompileCurBlockDummy;
    procedure CompileUnit(uni: TxpElement);
    procedure CompileUsesDeclaration;
    procedure CompileProgram;
    procedure CompileLinkProgram;
  public
    OnAfterCompile: procedure of object;   //Al finalizar la compilación.
    {Indica que TCompiler, va a acceder a un archivo, peor está pregunatndo para ver
     si se tiene un Stringlist, con los datos ya caragdos del archivo, para evitar
     tener que abrir nuevamente al archivo.}
    OnRequireFileString: procedure(FilePath: string; var strList: TStrings) of object;
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
  ER_NOT_IMPLEM_, ER_IDEN_EXPECT, ER_DUPLIC_IDEN, ER_INVAL_FLOAT: string;
  ER_ERR_IN_NUMB, ER_NOTYPDEFNUM, ER_UNDEF_TYPE_: string;
  ER_INV_MAD_DEV, ER_EXP_VAR_IDE, ER_INV_MEMADDR, ER_BIT_VAR_REF: String;
  ER_UNKNOWN_ID_: string;
  ER_IDE_CON_EXP, ER_NUM_ADD_EXP, ER_IDE_TYP_EXP, ER_SEM_COM_EXP: String;
  ER_EQU_COM_EXP, ER_END_EXPECTE, ER_EOF_END_EXP, ER_BOOL_EXPECT: String;
  ER_UNKN_STRUCT, ER_PROG_NAM_EX, ER_COMPIL_PROC, ER_CON_EXP_EXP: String;
  ER_NOT_AFT_END, ER_ELS_UNEXPEC : String;
  ER_INST_NEV_EXE, ER_ONLY_ONE_REG: String;
  ER_VARIAB_EXPEC, ER_ONL_BYT_WORD, ER_ASIG_EXPECT: String;
  ER_FIL_NOFOUND, WA_UNUSED_CON_, WA_UNUSED_VAR_,WA_UNUSED_PRO_: String;
  MSG_RAM_USED, MSG_FLS_USED, ER_NOTYPDEF_NU, ER_ARR_SIZ_BIG: String;
  ER_INV_ARR_SIZ: String;
//Funciones básicas
procedure SetLanguage;
begin
  ParserDirec_PIC16.SetLanguage;
{$I ..\language\tra_Parser.pas}
end;
procedure TCompiler_PIC16.cInNewLine(lin: string);
//Se pasa a una nueva _Línea en el contexto de entrada
begin
  if Config.IncComment then begin
    pic.addTopComm('    ;'+trim(lin));  //agrega _Línea al código ensmblador
  end;
end;
function TCompiler_PIC16.StartOfSection: boolean;
begin
  Result := (cIn.tokL ='var') or (cIn.tokL ='const') or
            (cIn.tokL ='type') or (cIn.tokL ='procedure');
end;
procedure TCompiler_PIC16.ResetRAM;
{Reinicia el dispositivo, para empezar a escribir en la posición $000 de la FLASH, y
en la posición inicial de la RAM.}
begin
  pic.iRam := 0;  //Ubica puntero al inicio.
  pic.ClearMemRAM;  //Pone las celdas como no usadas y elimina nombres.
  StartRegs;        //Limpia registros de trabajo, auxiliares, y de pila.
end;
procedure TCompiler_PIC16.getListOfIdent(var itemList: TStringDynArray; out srcPosArray: TSrcPosArray);
{Lee una lista de identificadores separados por comas, hasta encontra un caracter distinto
de coma. Si el primer elemento no es un identificador o si después de la coma no sigue un
identificador, genera error.
También devuelve una lista de las posiciones de los identificadores, en el código fuente.}
var
  item: String;
  n: Integer;
begin
  setlength(srcPosArray,0 );
  setlength(itemList, 0);  //hace espacio
  repeat
    ProcComments;
    //ahora debe haber un identificador
    if cIn.tokType <> tnIdentif then begin
      GenError(ER_IDEN_EXPECT);
      exit;
    end;
    //hay un identificador
    item := cIn.tok;
    //sgrega nombre de ítem
    n := high(itemList)+1;
    setlength(itemList, n+1);  //hace espacio
    setlength(srcPosArray, n+1);  //hace espacio
    itemList[n] := item;  //agrega nombre
    srcPosArray[n] := cIn.ReadSrcPos;  //agrega ubicación de declaración
    cIn.Next;  //lo toma identificador despues, de guardar ubicación
    ProcComments;
    if cIn.tok <> ',' then break; //sale
    cIn.Next;  //toma la coma
  until false;
end;
function TCompiler_PIC16.getParamType: TxpEleType;
{Lee el tipo que acompaña a una declaración de parámetro de un procedimeinto.
Es muy similar a GetTypeVarDeclaration (excepto porque aquí se verifican los tipos
copias), y tal vez debería unificarse.}
var
  typName: String;
  typ: TxpEleType;
  ele: TxpElement;
begin
  ProcComments;
  typName := cIn.tok;   //lee tipo de parámetro
  //Primero verifica si es un tipo del sistema
  typ := FindSysEleType(typName);  //tipos básicos
  if typ <> nil then begin
    //Es un tipo del sistema
  end else begin
    //No es un tipo delñ sistema, debe ser otro tipo
    ele := TreeElems.FindFirst(typName);  //identifica elemento
    if ele = nil then begin
      //No identifica a este elemento
      GenError('Unknown identifier: %s', [typName]);
      exit(nil);
    end;
    if ele.idClass = eltType then begin
      //Es un tipo
      typ := TxpEleType(ele);
      AddCallerTo(typ);   //lleva la cuenta
      if typ.copyOf<>nil then typ := typ.copyOf;
    end else begin
      GenError(ER_IDE_TYP_EXP);
      exit(nil);
    end;
  end;
  cIn.Next;
  Result := typ;
end;
procedure TCompiler_PIC16.ProcComments;
{Procesa comentarios, directivas y bloques ASM. Los bloques ASM, se processan también
como comentarios o directivas, para poder ubicarlos dentro de instrucciones, y poder
darle mayor poder, en el futuro.
Notar que este procedimiento puede detectar varios errores en el mismo bloque, y que
pasa al siguiente token, aún cuando detecta errores. Esto permite seguir proesando
el texto, después de que ya se han generado errores dentro de este blqoue. Así, no
sería necesario verificar error después de esta rutina, y se podrían detectar errores
adicionales en el código fuente.}
var
  ctxChanged: Boolean;  //Manejamos variables locales para permitir recursividad
begin
  cIn.SkipWhites;
  while (cIn.tokType = tnDirective) or (cIn.tokType = tnAsm) do begin
    if cIn.tokType = tnAsm then begin
      //Es una línea ASM
      ProcASMlime(cIn.tok);  //procesa línea
      if HayError then begin
        cIn.Next;   //Pasa, porque es un error ya ubicado, y mejor buscamos otros
        cIn.SkipWhites;
        continue;
      end;
    end else begin
      //Es una directiva
     ProcDIRline(cIn.tok, ctxChanged);  //procesa línea
      if HayError then begin
        cIn.Next;   //Pasa, porque es un error ya ubicado, y mejor buscamos otros
        cIn.SkipWhites;
        continue;
      end;
      if ctxChanged then begin
        {Hubo cambio de contexto. Procesamos nuevamente, porque ahora estamos ya en
        otro contexto y se supone que esta llamada a ProcComments(), se hace precisamente
        para saltar blancos, comentarios, directivas, o bloques ASM.}
//        cIn.SkipWhites;   {En el nuevo contexto puede haber nuevos comentarios.}
        ProcComments;   {En el nuevo contexto puede haber nuevos comentarios o bloques Asm.}
        exit;
      end;
    end;
    //Pasa a siguiente
    cIn.Next;
    cIn.SkipWhites;  //limpia blancos
  end;
end;
procedure TCompiler_PIC16.ProcCommentsNoExec;
{Similar a ProcComments(), pero no ejecuta directivas o bloques ASM.}
begin
  cIn.SkipWhites;
  while (cIn.tokType = tnDirective) or (cIn.tokType = tnAsm) do begin
    //Pasa a siguiente
    cIn.Next;
    cIn.SkipWhites;  //limpia blancos
  end;
end;
procedure TCompiler_PIC16.CompileLastEnd;
{Compila la parte de final de un programa o una unidad}
begin
  if cIn.Eof then begin
    GenError(ER_EOF_END_EXP);
    exit;       //sale
  end;
  if cIn.tokL <> 'end' then begin  //verifica si termina el programa
    if cIn.tokL = 'else' then begin
      //Precisa un poco más en el error
      GenError(ER_ELS_UNEXPEC);
      exit;       //sale
    end else begin
      GenError(ER_END_EXPECTE);
      exit;       //sale
    end;
  end;
  cIn.Next;   //coge "end"
  //Debería seguir el punto
  if not CaptureTok('.') then exit;
  //no debe haber más instrucciones
  ProcComments;
  if not cIn.Eof then begin
    GenError(ER_NOT_AFT_END);
    exit;       //sale
  end;
end;
function TCompiler_PIC16.AddVariable(varName: string; eleTyp: TxpEleType; srcPos: TSrcPos
  ): TxpEleVar;
{Crea un elemento variable y lo agrega en el nodo actual del árbol de sintaxis.
Si no hay errores, devuelve la referencia a la variable. En caso contrario,
devuelve NIL.
Notar que este método, no asigna RAM a la variable. En una creación completa de
variables, se debería llamar a CreateVarInRAM(), después de agregar la variable.}
var
  xvar: TxpEleVar;
begin
  //Inicia parámetros adicionales de declaración
  xvar := CreateVar(varName, eleTyp);
  xvar.srcDec := srcPos;  //Actualiza posición
  Result := xvar;
  if not TreeElems.AddElement(xvar) then begin
    GenErrorPos(ER_DUPLIC_IDEN, [xvar.name], xvar.srcDec);
    xvar.Destroy;   //Hay una variable creada
    exit(nil);
  end;
end;
function TCompiler_PIC16.AddType(typName: string; srcPos: TSrcPos): TxpEleType;
{Crea un elemento tipo y lo agrega en el nodo actual del árbol de sintaxis.
Si no hay errores, devuelve la referencia al tipo. En caso contrario,
devuelve NIL.}
var
  xtyp: TxpEleType;
begin
  //Inicia parámetros adicionales de declaración
  xtyp := CreateEleType(typName);
  xtyp.srcDec := srcPos;  //Actualiza posición
  Result := xtyp;
  if not TreeElems.AddElement(xtyp) then begin
    GenErrorPos(ER_DUPLIC_IDEN, [xtyp.name], xtyp.srcDec);
    xtyp.Destroy;   //Hay una variable creada
    exit(nil);
  end;
end;
procedure TCompiler_PIC16.CaptureDecParams(fun: TxpEleFun);
//Lee la declaración de parámetros de una función.
var
  typ: TxpEleType;
  xvar: TxpEleVar;
  IsRegister: Boolean;
  itemList: TStringDynArray;
  srcPosArray: TSrcPosArray;
  i: Integer;
begin
  cIn.SkipWhites;
  SetLength(itemList, 0);
  if EOBlock or EOExpres or (cIn.tok = ':') then begin
    //no tiene parámetros
  end else begin
    //Debe haber parámetros
    if not CaptureTok('(') then exit;
    cin.SkipWhites;
    repeat
      IsRegister := false;
      if cIn.tokL = 'register' then begin
        IsRegister := true;
        cin.Next;
        cin.SkipWhites;
      end;
      getListOfIdent(itemList, srcPosArray);
      if HayError then begin  //precisa el error
        GenError(ER_IDEN_EXPECT);
        exit;
      end;
      if not CaptureTok(':') then exit;
      typ := getParamType;  //lee tipo
      if HayError then exit;
      //Ya tiene los nombres y el tipo
      //Crea el parámetro como una varaible local
      for i:= 0 to high(itemList) do begin
        //Crea los parámetros de la lista.
        if IsRegister then begin
          //Parámetro REGISTER. Solo puede haber uno
          if high(itemList)>0 then begin
            GenErrorPos(ER_ONLY_ONE_REG, [], srcPosArray[1]);
            exit;
          end;
          {Crea como variable absoluta a una posición cualquiera porque esta variable,
          no debería estar mapeada.}
          xvar := AddVariable({fun.name + '_' + }itemList[i], typ, srcPosArray[i]);
          xvar.IsParameter := true;  //Marca bandera
          xvar.IsRegister := true;
          //CreateVarInRAM(xvar);  //Crea la variable
          if HayError then exit;
        end else begin
          //Parámetro normal
          xvar := AddVariable({fun.name + '_' + }itemList[i], typ, srcPosArray[i]);
          xvar.IsParameter := true;  //Marca bandera
          xvar.IsRegister := false;
          //CreateVarInRAM(xvar);  //Crea la variable
          if HayError then exit;
        end;
        //Ahora ya tiene la variable
        fun.CreateParam(itemList[i], typ, xvar);
        if HayError then exit;
      end;
      //Busca delimitador
      if cIn.tok = ';' then begin
        cIn.Next;   //toma separador
        cIn.SkipWhites;
      end else begin
        //no sigue separador de parámetros,
        //debe terminar la lista de parámetros
        //¿Verificar EOBlock or EOExpres ?
        break;
      end;
    until false;
    //busca paréntesis final
    if not CaptureTok(')') then exit;
  end;
end;
function TCompiler_PIC16.CompileStructBody(GenCode: boolean): boolean;
{Compila el cuerpo de un THEN, ELSE, WHILE, ... considerando el modo del compilador.
Si se genera error, devuelve FALSE. }
begin
  if GenCode then begin
    //Este es el modo normal. Genera código.
    if mode = modPascal then begin
      //En modo Pascal se espera una instrucción
      CompileInstruction;
    end else begin
      //En modo normal
      CompileCurBlock;
    end;
    if HayError then exit(false);
  end else begin
    //Este modo no generará instrucciones
    cIn.SkipWhites;
    GenWarn(ER_INST_NEV_EXE);
    if mode = modPascal then begin
      //En modo Pascal se espera una instrucción
      CompileInstructionDummy //solo para mantener la sintaxis
    end else begin
      //En modo normal
      CompileCurBlockDummy;  //solo para mantener la sintaxis
    end;
    if HayError then exit(false);
  end;
  //Salió sin errores
  exit(true);
end;
function TCompiler_PIC16.CompileConditionalBody: boolean;
{Versión de CompileStructBody(), para bloques condicionales.
Se usa para bloque que se ejecutarán de forma condicional, es decir, que no se
garantiza que se ejecute siempre. "FinalBank" indica el banco en el que debería
terminar el bloque.}
begin
  Result := CompileStructBody(true);  //siempre genera código
end;
function TCompiler_PIC16.CompileNoConditionBody(GenCode: boolean): boolean;
{Versión de CompileStructBody(), para bloques no condicionales.
Se usa para bloques no condicionales, es decir que se ejecutará siempre (Si GenCode
es TRUE) o nunca (Si GenCode es FALSE);
}
begin
  //"BankChanged" sigue su curso normal
  Result := CompileStructBody(GenCode);
end;
function TCompiler_PIC16.VerifyEND: boolean;
{Compila la parte final de la estructura, que en el modo PicPas, debe ser el
 delimitador END. Si encuentra error, devuelve FALSE.}
begin
  Result := true;   //por defecto
  if mode = modPicPas then begin
    //En modo PicPas, debe haber un delimitador de bloque
    if not CaptureStr('end') then exit(false);
  end;
end;
function TCompiler_PIC16.GetExpressionBool: boolean;
{Lee una expresión booleana. Si hay algún error devuelve FALSE.}
begin
  GetExpressionE(0);
  if HayError then exit(false);
  if res.Typ <> typBool then begin
    GenError(ER_BOOL_EXPECT);
    exit(false);
  end;
  ProcComments;
  exit(true);  //No hay error
end;
procedure TCompiler_PIC16.CompileIF;
{Compila una extructura IF}
var
  jEND_TRUE: integer;
  lbl1: TIfInfo;
begin
  if not GetExpressionBool then exit;
  if not CaptureStr('then') then exit; //toma "then"
  //Aquí debe estar el cuerpo del "if"
  case res.Sto of
  stConst: begin  //la condición es fija
    if res.valBool then begin
      //Es verdadero, siempre se ejecuta
      if not CompileNoConditionBody(true) then exit;
      //Compila los ELSIF que pudieran haber
      while cIn.tokL = 'elsif' do begin
        cIn.Next;   //toma "elsif"
        if not GetExpressionBool then exit;
        if not CaptureStr('then') then exit;  //toma "then"
        //Compila el cuerpo pero sin código
        if not CompileNoConditionBody(false) then exit;
      end;
      //Compila el ELSE final, si existe.
      if cIn.tokL = 'else' then begin
        //Hay bloque ELSE, pero no se ejecutará nunca
        cIn.Next;   //toma "else"
        if not CompileNoConditionBody(false) then exit;
        if not VerifyEND then exit;
      end else begin
        VerifyEND;
      end;
    end else begin
      //Es falso, nunca se ejecuta
      if not CompileNoConditionBody(false) then exit;
      if cIn.tokL = 'else' then begin
        //hay bloque ELSE, que sí se ejecutará
        cIn.Next;   //toma "else"
        if not CompileNoConditionBody(true) then exit;
        VerifyEND;
      end else if cIn.tokL = 'elsif' then begin
        cIn.Next;
        CompileIF;  //más fácil es la forma recursiva
        if HayError then exit;
        //No es necesario verificar el END final.
      end else begin
        VerifyEND;
      end;
    end;
  end;
  stVariab, stExpres:begin
    IF_TRUE(@res, lbl1);
//    Cod_JumpIfTrue;
//    _JMP_lbl(jFALSE);  //salto pendiente
    //Compila la parte THEN
    if not CompileConditionalBody then exit;
    //Verifica si sigue el ELSE
    if cIn.tokL = 'else' then begin
      //Es: IF ... THEN ... ELSE ... END
      cIn.Next;   //toma "else"
      _JMP_lbl(jEND_TRUE);  //llega por aquí si es TRUE
      IF_END( lbl1);
      if not CompileConditionalBody then exit;
      _LABEL(jEND_TRUE);   //termina de codificar el salto
      VerifyEND;   //puede salir con error
    end else if cIn.tokL = 'elsif' then begin
      //Es: IF ... THEN ... ELSIF ...
      cIn.Next;
      _JMP_lbl(jEND_TRUE);  //llega por aquí si es TRUE
      IF_END( lbl1);
      CompileIF;  //más fácil es la forma recursiva
      if HayError then exit;
      _LABEL(jEND_TRUE);   //termina de codificar el salto
      //No es necesario verificar el END final.
    end else begin
      //Es: IF ... THEN ... END. (Puede ser recursivo)
      IF_END( lbl1);
      VerifyEND;  //puede salir con error
    end;
  end;
  end;
end;
procedure TCompiler_PIC16.CompileREPEAT;
{Compila uan extructura WHILE}
var
  l1: Word;
  info: TIfInfo;
begin
  l1 := _PC;        //guarda dirección de inicio
  CompileCurBlock;
  if HayError then exit;
  cIn.SkipWhites;
  if not CaptureStr('until') then exit; //toma "until"
  if not GetExpressionBool then exit;
  case res.Sto of
  stConst: begin  //la condición es fija
    if res.valBool then begin
      //lazo nulo
    end else begin
      //lazo infinito
      _JMP(l1);
    end;
  end;
  stVariab, stExpres: begin
    IF_FALSE(@res, info);   { TODO : Se debería optimizar. Hay un salto innecesario, útil solo para bloques largos. }
    _JMP(l1);
    IF_END(info)
    //sale cuando la condición es verdadera
  end;
  end;
end;
procedure TCompiler_PIC16.CompileWHILE;
{Compila una extructura WHILE}
var
  l1: Word;
  info: TIfInfo;
begin
  l1 := _PC;        //guarda dirección de inicio
  if not GetExpressionBool then exit;  //Condición
  if not CaptureStr('do') then exit;  //toma "do"
  //Aquí debe estar el cuerpo del "while"
  case res.Sto of
  stConst: begin  //la condición es fija
    if res.valBool then begin
      //Lazo infinito
      if not CompileNoConditionBody(true) then exit;
      if not VerifyEND then exit;
      _JMP(l1);
    end else begin
      //Lazo nulo. Compila sin generar código.
      if not CompileNoConditionBody(false) then exit;
      if not VerifyEND then exit;
    end;
  end;
  stVariab, stExpres: begin
    IF_TRUE(@res, info);
    if not CompileConditionalBody then exit;
    _JMP(l1);
    IF_END(info);
    if not VerifyEND then exit;
  end;
  end;
end;
procedure TCompiler_PIC16.CompileFOR;
{Compila uan extructura WHILE}
var
  l1: Word;
  dg, LABEL1: Integer;
  Op1, Op2: TOperand;
  opr1: TxpOperator;
  info: TIfInfo;
begin
  Op1 :=  GetOperand;
  if Op1.Sto <> stVariab then begin
    GenError(ER_VARIAB_EXPEC);
    exit;
  end;
  if HayError then exit;
  if (Op1.Typ<>typByte) and (Op1.Typ<>typWord) then begin
    GenError(ER_ONL_BYT_WORD);
    exit;
  end;
  cIn.SkipWhites;
  opr1 := GetOperator(Op1);   //debe ser ":="
  if opr1.txt <> ':=' then begin
    GenError(ER_ASIG_EXPECT);
    exit;
  end;
  GetExpressionE(0);
  if HayError then exit;
  //Ya se tiene la asignación inicial
  Oper(Op1, opr1, res);   //codifica asignación
  if HayError then exit;
  if not CaptureStr('to') then exit;
  //Toma expresión Final
  GetExpressionE(0);
  if HayError then exit;
  cIn.SkipWhites;
  if not CaptureStr('do') then exit;  //toma "do"
  //Aquí debe estar el cuerpo del "for"
  if (res.Sto = stConst) or (res.Sto = stVariab) then begin
    //Es un for con valor final de tipo constante
    //Se podría optimizar, si el valor inicial es también constante
    l1 := _PC;        //guarda dirección de inicio
    //Codifica rutina de comparación, para salir
    opr1 := Op1.Typ.FindBinaryOperator('<=');  //Busca operador de comparación
    if opr1 = nullOper then begin
      GenError('Internal: No operator <= defined for %s.', [Op1.Typ.name]);
      exit;
    end;
    Op2 := res;   //Copia porque la operación Oper() modificará res
    Oper(Op1, opr1, Op2);   //verifica resultado
    IF_TRUE(@res, info);
    if not CompileConditionalBody then exit;
    if not VerifyEND then exit;
    //Incrementa variable cursor
    if Op1.Typ = typByte then begin
      _INC(Op1.rVar.adrByte0);
    end else if Op1.Typ = typWord then begin
      _INC(Op1.rVar.adrByte0);
      _BNE_lbl(LABEL1);  //label
      _INC(Op1.rVar.adrByte1);
_LABEL(LABEL1);
    end;
    _JMP(l1);  //repite el lazo
    //ya se tiene el destino del salto
    IF_END(info);   //termina de codificar el salto
  end else begin
    GenError('Last value must be Constant or Variable');
    exit;
  end;
end;
procedure TCompiler_PIC16.Tree_AddElement(elem: TxpElement);
begin
  if FirstPass then begin
    //Caso normal. Solo aquí dede modificarse el árbol de sintaxis.
  end else begin
    //Solo se permiet agregar elementos en la primera pasada
    GenError('Internal Error: Syntax Tree modified on linking.');
  end;
end;
//Métodos OVERRIDE
procedure TCompiler_PIC16.TipDefecNumber(var Op: TOperand; toknum: string);
{Procesa constantes numéricas, ubicándolas en el tipo de dato apropiado (byte, word, ... )
 Si no logra ubicar el tipo de número, o no puede leer su valor, generará  un error.}
var
  n: int64;   //para almacenar a los enteros
//  f: extended;  //para almacenar a reales
begin
  if pos('.',toknum) <> 0 then begin  //es flotante
    GenError('Unvalid float number.');  //No hay soporte aún para flotantes
//    try
//      f := StrToFloat(toknum);  //carga con la mayor precisión posible
//    except
//      Op.typ := nil;
//      GenError('Unvalid float number.');
//      exit;
//    end;
//    //busca el tipo numérico más pequeño que pueda albergar a este número
//    Op.size := 4;   //se asume que con 4 bytes bastará
//    {Aquí se puede decidir el tamaño de acuerdo a la cantidad de decimales indicados}
//
//    Op.valFloat := f;  //debe devolver un extended
//    menor := 1000;
//    for i:=0 to typs.Count-1 do begin
//      { TODO : Se debería tener una lista adicional TFloatTypes, para acelerar la
//      búsqueda}
//      if (typs[i].cat = t_float) and (typs[i].size>=Op.size) then begin
//        //guarda el menor
//        if typs[i].size < menor then  begin
//           imen := i;   //guarda referencia
//           menor := typs[i].size;
//        end;
//      end;
//    end;
//    if menor = 1000 then  //no hubo tipo
//      Op.typ := nil
//    else  //encontró
//      Op.typ:=typs[imen];
//
  end else begin     //es entero
    //Intenta convertir la cadena. Notar que se reconocen los formatos $FF y %0101
    if not TryStrToInt64(toknum, n) then begin
      //Si el lexer ha hecho bien su trabajo, esto solo debe pasar, cuando el
      //número tiene mucHos dígitos.
      GenError('Error in number.');
      exit;
    end;
    Op.valInt := n;   //copia valor de constante entera
    {Asigna un tipo, de acuerdo al rango. Notar que el tipo más pequeño, usado
    es el byte, y no el bit.}
    if (n>=0) and  (n<=255) then begin
      Op.SetAsConst(typByte);
    end else if (n>= 0) and (n<=$FFFF) then begin
      Op.SetAsConst(typWord);
    end else  begin //no encontró
      GenError(ER_NOTYPDEF_NU);
      Op.SetAsNull;
    end;
  end;
end;
procedure TCompiler_PIC16.TipDefecString(var Op: TOperand; tokcad: string);
//Devuelve el tipo de cadena encontrado en un token
//var
//  i: Integer;
begin
{  Op.catTyp := t_string;   //es cadena
  Op.size:=length(tokcad);
  //toma el texto
  Op.valStr := copy(cIn.tok,2, length(cIn.tok)-2);   //quita comillas
  //////////// Verifica si hay tipos string definidos ////////////
  if length(Op.valStr)=1 then begin
    Op.typ := tipChr;
  end else
    Op.typ :=nil;  //no hay otro tipo}
end;
procedure TCompiler_PIC16.TipDefecBoolean(var Op: TOperand; tokcad: string);
//Devuelve el tipo de cadena encontrado en un token
begin
  //convierte valor constante
  Op.SetAsConst(typBool);
  Op.valBool:= (tokcad[1] in ['t','T']);
end;
//Rutinas para la compilación y enlace
procedure TCompiler_PIC16.CompileProcBody(fun: TxpEleFun);
{Compila el cuerpo de un procedimiento}
begin
  StartCodeSub(fun);    //Inicia codificación de subrutina
  CompileInstruction;
  if HayError then exit;
  if fun.IsInterrupt then begin
    //Las interrupciones terminan así
    _RTI;
  end else begin
    //Para los procedimeintos, podemos terminar siemrpe con un i_RETURN u optimizar,
    if OptRetProc then begin
      //Verifica es que ya se ha incluido exit().
      if fun.ObligatoryExit<>nil then begin
        //Ya tiene un exit() obligatorio y en el final (al menos eso se espera)
        //No es necesario incluir el i_RETURN().
      end else begin
        //No hay un exit(), seguro
        _RTS();  //instrucción de salida
      end;
    end else begin
      _RTS();  //instrucción de salida
    end;
  end;
  EndCodeSub;  //termina codificación
  //Calcula tamaño
  fun.srcSize := pic.iRam - fun.adrr;
end;
function TCompiler_PIC16.OpenContextFrom(filePath: string): boolean;
{Abre un contexto con el archivo indicado. Si lo logra abrir, devuelve TRUE.}
var
  strList: TStrings;
begin
  //Primero ve si puede obteenr acceso directo al contenido del archivo
  if OnRequireFileString<>nil then begin
    //Hace la consulta a través del evento
    strList := nil;
    OnRequireFileString(filePath, strList);
    if strList=nil then begin
      //No hay acceso directo al contenido. Carga de disco
      //debugln('>disco:'+filePath);
      cIn.MsjError := '';
      cIn.NewContextFromFile(filePath);
      Result := cIn.MsjError='';  //El único error es cuando no se encuentra el archivo.
    end else begin
      //Nos están dando el acceso al contenido. Usamos "strList"
      cIn.NewContextFromFile(filePath, strList);
      Result := true;
    end;
  end else begin
    //No se ha establecido el evento. Carga de disco
    //debugln('>disco:'+filePath);
    cIn.MsjError := '';
    cIn.NewContextFromFile(filePath);
    Result := cIn.MsjError='';  //El único error es cuando no se encuentra el archivo.
  end;
end;
//Compilación de secciones
procedure TCompiler_PIC16.CompileGlobalConstDeclar;
var
  consNames: array of string;  //nombre de variables
  cons: TxpEleCon;
  srcPosArray: TSrcPosArray;
  i: integer;
begin
  SetLength(consNames, 0);
  //Procesa lista de constantes a,b,cons ;
  getListOfIdent(consNames, srcPosArray);
  if HayError then begin  //precisa el error
    GenError(ER_IDE_CON_EXP);
    exit;
  end;
  //puede seguir "=" o identificador de tipo
  if cIn.tok = '=' then begin
    cIn.Next;  //pasa al siguiente
    //Debe seguir una expresiócons constante, que no genere consódigo
    GetExpressionE(0);
    if HayError then exit;
    if res.Sto <> stConst then begin
      GenError(ER_CON_EXP_EXP);
    end;
    //Hasta aquí todo bien, crea la(s) constante(s).
    for i:= 0 to high(consNames) do begin
      //crea constante
      cons := CreateCons(consNames[i], res.Typ);
      cons.srcDec := srcPosArray[i];  //guarda punto de declaración
      if not TreeElems.AddElement(cons) then begin
        GenErrorPos(ER_DUPLIC_IDEN, [cons.name], cons.srcDec);
        cons.Destroy;   //hay una constante creada
        exit;
      end;
      res.CopyConsValTo(cons); //asigna valor
    end;
//  end else if cIn.tok = ':' then begin
  end else begin
    GenError(ER_EQU_COM_EXP);
    exit;
  end;
  if not CaptureDelExpres then exit;
  ProcComments;
  //puede salir con error
end;
function TCompiler_PIC16.GetAdicVarDeclar(out IsBit: boolean): TAdicVarDec;
{Verifica si lo que sigue es la sintaxis ABSOLUTE ... . Si esa así, procesa el texto,
pone "IsAbs" en TRUE y actualiza los valores "absAddr" y "absBit". }
  function ReadAddres(tok: string): word;
  {Lee una dirección de RAM a partir de una cadena numérica.
  Puede generar error.}
  var
    n: LongInt;
  begin
    //COnvierte cadena (soporta binario y hexadecimal)
    if not TryStrToInt(tok, n) then begin
      //Podría fallar si es un número muy grande
      GenError(ER_INV_MEMADDR);
      {%H-}exit;
    end;
    if (n<0) or (n>$ffff) then begin
      //Debe set Word
      GenError(ER_INV_MEMADDR);
      {%H-}exit;
    end;
    Result := n;
    if not pic.ValidRAMaddr(Result) then begin
      GenError(ER_INV_MAD_DEV);
      {%H-}exit;
    end;
  end;
  function ReadAddresBit(tok: string): byte;
  {Lee la parte del bit de una dirección de RAM a partir de una cadena numérica.
  Puede generar error.}
  var
    n: Longint;
  begin
    if not TryStrToInt(tok, n) then begin
      GenError(ER_INV_MEMADDR);
      {%H-}exit;
    end;
    if (n<0) or (n>7) then begin
      GenError(ER_INV_MEMADDR);
      {%H-}exit;
    end;
    Result := n;   //no debe fallar
  end;
var
  xvar: TxpEleVar;
  n: integer;
  Op: TOperand;
begin
  Result.srcDec  := cIn.PosAct;  //Posición de inicio de posibles parámetros adic.
  Result.isAbsol := false;       //Bandera
  Result.absVar := nil;          //Por defecto
  if (cIn.tokL <> 'absolute') and (cIn.tok <> '@') then begin
    exit;  //no es variable absoluta
  end;
  //// Hay especificación de dirección absoluta ////
  Result.isAbsol := true;    //marca bandera
  cIn.Next;
  ProcComments;
  if cIn.tokType = tnNumber then begin
    if (cIn.tok[1]<>'$') and ((pos('e', cIn.tok)<>0) or (pos('E', cIn.tok)<>0)) then begin
      //La notación exponencial, no es válida.
      GenError(ER_INV_MEMADDR);
      exit;
    end;
    n := pos('.', cIn.tok);   //no debe fallar
    if n=0 then begin
      //Número entero sin parte decimal
      Result.absAddr := ReadAddres(cIn.tok);
      cIn.Next;  //Pasa con o sin error, porque esta rutina es "Pasa siempre."
      //Puede que siga la parte de bit
      if cIn.tok = '.' then begin
        cIn.Next;
        IsBit := true;  //Tiene parte de bit
        Result.absBit := ReadAddresBit(cIn.tok);  //Parte decimal
        cIn.Next;  //Pasa con o sin error, porque esta rutina es "Pasa siempre."
      end else begin
        IsBit := false;  //No tiene parte de bit
      end;
    end else begin
      //Puede ser el formato <dirección>.<bit>, en un solo token, que es válido.
      IsBit := true;  //Se deduce que tiene punto decimal
      //Ya sabemos que tiene que ser decimal, con punto
      Result.absAddr := ReadAddres(copy(cIn.tok, 1, n-1));
      //Puede haber error
      Result.absBit := ReadAddresBit(copy(cIn.tok, n+1, 100));  //Parte decimal
      cIn.Next;  //Pasa con o sin error, porque esta rutina es "Pasa siempre."
    end;
  end else if cIn.tokType = tnIdentif then begin
    //Puede ser variable
    GetOperandIdent(Op); //
    if HayError then exit;
    if Op.Sto <> stVariab then begin
      GenError(ER_EXP_VAR_IDE);
      cIn.Next;  //Pasa con o sin error, porque esta rutina es "Pasa siempre."
      exit;
    end;
    //Mapeado a variable. Notar que puede ser una variable temporal, si se usa: <var_byte>.0
    xvar := Op.rVar;
    if Op.rVarBase=nil then begin
      Result.absVar := Op.rVar;  //Guarda referencia
    end else begin
      {Es un caso como "<Variab.Base>.0", conviene devolver la referencia a <Variab.Base>,
      en lugar de a la variable "<Variab.Base>.0", considerando que:
      * GetOperandIdent() usa <Variab.Base>, para registrar la llamada.
      * Esta referencia se usará luego para ver variables no usadas en
        TCompiler.CompileLinkProgram().}
      Result.absVar := Op.rVarBase;  //Guarda referencia
    end;
    //Ya tiene la variable en "xvar".
    if xvar.typ.IsBitSize then begin //boolean o bit
      IsBit := true;  //Es una dirección de bit
      Result.absAddr := xvar.addr;  //debe ser absoluta
      Result.absBit := xvar.adrBit.bit;
    end else begin
      //Es cualquier otra variable, que no sea bit. Se intentará
      IsBit := false;  //Es una dirección normal (byte)
      Result.absAddr := xvar.addr;  //debe ser absoluta
    end;
    if Result.absAddr = ADRR_ERROR then begin
      //No se puede obtener la dirección.
      GenError('Cannot locate variable at: %s', [xvar.name]);
//      GenError('Internal Error: TxpEleVar.AbsAddr.');
      exit;
    end;
  end else begin   //error
    GenError(ER_NUM_ADD_EXP);
    cIn.Next;    //pasa siempre
    exit;
  end;
end;
procedure TCompiler_PIC16.array_length(const OpPtr: pointer);
//Devuelve la cantidad de elementos de un arreglo
var
  Op: ^TOperand;
  xvar: TxpEleVar;
begin
  cIn.Next;  //Toma identificador de campo
  Op := OpPtr;
  case Op^.Sto of
  stVariab: begin
    xvar := Op^.rVar;  //Se supone que debe ser de tipo ARRAY
    //Se devuelve una constante, byte
    res.SetAsConst(typByte);
    res.valInt := xvar.typ.arrSize;
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TCompiler_PIC16.array_high(const OpPtr: pointer);
//Devuelve el índice máximo de un arreglo
var
  Op: ^TOperand;
  xvar: TxpEleVar;
begin
  cIn.Next;  //Toma identificador de campo
  Op := OpPtr;
  case Op^.Sto of
  stVariab: begin
    xvar := Op^.rVar;  //Se supone que debe ser de tipo ARRAY
    //Se devuelve una constante, byte
    res.SetAsConst(typByte);
    res.valInt {%H-}:= xvar.typ.arrSize-1;
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TCompiler_PIC16.array_low(const OpPtr: pointer);
//Devuelve el índice mínimo de un arreglo
var
  Op: ^TOperand;
begin
  cIn.Next;  //Toma identificador de campo
  Op := OpPtr;
  case Op^.Sto of
  stVariab: begin
    //Se devuelve una constante, byte
    res.SetAsConst(typByte);
    res.valInt := 0;  //por ahora siempre inicia en 0
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TCompiler_PIC16.ArrayDeclaration(out itemTyp: TxpEleType; out nEle: integer);
{Compila una declaración de arreglo.}
var
  varType: String;
begin
  cIn.Next;  //lo toma
  cIn.SkipWhites;
  if not CaptureTok('[') then exit;
  cIn.SkipWhites;
  if cIn.tokType = tnNumber then begin
    //Intenta obtener valor
    if not TryStrToInt(cIn.tok, nEle) then begin
      //Podría fallar si es un número muy grande o exponencial
      GenError(ER_ERR_IN_NUMB);
      exit;
    end;
    cIn.Next;  //Pasa con o sin error, porque esta rutina es "Pasa siempre."
    //Verifica si hay memoria para este arreglo


//  end else if cIn.tokType = tnIdentif then begin
//    //Puede ser variable
//    GetOperandIdent(Op);
//    if HayError then exit;
//    if Op.catOp <> stVariab then begin
//      GenError(ER_EXP_VAR_IDE);
//      cIn.Next;  //Pasa con o sin error, porque esta rutina es "Pasa siempre."
//      exit;
//    end;
//    //Es variable. Notar que puede ser una variable temporal, si se usa: <var_byte>.0
//    xvar := Op.rVar;
//    //Ya tiene la variable en "xvar".
//    if xvar.typ.IsSizeBit then begin //boolean o bit
//      IsBit := true;  //Es una dirección de bit
//      Result.absAddr := xvar.AbsAddr;  //debe ser absoluta
//      Result.absBit := xvar.adrBit.bit;
//    end else begin
//      IsBit := false;  //Es una dirección normal (byte)
//      Result.absAddr := xvar.AbsAddr;  //debe ser absoluta
//    end;
//    if Result.absAddr = ADRR_ERROR then begin
//      //No se implemento el tipo. No debería pasar.
//      GenError('Internal Error: TxpEleVar.AbsAddr.');
//      exit;
//    end;
  end else begin   //error
    GenError(ER_INV_ARR_SIZ);
    cIn.Next;    //pasa siempre
    exit;
  end;
  if not CaptureTok(']') then exit;
  //Ya se tiene el tamaño del arreglo
  cIn.SkipWhites;
  if not CaptureStr('of') then exit;
  cIn.SkipWhites;
  if (cIn.tokType <> tnType) then begin
    GenError(ER_IDE_TYP_EXP);
    exit;
  end;
  varType := cIn.tok;   //lee tipo
  cIn.Next;
  cIn.SkipWhites;
  //Valida el tipo
  itemTyp := FindSysEleType(varType);
  if itemTyp = nil then begin
    GenError(ER_UNDEF_TYPE_, [varType]);
    exit;
  end;
  if (nEle<0) or (nEle>$FF) then begin
    //Límite físico
    GenError(ER_ARR_SIZ_BIG);
    exit;
  end;
  //Ya se tiene el tipo
  //¿Y si es bit?
//  totSize := nEle * itemTyp.size;  //tamaño en bytes

end;
procedure TCompiler_PIC16.CompileTypeDeclar(IsInterface: boolean; typName: string = '');
{Compila la sección de declaración de un tipo, y genera un elemento TxpEleType, en el
árbol de sintaxis.
Si se especifica typName, se obvia la extracción de la parte " nombreTipo = ", y se
toma el nombre indicado.}
var
  etyp, systyp, itemTyp, reftyp: TxpEleType;
  srcpos: TSrcPos;
  nEle: integer;
begin
  ProcComments;
  if cIn.tokType <> tnIdentif then begin
    GenError(ER_IDEN_EXPECT);
    exit;
  end;
  //hay un identificador
  srcpos := cIn.ReadSrcPos;
  typName := cIn.tok;
  cIn.Next;
  ProcComments;
  if not CaptureTok('=') then exit;
  ProcComments;
  //Analiza el tipo declarado
  if (cIn.tokType = tnType) then begin
    //Caso normal. Es un tipo del sistema
    systyp := FindSysEleType(cIn.tok); //Busca elemento
    if systyp = nil then begin
      //Esto no debería pasar, porque el lexer indica que es un tipo del sistema.
      GenError(ER_NOT_IMPLEM_, [typName]);
      exit;
    end;
    //Encontró al tipo del sistema
    cIn.Next;   //lo toma
    etyp := AddType(typName, srcpos);
    if HayError then exit;        //Sale para ver otros errores
    {Crea la copia del tipo del sistema, que básicamente es el mismo tipo, solo que
    con otro nombre y que además, ahora, está en el árbol de sintaxis, por lo tanto
    tiene otras reglas de alcance.}
    etyp.copyOf := systyp;  //Indica que es una copia
    etyp.catType := systyp.catType; //tctAtomic. No debería ser necesario
    etyp.InInterface := IsInterface; //No debería ser necesario
  end else if cIn.tokL = 'array' then begin
    //Es un arreglo
    ArrayDeclaration(itemTyp, nEle);
    if HayError then exit;        //Sale para ver otros errores
    //Es una declaración de arreglo
    etyp := AddType(typName, srcpos);
    if HayError then exit;     //Sale para ver otros errores
    etyp.catType := tctArray;  //Tipo arreglo
    etyp.arrSize := nEle;      //Número de ítems
    etyp.refType := itemTyp;   //Tipo de dato
    etyp.InInterface := IsInterface; //No debería ser necesario
    //Crea campos comunes del arreglo
    etyp.CreateField('length', @array_length);
    etyp.CreateField('high', @array_high);
    etyp.CreateField('low', @array_low);
    //Campos que dependen del tipo
    if itemTyp.OnGetItem=nil then begin
      GenError('Cannot declare array of type: %s', [itemTyp.name]);
    end;
    etyp.CreateField('item', itemTyp.OnGetItem);
    if itemTyp.OnClearItems <> nil then begin
      etyp.CreateField('clear'  , itemTyp.OnClearItems);
    end;
  end else if cIn.tok = '^' then begin
    //Es un puntero
    cIn.Next;
    //Por ahora solo permitiremos identificadores de tipos
    reftyp := FindSysEleType(cIn.tok); //Busca elemento
    if reftyp = nil then begin
      //No es un tipo del sistema, pero puede ser un tipo prdefinido
      reftyp := TreeElems.FindType(cIn.tok); //Busca elemento
      if reftyp = nil then begin
        GenError('Expected a type identifier.', [typName]);
        exit;
      end;
    end;
    //Encontró un tipo
    cIn.Next;   //lo toma
    etyp := AddType(typName, srcpos);  //Crae el elemento tipo
    if HayError then exit;       //Sale para ver otros errores
    etyp.catType := tctPointer;  //Tipo puntero
    etyp.refType := reftyp;      //El tipo a donde apunta
    etyp.InInterface := IsInterface; //No debería ser necesario
    //Fija operaciones para la aritmética del puntero
    DefPointerArithmetic(etyp);
    etyp.CreateUnaryPostOperator('^',6,'deref', @ROU_derefPointer);  //dereferencia

  end else begin
    GenError(ER_IDE_TYP_EXP);
    exit;
  end;

  if not CaptureDelExpres then exit;
  ProcComments;
  //puede salir con error
end;
function TCompiler_PIC16.GetTypeVarDeclar: TxpEleType;
{Extrae la sección de tipo de la declaración de una variable, y devuelve la referencia
al elemento TxpEleType correspondiente.
Si encuentra algún problema, genera error, y devuelve NIL.
Todas las variables y constantes tienen un tipo asociado.

* En los casos de tipos simples como: bit, byte o word, se devuelve la referencia, al
tipo del sistema, almacenado en "listTypSys":
VAR
  a, b: byte;
  c: bit;

* En los casos de tipos definidos en la declaración, se crea una nueva definición de tipo
y se agrega al árbol de sintaxis:
VAR
  a, b: array[0..5] of char;  //Se crea nuevo tipo: "array[0..5] of char"

* Para los casos de tipos con nombre, simplemente se devuelve la referencia al tipo que
debe estar creado en el árbol de sintaxis:
VAR
  a, b: MiTipo;
}
var
  systyp: TxpEleType;
  typName: String;
  typ: TxpEleType;
  ele: TxpElement;
begin
  Result := nil;
  ProcComments;
  typName := cIn.tok;   //Nombre de tipo
  //Primero verifica si es un tipo del sistema
  systyp := FindSysEleType(typName);  //tipos básicos
  if systyp<>nil then begin
    //Caso normal. Es un tipo del sistema
    cIn.Next;   //lo toma
    ProcComments;
    exit(systyp);  //devuelve la referencia
  end else if cIn.tokType = tnIdentif then begin
    //Puede ser identificador de tipo
    {Se pensó usar GetOperandIdent(), para identificar al tipo, pero no está preparado
    para procesar tipos y no se espera tanta flexibilidad. Así que se hace "a mano".}
    ele := TreeElems.FindFirst(cIn.tok);
    if ele = nil then begin
      //No identifica a este elemento
      GenError('Unknown identifier: %s', [cIn.tok]);
      exit(nil);
    end;
    if ele.idClass = eltType then begin
      typ := TxpEleType(ele);
      AddCallerTo(typ);   //lleva la cuenta
      cIn.Next;   //lo toma
      ProcComments;
      exit(typ);
    end else begin
      GenError(ER_IDE_TYP_EXP);
      exit(nil);
    end;
//  end else if cIn.tokL = 'array' then begin
//    //Es un arreglo
//    ArrayDeclaration(itemTyp, nEle);
//    {%H-}exit;  //puede salir con error
  end else begin
    GenError(ER_IDE_TYP_EXP);
    exit(nil);
  end;
end;
procedure TCompiler_PIC16.CompileVarDeclar(IsInterface: boolean = false);
{Compila la declaración de variables en el nodo actual.
"IsInterface", indica el valor que se pondrá al as variables, en la bandera "IsInterface" }
var
  varNames: array of string;  //nombre de variables
  IsBit: Boolean;
  srcPosArray: TSrcPosArray;
  i: Integer;
  xvar: TxpEleVar;
  adicVarDec: TAdicVarDec;
  typEleDec: TxpEleType;
begin
  SetLength(varNames, 0);
  //Procesa variables a,b,c : int;
  getListOfIdent(varNames, srcPosArray);
  if HayError then begin  //precisa el error
    GenError(ER_EXP_VAR_IDE);
    exit;
  end;
  //usualmente debería seguir ":"
  if cIn.tok = ':' then begin
    //Debe seguir, el tipo de la variable
    cIn.Next;  //lo toma
    ProcComments;
    //Lee el tipo de la variable
    typEleDec := GetTypeVarDeclar;
    //Lee información aicional de la declaración (ABSOLUTE)
    adicVarDec := GetAdicVarDeclar(IsBit);
    if HayError then exit;
    if adicVarDec.isAbsol then begin  //valida tamaño
      //Es una declaración ABSOLUTE
      if typEleDec.IsBitSize and (not Isbit) then begin
        //Se esperaba un bit, en el ABSOLUTE.
        GenError(ER_INV_MEMADDR);
      end;
      if not typEleDec.IsBitSize and IsBit then begin
        {En realidad se podría aceptar posicionar un byte en una variable bit,
        posicionándolo en su byte contenedor.}
        GenError(ER_INV_MEMADDR);
      end;
    end;
    if HayError then exit;
    {Elimina la llamada agregada a la variable, porque se van a agregar llamadas más
    específicas desde la(s) varaible(s) declaradas.}
    if adicVarDec.absVar<>nil then begin
      adicVarDec.absVar.RemoveLastCaller;
    end;
    //Reserva espacio para las variables
    for i := 0 to high(varNames) do begin
      xvar := AddVariable(varNames[i], typEleDec, srcPosArray[i]);
      if HayError then break;        //Sale para ver otros errores
      xvar.adicPar := adicVarDec;    //Actualiza propiedades adicionales
      xvar.InInterface := IsInterface;  //Actualiza bandera
      {Técnicamente, no sería necesario, asignar RAM a la variable aquí (y así se
      optimizaría), porque este método, solo se ejecuta en la primera pasada, y no
      es vital tener las referencias a memoria, en esta pasada.
      Pero se incluye la ásignación de RAM, por:
      * Porque el acceso con directivas, a variables del sistema como "CurrBank",
      se hace en la primera pasada, y es necesario que estas variables sean válidas.
      * Para tener en la primera pasada, un código más similar al código final.}
      CreateVarInRAM(xvar);  //Crea la variable
      //Agrega la llamada, específica, desde esta variable.
      if adicVarDec.absVar<>nil then begin
        AddCallerTo(adicVarDec.absVar, xvar);
      end
    end;
  end else begin
    GenError(ER_SEM_COM_EXP);
    exit;
  end;
  if not CaptureDelExpres then exit;
  ProcComments;
  //puede salir con error
end;
procedure TCompiler_PIC16.CompileProcHeader(out fun: TxpEleFun; ValidateDup: boolean = true);
{Hace el procesamiento del encabezado de la declaración de una función/procedimiento.
Devuelve la referencia al objeto TxpEleFun creado, en "fun".
Conviene separar el procesamiento del enzabezado, para poder usar esta rutina, también,
en el procesamiento de unidades.}
var
  srcPos: TSrcPos;
  procName: String;
  typ   : TxpEleType;
begin
  //Toma información de ubicación, al inicio del procedimiento
  cIn.SkipWhites;
  srcPos := cIn.ReadSrcPos;
  //Ahora debe haber un identificador
  if cIn.tokType <> tnIdentif then begin
    GenError(ER_IDEN_EXPECT);
    exit;
  end;
  //hay un identificador
  procName := cIn.tok;
  cIn.Next;  //lo toma
  {Ya tiene los datos mínimos para crear la función. }
  fun := CreateFunction(procName, typNull, @callParam, @callFunct);
  fun.srcDec := srcPos;   //Toma ubicación en el código
  TreeElems.AddElementAndOpen(fun);  //Se abre un nuevo espacio de nombres

  CaptureDecParams(fun);
  if HayError then exit;
  //Recién aquí puede verificar duplicidad, porque ya se leyeron los parámetros
  if ValidateDup then begin   //Se pide validar la posible duplicidad de la función
    if not ValidateFunction then exit;
  end;
  //Verifica si es función
  cIn.SkipWhites;
  if cIn.tok = ':' then begin
    cIn.Next;
    cIn.SkipWhites;
    //Es función
    typ := getParamType;  //lee tipo
    if HayError then exit;

//    parType := cIn.tok;   //lee tipo de parámetro
//    cIn.Next;
//    //Valida el tipo
//    typ := FindSysEleType(parType);
//    if typ = nil then begin
//      GenError(ER_UNDEF_TYPE_, [parType]);
//      exit;
//    end;
    //Fija el tipo de la función
    fun.typ := typ;
  end;
  if not CaptureTok(';') then exit;
  //Verifica si es INTERRUPT
  cIn.SkipWhites;
  if cIn.tokL = 'interrupt' then begin
    cIn.Next;
    fun.IsInterrupt := true;
    if not CaptureTok(';') then exit;
  end;
  ProcComments;  //Quita espacios. Puede salir con error
end;
procedure TCompiler_PIC16.CompileProcDeclar(IsImplementation: boolean);
{Compila la declaración de procedimientos. Tanto procedimientos como funciones
 se manejan internamente como funciones.
 IsImplementation, se usa para cuando se está compilando en la sección IMPLEMENTATION.}
var
  fun, funcX: TxpEleFun;
  bod: TxpEleBody;
  Parent: TxpElement;
  i: Integer;
  Found: Boolean;
begin
  {Este método, solo se ejecutará en la primera pasada, en donde todos los procedimientos
  se codifican al inicio de la memoria FLASH, y las variables y registros se ubican al
  inicio de la memeoria RAM, ya que lo que importa es simplemente recabar información
  del procedimiento, y no tanto codificarlo. }
  ResetRAM;   //Limpia RAM y FLASH, y fija CurrBank
  if IsImplementation then begin
    //Se compila para implementación.
    {Este proceso es más complejo. La idea es compilar el enzabezado de cualquier función,
    y luego comparar para ver si corresponde a una implementación o no. Si es
    implemenatción, se elimina el nodo creado y se trabaja con el de la declaración.}
    CompileProcHeader(fun, false);  //No verifica la duplicidad por ahora
    if HayError then exit;
    //Verifica si es implementación de una función en la INTERFACE o no.
    Parent := TreeElems.curNode.Parent;  //Para comparar
    {Se supone que esta exploración solo se hará en la primera pasada, así que no hay
    problema, en hacer una exploración común.}
    //debugln('Buscando declaración de %s en nodo %s desde 0 hasta %d', [fun.name, Parent.name, Parent.elements.Count-2]);
    Found := false;
    for i:=0 to Parent.elements.Count-2 do begin  //No se considera a el mismo
      if not (Parent.elements[i].idClass = eltFunc) then continue;
      funcX := TxpEleFun(Parent.elements[i]);
      if (UpCase(funcX.name) = Upcase(fun.name)) and
         (fun.SameParams(funcX)) then begin
         Found := true;
         break;
      end;
    end;
    if Found then begin
      //Es una implementación. No vale la pena tener otro nodo.
      TreeElems.CloseElement;  //Cierra Nodo de la función
      Parent.elements.Remove(fun);   //elimina función
      fun := {%H-}funcX; //apunta a la nueva función
      TreeElems.OpenElement(fun);  //Abre el nodo anterior
      fun.Implemented := true;   //marca como implementada
    end else begin
      //Debe ser una función privada
    end;
  end else begin
    //Es una compilación normal
    CompileProcHeader(fun);  //Procesa el encabezado
    if HayError then exit;
  end;
  //Empiezan las declaraciones VAR, CONST, PROCEDURE, TYPE
  while StartOfSection do begin
    if cIn.tokL = 'var' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'begin') do begin
        CompileVarDeclar;
        if HayError then exit;;
      end;
    end else if cIn.tokL = 'const' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'begin') do begin
        CompileGlobalConstDeclar;
        if HayError then exit;;
      end;
//    end else if cIn.tokL = 'procedure' then begin
//      cIn.Next;    //lo toma
//      CompileProcDeclar;
    end else begin
      GenError('Expected VAR, CONST or BEGIN.');
      exit;
    end;
  end;
  if cIn.tokL <> 'begin' then begin
    GenError('Expected "begin", "var", "type" or "const".');
    exit;
  end;
  //Ahora empieza el cuerpo de la función o las declaraciones
  fun.adrr := pic.iRam;    //toma dirección de inicio del código. Es solo referencial.
  fun.posCtx := cIn.PosAct;  //Guarda posición para la segunda compilación
  bod := CreateBody;   //crea elemento del cuerpo de la función
  bod.srcDec := cIn.ReadSrcPos;
  TreeElems.AddElementAndOpen(bod);  //Abre nodo Body
  CompileProcBody(fun);
  TreeElems.CloseElement;  //Cierra Nodo Body
  TreeElems.CloseElement; //cierra espacio de nombres de la función
  bod.srcEnd := cIn.ReadSrcPos;  //Fin de cuerpo
//  fun.adrReturn := pic.iRam-1;  //Guarda dirección del i_RETURN
  if not CaptureTok(';') then exit;
  ProcComments;  //Quita espacios. Puede salir con error
end;
procedure TCompiler_PIC16.CompileInstruction;
{Compila una única instrucción o un bloque BEGIN ... END. Puede generar Error.
 Una instrucción se define como:
 1. Un bloque BEGIN ... END
 2. Una estrutura
 3. Una expresión
 La instrucción, no incluye al delimitador.
 }
var
  curCodCon: TxpEleCodeCont;
begin
  if TreeElems.curNode.idClass <> eltBody then begin
    //No debería pasar, porque las instrucciones solo pueden estar en eltBody
    GenError('Syntax error.');
    exit;
  end;
  curCodCon := TreeElems.CurCodeContainer;
  ProcComments;
  if cIn.tokL='begin' then begin
    //es bloque
    cIn.Next;  //toma "begin"
    CompileCurBlock;   //llamada recursiva
    if HayError then exit;
    if not CaptureStr('end') then exit;
    ProcComments;
    //puede salir con error
  end else begin
    //es una instrucción
    if cIn.tokType = tnStruct then begin
      if cIn.tokl = 'if' then begin
        curCodCon.OpenBlock(sbiIF);
        cIn.Next;         //pasa "if"
        CompileIF;
        curCodCon.CloseBlock;
      end else if cIn.tokl = 'while' then begin
        curCodCon.OpenBlock(sbiWHILE);
        cIn.Next;         //pasa "while"
        CompileWHILE;
        curCodCon.CloseBlock;
      end else if cIn.tokl = 'repeat' then begin
        curCodCon.OpenBlock(sbiREPEAT);
        cIn.Next;         //pasa "until"
        CompileREPEAT;
        curCodCon.CloseBlock;
      end else if cIn.tokl = 'for' then begin
        curCodCon.OpenBlock(sbiFOR);
        cIn.Next;         //pasa "until"
        CompileFOR;
        curCodCon.CloseBlock;
      end else begin
        GenError(ER_UNKN_STRUCT);
        exit;
      end;
    end else begin
      //debe ser es una expresión
      GetExpressionE(0);
    end;
    if HayError then exit;
    if pic.MsjError<>'' then begin
      //El pic también puede dar error
      GenError(pic.MsjError);
    end;
  end;
end;
procedure TCompiler_PIC16.CompileInstructionDummy;
{Compila una instrucción pero sin generar código. }
var
  p, InvertedFromZ0: Integer;
  InvertedFromC0: Integer;
begin
  p := pic.iRam;
  InvertedFromC0 := BooleanFromC; //Guarda estado
  InvertedFromZ0 := BooleanFromZ; //Guarda estado

  CompileInstruction;  //Compila solo para mantener la sintaxis

  BooleanFromC := InvertedFromC0; //Restaura
  BooleanFromZ := InvertedFromZ0; //Restaura
  pic.iRam := p;     //Elimina lo compilado
  //puede salir con error
  { TODO : Debe limpiar la memoria flash que ocupó, para dejar la casa limpia. }
end;
procedure TCompiler_PIC16.CompileCurBlock;
{Compila el bloque de código actual hasta encontrar un delimitador de bloque, o fin
de archivo. }
begin
  ProcComments;
  while not cIn.Eof and (cIn.tokType<>tnBlkDelim) do begin
    //se espera una expresión o estructura
    CompileInstruction;
    if HayError then exit;   //aborta
    //se espera delimitador
    if cIn.Eof then break;  //sale por fin de archivo
    //busca delimitador
    ProcComments;
    //Puede terminar con un delimitador de bloque
    if cIn.tokType=tnBlkDelim then break;
    //Pero lo común es que haya un delimitador de expresión
    if not CaptureTok(';') then exit;
    ProcComments;  //Puede haber Directivas o ASM también
  end;
end ;
procedure TCompiler_PIC16.CompileCurBlockDummy;
{Compila un bloque pero sin geenrar código.}
var
  p: Integer;
  InvertedFromC0, InvertedFromZ0: Integer;
begin
  p := pic.iRam;
  InvertedFromC0 := BooleanFromC; //Guarda estado
  InvertedFromZ0 := BooleanFromZ; //Guarda estado

  CompileCurBlock;  //Compila solo para mantener la sintaxis

  BooleanFromC := InvertedFromC0; //Restaura
  BooleanFromZ := InvertedFromZ0; //Restaura
  pic.iRam := p;     //Elimina lo compilado
  //puede salir con error
  { TODO : Debe limpiar la memoria flash que ocupó, para dejar la casa limpia. }
end;
procedure TCompiler_PIC16.CompileUnit(uni: TxpElement);
{Realiza la compilación de una unidad}
var
  fun: TxpEleFun;
  elem: TxpElement;
begin
//debugln('   Ini Unit: %s-%s',[TreeElems.curNode.name, ExtractFIleName(cIn.curCon.arc)]);
  ClearError;
  pic.MsjError := '';
  ProcComments;
  //Busca UNIT
  if cIn.tokL = 'unit' then begin
    cIn.Next;  //pasa al nombre
    ProcComments;
    if cIn.Eof then begin
      GenError('Name of unit expected.');
      exit;
    end;
    if UpCase(cIn.tok)<>UpCase(uni.name) then begin
      GenError('Name of unit doesn''t match file name.');
      exit;
    end;
    cIn.Next;  //Toma el nombre y pasa al siguiente
    if not CaptureDelExpres then exit;
  end else begin
    GenError('Expected: UNIT');
    exit;
  end;
  ProcComments;
  if cIn.tokL <> 'interface' then begin
    GenError('Expected: INTERFACE');
    exit;
  end;
  cIn.Next;   //toma
  ProcComments;
  if cIn.Eof then begin
    GenError('Expected "uses", "var", "type", "const" or "implementation".');
    exit;
  end;
  ProcComments;
  //Busca USES
  CompileUsesDeclaration;
  if cIn.Eof then begin
    GenError('Expected "var", "type" or "const".');
    exit;
  end;
  ProcComments;
//  Cod_StartProgram;  //Se pone antes de codificar procedimientos y funciones
  if HayError then exit;
  //Empiezan las declaraciones
  while StartOfSection do begin
    if cIn.tokL = 'var' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'implementation') do begin
        CompileVarDeclar(true);  //marca como "IsInterface"
        if HayError then exit;;
      end;
    end else if cIn.tokL = 'type' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'implementation') do begin
        CompileTypeDeclar(true);
        if HayError then exit;
      end;
    end else if cIn.tokL = 'const' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'implementation') do begin
        CompileGlobalConstDeclar;
        if HayError then exit;;
      end;
    end else if cIn.tokL = 'procedure' then begin
      cIn.Next;    //lo toma
      CompileProcHeader(fun);   //Se ingresa al árbol de sintaxis
      if HayError then exit;
      fun.InInterface := true;  //marca ubicación
      TreeElems.CloseElement;   //CompileProcHeader, deja abierto el elemento
    end else begin
      GenError(ER_NOT_IMPLEM_, [cIn.tok]);
      exit;
    end;
  end;
  ProcComments;
  if cIn.tokL <> 'implementation' then begin
    GenError('Expected: IMPLEMENTATION');
    exit;
  end;
  cIn.Next;   //toma
  /////////////////  IMPLEMENTATION /////////////////////
  ProcComments;
  //Explora las declaraciones e implementaciones
  //Empiezan las declaraciones
  while StartOfSection do begin
    if cIn.tokL = 'var' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'end') do begin
        CompileVarDeclar;
        if HayError then exit;;
      end;
    end else if cIn.tokL = 'const' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'end') do begin
        CompileGlobalConstDeclar;
        if HayError then exit;;
      end;
    end else if cIn.tokL = 'procedure' then begin
      cIn.Next;    //lo toma
      CompileProcDeclar(true);  //Compila en IMPLEMENTATION
      if HayError then exit;
    end else begin
      GenError(ER_NOT_IMPLEM_, [cIn.tok]);
      exit;
    end;
  end;
  //Verifica si todas las funciones de INTERFACE, se implementaron
  for elem in TreeElems.curNode.elements do if elem.idClass = eltFunc then begin
    fun := TxpEleFun(elem);
    if fun.InInterface and not fun.Implemented then begin
      GenErrorPos('Function %s not implemented.', [fun.name], fun.srcDec);
      exit;
    end;
  end;
  CompileLastEnd;
  if HayError then exit;
//  //procesa cuerpo
//  ResetRAM;  {No es tan necesario, pero para seguir un orden y tener limpio
//                     también, la flash y memoria, después de algún psoible procedimiento.}
//  if cIn.tokL = 'begin' then begin
//    bod := CreateBody;
//    bod.srcDec := cIn.ReadSrcPos;
//    cIn.Next;   //coge "begin"
//    //Guardamos la ubicación física, real, en el archivo, después del BEGIN
//    bod.posCtx := cIn.PosAct;
//    //codifica el contenido
//    CompileCurBlock;   //compila el cuerpo
//    if HayError then exit;

//    _SLEEP();   //agrega instrucción final
//  end else begin
//    GenError('Expected "begin", "var", "type" or "const".');
//    exit;
//  end;
//  Cod_EndProgram;
//debugln('   Fin Unit: %s-%s',[TreeElems.curNode.name, ExtractFIleName(cIn.curCon.arc)]);
end;
procedure TCompiler_PIC16.CompileUsesDeclaration;
{Compila la unidad indicada.}
var
  uni: TxpEleUnit;
  uPath: String;
  uName: String;
  p: TPosCont;
begin
  if cIn.tokL = 'uses' then begin
    cIn.Next;  //pasa al nombre
    //Toma una a una las unidades
    repeat
      ProcComments;
      //ahora debe haber un identificador
      if cIn.tokType <> tnIdentif then begin
        GenError(ER_IDEN_EXPECT);
        exit;
      end;
      //hay un identificador de unidad
      uName := cIn.tok;
      uni := CreateUnit(uName);
      //Verifica si existe ya el nombre de la unidad
      if uni.DuplicateIn(TreeElems.curNode.elements) then begin
        GenError('Identifier duplicated: %s.', [uName]);
        uni.Destroy;
        exit;
      end;
      uni.srcDec := cIn.ReadSrcPos;   //guarda posición de declaración
      uName := uName + '.pas';  //nombre de archivo
{----}TreeElems.AddElementAndOpen(uni);
      //Ubica al archivo de la unidad
      p := cIn.PosAct;   //Se debe guardar la posición antes de abrir otro contexto
      //Primero busca en la misma ubicación del archivo fuente
      uPath := ExtractFileDir(mainFile) + DirectorySeparator + uName;
      if OpenContextFrom(uPath) then begin
        uni.srcFile := uPath;   //Gaurda el archivo fuente
      end else begin
        //No lo encontró, busca en la carpeta de dispositivos
        uPath := devicesPath + DirectorySeparator + uName;
        if OpenContextFrom(uPath) then begin
          uni.srcFile := uPath;   //Gaurda el archivo fuente
        end else begin
           //No lo encontró, busca en la carpeta de librerías
           uPath := patUnits + DirectorySeparator + uName;
           if OpenContextFrom(uPath) then begin
             uni.srcFile := uPath;   //Gaurda el archivo fuente
           end else begin
             //No lo encuentra
             GenError(ER_FIL_NOFOUND, [uName]);
             exit;
           end;
        end;
      end;
      //Aquí ya se puede realizar otra exploración, como si fuera el archivo principal
      CompileUnit(uni);
      cIn.PosAct := p;
      if HayError then exit;  //El error debe haber guardado la ubicaicón del error
{----}TreeElems.CloseElement; //cierra espacio de nombres de la función
      cIn.Next;  //toma nombre
      cIn.SkipWhites;
      if cIn.tok <> ',' then break; //sale
      cIn.Next;  //toma la coma
    until false;
    if not CaptureDelExpres then exit;
  end;
end;
procedure TCompiler_PIC16.CompileProgram;
{Compila un programa en el contexto actual. Empieza a codificar el código a partir de
la posición actual de memoria en el PIC (iRam).}
var
  bod: TxpEleBody;
begin
  ClearError;
  pic.MsjError := '';
  ProcComments;
  //Busca PROGRAM
  if cIn.tokL = 'unit' then begin
    //Se intenta compilar una unidad
    GenError('Expected a program. No a unit.');
    exit;
  end;
  if cIn.tokL = 'program' then begin
    cIn.Next;  //pasa al nombre
    ProcComments;
    if cIn.Eof then begin
      GenError(ER_PROG_NAM_EX);
      exit;
    end;
    cIn.Next;  //Toma el nombre y pasa al siguiente
    if not CaptureDelExpres then exit;
  end;
  if cIn.Eof then begin
    GenError('Expected "program", "begin", "var", "type" or "const".');
    exit;
  end;
  ProcComments;
  //Busca USES
  if HayError then exit;  //CompileUsesDeclaration, va a limpiar "HayError"
  CompileUsesDeclaration;
  if cIn.Eof then begin
    GenError('Expected "begin", "var", "type" or "const".');
    exit;
  end;
  ProcComments;
  Cod_StartProgram;  //Se pone antes de codificar procedimientos y funciones
  if HayError then exit;
  //Empiezan las declaraciones
  while StartOfSection do begin
    if cIn.tokL = 'var' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'begin') do begin
        CompileVarDeclar;
        if HayError then exit;
      end;
    end else if cIn.tokL = 'type' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'begin') do begin
        CompileTypeDeclar(false);
        if HayError then exit;
      end;
    end else if cIn.tokL = 'const' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'begin') do begin
        CompileGlobalConstDeclar;
        if HayError then exit;
      end;
    end else if cIn.tokL = 'procedure' then begin
      cIn.Next;    //lo toma
      CompileProcDeclar(false);
      if HayError then exit;
    end else begin
      GenError(ER_NOT_IMPLEM_, [cIn.tok]);
      exit;
    end;
  end;
  //procesa cuerpo
  ResetRAM;  {No es tan necesario, pero para seguir un orden y tener limpio
                     también, la flash y memoria, después de algún posible procedimiento.}
  if cIn.tokL <> 'begin' then begin
    GenError('Expected "begin", "var", "type" or "const".');
    exit;
  end;
  bod := CreateBody;
  bod.srcDec := cIn.ReadSrcPos;
  TreeElems.AddElementAndOpen(bod);  //Abre nodo Body
  cIn.Next;   //coge "begin"
  //Guardamos popsisicón en contexto para la segunda compilación
  bod.posCtx := cIn.PosAct;
  //codifica el contenido
  CompileCurBlock;   //compila el cuerpo
  TreeElems.CloseElement;   //No debería ser tan necesario.
  bod.srcEnd := cIn.ReadSrcPos;
  if HayError then exit;
  CompileLastEnd;  //Compila el "END." final
  if HayError then exit;
  //_RTS();   //agrega instrucción final
  Cod_EndProgram;
end;
procedure TCompiler_PIC16.CompileLinkProgram;
{Genera el código compilado final. Usa la información del árbol de sintaxis, para
ubicar a los diversos elementos que deben compilarse.
Se debe llamar después de compilar con CompileProgram.
Esto es lo más cercano a un enlazador, que hay en PicPas.}
  procedure UpdateFunLstCalled;
  {Actualiza la lista lstCalled de las funciones, para saber, a qué fúnciones llama
   cada función.}
  var
    fun    : TxpEleFun;
    itCall : TxpEleCaller;
    whoCalls: TxpEleBody;
  begin
    for fun in TreeElems.AllFuncs do begin
      if fun.nCalled = 0 then continue;  //No usada
      //Procesa las llamadas hechas desde otras funciones, para llenar
      //su lista "lstCalled", y así saber a quienes llama
      for itCall in fun.lstCallers do begin
        //Agrega la referencia de la llamada a la función
        if itCall.caller.idClass <> eltBody then begin
          //Según diseño, itCall.caller debe ser TxpEleBody
          GenError('Design error.');
          exit;
        end;
        whoCalls := TxpEleBody(itCall.caller);
        //Se agregan todas las llamadas (así sean al mismo porcedimiento) pero luego
        //AddCalled(), los filtra.
        whoCalls.Parent.AddCalled(fun);  //Agrega al procediminto
      end;
    end;
    {Actualizar la lista fun.lstCalledAll con la totalidad de llamadas a todas
     las funciones, sean de forma directa o indirectamente.}
    for fun in TreeElems.AllFuncs do begin
      fun.UpdateCalledAll;
    end;
    //Actualiza el programa principal
    TreeElems.main.UpdateCalledAll;
    if TreeElems.main.maxNesting>8 then begin
      GenError('Not enough stack.');
    end;
  end;
  procedure AssignRAMtoVar(xvar: TxpEleVar; shared: boolean = false);
  var
    posAct : TPosCont;
    adicVarDec: TAdicVarDec;
    IsBit  : boolean;
  begin
//debugln('  Asignando espacio a %s', [xvar.name]);
    if xvar.adicPar.isAbsol then begin
//debugln('Abs: xvar=%s at %d', [xvar.name, xvar.adicPar.absAddr]);
      {Tiene declaración absoluta. Mejor compilamos de nuevo la declaración, porque
      puede haber referencia a variables que han cambiado de ubicación, por
      optimización.
      Se podría hacer una verificación, para saber si la referencia es a direcciones
      absolutas, en lugar de a variables (o a varaibles temporales), y así evitar
      tener que compilar de nuevo, la declaración.}
      posAct := cIn.PosAct;   //guarda posición actual
      cIn.PosAct := xVar.adicPar.srcDec;  //Posiciona en la declaración adicional
      TreeElems.curNode := xvar.Parent;   {Posiciona el árbol, tal cual estaría en la
                                           primera pasada, para una correcta resolución
                                           de nombres}
      adicVarDec := GetAdicVarDeclar(IsBit);
      //No debería dar error, porque ya pasó la primera pasada
      xvar.adicPar := adicVarDec;
      cIn.PosAct := posAct;
    end;
    CreateVarInRAM(xVar, shared);  //Crea la variable
    xvar.typ.DefineRegister;  //Asegura que se dispondrá de los RT necesarios
    //Puede salir error
  end;
var
  elem   : TxpElement;
  bod    : TxpEleBody;
  xvar   : TxpEleVar;
  fun    : TxpEleFun;
  iniMain: integer;
begin
  ExprLevel := 0;
  ResetRAM;
  ClearError;
  pic.MsjError := '';
  //Verifica las constantes usadas. Solo en el nodo principal, para no sobrecargar mensajes.
  for elem in TreeElems.main.elements do if elem.idClass = eltCons then begin
    if elem.nCalled = 0 then begin
      GenWarnPos(WA_UNUSED_CON_, [elem.name], elem.srcDec);
    end;
  end;
  //Explora las funciones, para identifcar a las no usadas
  RefreshAllElementLists;
  RemoveUnusedFunc;  //Se debe empezar con las funciones
  RemoveUnusedVars;  //Luego las variables
  RemoveUnusedCons;
  RemoveUnusedTypes;
  //Inicio de generación de código.
  pic.iRam := GeneralORG;  //inicia puntero a RAM
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
  _JMP_lbl(iniMain);   //Salto hasta después del espacio de variables
  ///////////////////////////////////////////////////////////////////////////////
  //Asigna memoria, primero a las variables locales (y parámetros) de las funciones
  ///////////////////////////////////////////////////////////////////////////////
  UpdateFunLstCalled;   //Actualiza lista "lstCalled" de las funciones usadas
  if HayError then exit;
  //Explora primero a las funciones terminales
  for fun in TreeElems.AllFuncs do if fun.nCalled > 0 then begin
    if not fun.IsTerminal2 then continue;
    //DebugLn('función terminal: %s con %d var.loc.', [fun.name, fun.nLocalVars]);
    //Los parámetros y variables locales aparecen como elementos de la función
    for elem in fun.elements do if elem.idClass = eltVar then begin
      xvar := TxpEleVar(elem);
      if xvar.nCalled>0 then begin
        //Asigna una dirección válida para esta variable
        AssignRAMtoVar(xvar, true);
        if HayError then exit;
      end;
    end;
    if OptReuProVar then pic.SetSharedUnused;   //limpia las posiciones usadas
  end;
  if OptReuProVar then pic.SetSharedUsed;  //Ahora marca como usados, porque ya se creó la zona de bytes comaprtidos
  //Explora solo a las funciones que no son terminales
  for fun in TreeElems.AllFuncs do if fun.nCalled > 0 then begin
    if fun.IsTerminal2 then continue;
    //Los parámetros y variables locales aparecen como elementos de la función
    for elem in fun.elements do if elem.idClass = eltVar then begin
      xvar := TxpEleVar(elem);
      if xvar.nCalled>0 then begin
        //Asigna una dirección válida para esta variable
        AssignRAMtoVar(xvar);
        if HayError then exit;
      end;
    end;
  end;
  ///////////////////////////////////////////////////////////////////////////////
  //Reserva espacio para las variables (Que no son de funciones).
  for xvar in TreeElems.AllVars do begin
    if xvar.Parent.idClass = eltFunc then continue;  //Las variables de funciones ya se crearon
    if xvar.nCalled>0 then begin
      //Asigna una dirección válida para esta variable
      AssignRAMtoVar(xvar);
      if HayError then exit;
    end else begin
      //Variable no usada
      xvar.ResetAddress;
      if xvar.Parent = TreeElems.main then begin
        //Genera mensaje solo para variables del programa principal.
        GenWarnPos(WA_UNUSED_VAR_, [xVar.name], xvar.srcDec);
      end;
    end;
  end;
  //Codifica la función INTERRUPT, si existe
  for fun in TreeElems.AllFuncs do begin
      if fun.IsInterrupt then begin
        //Compila la función en la dirección 0x04
        pic.iRam := $04;
        fun.adrr := pic.iRam;    //Actualiza la dirección final
        fun.typ.DefineRegister;    //Asegura que se dispondrá de los RT necesarios
        cIn.PosAct := fun.posCtx;  //Posiciona escáner
        PutLabel('__'+fun.name);
        TreeElems.OpenElement(fun.BodyNode); //Ubica el espacio de nombres, de forma similar a la pre-compilación
        CompileProcBody(fun);
        TreeElems.CloseElement;  //cierra el body
        TreeElems.CloseElement;  //cierra la función
        if HayError then exit;     //Puede haber error
      end;
  end;
  //Codifica las funciones del sistema usadas
  for fun in listFunSys do begin
    if (fun.nCalled > 0) and (fun.compile<>nil) then begin
      //Función usada y que tiene una subrutina ASM
      fun.adrr := pic.iRam;  //actualiza la dirección final
      PutLabel('__'+fun.name);
      fun.compile(fun);   //codifica
      if HayError then exit;  //Puede haber error
      if pic.MsjError<>'' then begin //Error en el mismo PIC
          GenError(pic.MsjError);
          exit;
      end;
    end;
  end;
  //Codifica las subrutinas usadas
  for fun in TreeElems.AllFuncs do begin
    if fun.IsInterrupt then continue;
    if fun.nCalled>0 then begin
      //Compila la función en la dirección actual
      fun.adrr := pic.iRam;    //Actualiza la dirección final
      fun.typ.DefineRegister;    //Asegura que se dispondrá de los RT necesarios
      cIn.PosAct := fun.posCtx;  //Posiciona escáner
      PutLabel('__'+fun.name);
      TreeElems.OpenElement(fun.BodyNode); //Ubica el espacio de nombres, de forma similar a la pre-compilación
      CompileProcBody(fun);
      TreeElems.CloseElement;  //cierra el body
      TreeElems.CloseElement;  //cierra la función
      if HayError then exit;     //Puede haber error
    end else begin
      //Esta función no se usa.
      GenWarnPos(WA_UNUSED_PRO_, [fun.name], fun.srcDec);
    end;
  end;
  //Compila cuerpo del programa principal
  _LABEL(iniMain);   //Termina de codificar el salto
  bod := TreeElems.BodyNode;  //lee Nodo del cuerpo principal
  if bod = nil then begin
    GenError('Body program not found.');
    exit;
  end;
  bod.adrr := pic.iRam;  //guarda la dirección de codificación
//  bod.nCalled := 1;        //actualiza
  cIn.PosAct := bod.posCtx;   //ubica escaner
  PutLabel('__main_program__');
  TreeElems.OpenElement(bod);
  CompileCurBlock;
  TreeElems.CloseElement;   //cierra el cuerpo principal
  PutLabel('__end_program__');
  {No es necesario hacer más validaciones, porque ya se hicieron en la primera pasada}
  //_RTS();   //agrega instrucción final
  if pic.MsjError<>'' then begin //Puede ser error al escribir la última instrucción
      GenError(pic.MsjError);
      exit;
  end;
end;
function TCompiler_PIC16.IsUnit: boolean;
{Indica si el archivo del contexto actual, es una unidad. Debe llamarse}
begin
  ProcCommentsNoExec;  //Solo es validación, así que no debe ejecutar nada
  //Busca UNIT
  if cIn.tokL = 'unit' then begin
    cIn.curCon.SetStartPos;   //retorna al inicio
    exit(true);
  end;
  cIn.curCon.SetStartPos;   //retorna al inicio
  exit(false);
end;
procedure TCompiler_PIC16.Compile(NombArc: string; Link: boolean);
//Compila el contenido de un archivo.
var
  p: SizeInt;
begin
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
    cIn.ClearAll;       //elimina todos los Contextos de entrada
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
    TreeElems.main.srcDec.fil := mainFile;
    //Continúa con preparación
    TreeDirec.Clear;
    TreeElems.OnAddElement := @Tree_AddElement;   //Se va a modificar el árbol
    listFunSys.Clear;
    CreateSystemElements;  //Crea los elementos del sistema
    ClearMacros;           //Limpia las macros
    //Inicia PIC
    ExprLevel := 0;  //inicia
    ResetRAM;  //Inicia la memoria RAM
    //Compila el archivo actual como programa o como unidad
    if IsUnit then begin
      CompiledUnit := true;
      //Hay que compilar una unidad
      consoleTickStart;
//      debugln('*** Compiling unit: Pass 1.');
      FirstPass := true;
      CompileUnit(TreeElems.main);
      UpdateCallersToUnits;
      consoleTickCount('** First Pass.');
    end else begin
      //Debe ser un programa
      CompiledUnit := false;
      {Se hace una primera pasada para ver, a modo de exploración, para ver qué
      procedimientos, y variables son realmente usados, de modo que solo estos, serán
      codificados en la segunda pasada. Así evitamos incluir, código innecesario.}
      consoleTickStart;
//      debugln('*** Compiling program: Pass 1.');
      pic.iRam := 0;     //dirección de inicio del código principal
      FirstPass := true;
      CompileProgram;  //puede dar error
      if HayError then exit;
      UpdateCallersToUnits;
      consoleTickCount('** First Pass.');
      if Link then begin  //El enlazado solo es válido para programas
        {Compila solo los procedimientos usados, leyendo la información del árbol de sintaxis,
        que debe haber sido actualizado en la primera pasada.}
        FirstPass := false;
        CompileLinkProgram;
        consoleTickCount('** Second Pass.');
        //Genera archivo hexa, en la misma ruta del programa
        pic.GenHex(hexFile);  //CONFIG_NULL;
      end;
    end;
    {-------------------------------------------------}
    cIn.ClearAll;//es necesario por dejar limpio
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
  adStr: String;
  v: TxpEleVar;
  nam, subUsed: string;
  reg: TPicRegister;
begin
  for v in TreeElems.AllVars do begin   //Se supone que "AllVars" ya se actualizó.
      if ExcUnused and (v.nCalled = 0) then continue;
      if v.nCalled = 0 then subUsed := '; <Unused>' else subUsed := '';
      if v.typ.IsBitSize then begin
        lins.Add('#define ' + v.name + ' ' + AdrStr(v.addr) + ',' +
                                             IntToStr(v.adrBit.bit)+ subUsed);
      end else if v.typ.IsByteSize then begin
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
  //Reporte de registros de trabajo, auxiliares y de pila
  if (listRegAux.Count>0) or (listRegAuxBit.Count>0) then begin
    lins.Add(';------ Work and Aux. Registers ------');
    for reg in listRegAux do begin
      if not reg.assigned then continue;  //puede haber registros de trabajo no asignados
      nam := pic.NameRAM(reg.addr); //debería tener nombre
      adStr := '0x' + IntToHex(reg.addr, 3);
      lins.Add(nam + ' EQU ' +  adStr);
    end;
  end;
  if (listRegStk.Count>0) or (listRegStkBit.Count>0) then begin
    lins.Add(';------ Stack Registers ------');
    for reg in listRegStk do begin
      nam := pic.NameRAM(reg.addr); //debería tener nombre
      adStr := '0x' + IntToHex(reg.addr, 3);
      lins.Add(nam + ' EQU ' +  adStr);
    end;
  end;
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
      if pic.ram[i].used then begin
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
  ramUse := usedRAM/ totRAM;
  //Calcula STACK
  TreeElems.main.UpdateCalledAll;   //Debe haberse llenado TreeElems.main.lstCalled
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

  fun: TxpEleFun;
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
    if pic.ram[i].used then begin
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
                fun.srcDec.RowColString + ':' + fun.srcDec.fil
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
  cIn.OnNewLine:=@cInNewLine;
  mode := modPicPas;   //Por defecto en sintaxis nueva
  StartSyntax;   //Debe hacerse solo una vez al inicio
  DefCompiler;   //Debe hacerse solo una vez al inicio
end;
destructor TCompiler_PIC16.Destroy;
begin
  inherited Destroy;
end;

initialization

finalization

end.
//2161
