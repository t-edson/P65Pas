{Unidad con rutinas del analizador sintáctico.
}
unit Compiler_PIC16;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, lclProc, SynEditHighlighter, types, MisUtils, XpresBas,
  XpresTypesPIC, XpresElementsPIC, P6502utils, CPUCore, CompBase, ParserDirec,
  GenCodBas_PIC16, GenCod_PIC16, ParserDirec_PIC16, Globales, FormConfig,
  CompOperands {Por diseño, FormConfig, no debería accederse desde aquí};
type
  { TCompiler_PIC16 }
  TCompiler_PIC16 = class(TParserDirec)
  private   //Funciones básicas
    procedure CompileInlineBody(fun: TxpEleInlin);
    procedure cInNewLine(lin: string);
    procedure ResetRAM;

  private   //Rutinas para la compilación y enlace
    procedure CompileProcBody(fun: TxpEleFun);
  private //Compilación de secciones

    procedure CompileProcDeclar;
    procedure CompileInlineDeclar(elemLocat: TxpEleLocation);
    procedure CompileUnit(uni: TxpElement);
    procedure CompileUsesDeclaration;
    procedure CompileProgram;
    procedure CompileLinkProgram;
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
  ER_PROG_NAM_EX, ER_COMPIL_PROC, ER_CON_EXP_EXP: String;
  ER_FIL_NOFOUND, WA_UNUSED_CON_, WA_UNUSED_VAR_,WA_UNUSED_PRO_: String;
  MSG_RAM_USED, MSG_FLS_USED: String;
//Funciones básicas
procedure SetLanguage;
begin
  ParserDirec_PIC16.SetLanguage;
  {$I ..\language\tra_Compiler.pas}
end;
procedure TCompiler_PIC16.ResetRAM;
{Reinicia el dispositivo, para empezar a escribir en la posición $000 de la FLASH, y
en la posición inicial de la RAM.}
begin
  pic.iRam := 0;  //Ubica puntero al inicio.
  pic.ClearMemRAM;  //Pone las celdas como no usadas y elimina nombres.
  StartRegs;        //Limpia registros de trabajo, auxiliares, y de pila.
end;
procedure TCompiler_PIC16.cInNewLine(lin: string);
//Se pasa a una nueva _Línea en el contexto de entrada
begin
  if Config.IncComment then begin
    pic.addTopComm('    ;'+trim(lin));  //agrega _Línea al código ensmblador
  end;
end;

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
procedure TCompiler_PIC16.CompileInlineBody(fun: TxpEleInlin);
{Compila el cuerpo de un procedimiento INLINE.}
begin
  //Faltaría revisar la compilación para que se adecúe a la de un proc. Inline
  CompileInstructionDummy;
end;
//Compilación de secciones

procedure TCompiler_PIC16.CompileProcDeclar;
{Compila la declaración de procedimientos. Tanto procedimientos como funciones
 se manejan internamente como funciones.
 IsImplementation, se usa para cuando se está compilando en la sección IMPLEMENTATION.}
var
  fun: TxpEleFun;
  bod: TxpEleBody;
  ele : TxpElement;
  Found, IsInterrupt: Boolean;
  ParentElems: TxpElements;
  procName, uname: String;
  retType: TxpEleType;
  srcPos: TSrcPos;
  pars: TxpParFuncArray;
begin
  {Este método, solo se ejecutará en la primera pasada, en donde todos los procedimientos
  se codifican al inicio de la memoria, y las variables y registros se ubican al
  inicio de la memoria RAM, ya que lo que importa es simplemente recabar información
  del procedimiento, y no tanto codificarlo. }
  ResetRAM;   //Limpia RAM y FLASH, y fija CurrBank
  case curLocation of
  locInterface: begin
    //Los procedimientos en INTERFACE, no se procesan aquí. Se procesan en CompileUnit().
  end;
  locImplement:  begin
    //Se compila para implementación.
    {Este proceso es más complejo. La idea es compilar el enzabezado de cualquier función,
    y luego comparar para ver si corresponde a una implementación o no. Si es
    implementación, se elimina el nodo creado y se trabaja con el de la declaración.}
    ReadProcHeader(procName, retType, srcPos, pars, IsInterrupt);
    if HayError then exit;
    //Verifica si es implementación de una función en la INTERFACE o no.
    ParentElems := TreeElems.curNode.elements;  //Para comparar
    {Se supone que esta exploración solo se hará en la primera pasada, así que no hay
    problema, en hacer una exploración común.}
    //debugln('Buscando declaración de %s en nodo %s desde 0 hasta %d', [fun.name, ParentElems.name, ParentElems.elements.Count-2]);
    Found := false;
    uname := upcase(procName);
    for ele in ParentElems do begin
      if ele.location = locInterface then begin
        //Es elemento de INTERFACE
        if ele.uname = uname then begin
          //Hay coincidencia de nombre
          if ele.idClass = eltFunc then begin
            //Para las funciones, se debe comparar los parámetros
            fun := TxpEleFun(ele);
            if fun.SameParamsType(pars) then begin
              Found := true;
              break;
            end;
          end else begin
            //Si tiene el mismo nombre que cualquier otro elemento, es conflicto
            GenError('Identifier "%s" already defined', [uname]);
            exit;
          end;
        end;
      end else begin
        {Debe ser elemento de IMPLEMENTATION, no hay otra opción porque se supone que
        estamos en la sección de IMPLEMENTATION, así que el Parent, debe ser una unidad.}
        GenErrorPos(ER_DUPLIC_FUNC_,[procName], srcPos);  //Está duplicada en IMPLEMENTATION
        exit;
      end;
    end;
    if Found then begin
      //Es una implementación. No vale la pena tener otro nodo.
      TreeElems.OpenElement(fun);  //Abre el nodo anterior
      fun.Implemented := true;   //marca como implementada
    end else begin
      //Debe ser una función privada. No declarada en Interface.
      //La creamos con seguridad porque ya verificamos que no hay conflicto en IMPLEMENTATION.
      fun := AddFunction(procName, retType, srcPos, pars, IsInterrupt, @callParam, @callFunct);
      //Un caso especial de proced. declarado solo en IMPLEMENTATION.
      fun.location := locImplement;
    end;
  end;
  locMain: begin
    //Es una compilación en el programa principal. ¿Y si es FORWARD?
    ReadProcHeader(procName, retType, srcPos, pars, IsInterrupt);
    if HayError then exit;
    if TreeElems.FunctionExistInCur(procName, pars) then begin
      GenErrorPos(ER_DUPLIC_FUNC_,[procName], srcPos);
      exit;
    end;
    fun := AddFunction(procName, retType, srcPos, pars, IsInterrupt, @callParam, @callFunct);
    //Aquí estamos en el entorno de la función.
    fun.location := locMain;
  end;
  else
    GenError(ER_NOT_IMPLEM_, ['locMain in TCompiler_PIC16.CompileProcDeclar()']);
  end;
  //Aquí ya se tiene "fun" abierta, validada y apuntando a la declaración.
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
procedure TCompiler_PIC16.CompileInlineDeclar(elemLocat: TxpEleLocation);
{Compila la declaración de procedimientos INLINE. Tanto procedimientos como funciones
 INLINE se manejan internamente como funciones.
 IsImplementation, se usa para cuando se está compilando en la sección IMPLEMENTATION.}
var
  fun: TxpEleInlin;
  bod: TxpEleBody;
  ParentElems: TxpElements;
  ele : TxpElement;
  Found: Boolean;
  procName, uname: String;
  retType: TxpEleType;
  srcPos: TSrcPos;
  pars: TxpParInlinArray;
begin
  {Este método, solo se ejecutará en la primera pasada, en donde todos los procedimientos
  se codifican al inicio de la memoria, y las variables y registros se ubican al
  inicio de la memoria RAM, ya que lo que importa es simplemente recabar información
  del procedimiento, y no tanto codificarlo. }
  ResetRAM;   //Limpia RAM y FLASH, y fija CurrBank
  case elemLocat of
  locInterface: begin
    //Los procedimientos en INTERFACE, no se procesan aquí. Se procesan en CompileUnit().
  end;
  locImplement:  begin
    //Se compila para implementación.
    {Este proceso es más complejo. La idea es compilar el encabezado de cualquier función,
    y luego comparar para ver si corresponde a una implementación o no. Si es
    implementación, se elimina el nodo creado y se trabaja con el de la declaración.}
    ReadInlineHeader(procName, retType, srcPos, pars);
    if HayError then exit;
    //Verifica si es implementación de una función en la INTERFACE o no.
    ParentElems := TreeElems.curNode.elements;  //Para comparar
    {Se supone que esta exploración solo se hará en la primera pasada, así que no hay
    problema, en hacer una exploración común.}
    //debugln('Buscando declaración de %s en nodo %s desde 0 hasta %d', [fun.name, ParentElems.name, ParentElems.elements.Count-2]);
    Found := false;
    uname := upcase(procName);
    for ele in ParentElems do begin
      if ele.location = locInterface then begin
        //Es elemento de INTERFACE
        if ele.uname = uname then begin
          //Hay coincidencia de nombre
          if ele.idClass = eltFunc then begin
            //Para las funciones, se debe comparar los parámetros
            fun := TxpEleInlin(ele);
            if fun.SameParamsType(pars) then begin
              Found := true;
              break;
            end;
          end else begin
            //Si tiene el mismo nombre que cualquier otro elemento, es conflicto
            GenError('Identifier "%s" already defined', [uname]);
            exit;
          end;
        end;
      end else begin
        {Debe ser elemento de IMPLEMENTATION, no hay otra opción porque se supone que
        estamos en la sección de IMPLEMENTATION, así que el Parent, debe ser una unidad.}
        GenErrorPos(ER_DUPLIC_FUNC_,[procName], srcPos);  //Está duplicada en IMPLEMENTATION
        exit;
      end;
    end;
    if Found then begin
      //Es una implementación. No vale la pena tener otro nodo.
      TreeElems.OpenElement(fun);  //Abre el nodo anterior
      fun.Implemented := true;   //marca como implementada
    end else begin
      //Debe ser una función privada. No declarada en Interface.
      //La creamos con seguridad porque ya verificamos que no hay conflicto en IMPLEMENTATION.
      fun := AddInline(procName, retType, srcPos, pars, @callParam, @callFunct);
      //Un caso especial de proced. declarado solo en IMPLEMENTATION.
      fun.location := locImplement;
    end;
  end;
  locMain: begin
    //Es una compilación en el programa principal. ¿Y si es FORWARD?
    ReadInlineHeader(procName, retType, srcPos, pars);  //Procesa el encabezado
    if HayError then exit;
    if TreeElems.InlineExistInCur(procName, pars) then begin
      GenErrorPos(ER_DUPLIC_FUNC_,[procName], srcPos);
      exit;
    end;
    fun := AddInline(procName, retType, srcPos, pars, @callParam, @callFunct);
    //Aquí estamos en el entorno de la función.
    fun.location := locMain;
  end
  else
    GenError(ER_NOT_IMPLEM_, ['locMain in TCompiler_PIC16.CompileProcDeclar()']);
  end;
  //Aquí ya se tiene "fun" abierta, validada y apuntando a la declaración.
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
  fun.posCtx := cIn.PosAct;  //Guarda posición para la segunda compilación
  bod := CreateBody;   //crea elemento del cuerpo de la función
  bod.srcDec := cIn.ReadSrcPos;
  TreeElems.AddElementAndOpen(bod);  //Abre nodo Body
  CompileInlineBody(fun);
  TreeElems.CloseElement;  //Cierra Nodo Body
  TreeElems.CloseElement; //cierra espacio de nombres de la función
  bod.srcEnd := cIn.ReadSrcPos;  //Fin de cuerpo
//  fun.adrReturn := pic.iRam-1;  //Guarda dirección del i_RETURN
  if not CaptureTok(';') then exit;
  ProcComments;  //Quita espacios. Puede salir con error
end;
procedure TCompiler_PIC16.CompileUnit(uni: TxpElement);
{Realiza la compilación de una unidad}
var
  fun: TxpEleFun;
  elem: TxpElement;
  procName: String;
  retType: TxpEleType;
  srcPos: TSrcPos;
  pars: TxpParFuncArray;
  IsInterrupt: Boolean;
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
    if UpCase(cIn.tok)<>uni.uname then begin
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
  curLocation := locInterface;
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
        CompileVarDeclar;  //marca como "IsInterface"
        if HayError then exit;;
      end;
    end else if cIn.tokL = 'type' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'implementation') do begin
        CompileTypeDeclar(locInterface);
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
      ReadProcHeader(procName, retType, srcPos, pars, IsInterrupt);
      if HayError then exit;
      if TreeElems.FunctionExistInCur(procName, pars) then begin
        GenErrorPos(ER_DUPLIC_FUNC_,[procName], srcPos);
        exit;
      end;
      fun := AddFunction(procName, retType, srcPos, pars, IsInterrupt, @callParam, @callFunct);
      //Aquí estamos en el entorno de la función.
      fun.location := locInterface;  //marca ubicación
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
  curLocation := locImplement;
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
      CompileProcDeclar;  //Compila en IMPLEMENTATION
      if HayError then exit;
    end else begin
      GenError(ER_NOT_IMPLEM_, [cIn.tok]);
      exit;
    end;
  end;
  //Verifica si todas las funciones de INTERFACE, se implementaron
  for elem in TreeElems.curNode.elements do if elem.idClass = eltFunc then begin
    fun := TxpEleFun(elem);
    if (fun.location = locInterface) and not fun.Implemented then begin
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
      if uni.ExistsIn(TreeElems.curNode.elements) then begin
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
  curLocation := locMain;
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
        CompileTypeDeclar(locMain);
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
      CompileProcDeclar;
      if HayError then exit;
    end else if cIn.tokL = 'inline' then begin
      cIn.Next;    //lo toma
      CompileInlineDeclar(locMain);
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
  begin
//debugln('  Asignando espacio a %s', [xvar.name]);
    case xvar.adicPar.hasAdic of
    decAbsol: begin
//debugln('Abs: xvar=%s at %d', [xvar.name, xvar.adicPar.absAddr]);
      {Tiene declaración absoluta. Mejor compilamos de nuevo la declaración, porque
      puede haber referencia a variables que han cambiado de ubicación, por
      optimización.
      Se podría hacer una verificación, para saber si la referencia es a direcciones
      absolutas, en lugar de a variables (o a variables temporales), y así evitar
      tener que compilar de nuevo, la declaración.}
      posAct := cIn.PosAct;   //guarda posición actual
      cIn.PosAct := xVar.adicPar.srcDec;  //Posiciona en la declaración adicional
      TreeElems.curNode := xvar.Parent;   {Posiciona el árbol, tal cual estaría en la
                                           primera pasada, para una correcta resolución
                                           de nombres}
      GetAdicVarDeclar(xvar.typ, adicVarDec);
      //No debería dar error, porque ya pasó la primera pasada
      xvar.adicPar := adicVarDec;
      cIn.PosAct := posAct;
      //Asigna RAM
      CreateVarInRAM(xVar, shared);  //Crea la variable
      xvar.typ.DefineRegister;  //Asegura que se dispondrá de los RT necesarios
      //Puede salir con error
    end;
    decRegis, decRegisA, decRegisX, decRegisY: begin
      //Variable registro. No se asigna espacio.
    end;
    decNone: begin
      //Variable normal. Necesita ser asiganda en RAM
      CreateVarInRAM(xVar, shared);  //Crea la variable
      xvar.typ.DefineRegister;  //Asegura que se dispondrá de los RT necesarios
      //Puede salir con error
    end;
    end;
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
  //Reporte de registros de trabajo, auxiliares y de pila
  if (listRegAux.Count>0) then begin
    lins.Add(';------ Work and Aux. Registers ------');
    for reg in listRegAux do begin
      if not reg.assigned then continue;  //puede haber registros de trabajo no asignados
      nam := pic.NameRAM(reg.addr); //debería tener nombre
      adStr := '0x' + IntToHex(reg.addr, 3);
      lins.Add(nam + ' EQU ' +  adStr);
    end;
  end;
  if (listRegStk.Count>0) then begin
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
//2909
