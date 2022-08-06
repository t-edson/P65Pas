{Parser

Esta sería la versión de XpresParser (definida en el framework t-Xpres), orientada
a trabajar con la CPU6502.
La idea es tener aquí todas las rutinas que en lo posible sean independientes del
lenguaje y del modelo de CPU.
Para mayor información sobre el uso del framework Xpres, consultar la documentación
técnica.
}
//{$Define LogExpres}
unit CompBase;
interface
uses
  Classes, SysUtils, Types, LazLogger, LexPas,
  XpresElemP65, XpresAST, CompContexts;
type
//Expression type, according the position it appears.
TPosExpres = (pexINDEP,  //Expresión independiente
              pexASIG,   //Expresión de asignación
              pexPROC,   //Expresión de procedimiento
              pexSTRUC,  //Expresión de estructura
              pexPARAM   //Expresión de parámetro de función
              );
TOperType = (operUnary,  //Operación Unaria
             operBinary  //Operación Binaria
             );
TTypeLocat = (
             tlCurrNode,   //Type at the current node
             tlParentNode  //Type at the parent node
           );
//Information about the las ASM code generated. Used for optimization.
TLastASMcode = (
             lacNone,    //No special code generated.
             lacLastIsTXA, {Last instruction is TXA, and it's used only
                           to move result from X to acumulator. ***** ¿Es necesario? ¿Se usa? ¿No bastaría leer la última instrucción en RAM?}
             //Flags applied to boolean expression results.
             lacCopyZtoA, {Last ASM code is for obtaining boolean expression in regA
                           from Z.}
             lacInvZtoA,  {Last ASM code is for obtaining boolean expression in regA
                           from Z (inverted).}
             lacCopyCtoA, {Las ASM code if for obtaining, boolean expression in regA,
                          using the bit C and copied to A. }
             lacInvCtoA,  {Last ASM code is for obtaining boolean expression in regA
                           from C (inverted).}
             lacInvAtoA   {Value of regA is inverted in all bits to regA}
             );
{ TCompilerBase }
{Clase base para crear a los objetos compiladores.
Esta clase debe ser el ancestro común de todos los compialdores a usar en PicPas.
Contiene métodos abstractos que deben ser impleemntados en las clases descendeintes.}
TCompilerBase = class(TContexts)
private
  function CreateArrayTypeDec(typName: string; nELem: integer;
    itType: TEleTypeDec; const srcPos: TSrcPos): TEleTypeDec;
protected  //Parser routines
  ExprLevel  : Integer;  //Nivel de anidamiento de la rutina de evaluación de expresiones
  function EOExpres: boolean;
  function EOBlock: boolean;
  function CaptureDelExpres: boolean;
  procedure ProcCommentsNoExec;
  procedure ProcComments;
  procedure TipDefecNumber(out typ: TEleTypeDec; out value: TConsValue;
    toknum: string);
  procedure TipDefecString(out typ: TEleTypeDec; out value: TConsValue;
    tokcad: string);
  function CaptureTok(ctok: string): boolean;
  function CaptureStr(cstr: string): boolean;
  procedure CaptureParams(out funPars: TxpParFuncArray);
protected  //Flags for boolean type.
  {These variables are reset in the procedures: SetFun<XXX>. They contains the state of
  the Register/Status-flags if the last UOR or BOR is executed. }
  lastASMcode : TLastASMcode;  //ASM code generated for last the UOR or BOR.
  lastASMaddr : integer;  //Memory address for the last code indicated by lastASMoper.
  AcumStatInZ : boolean;  {Indicates the Z flag contains the status of the value in A
                          register. For example if regA = 0, Z wil be 1.}
protected  //Elements creation
  nTypesCreated: integer;
  function NameExistsIn(eName: string; list: TxpElements): boolean;
  function CreateEleVarDec(varName: string; eleTyp: TEleTypeDec): TEleVarDec;
  function CreateEleType(const typName: string; const srcPos: TSrcPos;
    size0: smallint; catType: TxpCatType; group: TTypeGroup): TEleTypeDec;
  function CreateEleTypePtr(const typName: string; const srcPos: TSrcPos;
                         ptrType: TEleTypeDec): TEleTypeDec;
  function CreateEleTypeObject(const typName: string; const srcPos: TSrcPos): TEleTypeDec;
  function CreateUnit(uniName: string): TEleUnit;
  function CreateFunction(funName: string; typ: TEleTypeDec): TEleFun;
  function CreateExpression(opName: string; dtType: TEleTypeDec; opType: TopType;
                            srcPos: TSrcPos): TEleExpress;
  function AddExpressAndOpen(opName: string; dtType: TEleTypeDec; opType: TopType;
           srcPos: TSrcPos): TEleExpress;
  function AddVarDecAndOpen(varName: string; eleTyp: TEleTypeDec; srcPos: TSrcPos
    ): TEleVarDec;
  function AddConsDecAndOpen(conName: string; eleTyp: TEleTypeDec; srcPos: TSrcPos
    ): TEleConsDec;
  function AddTypeDecAndOpen(typName: string; typeSize: integer; catType: TxpCatType;
    group: TTypeGroup; srcPos: TSrcPos): TEleTypeDec;
  function OpenTypeDec(const srcPos: TSrcPos; tname: string; tsize: integer;
    catType: TxpCatType; group: TTypeGroup; location: TTypeLocat): TEleTypeDec;
  procedure CloseTypeDec(typeDec: TEleTypeDec);
  procedure CreateFunctionParams(var funPars: TxpParFuncArray);
  function AddFunctionUNI(funName: string; retTyp: TEleTypeDec;
    const srcPos: TSrcPos; const pars: TxpParFuncArray; Interrup: boolean;
  addParam: boolean): TEleFun;
  function AddFunctionDEC(funName: string; retTyp: TEleTypeDec; const srcPos: TSrcPos;
                          const pars: TxpParFuncArray; Interrup: boolean): TEleFunDec;
  function AddFunctionIMP(funName: string; retTyp: TEleTypeDec;
    const srcPos: TSrcPos; functDeclar: TEleFunDec; addParam: boolean): TEleFun;
protected  //Containers
  procedure RefreshAllElementLists;
  procedure RemoveUnusedFunc;
  procedure RemoveUnusedVars;
  procedure RemoveUnusedCons;
  procedure RemoveUnusedTypes;
  procedure UpdateCallersToUnits;
  procedure UpdateFunLstCalled;
  procedure SeparateUsedFunctions;
public     //Containers
  TreeElems  : TXpTreeElements; //Abstract syntax tree.
  usedFuncs  : TEleFuns;    //Store only used functions
  unusedFuncs: TEleFuns;    //Store only unused functions
  interruptFunct: TEleFun;  //Store ths only Interrupt function
protected //Compiler events
  {This is one way the Parser can communicate with the Code Generator, considering this
  unit is independent of Code Generation.}
  OnExprStart : procedure of object;  {Se genera al iniciar la
                                       evaluación de una expresión.}
  OnExprEnd   : procedure(posExpres: TPosExpres) of object;  {Se genera al terminar de
                                                              evaluar una expresión.}
protected //Calls to Directive Module (ParserDirec.pas)
  callProcDIRline  : procedure(const AsmLin: string; out ctxChanged: boolean) of object;
protected //Calls to Code Generator (GenCod)
  { TODO : Estas llamadas deben desaparecer para hacer a esta unidad independiente del Generador de Código }
  {These are routines that must be implemented in Code-generator.}
  callDefineArray  : procedure(etyp: TEleTypeDec) of object;  //routines to implement Arrays.
  callDefinePointer: procedure(etyp: TEleTypeDec) of object; //routines to implement Pointers.
  //Validate phisycal address
  callValidRAMaddr : procedure(addr: integer) of object;
  callStartProgram : procedure of object;
  callEndProgram   : procedure of object;
  eleFunInc: TEleFun;   {Referencia a la función de incremento. Se guarda para evitar
                        hacer búsqueda innecesaria. Notar que esta es una referencia
                        hacia el generador de código que se llenará posteriormente.}
  eleFunRef: TEleFun;   {Referencia a la función de _ref(). Se guarda para evitar
                        hacer búsqueda innecesaria. Notar que esta es una referencia
                        hacia el generador de código que se llenará posteriormente.}
protected //Expressions
  function CompatibleTypes(typ1, typ2: TEleTypeDec): boolean;
  function MethodFromBinOperator(const OpType: TEleTypeDec; Op: string;
    OpType2: TEleTypeDec): TEleFunBase;
  function MethodFromUnaOperator(const OpType: TEleTypeDec; Op: string
    ): TEleFunBase;
  function AddConstDeclarByte(decName: string; consVal: integer): TEleConsDec;
  function GetConstantArray(arrDelimt: char): TEleExpress;
  function GetConstantArrayStr(allowChar: boolean = true): TEleExpress;
  function GetOperand: TEleExpress;
  function GetExpression(const prec: Integer): TEleExpress;
  function AddExpressionConstByte(name: string; bytValue: byte; srcPos: TSrcPos
    ): TEleExpress;
  function AddExpressionConstBool(name: string; boolValue: Boolean; srcPos: TSrcPos
    ): TEleExpress;
public    //Types to implement
  typByte : TEleTypeDec;
  typBool : TEleTypeDec;
  typChar : TEleTypeDec;
  typWord : TEleTypeDec;
//  typString: TxpEleType;
public    //Public attributes of compiler
  ID        : integer;     //Identificador para el compilador.
  IsUnit    : boolean;     //Flag to identify a Unit
  //Variables públicas del compilador
  ejecProg  : boolean;     //Indicates the compiler is working
  stopEjec  : boolean;     //To stop compilation
public    //Compiling Options. Deberían ser PROTECTED.
  GeneralORG  : integer;   //Dirección general de origen de código
  protected
  mode        : (modPascal, modPicPas);
  enabDirMsgs : boolean;   //Bandera para permitir generar mensajes desde las directivas.
  incDetComm  : boolean;   //Incluir Comentarios detallados.
  OptBnkAftIF : boolean;   //Optimizar instrucciones de cambio de banco al final de IF
  OptReuProVar: boolean;   //Optimiza reutilizando variables locales de procedimientos
  OptRetProc  : boolean;   //Optimiza el último exit de los procedimientos.
  AsmIncComm  : boolean;   //Incluye comentarios en salida de ASM
protected //Files
  mainFile    : string;    //Archivo inicial que se compila
  hexFile     : string;    //Nombre de archivo de salida
public    //Files
  function hexFilePath: string;
  function mainFilePath: string;
  function ExpandRelPathTo(BaseFile, FileName: string): string;
  function ExpandRelPathToMain(FileName: string): string;
public    //Abstract methods
  function CompilerName: string; virtual; abstract;  //Name of the compiler
  procedure RAMusage(lins: TStrings; ExcUnused: boolean); virtual; abstract;
  function RAMusedStr: string; virtual; abstract;
  procedure GetResourcesUsed(out ramUse, romUse, stkUse: single); virtual; abstract;
  procedure DumpCode(lins: TSTrings; asmMode, IncVarDec, ExcUnused: boolean;
                     incAdrr, incCom, incVarNam: boolean); virtual; abstract;
  procedure GenerateListReport(lins: TStrings); virtual; abstract;
public    //Callers methods
  function AddCallerToFrom(toElem: TxpElement; callerElem: TxpElement): TxpEleCaller;
  function AddCallerToFromCurr(toElem: TxpElement): TxpEleCaller;
protected //Miscellaneous
  elemCnt: integer;  //Counter to generate names.
  function GenerateUniqName(base: string): string;
  function getListOfIdent(out itemList: TStringDynArray; out
    srcPosArray: TSrcPosArray): boolean;
  procedure LogExpLevel(txt: string);
  function IsTheSameVar(var1, var2: TEleVarDec): boolean; inline;
public    //Initialization
  constructor Create; override;
  destructor Destroy; override;
end;

implementation
uses Graphics;
resourcestring
  ER_IDEN_EXPECT  = 'Identifier expected.';
  ER_DUPLIC_IDEN  = 'Duplicated identifier: "%s"';
  ER_UNDEF_TYPE_  = 'Undefined type "%s"';
  ER_SEMIC_EXPEC  = '";" expected.';
  ER_STR_EXPECTED = '"%s" expected.';
  ER_IN_EXPRESSI  = 'Error in expression. ")" expected.';
  ER_OPERAN_EXPEC = 'Operand expected.';
  ER_UND_OPER_TY_ = 'Undefined operator: %s for type: %s';
  ER_CAN_AP_OPER_ = 'Cannot apply the operator "%s" to type "%s"';
  ER_RA_HAV_USED  = 'Register A has been used.';
  ER_RX_HAV_USED  = 'Register X has been used.';
  ER_RY_HAV_USED  =  'Register Y has been used.';
  ER_CON_EXP_EXP  = 'Constant expression expected.';
  ER_ILLEG_OPERA_ = 'Illegal Operation: %s';
  ER_UNKNOWN_IDE_ = 'Unknown identifier: %s';
  ER_TYP_PARM_ER_ = 'Type parameters error on %s';
  ER_IN_CHARACTER = 'Error in character.';
  ER_INV_COD_CHAR = 'Invalid code for char.';
  ER_NOTYPDEF_NU  = 'No type defined to allocate this number.';

{TCompilerBase}
function TCompilerBase.EOExpres: boolean; inline;
//Indica si se ha llegado al final de una expresión.
begin
  Result := token = ';';  //en este caso de ejemplo, usamos punto y coma
  {En la práctica, puede ser conveniente definir un tipo de token como "tkExpDelim", para
   mejorar el tiempo de respuesta del procesamiento, de modo que la condición sería:
     Result := tokType = tkExpDelim;
  }
end;
function TCompilerBase.EOBlock: boolean; inline;
//Indica si se ha llegado el final de un bloque
begin
  Result := tokType = tkBlkDelim;
  {No está implementado aquí, pero en la práctica puede ser conveniente definir un tipo de token
   como "tnBlkDelim", para mejorar el tiempo de respuesta del procesamiento, de modo que la
   condición sería:
  Result := tokType = tnBlkDelim;}
end;
function TCompilerBase.CaptureDelExpres: boolean;
//Verifica si sigue un delimitador de expresión. Si encuentra devuelve false.
begin
  SkipWhites;
  if EOExpres then begin //encontró
    Next;   //pasa al siguiente
    exit(true);
  end else begin   //es un error
    GenError(ER_SEMIC_EXPEC);
    exit(false);  //sale con error
  end;

end;
procedure TCompilerBase.ProcComments;
{Procesa comentarios y directivas.
Notar que este procedimiento puede detectar varios errores en el mismo bloque, y que
pasa al siguiente token, aún cuando detecta errores. }
var
  ctxChanged: Boolean;  //Manejamos variables locales para permitir recursividad
begin
  SkipWhites;
  while (tokType = tkDirective) do begin
    //Es una directiva
    callProcDIRline(token, ctxChanged);  //procesa línea
    if HayError then begin
      Next;   //Pasa, porque es un error ya ubicado, y mejor buscamos otros
      SkipWhites;
      continue;
    end;
    if ctxChanged then begin
      {Hubo cambio de contexto. Procesamos nuevamente, porque ahora estamos ya en
      otro contexto y se supone que esta llamada a ProcComments(), se hace precisamente
      para saltar blancos, comentarios, directivas, o bloques ASM.}
      ProcComments;   {En el nuevo contexto puede haber nuevos comentarios o bloques Asm.}
      exit;
    end;
  //Pasa a siguiente
    Next;
    SkipWhites;  //limpia blancos
  end;
end;
procedure TCompilerBase.ProcCommentsNoExec;
{Similar a ProcComments(), pero no ejecuta directivas o bloques ASM.}
begin
  SkipWhites;
  while (tokType = tkDirective) do begin
    //Pasa a siguiente
    Next;
    SkipWhites;  //limpia blancos
  end;
end;
procedure TCompilerBase.TipDefecNumber(out typ: TEleTypeDec; out value: TConsValue; toknum: string);
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
    value.valInt := n;   //copia valor de constante entera
    {Asigna un tipo, de acuerdo al rango. Notar que el tipo más pequeño, usado
    es el byte, y no el bit.}
    if (n>=0) and  (n<=255) then begin
      typ := typByte;
    end else if (n>= 0) and (n<=$FFFF) then begin
      typ := typWord;
    end else  begin //no encontró
      GenError(ER_NOTYPDEF_NU);
      typ := typNull;
    end;
  end;
end;
procedure TCompilerBase.TipDefecString(out typ: TEleTypeDec; out value: TConsValue; tokcad: string);
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
function TCompilerBase.CaptureTok(ctok: string): boolean;
{Toma el token indicado del contexto de entrada. Si no lo encuentra, genera error y
devuelve FALSE.}
  procedure GenErrorInLastLine(var p: TSrcPos);
  {Genera error posicionando el punto del error, en una línea anterior, que no esté
  vacía.}
  var
    lin: String;
  begin
    if p.row>1 then begin
      //Hay línea anterior
      repeat
        p.row := p.row - 1;
        lin := curCtx.curLines[p.row - 1];
      until (p.row<=1) or (trim(lin)<>'');
      //Encontró línea anterior no nula o llegó a la primera línea.
//      xlex.ExploreLine(Point(length(lin), p.row), toks, CurTok );
      p.col := length(lin);   //mueve al final (antes del EOL)
      GenError(ER_STR_EXPECTED, [ctok], p);  //Genera error
    end else begin
      //No hay línea anterior
      p.col := 1;   //mueve al inicio
      GenError(ER_STR_EXPECTED, [ctok], p);  //Genera error
    end;
  end;

var
  x: integer;
  lin: String;
  p: TSrcPos;
begin
  if token<>ctok then begin
    //No se encontró el token. Muestra mensaje de error.
    {Pero el error, debe estar antes, así que hacemos la magia de explorar hacia atrás,
    hasta encontrar el token involucrado.}
    p := GetSrcPos;   //posición actual
    x := p.col;   //lee posición actual
    if x>1 then begin
      //Hay algo antes del token. Tomamos la línea
      if tokType = tkEol then begin
        //Ya estamos apuntando a la siguiente línea
        lin := curCtx.curLines[curCtx.row-2];  //Leemos la anterior
      end else begin
        lin := curCtx.CurLine;
      end;
      repeat
        dec(x);
      until (x<=1) or (lin[x] <> ' ');
      if x<=1 then begin
        //Está lleno de espacios, hasta el inicio.
        //Es muy probable que el error esté en la línea anterior.
        GenErrorInLastLine(p);
      end else begin
        //Encontró, en la misma línea un caracter diferente de espacio
        GenError(ER_STR_EXPECTED, [ctok], p);  //Genera error ahí mismo
      end;
    end else begin
      //Está al inicio de la línea. El error debe estar antes
      GenErrorInLastLine(p);
    end;
    exit(false);
  end;
  Next;
  exit(true);
end;
function TCompilerBase.CaptureStr(cstr: string): boolean;
//Similar a CaptureTok(), pero para cadenas. Se debe dar el texto en minúscula.
begin
  //Debe haber parámetros
  if UpCase(token)<>cstr then begin
    GenError(ER_STR_EXPECTED, [cstr]);
    exit(false);
  end;
  Next;
  exit(true);
end;
procedure TCompilerBase.CaptureParams(out funPars: TxpParFuncArray);
{Lee los parámetros (tipo) con la que una función es llamada. EL resultado lo
devuelve en "funPars". Solo actualiza el campo de tipo de "funPars".
Notar la similitud de este procedimiento con la rutina implementada en ReadProcHeader()
en la unidad "Compiler_PIC16". }
const
  BLOCK_SIZE = 10;    //Tamaño de bloque.
var
  curSize, n: Integer;
  par: TEleExpress;
begin
  //func0.ClearParams;
  if EOBlock or EOExpres then begin
    //no tiene parámetros
  end else begin
    //Debe haber parámetros
    if token <> '(' then begin
      //Si no sigue '(', significa que no hay parámetros.
      exit;
    end;
    Next;  //Toma paréntesis
    SkipWhites;
    if token = ')' then begin
      //There is not parameter.
      Next;
      exit;
    end;
    //Debe haber parámetros. Prepara espacio para leer.
    curSize := BLOCK_SIZE;    //Tamaño inicial de bloque
    setlength(funPars, curSize);  //Tamaño inicial
    n := 0;
    repeat
      par := GetExpression(0);  //captura parámetro
      if HayError then exit;   //aborta
      //Guarda tipo de parámetro
      funPars[n].typ   := par.Typ;
      //Prepara siguiente lectura
      inc(n);
      if n >= curSize then begin
        curSize += BLOCK_SIZE;   //Incrementa tamaño en bloque
        setlength(funPars, curSize);  //hace espacio en bloque
      end;
      //Busca delimitador
      if token = ',' then begin
        Next;   //toma separador
        SkipWhites;
      end else begin
        //No sigue separador de parámetros,
        //debe terminar la lista de parámetros
        //¿Verificar EOBlock or EOExpres ?
        break;
      end;
    until false;
    //busca paréntesis final
    if not CaptureTok(')') then exit;
    //Asigna tamaño final
    setlength(funPars, n);
  end;
end;
//Elements creation
function TCompilerBase.NameExistsIn(eName: string; list: TxpElements): boolean;
{VErify is a element name exists in a elment list. "eName" must be in upper Case.}
var
  ele: TxpElement;
begin
  for ele in list do begin
    if ele.uname = eName then begin
      exit(true);
    end;
  end;
  exit(false);
end;
function TCompilerBase.CreateEleVarDec(varName: string; eleTyp: TEleTypeDec): TEleVarDec;
{Rutina para crear una variable. Devuelve referencia a la variable creada.}
var
  xVar: TEleVarDec;
begin
  xVar        := TEleVarDec.Create;
  xVar.name   := varName;
  xVar.typ    := eleTyp;
  xVar.adicPar.hasAdic := decNone;
//  xVar.adicPar.hasInit := false;
  xVar.storage := stRamFix;  //The most common storage for variables
  Result       := xVar;
end;
function TCompilerBase.CreateEleType(const typName: string; const srcPos: TSrcPos;
                     size0: smallint; catType: TxpCatType; group: TTypeGroup): TEleTypeDec;
begin
  Result := TEleTypeDec.Create;
  Result.name    := typName;
  Result.srcDec  := srcPos;
  Result.size    := size0;
  Result.catType := catType;
  Result.group := group;
end;
function TCompilerBase.CreateEleTypePtr(const typName: string;
  const srcPos: TSrcPos; ptrType: TEleTypeDec): TEleTypeDec;
var
  xtyp: TEleTypeDec;
begin
  xtyp := CreateEleType(typName, srcPos, -1, tctPointer, t_object);
  xtyp.ptrType := ptrType; //Item type
  callDefinePointer(xtyp);   //Define operations to array
  exit(xtyp);
end;
function TCompilerBase.CreateEleTypeObject(const typName: string;
  const srcPos: TSrcPos): TEleTypeDec;
begin
  Result := CreateEleType(typName, srcPos, -1, tctObject, t_object);

end;
function TCompilerBase.CreateUnit(uniName: string): TEleUnit;
var
  uni: TEleUnit;
begin
  uni := TEleUnit.Create;
  uni.name := uniName;
  Result := uni;
end;
function TCompilerBase.CreateFunction(funName: string; typ: TEleTypeDec): TEleFun;
{Crea una nueva función y devuelve la referencia a la función.}
var
  fun : TEleFun;
begin
  fun := TEleFun.Create;
  fun.name:= funName;
  fun.retType := typ;
  fun.ClearParams;
  Result := fun;
end;
function TCompilerBase.CreateExpression(opName: string; dtType: TEleTypeDec;
  opType: TopType; srcPos: TSrcPos): TEleExpress;
var
  expr: TEleExpress;
begin
  expr := TEleExpress.Create;
  expr.name := opName;
  expr.Typ := dtType;  //Get data type
  expr.opType := opType;
  expr.srcDec := srcPos;
  exit(expr);
end;
function TCompilerBase.AddExpressAndOpen(opName: string; dtType: TEleTypeDec; opType: TopType;
         srcPos: TSrcPos): TEleExpress;
var
  expr: TEleExpress;
begin
  expr := CreateExpression(opName, dtType, opType, srcPos);
  if opType = otConst then expr.Sto := stConst;  //The only option.
  TreeElems.AddElementAndOpen(expr);
  exit(expr);
end;
function TCompilerBase.AddExpressionConstByte(name: string; bytValue: byte;
         srcPos: TSrcPos): TEleExpress;
{Create a constant element expression like generated by GetExpression(), when analyze
a literal numeric of type Byte.}
begin
  Result := CreateExpression(name, typByte, otConst, srcPos);
  Result.Sto := stConst;
  Result.evaluated := true;
  Result.value.ValInt := bytValue;
  TreeElems.AddElement(Result);
end;
function TCompilerBase.AddExpressionConstBool(name: string; boolValue: Boolean;
  srcPos: TSrcPos): TEleExpress;
{Create a constant element expression like generated by GetExpression(), when analyze
a literal boolean.}
begin
  Result := CreateExpression(name, typBool, otConst, srcPos);
  Result.Sto := stConst;
  Result.evaluated := true;
  Result.value.ValBool := boolValue;
  TreeElems.AddElement(Result);
end;
function TCompilerBase.AddVarDecAndOpen(varName: string; eleTyp: TEleTypeDec;
  srcPos: TSrcPos): TEleVarDec;
{Crea un elemento variable y lo agrega en el nodo actual del árbol de sintaxis.
Si no hay errores, devuelve la referencia a la variable. En caso contrario,
devuelve NIL.
Notar que este método, no asigna RAM a la variable. En una creación completa de
variables, se debería llamar a CreateVarInRAM(), después de agregar la variable.}
var
  xvar: TEleVarDec;
begin
  //Check for duplicated name. Only a search in the current node is needed.
  if NameExistsIn(UpCase(varName), TreeElems.curNode.elements) then begin
    GenError(ER_DUPLIC_IDEN, [varName], srcPos);
    exit(nil);
  end;
  //xvar := CreateEleVarDec(varName, eleTyp);
  //xvar.srcDec := srcPos;  //Actualiza posición
  //TreeElems.AddElement(xvar);
  //Result := xvar;
  Result := TreeElems.AddVarDecAndOpen(srcPos, varName, eleTyp);
  Result.storage := stRamFix;
end;
function TCompilerBase.AddConsDecAndOpen(conName: string; eleTyp: TEleTypeDec;
  srcPos: TSrcPos): TEleConsDec;
{Crea un elemento constante y lo agrega en el nodo actual del árbol de sintaxis.
Si no hay errores, devuelve la referencia a la variable. En caso contrario,
devuelve NIL. }
begin
  //Check for duplicated name. Only a search in the current node is needed.
  if NameExistsIn(UpCase(conName), TreeElems.curNode.elements) then begin
    GenError(ER_DUPLIC_IDEN, [conName], srcPos);
    exit(nil);
  end;
  Result := TreeElems.AddConsDecAndOpen(srcPos, conName, eleTyp);
end;
function TCompilerBase.AddTypeDecAndOpen(typName: string; typeSize: integer;
  catType: TxpCatType; group: TTypeGroup; srcPos: TSrcPos): TEleTypeDec;
begin
  //Check for duplicated name. Only a search in the current node is needed.
  if NameExistsIn(UpCase(typName), TreeElems.curNode.elements) then begin
    GenError(ER_DUPLIC_IDEN, [typName], srcPos);
    exit(nil);
  end;
  Result := TreeElems.AddTypeDecAndOpen(srcPos, typName, typeSize, catType, group);
end;
function TCompilerBase.OpenTypeDec(const srcPos: TSrcPos; tname: string;
  tsize: integer; catType: TxpCatType; group: TTypeGroup;
  location: TTypeLocat): TEleTypeDec;
var
  tmp, progFrame: TxpElement;
  ipos: Integer;
  typeDec: TEleTypeDec;
begin
  {Similar to TreeElems.AddTypeDecAndOpen() but can specify the location where the
  type is opened.
  This instruction must used with CloseTypeDec()
  }
  if location = tlParentNode then begin
    //Create in the parent location.
    tmp := TreeElems.curNode;  //Save current location
    //Change to the parent of the current code container. This will work always.
    progFrame := TreeElems.curCodCont.Parent;  //Should be TEleProgFrame (Function, unit or main program)
    TreeElems.OpenElement(progFrame);
    ipos := progFrame.elements.Count-1;  //Before of the current COde container
  end else begin
    //Creates in the current location.
    tmp := TreeElems.curNode;  //Save current location
    ipos := -1;
  end;
  //----------- Create Type -----------
  typeDec := TreeElems.AddTypeDecAndOpen(srcPos, tname, tsize, catType, group, ipos);
  typeDec.tmpNode := tmp;  //Save current node here
  exit(typeDec);
end;
procedure TCompilerBase.CloseTypeDec(typeDec: TEleTypeDec);
{Close a Type declaration element, opened with OpenTypeDec().}
begin
  TreeElems.CloseElement;  //Close type declaration.
  TreeElems.curNode := typeDec.tmpNode;  //Restore location
end;
function TCompilerBase.CreateArrayTypeDec(typName: string; nELem: integer;
                                          itType: TEleTypeDec;
                                          const srcPos: TSrcPos): TEleTypeDec;
{Creates a type declaration for an array.
"typName"  must be unique in the scope. Verification isn't done here.
The location where the type is created is before the current node.
}
var
  xtyp: TEleTypeDec;
  consDec: TEleConsDec;
  //tmp, progFrame: TxpElement;
  //ipos: Integer;
begin
  xtyp := OpenTypeDec(srcPos, typName, -1, tctArray, t_object, tlParentNode);
    //Crea campo "length".
    consDec := AddConstDeclarByte('length', nElem);
    //Termina definición
    xtyp.consNitm := consDec;  //Update reference to the number of items.
    xtyp.itmType := itType;  //Actualiza tipo
    callDefineArray(xtyp);   //Define operations to array
    //Add location {******** ¿Es correcto hacer esto desde aquí? }
    xtyp.location := curLocation;   //Ubicación del tipo (Interface/Implementation/...)
  CloseTypeDec(xtyp);
  inc(nTypesCreated);   //Updates counter for types created
  exit(xtyp);
end;
procedure TCompilerBase.CreateFunctionParams(var funPars: TxpParFuncArray);
{Crea los parámetros de una función como variables globales, a partir de un arreglo
TxpParFunc. }
var
  i: Integer;
  par: TxpParFunc;
  xvar: TEleVarDec;
  regAused: boolean = false;
  regXused: boolean = false;
  regYused: boolean = false;
begin
  for i := 0 to high(funPars) do begin
      par := funPars[i];
      xvar := AddVarDecAndOpen({fun.name + '_' + }par.name, par.typ, par.srcPos);
      TreeElems.CloseElement;
      if HayError then exit;
      xvar.IsParameter := true;  //Marca bandera
      xvar.adicPar := par.adicVar;  //Copy aditional settings
      xvar.srcDec := par.srcPos;
      case par.adicVar.hasAdic of
      decRegis: begin
        //Parameters REGISTER use: A or H,A register. Only can be used once.
        if regAused then begin
          GenError(ER_RA_HAV_USED, [], par.srcPos);
          exit;
        end;
        regAused := true;  //Activa bandera
        xvar.storage := stRegister;
      end;
      decRegisA: begin
        //Parameter REGISTER A
        if regAused then begin
          GenError(ER_RA_HAV_USED, [], par.srcPos);
          exit;
        end;
        regAused := true;  //Activa bandera
        xvar.storage := stRegistA;
      end;
      decRegisX: begin
        //Parameter REGISTER X
        if regXused then begin
          GenError(ER_RX_HAV_USED, [], par.srcPos);
          exit;
        end;
        regXused := true;  //Activa bandera
        xvar.storage := stRegistX;
      end;
      decRegisY: begin
        //Parameter REGISTER Y
        if regYused then begin
          GenError(ER_RY_HAV_USED, [], par.srcPos);
          exit;
        end;
        regYused := true;  //Activa bandera
        xvar.storage := stRegistY;
      end;
      decNone: begin
        xvar.storage := stRamFix;
      end;
      end;
      //Actualiza referencia a la variable que almacena el parámetro.
      funPars[i].pvar := xvar;
  end;
end;
function TCompilerBase.AddFunctionUNI(funName: string; retTyp: TEleTypeDec;
  const srcPos: TSrcPos; const pars: TxpParFuncArray; Interrup: boolean;
  addParam: boolean): TEleFun;
{Create a new function, in normal mode (In the Main program or a like a private function
in Implementation section) and add it to the Syntax Tree in the current node.
- addParam -> Indicates whether the parameters will be created as variables. }
var
  xfun: TEleFun;
begin
  xfun := CreateFunction(funName, retTyp);
  xfun.srcDec := srcPos;   //Toma ubicación en el código
  xfun.declar := nil;   //This is declaration
  xfun.pars := pars;    //Copy parameters
  xfun.IsInterrupt := Interrup;
  //La validación de duplicidad no se puede hacer hasta tener los parámetros.
  TreeElems.AddElementAndOpen(xfun);  //Se abre un nuevo espacio de nombres
  Result := xfun;
  //Crea parámetros en el nuevo espacio de nombres de la función
  if addParam then CreateFunctionParams(xfun.pars);
end;
function TCompilerBase.AddFunctionDEC(funName: string; retTyp: TEleTypeDec;
  const srcPos: TSrcPos; const pars: TxpParFuncArray; Interrup: boolean): TEleFunDec;
{Create a new function, in DECLARATION mode (Forward or Interface) and add it
to the Syntax Tree in the current node. No new node is opened.}
var
  xfundec: TEleFunDec;
begin
  xfundec := TEleFunDec.Create;
  xfundec.name:= funName;
  xfundec.retType := retTyp;
//  xfun.ClearParams;  //Not necessary.

  xfundec.srcDec := srcPos;   //Toma ubicación en el código
  xfundec.implem := nil;  //Not yet implemented
  xfundec.pars := pars;   //Copy parameters
  xfundec.IsInterrupt := Interrup;
  TreeElems.AddElement(xfundec);  //Doesn't open the element
  Result := xfundec;
  //Note that variables for parameters are not created here.
end;
function TCompilerBase.AddFunctionIMP(funName: string; retTyp: TEleTypeDec;
  const srcPos: TSrcPos; functDeclar: TEleFunDec; addParam: boolean): TEleFun;
{Create a new function, in IMPLEMENTATION mode (Forward or Interface) and add it
to the Syntax Tree in the current node. }
var
  xfun: TEleFun;
  tmp: TxpListCallers;
begin
  xfun := CreateFunction(funName, retTyp);
  xfun.srcDec := srcPos;       //Take position in code.
  functDeclar.implem := xfun;  //Complete reference
  xfun.declar := functDeclar;  //Reference to declaration
  xfun.pars := functDeclar.pars;     //Copy from declaration
  xfun.IsInterrupt := functDeclar.IsInterrupt; //Copy from declaration
  //La validación de duplicidad no se puede hacer hasta tener los parámetros.
  TreeElems.AddElementAndOpen(xfun);  //Se abre un nuevo espacio de nombres
  Result := xfun;
  //Crea parámetros en el nuevo espacio de nombres de la función
  if addParam then CreateFunctionParams(xfun.pars);
  //Pass calls list form declaration to implementation.
  tmp := functDeclar.lstCallers;
  functDeclar.lstCallers := xfun.lstCallers;
  xfun.lstCallers := tmp;
  //New calls will be added to implementation since now.
end;
//Containers
procedure TCompilerBase.RefreshAllElementLists;
begin
  TreeElems.RefreshAllUnits;   //Caso especial
end;
procedure TCompilerBase.RemoveUnusedFunc;
{Explora las funciones, para quitarle las referencias de funciones no usadas.
Para que esta función trabaje bien, debe haberse llamado a RefreshAllElementLists(). }
  function RemoveUnusedFuncReferences: integer;
  {Explora las funciones, para quitarle las referencias de funciones no usadas.
  Devuelve la cantidad de funciones no usadas.
  Para que esta función trabaje bien, debe estar actualizada "TreeElems.AllFuncs". }
  var
    fun, fun2: TEleFun;
  begin
    Result := 0;
    for fun in TreeElems.AllFuncs do begin
      if fun.nCalled = 0 then begin
        inc(Result);   //Lleva la cuenta
        //Si no se usa la función, tampoco sus elementos locales
        fun.SetElementsUnused;  //Elements are in Implementation
        //También se quita las llamadas que hace a otras funciones
        for fun2 in TreeElems.AllFuncs do begin
          fun2.RemoveCallsFrom(fun.BodyNode);
//          debugln('Eliminando %d llamadas desde: %s', [n, fun.name]);
        end;
      end;
    end;
  end;
var
  noUsed, noUsedPrev: Integer;
begin
  //Explora las funciones, para identifcar a las no usadas
  //Se requieren varias iteraciones.
  noUsed := 0;
  repeat  //Explora en varios niveles
    noUsedPrev := noUsed;   //valor anterior
    noUsed := RemoveUnusedFuncReferences;
  until noUsed = noUsedPrev;
end;
procedure TCompilerBase.RemoveUnusedVars;
{Explora las variables de todo el programa, para detectar las que no son usadas
(quitando las referencias que se hacen a ellas)).
Para que esta función trabaje bien, debe haberse llamado a RefreshAllElementLists()
y a RemoveUnusedFunc(). }
  function RemoveUnusedVarReferences: integer;
  {Explora las variables de todo el programa, de modo que a cada una:
  * Le quita las referencias hechas por variables no usadas.
  Devuelve la cantidad de variables no usadas.}
  var
    xvar, xvar2: TEleVarDec;
    fun: TEleFun;
  begin
    Result := 0;
    {Quita, a las variables, las referencias de variables no usadas.
    Una referencia de una variable a otra se da, por ejemplo, en el caso:
    VAR
      STATUS_IRP: bit absolute STATUS.7;
    En este caso, la variable STATUS_IRP, hace referencia a STATUS.
    Si STATUS_IRP no se usa, esta referencia debe quitarse.
    }
    for xvar in TreeElems.AllVars do begin
      if xvar.nCalled = 0 then begin
        //Esta es una variable no usada
        inc(Result);   //Lleva la cuenta
        //Quita las llamadas que podría estar haciendo a otras variables
        for xvar2 in TreeElems.AllVars do begin
          xvar2.RemoveCallsFrom(xvar);
//            debugln('Eliminando llamada a %s desde: %s', [xvar2.name, xvar.name]);
        end;
      end;
    end;
    //Ahora quita las referencias de funciones no usadas
    for fun in TreeElems.AllFuncs do begin
      if fun.nCalled = 0 then begin
        //Esta es una función no usada
        inc(Result);   //Lleva la cuenta
        for xvar2 in TreeElems.AllVars do begin
          xvar2.RemoveCallsFrom(fun.BodyNode);
//          debugln('Eliminando llamada a %s desde: %s', [xvar2.name, xvar.name]);
        end;
      end;
    end;
  end;
var
  noUsed, noUsedPrev: Integer;
begin
  noUsed := 0;
  repeat  //Explora en varios niveles
    noUsedPrev := noUsed;   //valor anterior
    noUsed := RemoveUnusedVarReferences;
  until noUsed = noUsedPrev;   //Ya no se eliminan más variables
end;
procedure TCompilerBase.RemoveUnusedCons;
{Explora las constantes de todo el programa, para detectar las que no son usadas
(quitando las referencias que se hacen a ellas)).
Para que esta función trabaje bien, debe haberse llamado a RefreshAllElementLists()
y a RemoveUnusedFunc(). }
  function RemoveUnusedConsReferences: integer;
  {Explora las constantes de todo el programa, de modo que a cada una:
  * Le quita las referencias hechas por constantes no usadas.
  Devuelve la cantidad de constantes no usadas.}
  var
    cons, cons2: TEleConsDec;
    xvar: TEleVarDec;
    fun: TEleFun;
  begin
    Result := 0;
    {Quita, a las constantes, las referencias de constantes no usadas.
    Una referencia de una constante a otra se da, por ejemplo, en el caso:
    CONST
      CONST_2 = CONST_1 + 1;
    En este caso, la constante CONST_2, hace referencia a CONST_1.
    Si CONST_2 no se usa, esta referencia debe quitarse.
    }
    for cons in TreeElems.AllCons do begin
      if cons.nCalled = 0 then begin
        //Esta es una constante no usada
        inc(Result);   //Lleva la cuenta
        //Quita las llamadas que podría estar haciendo a otras constantes
        for cons2 in TreeElems.AllCons do begin
          cons2.RemoveCallsFrom(cons);
//            debugln('Eliminando llamada a %s desde: %s', [cons2.name, cons.name]);
        end;
      end;
    end;
    {Si se incluye la posibilidad de definir variables a partir de constantes,
    como en:
    VAR mi_var: byte absolute CONST_DIR;
    Entonces es necesario este código:}
    for xvar in TreeElems.AllVars do begin
      if xvar.nCalled = 0 then begin
        //Esta es una variable no usada
        inc(Result);   //Lleva la cuenta
        //Quita las llamadas que podría estar haciendo a constantes
        for cons2 in TreeElems.AllCons do begin
          cons2.RemoveCallsFrom(xvar);
//debugln('Eliminando llamada a %s desde: %s', [cons2.name, xvar.name]);
        end;
      end;
    end;
    //Ahora quita las referencias de funciones no usadas
    for fun in TreeElems.AllFuncs do begin  { TODO : Una forma más óptima sería considerar solo las funciones del programa o unidad actual, porque las funciones dentro de unidades (USES ...), no pueden llamar a funciones del programa o unidad actual. }
      if fun.BodyNode = nil then continue;   //Funciones INLINE
      if fun.nCalled = 0 then begin
        //Esta es una función no usada
        inc(Result);   //Lleva la cuenta
        for cons2 in TreeElems.AllCons do begin
//debugln('Eliminando llamada a %s desde func.no usada: %s', [cons2.name, fun.name+':'+fun.srcDec.RowColString]);
          cons2.RemoveCallsFrom(fun.BodyNode);
        end;
      end;
    end;
  end;
var
  noUsed, noUsedPrev: Integer;
begin
  noUsed := 0;
  repeat  //Explora en varios niveles
    noUsedPrev := noUsed;   //valor anterior
    noUsed := RemoveUnusedConsReferences;
  until noUsed = noUsedPrev;   //Ya no se eliminan más constantes
end;
procedure TCompilerBase.RemoveUnusedTypes;
{Explora los tipos (definidos por el usuario) de todo el programa, para detectar
los que no son usados (quitando las referencias que se hacen a ellos)).
Para que esta función trabaje bien, debe haberse llamado a RefreshAllElementLists()
y a RemoveUnusedFunc(). }
  function RemoveUnusedTypReferences: integer;
  {Explora los tipos de todo el programa, de modo que a cada uno:
  * Le quita las referencias hechas por constantes, variables, tipos y funciones no usadas.
  Devuelve la cantidad de tipos no usados.
  ////////// POR REVISAR ///////////}
  var
    cons: TEleConsDec;
    xvar: TEleVarDec;
    xtyp, xtyp2{, ntyp}: TEleTypeDec;
    fun : TEleFun;
  begin
//TreeElems.OpenElement(TreeElems.main);
//ntyp := TEleTypeDec(TreeElems.FindFirst('tarr1'));
    Result := 0;
    {Quita, a los tipos, las referencias de constantes no usadas (de ese tipo).}
    for cons in TreeElems.AllCons do begin
      if cons.nCalled = 0 then begin
        //Esta es una constante no usada
        inc(Result);   //Lleva la cuenta
        //Quita las llamadas que podría estar haciendo a otras constantes
        for xtyp in TreeElems.AllTypes do begin
          xtyp.RemoveCallsFrom(cons);
//if xtyp.name='tarr1' then debugln('Eliminando llamada a %s desde: %s', [xtyp.name, cons.name]);
        end;
      end;
    end;
//debugln('ntyp.lstCallers=%d', [ntyp.lstCallers.Count]);
    {Quita, a los tipos, las referencias de variables no usadas (de ese tipo).}
    for xvar in TreeElems.AllVars do begin
      if xvar.nCalled = 0 then begin
        //Esta es una variable no usada
        inc(Result);   //Lleva la cuenta
        //Quita las llamadas que podría estar haciendo a constantes
        for xtyp in TreeElems.AllTypes do begin
          xtyp.RemoveCallsFrom(xvar);
//if xtyp.name='tarr1' then debugln('Eliminando llamada a %s desde: %s', [xtyp.name, xvar.name]);
        end;
      end;
    end;
//debugln('ntyp.lstCallers=%d', [ntyp.lstCallers.Count]);
    {Quita, a los tipos, las referencias de otros tipos no usadas.
    Como en los tipos que se crean a partir de otros tipos}
    for xtyp2 in TreeElems.AllTypes do begin
      if xtyp2.nCalled = 0 then begin
        //Esta es una variable no usada
        inc(Result);   //Lleva la cuenta
        //Quita las llamadas que podría estar haciendo a constantes
        for xtyp in TreeElems.AllTypes do begin
          xtyp.RemoveCallsFrom(xtyp2);
//if xtyp.name='tarr1' then debugln('Eliminando llamada a %s desde: %s', [xtyp.name, xtyp2.name]);
        end;
      end;
    end;
//debugln('ntyp.lstCallers=%d', [ntyp.lstCallers.Count]);
    //Ahora quita las referencias de funciones no usadas (de ese tipo)
    for fun in TreeElems.AllFuncs do begin
      if fun.nCalled = 0 then begin
        //Esta es una función no usada
        inc(Result);   //Lleva la cuenta
        for xtyp in TreeElems.AllTypes do begin
          if fun.BodyNode = nil then continue;  { TODO : ¿Las funciones sin cuerpo, como las del sistema deberían generar llamadas? }
          xtyp.RemoveCallsFrom(fun.BodyNode);
if xtyp.name='tarr1' then debugln('Eliminando llamada a %s desde: %s', [xtyp.name, cons.name]);
        end;
      end;
    end;
//debugln('ntyp.lstCallers=%d', [ntyp.lstCallers.Count]);
  end;
var
  noUsed, noUsedPrev: Integer;
begin
  noUsed := 0;
  repeat  //Explora en varios niveles
    noUsedPrev := noUsed;   //valor anterior
    noUsed := RemoveUnusedTypReferences;
//debugln('---noUsed=%d',[noUsed]);
  until noUsed = noUsedPrev;   //Ya no se eliminan más constantes
end;
procedure TCompilerBase.UpdateCallersToUnits;
{Explora recursivamente el arbol de sintaxis para encontrar (y agregar) las
llamadas que se hacen a una unidad desde el programa o unidad que la incluye.
El objetivo final es determinar los accesos a las unidades.}
  procedure ScanUnits(nod: TxpElement);
  var
    ele, eleInter , elemUnit: TxpElement;
    uni : TEleUnit;
    cal , c: TxpEleCaller;
  begin
//debugln('+Scanning in:'+nod.name);
    if nod.elements<>nil then begin
      for ele in nod.elements do begin
        //Solo se explora a las unidades
        if ele.idClass = eleUnit then begin
//debugln('  Unit:'+ele.name);
          //"ele" es una unidad de "nod". Verifica si es usada
          uni := TEleUnit(ele);    //Accede a la unidad.
          uni.ReadInterfaceElements; //Accede a sus campos
          {Buscamos por los elementos de la interfaz de la unidad para ver si son
           usados}
          for eleInter in uni.InterfaceElements do begin
//debugln('    Interface Elem:'+eleInter.name);
            //Explora por los llamadores de este elemento.
            for cal in eleInter.lstCallers do begin
              elemUnit := cal.CallerUnit;   //Unidad o programa
              if elemUnit = nod then begin
                {Este llamador está contenido en "nod". Lo ponemos como llamador de
                la unidad.}
                c := AddCallerToFromCurr(uni);
                c.caller := cal.caller;
                c.curPos := cal.curPos;
//                debugln('      Added caller to %s from %s (%d,%d)',
//                        [uni.name, c.curPos.fil, c.curPos.row, c.curPos.col]);
              end;
            end;
          end;
          //Ahora busca recursivamente, por si la unidad incluye a otras unidades
          ScanUnits(ele);  //recursivo
        end;
      end;
    end;
  end;
begin
  ScanUnits(TreeElems.main);
end;
procedure TCompilerBase.UpdateFunLstCalled;
{Actualiza la lista lstCalled de las funciones, para saber, a qué fúnciones llama
 cada función.}
var
  fun    : TEleFun;
  itCall : TxpEleCaller;
  whoCalls: TEleBody;
  n: Integer;
begin
  for fun in TreeElems.AllFuncs do begin
    if fun.nCalled = 0 then continue;  //No usada
    //Procesa las llamadas hechas desde otras funciones, para llenar
    //su lista "lstCalled", y así saber a quienes llama.
    for itCall in fun.lstCallers do begin
      //Agrega la referencia de la llamada a la función
      if not (itCall.caller.idClass in [eleBody, eleConsDec, eleVarDec]) then begin
        //Según diseño, itCall.caller debe ser alguno de los elementos válidos.
        GenError('Design error.');
        exit;
      end;
      whoCalls := TEleBody(itCall.caller);
      //Se agregan todas las llamadas (así sean al mismo porcedimiento) pero luego
      //AddCalled(), los filtra.
      whoCalls.Parent.AddCalled(fun);  //Agrega al procediminto
    end;
  end;
  {Actualizar la lista fun.lstCalledAll con la totalidad de llamadas a todas
   las funciones, sean de forma directa o indirectamente.}
  for fun in TreeElems.AllFuncs do begin
    n := fun.UpdateCalledAll;
    if n<0 then begin
      GenError('Recursive call or circular recursion in %s', [fun.name], fun.srcDec);
    end;
  end;
  if HayError then exit;
  //Actualiza el programa principal
  TreeElems.main.UpdateCalledAll;  //No debería dar error de recursividad, porque ya se verificaron las funciones
  if TreeElems.main.maxNesting>128 then begin
    {Stack is 256 bytes size, and it could contain 128 max. JSR calls, without
    considering stack instructions.}
    GenError('Not enough stack.');
  end;
end;
procedure TCompilerBase.SeparateUsedFunctions;
{Performs two tasks:
1. Fill the list usedFuncs with all used functions, including Interrupt function.
2. Set interruptFunct to point to the interrupt function (Only one).
Must be called after call to RemoveUnusedFunc().}
var
  fun : TEleFun;
begin
  usedFuncs.Clear;
  unusedFuncs.Clear;
  interruptFunct := nil;
  for fun in TreeElems.AllFuncs do begin
    if fun.nCalled>0 then usedFuncs.Add(fun) else unusedFuncs.Add(fun);
    if fun.IsInterrupt then interruptFunct := fun;
  end;
end;
function TCompilerBase.CompatibleTypes(typ1, typ2: TEleTypeDec): boolean;
{Indicates if the types are equals or similar.}
begin
  if typ1 = typ2 then exit(true);
  if (typ1.copyOf<>nil) and (typ1.copyOf = typ2) then exit(true);
  if (typ2.copyOf<>nil) and (typ2.copyOf = typ1) then exit(true);
  {No se verifica la consistencia en el número de ítems aquí, porque el número de ítems
  podría no estar definido en esta fase, como cuando se define así:
  CONST
    CONST1 = 1;
    CONST2 = CONST1 + 1;
  VAR
     a: array[CONST2] of char;
  De todas formas se realizará una verificación posteriormente.
  }
  if (typ1.catType = tctArray) and (typ2.catType = tctArray) and
     (typ1.itmType = typ2.itmType) {and (typ1.nItems = typ2.nItems)}
  then begin
     exit(true);
  end;
  exit(false);
end;
//Array utilities
function TCompilerBase.AddConstDeclarByte(decName: string;
                                      consVal: integer): TEleConsDec;
{Add a constant declaration of type byte, named "decName" containing a constant element
"n" set to the value "consVal".
The constant declaration is added to the current node in the AST.}
var
  consDec: TEleConsDec;
  expr: TEleExpress;
begin
  consDec := AddConsDecAndOpen(decName, typByte, GetSrcPos);
  if HayError then exit;  //Can be duplicated?
  expr := AddExpressionConstByte('n', consVal, GetSrcPos);
  //consDec.typ := expr.Typ;  no needed
  consDec.value := @expr.value;
  consDec.evaluated := true;
  TreeElems.CloseElement;  //Close constant.
  exit(consDec);
end;
function TCompilerBase.GetConstantArray(arrDelimt: char): TEleExpress;
{Get an array literal, like [1,2,3] or ('a','b').
Paraemters
* "arrDelimt"     -> Is the ending delimiter of the array. The frrst delimiter is not
                     checked here.
}
var
  srcpos: TSrcPos;
  ReadType, endWithComma: Boolean;
  Op, Op1: TEleExpress;
  itType, xtyp: TEleTypeDec;
  nElem: Integer;
  typName: String;
begin
  srcpos := GetSrcPos;
  Next;  //Get '['
  ProcComments;
  //Start reading the items
  ReadType := true;  //Set flag to read the item type
  Op := AddExpressAndOpen(token, typNull, otConst, srcPos);
  Op.evaluated := true;  //We have the value directly.
  Op.value.InitItems;
  while not atEof and (token <> arrDelimt) do begin
    //Must be an item
    Op1 := GetExpression(0);  //read item
    if HayError then begin
      Op.value.CloseItems;  //Resize
      exit(Op);
    end;
    if Op1.Sto <> stConst then begin
      GenError('Expected constant item');
      exit;
    end;
    if ReadType then begin
       //First item
       itType := Op1.Typ;  //Now We have the type of the item
       ReadType := false;  //Already read
    end;
    //Asure all items have the same type
    if Op1.Typ <> itType then begin
      GenError('Expected item of type: %s', [itType.name]);
      Op.value.CloseItems;  //Resize
      exit(Op);
      //endWithComma := false;
      //break;  //To finish cleaning the house.
    end;
    //Add the item to the operand
    Op.value.AddConsItem(Op1.Value);
    //Verify delimiter
    endWithComma := false;
    if token = ',' then begin
      Next;
      ProcComments;
      endWithComma := true;
    end;
  end;
  if endWithComma then begin
    GenError('Expected item.');
    exit(Op);
  end;
  if token = arrDelimt then begin
    //Raise the end of array. Now we can create new type.
    Op.value.CloseItems;  //Resize
    //Now we can know the type of the item and of the array
    Next;  //Take ']' or ')'.
    nElem := Op.Value.nItems;
    if nElem = 0 then itType := typNull;  //Something like []
    if not TreeElems.ExistsArrayType(itType, nElem, xtyp) then begin
      //The type doesn't exist. We need to create.
      typName := GenArrayTypeName(itType.name, nElem); //Op.nItems won't work
      xtyp := CreateArrayTypeDec(typName, nElem, itType, srcpos);
    end;
    //Finally we set the operand type.
    Op.Typ := xtyp;
  end else if atEof then begin
    GenError('Unexpected end of file');
    exit(Op);
  end else begin  //Only happen when break loop (Error)
    Op.value.CloseItems;  //Resize
    Op.Typ := xtyp;
  end;
  exit(Op);
end;
function TCompilerBase.GetConstantArrayStr(allowChar: boolean = true): TEleExpress;
var
  Op1: TEleExpress;
  srcpos: TSrcPos;
  nElem: SizeInt;
  str, typName: String;
  ascCode: Longint;
  xtyp: TEleTypeDec;
begin
  srcpos := GetSrcPos;
  nElem := length(token) - 2;  //Don't consider quotes
  str := copy(token, 2, nElem);
  Next;    //Pasa al siguiente
  while tokType = tkChar do begin  //like #255
    //Concat the next char to simulate concat, considering there is not a
    //string type.
    if TryStrToInt(Copy(token,2,3), ascCode) then begin
      str += chr(ascCode and $FF);
      Next;    //Pasa al siguiente
      inc(nElem);
    end else begin
      GenError(ER_IN_CHARACTER);   //Casi seguro que es el caracter "#" solo.
      exit;
    end;
  end;
  {$IFDEF LogExpres} Op.txt:= cIn.tok; {$ENDIF}   //toma el texto
  if allowChar and (length(str) = 1) then begin
    //De un caracter. Se asume de tipo Char
    Op1 := AddExpressAndOpen(token, typChar, otConst, srcPos);
    Op1.value.ValInt := ord(STR[1]);
    Op1.evaluated := true;
  end else begin
    //Will be considered as array of char
    if not TreeElems.ExistsArrayType(typChar, nElem, xtyp) then begin
      //There is not a similar type. We create a new type.
      typName := GenArrayTypeName('char', nElem); //Op.nItems won't work
      xtyp := CreateArrayTypeDec(typName, nElem, typChar, srcpos);

    end;
    Op1 := AddExpressAndOpen('str', xtyp, otConst, srcPos);
    AddCallerToFromCurr(xtyp);
    Op1.StringToArrayOfChar(str);
    Op1.evaluated := true;
  end;
  exit(Op1);
end;
//Expressions
function TCompilerBase.MethodFromBinOperator(const OpType: TEleTypeDec; Op: string;
  OpType2: TEleTypeDec): TEleFunBase;
var
  xfun: TEleFunBase;
  att: TxpElement;
begin
{Find a method in the class "OpType" associated to a binary operator "Op", whose unique
parameter is of type "OpType2".
If not a matching method found, returns NIL.
This function is designed to be used in cnjunction with FindFirstMethodFromOperator().
}
  Op := UpCase(Op);
  if OpType.elements = nil then exit(nil);
  for att in OpType.elements do begin
    if att.idClass in [eleFunc, eleFuncDec] then begin  //Only for methods
      xfun := TEleFunBase(att);
      if (xfun.oper = Op) and
         (length(xfun.pars) = 2) and  //Binary methods have 2 parameters.
         CompatibleTypes(xfun.pars[1].typ, OpType2) {(xfun.pars[1].typ = OpType2)}
      then begin  //Second parameter muts match OpyYpe2
        exit(xfun);
      end;
    end;
  end;
  //Not found
  exit(nil);
end;
function TCompilerBase.MethodFromUnaOperator(const OpType: TEleTypeDec; Op: string
  ): TEleFunBase;
var
  xfun: TEleFunBase;
  att: TxpElement;
begin
{Find a method in the class "OpType" associated to a unary operator "Op".
If not a matching method found, returns NIL.
}
  Op := UpCase(Op);
  for att in OpType.elements do begin
    if att.idClass in [eleFunc, eleFuncDec] then begin  //Only for methods
      xfun := TEleFunBase(att);
      if (xfun.oper = Op) and
         (length(xfun.pars) = 1) then begin  //Unary methods have 1 parameter.
        exit(xfun);
      end;
    end;
  end;
  //Not found
  exit(nil);
end;
function TCompilerBase.GetOperand(): TEleExpress;
{Get an "Operand" element of the Pascal language, from the current context.
If an operand is obtained, it is added to the current node in the AST as a TxpEleExpress
(and set as active node), and the reference is returned in this function.
If an operand is not found, an error is generated, a NIL value is returned and the AST
can be modified or not, depending on the point where the error is raised.

--> [identifier] ----+ <---------------------------------- +
                     +--> '[' --> [expression] --> ']' --> +
                     +--> '.' --> [field identifier] ----> +
                     +----------------> '^' -------------> +
                     |
                     +--------------------------------------->

The operand read is added to the syntax tree, as a TxpEleExpress element, and returned
in this function.

}
  function ResolveFunction(const pars: TxpParFuncArray;   //Parameters
           xfun: TEleFunBase;   //First function found in the Syntax Tree.
           searchState: TxpFindState): TEleFunBase;
  {Identifies the function that match with the parameters}
  var
    firstFunc: TEleFunBase;
  begin
    TreeElems.curFind := searchState;  //Restore previous Finding, to continue the searching.
    firstFunc := xfun;  //Save reference to original function.
    repeat
      if xfun.SameParamsType(pars) then break;
      //Usar FindNextFunc, es la forma es eficiente, porque retoma la búsqueda anterior.
      xfun := TreeElems.FindNextFuncName;
    until xfun = nil;
    if xfun = nil then begin
      //None of the versions match the parameters.
      GenError(ER_TYP_PARM_ER_, [firstFunc.name + '()']);
      exit(nil);
    end;
    //Final function is identified here.
    {$IFDEF LogExpres} Op.txt:= cIn.tok; {$ENDIF} //Toma el texto
    Result := xfun;
  end;
var
  xvar: TEleVarDec;
  xcon: TEleConsDec;
  eleMeth, Op1: TEleExpress;
  level: Integer;
  ele, field: TxpElement;
  posCall: TSrcPos;
  pars: TxpParFuncArray;
  xfun: TEleFunBase;
  findState: TxpFindState;
  upTok: String;
  value: TConsValue;
  typ: TEleTypeDec;
  cod: Longint;
  opr1: TEleFun;
begin
  SkipWhites;
  upTok := UpCase(token);
  if tokType = tkLitNumber then begin
    TipDefecNumber(typ, value, token); //encuentra tipo de número, tamaño y valor
    if HayError then exit;  //verifica
    Op1 := AddExpressAndOpen(token, typ, otConst, GetSrcPos);
    Op1.evaluated := true;  //We have the value directly.
    Op1.value := value;
    Next;    //Pasa al siguiente
  end else if upTok = 'TRUE' then begin  //Constant boolean.
    Op1 := AddExpressAndOpen('T', typBool, otConst, GetSrcPos);
    Op1.value.ValBool := True;
    Op1.evaluated := true;
    Next;    //Pasa al siguiente
  end else if upTok = 'FALSE' then begin  //Constant boolean.
    Op1 := AddExpressAndOpen('F', typBool, otConst, GetSrcPos);
    Op1.value.ValBool := False;
    Op1.evaluated := true;
    Next;    //Pasa al siguiente
  end else if toktype = tkIdentifier then begin
    ele := TreeElems.FindFirst(token);  //Identify element
    findState := TreeElems.curFind;  //Save because can be altered with CaptureParams()
    if ele = nil then begin
      //Unidentified element.
      GenError(ER_UNKNOWN_IDE_, [token]);
      exit;
    end;
    if ele.idClass = eleConsDec then begin  //Is constant
      xcon := TEleConsDec(ele);
      AddCallerToFromCurr(ele);
      Op1 := AddExpressAndOpen(ele.name, xcon.Typ, otConst, GetSrcPos);
      if xcon.evaluated then begin
        //The constant is already calculated. We can obtain the value.
        Op1.evaluated := true;
        Op1.value := xcon.value^;
      end else begin
        //No yet calculated. Maybe "xcon" depends on an expression.
        Op1.evaluated := false;
        Op1.cons := xcon;  //Keep reference to calculate later.
      end;
      Next;    //Pasa al siguiente
    end else if ele.idClass = eleVarDec then begin  //Is variable
      xvar := TEleVarDec(ele);
      AddCallerToFromCurr(ele); //Add reference to variable, however final operand can be: <variable>.<fieldName>
      Op1 := AddExpressAndOpen(ele.name, xvar.Typ, otVariab, GetSrcPos);
      Op1.SetVariab(xvar);
      Next;    //Pasa al siguiente
    end else if ele.idClass in [eleFunc, eleFuncDec] then begin  //Is function
      {It's a function (or procedure), but we don't know what's the exact funtion because
      could be different overload versions.}
      posCall := GetSrcPos;  //Save position of the call.
      Next;               //Take identifier
      SkipWhites;         //Take spaces
      xfun := TEleFunBase(ele);  //The ancestor of eleFunc and eleFuncDec
      {We create the expression here because we're going to create parameters nodes
      when scanning with CaptureParams()}
      Op1 := AddExpressAndOpen(ele.name, xfun.retType, otFunct, posCall);
      //Op1.Sto := ; { TODO : ¿No es necesario completar el almacenamiento de esta función? }
      //Capture parameters
      CaptureParams(pars);    //Read parameters in "pars".
      if HayError then exit;
      //Resolve final function called, acording to parameters.
      xfun := ResolveFunction(pars, xfun, findState);
      if HayError then exit;
      AddCallerToFromCurr(xfun).curPos := posCall;  {Fix call position, otherwise will be pointing to the
                                   end of parameters}
      Op1.rfun := xfun;  //We can now set the final function.
    end else begin
      //Operand expected
      GenError(ER_OPERAN_EXPEC);
      exit;
    end;
  end else if tokType = tkChar then begin  //Constant character
    {$IFDEF LogExpres} Op.txt:= cIn.tok; {$ENDIF}   //toma el texto
    if not TryStrToInt(copy(token, 2), cod) then begin
      GenError(ER_IN_CHARACTER);   //tal vez, sea muy grande
      exit;
    end;
    if (cod<0) or (cod>255) then begin
      GenError(ER_INV_COD_CHAR);
      exit;
    end;
    Op1 := AddExpressAndOpen(token, typChar, otConst, GetSrcPos);
    Op1.value.ValInt := cod;
    Op1.evaluated := true;
    Next;
  end else if tokType = tkString then begin  //Constant String
    Op1 := GetConstantArrayStr();
    if HayError then exit;
  end else if token = '(' then begin  // (...)
    Next;
    Op1 := GetExpression(0);
    if HayError then exit;
    if token <> ')' then begin
       GenError(ER_IN_EXPRESSI);
       exit;
    end;
    Next;
  end else if token = '[' then begin  //Constant array
    //Here we only know the operand is an array
    Op1 := GetConstantArray(']');
  end else begin
    //Operand expected
    Op1 := nil;
    GenError(ER_OPERAN_EXPEC);
    exit;
  end;
  level := 1;   //Count the deeps of methods
  //Verify if has reference to fields with "."
  while (length(token)=1) and (token[1] in ['.','[','^']) do begin
    //There is a modifier
    if token = '.' then begin
      Next;    //Takes "."
      if (tokType<>tkIdentifier) and (tokType<>tkLitNumber) then begin { TODO : No es necesario explorar ambos casos. SOlo hay que saber bien cual se usa en los tipos }
        GenError('Identifier expected.');
        Next;    //Pasa siempre
        exit;
      end;
      //There is an identifier. It should be a method or attribute. Let's find.
      if not Op1.Typ.FindElemName(token, field) then begin
        //There are not fields for this type
        GenError(ER_UNKNOWN_IDE_, [token]);
        exit;
      end;
      //Found the field. Create an expression node.
      if field.idClass = eleConsDec then begin
        xcon := TEleConsDec(field);
        AddCallerToFromCurr(field);
        //We rather convert all to a constant
        eleMeth := Op1;
        eleMeth.opType := otConst;
        eleMeth.Sto    := stConst;
        eleMeth.Typ    := xcon.typ;
        if xcon.evaluated then begin
          //The constant is already calculated. We can obtain the value.
          eleMeth.evaluated := true;
          eleMeth.value := xcon.value^;
        end else begin
          //No yet calculated. Maybe "xcon" depends on an expression.
          eleMeth.evaluated := false;
          eleMeth.cons := xcon;  //Keep reference to calculate later.
        end;
        Next;   //Take the identifier
      end else if field.idClass = eleVarDec then begin
        xvar := TEleVarDec(field);
        //AddCallerTo(field);  { TODO : ¿Es necesario? }
        eleMeth := CreateExpression(token, xvar.typ, otVariab, GetSrcPos);
        //TreeElems.AddElementAndOpen(eleMeth);
        TreeElems.InsertParentTo(eleMeth, Op1);
        Next;   //Take the identifier
      end else if field.idClass in [eleFuncDec, eleFunc] then begin
        {It's a method, but we don't know what's the exact method because
        could be different overload versions.}
        posCall := GetSrcPos;   //Save position of the call.
        Next;               //Take identifier
        SkipWhites;         //Take spaces
        xfun := TEleFunBase(field);  //The ancestor of eleFunc and eleFuncDec
        eleMeth := CreateExpression(field.name, xfun.retType, otFunct, posCall);
        TreeElems.InsertParentTo(eleMeth, Op1);
        TreeElems.OpenElement(eleMeth);  //Set parent to add parameters.
        eleMeth.rfun := xfun;            //Set function
        //Capture parameters
        CaptureParams(pars);    //Read parameters in "pars".
        if HayError then exit;
      end else begin
        { TODO : Formally must be something like "Cannot use this here" }
        GenError(ER_UNKNOWN_IDE_, [token]);
        exit;
      end;
    end else if token='^' then begin
      Next;    //Takes "^".
      //Validates if operand is pointer
      if Op1.Typ.catType <> tctPointer then begin
        GenError('Expression is not a pointer type.');
        exit;
      end;
      //Put element as parent of Op1
      eleMeth := CreateExpression('_ref', typWord, otFunct, GetSrcPos);
      eleMeth.fcallOp := true;  //Come from an operator.
      TreeElems.InsertParentTo(eleMeth, Op1);
      TreeElems.OpenElement(eleMeth);  //Set parent to method to allow add parameters as child node.
      //Get reference to system function _ref().
      opr1 := eleFunRef;  //Take direct reference to avoid call to TreeElems.FindFirst('_ref')
      eleMeth.name := opr1.name;
      eleMeth.rfun := opr1;  //Method for operator.
      //eleMeth.Typ  := opr1.retType;  //Complete returning type.
      eleMeth.Typ := Op1.Typ.ptrType;  //Complete with the type referenced.
      AddCallerToFromCurr(opr1);     //Mark as used.
//      //Prepare next operation.
//      Op1 := eleMeth;   //Set new operand 1
//      TreeElems.OpenElement(Op1.Parent);  //Returns to parent (sentence).
//      SkipWhites;  //Prepares for take next operator.
    end else begin  //Must be '['.
      Next;    //Takes "[".
      if not Op1.Typ.FindElemName('_GETITEM', field) then begin
        //There are not fields for this type
        GenError('Undefined method %s for type %s', ['_getitem()', Op1.Typ.name]);
        exit;
      end;
      if not (field.idClass in [eleFuncDec, eleFunc]) then begin
        GenError('_getitem() should be a method');
        exit;
      end;
      xfun := TEleFunBase(field);  //The ancestor of eleFunc and eleFuncDec
      eleMeth := CreateExpression(field.name, xfun.retType, otFunct, GetSrcPos);
      TreeElems.InsertParentTo(eleMeth, Op1);
      TreeElems.OpenElement(eleMeth);  //Set parent to add parameters.
      eleMeth.rfun := xfun;            //Set function
      //Capture parameter.
      GetExpression(0);
      if HayError then exit;
      if token<>']' then begin
        GenError('"]" expected.');
        exit;
      end;
      Next;   //Takes ']'.
    end;
    inc(level);
    Op1 := eleMeth;   //Set new operand 1
  end;
  Result := Op1;  //aquí debe haber quedado el resultado
end;
function TCompilerBase.GetExpression(const prec: Integer): TEleExpress;
{Analyze a common Pascal expression, and represent it in the AST, according to the
predefined rules.
In normal case, creates an element TxpEleExpress, in the current node, and returns a
reference to the expression element root.
In some cases like when reading arrays or objects, a new type can be created.
}
var
  Op1, Op2: TEleExpress;
  p: TContextState;
  eleMeth: TEleExpress;
  opr1: TEleFunBase;
  oprTxt: String;
  oprPos: TSrcPos;
  oprPre: Integer;
begin
  ProcComments;
  //------------- Get first operand ---------------
  if tokType = tkOperator then begin
    //First token should be an pre-unary operator.
    p := GetCtxState;     //In the case we need to go back.
    oprPos := GetSrcPos;  //Save start position for operator.
    oprTxt := token;      //Save operator text.
    oprPre := curCtx.tokPrecU; //Read unary operator precedence.
    Next;                 //Pass to the operand
    Op1 := GetExpression(oprPre);  //Takes the operand.
    if HayError then exit;
    //Now we got the type. Now lets check if operator is a method of this type
    opr1 := MethodFromUnaOperator(Op1.Typ, oprTxt);
    if opr1 = nil then begin
      {Este tipo no permite este operador Unario (a lo mejor ni es unario)}
      SetCtxState(p);
      GenError(ER_UND_OPER_TY_, [token, Op1.Typ.name]);
      exit;
    end;
    //Put method as parent
    eleMeth := CreateExpression(opr1.name, opr1.retType, otFunct, oprPos);  //Type will updated later.
    eleMeth.fcallOp := true;  //Come from an operator.
    TreeElems.InsertParentTo(eleMeth, Op1);
    TreeElems.OpenElement(eleMeth);  //Set parent to add parameters.
    eleMeth.rfun := opr1;  //Method for operator.
    Op1 := eleMeth;   //Set new operand 1 (Expression)
  end else begin
    //First token should be a common operand
    Op1 := GetOperand();  //Add operand to the current node.
    if HayError then exit;
  end;
  TreeElems.OpenElement(Op1.Parent);  //Returns to parent (sentence), in the case the expression have only one operand.
  SkipWhites;
  p := GetCtxState; //In the case we need to go back.  { TODO : ¿No debería ir dentro del WHILE? }
  //--------- Start loop: <Operator> <Operand> ----------
  while tokType = tkOperator do begin
    //Follows an operator. Checks if exist.
    oprPos := GetSrcPos;   //Save start position for operator.
    oprTxt := token;       //Save operator text.
    oprPre := curCtx.tokPrec; //Read binary operator precedence.
    //Delimited by precedence?
    if oprPre <= prec Then begin  //Is lower than the following.
      Result := Op1;  //Solo devuelve el único operando que leyó
      SetCtxState(p); //Returns to before reading the operator.
      exit;
    end;
    Next;  //Takes operator
    //Put element as parent of Op1
    eleMeth := CreateExpression('', typNull, otFunct, oprPos);  //Type will be updated later.
    eleMeth.fcallOp := true;  //Come from an operator.
    TreeElems.InsertParentTo(eleMeth, Op1);
    TreeElems.OpenElement(eleMeth);  //Set parent to method to allow add parameters as child node.
    //-------------------- Get second operand --------------------
    Op2 := GetExpression(oprPre);   //toma operando con precedencia
    if HayError then exit;
    //Find the final method (operator).
    opr1 := MethodFromBinOperator(Op1.Typ, oprTxt, Op2.Typ);
    if opr1 = nil then begin   //Operator not found
      GenError('Undefined operation: %s %s %s', [Op1.Typ.name, oprTxt, Op2.Typ.name]);
      exit;
    end;
    eleMeth.name := opr1.name;
    eleMeth.rfun := opr1;  //Method for operator.
    eleMeth.Typ  := opr1.retType;  //Complete returning type.
    AddCallerToFromCurr(opr1);   //Mark as used.
    //Prepare next operation.
    Op1 := eleMeth;   //Set new operand 1
    TreeElems.OpenElement(Op1.Parent);  //Returns to parent (sentence).
    SkipWhites;  //Prepares for take next operator.
  end;  //hasta que ya no siga un operador
  Result := Op1;  //aquí debe haber quedado el resultado
end;
//Files
function TCompilerBase.ExpandRelPathTo(BaseFile, FileName: string): string;
{Convierte una ruta relativa (FileName), a una absoluta, usnado como base la ruta de
otro archivo (BaseFile)}
var
  BasePath: RawByteString;
begin
   //if pos(DirectorySeparator, FileName)=0 then begin
   if (pos('/', FileName)=0) and (pos('\', FileName)=0) then begin
     //Ruta relativa. Se completa
     BasePath := ExtractFileDir(BaseFile);
     if BasePath = '' then begin
       //No hay de donde completar, usa la ruta actual
       Result := ExpandFileName(FileName);
     end else  begin
       Result := ExtractFileDir(BaseFile) + DirectorySeparator + FileName;
     end;
   end else begin
     //Tiene "DirectorySeparator", se asume que es ruta absoluta, y no se cambia.
     Result := FileName;
   end;
end;
function TCompilerBase.ExpandRelPathToMain(FileName: string): string;
{Convert a relative path to absolute path, considering the base path is "mainFile".}
begin
  Result := ExpandRelPathTo(mainFile, FileName);
end;
function TCompilerBase.hexFilePath: string;
begin
  Result := ExpandRelPathTo(mainFile, hexfile); //Convierte a ruta absoluta
end;
function TCompilerBase.mainFilePath: string;
begin
  Result := mainFile;
end;
//Callers methods
function TCompilerBase.AddCallerToFrom(toElem: TxpElement; callerElem: TxpElement): TxpEleCaller;
{General function to add a call to an element of the syntax ("toElem"). The "caller"
element is "callerElem".
Returns the reference to the caller class: TxpEleCaller.
The objective of this function is not to be called directly (that is why it is private)
but rather to use the most specialized functions: AddCallerToFromBody, AddCallerToFromVar
and AddCallerToFromCurBody, to have more control on the types of elements added as
"callers". }
var
  fc: TxpEleCaller;
  fun: TEleFun;
  fundec: TEleFunDec;
begin
  //Creates caller class.
  fc:= TxpEleCaller.Create;
  fc.caller := callerElem;
  fc.curPos := GetSrcPos;  //Can be changed later if not apply.
  if toElem.idClass = eleFunc then begin
    //For implementation of functions, the caller are added directly.
    fun := TEleFun(toElem);
    fun.lstCallers.Add(fc);
  end else if toElem.idClass = eleFuncDec then begin
    {When functions have declaration (INTERFACE or FORWARD), the calls have been added
    to declaration elements (because implementation could not exist yet) but when
    implementation appears, all calls must be passed to implementation.}
    fundec := TEleFunDec(toElem);
    if fundec.implem = nil then begin
      //Not yet implemented
      fundec.lstCallers.Add(fc);
    end else begin
      //Pass call to the function implementation.
      fundec.implem.lstCallers.Add(fc);
    end;
  end else begin
    //Other elements
    toElem.lstCallers.Add(fc);
  end;
  exit(fc);
end;
function TCompilerBase.AddCallerToFromCurr(toElem: TxpElement): TxpEleCaller;
{Add a call to the element "toElem", from the current Code container (If there is one).
Returns the reference to the caller class: TxpEleCaller. }
begin
  Result := AddCallerToFrom(toElem, TreeElems.curCodCont);
end;
//Miscellaneous
function TCompilerBase.GenerateUniqName(base: string): string;
{Generate a unique name using the string "base" as prefix.}
begin
  exit(base + IntToStr(elemCnt));
  inc(elemCnt);
end;
function TCompilerBase.getListOfIdent(out itemList: TStringDynArray; out
  srcPosArray: TSrcPosArray): boolean;
{Lee una lista de identificadores separados por comas, hasta encontra un caracter distinto
de coma. Si el primer elemento no es un identificador o si después de la coma no sigue un
identificador, genera error.
También devuelve una lista de las posiciones de los identificadores, en el código fuente.
Si hay error, devuelve FALSE.}
const
  BLOCK_SIZE = 10;  //Tamaño de bloque de memoria inicial
var
  item: String;
  n, curSize: Integer;
begin
  //Se empieza con un tamaño inicial para evitar muchas llamadas a setlength()
  curSize := BLOCK_SIZE;    //Tamaño inicial de bloque
  setlength(itemList   , curSize);  //Tamaño inicial
  setlength(srcPosArray, curSize);  //Tamaño inicial
  n := 0;
  repeat
    ProcComments;
    //ahora debe haber un identificador
    if tokType <> tkIdentifier then begin
      GenError(ER_IDEN_EXPECT);
      setlength(itemList   , n);
      setlength(srcPosArray, n);
      exit(false);
    end;
    //hay un identificador
    item := token;
    itemList[n] := item;  //agrega nombre
    srcPosArray[n] := GetSrcPos;  //agrega ubicación de declaración
    Next;  //Toma identificador despues, de guardar ubicación
    ProcComments;
    if token <> ',' then break; //sale
    Next;  //Toma la coma
    //Hay otro ítem, verifica límite de arreglo
    inc(n);
    if n >= curSize then begin
      curSize += BLOCK_SIZE;   //Incrementa tamaño en bloque
      setlength(itemList   , curSize);  //hace espacio
      setlength(srcPosArray, curSize);  //hace espacio
    end;
  until false;
  //Define el tamaño final.
  setlength(itemList   , n+1);
  setlength(srcPosArray, n+1);
  exit(true);
end;
procedure TCompilerBase.LogExpLevel(txt: string);
{Genera una cadena de registro , considerando el valor de "ExprLevel"}
begin
  debugln(space(3*ExprLevel)+ txt );
end;
function TCompilerBase.IsTheSameVar(var1, var2: TEleVarDec): boolean; inline;
{Indica si dos variables bit son la misma, es decir que apuntan, a la misma dirección
física}
begin
  Result := (var1.addr = var2.addr);
end;
//Initialization
constructor TCompilerBase.Create;
begin
  inherited;
  ClearError;   //inicia motor de errores
  //Crea arbol de elementos y listas
  TreeElems  := TXpTreeElements.Create;
  ejecProg := false;
  //Containers for functions
  usedFuncs := TEleFuns.Create(false);     //Only references
  unusedFuncs:= TEleFuns.Create(false);
end;
destructor TCompilerBase.Destroy;
begin
  unusedFuncs.Destroy;
  usedFuncs.Destroy;
  TreeElems.Destroy;
  inherited Destroy;
end;

end. //2400, 2886
