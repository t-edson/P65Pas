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
  Classes, SysUtils, Types, fgl, Forms, LCLType, lclProc, SynFacilHighlighter,
  SynEditHighlighter, XpresBas, XpresTypesPIC, XpresElementsPIC, CompOperands,
  MisUtils, CPUCore, Globales;  //Para usar solo Trans()
const
  TIT_BODY_ELE = 'Body';
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
{ TCompilerBase }
{Clase base para crear a los objetos compiladores.
Esta clase debe ser el ancestro común de todos los compialdores a usar en PicPas.
Contiene métodos abstractos que deben ser impleemntados en las clases descendeintes.}
TCompilerBase = class(TCompOperands)
protected  //Flags for boolean type.
  {This variables are reset at the begginig of each ROU or ROB. They contains the state
  of the Register/Satus-flags if the last ROU or ROB is executed.  }
  BooleanFromC: integer; {Flag and index. When <>0, indicates the boolean expression result
                          was obtained, in the last expression, using the bit C and moved
                          to A. Used for code optimization.}
  BooleanFromZ: integer; {Flag and index. When <>0, indicates the boolean expression result
                          was obtained, in the last expression, using the bit Z and moved
                          to A. Used for code optimization.}
  AcumStatInZ: boolean;  {Indicates the Z flag contains the status of the value in A
                          register. For example if A = 0, Z wil be 1.
                          }
protected  //Elements creation
  procedure ClearSystemTypes;
  function CreateSysType(nom0: string; cat0: TTypeGroup; siz0: smallint
    ): TxpEleType;
  function FindSysEleType(TypName: string): TxpEleType;
  function CreateCons(consName: string; eletyp: TxpEleType): TxpEleCon;
  function CreateVar(varName: string; eleTyp: TxpEleType): TxpEleVar;
  function CreateEleType(const typName: string; const srcPos: TSrcPos;
                         catType: TxpCatType): TxpEleType;
  function CreateEleTypeArray(const typName: string; const srcPos: TSrcPos;
                         itmType: TxpEleType; nItems: integer ): TxpEleType;
  function CreateEleTypePtr(const typName: string; const srcPos: TSrcPos;
                         ptrType: TxpEleType): TxpEleType;
  function CreateEleTypeObject(const typName: string; const srcPos: TSrcPos): TxpEleType;
  function CreateFunction(funName: string; typ: TxpEleType;
    procParam: TxpProcParam; procCall: TxpProcCall): TxpEleFun;
  function CreateSysFunction(funName: string; procParam: TxpProcParam;
    procCall: TxpProcCall): TxpEleFun;
  function AddVariable(varName: string; eleTyp: TxpEleType; srcPos: TSrcPos
    ): TxpEleVar;
  function AddConstant(conName: string; eleTyp: TxpEleType; srcPos: TSrcPos
    ): TxpEleCon;
  procedure CreateFunctionParams(var funPars: TxpParFuncArray);
  function AddFunctionUNI(funName: string; retTyp: TxpEleType; const srcPos: TSrcPos;
    const pars: TxpParFuncArray; Interrup: boolean): TxpEleFun;
  function AddFunctionDEC(funName: string; retTyp: TxpEleType;
    const srcPos: TSrcPos; const pars: TxpParFuncArray; Interrup: boolean
  ): TxpEleFunDec;
  function AddFunctionIMP(funName: string; retTyp: TxpEleType;
    const srcPos: TSrcPos; functDeclar: TxpEleFunDec): TxpEleFun;
  function AddInline(funName: string; eleTyp: TxpEleType; const srcPos: TSrcPos;
    const pars: TxpParInlinArray; procParam, procCall: TProcExecFunction
  ): TxpEleInlin;
protected
  ExprLevel  : Integer;  //Nivel de anidamiento de la rutina de evaluación de expresiones
  RTstate    : TxpEleType;    {Estado de los RT. Si es NIL, indica que los RT, no tienen
                         ningún dato cargado, sino indican el tipo cargado en los RT.}
  function CaptureDelExpres: boolean;
  procedure ProcCommentsNoExec;
  procedure ProcComments;
  procedure TipDefecNumber(var Op: TOperand; toknum: string);
  procedure TipDefecString(var Op: TOperand; tokcad: string);
  procedure TipDefecBoolean(var Op: TOperand; tokcad: string);
  function EOExpres: boolean;
  function EOBlock: boolean;
  procedure CaptureParamsFinal(var funPars: TxpParFUncArray);
  function CaptureTok(tok: string): boolean;
  function CaptureStr(str: string): boolean;
  procedure CaptureParams(out funPars: TxpParFuncArray);
  //Manejo de Inline
  function CreateInline(funName: string; typ: TxpEleType; procParam,
    procCall: TProcExecFunction): TxpEleInlin;
  //Manejo del cuerpo del programa
  function CreateBody: TxpEleBody;
  //Manejo de Unidades
  function CreateUnit(uniName: string): TxpEleUnit;
public    //Containers
  TreeElems  : TXpTreeElements; //Árbol de sintaxis del lenguaje
  TreeDirec  : TXpTreeElements; //Árbol de sinatxis para directivas
  listFunSys : TxpEleFuns;   //lista de funciones del sistema
  listTypSys : TxpEleTypes;  //lista de tipos del sistema

  usedFuncs  : TxpEleFuns;    //Store only used functions
  unusedFuncs: TxpEleFuns;    //Store only unused functions
  interruptFunct: TxpEleFun;  //Store ths only Interrupt function
  procedure UpdateFunLstCalled;
  procedure SeparateUsedFunctions;
protected //Compiler events
  {This is one way the Parser can communicate with the Code Generator, considering this
  unit is independent of Code Generation.}
  OnExprStart : procedure of object;  {Se genera al iniciar la
                                       evaluación de una expresión.}
  OnExprEnd   : procedure(posExpres: TPosExpres) of object;  {Se genera al terminar de
                                                              evaluar una expresión.}
  OnReqStopCodeGen : procedure of object;  //Required stop the Code Generation
  OnReqStartCodeGen: procedure of object;  //Required start the Code Generation
protected //Calls to Assembler Module (ParserAsm.pas)
  callProcASMlime  : procedure(const AsmLin: string) of object;
protected //Call to Directive Module (ParserDirec.pas)
  callProcDIRline  : procedure(const AsmLin: string; out ctxChanged: boolean) of object;
protected //Calls to Code Generator (GenCodBas)
  callCurrRAM      : function(): integer of object;
  callResetRAM     : procedure of object;
  callCreateVarInRAM: procedure(xVar: TxpEleVar; shared: boolean = false) of object;
  callSetSharedUnused: procedure of object;
  callSetSharedUsed: procedure of object;

  callReturnAttribIn: function(typ: TxpEleType; const Op: TOperand; addr: integer): boolean of object;
  callDeviceError  : function(): string of object;
  callClearDeviceError: procedure of object;

  callCompileProcBody: procedure(fun: TxpEleFun) of object;
  callFunctParam   : procedure(fun: TxpEleFunBase) of object;
  callFunctCall    : procedure(fun: TxpEleFunBase; out AddrUndef: boolean) of object;
  callStartCodeSub : procedure(fun: TxpEleFun) of object;
  callEndCodeSub   : procedure() of object;

  callCompileIF    : procedure of object;
  callCompileWHILE : procedure of object;
  callCompileREPEAT: procedure of object;
  callCompileFOR   : procedure of object;
  //Load to register
  callLoadToA      : procedure(Op: TOperand) of object;
  callLoadToX      : procedure(Op: TOperand) of object;
  callLoadToY      : procedure(Op: TOperand) of object;
protected //Calls to Code Generator (GenCod)
  {These are routines that must be implemented in Code-generator.}
  callDefineArray  : procedure(etyp: TxpEleType) of object;  //routines to implemet Arrays.
  callDefinePointer: procedure(etyp: TxpEleType) of object; //routines to implemet Pointers.
  //Validate phisycal address
  callValidRAMaddr : procedure(addr: integer) of object;
  callStartProgram : procedure of object;
  callEndProgram   : procedure of object;
protected //ROP and expressions
  procedure LoadToRT(Op: TOperand);
  procedure Oper(var Op1: TOperand; opr: TxpOperator; var Op2: TOperand);
  procedure OperPre(var Op1: TOperand; opr: TxpOperator);
  procedure OperPost(var Op1: TOperand; opr: TxpOperator);
  //Expresions manage
  procedure GetOperandIdent(out Op: TOperand; opMode: TOpReadMode);
  procedure GetOperand(out Op: Toperand; opMode: TOpReadMode); virtual;
  function GetOperator(const Op: Toperand): TxpOperator;
  function GetExpression(const prec: Integer; opMode: TOpReadMode=opmGetter
    ): TOperand;
  procedure GetExpressionE(posExpres: TPosExpres);
public    //Types to implement
  typBool : TxpEleType;
  typByte : TxpEleType;
  typChar : TxpEleType;
  typWord : TxpEleType;
//  typString: TxpEleType;
public    //Public attributes of compiler
  ID        : integer;     //Identificador para el compilador.
  Compiling : boolean;     //Bandera para el compilado
  FirstPass : boolean;     //Flag to indicate we are in the First Pass
  DisableAddCalls: boolean; //Flag to disable registering calls
  xLex      : TSynFacilSyn; //Highlighter - lexer
  CompiledUnit: boolean;   //Flag to identify a Unit
  //Variables públicas del compilador
  ejecProg  : boolean;     //Indicates the compiler is working
  stopEjec  : boolean;     //To stop compilation
  procedure Compile(NombArc: string; Link: boolean); virtual; abstract;
public    //Compiling Options
  incDetComm  : boolean; //Incluir Comentarios detallados.
  GeneralORG  : integer; //Dirección general de origen de código
  mode        : (modPascal, modPicPas);
  OptBnkAftIF : boolean; //Optimizar instrucciones de cambio de banco al final de IF
  OptReuProVar: boolean; //Optimiza reutilizando variables locales de procedimientos
  OptRetProc  : boolean; //Optimiza el último exit de los procedimeintos.
protected //Files
  mainFile    : string;    //Archivo inicial que se compila
  hexFile     : string;    //Nombre de archivo de salida
  function ExpandRelPathTo(BaseFile, FileName: string): string;
protected //Container lists for registers
  listRegAux : TPicRegister_list;  //lista de registros de trabajo y auxiliares
public    //Access to CPU hardware
  picCore    : TCPUCore;   //Objeto PIC Core. This is an abstraction. Real CPU is not yet specified.
  devicesPath: string;     //path to untis for this device
  function PICName: string; virtual; abstract;
  function RAMmax: integer; virtual; abstract;
public    //Information and memory access.
  function hexFilePath: string;
  function mainFilePath: string;
  function CompilerName: string; virtual; abstract;  //Name of the compiler
  procedure RAMusage(lins: TStrings; ExcUnused: boolean); virtual; abstract;
  function RAMusedStr: string; virtual; abstract;
  procedure GetResourcesUsed(out ramUse, romUse, stkUse: single); virtual; abstract;
  procedure DumpCode(lins: TSTrings; asmMode, IncVarDec, ExcUnused: boolean;
                     incAdrr, incCom, incVarNam: boolean); virtual; abstract;
  procedure GenerateListReport(lins: TStrings); virtual; abstract;
  property ProplistRegAux: TPicRegister_list read listRegAux;
private
  elemCnt: integer;  //Count for
protected  //Miscellaneous
  function GenerateUniqName(base: string): string;
  procedure getListOfIdent(out itemList: TStringDynArray; out
    srcPosArray: TSrcPosArray);
  procedure IdentifyField(xOperand: TOperand; opMode: TOpReadMode);
  procedure LogExpLevel(txt: string);
  function IsTheSameVar(var1, var2: TxpEleVar): boolean; inline;
  function AddCallerTo(elem: TxpElement): TxpEleCaller;
  function AddCallerTo(elem: TxpElement; callerElem: TxpElement): TxpEleCaller;
  function AddCallerTo(elem: TxpElement; const curPos: TSrcPos): TxpEleCaller;
protected
  procedure RefreshAllElementLists;
  procedure RemoveUnusedFunc;
  procedure RemoveUnusedVars;
  procedure RemoveUnusedCons;
  procedure RemoveUnusedTypes;
  procedure UpdateCallersToUnits;
public    //Initialization
  constructor Create; virtual;
  destructor Destroy; override;
end;

procedure SetLanguage;

implementation
uses Graphics;
var
  ER_NOT_IMPLEM_ , ER_IDEN_EXPECT,  ER_DUPLIC_IDEN , ER_UNDEF_TYPE_,
  ER_SEMIC_EXPEC , ER_STR_EXPECTED, ER_TYP_PARM_ER_, ER_UNKNOWN_IDE_, ER_IN_EXPRESSI ,
  ER_OPERAN_EXPEC, ER_ILLEG_OPERA_, ER_UND_OPER_TY_, ER_CAN_AP_OPER_, ER_IN_CHARACTER,
  ER_INV_COD_CHAR, ER_RA_HAV_USED,  ER_RX_HAV_USED,  ER_RY_HAV_USED,  ER_CON_EXP_EXP,
  ER_NOTYPDEF_NU
  : string;

procedure SetLanguage;
begin
  {$I ..\language\tra_CompBase.pas}
end;
{TCompilerBase}
function TCompilerBase.EOExpres: boolean; inline;
//Indica si se ha llegado al final de una expresión.
begin
  Result := cIn.tok = ';';  //en este caso de ejemplo, usamos punto y coma
  {En la práctica, puede ser conveniente definir un tipo de token como "tkExpDelim", para
   mejorar el tiempo de respuesta del procesamiento, de modo que la condición sería:
     Result := cIn.tokType = tkExpDelim;
  }
end;
function TCompilerBase.EOBlock: boolean; inline;
//Indica si se ha llegado el final de un bloque
begin
  Result := cIn.tokType = tnBlkDelim;
  {No está implementado aquí, pero en la práctica puede ser conveniente definir un tipo de token
   como "tnBlkDelim", para mejorar el tiempo de respuesta del procesamiento, de modo que la
   condición sería:
  Result := cIn.tokType = tnBlkDelim;}
end;
function TCompilerBase.CaptureDelExpres: boolean;
//Verifica si sigue un delimitador de expresión. Si encuentra devuelve false.
begin
  cIn.SkipWhites;
  if EOExpres then begin //encontró
    cIn.Next;   //pasa al siguiente
    exit(true);
  end else begin   //es un error
    GenError(ER_SEMIC_EXPEC);
    exit(false);  //sale con error
  end;

end;

procedure TCompilerBase.ProcComments;
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
      callProcASMlime(cIn.tok);  //procesa línea
      if HayError then begin
        cIn.Next;   //Pasa, porque es un error ya ubicado, y mejor buscamos otros
        cIn.SkipWhites;
        continue;
      end;
    end else begin
      //Es una directiva
     callProcDIRline(cIn.tok, ctxChanged);  //procesa línea
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
procedure TCompilerBase.TipDefecNumber(var Op: TOperand; toknum: string);
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
procedure TCompilerBase.TipDefecString(var Op: TOperand; tokcad: string);
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
procedure TCompilerBase.TipDefecBoolean(var Op: TOperand; tokcad: string);
//Devuelve el tipo de cadena encontrado en un token
begin
  //convierte valor constante
  Op.SetAsConst(typBool);
  Op.valBool:= (tokcad[1] in ['t','T']);
end;
procedure TCompilerBase.ProcCommentsNoExec;
{Similar a ProcComments(), pero no ejecuta directivas o bloques ASM.}
begin
  cIn.SkipWhites;
  while (cIn.tokType = tnDirective) or (cIn.tokType = tnAsm) do begin
    //Pasa a siguiente
    cIn.Next;
    cIn.SkipWhites;  //limpia blancos
  end;
end;
//Creación de elementos
procedure TCompilerBase.ClearSystemTypes;  //Limpia los tipos del sistema
begin
  listTypSys.Clear;
end;
function TCompilerBase.CreateSysType(nom0: string; cat0: TTypeGroup; siz0: smallint): TxpEleType;
{Crea un elemento tipo, del sistema. Devuelve referencia al tipo creado.}
var
  eType: TxpEleType;
begin
  //Verifica nombre
  if FindSysEleType(nom0) <> nil then begin
    GenError(ER_DUPLIC_IDEN, [nom0]);
    exit(nil);  //Devuelve tipo nulo
  end;
  //Crea elemento de tipo
  eType := TxpEleType.Create;
  eType.name := nom0;
  eType.grp := cat0;
  eType.size := siz0;
  eType.catType := tctAtomic;
  listTypSys.Add(eType);
  //Devuelve referencia al tipo
  Result:=eType;
end;
function TCompilerBase.FindSysEleType(TypName: string): TxpEleType;
{Busca un elemento de tipo por su nombre. Si no encuentra, devuelve NIL.}
var
  etyp: TxpEleType;
begin
  typName := upcase(typName);
  for etyp in listTypSys do begin
    if etyp.uname = typName then exit(etyp);  //devuelve referencia
  end;
  exit(nil);
end;
function TCompilerBase.CreateCons(consName: string; eletyp: TxpEleType): TxpEleCon;
{Rutina para crear una constante. Devuelve referencia a la constante creada.}
var
  conx  : TxpEleCon;
begin
  //registra variable en la tabla
  conx := TxpEleCon.Create;
  conx.name:=consName;
  conx.typ := eletyp;   //fija  referencia a tipo
  Result := conx;
end;
function TCompilerBase.CreateVar(varName: string; eleTyp: TxpEleType): TxpEleVar;
{Rutina para crear una variable. Devuelve referencia a la variable creada.}
var
  xVar: TxpEleVar;
begin
  xVar := TxpEleVar.Create;
  xVar.name := varName;
  xVar.typ := eleTyp;
  xVar.adicPar.hasAdic := decNone;
  xVar.adicPar.hasInit := false;
  Result := xVar;
end;
function TCompilerBase.CreateEleType(const typName: string; const srcPos: TSrcPos;
                                           catType: TxpCatType): TxpEleType;
begin
  Result := TxpEleType.Create;
  Result.name    := typName;
  Result.srcDec  := srcPos;
  Result.catType := catType;
end;
function TCompilerBase.CreateEleTypeArray(const typName: string;
  const srcPos: TSrcPos; itmType: TxpEleType; nItems: integer): TxpEleType;
var
  xtyp: TxpEleType;
begin
  xtyp := CreateEleType(typName, srcPos, tctArray);
  xtyp.itmType := itmType; //Item type
  xtyp.nItems  := nItems;   //number of items
  callDefineArray(xtyp);   //Define operations to array
  exit(xtyp);
end;
function TCompilerBase.CreateEleTypePtr(const typName: string;
  const srcPos: TSrcPos; ptrType: TxpEleType): TxpEleType;
var
  xtyp: TxpEleType;
begin
  xtyp := CreateEleType(typName, srcPos, tctPointer);
  xtyp.ptrType := ptrType; //Item type
  callDefinePointer(xtyp);   //Define operations to array
  exit(xtyp);
end;
function TCompilerBase.CreateEleTypeObject(const typName: string;
  const srcPos: TSrcPos): TxpEleType;
begin
  Result := CreateEleType(typName, srcPos, tctObject);

end;
function TCompilerBase.CreateFunction(funName: string; typ: TxpEleType;
  procParam: TxpProcParam; procCall: TxpProcCall): TxpEleFun;
{Crea una nueva función y devuelve la referencia a la función.}
var
  fun : TxpEleFun;
begin
  fun := TxpEleFun.Create;
  fun.name:= funName;
  fun.typ := typ;
  fun.procParam := procParam;
  fun.procCall:= procCall;
  fun.ClearParams;
  Result := fun;
end;
function TCompilerBase.CreateSysFunction(funName: string;
  procParam: TxpProcParam; procCall: TxpProcCall): TxpEleFun;
{Crea una función del sistema. A diferencia de las funciones definidas por el usuario,
una función del sistema se crea, sin crear espacios de nombre. La idea es poder
crearlas rápidamente. "procParam", solo es necesario, cuando la función del sistema
debe devolver valores (No es procedimiento).}
var
  fun : TxpEleFun;
begin
  fun := TxpEleFun.Create;  //Se crea como una función normal
  fun.name:= funName;
  fun.typ := typNull;
  fun.procParam := procParam;
  fun.procCall:= procCall;
  fun.ClearParams;
  listFunSys.Add(fun);  //Las funciones de sistema son accesibles siempre
  Result := fun;
end;
function TCompilerBase.AddVariable(varName: string; eleTyp: TxpEleType; srcPos: TSrcPos
  ): TxpEleVar;
{Crea un elemento variable y lo agrega en el nodo actual del árbol de sintaxis.
Si no hay errores, devuelve la referencia a la variable. En caso contrario,
devuelve NIL.
Notar que este método, no asigna RAM a la variable. En una creación completa de
variables, se debería llamar a CreateVarInRAM(), después de agregar la variable.}
var
  xvar: TxpEleVar;
begin
  xvar := CreateVar(varName, eleTyp);
  xvar.srcDec := srcPos;  //Actualiza posición
  //Verifica si hay conflicto. Solo es necesario buscar en el nodo actual.
  if xvar.ExistsIn(TreeElems.curNode.elements) then begin
    GenErrorPos(ER_DUPLIC_IDEN, [xvar.name], xvar.srcDec);
    xvar.Destroy;   //Hay una variable creada
    exit(nil);
  end;
  TreeElems.AddElement(xvar);
  Result := xvar;
end;
function TCompilerBase.AddConstant(conName: string; eleTyp: TxpEleType; srcPos: TSrcPos
  ): TxpEleCon;
{Crea un elemento constante y lo agrega en el nodo actual del árbol de sintaxis.
Si no hay errores, devuelve la referencia a la variable. En caso contrario,
devuelve NIL. }
var
  xcons: TxpEleCon;
begin
  xcons := CreateCons(conName, eleTyp);
  xcons.srcDec := srcPos;
  if xcons.ExistsIn(TreeElems.curNode.elements) then begin
    GenErrorPos(ER_DUPLIC_IDEN, [xcons.name], xcons.srcDec);
    xcons.Destroy;   //hay una constante creada
    exit;
  end;
  TreeElems.AddElement(xcons);
  Result := xcons;
end;
procedure TCompilerBase.CreateFunctionParams(var funPars: TxpParFuncArray);
{Crea los parámetros de una función como variables globales, a partir de un arreglo
TxpParFunc. }
var
  i: Integer;
  par: TxpParFunc;
  xvar: TxpEleVar;
  regAused: boolean = false;
  regXused: boolean = false;
  regYused: boolean = false;
begin
  for i := 0 to high(funPars) do begin
      par := funPars[i];
      xvar := AddVariable({fun.name + '_' + }par.name, par.typ, par.srcPos);
      if HayError then exit;
      xvar.IsParameter := true;  //Marca bandera
      xvar.adicPar := par.adicVar;  //Copy aditional settings
      case par.adicVar.hasAdic of
      decRegis: begin
        //Parameters REGISTER use: A or H,A register. Only can be used once.
        if regAused then begin
          GenErrorPos(ER_RA_HAV_USED, [], par.srcPos);
          exit;
        end;
        regAused := true;  //Activa bandera
      end;
      decRegisA: begin
        //Parameter REGISTER A
        if regAused then begin
          GenErrorPos(ER_RA_HAV_USED, [], par.srcPos);
          exit;
        end;
        regAused := true;  //Activa bandera
      end;
      decRegisX: begin
        //Parameter REGISTER X
        if regXused then begin
          GenErrorPos(ER_RX_HAV_USED, [], par.srcPos);
          exit;
        end;
        regXused := true;  //Activa bandera
      end;
      decRegisY: begin
        //Parameter REGISTER Y
        if regYused then begin
          GenErrorPos(ER_RY_HAV_USED, [], par.srcPos);
          exit;
        end;
        regYused := true;  //Activa bandera
      end;
      end;
      //Actualiza referencia a la variable que almacena el parámetro.
      funPars[i].pvar := xvar;
  end;
end;
function TCompilerBase.AddFunctionUNI(funName: string; retTyp: TxpEleType;
  const srcPos: TSrcPos; const pars: TxpParFuncArray; Interrup: boolean
  ): TxpEleFun;
{Create a new function, in normal mode (In the Main program or a like a private function
in Implementation section) and add it to the Syntax Tree in the current node.}
var
  xfun: TxpEleFun;
begin
  xfun := CreateFunction(funName, retTyp, callFunctParam, callFunctCall);
  xfun.srcDec := srcPos;   //Toma ubicación en el código
  xfun.declar := nil;   //This is declaration
  xfun.pars := pars;    //Copy parameters
  xfun.IsInterrupt := Interrup;
  //La validación de duplicidad no se puede hacer hasta tener los parámetros.
  TreeElems.AddElementAndOpen(xfun);  //Se abre un nuevo espacio de nombres
  Result := xfun;
  //Crea parámetros en el nuevo espacio de nombres de la función
  CreateFunctionParams(xfun.pars);
end;

function TCompilerBase.AddFunctionDEC(funName: string; retTyp: TxpEleType;
  const srcPos: TSrcPos; const pars: TxpParFuncArray; Interrup: boolean): TxpEleFunDec;
{Create a new function, in DECLARATION mode (Forward or Interface) and add it
to the Syntax Tree in the current node. No new node is opened.}
var
  xfundec: TxpEleFunDec;
begin
  xfundec := TxpEleFunDec.Create;
  xfundec.name:= funName;
  xfundec.typ := retTyp;
  xfundec.procCall := callFunctCall;
  xfundec.procParam := callFunctParam;
//  xfun.ClearParams;

  xfundec.srcDec := srcPos;   //Toma ubicación en el código
  xfundec.implem := nil;  //Not yet implemented
  xfundec.pars := pars;   //Copy parameters
  xfundec.IsInterrupt := Interrup;
  TreeElems.AddElement(xfundec);  //Doesn't open the element
  Result := xfundec;
  //Note that variables for parameters are not created here.
end;
function TCompilerBase.AddFunctionIMP(funName: string; retTyp: TxpEleType;
  const srcPos: TSrcPos; functDeclar: TxpEleFunDec): TxpEleFun;
{Create a new function, in IMPLEMENTATION mode (Forward or Interface) and add it
to the Syntax Tree in the current node. }
var
  xfun: TxpEleFun;
  tmp: TxpListCallers;
begin
  xfun := CreateFunction(funName, retTyp, callFunctParam, callFunctCall);
  xfun.srcDec := srcPos;       //Take position in code.
  functDeclar.implem := xfun;  //Complete reference
  xfun.declar := functDeclar;  //Reference to declaration
  xfun.pars := functDeclar.pars;     //Copy from declaration
  xfun.IsInterrupt := functDeclar.IsInterrupt; //Copy from declaration
  //La validación de duplicidad no se puede hacer hasta tener los parámetros.
  TreeElems.AddElementAndOpen(xfun);  //Se abre un nuevo espacio de nombres
  Result := xfun;
  //Crea parámetros en el nuevo espacio de nombres de la función
  CreateFunctionParams(xfun.pars);
  //Pass calls list form declaration to implementation.
  tmp := functDeclar.lstCallers;
  functDeclar.lstCallers := xfun.lstCallers;
  xfun.lstCallers := tmp;
  //New calls will be added to implementation since now.
end;
function TCompilerBase.AddInline(funName: string; eleTyp: TxpEleType;
  const srcPos: TSrcPos; const pars: TxpParInlinArray;
  procParam, procCall: TProcExecFunction): TxpEleInlin;
{Crea una función y lo agrega en el nodo actual del árbol de sintaxis. }
var
  xfun: TxpEleInlin;
begin
  xfun := CreateInline(funName, eleTyp, procParam, procCall);
  xfun.srcDec := srcPos;   //Toma ubicación en el código
  xfun.pars := pars;
  //La validación de duplicidad no se puede hacer hasta tener los parámetros.
  TreeElems.AddElementAndOpen(xfun);  //Se abre un nuevo espacio de nombres
  Result := xfun;
end;

function TCompilerBase.CaptureTok(tok: string): boolean;
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
        lin := cIn.curCon.curLines[p.row - 1];
      until (p.row<=1) or (trim(lin)<>'');
      //Encontró línea anterior no nula o llegó a la primera línea.
//      xlex.ExploreLine(Point(length(lin), p.row), toks, CurTok );
      p.col := length(lin);   //mueve al final (antes del EOL)
      GenErrorPos(ER_STR_EXPECTED, [tok], p);  //Genera error
    end else begin
      //No hay línea anterior
      p.col := 1;   //mueve al inicio
      GenErrorPos(ER_STR_EXPECTED, [tok], p);  //Genera error
    end;
  end;

var
  x: integer;
  lin: String;
  p: TSrcPos;
begin
  //Debe haber parámetros
  if cIn.tok<>tok then begin
    //No se encontró el token. Muestra mensaje de error.
    {Pero el error, debe estar antes, así que hacemos la magia de explorar hacia atrás,
    hasta encontrar el token involucrado.}
    p := cIn.ReadSrcPos;   //posición actual
    x := p.col;   //lee posición actual
    if x>1 then begin
      //Hay algo antes del token
      lin := cIn.curCon.CurLine;
      repeat
        dec(x);
      until (x<=1) or (lin[x] <> ' ');
      if x<=1 then begin
        //Está lleno de espacios, hasta el inicio.
        //Es muy probable que el error esté en la línea anterior.
        GenErrorInLastLine(p);
      end else begin
        //Encontró, en la misma línea un caracter diferente de espacio
        GenErrorPos(ER_STR_EXPECTED, [tok], p);  //Genera error ahí mismo
      end;
    end else begin
      //Está al inicio de la línea. El error debe estar antes
      GenErrorInLastLine(p);
    end;
    exit(false);
  end;
  cin.Next;
  exit(true);
end;
function TCompilerBase.CaptureStr(str: string): boolean;
//Similar a CaptureTok(), pero para cadenas. Se debe dar el texto en minúscula.
begin
  //Debe haber parámetros
  if cIn.tokL<>str then begin
    GenError(ER_STR_EXPECTED, [str]);
    exit(false);
  end;
  cin.Next;
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
begin
  //func0.ClearParams;
  if EOBlock or EOExpres then begin
    //no tiene parámetros
  end else begin
    //Debe haber parámetros
    if cIn.tok <> '(' then begin
      //Si no sigue '(', significa que no hay parámetros.
      exit;
    end;
    cIn.Next;  //Toma paréntesis
    //Debe haber parámetros. Prepara espacio para leer.
    curSize := BLOCK_SIZE;    //Tamaño inicial de bloque
    setlength(funPars, curSize);  //Tamaño inicial
    n := 0;
    repeat
      res := GetExpression(0);  //captura parámetro
      if HayError then exit;   //aborta
      //Guarda tipo de parámetro
      funPars[n].typ   := res.Typ;
      //Prepara siguiente lectura
      inc(n);
      if n >= curSize then begin
        curSize += BLOCK_SIZE;   //Incrementa tamaño en bloque
        setlength(funPars, curSize);  //hace espacio en bloque
      end;
      //Busca delimitador
      if cIn.tok = ',' then begin
        cIn.Next;   //toma separador
        cIn.SkipWhites;
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
function TCompilerBase.CreateInline(funName: string; typ: TxpEleType;
  procParam, procCall: TProcExecFunction): TxpEleInlin;
{Crea un nueva función Inline y devuelve la referencia a la función.}
var
  fun : TxpEleInlin;
begin
  fun := TxpEleInlin.Create;
  fun.name:= funName;
  fun.typ := typ;
//  fun.procParam := procParam;
//  fun.procCall:= procCall;
  fun.ClearParams;
  Result := fun;
end;
procedure TCompilerBase.CaptureParamsFinal(var funPars: TxpParFUncArray);
{Captura los parámetros asignándolos a las variables de la función que representan a los
parámetros. No hace falta verificar, no debería dar error, porque ya se verificó con
CaptureParams. }
var
  i: Integer;
  par: TxpParFunc;
  Op1, Op2: TOperand;
  op: TxpOperator;
begin
  if EOBlock or EOExpres then exit;  //sin parámetros
  CaptureTok('(');   //No debe dar error porque ya se verificó
  for i := 0 to high(funPars) do begin
    par := funPars[i];
    {Ya sirvió "RTstate", ahora lo limpiamos, no vaya a pasar que las rutinas de
    asignación, piensen que los RT están ocupados, cuando la verdad es que han sido
    liberados, precisamente para ellas.}
    RTstate := nil;
    //Evalúa parámetro
    Inc(ExprLevel);    //cuenta el anidamiento
    Op2 := GetExpression(0);  //llama como sub-expresión
    Dec(ExprLevel);
    if HayError then exit;   //aborta
    if cIn.tok = ',' then begin
      cIn.Next;
      cIn.SkipWhites;
    end;
    //Genera código para la asignación
    if par.adicVar.hasAdic = decRegis then begin
      {Register parameter is not assigned. It's loaded in RT.}
      LoadToRT(Op2);
    end else if par.adicVar.hasAdic = decRegisA then begin
      {Register parameter is not assigned.}
      callLoadToA(Op2);
    end else if par.adicVar.hasAdic = decRegisX then begin
      {Register parameter is not assigned.}
      callLoadToX(Op2);
    end else if par.adicVar.hasAdic = decRegisY then begin
      {Register parameter is not assigned.}
      callLoadToY(Op2);
    end else if par.adicVar.hasAdic in [decNone, decAbsol] then begin
      //Cretae operand-variable to generate assignment code.
      Op1.SetAsVariab(par.pvar); //Apunta a la variable
      AddCallerTo(par.pvar);    //Agrega la llamada
      op := Op1.Typ.operAsign;
      Oper(Op1, op, Op2);       //Codifica la asignación
    end else begin
      GenError('Not implemented.');
      exit;
    end;
  end;
  if not CaptureTok(')') then exit;
end;
function TCompilerBase.CreateBody: TxpEleBody;
var
  body: TxpEleBody;
begin
  body := TxpEleBody.Create;
  body.name := TIT_BODY_ELE;
  Result := body;
end;
function TCompilerBase.CreateUnit(uniName: string): TxpEleUnit;
var
  uni: TxpEleUnit;
begin
  uni := TxpEleUnit.Create;
  uni.name := uniName;
  Result := uni;
end;

procedure TCompilerBase.UpdateFunLstCalled;
{Actualiza la lista lstCalled de las funciones, para saber, a qué fúnciones llama
 cada función.}
var
  fun    : TxpEleFun;
  itCall : TxpEleCaller;
  whoCalls: TxpEleBody;
  n: Integer;
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
    n := fun.UpdateCalledAll;
    if n<0 then begin
      GenErrorPos('Recursive call or circular recursion in %s', [fun.name], fun.srcDec);
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
{Fill the list usedFuncs with all used functions, including Interrupt function.
Set interruptFunct to point to the interrupt function (Only one).
Must be called after call to RemoveUnusedFunc().}
var
  fun : TxpEleFun;
begin
  usedFuncs.Clear;
  unusedFuncs.Clear;
  interruptFunct := nil;
  for fun in TreeElems.AllFuncs do begin
    if fun.nCalled>0 then usedFuncs.Add(fun) else unusedFuncs.Add(fun);
    if fun.IsInterrupt then interruptFunct := fun;
  end;
end;
procedure TCompilerBase.getListOfIdent(out itemList: TStringDynArray; out
  srcPosArray: TSrcPosArray);
{Lee una lista de identificadores separados por comas, hasta encontra un caracter distinto
de coma. Si el primer elemento no es un identificador o si después de la coma no sigue un
identificador, genera error.
También devuelve una lista de las posiciones de los identificadores, en el código fuente.}
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
    if cIn.tokType <> tnIdentif then begin
      GenError(ER_IDEN_EXPECT);
      exit;
    end;
    //hay un identificador
    item := cIn.tok;
    itemList[n] := item;  //agrega nombre
    srcPosArray[n] := cIn.ReadSrcPos;  //agrega ubicación de declaración
    cIn.Next;  //Toma identificador despues, de guardar ubicación
    ProcComments;
    if cIn.tok <> ',' then break; //sale
    cIn.Next;  //Toma la coma
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
end;
procedure TCompilerBase.IdentifyField(xOperand: TOperand; opMode: TOpReadMode);
{Identifica el campo de una variable. Si encuentra algún problema genera error.
Notar que el parámetro es por valor, es decir, se crea una copia, por seguridad.
Puede generar código de evaluación. Devuelve el resultado en "res". }
  function CallProc(const field: TTypField): boolean; inline;
  {Call the Getter or Setter, of the field acordding to "ToSet". If gives error
  return FALSE}
  begin
    if opMode = opmSetter then begin
      //Mode Setter
      if field.procSet= nil then begin
        GenError('Cannot assign to this operand.');
        exit(false);
      end;
      field.procSet(@xOperand)
    end else begin
      //Mode Getter
      if field.procGet= nil then begin
        GenError('Cannot read this operand.');
        exit(false);
      end;
      field.procGet(@xOperand);  //Devuelve resultado en "res"
    end;
    exit(true);
  end;
var
  field: TTypField;
  identif: String;
  att: TTypAttrib;
begin
  if cIn.tok = '[' then begin
    //Caso especial de llamada a Item().
    for field in xOperand.Typ.fields do begin
      if LowerCase(field.Name) = 'item' then begin
        //Find de method
        if not CallProc(field) then exit;
        if cIn.tok = '.' then begin
          //Aún hay más campos, seguimos procesando
          //Como "IdentifyField", crea una copia del parámetro, no hay cruce con el resultado
          IdentifyField(res, opMode);
        end;
        exit;
      end;
    end;
    //setitem() not found
    GenError('Cannot access to index in: %s', [xOperand.txt]);
    exit;
  end;
  cIn.Next;    //Toma el "."
  if (cIn.tokType<>tnIdentif) and (cIn.tokType<>tnNumber) then begin
    GenError('Identifier expected.');
    cIn.Next;    //Pasa siempre
    exit;
  end;
  //Hay un identificador
  identif :=  cIn.tokL;
  //Test with methods
  for field in xOperand.Typ.fields do begin
    if LowerCase(field.Name) = identif then begin
      //Find de method
      if not CallProc(field) then exit;
      if cIn.tok = '.' then begin
        //Aún hay más campos, seguimos procesando
        //Como "IdentifyField", crea una copia del parámetro, no hay cruce con el resultado
        IdentifyField(res, opMode);
      end;
      exit;
    end;
  end;
  //Test with attributes
  for att in xOperand.Typ.attribs do begin
    if LowerCase(att.Name) = identif then begin
      //Attribute found
      if not callReturnAttribIn(att.typ, xOperand, att.offs) then exit;
      cIn.Next;  //Take tonen
      if cIn.tok = '.' then begin
        //Aún hay más campos, seguimos procesando
        //Como "IdentifyField", crea una copia del parámetro, no hay cruce con el resultado
        IdentifyField(res, opMode);
      end;
      exit;
    end;
  end;
  //No found
  GenError(ER_UNKNOWN_IDE_, [identif]);
end;
//Manejo de expresiones
procedure TCompilerBase.LogExpLevel(txt: string);
{Genera una cadena de registro , considerando el valor de "ExprLevel"}
begin
  debugln(space(3*ExprLevel)+ txt );
end;
function TCompilerBase.IsTheSameVar(var1, var2: TxpEleVar): boolean; inline;
{Indica si dos variables bit son la misma, es decir que apuntan, a la misma dirección
física}
begin
  Result := (var1.addr = var2.addr);
end;
function TCompilerBase.AddCallerTo(elem: TxpElement): TxpEleCaller;
{Agregar una llamada a un elemento de la sintaxis.
Para el elemento llamador, se usa el nodo actual, que debería ser la función/cuerpo
desde donde se hace la llamada.
Devuelve la referencia al elemento llamador, cuando es efectiva la agregación, de otra
forma devuelve NIL.}
var
  fc: TxpEleCaller;
  fun: TxpEleFun;
  fundec: TxpEleFunDec;
begin
  //Verify Flag
  if DisableAddCalls then exit(nil);
  //Solo se agregan llamadas en la primera pasada
  if not FirstPass then begin
    exit(nil);
  end;
  fc:= TxpEleCaller.Create;
  //Carga información del estado actual del parser
  fc.caller := TreeElems.curNode;
  fc.curPos := cIn.ReadSrcPos;
  if elem.idClass = eltFunc then begin
    fun := TxpEleFun(elem);
    fun.lstCallers.Add(fc);
  end else if elem.idClass = eltFuncDec then begin
    {When functions have declaration (INTERFACE or FORWARD), the calls have been added
    to declaration elements (because implementation could not exist yet) but when
    implementation appears, all calls must be passed to implementation.}
    fundec := TxpEleFunDec(elem);
    if fundec.implem = nil then begin
      //Not yet implemented
      fundec.lstCallers.Add(fc);
    end else begin
      //Pass call to the function
      fundec.implem.lstCallers.Add(fc);
    end;
  end else begin
    //Other elements
    elem.lstCallers.Add(fc);
  end;
  exit(fc);
end;
function TCompilerBase.AddCallerTo(elem: TxpElement; callerElem: TxpElement): TxpEleCaller;
{El elemento llamador es "callerElem". Agrega información sobre el elemento "llamador", es decir, el elemento que hace
referencia a este elemento.}
begin
  Result := AddCallerTo(elem);
  if Result = nil then exit;
  Result.caller := callerElem;
end;
function TCompilerBase.AddCallerTo(elem: TxpElement; const curPos: TSrcPos): TxpEleCaller;
{Versión de AddCallerTo() que agrega además la posición de la llamada, en lugar de usar
la posición actual.}
begin
  Result := AddCallerTo(elem);
  if Result = nil then exit;
  Result.curPos := curPos;  //Corrige posición de llamada
end;

procedure TCompilerBase.Oper(var Op1: TOperand; opr: TxpOperator; var Op2: TOperand);
{Ejecuta una operación con dos operandos y un operador. "opr" es el operador de Op1.
El resultado debe devolverse en "res". En el caso de intérpretes, importa el
resultado de la Operación.
En el caso de compiladores, lo más importante es el tipo del resultado, pero puede
usarse también "res" para cálculo de expresiones constantes.
}
var
  Operation: TxpOperation;
  tmp: String;
begin
   {$IFDEF LogExpres}
   LogExpLevel('-- Op1='+Op1.txt+', Op2='+Op2.txt+' --');
   {$ENDIF}
   //Busca si hay una operación definida para: <tipo de Op1>-opr-<tipo de Op2>
   Operation := opr.FindOperation(Op2.Typ);
   if Operation = nil then begin
      tmp := '(' + Op1.Typ.name + ') '+ opr.txt;
      tmp := tmp + ' ('+Op2.Typ.name+')';
      GenError(ER_ILLEG_OPERA_,
               [tmp]);
      exit;
    end;
   {Llama al evento asociado con p1 y p2 como operandos. }
   p1 := @Op1; p2 := @Op2;  { Se usan punteros por velocidad. De otra forma habría que
                             copiar todo el objeto.}
   {Ejecuta la operación.
   Los parámetros de entrada se dejan en p1 y p2. El resultado debe dejarse en "res"}
   Operation.proc(Operation, true);  //Llama normalmente
   //Completa campos de "res", si es necesario
   {$IFDEF LogExpres}
   LogExpLevel('Oper('+Op1.catOpChr + ' ' + opr.txt + ' ' + Op2.catOpChr+') -> ' +
                res.catOpChr);
   res.txt := Op1.txt + ' ' + opr.txt + ' ' + Op2.txt;   //texto de la expresión
   {$ENDIF}
End;
procedure TCompilerBase.OperPre(var Op1: TOperand; opr: TxpOperator);
{Ejecuta una operación con un operando y un operador unario de tipo Pre. "opr" es el
operador de Op1.
El resultado debe devolverse en "res".}
begin
  {$IFDEF LogExpres}
  LogExpLevel('-- Op1='+Op1.txt+' --');
  {$ENDIF}
   if opr.OperationPre = nil then begin
      GenError(ER_ILLEG_OPERA_,
                 [opr.txt + '('+Op1.Typ.name+')']);
      exit;
    end;
   {Llama al evento asociado con p1 como operando. }
   p1 := @Op1; {Solo hay un parámetro}
   {Ejecuta la operación. El resultado debe dejarse en "res"}
   opr.OperationPre(opr, true);  //Llama normalmente
   //Completa campos de "res", si es necesario
   {$IFDEF LogExpres}
   LogExpLevel('Oper('+ opr.txt + ' ' + Op1.catOpChr+ ') -> ' + res.catOpChr);
   res.txt := opr.txt + Op1.txt;
   {$ENDIF}
end;
procedure TCompilerBase.OperPost(var Op1: TOperand; opr: TxpOperator);
{Ejecuta una operación con un operando y un operador unario de tipo Post. "opr" es el
operador de Op1.
El resultado debe devolverse en "res".}
begin
  {$IFDEF LogExpres}
  LogExpLevel('-- Op1='+Op1.txt+' --');
  {$ENDIF}
   if opr.OperationPost = nil then begin
      GenError(ER_ILLEG_OPERA_,
                 ['('+Op1.Typ.name+')' + opr.txt]);
      exit;
    end;
   {Llama al evento asociado con p1 como operando. }
   p1 := @Op1; {Solo hay un parámetro}
   {Ejecuta la operación. El resultado debe dejarse en "res"}
   opr.OperationPost(opr, true);  //Llama normalmente
   //Completa campos de "res", si es necesario
   {$IFDEF LogExpres}
   LogExpLevel('Oper('+Op1.catOpChr+ ' ' +opr.txt +') -> ' + res.catOpChr);
   res.txt := Op1.txt + opr.txt;   //indica que es expresión
   {$ENDIF}
end;

procedure TCompilerBase.GetOperandIdent(out Op: TOperand; opMode: TOpReadMode);
{Read an operand spcified by one identifier. Return in "Op". This routine was part of
GetOperand(), but it was splitted because:
* This is a big code and could grow more.
* It's used separately too. }
var
  ele     : TxpElement;
  xvar    : TxpEleVar;
  xcon    : TxpEleCon;
  posCall : TSrcPos;
  posPar  : TPosCont;
  RTstate0: TxpEleType;
  xfun    : TxpEleFunBase;
  Found   , AddrUndef: Boolean;
  pars    : TxpParFuncArray;   //Para almacenar parámetros
  findState: TxpFindState;
begin
//cIn.ShowCurContInformat;
//debugln(' ++CurNode:' + TreeElems.curNode.Path);
  ele := TreeElems.FindFirst(cIn.tok);  //Identify element
  findState := TreeElems.curFind;  //Save because can be aletered with CaptureParams()
  if ele = nil then begin
    //No identifica a este elemento
    GenError(ER_UNKNOWN_IDE_, [cIn.tok]);
    exit;
  end;
//debugln(' --Element ' + cIn.tok + ':' + ele.Path);
  if ele.idClass = eltVar then begin
    //Es una variable
    xvar := TxpEleVar(ele);    //Referencia con tipo
    //Lleva la cuenta de la llamada.
    {Notar que se agrega la referencia a la variable, pero que finalmente el operando
    puede apuntar a otra variable, si es que se tiene la forma: <variable>.<campo> }
    AddCallerTo(xvar);
    cIn.Next;    //Pasa al siguiente
    case xvar.adicPar.hasAdic of
    decRegis: begin //Is a REGISTER variable
      Op.SetAsExpres(xvar.typ);  //Will be considered as expression
      Op.DefineRegister;
    end;
    decRegisA: begin //Is a REGISTER variable
      Op.SetAsExpresA(xvar.typ);
    end;
    decRegisX: begin //Is a REGISTER variable
      Op.SetAsExpresX(xvar.typ);
    end;
    decRegisY: begin //Is a REGISTER variable
      Op.SetAsExpresY(xvar.typ);
    end;
    decNone, decAbsol: begin
      //It's a commmon variable
      Op.SetAsVariab(xvar);   //Save reference to variable (and update the type).
      {$IFDEF LogExpres} Op.txt:= xvar.name; {$ENDIF}   //Take the text
      //Verify if has reference to fields with "."
      if (cIn.tok = '.') or (cIn.tok = '[') then begin
        IdentifyField(Op, opMode);
        Op := res;  //notar que se usa "res".
        if HayError then exit;
        {Como este operando es de tipo <variable>.<algo>... , actualizamos el campo
        "rVarBase", y se hace al final porque los métodos Op.SetAsXXXX() }
        Op.rVarBase := xvar;    //Fija referencia a la variable base
      end;
    end else
      GenError('Not implemented.');
      exit;
    end;
  end else if ele.idClass = eltCons then begin  //es constante
    //es una constante
    xcon := TxpEleCon(ele);
    AddCallerTo(xcon);//lleva la cuenta
    cIn.Next;    //Pasa al siguiente
    Op.SetAsConst(xcon.typ); //fija como constante
    Op.GetConsValFrom(xcon);  //lee valor
    {$IFDEF LogExpres} Op.txt:= xcon.name; {$ENDIF}   //toma el texto
    //Verifica si tiene referencia a campos con "."
    if (cIn.tok = '.') or (cIn.tok = '[') then begin
      IdentifyField(Op, opMode);
      Op := res;  //notar que se usa "res".
      if HayError then exit;;
    end;
  end else if ele.idClass in [eltFunc, eltFuncDec] then begin  //es función
    {Se sabe que es función, pero no se tiene la función exacta porque puede haber
     versiones, sobrecargadas de la misma función.}
    posCall := cIn.ReadSrcPos;   //guarda la posición de llamada.
    cIn.Next;               //Toma identificador
    cIn.SkipWhites;         //Quita posibles blancos
    posPar := cIn.PosAct;   //Guarda porque va a pasar otra vez por aquí
    OnReqStopCodeGen();     //Para que no se genere el código(Por ejemplo cuando se leen parámetros de tipo expresión.)
    RTstate0 := RTstate;    //Guarda porque se va a alterar con CaptureParams().
    CaptureParams(pars);    //Primero lee parámetros en "pars".
    if HayError then begin
      exit;
    end;
    //Aquí se identifica la función exacta, que coincida con sus parámetros
    xfun := TxpEleFunBase(ele);  //The ancestor of eltFunc and eltFuncDec
    //Primero vemos si la primera función encontrada, coincide:
    if xfun.SameParamsType(pars) then begin
      //Coincide
      Found := true;
    end else begin
      //No es, es una pena. Ahora tenemos que seguir buscando en el árbol de sintaxis.
      TreeElems.curFind := findState;  //Restore previous Finding
      repeat
        //Usar FindNextFunc, es la forma es eficiente, porque retoma la búsqueda anterior.
        xfun := TreeElems.FindNextFuncName;
      until (xfun = nil) or xfun.SameParamsType(pars);
      Found := (xfun <> nil);
    end;
    if Found then begin
      //Ya se identificó a la función que cuadra con los parámetros
      {$IFDEF LogExpres} Op.txt:= cIn.tok; {$ENDIF}   //toma el texto
      {Ahora que ya sabe cuál es la función referenciada, captura de nuevo los
      parámetros, pero asignándola al parámetro que corresponde.}
      cIn.PosAct := posPar;
      OnReqStartCodeGen();   //Reactiva la generación de código
      RTstate := RTstate0;
      xfun.procParam(xfun);  //Antes de leer los parámetros
      if high(pars)+1>0 then begin
        CaptureParamsFinal(xfun.pars);  //evalúa y asigna
      end;
      //Se hace después de leer parámetros.
      AddCallerTo(xfun, posCall);  {Corrige posición de llamada, sino estaría apuntando
                                    al final de los parámetros}
      xfun.procCall(xfun, AddrUndef); //codifica el "JSR"
      if AddrUndef then begin
        //"xfun" doesn't have been implemented.
//        GenError('Cannot get address of %s', [xfun.name]);
//        exit;
      end;
      RTstate := xfun.typ;  //para indicar que los RT están ocupados
      Op.SetAsExpres(xfun.typ);
      exit;
    end else begin
      //Encontró la función, pero no coincidió con los parámetros
      GenError(ER_TYP_PARM_ER_, [ele.name + '()']);
      exit;
    end;
  end else if ele.idClass = eltInLin then begin  //es función INLINE
//    {Se sabe que es función, pero no se tiene la función exacta porque puede haber
//     versiones, sobrecargadas de la misma función.}
//    posCall := cIn.ReadSrcPos;   //guarda la posición de llamada.
//    cIn.Next;    //Toma identificador
//    cIn.SkipWhites;  //Quita posibles blancos
//    posPar := cIn.PosAct;   //Guarda porque va a pasar otra vez por aquí
//    OnReqStopCodeGen();
//    RTstate0 := RTstate;    //Guarda porque se va a alterar con CaptureParams().
//    {Crea func0 localmente, para permitir la recursividad en las llamadas a las funciones.
//    Adicionalmente, debería plantearse otro método en la exploración de parámetros, tal
//    vez la creeación de un árbol de funciones sobrecargdas. Y así se podría incluso
//    implementar más fácilmente la adaptación de parámetros como byte->word.}
//    try
//      func0 := TxpEleFun.Create;  //crea la función 0, para uso interno
//      CaptureParams(func0);  //primero lee parámetros
//      if HayError then begin
//        exit;
//      end;
//      //Aquí se identifica la función exacta, que coincida con sus parámetros
//      xfun := TxpEleFun(ele);
//      //Primero vemos si la primera función encontrada, coincide:
//      if func0.SameParams(xfun) then begin
//        //Coincide
//        Found := true;
//      end else begin
//        //No es, es una pena. Ahora tenemos que seguir buscando en el árbol de sintaxis.
//        repeat
//          //Usar FindNextFunc, es la forma eficiente, porque retoma la búsqueda anterior.
//          xfun := TreeElems.FindNextFuncName;
//        until (xfun = nil) or func0.SameParams(xfun);
//        Found := (xfun <> nil);
//      end;
//      if Found then begin
//        //Ya se identificó a la función que cuadra con los parámetros
//        {$IFDEF LogExpres} Op.txt:= cIn.tok; {$ENDIF}   //toma el texto
//        {Ahora que ya sabe cúal es la función referenciada, captura de nuevo los
//        parámetros, pero asignándola al parámetro que corresponde.}
//        cIn.PosAct := posPar;
//        OnReqStartCodeGen();
//        RTstate := RTstate0;
//        xfun.procParam(xfun);  //Antes de leer los parámetros
//        if high(func0.pars)+1>0 then
//          CaptureParamsFinal(xfun);  //evalúa y asigna
//        //Se hace después de leer parámetros, para tener información del banco.
//        AddCallerTo(xfun, posCall);  {Corrige posición de llamada, sino estaría apuntando
//                                      al final de los parámetros}
//        xfun.procCall(xfun); //codifica el "CALL"
//        RTstate := xfun.typ;  //para indicar que los RT están ocupados
//        Op.SetAsExpres(xfun.typ);
//        exit;
//      end else begin
//        //Encontró la función, pero no coincidió con los parámetros
//        GenError(ER_TYP_PARM_ER_, [ele.name + '()']);
//        exit;
//      end;
//    finally
//      func0.Destroy;
//    end;
  end else begin
    GenError(ER_NOT_IMPLEM_, [ele.name]);
    exit;
  end;
end;
procedure TCompilerBase.GetOperand(out Op: Toperand; opMode: TOpReadMode);
{This is part of the Expression analyzer function. Read and, optionally, can generate
code to read an operand. The operand is returned in the parameter "Op", by reference, to
avoid using a local copy (like "Result").
In some cases can modify the global variable "res".
When "ToSet" is TRUE it forces to mantain the operand with its original storage.
Otherwise, the storages like stVarRef o stExpRef, are simplified to stExpres. This is
useful when compiling assignments.}
var
  xfun  : TxpEleFun;
  tmp, oprTxt, typName, str: String;
  Op1    : TOperand;
  posAct: TPosCont;
  opr   : TxpOperator;
  cod   , ascCode: Longint;
  ReadType, endWithComma, AddrUndef: Boolean;
  itType: TxpEleType;
  xtyp : TxpEleType;
  srcpos: TSrcPos;
  nElem: Integer;
begin
  //cIn.SkipWhites;
  ProcComments;
  Op.logic := logNormal;   //inicia campo
  if cIn.tokType = tnNumber then begin  //constantes numéricas
    {$IFDEF LogExpres} Op.txt:= cIn.tok; {$ENDIF}   //toma el texto
    TipDefecNumber(Op, cIn.tok); //encuentra tipo de número, tamaño y valor
    if HayError then exit;  //verifica
    cIn.Next;    //Pasa al siguiente
  end else if cIn.tokType = tnChar then begin  //constante caracter
    {$IFDEF LogExpres} Op.txt:= cIn.tok; {$ENDIF}   //toma el texto
    if not TryStrToInt(copy(cIn.tok, 2), cod) then begin
      GenError(ER_IN_CHARACTER);   //tal vez, sea muy grande
      exit;
    end;
    if (cod<0) or (cod>255) then begin
      GenError(ER_INV_COD_CHAR);
      exit;
    end;
    Op.SetAsConst(typChar);
    Op.valInt := cod;
    cIn.Next;    //Pasa al siguiente
  end else if cIn.tokType = tnString then begin  //constante cadena
    srcpos := cIn.ReadSrcPos;
    nElem := length(cIn.tok) - 2;  //Don't consider quotes
    str := copy(cIn.tok, 2, nElem);
    cIn.Next;    //Pasa al siguiente
    while cIn.tokType = tnChar do begin  //like #255
      //Concat the next char to simulate concat, considering there is not a
      //string type.
      if TryStrToInt(Copy(cIn.tok,2,3), ascCode) then begin
        str += chr(ascCode and $FF);
        cIn.Next;    //Pasa al siguiente
        inc(nElem);
      end else begin
        GenError(ER_IN_CHARACTER);   //Casi seguro que es el caracter "#" solo.
        exit;
      end;
    end;
    {$IFDEF LogExpres} Op.txt:= cIn.tok; {$ENDIF}   //toma el texto
    if length(str) = 1 then begin
      //De un caracter. Se asume de tipo Char
      Op.SetAsConst(typChar);
      Op.valInt := ord(str[1]);
    end else begin
      //Will be considered as array of char
      if not TreeElems.ExistsArrayType(typChar, nElem, xtyp) then begin
        //There is not a similar type. We create a new type.
        typName := GenArrayTypeName('char', nElem); //Op.nItems won't work
        xtyp := CreateEleTypeArray(typName, srcPos, typChar, nElem);
        //Add to the syntax tree
        xtyp.location := curLocation;   //Ubicación del tipo (Interface/Implementation/...)
        if TreeElems.curNode.idClass = eltBody then begin
          //This should be the normal position where we espect to have a lieral string
          //We prefer to declare the type in the parent (procedure or main)
          TreeElems.AddElementParent(xtyp, true);  //Add at the beginning
        end else begin
          //This shouldn't appear here.
          TreeElems.AddElement(xtyp);
        end;
      end;
      Op.SetAsConst(xtyp);
      Op.StringToArrayOfChar( str );
      //Op.SetAsConst(typString);
      //Op.valStr := copy(cIn.tok, 2, length(cIn.tok)-2);
    end;
  end else if (cIn.tokType = tnSysFunct) or //función del sistema
              (cIn.tokL = 'boolean') or //"boolean" es de tipo "tnType"
              (cIn.tokL = 'byte') or    //"byte" es de tipo "tnType"
              (cIn.tokL = 'word') or    //"word" es de tipo "tnType"
              (cIn.tokL = 'dword') then begin  //"dword" es de tipo "tnType"
    {Se sabe que es función, pero no se tiene la función exacta porque puede haber
     versiones, sobrecargadas de la misma función.}
    tmp := UpCase(cIn.tok);  //guarda nombre de función
    cIn.Next;    //Toma identificador
    //Busca la función
    for xfun in listFunSys do begin
      if (xfun.uname = tmp) then begin
        {Encontró. Llama a la función de procesamiento, quien se encargará de
        extraer los parámetros y analizar la sintaxis.}
        if xfun.compile<>nil then begin
          {LLeva la cuenta de llamadas, solo cuando hay subrutinas. Para funciones
           INLINE, no vale la pena, gastar recursos.}
          AddCallerTo(xfun);
        end;
        xfun.procCall(xfun, AddrUndef);  //Para que devuelva el tipo y codifique el _CALL o lo implemente
        //Puede devolver typNull, si no es una función.
        Op := res;  //copia tipo, almacenamiento y otros campos relevantes
        {$IFDEF LogExpres} Op.txt:= tmp; {$ENDIF}    //toma el texto
        exit;
      end;
    end;
    GenError(ER_NOT_IMPLEM_);
  end else if cIn.tokType = tnIdentif then begin  //puede ser variable, constante, función
    GetOperandIdent(Op, opMode);
    //Puede salir con error.
  end else if cIn.tokType = tnBoolean then begin  //true o false
    {$IFDEF LogExpres} Op.txt:= cIn.tok; {$ENDIF}   //toma el texto
    TipDefecBoolean(Op, cIn.tok); //encuentra tipo y valor
    if HayError then exit;  //verifica
    cIn.Next;    //Pasa al siguiente
  end else if cIn.tok = '(' then begin  //"("
    cIn.Next;
    Inc(ExprLevel);  //cuenta el anidamiento
    Op := GetExpression(0);
    Dec(ExprLevel);
    if HayError then exit;
    If cIn.tok = ')' Then begin
       cIn.Next;  //lo toma
        if (cIn.tok = '.') or (cIn.tok = '[') then begin
         IdentifyField(Op, opMode);
         Op := res;  //notar que se usa "res".
         if HayError then exit;;
       end;
    end Else begin
       GenError(ER_IN_EXPRESSI);
       Exit;       //error
    end;
  end else if cIn.tok = '[' then begin  //Constant array
   //Here we only know the operand is an array
    srcpos := cIn.ReadSrcPos;
    cIn.Next;
    ProcComments;
    //Start reading the items
    ReadType := true;  //Set flag to read the item type
    Op.InitItems;  //Prepare filling constant items
    while not cIn.Eof and (cIn.tok <> ']') do begin
      //Must be an item
      Op1 := GetExpression(0);  //read item
      if HayError then exit;
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
        exit;
      end;
      //Add the item to the operand
      Op.AddConsItem(Op1.Value);
      //Verify delimiter
      endWithComma := false;
      if cIn.tok = ',' then begin
        cIn.Next;
        ProcComments;
        endWithComma := true;
      end;
    end;
    if endWithComma then begin
      GenError('Expected item.');
      exit;
    end;
    if cIn.tok = ']' then begin
      //Raise the end of array. Now we can create new type.
      Op.CloseItems;  //Resize
      //Now we can know the type of the item and of the array
      cIn.Next;  //Take ']'.
      nElem := Op.Value.nItems;
      if nElem = 0 then itType := typNull;  //Something like []
      if not TreeElems.ExistsArrayType(itType, nElem, xtyp) then begin
        //The type doesn't exist. We need to create.
        typName := GenArrayTypeName(itType.name, nElem); //Op.nItems won't work
        xtyp := CreateEleTypeArray(typName, srcPos, itType, nElem);
        //Add to the syntax tree
        xtyp.location := curLocation;   //Ubicación del tipo (Interface/Implementation/...)
        if TreeElems.curNode.idClass = eltBody then begin
          //When the array is declared in some code-section.
          TreeElems.AddElementParent(xtyp, true);  //Add at the beginning
        end else begin
          //In declaration section.
          TreeElems.AddElement(xtyp);
        end;
      end;
      //Finally we set the operand type.
      Op.SetAsConst(xtyp);
    end else if cIn.Eof then begin
      GenError('Unexpected end of file');
      exit;
    end else begin  //Only happen when break loop
      exit;
    end;
  end else if cIn.tokType = tnOperator then begin
    {Si sigue un operador puede ser un operador Unario.
    El problema que tenemos, es que no sabemos de antemano el tipo, para saber si el
    operador aplica a ese tipo como operador Unario Pre. Así que asumiremos que es así,
    sino retrocedemos.}
    posAct := cIn.PosAct;   //Esto puede ser pesado en términos de CPU
    oprTxt := cIn.tok;   //guarda el operador
    cIn.Next; //pasa al siguiente
    GetOperand(Op1, opMode);   //Takes the operand.
    if HayError then exit;
    //Ahora ya tenemos el tipo. Hay que ver si corresponde el operador
    opr := Op1.Typ.FindUnaryPreOperator(oprTxt);
    if opr = nullOper then begin
      {Este tipo no permite este operador Unario (a lo mejor ni es unario)}
      cIn.PosAct := posAct;
      GenError(ER_CAN_AP_OPER_, [oprTxt, Op1.Typ.name]);
      exit;
    end;
    //Sí corresponde. Así que apliquémoslo
    OperPre(Op1, opr);
    Op := res;
  end else begin
    //No se reconoce el operador
    GenError(ER_OPERAN_EXPEC);
  end;
end;
function TCompilerBase.GetOperator(const Op: Toperand): TxpOperator;
{Busca la referencia a un operador de "Op", leyendo del contexto de entrada
Si no encuentra un operador en el contexto, devuelve NIL, pero no lo toma.
Si el operador encontrado no se aplica al operando, devuelve nullOper.}
begin
  if cIn.tokType <> tnOperator then exit(nil);
  //Hay un operador
  Result := Op.Typ.FindBinaryOperator(cIn.tok);
  if Result = nullOper then begin
    //No lo encontró, puede ser oeprador unario
    Result := Op.Typ.FindUnaryPostOperator(cIn.tok);
  end;
  cIn.Next;   //toma el token
end;
function TCompilerBase.GetExpression(const prec: Integer; opMode: TOpReadMode = opmGetter): TOperand; //inline;
{Expression analyzer. This is probably, the most important function of the compiler
 It process an expression in the current input context, and call events in order to
 the expression be compiled (or interpreted if we implement an interpreter).
 Returns an operand containing information about the result of the expression.
 Expression evaluation stops when no more operators founds or found an operator
 with precedence smaller than parameter "prec".}
var
  Op1, Op2  : TOperand;   //Operandos
  opr1: TxpOperator;  //Operadores
  p: TPosCont;
  nOpern: Integer;
begin
  nOpern := 0;  //Clear counter for operands
  //----------------Get first operand------------------
  GetOperand(Op1, opMode);
  if HayError then exit;
  //Verifica si termina la expresion
  cIn.SkipWhites;
  p := cIn.PosAct;  //por si necesita volver
  inc(nOpern);      //We have Op1
  opr1 := GetOperator(Op1);
  if opr1 = nil then begin  //no sigue operador
    //Expresión de un solo operando. Lo carga por si se necesita
    //Oper(Op1);
    Result:=Op1;
    exit;  //termina ejecucion
  end;
  //------- Follows an operator ---------
  //Verifica si el operador aplica al operando
  if opr1 = nullOper then begin
    GenError(ER_UND_OPER_TY_, [opr1.txt, Op1.Typ.name]);
    exit;
  end;
  //inicia secuencia de lectura: <Operador> <Operando>
  while opr1<>nil do begin
    //¿Delimitada por precedencia?
    If opr1.prec<= prec Then begin  //es menor que la que sigue, expres.
      Result := Op1;  //solo devuelve el único operando que leyó
      cIn.PosAct := p;  //vuelve
      exit;
    End;
    if opr1.OperationPost<>nil then begin  //Verifica si es operación Unaria
      OperPost(Op1, opr1);
      if HayError then exit;
      Op1 := res;
      cIn.SkipWhites;
      //Verificación
      if (cIn.tok = '.') or (cIn.tok = '[') then begin
        IdentifyField(Op1, opMode);
        if HayError then exit;;
        Op1 := res;  //notar que se usa "res".
        cIn.SkipWhites;
      end;
      p := cIn.PosAct;  //actualiza por si necesita volver
      //Verifica operador
      opr1 := GetOperator(Op1);
      continue;
    end;
    //--------------------coger segundo operando--------------------
//    Op2 := GetOperandPrec(Opr1.prec);   //toma operando con precedencia
    Op2 := GetExpression(Opr1.prec);   //toma operando con precedencia
    if HayError then exit;
    inc(nOpern);   //We have a new operand
    //prepara siguiente operación
    Oper(Op1, opr1, Op2);    //evalua resultado en "res"
    Op1 := res;
    if HayError then exit;
    cIn.SkipWhites;
    opr1 := GetOperator(Op1);   {lo toma ahora con el tipo de la evaluación Op1 (opr1) Op2
                                porque puede que Op1 (opr1) Op2, haya cambiado de tipo}
  end;  //hasta que ya no siga un operador
  Result := Op1;  //aquí debe haber quedado el resultado
  Result.nOpern := nOpern;   //Update counter
end;
procedure TCompilerBase.GetExpressionE(posExpres: TPosExpres);
{Used to compile complete expressions. It means expressions that can be considered as
complete Pascal instructions, like:
  x := 1;        //Assigment
  proc1(1,2,3);  //Call to procedure/fucntion
  p^ := 1 + x;   //Assigment
  obj.field := funct1();  //Assigment
To process subexpresions (ROB o ROU) or function parameters, it must be used
GetExpression().
No result are returned, because for definition, instructions doesn't return values.
}
var
  Op1, Op2: TOperand;
  opr1: TxpOperator;
begin
  Inc(ExprLevel);  //Count the nesting
  {$IFDEF LogExpres} LogExpLevel('>Inic.expr'); {$ENDIF}
  OnExprStart;  //Call event
  try
    {First we stop code generation, because we don't know if the instruction is a getter:
      proc()
    or a setter:
      a := b;
    }
    {First operand compiled always as setter, because it's supposed to be
    de first part of an assigment. We use GetExpression() (instead of GetOperand() because
    we want to consider expressions like p^.something to be conisedered as a whole operand.}
    //Compile as setter until an operator of precedence of ":=" (2).
    Op1 := GetExpression(2, opmSetter);
    if HayError then exit;
    ProcComments;
    opr1 := GetOperator(Op1); //We can validate assigment
    if opr1 = nil then begin
      //Not following an operator. We assume instruction ends;
      if (Op1.Sto  = stConst) {and (Op1.Typ<>typNull)} then begin
        //Instruction alone is not valid
        GenError('Constants are not allowed here.');
      end;
    end else begin
      //We have an operator.
      case opr1.txt of
      ':=', '+=', '-=' : begin  end;  //Expected
      else
        GenError('Expected ":=", "+=" or "-=".');
        exit;
      end;
      Op2 := GetExpression(Opr1.prec); //Value to assign
      if HayError then exit;
      //Call de operation
      Oper(Op1, opr1, Op2);    //Compile operation
      //Can generate error
    end;
  finally
    OnExprEnd(posExpres);    //Call end event
    {$IFDEF LogExpres} LogExpLevel('>Fin.expr'); {$ENDIF}
    Dec(ExprLevel);
    {$IFDEF LogExpres}
    if ExprLevel = 0 then debugln('');
    {$ENDIF}
  end;
end;
procedure TCompilerBase.LoadToRT(Op: TOperand);
{Carga un operando a los Registros de Trabajo (RT).}
begin
  if Op.Typ.OnLoadToRT=nil then begin
    //No implementado
    GenError(ER_NOT_IMPLEM_);
  end else begin
    Op.Typ.OnLoadToRT(@Op);
  end;
end;
function TCompilerBase.ExpandRelPathTo(BaseFile, FileName: string): string;
{Convierte una ruta relativa (FileName), a una absoluta, usnado como base la ruta de
otro archivo (BaseFile)}
var
  BasePath: RawByteString;
begin
   if pos(DirectorySeparator, FileName)=0 then begin
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

function TCompilerBase.hexFilePath: string;
begin
  Result := ExpandRelPathTo(mainFile, hexfile); //Convierte a ruta absoluta
end;
function TCompilerBase.mainFilePath: string;
begin
  Result := mainFile;
end;
function TCompilerBase.GenerateUniqName(base: string): string;
{Generate a unique name using the string "base" as prefix.}
begin
  exit(base + IntToStr(elemCnt));
  inc(elemCnt);
end;
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
    fun, fun2: TxpEleFun;
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
        //Incluyendo a funciones del sistema
        for fun2 in listFunSys do begin
          fun2.RemoveCallsFrom(fun.BodyNode);
        end;
      end;
    end;
  end;
var
  noUsed, noUsedPrev: Integer;
begin
  //Explora las funciones, para identifcar a las no usadas
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
    xvar, xvar2: TxpEleVar;
    fun: TxpEleFun;
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
    cons, cons2: TxpEleCon;
    xvar: TxpEleVar;
    fun: TxpEleFun;
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
//            debugln('Eliminando llamada a %s desde: %s', [cons2.name, xvar.name]);
        end;
      end;
    end;
    //Ahora quita las referencias de funciones no usadas
    for fun in TreeElems.AllFuncs do begin
      if fun.nCalled = 0 then begin
        //Esta es una función no usada
        inc(Result);   //Lleva la cuenta
        for cons2 in TreeElems.AllCons do begin
          cons2.RemoveCallsFrom(fun.BodyNode);
//          debugln('Eliminando llamada a %s desde: %s', [cons2.name, cons.name]);
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
    cons: TxpEleCon;
    xvar: TxpEleVar;
    xtyp, xtyp2: TxpEleType;
    fun : TxpEleFun;
  begin
    Result := 0;
    {Quita, a los tipos, las referencias de constantes no usadas (de ese tipo).}
    for cons in TreeElems.AllCons do begin
      if cons.nCalled = 0 then begin
        //Esta es una constante no usada
        inc(Result);   //Lleva la cuenta
        //Quita las llamadas que podría estar haciendo a otras constantes
        for xtyp in TreeElems.AllTypes do begin
          xtyp.RemoveCallsFrom(cons);
//            debugln('Eliminando llamada a %s desde: %s', [xtyp.name, cons.name]);
        end;
      end;
    end;
    {Quita, a los tipos, las referencias de variables no usadas (de ese tipo).}
    for xvar in TreeElems.AllVars do begin
      if xvar.nCalled = 0 then begin
        //Esta es una variable no usada
        inc(Result);   //Lleva la cuenta
        //Quita las llamadas que podría estar haciendo a constantes
        for xtyp in TreeElems.AllTypes do begin
          xtyp.RemoveCallsFrom(xvar);
//            debugln('Eliminando llamada a %s desde: %s', [xtyp.name, xvar.name]);
        end;
      end;
    end;
    {Quita, a los tipos, las referencias de otros tipos no usadas.
    Como en los tipos que se crean a partir de otros tipos}
    for xtyp2 in TreeElems.AllTypes do begin
      if xtyp2.nCalled = 0 then begin
        //Esta es una variable no usada
        inc(Result);   //Lleva la cuenta
        //Quita las llamadas que podría estar haciendo a constantes
        for xtyp in TreeElems.AllTypes do begin
          xtyp.RemoveCallsFrom(xtyp2);
//            debugln('Eliminando llamada a %s desde: %s', [xtyp.name, xtyp2.name]);
        end;
      end;
    end;
    //Ahora quita las referencias de funciones no usadas (de ese tipo)
    for fun in TreeElems.AllFuncs do begin
      if fun.nCalled = 0 then begin
        //Esta es una función no usada
        inc(Result);   //Lleva la cuenta
        for xtyp in TreeElems.AllTypes do begin
          xtyp.RemoveCallsFrom(fun.BodyNode);
//          debugln('Eliminando llamada a %s desde: %s', [xtyp.name, cons.name]);
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
    noUsed := RemoveUnusedTypReferences;
  until noUsed = noUsedPrev;   //Ya no se eliminan más constantes
end;
procedure TCompilerBase.UpdateCallersToUnits;
{Explora recursivamente el arbol de sintaxis para encontrar( y agregar) las
llamadas que se hacen a una unidad desde el programa o unidad que la incluye.
El objetivo final es determinar los accesos a las unidades.}
  procedure ScanUnits(nod: TxpElement);
  var
    ele, eleInter , eleUnit: TxpElement;
    uni : TxpEleUnit;
    cal , c: TxpEleCaller;
  begin
//debugln('+Scanning in:'+nod.name);
    if nod.elements<>nil then begin
      for ele in nod.elements do begin
        //Solo se explora a las unidades
        if ele.idClass = eltUnit then begin
//debugln('  Unit:'+ele.name);
          //"ele" es una unidad de "nod". Verifica si es usada
          uni := TxpEleUnit(ele);    //Accede a la unidad.
          uni.ReadInterfaceElements; //Accede a sus campos
          {Buscamos por los elementos de la interfaz de la unidad para ver si son
           usados}
          for eleInter in uni.InterfaceElements do begin
//debugln('    Interface Elem:'+eleInter.name);
            //Explora por los llamadores de este elemento.
            for cal in eleInter.lstCallers do begin
              eleUnit := cal.CallerUnit;   //Unidad o programa
              if eleUnit = nod then begin
                {Este llamador está contenido en "nod". Lo ponemos como llamador de
                la unidad.}
                c := AddCallerTo(uni);
                c.caller := cal.caller;
                c.curPos := cal.curPos;
//                debugln('      Added caller to %s from %s (%d,%d)',
//                        [uni.name, c.curPos.fil, c.curPos.row, c.curPos.col]);
              end;
            end;
          end;
          //Ahora busca recursivamente, por si la unidad incluyea a otras unidades
          ScanUnits(ele);  //recursivo
        end;
      end;
    end;
  end;
begin
  ScanUnits(TreeElems.main);
end;
//Inicialización
constructor TCompilerBase.Create;
begin
  ClearError;   //inicia motor de errores
  //Crea arbol de elementos y listas
  TreeElems  := TXpTreeElements.Create;
  TreeDirec  := TXpTreeElements.Create;
  listFunSys := TxpEleFuns.Create(true);
  listTypSys := TxpEleTypes.Create(true);
  //inicia la sintaxis
  xLex := TSynFacilSyn.Create(nil);   //crea lexer

  cIn := TContexts.Create(xLex); //Crea lista de Contextos
  ejecProg := false;
  //Actualiza las referencias a los tipos de tokens existentes en SynFacilSyn
  tnEol     := xLex.tnEol;
  tnSymbol  := xLex.tnSymbol;
  tnSpace   := xLex.tnSpace;
  tnIdentif := xLex.tnIdentif;
  tnNumber  := xLex.tnNumber;
  tnKeyword := xLex.tnKeyword;
  tnString  := xLex.tnString;
  tnComment := xLex.tnComment;
  //Atributos
  tkEol     := xLex.tkEol;
  tkSymbol  := xLex.tkSymbol;
  tkSpace   := xLex.tkSpace;
  tkIdentif := xLex.tkIdentif;
  tkNumber  := xLex.tkNumber;
  tkKeyword := xLex.tkKeyword;
  tkString  := xLex.tkString;
  tkComment := xLex.tkComment;
  //Crea nuevos tipos necesarios para el Analizador Sintáctico
  tnOperator := xLex.NewTokType('Operator', tkOperator); //necesario para analizar expresiones
  tnBoolean  := xLex.NewTokType('Boolean', tkBoolean);  //constantes booleanas
  tnSysFunct := xLex.NewTokType('SysFunct', tkSysFunct); //funciones del sistema
  tnType     := xLex.NewTokType('Types', tkType);    //tipos de datos
  //Containers for functions
  usedFuncs := TxpEleFuns.Create(false);     //Only references
  unusedFuncs:= TxpEleFuns.Create(false);
end;
destructor TCompilerBase.Destroy;
begin
  unusedFuncs.Destroy;
  usedFuncs.Destroy;
  cIn.Destroy; //Limpia lista de Contextos
  xLex.Free;
  listTypSys.Destroy;
  listFunSys.Destroy;
  TreeDirec.Destroy;
  TreeElems.Destroy;
  inherited Destroy;
end;

end. //2886
