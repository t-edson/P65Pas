{
XpresElemP65
============
By Tito Hinostroza.

Basic definitions for syntax elements used in the Abstract Syntax Tree.
This unit includes some definitions (like variable storage) that are dependent for the
hardware architecture}
unit XpresElemP65;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, TypInfo, LexPas, LCLProc, StrUtils;
const
  ADRR_ERROR = $FFFF;
const //Prefixes used to name anonym type declarations
  //Short string are used to don't affect the searching speed
  PREFIX_ARR = 'ar';
  PREFIX_PTR = 'pt';
  PREFIX_OBJ = 'ob';
  //Name for elements
  TIT_BODY_ELE = 'Body';
type  //Hardware dependent definitions
  { TODO : Este tipo es muy dependiente del hardware y no debería estar aquí }
  { TCpuRegister }
  {Used to modelate an internal CPU register}
  TCpuRegister = object
    used    : boolean;   //Indica si está usado.
  end;

  { TxpAdicDeclar }
  {Define aditional declaration settings for variable. Depends on target CPU architecture.}
  TxpAdicDeclar = (
    decNone,   //Normal declaration. Will be mapped in free RAM.
    decAbsol,  //Mapped in ABSOLUTE address
    decRegis,  //Mapped at WR
    decRegisA, //Mapped at A register
    decRegisX, //Mapped at X register
    decRegisY  //Mapped at Y register
  );
type  //Previous definitions for elements
  TVarOffs = word;

  TxpElement = class;
  TxpElements = specialize TFPGObjectList<TxpElement>;

  { TxpEleCaller }
  //Information about the call to one element from other element.
  TxpEleCaller = class
    curPos: TSrcPos;    //Position from where it is called this element.
    caller: TxpElement; //Element that calls this element (Function body or variable).
    function CallerUnit: TxpElement;  //Unit/Program from where it is called this element.
  end;
  TxpListCallers = specialize TFPGObjectList<TxpEleCaller>;

  //Datos de las llamadas que se hacen a otro elemento
//  TxpEleCalled = class
//    curPos: TSrcPos;    //Posición desde donde es llamado
//    curBnk: byte;       //banco RAM, desde donde se llama
//    called: TxpElement; //función que llama a esta función
//  end;
  TxpListCalled = specialize TFPGObjectList<TxpElement>;

  //Groups of data types.
  TTypeGroup=(
    t_integer,  //Signed integer numbers
    t_uinteger, //Unsigned integer numbers
    t_float,    //Float numbers
    t_boolean,  //Booleans
    t_string,   //String of chars
    t_enum  ,   //Enumerated. { TODO : Check if needed }
    t_object    //Object (contain fields)
  );

const
  CONS_ITEM_BLOCK = 20;

type
  { TConsValue }
  {Structure to store all the possible values for a constant.
  Must have fields for all basic types defined in "TTypeGroup" and for composed
  values}
  TConsValue = object
  public
    ValInt  : Int64;    //For values t_integer y t_uinteger
    ValFloat: extended; //For values t_float
    ValBool : boolean;  //For values t_boolean
    ValStr  : string;   //For values t_string
  public //Arrays
    items   : array of TConsValue;  //Ítems list
    nItems  : integer;  //Number of items
    curSize : integer;
    procedure InitItems;
    procedure AddConsItem(const c: TConsValue);
    procedure CloseItems;
  public  //Access to ValInt
    function LByte: byte; inline;  //Returns low byte of integer value.
    function HByte: byte; inline;  //Returns high byte of integer value.
    function EByte: byte; inline;
    function UByte: byte; inline;
    function valuesAsString: string;
  end;

  TProcDefineVar = procedure(const varName, varInitVal: string) of object;

  TEleExpress = class;
  {Event to load operand to a WR
  "OpPtr" should be "TOperand", but it's not yet defined.}
  TProcLoadOperand = procedure(fun: TEleExpress) of object;

  TEleVarDec = class;

  TEleConsDec = class;

  {Description for aditional information in variables declaration: ABSOLUTE ,
  REGISTER,  or initialization. }
  TAdicVarDec = record
    hasAdic  : TxpAdicDeclar; //ABSOLUTE, REGISTERA, ...
    absVar   : TEleVarDec;    //Reference to variable, when is ABSOLUTE <variable>
    absAddr  : integer;       //ABSOLUTE address
    absOff   : integer;       //Offset to variable when ABSOLUTE.
    hasInit  : boolean;       //Indicate if variable is initialized
    constDec : TEleConsDec;   //Reference to the const declaration for init value.
  end;
type  //Type categories and declaration styles
  //Type categories
  TxpCatType = (
    tctAtomic,  //Basic types as Byte, Word or Char.
    tctArray,   //Array of some other type.
    tctPointer, //Pointer to other type.
    tctObject   //Record with several fields
  );
  {Types categories define the way a type is structured.

  ==== ATOMIC ====
  We say a type is atomic, when it cannot be expressed as a construction of other type.
  For example: CHAR or BYTE types. WORD type should be atomic too. Although a WORD can be
  expressed as an OBJECT. Here in P65Pas we define WORD as atomic.
  Declaraction for atomic types are:
  TYPE
    mytype = byte;
    mytype2 = char;
    mytype3 = mytype;  //Because "mytype" is tomic too.

  ==== ARRAY ====
  Array of some other type (atomic or not).
  Declaration for array types are:
  TYPE
    artype = ARRAY[10] OF byte;
    otherarray = artype;  //Because artype is array
    alsoarray = ARRAY OF noAtomicType;

  As an alternative notation we can use is:
  TYPE
    artype = [10]byte;

  ==== POINTER ====
  Pointer to some other type (atomic or not).
  Declaration for pointer types are:
  TYPE
    ptrtype = POINTER TO byte;
    otherptr = ptrtype;  //Because ptrtype is pointer
    alsoptr = POINTER TO noAtomicType;

  As an alternative notation we can use is:
  TYPE
    artype = ^byte;

  }

  //Type declaration style.
  TTypDeclarStyle = (
    ttdDirect,  {Like:
                      TYPE mytype = byte;
                      TYPE mytype2 = mytype;  //"mytype" could be ARRAY/POINTER/OBJECT
                }
    ttdDeclar   {Like:
                      TYPE mytype = ARRAY[30] OF char;
                      TYPE refchar = POINTER TO char; }
  );

type  //TxpElement class
  //Element types for the language.
  TxpIDClass = (//Declaraction
                eleNone,      //No type
                eleVarDec,    //Variable declaration
                eleConsDec,   //Constant declaration
                eleTypeDec,   //Type declaration
                eleFuncDec,   //Function declaration
                eleFunc,      //Function
                //Structural
                eleBody,      //Body procedure/program
                eleBlock,     //Block of code
                eleProgFrame, //Code container
                eleProg,      //Main program
                eleUnit,      //Unit
                eleFinal,     //FINALIZATION section
                //Expressions
                eleExpress,   //Expression
                eleAsmOperat, //ASM Operation
                eleCondit,    //Condition
                //Instructions relative
                eleSenten,    //Sentence/Instruction
                eleAsmInstr,  //ASM instruction
                eleAsmBlock   //ASM block
                );
  TxpEleCodeCont = class;
  { TxpElement }
  //Base class for all syntactic elements
  TxpElement = class
  private
    Fname    : string;   //Element name
    Funame   : string;   //Upper case name. Used to acelerate searchings.
    procedure Setname(AValue: string);
  public  //Callers management
    //List of functions that calls to this function.
    lstCallers: TxpListCallers;
    function nCalled: integer; virtual; //número de llamadas
    function IsCalledBy(callElem: TxpElement): boolean; //Identifica a un llamador
    function IsCalledByChildOf(callElem: TxpElement): boolean; //Identifica a un llamador
    function IsCalledAt(callPos: TSrcPos): boolean;
    function IsDeclaredAt(decPos: TSrcPos): boolean;
    function FindCalling(callElem: TxpElement): TxpEleCaller; //Identifica a un llamada
    function RemoveCallsFrom(callElem: TxpElement): integer; //Elimina llamadas
    procedure RemoveLastCaller; //Elimina la última llamada
    procedure ClearCallers;  //limpia lista de llamantes
//    function ExistsIn(list: TxpElements): boolean;
  public  //Gestión de los elementos llamados
    curNesting: Integer;   //Nested level for calls
    maxNesting: Integer;   //Max nested level for calls
    //Lista de funciones que son llamadas directamente (Se llena en el enlazado)
    lstCalled : TxpListCalled;
    //Lista de funciones que son llamadas dirceta o indirectamente (Se llena en el enlazado)
    lstCalledAll: TxpListCalled;
    //Métodos para el llemado
    procedure AddCalled(elem: TxpElement);
    function UpdateCalledAll: integer;
  public
    Parent  : TxpElement;  //Reference to parent element
    idClass : TxpIDClass;  //To avoid use RTTI
    elements: TxpElements; //Container list for other elements
    location: TxpEleLocation;  //Element location
    codCont : TxpEleCodeCont;  //Temporal field for Code container.
    property name: string read Fname write Setname;
    property uname: string read Funame;
    function Path: string;
    function FindElemName(const eName: string; out ele: TxpElement): boolean;
    function FindIdxElemName(const eName: string; var idx0: integer): boolean;
    function LastNode: TxpElement;
    function Index: integer;
    function AddElement(elem: TxpElement; position: integer = - 1): TxpElement;
  public  //Location in the source code.
    //Where the element is declared.
    srcDec: TSrcPos;
    {Ending location of the element. Useful in elements TxpEleBody, to limit the
    block of code.}
    srcEnd: TSrcPos;
    function posXYin(const posXY: TPoint): boolean;
  public  //Inicialización
    procedure Clear; virtual;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

type  //Declaration elements
  {Base class to derivate "Program frame structures".}
  TxpEleCodeCont = class(TxpElement)
  end;

  TEleTypeDec= class;
  TEleTypeDecs= specialize TFPGObjectList<TEleTypeDec>; //lista de variables

  { TxpEleType }
  {Clase para modelar a los tipos definidos por el usuario y a los tipos del sistema.
  Es una clase relativamente extensa, debido a la flxibilidad que ofrecen lso tipos en
  Pascal.}
  TEleTypeDec = class(TxpEleCodeCont)
  private
    fSize: SmallInt;
    internalTypes: TEleTypeDecs;  //Container for types recursively defined.
    function getSize: smallint;
    procedure setSize(AValue: smallint);
  public   //Events
    {Estos eventos NO se generan automáticamente en TCompilerBase, sino que es la
    implementación del tipo, la que deberá llamarlos. Son como una ayuda para facilitar
    la implementación.}
    OnSaveToStk : procedure of object; //Save data to stack.
    OnGlobalDef : TProcDefineVar; {Es llamado cada vez que se encuentra la declaración
                                  de una variable (de este tipo) en el ámbito global.}
    OnLoadToWR  : TProcLoadOperand;    //Used when required to load an operand in Work Register.
    OnRequireWR : procedure of object; //Used to detect dependencies on Work registers.
  public
    copyOf  : TEleTypeDec;  //Indicates this type is copy of other
    group   : TTypeGroup;   //Type group (numéric, string, etc)
    catType : TxpCatType;   //Categoría del tipo
    property size: smallint read getSize write setSize;   //Tamaño en bytes del tipo
    function groupStr: string;
    function catTypeStr: string;
  public   //Arrays and pointers
    consNitm: TEleConsDec;  //Reference to constant defining the number of items.
    itmType : TEleTypeDec;  {Reference to the item type when it's array.
                                TArr = array[255] of byte;  //itemType = byte
                            }
    isDynam : boolean;      //Indicates the size is dynamic. No current supported except when initialized.
    ptrType : TEleTypeDec;  {Reference to the type pointed, when it's pointer.
                                TPtr = ^integer;       //ptrType = integer
                           }
    function nItems: integer;  //Number of items, when is tctArray (-1 if it's dynamic.)
  public
    procedure SaveToStk;
  public   //Identificación
    function IsByteSize: boolean;
    function IsWordSize: boolean;
    function IsDWordSize: boolean;
    function IsArrayOf(itTyp: TEleTypeDec; numIt: integer): boolean;
    function IsPointerTo(ptTyp: TEleTypeDec): boolean;
    function IsEquivalent(typ: TEleTypeDec): boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TxpEleCon }
  //Class to modelate constants declaration.
  TEleConsDec = class(TxpEleCodeCont)
    //Element type
    typ: TEleTypeDec;
    {Flag to indicate if the constant value, stored in "value" field, is valid.
    If evaluated = true  -> The constant value can be read in "value".
    If evaluated = false -> The constant is not yet evaluated. It has been defined as an
                            expression, not yet evaluated.}
    evaluated: boolean;
    //Constant value
    value : TConsValue;
    constructor Create; override;
  end;
  TEleConsDecs = specialize TFPGObjectList<TEleConsDec>; //lista de constantes

  //Operand value storage. Hardware dependent.
  TStorage = (
    stNone     = %0000,  //Without storage.
    stConst    = %0001,  //Constant value.
    stRamFix   = %0010,  //In RAM. Address constant and defined.
    //stRamVar = %0100,  //In RAM. Address obtained reading a variable.
    stRamVarOf = %0011,  //In RAM. Address obtained reading a variable and offset.
    stRamReg   = %0100,  //In RAM. Address allocated in Work Register.

    stRegister = %1000,  //In default registers (Work register).
    stRegistA  = %1001,  //In register A. Only for byte-size variables.
    stRegistX  = %1010,  //In register X. Only for byte-size variables.
    stRegistY  = %1011   //In register Y. Only for byte-size variables.
  );
  {Mixed storage used to implement INLINE binary operands code. This is created to
  represent two TStorage values in a simple byte constant (joining bits), in order to
  facilitate the use in a CASE ... OF structure.}
  TStoOperandsBOR =(
    stConst_Const    = %00010001,
    stConst_RamFix   = %00010010,
    stConst_Regist   = %00011000,

    stRamFix_Const   = %00100001,
    stRamFix_RamFix  = %00100010,
    stRamFix_Regist  = %00101000,

    stRegist_Const   = %10000001,
    stRegist_RamFix  = %10000010,
    stRegist_Regist  = %10001000
  );

  //Operand type
  TopType = (
    otVariab,  {Operand is variable. Allows read/write.
               Support storages:
                - stRamFix
                - stRamReg
                - stRamVar
                - stRegister
                - stRegistA
                - stRegistX
                - stRegistY
               }
    otConst,   {Operand is constant. Only for read.
               Support storages:
                - stConst
                - stRamFix   //Special case
               }
    otFunct    {Operand is a function/method or expression result. Only for read.
               Support storages:
                - stRegister
                - stRegistA
                - stRegistX
                - stRegistY
               }
  );

  { TEleVarDec }
  //Class to modelate variable declarations.
  TEleVarDec = class(TxpEleCodeCont)
  private
    ftyp: TEleTypeDec;
    function Gettyp: TEleTypeDec;
    procedure Settyp(AValue: TEleTypeDec);
  public   //Manejo de parámetros adicionales
    adicPar: TAdicVarDec;  //Parámetros adicionales en la declaración de la variable.
  public
    //Flag to validate if varriable is used as parameter.
    IsParameter: boolean;
    {Indica si la variable es temporal, es decir que se ha creado solo para acceder a
    una parte de otra variable, que si tiene almacenamiento físico.}
    IsTmp      : boolean;
    //IsReg      : boolean;  //Flag to indicate the variable is used as register.
    required   : boolean;    {Indicates the variable is required to be allocated. Work
                              for variables used as registers. }
    //Reference to Type element
    property typ: TEleTypeDec read Gettyp write Settyp;
  public  //Campos para guardar las direcciones físicas asignadas en RAM.
    allocated: boolean;   //Activated when variable is allocated (RAM or register).
    storage  : TStorage;  //Depend on adicPar.hasAdic.
    addr     : word;      //Base address.
    function addrL: word; inline;  //Devuelve la dirección absoluta de la variable (LOW)
    function addrH: word; inline;  //Devuelve la dirección absoluta de la variable (HIGH)
    function addrE: word; inline;  //Devuelve la dirección absoluta de la variable (EXTRA)
    function addrU: word; inline;  //Devuelve la dirección absoluta de la variable (ULTRA)
    function AddrString: string; //Devuelve la dirección física como cadena
    procedure ResetAddress; //Limpia las direcciones físicas
    function stoStr: string;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;
  TEleVarDecs = specialize TFPGObjectList<TEleVarDec>;

type  //Expression elements
  //Valid operations for TEleAsmOperat
  TAsmInstOperation = (
    aopSelByte,  //Selecciona un byte
    aopAddValue, //Suma un valor
    aopSubValue  //Resta un valor
  );

  { TxpEleOperator }
  {Represent an operation in ASM expressions.}
  TEleAsmOperat= class(TxpElement)
    operation : TAsmInstOperation;   //Operation
    value     : word;                //Value
    constructor Create; override;
  end;

  TEleFunBase = class;

  { TEleExpress }
  {Represents an expression/operand. }
  TEleExpress = class(TxpElement)
  public
    opType : TopType;     //Operand type: otVariab, otConst, otFunct.
    Sto    : TStorage;    //Storage of the value (memory, register, value)
    Typ    : TEleTypeDec;  //Data type for the operand.
    function opTypeAsStr: string; //"opType" as string
    function StoAsStr: string;  //Storage as string
    procedure StringToArrayOfChar(str: string);
    function ValueIsZero: boolean;
  public  //Temporal variables required for evaluating expression.
    tempVars: TEleVarDecs;
  public  //Fields used when opType is otFunct.
    rfun   : TEleFunBase;  //Reference to function
    {When element is "otFunct", this flag indicates the function/method has been
    called using an operator instead of call the function by its name.}
    fcallOp: boolean;
  public  //Fields used when opType is otConst.
    evaluated: boolean;  //Activated when constant is evaluated.
    //Fields used when constant is solved but not evaluated.
    cons   : TEleConsDec;
    //Fields used when constant is evaluated, or it's a literal constant.
    value  : TConsValue;  //Constant value, when storage opType is otConst.
    function val: dword;
    function valL: word;
    function valH: word;
    function valU: word;
    function valE: word;
  public  //Set variable fields.
    procedure SetVariab(var0: TEleVarDec);
    procedure SetVariab(add0: word);
    procedure SetVariab_RamVarOf(var0: TEleVarDec; offset: integer);
  private //Fields used when opType is otVariab and Sto is stRamFix.
    dirVar: boolean;  //Flag that indicates the address should be read from "dirAdd".
    dirAdd: word;     //Physical address when this expression is not associated to a variable (rvar).
  public  //Fields used when opType is otVariab.
    //Fields used when variable is solved but not allocated.
    rvar   : TEleVarDec;  {Reference to variable, when variable is associated to a
                          variable declaration (stRamFix). Also is used to reference to
                          the variable with the final address (stRamVar or stRamVarOf).}
    //Fields used when variable is allocated.
    offs   : integer;    {Offset to address when storage is stRamVarOf.}
    function add: word;  {Base address.}
    function addL: word;
    function addH: word;
    function allocated: boolean; //Activated when variable is allocated (RAM or register).
  public  //Initialization
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TEleCondit }
  {Represents a condition or boolean expression. Used to represent conditional
  expression in conditional statements.}
  TEleCondit = class(TxpElement)
  public  //Initialization
    constructor Create; override;
  end;
type  //Structural elements

  { TxpExitCall }
//  //Clase que representa una llamada a la instrucción exit()
//  TxpExitCall = class
//    srcPos : TSrcPos;    //Posición en el código fuente
//    codeBlk: TxpEleCodeCont; {Must refer to a:
//                             - Body of a function/program.
//                             - The block section of a sentence that can contain block of
//                               code like: IF, FOR, WHILE, REPEAT.
//                             Other cases are not yet analyzed if are valid.  }
//    function IsObligat: boolean;
//  end;
//  TxpExitCalls = specialize TFPGObjectList<TxpExitCall>; //lista de variables

  { TxpEleBody }
  //Class to modelate the body of the main program or the procedures.
  TEleBody = class(TxpEleCodeCont)
    adrr   : integer;  //Physical address
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TxpEleBlock }
  //Class to modelate a block of code, like a BEGIN...END or the body of conditional.
  TEleBlock = class(TxpEleCodeCont)
    //adrr   : integer;  //dirección física
    constructor Create; override;
    destructor Destroy; override;
  end;

  TEleSentence = class;
  { TEleProgFrame }
  {Defines an element that have a strucure similar to a general Pascal program,
  including declaractions (VAR, CONST, PROCEDURE) and a Code container (BODY).
  be used as a general code container, like the main program,
  a procedure or a unit.}
  TEleProgFrame = class(TxpElement)
  public
    function BodyNode: TEleBody;
  public //Manejo de llamadas a exit()
    firstObligExit: TEleSentence;  {Referencia al primer exit(), en código obligatorio.
                           Si es NIL, significa que no hay ningún exit() en código
                           obligatorio, aunque podrían haber algunos en código condicional.
                           Solo importa el primero, porque después de este, ya no se
                           ejecutará ningún otro código.}
    procedure RegisterExitCall(exitSent: TEleSentence);
    {**************************************************************
    De momento, no se están usando estos campos de abajo. Con los de arriba es
    suficiente por ahora. No se necesita crear una estructura de bloques de sintaxis
    porque el AST ya lo incluye. Además para optimización del RTS, solo basta con
    saber si hay al menos un exit() en código obligatorio, que por fuerza será el
    último en ejecutarse}
    //lstExitCalls: TxpExitCalls;
    //procedure AddExitCall(srcPos: TSrcPos; codeBlk: TxpEleCodeCont);
    //function ObligatoryExit: TxpExitCall;
  public //Inicialización
    procedure Clear; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  //Clase para modelar al bloque principal
  { TxpEleProg }
  TEleProg = class(TEleProgFrame)
    //Como este nodo representa al programa principal, se incluye información física
    srcSize: integer;  {Tamaño del código compilado. En la primera pasada, es referencial,
                        porque el tamaño puede variar al reubicarse.}
    constructor Create; override;
  end;

  { TxpEleUnit }
  //Clase para modelar a las unidades
  TEleUnit = class(TEleProgFrame)
  public
    srcFile: string;   //El archivo en donde está físicamente la unidad.
    InterfaceElements: TxpElements;  //Lista de eleemntos en la sección INTERFACE
    procedure ReadInterfaceElements;
    constructor Create; override;
    destructor Destroy; override;
  end;
  TEleUnits = specialize TFPGObjectList<TEleUnit>; //lista de constantes

  { TxpEleFinal }
  //Clase para modelar al bloque FINALIZATION de una unidad
  TEleFinal = class(TxpElement)
    adrr   : integer;  //dirección física
    constructor Create; override;
    destructor Destroy; override;
  end;

type  //Instructions relative elements
  //Sentences categories
  TxpSentence = (
    sntNull,       //Default value
    //sntExpres,     //Expression or operand
    sntAssign,     //Assignment
    sntProcCal,    //Procedure call
    sntAsmBlock,   //ASM block
    sntBeginEnd,   //BEGIN-END block
    sntIF,         //Conditional IF
    sntREPEAT,     //REPEAT Loop
    sntWHILE,      //WHILE Loop
    sntFOR,        //FOR loop
    sntCASE,       //Conditional CASE
    sntExit        //Exit instruction
  );

  { TEleSentence }
  {Represents a Pascal instruction.}
  TEleSentence = class(TxpElement)
  public
    sntType: TxpSentence;  //Sentence type
    function sntTypeAsStr: string;
    constructor Create; override;
  end;

  //ASM instruction type
  TiType = (
    itOpcode,     //Common instruction with an Opcode and Operand.
    itLabel,      //An ASM label.
    itOrgDir,     //Instrcution ORG
    itDefByte     //Instruction DB
  );

  { TEleAsmInstr }
  {Represents a line of assembler inside an ASM block.
  Consider this is a hardware dependent format}
  TEleAsmInstr = class(TxpElement)
    addr   : integer;  //Starting Address. Used only in code generation.
    iType  : TiType;   //ASM instruction type
    //Fields to generate instructions, using TP6502.codAsm() or similar.
    opcode : word;     {Formally should be TP6502Inst or similar. Defined as word
                        because we don't want to depend on unit P6502Utils here. }
    addMode: byte;     {Formally should be TP6502AddMode or similar. Defined as byte
                        because we don't want to depend on unit P6502Utils here. }
    operVal: integer;  {The value of instruction operand, when it's a simple number.
                        When it's -1, the operand is a reference to an element and
                        should be read in "operRef".}
    operRef: TxpElement; {Reference to element when operand refers to some Pascal or
                          ASM element.}
    operNam: string;  {Operand name. Used when operand is an unsolved reference}
    constructor Create; override;
  end;
  TEleAsmInstrs = specialize TFPGObjectList<TEleAsmInstr>;

  { TEleAsmBlock }
  {Represents an ASM block. An ASM block contains several ASM lines ()}
  TEleAsmBlock = class(TxpElement)
    undefInstrucs: TEleAsmInstrs;   //List of instruction with operands undefined
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TxpEleDIREC }
  //Represents a directive. Designed to represent nodes {$IFDEF}
  TxpEleDIREC = class(TxpElement)
    ifDefResult  : boolean;   //Boolean value of expression $IFDEF
    constructor Create; override;
  end;

type  //Declaration elements (functions)
  //Function parameter
  TxpParFunc = record
    name   : string;      //Nombre de parámetro
    typ    : TEleTypeDec; //Referencia al tipo
    pvar   : TEleVarDec;  //Referencia a la variable que se usa para el parámetro
    srcPos : TSrcPos;     //Posición del parámetro.
    adicVar: TAdicVarDec; //Parámetros adicionales
  end;
  TxpParFuncArray = array of TxpParFunc;

  //Clase para almacenar información de las funciones
  TCodSysInline = procedure(funEleExp: TEleExpress) of object;
  TCodSysNormal = procedure(funEleExp: TEleFunBase) of object;

  TOperatorType = (
    opkNone,       //Not an operator
    opkUnaryPre,   //Unary Pre operator
    opkUnaryPost,  //Unary Post operator
    opkBinary      //Binary operator
  );
  TCallType = (
    ctUsrNormal,   //Common user function
    ctUsrInline,   //Inline user function
    ctSysNormal,   //Common system function
    ctSysInline,   //Inline system function
    ctUsrExtern    //External function
  );
  { TxpEleFunBase }
  TEleFunBase = class(TEleProgFrame)
    retType     : TEleTypeDec;   //Type returned
    IsInterrupt : boolean;      //Indicates the function is an ISR
    IsForward   : boolean; //Identifies a forward declaration.
  public //Operator
    operTyp: TOperatorType;  //Operand type
    oper   : string;   //Operator associated to the function when it works as a method.
    {Note that the precedence of the operators, is fixed and depends only of operator.}
  public //Flags for operators
    fConmutat : boolean; //Represents a conmutative binary operator.
  public //References
    callType: TCallType;  //How to call the function.
    //Routine BOR o UOR, when callType is ctSysInline.
    codSysInline: TCodSysInline;
    //Routine that generates code for the function, when callType is ctSysNormal.
    codSysNormal: TCodSysNormal;
  public //Parameters manage
    pars   : TxpParFuncArray;  //parámetros de entrada
    procedure ClearParams;
    function SameParamsType(const funpars: TxpParFuncArray): boolean;
    function ParamTypesList: string;
  end;

  TEleFun = class;

  { TxpEleFunDec }
  {Basic class to represent a function header or declaration (INTERFACE o FORWARD).
  Basically what we store here is a reference to the implementation.}
  TEleFunDec = class(TEleFunBase)
  public
    implem    : TEleFun;    //Reference to implementation element.
  public //Initialization
    constructor Create; override;
  end;

  { TxpEleFun }
  { Represents a common function (simple or inline) or a method (simple or inline). }
  TEleFun = class(TEleFunBase)
  public  //Main attributes
    adrr   : integer;  //Physical address where function is compiled.
    adrr2  : integer;  //Aditional physical address, for other entry point of the function.
    srcSize: integer;  {Tamaño del código compilado. En la primera pasada, es referencial,
                        porque el tamaño puede variar al reubicarse.}
    coded : boolean;   //Indicates the function was compiled in memory.
    procedure SetElementsUnused;
  public  //Declaration
    {These properties allows to have reference to the function declaration, when there is
    one:
      - Interface versions, or
      - Forward version.
    In other cases there is just a function element without separated declaration.
    According to design:
     Declaration elements -> Contains information about:
        - The parameters and return value.
        - The calls.
     Implementation elements -> Contains information about:
        - The parameters and return value.
        - The calls.
        - Local variables.
        - The body (Calls to other elements.)
     Declaration elements are included too in the Syntax Tree, but they aer used only for
     declaration. All the information must be read in the function.
    }
    declar : TEleFunDec; //Reference to declaration (When it's FORWARD or in INTERFACE)
    function HasDeclar: boolean; inline;
  public  //References information
    function nCalled: integer; override; //número de llamadas
    function nLocalVars: integer;
    function IsTerminal: boolean;
    function IsTerminal2: boolean;
  private //Manage of pending calls
    curSize: integer;
  public  //Manage of pending calls
    {Address of pending calls (JSR) made when the function was not still implemented }
    nAddresPend : integer;
    addrsPend   : array of word;
    procedure AddAddresPend(ad: word);
  public  //Initialization
    constructor Create; override;
    destructor Destroy; override;
  end;
  TEleFuns = specialize TFPGObjectList<TEleFun>;

var
  // Tipo nulo. Usado para elementos sin tipo.
  typNull : TEleTypeDec;

  function GenArrayTypeName(itTypeName: string; nItems: integer): string; inline;
  function GenPointerTypeName(refTypeName: string): string; inline;

implementation
{Functions to Generates standard names for dinamyc types creation. Have standard
names is important to let the compiler:
 * Reuse types definitions.
 * Implement compatibility for types.
}
function GenArrayTypeName(itTypeName: string; nItems: integer): string; inline;
begin
  if nItems=-1 then begin  //dynamic
    exit(PREFIX_ARR + '-' + itTypeName);
  end else begin          //static
    exit(PREFIX_ARR + IntToSTr(nItems) + '-' + itTypeName);
  end;
end;
function GenPointerTypeName(refTypeName: string): string; inline;
begin
  exit(PREFIX_PTR + '-' +refTypeName);
end;
{ TEleCondit }
constructor TEleCondit.Create;
begin
  inherited Create;
  idClass := eleCondit;
end;

procedure TConsValue.InitItems;
begin
  nItems := 0;
  curSize := CONS_ITEM_BLOCK;   //Block size
  setlength(items, curSize);  //initial size
end;

procedure TConsValue.AddConsItem(const c: TConsValue);
begin
  items[nItems] := c;
  inc(nItems);
  if nItems >= curSize then begin
    curSize += CONS_ITEM_BLOCK;   //Increase size by block
    setlength(items, curSize);  //make space
  end;
end;

procedure TConsValue.CloseItems;
begin
  setlength(items, nItems);
end;

{ TConsValue }
function TConsValue.LByte: byte;
begin
  Result := LO(word(valInt));
end;
function TConsValue.HByte: byte;
begin
  Result := HI(word(valInt));
end;
function TConsValue.EByte: byte;
begin
  Result := (valInt >> 16) and $FF;
end;
function TConsValue.UByte: byte;
begin
  Result := (valInt >> 24) and $FF;
end;
function TConsValue.valuesAsString: string;
{Returns a string containing the abstract of values stored.}
begin
  Result := 'int=' + IntToStr(ValInt) + ',bool=' + IfThen(ValBool,'T','F');
end;
{ TxpEleBlock }
constructor TEleBlock.Create;
begin
  inherited Create;
  idClass := eleBlock;
end;
destructor TEleBlock.Destroy;
begin
  inherited Destroy;
end;
{ TxpEleOperator }
constructor TEleAsmOperat.Create;
begin
  inherited Create;
  idClass := eleAsmOperat;
end;
{ TEleExpress }
function TEleExpress.opTypeAsStr: string;
begin
  WriteStr(Result, opType);
end;
function TEleExpress.StoAsStr: string;
//Resturns storage as string.
begin
  WriteStr(Result, Sto);
end;
procedure TEleExpress.StringToArrayOfChar(str: string);
{Init the constant value as array of char from a string.}
var
  i: Integer;
begin
  value.nItems := length(str);
  setlength(value.items, value.nItems);
  for i:=0 to value.nItems-1 do begin
    value.items[i].ValInt := ord(str[i+1]);
  end;
end;
function TEleExpress.ValueIsZero: boolean;
{Check if the Expression is numéric and the value stored is 0. Only applies to constants.}
begin
  Result := (Typ.group in [t_uinteger, t_integer, t_float]) and  //Is a numeric type
            (value.ValInt = 0);  //Has value zero.
end;
//Access to constant value
function TEleExpress.val: dword; inline;
begin
  Result := value.ValInt;
end;
function TEleExpress.valL: word; inline;
begin
  Result := LO(word(value.ValInt));
end;
function TEleExpress.valH: word; inline;
begin
  Result := HI(word(value.ValInt));
end;
function TEleExpress.valU: word; inline;
begin
  Result := (value.valInt >> 24) and $FF;
end;
function TEleExpress.valE: word; inline;
begin
  Result := (value.valInt >> 16) and $FF;
end;
procedure TEleExpress.SetVariab(var0: TEleVarDec);
{Set as variable type (and storage stRamFix) from a variable.}
begin
  opType    := otVariab;
  Sto       := var0.storage;  //Common is stRamFix
  rvar      := var0;
end;
procedure TEleExpress.SetVariab(add0: word);
{Set as variable from an address.}
begin
  opType    := otVariab;
  Sto       := stRamFix;
  rvar      := nil;
  //Set to used direct Address
  dirVar    := true;
  dirAdd    := add0;
end;
procedure TEleExpress.SetVariab_RamVarOf(var0: TEleVarDec; offset: integer);
{Set as variable type with storage stRamVarOf.}
begin
  opType    := otVariab;
  Sto       := stRamVarOf;  //Almacenamiento por defecto.
  rvar      := var0;  //Use this reference to the variable-address.
  //add := "add" is not defined until reading variable;
  offs := offset;
end;

function TEleExpress.add: word;
begin
  //By now we obtain it from rvar (when allocated)
  if dirVar then begin
    //The address is stored in dirAdd
    exit(dirAdd);
  end else begin
    //The address is the same of "rvar"
    exit(rvar.addr);
  end;
end;

//Access to variable addres
function TEleExpress.addL: word; inline;
begin
  Result := add;
end;
function TEleExpress.addH: word; inline;
begin
  Result := add+1;
end;
function TEleExpress.allocated: boolean;
begin
  //This should work only for stRamFix.
  if dirVar then begin
    //We assume if "dirVar" is set, "dirAdd" should be set too.
    exit(true);
  end else begin
    exit(rvar.allocated);
  end;
end;

//Initialization
constructor TEleExpress.Create;
begin
  inherited Create;
  idClass := eleExpress;
  tempVars := TEleVarDecs.Create(true);
end;
destructor TEleExpress.Destroy;
begin
  tempVars.Destroy;
  inherited Destroy;
end;
{ TEleSentence }
function TEleSentence.sntTypeAsStr: string;
begin
  WriteStr(Result, sntType);
end;
constructor TEleSentence.Create;
begin
  inherited Create;
  //name := 'sent';  Don't give name to optimize
  sntType := sntNull;
  idClass := eleSenten;
end;
{ TEleAsmInstr }
constructor TEleAsmInstr.Create;
begin
  inherited Create;
  idClass := eleAsmInstr;
end;
{ TEleAsmBlock }
constructor TEleAsmBlock.Create;
begin
  inherited Create;
  idClass := eleAsmBlock;
  undefInstrucs:= TEleAsmInstrs.Create(false);
end;

destructor TEleAsmBlock.Destroy;
begin
  undefInstrucs.Destroy;
  inherited Destroy;
end;

{ TxpEleDIREC }
constructor TxpEleDIREC.Create;
begin
  inherited Create;
  idClass := eleBody;
end;
{ TxpExitCall }
//function TxpExitCall.IsObligat: boolean;
//{Indica si el exit se encuentra dentro de código obligatorio}
//begin
//  {Para detectar si el exit() está en código obligatorio, se verifica si se enceuntra
//  directamente en el Body, y no dentro de bloques de tipo
//  IF, WHILE, FOR, REPEAT. Este método no es del todo preciso si se considera que puede
//  haber también código obligatorio, dentro de bloques REPEAT o códigos IF definido en
//  tiempo de compilación. Se podrúa mejorar después.}
//  Result := (codeBlk.idClass = eleBody);  //Cuerpo de una función o el programa principal
//end;
{ TxpEleCaller }
function TxpEleCaller.CallerUnit: TxpElement;
{Devuelve el elemento unidad o programa principal, desde donde se hace esta llamada.}
var
  container: TxpElement;
begin
  {Se asume que la llamda se puede hacer solo desde dos puntos:
   - Desde una declaración.
   - Desde el cuerpo de una función.
  }
  if caller = nil then exit(nil);
  //La idea es retorceder hasta encontrar una unidad o el programa principal
  container := caller;
  while not (container.idClass in [eleUnit, eleProg]) do begin
    container := container.Parent;  //Go back in the Tree
  end;
  Result := container;
  //No debería haber otro caso
end;
{ TxpElement }
function TxpElement.AddElement(elem: TxpElement; position: integer=-1): TxpElement;
{Add an child element to the current node. Return reference. }
begin
  elem.Parent := self;  //Update reference
  if position<>-1 then begin
    elements.Insert(position, elem);   //Add to list of elements
  end else begin
    elements.Add(elem);   //Add to list of elements
  end;
  Result := elem;       //No so useful
end;
procedure TxpElement.Setname(AValue: string);
begin
  if Fname=AValue then Exit;
  Fname:=AValue;
  Funame:=Upcase(AValue);
end;
function TxpElement.LastNode: TxpElement;
{Devuelve una referencia al último nodo de "elements"}
begin
  if elements = nil then exit(nil);
  if elements.Count = 0 then exit(nil);
  Result := elements[elements.Count-1];
end;
function TxpElement.Index: integer;
{Returns element location of node within its parent node.}
begin
  Result := Parent.elements.IndexOf(self);  //No so fast.
end;
//Gestion de llamadas al elemento
function TxpElement.nCalled: integer;
begin
  Result := lstCallers.Count;
end;
function TxpElement.IsCalledBy(callElem: TxpElement): boolean;
{Indica si el elemento es llamado por "callElem". Puede haber varias llamadas desde
"callElem", pero basta que haya una para devolver TRUE.}
var
  cal : TxpEleCaller;
begin
  for cal in lstCallers do begin
    if cal.caller = callElem then exit(true);
  end;
  exit(false);
end;
function TxpElement.IsCalledByChildOf(callElem: TxpElement): boolean;
{Indica si el elemento es llamado por algún elemento hijo de "callElem".
Puede haber varias llamadas desde "callElem", pero basta que haya una para devolver TRUE.}
var
  cal : TxpEleCaller;
begin
  for cal in lstCallers do begin
    if cal.caller.Parent = callElem then exit(true);
  end;
  exit(false);
end;
function TxpElement.IsCalledAt(callPos: TSrcPos): boolean;
{Indica si el elemento es llamado, desde la posición indicada.}
var
  cal : TxpEleCaller;
begin
  for cal in lstCallers do begin
    if cal.curPos.EqualTo(callPos) then exit(true);
  end;
  exit(false);
end;
function TxpElement.IsDeclaredAt(decPos: TSrcPos): boolean;
begin
  Result := srcDec.EqualTo(decPos);
end;
function TxpElement.FindCalling(callElem: TxpElement): TxpEleCaller;
{Busca la llamada de un elemento. Si no lo encuentra devuelve NIL.}
var
  cal : TxpEleCaller;
begin
  for cal in lstCallers do begin
    if cal.caller = callElem then exit(cal);
  end;
  exit(nil);
end;
function TxpElement.RemoveCallsFrom(callElem: TxpElement): integer;
{Elimina las referencias de llamadas desde un elemento en particular.
Devuelve el número de referencias eliminadas.}
var
  cal : TxpEleCaller;
  n, i: integer;
begin
  {La búsqueda debe hacerse al revés para evitar el problema de borrar múltiples
  elementos}
  n := 0;
  for i := lstCallers.Count-1 downto 0 do begin
    cal := lstCallers[i];
    if cal.caller = callElem then begin
//if callElem=nil then begin
//  debugln('+Eliminado de ' + self.name + ' caller: '+'nil');
//end else begin
//  debugln('+Eliminado de ' + self.name + ' caller: ' + callElem.name);
//end;
      lstCallers.Delete(i);
      inc(n);
    end;
  end;
  Result := n;
end;
procedure TxpElement.RemoveLastCaller;
//Elimina el último elemento llamador agregado.
begin
  if lstCallers.Count>0 then lstCallers.Delete(lstCallers.Count-1);
end;
procedure TxpElement.ClearCallers;
begin
  lstCallers.Clear;
end;
//function TxpElement.ExistsIn(list: TxpElements): boolean;
//{Debe indicar si el elemento está duplicado en la lista de elementos proporcionada.}
//var
//  ele: TxpElement;
//begin
//  for ele in list do begin
//    if ele.uname = uname then begin
//      exit(true);
//    end;
//  end;
//  exit(false);
//end;
//Gestión de los elementos llamados
procedure TxpElement.AddCalled(elem: TxpElement);
begin
  if lstCalled.IndexOf(elem) = -1 then begin
    lstCalled.Add(elem);
  end;
end;
function TxpElement.UpdateCalledAll: integer;
{Update list "lstCalledAll", using AddCalledAll_FromList().
  The return value is:
  * curNesting -> if not error happens.
  * <0  ->  If found recursion.
}
  function AddCalledAll(elem: TxpElement): boolean;
  {Add reference to lstCalledAll. That is, indicates some element is called from this
  element.
  If reference already exists, retunr FALSE.}
  begin
    //Solo agrega una vez el elemento
    if lstCalledAll.IndexOf(elem) = -1 then begin
      lstCalledAll.Add(elem);
      exit(true);
    end else begin
      exit(false);
    end;
  end;
  function AddCalledAll_FromList(lstCalled0: TxpListCalled): integer;
  {Add the call references (to lstCalledAll) of all elements of the list lstCalled0,
  including its called too (recursive).}
  var
    elem: TxpElement;
    err: Integer;
  begin
    inc(curNesting);    //incrementa el anidamiento
    if curNesting>maxNesting then maxNesting := curNesting;

    if lstCalled0.Count = 0 then exit;
    for elem in lstCalled0 do begin
//      debugln('Call to ' + elem.name + ' from ' + self.name);
//      if elem = self then begin
//        {This is some way to detect circular references like:
//        procedure proc2;
//        begin
//          proc1;
//        end;
//        procedure proc1;
//        begin
//          proc2;
//        end;
//        But fails when this element is not part of the circualr reference like
//        procedure proc2;   <-- We are proc2
//        begin
//          proc1;
//        end;
//        procedure proc1; <-- Here is the recursion
//        begin
//          proc1;
//        end;
//        In this case, several call to proc1() will be adding.
//        }
//        if curNesting = 1 then begin
//          exit(-1);
//        end else begin
//          exit(-2);
//        end;
//      end;
      //Add element reference
      if not AddCalledAll(elem) then begin
        //This is better way to detect circle references, because lstCalled, doesn't
        //contain duplicated calls.
        //exit(-1);  *** Commented in version 0.7.8 because a flase recursion detected
      end;
      if curNesting > 100 then begin
        //This is a secure way (but less elegant) for checking recursion. (If curNesting
        //grows too much). I don't expect this happens, unless exists some case I haven't
        //considered.
        exit(-1);
      end;
      //Verify if this element have other calls to add too.
      if elem.lstCalled.Count <> 0 then begin
        err := AddCalledAll_FromList(elem.lstCalled);
        if err<0 then exit(err);
      end;
    end;
    dec(curNesting);    //incrementa el anidamiento
    exit(curNesting);
  end;
begin
//debugln('UpdateCalledAll' + IntToStr(lstCalledAll.Count));
  lstCalledAll.Clear;  //By security
  curNesting := 0;
  maxNesting := 0;
  Result := AddCalledAll_FromList(lstCalled);
end;
function TxpElement.Path: string;
{Devuelve una cadena, que indica la ruta del elemento, dentro del árbol de sintaxis.}
var
  ele: TxpElement;
begin
  ele := self;
  Result := '';
  while ele<>nil do begin
    Result := '\' + ele.name + Result;
    ele := ele.Parent;
  end;
end;
function TxpElement.FindElemName(const eName: string; out ele: TxpElement
  ): boolean;
{Search a child element with the indicated name (eName). If found returns TRUE.}
var
  eleName: String;
  att: TxpElement;
begin
  eleName := UpCase(eName);
  if elements = nil then begin
    ele := nil;
    exit(false);
  end;
  for att in elements do begin
    if att.uname = eleName then begin
      ele := att;
      exit(true);
    end;
  end;
  ele := nil;
  exit(false);
end;
function TxpElement.FindIdxElemName(const eName: string; var idx0: integer): boolean;
{Busca un nombre en su lista de elementos. Inicia buscando desde idx0, hasta el inicio.
 Si encuentra, devuelve TRUE y deja en idx0, la posición en donde se encuentra.}
var
  i: Integer;
  uEleName: String;
begin
  uEleName := upcase(eName);
  //empieza la búsqueda en "idx0"
  for i := idx0 downto 0 do begin
    if elements[i].uname = uEleName then begin
      //sale dejando idx0 en la posición encontrada
      idx0 := i;
      exit(true);
    end;
  end;
  exit(false);
end;
function TxpElement.posXYin(const posXY: TPoint): boolean;
{Indica si la coordeda del cursor, se encuentra dentro de las coordenadas del elemento.}
var
  y1, y2: integer;
begin
  y1 := srcDec.row;
  y2 := srcEnd.row;
  //Primero verifica la fila
  if (posXY.y >= y1) and (posXY.y<=y2) then begin
    //Está entre las filas. Pero hay que ver también las columnas, si posXY, está
    //en los bordes.
    if y1 = y2 then begin
      //Es rango es de una sola fila
      if (posXY.X >= srcDec.col) and (posXY.X <= srcEnd.col) then begin
        exit(true)
      end else begin
        exit(false);
      end;
    end else if posXY.y = y1 then begin
      //Está en el límite superior
      if posXY.X >= srcDec.col then begin
        exit(true)
      end else begin
        exit(false);
      end;
    end else if posXY.y = y2 then begin
      //Está en el límite inferior
      if posXY.X <= srcEnd.col then begin
        exit(true)
      end else begin
        exit(false);
      end;
    end else begin
      //Está entre los límites
      exit(true);
    end;
  end else begin
    //Esta fuera del rango
    exit(false);
  end;
end;
//Inicialización
procedure TxpElement.Clear;
{Inicializa los campos del objeto. Este método es usado, solamente, para el Nodo Main,
porque los otors nodos son eliminados de la memoria al iniciar el árbol}
begin
  elements.Clear;
  lstCallers.Clear;
  lstCalled.Clear;
  lstCalledAll.Clear;
end;
constructor TxpElement.Create;
begin
  idClass := eleNone;
  lstCallers:= TxpListCallers.Create(true);
  lstCalled := TxpListCalled.Create(false);  //solo guarda referencias
  lstCalledAll:= TxpListCalled.Create(false);
end;
destructor TxpElement.Destroy;
begin
  lstCalledAll.Destroy;
  lstCalled.Destroy;
  lstCallers.Destroy;
  elements.Free;  //por si contenía una lista
  inherited Destroy;
end;
{ TxpEleProgFrame }
function TEleProgFrame.BodyNode: TEleBody;
{Devuelve la referencia al cuerpo del programa. Si no lo encuentra, devuelve NIL.}
var
  elem: TxpElement;
begin
  elem := LastNode;   //Debe ser el último
  if elem = nil then exit(nil);
  if elem.idClass <> eleBody then begin
    exit(nil);  //No debería pasar
  end;
  //Devuelve referencia
  Result := TEleBody(elem);
end;
procedure TEleProgFrame.RegisterExitCall(exitSent: TEleSentence);
{Registra una llamada a una instrucción exit(). De momento solo se usa para actualizar a
"firstObligExit".
"exitSent" es la instrucción exit() que queremos registrar. Debe haber sido ya incluida
en el AST.}
begin
  if firstObligExit = nil then begin
    //Aún no ha sido inicializado. Este puede ser el primero.
    //Verifica si está dentro de código obligatorio.
    {Para detectar si el exit() está en código obligatorio, se verifica si se enceuntra
     directamente en el Body, y no dentro de bloques de tipo
     IF, WHILE, FOR, REPEAT. Este método no es del todo preciso si se considera que puede
     haber también código obligatorio, dentro de bloques REPEAT o códigos IF definido en
     tiempo de compilación. Se podrúa mejorar después.}
    if exitSent.Parent.idClass = eleBody then begin
      {Es código obligatorio, porque si estuviera dentro de estructuras IF o REPEAT o
      WHILE; el "idClass" sería un elemento "eleBlock".}
      firstObligExit := exitSent;
    end;
  end;
end;

//procedure TEleProgFrame.AddExitCall(srcPos: TSrcPos; codeBlk: TxpEleCodeCont);
//var
//  exitCall: TxpExitCall;
//begin
//  exitCall := TxpExitCall.Create;
//  exitCall.srcPos := srcPos;
//  {Se guarda el ID, en lugar de la referencia al bloque, porque en el modo de trabajo
//   actual, los bloques se crean y destruyen, dinámicamente}
//  exitCall.codeBlk  := codeBlk;
//  lstExitCalls.Add(exitCall);
//end;
//function TEleProgFrame.ObligatoryExit: TxpExitCall;
//{Devuelve la referencia de una llamada a exit(), dentro de código obligatorio del Body.
//Esto ayuda a saber si ya el usuario incluyó la salida dentro del código y no es necesario
//agregar un RETURN al final.
//Si no encuentra ninguna llamada a exit() en código obligatorio, devuelve NIL.
//Según la documentación, el exit() en código obligatorio, solo debe estar al final del
//código del procedimiento. Si estuviera antes, dejaría código "no-ejecutable".}
//var
//  exitCall: TxpExitCall;
//begin
//  if lstExitCalls.Count = 0 then exit(nil);  //No incluye exit()
//  for exitCall in lstExitCalls do begin
//    //Basta detectar un exit(), porque no se espera que haya más.
//    if exitCall.IsObligat then begin
//      exit(exitCall);  //tiene una llamada en código obligatorio
//    end;
//  end;
//  //No se encontró ningún exit en el mismo "body"
//  exit(nil);
//end;
//Inicialización
procedure TEleProgFrame.Clear;
begin
  inherited Clear;
  //lstExitCalls.Clear;
end;
constructor TEleProgFrame.Create;
begin
  inherited Create;
  idClass := eleProgFrame;
  //lstExitCalls:= TxpExitCalls.Create(true);
end;
destructor TEleProgFrame.Destroy;
begin
  //lstExitCalls.Destroy;
  inherited Destroy;
end;
{ TxpEleCon }
constructor TEleConsDec.Create;
begin
  inherited;
  idClass := eleConsDec;
end;
{ TEleVarDec }
function TEleVarDec.Gettyp: TEleTypeDec;
begin
  if ftyp.copyOf<>nil then begin
    Result := ftyp.copyOf
  end else begin
    Result := ftyp;
  end;
end;
procedure TEleVarDec.Settyp(AValue: TEleTypeDec);
begin
  ftyp := AValue;
end;

function TEleVarDec.addrL: word;
{Dirección absoluta de la variable de menor pero, cuando es de tipo WORD.}
begin
  Result := addr;
end;
function TEleVarDec.addrH: word;
{Dirección absoluta de la variable de mayor pero, cuando es de tipo WORD.}
begin
  Result := addr + 1;
end;
function TEleVarDec.addrE: word;
begin
  Result := addr + 2;
end;
function TEleVarDec.addrU: word;
begin
  Result := addr + 3;
end;
function TEleVarDec.AddrString: string;
{Devuelve una cadena, que representa a la dirección física.}
begin
  if typ.IsByteSize then begin
    Result := '$' + IntToHex(addr, 3);
  end else if typ.IsWordSize then begin
    Result := '$' + IntToHex(addr, 3);
  end else if typ.IsDWordSize then begin
    Result := '$' + IntToHex(addr, 3);
  end else begin
    Result := '';   //Error
  end;
end;
procedure TEleVarDec.ResetAddress;
begin
  addr := 0;
end;

function TEleVarDec.stoStr: string;
begin
  WriteStr(Result, storage);
end;

constructor TEleVarDec.Create;
begin
  inherited;
  idClass:=eleVarDec;
end;
destructor TEleVarDec.Destroy;
begin
  inherited Destroy;
end;
function TEleTypeDec.getSize: smallint;
var
  nItms: integer;
begin
  if catType = tctArray then begin
    //Array size is calculated
    if nItems= -1 then exit(0) else exit(itmType.size * nItems);
  end else if catType = tctPointer then begin
    exit(2);  //Pointer are like words
  end else if catType = tctObject then begin
//    if attribs.Count = 0 then exit(0);
//    lastAttrib := attribs[attribs.Count-1];
//    exit(lastAttrib.offs + lastAttrib.typ.size);  //
    exit(0);
  end else begin
    exit(fSize)
  end;
end;
procedure TEleTypeDec.setSize(AValue: smallint);
begin
  fSize := AValue;
end;

function TEleTypeDec.groupStr: string;
begin
  WriteStr(Result, group);
end;
function TEleTypeDec.catTypeStr: string;
begin
  WriteStr(Result, catType);
end;
function TEleTypeDec.nItems: integer;
begin
  if copyOf<>nil then begin
    exit(copyOf.consNitm.value.ValInt)
  end else begin
    exit(consNitm.value.ValInt)
  end;
end;
{ TxpEleType }
procedure TEleTypeDec.SaveToStk;
begin
  if OnSaveToStk<>nil then OnSaveToStk;
end;
function TEleTypeDec.IsByteSize: boolean;
{Indica si el tipo, tiene 1 byte de tamaño}
begin
//  if copyOf<>nil then exit(copyOf.IsByteSize);  //verifica
  Result := size = 1;
end;
function TEleTypeDec.IsWordSize: boolean;
{Indica si el tipo, tiene 2 bytes de tamaño}
begin
//  if copyOf<>nil then exit(copyOf.IsWordSize);  //verifica
  Result := size = 2;
end;
function TEleTypeDec.IsDWordSize: boolean;
{Indica si el tipo, tiene 4 bytes de tamaño}
begin
//  if copyOf<>nil then exit(copyOf.IsDWordSize);  //verifica
  Result := size = 4;
end;
function TEleTypeDec.IsArrayOf(itTyp: TEleTypeDec; numIt: integer): boolean;
{Indicates if this type is an array of the specified type and with the specified
number of elements.}
begin
  if catType <> tctArray then exit(false);
  //I'm an array
  debugln('Buscando arreglo en: ' + self.name);
  if consNitm = nil then exit(false);  //Not yet set the size.
  exit( (nItems = numIt) and itmType.IsEquivalent(itTyp) );
end;
function TEleTypeDec.IsPointerTo(ptTyp: TEleTypeDec): boolean;
begin
  exit( (catType = tctPointer) and ptrType.IsEquivalent(ptTyp) );
end;
function TEleTypeDec.IsEquivalent(typ: TEleTypeDec): boolean;
{Indicates if the type is the same type as the specified or has the same definition.}
begin
  if self = typ then exit(true);
  if catType <> typ.catType then exit(false);
  //Have the same category
  if (self.copyOf = typ) or (typ.copyOf = self) then exit(true);
  if (self.copyOf<>nil) and (self.copyOf = typ.copyOf) then exit(true);
  if catType = tctArray then begin
    //Equivalence for arrays
    if (self.nItems = typ.nItems) and itmType.IsEquivalent(typ.itmType) then exit(true);
  end else if catType = tctPointer then begin
    //Equivalence for pointers
    if (self.ptrType.IsEquivalent(typ.ptrType)) then exit(true);
  end;
  exit(false);
end;
constructor TEleTypeDec.Create;
begin
  inherited;
  idClass:=eleTypeDec;
  //Ceeate list
  internalTypes:= TEleTypeDecs.Create(true);
end;
destructor TEleTypeDec.Destroy;
begin
  internalTypes.Destroy;
  inherited;
end;
{ TxpEleProg }
constructor TEleProg.Create;
begin
  inherited;
  idClass := eleProg;
  Parent := nil;  //la raiz no tiene padre
end;
{ TxpEleFunBase }
function TEleFunBase.SameParamsType(const funpars: TxpParFuncArray): boolean;
{Compara los parámetros de la función con una lista de parámetros. Si tienen el mismo
número de parámetros y el mismo tipo, devuelve TRUE.}
var
  i: Integer;
begin
  Result:=true;   //We assume they are the same
  if High(pars) <> High(funpars) then
    exit(false);   //Distinct parameters number
  //They have the same numbers of parameters, verify:
  for i := 0 to High(pars) do begin
    //A Null type matches everything (wildcard). Used in INLINE functions.
    if pars[i].typ = typNull then continue;
    //Compare tipe
    if pars[i].typ <> funpars[i].typ then begin
      exit(false);
    end;
  end;
  //si llegó hasta aquí, hay coincidencia, sale con TRUE
end;
procedure TEleFunBase.ClearParams;
//Elimina los parámetros de una función
begin
  setlength(pars,0);
end;
function TEleFunBase.ParamTypesList: string;
{Devuelve una lista con los nombres de los tipos de los parámetros, de la forma:
(byte, word) }
var
  tmp: String;
  j: Integer;
begin
  tmp := '';
  for j := 0 to High(pars) do begin
    tmp += pars[j].name+', ';
  end;
  //quita coma final
  if length(tmp)>0 then tmp := copy(tmp,1,length(tmp)-2);
  Result := '('+tmp+')';
end;
{ TxpEleFunDec }
constructor TEleFunDec.Create;
begin
  inherited Create;
  idClass := eleFuncDec;
end;
{ TxpEleFun }
procedure TEleFun.SetElementsUnused;
{Marca todos sus elementos con "nCalled = 0". Se usa cuando se determina que una función
no es usada.}
var
  elem: TxpElement;
begin
  if elements = nil then exit;  //No tiene
  //Marca sus elementos, como no llamados
  for elem in elements do begin
    elem.ClearCallers;
  end;
end;
function TEleFun.HasDeclar: boolean;
begin
  exit(declar<>nil);
end;
function TEleFun.nCalled: integer;
begin
  if IsInterrupt then exit(1);   //Los INTERRUPT son llamados implícitamente
  Result := lstCallers.Count;
end;
function TEleFun.nLocalVars: integer;
{Returns the numbers of local variables for this function.}
var
  elem : TxpElement;
begin
  Result := 0;
  for elem in elements do begin
    if elem.idClass = eleVarDec then inc(Result);
  end;
end;
function TEleFun.IsTerminal: boolean;
{Indica si la función ya no llama a otras funciones. Para que funcione, se debe haber
llenado primero, "lstCalled".}
begin
  Result := (lstCalled.Count = 0);
end;
function TEleFun.IsTerminal2: boolean;
{Indica si la función es Terminal, en el sentido que cumple:
- Tiene variables locales.
- No llama a otras funciones o las funciones a las que llama no tienen variables locales.
Donde "Variables" locales, se refiere también a parámetros del procedimiento.}
var
  called   : TxpElement;
  nCallesFuncWithLocals: Integer;
begin
  if nLocalVars = 0 then exit(false);
  //Tiene variables locales
  //Verifica llamada a funciones
  nCallesFuncWithLocals := 0;
  for called in lstCalledAll do begin
    if called.idClass = eleFunc then begin
      if TEleFun(called).nLocalVars > 0 then inc(nCallesFuncWithLocals);
    end;
  end;
  if nCallesFuncWithLocals = 0 then begin
    //Todas las funciones a las que llama, no tiene variables locales
    exit(true);
  end else begin
    exit(false);
  end;
end;
procedure TEleFun.AddAddresPend(ad: word);
{Add a pending address to the function to be completed later.}
begin
  addrsPend[nAddresPend] := ad;
  inc(nAddresPend);
  if nAddresPend > curSize then begin
    curSize += CONS_ITEM_BLOCK;   //Increase size by block
    setlength(addrsPend, curSize);  //make space
  end;
end;
//Inicialización
constructor TEleFun.Create;
begin
  inherited;
  idClass:=eleFunc;
  //Init addrsPend[]
  nAddresPend := 0;
  curSize := CONS_ITEM_BLOCK;   //Block size
  setlength(addrsPend, curSize);  //initial size
end;
destructor TEleFun.Destroy;
begin
  inherited Destroy;
end;
{ TxpEleUnit }
procedure TEleUnit.ReadInterfaceElements;
{Actualiza la lista "InterfaceElements", con los elementos accesibles desde la
sección INTERFACE. De esta forma se facilita la exploración de elementos públicos.}
var
  ele: TxpElement;
begin
  InterfaceElements.Clear;
  if elements = nil then exit;
  //Solo basta explorar a un nivel
  for ele in elements do begin
    if ele.location = locInterface then begin
       InterfaceElements.Add(ele);
    end;
  end;
end;
constructor TEleUnit.Create;
begin
  inherited;
  idClass:=eleUnit;
  InterfaceElements:= TxpElements.Create(false);
end;
destructor TEleUnit.Destroy;
begin
  InterfaceElements.Destroy;
  inherited Destroy;
end;
{ TxpEleBody }
constructor TEleBody.Create;
begin
  inherited;
  idClass := eleBody;
end;
destructor TEleBody.Destroy;
begin
  inherited Destroy;
end;
{ TxpEleFinal }
constructor TEleFinal.Create;
begin
  inherited Create;
  idClass := eleFinal;
end;
destructor TEleFinal.Destroy;
begin
  inherited Destroy;
end;

end.
//1871
