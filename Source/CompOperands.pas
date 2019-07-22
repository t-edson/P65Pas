{
Unit including the base clase for the compiler, the routines for manage operands
and the TOperand class definition.
The idea is hace here most of the code that is independent of the language to implement
and of the hardwrare.
                                            By Tito Hinostroza 2019 - Lima - Perú.
}
unit CompOperands;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, SynEditHighlighter,
  XpresTypesPIC, XpresElementsPIC, MisUtils, XpresBas;
type
  //Logic level type.
  TLogicType = (
    logNormal,   //Normal logic
    logInverted  //Inverted logic
  );
  //Mode operand must be read
  TOpReadMode = (
    opmGetter,   //When operand must be read as Getter
    opmSetter    //When operand must be read as Setter
  );
  { TOperand }
  //Operand Object.
  TOperand = object
  private
    FSto   : TStoOperand; //Operand storage.
    FTyp   : TxpEleType;  //Tipo del operando, cuando no es stVariab
    FVar   : TxpEleVar;   //Referencia a variable, cuando es stVariab o stVarRef.
    FValue : TConsValue;  //Valores constantes, cuando el operando es constante
    function GetTyp: TxpEleType;
    procedure SetvalFloat(AValue: extended);
    procedure SetvalInt(AValue: Int64);
  public  //Campos generales
    txt     : string;  //Text of operand or expression, as it appears in source.
    logic   : TLogicType; {Used when operand is Boolean type. Indicates the type of logic of
                          the operand value.}
    rVarBase: TxpEleVar;  {Referencia a variable base, cuando el operando es de tipo:
                           <variable>.<campo>.<campo>. }
    nOpern  : byte;       //Number of operands in expression
    {"Sto", "Typ" y "rVar" se consideran de solo lectura. Para cambiarlos, se han definido
    los métodos: SetAsConst(), SetAsVariab(), SetAsExpres() y SetAsNull, que ofrecen una
    forma más sencilla y segura que cambiar "Cat", "Typ", y rVar" individualmente (que es
    como se hacía en versiones anteriores).}
    property Sto : TStoOperand read FSto;  //Alamcenamiento de operando
    property Typ : TxpEleType read GetTyp; //Tipo del operando
    property rVar: TxpEleVar  read FVar;   //Referencia a la variable, cuando es stVariab o stVarRef.
    procedure SetAs(Op: TOperand);
    procedure SetAsConst(xtyp: TxpEleType);
    procedure SetAsConst(xtyp: TxpEleType; const iniVal: TConsValue);
    procedure SetAsVariab(xvar: TxpEleVar);
    procedure SetAsExpres(xtyp: TxpEleType);
    procedure SetAsExpresA(xtyp: TxpEleType);
    procedure SetAsExpresX(xtyp: TxpEleType);
    procedure SetAsExpresY(xtyp: TxpEleType);

    procedure SetAsVarRef(VarBase: TxpEleVar; xtyp: TxpEleType);
    procedure SetAsVarConRef(VarBase: TxpEleVar; ValOff: integer; xtyp: TxpEleType);
    procedure SetAsExpRef(Etyp: TxpEleType);
    procedure SetAsNull;
    function StoOpStr: string;
    function StoOpChr: char;
    procedure DefineRegister; inline;
    function FindOperator(const oper: string): TxpOperator; //devuelve el objeto operador
    procedure Invert;  //Invierte la lógica del operando
  public  //Campos acceso cuando sea variable.
    function VarName: string; inline; //nombre de la variable, cuando sea de categ. coVariab
    function addr : TVarOffs; //dirección absoluta de la variable
    function addrL: TVarOffs; inline; //dirección del byte bajo
    function addrH: TVarOffs; inline; //dirección del byte alto
    function addrE: TVarOffs; inline; //dirección del byte alto
    function addrU: TVarOffs; inline; //dirección del byte alto
  public  //Campos de acceso a los valores constantes
    property Value   : TConsValue read FValue;
    property valInt  : Int64 read FValue.ValInt write SetvalInt;
    property valFloat: extended read FValue.ValFloat write SetvalFloat;
    property valBool : boolean read FValue.ValBool write FValue.ValBool;
    property valStr  : string read FValue.ValStr write FValue.ValStr;
    //Funciones de ayuda para adaptar los tipos numéricos
    function aWord: word; inline;  //devuelve el valor en Word
    function LByte: byte; inline;  //devuelve byte bajo de valor entero
    function HByte: byte; inline;  //devuelve byte alto de valor entero
    function EByte: byte; inline;  //devuelve byte bajo de valor entero
    function UByte: byte; inline;  //devuelve byte bajo de valor entero
    //campos para validar el rango de los valores
    function CanBeByte: boolean;   //indica si cae en el rango de un BYTE
    function CanBeWord: boolean;   //indica si cae en el rango de un WORD
    //Métodos para mover valores desde/hacia una constante externa
    procedure CopyConsValTo(var c: TxpEleCon); inline;
    procedure GetConsValFrom(const c: TxpEleCon); inline;
  private //Manage items
    curSize: integer;
    function GetNItems: integer;
  public  //Manage items when it's constant array
    property nItems: integer read GetNItems;
    procedure InitItems;
    procedure AddConsItem(const c: TConsValue);
    procedure CloseItems;
    procedure StringToArrayOfChar(str: string);
  end;
  TOperandPtr = ^TOperand;

  { TCompOperands }

  TCompOperands = class
  protected //Access to properties of p1^ y p2^.
    p1, p2   : ^TOperand;   //Pointers to the operands of the current operation
    res      : TOperand;    //Result of the last expression evaluation.
    function value1: dword;
    function value1L: word;
    function value1H: word;
    function value1U: word;
    function value1E: word;
    function value2: dword;
    function value2L: word;
    function value2H: word;
    function byte1: word;
    function byte1L: word;
    function byte1H: word;
    function byte2: word;
    function byte2L: word;
    function byte2H: word;
    function stoOperation: TStoOperandsROB; inline;
    procedure ExchangeP1_P2;
    function OperationStr(Opt: TxpOperation): string;
  protected
    cIn      : TContexts;   //Entrada de datos
  public    //Errors and warnings
    curLocation: TxpEleLocation;   //Ubicación actual de exploración
    HayError: boolean;
    OnWarning: procedure(warTxt: string; fileName: string; row, col: integer) of object;
    OnError  : procedure(errTxt: string; fileName: string; row, col: integer) of object;
    OnInfo   : procedure(infTxt: string) of object;
    procedure ClearError;
    //Rutinas de generación de mensajes
    procedure GenInfo(msg: string);
    //Rutinas de generación de advertencias
    procedure GenWarn(msg: string; fil: String; row, col: integer);
    procedure GenWarn(msg: string; const Args: array of const; fil: String; row, col: integer);
    procedure GenWarn(msg: string);
    procedure GenWarn(msg: string; const Args: array of const);
    procedure GenWarnPos(msg: string; const Args: array of const; srcPos: TSrcPos);
    //Rutinas de generación de error
    procedure GenError(msg: string; fil: String; row, col: integer);
    procedure GenError(msg: String; const Args: array of const; fil: String; row, col: integer);
    procedure GenError(msg: string);
    procedure GenError(msg: String; const Args: array of const);
    procedure GenErrorPos(msg: String; const Args: array of const; srcPos: TSrcPos);
  public    //References to predefined token types
    tnEol       : integer;
    tnSymbol  : integer;
    tnSpace   : integer;
    tnIdentif : integer;
    tnNumber  : integer;
    tnKeyword : integer;
    tnString  : integer;
    tnComment : integer;
    //Atributos
    tkEol     : TSynHighlighterAttributes;
    tkSymbol  : TSynHighlighterAttributes;
    tkSpace   : TSynHighlighterAttributes;
    tkIdentif : TSynHighlighterAttributes;
    tkNumber  : TSynHighlighterAttributes;
    tkKeyword : TSynHighlighterAttributes;
    tkString  : TSynHighlighterAttributes;
    tkComment : TSynHighlighterAttributes;
    //otras referencias
    tnOperator: integer;
    tnBoolean : integer;
    tnSysFunct: integer;
    tnType    : integer;
    //Atributos
    tkOperator: TSynHighlighterAttributes;
    tkBoolean : TSynHighlighterAttributes;
    tkSysFunct: TSynHighlighterAttributes;
    tkType    : TSynHighlighterAttributes;
  public    //Aditional token types
    tnStruct   : integer;
    tnDirective: integer;
    tnAsm      : integer;
    tnExpDelim : integer;
    tnBlkDelim : integer;
    tnChar     : integer;
    tnOthers   : integer;
  end;

implementation

{ TOperand }
function TOperand.VarName: string; inline;
begin
  Result := rVar.name;
end;
function TOperand.addr: TVarOffs;
begin
  Result := FVar.addr;
end;
function TOperand.addrL: TVarOffs;
{Dirección de memoria baja, cuando es de tipo Word.}
begin
  Result := rVar.addrL;
end;
function TOperand.addrH: TVarOffs;
begin
  Result := rVar.addrH;
end;
function TOperand.addrE: TVarOffs;
begin
  Result := rVar.addrE;
end;
function TOperand.addrU: TVarOffs;
begin
  Result := rVar.addrU;
end;
procedure TOperand.Invert;
begin
  if Sto = stConst then begin
    //In constants, logic is not used. We can change teh value
    valBool := not valBool;
  end else begin
    //Variables and expresions, yes.
    if logic = logNormal then logic := logInverted else logic := logNormal;
  end;
end;
function TOperand.aWord: word; inline;
begin
  Result := word(valInt);
end;
function TOperand.LByte: byte; inline;
begin
  Result := LO(word(valInt));
end;
function TOperand.HByte: byte; inline;
begin
  Result := HI(word(valInt));
end;
function TOperand.EByte: byte;
begin
  Result := (valInt >> 16) and $FF;
end;

function TOperand.UByte: byte;
begin
  Result := (valInt >> 24) and $FF;
end;
function TOperand.CanBeWord: boolean;
{Indica si el valor constante que contiene, puede ser convertido a un WORD sin pérdida}
begin
  Result := (valInt>=0) and (valInt<=$ffff);
end;
function TOperand.CanBeByte: boolean;
{Indica si el valor constante que contiene, puede ser convertido a un BYTE sin pérdida}
begin
  Result := (valInt>=0) and (valInt<=$ff);
end;
procedure TOperand.CopyConsValTo(var c: TxpEleCon);
begin
  c.val := FValue;
end;
procedure TOperand.GetConsValFrom(const c: TxpEleCon);
{Copia valores constante desde una constante. Primero TOperand, debería tener inicializado
 correctamente su campo "catTyp". }
begin
  FValue := c.val;
end;
function TOperand.GetNItems: integer;
{Returns the number of items when operand is a static array.}
begin
  if Sto = stVariab then begin
    //It's a variable
    exit(rVar.typ.nItems)
  end else if Sto = stConst then begin
    exit(FValue.nItems);
  end else begin
      exit(0);
  end;
end;
//Manage items
const CONS_ITEM_BLOCK = 3;
procedure TOperand.InitItems;
begin
  FValue.nItems := 0;
  curSize := CONS_ITEM_BLOCK;   //Block size
  setlength(FValue.items, curSize);  //initial size
end;
procedure TOperand.AddConsItem(const c: TConsValue);
begin
  FValue.items[FValue.nItems] := c;
  inc(FValue.nItems);
  if FValue.nItems >= curSize then begin
    curSize += CONS_ITEM_BLOCK;   //Increase size by block
    setlength(FValue.items, curSize);  //make space
  end;
end;
procedure TOperand.CloseItems;
begin
  setlength(FValue.items, FValue.nItems);
end;
procedure TOperand.StringToArrayOfChar(str: string);
{Init the constant value as array of char from a string.}
var
  i: Integer;
begin
  FValue.nItems := length(str);
  setlength(FValue.items, FValue.nItems);
  for i:=0 to FValue.nItems-1 do begin
    FValue.items[i].ValInt := ord(str[i+1]);
  end;
end;

function TOperand.GetTyp: TxpEleType;
{Devuelve el tipo de la variable referenciada. }
begin
  if Sto = stVariab then begin
    Result := FVar.typ;
  end else if Sto = stVarRef then begin
    //Variable referenciada por variables.
    //Se implementa de acuerdo a la Doc. Técnica - Sección "Operandos sVarRef"
    Result := Ftyp;
  end else if Sto = stExpRef then begin
    //Variable referenciada por expresión.
    //Se implementa de acuerdo a la Doc. Técnica - Sección "Operandos sExpRef"
    Result := Ftyp;
  end else begin
    //Constante o expresión
    Result := FTyp;
  end;
end;
procedure TOperand.SetvalFloat(AValue: extended);
begin
//  if FvalFloat=AValue then Exit;
  FValue.ValFloat:=AValue;
end;
procedure TOperand.SetvalInt(AValue: Int64);
begin
//  if FvalInt=AValue then Exit;
  FValue.ValInt:=AValue;
end;
procedure TOperand.SetAs(Op: TOperand);
//Set Operand the same as other operand.
begin
  FSto := Op.Sto;
  FTyp := Op.Typ;
  FVar := Op.rVar;
  if Op.Sto = stConst then begin
    //Only copy if really matter, because could be slow
    FValue := Op.Value;
  end;
  logic := Op.logic;
  rVarBase := Op.rVarBase;
  nOpern := Op.nOpern;
end;
procedure TOperand.SetAsConst(xtyp: TxpEleType);
{Fija el almacenamiento del Operando como Constante, del tipo indicado}
begin
  FSto := stConst;
  FTyp := xtyp;
  rVarBase := nil;  //Inicia a este valor
end;
procedure TOperand.SetAsConst(xtyp: TxpEleType; const iniVal: TConsValue);
begin
  FSto := stConst;
  FTyp := xtyp;
  rVarBase := nil;  //Inicia a este valor
  FValue := iniVal;
end;
procedure TOperand.SetAsVariab(xvar: TxpEleVar);
{Set the operand storage to stVariab}
begin
  FSto := stVariab;
  FVar := xvar;    //No hace falta actualziar el tipo
  rVarBase := nil;  //Inicia a este valor
end;
procedure TOperand.SetAsExpres(xtyp: TxpEleType);
{Fija el almacenamiento del Operando como Expresión, del tipo indicado}
begin
  FSto := stExpres;
  FTyp := xtyp;
  rVarBase := nil;  //Inicia a este valor
end;
procedure TOperand.SetAsExpresA(xtyp: TxpEleType);
begin
  FSto := stExpresA;
  FTyp := xtyp;
  rVarBase := nil;  //Inicia a este valor
end;
procedure TOperand.SetAsExpresX(xtyp: TxpEleType);
begin
  FSto := stExpresX;
  FTyp := xtyp;
  rVarBase := nil;  //Inicia a este valor
end;
procedure TOperand.SetAsExpresY(xtyp: TxpEleType);
begin
  FSto := stExpresY;
  FTyp := xtyp;
  rVarBase := nil;  //Inicia a este valor
end;
procedure TOperand.SetAsVarRef(VarBase: TxpEleVar; xtyp: TxpEleType);
{Fija el operando como de tipo stVarRef, siguiendo el estandar de la
documentación}
begin
  FSto := stVarRef;
  FVar := VarBase;
  FTyp := xtyp;
  rVarBase := nil;  //Inicia a este valor
end;
procedure TOperand.SetAsVarConRef(VarBase: TxpEleVar; ValOff: integer;
  xtyp: TxpEleType);
{Fija el operando como de tipo stVarConRef.}
begin
  FSto := stVarConRef;
  FVar := VarBase;
  FValue.ValInt := ValOff;
  FTyp := xtyp;
  rVarBase := nil;  //Inicia a este valor
end;
procedure TOperand.SetAsExpRef(Etyp: TxpEleType);
begin
  FSto := stExpRef;
  FTyp := Etyp;
  rVarBase := nil;  //Inicia a este valor
end;
procedure TOperand.SetAsNull;
{Configura al operando como de tipo Null}
begin
  {Se pone como constante, que es el tipo más simple, además se protege, pro si era
  variable}
  FSto := stNull;
  FTyp := typNull;   //Este es el tipo NULO
end;
function TOperand.StoOpStr: string;
{Devuelve lmacenamiento como cadena.}
begin
  case Sto of
  stConst    : exit('Constant');
  stVariab   : exit('Variable');
  stExpres   : exit('Expression');
  stVarRef   : exit('Referenced by Var');
  stVarConRef: exit('Referenced by Var');
  stExpRef   : exit('Referenced by Expresion ');
  //Aditional expressions types
  stExpresA  : exit('Register A');
  stExpresX  : exit('Register X');
  stExpresY  : exit('Register Y');
  else
    exit('?');
  end;
end;
function TOperand.StoOpChr: char;
{Return operand storage as a char.}
begin
  Result := ' ';
  case Sto of
  stConst    : exit('k');
  stVariab   : exit('v');
  stExpres   : exit('X');
  stVarRef   : exit('r');
  stVarConRef: exit('s');
  stExpRef   : exit('t');
  else
    exit('?');
  end;
end;
procedure TOperand.DefineRegister;
begin
  //llama al evento de pila
  if Typ.OnDefRegister<> nil then Typ.OnDefRegister;
end;
function TOperand.FindOperator(const oper: string): TxpOperator;
//Recibe la cadena de un operador y devuelve una referencia a un objeto Toperator, del
//operando. Si no está definido el operador para este operando, devuelve nullOper.
begin
  Result := Typ.FindBinaryOperator(oper);
end;
//Accesos a propiedades de p1^ y p2^.
function TCompOperands.value1: dword; inline;
begin
  Result := p1^.valInt;
end;
function TCompOperands.value1L: word; inline;
begin
  Result := p1^.LByte;
end;
function TCompOperands.value1H: word; inline;
begin
  Result := p1^.HByte;
end;
function TCompOperands.value1U: word; inline;
begin
  Result := p1^.UByte;
end;
function TCompOperands.value1E: word; inline;
begin
  Result := p1^.EByte;
end;
function TCompOperands.value2: dword; inline;
begin
  Result := p2^.valInt;
end;
function TCompOperands.value2L: word; inline;
begin
  Result := p2^.LByte;
end;
function TCompOperands.value2H: word; inline;
begin
  Result := p2^.HByte;
end;
function TCompOperands.byte1: word;
begin
  Result := p1^.rVar.addr;
end;
function TCompOperands.byte1L: word;
begin
  Result := p1^.rVar.addr;
end;
function TCompOperands.byte1H: word; inline;
begin
  Result := p1^.rVar.addrH;
end;
function TCompOperands.byte2: word;
begin
  Result := p2^.rVar.addr;
end;
function TCompOperands.byte2L: word;
begin
  Result := p2^.rVar.addr;
end;
function TCompOperands.byte2H: word; inline;
begin
  Result := p2^.rVar.addrH;
end;
function TCompOperands.OperationStr(Opt: TxpOperation): string;
{Return a string with types/storag and operation, having in "p1", "p2" y "Opt".}
var
  Operat: TxpOperator;
begin
  Operat:= Opt.parent;
  Result := p1^.StoOpStr+'(' + p1^.Typ.name + ') ' + Operat.txt + ' ' +
            p2^.StoOpStr+'(' + p2^.Typ.name + ')';
end;
function TCompOperands.stoOperation: TStoOperandsROB;
begin
  //Combinación de los almacenamientos de los operandos
  Result := TStoOperandsROB((Ord(p1^.Sto) << 4) or ord(p2^.Sto));
end;
procedure TCompOperands.ExchangeP1_P2;
{Intercambia el orden de los operandos.}
var
  tmp : ^TOperand;
begin
  //Invierte los operandos
  tmp := p1;
  p1 := p2;
  p2 := tmp;
end;
//Errors and warnings
procedure TCompOperands.ClearError;
{Limpia la bandera de errores. Tomar en cuenta que solo se debe usar para iniciar el
procesamiento de errores. Limpiar errores en medio de la compilación, podría hacer que
se pierda el rastro de errores anteriores, y que inclusive, la compilación termine sin
error, aún cuando haya generado errores intermedios.
Como norma, se podría decir que solo se debe usar, después de haber proecsado un posible
error anterior.}
begin
  HayError := false;
end;
procedure TCompOperands.GenInfo(msg: string);
begin
  if OnInfo<>nil then OnInfo(msg);
end;
procedure TCompOperands.GenWarn(msg: string; fil: String; row, col: integer);
{Genera un mensaje de advertencia en la posición indicada.}
begin
  if OnWarning<>nil then OnWarning(msg, fil, row, col);
end;
procedure TCompOperands.GenWarn(msg: string; const Args: array of const;
  fil: String; row, col: integer);
begin
  GenWarn(Format(msg, Args), fil, row, col);
end;
procedure TCompOperands.GenWarn(msg: string);
{Genera un mensaje de Advertencia, en la posición actual del contexto. }
begin
  if (cIn = nil) or (cIn.curCon = nil) then begin
    GenWarn(msg, '', -1, -1);
  end else begin
    GenWarn(msg, cIn.curCon.arc, cIn.curCon.row, cIn.curCon.col);
  end;
end;
procedure TCompOperands.GenWarn(msg: string; const Args: array of const);
{Genera un mensaje de Advertencia, en la posición actual del contexto. }
begin
  GenWarn(Format(msg, Args));
end;
procedure TCompOperands.GenWarnPos(msg: string; const Args: array of const;
  srcPos: TSrcPos);
begin
  GenWarn(Format(msg, Args), srcPos.fil, srcPos.row, srcPos.col);
end;
procedure TCompOperands.GenError(msg: string; fil: String; row, col: integer);
{Genera un mensaje de error en la posición indicada.}
begin
  //Protección
  if cIn.curCon = nil then begin
    HayError := true;
    exit;
  end;
  if cIn.curCon.FixErrPos then begin
    //El contexto actual, tiene configurado uan posición fija para los errores
    msg := cIn.curCon.PreErrorMsg + msg;  //completa mensaje
    if OnError<>nil then OnError(msg, cIn.curCon.ErrPosition.fil,
                                      cIn.curCon.ErrPosition.row,
                                      cIn.curCon.ErrPosition.col);

  end else begin
    if OnError<>nil then OnError(msg, fil, row, col);
  end;
  HayError := true;
end;
procedure TCompOperands.GenError(msg: String; const Args: array of const;
  fil: String; row, col: integer);
{Versión con parámetros de GenError.}
begin
  GenError(Format(msg, Args), fil, row, col);
end;
procedure TCompOperands.GenError(msg: string);
{Función de acceso rápido para Perr.GenError(). Pasa como posición a la posición
del contexto actual. Realiza la traducción del mensaje también.}
begin
  if (cIn = nil) or (cIn.curCon = nil) then begin
    GenError(msg, '', -1, -1);
  end else begin
    GenError(msg, cIn.curCon.arc, cIn.curCon.row, cIn.curCon.col);
  end;
end;
procedure TCompOperands.GenError(msg: String; const Args: array of const);
{Genera un mensaje de error eb la posición actual del contexto.}
begin
  GenError(Format(msg, Args));
end;
procedure TCompOperands.GenErrorPos(msg: String; const Args: array of const;
  srcPos: TSrcPos);
{Genera error en una posición específica del código}
begin
  GenError(Format(msg, Args), srcPos.fil, srcPos.row, srcPos.col);
end;

end.

