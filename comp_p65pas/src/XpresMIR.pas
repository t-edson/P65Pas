unit XpresMIR;
{Unidad con la definiicón del objeto MIR. A diferencia del árbol de sintaxis esta
representación intermedia es lineal.
                                          Por Tito Hinostroza 21/10/2023.}
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, fgl, XpresElemP65;
type
  //MIR Element type
  TMirType = (
     mtyAssign      //Assignment
    ,mtyVarDec      //Var declaration
    ,mtyFunCall     //Function call
    );

  { TMirElement }
  TMirElement = Class;
  TMirElements = specialize TFPGObjectList<TMirElement>;
  TMirElement = Class
    mirType: TMirType;
    text   : String;  //Label to show
  end;

  { TMirAssign }
  TMirAssign = Class(TMirElement)
    varName0: TEleVarDec;  //Target variable or declared variable.
    varName1: TEleVarDec;  //Source variable.
    varName2: TEleVarDec;  //Source variable.
    constructor Create; virtual;
  end;

  { TMirFunction }
  TMirFunction = Class(TMirElement)
    funcName: TEleFun;  //Target variable or declared variable.
    instructions: TMirElements;     //Instruction list.
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TMirVarDec }

  TMirVarDec = Class(TMirElement)
    vardec: TEleVarDec;  //Target variable or declared variable.
  public  //Campos para guardar las direcciones físicas asignadas en RAM.
    allocated: boolean;   //Activated when variable is allocated (RAM or register).
    storage  : TStorage;  //Depend on adicPar.hasAdic.
    addr     : word;      //Base address.
    function addrL: word; inline;  //Devuelve la dirección absoluta de la variable (LOW)
    function addrH: word; inline;  //Devuelve la dirección absoluta de la variable (HIGH)
    function addrE: word; inline;  //Devuelve la dirección absoluta de la variable (EXTRA)
    function addrU: word; inline;  //Devuelve la dirección absoluta de la variable (ULTRA)
    function AddrString: string;   //Devuelve la dirección física como cadena
    procedure ResetAddress; //Limpia las direcciones físicas
    function stoStr: string;
  public
    constructor Create; virtual;
  end;

  { TMirList }
  //Main container
  TMirList = class
    mirElements: TMirElements;
  public
    procedure AddVarDeclar(varDec0: TEleVarDec);
    procedure AddInstruction(varName0, varName1, varName2: TEleVarDec);
    procedure AddSysNormalFunction(funcName0: TEleFun);
    procedure AddUsrNormalFunction(funcName0: TEleFun);
    procedure Clear;
  public  //Constructor and destructror
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

function TMirVarDec.addrL: word;
{Dirección absoluta de la variable de menor pero, cuando es de tipo WORD.}
begin
  Result := addr;
end;
function TMirVarDec.addrH: word;
{Dirección absoluta de la variable de mayor pero, cuando es de tipo WORD.}
begin
  Result := addr + 1;
end;
function TMirVarDec.addrE: word;
begin
  Result := addr + 2;
end;
function TMirVarDec.addrU: word;
begin
  Result := addr + 3;
end;
function TMirVarDec.AddrString: string;
{Devuelve una cadena, que representa a la dirección física.}
begin
  if vardec.typ.IsByteSize then begin
    Result := '$' + IntToHex(addr, 3);
  end else if vardec.typ.IsWordSize then begin
    Result := '$' + IntToHex(addr, 3);
  end else if vardec.typ.IsDWordSize then begin
    Result := '$' + IntToHex(addr, 3);
  end else begin
    Result := '';   //Error
  end;
end;
procedure TMirVarDec.ResetAddress;
begin
  addr := 0;
end;
function TMirVarDec.stoStr: string;
begin
  WriteStr(Result, storage);
end;

{ TMirVarDec }
constructor TMirVarDec.Create;
begin
  mirType := mtyVarDec;
end;
{ TMirAssign }
constructor TMirAssign.Create;
begin
  mirType := mtyAssign;
end;
{ TMirFunction }
constructor TMirFunction.Create;
begin
  mirType := mtyFunCall;
  instructions:= TMirElements.Create(true);
end;
destructor TMirFunction.Destroy;
begin
  instructions.Destroy;
  inherited Destroy;
end;
{ TMirList }
procedure TMirList.AddInstruction(varName0,       //Target variable
    varName1, varName2: TEleVarDec);       //Source variables;
{Add element to the MIR container.}
var
  assigElem: TMirAssign;
begin
  assigElem:= TMirAssign.Create;
  assigElem.varName0 := varName0;
  assigElem.varName1 := varName1;
  assigElem.varName2 := varName2;
  mirElements.Add(assigElem);
end;
procedure TMirList.AddSysNormalFunction(funcName0: TEleFun);
var
  functElem: TMirFunction;
begin
  functElem := TMirFunction.Create;
  functElem.text := funcName0.name;
  functElem.funcName := funcName0;
  mirElements.Add(functElem);
end;
procedure TMirList.AddUsrNormalFunction(funcName0: TEleFun);
var
  functElem: TMirFunction;
begin
  functElem := TMirFunction.Create;
  functElem.text := funcName0.name;
  functElem.funcName := funcName0;
  mirElements.Add(functElem);
end;
procedure TMirList.AddVarDeclar(varDec0: TEleVarDec);
var
  varDec: TMirVarDec;
begin
  varDec := TMirVarDec.Create;
  varDec.text := varDec0.name;
  varDec.vardec:= varDec0;
  mirElements.Add(varDec);
end;
procedure TMirList.Clear;
begin
  mirElements.Clear;
end;
constructor TMirList.Create;
begin
  mirElements:= TMirElements.Create(true);
end;
destructor TMirList.Destroy;
begin
  mirElements.Destroy;
  inherited Destroy;
end;

end.

