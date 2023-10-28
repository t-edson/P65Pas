unit XpresMIR;
{Unidad con la definiicón del objeto MIR. A diferencia del árbol de sintaxis esta
representación intermedia es lineal.
                                          Por Tito Hinostroza 21/10/2023.}
{$mode ObjFPC}{$H+}
{$Define DEBUGMODE}   //Enable MIR visualization
interface
uses
  Classes, SysUtils, fgl, XpresElemP65;
type  //MIR base class
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

type  //MIR elements
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

  TMirOperand = object
    opType: TopType;        //Operand type (otVariab, otConst, otFunct) like AST elements.
    varDec: TMirVarDec;     //Reference to var declaration, when it's variable. Otherwise it' will be NIL.
    astOperand: TEleExpress; //Ref. to AST element. Should be used only for error location.
  end;

  { TMirAssign }
  TMirAssign = Class(TMirElement)
    mirAssType: (mat1op,  //One operand in the right side.
                 mat2op); //Two operands in the right side.
    opDest: TMirOperand;  //Target variable or declared variable.
    opSrc1: TMirOperand;  //Source operand.
    opSrc2: TMirOperand;  //Source operand.
    constructor Create; virtual;
  end;

  { TMirFunction }
  TMirFunction = Class(TMirElement)
    funcName: TEleFun;  //Target variable or declared variable.
    instructions: TMirElements;     //Instruction list.
    constructor Create; virtual;
    destructor Destroy; override;
  end;

type  //Main Container
  { TMirList }
  TMirList = class
    mirElements: TMirElements;
  public  //Adding elements
    function AddVarDeclar(fdest: TMirFunction; varDec0: TEleVarDec): TMirVarDec;
    procedure AddAssign(fdest: TMirFunction; Op1, Op2: TEleExpress);
    procedure AddAssign(fdest: TMirFunction; Op1, Op2, Op3: TEleExpress);
    function AddSysNormalFunction(funcName0: TEleFun): TMirFunction;
    function AddUsrNormalFunction(funcName0: TEleFun): TMirFunction;
  public  //Reading from AST
    procedure ConvertBody(cntFunct: TMirFunction; sntBlock: TEleCodeCont);
  public  //Initialization
    procedure Clear;
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
function TMirList.AddVarDeclar(fdest: TMirFunction; varDec0: TEleVarDec): TMirVarDec;
{Add a Variable  declaration}
begin
  Result := TMirVarDec.Create;
  Result.text := varDec0.name;
  Result.vardec := varDec0;
  if fdest = nil then mirElements.Add(Result)
  else fdest.instructions.Add(Result)
end;
procedure SetOperandFromASTExpress(const AstOper: TEleExpress;
                                   out MirOper: TMirOperand);
{Read data from a TEleExpress and set a TMirOperand}
begin
  MirOper.opType := AstOper.opType;  //Must be set
  if AstOper.opType = otConst then begin
     MirOper.varDec := nil;
     MirOper.astOperand := AstOper;
  end else if AstOper.opType = otVariab then begin
     MirOper.varDec := TMirVarDec(AstOper.rvar.mirVarDec);  //Must be set
     MirOper.astOperand := AstOper;
  end else if AstOper.opType = otFunct then begin
     MirOper.varDec := nil;
     MirOper.astOperand := AstOper;
  end;
end;
procedure TMirList.AddAssign(fdest: TMirFunction;
    Op1,       //Target variable
    Op2: TEleExpress);  //Source expression
{Add element to the MIR container.}
var
  assigElem: TMirAssign;
begin
  //Set destination
  if Op1.opType <> otVariab then exit;
  assigElem:= TMirAssign.Create;
  {$IFDEF DEBUGMODE}  //Only needed to display MIR
  assigElem.text := Op1.name + ':=' + Op2.name;
  {$ENDIF}
  SetOperandFromASTExpress(Op1, assigElem.opDest);
  //Set right operand
  SetOperandFromASTExpress(Op2, assigElem.opSrc1);
  assigElem.mirAssType := mat1op;  //One operand at right.
  //Add to list
  if fdest = nil then mirElements.Add(assigElem)
  else fdest.instructions.Add(assigElem)
end;
procedure TMirList.AddAssign(fdest: TMirFunction;
    Op1,       //Target variable
    Op2, Op3: TEleExpress);  //Source operands
{Add element to the MIR container.}
var
  assigElem: TMirAssign;
begin
  //Set destination
  if Op1.opType <> otVariab then exit;
  assigElem:= TMirAssign.Create;
  {$IFDEF DEBUGMODE}  //Only needed to display MIR
  assigElem.text := Op1.name + ':=' + Op2.name + '' + Op3.name;
  {$ENDIF}
  SetOperandFromASTExpress(Op1, assigElem.opDest);
  //Set right operands
  SetOperandFromASTExpress(Op2, assigElem.opSrc1);
  SetOperandFromASTExpress(Op3, assigElem.opSrc2);
  assigElem.mirAssType := mat2op; //We have 2 operands
  //Add to list
  if fdest = nil then mirElements.Add(assigElem)
  else fdest.instructions.Add(assigElem)
end;
function TMirList.AddSysNormalFunction(funcName0: TEleFun): TMirFunction;
begin
  Result := TMirFunction.Create;
  Result.text := funcName0.name;
  Result.funcName := funcName0;
  mirElements.Add(Result);
end;
function TMirList.AddUsrNormalFunction(funcName0: TEleFun): TMirFunction;
begin
  Result := TMirFunction.Create;
  Result.text := funcName0.name;
  Result.funcName := funcName0;
  mirElements.Add(Result);
end;
//Reading from AST
procedure TMirList.ConvertBody(cntFunct: TMirFunction; sntBlock: TEleCodeCont);
{Convert a ASTBody to instructions in MIR representation.
Parameters:
  cntFunct  -> The function where MIR will be created, or the main program. This will
               be used as reference to locate the new variable declarations.
  sntBlock  -> Block of code where are the sentences to need be prepared. It's the
               same of "cntFunct" except when "block" is nested like in a condiitonal.
}
{  function MoveParamToAssign(curContainer: TxpElement; Op: TEleExpress;
                             parvar: TEleVarDec): TEleExpress;
  {Mueve el nodo especificado "Op", que representa a un parámetro de la función, a una
  nueva instruccion de asignación (que es creada al inicio del bloque "curContainer") y
  reemplaza el nodo faltante por una variable temporal que es la que se crea en la
  instrucción de asignación.
  Es similar a MoveNodeToAssign() pero no reemplaza el nodo movido y no crea una variable
  auxiliar, sino que usa "parvar".
  Retorna la instrucción de asignación creada.
  }
  var
    _setaux: TEleExpress;
    Op1aux: TEleExpress;
    funSet: TEleFunBase;
  begin
    //Create the new _set() expression.
    _setaux := CreateExpression('_set', typNull, otFunct, Op.srcDec);
    funSet := MethodFromBinOperator(Op.Typ, ':=', Op.Typ);
    if funSet = nil then begin   //Operator not found
      GenError('Undefined operation: %s %s %s', [Op.Typ.name, ':=', Op.Typ.name], Op.srcDec);
      _setaux.Destroy;    //We destroy because it hasn't been included in the AST.
      exit(nil);
    end;
    _setaux.rfun := funSet;

    //Add the new assigment before the main
    TreeElems.openElement(curContainer);
    TreeElems.AddElement(_setaux, 0);    //Add a new assigmente before
    _setaux.elements := TxpElements.Create(true);  //Create list
    TreeElems.openElement(_setaux);

    //Add first operand (variable) of the assignment.
    Op1aux := CreateExpression(parvar.name, parvar.typ, otVariab, Op.srcDec);
    Op1aux.SetVariab(parvar);
    TreeElems.addElement(Op1aux);
    AddCallerToFromCurr(parvar); //Add reference to auxiliar variable.

    //Move the second operand to the previous _set created
    TreeElems.ChangeParentTo(_setaux, Op);

    exit(_setaux);
  end;
  function SplitProcCall(curContainer: TxpElement; expMethod: TEleExpress): boolean; forward;
}  function SplitSet(curContainer: TxpElement; setMethod: TxpElement): boolean;
  {Process a set sentence. If a set expression has more than three operands
  it's splitted adding one or more aditional set sentences, at the beggining of
  "curContainer".
  If at least one new set sentence is added, returns TRUE.}
  var
    Op2, parExp, new_set, Op1, idx: TEleExpress;
    par: TxpElement;
    nParams: Integer;
  begin
    Result := false;
    if TEleExpress(setMethod).rfun.getset <> gsSetInSimple then exit;
    Op1 := TEleExpress(setMethod.elements[0]);  //Takes target.
    if Op1.opType <> otVariab then exit;
    //Split expressions in second operand of assignment.
    Op2 := TEleExpress(setMethod.elements[1]);  //Takes assignment source.
    if (Op2.opType = otVariab) then begin       //x := var1
      AddAssign(cntFunct, Op1, Op2);
    end else if (Op2.opType = otConst) then begin //x := CONS1
      AddAssign(cntFunct, Op1, Op2);
    end else if (Op2.opType = otFunct) then begin
      //Op2 is a function: 2 or more operands
      if Op2.rfun.callType in [ctSysNormal, ctUsrNormal] then begin  //Normal function
//        {IT's the form:
//             x := func(x,y);
//                  \_______/
//                     Op2
//        }
//        //Generates an asignment for each parameter.
//        SplitProcCall(curContainer, Op2);
      end else if Op2.rfun.callType = ctSysInline then begin       //INLINE function
        {IT's the form:
             x := A + B
                  \___/
                   Op2
        or:
             x := A++        }
        {We expect parameters A, B should be simple operands (Constant or variables)
        otherwise we will move them to a separate assignment}
        nParams := Op2.elements.Count;  //Parameters quantity
        if nParams = 1 then begin   //x := func(1); or x := a++;
          AddAssign(cntFunct, Op1, Op2);
        end else if nParams = 2 then begin

        end else begin  //3 or more paremeters? No Supported
//          for par in Op2.elements do begin
//            parExp := TEleExpress(par);
//            if parExp.opType = otFunct then begin
//              new_set := MoveNodeToAssign(cntFunct, curContainer, parExp);
//              if HayError then exit;
//              SplitSet(curContainer, new_set);  //Check if it's needed split the new _set() created.
//              Result := true;
//            end;
//          end;
        end;
      end;
    end;
  end;
{  function SplitExpress(curContainer: TxpElement; expMethod: TEleExpress): boolean;
  {Verify if an expression has more than three operands. If so then
  it's splitted adding one or more set sentences.
  If at least one new set sentence is added, returns TRUE.}
  var
    parExp, new_set: TEleExpress;
    par: TxpElement;
  begin
    Result := false;
    if (expMethod.opType = otFunct) then begin  //Neither variables nor constants.
      {We expect parameters should be simple operands (Constant or variables)
      otherwise we will move them to a separate assignment}
      if expMethod.rfun.callType in [ctSysNormal, ctUsrNormal] then begin  //Normal function

        //Generates an asignment for each parameter.
        SplitProcCall(curContainer, expMethod);
      end else if expMethod.rfun.callType = ctSysInline then begin  //Like =, >, and, ...
        for par in expMethod.elements do begin
          parExp := TEleExpress(par);
          if parExp.opType = otFunct then begin
            new_set := MoveNodeToAssign(cntFunct, curContainer, parExp);
            if HayError then exit;
            SplitSet(curContainer, new_set);  //Check if it's needed split the new _set() created.
            Result := true;
          end;
        end;
      end;
    end;
  end;}
//  function SplitProcCall(curContainer: TxpElement; expMethod: TEleExpress): boolean;
//  {Split a procedure (not INLINE) call instruction, inserting an assignment instruction
//  for each parameter.}
//  var
//    parExp, new_set: TEleExpress;
//    funcBase: TEleFunBase;
//    ipar: Integer;
//    par: TxpParFunc;
//  begin
//    Result := false;
//    if expMethod.opType <> otFunct then exit;   //Not a fucntion call
//    funcBase := expMethod.rfun;    //Base function reference
//    if funcBase.codSysInline=nil then begin   //Not INLINE
//      {Move all parameters (children nodes) to a separate assignment}
//      ipar := 0;  //Parameter index
//      while expMethod.elements.Count>0 do begin  //While remain parameters.
//        parExp := TEleExpress(expMethod.elements[0]);  //Take parameter element
//        par := funcBase.pars[ipar];
//        new_set := MoveParamToAssign(curContainer, parExp, par.pvar);
//        if HayError then exit;
//        SplitSet(curContainer, new_set);  //Check if it's needed split the new _set() created.
//        Result := true;
//        inc(ipar);
//      end;
//    end;
//  end;

var
  sen: TEleSentence;
  eleSen, _set, ele, _proc: TxpElement;
  _exp, Op1, Op2, val1, val2: TEleExpress;
  _blk, _blk0: TEleCodeCont;
begin
  //Prepare assignments for arrays.
//  for eleSen in sntBlock.elements do begin
//    if eleSen.idClass <> eleSenten then continue;
//    //We have a sentence here.
//    sen := TEleSentence(eleSen);
//    if sen.sntType = sntAssign then begin  //Assignment
//      _set := sen.elements[0];  //Takes the _set method.
//      Op1 := TEleExpress(_set.elements[0]);  //Takes assigment target.
//      Op2 := TEleExpress(_set.elements[1]);  //Takes assigment target.
//      if (Op1.opType = otFunct) and (Op1.rfun.getset = gsGetInItem) then begin
//        //It's a _set() for a _getitem() INLINE assignment for array.
//        if Op1.rfun.funset = nil then begin
//          GenError('Cannot locate the setter for this type.');
//          exit;
//        end;
//        //Convert getter() to setter().
//        Op1.rfun := Op1.rfun.funset;     //Must be gsSetInItem
//        Op1.name := Op1.rfun.name;
//        Op1.Typ  := Op1.rfun.retType;
//        //Move third parameter to _setitem() and locate it at the Top
//        TreeElems.ChangeParentTo(Op1, Op2);
//        TreeElems.ChangeParentTo(Op1.Parent.Parent, Op1);
//        _set.Parent.elements.Remove(_set);
//      end else if (Op1.opType = otFunct) and (Op1.rfun.getset = gsGetInPtr) then begin
//        //It's a _set() for a _getptr() INLINE assignment for POINTER.
//        if Op1.rfun.funset = nil then begin
//          GenError('Cannot locate the setter for this type.');
//          exit;
//        end;
//        //Convert getter() to setter().
//        Op1.rfun := Op1.rfun.funset;     //Must be gsSetInPtr;
//        Op1.name := Op1.rfun.name;
//        Op1.Typ  := Op1.rfun.retType;
//        //Move third parameter to _setptr() and locate it at the Top
//        TreeElems.ChangeParentTo(Op1, Op2);
//        TreeElems.ChangeParentTo(Op1.Parent.Parent, Op1);
//        _set.Parent.elements.Remove(_set);
//      end;
//    end;
//  end;
  //Prepare sentences
  for eleSen in sntBlock.elements do begin
    if eleSen.idClass <> eleSenten then continue;
    //We have a sentence here.
    sen := TEleSentence(eleSen);
    if sen.sntType = sntAssign then begin  //Assignment
      _set := sen.elements[0];  //Takes the one _set method.
      SplitSet(sen, _set)  //Might generate additional assignments sentences
{    end else if sen.sntType = sntProcCal then begin  //Procedure call
      _proc := sen.elements[0];  //Takes the proc.
      SplitProcCall(sen, TEleExpress(_proc))
    end else if sen.sntType in [sntIF, sntREPEAT, sntWHILE] then begin
      //There are expressions and blocks inside conditions and loops.
      for ele in sen.elements do begin
        if ele.idClass = eleCondit then begin  //It's a condition
          _exp := TEleExpress(ele.elements[0]);  //The first item is a TEleExpress
          SplitExpress(ele, _exp)
        end else if ele.idClass = eleBlock then begin   //body of IF
          _blk := TEleCodeCont(ele);  //The first item is a TEleExpress
          PrepareBody(cntFunct, _blk);
        end;
      end;
    end else if sen.sntType = sntFOR then begin
      //FOR sentences could need some aditional changes.
      _blk0 := nil;
      for ele in sen.elements do begin
        if ele.idClass = eleCondit then begin  //It's a condition
          _exp := TEleExpress(ele.elements[0]);  //The first item is a TEleExpress
          SplitExpress(ele, _exp)
        end else if ele.idClass = eleBlock then begin   //Initialization or body
          _blk := TEleCodeCont(ele);  //The first item is a TEleExpress
          PrepareBody(cntFunct, _blk);
          if _blk0 = nil then _blk0 := _blk;  //Get intialization block
        end;
      end;
      //Get first and last value of index.
      val1 := TEleExpress(_blk0.elements[0].elements[1]);
      val2 := TEleExpress(_exp.elements[1]);
      //Special cases
      if (val1.opType = otConst) and (val2.opType = otConst) then begin
        if val1.val > val2.val then begin
          //Unreachable code
//          TreeElems.DeleteTypeNode();
        end;
      end;
    end else if sen.sntType = sntExit then begin
      if sen.elements.Count>0 then begin   //If there is argument
        _exp := TEleExpress(sen.elements[0]);  //The first item is a TEleExpress
        SplitExpress(sen, _exp)
      end;
 }   end;

  end;
end;

//Initialization
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

