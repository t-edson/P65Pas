{Unidad con rutinas del analizador sintáctico.
}
unit Compiler_PIC16;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LazLogger,
  P65C02utils, CPUCore, CompBase, ParserDirec, GenCodBas_PIC16,
  GenCod_PIC16, CompGlobals, XpresElemP65, ParserASM_6502, StrUtils;
type
  { TCompiler_PIC16 }
  TCompiler_PIC16 = class(TGenCod)
  private  //Funciones básicas
    addBootldr: integer;  //Address where start Bootloader.
    addVariab : integer;  //Address where start Variables section.
    addFuncts : integer;  //Address where start function section.
    procedure ConstantFoldExpr(eleExp: TEleExpress);
    procedure PrepareBody(cntBody, sntBlock: TEleCodeCont);
    procedure PrepareSentences;
  private  //Compilers steps
    procedure EvaluateConstantDeclare;
    procedure ConstantFolding;
    procedure ConstanPropagation;
    procedure DoOptimize;
    procedure DoGenerateCode;
    procedure DoCompile;
  public      //Events
    OnAfterCompile: procedure of object;   //Al finalizar la compilación.
  public      //Override methods
    procedure DumpCode(lins: TSTrings); override;
    function RAMusedStr: string; override;
    procedure GetResourcesUsed(out ramUse, romUse, stkUse: single); override;
    procedure GenerateListReport(lins: TStrings); override;
  public      //Interfaz for IDE
    procedure Exec(srcFile, outFile: string; pars: string);
  public      //Inicialización
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
  GenCod_PIC16.SetLanguage;
  ParserASM_6502.SetLanguage;
  {$I _language\tra_Compiler.pas}
end;
procedure TCompiler_PIC16.ConstantFoldExpr(eleExp: TEleExpress);
{Performs:
- Constant evaluation, for constant nodes that can be evaluated.
- Constant folding, for expression nodes, that returns constants.
Note the similarity of this method with GenCodeExpr().}
var
  funcBase: TEleFunBase;
  ele: TxpElement;
  parExpr: TEleExpress;
begin
  if eleExp.opType = otFunct then begin
    //It's an expression. There should be a function
    funcBase := eleExp.rfun;
    if funcBase.callType = ctSysInline then begin
      //Only INLINE functions can returns constants.
      if funcBase.idClass = eleFunc then begin
        //It's the implementation. No problem.
        //Process all parameters.
        for ele in eleExp.elements do begin
          parExpr := TEleExpress(ele);
          ConstantFoldExpr(parExpr);  //Try to evaluate constant.
          if HayError then exit;
        end;
        { TODO : Tal vez sea posible que por optimización, solo llamar a
        funcBase.codSysInline() cuando los parámetros (o alguno) sean constante,
        para evitar muchas llamadas }
        funcBase.codSysInline(eleExp);  //We don't expect this generates code.
        //Check if we can simplify
        if eleExp.opType = otConst then begin
          //Node resulted as a constant.
          if eleExp.evaluated then begin
            //We convert it to a simple constant (Constant fold).
            eleExp.elements.Clear;  //Constants don't have childrens.
          end else begin
            //Maybe this constant depend of RAM assigment
          end;
        end else if eleExp.opType = otVariab then begin
          eleExp.elements.Clear;  //Variables don't have childrens.
        end;
      end else begin
        //Should be the declaration. Maybe it's already implemented, or maybe not.
        //funcDec := TxpEleFunDec(funcBase);
        { TODO : Completar este caso. Por ahora no lo permitiremos. }
        GenError('No supported unimplemented INLINE functions.');
      end;
    end else begin
      //In Normal subroutine, we scan for parameters
      //Process all parameters.
      for ele in eleExp.elements do begin
        parExpr := TEleExpress(ele);
        ConstantFoldExpr(parExpr);  //Try to evaluate constant.
        if HayError then exit;
      end;
      { TODO : ¿No debería llamarse también a functCall(). Allí también se genera código.? }
    end;
  end else if eleExp.opType = otConst then begin
    eleExp.Evaluate();   //Try to evaluate
    //Some constants like @variable or @function cannot be evaluated in optimization
    //if not eleExp.evaluated then begin
    //  GenError('Constant not evaluated.', eleExp.srcDec);
    //  exit;
    //end;
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
  vard: TEleVarDec;
  consExpres: TEleExpress;
begin
  //Calculate values in CONST sections
  for cons in TreeElems.AllCons do begin
    {For optimization we should evaluate only used Constant, but we evaluate alls
    because constants like the field "length" of arrays, needs to be evaluated in order
    to get the size defined, before assign RAM. }
    //if vard.nCalled = 0 then continue; //Skip unused variable.
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
       cons.value := @consExpres.value;
       cons.evaluated := true;
     end;
  end;
  //Calculate values in VAR sections
  for vard in TreeElems.AllVars do begin
    if vard.nCalled = 0 then continue; //Skip unused variable.
    if vard.elements.Count = 0 then continue;  //Skip vars with no initialization.
    TreeElems.OpenElement(vard);  //To resolve properly identifiers
    consExpres := TEleExpress(vard.elements[0]);  //Takes the expression node.
    if not consExpres.evaluated then begin
      //If it isn't evaluated, must be an expression.
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
      consExpres.value := consExpres.value;
      consExpres.evaluated := true;
    end;
  end;
end;
procedure TCompiler_PIC16.ConstantFolding;
{Do a fold constant optimization and evaluate constant expresions. }
  procedure ConstantFoldBody(body: TEleBody);
  {Do constant folding simplification in all expression nodes. Note the similarity with
  TGenCodeBas.GenCodeSentences(), for scanning the AST.
  Constant fold are done only if constant are in the same Node. It is:

    <constant> + <constant> + <variable>

  Cases like:

    <constant> + <variable> + <constant>

  Won't be folded because constant will be located in the AST in different nodes.
  }
  var
    expFun: TEleExpress;
    sen: TEleSentence;
    eleSen: TxpElement;
    ele: TxpElement;
    i: Integer;
  begin
    //Process body
    TreeElems.OpenElement(body);
    for eleSen in TreeElems.curNode.elements do begin
      if eleSen.idClass <> eleSenten then continue;
      sen := TEleSentence(eleSen);
      if sen.sntType = sntAssign then begin    //assignment
        {After preparation, assignment sentences could be splitted in several assignment.}
        for ele in sen.elements do begin
          expFun := TEleExpress(ele);
          ConstantFoldExpr(expFun);  //Try to evaluate constant.
          if HayError then exit;
        end;
      end else if sen.sntType = sntProcCal then begin
        {After preparation, sntProcCal sentences could be include several
        assignment before the Call.}
        for i:=0 to sen.elements.Count-2 do begin  //Excluding the call
          expFun := TEleExpress(sen.elements[i]);
          ConstantFoldExpr(expFun);  //Try to evaluate constant.
          if HayError then exit;
        end;
      end;
    end;
    TreeElems.CloseElement;              //Close the Body.
  end;
var
  fun : TEleFun;
  bod: TEleBody;
begin
  compMod := cmConsEval;    //Mode Constant evaluation.
  pic.disableCodegen := true;  //Disable the code generation
  pic.iRam := 0;  //Clear RAM position
  //Code subroutines
  for fun in usedFuncs do begin
    if fun.codSysInline <> nil then continue;  //No INLINE
    ConstantFoldBody(fun.BodyNode);
    if HayError then exit;   //Puede haber error
  end;
  //Code body
  bod := TreeElems.BodyNode;  //lee Nodo del cuerpo principal
  ConstantFoldBody(bod);
end;
procedure TCompiler_PIC16.ConstanPropagation;
{Do a constant propagation optimization. }
  function IsForm_var_assign_const(assigExp: TEleExpress;
        out varDec: TEleVarDec;
        out consVal: TConsValue
        ): boolean;
  {Indicates if the assigment expression is of the form:
     <variable> := <constant>
  }
  var
    leftOp, rightOp: TEleExpress;
  begin
    if assigExp.opType <> otFunct then exit(false);  //Validation
    //Must be an assignment expression
    leftOp := TEleExpress(assigExp.elements[0]);
    rightOp := TEleExpress(assigExp.elements[1]);
    if (rightOp.Sto = stConst) and rightOp.evaluated then begin
       //It's the form: <variable> := <constant>
      varDec := leftOp.rvar;  //Takes var declaration
      consVal := rightOp.value;
      exit(true);
    end;
  end;
  function ChangeToConstant(Op: TEleExpress; varDec: TEleVarDec; const consVal: TConsValue): boolean;
  {Test if the operand is a variable refering to "varDec". If so, change it to a constant
  type, set to "consVal" and returns TRUE. }
  begin
    if Op.opType <> otVariab then exit(false);
    if varDec.IsParameter then exit(false);
    if (Op.rvar = varDec) then begin
      Op.opType := otConst;
      Op.Sto := stConst;
      Op.evaluated := true;
      Op.value := consVal;
      exit(True);
    end else begin
      exit(False);
    end;
  end;
  function ReplaceVarByConst(assigExp: TEleExpress; varDec: TEleVarDec;
        const consVal: TConsValue): boolean;
  {Replace the variable by a constant in the right part of an assignment expression.
  It replacing is done, returns TRUE.
  }
  var
    rightOp, Op: TEleExpress;
    ele: TxpElement;
  begin
    //Search at the right expression
    Result := false;
    rightOp := TEleExpress(assigExp.elements[1]);
    case rightOp.opType of
    otConst: begin
      //Nothing to replace
    end;
    otVariab: begin
      //Could be
      if ChangeToConstant(rightOp, varDec, consVal) then begin
        Result := True;
      end;
    end;
    otFunct: begin
      {Check for all of the operands of the function. Normal function at this level,
      won't have child nodes.}
      for ele in rightOp.elements do begin
        Op := TEleExpress(ele);
        if ChangeToConstant(Op, varDec, consVal) then begin
          Result := True;
        end;
      end;
    end;
    end;
  end;
  procedure ConstantPropagBody(body: TEleBody);
  {Do constant propagation in all sentences of the body. }
  var
    assigExp, assigToDelete: TEleExpress;
    sen: TEleSentence;
    eleSen, par: TxpElement;
    n, i: Integer;
    varDec, varDecToDelete: TEleVarDec;
    consVal: TConsValue;
    replaceMode, replaced: Boolean;
  begin
    //Process body
    TreeElems.OpenElement(body);
    for eleSen in TreeElems.curNode.elements do begin
      if eleSen.idClass <> eleSenten then continue;
      sen := TEleSentence(eleSen);
      if sen.sntType = sntAssign then begin    //Assignment
        //Explore previous posssible assigments
        n := sen.elements.Count;
        replaceMode := false;
        replaced := false;
        for i:=0 to n - 1 do begin
          //Search the type: <variable> := <constant>
          assigExp := TEleExpress(sen.elements[i]);  //Shoudn't fail
          if replaceMode then begin
            if ReplaceVarByConst(assigExp, varDec, consVal) then begin
              //Replaced here.
              replaced := true;
              debugln('Constant propagation applied to: ' + varDecToDelete.name);
            end;
          end else begin
            //Find mode. Finding the form: <variable> := <constant>
            if i = n-1 then continue;  //The last assigment is unuseful for replacing.
            if IsForm_var_assign_const(assigExp, varDec, consVal) then begin
              replaceMode := true;  //Enter to mode replace for following sentences
              assigToDelete := assigExp;  //This assigment will be deleted after replaced.
              varDecToDelete := varDec;
            end;
          end;
        end;
        //Delete assigment
        {Formally we can delete an assigment of the form: <variable> := <constant>
        even if it hasn't been replaced. In that case it will mean that <variable> is
        not used (if it cannot be replaced is other problem but we assume it can be
        replaced in all cases.).}
        if replaced then begin
          assigToDelete.elements.Clear;
          par := assigToDelete.Parent;
          par.elements.Remove(assigToDelete);
          //Delete references, because we've deleted the only one remained existent.
          varDecToDelete.lstCallers.Clear;
          //We can even delete the variable declaration if it's no used from other sentences.
        end;
      end;
    end;
    TreeElems.CloseElement;              //Close the Body.
  end;
var
  fun : TEleFun;
  bod: TEleBody;
begin
  compMod := cmConsEval;    //Generates code.
  pic.disableCodegen := true;  //Disable the code generation
  pic.iRam := 0;  //Clear RAM position
  //Code subroutines
  for fun in usedFuncs do begin
    if fun.codSysInline <> nil then continue;  //No INLINE
    ConstantPropagBody(fun.BodyNode);
    if HayError then exit;   //Puede haber error
  end;
  //Code body
  bod := TreeElems.BodyNode;  //lee Nodo del cuerpo principal
  ConstantPropagBody(bod);
end;
procedure TCompiler_PIC16.PrepareBody(cntBody, sntBlock: TEleCodeCont);
{Do a separation for assigmente sentences in order to have the "three-address code" form
like used in several compilers.
Parameters:
  cntBody  -> The main Body of a procedure or the main program. This will be used
               as reference to locate the new variable declarations.
  sntBlock  -> Block of code where are the sentences to need be prepared. It's the
               same of "container" except when "block" is nested like in a condiitonal.
}
  function MoveParamToAssign(curContainer: TxpElement; Op: TEleExpress;
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
  function SplitSet(curContainer: TxpElement; setMethod: TxpElement): boolean;
  {Verify if a set expression has more than three operands. If so then
  it's splitted adding one or more aditional set sentences, at the beggining of
  "curContainer".
  If at least one new set sentence is added, returns TRUE.}
  var
    Op2, parExp, new_set, Op1, idx: TEleExpress;
    par: TxpElement;
  begin
    Result := false;
    if TEleExpress(setMethod).rfun.getset <> gsSetInSimple then exit;
    //Split expressions in second operand of assignment.
    Op2 := TEleExpress(setMethod.elements[1]);  //Takes assignment source.
    if (Op2.opType = otFunct) then begin
      //Op2 is a function.
      if Op2.rfun.callType in [ctSysNormal, ctUsrNormal] then begin  //Normal function
        {IT's the form:
             x := func(x,y);
                  \_______/
                     Op2
        }
        //Generates an asignment for each parameter.
        SplitProcCall(curContainer, Op2);
      end else if Op2.rfun.callType = ctSysInline then begin       //INLINE function
        {IT's the form:
             x := A + B
                  \___/
                   Op2
        or:
             x := A++        }
        {We expect parameters A, B should be simple operands (Constant or variables)
        otherwise we will move them to a separate assignment}
        for par in Op2.elements do begin
          parExp := TEleExpress(par);
          if parExp.opType = otFunct then begin
            new_set := MoveNodeToAssign(cntBody, curContainer, parExp);
            if HayError then exit;
            SplitSet(curContainer, new_set);  //Check if it's needed split the new _set() created.
            Result := true;
          end;
        end;
      end;
    end;
  end;
  function SplitExpress(curContainer: TxpElement; expMethod: TEleExpress): boolean;
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
            new_set := MoveNodeToAssign(cntBody, curContainer, parExp);
            if HayError then exit;
            SplitSet(curContainer, new_set);  //Check if it's needed split the new _set() created.
            Result := true;
          end;
        end;
      end;
    end;
  end;
  function SplitProcCall(curContainer: TxpElement; expMethod: TEleExpress): boolean;
  {Split a procedure (not INLINE) call instruction, inserting an assignment instruction
  for each parameter.}
  var
    parExp, new_set: TEleExpress;
    funcBase: TEleFunBase;
    ipar: Integer;
    par: TxpParFunc;
  begin
    Result := false;
    if expMethod.opType <> otFunct then exit;   //Not a fucntion call
    funcBase := expMethod.rfun;    //Base function reference
    if funcBase.codSysInline=nil then begin   //Not INLINE
      {Move all parameters (children nodes) to a separate assignment}
      ipar := 0;  //Parameter index
      while expMethod.elements.Count>0 do begin  //While remain parameters.
        parExp := TEleExpress(expMethod.elements[0]);  //Take parameter element
        par := funcBase.pars[ipar];
        new_set := MoveParamToAssign(curContainer, parExp, par.pvar);
        if HayError then exit;
        SplitSet(curContainer, new_set);  //Check if it's needed split the new _set() created.
        Result := true;
        inc(ipar);
      end;
    end;
  end;
var
  sen: TEleSentence;
  eleSen, _set, ele, _proc: TxpElement;
  _exp, Op1, Op2, val1, val2: TEleExpress;
  _blk, _blk0: TEleCodeCont;
begin
  //Prepare assignments for arrays.
  for eleSen in sntBlock.elements do begin
    if eleSen.idClass <> eleSenten then continue;
    //We have a sentence here.
    sen := TEleSentence(eleSen);
    if sen.sntType = sntAssign then begin  //Assignment
      _set := sen.elements[0];  //Takes the _set method.
      Op1 := TEleExpress(_set.elements[0]);  //Takes assigment target.
      Op2 := TEleExpress(_set.elements[1]);  //Takes assigment target.
      if (Op1.opType = otFunct) and (Op1.rfun.getset = gsGetInItem) then begin
        //It's a _set() for a _getitem() INLINE assignment for array.
        if Op1.rfun.funset = nil then begin
          GenError('Cannot locate the setter for this type.');
          exit;
        end;
        //Convert getter() to setter().
        Op1.rfun := Op1.rfun.funset;     //Must be gsSetInItem
        Op1.name := Op1.rfun.name;
        Op1.Typ  := Op1.rfun.retType;
        //Move third parameter to _setitem() and locate it at the Top
        TreeElems.ChangeParentTo(Op1, Op2);
        TreeElems.ChangeParentTo(Op1.Parent.Parent, Op1);
        _set.Parent.elements.Remove(_set);
      end else if (Op1.opType = otFunct) and (Op1.rfun.getset = gsGetInPtr) then begin
        //It's a _set() for a _getptr() INLINE assignment for POINTER.
        if Op1.rfun.funset = nil then begin
          GenError('Cannot locate the setter for this type.');
          exit;
        end;
        //Convert getter() to setter().
        Op1.rfun := Op1.rfun.funset;     //Must be gsSetInPtr;
        Op1.name := Op1.rfun.name;
        Op1.Typ  := Op1.rfun.retType;
        //Move third parameter to _setptr() and locate it at the Top
        TreeElems.ChangeParentTo(Op1, Op2);
        TreeElems.ChangeParentTo(Op1.Parent.Parent, Op1);
        _set.Parent.elements.Remove(_set);
      end;
    end;
  end;
  //Prepare sentences
  for eleSen in sntBlock.elements do begin
    if eleSen.idClass <> eleSenten then continue;
    //We have a sentence here.
    sen := TEleSentence(eleSen);
    if sen.sntType = sntAssign then begin  //Assignment
      _set := sen.elements[0];  //Takes the one _set method.
      SplitSet(sen, _set)  //Might generate additional assignments sentences
    end else if sen.sntType = sntProcCal then begin  //Procedure call
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
          PrepareBody(cntBody, _blk);
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
          PrepareBody(cntBody, _blk);
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
    end;
  end;
end;
procedure TCompiler_PIC16.PrepareSentences;
var
  fun : TEleFun;
  bod: TEleBody;
begin
  //Split subroutines
  for fun in usedFuncs do begin
    if fun.callType = ctUsrNormal then begin
      PrepareBody(fun.BodyNode, fun.BodyNode);
      if HayError then exit;   //Puede haber error
    end;
  end;
  //Split body
  bod := TreeElems.BodyNode;  //lee Nodo del cuerpo principal
  PrepareBody(bod, bod);
end;
procedure TCompiler_PIC16.DoOptimize;
{Usa la información del árbol de sintaxis, para optimizar su estructura con
miras a la síntesis.
Se debe llamar después de llamar a DoAnalyzeProgram().}
begin
  if IsUnit then exit;
  ExprLevel := 0;

  ClearError;
  //Detecting unused elements
  RefreshAllElementLists; //Actualiza lista de elementos
  RemoveUnusedFunc;       //Se debe empezar con las funciones. 1ms aprox.
  RemoveUnusedVars;       //Luego las variables. 1ms aprox.
  RemoveUnusedCons;       //1ms aprox.
  RemoveUnusedTypes;      //1ms aprox.
  //Updating callers and calleds.
  UpdateFunLstCalled;     //Actualiza lista "lstCalled" de las funciones usadas.
  if HayError then exit;
  SeparateUsedFunctions;
  //Evaluate declared constants
  EvaluateConstantDeclare;
  if HayError then exit;
  //Simplify expressions
  PrepareSentences;
  {Do a first folding in nodes. Some constants (like those that depend on addresses)
  might not be evaluated. So it should be needed to do other Code folding again.}
  ConstantFolding;
  if HayError then exit;
  ConstanPropagation;
end;
procedure TCompiler_PIC16.DoGenerateCode;
{Generates the final binary code using information from the AST as input.
Must be called after DoOptimize().}
  procedure GenCodeMainBody(body: TEleBody);
  {Generates code for a Main Body element.}
  begin
    //It's the main program
    PutLabel('__main__');
    //Process body
    TreeElems.OpenElement(body); //Locate in the Body. Formally this won't be necessary if we are not going to solve identifiers.
    GenCodeSentences(TreeElems.curNode.elements);
    TreeElems.CloseElement;              //Close the Body.
    //Ending label
    PutLabel('__end__');
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
    TreeElems.curCodCont := body;  //Needed because TreeElems.OpenElement() doesn't do it.
    GenCodeSentences(TreeElems.curNode.elements);
    TreeElems.CloseElement;              //Close the Body.
    //Includes the final RTS
    if OptRetProc then begin  //Optimize
      //Verifica es que ya se ha incluido exit().
      if funcPar.firstObligExit<>nil then begin
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
  procedure GenBootloader(out add1, add2: word);
  {Generates the bootloader. Returns in "add1" and "add2" the start address and the end
  address of the bootloader;}
  var
    i: Integer;
  begin
    add1 := pic.iRam;
    if          bootloader = bldNone then begin
      //No bootloader
    end else if bootloader = bldJMP then begin
      pic.codByte(76, ruCodeOp);  //Opcode JMP
      pic.codByte(0, ruData, 'COD_HL');   //To complete later
      pic.codByte(0, ruData);             //To complete later
    end else if bootloader = bldC64 then begin
      //GenBootloaderC64;    //Commodore 64 bootloader.
      PutTopComm(';BASIC starter code: 10 SYS __main__');
      pic.codByte($0C, ruData);  //Dirección de siguiente línea
      pic.codByte($08, ruData);
      pic.codByte($0A, ruData);  //Número de línea
      pic.codByte($00, ruData);
      pic.codByte($9e, ruData);  //Token de instrucción SYS
      pic.codByte(0, ruData, 'COD_4A'); //To complete later
      pic.codByte(0, ruData);           //To complete later
      pic.codByte(0, ruData);           //To complete later
      pic.codByte(0, ruData);           //To complete later
      pic.codByte($00, ruData);  //Fin de instrucción
      pic.codByte($00, ruData);  //Sgte línea BASIC
      pic.codByte($00, ruData);  //Sgte línea BASIC
    end else if bootloader = bldCustom then begin
      PutTopComm(';Custom Bootloader.');
      for i:=0 to high(loaderBytes) do begin
        if loaderBytes[i]=-76 then begin
          pic.codByte(76, ruCodeOp);  //Opcode JMP
        end else if loaderBytes[i]=-1001 then begin  //2 bytes address for entry point.
          pic.codByte(0, ruData, 'COD_HL');  //To complete later
          pic.codByte(0, ruData);            //To complete later
        end else if loaderBytes[i]=-1002 then begin  //5 bytes ASCII address for entry point.
          pic.codByte(0, ruData, 'COD_5A');  //To complete later
          pic.codByte(0, ruData);            //To complete later
          pic.codByte(0, ruData);            //To complete later
          pic.codByte(0, ruData);            //To complete later
          pic.codByte(0, ruData);            //To complete later
        end else if loaderBytes[i]=-1003 then begin  //4 bytes ASCII address for entry point.
          pic.codByte(0, ruData, 'COD_4A');  //To complete later
          pic.codByte(0, ruData);            //To complete later
          pic.codByte(0, ruData);            //To complete later
          pic.codByte(0, ruData);            //To complete later
        end else begin  //Common byte
          pic.codByte(loaderBytes[i], true);
        end;
      end;
    end;
    add2 := pic.iRam;
  end;
  procedure CompleteBootloader(add1, add2: word; cod_entrypoint: word);
  {Complete the sections of the bootloader that need to be completed.
  "cod_entrypoint" is the address for the entry point of the compiled code.}
  var
    i: Word;
    tmp: string;
  begin
    for i:= add1 to add2 do begin
      if pic.ram[i].name = 'COD_HL' then begin
        pic.ram[i].value := lo(cod_entrypoint);
        pic.ram[i+1].value := hi(cod_entrypoint);
      end else if pic.ram[i].name = 'COD_5A' then begin
        tmp := RightStr('0000' + IntToStr(cod_entrypoint), 5);
        pic.ram[i  ].value := ord(tmp[1]);
        pic.ram[i+1].value := ord(tmp[2]);
        pic.ram[i+2].value := ord(tmp[3]);
        pic.ram[i+3].value := ord(tmp[4]);
        pic.ram[i+4].value := ord(tmp[5]);
      end else if pic.ram[i].name = 'COD_4A' then begin
        tmp := RightStr('000' + IntToStr(cod_entrypoint), 4);
        pic.ram[i  ].value := ord(tmp[1]);
        pic.ram[i+1].value := ord(tmp[2]);
        pic.ram[i+2].value := ord(tmp[3]);
        pic.ram[i+3].value := ord(tmp[4]);
      end;
    end;
  end;
var
  add , addr: word;
  add1, add2: word;
  fun    : TEleFun;
  i      : Integer;
  bod    : TEleBody;
  elem   : TxpElement;
begin
  if IsUnit then exit;
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
  //Create Bootloader
  addBootldr := pic.iRam;  //Save position.
  GenBootloader(add1, add2);
  //Asigna memoria a registros
  //Asigna memoria para las variables, buscando memoria libre a partir de "GeneralORG".
  addVariab  := pic.iRam;   //Save position.
  CreateVarsAndPars;  //Primero a las variables locales (y parámetros) de las funciones
  //Find the next free RAM location, to write functions.
  pic.freeStart := GeneralORG;  //Start of program block
  pic.dataAddr1   := -1; {Disable. It has been already used for allocatig variables, but
                          now we just want to find a free RAM location in the program block}
  pic.GetFreeByte(addr);
  pic.iRam := addr;
  addFuncts  := pic.iRam;  //Save position.
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
    case fun.callType of
    ctUsrNormal: begin  //Función normal de usuario
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
    ctSysNormal: begin  //Función normal del sistema.
      //Compile used function in the current address.
      fun.adrr := pic.iRam;    //Actualiza la dirección final
      fun.codSysNormal(fun);   //Rutina para generar código
      if HayError then exit;   //Puede haber error
      fun.coded := true;       //Marca como ya codficada en memoria.
      { TODO : ¿Hace falta completar llamadas? }
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
  CompleteBootloader(add1, add2, pic.iRam);  //Complete bootloader
  bod := TreeElems.BodyNode;  //lee Nodo del cuerpo principal
  if bod = nil then begin
    GenError('Body program not found.');
    exit;
  end;
  bod.adrr := pic.iRam;  //guarda la dirección de codificación
  GenCodeMainBody(bod);
  if HayError then exit;     //Puede haber error
  //Clean extra RAM firstly used and later not used by optimization.
  for add := pic.iRam to pic.iRam +3 do begin
    pic.ram[add].used := ruUnused;
  end;
end;
procedure TCompiler_PIC16.DoCompile;
{Compila el contenido del archivo "mainFile" con las opciones que se hayan definido en
esta instancia del compilador.
No debería usarse directamente, sino a través del método Exec(), para asegurarse de que
se inicializan correctamente las configuraiones.}
var
  p: SizeInt;
begin
  if comp_level = clNull then exit;
  debugln('');
  StartCountElapsed;  //Start timer
  DefCompiler;   //Debe hacerse solo una vez al inicio
  hexfile  := ChangeFileExt(mainFile, '.prg');     //Obtiene nombre
  hexfile  := hexFilePath;   //Expande nombre si es necesario
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
    if not OpenContextFrom(mainFile) then begin
      //No lo encuentra
      GenError(ER_FIL_NOFOUND, [mainFile]);
      exit;
    end;
    {-------------------------------------------------}
    TreeElems.Clear;
    //Asigna nombre y archivo a elemento
    TreeElems.main.name := ExtractFileName(mainFile);
    p := pos('.',TreeElems.main.name);
    if p <> 0 then TreeElems.main.name := copy(TreeElems.main.name, 1, p-1);
    TreeElems.main.srcDec := GetSrcPos;
    //Continúa con preparación
//    EndCountElapsed('** Setup in: ');
//    StartCountElapsed;  //Start timer
    CreateSystemElements;  //Crea los elementos del sistema. 3ms aprox.
    ClearMacros;           //Limpia las macros
    //Initiate CPU
    ExprLevel := 0;
    pic.dataAddr1 := -1;  //Reset flag
    pic.MsjError := '';
    //Compila el archivo actual como programa o como unidad
    pic.InitMemRAM;  //Init RAM and clear.
    pic.iRam := 0;  //Ubica puntero al inicio.
    IsUnit := GetUnitDeclaration();
    DoAnalyze;
    if HayError then exit;
    UpdateCallersToUnits;
    EndCountElapsed('-- Analyzed in: ');
    if comp_level >= clAnalOptim then begin  //Hay optimización
      if not IsUnit then begin
        {Compila solo los procedimientos usados, leyendo la información del árbol de sintaxis,
        que debe haber sido actualizado en la primera pasada.}
        StartCountElapsed;
        DoOptimize;
        if HayError then exit;
        EndCountElapsed('-- Optimized in: ');
      end;
    end;
    if comp_level >= clComplete then begin  //Hay síntesis
      if not IsUnit then begin
        StartCountElapsed;
        DoGenerateCode;
        //EndCountElapsed('-- Synthetized in: ');
        //StartCountElapsed;
        //Genera archivo hexa, en la misma ruta del programa
        pic.GenHex(hexFile, GeneralORG);
        EndCountElapsed('-- Output generated in: ');
      end;
    end;
    {-------------------------------------------------}
    //ClearAll;//es necesario por dejar limpio
  finally
    StartCountElapsed;
    ejecProg := false;
    //Tareas de finalización
    if OnAfterCompile<>nil then OnAfterCompile;
    EndCountElapsed('-- OnAfterCompile in: ');
  end;
end;
function AdrStr(absAdr: word): string;
{formatea una dirección en cadena.}
begin
  Result := '$' + IntToHex(AbsAdr, 4);
end;
procedure TCompiler_PIC16.DumpCode(lins: TSTrings);
{Genera el código ensamblador en el StringList "lins", con las configuraciones
actuales del compilador.
Se debe llamar despues de llamar a pic.GenHex(), para que se actualicen las variables
minUsed y maxUsed.}
const
  SPACEPAD = '          ';
  ASMPAD = '  ';
  LSPC = length(SPACEPAD);

  procedure VariablesLocation(lins: TStrings; ExcUnused: boolean);
  {Return a string with information about all variables location.}
  var
    v: TEleVarDec;
    subUsed: string;
  begin
    for v in TreeElems.AllVars do begin   //Se supone que "AllVars" ya se actualizó.
        //debugln('AllVars['+IntToStr(i)+']='+v.name+','+v.Parent.name);
        if ExcUnused and (v.nCalled = 0) then continue;
        if v.storage in [stRegister, stRegistX, stRegistY] then continue;
        if v.nCalled = 0 then subUsed := '; <Unused>' else subUsed := '';
        if v.typ.IsByteSize then begin
          lins.Add(PadRight(v.name, LSPC) + ' EQU ' + AdrStr(v.addr)+ subUsed +
                   '       ;' + v.typ.name);
        end else if v.typ.IsWordSize then begin
          lins.Add(PadRight(v.name, LSPC) + ' EQU ' +  AdrStr(v.addrL)+ subUsed +
                   '       ;' + v.typ.name);
          lins.Add(SPACEPAD + ' EQU ' +  AdrStr(v.addrH)+ subUsed);
        end else if v.typ.catType = tctArray then begin   //It's an array
          lins.Add(PadRight(v.name, LSPC) + ' EQU ' +  AdrStr(v.addrL) + '~' +
                   AdrStr(v.addr + v.typ.size-1) + subUsed + ' ;' + v.typ.name);
        end else begin
          lins.Add(PadRight(v.name, LSPC) + ' EQU ' +  AdrStr(v.addr) + subUsed +
                   '       ;' + v.typ.name);
        end;
    end;
  end;

var
  i: word;
  minUsed: integer;
  lblLin, comLin, lin: String;
  nBytes: byte;
begin
  if asmOutType=0 then begin  //Normal Assembler output
    //Include header
    lins.Add('      ;Code generated by P65Pas compiler');
    lins.Add('      processor ' + PICName);
    //Variables location section
    if IncVarDec then begin
       lins.Add('__all_variables:');
       VariablesLocation(lins, ExcUnused);
    end;
    //Se supone que minUsed y maxUsed, ya deben haber sido actualizados.
    if IncAddress then begin  //ORG title
      lins.Add(SPACEPAD + '      ORG $' + IntToHex(pic.minUsed, 4));
    end else begin
      lins.Add(SPACEPAD + 'ORG $' + IntToHex(pic.minUsed, 4));
    end;
    //Write the RAM content.
    i := pic.minUsed;
    while i <= pic.maxUsed do begin
      if (i=addBootldr) and (addBootldr<>addVariab) then lins.Add('__bootloader:');
      if (i=addVariab) and (addVariab<>addFuncts) then lins.Add('__var_section:');
      //Read label and comments.
      lblLin := pic.ram[i].name;
      comLin := pic.ram[i].topComment;
      //Check RAM position.
      if pic.ram[i].used in [ruData, ruAbsData] then begin
        //Must be a variable.
        if IncAddress then begin
          if comLin<>'' then lins.add(comLin);
          lins.Add( PadRight(lblLin, LSPC) + '$'+IntToHex(i,4) + ' DB ' +
                    IntToHEx(pic.ram[i].value,2) );
        end else begin
          lins.Add( PadRight(lblLin, LSPC) + 'DB ' + IntToHEx(pic.ram[i].value,2) );
        end;
        i := i + 1;
      end else begin
        //Debe ser código o memoria sin usar.
        if lblLin<>'' then lins.Add(lblLin+':');  //Etiqueta al inicio de línea
        //Escribe comentario al inicio de línea
        if asmIncComm and (comLin<>'') then  begin
          lins.Add(comLin);
        end;
        lin := pic.GetASMlineAt(i, IncAddress, IncAddress, asmIncComm, incVarName, nBytes);
        lins.Add(ASMPAD + lin);
        i := i + nBytes;   //Incrementa a siguiente instrucción
      end;
    end;
    lins.Add(';--------------------');
    lins.Add('      END');
  end else begin  //Generate POKE's BASIC code.
    minUsed := pic.CPUMAXRAM;
    i := pic.minUsed;
    while i <= pic.maxUsed do begin
      if pic.ram[i].used <> ruUnused then begin
        if i<minUsed then minUsed := i;  //Calcula mínimo
        lins.Add('poke ' +  IntToStr(i) + ',' + IntToStr(pic.ram[i].value));
      end;
      inc(i);
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
{Genera un reporte detallado de la compilación}
var
  curInst, opc: TP6502Inst;
  i: word;
  OpCodeCoun: array[low(TP6502Inst)..high(TP6502Inst)] of integer;
  tmpList: TStringList;
  txt, OpCode, Times, state: String;

  fun: TEleFun;
  caller : TxpEleCaller;
  called : TxpElement;
  //exitCall: TxpExitCall;
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

  lins.Add(';NAME                    USED   POSITION IN SOURCE');
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

      lins.Add('  Exit Instruction in obligatory code:');
      if fun.firstObligExit = nil then begin
        lins.Add('    <none>');
      end else begin
        //for exitCall in fun.lstExitCalls do begin
        //  lins.Add('    - Exit() in ' +exitCall.srcPos.RowColString);
        //end;
        lins.Add('    - Oblig. exit() in ' + fun.firstObligExit.srcDec.RowColString);
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
//Interfaz for IDE
procedure TCompiler_PIC16.Exec(srcFile, outFile: string; pars: string);
{Execute the compiler. Commonly it will compile "srcFile" getting the parameters fron the
string "pars". Pars must contain a parameter each line.
This must be the main entry point to the compiler.}
var
  parsList: TStringList;
  txt, tmp: string;
begin
  //Default settings for Command line Options
  mainFile := srcFile;
  comp_level  := clComplete;
  ForToRepeat := true;
  enabDirMsgs := true;
  OptReuProVar:= false;   //Optimiza reutilizando variables locales de procedimientos.
  OptRetProc  := false;   //Optimiza el último exit de los procedimientos.
  RemUnOpcod  := false;

  asmOutType  := 0;  //Normal Assembler
  asmIncComm  := false;
  IncVarDec   := false;
  IncVarName  := false;
  IncAddress  := false;
  //Default settings for Directive settings.
  syntaxMode  := modPicPas;   //Por defecto en sintaxis nueva
  cpuMode     := cpu6502;
  bootloader  := bldJMP;
  str_nullterm:= false;
  //Load parameters in a list
  parsList := TStringList.Create;
  parsList.Text := trim(pars);
//debugln('--Executing:('+ StringReplace(pars, LineEnding,' ',[rfReplaceAll])+')');
  //Extract and set parameters
  unitPaths.Clear;
  for txt in parsList do begin
    if length(txt)<2 then continue;
    if          copy(txt,1,2) = '-C' then begin  //---Compiling options
      case txt of
      //Compiler level
      '-Cn' : comp_level := clNull;
      '-Ca' : comp_level := clAnalys;
      '-Cao': comp_level := clAnalOptim;
      '-C'  : comp_level := clComplete;
      //Compiler settings
      '-Cf' : ForToRepeat := false;
      end;
    end else if copy(txt,1,2) = '-O' then begin  //---Optimization options
      case txt of
      '-Ov' : OptReuProVar := true;
      '-Or' : OptRetProc   := true;
      '-Ou' : RemUnOpcod   := true;
      end;
    end else if copy(txt,1,2) = '-A' then begin  //---Assembler options
      case txt of
      '-A0': asmOutType := 0;    //Output in normal Assembler.
      '-A1': asmOutType := 1;    //Output in BASIC POKE's loader.
      '-Ac': asmIncComm := true; //Include commnents in ASM output.
      '-Av': IncVarDec  := true; //Include variables information section.
      '-Au': ExcUnused  := true; //Exclude unused variables in variable section.
      '-An': incVarName := true; //Include nombres de variables en las instrucciones.
      '-Aa': IncAddress := true; //Include memory address in instructions.
      end;
    end else if copy(txt,1,2) = '-F' then begin  //File names and paths
      if copy(txt,1,3) = '-Fu' then begin  //Add unit path
        tmp := copy(txt,4,length(txt));
        if tmp='' then continue;
        if tmp[1]='"' then delete(tmp,1,1);
        if tmp[length(tmp)]='"' then delete(tmp,length(tmp),1);
        unitPaths.Add(tmp);
      end;
    end else if txt = '-Dn' then begin  //Disable directive messages
      enabDirMsgs := false;
    end else begin         //Other.

    end;
  end;
  //Compile
  DoCompile;
  //Destroy list
  parsList.Destroy;
end;
constructor TCompiler_PIC16.Create;
begin
  inherited Create;
  //OnNewLine:=@cInNewLine;
  syntaxMode := modPicPas;   //Por defecto en sintaxis nueva
end;
destructor TCompiler_PIC16.Destroy;
begin
  inherited Destroy;
end;

end.

