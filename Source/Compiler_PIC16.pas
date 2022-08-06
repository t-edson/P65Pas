{Unidad con rutinas del analizador sintáctico.
}
unit Compiler_PIC16;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LazLogger,
  P6502utils, CPUCore, CompBase, ParserDirec, GenCodBas_PIC16,
  GenCod_PIC16, ParserDirec_PIC16, CompGlobals, XpresElemP65, ParserASM_6502;
type
  { TCompiler_PIC16 }
  TCompiler_PIC16 = class(TParserDirec)
  private  //Funciones básicas
    procedure ConstantFoldExpr(eleExp: TEleExpress);
    procedure SplitExpresBody(body: TxpEleCodeCont);
    procedure SplitExpressions;
  private  //Compilers steps
    procedure EvaluateConstantDeclare;
    procedure ConstantFolding;
    procedure ConstanPropagation;
    procedure DoOptimize;
    procedure DoGenerateCode;
    procedure Compile(srcFile: string);
  public      //Events
    OnAfterCompile: procedure of object;   //Al finalizar la compilación.
  public      //Override methods
    procedure RAMusage(lins: TStrings; ExcUnused: boolean); override; //uso de memoria RAM
    procedure DumpCode(lins: TSTrings; AsmMode, IncVarDec, ExcUnused: boolean;
      incAdrr, incCom, incVarNam: boolean); override; //uso de la memoria Flash
    function RAMusedStr: string; override;
    procedure GetResourcesUsed(out ramUse, romUse, stkUse: single); override;
    procedure GenerateListReport(lins: TStrings); override;
  public      //Interfaz for IDE
    procedure Exec(srcFile, outFile: string; pars: string);
    procedure PrintHelp;
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
  ParserDirec_PIC16.SetLanguage;
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
        { TODO : Tal vez sea posible que por optimización, solo llamar a funcBase.codSysInline() cuando los parámetros (o alguno) sean constante, para evitar muchas llamadas }
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
    if eleExp.evaluated then exit;  //No problem it's evaluated.
    if eleExp.cons = nil then begin
      //We cannot evaluate it.
      GenError('Constant not evaluated.', eleExp.srcDec);
      exit;
    end else begin
      //Could be evaluated using "cons"
      if eleExp.cons.evaluated then begin
        eleExp.value := eleExp.cons.value^;
        eleExp.evaluated := true;
      end else begin
        GenError('Constant not evaluated.', eleExp.srcDec);
        exit;
      end;
    end;
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
var
  SCREEN    : array[0..1000] of byte absolute $0400;
  BITMAP    : array [0..1] of byte   absolute $2000;  //Size doesn't matter
var
  location: ^byte;

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
  begin
    //Process body
    TreeElems.OpenElement(body);
    for eleSen in TreeElems.curNode.elements do begin
      if eleSen.idClass <> eleSenten then continue;
      sen := TEleSentence(eleSen);
      //if sen.sntType = sntExpres then begin  //Call to function or method (including assignment)
      if sen.sntType = sntAssign then begin    //assignment)
        expFun := TEleExpress(sen.elements[0]);  //Takes root node.
        ConstantFoldExpr(expFun);
        if HayError then exit;
      end;
    end;
    TreeElems.CloseElement;              //Close the Body.
  end;
var
  fun : TEleFun;
  bod: TEleBody;
begin
  location := @BITMAP;

  compMod := cmConsEval;    //Generates code.
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
      if sen.sntType = sntAssign then begin    //assignment)
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
        {Fromally we can delete an assigment of the form: <variable> := <constant>
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
procedure TCompiler_PIC16.SplitExpresBody(body: TxpEleCodeCont);
{Do a separation for assigmente sentences in order to have the "three-address code" form
like used in several compilers.}
  function MoveNodeToAssign(curContainer: TxpElement; Op: TEleExpress): TEleExpress;
  {Mueve el nodo especificado "Op" a una nueva instruccion de asignación (que es creada
  al inicio del bloque "curContainer") y reemplaza el nodo faltante por una variable
  temporal que es la que se crea en la instrucción de asignación.
  Retorna la instrucción de asignación creada.
  }
  var
    _varaux: TEleVarDec;
    _setaux: TEleExpress;
    Op1aux, Op2aux: TEleExpress;
    funSet: TEleFunBase;
    OpPos: Integer;
    OpParent: TxpElement;
  begin
    //Create a new variable in the declaration section of this body.
    _varaux := CreateEleVarDec('_x' + IntToStr(body.Index), Op.typ);  //Generate a unique name in this body
    TreeElems.openElement(body.Parent);
    TreeElems.AddElement(_varaux, body.Index);  //Add before the body.

    //Create the new _set() expression.
    _setaux := CreateExpression('_set', typNull, otFunct, Op.srcDec);
    funSet := MethodFromBinOperator(Op.Typ, ':=', Op.Typ);
    if funSet = nil then begin   //Operator not found
      GenError('Undefined operation: %s %s %s', [Op.Typ.name, ':=', Op.Typ.name]);
      exit(nil);
    end;
    _setaux.rfun := funSet;

    //Add the new assigment before the main
    TreeElems.openElement(curContainer);
    TreeElems.AddElement(_setaux, 0);    //Add a new assigmente before
    _setaux.elements := TxpElements.Create(true);  //Create list
    TreeElems.openElement(_setaux);

    //Add first operand (variable) of the assignment.
    Op1aux := CreateExpression(_varaux.name, _varaux.typ, otVariab, Op.srcDec);
    Op1aux.SetVariab(_varaux);
    TreeElems.addElement(Op1aux);
    AddCallerToFromCurr(_varaux); //Add reference to auxiliar variable.

    //Move the second operand to the previous _set created
    OpParent := Op.Parent;    //Keep current parent reference.
    OpPos := Op.Index;        //Keep current operand position.
    TreeElems.ChangeParentTo(_setaux, Op);
    //Replace the missed function
    TreeElems.openElement(OpParent);
    Op2aux := CreateExpression(_varaux.name, _varaux.typ, otVariab, Op.srcDec);
    Op2aux.SetVariab(_varaux);
    TreeElems.addElement(Op2aux, OpPos);
    exit(_setaux);
  end;
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
      GenError('Undefined operation: %s %s %s', [Op.Typ.name, ':=', Op.Typ.name]);
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
    Op2, parExp, new_set: TEleExpress;
    par: TxpElement;
  begin
    Result := false;
    Op2 := TEleExpress(setMethod.elements[1]);  //Takes assigment source.
    if (Op2.opType = otFunct) then begin
      if Op2.rfun.codSysInline = nil then begin  //Normal function
        {IT's the form:
             x := func(x,y);
                  \_______/
                     Op2
        }
        //¿Conviene mover nodo OP2 a asignación previa?
        SplitProcCall(curContainer, Op2);
      end else begin          //INLINE function
        {IT's the form:
             x := A + B
                  \___/
                   Op2
        or:
             x := A++        }
        {We expect parameters A, B should be simple operands (Constant or variables)
        otherwise we will move them to a separate assigment}
        for par in Op2.elements do begin
          parExp := TEleExpress(par);
          if parExp.opType = otFunct then begin
            new_set := MoveNodeToAssign(curContainer, parExp);
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
    if (expMethod.opType = otFunct) and expMethod.fcallOp then begin
      {We expect parameters should be simple operands (Constant or variables)
      otherwise we will move them to a separate assigment}
      for par in expMethod.elements do begin
        parExp := TEleExpress(par);
        if parExp.opType = otFunct then begin
          new_set := MoveNodeToAssign(curContainer, parExp);
          if HayError then exit;
          SplitSet(curContainer, new_set);  //Check if it's needed split the new _set() created.
          Result := true;
        end;
      end;
    end;
  end;
  function SplitProcCall(curContainer: TxpElement; expMethod: TEleExpress): boolean;
  {Split a procedure (not INLINE) call instruction, inserting an assigment instruction
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
      {Move all parameters (children nodes) to a separate assigment}
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
  _exp: TEleExpress;
  _blk: TxpEleCodeCont;
begin
  for eleSen in body.elements do begin
    if eleSen.idClass <> eleSenten then continue;
    //We have a sentence here.
    sen := TEleSentence(eleSen);
    if sen.sntType = sntAssign then begin  //Assignment
      _set := sen.elements[0];  //Takes the one _set method.
      SplitSet(sen, _set)  //Might generate additional assigments sentences
    end else if sen.sntType = sntProcCal then begin  //Procedure call
      _proc := sen.elements[0];  //Takes the proc.
      SplitProcCall(sen, TEleExpress(_proc))
    end else if sen.sntType in [sntIF, sntREPEAT, sntFOR, sntWHILE] then begin
      //There are expressions and blocks inside conditions and loops.
      for ele in sen.elements do begin
        if ele.idClass = eleCondit then begin  //It's a condition
          _exp := TEleExpress(ele.elements[0]);  //The first item is a TEleExpress
          SplitExpress(ele, _exp)
        end else if ele.idClass = eleBlock then begin   //Body of IF
          _blk := TxpEleCodeCont(ele);  //The first item is a TEleExpress
          SplitExpresBody(_blk);
        end;
      end;
    end else if sen.sntType = sntExit then begin
      _exp := TEleExpress(sen.elements[0]);  //The first item is a TEleExpress
      SplitExpress(sen, _exp)
    end;
  end;
end;
procedure TCompiler_PIC16.SplitExpressions;
var
  fun : TEleFun;
  bod: TEleBody;
begin
  //Split subroutines
  for fun in usedFuncs do begin
    if fun.callType = ctUsrNormal then begin
      SplitExpresBody(fun.BodyNode);
      if HayError then exit;   //Puede haber error
    end;
  end;
  //Split body
  bod := TreeElems.BodyNode;  //lee Nodo del cuerpo principal
  SplitExpresBody(bod);
end;
procedure TCompiler_PIC16.DoOptimize;
{Usa la información del árbol de sintaxis, para optimizar su estructura con
miras a la síntesis.
Se debe llamar después de llamar a DoAnalyzeProgram().}
begin
  if IsUnit then exit;
  ExprLevel := 0;
  ResetRAM;    //2ms aprox.
  ClearError;
  pic.MsjError := '';
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
  SplitExpressions;
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
var
  add, addr: word;
  fun    : TEleFun;
  i      : Integer;
  bod    : TEleBody;
  iniMain: integer;
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
    ctSysNormal: begin  //Función normal del systema.
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
  _LABEL_post(iniMain);   //Termina de codificar el salto
  bod := TreeElems.BodyNode;  //lee Nodo del cuerpo principal
  if bod = nil then begin
    GenError('Body program not found.');
    exit;
  end;
  bod.adrr := pic.iRam;  //guarda la dirección de codificación
  GenCodeMainBody(bod);
  //if HayError then exit;     //Puede haber error
  {No es necesario hacer más validaciones, porque ya se hicieron en la primera pasada}
  //_RTS();   //agrega instrucción final
end;
procedure TCompiler_PIC16.Compile(srcFile: string);
//Compila el contenido de un archivo.
var
  p: SizeInt;
begin
  if comp_level = clNull then exit;
  debugln('');
  StartCountElapsed;  //Start timer
  DefCompiler;   //Debe hacerse solo una vez al inicio
  mode := modPicPas;   //Por defecto en sintaxis nueva
  mainFile := srcFile;
  hexfile := ChangeFileExt(srcFile, '.prg');     //Obtiene nombre
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
    if not OpenContextFrom(srcFile) then begin
      //No lo encuentra
      GenError(ER_FIL_NOFOUND, [srcFile]);
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
    //Inicia PIC
    ExprLevel := 0;  //inicia
    pic.dataAddr1 := -1;  {Reset flag}
    //Compila el archivo actual como programa o como unidad
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
  //Load parameters in a list
  parsList := TStringList.Create;
  parsList.Text := trim(pars);
//debugln('--Executing:('+ StringReplace(pars, LineEnding,' ',[rfReplaceAll])+')');
  //Extract and set parameters
  //Default settings
  comp_level  := clComplete;
  enabDirMsgs := true;
  AsmIncComm  := false;
  unitPaths.Clear;
  //Read parameters
  for txt in parsList do begin
    if length(txt)<2 then continue;
    //---Compiling options
    if copy(txt,1,2) = '-C' then begin
      case txt of
        '-Cn' : comp_level := clNull;
        '-Ca' : comp_level := clAnalys;
        '-Cao': comp_level := clAnalOptim;
        '-C'  : comp_level := clComplete;
      end;
    //---Optimization options
    end else if copy(txt,1,2) = '-O' then begin
      case txt of
        '-On' : comp_level := clNull;
        '-Oa' : comp_level := clAnalys;
        '-Oao': comp_level := clAnalOptim;
        '-O'  : comp_level := clComplete;
      end;
    //---Other options
    end else if copy(txt,1,2) = '-F' then begin  //File names and paths
      if copy(txt,1,3) = '-Fu' then begin  //Add unit path
        tmp := copy(txt,4,length(txt));
        if tmp='' then continue;
        if tmp[1]='"' then delete(tmp,1,1);
        if tmp[length(tmp)]='"' then delete(tmp,length(tmp),1);
        unitPaths.Add(tmp);
      end;
    end else if txt = '-Ac' then begin  //Include commnents in ASM output
      AsmIncComm := true;
    end else if txt = '-Dn' then begin  //Disable directive messages
      enabDirMsgs := false;
    end else begin         //Other.

    end;
  end;
  //Compile
  Compile(srcFile);
  //Destroy list
  parsList.Destroy;
end;
procedure TCompiler_PIC16.PrintHelp;
{Muestra las opciones de línea de comando que soporta este compilador}
begin

end;
constructor TCompiler_PIC16.Create;
begin
  inherited Create;
  //OnNewLine:=@cInNewLine;
  mode := modPicPas;   //Por defecto en sintaxis nueva
end;
destructor TCompiler_PIC16.Destroy;
begin
  inherited Destroy;
end;

end.

