{Unit for parse ASM blocks of P65Pas compiler.
Functionality of parser is defined in the class TParserAsm_6502.
The input for this unit is the ASM code accessed trought the lexer of the compiler.
The output of the lexer is a new node element created in the syntax tree.
By Tito Hinostroza 03/09/2020.
}
unit ParserASM_6502;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, LexPas, CompBase, P65C02utils, CompGlobals, XpresElemP65;
type
  { TParserAsm_6502 }
  TParserAsm_6502 = class
  private
    cpx     : TCompilerBase;   //Reference to compiler
    labels  : TEleAsmInstrs;   //Lista de etiquetas
    curBlock: TEleAsmBlock;    //Bloque ASM actual.
    curInst : TEleAsmInstr;    //Instruction ASM actual.
    procedure AddDirectiveDB;
    procedure AddDirectiveDW;
    procedure AddInstructionLabel(lblName: string);
    function CaptureOperand(var operand: TAsmOperand; out undefLabel: boolean
      ): boolean;
    function CaptureParenthes: boolean;
    procedure EndASM;
    function GetFaddressByte(addr: integer): byte;
    function IsLabelDeclared(txt: string; out lblEle: TEleAsmInstr): boolean;
    procedure ProcASMline(out blkEnd: boolean);
    procedure ProcInstrASM(idInst: TP6502Inst; var blkEnd: boolean);
    procedure StartASM;
  private
    procedure AddInstruction(const inst: TP6502Inst; addMode: TP6502AddMode;
      param: integer; srcDec: TSrcPos);
    procedure AddDirectiveORG(param: word);
  public //Inicialización
    procedure ProcessASMblock(cpx0: TCompilerBase);
    function DecodeNext: boolean;
    constructor Create;
    destructor Destroy; override;
  end;
var
  vParserASM_6502 : TParserAsm_6502;

procedure SetLanguage;

implementation

var  //Mensajes
  ER_EXPEC_COMMA, ER_EXPEC_PAREN, ER_EXP_ADR_VAR, ER_EXP_CON_VAL, ER_NOGETADD_VAR,
  ER_NOGETVAL_CON,  ER_INV_ASMCODE: String;
  ER_EXPECT_W_F, ER_SYNTAX_ERR_, ER_DUPLIC_LBL_, ER_EXPE_NUMBIT: String;
  ER_EXPECT_ADDR, ER_EXPECT_BYTE, WA_ADDR_TRUNC, ER_UNDEF_LABEL_: String;

procedure SetLanguage;
begin
  {$I _language\tra_ParserAsm.pas}
end;

{ TParserAsm_6502 }
function TParserAsm_6502.GetFaddressByte(addr: integer): byte;
{Obtiene una dirección de registro para una isntrucción ASM, truncando, si es necesario,
los bits adicionales.}
begin
  if addr>255 then begin
    addr := addr and $7F;
    //Indica con advertencia
    cpx.GenWarn(WA_ADDR_TRUNC);
  end;
  Result := addr;
end;
function TParserAsm_6502.IsLabelDeclared(txt: string; out lblEle: TEleAsmInstr): boolean;
{Indica si un nombre es una etiqueta. Si lo es, devuelve TRUE, y devuelve en lblEle, la
referencia a la instrucción de la etiqueta.}
var
  lbl: TEleAsmInstr;
begin
  //No se espera procesar muchas etiquetas
  for lbl in labels do begin  { TODO : ¿No se podría prescindir de "labels2 y usar solamente la lista de todas las instrucciones? }
    if lbl.uname = upcase(txt) then begin
      lblEle := lbl;
      exit(true);
    end;
  end;
  //No encontró
  exit(false);
end;
function TParserAsm_6502.CaptureParenthes: boolean;
{Captura el paréntesis ')'. Si no encuentra devuelve error}
begin
  cpx.SkipWhitesNoEOL;
  if cpx.token = ')' then begin
    cpx.Next;   //toma la coma
    exit(true);
  end else begin
    cpx.GenError(ER_EXPEC_PAREN);
    exit(false);
  end;
end;
function TParserAsm_6502.CaptureOperand(var operand: TAsmOperand;
                                        out undefLabel: boolean): boolean;
{Capture the operand (value, label or address) of an ASM instruction, including
operations if exist.
 Operands can have one of the following formats:
   [ > | < ] "$" [.HIGH | .LOW | @0 | @1 | @2 | @3 | <+|-><numeric literal>]
   [ > | < ] <numeric literal>
   [ > | < ] <identifier> [.HIGH | .LOW | @0 | @1 | @2 | @3 | <+|-><numeric literal>]
If not operand eas found error is generated and returns FALSE.}
  function ScanOperation(out operation: TAsmInstOperation; out value: word): boolean;
  {Look for one operations, in the current context. Operatiosn valids are:
        .HIGH
        .LOW
        @0
        @1
        @2
        @3
        + <VALUE>
        - <VALUE>.
  If one operations is found:
     * Retunns operation and value in parameters.
     * Returns TRUE in the function.
  If not operations are found, returns FALSE.}
  var
    valueInt: Longint;
  begin
    cpx.SkipWhitesNoEOL;
    if (cpx.tokType = tkEol) or (cpx.token = ';') then begin
      //End of line
      exit(false);
    end;
    if cpx.token = '.' then begin
      //Hay precisión de campo
      cpx.Next;
      if UpCase(cpx.token) = 'LOW' then begin
        operation := aopSelByte;
        value := 0;
        cpx.Next;
        exit(true);
      end else if UpCase(cpx.token) = 'HIGH' then begin
        operation := aopSelByte;
        value := 1;
        cpx.Next;
        exit(true);
      end else begin
        cpx.GenError('Field expected after "."');
        exit(false);
      end;
    end else if cpx.token = '@' then begin
      cpx.Next;
      if cpx.token = '0' then begin
        operation := aopSelByte;
        value := 0;
        cpx.Next;
        exit(true);
      end else if cpx.token = '1' then begin
        operation := aopSelByte;
        value := 1;
        cpx.Next;
        exit(true);
      end else if cpx.token = '2' then begin
        operation := aopSelByte;
        value := 2;
        cpx.Next;
        exit(true);
      end else if cpx.token = '3' then begin
        operation := aopSelByte;
        value := 3;
        cpx.Next;
        exit(true);
      end else begin
        cpx.GenError('Field expected after "@"');
        exit(false);
      end;
    end else if (cpx.token = '+') or (cpx.token = '-') then begin
      if cpx.token='+' then operation := aopAddValue else operation := aopSubValue;
      //Get operand
      cpx.Next;
      cpx.SkipWhitesNoEOL;
      if (cpx.tokType = tkEol) or (cpx.token = ';') then begin
        //End of line
        cpx.GenError('Operand expected');
        exit(false);
      end else begin
        //Follows something
        if not TryStrToInt(cpx.token, valueInt) then begin
          cpx.GenError('Numeric operand expected');
          exit(false);
        end;
        cpx.Next;
        value := word(valueInt);
        exit(true);
      end;
    end else begin
      //Other token
      exit(false);
    end;
  end;
  procedure ScanOperations(firstOperation: char);
  {Scan in the current line for ASM operations. If operations are found, they will be
  added as nodes in the current node of the AST.
  "firstOperation" allows to indicate if a position operator, like '>' or '<' has been
  found before de parameter.}
  var
    operation: TAsmInstOperation;
    value: word;
  begin
    if firstOperation='>' then begin
      //There is an operation
      operand.AddOperation(aopSelByte, 1);
    end else if firstOperation='<' then begin
      //There is an operation
      operand.AddOperation(aopSelByte, 0);
    end;
    while ScanOperation(operation, value) do begin
      //There is an operation
      operand.AddOperation(operation, value);
    end;
  end;
  function TestForPositionOperand: char;
  {Test if a position operand ('>' or '<') exist. If so return the operator,
  otherwise returns ' '.}
  begin
    if cpx.token = '>' then begin
      cpx.Next;
      exit('>');
    end else if cpx.token = '<' then begin
      cpx.Next;
      exit('<');
    end else begin
      //Other
      exit(' ');
    end;
  end;
var
  ele: TxpElement;
  xfun: TEleFun;
  xvar: TEleVarDec;
  xcon: TEleConsDec;
  positOper: char;
  lblEle: TEleAsmInstr;
begin
  Result := false;
  operand.used := false;
  undefLabel := false;
  cpx.SkipWhitesNoEOL;
  positOper := TestForPositionOperand();  //Check for ">" or "<"
  if cpx.token = '$' then begin
    //Es una dirección relativa
    cpx.Next;
    cpx.SkipWhitesNoEOL;
    //Creates node "Operand".
    operand.Val := -2;  //To indicates it's $
    //Check for operations
    ScanOperations(positOper);
    if cpx.HayError then exit(false);
    operand.used := true;
    exit(true);
  end else if cpx.tokType = tkLitNumber then begin
    //Es una dirección numérica
    operand.Val := StrToInt(cpx.token);  //Simple number
    cpx.Next;
    operand.used := true;
    exit(true);
  end else if cpx.tokType = tkIdentifier  then begin
    if IsLabelDeclared(cpx.token, lblEle) then begin
      //Es un identificador de etiqueta
      operand.Val := -1;      //Indicates to use "operRef"
      operand.Ref := lblEle;  //Referencia a la etiqueta.
      //cpx.AddCallerToFromCurr(lblEle);  //Agrega referencia
      cpx.Next;
      //Check for operations
      ScanOperations(positOper);
      if cpx.HayError then exit(false);
      operand.used := true;
      exit(true);
    end;
    ele := cpx.TreeElems.FindFirst(cpx.token);  //identifica elemento
    if ele=nil then begin
      //Es un identificador no definido (como una etiqueta). Puede definirse luego.
      operand.Val := -1;        //Indicates to use "operRef"
      operand.Ref := nil;        //Will be later linked.
      operand.Nam := UpCase(cpx.token);  //Keep name to find reference.
      //Hay salto indefinido.
      undefLabel := true;
      cpx.Next;
      //Check for operations
      ScanOperations(positOper);
      if cpx.HayError then exit(false);
      operand.used := true;
      exit(true);
    end else begin
      //Se identifica un elemento del lenguaje
      if ele.idClass = eleFunc then begin
        //Es un identificador de función del árbol de sintaxis
        xfun := TEleFun(ele);
        cpx.AddCallerToFromCurr(xfun);  //lleva la cuenta
        cpx.Next;  //Take variable name
        operand.Val := -1;        //Indicates to use "operRef"
        operand.Ref := xfun;
        //Check for operations
        ScanOperations(positOper);
        if cpx.HayError then exit(false);
        operand.used := true;
        exit(true);
      end else if ele.idClass = eleVarDec then begin
        //It's variable identifier
        xvar := TEleVarDec(ele);
        cpx.AddCallerToFromCurr(xvar);  //lleva la cuenta
        cpx.Next;  //Take variable name
        operand.Val := -1;        //Indicates to use "operRef"
        operand.Ref := xvar;
        //Check for operations
        ScanOperations(positOper);
        if cpx.HayError then exit(false);
        operand.used := true;
        exit(true);
      end else if ele.idClass = eleConsDec then begin
        //Es identificador de constante
        xcon := TEleConsDec(ele);
        cpx.AddCallerToFromCurr(xcon);  //lleva la cuenta
        //Constants can be resolved as numbers
        if (xcon.typ = cpx.typByte) or (xcon.typ = cpx.typWord) then begin
          cpx.Next;
          operand.Val := -1;        //Indicates to use "operRef"
          operand.Ref := xcon;
          //Check for operations
          ScanOperations(positOper);
          if cpx.HayError then exit(false);
        end else begin
          //Otro tipo de constante
          cpx.GenError(ER_NOGETVAL_CON);
          exit(false);
        end;
        operand.used := true;
        exit(true);
      end else begin
        //No se puede leer dirección
        cpx.GenError(ER_EXP_CON_VAL);
        exit(false);
      end;
    end;
  end else begin
    cpx.GenError(ER_EXP_CON_VAL);
    exit(false);
  end;
end;
procedure TParserAsm_6502.StartASM; //Inicia el procesamiento de código ASM
begin
  labels.Clear;   //limpia etiquetas
end;
procedure TParserAsm_6502.EndASM;  //Termina el procesamiento de código ASM
  function CompleteUndefJump(unsInstruct : TEleAsmInstr): boolean;
  {Completa la instrucción "unsInstruct", buscando en la lista de etiquetas.
  Si no encuentra la etiqueta, devuelve FALSE.}
  var
    lblInstr: TEleAsmInstr;
  begin
    for lblInstr in labels do begin  //Ve si la etiqueta existe
      if lblInstr.uname = unsInstruct.operand.Nam then begin
        //Sí existe la etiqueta.
        unsInstruct.operand.Ref := lblInstr;  //Actualiza la referencia a la etiqueta.
        //cpx.AddCallerToFromCurr(lblInstr);  //Agrega referencia
        exit(true);  //Encontrado y actualizado.
      end;
    end;
    exit(false);  //No se encontró.
  end;
var
  jmpInst : TEleAsmInstr;
begin
  //Completa los saltos a etiquetas no definidas.
  //Al final de esta iteración todas las instruciones que incluyan operandos con
  //saltos a etiquetas indefinidas, estarán referenciando a la etiqueta correspondiente
  //en lugar de solo guardar el nombre de la etiqueta.
  for jmpInst in curBlock.undefInstrucs do begin
    if not CompleteUndefJump(jmpInst) then begin
      //No se enuentra "jmpInst" en "labels".
      cpx.GenError(ER_UNDEF_LABEL_, [jmpInst.name], jmpInst.srcDec);
    end;
  end;
end;
procedure TParserAsm_6502.ProcInstrASM(idInst: TP6502Inst; var blkEnd: boolean);
{Proccess an 6502 ASM instruction. Instruction must be previously validated and
 identified in "idInst".
 Basically this procedure, add a new TEleAsmInstr (including instruction, addresing
 mode and operamd) to the current TEleAsmBlock, that represents a 6502 instruction.
 An instruction ends with the EOL token or the ASM delimiter "END".
 This procedure must not process the EOL token or the "END" delimiter.
 If the the "END" delimiter is found, the flag "blkEnd" is activated.
}
var
  tok: String;
  addressModes: TP6502AddModes;
  srcInst: TSrcPos;
  undefLabel: boolean;
begin
  blkEnd := false;
  addressModes := PIC16InstName[idInst].addressModes;
   srcInst := cpx.GetSrcPos;
  //Capture operand
  cpx.Next;
  cpx.SkipWhitesNoEOL;
  tok := cpx.token;
  if (cpx.tokType = tkEol) then begin
    //Sin parámetros. Puede ser Implícito o Acumulador
    if aImplicit in addressModes then begin
      //Tiene modo implícito
      AddInstruction(idInst, aImplicit, 0, srcInst);
    end else if aAcumulat in addressModes then begin
      //Tiene modo implícito
      AddInstruction(idInst, aAcumulat, 0, srcInst);
    end else begin
      //An operand must follow.
      cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
      exit;
    end;
  end else if (cpx.tokType=tkIdentifier) and (Upcase(cpx.token)='END') then begin
    //Sin parámetros. Puede ser Implícito o Acumulador
    if aImplicit in addressModes then begin
      //Tiene modo implícito
      AddInstruction(idInst, aImplicit, 0, srcInst);
    end else if aAcumulat in addressModes then begin
      //Tiene modo implícito
      AddInstruction(idInst, aAcumulat, 0, srcInst);
    end else begin
      //An operand must follow.
      cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
      exit;
    end;
    blkEnd := true;
  end else if tok = '#' then begin
    //Direccionamiento Inmediato
    cpx.Next;      //Toma "#"
    AddInstruction(idInst, aImmediat, 0, srcInst);
    //Complete the "param" of "curInst".
    if not CaptureOperand(curInst.operand, undefLabel) then begin
      cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
      exit;
    end;
    if undefLabel then curBlock.undefInstrucs.Add(curInst);  //Instruction with unsolved label
    cpx.SkipWhitesNoEOL;
  end else if tok = '(' then begin
    //Direccionamiento Indirecto: (indirect), (indirect,X), (indirect),Y o (aAbsInIdX, X)
    AddInstruction(idInst, aIndirect, 0, srcInst);  //Add the instruction with "aImplicit" temporally. Later will be updated.
    cpx.Next;
    if cpx.tokType in [tkLitNumber, tkIdentifier] then begin
      if not CaptureOperand(curInst.operand, undefLabel) then begin
        cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
        exit;
      end;
      if undefLabel then curBlock.undefInstrucs.Add(curInst);  //Instruction with unsolved label
      cpx.SkipWhitesNoEOL;
      if cpx.token = ',' then begin
        //Can only be (indirect,X)
        cpx.Next;  //Take number
        cpx.SkipWhitesNoEOL;
        if UpCase(cpx.token) <> 'X' then begin
          cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
          exit;
        end;
        cpx.Next;  //Take X
        cpx.SkipWhitesNoEOL;
        //Only could be aIndirecX or aAbsInIdX
        if aAbsInIdX in addressModes then begin  //Only JMP have this mode and don't have aIndirecX
          curInst.addMode := ord(aAbsInIdX);
        end else begin  //The only option
          curInst.addMode := ord(aIndirecX);
        end;
        //Verify ')'
        if not CaptureParenthes then begin
          cpx.GenError(ER_EXPEC_PAREN);
          exit;
        end;
      end else if cpx.token = ')' then begin
        //(indirect) or (indirect),Y
        cpx.Next;
        cpx.SkipWhitesNoEOL;
        if cpx.token = ',' then begin
          //Can only be (indirect),Y
          curInst.addMode := ord(aIndirecY);
          cpx.Next;  //Toma número
          cpx.SkipWhitesNoEOL;
          if UpCase(cpx.token) <> 'Y' then begin
            cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
            exit;
          end;
          cpx.Next;  //Takes Y
          cpx.SkipWhitesNoEOL;
        end else if cpx.tokType = tkEol then begin
          //Can only be (indirect)
          //No need to change anything.
        end else begin
          cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
          exit;
        end;
      end else begin
        cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
        exit;
      end;
    end else begin
      cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
      exit;
    end;
  end else begin
    //Puede ser absoluto o página cero, o sus versiones indexadas con X o Y.
    AddInstruction(idInst, aImplicit, 0, srcInst);  //Add the instruction with "aImplicit" temporally. Later will be updated.
    //Complete the "param" of "curInst".
    if not CaptureOperand(curInst.operand, undefLabel) then begin
      cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
      exit;
    end;
    if undefLabel then curBlock.undefInstrucs.Add(curInst);  //Instruction with unsolved label
    {Get the addressing mode, considering operand is 16bits. If it's 8 bits, the
     addressing mode should be changed when code is generated.}
    cpx.SkipWhitesNoEOL;
    //Verify is follows ,X o ,Y
    if cpx.token = ',' then begin
      cpx.Next;
      cpx.SkipWhitesNoEOL;
      if Upcase(cpx.token) = 'X' then begin
        cpx.Next;
        cpx.SkipWhitesNoEOL;
        curInst.addMode := ord(aAbsolutX);
      end else if Upcase(cpx.token) = 'Y' then begin
        cpx.Next;
        cpx.SkipWhitesNoEOL;
        curInst.addMode := ord(aAbsolutY);
      end else begin
        //Could be the 65c02 instruction BBR0 $12, <label>
        if not CaptureOperand(curInst.operand, undefLabel) then begin
          cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
          exit;
        end;
        if undefLabel then curBlock.undefInstrucs.Add(curInst);  //Instruction with unsolved label
        //We have an operand.
        curInst.addMode := ord(aZeroPRel);
      end;
    end else begin
      if addressModes = [aRelative] then begin
        //Only accept "aRelative" address. Like BEQ, BNE, ...
        curInst.addMode := ord(aRelative);
      end else if addressModes = [aImplicit] then begin
        //Only accept "aImplicit" address. Like CLC, CLD, ...
        curInst.addMode := ord(aImplicit);
      end else begin
        curInst.addMode := ord(aAbsolute);
      end;
    end;
    if (cpx.tokType=tkIdentifier) and (Upcase(cpx.token)='END') then begin
      blkEnd := true;
    end;
  end;
end;
procedure TParserAsm_6502.ProcASMline(out blkEnd: boolean);
{Process a line of ASM code. That line can be a mnemonic, a label, a comment, ...
 A line of ASM ends with the EOL or with the END reserved word.
 If found END, returns TRUE in "blkEnd".
 After processing a line (with error or not), this procedure leaves the lexer cursor at
 the start of the next line, except when the delimiter END is found. }
var
  idInst: TP6502Inst;
  tok, lbl: String;
  lblEle: TEleAsmInstr;
  undefLabel: boolean;
begin
  blkEnd := false;
  cpx.SkipWhitesNoEOL;
  if cpx.tokType = tkEol then begin
    cpx.Next;   //Go to next line
    exit; //Empty line
  end;
  //Proccess the ASM line
  if cpx.tokType = tkIdentifier then begin
    //Could be a mnemonic, directive "ORG" or a label.
    tok := Upcase(cpx.token);
    if FindOpcode(tok, idInst) then begin
      //It's a mnemonic
      cpx.HayError := false;
      ProcInstrASM(idInst, blkEnd);
      if cpx.HayError then begin
        //There were an error in the last instruction
        cpx.GotoEOL;   //Move to end of line.
        cpx.Next;      //Pass to the start of the next line.
        exit;
      end;
      if blkEnd then exit;  //The END delimiter has found.
    end else if tok = 'END' then begin
      //It's the end of ASM block
      blkEnd := true;
      exit;
    end else if tok = 'ORG' then begin
      //It's the ORG directive
      cpx.Next;
      AddDirectiveORG(0);  //Operand of ORG will be updated with CaptureOperand().
      if not CaptureOperand(curInst.operand, undefLabel) then exit;
      if undefLabel then curBlock.undefInstrucs.Add(curInst);  //Instruction with unsolved label
      exit;
    end else if tok = 'DB' then begin
      //Define a byte. Could be multiples bytes.
      repeat
        cpx.Next;    //Take DB
        AddDirectiveDB;  //Operand of DB will be updated with CaptureOperand().
        if not CaptureOperand(curInst.operand, undefLabel) then exit;
        if undefLabel then curBlock.undefInstrucs.Add(curInst);  //Instruction with unsolved label
        cpx.SkipWhitesNoEOL;
      until cpx.token<>',';
      if cpx.tokType = tkEol then begin
        //Must follow Eol
        cpx.Next;
      end else begin
        cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
        cpx.GotoEOL;   //Move to end of line.
        cpx.Next;      //Pass to the start of the next line.
      end;
      exit;
    end else if tok = 'DW' then begin
      //Define a byte. Could be multiples bytes.
      repeat
        cpx.Next;    //Take DB
        AddDirectiveDW;  //Operand of DB will be updated with CaptureOperand().
        if not CaptureOperand(curInst.operand, undefLabel) then exit;
        if undefLabel then curBlock.undefInstrucs.Add(curInst);  //Instruction with unsolved label
        cpx.SkipWhitesNoEOL;
      until cpx.token<>',';
      if cpx.tokType = tkEol then begin
        //Must follow Eol
        cpx.Next;
      end else begin
        cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
        cpx.GotoEOL;   //Move to end of line.
        cpx.Next;      //Pass to the start of the next line.
      end;
      exit;
    end else begin
      //Must be a label
      lbl := cpx.token;   //guarda posible etiqueta
      cpx.Next;
      if cpx.token = ':' then begin
        //Definitivamente es una etiqueta
        if IsLabelDeclared(lbl, lblEle) then begin  //¿Ya existe?
          cpx.GenError(ER_DUPLIC_LBL_, [lbl]);
          exit;
        end;
        //Crea la instrucción de etiqueta
        cpx.Next;      //Toma ":"
        AddInstructionLabel(lbl);
        //Verifica si sigue una instrucción
        cpx.SkipWhitesNoEOL;
        ProcASMline(blkEnd);
        exit;
      end else begin
        //Not a label
        cpx.GenError(ER_SYNTAX_ERR_, [lbl]);
        exit;
      end;
    end;
  end else if cpx.tokType = tkComment then begin
    cpx.SkipWhitesNoEOL;
  end else begin
    //Something is wrong
    cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
    cpx.GotoEOL;   //Move to end of line.
  end;
  //Process the line delimiter
  if cpx.tokType = tkEol then begin
    cpx.Next;      //Pass to the start of the next line.
  end else begin
    cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
    cpx.GotoEOL;   //Move to end of line.
    cpx.Next;      //Pass to the start of the next line.
  end;
end;
procedure TParserAsm_6502.AddInstruction(const inst: TP6502Inst;
  addMode: TP6502AddMode; param: integer; srcDec: TSrcPos);
{Add a new instruction to the current ASM block element. Set "curInst" pointing
to the instruction added.}
begin
  if curInst <> nil then begin
    //We need to close the current instruction.
    cpx.TreeElems.CloseElement;
  end;
  curInst := TEleAsmInstr.Create;
  curInst.name := '<inst>';
  curInst.srcDec := srcDec;
  curInst.addr := -1;   //Indica que la dirección física aún no ha sido fijada.
  curInst.iType := itOpcode;   //Marca como instrucción de salto.
  cpx.TreeElems.AddElementAndOpen(curInst);
  //Actualiza propiedades de la instrucción
  curInst.opcode := ord(inst);
  curInst.addMode := ord(addMode);
  curInst.operand.Val := param;
end;
procedure TParserAsm_6502.AddInstructionLabel(lblName: string);
{Add a new instruction to the current ASM block element. Set "curInst" pointing
to the instruction added.
If operand of the instruction is expression, it mus be added in the child nodes.}
begin
  if curInst <> nil then begin
    //We need to close the current instruction.
    cpx.TreeElems.CloseElement;
  end;
  curInst := TEleAsmInstr.Create;
  curInst.name := lblName;
  curInst.srcDec := cpx.GetSrcPos;
  curInst.addr := -1;   //Indica que la dirección física aún no ha sido fijada.
  curInst.iType := itLabel;   //Marca como instrucción de salto.
  cpx.TreeElems.AddElementAndOpen(curInst);
  labels.add(curInst);  //Agrega a la lista de etiquetas
end;
procedure TParserAsm_6502.AddDirectiveORG(param: word);
begin
  if curInst <> nil then begin
    //We need to close the current instruction.
    cpx.TreeElems.CloseElement;
  end;
  curInst := TEleAsmInstr.Create;
  curInst.name := 'ORG';
  curInst.srcDec := cpx.GetSrcPos;
  curInst.addr := -1;   //Indica que la dirección física aún no ha sido fijada.
  curInst.iType := itOrgDir;  //Represents ORG
  cpx.TreeElems.AddElementAndOpen(curInst);
  curInst.operand.Val := param;
end;
procedure TParserAsm_6502.AddDirectiveDB;
begin
  if curInst <> nil then begin
    //We need to close the current instruction.
    cpx.TreeElems.CloseElement;
  end;
  curInst := TEleAsmInstr.Create;
  curInst.name := 'DB';
  curInst.srcDec := cpx.GetSrcPos;
  curInst.addr := -1;   //Indica que la dirección física aún no ha sido fijada.
  curInst.iType := itDefByte;  //Represents DB
  cpx.TreeElems.AddElementAndOpen(curInst);
end;
procedure TParserAsm_6502.AddDirectiveDW;
begin
  if curInst <> nil then begin
    //We need to close the current instruction.
    cpx.TreeElems.CloseElement;
  end;
  curInst := TEleAsmInstr.Create;
  curInst.name := 'DW';
  curInst.srcDec := cpx.GetSrcPos;
  curInst.addr := -1;   //Indica que la dirección física aún no ha sido fijada.
  curInst.iType := itDefWord;  //Represents DB
  cpx.TreeElems.AddElementAndOpen(curInst);
end;
procedure TParserAsm_6502.ProcessASMblock(cpx0: TCompilerBase);
var
  blkEnd: boolean;
begin
  cpx := cpx0;  //Reference to compiler.
  cpx.Next;     //Get ASM
  cpx.curCtx.OnDecodeNext := @DecodeNext;  //Set a new lexer
  curBlock := TEleAsmBlock.Create;
  curBlock.srcDec := cpx.GetSrcPos;
  curBlock.name := 'ASMblk';
  cpx.TreeElems.AddElementAndOpen(curBlock);
  StartASM;
  curInst := nil;
  repeat
    ProcASMline(blkEnd);
  until cpx.atEof or blkEnd;
  if cpx.atEof then begin
    cpx.GenError('Unclosed ASM block.');  //Don't stop scanning
  end;
  EndASM;
  if curInst <> nil then begin
    //There are an instruction opened
    cpx.TreeElems.CloseElement;
  end;
  //Current token is delimiter END.
  cpx.curCtx.OnDecodeNext := nil;   //Restore lexer here, in order to take the "END" with the new lexer and avoid problems of syntax.
  cpx.Next;   //Take END with default lexer.
  cpx.TreeElems.CloseElement;  //Close ASM block
end;
function TParserAsm_6502.DecodeNext: boolean;
{Decode the token in the current position, indicated by (frow, fcol), and returns:
 - Token type in "toktyp".
 - Start of next token in (frow, fcol).
 - Value TRUE if the current line has changed.
}
var
  ctx: TContext;
begin
  ctx := cpx.curCtx;
  if ctx._Eof then begin
    ctx.tokType := tkNull;
    exit(false);
  end else if ctx._Eol then begin
    ctx.tokType := tkEol;
    if ctx._LastLine then begin
      //Cannot advance to a NextChar line. Keep position (EOF)
    end else begin
      //In a common line
      ctx._setRow(ctx.frow+1);
      ctx._setCol(1);
    end;
    exit(true);
  end;
  case ctx.curLine[ctx.fcol] of
  #32, #9: begin
    repeat
      inc(ctx.fcol);
    until ctx._Eol or not(ctx.curline[ctx.fcol] in [#32, #9]);
    ctx.tokType := tkSpace;
    //Leaves (ctx.frow, ctx.fcol) in the begin of the next token.
  end;
  '0'..'9': begin
    repeat
      inc(ctx.fcol);
    until ctx._Eol or not(ctx.curline[ctx.fcol] in ['0'..'9','.']);
    ctx.tokType := tkLitNumber;
  end;
  '$': begin
    repeat
      inc(ctx.fcol);
    until ctx._Eol or not(ctx.curline[ctx.fcol] in ['0'..'9','A'..'F','a'..'f']);
    ctx.tokType := tkLitNumber;
  end;
  '%': begin
    repeat
      inc(ctx.fcol);
    until ctx._Eol or not(ctx.curline[ctx.fcol] in ['0','1']);
    ctx.tokType := tkLitNumber;
  end;
  'A'..'Z','_',
  'a'..'z': begin
    repeat inc(ctx.fcol); until ctx._Eol or not(ctx.curline[ctx.fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    ctx.tokType := tkIdentifier;
  end;
  '+','-','*','/','\','=','^','@','.','#','>','<',':': begin
    ctx._NextChar;
    ctx.tokType := tkOperator;
  end;
  ';': begin
    ctx._NextChar;
    while not ctx._Eol do ctx._NextChar;
    //repeat ctx._NextChar until ctx._Eol;
    ctx.tokType := tkComment;
  end;
  '(',')',',','[',']': begin
    ctx._NextChar;
    ctx.tokType := tkOthers;
  end;
  '''': begin
    repeat inc(ctx.fcol); until ctx._Eol or (ctx.curline[ctx.fcol] = '''');
    if ctx._Eol then begin
      cpx.GenError('Unclosed string.');  //Don't stop scanning
    end else begin
      ctx._NextChar;  //Go to next character
    end;
    ctx.tokType := tkString;
  end;
  else
    //Unkmown token.
    ctx.tokType := tkNull;  //WARNING: This make the current token will read as empty.
    ctx._NextChar;
  end;
  exit(false);
end;
constructor TParserAsm_6502.Create;
begin
  inherited Create;
  labels := TEleAsmInstrs.Create(false);
end;
destructor TParserAsm_6502.Destroy;
begin
  labels.Destroy;
  inherited Destroy;
end;

initialization
  vParserASM_6502 := TParserAsm_6502.Create;
finalization
  vParserASM_6502.Destroy;
end.

