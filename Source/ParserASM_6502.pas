{Unit for parse ASM blocks of P65Pas compiler.
Functionality of parser is defined in the class TParserAsm_6502.
The input for this unit if the ASM code acceded trought the lexer of the compiler.
The output of the lexer is a new node element created in the syntax tree.
baed in the old unit: ParserAsm_PIC16
By Tito Hinostroza 03/09/2020.
}
unit ParserASM_6502;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, LexPas, CompBase, P6502utils, Globales, XpresElemP65;
type
  //Datos de una etiqueta
  TPicLabel = class
    txt: string;   //nombre de la etiqueta
    add: integer;  //dirección
  end;
  TPicLabel_list = specialize TFPGObjectList<TPicLabel>;

  //Datos de una instrucción de salto, indefinido.
  TPicUJump = class
    txt: string;        //Nombre de la etiqueta
    add: integer;       //Dirección donde inicia el salto
    idInst: TP6502Inst; //Instrucción
  end;
  TPicUJump_list = specialize TFPGObjectList<TPicUJump>;

  { TParserAsm_6502 }

  TParserAsm_6502 = class
  private
    cpx : TCompilerBase;   //Reference to compiler
    labels : TPicLabel_list; //Lista de etiquetas
    uJumps : TPicUJump_list; //Lista de instrucciones GOTO o i_CALL, indefinidas
    HayError: boolean;
    curInst : TxpEleAsmLine;  //Current instruction
    procedure AddLabel(name: string; addr: integer);
    procedure AddUJump(name: string; addr: integer; idInst: TP6502Inst);
    function CaptureAddress: boolean;
    function CaptureByte(out k: byte): boolean;
    function CaptureParenthes: boolean;
    procedure EndASM;
    function GetFaddressByte(addr: integer): byte;
    function HaveByteInformation(out bytePos: byte): boolean;
    function IsLabel(txt: string; out dir: integer): boolean;
    procedure ProcASMline(out blkEnd: boolean);
    procedure ProcInstrASM(idInst: TP6502Inst; var blkEnd: boolean);

    procedure StartASM;
  protected
    procedure UpdateInstruction(const inst: TP6502Inst; addMode: TP6502AddMode;
      param: integer);
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
  {$I ..\_language\tra_ParserAsm.pas}
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
procedure TParserAsm_6502.AddLabel(name: string; addr: integer);
{Agrega una etiqueta a la lista}
var
  lbl: TPicLabel;
begin
  lbl := TPicLabel.Create;
  lbl.txt:= UpCase(name);
  lbl.add := addr;
  labels.Add(lbl);
end;
procedure TParserAsm_6502.AddUJump(name: string; addr: integer; idInst: TP6502Inst);
{Agrega un salto indefinido a la lista}
var
  jmp: TPicUJump;
begin
  jmp := TPicUJump.Create;
  jmp.txt:= UpCase(name);
  jmp.add := addr;
  jmp.idInst := idInst;
  uJumps.Add(jmp);
end;
function TParserAsm_6502.IsLabel(txt: string; out dir: integer): boolean;
{Indica si un nombre es una etiqueta. Si lo es, devuelve TRUE, y la dirección la retorna
en "dir".}
var
  lbl: TPicLabel;
begin
  //No se espera procesar muchsa etiquetas
  for lbl in labels do begin
    if lbl.txt = upcase(txt) then begin
      dir := lbl.add;
      exit(true);
    end;
  end;
  //No encontró
  exit(false);
end;
function TParserAsm_6502.HaveByteInformation(out bytePos: byte): boolean;
begin
//    state0 := lexAsm.State;  //gaurda posición
  if cpx.token = '.' then begin
    //Hay precisión de campo
    cpx.Next;
    if UpCase(cpx.token) = 'LOW' then begin
      bytePos := 0;
      cpx.Next;
      exit(true);
    end else if UpCase(cpx.token) = 'HIGH' then begin
      bytePos := 1;
      cpx.Next;
      exit(true);
    end else begin
      //No es ninguno
      exit(false);
    end;
  end else if cpx.token = '@' then begin
    cpx.Next;
    if UpCase(cpx.token) = '0' then begin
      bytePos := 0;
      cpx.Next;
      exit(true);
    end else if UpCase(cpx.token) = '1' then begin
      bytePos := 1;
      cpx.Next;
      exit(true);
    end else if UpCase(cpx.token) = '2' then begin
      bytePos := 2;
      cpx.Next;
      exit(true);
    end else if UpCase(cpx.token) = '3' then begin
      bytePos := 3;
      cpx.Next;
      exit(true);
    end else begin
      //No es ninguno
      exit(false);
    end;
  end else begin
    //No tiene indicación de campo
    exit(false);
  end;
end;
function TParserAsm_6502.CaptureByte(out k: byte): boolean;
{Captura un byte y devuelve en "k". Si no encuentra devuelve FALSE.}
var
  n: Integer;
  xcon: TEleConsDec;
  ele: TxpElement;
  bytePos: byte;
  str: String;
  xvar: TEleVarDec;
begin
  Result := false;
  cpx.SkipWhitesNoEOL;
  if cpx.tokType = tkLitNumber then begin
    //es una dirección numérica
    n := StrToInt(cpx.token);
    if (n>255) then begin
      cpx.GenError(ER_EXPECT_BYTE);
      exit(false);
    end;
    k:=n;
    cpx.Next;
    exit(true);
  end else if cpx.tokType = tkIdentifier then begin
    //Es un identificador, puede ser referencia a una constante o variable.
    ele := cpx.TreeElems.FindFirst(cpx.token);  //identifica elemento
    if ele = nil then begin
      //No identifica a este elemento
      cpx.GenError(ER_EXP_CON_VAL);
      exit;
    end;
    if ele.idClass = eleConsDec then begin
      xcon := TEleConsDec(ele);
      cpx.AddCallerToFromCurr(xcon);  //lleva la cuenta
      if (xcon.typ = cpx.typByte) or (xcon.typ = cpx.typChar) then begin
        k := xcon.value.ValInt;
        cpx.Next;
        exit(true);
      end else if xcon.typ = cpx.typWord then begin
        cpx.Next;
        if HaveByteInformation(bytePos) then begin
          //Hay precisión de byte
          if bytePos = 0 then begin  //Byte bajo
            k := (xcon.value.ValInt and $FF);
          end else begin        //Byte alto
            k := (xcon.value.ValInt and $FF00) >> 8;
          end;
        end else begin  //No se indica byte
          k := (xcon.value.ValInt and $FF);
        end;
        exit(true);
      end else begin
        cpx.GenError(ER_NOGETVAL_CON);
        exit(false);
      end;
    end else if ele.idClass = eleVarDec then begin
      //Para varaibles, se toma la dirección
      xvar := TEleVarDec(ele);
      cpx.AddCallerToFromCurr(xvar);  //lleva la cuenta
      n := xvar.addr;
      k := GetFaddressByte(n);
      cpx.Next;
      exit(true);
    end else begin
      //No es constante
      cpx.GenError(ER_EXP_CON_VAL);
      exit(false);
    end;
  end else if (cpx.tokType = tkString) and (length(cpx.token) = 3) then begin
    //Es un caracter
    str := cpx.token;
    k := ord(str[2]);   //lee código de caracter
    cpx.Next;
    exit(true);
  end else begin
    cpx.GenError(ER_EXPECT_BYTE);
    exit(false);
  end;
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
function TParserAsm_6502.CaptureAddress: boolean;
{Captura una dirección a una instrucción. Si no encuentra genera
error y devuelve FALSE.}
var
  dir: integer;
  bytePos: byte;
  ele: TxpElement;
  xfun: TEleFun;
  xvar: TEleVarDec;
  xcon: TEleConsDec;
  opd: TEleOperand;
  opr: TEleOperator;
begin
  Result := false;
  cpx.SkipWhitesNoEOL;
  if cpx.token = '$' then begin
    //Es una dirección relativa
    cpx.Next;
    cpx.SkipWhitesNoEOL;
    //Puede tener + o -
    if (cpx.token= '') or (cpx.token = ';') then begin
      //Termina la instrucción sin o con comentario
      curInst.param := -1;  //Create as expression
      opd := TEleOperand.Create;
      opd.txt := '$';
      curInst.AddElement(opd);
      exit(true);
    end else if (cpx.token = '+') or (cpx.token = '-') then begin
      //Es dirección sumada
      curInst.param := -1;  //Create as expression
      opr := TEleOperator.Create;
      opr.txt := cpx.token;
      curInst.AddElement(opr);
      //Get operand
      cpx.Next;
      cpx.SkipWhitesNoEOL;
      if cpx.tokType = tkEol then begin
        cpx.GenError('Operand expected');
        exit(false);
      end else begin
        //Follow something
        opd := TEleOperand.Create;
        opd.txt := cpx.token;  //Get operand
        curInst.AddElement(opd);
        //CaptureByte(offset);  //captura desplazamiento
        //if HayError then exit(false);
        exit(true);
        //Formally we should continue checking expression or check nothing follows
      end;
    end else begin
      //Sigue otra cosa
      cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
    end;
  end else if cpx.tokType = tkLitNumber then begin
    //Es una dirección numérica
    curInst.param := StrToInt(cpx.token);  //Simple number
    cpx.Next;
    exit(true);
  end else if (cpx.tokType = tkIdentifier) and IsLabel(cpx.token, dir) then begin
    //Es un identificador de etiqueta
    curInst.param := dir;  //Simple number
    cpx.Next;
    exit(true);
  end else if cpx.tokType = tkIdentifier  then begin
    ele := cpx.TreeElems.FindFirst(cpx.token);  //identifica elemento
    if ele<>nil then begin
      //Se identifica un elemento del lenguaje
      if ele.idClass = eleFunc then begin
        //Es un identificador de función del árbol de sintaxis
        xfun := TEleFun(ele);
        cpx.AddCallerToFromCurr(xfun);  //lleva la cuenta
        //ad := xfun.adrr;   //lee su dirección
        curInst.param := -1;  //Set as expression
        opd := TEleOperand.Create;
        opd.elem := xfun;
        curInst.AddElement(opd);  //Add operand

        cpx.Next;
        exit(true);
      end else if ele.idClass = eleVarDec then begin
        //Es identificador de variable
        xvar := TEleVarDec(ele);
        cpx.AddCallerToFromCurr(xvar);  //lleva la cuenta
        curInst.param := -1;  //Set as expression
        cpx.Next;
        if HaveByteInformation(bytePos) then begin
          //Add operator of position
          opr := TEleOperator.Create;
          opr.txt := '@'+chr(bytePos+48);  //@0, @1, ...
          //Could be optimized if using char or number instead of "opr.txt".
          curInst.AddElement(opr);
        end;
        opd := TEleOperand.Create;
        opd.elem := xvar;
        curInst.AddElement(opd);
        exit(true);
      end else if ele.idClass = eleConsDec then begin
        //Es identificador de constante
        xcon := TEleConsDec(ele);
        cpx.AddCallerToFromCurr(xcon);  //lleva la cuenta
        //Constants can be resolved as numbers
        if (xcon.typ = cpx.typByte) or (xcon.typ = cpx.typWord) then begin
            curInst.param := xcon.value.ValInt;  //Get Value
          cpx.Next;
        end else begin
          //Otro tipo de constante
          cpx.GenError(ER_NOGETVAL_CON);
          exit(false);
        end;
        exit(true);
      end else begin
        //No se puede leer dirección
        cpx.GenError(ER_EXP_CON_VAL);
        exit(false);
      end;
    end else begin
      //Es un identificador, no definido. Puede definirse luego.
      curInst.param := -1;  //Create as expression
      opd := TEleOperand.Create;
      opd.txt := cpx.token;  //Probably a label. Will ne later linked.
      curInst.AddElement(opd);
      cpx.Next;
      exit(true);

      ////Los saltos indefinidos, se guardan en la lista "uJumps"
      //AddUJump(cpx.token, pic.iRam, idInst);
      //cpx.Next;
      //exit(true);
    end;
  end else begin
    cpx.GenError(ER_EXPECT_ADDR);
    Result := false;
    exit;
  end;
end;
procedure TParserAsm_6502.StartASM; //Inicia el procesamiento de código ASM
begin
  labels.Clear;   //limpia etiquetas
  uJumps.Clear;
end;
procedure TParserAsm_6502.EndASM;  //Termina el procesamiento de código ASM
var
  jmp : TPicUJump;
  loc: integer;
begin
  ////Completa los saltos indefinidos
  //if uJumps.Count>0 then begin
  //  for jmp in uJumps do begin
  //    if IsLabel(jmp.txt, loc) then begin
  //      //Sí existe la etiqueta
  //      if jmp.idInst in [i_BPL, i_BMI, i_BVC, i_BVS, i_BCC, i_BCS, i_BNE, i_BEQ] then
  //        //Salto relativo
  //        pic.cod_REL_JMP_at(jmp.add, loc-jmp.add-2)
  //      else  //Deberían ser JMP, JSR, LDA, STA ...
  //        pic.cod_JMP_at(jmp.add, loc);
  //    end else begin
  //      //No se enuentra
  //      GenErrorAsm(ER_UNDEF_LABEL_, [jmp.txt]);
  //      exit;
  //    end;
  //  end;
  //end;
end;
procedure TParserAsm_6502.ProcInstrASM(idInst: TP6502Inst; var blkEnd: boolean);
{Proccess an 6502 ASM instruction. Instruction must be previously validated and
 identified in "idInst".
 Basically this procedure, add a new TxpEleAsmLine (including instruction, addresing
 mode and operamd) to the current TxpEleAsmBlock, that represents a 6502 instruction.
 An instruction ends with the EOL token or the ASM delimiter "END".
 This procedure must not process the EOL token or the "END" delimiter.
 If the the "END" delimiter is found, the flag "blkEnd" is activated.
}
var
  preOperator: Char;
  procedure TestForByteOperator;
  {Test if position operand exist, and updates "preOperator".}
  begin
    if cpx.token = '>' then begin
      preOperator := '>';
      cpx.Next;
    end else if cpx.token = '<' then begin
      preOperator := '<';
      cpx.Next;
    end else begin
      preOperator := ' ';
    end;
  end;
  function ApplyByteOperator(n: integer): integer;
  {Apply operation defined in "preOperator"}
  begin
    if preOperator = '<' then exit(n and $FF)
    else if preOperator = '>' then exit( (n and $ff00) >> 8)
    else exit(n);
  end;
var
  tok: String;
  n: integer;
  xcon: TEleConsDec;
  ele: TxpElement;
  xvar: TEleVarDec;
  xfun: TEleFun;
  addressModes: TP6502AddModes;
  srcInst: TSrcPos;
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
    //Inmediato
    cpx.Next;
    TestForByteOperator;
    if cpx.tokType = tkLitNumber then begin
      if not TryStrToInt(cpx.token, n) then begin
        cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
        exit;
      end;
      n := ApplyByteOperator(n);
      if (n>255) then begin
        cpx.GenError(ER_EXPECT_BYTE);
        exit;
      end;
      cpx.Next;
      cpx.SkipWhitesNoEOL;
      //We assume there are not more text but We can process aditional text for expression
      AddInstruction(idInst, aImmediat, n, srcInst);
    end else if cpx.tokType = tkIdentifier then begin
      //Identificador
      ele := cpx.TreeElems.FindFirst(cpx.token);  //identifica elemento
      if ele = nil then begin
        cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
        exit;
      end else if ele.idClass = eleConsDec then begin
        //Es un identificador de constante del árbol de sintaxis
        xcon := TEleConsDec(ele);
        cpx.AddCallerToFromCurr(xcon);  //lleva la cuenta
        cpx.Next;
        n := ApplyByteOperator(xcon.value.ValInt);
        AddInstruction(idInst, aImmediat, n, srcInst);
      end else if ele.idClass = eleVarDec then begin
        //Es un identificador de variable del árbol de sintaxis
        xvar := TEleVarDec(ele);
        cpx.AddCallerToFromCurr(xvar);  //lleva la cuenta
        cpx.Next;
        n := ApplyByteOperator(xvar.addr);  //Lee dirección
        AddInstruction(idInst, aImmediat, n, srcInst);
      end else if ele.idClass = eleFunc then begin
        //Es un identificador de variable del árbol de sintaxis
        xfun := TEleFun(ele);
        cpx.AddCallerToFromCurr(xfun);  //lleva la cuenta
        cpx.Next;
        n := ApplyByteOperator(xfun.adrr);  //Lee dirección
        AddInstruction(idInst, aImmediat, n, srcInst);
      end else begin
        cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
        exit;
      end;
    end else begin
      cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
      exit;
    end;
  end else if cpx.tokType in [tkLitNumber, tkIdentifier] then begin
    //Puede ser absoluto o página cero, o sus versiones indexadas con X o Y.
    AddInstruction(idInst, aImplicit, 0, srcInst);  //Add the instruction with "aImplicit" temporally. Later will be updated.
    //Complete the "param" of "curInst".
    if not CaptureAddress then begin
      cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
      exit;
    end;
    {Get the addressing mode, considering operand is 16bits. If it's 8 bits, the
     addressing mode should be changed when linking.}
    cpx.SkipWhitesNoEOL;
    //Verify is follows ,X o ,Y
    if cpx.token = ',' then begin
      cpx.Next;
      cpx.SkipWhitesNoEOL;
      if Upcase(cpx.token) = 'X' then begin
        cpx.Next;
        UpdateInstruction(idInst, aAbsolutX, curInst.param);
      end else if Upcase(cpx.token) = 'Y' then begin
        cpx.Next;
        UpdateInstruction(idInst, aAbsolutY, curInst.param);
      end else begin
        cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
        exit;
      end;
    end else begin
        UpdateInstruction(idInst, aAbsolute, curInst.param);
    end;
  end else if tok = '(' then begin
    //Direccionamiento Indirecto: (indirect), (indirect,X) o (indirect),Y
    AddInstruction(idInst, aIndirect, 0, srcInst);  //Add the instruction with "aImplicit" temporally. Later will be updated.
    cpx.Next;
    if cpx.tokType in [tkLitNumber, tkIdentifier] then begin
//      n := StrToInt(cpx.token);
      if not CaptureAddress then begin
        cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
        exit;
      end;
      cpx.SkipWhitesNoEOL;
      if cpx.token = ',' then begin
        //Can only be (indirect,X)
        cpx.Next;  //Take number
        cpx.SkipWhitesNoEOL;
        if UpCase(cpx.token) <> 'X' then begin
          cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
          exit;
        end;
        UpdateInstruction(idInst, aIndirecX, curInst.param);
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
          UpdateInstruction(idInst, aIndirecY, curInst.param);
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
    cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
    exit;
  end;
end;
procedure TParserAsm_6502.ProcASMline(out blkEnd: boolean);
{Process a line of ASM code. That line can be a mnemonic, a label, a comment, ...
 A line of ASM ends with the EOL or with the END reserved word.
 If found END, returns TRUE in "blkEnd".
 After processing a line (with error or not), this procedure leaves the lexer cursor at
 the start of the next line, except when the delimiter END is found. }
var
  n: LongInt;
  idInst: TP6502Inst;
  tok, lbl: String;
  d: integer;
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
    if tok = 'ORG' then begin
      //It's the ORG directive
      cpx.Next;
      AddDirectiveORG(0);  //Operand of ORG will be updated with CaptureAddress().
      if not CaptureAddress then exit;
      exit;
//    end else if tok = 'DB' then begin
//      //Define a byte
    end else if tok = 'END' then begin
      //It's the end of ASM block
      blkEnd := true;
      exit;
    end else if FindOpcode(tok, idInst) then begin
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
    end else begin
      //Must be a label
      lbl := cpx.token;   //guarda posible etiqueta
      cpx.Next;
      if cpx.token = ':' then begin
        //Definitivamente es una etiqueta
        if IsLabel(lbl, d) then begin
          cpx.GenError(ER_DUPLIC_LBL_, [lbl]);
          exit;
        end;
        cpx.Next;
        cpx.SkipWhitesNoEOL;
        if cpx.tokType <> tkEol then begin
          //Hay algo más. Solo puede ser una instrucción
          if FindOpcode(cpx.token, idInst) then begin
            cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
            exit;
          end;
          //It's a mnemonic
          ProcInstrASM(idInst, blkEnd);
          if cpx.HayError then begin
            //There were an error in the last instruction
            cpx.GotoEOL;   //Move to end of line.
            cpx.Next;      //Pass to the start of the next line.
            exit;
          end;
          if blkEnd then exit;  //The END delimiter has found.
        end;
        exit;
      end else begin
        //Not a label
        cpx.GenError(ER_SYNTAX_ERR_, [cpx.token]);
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
procedure TParserAsm_6502.UpdateInstruction(const inst: TP6502Inst;
  addMode: TP6502AddMode; param: integer);
{Update the current instruction.}
begin
  curInst.inst := ord(inst);
  curInst.addMode := ord(addMode);
  curInst.param := param;
end;
procedure TParserAsm_6502.AddInstruction(const inst: TP6502Inst;
  addMode: TP6502AddMode; param: integer; srcDec: TSrcPos);
{Add a new instruction to the current ASM block element. Set "curInst" pointing
to the instruction added.
If operand of the instruction is expression, it mus be added in the child nodes.}
begin
  if curInst <> nil then begin
    //We need to close the current instruction.
    cpx.TreeElems.CloseElement;
  end;
  curInst := TxpEleAsmLine.Create;
  curInst.name := 'inst.';
  curInst.srcDec := srcDec;
  cpx.TreeElems.AddElementAndOpen(curInst);
  UpdateInstruction(inst, addMode, param);
end;
procedure TParserAsm_6502.AddDirectiveORG(param: word);
begin
  if curInst <> nil then begin
    //We need to close the current instruction.
    cpx.TreeElems.CloseElement;
  end;
  curInst := TxpEleAsmLine.Create;
  cpx.TreeElems.AddElementAndOpen(curInst);
  curInst.inst := -2;  //Represents ORG
  curInst.param := param;
end;
procedure TParserAsm_6502.ProcessASMblock(cpx0: TCompilerBase);
var
  blkEnd: boolean;
  asmBlock: TxpEleAsmBlock;
begin
  cpx := cpx0;  //Reference to compiler.
  cpx.Next;     //Get ASM
  cpx.curCtx.OnDecodeNext := @DecodeNext;  //Set a new lexer
  StartASM;
  asmBlock := TxpEleAsmBlock.Create;
  asmBlock.srcDec := cpx.GetSrcPos;
  asmBlock.name := 'ASMblk';
  cpx.TreeElems.AddElementAndOpen(asmBlock);
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
  //Curren token is delimiter END.
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
  '+','-','*','/','\','=','^','@','.','#': begin
    ctx._NextChar;
    ctx.tokType := tkOperator;
  end;
  ';': begin
    ctx._NextChar;
    repeat ctx._NextChar until ctx._Eol;
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
    ctx.tokType := tkNull;
    ctx._NextChar;
  end;
  exit(false);
end;
constructor TParserAsm_6502.Create;
begin
  inherited Create;
  labels := TPicLabel_list.Create(true);
  uJumps := TPicUJump_list.Create(true);
end;
destructor TParserAsm_6502.Destroy;
begin
  uJumps.Destroy;
  labels.Destroy;
  inherited Destroy;
end;

initialization
  vParserASM_6502 := TParserAsm_6502.Create;
finalization
  vParserASM_6502.Destroy;
end.

