{Unidad que agrega campos necesarios a la clase TCompilerBase, para la generación de
código con el PIC16F.}
unit GenCodBas_PIC16;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, CPUCore, P6502utils, CompBase, ParserDirec, Globales,
  XpresElemP65, XpresAST, LexPas, StrUtils, MisUtils, LCLType, LCLProc;
const
  STACK_SIZE = 8;      //tamaño de pila para subrutinas en el PIC
  MAX_REGS_AUX_BYTE = 6;   //cantidad máxima de registros a usar
  MAX_REGS_AUX_BIT = 4;    //cantidad máxima de registros bit a usar
  MAX_REGS_STACK_BYTE = 8; //cantidad máxima de registros a usar en la pila
  MAX_REGS_STACK_BIT = 4;  //cantidad máxima de registros a usar en la pila

type
  //Modes the compiler can call to the Code Generation routines
  TCompMod = (
    cmGenCode,   //Generating code
    cmConsEval,  //Evaluating constant
    cmRequire    //Checking register and variables requirement
  );
  {Información sobre los saltos con la instrucción kIF_TRUE}
  TIfInfo = record
    igoto  : integer;   //Address where is GOTO
  end;
  PtrTCPURam = ^TCPURam;
  { TGenCodBas }
  TGenCodBas = class(TParserDirecBase)
  protected //Operations for parameters or Binary Operators
    function stoOperation(parA, parB: TEleExpress): TStoOperandsBOR; inline;
    procedure Exchange(var parA, parB: TEleExpress);
    function BinOperationStr(fun: TEleExpress): string;
  private
    linRep : string;   //línea para generar de reporte
    posFlash: Integer;
    procedure GenCodeASMline(inst: TxpEleAsmLine);
    procedure GenCodLoadToA(fun: TEleExpress);  { TODO : ¿Se necesita? No se usa }
    procedure GenCodLoadToX(fun: TEleExpress);  { TODO : ¿Se necesita? No se usa }
    procedure GenCodLoadToY(fun: TEleExpress);  { TODO : ¿Se necesita? No se usa }
    procedure GenCondeIF(sen: TxpEleSentence);
    procedure StartCodeGen;
    procedure StopCodeGen;
    procedure ProcByteUsed(offs: word; regPtr: TCPURamCellPtr);
    procedure SetSharedUnused;
    procedure SetSharedUsed;
    procedure word_ClearItems(const OpPtr: pointer);
  protected  //Register work
    //Work register (WR)
    A      : TCpuRegister;     //Registro Interno.
    //System variables used as registers
    H      : TEleVarDec;  //To load the high byte of words.
    E      : TEleVarDec;  //To load the high word of dwords.
    U      : TEleVarDec;  //To load the high word of dwords.
    IX     : TEleVarDec;  //To index operands
    procedure PutLabel(lbl: string); inline;
    procedure PutTopComm(cmt: string; replace: boolean = true); inline;
    procedure PutComm(cmt: string); inline;
    procedure PutFwdComm(cmt: string); inline;
    function ReportRAMusage: string;
    function ValidateByteRange(n: integer): boolean;
    function ValidateWordRange(n: integer): boolean;
    function ValidateDWordRange(n: Int64): boolean;
    procedure LoadToWR(fun: TEleExpress);
  protected  //Register and temporal variables requirement.
    compMod: TCompMod;  //Mode of the compiler
    function requireA: boolean;  //Declare use of register A
    function requireH: boolean;  //Declare use of register H
    function requireE: boolean;  //Declare use of register E
    function requireU: boolean;  //Declare use of register U
    function requireVar2(fun: TEleExpress; rname: string; rtype: TEleTypeDec; out
      rvar: TEleVarDec): boolean;
  protected
    procedure ResetRAM;
    procedure functCall(fun: TEleFunBase; out AddrUndef: boolean);
    procedure CreateVarsAndPars;
    procedure codRTS(isInterrupt: boolean);
    procedure GenCodeExpr(eleExp: TEleExpress);
    procedure GenCodeSentences(sentList: TxpElements);
//    procedure GenCodeBody(body: TEleBody);
    procedure GenCodeBlock(block: TEleBlock);
  protected  //Memory managing routines for variables
    procedure WriteVaLueToRAM(target: PtrTCPURam; add: word; typ: TEleTypeDec;
      const value: TConsValue);
    procedure CreateVarInRAM(xVar: TEleVarDec; shared: boolean);
    procedure CreateValueInCode(typ: TEleTypeDec; const value: TConsValue; out startAddr: integer);
  protected  //Methods for set a function result.
    procedure SetFunNull(fun: TEleExpress);
    procedure SetFunConst(fun: TEleExpress);
    procedure SetFunVariab(fun: TEleExpress; rVar: TEleVarDec);
    procedure SetFunVariab(fun: TEleExpress; addr: word);
    procedure SetFunVariab_RamVarOf(fun: TEleExpress; rVar: TEleVarDec;
      offset: integer);
    procedure SetFunExpres(fun: TEleExpress);
    //Set result as a constant.
    procedure SetFunConst_bool(fun: TEleExpress; valBool: Boolean);
    procedure SetFunConst_byte(fun: TEleExpress; valByte: integer);
    procedure SetFunConst_char(fun: TEleExpress; valByte: integer);
    procedure SetFunConst_word(fun: TEleExpress; valWord: integer);
  protected  //Code instructions
    function _PC: word;
    function _CLOCK: integer;
    procedure _LABEL_post(igot: integer);
    procedure _LABEL_pre(out curAddr: integer);
    //Instrucciones simples
    procedure _ADCi(const k: word);  //immidiate
    procedure _ADC(const addr: integer);  //Absolute/Zeropage
    procedure _ANDi(const k: word);
    procedure _AND(const addr: integer);
    procedure _ASL(const f: word);
    procedure _ASLa;
    procedure _LSR(const f: word);
    procedure _LSRa;
    procedure _JMP(const ad: word);
    procedure _JMP_post(out igot: integer);
    procedure _JSR(const ad: word);
    procedure _BEQ(const ad: ShortInt);
    procedure _BEQ_post(out ibranch: integer);
    procedure _BNE(const ad: ShortInt);
    procedure _BNE_pre(curAddr: integer);
    procedure _BNE_post(out ibranch: integer);
    procedure _BCC(const ad: ShortInt);
    procedure _BCC_post(out ibranch: integer);
    procedure _BCS(const ad: ShortInt);
    procedure _BCS_post(out ibranch: integer);
    //Alias for BCC and BCS
    procedure _BLT_post(out ibranch: integer);  //Less than
    procedure _BGE_post(out ibranch: integer);  //Greater or equal to
    procedure _BPL(const ad: ShortInt);
    procedure _BPL_pre(curAddr: integer);
    procedure _BPL_post(out ibranch: integer);
    procedure _CLC;
    procedure _CMPi(const k: word);  //immidiate
    procedure _CMP(const addr: integer);  //Absolute/Zeropage
    procedure _DEX;
    procedure _DEY;
    procedure _DEC(const addr: integer);
    procedure _EORi(const k: word);
    procedure _EOR(const addr: integer);
    procedure _INC(const addr: integer);
    procedure _INX;
    procedure _INY;
    procedure _LDAi(const k: word);
    procedure _LDA(const addr: integer);
    procedure _LDXi(const k: word);
    procedure _LDX(const addr: integer);
    procedure _LDYi(const k: word);
    procedure _LDY(const addr: integer);
    procedure _NOP;
    procedure _ORAi(const k: word);
    procedure _ORA(const addr: integer);
    procedure _PHA; inline;
    procedure _PHP; inline;
    procedure _PLA; inline;
    procedure _PLP; inline;
    procedure _ROL(const addr: integer);
    procedure _RORa;  //Acumulator
    procedure _ROR(const addr: integer);  //Absolute/Zeropage
    procedure _RTS;
    procedure _RTI;
    procedure _SEC;
    procedure _SED;
    procedure _SBCi(const k: word);   //SBC Immediate
    procedure _SBC(const addr: integer);  //SBC Absolute/Zeropage
    procedure _STA(addr: integer);          //STX Absolute/Zeropage
    procedure _STX(const addr: integer);  //STX Absolute/Zeropage
    procedure _STY(const addr: integer);  //STY Absolute/Zeropage
    procedure _TAX;
    procedure _TAY;
    procedure _TYA;
    procedure _TXA;
    procedure IF_TRUE(OpRes: TEleExpress; out info: TIfInfo);
//    procedure IF_FALSE(OpRes: TEleExpress; out info: TIfInfo);
    procedure IF_END(const info: TIfInfo);
  protected  //Functions for types
    //////////////// Tipo Boolean /////////////
    procedure bool_LoadToRT(fun: TEleExpress);
    //////////////// Tipo Byte /////////////
    procedure byte_LoadToRT(fun: TEleExpress);
    procedure byte_DefineRegisters;
    procedure byte_SaveToStk;
    //////////////// Tipo Word /////////////
    procedure word_LoadToRT(fun: TEleExpress);
    procedure word_DefineRegisters;
    procedure word_SaveToStk;
    procedure word_Low(fun: TEleExpress);
    procedure word_High(fun: TEleExpress);
  public     //Access to CPU information
    function PICName: string; override;
    function RAMmax: integer; override;
  public     //Initialization
    pic        : TP6502;       //CPU object
    function CompilerName: string; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  procedure SetLanguage;
implementation
var
  TXT_SAVE_W, TXT_SAVE_Z, TXT_SAVE_H, MSG_NO_ENOU_RAM, MSG_VER_CMP_EXP,
  MSG_STACK_OVERF, MSG_NOT_IMPLEM, WA_UNUSED_VAR_, ER_NOT_IMPLEM_ ,
  ER_ASIG_EXPECT
  : string;

procedure SetLanguage;
begin
  ParserDirec.SetLanguage;
  {$I ..\_language\tra_GenCodBas.pas}
end;
{ TGenCodPic }
function TGenCodBas.BinOperationStr(fun: TEleExpress): string;
{Returns a string representing a binary operation.}
var
  parA, parB: TEleExpress;
  Oper: String;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  Oper := IfThen(fun.rfun.operTyp = opkBinary, fun.rfun.oper, fun.name);
  Result := parA.StoAsStr+'(' + parA.Typ.name + ') ' + Oper + ' ' +
            parB.StoAsStr+'(' + parB.Typ.name + ')';
end;
function TGenCodBas.stoOperation(parA, parB: TEleExpress): TStoOperandsBOR;
begin
  //Combinación de los almacenamientos de los operandos
  Result := TStoOperandsBOR((Ord(parA.Sto) << 4) or ord(parB.Sto));
end;
procedure TGenCodBas.Exchange(var parA, parB: TEleExpress);
{Intercambia el orden de los operandos.}
var
  tmp: TEleExpress;
begin
  //Invierte los operandos
  tmp := parA;
  parA := parB;
  parB := tmp;
end;
procedure TGenCodBas.ProcByteUsed(offs: word; regPtr: TCPURamCellPtr);
begin
  linRep := linRep + regPtr^.name +
            ' DB ' + '$' + IntToHex(offs, 3) + LineEnding;
end;
procedure TGenCodBas.SetSharedUnused;
begin
  pic.SetSharedUnused;
end;
procedure TGenCodBas.SetSharedUsed;
begin
  pic.SetSharedUsed;
end;
procedure TGenCodBas.word_ClearItems(const OpPtr: pointer);
begin

end;
procedure TGenCodBas.PutLabel(lbl: string);
{Agrega uan etiqueta antes de la instrucción. Se recomienda incluir solo el nombre de
la etiqueta, sin ":", ni comentarios, porque este campo se usará para desensamblar.}
begin
  pic.addTopLabel(lbl);  //agrega línea al código ensmblador
end;
procedure TGenCodBas.PutTopComm(cmt: string; replace: boolean = true);
//Agrega comentario al inicio de la posición de memoria
begin
  pic.addTopComm(cmt, replace);  //agrega línea al código ensmblador
end;
procedure TGenCodBas.PutComm(cmt: string);
//Agrega comentario lateral al código. Se llama después de poner la instrucción.
begin
  pic.addSideComm(cmt, true);  //agrega línea al código ensmblador
end;
procedure TGenCodBas.PutFwdComm(cmt: string);
//Agrega comentario lateral al código. Se llama antes de poner la instrucción.
begin
  pic.addSideComm(cmt, false);  //agrega línea al código ensmblador
end;
function TGenCodBas.ReportRAMusage: string;
{Genera un reporte de uso de la memoria RAM}
begin
  linRep := '';
  pic.ExploreUsed(@ProcByteUsed);
  Result := linRep;
end;
function TGenCodBas.ValidateByteRange(n: integer): boolean;
//Verifica que un valor entero, se pueda convertir a byte. Si no, devuelve FALSE.
begin
  if (n>=0) and (n<256) then
     exit(true)
  else begin
    GenError('Numeric value exceeds a byte range.');
    exit(false);
  end;
end;
function TGenCodBas.ValidateWordRange(n: integer): boolean;
//Verifica que un valor entero, se pueda convertir a byte. Si no, devuelve FALSE.
begin
  if (n>=0) and (n<65536) then
     exit(true)
  else begin
    GenError('Numeric value exceeds a word range.');
    exit(false);
  end;
end;
function TGenCodBas.ValidateDWordRange(n: Int64): boolean;
begin
  if (n>=0) and (n<$100000000) then
     exit(true)
  else begin
    GenError('Numeric value exceeds a dword range.');
    exit(false);
  end;
end;
procedure TGenCodBas.LoadToWR(fun: TEleExpress);
{Carga un operando a los Registros de Trabajo (WR).}
begin
  if fun.Typ.OnLoadToWR=nil then begin
    //No implementado
    GenError(ER_NOT_IMPLEM_, ['LoadToRT']);
  end else begin
    fun.Typ.OnLoadToWR(fun);
  end;
end;
//Register and temporal variables requirement.
function TGenCodBas.requireA: boolean;
begin
  //if ModeRequire then a.used := True;
  exit(true);   //Always available
end;
function TGenCodBas.requireH: boolean;
{Indicates the register H will be used in the code generation.
Returns TRUE if the register is allocated in RAM.}
begin
  //if ModeRequire then H.regUsed := true;
  H.required := true; //Mark to be allocated later
  exit(H.allocated);   //Already allocated in memory.
end;
function TGenCodBas.requireE: boolean;
begin
  E.required := true;
  exit(E.allocated);   //Already allocated in memory.
end;
function TGenCodBas.requireU: boolean;
begin
  U.required := true;
  exit(U.allocated);   //Already allocated in memory.
end;
function TGenCodBas.requireVar2(fun: TEleExpress; rname: string; rtype: TEleTypeDec;
         out rvar: TEleVarDec): boolean;
{Indicates we are going to need a temporal variable of the type indicated.
- Parameter "rname" is a name used to identify the variable. It's case sensitive. Must be
  unique for the same TxpEleExpress. It's recommended to be short for quick searches.
- Parameter "rtype" is the type of the variable required.
- Parameter "rvar" is the reference to the variable required.
So is we are going to require 2 temporal variables, we should use:
    requireVar(fun, 'a', typByte, varA);
    requireVar(fun, 'b', typWord, varB);
Is the variable is available (created and allocated), returns TRUE.}
var
  v, exist: TEleVarDec;
begin
  //Test if requirement exist
  exist := nil;
  for v in fun.tempVars do begin
    if v.name = rname then begin
      exist := v;
      break;
    end;
  end;
  if exist<>nil then begin
    //Exists
    exit(exist.allocated);
  end else begin
    //No exists. We need to create requirement.
    rvar := CreateVar('', rtype);
    fun.tempVars.Add(rvar);
    exit(false);
  end;
  //if ModeRequire then begin
end;
//Memory managing routines for variables.
procedure TGenCodBas.WriteVaLueToRAM(target: PtrTCPURam; add: word; typ: TEleTypeDec;
  const value: TConsValue);
//Write a constant value, of any type, to a some position in the RAM.
var
  i: Integer;
begin
  if typ.catType = tctAtomic then begin
    if typ = typByte then begin
      target^[add].value := value.ValInt and $ff;
    end else if typ = typChar then begin
      target^[add].value := value.ValInt and $ff;
    end else if typ = typBool then begin
      if value.ValBool then target^[add].value := 1
      else target^[add].value := 0;
    end else if typ = typWord then begin
      target^[add].value := value.ValInt and $ff;
      target^[add+1].value := (value.ValInt >> 8) and $ff;
    end else begin
      GenError(MSG_NOT_IMPLEM);
    end;
  end else if typ.catType = tctArray then begin
    //Composite type
    for i:=0 to high(value.items) do begin
      WriteVaLueToRAM(target, add, typ.itmType, value.items[i]);  //Recursion
      if HayError then exit;
      inc(add, typ.itmType.size);
    end;
  end else if typ.catType = tctPointer then begin
    //Pointer are as words
    target^[add].value := value.ValInt and $ff;
    target^[add+1].value := (value.ValInt >> 8) and $ff;
  end else begin
    GenError(MSG_NOT_IMPLEM);
  end;
end;
procedure TGenCodBas.CreateVarInRAM(xVar: TEleVarDec; shared: boolean);
{Assign physical location in RAM to a variable (If it's not defined as REGISTER).
Variables are created starting at the "GeneralORG" compiler option position.
Variables are created in Free RAM location, except if they are ABSOLUTE. }
var
  varName: String;
  nbytes: integer;
  typ: TEleTypeDec;
  startAdd: word;
  i: integer;
  outOfProgram: Boolean;
begin
  //Validation
  if xVar.adicPar.hasAdic in [decRegis, decRegisA, decRegisX, decRegisY] then begin
    //Register variables don't use RAM.
    exit;
  end;
  varName := xVar.name;
  typ := xVar.typ;
  //Find the memory address where to place the variable.
  pic.freeStart := GeneralORG;  //Find at the current program block.
  nbytes := typ.size;
  if xVar.adicPar.hasAdic = decAbsol then begin
    //It's ABSOLUTE to something
    if xVar.adicPar.absVar<>nil then begin
      //ABSOLUTE to a variable.
      startAdd := xVar.adicPar.absVar.addr+  //Se supone que "xVar.adicPar.absVar" ya está mapeada en RAM.
                  xVar.adicPar.absOff;
    end else begin
      //ABSOLUTE to a fixed address.
      startAdd := xVar.adicPar.absAddr;
    end;
  end else begin
    if not pic.GetFreeBytes(nbytes, startAdd) then begin
      GenError(MSG_NO_ENOU_RAM);
      exit;
    end;
  end;
  xVar.addr:=startAdd;  //Set address
  xVar.allocated := true;
  //Detect if variable location is out of the code block.
  //We assume absolute variables are out of code to protect from initialization
  {The problem is in the *.PRG format we use for output, doesn't allow to specify
  separates blocks of memory to fill. For example if we have specified an address like
  $FFFF for an absolute variable, and the program start at $0000, all the RAM must be
  included in *.PRG.}
  outOfProgram := (xVar.adicPar.hasAdic = decAbsol) or
                  (pic.dataAddr1<>-1);  //This means the variable has been placed in the primary data address.
  //Mark as used as variable Data. Not instruction.
  if outOfProgram then begin
    //Out of the program block, mark as "ruAbsData", in order to not be considered
    //to generate the PRG file.
    for i:=startAdd to startAdd+nbytes-1 do begin
      pic.ram[i].used := ruAbsData;
      if shared then begin
        pic.ram[i].shared := true;  //Marca como compartido
      end;
    end;
  end else begin
    //In the program block
    for i:=startAdd to startAdd+nbytes-1 do begin
      pic.ram[i].used := ruData;
      if shared then begin
        pic.ram[i].shared := true;  //Marca como compartido
      end;
    end;
  end;
  //Set name to that position
  if typ.IsByteSize then begin
    pic.SetNameRAM(startAdd, xVar.name);
  end else begin
    pic.SetNameRAM(startAdd, xVar.name + '@0');
    pic.SetNameRAM(startAdd+1, xVar.name + '@1');
  end;
  //Set initial value.
  if xVar.adicPar.hasInit then begin
    if outOfProgram then begin  //Only allowed in the program block
      GenError('Cannot initialize absolute variable "%s" in this location.', [varName]);
    end;
    //Here, we need to know the type
    WriteVaLueToRAM(@pic.ram, startAdd, typ, xVar.adicPar.constDec.value);
    if HayError then  exit;
  end;
  if typ.OnGlobalDef<>nil then typ.OnGlobalDef(varName, '');
end;
procedure TGenCodBas.CreateValueInCode(typ: TEleTypeDec;
  const value: TConsValue; out startAddr: integer);
{Write a constant value in RAM, in the current code section, adding the correspondent JMP
instruction. Returns in "startAddr", the address where start the value.}
var
  j1, i: integer;
  nbytes: SmallInt;
begin
  nbytes := typ.size;
  _JMP_post(j1);   //Salto hasta después del espacio de variables
  startAddr := pic.iRam;
  WriteVaLueToRAM(@pic.ram, pic.iRam, typ, value);
  for i:=pic.iRam to pic.iRam+nbytes-1 do begin
    pic.ram[i].used := ruData;
  end;
  inc(pic.iRam, nBytes);  //Move pointer.
_LABEL_post(j1);   //Termina de codificar el salto
end;
//Métodos para fijar el resultado
procedure TGenCodBas.SetFunNull(fun: TEleExpress);
{Fija el resultado como NULL.}
begin
  fun.Typ := typNull;
  fun.Sto := stNone;
  lastASMcode := lacNone;
  AcumStatInZ := true;
end;
procedure TGenCodBas.SetFunConst(fun: TEleExpress);
{Fija los parámetros del resultado de una subexpresion. Este método se debe ejcutar,
siempre antes de evaluar cada subexpresión.}
begin
  fun.opType := otConst;
  fun.Sto := stConst;  //La única opción es esta.
  lastASMcode := lacNone;
  AcumStatInZ := true;
end;
procedure TGenCodBas.SetFunVariab(fun: TEleExpress; rVar: TEleVarDec);
{Set an operand TxpEleExpress to type otVariab and storage stRamFix.}
begin
  fun.SetVariab(rVar);
  lastASMcode := lacNone;
  AcumStatInZ := true;   //Default TRUE is explained in Documentation.
end;
procedure TGenCodBas.SetFunVariab(fun: TEleExpress; addr: word);
{Fija los parámetros del resultado de una subexpresion. Este método se debe ejecutar,
siempre antes de evaluar cada subexpresión.}
begin
  fun.SetVariab(addr);
  lastASMcode := lacNone;
  AcumStatInZ := true;   //Default TRUE is explained in Documentation.
end;
procedure TGenCodBas.SetFunVariab_RamVarOf(fun: TEleExpress; rVar: TEleVarDec;
  offset: integer);
{Set an operand TxpEleExpress to type otVariab and storage stRamVarOf.}
begin
  fun.SetVariab_RamVarOf(rVar, offset);
  lastASMcode := lacNone;
  AcumStatInZ := true;   //Default TRUE is explained in Documentation.
end;
procedure TGenCodBas.SetFunExpres(fun: TEleExpress);
{Fija los parámetros del resultado de una subexpresion. Este método se debe
ejecutar, siempre antes de evaluar cada subexpresión.}
begin
  fun.opType := otFunct; //Fija como expresión
  fun.Sto := stRegister; //Almacenamiento por defecto

  //Limpia el estado. Esto es útil que se haga antes de generar el código para una operación
  lastASMcode := lacNone;
  AcumStatInZ := true;
end;
//Fija el resultado de ROP como constante
procedure TGenCodBas.SetFunConst_bool(fun: TEleExpress; valBool: Boolean);
begin
  SetFunConst(fun);
  fun.evaluated := true;
  fun.value.valBool := valBool;
end;
procedure TGenCodBas.SetFunConst_byte(fun: TEleExpress; valByte: integer);
begin
  if not ValidateByteRange(valByte) then
    exit;  //Error de rango
  SetFunConst(fun);
  fun.evaluated := true;
  fun.value.valInt := valByte;
end;
procedure TGenCodBas.SetFunConst_char(fun: TEleExpress; valByte: integer);
begin
  SetFunConst(fun);
  fun.evaluated := true;
  fun.value.valInt := valByte;
end;
procedure TGenCodBas.SetFunConst_word(fun: TEleExpress; valWord: integer);
begin
  if not ValidateWordRange(valWord) then
    exit;  //Error de rango
  SetFunConst(fun);
  fun.evaluated := true;
  fun.value.valInt := valWord;
end;
//Codifición de instrucciones
function TGenCodBas._PC: word; inline;
{Devuelve la dirección actual en Flash}
begin
  Result := pic.iRam;
end;
function TGenCodBas._CLOCK: integer; inline;
{Devuelve la frecuencia de reloj del PIC}
begin
  Result := pic.frequen;
end;
procedure TGenCodBas._LABEL_post(igot: integer);
{Finish a previous JMP_lbl, BNE_post, BEQ_post, ... instructions.}
var
  offset: integer;
begin
  if pic.ram[igot].value = 0 then begin
    //Es salto absoluto
    pic.ram[igot].value   := lo(_PC);
    pic.ram[igot+1].value := hi(_PC);
  end else begin
    //Es salto relativo
    if _PC > igot then begin
      //Salto hacia adelante
      offset := _PC - igot-1;
      if offset>127 then begin
        GenError('Block to long.');
        exit;
      end;
      pic.ram[igot].value := offset;
    end else begin
      //Backward jump. Does this really happens?
      offset := _PC - igot;  //negative
      if offset<-128 then begin
        GenError('Block to long.');
        exit;
      end;
      pic.ram[igot].value := 256 + offset;
    end;
  end;
end;
procedure TGenCodBas._LABEL_pre(out curAddr: integer);
{Set a label for a later jump BNE_pre, BEQ_pre, BCC_pre ... instructions.}
begin
  curAddr := pic.iRam;
end;
//Instrucciones simples
procedure TGenCodBas._ADCi(const k: word);
begin
  pic.codAsm(i_ADC, aImmediat, k);
end;
procedure TGenCodBas._ADC(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_ADC, aZeroPage, addr);
  end else begin
    pic.codAsm(i_ADC, aAbsolute, addr);
  end;
end;
procedure TGenCodBas._ANDi(const k: word);
begin
  pic.codAsm(i_AND, aImmediat, k);
end;
procedure TGenCodBas._AND(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_AND, aZeroPage, addr);
  end else begin
    pic.codAsm(i_AND, aAbsolute, addr);
  end;
end;
procedure TGenCodBas._ASL(const f: word);  //ASL Absolute/Zeropage
begin
  if f<256 then begin
    pic.codAsm(i_ASL, aZeroPage, f);
  end else begin
    pic.codAsm(i_ASL, aAbsolute, f);
  end;
end;
procedure TGenCodBas._ASLa;
begin
  pic.codAsm(i_ASL, aAcumulat, 0);
end;
procedure TGenCodBas._LSR(const f: word);  //LSR Absolute/Zeropage
begin
  if f<256 then begin
    pic.codAsm(i_LSR, aZeroPage, f);
  end else begin
    pic.codAsm(i_LSR, aAbsolute, f);
  end;
end;
procedure TGenCodBas._LSRa;
begin
  pic.codAsm(i_LSR, aAcumulat, 0);
end;
procedure TGenCodBas._JMP(const ad: word);
begin
  pic.codAsm(i_JMP, aAbsolute, ad);  //pone salto indefinido
end;
procedure TGenCodBas._JMP_post(out igot: integer);
{Escribe una instrucción GOTO, pero sin precisar el destino aún. Devuelve la dirección
 donde se escribe el GOTO, para poder completarla posteriormente.
}
begin
  igot := pic.iRam+1;  //guarda posición de instrucción de salto
  pic.codAsm(i_JMP, aAbsolute, 0);  //1 en Offset indica que se completará con salto absoluto
end;
procedure TGenCodBas._JSR(const ad: word);
begin
  pic.codAsm(i_JSR, aAbsolute, ad);  //1 en Offset indica que se completará con salto absoluto
end;
procedure TGenCodBas._BEQ(const ad: ShortInt);
begin
  if ad>=0 then begin
    pic.codAsm(i_BEQ, aRelative, ad);
  end else begin
    pic.codAsm(i_BEQ, aRelative, 256+ad);
  end;
end;
procedure TGenCodBas._BEQ_post(out ibranch: integer);
begin
  ibranch := pic.iRam+1;  //guarda posición del offset de salto
  pic.codAsm(i_BEQ, aRelative, 1);  //1 en Offset indica que se completará con salto relativo
end;
procedure TGenCodBas._BNE(const ad: ShortInt);
begin
  if ad>=0 then begin
    pic.codAsm(i_BNE, aRelative, ad);
  end else begin
    pic.codAsm(i_BNE, aRelative, 256+ad);
  end;
end;
procedure TGenCodBas._BNE_post(out ibranch: integer);
begin
  ibranch := pic.iRam+1;  //guarda posición del offset de salto
  pic.codAsm(i_BNE, aRelative, 1);  //1 en Offset indica que se completará con salto relativo
end;
procedure TGenCodBas._BNE_pre(curAddr: integer);
begin
  pic.codAsm(i_BNE, aRelative, (curAddr - pic.iRam-2) and $ff);
end;
procedure TGenCodBas._BCC(const ad: ShortInt);
begin
  if ad>=0 then begin
    pic.codAsm(i_BCC, aRelative, ad);
  end else begin
    pic.codAsm(i_BCC, aRelative, 256+ad);
  end;
end;
procedure TGenCodBas._BCC_post(out ibranch: integer);
begin
  ibranch := pic.iRam+1;  //guarda posición del offset de salto
  pic.codAsm(i_BCC, aRelative, 1);  //1 en Offset indica que se completará con salto relativo
end;
procedure TGenCodBas._BCS(const ad: ShortInt);
begin
  if ad>=0 then begin
    pic.codAsm(i_BCS, aRelative, ad);
  end else begin
    pic.codAsm(i_BCS, aRelative, 256+ad);
  end;
end;
procedure TGenCodBas._BCS_post(out ibranch: integer);
begin
  ibranch := pic.iRam+1;  //guarda posición del offset de salto
  pic.codAsm(i_BCS, aRelative, 1);  //1 en Offset indica que se completará con salto relativo
end;
procedure TGenCodBas._BLT_post(out ibranch: integer);
begin
  ibranch := pic.iRam+1;  //guarda posición del offset de salto
  pic.codAsm(i_BCC, aRelative, 1);  //1 en Offset indica que se completará con salto relativo
end;
procedure TGenCodBas._BGE_post(out ibranch: integer);
begin
  ibranch := pic.iRam+1;  //guarda posición del offset de salto
  pic.codAsm(i_BCS, aRelative, 1);  //1 en Offset indica que se completará con salto relativo
end;
procedure TGenCodBas._BPL(const ad: ShortInt);
begin
  if ad>=0 then begin
    pic.codAsm(i_BPL, aRelative, ad);
  end else begin
    pic.codAsm(i_BPL, aRelative, 256+ad);
  end;
end;
procedure TGenCodBas._BPL_pre(curAddr: integer);
begin
  pic.codAsm(i_BPL, aRelative, (curAddr - pic.iRam-2) and $ff);
end;
procedure TGenCodBas._BPL_post(out ibranch: integer);
begin
  ibranch := pic.iRam+1;  //guarda posición del offset de salto
  pic.codAsm(i_BPL, aRelative, 1);  //1 en Offset indica que se completará con salto relativo
end;
procedure TGenCodBas._CLC;
begin
  pic.codAsm(i_CLC, aImplicit, 0);
end;
procedure TGenCodBas._CMPi(const k: word);
begin
  pic.codAsm(i_CMP, aImmediat, k);
end;
procedure TGenCodBas._CMP(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_CMP, aZeroPage, addr);
  end else begin
    pic.codAsm(i_CMP, aAbsolute, addr);
  end;
end;
procedure TGenCodBas._DEX;
begin
  pic.codAsm(i_DEX, aImplicit, 0);
end;
procedure TGenCodBas._DEY;
begin
  pic.codAsm(i_DEY, aImplicit, 0);
end;
procedure TGenCodBas._DEC(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_DEC, aZeroPage, addr);
  end else begin
    pic.codAsm(i_DEC, aAbsolute, addr);
  end;
end;
procedure TGenCodBas._EOR(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_EOR, aZeroPage, addr);
  end else begin
    pic.codAsm(i_EOR, aAbsolute, addr);
  end;
end;
procedure TGenCodBas._EORi(const k: word);
begin
  pic.codAsm(i_EOR, aImmediat, k);
end;
procedure TGenCodBas._INC(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_INC, aZeroPage, addr);
  end else begin
    pic.codAsm(i_INC, aAbsolute, addr);
  end;
end;
procedure TGenCodBas._INX;
begin
  pic.codAsm(i_INX, aImplicit, 0);
end;
procedure TGenCodBas._INY;
begin
  pic.codAsm(i_INY, aImplicit, 0);
end;
procedure TGenCodBas._LDAi(const k: word);  //LDA Immediate
begin
  pic.codAsm(i_LDA, aImmediat, k);
end;
procedure TGenCodBas._LDA(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_LDA, aZeroPage, addr);
  end else begin
    pic.codAsm(i_LDA, aAbsolute, addr);
  end;
end;
procedure TGenCodBas._LDXi(const k: word); inline;  //LDA Immediate
begin
  pic.codAsm(i_LDX, aImmediat, k);
end;
procedure TGenCodBas._LDX(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_LDX, aZeroPage, addr);
  end else begin
    pic.codAsm(i_LDX, aAbsolute, addr);
  end;
end;
procedure TGenCodBas._LDYi(const k: word); inline;  //LDA Immediate
begin
  pic.codAsm(i_LDY, aImmediat, k);
end;
procedure TGenCodBas._LDY(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_LDY, aZeroPage, addr);
  end else begin
    pic.codAsm(i_LDY, aAbsolute, addr);
  end;
end;
procedure TGenCodBas._NOP; inline;
begin
  pic.codAsm(i_NOP, aImplicit, 0);
end;
procedure TGenCodBas._ORAi(const k: word);
begin
  pic.codAsm(i_ORA, aImmediat, k);
end;
procedure TGenCodBas._ORA(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_ORA, aZeroPage, addr);
  end else begin
    pic.codAsm(i_ORA, aAbsolute, addr);
  end;
end;
procedure TGenCodBas._PHA; inline;
begin
  pic.codAsm(i_PHA, aImplicit, 0);
end;
procedure TGenCodBas._PHP;
begin
  pic.codAsm(i_PHP, aImplicit, 0);
end;
procedure TGenCodBas._PLA;
begin
  pic.codAsm(i_PLA, aImplicit, 0);
end;
procedure TGenCodBas._PLP;
begin
  pic.codAsm(i_PLP, aImplicit, 0);
end;
procedure TGenCodBas._ROL(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_ROL, aZeroPage, addr);
  end else begin
    pic.codAsm(i_ROL, aAbsolute, addr);
  end;
end;
procedure TGenCodBas._RORa;
begin
  pic.codAsm(i_ROR, aAcumulat, 0);
end;
procedure TGenCodBas._ROR(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_ROR, aZeroPage, addr);
  end else begin
    pic.codAsm(i_ROR, aAbsolute, addr);
  end;
end;
procedure TGenCodBas._RTS; inline;
begin
  pic.codAsm(i_RTS, aImplicit, 0);
end;
procedure TGenCodBas._RTI; inline;
begin
  pic.codAsm(i_RTI, aImplicit, 0);
end;
procedure TGenCodBas._SEC; inline;
begin
  pic.codAsm(i_SEC, aImplicit, 0);
end;
procedure TGenCodBas._SED; inline;
begin
  pic.codAsm(i_SED, aImplicit, 0);
end;
procedure TGenCodBas._SBCi(const k: word); inline;  //SBC Immediate
begin
  pic.codAsm(i_SBC, aImmediat, k);
end;
procedure TGenCodBas._SBC(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_SBC, aZeroPage, addr);
  end else begin
    pic.codAsm(i_SBC, aAbsolute, addr);
  end;
end;
procedure TGenCodBas._STA(addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_STA, aZeroPage, addr);
  end else begin
    pic.codAsm(i_STA, aAbsolute, addr);
  end;
end;
procedure TGenCodBas._STX(const addr: integer);  //STA Absolute/Zeropage
begin
  if addr<256 then begin
    pic.codAsm(i_STX, aZeroPage, addr);
  end else begin
    pic.codAsm(i_STX, aAbsolute, addr);
  end;
end;
procedure TGenCodBas._STY(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_STY, aZeroPage, addr);
  end else begin
    pic.codAsm(i_STY, aAbsolute, addr);
  end;
end;
procedure TGenCodBas._TAX;
begin
  pic.codAsm(i_TAX, aImplicit, 0);
end;
procedure TGenCodBas._TAY;
begin
  pic.codAsm(i_TAY, aImplicit, 0);
end;
procedure TGenCodBas._TYA;
begin
  pic.codAsm(i_TYA, aImplicit, 0);
end;
procedure TGenCodBas._TXA;
begin
  pic.codAsm(i_TXA, aImplicit, 0);
end;
procedure TGenCodBas.IF_TRUE(OpRes: TEleExpress; out info: TIfInfo);
{Conditional instruction. Test if last expression is TRUE. In this case, execute
the following block. The syntax is:

IF_TRUE(@OpRes, info)
<block of code>
IF_END(info)

This instruction require to call to IF_END() to define the End of the block.

The block of code can be one or more instructions.
}
  procedure JumpIfZero(Invert: boolean);
  {Jump using the Z flag}
  begin
    if Invert then begin
      _BNE_post(info.igoto);
    end else begin
      _BEQ_post(info.igoto);
    end;
  end;
  procedure JumpIfZeroC(Invert: boolean);
  {Jump using the C flag}
  begin
    if Invert then begin
      _BCC_post(info.igoto);
    end else begin
      _BCS_post(info.igoto);
    end;
  end;
begin
  if OpRes.Sto = stRamFix then begin
    //Result in variable
    _LDA(OpRes.rVar.addr);
    JumpIfZero(false);  //We cannot apply optimization
  end else if OpRes.Sto = stRegister then begin
    {We first evaluate the case when it could be done an optimization}
    if lastASMcode = lacCopyCtoA then begin
      //Expression result has been copied from C to A
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check C flag
      JumpIfZeroC(true);
    end else if lastASMcode = lacInvCtoA then begin
      //Expression result has been copied from C to A inverted
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check C flag
      JumpIfZeroC(false);
    end else if lastASMcode = lacCopyZtoA then begin
      //Expression result has been copied from Z to A
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check Z flag
      JumpIfZero(true);
    end else if lastASMcode = lacInvZtoA then begin
      //Expression result has been copied from Z to A inverted
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check Z flag
      JumpIfZero(false);
    end else if lastASMcode = lacInvAtoA then begin
      //Expression result has been copied from A to A inverted, and Z reflect the regA value.
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check Z flag
      JumpIfZero(true);
    end else begin
      {Cannot be (or should be) optimized }
      if AcumStatInZ then begin
        //Still we can use the optimizaction of testing Z flag
        JumpIfZero(false);
      end else begin
        //Operand value in A but not always in Z
        _TAX;  //To update Z
        JumpIfZero(false);
      end;
    end;
  end else begin
    genError('Expression storage not supported.');
  end;
end;
//procedure TGenCodBas.IF_FALSE(OpRes: TEleExpress; out info: TIfInfo);
////Negated version of IF_TRUE()
//begin
//  OpRes.Invert;   //Change logic
//  IF_TRUE(OpRes, info);
//  OpRes.Invert;   //Restore logic
//end;
procedure TGenCodBas.IF_END(const info: TIfInfo);
{Define the End of the block, created with IF_TRUE().}
begin
  _LABEL_post(info.igoto);  //termina de codificar el salto
end;
//////////////// Tipo Boolean /////////////
procedure TGenCodBas.bool_LoadToRT(fun: TEleExpress);
begin
  case fun.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if fun.value.valBool then _LDAi(2) else _LDAi(0);
  end;
  stRamFix: begin
    _LDA(fun.rVar.addr);  //values $00 or $02
  end;
  stRegister: begin  //Already in WR
  end;
  //stVarRef, stExpRef, stVarConRef: begin
  // Must be similar to byte type
  //end
  else
    //Almacenamiento no implementado
    GenError(MSG_NOT_IMPLEM);
  end;
end;
function TGenCodBas.PICName: string;
begin
  Result := pic.Model;
end;
//////////////// Tipo Byte /////////////
procedure TGenCodBas.byte_LoadToRT(fun: TEleExpress);
{Load operand to WR. It's, convert storage to stRegister }
begin
  case fun.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    _LDAi(fun.value.valInt);
  end;
  stRamFix: begin
    _LDA(fun.rVar.addr);
  end;
  stRamVarOf: begin
    if fun.rvar.typ.IsByteSize then begin
      //Indexado por Byte
      if fun.offs<256 then begin
        _LDX(fun.rvar.addr);  //Load address
        pic.codAsm(i_LDA, aZeroPagX, fun.offs);
      end else begin
        _LDX(fun.rvar.addr);  //Load address
        pic.codAsm(i_LDA, aAbsolutX, fun.offs);
      end;
    end else if fun.rvar.typ.IsWordSize then begin
      if fun.offs<256 then begin
        AddCallerToFromCurr(IX);  //We declare using IX
        //if not IX.allocated then begin
        //  GenError(ER_NOT_IMPLEM_, [fun.StoAsStr]);
        //  exit;
        //end;
        //Escribe dirección en puntero
        _LDA(fun.rvar.addr);
        _STA(IX.addr);
        _LDA(fun.rvar.addr+1);
        _STA(IX.addr+1);
        //Carga desplazamiento
        _LDYi(fun.offs);  //Load address
        //Carga indexado
        pic.codAsm(i_LDA, aIndirecY, IX.addr);
      end else begin
        GenError(ER_NOT_IMPLEM_, [fun.StoAsStr]);
      end;
    end;
  end;
  stRegister: begin  //Already in WR
  end;
//  stVarRef, stExpRef: begin
//    if Op^.Sto = stExpRef then begin
//      idx := IX;  //Index variable
//    end else begin
//      idx := Op^.rVar;  //Index variable
//    end;
//    if idx.typ.IsByteSize then begin
//      //Indexed in zero page is simple
//      _LDX(idx.addr);
//      pic.codAsm(i_LDA, aZeroPagX, 0);
//    end else if idx.typ.IsWordSize then begin
//      if idx.addr<256 then begin
//        //Index in zero page. It's simple
//        _LDYi(0);
//        pic.codAsm(i_LDA, aIndirecY, idx.addr);
//      end else begin
//        //Index is word and not in zero page
//        //WARNING this is "Self-modifiying" code.
//        _LDA(idx.addr);  //Load LSB index
//addrNextOp1 := pic.iRam + 1;  //Address next instruction
//        pic.codAsm(i_STA, aAbsolute, 0); //Store forward
//        _LDA(idx.addr+1);  //Load virtual MSB index
//addrNextOp2 := pic.iRam + 1;  //Address next instruction
//        PIC.codAsm(i_STA, aAbsolute, 0);  //Store forward
//        //Modified LDA instruction
//        pic.codAsm(i_LDA, aAbsolute, 0); //Store forward
//        //Complete address
//        pic.ram[addrNextOp1].value := (pic.iRam - 2) and $FF;
//        pic.ram[addrNextOp1+1].value := (pic.iRam - 2)>>8;
//        pic.ram[addrNextOp2].value := (pic.iRam - 1) and $FF;
//        pic.ram[addrNextOp2+1].value := (pic.iRam - 1)>>8;
//      end;
//    end else begin
//      //refVar can only be byte or word size.
//      GenError('Not supported this index.');
//    end;
//  end;
//  stVarConRef: begin
//    idx := Op^.rVar;  //Index variable
//    off := Op^.valInt and $FFFF;
//    if idx.typ.IsByteSize then begin
//      //Indexed in zero page
//      _LDX(idx.addr);
//      if off<256 then begin
//        pic.codAsm(i_LDA, aZeroPagX, off);
//      end else begin
//        pic.codAsm(i_LDA, aAbsolutX, off);
//      end;
//    end else if idx.typ.IsWordSize then begin
//      //Index is word and not in zero page
//      //WARNING this is "Self-modifiying" code.
//      _CLC;
//      _LDA(idx.addr);  //Load LSB index
//      _ADCi(lo(off));
//addrNextOp1 := pic.iRam + 1;  //Address next instruction
//      pic.codAsm(i_STA, aAbsolute, 0); //Store forward
//      _LDA(idx.addr+1);  //Load virtual MSB index
//      _ADCi(hi(off));
//addrNextOp2 := pic.iRam + 1;  //Address next instruction
//      PIC.codAsm(i_STA, aAbsolute, 0);  //Store forward
//      //Modified LDA instruction
//      pic.codAsm(i_LDA, aAbsolute, 0); //Store forward
//      //Complete address
//      pic.ram[addrNextOp1].value := (pic.iRam - 2) and $FF;
//      pic.ram[addrNextOp1+1].value := (pic.iRam - 2)>>8;
//      pic.ram[addrNextOp2].value := (pic.iRam - 1) and $FF;
//      pic.ram[addrNextOp2+1].value := (pic.iRam - 1)>>8;
//    end else begin
//      //refVar can only be byte or word size.
//      GenError('Not supported this index.');
//    end;
//  end
  else
    //Almacenamiento no implementado
    GenError(ER_NOT_IMPLEM_, [fun.StoAsStr]);
  end;
end;
procedure TGenCodBas.byte_DefineRegisters;
begin
  //No es encesario, definir registros adicionales a A
end;
procedure TGenCodBas.byte_SaveToStk;
begin
  _PHA;
end;
//////////////// Tipo Word /////////////
procedure TGenCodBas.word_LoadToRT(fun: TEleExpress);
{Carga el valor de una expresión a los registros de trabajo.}
var
  idx: TEleVarDec;
  addrNextOp1, addrNextOp2: Integer;
begin
  case fun.Sto of  //el parámetro debe estar en "Op^"
  stConst : begin
    //byte alto
    _LDAi(fun.value.HByte);
    _STA(H.addr);
    //byte bajo
    _LDAi(fun.value.LByte);
  end;
  stRamFix: begin
    _LDA(fun.rVar.addr+1);
    _STA(H.addr);
    _LDA(fun.rVar.addr);
  end;
  stRegister: begin  //Already in (H,A)
  end;
//  stVarRef, stExpRef: begin
//    if Op^.Sto = stExpRef then begin
//      idx := IX;  //Index variable
//    end else begin
//      idx := Op^.rVar;  //Index variable
//    end;
//    if idx.typ.IsByteSize then begin
//      //Indexed in zero page is simple
//      _LDX(idx.addr);
//      _INX;  //Fail in cross-page
//      pic.codAsm(i_LDA, aZeroPagX, 0);  //MSB
//      _STA(H.addr);
//      _DEX;
//      pic.codAsm(i_LDA, aZeroPagX, 0);  //LSB
//    end else if idx.typ.IsWordSize then begin
//      if idx.addr<256 then begin
//        //Index in zero page. It's simple
//        _LDYi(1);
//        pic.codAsm(i_LDA, aIndirecY, idx.addr);  //MSB
//        _STA(H.addr);
//        _DEY;
//        pic.codAsm(i_LDA, aIndirecY, idx.addr);  //LSB
//      end else begin
//        //Index is word and not in zero page
//        //WARNING this is "Self-modifiying" code.
//        //---------- MSB ------------
//        _CLC;   //Prepare adding 1
//        _LDA(idx.addr);  //Load LSB index
//        _ADCi(1);
//addrNextOp1 := pic.iRam + 1;  //Address next instruction
//        pic.codAsm(i_STA, aAbsolute, 0); //Store forward
//        _LDA(idx.addr+1);  //Load virtual MSB index
//        _ADCi(0);   //Just to add the carry
//addrNextOp2 := pic.iRam + 1;  //Address next instruction
//        PIC.codAsm(i_STA, aAbsolute, 0);  //Store forward
//        //Modified LDA instruction
//        pic.codAsm(i_LDA, aAbsolute, 0); //Store forward
//        //Complete address
//        pic.ram[addrNextOp1].value := (pic.iRam - 2) and $FF;
//        pic.ram[addrNextOp1+1].value := (pic.iRam - 2)>>8;
//        pic.ram[addrNextOp2].value := (pic.iRam - 1) and $FF;
//        pic.ram[addrNextOp2+1].value := (pic.iRam - 1)>>8;
//        _STA(H.addr);  //Store MSB in H
//        //---------- LSB ------------
//        _LDA(idx.addr);  //Load LSB index
//addrNextOp1 := pic.iRam + 1;  //Address next instruction
//        pic.codAsm(i_STA, aAbsolute, 0); //Store forward
//        _LDA(idx.addr+1);  //Load virtual MSB index
//addrNextOp2 := pic.iRam + 1;  //Address next instruction
//        PIC.codAsm(i_STA, aAbsolute, 0);  //Store forward
//        //Modified LDA instruction
//        pic.codAsm(i_LDA, aAbsolute, 0); //LSB
//        //Complete address
//        pic.ram[addrNextOp1].value := (pic.iRam - 2) and $FF;
//        pic.ram[addrNextOp1+1].value := (pic.iRam - 2)>>8;
//        pic.ram[addrNextOp2].value := (pic.iRam - 1) and $FF;
//        pic.ram[addrNextOp2+1].value := (pic.iRam - 1)>>8;
//      end;
//    end else begin
//      //refVar can only be byte or word size.
//      GenError('Not supported this index.');
//    end;
//  end;
  else
    //Almacenamiento no implementado
    GenError(MSG_NOT_IMPLEM);
  end;
end;
procedure TGenCodBas.word_DefineRegisters;
begin
  //Changed from versión 0.7.1
  AddCallerToFromCurr(H);
end;
procedure TGenCodBas.word_SaveToStk;
begin
  //guarda A
  _PHA;
  //guarda H
  _LDA(H.addr);
  _PHA;
end;
procedure TGenCodBas.word_Low(fun: TEleExpress);
{Acceso al byte de menor peso de un word.}
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  requireA;
  case par.Sto of
  stRamFix: begin
    SetFunExpres(fun);
    if par.rvar.allocated then begin
      SetFunVariab(fun, par.addL);
    end else begin
      //We cannot set a variable yet
      SetFunExpres(fun);
      //_LDA(par.addH);
    end;
  end;
  stConst: begin
    //Se devuelve una constante
    SetFunConst_byte(fun, par.value.ValInt and $ff);
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TGenCodBas.word_High(fun: TEleExpress);
{Acceso al byte de mayor peso de un word.}
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  requireA;
  case par.Sto of
  stRamFix: begin
    if par.rvar.allocated then begin
      SetFunVariab(fun, par.addH);
    end else begin
      //We cannot set a variable yet
      SetFunExpres(fun);
      //_LDA(par.addH);
    end;
  end;
  stConst: begin
    //Se devuelve una constante
    SetFunConst_byte(fun, par.value.ValInt and $ff00 >>8);
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TGenCodBas.StopCodeGen;
{Required Stop the Code generation}
begin
  posFlash := pic.iRam; //Probably not the best way.
end;
procedure TGenCodBas.StartCodeGen;
{Required Start the Code generation}
begin
  pic.iRam := posFlash; //Probably not the best way.
end;
//Inicialización
procedure TGenCodBas.GenCodLoadToA(fun: TEleExpress);
begin
  if fun.Typ.IsByteSize then begin
    case fun.Sto. of
    stConst: begin
      _LDAi(fun.value.valInt and $ff);
    end;
    stRamFix: begin
      _LDA(fun.rVar.addr);
    end;
    stRegister: begin
      //Already in A
    end
    else
      GenError('Cannot load this operand to register A.');
    end;
  end else begin
    GenError('Operand must be byte-size to fit in register A.');
  end;
end;
procedure TGenCodBas.GenCodLoadToX(fun: TEleExpress);
begin
  if fun.Typ.IsByteSize then begin
    case fun.Sto. of
    stConst: begin
      _LDXi(fun.value.valInt and $ff);
    end;
    stRamFix: begin
      _LDX(fun.rVar.addr);
    end;
    stRegister: begin
      _TAX;
    end
    else
      GenError('Cannot load this operand to register X.');
    end;
  end else begin
    GenError('Operand must be byte-size to fit in register Y.');
  end;
end;
procedure TGenCodBas.GenCodLoadToY(fun: TEleExpress);
begin
  if fun.Typ.IsByteSize then begin
    case fun.Sto. of
    stConst: begin
      _LDYi(fun.value.valInt and $ff);
    end;
    stRamFix: begin
      _LDY(fun.rVar.addr);
    end;
    stRegister: begin
      _TAY;
    end
    else
      GenError('Cannot load this operand to register Y.');
    end;
  end else begin
    GenError('Operand must be byte-size to fit in register Y.');
  end;
end;
function TGenCodBas.CompilerName: string;
begin
  Result := 'P65Pas Compiler'
end;
function TGenCodBas.RAMmax: integer;
begin
   Result := high(pic.ram);
end;
procedure TGenCodBas.functCall(fun: TEleFunBase; out AddrUndef: boolean);
{General routine to make the call to a Normal function.}
var
  xfun: TEleFun;
begin
  //////// Make the CALL
  AddrUndef := false;
  //In linking, it's supposed all functions are implemented ?????
  if fun.idClass = eleFunc then begin
    xfun := TEleFun(fun);
  end else begin
    //Must be a declaration
    xfun := TEleFunDec(fun).implem;
  end;
  if xfun.coded then begin
    //We have a real address
    _JSR(xfun.adrr);  //It's a complete function
  end else begin
    //Function is not yet coded. We need to complete this call later.
    _JSR($0000);
    if not pic.disableCodegen then begin  //Verify if we are in mode no-code-generation.
      xfun.AddAddresPend(pic.iRam-2);  //Register the address to complete later
    end;
    AddrUndef := true;
  end;
end;
procedure TGenCodBas.ResetRAM;
{Reset the device RAM memory, and set the pointer iRam to start writing at the
beggining of the RAM.}
begin
  pic.iRam := 0;  //Ubica puntero al inicio.
  pic.ClearMemRAM;  //Pone las celdas como no usadas y elimina nombres.
end;
procedure TGenCodBas.CreateVarsAndPars;
{Create in RAM, local variables and parameters for functions.}
var
  elem   : TxpElement;
  xvar   : TEleVarDec;
  fun    : TEleFun;
begin
  //Explora primero a las funciones terminales
  for fun in usedFuncs do begin
    if not fun.IsTerminal2 then continue;
    //DebugLn('función terminal: %s con %d var.loc.', [fun.name, fun.nLocalVars]);
    //Los parámetros y variables locales aparecen como elementos de la función
    for elem in fun.elements do if elem.idClass = eleVarDec then begin  //Para todas sus variables.
      xvar := TEleVarDec(elem);
      if xvar.IsParameter or  //If function is used, we assume the all the parameters too.
         (xvar.nCalled>0) then begin
        //Asign RAM space to this variable in shared mode.
        CreateVarInRAM(xvar, true);
        if HayError then exit;
      end;
    end;
    if OptReuProVar then SetSharedUnused;   //limpia las posiciones usadas
  end;
  if OptReuProVar then SetSharedUsed;  //Ahora marca como usados, porque ya se creó la zona de bytes compartidos
  //Explora solo a las funciones que no son terminales
  for fun in usedFuncs do begin
    if fun.IsTerminal2 then continue;
    //Los parámetros y variables locales aparecen como elementos de la función
    for elem in fun.elements do if elem.idClass = eleVarDec then begin  //Para todas sus variables.
      xvar := TEleVarDec(elem);
      if xvar.IsParameter or  //If function is used, we assume the all the parameters too.
         xvar.required or      //Variable is used as a register and it's used.
         (xvar.nCalled>0) then begin
        //Asign RAM space to this variable in Normal mode.
        CreateVarInRAM(xvar, false);
        if HayError then exit;
      end;
    end;
  end;
  //Reserva espacio para las variables (Que no son de funciones).
  for xvar in TreeElems.AllVars do begin
    if xvar.Parent.idClass = eleFunc then continue;  //Las variables de funciones ya se crearon
    //if xvar.Parent.idClass = eleUnit then continue;
//debugln('Verificando: ' + xvar.name);
    if (xvar.nCalled>0) or xvar.required then begin
      //Asigna una dirección válida para esta variable
//debugln('  ubicando: ' + xvar.name);
      CreateVarInRAM(xvar, false);
      if HayError then exit;
    end else begin
      //Variable no usada
      xvar.ResetAddress;
      if xvar.Parent = TreeElems.main then begin
        //Genera mensaje solo para variables del programa principal.
        GenWarn(WA_UNUSED_VAR_, [xVar.name], xvar.srcDec);
      end;
    end;
  end;
end;
procedure TGenCodBas.codRTS(isInterrupt: boolean);
{Encodes a RTS or RTI instruction.}
begin
  if isInterrupt then _RTI else _RTS;
end;
procedure TGenCodBas.GenCodeExpr(eleExp: TEleExpress);
{Generate code for a node expression. Some expression (like operation in constant) may
not generate code. This is a recursive procedure.
Nodes otConst, must be evaluated.}
var
  funcBase: TEleFunBase;
  AddrUndef, regsUsed: boolean;
  ele: TxpElement;
  parExpr: TEleExpress;
begin
  if eleExp.opType = otFunct then begin
    //It's an expression. There should be a function
    funcBase := eleExp.rfun;
    if funcBase.codInline<>nil then begin
      //It's an INLINE function. It could be already implemented or not.
      if funcBase.idClass = eleFunc then begin
        //It's the implementation. No problem.
        regsUsed := false;  //Set flag to indicate Registers are not used.
        //Check first if it's needed to evaluate parameters.
        for ele in eleExp.elements do begin
          parExpr := TEleExpress(ele);
          GenCodeExpr(parExpr);
          //Check availability of registers
          if parExpr.opType = otFunct then begin
            //An Expression result use registers.
            if regsUsed = false then begin
              regsUsed := true;  //Set to used.
            end else begin
              //Register are already used.
              GenError('Too complex expression.', parExpr.srcDec);
              exit;
            end;
          end;
        end;
        funcBase.codInline(eleExp);   //Process function
        //Check if we can simplify
        if eleExp.opType = otConst then begin
          //Node resulted as a constant.
          if eleExp.evaluated then begin
            //We convert it to a simple constant (Constant fold).
            eleExp.elements.Clear;  //Constants don't have childrens.
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
      //Should be a Normal subroutine. Generates the CALL instruction.
      {Even though this is a Normal function, we can consider functCall() like the
      INLINE routine callback to this function, like codInline.}
      functCall(funcBase, AddrUndef);
      SetFunExpres(eleExp);
    end;
  end else if eleExp.opType = otConst then begin
    //A constant expression. We have to evaluate it, if not already evaluated.
    if not eleExp.evaluated then begin
      //It's a simple constant
      GenError('Constant not evaluated.', eleExp.srcDec);
      exit;
    end;
  end else if eleExp.opType = otVariab then begin
    //We don't need to generate code for this.
  end else begin
    GenError('Design error.');
  end;
end;
procedure TGenCodBas.GenCodeASMline(inst: TxpEleAsmLine);
{Generate code for an ASM instruction.}
var
  xvar: TEleVarDec;
  cpu_inst: TP6502Inst;
  cpu_amod: TP6502AddMode;
  elem, opdo: TxpElement;
  addressModes: TP6502AddModes;
  xfun: TEleFun;
  xcon: TEleConsDec;
  i: Integer;
  operat: TEleAsmOperat;
begin
  if compMod = cmRequire then exit;  //We don't require anything to generate an ASM instruction.
  if inst.inst>=0 then begin
    cpu_inst := TP6502Inst(inst.inst);
    cpu_amod := TP6502AddMode(inst.addMode);
    if (inst.param = -1) then begin
      //There is an expresion for the operand. We need to solve the parameter.
      opdo := inst.elements[0];  //Operand
      if opdo.idClass <> eleAsmOperand then begin
        //We don't expect this happens.
        GenError('Design error.', opdo.srcDec);
        exit;
      end;
      elem := TEleAsmOperand(opdo).elem;
      if (elem=nil) and (opdo.name = '$') then begin
        inst.param := pic.iRam;
      end else if elem.idClass = eleVarDec then begin
        xvar := TEleVarDec(elem);
        if not xvar.allocated then begin
          GenError('Variable not allocated.', elem.srcDec);
          exit;
        end;
        inst.param := xvar.addr;
      end else if elem.idClass = eleConsDec then begin
        xcon := TEleConsDec(elem);
        if not xcon.evaluated then begin
          GenError('Constant not evaluated.', elem.srcDec);
          exit;
        end;
        inst.param := xcon.value.ValInt;
      end else if elem.idClass = eleFunc then begin
        xfun := TEleFun(elem);
        if not xfun.coded then begin
          GenError('Function not coded.', elem.srcDec);
          exit;
        end;
        inst.param := xfun.adrr;
      end else begin
        GenError('Operand not evaluated.', elem.srcDec);
        exit;
      end;
      //Validates possible operations
      for i:=1 to inst.elements.Count-1 do begin
        operat := TEleAsmOperat(inst.elements[i]);
        case operat.operation of
        aopAddValue: begin
          inst.param += operat.value;
        end;
        aopSubValue: begin
          inst.param -= operat.value;
        end;
        aopSelByte: begin
          case operat.value of
          0: inst.param := inst.param and $ff;
          1: inst.param := (inst.param and $ff00)>>8;
          end ;
        end;
        end;
      end;
    end;
    //Write the instruction
    addressModes := PIC16InstName[cpu_inst].addressModes;
    if (inst.param<256) then begin
       //It could be expressed as zero-page instruction
       if (cpu_amod = aAbsolute) and (aZeroPage in addressModes) then begin
         pic.codAsm(cpu_inst, aZeroPage, inst.param);
       end else if (cpu_amod = aAbsolutX) and (aZeroPagX in addressModes) then begin
         pic.codAsm(cpu_inst, aZeroPagX, inst.param);
       end else if (cpu_amod = aAbsolutY) and (aZeroPagY in addressModes) then begin
         pic.codAsm(cpu_inst, aZeroPagY, inst.param);
       end else begin
         pic.codAsm(cpu_inst, cpu_amod, inst.param);
       end;
    end else begin
      pic.codAsm(cpu_inst, cpu_amod, inst.param);
    end;
  end else begin
    //It's not an instruction
  end;
end;
procedure TGenCodBas.GenCondeIF(sen: TxpEleSentence);
var
  expBool, expSet: TEleExpress;
  i: Integer;
  cond, ele: TxpElement;
  lbl1: TIfInfo;
  //Variables for jumps completion.
  njumps: integer;
  jumps: array of integer;
begin
  njumps := 0;
  SetLength(jumps, njumps);
  i:=0;
  while i<sen.elements.Count do begin
    //Takes condition
    cond := sen.elements[i];
    //The last expression should be the boolean condition
    expBool := TEleExpress(cond.elements[cond.elements.Count-1]);
    for ele in cond.elements do begin
      expSet := TEleExpress(ele);  //Takes assigment function or the last expression.
      GenCodeExpr(expSet);
      if HayError then exit;
    end;
    if (expBool.opType = otConst) then begin
      //Constant conditions have special behaviour.
      if (expBool.value.ValBool=false) then begin
        i+=2;  //Not processed
      end else begin
        //True expressions are the last executed.
        GenCodeBlock(TEleBlock(sen.elements[i+1]));
        break;  //No more is executed.
      end;
    end else begin
      //Not constant expressions.
      IF_TRUE(expBool, lbl1);
      GenCodeBlock(TEleBlock(sen.elements[i+1]));
      //Check if there is more conditions
      if i+2<sen.elements.Count then begin
        //There are more conditions.
        //We need to include a jump to the end
        inc(njumps);
        SetLength(jumps, njumps);  //Create new jump address
        _JMP_post(jumps[njumps-1]); //New jump to complete later
      end;
      IF_END(lbl1);
      i+=2;
    end;
  end;
  //Complete jumps
  for i:=0 to high(jumps) do begin
    _LABEL_post(jumps[i]);
  end;
end;
procedure TGenCodBas.GenCodeSentences(sentList: TxpElements);
{Generate code for a list of sentences.}
var
  eleSen, ele: TxpElement;
  sen: TxpEleSentence;
  expSet, expAsm: TEleExpress;
  inst: TxpEleAsmLine;
begin
  for eleSen in sentList do begin
    if eleSen.idClass <> eleSenten then begin
      GenError('Sentence expected.');
      exit;
    end;
    //Generates code to the sentence.
    sen := TxpEleSentence(eleSen);
    case sen.sntType of
    sntAssign: begin  //Assignment
      for ele in sen.elements do begin
        expSet := TEleExpress(ele);  //Takes assigment function.
        GenCodeExpr(expSet);
      end;
    end;
    sntProcCal: begin  //Call to function or method
      for ele in sen.elements do begin
        expSet := TEleExpress(ele);  //Takes assigment function.
        GenCodeExpr(expSet);
      end;
    end;
    sntAsmBlock: begin
      expAsm := TEleExpress(sen.elements[0]);  //Takes root node.
      for ele in expAsm.elements do begin
        inst := TxpEleAsmLine(ele);
        GenCodeASMline(inst);
      end;
    end;
    sntIF: begin
      GenCondeIF(sen);
    end
    else
      GenError('Unknown sentence type.', sen.srcDec);
      exit;
    end;
    if HayError then exit;
  end;
end;
//procedure TGenCodBas.GenCodeBody(body: TEleBody);
//{Do code generation for the body element specified. }
//begin
//  TreeElems.OpenElement(body);
//  GenCodeSentences(TreeElems.curNode.elements);
//end;
procedure TGenCodBas.GenCodeBlock(block: TEleBlock);
{Do code generation for the body element specified. }
begin
  TreeElems.OpenElement(block);
  GenCodeSentences(TreeElems.curNode.elements);
end;
constructor TGenCodBas.Create;
var
  srcPosNull: TSrcPos;
begin
  inherited Create;
  ID := 16;  //Identifica al compilador PIC16
  devicesPath := patDevices16;
  pic := TP6502.Create;
  picCore := pic;   //Referencia picCore
end;
destructor TGenCodBas.Destroy;
begin
  pic.Destroy;
  inherited Destroy;
end;

end.
//1873
