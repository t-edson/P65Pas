{Unidad que agrega campos necesarios a la clase TCompilerBase, para la generación de
código con el PIC16F.}
unit GenCodBas_PIC16;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, CPUCore, P65c02utils, CompBase, ParserDirec, CompGlobals,
  XpresElemP65, LexPas, StrUtils, LazLogger;
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
    cmConsEval   //Evaluating constant
  );
  {Información sobre los saltos con la instrucción IF_TRUE}
  TIfInfo = record
    igoto  : integer;   //Address where is GOTO
  end;
  PtrTCPURam = ^TCPURam;
  { TGenCodBas }
  TGenCodBas = class(TParserDirecBase)
  protected //Operations for parameters or Binary Operators
    function stoOperation(parA, parB: TEleExpress): TStoOperandsBSIF; inline;
    procedure Exchange(var parA, parB: TEleExpress);
    function BinOperationStr(fun: TEleExpress): string;
  private
    linRep : string;       //línea para generar de reporte
    posFlash: Integer;
    lastASMLabel: string;  //Name of a label when the last instruction was a LABEL.
    procedure GenCodeASMline(asmInst: TEleAsmInstr);
    function GenCodeCodition(cond: TxpElement): TEleExpress;
    procedure GenCodeExit(sen: TEleSentence);
    procedure GenCodLoadToA(fun: TEleExpress);  { TODO : ¿Se necesita? No se usa }
    procedure GenCodLoadToX(fun: TEleExpress);  { TODO : ¿Se necesita? No se usa }
    procedure GenCodLoadToY(fun: TEleExpress);  { TODO : ¿Se necesita? No se usa }
    procedure GenCondeIF(sen: TEleSentence);
    procedure GenCodeWHILE(sen: TEleSentence);
    procedure GenCodeFOR(sen: TEleSentence);
    procedure GenCodeREPEAT(sen: TEleSentence);
    procedure JUMP_IF_C_pre(Invert, longJump: boolean; igoto: integer);
    procedure JUMP_IF_pre(OpRes: TEleExpress; boolVal, longJump: boolean;
      igoto: integer; out relatOver: boolean);
    procedure JUMP_IF_Z_post(Invert, longJump: boolean; out curAddr: integer);
    procedure JUMP_IF_C_post(Invert, longJump: boolean; out curAddr: integer);
    procedure JUMP_IF_post(OpRes: TEleExpress; boolVal, longJump: boolean; out
      curAddr: integer);
    procedure JUMP_IF_Z_pre(Invert, longJump: boolean; igoto: integer);
    procedure BRA2JMP(var info: TIfInfo);
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
  protected
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
      offset: integer; offsetVar: TEleVarDec);
    procedure SetFunExpres(fun: TEleExpress);
    //Set result as a constant.
    procedure SetFunConst_bool(fun: TEleExpress; valBool: Boolean);
    procedure SetFunConst_byte(fun: TEleExpress; valByte: integer);
    procedure SetFunConst_char(fun: TEleExpress; valByte: integer);
    procedure SetFunConst_word(fun: TEleExpress; valWord: integer);
  protected  //Code instructions
    function _PC: word;
    function _CLOCK: integer;
    procedure _LABEL_post(igoto: integer);
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
    procedure _CPYi(const k: word);  //immidiate
    procedure _CPY(const addr: integer);  //Absolute/Zeropage
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
    procedure _LDAx(const addr: word);
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
    procedure _STA(addr: integer);        //STA Absolute/Zeropage
    procedure _STAx(addr: integer; forceAbsolute: boolean = false);       //STA X indexed
    procedure _STX(const addr: integer);  //STX Absolute/Zeropage
    procedure _STY(const addr: integer);  //STY Absolute/Zeropage
    procedure _TAX;
    procedure _TAX_opt;
    procedure _TAY;
    procedure _TYA;
    procedure _TXA;
    procedure IF_TRUE(OpRes: TEleExpress; longJump: boolean; out info: TIfInfo);
//    procedure IF_FALSE(OpRes: TEleExpress; out info: TIfInfo);
    procedure IF_END(const info: TIfInfo; out relatOver: boolean);
  protected  //Functions for types
    //////////////// Tipo Boolean /////////////
    procedure bool_LoadToRT(fun: TEleExpress);
    //////////////// Tipo Byte /////////////
    procedure byte_LoadToWR(fun: TEleExpress);
    procedure byte_DefineRegisters;
    procedure byte_SaveToStk;
    //////////////// Tipo Word /////////////
    procedure word_RequireWR;
    procedure word_LoadToWR(fun: TEleExpress);
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
  {$I _language\tra_GenCodBas.pas}
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
function TGenCodBas.stoOperation(parA, parB: TEleExpress): TStoOperandsBSIF;
begin
  //Combinación de los almacenamientos de los operandos
  Result := TStoOperandsBSIF((Ord(parA.Sto) << 4) or ord(parB.Sto));
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
//  pic.addTopLabel(lbl);  //agrega línea al código ensmblador
  pic.ram[pic.iRam].name := lbl;  //Add as cell name
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
  if (typ.catType = tctArray) and (typ.itmType = typChar) and str_nullterm then begin
    inc(nbytes);  //On more byte for the NULL character
  end;
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
  end else if xVar.adicPar.hasAdic = decZeroP then begin
    //Required to locate in Zero page.
    if not pic.GetFreeBytes(nbytes, 0, 255, startAdd) then begin
      GenError('Not free bytes in Zero page to allocate: ' + xVar.name);
      exit;
    end;
  end else if xVar.adicPar.hasAdic = decDatSec then begin
    //Required to locate in the Data section.
    if not pic.GetFreeBytes(nbytes, GeneralORG, pic.cpuMAXRAM-1, startAdd) then begin
      GenError('Not free bytes in Data section to allocate: ' + xVar.name);
      exit;
    end;
  end else begin
    //Compiler decide where to locate.
    if pic.dataAddr1<>-1 then begin   //Exist Data Zone?
      //First search in the Data zone, defined by {$SET_DATA_ADDR}
     if pic.GetFreeBytes(nbytes, pic.dataAddr1, pic.dataAddr2, startAdd) then begin
        //OK. We found a free zone.
      end else begin
        //Lets try in the Normal Data section
        if not pic.GetFreeBytes(nbytes, GeneralORG, pic.CPUMAXRAM-1, startAdd) then begin
          GenError(MSG_NO_ENOU_RAM);
          exit;
        end;
      end;
    end else begin  //No Data Zone.
      if not pic.GetFreeBytes(nbytes, GeneralORG, pic.CPUMAXRAM-1, startAdd) then begin
        GenError(MSG_NO_ENOU_RAM);
        exit;
      end;
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
    if startAdd+nbytes-1>high(pic.ram) then begin
      GenError('Cannot allocate variable: %s', [varName], xVar.srcDec);
      exit;
    end;
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
  end else if typ.IsWordSize then begin
    pic.SetNameRAM(startAdd, xVar.name + '@0');
    pic.SetNameRAM(startAdd+1, xVar.name + '@1');
  end else begin
    pic.SetNameRAM(startAdd, xVar.name);
  end;
  //Set initial value.
  if xVar.adicPar.hasInit then begin
    if outOfProgram then begin  //Only allowed in the program block
      GenError('Cannot initialize absolute variable "%s" in location $%x.',
                       [varName, startAdd], xvar.srcDec);
    end;
    //Here, we need to know the type
    WriteVaLueToRAM(@pic.ram, startAdd, typ, xVar.adicPar.constDec.value);
    if HayError then  exit;
    if (typ.catType = tctArray) and (typ.itmType = typChar) and str_nullterm then begin
      //Special case. Literal arrays of chars (strings) with Null character
      pic.ram[pic.iRam+nbytes].used := ruData;
      pic.ram[pic.iRam+nbytes].value := 0;
      inc(nBytes);
    end;
  end;
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
  if (typ.catType = tctArray) and (typ.itmType = typChar) and str_nullterm then begin
    //Special case. Literal arrays of chars (strings) with Null character
    pic.ram[pic.iRam+nbytes].used := ruData;
    pic.ram[pic.iRam+nbytes].value := 0;
    inc(nBytes);
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
{Fija los parámetros del resultado de una subexpresion. Este método se debe ejecutar,
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
  offset: integer; offsetVar: TEleVarDec);
{Set an operand TxpEleExpress to type otVariab and storage stRamVarOf.}
begin
  fun.SetVariab_RamVarOf(rVar, offset, offsetVar);
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
  fun.SetLiteralBoolConst(valBool);
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
//Codificación de instrucciones
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
procedure TGenCodBas._LABEL_post(igoto: integer);
{Finish a previous absolute jump (JMP_post), or relative jump (BNE_post, BEQ_post, ...)
instructions.}
var
  offset: integer;
begin
  if pic.ram[igoto].value = 0 then begin
    //Es salto absoluto
    pic.ram[igoto].value   := lo(_PC);
    pic.ram[igoto+1].value := hi(_PC);
  end else begin
    //Es salto relativo
    if _PC > igoto then begin
      //Salto hacia adelante
      offset := _PC - igoto-1;
      if offset>127 then begin
        GenError('Block to long.');
        exit;
      end;
      pic.ram[igoto].value := offset;
    end else begin
      //Backward jump. Does this really happens?
      offset := _PC - igoto;  //negative
      if offset<-128 then begin
        GenError('Block to long.');
        exit;
      end;
      pic.ram[igoto].value := 256 + offset;
    end;
  end;
end;
procedure TGenCodBas._LABEL_pre(out curAddr: integer);
{Set a label for a later jump BNE_pre, BEQ_pre, BCC_pre ... instructions.}
begin
  curAddr := pic.iRam;
end;
{%REGION Instrucciones simples}
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
procedure TGenCodBas._CPYi(const k: word);
begin
  pic.codAsm(i_CPY, aImmediat, k);
end;
procedure TGenCodBas._CPY(const addr: integer);
begin
  if addr<256 then begin
    pic.codAsm(i_CPY, aZeroPage, addr);
  end else begin
    pic.codAsm(i_CPY, aAbsolute, addr);
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
procedure TGenCodBas._LDAx(const addr: word);
{Generate the LDA with addressing aZeroPagX or aAbsolutX}
begin
  if addr<256 then begin
    pic.codAsm(i_LDA, aZeroPagX, addr);
  end else begin
    pic.codAsm(i_LDA, aAbsolutX, addr);
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
procedure TGenCodBas._STAx(addr: integer; forceAbsolute: boolean = false);
begin
  if forceAbsolute then begin
    pic.codAsm(i_STA, aAbsolutX, addr);
  end else begin
    if addr<256 then begin
      pic.codAsm(i_STA, aZeroPagX, addr);
    end else begin
      pic.codAsm(i_STA, aAbsolutX, addr);
    end;
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
procedure TGenCodBas._TAX_opt;
{TAX version that delete the possible sequence TXA-TAX}
var
  ramcell : ^TCPURamCell;
begin
  ramcell := @pic.ram[pic.iRam-1];
  if RemUnOpcod and (ramcell^.used = ruCodeOp) and (ramcell^.value = $8A) then begin
    //We have a TXA before.
    pic.iRam := pic.iRam-1;
  end else begin
    _TAX;          //Save A in X
  end;
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
{%ENDREGION}
procedure TGenCodBas.JUMP_IF_Z_pre(Invert, longJump: boolean; igoto: integer);
{Jump using the Z flag. Jump if Z is set.}
begin
  if longJump then begin          //Long jump
      if Invert then begin
        _BEQ(3);
        _JMP(igoto);
      end else begin
        _BNE(3);
        _JMP(igoto);
      end;
  end else begin
      if Invert then begin
        _BNE(igoto - _PC - 2);
      end else begin
        _BEQ(igoto - _PC - 2);
      end;
  end;
end;
procedure TGenCodBas.JUMP_IF_C_pre(Invert, longJump: boolean; igoto: integer);
{Jump using the C flag. Jump if C is set.}
begin
  if longJump then begin          //Long jump
      if Invert then begin
        _BCS(3);
        _JMP(igoto);
      end else begin
        _BCC(3);
        _JMP(igoto);
      end;
  end else begin
      if Invert then begin
        _BCC(igoto - _PC - 2);
      end else begin
        _BCS(igoto - _PC - 2);
      end;
  end;
end;
procedure TGenCodBas.JUMP_IF_Z_post(Invert, longJump: boolean; out curAddr: integer);
{Jump using the Z flag. Jump if Z is set.
If "longJump" is set it generates a long jump (more than 128 bytes). }
begin
  if longJump then begin          //Long jump
      if Invert then begin
        _BEQ(3);
        _JMP_post(curAddr);
      end else begin
        _BNE(3);
        _JMP_post(curAddr);
      end;
  end else begin
      if Invert then begin
        _BNE_post(curAddr);
      end else begin
        _BEQ_post(curAddr);
      end;
  end;
end;
procedure TGenCodBas.JUMP_IF_C_post(Invert, longJump: boolean; out curAddr: integer);
{Jump using the C flag. Jump if C is set.
If "longJump" is set it generates a long jump (more than 128 bytes). }
begin
  if longJump then begin          //Long jump
      if Invert then begin
        _BCS(3);
        _JMP_post(curAddr);
      end else begin
        _BCC(3);
        _JMP_post(curAddr);
      end;
  end else begin
      if Invert then begin
        _BCC_post(curAddr);
      end else begin
        _BCS_post(curAddr);
      end;
  end;
end;
procedure TGenCodBas.JUMP_IF_pre(OpRes: TEleExpress; boolVal, longJump: boolean;
                                 igoto: integer; out relatOver: boolean);
{Jump to a pre label, if the last operand "OpRes" returned a boolean result equal to
"boolVal".
If "longJump" is set it generates a long jump (more than 128 bytes). }
var
  offset: Integer;
begin
  if longJump then begin
    //In lonj jumps, we won't have overflow
    relatOver := false;
  end else begin
    //For short jumps, we need to verifiy the ffset
    offset := _PC-igoto + 2;
    if offset>127 then begin
      relatOver := true;
      exit;
    end;
  end;
  if OpRes.Sto = stRamFix then begin
    //Result in variable
    _LDA(OpRes.rVar.addr);
    JUMP_IF_Z_pre(boolVal, longJump, igoto);  //We cannot apply optimization
  end else if OpRes.Sto = stRegister then begin
    {We first evaluate the case when it could be done an optimization}
    if lastASMcode = lacCopyCtoA then begin
      //Expression result has been copied from C to A
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check C flag
      JUMP_IF_C_pre(not boolVal, longJump, igoto);
    end else if lastASMcode = lacInvCtoA then begin
      //Expression result has been copied from C to A inverted
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check C flag
      JUMP_IF_C_pre(boolVal, longJump, igoto);
    end else if lastASMcode = lacCopyZtoA then begin
      //Expression result has been copied from Z to A
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check Z flag
      JUMP_IF_Z_pre(not boolVal, longJump, igoto);
    end else if lastASMcode = lacInvZtoA then begin
      //Expression result has been copied from Z to A inverted
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check Z flag
      JUMP_IF_Z_pre(boolVal, longJump, igoto);
    end else if lastASMcode = lacInvAtoA then begin
      //Expression result has been copied from A to A inverted, and Z reflect the regA boolVal.
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check Z flag
      JUMP_IF_Z_pre(not boolVal, longJump, igoto);
    end else begin
      {Cannot be (or should be) optimized }
      if AcumStatInZ then begin
        //Still we can use the optimizaction of testing Z flag
        JUMP_IF_Z_pre(boolVal, longJump, igoto);
      end else begin
        //Operand boolVal in A but not always in Z
        _TAX;  //To update Z
        JUMP_IF_Z_pre(boolVal, longJump, igoto);
      end;
    end;
  end else begin
    genError('Expression storage not supported.');
  end;
end;
procedure TGenCodBas.JUMP_IF_post(OpRes: TEleExpress; boolVal, longJump: boolean;
                                  out curAddr: integer);
{Jump to a post label, if the last operand "OpRes" returned a boolean result equal to
"boolVal".
If "longJump" is set it generates a long jump (more than 128 bytes). }
begin
  if OpRes.Sto = stRamFix then begin
    //Result in variable
    _LDA(OpRes.rVar.addr);
    JUMP_IF_Z_post(boolVal, longJump, curAddr);  //We cannot apply optimization
  end else if OpRes.Sto = stRegister then begin
    {We first evaluate the case when it could be done an optimization}
    if lastASMcode = lacCopyCtoA then begin
      //Expression result has been copied from C to A
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check C flag
      JUMP_IF_C_post(not boolVal, longJump, curAddr);
    end else if lastASMcode = lacInvCtoA then begin
      //Expression result has been copied from C to A inverted
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check C flag
      JUMP_IF_C_post(boolVal, longJump, curAddr);
    end else if lastASMcode = lacCopyZtoA then begin
      //Expression result has been copied from Z to A
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check Z flag
      JUMP_IF_Z_post(not boolVal, longJump, curAddr);
    end else if lastASMcode = lacInvZtoA then begin
      //Expression result has been copied from Z to A inverted
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check Z flag
      JUMP_IF_Z_post(boolVal, longJump, curAddr);
    end else if lastASMcode = lacInvAtoA then begin
      //Expression result has been copied from A to A inverted, and Z reflect the regA boolVal.
      pic.iRam := lastASMaddr;   //Delete last instructions
      //Check Z flag
      JUMP_IF_Z_post(not boolVal, longJump, curAddr);
    end else begin
      {Cannot be (or should be) optimized }
      if AcumStatInZ then begin
        //Still we can use the optimizaction of testing Z flag
        JUMP_IF_Z_post(boolVal, longJump, curAddr);
      end else begin
        //Operand boolVal in A but not always in Z
        _TAX;  //To update Z
        JUMP_IF_Z_post(boolVal, longJump, curAddr);
      end;
    end;
  end else begin
    genError('Expression storage not supported.');
  end;
end;
procedure TGenCodBas.IF_TRUE(OpRes: TEleExpress; longJump: boolean; out info: TIfInfo);
{Conditional instruction. Test if last expression is TRUE. In this case, execute
the following block. The syntax is:

IF_TRUE(OpRes, info)
  <block of code>
IF_END(info)

This instruction require to call to IF_END() to define the End of the block.

The block of code can be one or more instructions.
}
begin
  JUMP_IF_post(OpRes, false, longJump, info.igoto);
end;
procedure TGenCodBas.IF_END(const info: TIfInfo; out relatOver: boolean);
{Define the End of the block, created with IF_TRUE().
Note the similarity with _LABEL_post().}
var
  offset, igoto: integer;
begin
  igoto := info.igoto;
  relatOver := false;
  if pic.ram[igoto].value = 0 then begin
    //Es salto absoluto
    pic.ram[igoto].value   := lo(_PC);
    pic.ram[igoto+1].value := hi(_PC);
  end else begin
    //Es salto relativo. Salto hacia adelante
    offset := _PC - igoto-1;
    if offset>127 then begin
      relatOver := true;
      exit;
    end;
    pic.ram[igoto].value := offset;
  end;
end;
procedure TGenCodBas.BRA2JMP(var info: TIfInfo);
{Change a relative jump (BEQ, BNE, BCS o BCC) of the form:
  BNE <offset>
To an absolute (long) jump:
  BEQ <+3>
  JMP <offset>
To generate the new code, the all jump instruction if overwritten and the pointer "iram"
is set to the next free position after the new junp instructions.
The IN/OUT parameter "info" give the addres (info.igoto) of the parameter of the relative
jump (BNE).
Field "info.igoto" will be set to the absolute jump after finishing this
procedure.}
var
  ramcell: TCPURamCellPtr;
begin
  ramcell := @pic.ram[info.igoto-1];  //Read Jump Opcode
  pic.iRam := info.igoto-1;    //Go to the start of the Opcode
  if (ramcell^.value = $F0) then begin  //BEQ
    _BNE(3);
    _JMP_post(info.igoto);
  end else if (ramcell^.value = $D0) then begin  //BNE
    _BEQ(3);
    _JMP_post(info.igoto);
  end else if (ramcell^.value = $90) then begin  //BCC
    _BCS(3);
    _JMP_post(info.igoto);
  end else if (ramcell^.value = $B0) then begin  //BCS
    _BCC(3);
    _JMP_post(info.igoto);
  end else begin
    GenError('Unsupported branch Opcode.');
  end;
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
procedure TGenCodBas.byte_LoadToWR(fun: TEleExpress);
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
procedure TGenCodBas.word_RequireWR;
{Generate de callings to Work Register used when loading a Word in Work registers.}
begin
  AddCallerToFromCurr(H);
end;
procedure TGenCodBas.word_LoadToWR(fun: TEleExpress);
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
      _TAX_opt;
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
    if funcBase.callType = ctSysInline then begin
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
        funcBase.codSysInline(eleExp);   //Process function
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
        GenError('No support for unimplemented INLINE functions.');
      end;
    end else if funcBase.callType in [ctSysNormal, ctUsrNormal] then begin
      //Should be a Normal subroutine. Generates the CALL instruction.
      {Even though this is a Normal function, we can consider functCall() like the
      INLINE routine callback to this function, like codInline.}
      functCall(funcBase, AddrUndef);
      SetFunExpres(eleExp);
    end else begin
      GenError('Unsupported.');
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
procedure TGenCodBas.GenCodeASMline(asmInst: TEleAsmInstr);
{Generate code for an ASM instruction (element TEleAsmInstr).}
  function ReadOperandValueRef(paramRef: TxpElement): integer;
  {Read the value of a Operand when it's a reference to an element.}
  var
    xvar: TEleVarDec;
    xcon: TEleConsDec;
    xfun: TEleFun;
    instTarget: TEleAsmInstr;
  begin
    Result := 0;
    if paramRef.idClass = eleVarDec then begin
      xvar := TEleVarDec(paramRef);
      if not xvar.allocated then begin
        GenError('Variable not allocated.', paramRef.srcDec);
        exit;
      end;
      Result := xvar.addr;
    end else if paramRef.idClass = eleConsDec then begin
      xcon := TEleConsDec(paramRef);
      if not xcon.evaluated then begin
        GenError('Constant not evaluated.', paramRef.srcDec);
        exit;
      end;
      Result := xcon.value^.ValInt;
    end else if paramRef.idClass = eleFunc then begin
      xfun := TEleFun(paramRef);
      if not xfun.coded then begin
        GenError('Function not coded.', paramRef.srcDec);
        exit;
      end;
      Result := xfun.adrr;
    end else if paramRef.idClass = eleAsmInstr then begin
      //Referencia a una instrucción ASM. Tal vez una etiqueta o DB, DW
      instTarget := TEleAsmInstr(paramRef);  //Instrucción destino
      if instTarget.addr=-1 then begin
        //La etiqueta aún no ha sido mapeada en memoria
        {Define una posición tentativa, considerando que la etiqueta referenciada debe
        estar más adelante. Luego se completará cuando se defina la etiqueta.}
        Result := pic.iRam+3;
      end else begin
        Result := instTarget.addr;  //Toma su dirección.
      end;
    end else begin
      GenError('Invalid Opcode operand.', paramRef.srcDec);
      exit;
    end;
  end;
  procedure ReadOperandValue(out operRef: TxpElement; out operVal: integer);
  {Read the value of an instruction Operand in "operVal".
  "operRef" returns the reference to the element when operand is an "element operand",
  otherwise returns NIL.}
  begin
    if (asmInst.operVal = -1) then begin
      //There is an expresion for the operand. We need to solve the parameter.
      operRef := asmInst.operRef;
      //Resolve operand value
      operVal := ReadOperandValueRef(operRef);
      if HayError then exit;
    end else if (asmInst.operVal = -2) then begin
      //Operand is '$'
      operRef := nil;
      operVal :=  pic.iRam;
    end else begin
      //Operand can be read directly
      operRef := nil;
      operVal := asmInst.operVal;
    end;
  end;
  procedure ApplyOperations(operRef: TxpElement; operations: TxpElements; var operVal: integer);
  {Apply the operations to the parameter "operVal"}
  var
    i: Integer;
    operat: TEleAsmOperat;
  begin
    for i:=0 to operations.Count-1 do begin
      operat := TEleAsmOperat(operations[i]);
      case operat.operation of
      aopAddValue: begin
        operVal += operat.value;
      end;
      aopSubValue: begin
        operVal -= operat.value;
      end;
      aopSelByte: begin
        case operat.value of
        0:  //Low byte
          operVal := operVal and $ff;
        1:  //High byte
          if operRef = nil then  begin
            //No hay referencia a operando.
            operVal := (operVal and $ff00)>>8
          end else if operRef.idClass = eleConsDec then  begin
            //En constantes tomamos el byte alto
            operVal := (operVal and $ff00)>>8
          end else begin
            //Para variables o funciones, tomamos la siguiente dirección
            //operVal := operVal+1;
            operVal := (operVal and $ff00)>>8
          end;
        end;
      end;
      end;
    end;
  end;
  procedure WriteInstruction(cpu_inst: TP6502Inst; cpu_amod: TP6502AddMode; param: integer);
  {Codifica la instrucción a partir de la posiicón actual de la RAM.
  Se debe haber ya definido: "param" }
  var
    addressModes: TP6502AddModes;
    offset: Integer;
  begin
    addressModes := PIC16InstName[cpu_inst].addressModes;
    //debugln('iRam = ' + IntToStr(pic.iRam));
    if cpu_amod = aRelative then begin  //Instrucciones de salto relativo
      offset := param-pic.iRam-2;
      { TODO : Validar si el salto es mayor a 127 o menor a -128 }
      pic.codAsm(cpu_inst, aRelative, word(offset));
    end else if (param<256) then begin
      //It could be expressed as zero-page instruction
      if (cpu_amod = aAbsolute) and (aZeroPage in addressModes) then begin
        pic.codAsm(cpu_inst, aZeroPage, param);
      end else if (cpu_amod = aAbsolutX) and (aZeroPagX in addressModes) then begin
        pic.codAsm(cpu_inst, aZeroPagX, param);
      end else if (cpu_amod = aAbsolutY) and (aZeroPagY in addressModes) then begin
        pic.codAsm(cpu_inst, aZeroPagY, param);
      end else begin
        pic.codAsm(cpu_inst, cpu_amod, param);
      end;
    end else begin
      pic.codAsm(cpu_inst, cpu_amod, param);
    end;
  end;
var
  cpu_inst    : TP6502Inst;
  cpu_amod    : TP6502AddMode;
  operRef     : TxpElement;
  finalOperVal: Integer;
begin
  if asmInst.iType = itOpcode then begin   //Instrucción normal.
    //Calculate the final Opcode operand parameter.
    ReadOperandValue(operRef, finalOperVal);
    //Validates possible operations to the operand
    ApplyOperations(operRef, asmInst.elements, finalOperVal);
    //Write the instruction
    asmInst.addr := pic.iRam;   //Set address
    cpu_inst := TP6502Inst(asmInst.opcode);
    cpu_amod := TP6502AddMode(asmInst.addMode);
    WriteInstruction(cpu_inst, cpu_amod, finalOperVal);
    lastASMLabel := '';
  end else if asmInst.iType = itLabel then begin  //Instrucción etiqueta.
    asmInst.addr := pic.iRam;   //Actualiza dirección actual
    lastASMLabel := asmInst.name;
  end else if asmInst.iType = itOrgDir then begin  //Instrucción ORG.
    //Calculate the final Opcode operand parameter.
    ReadOperandValue(operRef, finalOperVal);
    //Validates possible operations to the operand
    ApplyOperations(operRef, asmInst.elements, finalOperVal);
    pic.iRam := finalOperVal;   //Actualiza dirección actual
    lastASMLabel := '';
  end else if asmInst.iType = itDefByte then begin  //Instrucción DB.
    //Calculate the final Opcode operand parameter.
    ReadOperandValue(operRef, finalOperVal);
    //Validates possible operations to the operand
    ApplyOperations(operRef, asmInst.elements, finalOperVal);
    pic.codByte(finalOperVal and $ff, ruData, lastASMLabel);
    lastASMLabel := '';
  end else if asmInst.iType = itDefWord then begin  //Instrucción DW.
    //Calculate the final Opcode operand parameter.
    ReadOperandValue(operRef, finalOperVal);
    //Validates possible operations to the operand
    ApplyOperations(operRef, asmInst.elements, finalOperVal);
    pic.codByte(finalOperVal and $ff, ruData, lastASMLabel);
    pic.codByte((finalOperVal >> 8) and $ff, ruData, '');
    lastASMLabel := '';
  end else begin
    //It's not an instruction
    GenError('Inalid ASM instruction.', asmInst.srcDec);
    exit;
  end;
end;
function TGenCodBas.GenCodeCodition(cond: TxpElement): TEleExpress;
{Generates code for a condition Block.
Returns the boolean expression inside the condition.
If an Error occurs returns FALSE.
There should be at least one Expression in "cond" and "cond" must be a TEleCondit
element. We won't check here.}
var
  expSet: TEleExpress;
  ele: TxpElement;
begin
  //The last expression should be the boolean condition
  Result := TEleExpress(cond.elements[cond.elements.Count-1]);
  //Boolean type has been checked in Analysis.
  for ele in cond.elements do begin
    expSet := TEleExpress(ele);  //Takes assigment function or the last expression.
    GenCodeExpr(expSet);
    if HayError then exit;
  end;
end;
procedure TGenCodBas.GenCondeIF(sen: TEleSentence);
var
  expBool: TEleExpress;
  i: Integer;
  lbl1: TIfInfo;
  //Variables for jumps completion.
  njumps: integer;
  jumps: array of integer;
  relatOver: boolean;
begin
  njumps := 0;
  SetLength(jumps, njumps);
  i:=0;
  while i<sen.elements.Count do begin
    //Takes condition
    expBool := GenCodeCodition(sen.elements[i]);
    if HayError then exit;
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
      //Check if we need to create space for a jump.
      if i+2<sen.elements.Count then begin  //There are more conditions.
        //We creates space for a JMP instruction
        inc(njumps);
        SetLength(jumps, njumps);  //Create new jump address
      end;
      //Creates the tentative conditional using short jumps
      IF_TRUE(expBool, false, lbl1);
      GenCodeBlock(TEleBlock(sen.elements[i+1]));
      if i+2<sen.elements.Count then begin  //There are more conditions.
        //We need to include a jump to the end
        _JMP_post(jumps[njumps-1]); //New jump to complete later
      end;
      IF_END(lbl1, relatOver);
      if relatOver then begin
        //GenError('Block to long.', sen.srcDec);
        //Block to long for short jump. We recompile using a long block.
        BRA2JMP(lbl1);  //We cannot use IF_TRUE() again because probably IF_TRUE() has made some optimization (delete Opcodes) before of generate the jump instruction.
        GenCodeBlock(TEleBlock(sen.elements[i+1]));
        if i+2<sen.elements.Count then begin  //There are more conditions.
          //We need to include a jump to the end
          _JMP_post(jumps[njumps-1]); //New jump to complete later
        end;
        IF_END(lbl1, relatOver);
      end;
      i+=2;
    end;
  end;
  //Complete jumps
  for i:=0 to high(jumps) do begin
    _LABEL_post(jumps[i]);
  end;
end;
procedure TGenCodBas.GenCodeWHILE(sen: TEleSentence);
{Compila una extructura WHILE}
var
  lbl1: Word;
  expBool: TEleExpress;
  lbl2: TIfInfo;
  relatOver: boolean;
begin
  lbl1 := _PC;        //guarda dirección de inicio
  expBool := GenCodeCodition(sen.elements[0]);
  if HayError then exit;
  //Aquí debe estar el cuerpo del "while"
  if (expBool.opType = otConst) then begin
    if (expBool.value.ValBool=false) then begin
      //We don't need to process body.
    end else begin
      //Infinite loop
      GenCodeBlock(TEleBlock(sen.elements[1]));
      _JMP(lbl1);
    end;
  end else begin  //otVariab. otFunct
    IF_TRUE(expBool, false, lbl2);
    GenCodeBlock(TEleBlock(sen.elements[1]));
    _JMP(lbl1);   //salta a evaluar la condición
    IF_END(lbl2, relatOver);
    if relatOver then begin
      //GenError('Block to long.');
      BRA2JMP(lbl2);
      GenCodeBlock(TEleBlock(sen.elements[1]));
      _JMP(lbl1);   //salta a evaluar la condición
      IF_END(lbl2, relatOver);
    end;
    //ya se tiene el destino del salto
    //_LABEL_post(dg);   //Termina de codificar el salto
  end;
end;
procedure TGenCodBas.GenCodeFOR(sen: TEleSentence);
var
  assign, ele: TxpElement;
  expSet, expBool: TEleExpress;
  lbl1: Word;
  lbl2: TIfInfo;
  relatOver: boolean;
begin
  //Generate code for the assigment
  assign := sen.elements[0];
  for ele in assign.elements do begin
    expSet := TEleExpress(ele);  //Takes assigment function.
    GenCodeExpr(expSet);
  end;
  //Condition
  lbl1 := _PC;        //guarda dirección de inicio
  expBool := GenCodeCodition(sen.elements[1]);
  if HayError then exit;
  //Aquí debe estar el cuerpo del "for"
  if (expBool.opType = otConst) then begin
    if (expBool.value.ValBool=false) then begin
      //We don't need to process body.
    end else begin
      //Infinite loop
      GenCodeBlock(TEleBlock(sen.elements[2]));
      _JMP(lbl1);
    end;
  end else begin  //otVariab, otFunct
    IF_TRUE(expBool, false, lbl2);
    GenCodeBlock(TEleBlock(sen.elements[2]));
    _JMP(lbl1);   //salta a evaluar la condición
    IF_END(lbl2, relatOver);
    if relatOver then begin
      //GenError('Block to long.');
      BRA2JMP(lbl2);
      GenCodeBlock(TEleBlock(sen.elements[2]));
      _JMP(lbl1);   //salta a evaluar la condición
      IF_END(lbl2, relatOver);
    end;
  end;

end;
procedure TGenCodBas.GenCodeREPEAT(sen: TEleSentence);
var
  lbl1: Word;
  expBool: TEleExpress;
  relatOver: boolean;
begin
  lbl1 := pic.iRam;        //guarda dirección de inicio
  //Compile Body
  GenCodeBlock(TEleBlock(sen.elements[0]));
  //Compile condiiton
  expBool := GenCodeCodition(sen.elements[1]);
  if HayError then exit;
  if (expBool.opType = otConst) then begin
    if (expBool.value.ValBool=true) then begin
      //A common block.
    end else begin
      //Infinite loop
      _JMP(lbl1);
    end;
  end else begin  //otVariab. otFunct
    JUMP_IF_pre(expBool, false, false, lbl1, relatOver);
    if relatOver then begin
      //Let's use long jumps
//      GenError('Block too long.', sen.srcDec);
//      exit;
      pic.iRam := lbl1; //Lets to the begin to compile loop again.
      //Compile Body
      GenCodeBlock(TEleBlock(sen.elements[0]));
      //Compile condition
      expBool := GenCodeCodition(sen.elements[1]);
      JUMP_IF_pre(expBool, false, true, lbl1, relatOver);
    end;
  end;
end;
procedure TGenCodBas.GenCodeExit(sen: TEleSentence);
{Se debe dejar en los registros de trabajo, el valor del parámetro indicado.}
var
  curFun: TEleFun;
  par, expSet: TEleExpress;
  parentNod: TxpElement;
  ele: TxpElement;
begin
  //TreeElems.curNode, debe ser de tipo "Body".
  if sen.elements.Count=0 then begin
    //There isn't an expression.
    _RTS;
  end else begin
    //There is an expression.
    //It's supposed to be a function. We don't validate here. It's been done in Analyze.
    parentNod := TreeElems.curCodCont.Parent;
    if parentNod.idClass <> eleFunc then begin  //Shouldn't happen
      GenError('Design error.');
      exit;
    end;
    curFun := TEleFun(parentNod);
    //Generate code for evaluating all possible expressions
    for ele in sen.elements do begin
      expSet := TEleExpress(ele);  //Takes assigment function or the last expression.
      GenCodeExpr(expSet);
      if HayError then exit;
    end;
    //par := TEleExpress(sen.elements[0]);  //Only one parameter
    par := TEleExpress(sen.elements[sen.elements.Count-1]);  //The last expression
    //El resultado de la expresión está en "par".
    LoadToWR(par);  //Carga expresión en WR y genera RTS
    _RTS;
  end;
end;
procedure TGenCodBas.GenCodeSentences(sentList: TxpElements);
{Generate code for a list of sentences.}
var
  eleSen, ele: TxpElement;
  sen: TEleSentence;
  expSet: TEleExpress;
  inst: TEleAsmInstr;
  asmBlock: TEleAsmBlock;
  idCtx, rowCtx, tmp: Integer;
  srcLin: String;
  blk: TEleBlock;
begin
//  ShowContexts;
//  ShowCurContInformat;
  for eleSen in sentList do begin
    if eleSen.idClass = eleSenten then begin
      //Generates code to the sentence.
      sen := TEleSentence(eleSen);
      if asmIncComm then begin
        {Genera los comentarios por instrucción, accediendo al contenido del
        código fuente a través del contexto al que apunta cada instrucción. }
        idCtx  := sen.srcDec.idCtx;
        rowCtx := sen.srcDec.row-1;
        srcLin := ctxList[idCtx].curLines[rowCtx];  {Podría fallar si el contenido del
         archivo no se encuentra en "curLines". El scanner podría usar otro almacenamiento.
         Habría que analizar mejor cuál es el acceso correcto al contenido fuente.}
        pic.addTopComm('    ;' + trim(srcLin));
        //MsgBox(srcLin);
      end;
      //Identifica a la sentencia
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
        asmBlock := TEleAsmBlock(sen.elements[0]);  //Takes root node.
        for ele in asmBlock.elements do begin
          inst := TEleAsmInstr(ele);
          GenCodeASMline(inst);
        end;
        //Remains to complete uncomplete instructions
        tmp := pic.iRam;  //Save
        for inst in asmBlock.undefInstrucs do begin
          pic.iRam := inst.addr;   //Set at its original RAM position
          GenCodeASMline(inst);    //Overwrite the code to complete
          { TODO : Sería mejor analizar si podría darse el caso de que la nueva instrucción
          tenga un tamaño diferente a la grabada anteriormente. De ser así, habría
          un grave error. }
        end;
        pic.iRam := tmp;   //Restore
      end;
      sntIF: begin
        GenCondeIF(sen);
      end;
      sntWHILE: begin
        GenCodeWHILE(sen);
      end;
      sntFOR: begin
        GenCodeFOR(sen);
      end;
      sntREPEAT: begin
        GenCodeREPEAT(sen);
      end;
      sntExit: begin
        GenCodeExit(sen);
      end;
      else
        GenError('Unknown sentence type.', sen.srcDec);
        exit;
      end;
      if HayError then exit;
    end else if eleSen.idClass = eleBlock then begin
      blk := TEleBlock(eleSen);
      GenCodeBlock(blk);
    end else begin
      GenError('Sentence expected.');
      exit;
    end;
  end;
end;
procedure TGenCodBas.GenCodeBlock(block: TEleBlock);
{Do code generation for the body element specified. }
begin
  if block.idClass <> eleBlock then begin
    GenError('Internal error. Block expected.', block.srcDec);
    exit;
  end;
  TreeElems.OpenElement(block);
  GenCodeSentences(TreeElems.curNode.elements);
end;
constructor TGenCodBas.Create;
var
  srcPosNull: TSrcPos;
begin
  inherited Create;
  ID := 16;  //Identifica al compilador PIC16
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
