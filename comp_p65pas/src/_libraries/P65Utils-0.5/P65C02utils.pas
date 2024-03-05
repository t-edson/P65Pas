{
Description
===========
Utilities for the 6502 CPU. The types here defined are intended to be used for:
* Assembling 6502 instructions.
* Disassembling 6502 instructions.
* Simulating the 6502 execution.
To simulate the CPU, it's assumed there are 64KB of RAM in a virtual system.
The main class TP6502 models a CPU6502 object including access to 64KB RAM.
The aim of this unit is to be used as base for assemblers, compilers and simulators.

                                         Created by Tito Hinostroza   19/05/2018
}

unit P65C02utils;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, CPUCore, strutils;
type  //Instructions set
  TP6502Inst = (
    i_ADC,  //add with carry
    i_AND,  //and (with accumulator)
    i_ASL,  //arithmetic shift left
    i_BCC,  //branch on carry clear
    i_BCS,  //branch on carry set
    i_BEQ,  //branch on equal (zero set)
    i_BIT,  //bit test
    i_BMI,  //branch on minus (negative set)
    i_BNE,  //branch on not equal (zero clear)
    i_BPL,  //branch on plus (negative clear)
    i_BRK,  //break / interrupt
    i_BVC,  //branch on overflow clear
    i_BVS,  //branch on overflow set
    i_CLC,  //clear carry
    i_CLD,  //clear decimal
    i_CLI,  //clear interrupt disable
    i_CLV,  //clear overflow
    i_CMP,  //compare (with accumulator)
    i_CPX,  //compare with X
    i_CPY,  //compare with Y
    i_DEC,  //decrement
    i_DEX,  //decrement X
    i_DEY,  //decrement Y
    i_EOR,  //exclusive or (with accumulator)
    i_INC,  //increment
    i_INX,  //increment X
    i_INY,  //increment Y
    i_JMP,  //jump
    i_JSR,  //jump subroutine
    i_LDA,  //load accumulator
    i_LDX,  //load X
    i_LDY,  //load Y
    i_LSR,  //logical shift right
    i_NOP,  //no operation
    i_ORA,  //or with accumulator
    i_PHA,  //push accumulator
    i_PHP,  //push processor status (SR)
    i_PLA,  //pull accumulator
    i_PLP,  //pull processor status (SR)
    i_ROL,  //rotate left
    i_ROR,  //rotate right
    i_RTI,  //return from interrupt
    i_RTS,  //return from subroutine
    i_SBC,  //subtract with carry
    i_SEC,  //set carry
    i_SED,  //set decimal
    i_SEI,  //set interrupt disable
    i_STA,  //store accumulator
    i_STX,  //store X
    i_STY,  //store Y
    i_TAX,  //transfer accumulator to X
    i_TAY,  //transfer accumulator to Y
    i_TSX,  //transfer stack pointer to X
    i_TXA,  //transfer X to accumulator
    i_TXS,  //transfer X to stack pointer
    i_TYA,  //transfer Y to accumulator

    i_BRA,
    i_STZ,
    i_PHX,
    i_PHY,
    i_PLX,
    i_PLY,
    i_TRB,
    i_TSB,
    i_STP,
    i_WAI,

    I_RMB0,
    I_RMB1,
    I_RMB2,
    I_RMB3,
    I_RMB4,
    I_RMB5,
    I_RMB6,
    I_RMB7,

    I_SMB0,
    I_SMB1,
    I_SMB2,
    I_SMB3,
    I_SMB4,
    I_SMB5,
    I_SMB6,
    I_SMB7,

    I_BBR0,
    I_BBR1,
    I_BBR2,
    I_BBR3,
    I_BBR4,
    I_BBR5,
    I_BBR6,
    I_BBR7,

    I_BBS0,
    I_BBS1,
    I_BBS2,
    I_BBS3,
    I_BBS4,
    I_BBS5,
    I_BBS6,
    I_BBS7,
  //INVALID INSTRUCTION
    i_Inval
  );
  //Addressing Modes
  {Implicit Mode and Acumulator Mode are not considered here. We consider
   Only Modes with parameters.}
  TP6502AddMode = (
    aImplicit,  //Implied         : BRK
    aAcumulat,  //Acumulator      : ASL
    aImmediat,  //Immediate       : ORA #$B2
    aAbsolute,  //Absolute        : JMP $4032
    aZeroPage,  //Zero page       : LDA: $35
    aRelative,  //Relative        : BNE LABEL
    aIndirect,  //Indirect        : JMP ($1000)
    aAbsolutX,  //Absolute Indexed by X  : STA $1000, X
    aAbsolutY,  //Absolute Indexed by Y  : STA $1000, Y
    aZeroPagX,  //Zero page Indexed by X : LDA $10, X
    aZeroPagY,  //Zero page Indexed by Y : LDA $10, Y
    aIndirecX,  //Indexed Indirect: LDA ($40,X)  Only for X
    aIndirecY,  //Indirect Indexed: LDA ($40),Y  Only for Y
    //New addressing mode for 65C02
    aIndirecZP, // Indirect to ZP: ADC ($12)
    aAbsInIdX,  //Absolute Indexed Indirect X: JMP ($1000, X) (65C02 only)
    aZeroPRel   //Zero page relative: BBR0 $12,LABEL
  );
  TP6502AddModes = set of TP6502AddMode;

  //Device support
  TDeviceSupport = (
    ALL_DEVICES,  //Supported in 6502 and 65C02
    ONLY_65C02    //Supported only by 65C02
  );
  //Instruction Information for each Address Mode
  TInstructInform = record
    Opcode    : byte; //Code for instruction
    nBytes    : byte; //Num. of bytes of the instruction.
    Cycles    : byte; //Num. of cycles the instruction takes.
    optCycles : byte; {Extra options in Num. of cycles:
                       0 -> No aditional cycles.
                       1 -> Add 1 to cycles if page boundery is crossed
                       2 -> Add 1 to cycles if branch occurs on same page.
                            Add 2 to cycles if branch occurs to different page.
                      }
    devSupport: TDeviceSupport;   //Support
  end;

  { TP6502Instruct }
  //Record for a 6502 instruction
  TP6502Instruct = object
  public
    //Name of the instruction
    name: string[4];
    //Address Modes supported
    addressModes: TP6502AddModes;
    //Information for each Address Mode supported
    instrInform: array[TP6502AddMode] of TInstructInform;
  public
    procedure Init(name0: string);
    procedure AddAddressMode(aMode: TP6502AddMode; Opcode, nBytes, nCycles: Byte;
      optCycles: byte; devSupport: TDeviceSupport = ALL_DEVICES);
    function HasOpcode(opc: byte; out adMod: TP6502AddMode): boolean;
  end;

const  //Constants of address and bit positions for some registers
  _C      = 0;
  _Z      = 1;
  _D      = 3;
  _B      = 4;
  _V      = 6;
  _N      = 7;
//  _IRP   = 7;
type
  {Object representing CPU6502 hardware}
  { TP6502 }
  TP6502 = class(TCPUCore)
  public  //Fields to disassembler instructions
    idIns: TP6502Inst;     //Instruction ID
    modIns: TP6502AddMode; //Address mode
    parIns: word;          //Instruction parameter. Only valid for some instructions.
  private //Fields to process instructions
    function GetINTCON: byte;
    function GetINTCON_GIE: boolean;
    function GetSTATUS_C: boolean;
    function GetSTATUS_D: boolean;
    function GetSTATUS_N: boolean;
    function GetSTATUS_I: boolean;
    function GetSTATUS_V: boolean;
    function GetSTATUS_Z: boolean;
    procedure SetINTCON_GIE(AValue: boolean);
    procedure SetSTATUS_C(AValue: boolean);
    procedure SetSTATUS_D(AValue: boolean);
    procedure SetSTATUS_N(AValue: boolean);
    procedure SetSTATUS_I(AValue: boolean);
    procedure SetSTATUS_V(AValue: boolean);
    procedure SetSTATUS_Z(AValue: boolean);
    procedure SetFRAM(value: byte);
    function GetFRAM: byte;
  public  //Fields to modelate internal register (For Simulation)
    W        : byte;    //Work register
    X,Y      : byte;    //Index registers
    PC       : TWordRec; //PC as record to fast access for bytes
    SP       : byte;    //Stack Pointer
    SR       : byte;    //Status Register
    property STATUS: byte read SR;
    property STATUS_N: boolean read GetSTATUS_N write SetSTATUS_N;
    property STATUS_V: boolean read GetSTATUS_V write SetSTATUS_V;
    property STATUS_D: boolean read GetSTATUS_D write SetSTATUS_D;
    property STATUS_I: boolean read GetSTATUS_I write SetSTATUS_I;
    property STATUS_Z: boolean read GetSTATUS_Z write SetSTATUS_Z;
    property STATUS_C: boolean read GetSTATUS_C write SetSTATUS_C;
    property INTCON: byte read GetINTCON;
    property INTCON_GIE: boolean read GetINTCON_GIE write SetINTCON_GIE;
    property FRAM: byte read GetFRAM write SetFRAM;
  public  //Execution control
    function CurInstruction: TP6502Inst;
    procedure Exec(aPC: word); override; //Ejecuta la instrucción en la dirección indicada.
    procedure Exec; override; //Ejecuta instrucción actual
    procedure ExecTo(endAdd: word); override; //Ejecuta hasta cierta dirección
    procedure ExecStep; override; //Execute one instruction considering CALL as one instruction
    procedure ExecNCycles(nCyc: integer; out stopped: boolean); override; //Ejecuta hasta cierta dirección
    procedure Reset(hard: boolean); override;
    function ReadPC: dword; override;
    procedure WritePC(AValue: dword); override;
  public  //Memories
    procedure Decode(const opCode: word);  //Decode instruction.
    function DisassemblerAt(addr: word; out nBytesProc: byte; useVarName: boolean
      ): string; override;
  public  //RAM memory functions
    freeStart: word;  //Start address where Free RAM will be searched.
    function GetFreeByte(out addr: word): boolean;
    function GetFreeBytes(const size: integer; ad1, ad2: word; out addr: word): boolean;  //obtiene una dirección libre
    function TotalMemRAM: integer; //devuelve el total de memoria RAM
    function UsedMemRAM: word;  //devuelve el total de memoria RAM usada
    procedure ExploreUsed(rutExplorRAM: TCPURutExplorRAM);    //devuelve un reporte del uso de la RAM
    function ValidRAMaddr(addr: word): boolean;  //indica si una posición de memoria es válida
  public  //Methods to code instructions according to syntax
    disableCodegen: boolean;   //Flag to disable the Code generation.
    procedure useRAMCodeOp;
    procedure useRAMCodeDa;
    procedure codByte(const value: byte; isData: boolean);
    procedure codByte(const value: byte; used: TCPURamUsed; name: string = '');
    procedure codAsm(const inst: TP6502Inst; addMode: TP6502AddMode; param: word;
      param2: word = 0);
    procedure cod_JMP_at(iRam0: integer; const k: word);
    procedure cod_REL_JMP_at(iRam0: integer; const k: word);
    function codInsert(iRam0, nInsert, nWords: integer): boolean;
  public  //Aditional methods
    function IsRelJump(idInst: TP6502Inst): boolean;  //Idnetify realtive jumps Opcodes
    procedure GenHex(hexFile: string; startAddr: integer = - 1);  //genera un archivo hex
    function GetASMlineAt(addr: word; incAdrr, incValues, incCom,
      incVarNam: boolean; out nBytes: byte): string;
    procedure DumpCodeAsm(lOut: TStrings; incAdrr, incValues, incCom,
      incVarNam: boolean);
  public  //Initialization
    constructor Create; override;
    destructor Destroy; override;
  end;

var  //Global variables
  //Instruction mnemonics.
  PIC16InstName: array[low(TP6502Inst)..high(TP6502Inst)] of TP6502Instruct;


  function FindOpcode(txt: string; out opCode: TP6502Inst): boolean;

implementation

function FindOpcode(txt: string; out opCode: TP6502Inst): boolean;
{Search a string that represent an instruction (Opcode). If found, returns TRUE and
the instruction identifier in "opCode", otherwise returns FALSE and "i_Inval" in
"opCode". }
var
  idInst: TP6502Inst;
  tmp: String;
begin
  tmp := UpperCase(txt);
  for idInst := low(TP6502Inst) to high(TP6502Inst) do begin
    if PIC16InstName[idInst].name = tmp then begin
      opCode := idInst;
      exit(true);
    end;
  end;
  //No found.
  opCode := i_Inval;
  exit(false);
end;

{ TP6502Instruct }

procedure TP6502Instruct.Init(name0: string);
//Initialize the instruction. Must be called before AddAddressMode().
begin
  name := name0;  //Set
  addressModes:= [];  //Clear
end;
procedure TP6502Instruct.AddAddressMode(aMode: TP6502AddMode;
          Opcode, nBytes, nCycles: Byte;
          optCycles: byte; devSupport: TDeviceSupport = ALL_DEVICES);
{Add a new Address Mode including additional information.
* "optCycles" is a flag and indicate aditional considerations on cycles:
  0 -> No aditional considerations.
  1 -> Add 1 to cycles if page boundery is crossed
  2 -> Add 1 to cycles if branch occurs on same page.
       Add 2 to cycles if branch occurs to different page.
* "devSupport" defines the deviced supported by this addressing mode.
}
begin
  addressModes := addressModes + [aMode];  //Register Mode
  //Add information
  instrInform[aMode].Opcode:= Opcode;
  instrInform[aMode].nBytes:= nBytes;
  instrInform[aMode].Cycles:= nCycles;
  instrInform[aMode].optCycles := optCycles;
  instrInform[aMode].devSupport := devSupport;
end;
function TP6502Instruct.HasOpcode(opc: byte; out adMod: TP6502AddMode
  ): boolean;
{Indicates if the Opcode "opc" exits in this instruction. If so, returns TRUE and
the address mode in "adMod".}
begin
  for adMod in addressModes do begin  //Test for all Address modes supported.
    if instrInform[adMod].Opcode = opc then exit(true);
  end;
  exit(false);
end;
{ TP6502 }
procedure TP6502.useRAMCodeOp;
{Set current position as used and increase the index iRam. If error;update "MsjError"}
begin
  ram[iRam].used := ruCodeOp;  //Mark as used.
  inc(iRam);
end;
procedure TP6502.useRAMCodeDa;
begin
  ram[iRam].used := ruCodeDa;  //Mark as used.
  inc(iRam);
end;
procedure TP6502.codByte(const value: byte; isData: boolean);
{Write a byte to the RAM memory.}
begin
  if iRam >= CPUMAXRAM then begin
    MsjError := 'RAM Memory limit exceeded.';
    exit;
  end;
  ram[iRam].value := value;
  if isData then ram[iRam].name := 'data';
  ram[iRam].used := ruData;  //Mark as used.
  inc(iRam);
end;
procedure TP6502.codByte(const value: byte; used: TCPURamUsed; name: string = '');
{Write a byte to the RAM memory.}
begin
  if iRam >= CPUMAXRAM then begin
    MsjError := 'RAM Memory limit exceeded.';
    exit;
  end;
  ram[iRam].value := value;
  ram[iRam].used := used;  //Mark as used.
  if name<>'' then ram[iRam].name := name;
  inc(iRam);
end;
procedure TP6502.codAsm(const inst: TP6502Inst; addMode: TP6502AddMode; param: word;
                        param2: word = 0);
{General routine to codify assembler instructions.}
var
  rInst: TP6502Instruct;
begin
  if disableCodegen then exit;  //Test flag
  rInst := PIC16InstName[inst];
  //Overflow protection
  if iRam >= CPUMAXRAM then begin
    MsjError := 'RAM Memory limit exceeded.';
    exit;
  end;
  //Write OpCode
  if not (addMode in rInst.addressModes) then begin
    MsjError := 'Invalid Adrress mode.';
    exit;
  end;
  ram[iRam].value := rInst.instrInform[addMode].Opcode;
  useRAMCodeOp;  //Set as used and increase index.
  //Encode parameters
  case addMode of
  aImplicit: begin
    //No parameters
    if addMode in rInst.addressModes then begin
      //It's OK
    end else begin
      MsjError:= 'Invalid Address Mode (Implicit)';
    end;
  end;
  aAcumulat: begin
    //No parameters
    if addMode in rInst.addressModes then begin
      //It's OK
    end else begin
      MsjError:= 'Invalid Address Mode (Acumulator)';
    end;
  end;
  aImmediat: begin
    if addMode in rInst.addressModes then begin
      //It's OK
    end else begin
      MsjError:= 'Invalid Address Mode (Immediate)';
    end;
    ram[iRam].value := lo(param);  //escribe parámetro
    useRAMCodeDa;
  end;
  aAbsolute:begin
    ram[iRam].value := lo(param);
    useRAMCodeDa;
    ram[iRam].value := hi(param);
    useRAMCodeDa;
  end;
  aZeroPage:begin
    ram[iRam].value := lo(param);
    useRAMCodeDa;
  end;
  aRelative:begin
    ram[iRam].value := lo(param);
    useRAMCodeDa;
  end;
  aIndirect:begin
    ram[iRam].value := lo(param);
    useRAMCodeDa;
    ram[iRam].value := hi(param);
    useRAMCodeDa;
  end;
  aAbsolutX:begin
    ram[iRam].value := lo(param);
    useRAMCodeDa;
    ram[iRam].value := hi(param);
    useRAMCodeDa;
  end;
  aAbsolutY:begin
    ram[iRam].value := lo(param);
    useRAMCodeDa;
    ram[iRam].value := hi(param);
    useRAMCodeDa;
  end;
  aZeroPagX:begin
    ram[iRam].value := lo(param);
    useRAMCodeDa;
  end;
  aZeroPagY:begin
    ram[iRam].value := lo(param);
    useRAMCodeDa;
  end;
  aIndirecX:begin
    ram[iRam].value := lo(param);
    useRAMCodeDa;
  end;
  aIndirecY:begin
    ram[iRam].value := lo(param);
    useRAMCodeDa;
  end;
  aAbsInIdX: begin
    ram[iRam].value := lo(param);
    useRAMCodeDa;
    ram[iRam].value := hi(param);
    useRAMCodeDa;
  end;
  aIndirecZP: begin
    ram[iRam].value := lo(param);
    useRAMCodeDa;
  end;
  aZeroPRel: begin
    ram[iRam].value := lo(param);
    useRAMCodeDa;
    ram[iRam].value := lo(param2);
    useRAMCodeDa;
  end
  else
    raise Exception.Create('Implementation Error.');
  end;
end;
procedure TP6502.cod_JMP_at(iRam0: integer; const k: word);
{Encode the jump address for a jump instruction. Used to complete undefined jumps.}
begin
  ram[iRam0+1].value := lo(k);
  ram[iRam0+2].value := hi(k);
end;
procedure TP6502.cod_REL_JMP_at(iRam0: integer; const k: word);
{Encode the jump address for a relative jump instruction. Used to complete undefined jumps.}
begin
  ram[iRam0+1].value := lo(k);
end;
function TP6502.codInsert(iRam0, nInsert, nWords: integer): boolean;
{Inserta en la posición iRam0, "nInsert" palabras, desplazando "nWords" palabras.
Al final debe quedar "nInsert" palabras de espacio libre en iRam0.
Si hay error devuelve FALSE.}
var
  i: Integer;
begin
  Result := True;  //By default
  if iRam+nInsert+nWords-1> CPUMAXRAM then begin
    //Overflow on address
    exit(false);
  end;
  for i:= iRam + nInsert + nWords -1 downto iRam + nWords do begin
    ram[i] := ram[i-nInsert];
  end;
end;
function TP6502.IsRelJump(idInst: TP6502Inst): boolean;
{Returns TRUE if the instruction accept the relative address mode}
begin
  Result := PIC16InstName[idInst].instrInform[aRelative].Opcode<>0;
end;
//Campos para procesar instrucciones
function TP6502.GetSTATUS_N: boolean;
begin
  Result := (SR and %10000000) <> 0;
end;
procedure TP6502.SetSTATUS_N(AValue: boolean);
begin
  if AVAlue then SR := SR or  %10000000
            else SR := SR and %01111111;
end;
function TP6502.GetSTATUS_V: boolean;
begin
  Result := (SR and %01000000) <> 0;
end;
procedure TP6502.SetSTATUS_V(AValue: boolean);
begin
  if AVAlue then SR := SR or  %01000000
            else SR := SR and %10111111;
end;
function TP6502.GetSTATUS_D: boolean;
begin
  Result := (SR and %00001000) <> 0;
end;
procedure TP6502.SetSTATUS_D(AValue: boolean);
begin
  if AVAlue then SR := SR or  %00001000
            else SR := SR and %11110111;
end;
function TP6502.GetSTATUS_I: boolean;
begin
  Result := (SR and %00000100) <> 0;
end;
procedure TP6502.SetSTATUS_I(AValue: boolean);
begin
  if AVAlue then SR := SR or  %00000100
            else SR := SR and %11111011;
end;
function TP6502.GetSTATUS_Z: boolean;
begin
  Result := (SR and %00000010) <> 0;
end;
procedure TP6502.SetSTATUS_Z(AValue: boolean);
begin
  if AVAlue then SR := SR or  %00000010
            else SR := SR and %11111101;
end;
function TP6502.GetSTATUS_C: boolean;
begin
  Result := (SR and %00000001) <> 0;
end;
procedure TP6502.SetSTATUS_C(AValue: boolean);
begin
  if AVAlue then SR := SR or  %00000001
            else SR := SR and %11111110;
end;
function TP6502.GetINTCON: byte;
begin
  Result := ram[$0B].dvalue;
end;
function TP6502.GetINTCON_GIE: boolean;
begin
  Result := (ram[$0B].dvalue and %10000000) <> 0;
end;
procedure TP6502.SetINTCON_GIE(AValue: boolean);
begin
  if AVAlue then ram[$0B].dvalue := ram[$0B].dvalue or  %10000000
            else ram[$0B].dvalue := ram[$0B].dvalue and %01111111;
end;
procedure TP6502.SetFRAM(value: byte);
{Escribe en la RAM; en la dirección global f_, el valor "value"
Para determinar el valor real de la dirección, se toma en cuenta los bits de STATUS}
begin
   ram[parIns].value := value;
end;
function TP6502.GetFRAM: byte;
{Devuelve el valor de la RAM, de la posición global f_.
Para determinar el valor real de la dirección, se toma en cuenta los bits de STATUS}
begin
  Result := ram[parIns].value;
end;
procedure TP6502.Decode(const opCode: word);
{Decode the value of "opCode" and update:
* "idIns" -> Instruction ID
* "modIns" -> Address Mode
If not found, returns "i_Inval" in "idIns".
}
var
  i : TP6502Inst;
begin
  //Search the Opcode
  for i := low(TP6502Inst) to high(TP6502Inst) do begin
    if PIC16InstName[i].HasOpcode(opCode, modIns) then begin
      idIns := i;
      exit;
    end;
  end;
  //Not found
  idIns := i_Inval;
end;
function TP6502.DisassemblerAt(addr: word; out nBytesProc: byte;
                               useVarName: boolean): string;
{Disassembler the instruction located at "addr". If the instruction is multibyte
the intruction length, will be returned in "nBytesProc".

Global variables used: "idIns", "modIns".
"useVarName" -> Flag to use the name of the variable instead of only the address.
                Valid only when a variable name exists in its address.
}
  function RelativeParStr(addr, par: word): string;
  {Returns relative offset as string}
  begin
    if par<128 then begin  //Positive
      Result := '$'+IntToHex((addr + par+2) and $FFFF, 4);
    end else begin
      Result := '$'+IntToHex((addr + par-254) and $FFFF, 4);
    end;
  end;
var
  nemo: String;
  opCode, par1: Byte;
  par2: word;
begin
  if addr>CPUMAXRAM-1 then exit('');
  opCode := ram[addr].value;
  //"par1" and "par2" will be used according to the instruction length.
  if addr+1>CPUMAXRAM-1 then exit('');
  par1   := ram[addr+1].value;
  Decode(opCode);   //Decode instruction. Update: "idIns", "modIns".
  nemo := trim(PIC16InstName[idIns].name) + ' ';
  case modIns of
  aImplicit: begin
    nBytesProc := 1;  //No parameters needed
    Result := nemo;
  end;
  aAcumulat: begin
    nBytesProc := 1;  //No parameters needed
    Result := nemo;
  end;
  aImmediat: begin
    Result := nemo + '#$'+IntToHex(par1,2);
    nBytesProc := 2;
  end;
  aAbsolute: begin
    nBytesProc := 3;
    if addr+2>CPUMAXRAM-1 then exit('');
    par2 := ram[addr+2].value<<8 + par1;
    if useVarName and (ram[par2].name<>'') then begin
      Result := nemo + ram[par2].name;
    end else begin
      Result := nemo + '$'+IntToHex(par2, 4);
    end;
  end;
  aZeroPage: begin
    if useVarName and (ram[par1].name<>'') then begin
      Result := nemo + ram[par1].name;
    end else begin
      Result := nemo + '$'+IntToHex(par1, 2);
    end;
    nBytesProc := 2;
  end;
  aRelative: begin
    nBytesProc := 2;
    //if par1<128 then begin  //Positive
    //  Result := nemo + '$'+IntToHex((addr + par1+2) and $FFFF, 4);
    //end else begin
    //  Result := nemo + '$'+IntToHex((addr + par1-254) and $FFFF, 4);
    //end;
    Result := nemo + RelativeParStr(addr, par1);
  end;
  aIndirect: begin
    nBytesProc := 3;
    if addr+2>CPUMAXRAM-1 then exit('');
    par2   := ram[addr+2].value;
    Result := nemo + '($'+IntToHex(par1 + par2*256, 4)+')';
  end;
  aAbsolutX: begin
    nBytesProc := 3;
    if addr+2>CPUMAXRAM-1 then exit('');
    par2   := ram[addr+2].value;
    Result := nemo + '$'+IntToHex(par1 + par2*256, 4)+',X';
  end;
  aAbsolutY: begin
    nBytesProc := 3;
    if addr+2>CPUMAXRAM-1 then exit('');
    par2   := ram[addr+2].value;
    Result := nemo + '$'+IntToHex(par1 + par2*256, 4)+',Y';
  end;
  aZeroPagX: begin
    nBytesProc := 2;
    Result := nemo + '$'+IntToHex(par1, 2)+',X';
  end;
  aZeroPagY: begin
    nBytesProc := 2;
    Result := nemo + '$'+IntToHex(par1, 2)+',Y';
  end;
  aIndirecX: begin
    nBytesProc := 2;
    Result := nemo + '($'+IntToHex(par1, 2)+',X)';
  end;
  aIndirecY: begin
    nBytesProc := 2;
    Result := nemo + '($'+IntToHex(par1, 2)+'),Y';
  end;
  aAbsInIdX: begin
    nBytesProc := 3;
    if addr+2>CPUMAXRAM-1 then exit('');
    par2   := ram[addr+2].value;
    Result := nemo + '($'+IntToHex(par1 + par2*256, 4)+',X)';
  end;
  aIndirecZP: begin
    nBytesProc := 2;
    Result := nemo + '($'+IntToHex(par1, 2)+')';
  end;
  aZeroPRel: begin
    nBytesProc := 3;
    if addr+2>CPUMAXRAM-1 then exit('');
    par2   := ram[addr+2].value;
    Result := nemo + '$'+IntToHex(par1, 2)+',' + RelativeParStr(addr, par2);
  end;
  end;
end;
function TP6502.CurInstruction: TP6502Inst;
{Return the instruction pointed by PC, in this moment.}
begin
  Decode(ram[PC.W].value);   //decode instruction
  Result := idIns;
end;
procedure TP6502.Exec;
{Execute the current instruction.}
begin
  Exec(PC.W);
end;
procedure TP6502.Exec(aPC: word);
{Ejecuta la instrución actual con dirección "pc".
Falta implementar las operaciones, cuando acceden al registro INDF, el Watchdog timer,
los contadores, las interrupciones}
    // Helper functions
    // WARNING: if Range Check is on, it will raise exception
  procedure Push(Value: byte); inline;
  begin
    ram[$100 + SP].value := Value;
    dec(SP);
  end;
  function Pull: byte; inline;
  begin
    inc(SP);
    Result := ram[$100 + SP].value;
    STATUS_Z := Result = 0;
    STATUS_N := Result > 127;
  end;

var
  opc: byte;
  nCycles, nBytes, tmp, off, OP1, OP2: byte;
  addr: word;
  C_tmp: Boolean;
  tmpRes: integer;
  tmpW: word;
  tmpS: ShortInt;
begin
  //Decodifica instrucción
  aPC := PC.W;
  opc := ram[aPC].value;
  Decode(opc);   //Decode instruction
  nCycles := PIC16InstName[idIns].instrInform[modIns].Cycles;
  nBytes  := PIC16InstName[idIns].instrInform[modIns].nBytes;
  //Get Operand
  case modIns of
  aImmediat: addr := (aPC+1) and $FFFF;
  aZeroPage: addr := ram[aPC+1].value;
  aZeroPagX: addr := (ram[aPC+1].value + X) and $FF;
  aAbsolute: addr := ram[aPC+1].value  + 256*ram[aPC+2].value;
  aAbsolutX: addr := (ram[aPC+1].value  + 256*ram[aPC+2].value + X) and $FFFF;
  aAbsolutY: addr := (ram[aPC+1].value  + 256*ram[aPC+2].value + Y) and $FFFF;
  aIndirecX: begin
             tmp  := (ram[aPC+1].value + X) and $FF;
             addr := ram[tmp].value  + 256*ram[tmp+1].value;
    end;
  aIndirecY: begin
             tmp  := ram[aPC+1].value;
             addr := ram[tmp].value  + 256*ram[tmp+1].value + Y;
    end;
  aIndirect: begin
             tmpW := (ram[aPC+1].value + 256*ram[aPC+2].value) and $FFFF;
             addr := ram[tmpW].value  + 256*ram[tmpW+1].value;
    end;
  aIndirecZP: begin
             tmp  := ram[aPC+1].value;
             addr := ram[tmp].value  + 256*ram[tmp+1].value;
    end;
  aAbsInIdX: begin
             tmpW := (ram[aPC+1].value + 256*ram[aPC+2].value + X) and $FFFF;
             addr := ram[tmpW].value  + 256*ram[tmpW+1].value;
    end;
  aRelative: begin
             tmpS := shortint(ram[aPC+1].value + nBytes);
             addr := (PC.W + tmpS) and $FFFF;
    end;
  aZeroPRel: begin
             tmpS := shortint(ram[aPC+2].value + nBytes);
             addr := (PC.W + tmpS) and $FFFF;
    end;
  end;

  //Execute
  case idIns of
  i_ADC: begin  //add with carry
    OP1 := W;   //Keep operand.
    OP2 := ram[addr].value;
    if STATUS_C then begin
      tmpRes := W + OP2 + 1;
    end else begin
      tmpRes := W + OP2;
    end;
    W := tmpRes and $FF;
    STATUS_Z := W = 0;
    STATUS_N := W > 127;
    STATUS_C := tmpRes>255;
    STATUS_V := ((OP1 XOR W) AND (OP2 XOR W) AND $80)<>0;   //Based on: http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
  end;
  i_AND: begin  //and (with accumulator)
    W := W and ram[addr].value;
    STATUS_Z := W = 0;
    STATUS_N := W > 127;
  end;
  i_ASL: begin  //arithmetic shift left
    if modIns = aAcumulat then  tmp := W
    else tmp := ram[addr].value;

    STATUS_C := (tmp and $80) <> 0;  //Read bit 7
    tmp := (tmp << 1) and $FF;
    STATUS_Z := tmp = 0;
    STATUS_N := tmp > 127;

    if modIns = aAcumulat then  W := tmp
    else ram[addr].value := tmp;
  end;
  i_BCC: begin  //branch on carry clear
    if not STATUS_C then begin
      off := ram[aPC+1].value;
      Inc(PC.W, nBytes);  //Normal Increase PC
      if off>127 then begin
        PC.W := (PC.W + off - 256) and $FFFF;
      end else begin
        PC.W := (PC.W + off) and $FFFF;
      end;
      Inc(nClck, nCycles + 1);  //Extra cycle in branch
      exit;
    end;
  end;
  i_BCS: begin  //branch on carry set
    if STATUS_C then begin
      off := ram[aPC+1].value;
      Inc(PC.W, nBytes);  //Normal Increase PC
      if off>127 then begin
        PC.w := (PC.W + off - 256) and $FFFF;
      end else begin
        PC.W := (PC.W + off) and $FFFF;
      end;
      Inc(nClck, nCycles + 1);  //Extra cycle in branch
      exit;
    end;
  end;
  i_BNE: begin //branch on not equal (zero clear)
    if not STATUS_Z then begin
      off := ram[aPC+1].value;
      Inc(PC.W, nBytes);  //Normal Increase PC
      if off>127 then begin
        PC.W := (PC.W + off - 256) and $FFFF;
      end else begin
        PC.W := (PC.W + off) and $FFFF;
      end;
      Inc(nClck, nCycles + 1);  //Extra cycle in branch
      exit;
    end;
  end;
  i_BEQ: begin  //branch on equal (zero set)
    if STATUS_Z then begin
      off := ram[aPC+1].value;
      Inc(PC.W, nBytes);  //Normal Increase PC
      if off>127 then begin
        PC.w := (PC.W + off - 256) and $FFFF;
      end else begin
        PC.W := (PC.W + off) and $FFFF;
      end;
      Inc(nClck, nCycles + 1);  //Extra cycle in branch
      exit;
    end;
  end;
  i_BIT: begin  //bit test
    STATUS_Z := (W and ram[addr].value) = 0;
    if modIns <> aImmediat then begin;
      STATUS_N := (ram[addr].value AND $80) <> 0;
      STATUS_V := (ram[addr].value AND $40) <> 0;
    end;
  end;
  i_BPL: begin  //branch on plus (negative clear)
    if not STATUS_N then begin
      off := ram[aPC+1].value;
      Inc(PC.W, nBytes);  //Normal Increase PC
      if off>127 then begin
        PC.W := (PC.W + off - 256) and $FFFF;
      end else begin
        PC.W := (PC.W + off) and $FFFF;
      end;
      Inc(nClck, nCycles + 1);  //Extra cycle in branch
      exit;
    end;
  end;
  i_BMI: begin  //branch on minus (negative set)
    if STATUS_N then begin
      off := ram[aPC+1].value;
      Inc(PC.W, nBytes);  //Normal Increase PC
      if off>127 then begin
        PC.w := (PC.W + off - 256) and $FFFF;
      end else begin
        PC.W := (PC.W + off) and $FFFF;
      end;
      Inc(nClck, nCycles + 1);  //Extra cycle in branch
      exit;
    end;
  end;
  i_BRK: begin  //break / interrupt
    ram[$100 + SP].value := PC.L;
    if SP = $00 then SP := $FF else dec(SP);
    ram[$100 + SP].value := PC.H;
    if SP = $00 then SP := $FF else dec(SP);
    STATUS_I := true;
    ram[$100 + SP].value := SR;
    if SP = $00 then SP := $FF else dec(SP);
    PC.L := ram[$FFFE].value;
    PC.H := ram[$FFFF].value;
  end;
  i_BVC: begin  //branch on overflow clear
    if not STATUS_V then begin
      off := ram[aPC+1].value;
      Inc(PC.W, nBytes);  //Normal Increase PC
      if off>127 then begin
        PC.W := (PC.W + off - 256) and $FFFF;
      end else begin
        PC.W := (PC.W + off) and $FFFF;
      end;
      Inc(nClck, nCycles + 1);  //Extra cycle in branch
      exit;
    end;
  end;
  i_BVS: begin  //branch on overflow set
    if STATUS_V then begin
      off := ram[aPC+1].value;
      Inc(PC.W, nBytes);  //Normal Increase PC
      if off>127 then begin
        PC.w := (PC.W + off - 256) and $FFFF;
      end else begin
        PC.W := (PC.W + off) and $FFFF;
      end;
      Inc(nClck, nCycles + 1);  //Extra cycle in branch
      exit;
    end;
  end;
  i_CLC: STATUS_C := false;  //clear carry
  i_CLD: STATUS_D := false;  //clear decimal
  i_CLI: STATUS_I := false;  //clear interrupt disable
  i_CLV: STATUS_V := false;  //clear overflow
  i_CMP: begin     //Compare (with accumulator)
    tmp := ram[addr].value;
    STATUS_Z := W = tmp;
    STATUS_C := W >= tmp;
    if W = tmp then begin
      STATUS_N := false;
    end else begin
      STATUS_N := ((W-tmp) and $80) <> 0;  //Copy bit 7
    end;
  end;
  i_CPX: begin;  //compare with X
    tmp := ram[addr].value;
    STATUS_Z := X = tmp;
    STATUS_C := X >= tmp;
    if X = tmp then begin
      STATUS_N := false;
    end else begin
      STATUS_N := ((X-tmp) and $80) <> 0;  //Copy bit 7
    end;
  end;
  i_CPY: begin  //compare with Y
    tmp := ram[addr].value;
    STATUS_Z := Y = tmp;
    STATUS_C := Y >= tmp;
    if Y = tmp then begin
      STATUS_N := false;
    end else begin
      STATUS_N := ((Y-tmp) and $80) <> 0;  //Copy bit 7
    end;
  end;
  i_DEC: begin  //decrement
    if modIns = aImplicit then begin
      W := (W - 1) and $FF;
      STATUS_Z := W = 0;
      STATUS_N := W > 127;
    end else begin
      tmp := (ram[addr].value - 1) and $FF;
      ram[addr].value := tmp;
      STATUS_Z := tmp = 0;
      STATUS_N := tmp > 127;
    end;
  end;
  i_DEX: begin  //decrement X
    X := (X - 1) and $FF;
    STATUS_Z := X = 0;
    STATUS_N := X > 127;
  end;
  i_DEY: begin  //decrement Y
    Y := (Y - 1) and $FF;
    STATUS_Z := Y = 0;
    STATUS_N := Y > 127;
  end;
  i_EOR: begin  //exclusive or (with accumulator)
    W := W xor ram[addr].value;
    STATUS_Z := W = 0;
    STATUS_N := W > 127;
  end;
  i_INC: begin  //increment
    if modIns = aImplicit then begin
      W := (W + 1) and $FF;
      STATUS_Z := W = 0;
      STATUS_N := W > 127;
    end else begin
      tmp := (ram[addr].value + 1) and $FF;
      ram[addr].value := tmp;
      STATUS_Z := tmp = 0;
      STATUS_N := tmp > 127;
    end
  end;
  i_INX: begin  //increment X
    X := (X + 1) and $FF;
    STATUS_Z := X = 0;
    STATUS_N := X > 127;
  end;
  i_INY: begin  //increment Y
    Y := (Y + 1) and $FF;
    STATUS_Z := Y = 0;
    STATUS_N := Y > 127;
  end;
  i_JMP: begin    //jump
    {case modIns of
    aAbsolute : begin
      PC.L := ram[aPC+1].value;
      PC.H := ram[aPC+2].value;
    end;
    aIndirect: begin
      target := ram[aPC+1].value + 256* ram[aPC+2].value;
      PC.L := ram[target+1].value;
      PC.H := ram[target+2].value;
    end;
    aAbsInIdX: begin
      PC.L := ram[addr].value;
      PC.H := ram[addr+1].value;
     end;
    end;}
    PC.W := addr;
    //Inc(PC.W, nBytes);  //No apply
    Inc(nClck, nCycles);
    exit;
  end;
  i_JSR: begin
    inc(PC.W, 3);  //Next position
    //Save return
    ram[$100 + SP].value := PC.H;
    if SP = $00 then SP := $FF else dec(SP);
    ram[$100 + SP].value := PC.L;
    if SP = $00 then SP := $FF else dec(SP);
    PC.L := ram[aPC+1].value;
    PC.H := ram[aPC+2].value;
    //Inc(PC.W, nBytes);  //No apply
    Inc(nClck, nCycles);
    exit;
  end;  //jump subroutine
  i_LDA: begin //Load accumulator
    W := ram[addr].value;
    STATUS_Z := W = 0;
    STATUS_N := W > 127;
  end;
  i_LDX: begin //Load X
    X := ram[addr].value;
    STATUS_Z := X = 0;
    STATUS_N := X > 127;
  end;
  i_LDY: begin //Load y
    Y := ram[addr].value;
    STATUS_Z := Y = 0;
    STATUS_N := Y > 127;
  end;
  i_LSR: begin
    STATUS_N := false;
    if modIns = aAcumulat then  tmp := W
    else tmp := ram[addr].value;

    STATUS_C := (tmp and $01) <> 0;
    tmp := tmp >> 1;
    STATUS_Z := tmp = 0;

    if modIns = aAcumulat then  W := tmp
    else ram[addr].value := tmp;
  end;  //logical shift right
  i_NOP: ;  //no operation
  i_ORA: begin  //Or with accumulator
    W := W or ram[addr].value;
    STATUS_Z := W = 0;
    STATUS_N := W > 127;
  end;
  i_PHA: Push(W);    //Push accumulator
  i_PHP: Push(SR);   //Push processor status (SR)
  i_PLA: W := Pull;  //Pull accumulator
  i_PLP: SR := Pull; //Pull processor status (SR)
  i_ROL: begin  //Rotate left
    STATUS_N := false;
    if modIns = aAcumulat then tmp := W
    else tmp := ram[addr].value;

    C_tmp := STATUS_C;
    STATUS_C := (tmp and $80) <> 0;  //Get bit 7
    tmp := byte(tmp << 1);
    if C_tmp then tmp := tmp or $01;  //Insert bit 0
    STATUS_Z := tmp = 0;
    STATUS_N := tmp > 127;

    if modIns = aAcumulat then W := tmp
    else ram[addr].value := tmp;
  end;
  i_ROR: begin  //Rotate right
    STATUS_N := false;
    if modIns = aAcumulat then  tmp := W
    else tmp := ram[addr].value;

    C_tmp := STATUS_C;          //Save previos C
    STATUS_C := (tmp and $01) <> 0;   //Get bit 0
    tmp := tmp >> 1;
    if C_tmp then tmp := tmp or $80;  //Insert bit 7
    STATUS_Z := tmp = 0;
    STATUS_N := tmp > 127;

    if modIns = aAcumulat then  W := tmp
    else ram[addr].value := tmp;
  end;
  i_RTI: begin  //Return from interrupt
    if SP = $FF then SP := $00 else inc(SP);
    SR := ram[$100 + SP].value;
    if SP = $FF then SP := $00 else inc(SP);
    PC.L := ram[$100 + SP].value;
    if SP = $FF then SP := $00 else inc(SP);
    PC.H := ram[$100 + SP].value;
    //Inc(PC.W, nBytes);  //No apply
    Inc(nClck, nCycles);
    exit;
  end;
  i_RTS: begin  //Return from subroutine
    if SP = $FF then SP := $00 else inc(SP);
    PC.L := ram[$100 + SP].value;
    if SP = $FF then SP := $00 else inc(SP);
    PC.H := ram[$100 + SP].value;
    //Inc(PC.W, nBytes);  //No apply
    Inc(nClck, nCycles);
    exit;
  end;
  i_SBC: begin  //Subtract with carry
    OP1 := W;   //Keep operand.
    OP2 := ram[addr].value;
    if STATUS_C then begin
      tmpRes := W - OP2;
    end else begin
      tmpRes := W - OP2 - 1;
    end;
    W := tmpRes and $FF;
    STATUS_Z := W = 0;
    STATUS_N := W > 127;
    STATUS_C := not (tmpRes<0);
    STATUS_V := ((OP1 XOR W) AND (OP2 XOR W) AND $80)<>0;   //Based on: http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
  end;
  i_SEC: STATUS_C := true;  //set carry
  i_SED: STATUS_D := true;  //set decimal
  i_SEI: STATUS_I := true;  //set interrupt disable
  i_STA: begin //store accumulator
    ram[addr].value := W;
  end;
  i_STX: begin  //store X
    ram[addr].value := X;
  end;
  i_STY: begin  //store Y
    ram[addr].value := Y;
  end;
  i_TAX: begin  //transfer accumulator to X
    X := W;
    STATUS_Z := X = 0;
    STATUS_N := X > 127;
  end;
  i_TAY: begin  //transfer accumulator to Y
    Y := W;
    STATUS_Z := Y = 0;
    STATUS_N := Y > 127;
  end;
  i_TSX: begin  //transfer stack pointer to X
    X := SP;
    STATUS_Z := X = 0;
    STATUS_N := X > 127;
  end;
  i_TXA: begin  //transfer X to accumulator
    W := X;
    STATUS_Z := W = 0;
    STATUS_N := W > 127;
  end;
  i_TXS: begin  //transfer X to stack pointer
    SP := X;
  end;
  i_TYA: begin  //transfer Y to accumulator
    W := Y;
    STATUS_Z := W = 0;
    STATUS_N := W > 127;
  end;

  i_STZ: begin // store zero
    ram[addr].value := 0;
  end;
  i_BRA: begin  // brunch always
    PC.W := addr;
    Inc(nClck, nCycles);
    exit;
  end;
  i_PHX: Push(X);  //push X in stack
  i_PHY: Push(Y);  //push Y in stack
  i_PLX: X := Pull;  //pull X from stack
  i_PLY: Y := Pull;  //push Y from stack
  i_TRB: begin
    tmp := ram[addr].value;
    STATUS_Z := (W and tmp) = 0;
    ram[addr].value := tmp AND (W XOR $FF);
  end;
  i_TSB: begin
    tmp := ram[addr].value;
    STATUS_Z := (W and tmp) = 0;
    ram[addr].value := tmp OR W;
  end;
  i_RMB0: ram[addr].value := ram[addr].value and $FE;
  i_RMB1: ram[addr].value := ram[addr].value and $FD;
  i_RMB2: ram[addr].value := ram[addr].value and $FB;
  i_RMB3: ram[addr].value := ram[addr].value and $F7;
  i_RMB4: ram[addr].value := ram[addr].value and $EF;
  i_RMB5: ram[addr].value := ram[addr].value and $DF;
  i_RMB6: ram[addr].value := ram[addr].value and $BF;
  i_RMB7: ram[addr].value := ram[addr].value and $7F;
  i_SMB0: ram[addr].value := ram[addr].value or (1 << 0);
  i_SMB1: ram[addr].value := ram[addr].value or (1 << 1);
  i_SMB2: ram[addr].value := ram[addr].value or (1 << 2);
  i_SMB3: ram[addr].value := ram[addr].value or (1 << 3);
  i_SMB4: ram[addr].value := ram[addr].value or (1 << 4);
  i_SMB5: ram[addr].value := ram[addr].value or (1 << 5);
  i_SMB6: ram[addr].value := ram[addr].value or (1 << 6);
  i_SMB7: ram[addr].value := ram[addr].value or (1 << 7);
  i_BBR0:  // Branch if Bit 0 is 0
    if (ram[aPC+1].value and 1) = 0 then begin
      PC.W := addr;
      Inc(nClck, nCycles + 1);  //Extra cycle in branch
      exit;
    end;

  i_Inval: begin
      MsjError := 'Invalid Opcode';
    end;
  end;
  //Increase counters
  Inc(PC.W, nBytes);
  Inc(nClck, nCycles);
end;
procedure TP6502.ExecTo(endAdd: word);
{Ejecuta las instrucciones secuencialmente, desde la instrucción actual, hasta que el
contador del programa, sea igual a la dirección "endAdd".}
begin
  //Hace una primera ejecución, sin verificar Breakpoints
  if ram[PC.W].used = ruUnused then begin
    //Encontró un BreakPoint, sale sin ejecutar esa instrucción
    if OnExecutionMsg<>nil then OnExecutionMsg('Stopped. Unused RAM location.');
    exit;
  end;
  Exec(PC.W);
  //Ejecuta cíclicamnente
  while PC.W <> endAdd do begin
    if ram[PC.W].breakPnt then begin
      //Encontró un BreakPoint, sale sin ejecutar esa instrucción
      if OnExecutionMsg<>nil then OnExecutionMsg('Stopped for breakpoint.');
      exit;
    end;
    if ram[PC.W].used = ruUnused then begin
      //Encontró un BreakPoint, sale sin ejecutar esa instrucción
      if OnExecutionMsg<>nil then OnExecutionMsg('Stopped. Unused RAM location.');
      exit;
    end;
    //Ejecuta
    Exec(PC.W);
    //Debe haber una forma de salir si es un lazo infinito
    //if (nClck and $800000) = $800000 then begin
    //end;
  end;
end;
procedure TP6502.ExecStep;
begin
  if CurInstruction = i_JSR then begin
    ExecTo(PC.W+3);  //Ejecuta hasta la sgte. instrucción, salta el JSR
  end else begin
    Exec;
  end;
end;
procedure TP6502.ExecNCycles(nCyc: integer; out stopped: boolean);
{Ejecuta el número de ciclos indicados, o hasta que se produzca alguna condición
externa, que puede ser:
* Se encuentre un Punto de Interrupción.
* Se detecta la señal, de detenerse.
* Se genere algún error en la ejecución.
* Se ejecuta la instrucción i_SLEEP.
la bandera "stopped", indica que se ha detendio la ejecución sin completar la cantidad
de instrucciones requeridas.
Normalmente Se ejecutará el número de ciclos indicados, pero en algunos casos se
ejecutará un ciclo más, debido a que algunas instrucciones toman dos ciclos.}
var
  clkEnd: Int64;
  _pc: word;
begin
  clkEnd := nClck + nCyc;   //Valor final del contador
  while nClck < clkEnd do begin
    _pc := PC.W;
    if ram[_pc].breakPnt then begin
      //Encontró un BreakPoint, sale sin ejecutar esa instrucción
      if OnExecutionMsg<>nil then OnExecutionMsg('Stopped for breakpoint.');
      stopped := true;
      exit;
    end;
    if ram[_pc].used = ruUnused then begin
      //Encontró un BreakPoint, sale sin ejecutar esa instrucción
      if OnExecutionMsg<>nil then OnExecutionMsg('Stopped for executing unused code.');
      stopped := true;
      exit;
    end;
    if CommStop then begin
      //Se detectó el comando STOP
      if OnExecutionMsg<>nil then OnExecutionMsg('Stopped for STOP command.');
      stopped := true;
      exit;
    end;
    //Ejecuta
    Exec(_pc);
  end;
  stopped := false;
end;
procedure TP6502.Reset(hard: boolean);
//Reinicia el dipsoitivo
var
  i: Integer;
begin
  PC.W   := 0;
  W      := 0;
  SP     := $FF;   //Posición inicial del puntero de pila
  SR     := %00000100;  //I -> 1
  nClck  := 0;   //Inicia contador de ciclos
  CommStop := false;  //Limpia bandera
  if hard then begin
    //Limpia solamente el valor inicial, no toca los otros campos
    for i:=0 to high(ram) do begin
      ram[i].dvalue := $00;
    end;
  end;
end;
function TP6502.ReadPC: dword;
begin
  Result := PC.W;
end;
procedure TP6502.WritePC(AValue: dword);
begin
  PC.W := AValue;
end;
//Funciones para la memoria RAM
function TP6502.GetFreeByte(out addr: word): boolean;
{Devuelve una dirección libre de la memoria RAM, a partir de la dirección iRam.
"Shared" indica que se marcará el bit como de tipo "Compartido", y se usa para el
caso en que se quiera comaprtir la misma posición para diversos variables.
Si encuentra espacio, devuelve TRUE.}
var
  i: Integer;
  maxRam: dword;
begin
  Result := false;   //valor inicial
  maxRam := CPUMAXRAM;  //posición máxima
  //Verifica si hay zona de varaibles
  if dataAddr1<>-1 then begin
    //Busca en zona especial
    for i:=dataAddr1 to dataAddr2 do begin
      if (ram[i].state = cs_impleGPR) and (ram[i].used = ruUnused) then begin
        //Esta dirección está libre
        addr := i;
        //Notar que la posición de memoria puede estar mapeada.
        exit(true);  //indica que encontró espacio
      end;
    end;
    //No found. No more space available.
    dataAddr1 := -1;  //Deactivate
    //Continue in the normal RAM
  end;
  //Busca en la zona normal
  for i:=freeStart to maxRam-1 do begin
    if (ram[i].state = cs_impleGPR) and (ram[i].used = ruUnused) then begin
      //Esta dirección está libre
      addr := i;
      //Notar que la posición de memoria puede estar mapeada.
      exit(true);  //indica que encontró espacio
    end;
  end;
end;
function TP6502.GetFreeBytes(const size: integer; ad1, ad2: word; out addr: word
  ): boolean;
{Search a free block of memory in RAM, to locate a block of the specified size (in bytes).
 The searching is done from "ad1" to "ad2".
 If found returns TRUE, and the start address in "addr". }
var
  i: Word;
begin
  for i:=ad1 to ad2 do begin  //verifica 1 a 1, por seguridad
    if HaveConsecRAM(i, size, ad2) then begin
      //Encontró del tamaño buscado
      addr := i;
      exit(true);
    end;
  end;
  exit(false);
end;

function TP6502.TotalMemRAM: integer;
{Devuelve el total de memoria RAM disponible}
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to CPUMAXRAM - 1 do begin
    if ram[i].Avail then begin
      Result := Result + 1;
    end;
  end;
end;
function TP6502.UsedMemRAM: word;
{Devuelve el total de memoria RAM usada}
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to CPUMAXRAM - 1 do begin
    if ram[i].Avail and (ram[i].used<>ruUnused) then begin
      //Notar que "AvailGPR" asegura que no se consideran registros maepados
      Result := Result + 1;
    end;
  end;
end;
procedure TP6502.ExploreUsed(rutExplorRAM: TCPURutExplorRAM);
{Genera un reporte de uso de RAM}
var
  i: Integer;
begin
  for i := 0 to CPUMAXRAM - 1 do begin
    if ram[i].Avail and (ram[i].used<>ruUnused) then begin
      rutExplorRAM(i, @ram[i]);
    end;
  end;
end;
function TP6502.ValidRAMaddr(addr: word): boolean;
{Indica si la dirección indicada es válida dentro del hardware del CPU}
begin
  if addr > CPUMAXRAM then exit(false);   //excede límite
  exit(true);
end;
function TP6502.GetASMlineAt(addr: word;
     incAdrr, incValues, incCom, incVarNam: boolean; out nBytes: byte): string;
{Returns a line of text with an ASM instruction.}
var
  opCode, comLat: string;
begin
  comLat := ram[addr].sideComment;
  //Decodifica instrucción
  opCode := DisassemblerAt(addr, nBytes, incVarNam);  //Instrucción
  //Verificas si incluye dirección física
  Result := '';
  if incAdrr then  begin
    //Agrega dirección al inicio
    Result := '$'+IntToHex(addr,4) + ' ';
  end;
  if incValues then begin
    //Agrega bytes después
    if nBytes = 1 then begin
        Result := Result + IntToHex(ram[addr].value, 2) + '       ' ;
    end else if nBytes = 2  then begin
        Result := Result + IntToHex(ram[addr].value, 2) + ' ' +
                     IntToHex(ram[addr+1].value, 2) + '    ';
    end else if nBytes = 3 then begin
        Result := Result + IntToHex(ram[addr].value, 2) + ' ' +
                     IntToHex(ram[addr+1].value, 2) + ' ' +
                     IntToHex(ram[addr+2].value, 2) + ' ';
    end;
  end;
  Result := Result + opCode;
  //Check if there is lateral comment
  if incCom then begin
    Result := Result  + ' ' + comLat;
  end;
end;
procedure TP6502.DumpCodeAsm(lOut: TStrings;
                             incAdrr, incValues, incCom, incVarNam: boolean);
{Desensambla las instrucciones grabadas en el PIC.
 Se debe llamar despues de llamar a GenHex(), para que se actualicen las variables
 minUsed y maxUsed.}
var
  i: Word;
  lblLin, comLin, lin: String;
  nBytes: byte;
const
  SPACEPAD = '      ';
begin
  //Se supone que minUsed y maxUsed, ya deben haber sido actualizados.
  if incAdrr then begin
    lOut.Add(SPACEPAD + '      ORG $' + IntToHex(minUsed, 4));
  end else begin
    lOut.Add(SPACEPAD + 'ORG $' + IntToHex(minUsed, 4));
  end;
  i := minUsed;
  while i <= maxUsed do begin
    //Read label and comments.
    lblLin := ram[i].name;
    comLin := ram[i].topComment;
    //Check RAM position.
    if ram[i].used in [ruData, ruAbsData] then begin
      //Must be a variable.
      if incAdrr then begin
        if comLin<>'' then lOut.add(comLin);
        lOut.Add( PadRight(lblLin, Length(SPACEPAD)) + '$'+IntToHex(i,4) + ' DB ' +
                  IntToHEx(ram[i].value,2) );
      end else begin
        lOut.Add( PadRight(lblLin, Length(SPACEPAD)) + 'DB ' + IntToHEx(ram[i].value,2) );
      end;
      i := i + 1;
    end else begin
      //Debe ser código o memoria sin usar.
      if lblLin<>'' then lOut.Add(lblLin+':');  //Etiqueta al inicio de línea
      //Escribe comentario al inicio de línea
      if incCom and (comLin<>'') then  begin
        lOut.Add(comLin);
      end;
      lin := GetASMlineAt(i, incAdrr, incValues, incCom, incVarNam, nBytes);
      lOut.Add(SPACEPAD + lin);
      i := i + nBytes;   //Incrementa a siguiente instrucción
    end;
  end;
end;
procedure TP6502.GenHex(hexFile: string; startAddr: integer = -1);
{Genera el archivo *.hex, a partir de los datos almacenados en la memoria RAM.
Actualiza los campos, minUsed y maxUsed.}
var
  i: Integer;
  f: file of byte;
begin
  hexLines.Clear;      //Clear list
  if startAddr = -1 then begin
    //Find first and last byte used
    minUsed := CPUMAXRAM;
    maxUsed := 0;
    for i := 0 to CPUMAXRAM-1 do begin
      //if ram[i].used in [ruCode, ruData] then begin //Changed in versión 0.7.1
      if ram[i].used in [ruCodeOp, ruCodeDa] then begin
        if i<minUsed then minUsed := i;
        if i>maxUsed then maxUsed := i;
      end;
    end;
  end else begin
    //Find only last byte used
    minUsed := startAddr;
    maxUsed := 0;
    for i := minUsed to CPUMAXRAM-1 do begin
      //if ram[i].used in [ruCode, ruData] then begin //Changed in versión 0.7.1
      if ram[i].used in [ruCodeOp, ruCodeDa] then begin
        if i>maxUsed then maxUsed := i;
      end;
    end;
  end;
  //Genera archivo PRG
  AssignFile(f, hexFile);
  Rewrite(f);
  Write(f, minUsed and $ff);
  Write(f, (minUsed >> 8) and $ff);
  for i := minUsed to maxUsed do begin  //Llena buffer
     Write(f, ram[i].value);
  end;
  close(f);
end;
constructor TP6502.Create;
begin
  inherited Create;
  //Default hardware settings
  Model := '6502';
  CPUMAXRAM  := $10000; //Máx RAM memory
  SetLength(ram, CPUMAXRAM);
  //Start a common config
  //InitMemRAM;    For not to waste time in cosntructor.
  iRam := 0;   //Start position
end;
destructor TP6502.Destroy;
begin
  inherited Destroy;
end;
procedure InitTables;
begin
  ///////////////////////////////////////////////////////////////////
  ////////////////// Set instructions information ///////////////////
  // Based on the information from:
  //   http://www.masswerk.at/6502/6502_instruction_set.html
  ///////////////////////////////////////////////////////////////////
  //////////////////////////////
  PIC16InstName[i_ADC].name := 'ADC';  //Add Memory to Accumulator with Carry
  PIC16InstName[i_ADC].AddAddressMode(aImmediat,$69,2,2,0);
  PIC16InstName[i_ADC].AddAddressMode(aZeroPage,$65,2,3,0);
  PIC16InstName[i_ADC].AddAddressMode(aZeroPagX,$75,2,4,0);
  PIC16InstName[i_ADC].AddAddressMode(aAbsolute,$6D,3,4,0);
  PIC16InstName[i_ADC].AddAddressMode(aAbsolutX,$7D,3,4,1);
  PIC16InstName[i_ADC].AddAddressMode(aAbsolutY,$79,3,4,1);
  PIC16InstName[i_ADC].AddAddressMode(aIndirecX,$61,2,6,0);
  PIC16InstName[i_ADC].AddAddressMode(aIndirecY,$71,2,5,1);
  PIC16InstName[i_AND].name := 'AND';  //AND Memory with Accumulator
  PIC16InstName[i_AND].AddAddressMode(aImmediat,$29,2,2,0);
  PIC16InstName[i_AND].AddAddressMode(aZeroPage,$25,2,3,0);
  PIC16InstName[i_AND].AddAddressMode(aZeroPagX,$35,2,4,0);
  PIC16InstName[i_AND].AddAddressMode(aAbsolute,$2D,3,4,0);
  PIC16InstName[i_AND].AddAddressMode(aAbsolutX,$3D,3,4,1);
  PIC16InstName[i_AND].AddAddressMode(aAbsolutY,$39,3,4,1);
  PIC16InstName[i_AND].AddAddressMode(aIndirecX,$21,2,6,0);
  PIC16InstName[i_AND].AddAddressMode(aIndirecY,$31,2,5,1);
  PIC16InstName[i_ASL].name := 'ASL';  //Shift Left One Bit (MemoryorAccumulator)
  PIC16InstName[i_ASL].AddAddressMode(aAcumulat,$0A,1,2,0);
  PIC16InstName[i_ASL].AddAddressMode(aZeroPage,$06,2,5,0);
  PIC16InstName[i_ASL].AddAddressMode(aZeroPagX,$16,2,6,0);
  PIC16InstName[i_ASL].AddAddressMode(aAbsolute,$0E,3,6,0);
  PIC16InstName[i_ASL].AddAddressMode(aAbsolutX,$1E,3,7,0);
  PIC16InstName[i_BCC].name := 'BCC';  //Branch on Carry Clear
  PIC16InstName[i_BCC].AddAddressMode(aRelative,$90,2,2,2);
  PIC16InstName[i_BCS].name := 'BCS';  //Branch on Carry Set
  PIC16InstName[i_BCS].AddAddressMode(aRelative,$B0,2,2,2);
  PIC16InstName[i_BEQ].name := 'BEQ';  //Branch on Result Zero
  PIC16InstName[i_BEQ].AddAddressMode(aRelative,$F0,2,2,2);
  PIC16InstName[i_BIT].name := 'BIT';  //Test Bits in Memory with Accumulator
  PIC16InstName[i_BIT].AddAddressMode(aZeroPage,$24,2,3,0);
  PIC16InstName[i_BIT].AddAddressMode(aAbsolute,$2C,3,4,0);
  PIC16InstName[i_BMI].name := 'BMI';  //Branch on Result Minus
  PIC16InstName[i_BMI].AddAddressMode(aRelative,$30,2,2,2);
  PIC16InstName[i_BNE].name := 'BNE';  //Branch on Result not Zero
  PIC16InstName[i_BNE].AddAddressMode(aRelative,$D0,2,2,2);
  PIC16InstName[i_BPL].name := 'BPL';  //Branch on Result Plus
  PIC16InstName[i_BPL].AddAddressMode(aRelative,$10,2,2,2);
  PIC16InstName[i_BRK].name := 'BRK';  //Force Break
  PIC16InstName[i_BRK].AddAddressMode(aImplicit,$00,1,7,0);
  PIC16InstName[i_BVC].name := 'BVC';  //Branch on Overflow Clear
  PIC16InstName[i_BVC].AddAddressMode(aRelative,$50,2,2,2);
  PIC16InstName[i_BVS].name := 'BVS';  //Branch on Overflow Set
  PIC16InstName[i_BVS].AddAddressMode(aRelative,$70,2,2,2);
  PIC16InstName[i_CLC].name := 'CLC';  //Clear Carry Flag
  PIC16InstName[i_CLC].AddAddressMode(aImplicit,$18,1,2,0);
  PIC16InstName[i_CLD].name := 'CLD';  //Clear Decimal Mode
  PIC16InstName[i_CLD].AddAddressMode(aImplicit,$D8,1,2,0);
  PIC16InstName[i_CLI].name := 'CLI';  //Clear Interrupt Disable Bit
  PIC16InstName[i_CLI].AddAddressMode(aImplicit,$58,1,2,0);
  PIC16InstName[i_CLV].name := 'CLV';  //Clear Overflow Flag
  PIC16InstName[i_CLV].AddAddressMode(aImplicit,$B8,1,2,0);
  PIC16InstName[i_CMP].name := 'CMP';  //Compare Memory with Accumulator
  PIC16InstName[i_CMP].AddAddressMode(aImmediat,$C9,2,2,0);
  PIC16InstName[i_CMP].AddAddressMode(aZeroPage,$C5,2,3,0);
  PIC16InstName[i_CMP].AddAddressMode(aZeroPagX,$D5,2,4,0);
  PIC16InstName[i_CMP].AddAddressMode(aAbsolute,$CD,3,4,0);
  PIC16InstName[i_CMP].AddAddressMode(aAbsolutX,$DD,3,4,1);
  PIC16InstName[i_CMP].AddAddressMode(aAbsolutY,$D9,3,4,1);
  PIC16InstName[i_CMP].AddAddressMode(aIndirecX,$C1,2,6,0);
  PIC16InstName[i_CMP].AddAddressMode(aIndirecY,$D1,2,5,1);
  PIC16InstName[i_CPX].name := 'CPX';  //Compare Memory and Index X
  PIC16InstName[i_CPX].AddAddressMode(aImmediat,$E0,2,2,0);
  PIC16InstName[i_CPX].AddAddressMode(aZeroPage,$E4,2,3,0);
  PIC16InstName[i_CPX].AddAddressMode(aAbsolute,$EC,3,4,0);
  PIC16InstName[i_CPY].name := 'CPY';  //Compare Memory and Index Y
  PIC16InstName[i_CPY].AddAddressMode(aImmediat,$C0,2,2,0);
  PIC16InstName[i_CPY].AddAddressMode(aZeroPage,$C4,2,3,0);
  PIC16InstName[i_CPY].AddAddressMode(aAbsolute,$CC,3,4,0);
  PIC16InstName[i_DEC].name := 'DEC';  //Decrement Memory by One
  PIC16InstName[i_DEC].AddAddressMode(aZeroPage,$C6,2,5,0);
  PIC16InstName[i_DEC].AddAddressMode(aZeroPagX,$D6,2,6,0);
  PIC16InstName[i_DEC].AddAddressMode(aAbsolute,$CE,3,3,0);
  PIC16InstName[i_DEC].AddAddressMode(aAbsolutX,$DE,3,7,0);
  PIC16InstName[i_DEX].name := 'DEX';  //Decrement Index X by One
  PIC16InstName[i_DEX].AddAddressMode(aImplicit,$CA,1,2,0);
  PIC16InstName[i_DEY].name := 'DEY';  //Decrement Index Y by One
  PIC16InstName[i_DEY].AddAddressMode(aImplicit,$88,1,2,0);
  PIC16InstName[i_EOR].name := 'EOR';  //Exclusive-OR Memory with Accumulator
  PIC16InstName[i_EOR].AddAddressMode(aImmediat,$49,2,2,0);
  PIC16InstName[i_EOR].AddAddressMode(aZeroPage,$45,2,3,0);
  PIC16InstName[i_EOR].AddAddressMode(aZeroPagX,$55,2,4,0);
  PIC16InstName[i_EOR].AddAddressMode(aAbsolute,$4D,3,4,0);
  PIC16InstName[i_EOR].AddAddressMode(aAbsolutX,$5D,3,4,1);
  PIC16InstName[i_EOR].AddAddressMode(aAbsolutY,$59,3,4,1);
  PIC16InstName[i_EOR].AddAddressMode(aIndirecX,$41,2,6,0);
  PIC16InstName[i_EOR].AddAddressMode(aIndirecY,$51,2,5,1);
  PIC16InstName[i_INC].name := 'INC';  //Increment Memory by One
  PIC16InstName[i_INC].AddAddressMode(aZeroPage,$E6,2,5,0);
  PIC16InstName[i_INC].AddAddressMode(aZeroPagX,$F6,2,6,0);
  PIC16InstName[i_INC].AddAddressMode(aAbsolute,$EE,3,6,0);
  PIC16InstName[i_INC].AddAddressMode(aAbsolutX,$FE,3,7,0);
  PIC16InstName[i_INX].name := 'INX';  //Increment Index X by One
  PIC16InstName[i_INX].AddAddressMode(aImplicit,$E8,1,2,0);
  PIC16InstName[i_INY].name := 'INY';  //Increment Index Y by One
  PIC16InstName[i_INY].AddAddressMode(aImplicit,$C8,1,2,0);
  PIC16InstName[i_JMP].name := 'JMP';  //Jump to New Location
  PIC16InstName[i_JMP].AddAddressMode(aAbsolute,$4C,3,3,0);
  PIC16InstName[i_JMP].AddAddressMode(aIndirect,$6C,3,5,0);
  PIC16InstName[i_JSR].name := 'JSR';  //Jump to New Location Saving Return Address
  PIC16InstName[i_JSR].AddAddressMode(aAbsolute,$20,3,6,0);
  PIC16InstName[i_LDA].name := 'LDA';  //Load Accumulator with Memory
  PIC16InstName[i_LDA].AddAddressMode(aImmediat,$A9,2,2,0);
  PIC16InstName[i_LDA].AddAddressMode(aZeroPage,$A5,2,3,0);
  PIC16InstName[i_LDA].AddAddressMode(aZeroPagX,$B5,2,4,0);
  PIC16InstName[i_LDA].AddAddressMode(aAbsolute,$AD,3,4,0);
  PIC16InstName[i_LDA].AddAddressMode(aAbsolutX,$BD,3,4,1);
  PIC16InstName[i_LDA].AddAddressMode(aAbsolutY,$B9,3,4,1);
  PIC16InstName[i_LDA].AddAddressMode(aIndirecX,$A1,2,6,0);
  PIC16InstName[i_LDA].AddAddressMode(aIndirecY,$B1,2,5,1);
  PIC16InstName[i_LDX].name := 'LDX';  //Load Index X with Memory
  PIC16InstName[i_LDX].AddAddressMode(aImmediat,$A2,2,2,0);
  PIC16InstName[i_LDX].AddAddressMode(aZeroPage,$A6,2,3,0);
  PIC16InstName[i_LDX].AddAddressMode(aZeroPagY,$B6,2,4,0);
  PIC16InstName[i_LDX].AddAddressMode(aAbsolute,$AE,3,4,0);
  PIC16InstName[i_LDX].AddAddressMode(aAbsolutY,$BE,3,4,1);
  PIC16InstName[i_LDY].name := 'LDY';  //Load Index Y with Memory
  PIC16InstName[i_LDY].AddAddressMode(aImmediat,$A0,2,2,0);
  PIC16InstName[i_LDY].AddAddressMode(aZeroPage,$A4,2,3,0);
  PIC16InstName[i_LDY].AddAddressMode(aZeroPagX,$B4,2,4,0);
  PIC16InstName[i_LDY].AddAddressMode(aAbsolute,$AC,3,4,0);
  PIC16InstName[i_LDY].AddAddressMode(aAbsolutX,$BC,3,4,1);
  PIC16InstName[i_LSR].name := 'LSR';  //Shift One Bit Right (Memory orAccumulator)
  PIC16InstName[i_LSR].AddAddressMode(aAcumulat,$4A,1,2,0);
  PIC16InstName[i_LSR].AddAddressMode(aZeroPage,$46,2,5,0);
  PIC16InstName[i_LSR].AddAddressMode(aZeroPagX,$56,2,6,0);
  PIC16InstName[i_LSR].AddAddressMode(aAbsolute,$4E,3,6,0);
  PIC16InstName[i_LSR].AddAddressMode(aAbsolutX,$5E,3,7,0);
  PIC16InstName[i_NOP].name := 'NOP';  //No Operation
  PIC16InstName[i_NOP].AddAddressMode(aImplicit,$EA,1,2,0);
  PIC16InstName[i_ORA].name := 'ORA';  //OR Memory with Accumulator
  PIC16InstName[i_ORA].AddAddressMode(aImmediat,$09,2,2,0);
  PIC16InstName[i_ORA].AddAddressMode(aZeroPage,$05,2,3,0);
  PIC16InstName[i_ORA].AddAddressMode(aZeroPagX,$15,2,4,0);
  PIC16InstName[i_ORA].AddAddressMode(aAbsolute,$0D,3,4,0);
  PIC16InstName[i_ORA].AddAddressMode(aAbsolutX,$1D,3,4,1);
  PIC16InstName[i_ORA].AddAddressMode(aAbsolutY,$19,3,4,1);
  PIC16InstName[i_ORA].AddAddressMode(aIndirecX,$01,2,6,0);
  PIC16InstName[i_ORA].AddAddressMode(aIndirecY,$11,2,5,1);
  PIC16InstName[i_PHA].name := 'PHA';  //Push Accumulator on Stack
  PIC16InstName[i_PHA].AddAddressMode(aImplicit,$48,1,3,0);
  PIC16InstName[i_PHP].name := 'PHP';  //Push Processor Status on Stack
  PIC16InstName[i_PHP].AddAddressMode(aImplicit,$08,1,3,0);
  PIC16InstName[i_PLA].name := 'PLA';  //Pull Accumulator from Stack
  PIC16InstName[i_PLA].AddAddressMode(aImplicit,$68,1,4,0);
  PIC16InstName[i_PLP].name := 'PLP';  //Pull Processor Status fromStack
  PIC16InstName[i_PLP].AddAddressMode(aImplicit,$28,1,4,0);
  PIC16InstName[i_ROL].name := 'ROL';  //Rotate One Bit Left (Memory orAccumulator)
  PIC16InstName[i_ROL].AddAddressMode(aAcumulat,$2A,1,2,0);
  PIC16InstName[i_ROL].AddAddressMode(aZeroPage,$26,2,5,0);
  PIC16InstName[i_ROL].AddAddressMode(aZeroPagX,$36,2,6,0);
  PIC16InstName[i_ROL].AddAddressMode(aAbsolute,$2E,3,6,0);
  PIC16InstName[i_ROL].AddAddressMode(aAbsolutX,$3E,3,7,0);
  PIC16InstName[i_ROR].name := 'ROR';  //Rotate One Bit Right (Memory or Accumulator)
  PIC16InstName[i_ROR].AddAddressMode(aAcumulat,$6A,1,2,0);
  PIC16InstName[i_ROR].AddAddressMode(aZeroPage,$66,2,5,0);
  PIC16InstName[i_ROR].AddAddressMode(aZeroPagX,$76,2,6,0);
  PIC16InstName[i_ROR].AddAddressMode(aAbsolute,$6E,3,6,0);
  PIC16InstName[i_ROR].AddAddressMode(aAbsolutX,$7E,3,7,0);
  PIC16InstName[i_RTI].name := 'RTI';  //Return from Interrupt
  PIC16InstName[i_RTI].AddAddressMode(aImplicit,$40,1,6,0);
  PIC16InstName[i_RTS].name := 'RTS';  //Return from Subroutine
  PIC16InstName[i_RTS].AddAddressMode(aImplicit,$60,1,6,0);
  PIC16InstName[i_SBC].name := 'SBC';  //Subtract Memory from Accumulator withBorrow
  PIC16InstName[i_SBC].AddAddressMode(aImmediat,$E9,2,2,0);
  PIC16InstName[i_SBC].AddAddressMode(aZeroPage,$E5,2,3,0);
  PIC16InstName[i_SBC].AddAddressMode(aZeroPagX,$F5,2,4,0);
  PIC16InstName[i_SBC].AddAddressMode(aAbsolute,$ED,3,4,0);
  PIC16InstName[i_SBC].AddAddressMode(aAbsolutX,$FD,3,4,1);
  PIC16InstName[i_SBC].AddAddressMode(aAbsolutY,$F9,3,4,1);
  PIC16InstName[i_SBC].AddAddressMode(aIndirecX,$E1,2,6,0);
  PIC16InstName[i_SBC].AddAddressMode(aIndirecY,$F1,2,5,1);
  PIC16InstName[i_SEC].name := 'SEC';  //Set Carry Flag
  PIC16InstName[i_SEC].AddAddressMode(aImplicit,$38,1,2,0);
  PIC16InstName[i_SED].name := 'SED';  //Set Decimal Flag
  PIC16InstName[i_SED].AddAddressMode(aImplicit,$F8,1,2,0);
  PIC16InstName[i_SEI].name := 'SEI';  //Set Interrupt Disable Status
  PIC16InstName[i_SEI].AddAddressMode(aImplicit,$78,1,2,0);
  PIC16InstName[i_STA].name := 'STA';  //Store Accumulator in Memory
  PIC16InstName[i_STA].AddAddressMode(aZeroPage,$85,2,3,0);
  PIC16InstName[i_STA].AddAddressMode(aZeroPagX,$95,2,4,0);
  PIC16InstName[i_STA].AddAddressMode(aAbsolute,$8D,3,4,0);
  PIC16InstName[i_STA].AddAddressMode(aAbsolutX,$9D,3,5,0);
  PIC16InstName[i_STA].AddAddressMode(aAbsolutY,$99,3,5,0);
  PIC16InstName[i_STA].AddAddressMode(aIndirecX,$81,2,6,0);
  PIC16InstName[i_STA].AddAddressMode(aIndirecY,$91,2,6,0);
  PIC16InstName[i_STX].name := 'STX';  //Store Index X in Memory
  PIC16InstName[i_STX].AddAddressMode(aZeroPage,$86,2,3,0);
  PIC16InstName[i_STX].AddAddressMode(aZeroPagY,$96,2,4,0);
  PIC16InstName[i_STX].AddAddressMode(aAbsolute,$8E,3,4,0);
  PIC16InstName[i_STY].name := 'STY';  //Sore Index Y in Memory
  PIC16InstName[i_STY].AddAddressMode(aZeroPage,$84,2,3,0);
  PIC16InstName[i_STY].AddAddressMode(aZeroPagX,$94,2,4,0);
  PIC16InstName[i_STY].AddAddressMode(aAbsolute,$8C,3,4,0);
  PIC16InstName[i_TAX].name := 'TAX';  //Transfer Accumulator to IndexX
  PIC16InstName[i_TAX].AddAddressMode(aImplicit,$AA,1,2,0);
  PIC16InstName[i_TAY].name := 'TAY';  //Transfer Accumulator to IndexY
  PIC16InstName[i_TAY].AddAddressMode(aImplicit,$A8,1,2,0);
  PIC16InstName[i_TSX].name := 'TSX';  //Transfer Stack Pointer toIndex X
  PIC16InstName[i_TSX].AddAddressMode(aImplicit,$BA,1,2,0);
  PIC16InstName[i_TXA].name := 'TXA';  //Transfer Index X to Accumulator
  PIC16InstName[i_TXA].AddAddressMode(aImplicit,$8A,1,2,0);
  PIC16InstName[i_TXS].name := 'TXS';  //Transfer Index X to StackRegister
  PIC16InstName[i_TXS].AddAddressMode(aImplicit,$9A,1,2,0);
  PIC16InstName[i_TYA].name := 'TYA';  //Transfer Index Y to Accumulator
  PIC16InstName[i_TYA].AddAddressMode(aImplicit,$98,1,2,0);

    //New instructions for 65C02
    PIC16InstName[i_BRA].name := 'BRA';  //Branch on Result not Zero
    PIC16InstName[i_BRA].AddAddressMode(aRelative,$80,2,3,0, ONLY_65C02);
    PIC16InstName[i_STZ].name := 'STZ';  //Store Index X in Memory
    PIC16InstName[i_STZ].AddAddressMode(aZeroPage,$64,2,3,0, ONLY_65C02);
    PIC16InstName[i_STZ].AddAddressMode(aZeroPagX,$74,2,4,0, ONLY_65C02);
    PIC16InstName[i_STZ].AddAddressMode(aAbsolute,$9C,3,4,0, ONLY_65C02);
    PIC16InstName[i_STZ].AddAddressMode(aAbsolutX,$9E,3,5,0, ONLY_65C02);
    PIC16InstName[i_PHX].name := 'PHX';
    PIC16InstName[i_PHX].AddAddressMode(aImplicit,$DA,1,3,0, ONLY_65C02);
    PIC16InstName[i_PHY].name := 'PHY';
    PIC16InstName[i_PHY].AddAddressMode(aImplicit,$5A,1,3,0, ONLY_65C02);
    PIC16InstName[i_PLX].name := 'PLX';
    PIC16InstName[i_PLX].AddAddressMode(aImplicit,$FA,1,4,0, ONLY_65C02);
    PIC16InstName[i_PLY].name := 'PLY';
    PIC16InstName[i_PLY].AddAddressMode(aImplicit,$7A,1,4,0, ONLY_65C02);
    PIC16InstName[i_TRB].name := 'TRB';
    PIC16InstName[i_TRB].AddAddressMode(aZeroPage,$14,2,5,0, ONLY_65C02);
    PIC16InstName[i_TRB].AddAddressMode(aAbsolute,$1C,3,6,0, ONLY_65C02);
    PIC16InstName[i_TSB].name := 'TSB';
    PIC16InstName[i_TSB].AddAddressMode(aZeroPage,$04,2,5,0, ONLY_65C02);
    PIC16InstName[i_TSB].AddAddressMode(aAbsolute,$0C,3,6,0, ONLY_65C02);
    PIC16InstName[i_STP].name := 'STP';
    PIC16InstName[i_STP].AddAddressMode(aImplicit,$DB,1,3,0, ONLY_65C02);
    PIC16InstName[i_WAI].name := 'WAI';
    PIC16InstName[i_WAI].AddAddressMode(aImplicit,$CB,1,3,0, ONLY_65C02);

    //New addressing modes for 65C02
    PIC16InstName[i_ADC].AddAddressMode(aIndirecZP,$72,2,5,1, ONLY_65C02);
    PIC16InstName[i_AND].AddAddressMode(aIndirecZP,$32,2,5,0, ONLY_65C02);
    PIC16InstName[i_CMP].AddAddressMode(aIndirecZP,$D2,2,5,0, ONLY_65C02);
    PIC16InstName[i_EOR].AddAddressMode(aIndirecZP,$52,2,5,0, ONLY_65C02);
    PIC16InstName[i_LDA].AddAddressMode(aIndirecZP,$B2,2,5,0, ONLY_65C02);
    PIC16InstName[i_ORA].AddAddressMode(aIndirecZP,$12,2,5,0, ONLY_65C02);
    PIC16InstName[i_SBC].AddAddressMode(aIndirecZP,$F2,2,5,1, ONLY_65C02);
    PIC16InstName[i_STA].AddAddressMode(aIndirecZP,$92,2,5,0, ONLY_65C02);

    PIC16InstName[i_DEC].AddAddressMode(aImplicit,$3A,1,2,0, ONLY_65C02);
    PIC16InstName[i_INC].AddAddressMode(aImplicit,$1A,1,2,0, ONLY_65C02);
    PIC16InstName[i_JMP].AddAddressMode(aAbsInIdX,$7C,3,6,0, ONLY_65C02);

    PIC16InstName[i_BIT].AddAddressMode(aImmediat,$89,2,3,0, ONLY_65C02);
    PIC16InstName[i_BIT].AddAddressMode(aZeroPagX,$34,2,2,0, ONLY_65C02);
    PIC16InstName[i_BIT].AddAddressMode(aAbsolutX,$3C,3,4,0, ONLY_65C02);

    // 65C02 extra WDC set: BBRn, BBSn, RMBn, SMBn
    PIC16InstName[i_RMB0].name := 'RMB0';
    PIC16InstName[i_RMB0].AddAddressMode(aZeroPage,$07,2,5,0, ONLY_65C02);
    PIC16InstName[i_RMB1].name := 'RMB1';
    PIC16InstName[i_RMB1].AddAddressMode(aZeroPage,$17,2,5,0, ONLY_65C02);
    PIC16InstName[i_RMB2].name := 'RMB2';
    PIC16InstName[i_RMB2].AddAddressMode(aZeroPage,$27,2,5,0, ONLY_65C02);
    PIC16InstName[i_RMB3].name := 'RMB3';
    PIC16InstName[i_RMB3].AddAddressMode(aZeroPage,$37,2,5,0, ONLY_65C02);
    PIC16InstName[i_RMB4].name := 'RMB4';
    PIC16InstName[i_RMB4].AddAddressMode(aZeroPage,$47,2,5,0, ONLY_65C02);
    PIC16InstName[i_RMB5].name := 'RMB5';
    PIC16InstName[i_RMB5].AddAddressMode(aZeroPage,$57,2,5,0, ONLY_65C02);
    PIC16InstName[i_RMB6].name := 'RMB6';
    PIC16InstName[i_RMB6].AddAddressMode(aZeroPage,$67,2,5,0, ONLY_65C02);
    PIC16InstName[i_RMB7].name := 'RMB7';
    PIC16InstName[i_RMB7].AddAddressMode(aZeroPage,$77,2,5,0, ONLY_65C02);

    PIC16InstName[i_SMB0].name := 'SMB0';
    PIC16InstName[i_SMB0].AddAddressMode(aZeroPage,$87,2,5,0, ONLY_65C02);
    PIC16InstName[i_SMB1].name := 'SMB1';
    PIC16InstName[i_SMB1].AddAddressMode(aZeroPage,$97,2,5,0, ONLY_65C02);
    PIC16InstName[i_SMB2].name := 'SMB2';
    PIC16InstName[i_SMB2].AddAddressMode(aZeroPage,$A7,2,5,0, ONLY_65C02);
    PIC16InstName[i_SMB3].name := 'SMB3';
    PIC16InstName[i_SMB3].AddAddressMode(aZeroPage,$B7,2,5,0, ONLY_65C02);
    PIC16InstName[i_SMB4].name := 'SMB4';
    PIC16InstName[i_SMB4].AddAddressMode(aZeroPage,$C7,2,5,0, ONLY_65C02);
    PIC16InstName[i_SMB5].name := 'SMB5';
    PIC16InstName[i_SMB5].AddAddressMode(aZeroPage,$D7,2,5,0, ONLY_65C02);
    PIC16InstName[i_SMB6].name := 'SMB6';
    PIC16InstName[i_SMB6].AddAddressMode(aZeroPage,$E7,2,5,0, ONLY_65C02);
    PIC16InstName[i_SMB7].name := 'SMB7';
    PIC16InstName[i_SMB7].AddAddressMode(aZeroPage,$F7,2,5,0, ONLY_65C02);

    PIC16InstName[i_BBR0].name := 'BBR0';
    PIC16InstName[i_BBR0].AddAddressMode(aZeroPRel,$0F,3,5,0, ONLY_65C02);
    PIC16InstName[i_BBR1].name := 'BBR1';
    PIC16InstName[i_BBR1].AddAddressMode(aZeroPRel,$1F,3,5,0, ONLY_65C02);
    PIC16InstName[i_BBR2].name := 'BBR2';
    PIC16InstName[i_BBR2].AddAddressMode(aZeroPRel,$2F,3,5,0, ONLY_65C02);
    PIC16InstName[i_BBR3].name := 'BBR3';
    PIC16InstName[i_BBR3].AddAddressMode(aZeroPRel,$3F,3,5,0, ONLY_65C02);
    PIC16InstName[i_BBR4].name := 'BBR4';
    PIC16InstName[i_BBR4].AddAddressMode(aZeroPRel,$4F,3,5,0, ONLY_65C02);
    PIC16InstName[i_BBR5].name := 'BBR5';
    PIC16InstName[i_BBR5].AddAddressMode(aZeroPRel,$5F,3,5,0, ONLY_65C02);
    PIC16InstName[i_BBR6].name := 'BBR6';
    PIC16InstName[i_BBR6].AddAddressMode(aZeroPRel,$6F,3,5,0, ONLY_65C02);
    PIC16InstName[i_BBR7].name := 'BBR7';
    PIC16InstName[i_BBR7].AddAddressMode(aZeroPRel,$7F,3,5,0, ONLY_65C02);

    PIC16InstName[i_BBS0].name := 'BBS0';
    PIC16InstName[i_BBS0].AddAddressMode(aZeroPRel,$8F,3,5,0, ONLY_65C02);
    PIC16InstName[i_BBS1].name := 'BBS1';
    PIC16InstName[i_BBS1].AddAddressMode(aZeroPRel,$9F,3,5,0, ONLY_65C02);
    PIC16InstName[i_BBS2].name := 'BBS2';
    PIC16InstName[i_BBS2].AddAddressMode(aZeroPRel,$AF,3,5,0, ONLY_65C02);
    PIC16InstName[i_BBS3].name := 'BBS3';
    PIC16InstName[i_BBS3].AddAddressMode(aZeroPRel,$BF,3,5,0, ONLY_65C02);
    PIC16InstName[i_BBS4].name := 'BBS4';
    PIC16InstName[i_BBS4].AddAddressMode(aZeroPRel,$CF,3,5,0, ONLY_65C02);
    PIC16InstName[i_BBS5].name := 'BBS5';
    PIC16InstName[i_BBS5].AddAddressMode(aZeroPRel,$DF,3,5,0, ONLY_65C02);
    PIC16InstName[i_BBS6].name := 'BBS6';
    PIC16InstName[i_BBS6].AddAddressMode(aZeroPRel,$EF,3,5,0, ONLY_65C02);
    PIC16InstName[i_BBS7].name := 'BBS7';
    PIC16InstName[i_BBS7].AddAddressMode(aZeroPRel,$FF,3,5,0, ONLY_65C02);

  PIC16InstName[i_Inval].name := 'Inv';


end;
initialization
  InitTables;
end. //1550
