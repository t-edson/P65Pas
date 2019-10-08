{PICCore

Contains basic definitions applicable to all PIC microcontroller Cores
                                         Created by Tito Hinostroza   28/04/2018
}
unit CPUCore;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LCLProc;
type
  //Union to access bytes of a word
  TWordRec = record
    case byte of
      1 : (W : Word);
      {$IFDEF ENDIAN_LITTLE}
      2 : (L, H: Byte);
      {$ELSE}
      2 : (H, L: Byte);
      {$ENDIF}
    end;

  TCPUCellState = (
     cs_impleSFR,   //Implemented. Used by Oeprative System o Kernel
     cs_impleGPR,   //Implemented. Can be used.
     cs_unimplem    //Not implemented.
  );
  TCPURamUsed = (
     ruUnused,  //(NOT included in PRG output file)
     ruCode,    //Used for code  (included in PRG output file)
     ruData,    //Used for variables  (included in PRG output file)
     ruAbsData  //Used for variables in absolute positions (NOT included in PRG output file)
  );

type //Models for RAM memory

  { TCPURamCell }
  {Modela a una dirección lógica de la memoria RAM. Se ha taratdo de hacer una
  definición eficiente de esta estructura para facilitar la implementación de
  simuladores en tiempo real. Podemos usar un tamaño mediano para este registro,
  porque no esperamos tener muchas celdas de RAM (<1K).}
  TCPURamCellPtr = ^TCPURamCell;
  TCPURamCell = object
  private
    Fvalue  : byte;     //value of the memory
    function Getvalue: byte;
    procedure Setvalue(AValue: byte);
  public
    name   : string;      //Name of the register (for variables)
    used   : TCPURamUsed; //Indicates if have been written
    shared : boolean;     //Used to share this register
    state  : TCPUCellState; //Status of the cell
    property value: byte read Getvalue write Setvalue;
    property dvalue: byte read Fvalue write Fvalue;   //Direct access to "Fvalue".
    function Avail: boolean;  //RAM implemented to use in programs
    function Free: boolean;   //RAM implemented and unused
  public  //Campos para deputación
    breakPnt  : boolean;  //Indicates if this cell have a Breakpoint
    {Be careful on the size of this record, because it's going to be multiplied by 64K}
  public     //Information of position in source code. Used for debug
    rowSrc    : word;     //Row number
    colSrc    : word;     //Column number
    idFile    : SmallInt; //Index to a file. No load the name to save space.
    rowGrid   : word;     //Used to include Grid information when debug.
    {Estos campos de cadena ocupan bastante espacio, aún cuado están en NULL. Si se
    quisiera optimizar el uso de RAM, se podría pensar en codificar, varios campos en
    una sola cadena.}
    topLabel   : string;  //Label on the top of the cell.
    topComment : string;  //Comment on the top of the cell.
    sideComment: string;  //Right comment to code
  end;

  TCPURam = array of TCPURamCell;
  TCPURamPtr = ^TCPURam;
  TCPURutExplorRAM = procedure(offs: word; regPtr: TCPURamCellPtr) of object;

type

  { TCPUCore }
  {Abcestor of all 8 bits CPU cores}
  TCPUCore = class
  private
    function GetTokAddress(var str: string; delimiter: char): word;
  public //Limits
    {This variables are set just one time. So they work as constant.}
    CPUMAXRAM: dword;  //Max virtual RAM used by the CPU
  public   //General fields
    Model    : string;    //modelo de PIC
    frequen  : integer;   //frecuencia del reloj
    MaxFreq  : integer;   //máxima frecuencia del reloj
    //Propiedades que definen la arquitectura del CPU.
    MsjError: string;
  public   //Execution control
    nClck   : Int64;    //Contador de ciclos de reloj
    CommStop: boolean;  //Bandera para detener la ejecución
    OnExecutionMsg: procedure(message: string) of object;  //Genera mensaje en ejecución
  protected  //Generation of PRG files
    minUsed  : dword;         //Dirección menor de la ROM usada
    maxUsed  : dword;         //Dirección mayor de la ROM usdas
    hexLines : TStringList;  //Uusado para crear archivo *.hex
  public  //Memories
    ram    : TCPURam;   //memoria RAM
    iRam   : integer;   //puntero a la memoria RAM, para escribir cuando se ensambla o compila código.
    function DisassemblerAt(addr: word; out nBytesProc: byte; useVarName: boolean
      ): string; virtual; abstract; //Desensambla la instrucción actual
  public  //RAM memory functions
    hasDataAdrr: integer; //Flag/index to indicate a Data block has defined.
    dataAddr1: integer;   //Start address for Data variables (-1 if not used)
    dataAddr2: integer;
    procedure ClearMemRAM;
    procedure DisableAllRAM;
    procedure SetStatRAM(i1, i2: word; status0: TCPUCellState);
    function SetStatRAMCom(strDef: string): boolean;
    function SetDataAddr(strDef: string): boolean;
    function HaveConsecRAM(const i, n: word; maxRam: dword): boolean; //Indica si hay "n" bytes libres
    procedure UseConsecRAM(const i, n: word);  //Ocupa "n" bytes en la posición "i"
    procedure SetSharedUnused;
    procedure SetSharedUsed;
  public  //RAM memory functions
    function UsedMemRAM: word;  //devuelve el total de memoria ram usada
  public  //RAM name managment
    function NameRAM(const addr: word): string;
    procedure SetNameRAM(const addr: word; const nam: string);  //Fija nombre a una celda de RAM
    procedure AddNameRAM(const addr: word; const nam: string);  //Agrega nombre a una celda de RAM
  public  //Execution control
    procedure AddBreakpoint(aPC: word);
    procedure ToggleBreakpoint(aPC: word);
    procedure Exec(aPC: word); virtual; abstract; //Ejecuta la instrucción en la dirección indicada.
    procedure Exec; virtual; abstract; //Ejecuta instrucción actual
    procedure ExecTo(endAdd: word); virtual; abstract; //Ejecuta hasta cierta dirección
    procedure ExecStep; virtual; abstract; //Execute one instruction considering CALL as one instruction
    procedure ExecNCycles(nCyc: integer; out stopped: boolean); virtual; abstract; //Ejecuta hasta cierta dirección
    procedure Reset(hard: boolean); virtual; abstract;
    function ReadPC: dword; virtual; abstract;  //Defined DWORD to cover the 18F PC register
    procedure WritePC(AValue: dword); virtual; abstract;
  public  //Others

    procedure addTopLabel(lbl: string);  //Add a comment to the ASM code
    procedure addTopComm(comm: string; replace: boolean = true);  //Add a comment to the ASM code
    procedure addSideComm(comm: string; before: boolean); //Add lateral comment to the ASM code
    procedure addPosInformation(rowSrc, colSrc: word; idFile: byte);
  public  //Initialization
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TCPURamCell }
function TCPURamCell.Getvalue: byte;
begin
  Result := Fvalue;
end;
procedure TCPURamCell.Setvalue(AValue: byte);
begin
  Fvalue := AValue;
end;
function TCPURamCell.Avail: boolean;
{Indicates if the RAM cell is available for programmer.}
begin
  Result := (state = cs_impleGPR);
end;
function TCPURamCell.Free: boolean;
begin
  Result := (state = cs_impleGPR) and (used = ruUnused);
end;
function TCPUCore.GetTokAddress(var str: string; delimiter: char): word;
{Extract a number (address) from a string and delete until de delimiter.
If delimiter is #0, it's consider all the text.
If fail update the variable "MsjError".}
var
  p: SizeInt;
  n: Longint;
begin
  if delimiter=#0 then begin
    //Until the end of line
    p := length(str) + 1;
  end else begin
    //Until delimiter
    p := pos(delimiter, str);
    if p = 0 then begin
      MsjError := 'Expected "'+delimiter+'".';
      exit(0);
    end;
  end;
  //Have delimiter. Get number
  if not TryStrToInt('$'+copy(str,1,p-1), n) then begin
    MsjError := 'Wrong address.';
    exit(0);
  end;
  delete(str, 1, p);  //delete number and delimiter
  if n<0 then begin
    MsjError := 'Address cannot be negative.';
    exit(0);
  end;
  if n>$FFFF then begin
    MsjError := 'Address cannot be greater than $FFFF.';
    exit(0);
  end;
  exit(n);
end;

{ TCPUCore }
//RAM memory functions
procedure TCPUCore.ClearMemRAM;
{Clear content of RAM. Doesn't change the state.}
var
  i: Integer;
begin
  for i:=0 to high(ram) do begin
    ram[i].dvalue    := $00;
    ram[i].used      := ruUnused;
    ram[i].name      :='';
    ram[i].shared    := false;
    ram[i].breakPnt  := false;
    ram[i].topLabel  := '';
    ram[i].sideComment:= '';
    ram[i].topComment:= '';
    ram[i].idFile    := -1;           //Not initialized.
    {Don't change the implementation. Because "state" must be defined just once at the
    First Pass when processing $CLEAR_STATE_RAM and $SET_STATE_RAM }
//    ram[i].state     := cs_impleGPR;
  end;
end;
procedure TCPUCore.DisableAllRAM;
{Inicia el estado de toda la memoria RAM física definida em el Modelo.
Solo debería usarse, para cuando se va a definir el hardware del dispositivo.}
var
  i: word;
begin
  for i:=0 to high(ram) do begin
    ram[i].state    := cs_unimplem;
  end;
end;
procedure TCPUCore.SetStatRAM(i1, i2: word; status0: TCPUCellState);
{Inicia el campo State, de la memoria. Permite definir el estado real de la memoria RAM.
}
var
  i: Integer;
begin
  for i:=i1 to i2 do begin  //verifica 1 a 1, por seguridad
    if i>CPUMAXRAM-1 then continue;  //protection
    ram[i].state := status0;
  end;
end;
function TCPUCore.SetStatRAMCom(strDef: string): boolean;
{Define the RAM state using a Definitions string.
Definitions string have the format:
<command 1>, <command 2>, ...
Each command have the format:
<addStart>-<addEnd>:<memory state>
One example for Definitions string, is:
   '0000-01FF:SFR, 8000-FFFF:NIM'
If error ocurrs, return FALSE, and the error messaje in MsjError.
}
var
  coms: TStringList;
  add1, add2: longint;
  state: TCPUCellState;
  com, str: String;
begin
  Result := true;
  coms:= TStringList.Create;
  try
    coms.Delimiter := ',';
    coms.DelimitedText := strDef;
    for str in coms do begin
      com := UpCase(trim(str));
      if com='' then continue;
      //Find Address1
      add1 := GetTokAddress(com, '-');
      if MsjError<>'' then begin
        MsjError := 'Memory definition syntax error: ' + MsjError;
        exit(false);
      end;
      //Find Address1
      add2 := GetTokAddress(com, ':');
      if MsjError<>'' then begin
        MsjError := 'Memory definition syntax error: ' + MsjError;
        exit(false);
      end;
      case copy(com,1,3) of
      'SFR': state := cs_impleSFR;
      'GPR': state := cs_impleGPR;
      'NIM': state := cs_unimplem;
      else
        MsjError := 'Memory definition syntax error: Expected SFR, GPR or NIM';
        exit(false);
      end;
      //Ya se tienen los parámetros, para definir la memoria
      SetStatRAM(add1, add2, state);
    end;
  finally
    coms.Destroy;
  end;
end;
function TCPUCore.SetDataAddr(strDef: string): boolean;
{Define primary address to be used for allocating variables.
Definitions string have the format:
<command 1>, <command 2>, ...
Each command have the format:
<addStart>-<addEnd>:<memory state>
One example for Definitions string, is:
   '00F0-00FF'
If error ocurrs, return FALSE, and the error message in MsjError.
}
var
  add1, add2: longint;
  com: String;
begin
  Result := true;

  com := UpCase(trim(strDef));
  if com='' then begin
    hasDataAdrr := -1; //Disable
    exit;
  end;
  //Find Address1
  add1 := GetTokAddress(com, '-');
  if MsjError<>'' then begin
    MsjError := 'Memory definition syntax error: ' + MsjError;
    exit(false);
  end;
  //Find Address1
  add2 := GetTokAddress(com, #0);
  if MsjError<>'' then begin
    MsjError := 'Memory definition syntax error: ' + MsjError;
    exit(false);
  end;
  //Ya se tienen los parámetros, para definir la memoria
  hasDataAdrr := add1;  //Set flag
  dataAddr1 := add1;    //Save
  dataAddr2 := add2;    //Save end
end;
function TCPUCore.HaveConsecRAM(const i, n: word; maxRam: dword): boolean;
{Indica si hay "n" bytes consecutivos libres en la posicióm "i", en RAM.
La búsqueda se hace solo hasta la posición "maxRam"}
var
  c: Integer;
  j: dword;
begin
  Result := false;
  c := 0;
  j := i;
  while (j<=maxRam) and (c<n) do begin
    if not ram[j].Free then exit;
    inc(c);      //verifica siguiente
    inc(j);
  end;
  if j>maxRam then exit;  //no hay más espacio
  //Si llega aquí es porque estaban libres los bloques
  Result := true;
end;
procedure TCPUCore.UseConsecRAM(const i, n: word);
{Marca "n" bytes como usados en la posición de memoria "i", en la RAM.
 Debe haberse verificado previamente que los parámetros son válidos, porque aquí no
 se hará ninguna verificación.}
var j: word;
begin
  for j:=i to i+n-1 do begin
    ram[j].used := ruData;  //todos los bits
  end;
end;
procedure TCPUCore.SetSharedUnused;
{Set positions marked as "shared", as unused for to be used again.}
var
  i: Integer;
begin
  for i:=0 to high(ram) do begin
    if (ram[i].shared) and (ram[i].state = cs_impleGPR) then begin
      ram[i].used := ruUnused;
    end;
  end;
end;
procedure TCPUCore.SetSharedUsed;
{Set positions marked as "shared", as used, for NOT to be used again.}
var
  i: Integer;
begin
  for i:=0 to high(ram) do begin
    if (ram[i].shared) and (ram[i].state = cs_impleGPR) then begin
      ram[i].used := ruData;  //Set as used for variables
    end;
  end;
end;
function TCPUCore.UsedMemRAM: word;
var
  i: Integer;
begin
  Result := 0;
  for i:=$0000 to CPUMAXRAM-1 do begin
    if ram[i].used<>ruUnused then inc(Result);
  end;
end;
//RAM name managment
function TCPUCore.NameRAM(const addr: word): string;
{Devuelve el nombre de una celda de la memoria RAM.}
begin
  Result := ram[addr].name;
end;
procedure TCPUCore.SetNameRAM(const addr: word; const nam: string
  );
{Escribe en el campo "name" de la RAM en la psoición indicada}
begin
   ram[addr].name:=nam;
end;
procedure TCPUCore.AddNameRAM(const addr: word; const nam: string);
{Escribe en el campo "name" de la RAM en la psoición indicada. Si ya existía un nombre,
lo argega después de una coma.}
begin
  if ram[addr].name = '' then begin
    ram[addr].name:=nam;
  end else begin
    ram[addr].name+=','+nam;
  end;
end;
//Execution control
procedure TCPUCore.AddBreakpoint(aPC: word);
//Agrega un punto de interrupción
begin
  if aPC>=CPUMAXRAM then exit;
  ram[aPC].breakPnt := true;
end;
procedure TCPUCore.ToggleBreakpoint(aPC: word);
//COnmuta el estado del Punto de Interrupción, en la posición indicada
begin
  if aPC>=CPUMAXRAM then exit;
  ram[aPC].breakPnt := not ram[aPC].breakPnt;
end;
procedure TCPUCore.addTopLabel(lbl: string);
begin
  ram[iRam].topLabel := lbl;
end;
procedure TCPUCore.addTopComm(comm: string; replace: boolean);
{Agrega un comentario de línea al código en la posición de memoria actual}
begin
  if iRam>=CPUMAXRAM then exit;
  if replace then begin
    ram[iRam].topComment := comm;
  end else begin
    ram[iRam].topComment := ram[iRam].topComment + comm;
  end;
end;
procedure TCPUCore.addSideComm(comm: string; before: boolean);
{Agrega un comentario para que apareza al lado de la instrucción.
 "before" = TRUE -> Se debe llamar después de codificar la instrucción
 "before" = FALSE -> Se debe llamar antes de codificar la instrucción
 }
begin
  if before then begin
    if iRam= 0 then exit;
    ram[iRam-1].sideComment+=comm;   //se agrega al que pudiera haber
  end else begin
    if iRam= 0 then exit;
    ram[iRam].sideComment+=comm;   //se agrega al que pudiera haber
  end;
end;
procedure TCPUCore.addPosInformation(rowSrc, colSrc: word; idFile: byte);
{Agrega information de la posición en el codigo fuente, a la posición actual de la
memoria RAM.}
begin
  ram[iRam].rowSrc := rowSrc;
  ram[iRam].colSrc := colSrc;
  ram[iRam].idFile := idFile;
end;
//Initialization
constructor TCPUCore.Create;
begin
  hasDataAdrr := -1;  //Disable
  hexLines := TStringList.Create;
  frequen := 1000000;    //4MHz
end;
destructor TCPUCore.Destroy;
begin
  hexLines.Destroy;
  inherited Destroy;
end;

initialization
end.
//659
