{Unidad que agrega campos necesarios a la clase TCompilerBase, para la generación de
código con el PIC16F.}
unit GenCodBas_PIC16;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, XpresElementsPIC, XpresTypesPIC, CPUCore, P6502utils,
  Parser, ParserDirec, Globales, MisUtils, LCLType, LCLProc;
const
  STACK_SIZE = 8;      //tamaño de pila para subrutinas en el PIC
  MAX_REGS_AUX_BYTE = 6;   //cantidad máxima de registros a usar
  MAX_REGS_AUX_BIT = 4;    //cantidad máxima de registros bit a usar
  MAX_REGS_STACK_BYTE = 8; //cantidad máxima de registros a usar en la pila
  MAX_REGS_STACK_BIT = 4;  //cantidad máxima de registros a usar en la pila

type
  {Información sobre los saltos con la instrucción kIF_TRUE}
  TIfInfo = record
    igoto  : integer;   //Address where is GOTO
  end;
  { TGenCodBas }
  TGenCodBas = class(TParserDirecBase)
  private
    linRep : string;   //línea para generar de reporte
    posFlash: Integer;
    procedure GenCodPicReqStartCodeGen;
    procedure GenCodPicReqStopCodeGen;
    function GetIdxParArray(out WithBrack: boolean; out par: TOperand): boolean;
    function GetValueToAssign(WithBrack: boolean; arrVar: TxpEleVar; out
      value: TOperand): boolean;
    procedure ProcByteUsed(offs: word; regPtr: TCPURamCellPtr);
    procedure word_ClearItems(const OpPtr: pointer);
    procedure word_GetItem(const OpPtr: pointer);
    procedure word_SetItem(const OpPtr: pointer);
  protected
    //Registros de trabajo
    A      : TPicRegister;     //Registro Interno.
    Z      : TPicRegisterBit;  //Registro Interno.
    C      : TPicRegisterBit;  //Registro Interno.
    H      : TPicRegister;     //Registros de trabajo. Se crean siempre.
    E      : TPicRegister;     //Registros de trabajo. Se crean siempre.
    U      : TPicRegister;     //Registros de trabajo. Se crean siempre.
    //Registros auxiliares
    INDF   : TPicRegister;     //Registro Interno.
    FSR    : TPicRegister;     //Registro Interno.
    procedure PutLabel(lbl: string); inline;
    procedure PutTopComm(cmt: string; replace: boolean = true); inline;
    procedure PutComm(cmt: string); inline;
    procedure PutFwdComm(cmt: string); inline;
    function ReportRAMusage: string;
    function ValidateByteRange(n: integer): boolean;
    function ValidateWordRange(n: integer): boolean;
    function ValidateDWordRange(n: Int64): boolean;
  protected
    procedure GenerateROBdetComment;
    procedure GenerateROUdetComment;
  protected  //Rutinas de gestión de memoria de bajo nivel
    procedure AssignRAM(out addr: word; regName: string; shared: boolean);  //Asigna a una dirección física
    function CreateRegisterByte(RegType: TPicRegType): TPicRegister;
    function CreateRegisterBit(RegType: TPicRegType): TPicRegisterBit;
  protected  //Variables temporales
    {Estas variables temporales, se crean como forma de acceder a campos de una variable
     como varbyte.bit o varword.low. Se almacenan en "varFields" y se eliminan al final}
    varFields: TxpEleVars;  //Contenedor
    function CreateTmpVar(nam: string; eleTyp: TxpEleType): TxpEleVar;
    {Estas variables se usan para operaciones en el generador de código.
     No se almacenan en "varFields". Así se definió al principio, pero podrían también
     almacenarse, asumiendo que no importe crear variables dinámicas.}
    function NewTmpVarWord(rL, rH: TPicRegister): TxpEleVar;
  protected  //Rutinas de gestión de memoria para registros
    varStkByte: TxpEleVar;   //variable byte. Usada para trabajar con la pila
    varStkWord: TxpEleVar;   //variable word. Usada para trabajar con la pila
    function GetAuxRegisterByte: TPicRegister;
    //Gestión de la pila
    function GetStkRegisterByte: TPicRegister;
    function GetVarByteFromStk: TxpEleVar;
    function GetVarWordFromStk: TxpEleVar;
    function FreeStkRegisterBit: boolean;
    function FreeStkRegisterByte: boolean;
    function FreeStkRegisterWord: boolean;
    function FreeStkRegisterDWord: boolean;
  protected  //Rutinas de gestión de memoria para variables
    {Estas rutinas estarían mejor ubicadas en TCompilerBase, pero como dependen del
    objeto "pic", se colocan mejor aquí.}
    procedure AssignRAMinByte(absAdd: integer; var addr: word; regName: string;
      shared: boolean = false);
    procedure CreateVarInRAM(nVar: TxpEleVar; shared: boolean = false);
  protected  //Métodos para fijar el resultado
    //Métodos básicos
    procedure SetResultNull;
    procedure SetResultConst(typ: TxpEleType);
    procedure SetResultVariab(rVar: TxpEleVar; Inverted: boolean = false);
    procedure SetResultExpres(typ: TxpEleType; ChkRTState: boolean = true);
    procedure SetResultVarRef(rVarBase: TxpEleVar);
    procedure SetResultExpRef(rVarBase: TxpEleVar; typ: TxpEleType; ChkRTState: boolean = true);
    //Fija el resultado de ROB como constante.
    procedure SetROBResultConst_bool(valBool: Boolean);
    procedure SetROBResultConst_byte(valByte: integer);
    procedure SetROBResultConst_char(valByte: integer);
    procedure SetROBResultConst_word(valWord: integer);
    //Fija el resultado de ROB como variable
    procedure SetROBResultVariab(rVar: TxpEleVar; Inverted: boolean = false);
    //Fija el resultado de ROB como expresión
    {El parámetro "Opt", es más que nada para asegurar que solo se use con Operaciones
     binarias.}
    procedure SetROBResultExpres_bool(Opt: TxpOperation; Inverted: boolean);
    procedure SetROBResultExpres_byte(Opt: TxpOperation);
    procedure SetROBResultExpres_char(Opt: TxpOperation);
    procedure SetROBResultExpres_word(Opt: TxpOperation);
    //Fija el resultado de ROU
    procedure SetROUResultConst_byte(valByte: integer);
    procedure SetROUResultVariab(rVar: TxpEleVar; Inverted: boolean = false);
    procedure SetROUResultVarRef(rVarBase: TxpEleVar);
    procedure SetROUResultExpres_byte;
    procedure SetROUResultExpRef(rVarBase: TxpEleVar; typ: TxpEleType);
    //Adicionales
    procedure ChangeResultCharToByte;
    function ChangePointerToExpres(var ope: TOperand): boolean;
  protected  //Instrucciones que no manejan el cambio de banco
    function _PC: word;
    function _CLOCK: integer;
    procedure _LABEL(igot: integer);
    //Instrucciones simples
    procedure _BTFSC(const f, b: byte);
    procedure _BTFSS(const f, b: byte);

    procedure _ADC(const k: word);  //AND Absolute/Zeropage
    procedure _ADC(const f: TPicRegister);  //AND Absolute/Zeropage
    procedure _AND(const k: word);
    procedure _AND(const f: TPicRegister);
    procedure _ASL(const f: word);
    procedure _ASLa;
    procedure _LSR(const f: word);
    procedure _LSRa;
    procedure _JMP(const ad: word);
    procedure _JMP_lbl(out igot: integer);
    procedure _JSR(const ad: word);
    procedure _BEQ(const ad: ShortInt);
    procedure _BEQ_lbl(out ibranch: integer);
    procedure _BNE(const ad: ShortInt);
    procedure _BNE_lbl(out ibranch: integer);
    procedure _BCC(const ad: ShortInt);
    procedure _CLC;
    procedure _DEX;
    procedure _DEY;
    procedure _DEC(const f: TPicRegister);
    procedure _EORi(const k: word);
    procedure _EOR(const f: TPicRegister);
    procedure _INC(const f: TPicRegister);
    procedure _INX;
    procedure _INY;
    procedure _LDA(const k: word);
    procedure _LDA(const f: TPicRegister);
    procedure _LDX(const k: word);
    procedure _LDX(const f: TPicRegister);
    procedure _LDY(const k: word);
    procedure _LDY(const f: TPicRegister);
    procedure _NOP;
    procedure _ORA(const k: word);
    procedure _ORA(const f: TPicRegister);
    procedure _PHA; inline;
    procedure _RTS;
    procedure _RTI;
    procedure _SEC;
    procedure _SED;
    procedure _SBC(const k: word);   //SBC Immediate
    procedure _SBC(const f: TPicRegister);  //SBC Absolute/Zeropage
    procedure _STA(const f: TPicRegister);
    procedure _STX(const f: TPicRegister);  //STX Absolute/Zeropage
    procedure _TAX;
    procedure _TAY;
    procedure _TYA;
    procedure _TXA;
    procedure IF_TRUE(OpRes: TOperandPtr; out info: TIfInfo);
    procedure IF_TRUE_END(const info: TIfInfo);
  public     //Acceso a registro de trabajo
    property H_register: TPicRegister read H;
    property E_register: TPicRegister read E;
    property U_register: TPicRegister read U;
  protected  //Funciones de tipos
    //////////////// Tipo Byte /////////////
    procedure byte_LoadToRT(const OpPtr: pointer);
    procedure byte_DefineRegisters;
    procedure byte_SaveToStk;
    procedure byte_GetItem(const OpPtr: pointer);
    procedure byte_SetItem(const OpPtr: pointer);
    procedure byte_ClearItems(const OpPtr: pointer);
    //////////////// Tipo Word /////////////
    procedure word_LoadToRT(const OpPtr: pointer);
    procedure word_DefineRegisters;
    procedure word_SaveToStk;
    procedure word_Low(const OpPtr: pointer);
    procedure word_High(const OpPtr: pointer);
  public     //Acceso a campos del PIC
    function PICName: string; override;
    function RAMmax: integer; override;
  public     //Inicialización
    pic        : TP6502;       //Objeto PIC de la serie 16.
    procedure StartRegs;
    function CompilerName: string; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  procedure SetLanguage;
implementation
var
  TXT_SAVE_W, TXT_SAVE_Z, TXT_SAVE_H, MSG_NO_ENOU_RAM,
  MSG_VER_CMP_EXP, MSG_STACK_OVERF, MSG_NOT_IMPLEM: string;

procedure SetLanguage;
begin
  ParserDirec.SetLanguage;
  {$I ..\language\tra_GenCodBas.pas}
end;
{ TGenCodPic }
procedure TGenCodBas.ProcByteUsed(offs: word; regPtr: TCPURamCellPtr);
begin
  linRep := linRep + regPtr^.name +
            ' DB ' + '$' + IntToHex(offs, 3) + LineEnding;
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
procedure TGenCodBas.GenerateROBdetComment;
{Genera un comentario detallado en el código ASM. Válido solo para
Rutinas de Operación binaria, que es cuando está definido operType, p1, y p2.}
begin
  if incDetComm then begin
    PutTopComm('      ;Oper(' + p1^.StoOpChr + ':' + p1^.Typ.name + ',' +
                                p2^.StoOpChr + ':' + p2^.Typ.name + ')', false);
  end;
end;
procedure TGenCodBas.GenerateROUdetComment;
{Genera un comentario detallado en el código ASM. Válido solo para
Rutinas de Operación unaria, que es cuando está definido operType, y p1.}
begin
  if incDetComm then begin
    PutTopComm('      ;Oper(' + p1^.StoOpChr + ':' + p1^.Typ.name + ')', false);
  end;
end;
//Rutinas de gestión de memoria de bajo nivel
procedure TGenCodBas.AssignRAM(out addr: word; regName: string; shared: boolean);
//Asocia a una dirección física de la memoria del PIC para ser usada como varible.
//Si encuentra error, devuelve el mensaje de error en "MsjError"
begin
  {Esta dirección física, la mantendrá este registro hasta el final de la compilación
  y en teoría, hasta el final de la ejecución de programa en el PIC.}
  if not pic.GetFreeByte(addr, shared) then begin
    GenError(MSG_NO_ENOU_RAM);
    exit;
  end;
  inc(pic.iRam);  //Pasa al siguiente byte.
  pic.SetNameRAM(addr, regName);  //pone nombre a registro
end;
function TGenCodBas.CreateRegisterByte(RegType: TPicRegType): TPicRegister;
{Crea una nueva entrada para registro en listRegAux[], pero no le asigna memoria.
 Si encuentra error, devuelve NIL. Este debería ser el único punto de entrada
para agregar un nuevo registro a listRegAux.}
var
  reg: TPicRegister;
begin
  //Agrega un nuevo objeto TPicRegister a la lista;
  reg := TPicRegister.Create;  //Crea objeto
  reg.typ := RegType;    //asigna tipo
  listRegAux.Add(reg);   //agrega a lista
  if listRegAux.Count > MAX_REGS_AUX_BYTE then begin
    //Se asume que se desbordó la memoria evaluando a alguna expresión
    GenError(MSG_VER_CMP_EXP);
    exit(nil);
  end;
  Result := reg;   //devuelve referencia
end;
function TGenCodBas.CreateRegisterBit(RegType: TPicRegType): TPicRegisterBit;
{Crea una nueva entrada para registro en listRegAux[], pero no le asigna memoria.
 Si encuentra error, devuelve NIL. Este debería ser el único punto de entrada
para agregar un nuevo registro a listRegAux.}
var
  reg: TPicRegisterBit;
begin
  //Agrega un nuevo objeto TPicRegister a la lista;
  reg := TPicRegisterBit.Create;  //Crea objeto
  reg.typ := RegType;    //asigna tipo
  listRegAuxBit.Add(reg);   //agrega a lista
  if listRegAuxBit.Count > MAX_REGS_AUX_BIT then begin
    //Se asume que se desbordó la memoria evaluando a alguna expresión
    GenError(MSG_VER_CMP_EXP);
    exit(nil);
  end;
  Result := reg;   //devuelve referencia
end;
function TGenCodBas.CreateTmpVar(nam: string; eleTyp: TxpEleType): TxpEleVar;
{Crea una variable temporal agregándola al contenedor varFields, que es
limpiado al iniciar la compilación. Notar que la variable temporal creada, no tiene
RAM asiganda.}
var
  tmpVar: TxpEleVar;
begin
  tmpVar:= TxpEleVar.Create;
  tmpVar.name := nam;
  tmpVar.typ := eleTyp;
  tmpVar.havAdicPar := false;
  tmpVar.IsTmp := true;   //Para que se pueda luego identificar.
  varFields.Add(tmpVar);  //Agrega
  Result := tmpVar;
end;
function TGenCodBas.NewTmpVarWord(rL, rH: TPicRegister): TxpEleVar;
{Crea una variable temporal Word, con las direcciones de los registros indicados, y
devuelve la referencia. La variable se crea sin asignación de memoria.}
begin
  Result := TxpEleVar.Create;
  Result.typ := typWord;
  Result.addr0 := rL.addr;  //asigna direcciones
  Result.addr1 := rH.addr;
end;
//Variables temporales
//Rutinas de Gestión de memoria
function TGenCodBas.GetAuxRegisterByte: TPicRegister;
{Devuelve la dirección de un registro de trabajo libre. Si no encuentra alguno, lo crea.
 Si hay algún error, llama a GenError() y devuelve NIL}
var
  reg: TPicRegister;
  regName: String;
begin
  //Busca en los registros creados
  {Notar que no se incluye en la búsqueda a los registros de trabajo. Esto es por un
  tema de orden, si bien podría ser factible, permitir usar algún registro de trabajo no
  usado, como registro auxiliar.}
  for reg in listRegAux do begin
    //Se supone que todos los registros auxiliares, estarán siempre asignados
    if (reg.typ = prtAuxReg) and not reg.used then begin
      reg.used := true;
      exit(reg);
    end;
  end;
  //No encontró ninguno libre, crea uno en memoria
  reg := CreateRegisterByte(prtAuxReg);
  if reg = nil then exit(nil);  //hubo error
  regName := 'aux'+IntToSTr(listRegAux.Count);
  AssignRAM(reg.addr, regName, false);   //Asigna memoria. Puede generar error.
  if HayError then exit;
  reg.assigned := true;  //Tiene memoria asiganda
  reg.used := true;  //marca como usado
  Result := reg;   //Devuelve la referencia
end;
function TGenCodBas.GetStkRegisterByte: TPicRegister;
{Pone un registro de un byte, en la pila, de modo que se pueda luego acceder con
FreeStkRegisterByte(). Si hay un error, devuelve NIL.
Notar que esta no es una pila de memoria en el PIC, sino una emulación de pila
en el compilador.}
var
  reg0: TPicRegister;
  regName: String;
begin
  //Validación
  if stackTop>MAX_REGS_STACK_BYTE then begin
    //Se asume que se desbordó la memoria evaluando a alguna expresión
    GenError(MSG_VER_CMP_EXP);
    exit(nil);
  end;
  if stackTop>listRegStk.Count-1 then begin
    //Apunta a una posición vacía. hay qie agregar
    //Agrega un nuevo objeto TPicRegister a la lista;
    reg0 := TPicRegister.Create;  //Crea objeto
    reg0.typ := prtStkReg;   //asigna tipo
    listRegStk.Add(reg0);    //agrega a lista
    regName := 'stk'+IntToSTr(listRegStk.Count);
    AssignRAM(reg0.addr, regName, false);   //Asigna memoria. Puede generar error.
    if HayError then exit(nil);
  end;
  Result := listRegStk[stackTop];  //toma registro
  Result.assigned := true;
  Result.used := true;   //lo marca
  inc(stackTop);  //actualiza
end;
function TGenCodBas.GetVarByteFromStk: TxpEleVar;
{Devuelve la referencia a una variable byte, que representa al último byte agregado en
la pila. Se usa como un medio de trabajar con los datos de la pila.}
var
  topreg: TPicRegister;
begin
  topreg := listRegStk[stackTop-1];  //toma referencia de registro de la pila
  //Usamos la variable "varStkByte" que existe siempre, para devolver la referencia.
  //Primero la hacemos apuntar a la dirección física de la pila
  varStkByte.addr0 := topReg.addr;
  //Ahora que tenemos ya la variable configurada, devolvemos la referecnia
  Result := varStkByte;
end;
function TGenCodBas.GetVarWordFromStk: TxpEleVar;
{Devuelve la referencia a una variable word, que representa al último word agregado en
la pila. Se usa como un medio de trabajar con los datos de la pila.}
var
  topreg: TPicRegister;
begin
  //Usamos la variable "varStkWord" que existe siempre, para devolver la referencia.
  //Primero la hacemos apuntar a la dirección física de la pila
  topreg := listRegStk[stackTop-1];  //toma referencia de registro de la pila
  varStkWord.addr1 := topreg.addr;
  topreg := listRegStk[stackTop-2];  //toma referencia de registro de la pila
  varStkWord.addr0 := topreg.addr;
  //Ahora que tenemos ya la variable configurada, devolvemos la referencia
  Result := varStkWord;
end;
function TGenCodBas.FreeStkRegisterBit: boolean;
{Libera el último bit, que se pidió a la RAM. Si hubo error, devuelve FALSE.
 Liberarlos significa que estarán disponibles, para la siguiente vez que se pidan}
begin
   if stackTopBit=0 then begin  //Ya está abajo
     GenError(MSG_STACK_OVERF);
     exit(false);
   end;
   dec(stackTopBit);   //Baja puntero
   exit(true);
end;
function TGenCodBas.FreeStkRegisterByte: boolean;
{Libera el último byte, que se pidió a la RAM. Devuelve en "reg", la dirección del último
 byte pedido. Si hubo error, devuelve FALSE.
 Liberarlos significa que estarán disponibles, para la siguiente vez que se pidan}
begin
   if stackTop=0 then begin  //Ya está abajo
     GenError(MSG_STACK_OVERF);
     exit(false);
   end;
   dec(stackTop);   //Baja puntero
   exit(true);
end;
function TGenCodBas.FreeStkRegisterWord: boolean;
{Libera el último word, que se pidió a la RAM. Si hubo error, devuelve FALSE.}
begin
   if stackTop<=1 then begin  //Ya está abajo
     GenError(MSG_STACK_OVERF);
     exit(false);
   end;
   dec(stackTop, 2);   //Baja puntero
   exit(true);
end;
function TGenCodBas.FreeStkRegisterDWord: boolean;
{Libera el último dword, que se pidió a la RAM. Si hubo error, devuelve FALSE.}
begin
   if stackTop<=3 then begin  //Ya está abajo
     GenError(MSG_STACK_OVERF);
     exit(false);
   end;
   dec(stackTop, 4);   //Baja puntero
   exit(true);
end;
////Rutinas de gestión de memoria para variables
procedure TGenCodBas.AssignRAMinByte(absAdd: integer;
  var addr: word; regName: string; shared: boolean = false);
{Asigna RAM a un registro o lo coloca en la dirección indicada.}
begin
  //Obtiene los valores de: offs, bnk, y bit, para el alamacenamiento.
  if absAdd=-1 then begin
    //Caso normal, sin dirección absoluta.
    AssignRAM(addr, regName, shared);
    //Puede salir con error
  end else begin
    //Se debe crear en una posición absoluta
    addr := absAdd;
    //Pone nombre a la celda en RAM, para que pueda desensamblarse con detalle
    pic.SetNameRAM(addr, regName);
    if pic.MsjError<>'' then begin
      GenError(pic.MsjError);
      pic.MsjError := '';  //Para evitar generar otra vez el mensaje
      exit;
    end;
  end;
end;
procedure TGenCodBas.CreateVarInRAM(nVar: TxpEleVar; shared: boolean = false);
{Rutina para asignar espacio físico a una variable. La variable, es creada en memoria
en la posición actual que indica iRam
con los parámetros que posea en ese momento. Si está definida como ABSOLUTE, se le
creará en la posicón indicada. }
var
  varName: String;
  absAdd: integer;
  absBit, nbytes: integer;
  typ: TxpEleType;
  //offs, bnk: byte;
  addr: word;
begin
  //Valores solicitados. Ya deben estar iniciado este campo.
  varName := nVar.name;
  typ := nVar.typ;
  if nVar.adicPar.isAbsol then begin
    absAdd := nVar.adicPar.absAddr;
    if typ.IsBitSize then begin
      absBit := nVar.adicPar.absBit;
    end else begin
      absBit := -1;
    end;
  end else begin
    absAdd  := -1;  //no aplica
    absBit  := -1;  //no aplica
  end;
  //Asigna espacio, de acuerdo al tipo
  if typ = typByte then begin
    AssignRAMinByte(absAdd, nVar.addr0, varName, shared);
  end else if typ = typChar then begin
    AssignRAMinByte(absAdd, nVar.addr0, varName, shared);
  end else if typ = typBool then begin
    AssignRAMinByte(absAdd, nVar.addr0, varName, shared);
  end else if typ = typWord then begin
    //Registra variable en la tabla
    if absAdd = -1 then begin  //Variable normal
      //Los 2 bytes, no necesariamente serán consecutivos (se toma los que estén libres)}
      AssignRAMinByte(-1, nVar.addr0, varName+'@0', shared);
      AssignRAMinByte(-1, nVar.addr1, varName+'@1', shared);
    end else begin             //Variable absoluta
      //Las variables absolutas se almacenarán siempre consecutivas
      AssignRAMinByte(absAdd  , nVar.addr0, varName+'@0');
      AssignRAMinByte(absAdd+1, nVar.addr1, varName+'@1');
    end;
  end else if typ.catType = tctArray then begin
    //Es un arreglo de algún tipo
    if absAdd<>-1 then begin
      //Se pide mapearlo de forma absoluta
      GenError(MSG_NOT_IMPLEM, [varName]);
      exit;
    end;
    //Asignamos espacio en RAM
    nbytes := typ.arrSize * typ.refType.size;
    if not pic.GetFreeBytes(nbytes, addr) then begin
      GenError(MSG_NO_ENOU_RAM);
      exit;
    end;
    pic.SetNameRAM(addr, nVar.name);   //Nombre solo al primer byte
    //Fija dirección física. Se usa solamente "addr0", como referencia, porque
    //no se tienen suficientes registros para modelar todo el arreglo.
    nVar.addr0 := addr;
  end else if typ.catType = tctPointer then begin
    //Es un puntero a algún tipo.
    //Los punteros cortos, se manejan como bytes
    AssignRAMinByte(absAdd, nVar.addr0, varName, shared);
  end else begin
    GenError(MSG_NOT_IMPLEM, [varName]);
  end;
  if HayError then  exit;
  if typ.OnGlobalDef<>nil then typ.OnGlobalDef(varName, '');
end;
//Métodos para fijar el resultado
procedure TGenCodBas.SetResultNull;
{Fija el resultado como NULL.}
begin
  res.SetAsNull;
  BooleanFromC:=false;   //para limpiar el estado
  res.Inverted := false;
end;
procedure TGenCodBas.SetResultConst(typ: TxpEleType);
{Fija los parámetros del resultado de una subexpresion. Este método se debe ejcutar,
siempre antes de evaluar cada subexpresión.}
begin
  res.SetAsConst(typ);
  BooleanFromC:=false;   //para limpiar el estado
  {Se asume que no se necesita invertir la lógica, en una constante (booleana o bit), ya
  que en este caso, tenemos control pleno de su valor}
  res.Inverted := false;
end;
procedure TGenCodBas.SetResultVariab(rVar: TxpEleVar; Inverted: boolean = false);
{Fija los parámetros del resultado de una subexpresion. Este método se debe ejcutar,
siempre antes de evaluar cada subexpresión.}
begin
  res.SetAsVariab(rVar);
  BooleanFromC:=false;   //para limpiar el estado
  //"Inverted" solo tiene sentido, para los tipos bit y boolean
  res.Inverted := Inverted;
end;
procedure TGenCodBas.SetResultExpres(typ: TxpEleType; ChkRTState: boolean = true);
{Fija los parámetros del resultado de una subexpresion (en "res"). Este método se debe
ejecutar, siempre antes de evaluar cada subexpresión. Más exactamente, antes de generar
código para ña subexpresión, porque esta rutina puede generar su propio código.}
begin
  if ChkRTState then begin
    //Se pide verificar si se están suando los RT, para salvarlos en la pila.
    if RTstate<>nil then begin
      //Si se usan RT en la operación anterior. Hay que salvar en pila
      RTstate.SaveToStk;  //Se guardan por tipo
    end else begin
      //No se usan. Están libres
    end;
  end;
  //Fija como expresión
  res.SetAsExpres(typ);
  //Limpia el estado. Esto es útil que se haga antes de generar el código para una operación
  BooleanFromC:=false;
  BooleanFromZ:=false;
  //Actualiza el estado de los registros de trabajo.
  RTstate := typ;
end;
procedure TGenCodBas.SetResultVarRef(rVarBase: TxpEleVar);
begin
  res.SetAsVarRef(rVarBase);
  BooleanFromC:=false;   //para limpiar el estado
  //No se usa "Inverted" en este almacenamiento
  res.Inverted := false;
end;
procedure TGenCodBas.SetResultExpRef(rVarBase: TxpEleVar; typ: TxpEleType; ChkRTState: boolean = true);
begin
  if ChkRTState then begin
    //Se pide verificar si se están suando los RT, para salvarlos en la pila.
    if RTstate<>nil then begin
      //Si se usan RT en la operación anterior. Hay que salvar en pila
      RTstate.SaveToStk;  //Se guardan por tipo
    end else begin
      //No se usan. Están libres
    end;
  end;
  res.SetAsExpRef(rVarBase, typ);
  BooleanFromC:=false;   //para limpiar el estado
  //No se usa "Inverted" en este almacenamiento
  res.Inverted := false;
end;
//Fija el resultado de ROP como constante
procedure TGenCodBas.SetROBResultConst_bool(valBool: Boolean);
begin
  GenerateROBdetComment;
  SetResultConst(typBool);
  res.valBool := valBool;
end;
procedure TGenCodBas.SetROBResultConst_byte(valByte: integer);
begin
  GenerateROBdetComment;
  if not ValidateByteRange(valByte) then
    exit;  //Error de rango
  SetResultConst(typByte);
  res.valInt := valByte;
end;
procedure TGenCodBas.SetROBResultConst_char(valByte: integer);
begin
  GenerateROBdetComment;
  SetResultConst(typChar);
  res.valInt := valByte;
end;
procedure TGenCodBas.SetROBResultConst_word(valWord: integer);
begin
  GenerateROBdetComment;
  if not ValidateWordRange(valWord) then
    exit;  //Error de rango
  SetResultConst(typWord);
  res.valInt := valWord;
end;
//Fija el resultado de ROP como variable
procedure TGenCodBas.SetROBResultVariab(rVar: TxpEleVar; Inverted: boolean);
begin
  GenerateROBdetComment;
  SetResultVariab(rVar, Inverted);
end;
//Fija el resultado de ROP como expresión
procedure TGenCodBas.SetROBResultExpres_bool(Opt: TxpOperation;
  Inverted: boolean);
{Define el resultado como una expresión de tipo Boolean, y se asegura de reservar el
registro Z, para devolver la salida. Debe llamarse cuando se tienen los operandos de
la oepración en p1^y p2^, porque toma información de allí.}
begin
  GenerateROBdetComment;
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if (p1^.Sto = stExpres) or (p2^.Sto = stExpres) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
    SetResultExpres(typBool, false);  //actualiza "RTstate"
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    SetResultExpres(typBool);  //actualiza "RTstate"
  end;
  //Fija la lógica
  res.Inverted := Inverted;
end;
procedure TGenCodBas.SetROBResultExpres_byte(Opt: TxpOperation);
{Define el resultado como una expresión de tipo Byte, y se asegura de reservar el
registro A, para devolver la salida. Debe llamarse cuando se tienen los operandos de
la oepración en p1^y p2^, porque toma información de allí.}
begin
  GenerateROBdetComment;
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if (p1^.Sto = stExpres) or (p2^.Sto = stExpres) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
    SetResultExpres(typByte, false);  //actualiza "RTstate"
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    SetResultExpres(typByte);  //actualiza "RTstate"
  end;
end;
procedure TGenCodBas.SetROBResultExpres_char(Opt: TxpOperation);
{Define el resultado como una expresión de tipo Char, y se asegura de reservar el
registro A, para devolver la salida. Debe llamarse cuando se tienen los operandos de
la oepración en p1^y p2^, porque toma información de allí.}
begin
  GenerateROBdetComment;
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if (p1^.Sto = stExpres) or (p2^.Sto = stExpres) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
    SetResultExpres(typChar, false);  //actualiza "RTstate"
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    SetResultExpres(typChar);  //actualiza "RTstate"
  end;
end;
procedure TGenCodBas.SetROBResultExpres_word(Opt: TxpOperation);
{Define el resultado como una expresión de tipo Word, y se asegura de reservar los
registros H,A, para devolver la salida.}
begin
  GenerateROBdetComment;
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if (p1^.Sto = stExpres) or (p2^.Sto = stExpres) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
    SetResultExpres(typWord, false);
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    SetResultExpres(typWord);
  end;
end;
//Fija el resultado de ROU
procedure TGenCodBas.SetROUResultConst_byte(valByte: integer);
begin
  GenerateROUdetComment;
  if not ValidateByteRange(valByte) then
    exit;  //Error de rango
  SetResultConst(typByte);
  res.valInt := valByte;
end;
procedure TGenCodBas.SetROUResultVariab(rVar: TxpEleVar; Inverted: boolean);
begin
  GenerateROUdetComment;
  SetResultVariab(rVar, Inverted);
end;
procedure TGenCodBas.SetROUResultVarRef(rVarBase: TxpEleVar);
{Fija el resultado como una referencia de tipo stVarRefVar}
begin
  GenerateROUdetComment;
  SetResultVarRef(rVarBase);
end;
procedure TGenCodBas.SetROUResultExpres_byte;
{Define el resultado como una expresión de tipo Byte, y se asegura de reservar el
registro A, para devolver la salida. Se debe usar solo para operaciones unarias.}
begin
  GenerateROUdetComment;
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if (p1^.Sto = stExpres) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
    SetResultExpres(typByte, false);  //actualiza "RTstate"
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    SetResultExpres(typByte);  //actualiza "RTstate"
  end;
end;
procedure TGenCodBas.SetROUResultExpRef(rVarBase: TxpEleVar; typ: TxpEleType);
{Define el resultado como una expresión stVarRefExp, protegiendo los RT si es necesario.
Se debe usar solo para operaciones unarias.}
begin
  GenerateROUdetComment;
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if (p1^.Sto = stExpres) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
    SetResultExpRef(rVarBase, typ, false);  //actualiza "RTstate"
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    SetResultExpRef(rVarBase, typ);  //actualiza "RTstate"
  end;
end;
//Adicionales
procedure TGenCodBas.ChangeResultCharToByte;
begin

end;
function TGenCodBas.ChangePointerToExpres(var ope: TOperand): boolean;
{Convierte un operando de tipo puntero dereferenciado (x^), en una expresión en los RT,
para que pueda ser evaluado, sin problemas, por las ROP.
Si hay error devuelve false.}
begin
  Result := true;
  if ope.Sto = stVarRefVar then begin
    //Se tiene una variable puntero dereferenciada: x^
    {Convierte en expresión, verificando los RT}
    if RTstate<>nil then begin
      //Si se usan RT en la operación anterior. Hay que salvar en pila
      RTstate.SaveToStk;  //Se guardan por tipo
      if HayError then exit(false);
    end;
    //Llama a rutina que mueve el operando a RT
    LoadToRT(ope);
    if HayError then exit(false);  //Por si no está implementado
    //COnfigura después SetAsExpres(), para que LoadToRT(), sepa el almacenamiento de "op"
    ope.SetAsExpres(ope.Typ);  //"ope.Typ" es el tipo al que apunta
    BooleanFromC:=false;
    RTstate := ope.Typ;
  end else if ope.Sto = stVarRefExp then begin
    //Es una expresión.
    {Se asume que el operando tiene su resultado en los RT. SI estuvieran en la pila
    no se aplicaría.}
    //Llama a rutina que mueve el operando a RT
    LoadToRT(ope);
    if HayError then exit(false);  //Por si no está implementado
    //COnfigura después SetAsExpres(), para que LoadToRT(), sepa el almacenamiento de "op"
    ope.SetAsExpres(ope.Typ);  //"ope.Typ" es el tipo al que apunta
    BooleanFromC:=false;
    RTstate := ope.Typ;
  end;
end;
//Rutinas que facilitan la codifición de instrucciones
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
procedure TGenCodBas._LABEL(igot: integer);
{Termina de codificar el GOTO_PEND}
begin
  if pic.ram[igot].value = 0 then begin
    //Es salto absoluto
    pic.ram[igot].value   := lo(_PC);
    pic.ram[igot+1].value := hi(_PC);
  end else begin
    //Es salto relativo
    if _PC > igot then begin
      //Salto hacia adelante
      pic.ram[igot].value := _PC - igot-1;
    end else begin
      //Salto hacia atrás
      pic.ram[igot].value := 256 + (_PC - igot);
    end;
  end;
end;
//Instrucciones simples
{Estas instrucciones no guardan la instrucción compilada en "lastOpCode".}
procedure TGenCodBas._BTFSC(const f, b: byte); inline;
begin
//  pic.flash[pic.iRam].curBnk := CurrBank;
//  pic.codAsmFB(i_BTFSC, f, b);
end;
procedure TGenCodBas._BTFSS(const f, b: byte); inline;
begin
//  pic.flash[pic.iRam].curBnk := CurrBank;
//  pic.codAsmFB(i_BTFSS, f, b);
end;

procedure TGenCodBas._ADC(const k: word);
begin
  pic.codAsm(i_ADC, aImmediat, k);
end;
procedure TGenCodBas._ADC(const f: TPicRegister);  //AND Absolute/Zeropage
begin
  if f.addr<256 then begin
    pic.codAsm(i_AND, aZeroPage, f.addr);
  end else begin
    pic.codAsm(i_AND, aAbsolute, f.addr);
  end;
end;
procedure TGenCodBas._AND(const k: word);
begin
  pic.codAsm(i_AND, aImmediat, k);
end;
procedure TGenCodBas._AND(const f: TPicRegister);  //AND Absolute/Zeropage
begin
  if f.addr<256 then begin
    pic.codAsm(i_AND, aZeroPage, f.addr);
  end else begin
    pic.codAsm(i_AND, aAbsolute, f.addr);
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
procedure TGenCodBas._JMP_lbl(out igot: integer);
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
procedure TGenCodBas._BEQ_lbl(out ibranch: integer);
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
procedure TGenCodBas._BNE_lbl(out ibranch: integer);
begin
  ibranch := pic.iRam+1;  //guarda posición del offset de salto
  pic.codAsm(i_BNE, aRelative, 1);  //1 en Offset indica que se completará con salto relativo
end;
procedure TGenCodBas._BCC(const ad: ShortInt);
begin
  if ad>=0 then begin
    pic.codAsm(i_BCC, aRelative, ad);
  end else begin
    pic.codAsm(i_BCC, aRelative, 256+ad);
  end;
end;
procedure TGenCodBas._CLC;
begin
  pic.codAsm(i_CLC, aImplicit, 0);
end;
procedure TGenCodBas._DEX;
begin
  pic.codAsm(i_DEX, aImplicit, 0);
end;
procedure TGenCodBas._DEY;
begin
  pic.codAsm(i_DEY, aImplicit, 0);
end;
procedure TGenCodBas._DEC(const f: TPicRegister);  //INC Absolute/Zeropage
begin
  if f.addr<256 then begin
    pic.codAsm(i_DEC, aZeroPage, f.addr);
  end else begin
    pic.codAsm(i_DEC, aAbsolute, f.addr);
  end;
end;
procedure TGenCodBas._EOR(const f: TPicRegister);  //EOR Absolute/Zeropage
begin
  if f.addr<256 then begin
    pic.codAsm(i_EOR, aZeroPage, f.addr);
  end else begin
    pic.codAsm(i_EOR, aAbsolute, f.addr);
  end;
end;
procedure TGenCodBas._EORi(const k: word);
begin
  pic.codAsm(i_EOR, aImmediat, k);
end;
procedure TGenCodBas._INC(const f: TPicRegister);  //INC Absolute/Zeropage
begin
  if f.addr<256 then begin
    pic.codAsm(i_INC, aZeroPage, f.addr);
  end else begin
    pic.codAsm(i_INC, aAbsolute, f.addr);
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
procedure TGenCodBas._LDA(const k: word);  //LDA Immediate
begin
  pic.codAsm(i_LDA, aImmediat, k);
end;
procedure TGenCodBas._LDA(const f: TPicRegister);  //LDA Absolute/Zeropage
begin
  if f.addr<256 then begin
    pic.codAsm(i_LDA, aZeroPage, f.addr);
  end else begin
    pic.codAsm(i_LDA, aAbsolute, f.addr);
  end;
end;
procedure TGenCodBas._LDX(const k: word); inline;  //LDA Immediate
begin
  pic.codAsm(i_LDX, aImmediat, k);
end;
procedure TGenCodBas._LDX(const f: TPicRegister);  //LDA  Absolute/Zeropage
begin
  if f.addr<256 then begin
    pic.codAsm(i_LDX, aZeroPage, f.addr);
  end else begin
    pic.codAsm(i_LDX, aAbsolute, f.addr);
  end;
end;
procedure TGenCodBas._LDY(const k: word); inline;  //LDA Immediate
begin
  pic.codAsm(i_LDY, aImmediat, k);
end;
procedure TGenCodBas._LDY(const f: TPicRegister);  //LDA Absolute/Zeropage
begin
  if f.addr<256 then begin
    pic.codAsm(i_LDY, aZeroPage, f.addr);
  end else begin
    pic.codAsm(i_LDY, aAbsolute, f.addr);
  end;
end;
procedure TGenCodBas._NOP; inline;
begin
  pic.codAsm(i_NOP, aImplicit, 0);
end;
procedure TGenCodBas._ORA(const k: word);
begin
  pic.codAsm(i_ORA, aImmediat, k);
end;
procedure TGenCodBas._ORA(const f: TPicRegister);  //ORA Absolute/Zeropage
begin
  if f.addr<256 then begin
    pic.codAsm(i_ORA, aZeroPage, f.addr);
  end else begin
    pic.codAsm(i_ORA, aAbsolute, f.addr);
  end;
end;
procedure TGenCodBas._PHA; inline;
begin
  pic.codAsm(i_PHA, aImplicit, 0);
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
procedure TGenCodBas._SBC(const k: word); inline;  //SBC Immediate
begin
  pic.codAsm(i_SBC, aImmediat, k);
end;
procedure TGenCodBas._SBC(const f: TPicRegister);  //SBC Absolute/Zeropage
begin
  if f.addr<256 then begin
    pic.codAsm(i_SBC, aZeroPage, f.addr);
  end else begin
    pic.codAsm(i_SBC, aAbsolute, f.addr);
  end;
end;
procedure TGenCodBas._STA(const f: TPicRegister);  //STA Absolute/Zeropage
begin
  if f.addr<256 then begin
    pic.codAsm(i_STA, aZeroPage, f.addr);
  end else begin
    pic.codAsm(i_STA, aAbsolute, f.addr);
  end;
end;
procedure TGenCodBas._STX(const f: TPicRegister);  //STA Absolute/Zeropage
begin
  if f.addr<256 then begin
    pic.codAsm(i_STX, aZeroPage, f.addr);
  end else begin
    pic.codAsm(i_STX, aAbsolute, f.addr);
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
procedure TGenCodBas.IF_TRUE(OpRes: TOperandPtr; out info: TIfInfo);
{Conditional instruction. Test if last expression is TRUE. In this case, execute
the following block. The syntax is:

IF_TRUE(offset, bit)
<block of code>
IF_TRUE_END

This instruction require to call to IF_TRUE_END() to define the End of the block.

The block of code can be one or more instructions. The instructions used in the jump
must be optimized, according to the length of the block.
}
begin
  if OpRes^.Sto = stVariab then begin
    //Result in variable
    if OpRes^.Inverted then begin
      _LDA(OpRes^.addr);
      _BEQ_lbl(info.igoto);
    end else begin
      _LDA(OpRes^.addr);
      _BNE_lbl(info.igoto);
    end;
  end else if OpRes^.Sto = stExpres then begin
    //Result in Z flag
    if OpRes^.Inverted then begin
      _BEQ_lbl(info.igoto);
    end else begin
      _BNE_lbl(info.igoto);
    end;
  end else begin
    genError('Not implemented.');
  end;
end;
procedure TGenCodBas.IF_TRUE_END(const info: TIfInfo);
{Define the End of the block, created with IF_TRUE().}
begin
  _LABEL(info.igoto);  //termina de codificar el salto
end;

function TGenCodBas.PICName: string;
begin
  Result := pic.Model;
end;
function TGenCodBas.GetIdxParArray(out WithBrack: boolean; out par: TOperand): boolean;
{Extrae el primer parámetro (que corresponde al índice) de las funciones getitem() o
setitem(). También reconoce las formas con corchetes [], y en ese caso pone "WithBrackets"
en TRUE. Si encuentra error, devuelve false.}
begin
  if cIn.tok = '[' then begin
    //Es la sintaxis a[i];
    WithBrack := true;
    cIn.Next;  //Toma "["
  end else begin
    //Es la sintaxis a.item(i);
    WithBrack := false;
    cIn.Next;  //Toma identificador de campo
    //Captura parámetro
    if not CaptureTok('(') then exit(false);
  end;
  par := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit(false);
  if par.Typ <> typByte then begin
    GenError('Expected byte as index.');
  end;
  if HayError then exit(false);
  exit(true);
end;
function TGenCodBas.GetValueToAssign(WithBrack: boolean; arrVar: TxpEleVar; out value: TOperand): boolean;
{Lee el segundo parámetro de SetItem y devuelve en "value". Valida que sea sel tipo
correcto. Si hay error, devuelve FALSE.}
var
  typItem: TxpEleType;
begin
  if WithBrack then begin
    if not CaptureTok(']') then exit(false);
    cIn.SkipWhites;
    {Legalmente, aquí podría seguir otro operador, o función como ".bit0", y no solo
    ":=". Esto es una implementación algo limitada. Lo que debería hacerse, si no se
    encuentra ":=", sería devolver una referencia a variable, tal vez a un nuevo tipo
    de variable, con dirección "indexada", pero obligaría a crear un atributo más a
    las varaibles. El caso de un índice constante es más sencillo de procesar.}
    if not CaptureTok(':=') then exit(false);
  end else begin
    if not CaptureTok(',') then exit(false);
  end;
  value := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  typItem := arrVar.typ.refType;
  if value.Typ <> typItem then begin  //Solo debería ser byte o char
    if (value.Typ = typByte) and (typItem = typWord) then begin
      //Son tipos compatibles
      value.SetAsConst(typWord);   //Cmabiamos el tipo
    end else begin
      GenError('%s expression expected.', [typItem.name]);
      exit(false);
    end;
  end;
  exit(true);
end;
//////////////// Tipo Byte /////////////
procedure TGenCodBas.byte_LoadToRT(const OpPtr: pointer);
{Carga operando a registros de trabajo.}
var
  Op: ^TOperand;
  varPtr: TxpEleVar;
begin
  Op := OpPtr;
  case Op^.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    _LDA(Op^.valInt);
  end;
  stVariab: begin
    _LDA(Op^.rVar.adrByte0.addr);
  end;
  stExpres: begin  //ya está en A
  end;
  stVarRefVar: begin
    ////Se tiene una variable puntero dereferenciada: x^
    //varPtr := Op^.rVar;  //Guarda referencia a la variable puntero
    ////Mueve a A
    //kMOVF(varPtr.adrByte0, toW);
    //kMOVWF(FSR);  //direcciona
    //kMOVF(INDF, toW);  //deje en A
  end;
  stVarRefExp: begin
//    //Es una expresión derefernciada (x+a)^.
//    {Se asume que el operando tiene su resultado en los RT. Si estuvieran en la pila
//    no se aplicaría.}
//    //Mueve a A
//    _MOVWF(FSR.addr);  //direcciona
//    _MOVF(0, toW);  //deje en A
  end;
  else
    //Almacenamiento no implementado
    GenError(MSG_NOT_IMPLEM);
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
procedure TGenCodBas.byte_GetItem(const OpPtr: pointer);
//Función que devuelve el valor indexado
var
  Op: ^TOperand;
  arrVar, tmpVar: TxpEleVar;
  idx: TOperand;
  WithBrack: Boolean;
begin
//  if not GetIdxParArray(WithBrack, idx) then exit;
//  //Procesa
//  Op := OpPtr;
//  if Op^.Sto = stVariab then begin
//    //Se aplica a una variable array. Lo Normal.
//    arrVar := Op^.rVar;  //referencia a la variable.
//    //Genera el código de acuerdo al índice
//    case idx.Sto of
//    stConst: begin  //ïndice constante
//        tmpVar := CreateTmpVar('', typByte);
//        tmpVar.addr0 := arrVar.addr0 + idx.valInt;  //¿Y si es de otro banco?
//        SetResultVariab(tmpVar);
//      end;
//    stVariab: begin
//        SetResultExpres(arrVar.typ.refType, true);  //Es array de bytes, o Char, devuelve Byte o Char
//        LoadToRT(idx);   //Lo deja en A
//        _ADDLW(arrVar.addr0);   //agrega OFFSET
//        _MOVWF(04);     //direcciona con FSR
//        _MOVF(0, toW);  //lee indexado en A
//    end;
//    stExpres: begin
//        SetResultExpres(arrVar.typ.refType, false);  //Es array de bytes, o Char, devuelve Byte o Char
//        LoadToRT(idx);   //Lo deja en A
//        _ADDLW(arrVar.addr0);   //agrega OFFSET
//        _MOVWF(04);     //direcciona con FSR
//        _MOVF(0, toW);  //lee indexado en A
//      end;
//    end;
//  end else begin
//    GenError('Syntax error.');
//  end;
//  if WithBrack then begin
//    if not CaptureTok(']') then exit;
//  end else begin
//    if not CaptureTok(')') then exit;
//  end;
end;
procedure TGenCodBas.byte_SetItem(const OpPtr: pointer);
//Función que fija un valor indexado
var
  WithBrack: Boolean;
  Op: ^TOperand;
  arrVar, rVar: TxpEleVar;
  idx, value: TOperand;
  idxTar: word;
begin
//  if not GetIdxParArray(WithBrack, idx) then exit;
//  //Procesa
//  Op := OpPtr;
//  if Op^.Sto = stVariab then begin  //Se aplica a una variable
//    arrVar := Op^.rVar;  //referencia a la variable.
//    res.SetAsNull;  //No devuelve nada
//    //Genera el código de acuerdo al índice
//    case idx.Sto of
//    stConst: begin  //ïndice constante
//        //Como el índice es constante, se puede acceder directamente
//        idxTar := arrVar.adrByte0.offs+idx.valInt; //índice destino
//        if not GetValueToAssign(WithBrack, arrVar, value) then exit;
//        if (value.Sto = stConst) and (value.valInt=0) then begin
//          //Caso especial, se pone a cero
//          _LDA(0);
//          _STA(idxTar);
//        end else begin
//          //Sabemos que hay una expresión byte
//          LoadToRT(value); //Carga resultado en A
//          _MOVWF(idxTar);  //Mueve a arreglo
//        end;
//      end;
//    stVariab: begin
//        //El índice es una variable
//        //Tenemos la referencia la variable en idx.rvar
//        if not GetValueToAssign(WithBrack, arrVar, value) then exit;
//        //Sabemos que hay una expresión byte
//        if (value.Sto = stConst) and (value.valInt=0) then begin
//          //Caso especial, se pide asignar una constante cero
//          _MOVF(idx.offs, toW);  //índice
//          _ADDLW(arrVar.addr0);  //Dirección de inicio
//          _MOVWF($04);  //Direcciona
//          _LDA(0);
//          _STA(00);
//        end else if value.Sto = stConst then begin
//          //Es una constante cualquiera
//          _MOVF(idx.offs, toW);  //índice
//          _ADDLW(arrVar.addr0);  //Dirección de inicio
//          _MOVWF($04);  //Direcciona
//          _LDA(value.valInt);
//          _MOVWF($00);   //Escribe valor
//        end else if value.Sto = stVariab then begin
//          //Es una variable
//          _MOVF(idx.offs, toW);  //índice
//          _ADDLW(arrVar.addr0);  //Dirección de inicio
//          _MOVWF($04);  //Direcciona
//          _MOVF(value.offs, toW);
//          _MOVWF($00);   //Escribe valor
//        end else begin
//          //Es una expresión. El resultado está en A
//          //hay que mover value a arrVar[idx.rvar]
//          typWord.DefineRegister;   //Para usar H
//          _MOVWF(H.offs);  //A->H   salva H
//          _MOVF(idx.offs, toW);  //índice
//          _ADDLW(arrVar.addr0);  //Dirección de inicio
//          _MOVWF($04);  //Direcciona
//          _MOVF(H.offs, toW);
//          _MOVWF($00);   //Escribe valor
//        end;
//      end;
//    stExpres: begin
//      //El índice es una expresión y está en A.
//      if not GetValueToAssign(WithBrack, arrVar, value) then exit;
//      //Sabemos que hay una expresión byte
//      if (value.Sto = stConst) and (value.valInt=0) then begin
//        //Caso especial, se pide asignar una constante cero
//        _ADDLW(arrVar.addr0);  //Dirección de inicio
//        _MOVWF($04);  //Direcciona
//        _LDA(0);
//        _STA(0);
//      end else if value.Sto = stConst then begin
//        //Es una constante cualquiera
//        _ADDLW(arrVar.addr0);  //Dirección de inicio
//        _MOVWF($04);  //Direcciona
//        _LDA(value.valInt);
//        _MOVWF($00);   //Escribe valor
//      end else if value.Sto = stVariab then begin
//        //Es una variable
//        _ADDLW(arrVar.addr0);  //Dirección de inicio
//        _MOVWF(FSR.offs);  //Direcciona
//        _MOVF(value.offs, toW);
//        _MOVWF($00);   //Escribe valor
//      end else begin
//        //Es una expresión. El valor a asignar está en A, y el índice en la pila
//        typWord.DefineRegister;   //Para usar H
//        _MOVWF(H.offs);  //A->H   salva valor a H
//        rVar := GetVarByteFromStk;  //toma referencia de la pila
//        _MOVF(rVar.adrByte0.offs, toW);  //índice
//        _ADDLW(arrVar.addr0);  //Dirección de inicio
//        _MOVWF($04);  //Direcciona
//        _MOVF(H.offs, toW);
//        _MOVWF($00);   //Escribe valor
//        FreeStkRegisterByte;   //Para liberar
//      end;
//      end;
//    end;
//  end else begin
//    GenError('Syntax error.');
//  end;
//  if WithBrack then begin
//    //En este modo, no se requiere ")"
//  end else begin
//    if not CaptureTok(')') then exit;
//  end;
end;
procedure TGenCodBas.byte_ClearItems(const OpPtr: pointer);
{Limpia el contenido de todo el arreglo}
var
  Op: ^TOperand;
  xvar: TxpEleVar;
  j1: Word;
begin
//  cIn.Next;  //Toma identificador de campo
//  //Limpia el arreglo
//  Op := OpPtr;
//  case Op^.Sto of
//  stVariab: begin
//    xvar := Op^.rVar;  //Se supone que debe ser de tipo ARRAY
//    res.SetAsConst(typByte);  //Realmente no es importante devolver un valor
//    res.valInt {%H-}:= xvar.typ.arrSize;  //Devuelve tamaño
//    if xvar.typ.arrSize = 0 then exit;  //No hay nada que limpiar
//    if xvar.typ.arrSize = 1 then begin  //Es de un solo byte
//      _LDA(0);
//      _STA(xvar.addr0);
//    end else if xvar.typ.arrSize = 2 then begin  //Es de 2 bytes
//      _LDA(0);
//      _STA(xvar.addr0);
//      _STA(xvar.addr0+1);
//    end else if xvar.typ.arrSize = 3 then begin  //Es de 3 bytes
//      _LDA(0);
//      _STA(xvar.addr0);
//      _STA(xvar.addr0+1);
//      _STA(xvar.addr0+2);
//    end else if xvar.typ.arrSize = 4 then begin  //Es de 4 bytes
//      _LDA(0);
//      _STA(xvar.addr0);
//      _STA(xvar.addr0+1);
//      _STA(xvar.addr0+2);
//      _STA(xvar.addr0+3);
//    end else if xvar.typ.arrSize = 5 then begin  //Es de 5 bytes
//      _LDA(0);
//      _STA(xvar.addr0);
//      _STA(xvar.addr0+1);
//      _STA(xvar.addr0+2);
//      _STA(xvar.addr0+3);
//      _STA(xvar.addr0+4);
//    end else if xvar.typ.arrSize = 6 then begin  //Es de 6 bytes
//      _LDA(0);
//      _STA(xvar.addr0);
//      _STA(xvar.addr0+1);
//      _STA(xvar.addr0+2);
//      _STA(xvar.addr0+3);
//      _STA(xvar.addr0+4);
//      _STA(xvar.addr0+5);
//    end else begin
//      //Implementa lazo, usando A como índice
//      _LDA(xvar.adrByte0.offs);  //dirección inicial
//      _MOVWF($04);   //FSR
//      _LDA(256-xvar.typ.arrSize);
//j1:= _PC;
//      _LDA(0);
//      _STA(0);
//      _INCF($04, toF);    //Siguiente
//      _ADDLW(1);   //A = A + 1
//      _BTFSS(_STATUS, _Z);
//      _JMP(j1);
//    end;
//  end;
//  else
//    GenError('Syntax error.');
//  end;
end;
//////////////// Tipo Word /////////////
procedure TGenCodBas.word_LoadToRT(const OpPtr: pointer);
{Carga el valor de una expresión a los registros de trabajo.}
var
  Op: ^TOperand;
  varPtr: TxpEleVar;
begin
  Op := OpPtr;
  case Op^.Sto of  //el parámetro debe estar en "Op^"
  stConst : begin
    //byte alto
    _LDA(Op^.HByte);
    _STA(H);
    //byte bajo
    _LDA(Op^.LByte);
  end;
  stVariab: begin
    _LDA(Op^.rVar.adrByte1.addr);
    _STA(H);
    _LDA(Op^.rVar.adrByte0.addr);
  end;
  stExpres: begin  //se asume que ya está en (H,A)
  end;
  stVarRefVar: begin
    ////Se tiene una variable puntero dereferenciada: x^
    //varPtr := Op^.rVar;  //Guarda referencia a la variable puntero
    ////Mueve a A
    //kINCF(varPtr.adrByte0, toW);  //varPtr.addr+1 -> A  (byte alto)
    //_MOVWF(FSR.addr);  //direcciona byte alto
    //_MOVF(0, toW);  //deje en A
    //_MOVWF(H.addr);  //Guarda byte alto
    //_DECF(FSR.addr,toF);
    //_MOVF(0, toW);  //deje en A byte bajo
  end;
  stVarRefExp: begin
//    //Es una expresión desrefernciada (x+a)^.
//    {Se asume que el operando tiene su resultado en los RT. Si estuvieran en la pila
//    no se aplicaría.}
//    //Mueve a A
//    _MOVWF(FSR.addr);  //direcciona byte bajo
//    _INCF(FSR.addr,toF);  //apunta a byte alto
//    _MOVF(0, toW);  //deje en A
//    _MOVWF(H.addr);  //Guarda byte alto
//    _DECF(FSR.addr,toF);
//    _MOVF(0, toW);  //deje en A byte bajo
  end;
  else
    //Almacenamiento no implementado
    GenError(MSG_NOT_IMPLEM);
  end;
end;
procedure TGenCodBas.word_DefineRegisters;
begin
  //Aparte de A, solo se requiere H
  if not H.assigned then begin
    AssignRAM(H.addr, '_H', false);
    H.assigned := true;
    H.used := false;
  end;
end;
procedure TGenCodBas.word_SaveToStk;
begin
  //guarda A
  _PHA;
  //guarda H
  _LDA(H);
  _PHA;
end;
procedure TGenCodBas.word_GetItem(const OpPtr: pointer);
//Función que devuelve el valor indexado
var
  Op: ^TOperand;
  arrVar, tmpVar: TxpEleVar;
  idx: TOperand;
  WithBrack: Boolean;
begin
  if not GetIdxParArray(WithBrack, idx) then exit;
  //Procesa
  Op := OpPtr;
  if Op^.Sto = stVariab then begin  //Se aplica a una variable
    arrVar := Op^.rVar;  //referencia a la variable.
    typWord.DefineRegister;
    //Genera el código de acuerdo al índice
    case idx.Sto of
    stConst: begin  //ïndice constante
      tmpVar := CreateTmpVar('', typWord);
      tmpVar.addr0 := arrVar.addr0+idx.valInt*2;  //¿Y si es de otro banco?
      tmpVar.addr1 := arrVar.addr0+idx.valInt*2+1;  //¿Y si es de otro banco?
      SetResultVariab(tmpVar);
//        SetResultExpres(arrVar.typ.refType, true);  //Es array de word, devuelve word
//        //Como el índice es constante, se puede acceder directamente
//        add0 := arrVar.adrByte0.offs+idx.valInt*2;
//        _MOVF(add0+1, toW);
//        _MOVWF(H.offs);    //byte alto
//        _MOVF(add0, toW);  //byte bajo
      end;
    stVariab: begin
      SetResultExpres(arrVar.typ.refType, true);  //Es array de word, devuelve word
      //_BCF(_STATUS, _C);
      //_RLF(idx.offs, toW);           //Multiplica Idx por 2
      //_ADDLW(arrVar.addr0+1);   //Agrega OFFSET + 1
      //_MOVWF(FSR.offs);     //direcciona con FSR
      //_MOVF(0, toW);  //lee indexado en A
      //_MOVWF(H.offs);    //byte alto
      //_DECF(FSR.offs, toF);
      //_MOVF(0, toW);  //lee indexado en A
    end;
    stExpres: begin
      SetResultExpres(arrVar.typ.refType, false);  //Es array de word, devuelve word
      //_MOVWF(FSR.offs);     //idx a  FSR (usa como varaib. auxiliar)
      //_BCF(_STATUS, _C);
      //_RLF(FSR.offs, toW);         //Multiplica Idx por 2
      //_ADDLW(arrVar.addr0+1);   //Agrega OFFSET + 1
      //_MOVWF(FSR.offs);     //direcciona con FSR
      //_MOVF(0, toW);  //lee indexado en A
      //_MOVWF(H.offs);    //byte alto
      //_DECF(FSR.offs, toF);
      //_MOVF(0, toW);  //lee indexado en A
    end;
    end;
  end else begin
    GenError('Syntax error.');
  end;
  if WithBrack then begin
    if not CaptureTok(']') then exit;
  end else begin
    if not CaptureTok(')') then exit;
  end;
end;
procedure TGenCodBas.word_SetItem(const OpPtr: pointer);
//Función que fija un valor indexado
var
  WithBrack: Boolean;
var
  Op: ^TOperand;
  arrVar, rVar: TxpEleVar;
  idx, value: TOperand;
  idxTar: Int64;
  aux: TPicRegister;
begin
//  if not GetIdxParArray(WithBrack, idx) then exit;
//  //Procesa
//  Op := OpPtr;
//  if Op^.Sto = stVariab then begin  //Se aplica a una variable
//    arrVar := Op^.rVar;  //referencia a la variable.
//    res.SetAsNull;  //No devuelve nada
//    //Genera el código de acuerdo al índice
//    case idx.Sto of
//    stConst: begin  //Indice constante
//        //Como el índice es constante, se puede acceder directamente
//        idxTar := arrVar.adrByte0.offs+idx.valInt*2; //índice destino
//        if not GetValueToAssign(WithBrack, arrVar, value) then exit;
//        if value.Sto = stConst then begin
//          //Es una constante
//          //Byte bajo
//          if value.LByte=0 then begin //Caso especial
//            _CLRF(idxTar);
//          end else begin
//            _LDAi(value.LByte);
//            _MOVWF(idxTar);
//          end;
//          //Byte alto
//          if value.HByte=0 then begin //Caso especial
//            _CLRF(idxTar+1);
//          end else begin
//            _LDAi(value.HByte);
//            _MOVWF(idxTar+1);
//          end;
//        end else begin
//          //El valor a asignar es variable o expresión
//          typWord.DefineRegister;   //Para usar H
//          //Sabemos que hay una expresión word
//          LoadToRT(value); //Carga resultado en H,A
//          _MOVWF(idxTar);  //Byte bajo
//          _MOVF(H.offs, toW);
//          _MOVWF(idxTar+1);  //Byte alto
//        end;
//      end;
//    stVariab: begin
//        //El índice es una variable
//        //Tenemos la referencia la variable en idx.rvar
//        if not GetValueToAssign(WithBrack, arrVar, value) then exit;
//        //Sabemos que hay una expresión word
//        if value.Sto = stConst then begin
//          //El valor a escribir, es una constante cualquiera
//          _BCF(_STATUS, _C);
//          _RLF(idx.offs, toW);  //índice * 2
//          _ADDLW(arrVar.addr0);  //Dirección de inicio
//          _MOVWF(FSR.offs);  //Direcciona
//          ////// Byte Bajo
//          if value.LByte = 0 then begin
//            _CLRF($00);
//          end else begin
//            _LDAi(value.LByte);
//            _MOVWF($00);   //Escribe
//          end;
//          ////// Byte Alto
//          _INCF(FSR.offs, toF);  //Direcciona a byte ALTO
//          if value.HByte = 0 then begin
//            _CLRF($00);
//          end else begin
//            _LDAi(value.HByte);
//            _MOVWF($00);   //Escribe
//          end;
//        end else if value.Sto = stVariab then begin
//          //El valor a escribir, es una variable
//          //Calcula dirfección de byte bajo
//          _BCF(_STATUS, _C);
//          _RLF(idx.offs, toW);  //índice * 2
//          _ADDLW(arrVar.addr0);  //Dirección de inicio
//          _MOVWF(FSR.offs);  //Direcciona
//          ////// Byte Bajo
//          _MOVF(value.Loffs, toW);
//          _MOVWF($00);   //Escribe
//          ////// Byte Alto
//          _INCF(FSR.offs, toF);  //Direcciona a byte ALTO
//          _MOVF(value.Hoffs, toW);
//          _MOVWF($00);   //Escribe
//        end else begin
//          //El valor a escribir, es una expresión y está en H,A
//          //hay que mover value a arrVar[idx.rvar]
//          aux := GetAuxRegisterByte;
//          typWord.DefineRegister;   //Para usar H
//          _MOVWF(aux.offs);  //A->   salva A (Valor.H)
//          //Calcula dirección de byte bajo
//          _BCF(_STATUS, _C);
//          _RLF(idx.offs, toW);  //índice * 2
//          _ADDLW(arrVar.addr0);  //Dirección de inicio
//          _MOVWF(FSR.offs);  //Direcciona
//          ////// Byte Bajo
//          _MOVF(aux.offs, toW);
//          _MOVWF($00);   //Escribe
//          ////// Byte Alto
//          _INCF(FSR.offs, toF);  //Direcciona a byte ALTO
//          _MOVF(H.offs, toW);
//          _MOVWF($00);   //Escribe
//          aux.used := false;
//        end;
//      end;
//    stExpres: begin
//      //El índice es una expresión y está en A.
//      if not GetValueToAssign(WithBrack, arrVar, value) then exit;
//      if value.Sto = stConst then begin
//        //El valor a asignar, es una constante
//        _MOVWF(FSR.offs);   //Salva A.
//        _BCF(_STATUS, _C);
//        _RLF(FSR.offs, toW);  //idx * 2
//        _ADDLW(arrVar.addr0);  //Dirección de inicio
//        _MOVWF(FSR.offs);  //Direcciona a byte bajo
//        //Byte bajo
//        if value.LByte = 0 then begin
//          _CLRF($00);   //Pone a cero
//        end else begin
//          _LDAi(value.LByte);
//          _MOVWF($00);
//        end;
//        //Byte alto
//        _INCF(FSR.offs, toF);
//        if value.HByte = 0 then begin
//          _CLRF($00);   //Pone a cero
//        end else begin
//          _LDAi(value.HByte);
//          _MOVWF($00);
//        end;
//      end else if value.Sto = stVariab then begin
//        _MOVWF(FSR.offs);   //Salva A.
//        _BCF(_STATUS, _C);
//        _RLF(FSR.offs, toW);  //idx * 2
//        _ADDLW(arrVar.addr0);  //Dirección de inicio
//        _MOVWF(FSR.offs);  //Direcciona a byte bajo
//        //Byte bajo
//        _MOVF(value.Loffs, toW);
//        _MOVWF($00);
//        //Byte alto
//        _INCF(FSR.offs, toF);
//        _MOVF(value.Hoffs, toW);
//        _MOVWF($00);
//      end else begin
//        //El valor a asignar está en H,A, y el índice (byte) en la pila
//        typWord.DefineRegister;   //Para usar H
//        aux := GetAuxRegisterByte;
//        _MOVWF(aux.offs);  //A->aux   salva A
//        rVar := GetVarByteFromStk;  //toma referencia de la pila
//        //Calcula dirección de byte bajo
//        _BCF(_STATUS, _C);
//        _RLF(rVar.adrByte0.offs, toW);  //índice * 2
//        _ADDLW(arrVar.addr0);  //Dirección de inicio
//        _MOVWF(FSR.offs);  //Direcciona
//        ////// Byte Bajo
//        _MOVF(aux.offs, toW);
//        _MOVWF($00);   //Escribe
//        ////// Byte Alto
//        _INCF(FSR.offs, toF);  //Direcciona a byte ALTO
//        _MOVF(H.offs, toW);
//        _MOVWF($00);   //Escribe
//        aux.used := false;
//      end;
//    end;
//    end;
//  end else begin
//    GenError('Syntax error.');
//  end;
//  if WithBrack then begin
//    //En este modo, no se requiere ")"
//  end else begin
//    if not CaptureTok(')') then exit;
//  end;
end;
procedure TGenCodBas.word_ClearItems(const OpPtr: pointer);
begin

end;
procedure TGenCodBas.word_Low(const OpPtr: pointer);
{Acceso al byte de menor peso de un word.}
var
  xvar, tmpVar: TxpEleVar;
  Op: ^TOperand;
begin
  cIn.Next;  //Toma identificador de campo
  Op := OpPtr;
  case Op^.Sto of
  stVariab: begin
    xvar := Op^.rVar;
    //Se devuelve una variable, byte
    //Crea una variable temporal que representará al campo
    tmpVar := CreateTmpVar(xvar.name+'.L', typByte);   //crea variable temporal
    tmpVar.addr0 :=  xvar.addr0;  //byte bajo
    res.SetAsVariab(tmpVar);
  end;
  stConst: begin
    //Se devuelve una constante bit
    res.SetAsConst(typByte);
    res.valInt := Op^.ValInt and $ff;
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TGenCodBas.word_High(const OpPtr: pointer);
{Acceso al byte de mayor peso de un word.}
var
  xvar, tmpVar: TxpEleVar;
  Op: ^TOperand;
begin
  cIn.Next;  //Toma identificador de campo
  Op := OpPtr;
  case Op^.Sto of
  stVariab: begin
    xvar := Op^.rVar;
    //Se devuelve una variable, byte
    //Crea una variable temporal que representará al campo
    tmpVar := CreateTmpVar(xvar.name+'.H', typByte);
    tmpVar.addr0 := xvar.addr1;  //byte alto
    res.SetAsVariab(tmpVar);
  end;
  stConst: begin
    //Se devuelve una constante bit
    res.SetAsConst(typByte);
    res.valInt := (Op^.ValInt and $ff00)>>8;
  end;
  else
    GenError('Syntax error.');
  end;
end;

procedure TGenCodBas.GenCodPicReqStopCodeGen;
{Required Stop the Code generation}
begin
  posFlash := pic.iRam; //Probably not the best way.
end;
procedure TGenCodBas.GenCodPicReqStartCodeGen;
{Required Start the Code generation}
begin
  pic.iRam := posFlash; //Probably not the best way.
end;
//Inicialización
procedure TGenCodBas.StartRegs;
{Inicia los registros de trabajo en la lista.}
begin
  listRegAux.Clear;
  listRegStk.Clear;   //limpia la pila
  stackTop := 0;
  listRegAuxBit.Clear;
  listRegStkBit.Clear;   //limpia la pila
  stackTopBit := 0;
  {Crea registros de trabajo adicionales H,E,U, para que estén definidos, pero aún no
  tienen asignados una posición en memoria.}
  H := CreateRegisterByte(prtWorkReg);
  E := CreateRegisterByte(prtWorkReg);
  U := CreateRegisterByte(prtWorkReg);
  //Puede salir con error
end;

function TGenCodBas.CompilerName: string;
begin
  Result := 'P65Pas Compiler'
end;
function TGenCodBas.RAMmax: integer;
begin
   Result := high(pic.ram);
end;
constructor TGenCodBas.Create;
begin
  inherited Create;
  ID := 16;  //Identifica al compilador PIC16
  devicesPath := patDevices16;
  OnReqStartCodeGen:=@GenCodPicReqStartCodeGen;
  OnReqStopCodeGen:=@GenCodPicReqStopCodeGen;
  pic := TP6502.Create;
  picCore := pic;   //Referencia picCore
  ///////////Crea tipos
  ClearTypes;
  //////////////// Tipo Byte /////////////
  typBool := CreateSysType('boolean',t_uinteger,1);   //de 1 byte
  typBool.OnLoadToRT   := @byte_LoadToRT;
  typBool.OnDefRegister:= @byte_DefineRegisters;
  typBool.OnSaveToStk  := @byte_SaveToStk;
  //typBool.OnReadFromStk :=
  typBool.OnGetItem    := @byte_GetItem;
//  typBool.OnSetItem    := @byte_SetItem;
  typBool.OnClearItems := @byte_ClearItems;
  //////////////// Tipo Byte /////////////
  typByte := CreateSysType('byte',t_uinteger,1);   //de 1 byte
  typByte.OnLoadToRT   := @byte_LoadToRT;
  typByte.OnDefRegister:= @byte_DefineRegisters;
  typByte.OnSaveToStk  := @byte_SaveToStk;
  //typByte.OnReadFromStk :=
  typByte.OnGetItem    := @byte_GetItem;
//  typByte.OnSetItem    := @byte_SetItem;
  typByte.OnClearItems := @byte_ClearItems;

  //////////////// Tipo Char /////////////
  //Tipo caracter
  typChar := CreateSysType('char',t_uinteger,1);   //de 1 byte. Se crea como uinteger para leer/escribir su valor como número
  typChar.OnLoadToRT   := @byte_LoadToRT;  //Es lo mismo
  typChar.OnDefRegister:= @byte_DefineRegisters;  //Es lo mismo
  typChar.OnSaveToStk  := @byte_SaveToStk; //Es lo mismo
  typChar.OnGetItem    := @byte_GetItem;   //Es lo mismo
//  typChar.OnSetItem    := @byte_SetItem;
  typChar.OnClearItems := @byte_ClearItems;

  //////////////// Tipo Word /////////////
  //Tipo numérico de dos bytes
  typWord := CreateSysType('word',t_uinteger,2);   //de 2 bytes
  typWord.OnLoadToRT   := @word_LoadToRT;
  typWord.OnDefRegister:= @word_DefineRegisters;
  typWord.OnSaveToStk  := @word_SaveToStk;
  typWord.OnGetItem    := @word_GetItem;   //Es lo mismo
//  typWord.OnSetItem    := @word_SetItem;
//  typWord.OnClearItems := @word_ClearItems;

  typWord.CreateField('Low', @word_Low);
  typWord.CreateField('High', @word_High);

  //Crea variables de trabajo
  varStkByte := TxpEleVar.Create;
  varStkByte.typ := typByte;
  varStkWord := TxpEleVar.Create;
  varStkWord.typ := typWord;
  //Crea lista de variables temporales
  varFields    := TxpEleVars.Create(true);
  //Inicializa contenedores
  listRegAux   := TPicRegister_list.Create(true);
  listRegStk   := TPicRegister_list.Create(true);
  listRegAuxBit:= TPicRegisterBit_list.Create(true);
  listRegStkBit:= TPicRegisterBit_list.Create(true);
  stackTop     := 0;  //Apunta a la siguiente posición libre
  stackTopBit  := 0;  //Apunta a la siguiente posición libre
  {Crea registro de trabajo A. El registro A, es el registro interno del PIC, y no
  necesita un mapeo en RAM. Solo se le crea aquí, para poder usar su propiedad "used"}
  A := TPicRegister.Create;
  A.assigned := false;   //se le marca así, para que no se intente usar
  {Crea registro de trabajo Z. El registro Z, es el registro interno del PIC, y está
  siempre asignado en RAM. }
  Z := TPicRegisterBit.Create;
  Z.bit := _Z;
  Z.assigned := true;   //ya está asignado desde el principio
  {Crea registro de trabajo C. El registro C, es el registro interno del PIC, y está
  siempre asignado en RAM. }
  C := TPicRegisterBit.Create;
  C.bit := _C;
  C.assigned := true;   //ya está asignado desde el principio
  //Crea registro interno INDF
  INDF := TPicRegister.Create;
  INDF.addr := $00;
  INDF.assigned := true;   //ya está asignado desde el principio
  {Crea registro auxiliar FSR. El registro FSR, es un registro interno del PIC, y está
  siempre asignado en RAM. }
  FSR := TPicRegister.Create;
  FSR.addr := $04;
  FSR.assigned := true;   //ya está asignado desde el principio
end;
destructor TGenCodBas.Destroy;
begin
  INDF.Destroy;
  FSR.Destroy;
  C.Destroy;
  Z.Destroy;
  A.Destroy;
  listRegAuxBit.Destroy;
  listRegStkBit.Destroy;
  listRegStk.Destroy;
  listRegAux.Destroy;
  varFields.Destroy;
  varStkByte.Destroy;
  varStkWord.Destroy;
  pic.Destroy;
  inherited Destroy;
end;

end.

