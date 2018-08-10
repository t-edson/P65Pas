{Unidad que implementa a la clase TParserAsm, que sirve como contenedor para
implementar las funcionaliddes de procesamiento de bloques ensamblador.
}
unit ParserAsm_PIC16;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, SynFacilHighlighter, P6502utils, GenCod_PIC16, Globales,
  XpresBas, strutils, XpresElementsPIC;

type
  //Datos de una etiqueta
  TPicLabel = class
    txt: string;   //nombre de la etiqueta
    add: integer;  //dirección
  end;
  TPicLabel_list = specialize TFPGObjectList<TPicLabel>;

  //Datos de una instrucción de salto, indefinido.
  TPicUJump = class
    txt: string;   //nombre de la etiqueta
    add: integer;  //dirección
    idInst: TP6502Inst;
  end;
  TPicUJump_list = specialize TFPGObjectList<TPicUJump>;

  { TParserAsm }
  TParserAsm = class(TGenCod)
  private
    lexAsm : TSynFacilSyn;   //lexer para analizar ASM
    tokIni2 : integer;  //Posición inicial del token actual
    labels : TPicLabel_list; //Lista de etiquetas
    uJumps : TPicUJump_list; //Lista de instrucciones GOTO o i_CALL, indefinidas
    asmRow : integer;     //número de fila explorada
    procedure AddLabel(name: string; addr: integer);
    procedure AddUJump(name: string; addr: integer; idInst: TP6502Inst);
    function CaptureAddress(const idInst: TP6502Inst; var a: word): boolean;
    function CaptureBitVar(out f, b: byte): boolean;
    function CaptureByte(out k: byte): boolean;
    function CaptureComma: boolean;
    function CaptureDestinat(out d: TPIC16destin): boolean;
    function CaptureNbit(var b: byte): boolean;
    function CaptureRegister(out f: byte): boolean;
    procedure EndASM;
    procedure GenErrorAsm(msg: string);
    procedure GenErrorAsm(msg: string; const Args: array of const);
    procedure GenWarnAsm(msg: string);
    function GetFaddress(addr: integer): byte;
    function HaveByteInformation(out bytePos: byte): boolean;
    function IsLabel(txt: string; out dir: integer): boolean;
    function IsStartASM(var lin: string): boolean;
    function IsEndASM(var lin: string): boolean;
    procedure ProcASM(const AsmLin: string);
    procedure ProcInstrASM;
    procedure skipWhites;
    procedure StartASM;
    function tokType: integer;
  protected
    procedure ProcASMlime(const AsmLin: string);
  public //Inicialización
    constructor Create; override;
    destructor Destroy; override;
  end;

  procedure SetLanguage;

implementation
var  //Mensajes
  ER_EXPEC_COMMA, ER_EXP_ADR_VAR, ER_EXP_CON_VAL, ER_NOGETADD_VAR,
  ER_NOGETVAL_CON,  ER_INV_ASMCODE: String;
  ER_EXPECT_W_F, ER_SYNTAX_ERR_, ER_DUPLIC_LBL_, ER_EXPE_NUMBIT: String;
  ER_EXPECT_ADDR, ER_EXPECT_BYTE, WA_ADDR_TRUNC, ER_UNDEF_LABEL_: String;

procedure SetLanguage;
begin
  GenCod_PIC16.SetLanguage;
  {$I ..\language\tra_ParserAsm.pas}
end;

{ TParserAsm }
procedure TParserAsm.GenErrorAsm(msg: string);
{Genera un error corrigiendo la posición horizontal}
var
  p: TSrcPos;
begin
  p := cIn.ReadSrcPos;
  p.col := tokIni2 + lexAsm.GetX;  //corrige columna
  GenErrorPos(msg, [], p);
end;
procedure TParserAsm.GenErrorAsm(msg: string; const Args: array of const);
var
  p: TSrcPos;
begin
  p := cIn.ReadSrcPos;
  p.col := tokIni2 + lexAsm.GetX;  //corrige columna
  GenErrorPos(msg, Args, p);
end;
procedure TParserAsm.GenWarnAsm(msg: string);
{Genera una advertencia corrigiendo la posición horizontal}
var
  p: TSrcPos;
begin
  p := cIn.ReadSrcPos;
  p.col := lexAsm.GetX;  //corrige columna
  GenWarnPos(msg, [], p);
end;
function TParserAsm.tokType: integer; inline;
begin
  Result := lexAsm.GetTokenKind;
end;
procedure TParserAsm.skipWhites;
//salta blancos o comentarios
begin
  if tokType = lexAsm.tnSpace then
    lexAsm.Next;  //quita espacios
  //puede que siga comentario
  if tokType = lexAsm.tnComment then
    lexAsm.Next;
  //después de un comentario no se espera nada.
end;
function TParserAsm.GetFaddress(addr: integer): byte;
{Obtiene una dirección de registro para una isntrucción ASM, truncando, si es necesario,
los bits adicionales.}
begin
  if addr>255 then begin
    addr := addr and $7F;
    //Indica con advertencia
    GenWarnAsm(WA_ADDR_TRUNC);
  end;
  Result := addr;
end;
procedure TParserAsm.AddLabel(name: string; addr: integer);
{Agrega una etiqueta a la lista}
var
  lbl: TPicLabel;
begin
  lbl := TPicLabel.Create;
  lbl.txt:= UpCase(name);
  lbl.add := addr;
  labels.Add(lbl);
end;
procedure TParserAsm.AddUJump(name: string; addr: integer; idInst: TP6502Inst);
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
function TParserAsm.IsLabel(txt: string; out dir: integer): boolean;
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
function TParserAsm.HaveByteInformation(out bytePos: byte): boolean;
begin
//    state0 := lexAsm.State;  //gaurda posición
  if lexasm.GetToken = '.' then begin
    //Hay precisión de campo
    lexAsm.Next;
    if UpCase(lexasm.GetToken) = 'LOW' then begin
      bytePos := 0;
      lexAsm.Next;
      exit(true);
    end else if UpCase(lexasm.GetToken) = 'HIGH' then begin
      bytePos := 1;
      lexAsm.Next;
      exit(true);
    end else begin
      //No es ninguno
      exit(false);
    end;
  end else if lexasm.GetToken = '@' then begin
    lexAsm.Next;
    if UpCase(lexasm.GetToken) = '0' then begin
      bytePos := 0;
      lexAsm.Next;
      exit(true);
    end else if UpCase(lexasm.GetToken) = '1' then begin
      bytePos := 1;
      lexAsm.Next;
      exit(true);
    end else if UpCase(lexasm.GetToken) = '2' then begin
      bytePos := 2;
      lexAsm.Next;
      exit(true);
    end else if UpCase(lexasm.GetToken) = '3' then begin
      bytePos := 3;
      lexAsm.Next;
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
function TParserAsm.CaptureByte(out k: byte): boolean;
{Captura un byte y devuelve en "k". Si no encuentra devuelve FALSE.}
var
  n: Integer;
  xcon: TxpEleCon;
  ele: TxpElement;
  bytePos: byte;
  str: String;
  xvar: TxpEleVar;
begin
  Result := false;
  skipWhites;
  if tokType = lexAsm.tnNumber then begin
    //es una dirección numérica
    n := StrToInt(lexAsm.GetToken);
    if (n>255) then begin
      GenErrorAsm(ER_EXPECT_BYTE);
      exit(false);
    end;
    k:=n;
    lexAsm.Next;
    exit(true);
  end else if tokType = lexAsm.tnIdentif then begin
    //Es un identificador, puede ser referencia a una constante o variable
    ele := TreeElems.FindFirst(lexAsm.GetToken);  //identifica elemento
    if ele = nil then begin
      //No identifica a este elemento
      GenErrorAsm(ER_EXP_CON_VAL);
      exit;
    end;
    if ele.idClass = eltCons then begin
      xcon := TxpEleCon(ele);
      AddCallerTo(xcon);  //lleva la cuenta
      if (xcon.typ = typByte) or (xcon.typ = typChar) then begin
        k := xcon.val.ValInt;
        lexAsm.Next;
        exit(true);
      end else if xcon.typ = typWord then begin
        lexAsm.Next;
        if HaveByteInformation(bytePos) then begin
          //Hay precisión de byte
          if bytePos = 0 then begin  //Byte bajo
            k := (xcon.val.ValInt and $FF);
          end else begin        //Byte alto
            k := (xcon.val.ValInt and $FF00) >> 8;
          end;
        end else begin  //No se indica byte
          k := (xcon.val.ValInt and $FF);
        end;
        exit(true);
      end else begin
        GenErrorAsm(ER_NOGETVAL_CON);
        exit(false);
      end;
    end else if ele.idClass = eltVar then begin
      //Para varaibles, se toma la dirección
      xvar := TxpEleVar(ele);
      AddCallerTo(xvar);  //lleva la cuenta
      n := xvar.addr;
      k := GetFaddress(n);
      lexAsm.Next;
      exit(true);
    end else begin
      //No es constante
      GenErrorAsm(ER_EXP_CON_VAL);
      exit(false);
    end;
  end else if (tokType = lexasm.tnString) and (length(lexAsm.GetToken) = 3) then begin
    //Es un caracter
    str := lexAsm.GetToken;
    k := ord(str[2]);   //lee código de caracter
    lexAsm.Next;
    exit(true);
  end else begin
    GenErrorAsm(ER_EXPECT_BYTE);
    exit(false);
  end;
end;
function TParserAsm.CaptureDestinat(out d: TPIC16destin): boolean;
{Captura el destino de una instrucción y devuelve en "d". Si no encuentra devuelve error}
var
  dest: String;
begin
  skipWhites;
  dest := lexAsm.GetToken;
  if (LowerCase(dest)='f') or (dest='1') then begin
    d := toF;
    lexAsm.Next;
    exit(true);
  end else if (LowerCase(dest)='w') or (dest='0') then begin
    d := toW;
    lexAsm.Next;
    exit(true);
  end else begin
    GenErrorAsm(ER_EXPECT_W_F);
    exit(false);
  end;
end;
function TParserAsm.CaptureNbit(var b: byte): boolean;
{Captura el número de bit de una instrucción y devuelve en "b". Si no encuentra devuelve error}
begin
  skipWhites;
  if tokType = lexAsm.tnNumber then begin
    //es una dirección numérica
    b := StrToInt(lexAsm.GetToken);
    if (b>7) then begin
      GenErrorAsm(ER_EXPE_NUMBIT);
      exit(false);
    end;
    lexAsm.Next;
    exit(true);
  end else if tokType = lexAsm.tnIdentif then begin
    //puede ser una constante
    CaptureByte(b);  //captura desplazamiento
    if HayError then exit(false);
    if (b>7) then begin
      GenErrorAsm(ER_EXPE_NUMBIT);
      exit(false);
    end;
    exit(true);
  end else begin
    GenErrorAsm(ER_EXPE_NUMBIT);
    exit(false);
  end;
end;
function TParserAsm.CaptureComma: boolean;
{Captura una coma. Si no encuentra devuelve error}
begin
  skipWhites;
  if lexAsm.GetToken = ',' then begin
    lexAsm.Next;   //toma la coma
    Result := true;
    exit;
  end else begin
    Result := false;
    GenErrorAsm(ER_EXPEC_COMMA);
    exit;
  end;
end;
function TParserAsm.CaptureBitVar(out f, b: byte): boolean;
{Captura una variable de tipo Bit. Si no encuentra, devuelve FALSE (no genera error).}
var
  ele: TxpElement;
  xvar: TxpEleVar;
begin
  skipWhites;
  if tokType <> lexAsm.tnIdentif then exit(false);  //no es identificador
  //Hay un identificador
  ele := TreeElems.FindFirst(lexAsm.GetToken);  //identifica elemento
  if ele = nil then exit(false);  //no se identifica
  //Se identificó elemento
  if ele.idClass <> eltVar then exit(false);
  //Es variable
  xvar := TxpEleVar(ele);
  if not xvar.typ.IsBitSize then exit(false);
  //Es variable bit o boolean
  lexAsm.Next;   //toma identificador
  AddCallerTo(xvar);  //lleva la cuenta
  f := GetFaddress(xvar.adrBit.offs);
  b := xvar.adrBit.bit;
  exit(true);
end;
function TParserAsm.CaptureRegister(out f: byte): boolean;
{Captura la referencia a un registro y devuelve en "f". Si no encuentra devuelve error}
var
  n: integer;
  ele: TxpElement;
  xvar: TxpEleVar;
  bytePos: byte;
begin
  Result := false;
  skipWhites;
  if tokType = lexAsm.tnNumber then begin
    //Es una dirección numérica
    if not TryStrToInt(lexAsm.GetToken, n) then begin
      GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
      exit;
    end;
    f := GetFaddress(n);
    lexAsm.Next;
    Result := true;
    exit;
  end else if lexAsm.GetToken = '_H' then begin
    //Es el registro  de trabajo _H
    f := H_register.offs;
    lexAsm.Next;
    Result := true;
    exit;
  end else if lexAsm.GetToken = '_E' then begin
    //Es el registro  de trabajo _H
    f := E_register.offs;
    lexAsm.Next;
    Result := true;
    exit;
  end else if lexAsm.GetToken = '_U' then begin
    //Es el registro  de trabajo _H
    f := U_register.offs;
    lexAsm.Next;
    Result := true;
    exit;
  end else if tokType = lexAsm.tnIdentif then begin
    //Es un identificador, puede ser referencia a una variable
    ele := TreeElems.FindFirst(lexAsm.GetToken);  //identifica elemento
    if ele = nil then begin
      //No identifica a este elemento
      GenErrorAsm(ER_EXP_ADR_VAR);
      exit;
    end;
    if ele.idClass = eltVar then begin
      xvar := TxpEleVar(ele);
      AddCallerTo(xvar);  //lleva la cuenta
      if xvar.typ.IsByteSize then begin
        n := xvar.addr;
        f := GetFaddress(n);
        lexAsm.Next;
        Result := true;
        exit;
      end else if xvar.typ.IsWordSize then begin
        lexAsm.Next;
        if HaveByteInformation(bytePos) then begin
          //Hay precisión de byte
          if bytePos = 0 then begin  //Byte bajo
            n := xvar.adrByte0.offs;
            f := GetFaddress(n);
          end else if bytePos = 1 then begin        //Byte alto
            n := xvar.adrByte1.offs;
            f := GetFaddress(n);
          end else begin
             GenErrorAsm(ER_NOGETADD_VAR);
             exit(false);
          end;
        end else begin
           n := xvar.addr;
           f := GetFaddress(n);
        end;
        exit(true);
      end else if xvar.typ.IsDWordSize then begin
        lexAsm.Next;
        if HaveByteInformation(bytePos) then begin
          //Hay precisión de byte
          if bytePos = 0 then begin  //Byte bajo
            n := xvar.adrByte0.offs;
            f := GetFaddress(n);
          end else if bytePos = 1 then begin        //Byte alto
            n := xvar.adrByte1.offs;
            f := GetFaddress(n);
          end else if bytePos = 2 then begin        //Byte alto
            n := xvar.adrByte2.offs;
            f := GetFaddress(n);
          end else if bytePos = 3 then begin        //Byte alto
            n := xvar.adrByte3.offs;
            f := GetFaddress(n);
          end else begin
             GenErrorAsm(ER_NOGETADD_VAR);
             exit(false);
          end;
        end else begin
           n := xvar.addr;
           f := GetFaddress(n);
        end;
        exit(true);
      end else begin
        GenErrorAsm(ER_NOGETADD_VAR);
        exit(false);
      end;
    end else begin
      //No es variable
      GenErrorAsm(ER_EXP_ADR_VAR);
      Result := false;
      exit;
    end;
  end else begin
    GenErrorAsm(ER_EXP_ADR_VAR);
    //asmErrLin := asmRow;
    Result := false;
    exit;
  end;
end;
function TParserAsm.CaptureAddress(const idInst: TP6502Inst; var a: word
  ): boolean;
{Captura una dirección a una instrucción y devuelve en "a". Si no encuentra genera
error y devuelve FALSE.}
var
  dir: integer;
  offset: byte;
  ele: TxpElement;
  xfun: TxpEleFun;
begin
  Result := false;
  skipWhites;
  if lexAsm.GetToken = '$' then begin
    //Es una dirección relativa
    lexAsm.Next;
    skipWhites;
    //Puede tener + o -
    if (lexAsm.GetToken= '') or (lexAsm.GetToken = ';') then begin
      //Termina la instrucción sin o con es comentario
      a := pic.iRam;
      Result := true;
      exit;
    end else if lexAsm.GetToken = '+' then begin
      //Es dirección sumada
      lexAsm.Next;
      skipWhites;
      CaptureByte(offset);  //captura desplazamiento
      if HayError then exit(false);
      Result := true;
      a := pic.iRam + offset;
      exit;
    end else if lexAsm.GetToken = '-' then begin
      //Es dirección restada
      lexAsm.Next;
      skipWhites;
      CaptureByte(offset);  //captura desplazamiento
      if HayError then exit(false);
      Result := true;
      a := pic.iRam - offset;
      exit;
    end else begin
      //Sigue otra cosa
      GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
    end;
  end else if tokType = lexAsm.tnNumber then begin
    //Es una dirección numérica
    a := StrToInt(lexAsm.GetToken);
    lexAsm.Next;
    Result := true;
    exit;
  end else if (tokType = lexAsm.tnIdentif) and IsLabel(lexAsm.GetToken, dir) then begin
    //Es un identificador de etiqueta
    a := dir;
    lexAsm.Next;
    Result := true;
    exit;
  end else if tokType = lexAsm.tnIdentif  then begin
    ele := TreeElems.FindFirst(lexAsm.GetToken);  //identifica elemento
    if (ele <> nil) and (ele.idClass = eltFunc) then begin
      //Es un identificador de función del árbol de sintaxis
      xfun := TxpEleFun(ele);
      AddCallerTo(xfun);  //lleva la cuenta
      a := xfun.adrr;   //lee su dirección
      lexAsm.Next;
      Result := true;
      exit;
    end;
    //Es un identificador, no definido. Puede definirse luego.
    a := $00;
    //Los saltos indefinidos, se guardan en la lista "uJumps"
    AddUJump(lexAsm.GetToken, pic.iRam, idInst);
    lexAsm.Next;
    Result := true;
    exit;
  end else begin
    GenErrorAsm(ER_EXPECT_ADDR);
    Result := false;
    exit;
  end;
end;
procedure TParserAsm.StartASM; //Inicia el procesamiento de código ASM
begin
  asmRow := 1;    //inicia línea
  labels.Clear;   //limpia etiquetas
  uJumps.Clear;
end;
procedure TParserAsm.EndASM;  //Termina el procesamiento de código ASM
var
  jmp : TPicUJump;
  loc: integer;
begin
  //Completa los saltos indefinidos
  if uJumps.Count>0 then begin
    for jmp in uJumps do begin
      if IsLabel(jmp.txt, loc) then begin
        //Si existe la etiqueta
        if jmp.idInst = i_JMP then
          pic.codGotoAt(jmp.add, loc)
        else  //Solo puede ser i_CALL
          pic.codCallAt(jmp.add, loc);
      end else begin
        //No se enuentra
        GenErrorAsm(ER_UNDEF_LABEL_, [jmp.txt]);
        exit;
      end;
    end;
  end;
end;
procedure TParserAsm.ProcInstrASM;
//Procesa una instrucción ASM
var
  stx: string;
  idInst: TP6502Inst;
  tok: String;
  f : byte;
  d: TPIC16destin;
  b: byte;
  a: word;
  k: byte;
begin
//  tok := lexAsm.GetToken;
//  //verifica directiva ORG
//  if upcase(tok) = 'ORG' then begin
//    lexAsm.Next;
//    idInst := i_JMP;  //no debería ser necesario
//    if not CaptureAddress(idInst, a) then exit;
//    pic.iRam := a;   //¡CUIDADO! cambia PC
//    exit;
//  end;
//  //debería ser una instrucción
//  idInst := pic.FindOpcode(tok, stx);
//  if idInst = i_Inval then begin
//    GenErrorAsm(ER_INV_ASMCODE, [tok]);
//    exit;
//  end;
//  //es un código válido
//  lexAsm.Next;
//  case stx of
//  'fd': begin   //se espera 2 parámetros
//    if not CaptureRegister(f) then exit;
//    if not CaptureComma then exit;
//    if not CaptureDestinat(d) then exit;
//    pic.codAsmFD(idInst, f, d);
//  end;
//  'f':begin
//    if not CaptureRegister(f) then exit;
//    pic.codAsmF(idInst, f);
//  end;
//  'fb':begin  //para instrucciones de tipo bit
//    if CaptureBitVar(f, b) then begin
//      //Es una referencia a variable bit.
//    end else begin
//      if not CaptureRegister(f) then exit;
//      if not CaptureComma then exit;
//      if not CaptureNbit(b) then exit;
//    end;
//    pic.codAsmFB(idInst, f, b);
//  end;
//  'a': begin  //i_CALL y GOTO
//    if not CaptureAddress(idInst, a) then exit;
//    pic.codAsmA(idInst, a);
//  end;
//  'k': begin  //i_MOVLW
//     if not CaptureByte(k) then exit;
//     pic.codAsmK(idInst, k);
//  end;
//  '': begin
//    pic.codAsm(idInst);
//  end;
//  end;
//  //no debe quedar más que espacios o comentarios
//  skipWhites;
//  if tokType <> lexAsm.tnEol then begin
//    GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
//    exit;
//  end;
//
end;
procedure TParserAsm.ProcASM(const AsmLin: string);
{Procesa una línea en ensamblador.}
  function ExtractLabel: boolean;
  {Extrae una etiqueta en la posición actual del lexer. Si no identifica
  a una etiqueta, devuelve FALSE.}
  var
    lbl: String;
    state0: TFaLexerState;
    d: integer;
  begin
    if tokType <> lexAsm.tnIdentif then
      exit(false);  //No es
    //Guarda posición por si acaso
    state0 := lexAsm.State;
    //Evalúa asumiendo que es etiqueta
    lbl := lexAsm.GetToken;   //guarda posible etiqueta
    lexAsm.Next;
    if lexAsm.GetToken = ':' then begin
      //Definitivamente es una etiqueta
      if IsLabel(lbl, d) then begin
        GenErrorAsm(ER_DUPLIC_LBL_, [lbl]);
        exit(false);
      end;
      AddLabel(lbl, pic.iRam);
      lexAsm.Next;
      skipwhites;
      if tokType <> lexAsm.tnEol then begin
        //Hay algo más. Solo puede ser una instrucción
        ProcInstrASM;
        if HayError then exit(false);
      end;
      exit(true);
    end else begin
      //No es etiqueta
      lexAsm.State := state0;  //recupera posición
      exit(false)
    end;
  end;
begin
  inc(asmRow);   //cuenta líneas
  if Trim(AsmLin) = '' then exit;
  //procesa la destínea
  lexAsm.SetLine(asmLin, asmRow);  //inicia cadena
  if tokType = lexAsm.tnKeyword then begin
    ProcInstrASM;
    if HayError then exit;
  end else if Extractlabel then begin
      //Era una etiqueta
  end else if tokType = lexAsm.tnComment then begin
    skipWhites;
    if tokType <> lexAsm.tnEol then begin
      GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
      exit;
    end;
  end else if tokType = lexAsm.tnSpace then begin
    skipWhites;
    if tokType <> lexAsm.tnEol then begin
      //Hay algo más. Solo puede ser una instrucción o etiqueta
      if tokType = lexAsm.tnKeyword then begin
        //Es instrucción
        ProcInstrASM;
        if HayError then exit;
      end else if Extractlabel then begin
        //Era una etiqueta
      end else begin
        //Es otra cosa
        GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
        exit;
      end;
    end;
  end else begin
    GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
    exit;
  end;
  skipWhites;  //quita espacios
//  msgbox(AsmLin);
end;
function TParserAsm.IsStartASM(var lin: string): boolean;
{Indica si una línea contiene al delimitador inicial "ASM". Si es así, la recorta.}
begin
  if not AnsiStartsText('asm', lin) then
    exit(false);  //definitivamente no es
  //hay coincidencia pero hay que analziar más
  if length(lin) = 3 then begin
    lin := copy(lin, 4, length(lin));  //quita "asm"
    exit(true);  //es exacto
  end;
  //podrìa ser, pero hay que descartar que no sea parte de un identificador
  if lin[4] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] THEN
    exit(false); //es parte de un identificador
  //es por descarte
  lin := copy(lin, 4, length(lin));  //quita "asm"
  exit(true);
end;
function TParserAsm.IsEndASM(var  lin: string): boolean;
{Indica si una línea contiene al delimitador final "END". Si es así, la recorta.}
begin
  if not AnsiEndsText('end', lin) then
    exit(false);  //definitivamente no es
  //Hay coincidencia pero hay que analziar más
  if length(lin) = 3 then begin
    lin := copy(Lin, 1, length(Lin)-3);  //quita "end"
    exit(true);  //es exacto
  end;
  //Podrìa ser, pero hay que descartar que no sea parte de un identificador
  if lin[length(lin)-3] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] THEN
    exit(false); //es parte de un identificador
  //Es por descarte
  lin := copy(Lin, 1, length(Lin)-3);  //quita "end"
  exit(true);
end;
procedure TParserAsm.ProcASMlime(const AsmLin: string);
{Procesa una línea de código ASM. Notar que los bloques ASM pueden tener muchas líneas
pero el procesamiento, se hace siempre línea por línea, debido a cómo trabaja el
lexer.}
var
  lin: String;
begin
  lin := AsmLin;  //crea copia para poder modificarla
  //Extrae el texto entre los delimitadores de ensamblador
  if IsStartASM(lin) then begin
    //Como se ha recortado el "ASM", se debe compensar "tokIni2"
    //Además se debe considerar si el delim. ASM, no inicia en 1.
    tokIni2 := 3 + Cin.curCon.lex.GetX - 1;
    //Es la primera línea de ensamblador
    StartASM;
    //puede incluir también al delimitador "end"
    if IsEndASM(lin) then begin
      ProcASM(lin);  //procesa por si queda código
      EndASM;
    end else begin
      ProcASM(lin);  //procesa por si queda código
    end;
  end else if IsEndASM(lin) then begin
    //Es la última línea de ensamblador
    tokIni2 := 0;   //En el margen izquierdo, porque no está el delimit. inicial "ASM"
    ProcASM(lin);  //procesa por si queda código
    EndASM;
  end else begin
    //Es una línea común
    tokIni2 := 0;   //una línea común, siempre empieza en al margen izquierdo
    ProcASM(lin);
  end;
end;
constructor TParserAsm.Create;
begin
  inherited Create;
  labels := TPicLabel_list.Create(true);
  uJumps := TPicUJump_list.Create(true);
  {Define la sintaxis del lexer que se usará para analizar el código en ensamblador.}
  lexAsm := TSynFacilSyn.Create(nil);  //crea lexer para analzar ensamblador
  lexAsm.DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');
  lexAsm.DefTokContent('[0-9]', '[0-9.]*', lexAsm.tnNumber);
  lexAsm.DefTokContent('[$]','[0-9A-Fa-f]*', lexAsm.tnNumber);
  lexAsm.DefTokContent('[%]','[01]*', lexAsm.tnNumber);
  lexAsm.AddIdentSpecList('ADDWF ANDWF CLRF CLRW COMF DECF DECFSZ INCF', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('INCFSZ IORWF MOVF MOVWF NOP RLF RRF SUBWF SWAPF XORWF', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('BCF BSF BTFSC BTFSS', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('ADDLW ANDLW CALL CLRWDT GOTO IORLW MOVLW RETFIE', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('RETLW RETURN SLEEP SUBLW XORLW', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('ORG', lexAsm.tnKeyword);
  lexAsm.DefTokDelim(';','', lexAsm.tnComment);
  lexAsm.DefTokDelim('''','''', lexAsm.tnString);
  lexAsm.Rebuild;
end;
destructor TParserAsm.Destroy;
begin
  lexAsm.Destroy;
  uJumps.Destroy;
  labels.Destroy;
  inherited Destroy;
end;

end.

