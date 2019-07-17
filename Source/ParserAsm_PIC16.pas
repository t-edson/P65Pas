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
    function CaptureAddress(const idInst: TP6502Inst; var ad: word): boolean;
    function CaptureByte(out k: byte): boolean;
    function CaptureComma: boolean;
    function CaptureNbit(var b: byte): boolean;
    function CaptureParenthes: boolean;
    function CaptureRegister(out f: byte): boolean;
    procedure EndASM;
    procedure GenErrorAsm(msg: string);
    procedure GenErrorAsm(msg: string; const Args: array of const);
    procedure GenWarnAsm(msg: string);
    function GetFaddressByte(addr: integer): byte;
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
  ER_EXPEC_COMMA, ER_EXPEC_PAREN, ER_EXP_ADR_VAR, ER_EXP_CON_VAL, ER_NOGETADD_VAR,
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
function TParserAsm.GetFaddressByte(addr: integer): byte;
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
      n := xvar.addr0;
      k := GetFaddressByte(n);
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
function TParserAsm.CaptureParenthes: boolean;
{Captura el paréntesis ')'. Si no encuentra devuelve error}
begin
  skipWhites;
  if lexAsm.GetToken = ')' then begin
    lexAsm.Next;   //toma la coma
    Result := true;
    exit;
  end else begin
    Result := false;
    GenErrorAsm(ER_EXPEC_PAREN);
    exit;
  end;
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
    f := GetFaddressByte(n);
    lexAsm.Next;
    Result := true;
    exit;
  end else if lexAsm.GetToken = '_H' then begin
    //Es el registro  de trabajo _H
    f := H_register.addr;
    lexAsm.Next;
    Result := true;
    exit;
  end else if lexAsm.GetToken = '_E' then begin
    //Es el registro  de trabajo _H
    f := E_register.addr;
    lexAsm.Next;
    Result := true;
    exit;
  end else if lexAsm.GetToken = '_U' then begin
    //Es el registro  de trabajo _H
    f := U_register.addr;
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
        n := xvar.addr0;
        f := GetFaddressByte(n);
        lexAsm.Next;
        Result := true;
        exit;
      end else if xvar.typ.IsWordSize then begin
        lexAsm.Next;
        if HaveByteInformation(bytePos) then begin
          //Hay precisión de byte
          if bytePos = 0 then begin  //Byte bajo
            n := xvar.addr0;
            f := GetFaddressByte(n);
          end else if bytePos = 1 then begin        //Byte alto
            n := xvar.addr0+1;
            f := GetFaddressByte(n);
          end else begin
             GenErrorAsm(ER_NOGETADD_VAR);
             exit(false);
          end;
        end else begin
           n := xvar.addr0;
           f := GetFaddressByte(n);
        end;
        exit(true);
      end else if xvar.typ.IsDWordSize then begin
        lexAsm.Next;
        if HaveByteInformation(bytePos) then begin
          //Hay precisión de byte
          if bytePos = 0 then begin  //Byte bajo
            n := xvar.addr0;
            f := GetFaddressByte(n);
          end else if bytePos = 1 then begin        //Byte alto
            n := xvar.addr0+1;
            f := GetFaddressByte(n);
          end else if bytePos = 2 then begin        //Byte alto
            n := xvar.addr0 + 2;
            f := GetFaddressByte(n);
          end else if bytePos = 3 then begin        //Byte alto
            n := xvar.addr0 + 3;
            f := GetFaddressByte(n);
          end else begin
             GenErrorAsm(ER_NOGETADD_VAR);
             exit(false);
          end;
        end else begin
           n := xvar.addr0;
           f := GetFaddressByte(n);
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
function TParserAsm.CaptureAddress(const idInst: TP6502Inst; var ad: word
  ): boolean;
{Captura una dirección a una instrucción y devuelve en "aD". Si no encuentra genera
error y devuelve FALSE.}
var
  dir: integer;
  offset, bytePos: byte;
  ele: TxpElement;
  xfun: TxpEleFun;
  xvar: TxpEleVar;
  xcon: TxpEleCon;
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
      ad := pic.iRam;
      exit(true);
    end else if lexAsm.GetToken = '+' then begin
      //Es dirección sumada
      lexAsm.Next;
      skipWhites;
      CaptureByte(offset);  //captura desplazamiento
      if HayError then exit(false);
      ad := pic.iRam + offset;
      Result := true;
      exit;
    end else if lexAsm.GetToken = '-' then begin
      //Es dirección restada
      lexAsm.Next;
      skipWhites;
      CaptureByte(offset);  //captura desplazamiento
      if HayError then exit(false);
      if FirstPass then begin
        //Para evitar errores, porque en Primera pasada se trabaja todo en $0000
        ad := 00;
      end else begin
        ad := (pic.iRam - offset) and $FFFF;
      end;
      Result := true;
      exit;
    end else begin
      //Sigue otra cosa
      GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
    end;
  end else if tokType = lexAsm.tnNumber then begin
    //Es una dirección numérica
    ad := StrToInt(lexAsm.GetToken);
    lexAsm.Next;
    exit(true);
  end else if (tokType = lexAsm.tnIdentif) and IsLabel(lexAsm.GetToken, dir) then begin
    //Es un identificador de etiqueta
    if pic.IsRelJump(idInst) then begin
      //Salto relativo BEQ, BNE, ...
      ad := (dir-pic.iRam-2) and $ff;
    end else begin
      //Se debe dejar 2 bytes de espacio
      ad := dir;
    end;
    lexAsm.Next;
    exit(true);
  end else if tokType = lexAsm.tnIdentif  then begin
    ele := TreeElems.FindFirst(lexAsm.GetToken);  //identifica elemento
    if ele<>nil then begin
      //Se identifica un elemento del lenguaje
      if ele.idClass = eltFunc then begin
        //Es un identificador de función del árbol de sintaxis
        xfun := TxpEleFun(ele);
        AddCallerTo(xfun);  //lleva la cuenta
        ad := xfun.adrr;   //lee su dirección
        lexAsm.Next;
        exit(true);
      end else if ele.idClass = eltVar then begin
        //Es identificador de variable
        xvar := TxpEleVar(ele);
        AddCallerTo(xvar);  //lleva la cuenta
        if xvar.typ.IsWordSize then begin
          lexAsm.Next;
          if HaveByteInformation(bytePos) then begin
            //Hay precisión de byte
            if bytePos = 0 then begin  //Byte bajo
              ad := xvar.addr0;
            end else if bytePos = 1 then begin        //Byte alto
              ad := xvar.addr0+1;
            end else begin
               GenErrorAsm(ER_NOGETADD_VAR);
               exit(false);
            end;
          end else begin
             ad := xvar.addr0;
          end;
        end else if xvar.typ.IsDWordSize then begin
          lexAsm.Next;
          if HaveByteInformation(bytePos) then begin
            //Hay precisión de byte
            if bytePos = 0 then begin  //Byte bajo
              ad := xvar.addr0;
            end else if bytePos = 1 then begin        //Byte alto
              ad := xvar.addr0+1;
            end else if bytePos = 2 then begin        //Byte alto
              ad := xvar.addr0 + 2;
            end else if bytePos = 3 then begin        //Byte alto
              ad := xvar.addr0 + 3;
            end else begin
               GenErrorAsm(ER_NOGETADD_VAR);
               exit(false);
            end;
          end else begin
             ad := xvar.addr0;
          end;
        end else begin  //Es BYTE u otro tipo de variable
          ad := xvar.addr0;  //Lee su dirección
          lexAsm.Next;
        end;
        exit(true);
      end else if ele.idClass = eltCons then begin
        //Es identificador de constante
        xcon := TxpEleCon(ele);
        AddCallerTo(xcon);  //lleva la cuenta
        if xcon.typ = typByte then begin
          ad := xcon.val.ValInt;  //Lee su dirección
          lexAsm.Next;
        end else if xcon.typ = typWord then begin
          ad := xcon.val.ValInt;  //Lee su dirección
          lexAsm.Next;
        end else begin
          //Otro tipo de constante
          GenErrorAsm(ER_NOGETVAL_CON);
          exit(false);
        end;
        exit(true);
      end else begin
        //No se puede leer dirección
        GenErrorAsm(ER_EXP_CON_VAL);
        exit(false);
      end;
    end else begin
      //Es un identificador, no definido. Puede definirse luego.
      if pic.IsRelJump(idInst) then begin
        //Deben ser instrucciones de salto relativo BNE, BEQ, ...
        ad := $FF;
      end else begin
        //Debe ser JMP o JSR. Se debe dejar 2 bytes de espacio
        ad := $FFFF;
      end;
      //Los saltos indefinidos, se guardan en la lista "uJumps"
      AddUJump(lexAsm.GetToken, pic.iRam, idInst);
      lexAsm.Next;
      Result := true;
      exit(true);
    end;
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
        if jmp.idInst in [i_JMP, i_JSR] then
          pic.cod_JMP_at(jmp.add, loc)
        else  //Deberían ser BEQ, BNE, ...
          pic.cod_REL_JMP_at(jmp.add, loc-jmp.add-2);
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
  idInst: TP6502Inst;
  tok: String;
  ad: word;
  n: integer;
  xcon: TxpEleCon;
  ele: TxpElement;
  xvar: TxpEleVar;
  xfun: TxpEleFun;
begin
  tok := lexAsm.GetToken;
  //verifica directiva ORG
  if upcase(tok) = 'ORG' then begin
    lexAsm.Next;
    idInst := i_JMP;  //no debería ser necesario
    if not CaptureAddress(idInst, ad) then exit;
    pic.iRam := ad;   //¡CUIDADO! cambia PC
    exit;
  end;
  //Debería ser una instrucción
  idInst := pic.FindOpcode(tok);
  if idInst = i_Inval then begin
    GenErrorAsm(ER_INV_ASMCODE, [tok]);
    exit;
  end;
  //Es un código válido
  lexAsm.Next;
  skipWhites;
  tok := lexAsm.GetToken;
  if tokType = lexAsm.tnEol then begin
    //Sin parámetros. Puede ser Implícito o Acumulador
    if aImplicit in PIC16InstName[idInst].addressModes then begin
      //Tiene modo implícito
      pic.codAsm(idInst, aImplicit, 0);
      if pic.MsjError<>'' then begin
        GenErrorAsm(pic.MsjError);
      end;
    end else begin
      //Debe ser acumulador
      pic.codAsm(idInst, aAcumulat, 0);
      if pic.MsjError<>'' then begin
        GenErrorAsm(pic.MsjError);
      end;
    end;
  end else if (tok = '#<') or (tok = '#>')  then begin
    //Inmediato con operador
    lexAsm.Next;
    if tokType = lexAsm.tnNumber then begin
      n := StrToInt(lexAsm.GetToken);
      if tok = '#>' then n := n and $FF else n := (n and $ff00) >> 8;
      lexAsm.Next;
      pic.codAsm(idInst, aImmediat, n);
      if pic.MsjError<>'' then begin
        GenErrorAsm(pic.MsjError);
      end;
    end else if tokType = lexAsm.tnIdentif then begin
      //Identificador
      ele := TreeElems.FindFirst(lexAsm.GetToken);  //identifica elemento
      if ele = nil then begin
        GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
        exit;
      end else if ele.idClass = eltCons then begin
        //Es un identificador de constante del árbol de sintaxis
        xcon := TxpEleCon(ele);
        AddCallerTo(xcon);  //lleva la cuenta
        lexAsm.Next;
        n := xcon.val.ValInt;
        if tok = '#<' then n := n and $FF else n := (n and $ff00) >> 8;
        pic.codAsm(idInst, aImmediat, n);
        if pic.MsjError<>'' then begin
          GenErrorAsm(pic.MsjError);
        end;
      end else if ele.idClass = eltVar then begin
        //Es un identificador de variable del árbol de sintaxis
        xvar := TxpEleVar(ele);
        AddCallerTo(xvar);  //lleva la cuenta
        lexAsm.Next;
        n := xvar.addr0;  //Lee dirección
        if tok = '#<' then n := n and $FF else n := (n and $ff00) >> 8;
        pic.codAsm(idInst, aImmediat, n);
        if pic.MsjError<>'' then begin
          GenErrorAsm(pic.MsjError);
        end;
      end else if ele.idClass = eltFunc then begin
        //Es un identificador de variable del árbol de sintaxis
        xfun := TxpEleFun(ele);
        AddCallerTo(xfun);  //lleva la cuenta
        lexAsm.Next;
        n := xfun.adrr;  //Lee dirección
        if tok = '#<' then n := n and $FF else n := (n and $ff00) >> 8;
        pic.codAsm(idInst, aImmediat, n);
        if pic.MsjError<>'' then begin
          GenErrorAsm(pic.MsjError);
        end;
      end else begin
        GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
        exit;
      end;
    end else begin
      GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
      exit;
    end;
  end else if tok = '#' then begin
    //Inmediato
    lexAsm.Next;
    if tokType = lexAsm.tnNumber then begin
      if not TryStrToInt(lexAsm.GetToken, n) then begin
        GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
        exit;
      end;
      if (n>255) then begin
        GenErrorAsm(ER_EXPECT_BYTE);
        exit;
      end;
      lexAsm.Next;
      pic.codAsm(idInst, aImmediat, n);
      if pic.MsjError<>'' then begin
        GenErrorAsm(pic.MsjError);
      end;
    end else if tokType = lexAsm.tnIdentif then begin
      ele := TreeElems.FindFirst(lexAsm.GetToken);  //identifica elemento
      if (ele <> nil) and (ele.idClass = eltCons) then begin
        //Es un identificador de constante del árbol de sintaxis
        xcon := TxpEleCon(ele);
        AddCallerTo(xcon);  //lleva la cuenta
        lexAsm.Next;
        pic.codAsm(idInst, aImmediat, xcon.val.ValInt and $FF);
        if pic.MsjError<>'' then begin
          GenErrorAsm(pic.MsjError);
        end;
      end else if (ele <> nil) and (ele.idClass = eltVar) then begin
        //Es un identificador de variable del árbol de sintaxis
        xvar := TxpEleVar(ele);
        AddCallerTo(xvar);  //lleva la cuenta
        lexAsm.Next;
        n := xvar.addr0;  //Lee dirección
        pic.codAsm(idInst, aImmediat, n and $FF);
        if pic.MsjError<>'' then begin
          GenErrorAsm(pic.MsjError);
        end;
      end else if (ele <> nil) and (ele.idClass = eltFunc) then begin
        //Es un identificador de variable del árbol de sintaxis
        xfun := TxpEleFun(ele);
        AddCallerTo(xfun);  //lleva la cuenta
        lexAsm.Next;
        n := xfun.adrr;  //Lee dirección
        pic.codAsm(idInst, aImmediat, n and $FF);
        if pic.MsjError<>'' then begin
          GenErrorAsm(pic.MsjError);
        end;
      end else begin
        GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
        exit;
      end;
    end else begin
      GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
      exit;
    end;
  end else if tokType in [lexAsm.tnNumber, lexAsm.tnIdentif] then begin
    //Puede ser absoluto o página cero, o sus versiones indexadas con X o Y.
    //n := StrToInt(lexAsm.GetToken);
    if not CaptureAddress(idInst, ad) then begin
      GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
      exit;
    end;
    if (ad>255) then begin
      skipWhites;
      //Verifica si sigue ,X o ,Y
      if lexAsm.GetToken = ',' then begin
        lexAsm.Next;
        skipWhites;
        if Upcase(lexAsm.GetToken) = 'X' then begin
          lexAsm.Next;
          pic.codAsm(idInst, aAbsolutX, ad);
        end else if Upcase(lexAsm.GetToken) = 'Y' then begin
          lexAsm.Next;
          pic.codAsm(idInst, aAbsolutY, ad);
        end else begin
          GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
          exit;
        end;
      end else begin
          pic.codAsm(idInst, aAbsolute, ad);
      end;
    end else begin  //<255
      skipWhites;
      //Verifica si sigue ,X o ,Y
      if lexAsm.GetToken = ',' then begin
        lexAsm.Next;
        skipWhites;
        if Upcase(lexAsm.GetToken) = 'X' then begin
          lexAsm.Next;
          pic.codAsm(idInst, aZeroPagX, ad);
        end else if Upcase(lexAsm.GetToken) = 'Y' then begin
          lexAsm.Next;
          pic.codAsm(idInst, aZeroPagY, ad);
        end else begin
          GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
          exit;
        end;
      end else begin
        //El parámetro es de un solo byte
        if pic.IsRelJump(idInst) then begin  //Es BEQ, BNE o similar
          pic.codAsm(idInst, aRelative, ad);
        end else begin
          pic.codAsm(idInst, aZeroPage, ad);
        end;
      end;
    end;
    if pic.MsjError<>'' then begin
      GenErrorAsm(pic.MsjError);
    end;
  end else if tok = '(' then begin
    //Direccionamiento Indirecto: (indirect), (indirect,X) o (indirect),Y
    lexAsm.Next;
    if tokType in [lexAsm.tnNumber, lexAsm.tnIdentif] then begin
//      n := StrToInt(lexAsm.GetToken);
      if not CaptureAddress(idInst, ad) then begin
        GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
        exit;
      end;
      if ad>255 then begin
        //Es un word. Solo podría ser del tiipo JMP ($FFFF)
        pic.codAsm(idInst, aIndirect, ad);
        //lexAsm.Next;  //Toma número
        CaptureParenthes;  //Captura ')'
      end else begin
        //Es un byte. Solo podría ser (indirect,X) o (indirect),Y
        //lexAsm.Next;  //Toma número
        skipWhites;
        if lexAsm.GetToken = ',' then begin
          //Solo puede ser (indirect,X)
          lexAsm.Next;  //Toma número
          skipWhites;
          if UpCase(lexAsm.GetToken) <> 'X' then begin
            GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
            exit;
          end;
          pic.codAsm(idInst, aIndirecX, ad);
          //Faltaría verificar  ')'
        end else if lexAsm.GetToken = ')' then begin
          //Solo puede ser (indirect),Y
          lexAsm.Next;  //Toma número
          skipWhites;
          if lexAsm.GetToken <> ',' then begin
            GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
            exit;
          end;
          pic.codAsm(idInst, aIndirecY, ad);
          lexAsm.Next;  //Toma número
          skipWhites;
          if UpCase(lexAsm.GetToken) <> 'Y' then begin
            GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
            exit;
          end;
        end else begin
          GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
          exit;
        end;
      end;
    end else begin
      GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
      exit;
    end;
    if pic.MsjError<>'' then begin
      GenErrorAsm(pic.MsjError);
    end;
  end else begin
    GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
    exit;
  end;
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
var
  n: LongInt;
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
      end else if tokType = lexAsm.tnNumber then begin
        //Era un número. Se codifica directamente la instrucción.
        //Por ahora solo soporta un código: ASM $ff END
        //No soporta la forma: ASM $ff,$ff,$ff END
        //Solo reconocerá el primer valor.
        if not TryStrToInt(lexAsm.GetToken, n) then begin
          //Es otra cosa
          GenErrorAsm(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
          exit;
        end;
        if (n>255) then begin
          GenErrorAsm(ER_EXPECT_BYTE);
          exit;
        end;
        pic.codByte(n, false);
        lexAsm.Next;
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
      EndASM;  //También termina porque es instrucción de una sola línea
    end else begin
      ProcASM(lin);  //procesa por si queda código
    end;
  end else if IsEndASM(lin) then begin
    //Es la última línea de ensamblador
    tokIni2 := 0;   //En el margen izquierdo, porque no está el delimit. inicial "ASM"
    ProcASM(lin);  //procesa por si queda código
    EndASM;  //Termina porque es la última instrucción
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
  lexAsm.AddIdentSpecList('ADC AND ASL', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('BCC BCS BEQ BIT BMI BNE BPL BRK BVC BVS CLC', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('CLD CLI CLV CMP CPX CPY', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('DEC DEX DEY', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('EOR', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('INC INX INY', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('JMP JSR', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('LDA LDX LDY LSR', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('NOP', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('ORA', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('PHA PHP PLA PLP', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('ROL ROR RTI RTS', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('SBC SEC SED SEI STA STX STY', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('TAX TAY TSX TXA TXS TYA', lexAsm.tnKeyword);

  lexAsm.AddIdentSpecList('ORG', lexAsm.tnKeyword);
  lexAsm.DefTokDelim(';','', lexAsm.tnComment);
  lexAsm.DefTokDelim('''','''', lexAsm.tnString);
  lexAsm.AddSymbSpec('(', lexAsm.tnSymbol);
  lexAsm.AddSymbSpec(')', lexAsm.tnSymbol);
  lexAsm.Rebuild;
  //Initialize events and functions of Compiler
  callProcASMlime := @ProcASMlime;
end;
destructor TParserAsm.Destroy;
begin
  lexAsm.Destroy;
  uJumps.Destroy;
  labels.Destroy;
  inherited Destroy;
end;

end.

