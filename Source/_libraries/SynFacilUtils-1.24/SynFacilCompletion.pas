{
SynFacilCompletion
==================
Por Tito Hinostroza

Pendientes:
* Incluir una forma simplficada de la forma <OpenOn AfterIdentif="Alter">, para simplificar
la definición clásica.
* Ver el trabajo de la librería con caracteres UTF-8 de dos bytes.
* Optimizar el método LookAround(), evitando tener que leer dos veces la línea actual
y de ser posible creando una rutina personalizada, en lugar de usar ExploreLine().
* Incluir el manejo de las ventanas de tipo "Tip", como ayuda para los parámetros de las
funciones.
* Hacer que la ventana de completado haga seguimiento del cursor, cuando este retrocede
mucho en un identificador.
* Realizar dos pasadas en la etiqueta <completion>, para que se puedan definir las listas
en cualquier parte.
}
{Descripción
============
Unidad que expande al resaltador TSynFacilSyn, para que pueda soportar configuraciones
de autocompletado de texto.

Se usa de forma similar a SynFacilSyn. Se debe crear un resaltador, pero ahora de la
clase TSynFacilComplet:

uses ... , SynFacilCompletion;

procedure TForm1.FormShow(Sender: TObject);
begin
 //configure highlighter
  hlt := TSynFacilComplet.Create(self);  //my highlighter
  SynEdit1.Highlighter := hlt;  //optional if we are going to use SelectEditor()
  hlt.LoadFromFile('./languages/ObjectPascal.xml');  //load syntax
  hlt.SelectEditor(SynEdit1);  //assign to editor
end;

Luego se debe interceptar los evento KeyUp y UTF8KeyPress, del SynEdit:

procedure TForm1.SynEdit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  hlt.KeyUp(Sender, Key, Shift);
end;

procedure TForm1.SynEdit1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  hlt.UTF8KeyPress(Sender, UTF8Key);
end;

Y se debe terminar correctamente:

procedure TForm1.FormDestroy(Sender: TObject);
begin
  hlt.UnSelectEditor;   //release editor (only necessary if we are to call to SelectEditor(), again)
  hlt.Free;  //destroy the highlighter
end;

Cuando se desea desaparecer la ventana de ayuda contextual por algún evento, se debe
llamar a CloseCompletionWindow().
}
unit SynFacilCompletion;
{$mode objfpc}{$H+}
//{$define Verbose}
interface
uses
  Classes, SysUtils, fgl, Dialogs, XMLRead, DOM, LCLType, Graphics, Controls,
  SynEdit, SynEditHighlighter, SynEditTypes, SynEditKeyCmds, Lazlogger,
  SynFacilHighlighter, SynFacilBasic, SynCompletion;

type
  TFaTokInfoPtr = ^TFaTokInfo;

  { TFaCompletItem }

  TFaCompletItem = class
  private
    fCaption: string;  //etiqueta a mostrar en el menú
    Replac : string;   //contenido a reemplazar
    Descrip: string;   //descripción del campo
    idxIcon: integer;  //índice al ícono
    function ExtractField(var str: string): string;
    procedure SetCaption(AValue: string);
  public
    property Caption: string read FCaption write SetCaption;
    function StartWith(const c: char): boolean; inline;
    function StartWithU(const c: char): boolean; inline;
  end;
  //TFaCompletItemPtr = ^TFaCompletItem;
  TFaCompletItems = specialize TFPGObjectList<TFaCompletItem>;

  //Filtros que se pueden aplicar a la lista mostrada
  TFaFilterList = (
    fil_None,           //Sin filtro. Muestra todos
    fil_LastTok,      //por el tokem -1
    fil_LastTokPart,  //por el token -1, hasta donde está el cursor
    fil_LastIdent,    //por el identificador anterior (usa su propia rutina para identifificadores)
    fil_LastIdentPart //similar pero toma hasta el cursor
  );

  //Objeto lista para completado
  { TFaCompletionList }
  TFaCompletionList = class
    Name : string;
    Items: TFaCompletItems; //lista de las palabras disponibles
    procedure AddItems(list: string; idxIcon: integer);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  //Colección de listas
  TFaCompletionLists = specialize TFPGObjectList<TFaCompletionList>;

  //Tipo de Elemento del patrón
  TFaTPatternElementKind = (
    pak_none,      //tipo no definido
    pak_String,    //es literal cadena
    pak_Identif,   //es token identificador (tkKeyword, tkIndetifier, ...)
    pak_NoIdentif, //no es token identificador
    pak_TokTyp,    //es un tipo específico de token
    pak_NoTokTyp   //no es un tipo específico de token
  );
  //Elemento del patrón
  TFaPatternElement = record
    patKind: TFaTPatternElementKind;
    str    : string;   //valor, cuando es del tipo pak_String
    toktyp : integer;  //valor cuando es de tipo pak_TokTyp o pak_NoTokTyp
  end;
  TFaPatternElementPtr = ^TFaPatternElement;

  {Tipos de secuencias de escape que se puede indicar para el reemplazo de texto.
   No son todas las secuencias de escape, sino solo las que necesitan procesarse
   independientemente para ejecutar correctamente la acción de reemplazo.}
  TFaCompletSeqType = (
    csqNone,    //no es secuencia de escape
    csqCurPos,  //secuencia que indica posición del cursor
    csqTabSpa   //tabulación al nivel del primer caracter de línea anterior
  );

  //Entorno del cursor
  { TFaCursorEnviron }
  TFaCursorEnviron = class
  private
    hlt: TSynFacilSyn;        //referencia al resaltador que lo contiene
    tokens   : TATokInfo;     //lista de tokens actuales
    StartIdentif : integer;   //inicio de identificador
    function ExtractStaticText(var ReplaceSeq: string; out seq: TFaCompletSeqType
      ): string;
    procedure InsertSequence(ed: TSynEdit; Pos1, Pos2: TPoint; ReplaceSeq: string);
    procedure UpdateStartIdentif;
  public
    inMidTok : boolean;        //indica si el cursor está en medio de un token
    tok0     : TFaTokInfoPtr;  //referencia al token actual.
    tok_1    : TFaTokInfoPtr;  //referencia al token anterior.
    tok_2    : TFaTokInfoPtr;  //referencia al token anterior a tok_1.
    tok_3    : TFaTokInfoPtr;  //referencia al token anterior a tok_2.
    CurX     : Integer;        //posición actual del cursor
    CurY     : Integer;        //posición actual del cursor
    curLine  : string;         //línea actual de exploración
    curBlock : TFaSynBlock;    //referencia al bloque actual
    caseSen  : boolean;        //indica el estado de caja actual
    procedure LookAround(ed: TSynEdit; CaseSen0: boolean);
    //Las siguientes funciones, deben llaamrse después de lamar a LookAround()
    function HaveLastTok: boolean;
    function LastTok: string;
    function LastTokPart: string;
    function HaveLastIdent: boolean;
    function LastIdent: string;
    function LastIdentPart: string;
    //Estas funciones implementan las acciones
    procedure ReplaceLastTok(ed: TSynEdit; ReplaceSeq: string);
    procedure ReplaceLastIden(ed: TSynEdit; ReplaceSeq: string);
    procedure Insert(ed: TSynEdit; ReplaceSeq: string);
  public
    constructor Create(hlt0: TSynFacilSyn);
  end;

  TFaOpenEvent = class;
  TFaOnLoadItems = procedure(opEve: TFaOpenEvent; curEnv: TFaCursorEnviron;
                             out Cancel: boolean) of object;
  //Acciones válidas que se realizarán al seleccionar un ítem
  TFAPatAction = (
    pac_None,       //no se realiza ninguna acción
    pac_Default,    //acción pro defecto
    pac_Insert,     //se inserta el texto seleccionado en la posición del cursor
    pac_Rep_LastTok //se reemplaza el token anterior
  );
  //Objeto evento de apertura
  { TFaOpenEvent }
  TFaOpenEvent = class
  private
    hlt: TSynFacilSyn;  //referencia al resaltador que lo contiene
    {Los índices de elem[] representan posiciones relativas de tokens
      [0]  -> Token que está justo después del cursor (token actual)
      [-1] -> Token que está antes del token actual
      [-2] -> Token que está antes del token [-1]
      [-3] -> Token que está antes del token [-2]    }
    elem : array[-3..0] of TFaPatternElement;
    nBef : integer;   //número de elementos válidos haste el ítem 0 (puede ser 0,1,2 o 3)
    nAft : integer;   //número de elementos válidos depués del ítem 0 (puede ser 0 o 1)
    procedure ExtractElementIn(var befPat: string;
      patEle: TFaPatternElementPtr; var ErrStr: string);
    function MatchPatternElement(nPe: integer; tokX: TFaTokInfoPtr;
      CaseSens: boolean): boolean;
    function MatchPatternBefore(const curEnv: TFaCursorEnviron): boolean;
    function MatchPatternAfter(const curEnv: TFaCursorEnviron): boolean;
    function MatchPattern(const curEnv: TFaCursorEnviron): boolean;
    procedure ShiftBeforePattern;
  public
    name  : string;    //nombre del evento de apertura
    startX: integer;   //posición inicial del token o identificador de trabajo
    filter: TFaFilterList;
    block : TFaSynBlock;   //bloque donde es válido
    Action: TFAPatAction;  //Acción al seleccionar lista
    OnLoadItems: TFaOnLoadItems; //Se llama antes de cargar los ítems.
    procedure FilterByChar(curEnv: TFaCursorEnviron; const c: char);
    procedure DoAction(ed: TSynEdit; env: TFaCursorEnviron; ReplaceSeq: string);
    procedure FillFilteredIn(const env: TFaCursorEnviron; lst: TStrings); //Llena Items en una lista
    //manejo patrones
    procedure ClearBeforePatt;  //limpia el patron anterior
    procedure ClearAfterPatt;   //limpia el patron anterior
    procedure AddBeforeElement(var befPat: string; out ErrStr: string);
    procedure AddAfterElement(var aftPat: string; var ErrStr: string);
  public   //Manejo de ítems
    Items : TFaCompletItems;    //Lista de las palabras disponibles para el completado
    Lists : TFaCompletionLists; //Referencias a listas
    Avails: TFaCompletItems;  //Ítems a cargar cuando se active el patrón.
    procedure ClearItems;
    procedure LoadItems(curEnv: TFaCursorEnviron);
    procedure AddItem(txt: string; idxIcon: integer);
    procedure AddItems(lst: TStringList; idxIcon: integer);
    procedure AddItems(list: string; idxIcon: integer);
    procedure AddList(Alist: TFaCompletionList; OnlyRef: boolean);
    procedure ClearAvails;
    procedure AddAvail(txt: string);  //Rutina simple para agregar cadena a Avails
    procedure Clear;
  public
    constructor Create(hlt0: TSynFacilSyn);
    destructor Destroy; override;
  end;

  //Lista de patrones
  TFaOpenEvents = specialize TFPGObjectList<TFaOpenEvent>;

type
  { TSynCompletionF }
  {Clase personalizada de "TSynCompletion" usada para el completado con "TSynFacilComplet"}
  TSynCompletionF = class(TSynCompletion)
    function OnSynCompletionPaintItem(const {%H-}AKey: string; ACanvas: TCanvas; X,
      Y: integer; {%H-}IsSelected: boolean; Index: integer): boolean;
  public
    IconList: TImageList;   //lista de íconos
    procedure Refresh;
    constructor Create(AOwner: TComponent); override;
  end;

  { TSynFacilComplet }
  //clase principal
  TSynFacilComplet = class(TSynFacilSyn)
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OnCodeCompletion(var Value: string; SourceValue: string;
      var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  protected
    ed        : TSynEdit;        //referencia interna al editor
    MenuComplet: TSynCompletionF;//menú contextual
    curEnv    : TFaCursorEnviron;  //entorno del cursor

    UtfKey         : TUTF8Char;     //tecla pulsada
    SpecIdentifiers: TArrayTokSpec;
    SearchOnKeyUp : boolean;       //bandera de control
    procedure MenuComplet_OnExecute(Sender: TObject); virtual;
    function CheckForClose: boolean;
    procedure FillCompletMenuFiltered;
    procedure ProcCompletionLabel(nodo: TDOMNode);
    procedure ReadSpecialIdentif;
  private  //Manejo de patrones de apertura
    CompletLists: TFaCompletionLists;  //colección de listas de compleatdo
    function FindOpenEventMatching: TFaOpenEvent;
    function GetIconList: TImageList;
    procedure ProcXMLOpenOn(nodo: TDOMNode);
    procedure SetIconList(AValue: TImageList);
  public   //Manejo de patrones de apertura
    OpenEvents  : TFaOpenEvents;   //lista de eventos de apertura
    CurOpenEve  : TFaOpenEvent;    //evento de apertura que se aplica en el momento
    function FindOpenEvent(oeName: string): TFaOpenEvent;  //Busca un evento de apertura
  public
    CompletionOn: boolean;  //activa o desactiva el auto-completado
    SelectOnEnter: boolean; //habilita la selección con enter
    CaseSensComp: boolean;  //Uso de caja, en autocompletado
    OpenOnKeyUp: boolean;   //habilita que se abra automáticamente al soltar una tecla
    property IconList: TImageList read GetIconList write SetIconList;
    function AddOpenEvent(AfterPattern, BeforePattern: string;
      filter: TFaFilterList): TFaOpenEvent;
    function AddComplList(lstName: string): TFaCompletionList;
    function GetListByName(lstName: string): TFaCompletionList;
    procedure LoadFromFile(const Filename: string); override;
    function LoadSyntaxFromPath(SourceFile: string; path: string;
      CaseSens: boolean=false): string;
    procedure SelectEditor(ed0: TSynEdit);  //inicia la ayuda contextual
    procedure UnSelectEditor;  //termina la ayuda contextual con el editor
    procedure UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OpenCompletionWindow(vKey: word; vShift: TShiftState; vUtfKey: TUTF8Char
      );
    procedure CloseCompletionWindow;
  public  //Constructor y Destructor
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  //utilidades
  function ReadExtenFromXML(XMLfile: string): string;
  function XMLFileHaveExten(XMLfile: string; exten: string;
                CaseSens: boolean = false): boolean;

implementation
uses SynEditMiscProcs;

const
//  ERR_ATTRIB_NO_EXIST = 'Atributo %s no existe. (etiqueta <COMPLETION ...>)';
//  ERR_FILTER_NO_EXIST = 'Filtro %s no existe. (etiqueta <OpenOn ...>)';
//  ERR_INVAL_LAB_COMP = 'Etiqueta %s no válida para etiqueta <COMPLETION ...>';
//  ERR_INVAL_BLK_NAME = 'Nombre de bloque inválido.';
//  ERROR_LOADING_ = 'Error loading: ';

//  ERR_PAT_EXP_ENDSTR = 'Se esperaba fin de cadena';
//  ERR_PAT_INVALID_ = 'Invalido: %s';
//  ERR_PAT_SYNTAX_ERR = 'Error de sintaxis.';
//  ERR_PAT_TOO_MAN_EL = 'Demasiados elementos.';
//  ERR_PAR_AFT_PATT = 'Error en "BeforePattern"';
//  ERR_PAR_BEF_PATT = 'Error en "AfterPattern"';

  ERR_ATTRIB_NO_EXIST = 'Attribute %s doesn''t exist. (label <OpenOn ...>)';
  ERR_LIST_NO_EXIST = 'List "%s" doesn''t exist. (label <OpenOn ...>)';
  ERR_FILTER_NO_EXIST = 'Filter %s doesn''t exist. (label <OpenOn ...>)';
  ERR_ACTION_NO_EXIST = 'Action %s doesn''t exist. (label <OpenOn ...>)';
  ERR_INVAL_LAB_OPNON = 'Invalid label %s for <OpenOn ...>';
  ERR_INVAL_LAB_COMP = 'Invalid label %s for  <COMPLETION ...>';
  ERR_INVAL_BLK_NAME = 'Invalid block name.';
  ERROR_LOADING_ = 'Error loading: ';

  ERR_PAT_EXP_ENDSTR = 'Expected end of string';
  ERR_PAT_INVALID_ = 'Invalid: %s';
  ERR_PAT_SYNTAX_ERR = 'Syntax error.';
  ERR_PAT_TOO_MAN_EL = 'Too many elements.';
  ERR_PAR_AFT_PATT   = 'Error in "AfterPattern"';
  ERR_PAR_BEF_PATT = 'Error in "BeforePattern"';

  //Constantes para manejar parámetros de <OpenOn>
  WORD_CHARS = ['a'..'z','0'..'9','A'..'Z','_'];
  STR_DELIM = ['''','"'];
  ALL_IDENTIF = 'AllIdentifiers';
  //Para el reconocimiento de identificadores, cuando se usa "fil_LastIdent" y "fil_LastIdentPart"
  CHAR_STRT_IDEN = ['a'..'z','A'..'Z','_'];
  CHAR_BODY_IDEN = CHAR_STRT_IDEN + ['0'..'9'];

function ReadExtenFromXML(XMLfile: string): string;
//Lee las extensiones que tiene definidas un archivo de sintaxis.
var doc: TXMLDocument;
  atri: TDOMNode;
  i: Integer;
begin
  try
    Result := '';   //por defecto
    ReadXMLFile(doc, XMLfile);  //carga archivo
    //busca el parámetro "ext"
    for i:= 0 to doc.DocumentElement.Attributes.Length-1 do begin
      atri := doc.DocumentElement.Attributes.Item[i];
      if UpCase(atri.NodeName) = 'EXT' then begin
        Result := trim(AnsiString(atri.NodeValue));  //valor sin espacios
      end;
    end;
    doc.Free;  //libera
  except
    on E: Exception do begin
      ShowMessage(ERROR_LOADING_ + XMLfile + #13#10 + e.Message);
      doc.Free;
    end;
  end;
end;
function XMLFileHaveExten(XMLfile: string; exten: string;
                CaseSens: boolean = false): boolean;
//Indica si un archivo XML de sintaxis, tiene definida la extensión que se indica
//La comparación se puede hacer con o sin diferecnia de caja
var
  lext: TStringList;
  s: String;
  tmp: String;
begin
  Result := false;  //por defecto
  lext:= TStringList.Create;  //crea lista
  tmp := ReadExtenFromXML(XMLfile);  //lee archivo
  lext.DelimitedText:=' ';
  lext.Text:=tmp;  //divide
  //busca de acuerdo a la caja
  if CaseSens then begin
    for s in lext do begin
      if s = exten then begin
        //encontró
        Result := true;
        lext.Free;
        exit;
      end;
    end;
  end else begin
    for s in lext do begin
      if Upcase(s) = Upcase(exten) then begin
        //encontró
        Result := true;
        lext.Free;
        exit;
      end;
    end;
  end;
  //No enecontró
  lext.Free;
end;
{ TFaCompletItem }
function TFaCompletItem.ExtractField(var str: string): string;
{Extrae un campo de la cadena. Los campos deben estar delimitado con "|" sin caracter
 de escape}
  function EscapeBefore(i: integer): boolean; inline;
  begin
    if i<1 then exit(false);
    if str[i-1] = '\' then exit(true) else exit(false);
  end;
var
  psep: SizeInt;
begin
  psep := pos('|', str);
  if (psep = 0) or EscapeBefore(psep) then begin
    //no hay separador de campos, es un caso simple
    Result := str;
    str := '';
  end else begin
    //hay separador
    Result:=copy(str,1,psep-1);
    str := copy(str, psep+1, length(str));
  end;
end;
procedure TFaCompletItem.SetCaption(AValue: string);
{Asigna el valor a Caption, separando los campos si es que vinieran codificados}
{Recibe una cadena que representa a un ítem y de el extrae los campos, si es que vinieran
codificados. El formato de la codificiacón es:
 <texto a mostrar> | <texto a reemplazar> | <descripción>
}
  function ExecEscape(const s: string): string;
  {Reemplaza las secuencias de escape para mostrarlas en el menú de completado.
   Tal vez convenga hacer este reemplazo, en la rutina que muestra los ítems, por un
   tema de velocidad.}
  begin
    Result := StringReplace(s, '\n', ' ', [rfReplaceAll]);
    Result := StringReplace(Result, '\t', ' ', [rfReplaceAll]);
    Result := StringReplace(Result, '\u', ' ', [rfReplaceAll]);
    Result := StringReplace(Result, '\\', '\', [rfReplaceAll]);
    Result := StringReplace(Result, '\|', '|', [rfReplaceAll]);
    Result := StringReplace(Result, '\_', '', [rfReplaceAll]);
  end;
var
  txt1, txt2: String;
begin
  if fCaption=AValue then Exit;
  txt1 := ExtractField(AValue);
  if AValue='' then begin
    //solo hay un campo
    fCaption :=ExecEscape(txt1);
    Replac   :=txt1;   //los caracteres de escape se expandirán al reemplazar
    Descrip  :='';
  end else begin
    //hay al menos otro campo
    txt2 := ExtractField(AValue);
    if AValue = '' then begin
      //hay solo dos campos
      fCaption :=ExecEscape(txt1);
      Replac   := txt2;  //los caracteres de escape se expandirán al reemplazar
      Descrip  :='';
    end else begin
      //has 3 o más campos
      fCaption :=ExecEscape(txt1);
      Replac   := txt2;  //los caracteres de escape se expandirán al reemplazar
      Descrip  := ExecEscape(AValue);
    end;
  end;
end;
function TFaCompletItem.StartWith(const c: char): boolean;
begin
  Result := (fCaption<>'') and (fCaption[1] = c);
end;
function TFaCompletItem.StartWithU(const c: char): boolean;
begin
  Result := (fCaption<>'') and (UpCase(fCaption[1]) = c);
end;
{ TFaCompletionList }
procedure TFaCompletionList.AddItems(list: string; idxIcon: integer);
//Agrega una lista de ítems, separados por espacios, a la lista de completado
var
  lst: TStringList;
  i: Integer;
  it: TFaCompletItem;
begin
  //divide
  lst := TStringList.Create;
  lst.Delimiter := ' ';
  lst.DelimitedText := list;

  //agrega
  for i:= 0 to lst.Count-1 do begin
    it := TFaCompletItem.Create;
    it.Caption := lst[i];
    it.idxIcon:=idxIcon;
    Items.Add(it);
  end;
  lst.Destroy;
end;
constructor TFaCompletionList.Create;
begin
  Items:= TFaCompletItems.Create(true);  //lista con administración
end;
destructor TFaCompletionList.Destroy;
begin
  Items.Destroy;
  inherited Destroy;
end;
{ TFaCursorEnviron }
constructor TFaCursorEnviron.Create(hlt0: TSynFacilSyn);
begin
  hlt := hlt0;
end;
procedure TFaCursorEnviron.LookAround(ed: TSynEdit; CaseSen0: boolean);
{Analiza el estado del cursor en el editor. Se supone que se debe llamar, después de
 actualizar el editor. Actualiza: PosiCursor, curBlock, tok0, tok_1, tok_2
 y tok_3. Utiliza punteros, para evitar perder tiempo creando copias.}
var
  iTok0    : integer;       //índice al token actual
begin
  caseSen:=CaseSen0;  //actualiza estado
  //valores por defecto
  curBlock := nil;
  //explora la línea con el resaltador
  hlt.ExploreLine(ed.CaretXY, tokens, iTok0);
  curLine := ed.Lines[ed.CaretY-1]; //Se gaurda porque se va a necesitar
  if iTok0=-1 then exit;   //no ubica al token actual
  tok0 := @tokens[iTok0];    //lee token actual token[0]
  CurX := ed.LogicalCaretXY.x;  //usa posición física para comparar
  CurY := ed.LogicalCaretXY.y;
  inMidTok := tokens[iTok0].posIni+1 <> CurX;  //actualiza bandera
  //actualiza tok_1
  if inMidTok then begin
    tok_1 := @tokens[iTok0];
    if iTok0>0 then tok_2 := @tokens[iTok0-1]
    else tok_2 := nil;
    if iTok0>1 then tok_3 := @tokens[iTok0-2]
    else tok_3 := nil;
  end else begin
    if iTok0>0 then tok_1 := @tokens[iTok0-1]
    else tok_1 := nil;
    if iTok0>1 then tok_2 := @tokens[iTok0-2]
    else tok_2 := nil;
    if iTok0>2 then tok_3 := @tokens[iTok0-3]
    else tok_3 := nil;
  end;
  //captura "curBlock"
  curBlock := tok0^.curBlk;  //devuelve bloque
  {$IFDEF Verbose}
  DbgOut('  LookAround:(');
  if tok_3<>nil then DbgOut(hlt.Attrib[tok_3^.TokTyp].Name+',');
  if tok_2<>nil then DbgOut(hlt.Attrib[tok_2^.TokTyp].Name+',');
  if tok_1<>nil then DbgOut(hlt.Attrib[tok_1^.TokTyp].Name+',');
  if tok0<>nil then DbgOut(hlt.Attrib[tok0^.TokTyp].Name);
  debugln(')');
  {$ENDIF}
end;
{Las siguientes funciones, deben llamarse después de lamar a LookAround(). Deben ser de
ejecución rápida}
function TFaCursorEnviron.HaveLastTok: boolean; inline;
begin
  Result := (tok_1 <> nil);
end;
function TFaCursorEnviron.LastTok: string; inline;
{Devuelve el último token}
begin
  Result := tok_1^.txt;
end;
function TFaCursorEnviron.LastTokPart: string; inline;
{Devuelve el último token, truncado a la posición del cursor}
begin
//  Result := copy(tok0^.txt,1,CurX-tok0^.posIni-1);
  Result := copy(tok_1^.txt, 1, CurX-tok_1^.posIni-1);
end;
procedure TFaCursorEnviron.UpdateStartIdentif;
{Actualiza el índice al inicio del identificador anterior, a la posición actual del cursor.
 Este es un algoritmo, un poco especial, porque los identificadores no se
 definen para explorarlos hacia atrás.}
var
  i: Integer;
begin
  StartIdentif := -1;   //valor por defecto
  if CurX<=1 then exit;  //está al inicio
  i:= CurX-1;  //caracter anterior al cursor
  {Se asume que el cursor, está después de un identificador y retrocede por los
  caracteres hasta encontrar un caracter que pueda ser inicio de identificador}
  while (i>0) and (curLine[i] in CHAR_BODY_IDEN) do begin
    if curLine[i] in CHAR_STRT_IDEN then begin
       StartIdentif := i;    //guarda una posible posición de inicio
    end;
    dec(i);
  end;
end;
function TFaCursorEnviron.HaveLastIdent: boolean;
{Indica si hay un identificador antes del cursor. Debe llamarse siempre antes de
 usar LastIdent().}
begin
  UpdateStartIdentif;
  Result := (StartIdentif <> -1);
end;
function TFaCursorEnviron.LastIdent: string;
{Devuelve el identificador anterior al cursor. Debe llamarse siempre despues de llamar
 a HaveLastIdent}
var
  i: Integer;
begin
  {Ya sabemos que hay identificador hasta antes del cursor, ahora debemos ver, hasta
   dónde se extiende}
  i := CurX;
  while curLine[i] in CHAR_BODY_IDEN do  //no debería ser necesario verificar el final
    inc(i);
  Result := copy(curLine, StartIdentif, i-StartIdentif+1);
end;
function TFaCursorEnviron.LastIdentPart: string;
{Devuelve el identificador anterior al cursor. Debe llamarse siempre despues de llamar
 a HaveLastIdent}
begin
  Result := copy(curLine, StartIdentif, CurX-StartIdentif);
end;
{Estas funciones implementan las acciones. Procesan las secuencias de escape}
function TFaCursorEnviron.ExtractStaticText(var ReplaceSeq: string;
                                            out seq: TFaCompletSeqType): string;
{Extrae un fragmento de texto de "ReplaceSeq", que puede insertarse directamente en el editor,
 sin necesidad de hacer cálculos de posición, o que no contengan comandos de posicionamiento
 del cursor. La idea es que el texto que se devuelva aquí, se pueda insertar directamente
 en el editor con una simple operación "Insert". El tipo de secuencia que produjo la ruptura,
 se devuelve en "seq"}
  function ReplaceEscape(const s: string): string;
  begin
    Result := StringReplace(s, '\n', LineEnding, [rfReplaceAll]);
    Result := StringReplace(Result, '\t', #9, [rfReplaceAll]);
    Result := StringReplace(Result, '\|', '|', [rfReplaceAll]);
    Result := StringReplace(Result, '\\', '\', [rfReplaceAll]);
  end;
  function FirstPos(substrs: array of string; str: string; out found: string): integer;
  {Busca la ocurrencia de cualquiera de las cadenas dadas en "substrs". Devuelve el índice
   a la primera encontrada. Si no enceuntra ninguna, devuelve 0.}
  var
    i, p: Integer;
    limit: Integer;
    lin: string;
  begin
    Result := 0;   //valor inicial
    found := '';
    limit := length(str);
    for i:=0 to high(substrs) do begin
      lin := copy(str, 1, limit);
      p := Pos(substrs[i], lin);
      if p<>0 then begin
        //encontró uno, compara
        if p<limit then begin
          limit := p;  //restringe límite para la siguiente búsqueda
          found := substrs[i];  //lo que enontró
          Result := p;
        end;
      end;
    end;
  end;
var
  p: Integer;
  hay: string;
begin
  //Detcta las secuencias de posición de cursor, o tabulación '\u'.
  p := FirstPos(['\_','\u'], ReplaceSeq, hay);  //tabulación al primer caracter no blanco de la línea superior no blanca
  if hay = '' then begin
    //No hay secuecnia especial
    Result := ReplaceEscape(ReplaceSeq);
    seq := csqNone;   //no se ecnontró secuencia de ruptura
    ReplaceSeq := '';
  end else if hay = '\_' then begin
    //primero está la secuencia de cursor
    Result := ReplaceEscape(copy(ReplaceSeq,1,p-1));
    seq := csqCurPos;   //Indica secuencia de posicionamiento de cursor
    ReplaceSeq := copy(ReplaceSeq, p+2, length(ReplaceSeq));
  end else if hay = '\u' then begin
    //primero está la secuencia de tabulación
    Result := ReplaceEscape(copy(ReplaceSeq,1,p-1));
    seq := csqTabSpa;   //Indica secuencia
    ReplaceSeq := copy(ReplaceSeq, p+2, length(ReplaceSeq));
  end;
end;
procedure TFaCursorEnviron.InsertSequence(ed: TSynEdit; Pos1, Pos2: TPoint; ReplaceSeq: string);
{Inserta una secuencia de reemplazo en el bloque definido por P1 y P2}
  function FindNoWhiteLineUp(ed: TSynEdit): string;
  {Busca hacia arriba, una línea con caracteres diferentes de espacio y que ocupen una posición
   más a la derecha de la posición actual del cursor. La búsqueda se hace a partir de la
   posición actual del cursor.  Si no encuentra, devuelve línea en blanco.}
  var
    x,y: Integer;
    lin: String;
  begin
    y := ed.CaretY-1;  //desde la línea anterior
    x := ed.CaretX;
    while y>0 do begin
      lin := ed.Lines[y-1];
      if trim(copy(lin,x, length(lin)))<>'' then
        exit(lin);
      dec(y);
    end;
    //no encontró
    exit('');
  end;
var
  toRepl: String;
  cursorPos: TPoint;
  seq: TFaCompletSeqType;
  linNoWhite, curLin: String;
  i, dif: Integer;
begin
  ed.BeginUndoBlock;
  ed.TextBetweenPointsEx[Pos1,Pos2, scamEnd] := ''; //elimina el contenido y deja cursor al final
  cursorPos.x := -1;  //marca bandera
  while ReplaceSeq<>'' do begin
    toRepl := ExtractStaticText(ReplaceSeq, seq);
    case seq of
    csqNone: begin
      //no hay ruptura, es un texto sencillo
      //reemplaza y deja cursor al final
      Pos1 := ed.CaretXY;
      ed.TextBetweenPointsEx[Pos1,Pos1, scamEnd] := toRepl;
      //se suepone que esta es la última secuencia
    end;
    csqCurPos: begin
      //hay comando de posicionamiento de cursor
      //reemplaza, deja cursor al final y guarda posición
      Pos1 := ed.CaretXY;
      ed.TextBetweenPointsEx[Pos1,Pos1, scamEnd] := toRepl;
      cursorPos := ed.CaretXY;
    end;
    csqTabSpa: begin
      //hay comando de tabulación inteligente
      Pos1 := ed.CaretXY;
      //inserta fragmento
      ed.TextBetweenPointsEx[Pos1,Pos1, scamEnd] := toRepl;
      //calcula espaciado
      linNoWhite := FindNoWhiteLineUp(ed);
      if linNoWhite<>'' then begin
        //hay línea sin blancos, busca posición de caracter no blanco
        for i:=ed.CaretX to length(linNoWhite) do begin
          //La definición de blanco #1..#32, corresponde al resaltador
          if not (linNoWhite[i] in [#1..#32]) then begin
             //Encontró. Ahora hay que posicionar el cursor en "i".
             curLin := ed.LineText;   //línea actual
             if length(curLin)<i then begin
               //No hay caracteres, en donde se quiere colocar el cursor.
               dif := i - length(curLin);   //esto es lo que falta
               ed.CaretX := length(curLin)+1;   //pone cursor al final
               ed.InsertTextAtCaret(Space(dif));  {Completa con espacios. Usa InsertTextAtCaret,
                                                  para poder deshacer.}
               ed.CaretX:=i;   //ahora sí se puede posicionar el cursor.
             end else begin
               //Se puede posicionar directamente
               ed.CaretX:=i;
             end;
             break; //sale del FOR
          end;
        end;
        {Si llega aquí sin encontrar el caracter buscado, indicaria que el
         algoritmo de búsqueda de FindNoWhiteLineUp() no es consistente con este código.}
      end;
    end;
    end;
  end;
  if cursorPos.x<>-1 then begin
    //ha habido posicionamiento de cursor
    ed.CaretXY := cursorPos;
  end;
  ed.EndUndoBlock;
end;
procedure TFaCursorEnviron.ReplaceLastTok(ed: TSynEdit; ReplaceSeq: string);
{Reemplaza el último token}
var
  Pos1, Pos2: TPoint;
begin
  if not HaveLastTok then exit;
  Pos1 := Point(tok_1^.posIni + 1, CurY);
  Pos2 := Point(tok_1^.posIni + tok_1^.length+1, CurY);
  //Realiza el reemplazo del texto, con procesamiento
  InsertSequence(ed, Pos1, Pos2, ReplaceSeq);
end;
procedure TFaCursorEnviron.ReplaceLastIden(ed: TSynEdit; ReplaceSeq: string);
{Reemplaza el último identificador}
var
  Pos1, Pos2: TPoint;
  i: Integer;
begin
  if not HaveLastIdent then exit;
  Pos1 := Point(StartIdentif, CurY);
  i := CurX;
  while (i<=length(curLine)) and (curLine[i] in CHAR_BODY_IDEN) do
    inc(i);
  Pos2 := Point(i, CurY);
  InsertSequence(ed, Pos1, Pos2, ReplaceSeq);
end;
procedure TFaCursorEnviron.Insert(ed: TSynEdit; ReplaceSeq: string);
{Reemplaza un texto en la posición actual del cursor}
var
  Pos1: TPoint;
begin
  Pos1 := Point(CurX, CurY);
  InsertSequence(ed, Pos1, Pos1, ReplaceSeq);
end;
{ TFaOpenEvent }
procedure TFaOpenEvent.DoAction(ed: TSynEdit; env: TFaCursorEnviron;
  ReplaceSeq: string);
{Ejecuta la acción que se tenga definido para el evento de apertura}
begin
  case Action of
  pac_None:;  //no ahce nada
  pac_Default:  //acción por defecto
      case Filter of
      fil_None: ;  //no hay elemento de selcción
      fil_LastTok,
      fil_LastTokPart:  //trabaja con el último token
        env.ReplaceLastTok(ed, ReplaceSeq);
      fil_LastIdent,
      fil_LastIdentPart:  //trabaja con el úmtimo identificador
        env.ReplaceLastIden(ed, ReplaceSeq);
      end;
  pac_Insert:  //inserta
    env.Insert(ed, ReplaceSeq);
  pac_Rep_LastTok:
    env.ReplaceLastTok(ed, ReplaceSeq);
  {Se pueden completar más acciones}
  else
    env.ReplaceLastTok(ed, ReplaceSeq);
  end;
end;
procedure TFaOpenEvent.FillFilteredIn(const env: TFaCursorEnviron; lst: TStrings);
{Filtra los ítems que contiene (usando "env") y los pone en la lista indicada}
  procedure FilterBy(const str: string);
  //Llena el menú de completado a partir de "Avails", filtrando solo las
  //palabras que coincidan con "str"
  var
    l: Integer;
    it: TFaCompletItem;
    str2: String;
  begin
    l := length(str);
    //Genera la lista que coincide
    if env.caseSen then begin
      for it in Avails do begin
        //esta no es la forma más eficiente de comparar, pero sirve por ahora.
        if str = copy(it.fCaption,1,l) then
//          lst.Add(Avails[i]^.text);
          lst.AddObject(it.fCaption, it);
      end;
    end else begin  //ignora la caja
      str2 := UpCase(str);
      for it in Avails do begin
        if str2 = upcase(copy(it.fCaption,1,l)) then begin
//          lst.Add(Avails[i]^.text);
          lst.AddObject(it.fCaption, it);
        end;
      end;
    end;
  end;

var
  it: TFaCompletItem;
begin
  case Filter of
  fil_None: begin  //agrega todos
      for it in Avails do begin  //agrega sus ítems
        lst.AddObject(it.fCaption, it);
      end;
    end;
  fil_LastTok: begin  //último token
      if env.HaveLastTok then
        FilterBy(env.LastTok);
    end;
  fil_LastTokPart: begin  //último token hasta el cursor
      if env.HaveLastTok then
        FilterBy(env.LastTokPart);
    end;
  fil_LastIdent: begin  //último token
      if env.HaveLastIdent then
        FilterBy(env.LastIdent);
    end;
  fil_LastIdentPart: begin  //último token hasta el cursor
      if env.HaveLastIdent then
        FilterBy(env.LastIdentPart);
    end;
  end;
end;
//manejo de elementos de patrones
procedure TFaOpenEvent.ClearBeforePatt;
begin
  elem[-3].patKind := pak_none;
  elem[-2].patKind := pak_none;
  elem[-1].patKind := pak_none;
  nBef:=0;  //no hay elementos válidos
end;
procedure TFaOpenEvent.ClearAfterPatt;
begin
  elem[0].patKind := pak_none;
  nAft:=0;  //no hay
end;
procedure TFaOpenEvent.ExtractElementIn(var befPat: string; patEle:TFaPatternElementPtr;
                                        var ErrStr: string);
{Extrae un elemento de un patrón de tokens que viene en cadena y lo almacena en "patEle".
 La cadena puede ser algo así como "Identifier,'.',AllIdentifiers".
 Si encuentra error, devuelve el mensaje en "ErrStr".}
  function ExtractIdentifier(var befPat: string): string;
  {Extrae un identificador de la cadena "befPat"}
  var
    i: Integer;
  begin
    i := 1;
    while (i<=length(befPat)) and (befPat[i] in WORD_CHARS)  do begin
      inc(i);
    end;
    Result := copy(befPat, 1,i-1);  //extrae cadena
    befPat := copy(befPat, i, length(befPat)); //recorta
  end;
  function ExtractString(var befPat: string; var ErrStr: string): string;
  {Extrae una cadena de "befPat". Si hay error devuelve mensaje en "ErrStr"}
  var
    i: Integer;
    ci: Char;
  begin
    ci := befPat[1];   //caracter inicial
    i := 2;
    while (i<=length(befPat)) and (befPat[i] <> ci) do begin
      inc(i);
    end;
    if i>length(befPat) then begin
      ErrStr := ERR_PAT_EXP_ENDSTR;
      exit;
    end;
    Result := copy(befPat, 1,i);  //extrae cadena
    befPat := copy(befPat, i+1, length(befPat)); //recorta
  end;
  procedure ExtractChar(var befPat: string);
  begin
    befPat := copy(befPat, 2, length(befPat));
  end;
  procedure ExtractComma(var befPat: string);
  begin
    befPat := TrimLeft(befPat);     //quita espacios
    //quita posible coma final
    if (befPat<>'') and (befPat[1] = ',') then
      befPat := copy(befPat, 2, length(befPat));
  end;
var
  strElem: String;
begin
  if befPat[1] in WORD_CHARS then begin
    //Es un identificador: tipo de token o la cadena especial "AllIdentifiers"
    strElem := ExtractIdentifier(befPat);
    if upcase(strElem) = upcase(ALL_IDENTIF) then begin
      //es de tipo "Todos los identificadores"
      patEle^.patKind := pak_Identif;
    end else if hlt.IsAttributeName(strElem) then begin  //es
      //Es nombre de tipo de token
      patEle^.patKind := pak_TokTyp;
      patEle^.toktyp := hlt.GetAttribIDByName(strElem);   //tipo de atributo
    end else begin  //no es, debe haber algún error
      ErrStr := Format(ERR_PAT_INVALID_,[strElem]);
      exit;
    end;
    ExtractComma(befpat);
  end else if befPat[1] = '!' then begin
    //debe ser de tipo "No es ..."
    ExtractChar(befPat);
    strElem := ExtractIdentifier(befPat);
    if upcase(strElem) = upcase(ALL_IDENTIF) then begin
       //es de tipo "Todos los identificadores"
       patEle^.patKind := pak_NoIdentif;
    end else if hlt.IsAttributeName(strElem) then begin
       //Es nombre de tipo de token
       patEle^.patKind := pak_NoTokTyp;
       patEle^.toktyp := hlt.GetAttribIDByName(strElem);   //tipo de atributo
    end else begin  //no es, debe haber algún error
      ErrStr := Format(ERR_PAT_INVALID_,[strElem]);
      exit;
    end;
    ExtractComma(befpat);
  end else if befPat[1] in STR_DELIM then begin
    //es un literal cadena
    strElem := ExtractString(befPat, ErrStr);
    if ErrStr<>'' then exit;
    patEle^.patKind := pak_String;
    patEle^.str:= copy(strElem, 2, length(strElem)-2);
    ExtractComma(befpat);
  end else begin
    ErrStr := ERR_PAT_SYNTAX_ERR;
    exit;
  end;
end;
procedure TFaOpenEvent.AddBeforeElement(var befPat: string; out ErrStr: string);
{Agrega un elemento al patrón anterior. Si encuentra error devuelve el mensaje en ErrStr}
var
  patEle: ^TFaPatternElement;
begin
  ErrStr := '';
  befPat := TrimLeft(befPat);  //quita espacios
  if befPat='' then exit;      //no hay elementos
  //Hay algo que agregar
  if nBef=3 then begin  //validación
    ErrStr := ERR_PAT_TOO_MAN_EL;  //no hay espacio
    exit;
  end;
  ShiftBeforePattern;        //hace espacio
  patEle := @elem[-1];  //fija puntero
  ExtractElementIn(befPat, patEle, ErrStr);
  //puede salir con mensaje de error en "ErrStr"
end;
procedure TFaOpenEvent.AddAfterElement(var aftPat: string; var ErrStr: string);
{Agrega un elemento al patrón siguiente. Si encuentra error devuelve el mensaje en ErrStr}
var
  patEle: ^TFaPatternElement;
begin
  ErrStr := '';
  aftPat := TrimLeft(aftPat);  //quita espacios
  if aftPat='' then exit;      //no hay elementos
  //Hay algo que agregar
  if nAft=1 then begin  //validación
    ErrStr := ERR_PAT_TOO_MAN_EL;  //no hay espacio
    exit;
  end;
  inc(nAft);    //lleva la cuenta
  patEle := @elem[0];  //fija puntero
  ExtractElementIn(aftPat, patEle, ErrStr);
  //puede salir con mensaje de error en "ErrStr"
end;
procedure TFaOpenEvent.ShiftBeforePattern;
{Desplaza los elementos del patrón anterior, a la izquierda, dejando el de la derecha
libre para usarlo. No toca al patrón siguiente}
begin
  elem[-3] := elem[-2];
  elem[-2] := elem[-1];
  elem[-1].patKind := pak_none;
  Inc(nBef);  //actualiza elementos anteriores válidos
end;
procedure TFaOpenEvent.AddAvail(txt: string);
{Agrega un ítem a la lista Avails[]}
var
  it: TFaCompletItem;
begin
  it := TFaCompletItem.Create;
  it.Caption:=txt;
  it.Replac:=txt;
  it.idxIcon:=-1;
  Avails.Add(it);
end;
procedure TFaOpenEvent.Clear;
begin
  ClearAvails;
  ClearItems;
end;
//manejo de ítems
procedure TFaOpenEvent.FilterByChar(curEnv: TFaCursorEnviron; const c: char);
{Filtra la lista Items[], usando un caracter. Se define como público, para poder usarla
como utilidad, si es que se necesita.}
var
  it: TFaCompletItem;
  lst: TFaCompletionList;
  cu: Char;
begin
  Avails.Clear;
  if curEnv.caseSen then begin
    for it in Items do begin
      if it.StartWith(c) then Avails.Add(it);  //copia las referencias
    end;
    //copia ítens de las listas
    for lst in Lists do begin
      for it in lst.Items do begin
        if it.StartWith(c) then Avails.Add(it);  //copia las referencias
      end;
    end;
  end else begin
    cu := UpCase(c);
    for it in Items do begin
      if it.StartWithU(cu) then Avails.Add(it);  //copia las referencias
    end;
    //copia ítens de las listas
    for lst in Lists do begin
      for it in lst.Items do begin
        if it.StartWithU(cu) then Avails.Add(it);  //copia las referencias
      end;
    end;
  end;
end;
procedure TFaOpenEvent.LoadItems(curEnv: TFaCursorEnviron);
{Carga todos los ítems con los que se va a trabajar en Avails[]. Los que se usarán para
 posteriormente filtrarse y cargarse al menú de completado.}
var
  it: TFaCompletItem;
  lst: TFaCompletionList;
  Cancel: boolean;
begin
  if OnLoadItems<>nil then begin
    //Hay evento configruado para llenar dinámciamente los ítems
    OnLoadItems(self, curEnv, Cancel);
    {$IFDEF Verbose}
    debugln(' LLenado dinámico de ítems con %d elem.', [items.Count]);
    {$ENDIF}
    if Cancel then exit;
  end;
  case filter of
  fil_None: begin  //no hay filtro
    Avails.Assign(Items);  //copia todas las referencias
    //Agrega, también las referencias de las listas que pueda contener.
    for lst in Lists do begin
      for it in lst.Items do begin
        Avails.Add(it);  //copia las referencias
      end;
    end;
    startX := curEnv.CurX;   //como no hay palabra de trabajo
  end;
  fil_LastTok,
  fil_LastTokPart: begin    //se usará el último token
    if curEnv.HaveLastTok then begin
      //hay un token anterior
      startX := curEnv.tok_1^.posIni;   //inicio del token
      FilterByChar(curEnv, curEnv.LastTok[1]);   //primer caracter como filtro (peor caso)
    end;
  end;
  fil_LastIdent,
  fil_LastIdentPart: begin    //se usará el último identif.
    if curEnv.HaveLastIdent then begin
      //hay un token anterior
      startX := curEnv.StartIdentif;   //es fácil sacar el inicio del identificador
      FilterByChar(curEnv, curEnv.LastIdentPart[1]);  //primer caracter como filtro (peor caso)
    end;
  end;
  else   //no debería pasar
    Avails.Clear;
    startX := curEnv.CurX;
  end;
  {$IFDEF Verbose}
  debugln('  Cargados en Avail: '+IntToStr(Avails.Count)+ ' ítems.')
  {$ENDIF}
end;
function TFaOpenEvent.MatchPatternElement(nPe: integer; tokX: TFaTokInfoPtr;
                CaseSens: boolean): boolean;
{Verifica el elemento de un patrón, coincide con un token de tokens[]. }
var
  pe: TFaPatternElement;
begin
  Result := false;  //por defecto
  pe := elem[nPe];  //no hay validación. Por velocidad, podría ser mejor un puntero.
  if tokX = nil then exit(false); //no existe este token
  case pe.patKind of
  pak_none:           //*** No definido.
    exit(true);  //No debería llegar aquí.
  pak_String: begin   //*** Es una cadena
    if CaseSens then begin //comparación con caja
      if tokX^.txt = pe.str then exit(true)
      else exit(false);
    end else begin    //comparación sin caja
      if UpCase(tokX^.txt) = UpCase(pe.str) then exit(true)
      else exit(false);
    end;
  end;
  pak_Identif: begin  //*** Es identificador
    Result := tokX^.IsIDentif;
  end;
  pak_NoIdentif: begin
    Result := not tokX^.IsIDentif;
  end;
  pak_TokTyp: begin   //*** Es un tipo específico de token
    Result := pe.toktyp = tokX^.TokTyp;
  end;
  pak_NoTokTyp: begin   //*** Es un tipo específico de token
    Result := not (pe.toktyp = tokX^.TokTyp);
  end;
  end;
end;
function TFaOpenEvent.MatchPatternBefore(const curEnv: TFaCursorEnviron
  ): boolean;
{Verifica si el patrón indicado, cumple con las condiciones actuales (before)}
begin
  Result := false;  //por defecto
  case nBef of
  0: begin  //no hay elementos, siempre cumple                  |
    exit(true);
  end;
  1: begin  //hay elem[-1]
     Result := MatchPatternElement(-1, curEnv.tok_1, curEnv.caseSen);
  end;
  2: begin  //hay elem[-2],elem[-1]
        Result := MatchPatternElement(-1, curEnv.tok_1, curEnv.caseSen) and
                  MatchPatternElement(-2, curEnv.tok_2, curEnv.caseSen);
  end;
  3: begin  //hay elem[-3],elem[-2],elem[-1]
        Result := MatchPatternElement(-1, curEnv.tok_1, curEnv.caseSen) and
                  MatchPatternElement(-2, curEnv.tok_2, curEnv.caseSen) and
                  MatchPatternElement(-3, curEnv.tok_3, curEnv.caseSen);
  end;
  end;
end;
function TFaOpenEvent.MatchPatternAfter(const curEnv: TFaCursorEnviron
  ): boolean;
{Verifica si el patrón indicado, cumple con las condiciones actuales (after)}
begin
  Result := false;  //por defecto
  case nAft of
  0: begin  //no hay elementos, siempre cumple
    exit(true);
  end;
  1: begin  //hay elem[0]
      //es independiente de "inMidTok"
      Result := MatchPatternElement(0, curEnv.tok0, curEnv.caseSen);
    end;
  end;
end;
function TFaOpenEvent.MatchPattern(const curEnv: TFaCursorEnviron): boolean;
  function ItemInBlock: boolean;  inline;
  begin
    Result := (block = nil) or //es válido para todos los bloques
              (block = curEnv.curBlock);
  end;
begin
  Result := MatchPatternBefore(curEnv) and
            MatchPatternAfter(curEnv) and ItemInBlock;
  {$IFDEF Verbose}
  if Result then debugln(' -> Aplicable Pat: %s con %d elem.', [name, items.Count]);
  {$ENDIF}
end;
procedure TFaOpenEvent.AddItem(txt: string; idxIcon: integer);
{Agrega un ítem al evento de apertura. Versión simplificada}
var
  it: TFaCompletItem;
begin
  it := TFaCompletItem.Create;
  it.Caption := txt;
  it.idxIcon:=idxIcon;
  Items.Add(it);
end;
procedure TFaOpenEvent.AddItems(lst: TStringList; idxIcon: integer);
{Agrega una lista de ítems al evento de apertura, desde un  TStringList}
var
  it: TFaCompletItem;
  i: Integer;
begin
  for i:= 0 to lst.Count-1 do begin
    it := TFaCompletItem.Create;
    it.Caption := lst[i];
    it.idxIcon:=idxIcon;
    Items.Add(it);
  end;
end;
procedure TFaOpenEvent.AddItems(list: string; idxIcon: integer);
{Agrega una lista de ítems al evento de apertura, desde una cadena }
var
  lst: TStringList;
begin
  lst := TStringList.Create;
  //troza
  lst.Delimiter := ' ';
  lst.DelimitedText := list;
  //agrega
  AddItems(lst, idxIcon);
  lst.Destroy;
end;
procedure TFaOpenEvent.AddList(Alist: TFaCompletionList; OnlyRef: boolean);
{Agrega los datos de una lista o la referencia.}
begin
  if OnlyRef then begin
    Lists.Add(Alist);  //solo guarda la referencia
  end else begin
    //No implementado. Es problemático incluir referencias de objetos no administrados.
    //Lo mejor sería usar una lista adicional, sin administración de objetos.
//    for it in Alist.Items do begin
//      Items.Add(it);
//    end;
  end;
end;
procedure TFaOpenEvent.ClearAvails;
begin
  Avails.Clear;
end;
procedure TFaOpenEvent.ClearItems;
begin
  Items.Clear;
end;
constructor TFaOpenEvent.Create(hlt0: TSynFacilSyn);
begin
  Items:= TFaCompletItems.Create(true);   //Lista con administración
  Avails:= TFaCompletItems.Create(false);  //referecnias sin administración
  Lists := TFaCompletionLists.Create(false);
  hlt := hlt0;
end;
destructor TFaOpenEvent.Destroy;
begin
  Lists.Destroy;
  Avails.Destroy;
  Items.Destroy;
  inherited Destroy;
end;

{ TSynCompletionF }
function TSynCompletionF.OnSynCompletionPaintItem(const AKey: string;
  ACanvas: TCanvas; X, Y: integer; IsSelected: boolean; Index: integer): boolean;
var
//  MaxX: Integer;
//  hl: TSynCustomHighlighter;
  Capt: String;
  idIcon: Integer;
  obj: TObject;
begin
{
  ACanvas.Font.Style:=[];
  if not IsSelected then
    ACanvas.Font.Color := FActiveEditDefaultFGColor
  else
    ACanvas.Font.Color := FActiveEditSelectedFGColor;
  MaxX:=TheForm.ClientWidth;}
{  hl := nil;
  if Editor <> nil then
    hl := Editor.Highlighter;}
  Capt := ItemList[Index];
  if IconList<>nil then begin
    obj := ItemList.Objects[Index];
    if obj=nil then begin
      //Puede pasar cuando no se ha asignado un objeto, sino solo texto
      idIcon := -1
    end else begin
      idIcon := TFaCompletItem(obj).idxIcon;
    end;
    IconList.Draw(ACanvas, X+2, Y, idIcon);
    ACanvas.TextOut(X+20, Y, Capt);
  end else begin
    ACanvas.TextOut(X+2, Y, Capt);
    //  PaintCompletionItem(AKey, ACanvas, X, Y, MaxX, IsSelected, Index, self);
  end;

  Result:=true;  //para indicar que lo intercepta
end;
procedure TSynCompletionF.Refresh;
begin
  if ItemList.Count = 0 then begin
    //cierra por no tener elementos
    Deactivate;
  end else begin
    //hay elementos
    Position:=0;  //selecciona el primero
  end;
  TheForm.Invalidate;  //para que se actualice
end;
constructor TSynCompletionF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TheForm.ShowSizeDrag := True;
  //intercepta este evento
  OnPaintItem:=@OnSynCompletionPaintItem;
end;

{ TSynFacilComplet }
procedure TSynFacilComplet.ReadSpecialIdentif;
//Hace una exploración para leer todos los identificadores especiales en la tabla
//SpecIdentifiers[].
var
  met: TFaProcMetTable;
  p: TPtrATokEspec;
  c: Char;
  i: Integer;
  n: Integer;
begin
  setlength(SpecIdentifiers,0);
  if CaseSensitive then begin
    for c in ['A'..'Z','a'..'z'] do begin
      TableIdent(c, p, met);
      if p<> nil then begin
        for i:= 0 to high(p^) do begin
          n := high(SpecIdentifiers)+1;  //tamaño de matriz
          setlength(SpecIdentifiers,n+1);
          SpecIdentifiers[n] := p^[i];
        end;
      end;
    end;
  end else begin  //no es sensible a la caja
    for c in ['A'..'Z'] do begin
      TableIdent(c, p, met);
      if p<> nil then begin
        for i:= 0 to high(p^) do begin
          n := high(SpecIdentifiers)+1;  //tamaño de matriz
          setlength(SpecIdentifiers,n+1);
          SpecIdentifiers[n] := p^[i];
        end;
      end;
    end;
  end;
end;
function TSynFacilComplet.LoadSyntaxFromPath(SourceFile: string; path: string;
              CaseSens: boolean = false): string;
//Carga un archivo de sintaxis, buscando el resaltador apropiado en un directorio.
//Si encuentra el archivo de sintaxis apropiado, devuelve el nombre del archivo usado
//(sin incluir la ruta), de otra forma, devuelve una cadena vacía.
var
  ext: String;
  Hay: Boolean;
  SR : TSearchRec;
  rut: String;
begin
  Result := '';
  ext := ExtractFileExt(SourceFile);
  if ext<>'' then ext := copy(ext, 2, 10);  //quita el punto
  //explora los lenguajes para encontrar alguno que soporte la extensión dada
  Hay := FindFirst(path + '\*.xml',faAnyFile - faDirectory, SR) = 0;
  while Hay do begin
     //encontró archivo, lee sus extensiones
     rut := path + '\' + SR.name;
     if XMLFileHaveExten(rut, ext, CaseSens) then  begin //encontró
       LoadFromFile(rut);  //carga sintaxis
       Result := SR.name;
       exit;
     end;
     //no encontró extensión, busca siguiente archivo
     Hay := FindNext(SR) = 0;
  end;
  //no encontró
end;
procedure TSynFacilComplet.SetIconList(AValue: TImageList);
begin
//  if FIconList=AValue then Exit;
  MenuComplet.IconList := AValue;
end;
function TSynFacilComplet.FindOpenEvent(oeName: string): TFaOpenEvent;
var
  eve: TFaOpenEvent;
begin
  for eve in OpenEvents do begin
    if eve.name = oeName then exit(eve);
  end;
  exit(nil);
end;
procedure TSynFacilComplet.ProcXMLOpenOn(nodo: TDOMNode);
{Procesa el bloque <OpenOn ... >}
  procedure GetItemsFromNode(nodo: TDOMNode; opEve: TFaOpenEvent; idxIcon: integer);
  var
    listIden: DOMString;
    i,j : Integer;
    nodo2: TDOMNode;
    lst: TFaCompletionList;
    tIncAttr, tIncList: TFaXMLatrib;
    tipTok: integer;
    tIncIcnI: TFaXMLatrib;
    IncIcnI: Integer;
  begin
    listIden := nodo.TextContent;
    if listIden<>'' then begin
      //Se ha especificado lista de palabras. Los carga
      opEve.AddItems(AnsiString(listIden), idxIcon);
    end;
    //explora nodos
    for i := 0 to nodo.ChildNodes.Count-1 do begin
      nodo2 := nodo.ChildNodes[i];
      if UpCAse(nodo2.NodeName)='INCLUDE' then begin  //incluye lista de palabras por atributo
        //lee parámetros
        tIncAttr := ReadXMLParam(nodo2,'Attribute');
        tIncList := ReadXMLParam(nodo2,'List');
        tIncIcnI := ReadXMLParam(nodo2,'IconIndex');
        CheckXMLParams(nodo2, 'Attribute List IconIndex');  //puede generar excepción
        if tIncAttr.hay then begin
          //se pide agregar la lista de identificadores de un atributo en especial
          if IsAttributeName(tIncAttr.val)  then begin
            tipTok := GetAttribIDByName(tIncAttr.val);   //tipo de atributo
            if tIncIcnI.hay then IncIcnI := tIncIcnI.n else IncIcnI:=-1;
            //busca los identificadores para agregarlos
            for j:= 0 to high(SpecIdentifiers) do begin
              if SpecIdentifiers[j].tTok = tipTok then begin
                opEve.AddItem(SpecIdentifiers[j].orig, IncIcnI); {Agrega a lista por defecto.}
              end;
            end;
          end else begin  //atributo no existe
            raise ESynFacilSyn.Create(Format(ERR_ATTRIB_NO_EXIST,[nodo2.NodeValue]));
          end;
        end;
        if tIncList.hay then begin
          //se pide agregar los ítems de una lista
          lst := GetListByName(tIncList.val);
          if lst<>nil then begin
            opEve.AddList(lst, true);
          end else begin
            raise ESynFacilSyn.Create(Format(ERR_LIST_NO_EXIST,[tIncList.val]));
          end;
        end;
      end else if nodo2.NodeName='#text' then begin
        //éste nodo aparece siempre que haya espacios, saltos o tabulaciones
      end else if LowerCase(nodo2.NodeName) = '#comment' then begin
        //solo para evitar que de mensaje de error
      end else begin
        raise ESynFacilSyn.Create(Format(ERR_INVAL_LAB_OPNON,[nodo2.NodeName]));
      end;
    end;
  end;
var
  tNamPatt, tBefPatt, tAftPatt: TFaXMLatrib;
  filt: TFaFilterList;
  tFilPatt, tBlkPatt: TFaXMLatrib;
  opEve: TFaOpenEvent;
  blk: TFaSynBlock;
  success: boolean;
  tActPatt: TFaXMLatrib;
  tIcnPatt: TFaXMLatrib;
  idxIcon: Integer;
begin
  tNamPatt := ReadXMLParam(nodo,'Name');
  tBefPatt := ReadXMLParam(nodo,'BeforePattern');
  tAftPatt := ReadXMLParam(nodo,'AfterPattern');
  tFilPatt := ReadXMLParam(nodo,'FilterBy');
  tBlkPatt := ReadXMLParam(nodo,'Block');
  tActPatt := ReadXMLParam(nodo,'Action');
  tIcnPatt := ReadXMLParam(nodo,'IconIndex');
  CheckXMLParams(nodo, 'Name BeforePattern AfterPattern FilterBy Block Action IconIndex');  //puede generar excepción
  if tFilPatt.hay then begin
    case UpCase(tFilPatt.val) of
    'NONE'         : filt := fil_None;
    'LASTTOK'      : filt := fil_LastTok;
    'LASTTOKPART'  : filt := fil_LastTokPart;
    'LASTIDENT'    : filt := fil_LastIdent;
    'LASTIDENTPART': filt := fil_LastIdentPart;
    else
      raise ESynFacilSyn.Create(Format(ERR_FILTER_NO_EXIST,[tFilPatt.val]));
    end;
  end else begin
    filt := fil_LastTokPart;   //valro por defecto
  end;
  if not tAftPatt.hay then begin
    tAftPatt.val:='Identifier';
  end;
  //agrega patrón
  opEve := AddOpenEvent(tAftPatt.val, tBefPatt.val, filt);
  //configrua nombre
  if tNamPatt.hay then begin  //se especificó nombre
    opEve.name := tNamPatt.val;
  end else begin  //se asuem un ombre por defecto
    opEve.name := '#Pat' + IntToStr(OpenEvents.Count);
  end;
  //configura bloque
  if tBlkPatt.hay then begin
    blk := SearchBlock(tBlkPatt.val, success);
    if not success then begin
      raise ESynFacilSyn.Create(ERR_INVAL_BLK_NAME);
    end;
    opEve.block := blk;
  end else begin
    opEve.block := nil;
  end;
  //configura acción
  if tActPatt.hay then begin
    case UpCAse(tActPatt.val) of
    'NONE'   : opEve.Action := pac_None;
    'DEFAULT': opEve.Action := pac_Default;
    'INSERT' : opEve.Action := pac_Insert;
    'REPLASTTOK': opEve.Action := pac_Rep_LastTok;
    else
      raise ESynFacilSyn.Create(Format(ERR_ACTION_NO_EXIST,[tActPatt.val]));
    end;
  end else begin
    opEve.Action := pac_Default;
  end;
  //configura ícono
  if tIcnPatt.hay then idxIcon := tIcnPatt.n else idxIcon:=-1;
  //verifica contenido
  GetItemsFromNode(nodo, opEve, idxIcon);
end;
procedure TSynFacilComplet.ProcCompletionLabel(nodo: TDOMNode);
//Procesa la etiqueta <Completion>, que es el bloque que define todo el sistema de
//completado de código.
var
  listIden: string;
  i,j     : Integer;
  nodo2   : TDOMNode;
  tipTok  : integer;
  hayOpen : Boolean;
  tIncAttr: TFaXMLatrib;
  tLstName, tLstIcnI: TFaXMLatrib;
  defPat  : TFaOpenEvent;
  cmpList : TFaCompletionList;
  idxIcon : integer;
  tIncIcnI: TFaXMLatrib;
  IncIcnI : Integer;
begin
  hayOpen := false;  //inicia bandera
  //crea evento de apertura por defecto
  defPat := AddOpenEvent('Identifier', '', fil_LastTokPart);
  defpat.name:='#Def';
  ////////// explora nodos hijos //////////
  for i := 0 to nodo.ChildNodes.Count-1 do begin
    nodo2 := nodo.ChildNodes[i];
    if UpCAse(nodo2.NodeName)='INCLUDE' then begin  //incluye lista de palabras por atributo
      //lee parámetros
      tIncAttr := ReadXMLParam(nodo2,'Attribute');
      tIncIcnI := ReadXMLParam(nodo2,'IconIndex');
      CheckXMLParams(nodo2, 'Attribute IconIndex');  //puede generar excepción
      if tIncAttr.hay then begin
        //se pide agregar la lista de identificadores de un atributo en especial
        if IsAttributeName(tIncAttr.val)  then begin
          tipTok := GetAttribIDByName(tIncAttr.val);   //tipo de atributo
          if tIncIcnI.hay then IncIcnI := tIncIcnI.n else IncIcnI:=-1;
          //busca los identificadores para agregarlos
          for j:= 0 to high(SpecIdentifiers) do begin
            if SpecIdentifiers[j].tTok = tipTok then begin
              defPat.AddItem(SpecIdentifiers[j].orig, IncIcnI); {Agrega a lista por defecto.}
            end;
          end;
        end else begin  //atributo no existe
          raise ESynFacilSyn.Create(Format(ERR_ATTRIB_NO_EXIST,[nodo2.NodeValue]));
        end;
      end;
    end else if UpCAse(nodo2.NodeName)='OPENON' then begin  //evento de apertura
      //lee parámetros
      hayOpen :=true;   //marca para indicar que hay lista
      ProcXMLOpenOn(nodo2);  //puede generar excepción.
    end else if UpCAse(nodo2.NodeName)='LIST' then begin  //forma alternativa para lista de palabras
      //Esta forma de declaración permite definir un orden en la carga de listas
      //lee parámetros
      tLstName := ReadXMLParam(nodo2,'Name');
      tLstIcnI := ReadXMLParam(nodo2,'IconIndex');
      CheckXMLParams(nodo2, 'Name IconIndex');  //puede generar excepción
      if not tLstName.hay then begin
        tLstName.val:='#list'+IntToStr(CompletLists.Count);
      end;
      cmpList := AddComplList(tLstName.val);
      //Ve si tiene contenido
      listIden := AnsiString(nodo2.TextContent);
      if listIden<>'' then begin
        if tLstIcnI.hay then idxIcon := tLstIcnI.n else idxIcon := -1;
        cmpList.AddItems(listIden, idxIcon);
        {Agrega los ítems de la lista en este patrón, por si se llegase a utilizar. Se
        hace aquí mismo para mantener el orden, si es que se mezcla con etiquetas <INCLUDE>
        o listas de palabras indicadas directamente en <COMPLETION> ... </COMPLETION>}
        defPat.AddItems(listIden, idxIcon);
      end;
    end else if nodo2.NodeName='#text' then begin
      //Este nodo aparece siempre que haya espacios, saltos o tabulaciones
      //Puede ser la lista de palabras incluidas directamente en <COMPLETION> </COMPLETION>
      defPat.AddItems(AnsiString(nodo2.NodeValue), -1);
    end else if LowerCase(nodo2.NodeName) = '#comment' then begin
      //solo para evitar que de mensaje de error
    end else begin
      raise ESynFacilSyn.Create(Format(ERR_INVAL_LAB_COMP,[nodo2.NodeName]));
    end;
  end;
  //verifica las opciones por defecto
  if hayOpen then begin
    //Se ha especificado patrones de apretura.
    OpenEvents.Remove(defPat);  //elimina el evento por defecto, porque no se va a usar
  end else begin
    //No se ha especificado ningún evento de apertura
    //mantiene el evento por defecto
  end;
end;
procedure TSynFacilComplet.LoadFromFile(const Filename: string);
var
  doc: TXMLDocument;
  i: Integer;
  nodo: TDOMNode;
  nombre: WideString;
  tCasSen: TFaXMLatrib;
  tOpenKUp: TFaXMLatrib;
  tSelOEnt: TFaXMLatrib;
begin
  inherited LoadFromFile(Filename);  {Puede disparar excepción. El mesnajes de error generado
                                incluye el nombre del archivo}
  OpenOnKeyUp := true;     //por defecto
  ReadSpecialIdentif;      //carga los identificadores especiales
  OpenEvents.Clear;      //limpia patrones de apertura
  CompletLists.Clear;
  try
    ReadXMLFile(doc, Filename);  //carga archivo
    //procede a la carga de la etiqueta <COMPLETION>
    for i:= 0 to doc.DocumentElement.ChildNodes.Count - 1 do begin
       // Lee un Nodo o Registro
       nodo := doc.DocumentElement.ChildNodes[i];
       nombre := UpCase(nodo.NodeName);
       if nombre = 'COMPLETION' then  begin
         //carga los parámetros
         tCasSen :=ReadXMLParam(nodo, 'CaseSensitive');
         tOpenKUp:=ReadXMLParam(nodo, 'OpenOnKeyUp');
         tSelOEnt:=ReadXMLParam(nodo, 'SelectOnEnter');
         //carga atributos leidos
         if tCasSen.hay then  //si se especifica
           CaseSensComp := tCasSen.bol  //se lee
         else  //si no
           CaseSensComp := CaseSensitive;  //toma el del resaltador
         if tOpenKUp.hay then OpenOnKeyUp:=tOpenKUp.bol;
         if tSelOEnt.hay then SelectOnEnter:=tSelOEnt.bol;
         ProcCompletionLabel(nodo);  //Puede generar error
       end;
    end;
    doc.Free;  //libera
  except
    on e: Exception do begin
      //Completa el mensaje con nombre de archivo, porque esta parte del código
      //no lo incluye.
      e.Message:=ERROR_LOADING_ + Filename + #13#10 + e.Message;
      doc.Free;
      raise   //genera de nuevo
    end;
  end;
end;
procedure TSynFacilComplet.SelectEditor(ed0: TSynEdit);
//Inicia el motor de ayuda contextual, en el editor indicado
begin
  ed := ed0;    //guarda referencia
  if ed = nil then begin
    showmessage('ERROR: Se requiere un editor para el autocompletado.');
    ed := nil;   //para indicar que no es válido
    exit;
  end;
  //asigna por si acaso no se había hecho
  ed.Highlighter :=  self;
  MenuComplet:=TSynCompletionF.Create(ed.Owner);   //crea menú contextual en el formulario
  MenuComplet.Editor:=ed;     //asigna editor
  MenuComplet.Width:=200;     //ancho inicial
  MenuComplet.OnExecute:=@MenuComplet_OnExecute;
  MenuComplet.OnCodeCompletion:=@OnCodeCompletion;
  //Para evitar que reemplace automáticamente, cuando se abre con un solo elemento en la lista
  MenuComplet.AutoUseSingleIdent := false;

  //iintercepta eventos de teclado, para cambiar comportamiento
  MenuComplet.OnKeyDown:=@FormKeyDown;
  MenuComplet.OnUTF8KeyPress:=@FormUTF8KeyPress;  //eventos del teclado de la ventana de completado
end;
procedure TSynFacilComplet.UnSelectEditor;
//Método que quita la ayuda contextual al formulario indicado y al editor.
//Se debería llamar siempre si se ha llamado a SelectEditor().
begin
  if MenuComplet = nil then exit;  //nunca se creó
  MenuComplet.Destroy;
  MenuComplet := nil;  //lo marca como  liberado
end;
function TSynFacilComplet.CheckForClose: boolean;
{Verifica si se dan las condiciones como para cerrar la ventana de completado, y si se da
el caso, la cierra y devuelve TRUE.}
var
  opEve: TFaOpenEvent;
begin
  curEnv.LookAround(ed, CaseSensComp);  //para actualizar el nuevo estado
  opEve := FindOpenEventMatching;    //Busca evento de apertura que aplica
  if opEve = nil then begin
    MenuComplet.Deactivate;
    CurOpenEve := nil;  //como se cierra ya no hay evento activo
    exit(true);  //ninguno aplica
  end;
  //hay un aptrón que aplica
  if opEve=CurOpenEve then begin
    //Es el mismo
    exit(false);
  end else begin
    //Es uno nuevo
    {Aquí se puede decidir cerrar la ventana, porque aplica otro patrón, pero yo prefiero
     el comportamiento en el que se aplica direcatamente el nuevo patrón, sin necesidad de
     esperar a pulsar otra tecla.}
    CurOpenEve := opEve;   //actualiza referencia
    CurOpenEve.LoadItems(curEnv);  //carga ítems
    exit(false);
  end;
end;

function TSynFacilComplet.AddOpenEvent(AfterPattern, BeforePattern: string;
  filter: TFaFilterList): TFaOpenEvent;
{Permite agregar un evento de apertura. Devuelve una referencia al evento agregado.}
var
  opEve: TFaOpenEvent;
  errStr: string;
begin
  opEve := TFaOpenEvent.Create(self);
  ///////analiza AfterPattern
  opEve.ClearBeforePatt;
  while AfterPattern<>'' do begin
    opEve.AddBeforeElement(AfterPattern, errStr);
    if errStr<>'' then begin
      opEve.Destroy;  //destruye proque no lo va a agregar
      raise ESynFacilSyn.Create(ERR_PAR_AFT_PATT + ':' + errStr);
    end;
  end;
  ///////analiza BeforePattern
  opEve.ClearAfterPatt;
  while BeforePattern<>'' do begin
    opEve.AddAfterElement(BeforePattern, errStr);
    if errStr<>'' then begin
      opEve.Destroy;  //destruye proque no lo va a agregar
      raise ESynFacilSyn.Create(ERR_PAR_BEF_PATT + ':' + errStr);
    end;
  end;
  //Hay propiedades que no se inician aquí como el nombre
  opEve.filter := filter;     //fija filtro
  opEve.block := nil;
  opEve.Action := pac_Default;  //acción por defecto
  OpenEvents.Add(opEve);   //agrega
  Result := opEve;  //devuelve referencia
end;
function TSynFacilComplet.AddComplList(lstName: string): TFaCompletionList;
var
  lst: TFaCompletionList;
begin
  lst := TFaCompletionList.Create;
  lst.Name:= lstName;
  CompletLists.Add(lst);
  Result := lst;
end;
function TSynFacilComplet.GetListByName(lstName: string): TFaCompletionList;
{Devuelve la referencia a una lista, usando su nombre. Si no enecuentra devuelve NIL}
var
  l: TFaCompletionList;
  UlstName: String;
begin
  UlstName := upcase(lstName);
  for l in CompletLists do begin
    if Upcase(l.Name) = UlstName then
      exit(l);
  end;
  exit(nil);
end;
procedure TSynFacilComplet.OnCodeCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
//Se genera antes de hacer el reemplazo del texto. "Value", es la cadena que se usará
//para reemplazar la palabra en el editor.
begin
  //Se puede usar "MenuComplet.Position" para saber el elemento seleccionado de la lista
//  value := 'hola';
//  showmessage(value);
end;
function TSynFacilComplet.FindOpenEventMatching: TFaOpenEvent;
{Devuelve el priumer evento que coincide con el entorno actual "curEnv". Si ninguno coincide,
devuelve NIL.}
var
  opEve: TFaOpenEvent;
begin
  for opEve in OpenEvents do begin
    if opEve.MatchPattern(curEnv) then begin
      Result := opEve;   //devuelve referencia
      exit;
    end;
  end;
  exit(nil);   //no enccontró
end;
function TSynFacilComplet.GetIconList: TImageList;
begin
  Exit(MenuComplet.IconList);
end;
procedure TSynFacilComplet.MenuComplet_OnExecute(Sender: TObject);
{Este evento se genera antes de abrir el menú de completado.
Se puede abrir al pulsar una tecla común. La otra opción es por un atajo.
Llena la lista "AvailItems", con los ítems que correspondan de acuerdo a la posición
actual del cursor y de la configuración del archivo XML.
Luego llena el menú contextual con los ítems filtrados de acuerdo a la posición actual.}
var
  opEve: TFaOpenEvent;
begin
  MenuComplet.ItemList.Clear;   //inicia menú
  //Prepara para llenar la lista de completado
  curEnv.LookAround(ed, CaseSensComp);  //Lee entorno.
  CurOpenEve := nil;
  opEve := FindOpenEventMatching;    //Busca evento de apertura que aplica
  if opEve<>nil then begin
    //Se cumple el evento en la posición actual del cursor
    opEve.LoadItems(curEnv);  //carga los ítems con los que trabajará.
    CurOpenEve := opEve;    //guarda referencia
  end;
  //Llena el menú de completado
  FillCompletMenuFiltered;  //hace el primer filtrado
  {$IFDEF Verbose}
  debugln('Llenado con %d', [MenuComplet.ItemList.Count]);
  {$ENDIF}
end;
procedure TSynFacilComplet.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  procedure SeleccionarPalabra;
  {Selecciona, la palabra actual de la lista de completado. Usamos nuestra propia
  rutina en vez de OnValidate(), para poder reemplazar identificadores de acuerdo
  a la definición de sintaxis, además de otros tipos de tokens.}
  var
    NewWord: String;
    obj: TObject;
  begin
    if CurrentLines = nil then exit;
    //Reemplaza actual
    obj := MenuComplet.ItemList.Objects[MenuComplet.Position];
    if obj=nil then begin
      //Puede pasar cuando no se ha asignado un objeto, sino solo texto
    end else begin
      NewWord := TFaCompletItem(obj).Replac;
      CurOpenEve.DoAction(ed, curEnv, NewWord);   //realiza la acción programada
    end;
    CloseCompletionWindow;  //cierra
  end;
begin
//debugln('   Form.OnKeyDown Key='+ IntToStr(Key) +':'+IntToStr(ed.CaretX));
  case Key of
    VK_RETURN: begin
        if Shift= [] then begin
           if SelectOnEnter then  //solo si está permitido reemplazar
             SeleccionarPalabra;
           Key:=VK_UNKNOWN;   //marca para que no lo procese SynCompletion
        end else begin
          Key:=VK_UNKNOWN;   //marca para que no lo procese SynCompletion
          CloseCompletionWindow;  //cierra
        end;
      end;
    VK_HOME: begin
        if Shift = [] then begin  //envía al editor
           ed.CommandProcessor(ecLineStart, #0, nil);
           MenuComplet.Deactivate;  //desactiva
        end else if Shift = [ssShift] then begin
          ed.CommandProcessor(ecSelLineStart, #0, nil);
          MenuComplet.Deactivate;  //desactiva
        end;
        Key:=VK_UNKNOWN;   //marca para que no lo procese SynCompletion
      end;
    VK_END: begin
        if Shift = [] then begin  //envía al editor
           ed.CommandProcessor(ecLineEnd, #0, nil);
           MenuComplet.Deactivate;  //desactiva
        end else if Shift = [ssShift] then begin
          ed.CommandProcessor(ecSelLineEnd, #0, nil);
          MenuComplet.Deactivate;  //desactiva
        end;
        Key:=VK_UNKNOWN;   //marca para que no lo procese SynCompletion
      end;
    VK_BACK: begin
        if Shift = [] then begin  //sin Ctrl o Shift
          ed.CommandProcessor(ecDeleteLastChar, #0, nil);  //envía al editor
          if CheckForClose then begin Key:=VK_UNKNOWN; exit end;
          FillCompletMenuFiltered;
        end;
        Key:=VK_UNKNOWN;   //marca para que no lo procese SynCompletion
      end;
    VK_DELETE: begin
        if Shift = [] then begin  //sin Ctrl o Shift
          ed.CommandProcessor(ecDeleteChar, #0, nil);  //envía al editor
          if CheckForClose then begin Key:=VK_UNKNOWN; exit end;
          FillCompletMenuFiltered;
        end;
        Key:=VK_UNKNOWN;   //marca para que no lo procese SynCompletion
      end;
    VK_LEFT: begin
        if Shift = [] then begin  //envía al editor
          ed.CommandProcessor(ecLeft, #0, nil);
          if CheckForClose then begin Key:=VK_UNKNOWN; exit end;
          FillCompletMenuFiltered;
        end else if Shift = [ssShift] then begin
          ed.CommandProcessor(ecSelLeft, #0, nil);
          MenuComplet.Deactivate;  //desactiva
        end else if Shift = [ssCtrl] then begin
          ed.CommandProcessor(ecWordLeft, #0, nil);
          MenuComplet.Deactivate;  //desactiva
        end else if Shift = [ssShift,ssCtrl] then begin
          ed.CommandProcessor(ecSelWordLeft, #0, nil);
          MenuComplet.Deactivate;  //desactiva
        end;
        Key:=VK_UNKNOWN;   //marca para que no lo procese SynCompletion
      end;
    VK_RIGHT: begin
        if Shift = [] then begin  //envía al editor
          ed.CommandProcessor(ecRight, #0, nil);
          if CheckForClose then begin Key:=VK_UNKNOWN; exit end;
          FillCompletMenuFiltered;
        end else if Shift = [ssShift] then begin
          ed.CommandProcessor(ecSelRight, #0, nil);
          MenuComplet.Deactivate;  //desactiva
        end else if Shift = [ssCtrl] then begin
          ed.CommandProcessor(ecWordRight, #0, nil);
          MenuComplet.Deactivate;  //desactiva
        end else if Shift = [ssShift,ssCtrl] then begin
          ed.CommandProcessor(ecSelWordRight, #0, nil);
          MenuComplet.Deactivate;  //desactiva
        end;
        Key:=VK_UNKNOWN;   //marca para que no lo procese SynCompletion
      end;
    VK_TAB: begin
        if Shift = [] then begin
          SeleccionarPalabra;
          SearchOnKeyUp := false;  {para que no intente buscar luego en el evento KeyUp,
                   porque TAB está configurado como tecla válida para abrir la lista, y si
                   se abre, (y no se ha isertado el TAB), aparecerá de nuevo el mismo
                   identificador en la lista}
          Key:=VK_UNKNOWN;   //marca para que no lo procese SynCompletion
        end;
      end;
  end;
  //si no lo procesó aquí, lo procesará SynCompletion
end;
procedure TSynFacilComplet.FormUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
//Este evento se dispara cuando ya está visible la ventana de autocompletado y por
//lo tanto no se generará el evento KeyUp
begin
  //Como este evento se genera apneas pulsar una tecla, primero pasamos la tecla al
  //editor para que lo procese y así tendremos el texto modificado, como si estuviéramos
  //después de un KeyUp().
  ed.CommandProcessor(ecChar, UTF8Key, nil);
  UTF8Key := '';  //limpiamos para que ya no lo procese SynCompletion
  //ahora ya tenemos al editor cambiado
  //Las posibles opciones ya se deben haber llenado. Aquí solo filtramos.
  if CheckForClose then exit;
  FillCompletMenuFiltered;
end;
procedure TSynFacilComplet.UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
{Debe recibir la tecla pulsada aquí, y guardarla para KeyUp, porque allí no se puede
reconocer caracteres ASCII. Se usa UTF para hacerlo más fléxible}
begin
  UtfKey:=UTF8Key;  //guarda tecla
end;
procedure TSynFacilComplet.KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{Verifica la tecla pulsada, para determinar si abrir o no el menú de ayuda contextual
 Debe llamarse después de que el editor ha procesado el evento, para tener
 el estado final del editor
 Este evento solo se ejecutará una vez antes de abrir la ventana de autocompletado}
begin
  if not CompletionOn then exit;
  if not OpenOnKeyUp then exit;
  if not SearchOnKeyUp then begin
    //Se ha inhabilitado este evento para el completado
    SearchOnKeyUp := true;
    exit;
  end;
  //verificación principal
  if MenuComplet.IsActive then begin
    //verifica si debe desaparecer la ventana, por mover el cursor a una posición anterior
//    if CompareCarets(Pos0, CurrentEditor.CaretXY) < 0 then
//      Deactivate;
    exit;   //ya está mostrado
  end;
  if ed = NIL then exit;     //no hay editor
  if ed.SelectionMode <> smNormal then exit;  //para no interferir en modo columna
  {Llama a OpenCompletionWindow(), con información del teclado, para que evalúe si
  corresponde abrir la ventana de completado. De ser así, llamará a MenuComplet_OnExecute.}
  OpenCompletionWindow(Key, Shift, UtfKey);  //solo se mostrará si hay ítems
  UtfKey := '';  //limpia por si la siguiente tecla pulsada no dispara a UTF8KeyPress()
  SearchOnKeyUp := true;  //limpia bandera
End;
procedure TSynFacilComplet.FillCompletMenuFiltered;
{Llena el menú de completado a partir de "AvailItems", aplicando el filtro de "CurOpenEve"
}
begin
  //Genera la lista que coincide
  { Este proceso puede ser lento si se actualizan muchas opciones en la lista }
  MenuComplet.ItemList.Clear;  {Limpia todo aquí porque este método es llamado desde distintos
                                puntos del programa.}
  if CurOpenEve <> nil then begin
    CurOpenEve.FillFilteredIn(curEnv, MenuComplet.ItemList);
  end;
  MenuComplet.Refresh;
end;
procedure TSynFacilComplet.OpenCompletionWindow(vKey: word; vShift: TShiftState;
                                                vUtfKey: TUTF8Char);
//Abre la ayuda contextual, en la posición del cursor.
var
  p:TPoint;
begin
  //Verifica si se va a abrir la lista por tecla común. La otra opción es por un atajo
  if (vKey in [VK_BACK, VK_TAB] ) and (vShift=[]) then begin
    //Esta tecla es válida
    {$IFDEF Verbose}
    debugln('--Tecla válida para abrir menú: %d', [vKey]);
    {$ENDIF}
  end else if (vUtfKey<>'') and (vUtfKey[1] in [#8,#9,' '..'@','A'..'z']) then begin
    //Esta tecla es válida
    {$IFDEF Verbose}
    debugln('--Tecla válida para abrir menú: %d', [vKey]);
    {$ENDIF}
  end else begin
    //Los otros casos no se consideran que deban explorarse
    {$IFDEF Verbose}
    debugln('--Tecla no válida para abrir menú: %d', [vKey]);
    {$ENDIF}
    exit;
  end;
  //Calcula posición donde aparecerá el menú de completado
  p := Point(ed.CaretXPix,ed.CaretYPix + ed.LineHeight);
  p.X:=Max(0,Min(p.X, ed.ClientWidth - MenuComplet.Width));
  p := ed.ClientToScreen(p);
  //Abre menú contextual, llamando primero a OnExecute(). Solo se mostrará si tiene elementos.
  MenuComplet.Execute('', p.x, p.y);   //pasa una clave cualquiera para identificación posterior
end;
procedure TSynFacilComplet.CloseCompletionWindow;
//Cierra la ventana del menú contextual
begin
  MenuComplet.Deactivate;
end;
constructor TSynFacilComplet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  curEnv   := TFaCursorEnviron.Create(self);
  OpenEvents := TFaOpenEvents.Create(True);
  CompletLists := TFaCompletionLists.Create(true);
  CaseSensComp := false;  //por defecto
  CompletionOn := true;  //activo por defecto
  SelectOnEnter := true;
  UtfKey := '';   //limpia
  SearchOnKeyUp := true;  //Para iniciar la búsqueda
end;
destructor TSynFacilComplet.Destroy;
begin
  if MenuComplet<>nil then MenuComplet.Destroy;  //por si no lo liberaron
  CompletLists.Destroy;
  OpenEvents.Destroy;
  curEnv.Destroy;
  inherited Destroy;
end;

end.
