{
Unidad similar a SynFacilUtil.pas de la librería SynFacilUtils.
Se crea una copia modificada para adaptarla a los requerimientos de un editor en un
control con múltiples pestañas.
Se han quitado funciones innecesarias pero se han mantenido algunas como posibles
ampliaciones.
}
unit EditView;
{$mode ObjFPC}{$H+}
interface
uses  Classes, SysUtils, Clipbrd, SynEdit, SynEditMarkupHighAll, lconvencoding,
  Graphics, FileUtil, Dialogs, Controls, Forms, LCLType, ComCtrls,
  SynEditKeyCmds, SynEditTypes, SynEditMiscClasses, SynPluginMultiCaret, Menus,
  ExtCtrls, strUtils, LazUTF8, MisUtils, FormSelFuente, SynFacilCompletion,
  SynFacilBasic, SynFacilHighlighter;  //necesario para rutinas de manejo de sintaxis

type
  //Tipos de delimitador de línea de archivo.
  TLineEnd = (LDEL_DESC,   //Tipo desconocido
              LDEL_DOS,    //Tipo Windows/DOS
              LDEL_UNIX,   //Tipo Unix/Linux
              LDEL_MAC     //Tipo Mac OS
             );

  { TMarkup }
  {Marcador para resaltar errores de sintaxis en SynEdit}
  TMarkup = class(TSynEditMarkupHighlightMatches)
    public
      procedure SetMark(p1, p2: TPoint);
  end;

  { TSynFacilComplet2 }
  {Versión personalizada de  TSynFacilComplet, que define palabras claves y
   hace público el campo SpecIdentifiers}
  TSynFacilComplet2 = class(TSynFacilComplet)
  protected
    function IsKeyword(const AKeyword: string): boolean; override;
  public
    tnDirective: integer;    //ID del token Directiva, si existe.
    property SpecIdentif: TArrayTokSpec read SpecIdentifiers;
  end;

  { TSynEditor }
  TEventFile = procedure of object;

  //Define las propiedades que debe tener un texto que se está editando
  TSynEditor = class
    procedure edKeyPress(Sender: TObject; var Key: char);
    procedure edUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    procedure ReplaceDialog_Find(Sender: TObject);
    procedure ReplaceDialog_Replace(Sender: TObject);
  protected
    mnLineEnding: TMenuItem;  //Menú de Fin de línea
    mnEncoding  : TMenuItem;  //Menú de Codificación de texto
    procedure edChange(Sender: TObject);
    procedure edStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure edMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edCommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);

    //Estado de modificación
    procedure SetModified(valor: boolean);
    function GetModified: boolean;

    function GetText: string;
    procedure SetText(AValue: string);
  private  //Manejo de edición síncrona
    cursorPos: array of TPOINT;  //guarda posiciones de cursor
    procedure AddCursorPos(x,y: integer);
    procedure SetCursors;
  private
    FCaption : string;
    procedure SetCaption(AValue: string);
  protected
    const MAX_NMARK = 4;
  protected
    MarkErr: array[0..MAX_NMARK] of TMarkup;   //lista de marcadores
    MarkFree: integer;  //Índice al marcador libre
    procedure edSpecialLineMarkup(Sender: TObject; Line: integer;
      var Special: boolean; Markup: TSynSelectedColor);
    function GetFreeMark: TMarkup;

  public   //Campos que no contiene SynFacilUtils
    sedit   : TSynEdit; //Referencia al editor SynEdit
    x1      : integer;  //Coordenada inicial de dibujo
    tabWidth: integer;  //ancho de lengueta
    panTabs : TPanel;   //referencia al Panel de las lenguetas.
    property Caption: string read FCaption write SetCaption;  //etiqueta de la pestaña
    procedure ClearMarkErr;
    procedure MarkError(p1: TPoint);
  public
    FileName: string;    //Nombre del archivo
    LineDelim: TLineEnd;  //Tipo de delimitador de fin de línea
    CodArc  : string;    //Codificación de archivo
    linErr  : integer;   //Línea de error. SOlo usada para marcar un error
    Error   : string;    //Mensaje de error en alguna operación
    extDef  : string;    //Extensión por defecto para los archivos (txt, xml, ...)
    namDef  : string;    //Nombre por defecto para nuevos archivos
    hl      : TSynFacilComplet2; //Resaltador.
    //Eventos
    OnChangeEditorState:TEventFile;  {Cuando cambia el estado de modificado, con opción
                          "Undo", con "Redo", con opción "Copiar", "Cortar", "Pegar"}
    OnChangeFileInform: TEventFile;  {Cuando cambia información de nombre de archivo, tipo
                           de delimitador de línea o tipo de codificación}
    OnSelectionChange : TEventFile; //Cuando cambia el área seleccionada
    OnFileOpened      : TEventFile; //Cuando se ha cargado un nuevo archivo
    //Reflejo de los eventos de TSynEdit:
    OnEditChange : TNotifyEvent;
    OnMouseDown  : TMouseEvent;
    OnKeyUp      : TKeyEvent;
    OnKeyDown    : TKeyEvent;
    OnKeyPress   : TKeyPressEvent;
    OnUTF8KeyPress: TUTF8KeyPressEvent;
    //funciones comunes de un editor
    procedure NewFile;
    procedure LoadFile(arc8: string);
    procedure SaveFile;
    function SaveAsDialog(SaveDialog1: TSaveDialog): boolean;
    function SaveQuery(SaveDialog1: TSaveDialog): boolean;
    //Funciones para cambio de Fin de Línea y Codificación
    procedure ChangeEndLineDelim(nueFor: TLineEnd); //cambia Fin de línea
    procedure InitMenuLineEnding(mnLineEnding0: TMenuItem);  //configura menú
    procedure LineEndingClick(Sender: TObject);     //evento click
    procedure ChangeEncoding(nueCod: string);       //cambia codificación
    procedure InitMenuEncoding(mnEncoding0: TMenuItem);  //configura menú
    procedure EncodingClick(Sender: TObject);       //evento click
    procedure ChangeFileInform;
    //funciones para completado de código
    procedure CloseCompletionWindow;
  public  //herramientas
    procedure TabToSpaces(fSelFuente: TfrmSelFuente);
    procedure TrimLines(fSelFuente: TfrmSelFuente);
    function FiltLines(fSelFuente: TfrmSelFuente; sal: TStringList): boolean;
  public  //Búsqueda/reemplazo
    //Diálogos para búsqueda/reemplazo
    FindDialog1: TFindDialog;
    ReplaceDialog1: TReplaceDialog;
    procedure FindDialog_Find(Sender: TObject);  //el método FindNext() ya existe
    procedure FindDialog;
    procedure ReplaceDialog;
  public  //Espejo de funciones comunes del editor
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Undo;
    procedure Redo;
    procedure SelectAll;
  public  //Lectura de estado
    function CanUndo: boolean;
    function CanRedo: boolean;
    function CanCopy: boolean;
    function CanPaste: boolean;
    property Modified: boolean read GetModified write SetModified;

  public  //Paneles informativos

    property Text: string read GetText write SetText;  //devuelve el contenido real del editor
    procedure RefreshPanCursor;  //Refresca panel de la posición del cursor
  public  //Inicialización
    procedure LoadSyntaxFromFile(XMLfile: string); //Carga un archivo de sintaxis
    constructor Create(AOwner: TComponent; nomDef0, extDef0: string;
      panTabs0: TPanel); virtual;
    destructor Destroy; override;
  end;

procedure InicEditorC1(ed: TSynEdit);
procedure StringToFile(const s: string; const FileName: string);
function StringFromFile(const FileName: string): string;
procedure GetFileInfo(filName: string; var lineDel: TLineEnd; var encoding: string);
function LoadLinesFromFile(arc8: string; Lineas: TStrings;
                           var LineDelim: TLineEnd; var CodArc: string): string;
function SaveLinesToFile(arc0: string; Lineas: TStrings;
                           var TipArc: TLineEnd; var CodArc: string): string;

const
  MIN_WIDTH_TAB = 50;  //Ancho por defecto de la lengueta
  FONT_TAB_SIZE = 9;
  SEPAR_TABS = 2;  //Separación adicional, entre pestañas

implementation
const
  szChar = SizeOf(Char);

resourcestring
  MSG_PASFILES = 'Pascal Files';
  MSG_ALLFILES = 'All files';
  MSG_MODIFSAV = 'File %s has been modified. Save?';

procedure msgErr(msje: string);  //Rutina útil
//Mensaje de error
begin
  Application.MessageBox(PChar(msje), '', MB_ICONERROR);
end;
procedure InicEditorC1(ed: TSynEdit);
//Inicia un editor con una configuración especial para empezar a trabajar con el.
var
  SynMarkup: TSynEditMarkupHighlightAllCaret;  //para resaltar palabras iguales
begin
   //Inicia resaltado de palabras iguales
  SynMarkup := TSynEditMarkupHighlightAllCaret(ed.MarkupByClass[TSynEditMarkupHighlightAllCaret]);
  SynMarkup.MarkupInfo.FrameColor := clSilver;
  SynMarkup.MarkupInfo.Background := TColor($FFF0B0);

  SynMarkup.WaitTime := 250; // millisec
  SynMarkup.Trim := True;     // no spaces, if using selection
  SynMarkup.FullWord := True; // only full words If "Foo" is under caret, do not mark it in "FooBar"
  SynMarkup.IgnoreKeywords := true;

  //  ed.Font.Name:='Courier New';
  //  ed.Font.Size:=10;
  ed.Options:=[eoHideRightMargin,eoBracketHighlight];  //quita la línea vertical
  ed.Options := ed.Options + [eoKeepCaretX];  //Limita posición X del cursor para que no escape de la línea
  ed.Options := ed.Options + [eoTabIndent];  //permite indentar con <Tab>
  ed.Options2 := ed.Options2 + [eoCaretSkipTab];  //trata a las tabulaciones como un caracter
end;
procedure StringToFile(const s: string; const FileName: string);
///   saves a string to a file
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    FileStream.WriteBuffer(Pointer(s)^, (Length(s) * szChar));
  finally
    FreeAndNil(FileStream);
  end; // try
end;
function StringFromFile(const FileName: string): string;
///   returns the content of the file as a string
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(Result, (FileStream.Size div szChar));
    FileStream.ReadBuffer(Pointer(Result)^, FileStream.Size);
  finally
    FreeAndNil(FileStream);
  end; // try
end;
procedure GetFileInfo(filName: string; var lineDel: TLineEnd; var encoding: string);
(*Obtiene el tipo de delimitador de línea (Line Ending) de un archivo de texto, explorando
 los primeros bytes de archivo. Solo explora los primeros 8K del archivo.
 Si no encuentra un salto de línea en ese tamaño, no podrá deetrminar de que tipo de
 archivo se trata. También explora el posible tipo de codificación usado.
 *)
const TAM_BOL = 8192;
var ar: file;
    bolsa : Array[0..TAM_BOL] of char;  //deja un byte más para el delimitador
    Pbolsa: PChar;  //variable Pchar para "bolsa"
    Pos13: Word;    //posición de caracter #13
    Pos10: Word;    //posición de caracter #10
    Leidos: Word;   //bytes leidos
begin
   //Lee bloque de datos
   AssignFile(ar,filName);
   reset(ar,1);  { TODO : Dio error al abrir un filName de solo lectura }
   BlockRead(ar, bolsa{%H-}, TAM_BOL, Leidos{%H-});  //Lectura masiva
   CloseFile(ar);
   bolsa[Leidos] := #0; //agrega delimitador
   Pbolsa := @bolsa;    //cadena PChar
   //Explora en busca de delimitadores de línea
   Pos13 := Pos(#13, Pbolsa);
   Pos10 := Pos(#10, Pbolsa);
   if Pos13 = 0 then
      //solo hay separador #10 o ninguno
      if Pos10<>0 then
         lineDel := LDEL_UNIX     //solo hay #10
      else
         lineDel := LDEL_DESC  //no se encontró separador
   else if Pos10 = 0 then
      //solo hay separador #13 o ninguno
      if Pos13 <> 0 then
         lineDel := LDEL_MAC     //solo hay #13
      else
         lineDel := LDEL_DESC  //no se encontró separador
   else if Pos10 = Pos13 + 1 then
      lineDel := LDEL_DOS    //no se encontró #13#10
   else
      lineDel := LDEL_DESC;  //no se reconoce delimitadores
   //Analiza codifiación
   encoding := GuessEncoding(Pbolsa);  //analiza los primeros bytes
{ TODO : Ver por qué no detectó correctaente la carga de un filName UTF-8 sin BOM }
end;
function LineEnd_To_Str(delim: TLineEnd): string;
//proporciona una descripción al tipo de delimitador
begin
  Result := 'Unknown'; //'Desconoc.';
  case delim of
    LDEL_DOS : Result := 'DOS/Win';  //DOS/Windows
    LDEL_UNIX: Result := 'UNIX/Linux';
    LDEL_MAC : Result := 'MAC OS';
    LDEL_DESC: Result := '<Unknown>'; //'Desconoc.';
  end;
end;
function Str_To_LineEnd(str: string): TLineEnd;
//proporciona una descripción al tipo de delimitador
begin
  Result := LDEL_DESC;   //desconocido
  case str of
    'DOS/Win'   : Result := LDEL_DOS;
    'UNIX/Linux': Result := LDEL_UNIX;
    'MAC OS'    : Result := LDEL_MAC;
    'Unknown'   : Result := LDEL_DESC;
  end;
end;
function LoadLinesFromFile(arc8: string; Lineas: TStrings;
                          var LineDelim: TLineEnd; var CodArc: string): string;
{Carga el contenido de un archivo en un "TStrings". Si la codificación es diferente de
 UTF-8, hace la conversión. Esta pensado para usarse en un SynEdit.
 Además actualiza el Tipo de Delimitador de línea y la Codificación.
 Devuelve una cadena que indica si hubo conversión }
var
  arc0: String;
begin
  CodArc := '';
  arc0 := UTF8ToSys(arc8);   //pone en modo ANSI
  GetFileInfo(arc0, LineDelim, CodArc);  //actualiza tipo de archivo de trabajo
  //Carga archivo solicitado
  Lineas.LoadFromFile(arc8);
  //realiza las conversiones necesarias, ya que "ed", solo maneja UTF-8
  if CodArc = 'cp1252' then begin
    Lineas.Text := CP1252ToUTF8(Lineas.Text);
    Result := 'Converted to UTF-8';
  end else if CodArc = 'utf8bom' then begin
    Lineas.Text := UTF8BOMToUTF8(Lineas.Text);
    Result := 'Converted to UTF-8';
  end else if CodArc = 'ISO-8859-1' then begin
    Lineas.Text := ISO_8859_1ToUTF8(Lineas.Text);
    Result := 'Converted to UTF-8';
  end else if CodArc = 'utf8' then begin
    Result := 'Without conversion';  //no se cambia
  end else begin  //Cualquier otra codificación se asume UTF-8 y no se cambia.
    //En windows tal vez debería cargarse cp1252 por defecto.
    Result := 'utf8';
  end;
end;
function SaveLinesToFile(arc0: string; Lineas: TStrings;
                           var TipArc: TLineEnd; var CodArc: string): string;
{Guarda el contenido de un "TStrings" en un archivo. Si la codificación es diferente de
 UTF-8, hace la conversión. Esta pensado para usarse en un SynEdit.
 Además usa el Tipo de Delimitador de línea para guardar el archivo.
 Devuelve una cadena con un mensaje de error, si es que lo hubiera. }
begin
  Result := '';  //Sin error por defecto.
  //configura tipo de separador
//  case TipArc of
//  LDEL_DOS: TSynEditLines(ed.Lines).FileWriteLineEndType := sfleCrLf;
//  LDEL_UNIX: TSynEditLines(ed.Lines).FileWriteLineEndType := sfleLf;
//  LDEL_MAC: TSynEditLines(ed.Lines).FileWriteLineEndType := sfleCr;
//  TAR_DESCON: TSynEditLines(ed.Lines).FileWriteLineEndType := sfleCrLf;
//  end;
  case TipArc of
  LDEL_DOS:  Lineas.TextLineBreakStyle := tlbsCRLF;
  LDEL_UNIX: Lineas.TextLineBreakStyle := tlbsLF;
  LDEL_MAC:  Lineas.TextLineBreakStyle := tlbsCR;
  LDEL_DESC: Lineas.TextLineBreakStyle := tlbsCRLF;
  end;

  if CodArc = 'utf8' then begin
    //opción sin conversión
    StringToFile(Lineas.Text,arc0);
  end else if CodArc = 'cp1252' then  begin
    StringToFile(UTF8ToCP1252(Lineas.Text),arc0);
  end else if CodArc = 'utf8bom' then begin
    StringToFile(UTF8ToUTF8BOM(Lineas.Text),arc0);
  end else if CodArc = 'ISO-8859-1' then begin
    StringToFile(UTF8ToISO_8859_1(Lineas.Text),arc0);
  end else begin //Si es otra codificación, se guarda como UTF-8
//    MsgExc('Unknown file encoding');
    StringToFile(Lineas.Text, arc0);
  end;
end;

{ TMarkup }
procedure TMarkup.SetMark(p1, p2: TPoint);
begin
  Matches.StartPoint[0] := p1;
  Matches.EndPoint[0]   := p2;
  InvalidateSynLines(p1.y, p2.y);
end;

{ TSynFacilComplet2 }
function TSynFacilComplet2.IsKeyword(const AKeyword: string): boolean;
{Esta rutina es llamada por el Markup, que resalta palabras iguales. Se implementa
para evitar que se resalten palabras muy conocidas}
begin
  //Para el lenguaje Pascal, esta rutina funciona bien
  case UpCase(AKeyword) of
  'CONS','VAR','TYPE','BEGIN','END','IF','THEN','ELSE','WHILE',
  'DO','REPEAT','UNTIL','FOR','TO','AND','OR','XOR','NOT','DIV','MOD','IN':
    exit(true)
  else
    exit(false);
  end;
end;

{ TSynEditor }
//respuesta a eventos del editor
procedure TSynEditor.edMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  RefreshPanCursor;
  linErr := 0;  //para que quite la marca de fondo del error.
                //Solo se notará cuando se refresque la línea en el editor.
  //pasa el evento
  if OnMouseDown <> nil then OnMouseDown(Sender, Button, Shift, X, Y);
end;
procedure TSynEditor.edStatusChange(Sender: TObject; Changes: TSynStatusChanges);
//Cambia el estado del editor
begin
  if scSelection in changes then begin   //cambios en la selección
    if OnSelectionChange<>nil then OnSelectionChange;  //dispara eventos
    if OnChangeEditorState<>nil then OnChangeEditorState;  //para iniciar controles
  end;
end;
procedure TSynEditor.edChange(Sender: TObject);
begin
  //Ha habido cambio de contenido
  if OnChangeEditorState<>nil then OnChangeEditorState;  //para iniciar controles
  //Pasa el evento
  if OnEditChange <> nil then OnEditChange(Sender);
end;
procedure TSynEditor.edCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
begin
  RefreshPanCursor;
  linErr := 0;  //para que quite la marca de fondo del error.
                //Solo se notará cuando se refresque la línea en el editor.
end;
procedure TSynEditor.edKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//var
//  lexState: TFaLexerState;
begin
//  if (Shift = [ssCtrl]) and (Key = VK_J) then begin
//    //Exploramos el texto usando el resaltador
//    //Utilizaremos el mismo resaltador

  //    SetCursors;           //Coloca los cursores
////    sedit.CommandProcessor(ecSelWordRight, '', nil);
//  end;
  if Key = VK_ESCAPE then begin
    //Cancela una posible edición de múltiples cursores
    sedit.CommandProcessor(ecPluginMultiCaretClearAll, '', nil);
  end;
  //Pasa el evento
  if OnKeyDown <> nil then OnKeyDown(Sender, Key, Shift);
end;
procedure TSynEditor.edKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //pasa el evento al resaltador por si necesita abrir el menú de completado
  hl.KeyUp(Sender, Key, Shift);
  //Pasa el evento
  if OnKeyUp <> nil then OnKeyUp(Sender, Key, Shift);
end;
procedure TSynEditor.edKeyPress(Sender: TObject; var Key: char);
begin
  //Pasa evento
  if OnKeyPress <> nil then OnKeyPress(Sender, Key);
end;
procedure TSynEditor.edUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char
  );
begin
  //pasa el evento al resaltador por si necesita abrir el menú de completado
  hl.UTF8KeyPress(Sender, UTF8Key);
  //Pasa el evento
  if OnUTF8KeyPress <> nil then OnUTF8KeyPress(Sender, UTF8Key);
end;
procedure TSynEditor.SetModified(valor: boolean);
//Cambia el valor del campo "Modified", del editor
begin
  if sedit.Modified<> valor then begin
    //se ha cambiado el estado de "Modificado"
    sedit.Modified := valor;    //Fija valor
    //Dispara evento
    if OnChangeEditorState<>nil then OnChangeEditorState;
  end;
end;
function TSynEditor.GetModified: boolean;
//Lee el valor del campo "Modified", del editor.
begin
  Result := sedit.Modified;
end;
function TSynEditor.GetText: string;
//Devuelve el contenido del editor, quitando el salto de línea final
begin
  Result := sedit.Text;
  if AnsiEndsStr(LineEnding, Result) then begin
     Setlength(Result, length(Result)-length(LineEnding));
  end;
end;
procedure TSynEditor.SetText(AValue: string);
//Fija el contenido del editor
begin
  sedit.Text:=AValue;
end;
procedure TSynEditor.AddCursorPos(x, y: integer);
{Agrega una posición de cursor al areglo CursorPos[]}
var
  n: Integer;
begin
  n := high(CursorPos) + 1;
  setlength(CursorPos, n + 1);
  CursorPos[n].x := x;
  CursorPos[n].y := y;
end;
procedure TSynEditor.SetCursors;
var
  i: Integer;
begin
  if high(cursorPos)<0 then exit;
//  sedit.CommandProcessor(ecPluginMultiCaretClearAll, '', nil);
  for i:= high(cursorPos) downto 0 do begin
    //Explora la revés para dejar el último cursor al inicio del texto
    if i = 0 then begin
      //El último
      sedit.CaretY := cursorPos[i].y;   //primero la fila
      sedit.CaretX := cursorPos[i].x;
      sedit.ExecuteCommand(ecPluginMultiCaretSetCaret, '', nil);
//    sedit.CommandProcessor(ecPluginMultiCaretSetCaret, '', nil);
    end else begin
      sedit.CaretY := cursorPos[i].y;   //primero la fila
      sedit.CaretX := cursorPos[i].x;
//    sedit.ExecuteCommand(ecPluginMultiCaretSetCaret, '', nil);
      sedit.CommandProcessor(ecPluginMultiCaretSetCaret, '', nil);
    end;
  end;
end;
procedure TSynEditor.edSpecialLineMarkup(Sender: TObject; Line: integer;
  var Special: boolean; Markup: TSynSelectedColor);
begin
  if Line = self.linErr then begin
      Special := True ;  //marca como línea especial
      Markup.Background := TColor($3030A0); //color de fondo
  end;
end;
function TSynEditor.GetFreeMark: TMarkup;
//Devuelve referencia a un marcador no usado. Si no encuentra alguno, devuelve NIL.
begin
  if MarkFree <= MAX_NMARK then begin
    Result := MarkErr[MarkFree];
    MarkFree := MarkFree + 1;
  end else begin
    Result := nil;
  end;
end;
procedure TSynEditor.ClearMarkErr;
{Limpia marcadores de error.}
var
  i: Integer;
begin
  for i:=0 to MAX_NMARK do begin
    MarkErr[i].Enabled := false;
  end;
  MarkFree := 0;
  sedit.Invalidate;
end;
procedure TSynEditor.MarkError(p1: TPoint);
{Marca el token que se encuentra en la coordenada indicada. Para ubicar al token afectado,
usa información del lexer/resaltador del editor, que debe ser consistente con el lexer del
compilador, para obtener resultados corectos.
En caso de errores dentro de bloques ASM o Directivas, usa un lexer interno simple,
porque no se tiene acceso a los lexer que procesan los bloques ASM y Directivas.}
  function LocEndOfWord(const lin: string; col1: integer): integer;
  {Devuelve el final de la palabra actual, que empieza en "col1".}
  var
    i: Integer;
  begin
    i := col1;  //empìeza por aquí
    if i>length(lin) then exit(length(lin));
    if lin[i] in ['A'..'Z','a'..'z','_'] then begin
      //Es identificador. Ubica los límites del identificador.
      while (i<=length(lin)) and (lin[i] in ['A'..'Z','a'..'z','0'..'9','_']) do begin
        inc(i);
      end;
    end else if lin[i] = ' ' then begin
      //Es espacio. Ubica fin espacio
      while (i<=length(lin)) and (lin[i]=' ') do begin
        inc(i);
      end;
    end else begin
      //Es otro token. Ubica espacio o fin de línea
      while (i<=length(lin)) and (lin[i]<>' ') do begin
        inc(i);
      end;
    end;
    Result := i;
  end;
var
  toks: TATokInfo;
  tokIdx, col1, col2: integer;
  curTok: TFaTokInfo;
  lin: String;
  MarkErr1: TMarkup;
begin
  hl.ExploreLine(p1, toks, tokIdx);  //Explora la línea aludida
  if tokIdx = -1 then exit;
  MarkErr1 := GetFreeMark;
  if MarkErr1 = nil then exit;
  curTok := toks[tokIdx];  //token actual
  //Obtiene línea actual
  if hl.CurrentLines = nil then begin
    exit;
  end else begin
    lin := hl.CurrentLines[p1.y-1];
  end;
  MarkErr1.Enabled := true;
  //Obtiene en los límites del token actual
  col1 := curTok.posIni+1;
  col2 := curTok.posIni+1+curTok.length;
  if curTok.TokTyp = hl.tnEol then begin
    //Es la marca de final de línea. Extiende para que sea visible
    MarkErr1.SetMark(Point(col1, p1.y),
                    Point(col2 + 1, p1.y));
  end else if curTok.TokTyp = hl.GetAttribIDByName('Asm') then begin
    //Es bloque ensamblador.
    col2 := LocEndOfWord(lin, p1.x);  //ubica a la palabra actual
    MarkErr1.SetMark(Point(p1.x, p1.y),
                    Point(col2, p1.y));
  end else if curTok.TokTyp = hl.GetAttribIDByName('Directive') then begin
    //Es directiva
    col2 := LocEndOfWord(lin, p1.x);  //ubica a la palabra actual
    MarkErr1.SetMark(Point(p1.x, p1.y),
                    Point(col2, p1.y));
  end else begin
    //Es un token normal
    MarkErr1.SetMark(Point(col1, p1.y),
                    Point(col2, p1.y));
  end;
end;
procedure TSynEditor.SetCaption(AValue: string);
{Cambiar el título, cambia el ancho de la lengueta}
var
  w: Integer;
begin
  if FCaption = AValue then Exit;
  FCaption := AValue;
  panTabs.Canvas.Font.Size := FONT_TAB_SIZE;  {Fija atrubutos de texto, para que el
                                        cálculo con "TextWidth", de ancho sea correcto}
  w := panTabs.Canvas.TextWidth(AValue) + 30;
  if w < MIN_WIDTH_TAB then w := MIN_WIDTH_TAB;
  tabWidth := w;
  panTabs.Invalidate;   //Para refrescar el dibujo
end;
procedure TSynEditor.NewFile();
//Inicia al editor con un nuevo nombre de archivo
//"QuerySave" indica si se debe o no preguntar por archivo modificado
begin
  Error := '';    //limpia bandera de error
  if extDef<> '' then //genera nombre por defecto
    FileName := namDef + '.' + extDef
  else FileName := namDef;
  LineDelim := LDEL_DOS;  //inicia con Windows por defecto
  CodArc := 'cp1252'; //inicia en formato Windows
  sedit.ClearAll;        //limpia editor
  sedit.ClearUndo;       //limpia acciones "deshacer"
  SetModified(false);
  ChangeFileInform;   //actualiza
  if OnChangeEditorState<>nil then OnChangeEditorState;  //Para iniciar controles
end;
procedure TSynEditor.LoadFile(arc8: string);
//Carga el contenido de un archivo en el editor, analizando la codificación.
//Si ocurre algún error, muestra el mensaje en pantalla y actualiza "Error".
var
  arc0: String;
begin
  Error := '';    //limpia bandera de error
  arc0 := UTF8ToSys(arc8);   //pone en modo ANSI
  //verifica existencia de archivo
  if not FileExists(arc0) then begin
    Error := 'File not found: ' + arc0;
    msgErr(Error);
    Exit;                    //sale
  end;
  //carga y lee formato
  LoadLinesFromFile(arc8, sedit.Lines, LineDelim, CodArc);
  if sedit.Lines.Count = 0 then begin
    if LineDelim = LDEL_DESC then begin
      //Archivo vacío en el que, lógicamente, no se puede determinar tipo de delimitador
      //ni la codificación. Por eso asumimos, para no generar los molestos mensajes.
      LineDelim := LDEL_UNIX;   //Asumimos Saltos en UNIX
      CodArc := 'utf8';
    end;
  end else begin
    //No es archivo vacío.
    if LineDelim = LDEL_DESC then begin
      MsgExc('Unknown line delimiter');
//      MsgExc('Unknown file encoding');
    end;
  end;
//  StatusBar1.Panels[4].Text := CodArc;  //actualiza codificación
  FileName := arc0;      //Fija nombre de archivo de trabajo.
  SetModified(false);    //Inicia estado.
  linErr := 0;           //Limpia línea marcada por si acaso.
  ChangeFileInform;      //Actualiza
  if OnFileOpened<>nil then OnFileOpened;  //dispara evento
end;
procedure TSynEditor.SaveFile;
//Guarda el contenido del editor en su archivo correspondiente
//Si ocurre algún error, muestra el mensaje en pantalla y actualiza "Error".
begin
  Error := '';    //limpia bandera de error
  try
    //Guarda en formato original
    SaveLinesToFile(FileName, sedit.Lines, LineDelim, CodArc);
    SetModified(false);
    edChange(self);  //para que actualice el panel fPanFileSaved
    //se actualiza por si acaso, se haya guardado con otro nombre
    ChangeFileInform;   //actualiza
  except
    Error := 'Error saving file: ' + FileName;
    msgErr(Error);
  end;
end;
function TSynEditor.SaveAsDialog(SaveDialog1: TSaveDialog): boolean;
//Guarda el contenido del editor, permitiendo cambiar el nombre con un diálogo.
//Si se ignora la acción, devuelve "true".
//Si ocurre algún error, muestra el mensaje en pantalla y actualiza "Error".
var
  arc0: String;
  resp: TModalResult;
begin
  Result := false;
  SaveDialog1.Filter := MSG_PASFILES + '|*.pas|' + MSG_ALLFILES + '|*.*';
  SaveDialog1.DefaultExt := '.pas';
  if not SaveDialog1.Execute then begin  //se canceló
    Result := true;   //Sale con "true"
    exit;    //se canceló
  end;
  arc0 := SaveDialog1.FileName;
  if FileExists(arc0) then begin
    resp := MessageDlg('', Format('File %s already exists.' + LineEnding + 'Overwrite?',[arc0]),
                       mtConfirmation, [mbYes, mbNo, mbCancel],0);
    if (resp = mrCancel) or (resp = mrNo) then Exit;
  end;
  FileName := UTF8ToSys(arc0);   //asigna nuevo nombre
  if ExtractFileExt(FileName) = '' then FileName += '.'+extDef;  //completa extensión
  SaveFile;   //lo guarda
  //Se ha cambiado el nombre del archivo. Actualiza.
  Caption := ExtractFileName(FileName);
end;
function TSynEditor.SaveQuery(SaveDialog1: TSaveDialog): boolean;
{Versión de SaveQuery(), que verifica si el editor tiene nombre.}
//Verifica si es necesario guardar el archivo antes de ejecutar alguna operación.
//Si se ignora la acción, devuelve "true".
//Si ocurre algún error, muestra el mensaje en pantalla y actualiza "Error".
var
  resp: integer;
begin
  Result := false;
  if sedit.Modified then begin
    resp := MessageDlg('', Format(MSG_MODIFSAV, [ExtractFileName(FileName)]),
                       mtConfirmation, [mbYes, mbNo, mbCancel],0);
    if resp = mrCancel then begin
      Result := true;   //Sale con "true"
      Exit;
    end;
    if resp = mrYes then begin  //guardar
      if FileName='' then begin
        //Es un archivo nuevo
        SaveAsDialog(SaveDialog1);
      end else begin
        SaveFile;  //ACtualiz "Error"
      end;
    end;
  end;
end;
//Herramientas
procedure TSynEditor.TabToSpaces(fSelFuente: TfrmSelFuente);
//Convierte tabulaciones a espacios.
//Requiere un formaulario de tipo TfrmSelFuente para mostrar el dialogo.
var
  i: integer;
begin
  if sedit.SelAvail then begin
    fSelFuente.optLin.Enabled:=false;
    fSelFuente.optSel.Enabled:=true;
    fSelFuente.optSel.Checked := true;
  end else begin
    fSelFuente.optSel.Enabled:=false;
    fSelFuente.optLin.Enabled := true;
    fSelFuente.optLin.Checked := true;
  end;
  fSelFuente.ShowModal;
  If fSelFuente.cancelado Then begin  //se canceló
    //no hace nada
  end else If fSelFuente.optLin.Checked Then begin  //línea
    sedit.LineText:=StringReplace(sedit.LineText,#9, stringOfChar(' ',sedit.TabWidth), [rfReplaceAll]);
  end else If fSelFuente.optSel.Checked Then begin  //seleción
    sedit.SelText:=StringReplace(sedit.seltext,#9, stringOfChar(' ',sedit.TabWidth), [rfReplaceAll]);;
  end else begin                           //todo
    for i := 0 to sedit.Lines.Count-1 do
      sedit.Lines[i] := StringReplace(sedit.Lines[i],#9, stringOfChar(' ',sedit.TabWidth), [rfReplaceAll]);
  End;
  sedit.ClearUndo;   //porque no se puede deshacer
  if OnChangeEditorState<>nil then OnChangeEditorState;  //actualiza
end;
procedure TSynEditor.TrimLines(fSelFuente: TfrmSelFuente);
//Quita espacios laterales
//Requiere un formaulario de tipo TfrmSelFuente para mostrar el dialogo.
var
  i: integer;
  f1: integer;
  f2: integer;
begin
  if sedit.SelAvail then begin
    fSelFuente.optLin.Enabled:=false;
    fSelFuente.optSel.Enabled:=true;
    fSelFuente.optSel.Checked := true;
  end else begin
    fSelFuente.optSel.Enabled:=false;
    fSelFuente.optLin.Enabled := true;
    fSelFuente.optLin.Checked := true;
  end;
  fSelFuente.ShowModal;
  If fSelFuente.cancelado Then begin  //se canceló
    //no hace nada
  end else If fSelFuente.optLin.Checked Then begin  //línea
    sedit.LineText:=trim(sedit.LineText);
  end else If fSelFuente.optSel.Checked Then begin  //seleción
    f1 := sedit.BlockBegin.y;
    f2 := sedit.BlockEnd.y;
    for i := f1 to f2-1 do
      sedit.Lines[i-1] := trim(sedit.Lines[i]);
  end else begin                           //todo
    for i := 0 to sedit.Lines.Count-1 do
      sedit.Lines[i] := trim(sedit.Lines[i]);
  End;
  sedit.ClearUndo;   //porque no se puede deshacer
  if OnChangeEditorState<>nil then OnChangeEditorState;  //actualiza
end;
function TSynEditor.FiltLines(fSelFuente: TfrmSelFuente; sal: TStringList): boolean;
//Filtra líneas del contenido. Si se cancela la operación devuelve false.
var
  i: integer;
  s: string;
begin
  Result := true;  //por defecto
  sal := TStringList.Create;  //salida
  fSelFuente.optLin.Enabled:=false;
  if sedit.SelAvail and (sedit.BlockEnd.y - sedit.BlockBegin.y >0) then begin
     //hay selección de más de una línea
     fSelFuente.optSel.Enabled:=true;
     fSelFuente.optSel.Checked := true;
     fSelFuente.ShowModal;  //solo muestra aquí
     If fSelFuente.cancelado Then exit;  //se canceló
  end else begin  //no hay selección
     fSelFuente.optTod.Checked := true;
//      fSelFuente.ShowModal;
  end;
  If fSelFuente.optSel.Checked Then begin  //seleción
    s := InputBox('Filter lines:','Enter text: ','');
    if s = '' then exit;
    for i:= sedit.BlockBegin.y to sedit.BlockEnd.y do
      if AnsiContainsText(sedit.Lines[i],s) then sal.Add(sedit.Lines[i]);
  end Else begin                           //todo
    if sedit.BlockEnd.y = sedit.BlockBegin.y then  //hay texto seleccionado, suguiere
      s := InputBox('Filter lines:','Enter text: ',sedit.SelText)
    else
      s := InputBox('Filter lines:','Enter text: ','');
    if s = '' then exit;
    for i:= 0 to sedit.Lines.Count-1 do
      if AnsiContainsText(sedit.Lines[i],s) then sal.Add(sedit.Lines[i]);
  End;
end;
//Funciones para cambio de Fin de Línea y Codificación
procedure TSynEditor.ChangeEndLineDelim(nueFor: TLineEnd);
//Cambia el formato de salto de línea del contenido
begin
  if LineDelim <> nueFor then begin  //verifica si hay cambio
    LineDelim := nueFor;
    SetModified(true); //para indicar que algo ha cambiado
    ChangeFileInform;   //actualiza
  end;
end;
procedure TSynEditor.InitMenuLineEnding(mnLineEnding0: TMenuItem);
//Inicia un menú con los tipos de delimitador de línea que maneja la unidad, y les
//les asigna un evento para implementar el cambio.
begin
  if mnLineEnding0 = nil then exit;
  mnLineEnding := mnLineEnding0;  //guarda referencia a menú
  //configura menú
  mnLineEnding.Caption:= 'Line Ending';
  mnLineEnding.Clear;
  //llena opciones
  AddItemToMenu(mnLineEnding, LineEnd_To_Str(LDEL_UNIX), @LineEndingClick);
  AddItemToMenu(mnLineEnding, LineEnd_To_Str(LDEL_DOS), @LineEndingClick);
  AddItemToMenu(mnLineEnding, LineEnd_To_Str(LDEL_MAC), @LineEndingClick);
  AddItemToMenu(mnLineEnding, LineEnd_To_Str(LDEL_DESC), @LineEndingClick).Enabled:=false;
  CheckOnlyOneItem(mnLineEnding, LineEnd_To_Str(LineDelim));   //actualiza
end;
procedure TSynEditor.LineEndingClick(Sender: TObject);
var
  it: TMenuItem;
  delim: TLineEnd;
begin
  it := TMenuItem(Sender);
  delim := Str_To_LineEnd(it.Caption);
  ChangeEndLineDelim(delim);
  CheckOnlyOneItem(it); //marca menú
end;
procedure TSynEditor.ChangeEncoding(nueCod: string);
//Cambia la codificación del archivo
begin
  if CodArc <> nueCod then begin
    CodArc := nueCod;
    SetModified(true); //para indicar que algo ha cambiado
    ChangeFileInform;   //actualiza
  end;
end;
procedure TSynEditor.InitMenuEncoding(mnEncoding0: TMenuItem);
//Inicia un menú con los tipos de delimitador de línea que maneja la unidad, y les
//les asigna un evento para implementar el cambio.
begin
  if mnEncoding0 = nil then exit;
  mnEncoding := mnEncoding0;  //guarda referencia a menú
  //configura menú
  mnEncoding.Caption:= 'Encoding';
  mnEncoding.Clear;
  //llena opciones
  AddItemToMenu(mnEncoding, 'utf8', @EncodingClick);
  AddItemToMenu(mnEncoding, 'utf8bom', @EncodingClick);
  AddItemToMenu(mnEncoding, 'cp1252', @EncodingClick);
  AddItemToMenu(mnEncoding, 'ISO-8859-1', @EncodingClick);
  AddItemToMenu(mnEncoding, '<Unknown>', @EncodingClick).Enabled:=false;
  CheckOnlyOneItem(mnEncoding, codArc);   //actualiza
end;
procedure TSynEditor.EncodingClick(Sender: TObject);
var
  it: TMenuItem;
begin
  it := TMenuItem(Sender);
  ChangeEncoding(it.Caption);
  CheckOnlyOneItem(it); //marca menú
end;
procedure TSynEditor.ChangeFileInform;
//Se debe llamar siempre que puede cambiar la información de nombre de archivo, tipo de
//delimitador de línea o tipo de codificación del archivo.
begin
  //Actualiza menús
  CheckOnlyOneItem(mnLineEnding, LineEnd_To_Str(LineDelim));
  CheckOnlyOneItem(mnEncoding, codArc);
  //dispara evento
  if OnChangeFileInform<>nil then OnChangeFileInform;
end;
procedure TSynEditor.CloseCompletionWindow;
//Cierra la ventana de completado
begin
  hl.CloseCompletionWindow;
end;
//Búsqueda y reemplazo
procedure TSynEditor.FindDialog_Find(Sender: TObject);
//Se ejecuta para encontrar el siguiente elemento. Es llamado por el evento OnFind()
//de "FindDialog1", pero puede ser llamado también de forma directa, por es es público.
var
  encon  : integer;
  target : string;
  opciones: TSynSearchOptions;
begin
  target := FindDialog1.FindText;
  opciones := [];
  if not(frDown in FindDialog1.Options) then opciones += [ssoBackwards];
  if frMatchCase in FindDialog1.Options then opciones += [ssoMatchCase];
  if frWholeWord in FindDialog1.Options then opciones += [ssoWholeWord];
  if frEntireScope in FindDialog1.Options then opciones += [ssoEntireScope];

  encon := sedit.SearchReplace(target,'',opciones);
  if encon = 0 then begin
    //MsgBox('No found: %s', [target]);
    if MsgYesNo('No found: %s. Continue from start?', [target]) = 1 then begin
      sedit.CaretX := 1;
      sedit.CaretY := 1;
    end;
  end;
end;
procedure TSynEditor.ReplaceDialog_Find(Sender: TObject);
var
  encon  : integer;
  target : string;
  opciones: TSynSearchOptions;
begin
  target := ReplaceDialog1.FindText;
  opciones := [];
  if not(frDown in ReplaceDialog1.Options) then opciones += [ssoBackwards];
  if frMatchCase in ReplaceDialog1.Options then opciones += [ssoMatchCase];
  if frWholeWord in ReplaceDialog1.Options then opciones += [ssoWholeWord];
  if frEntireScope in ReplaceDialog1.Options then opciones += [ssoEntireScope];

  encon := sedit.SearchReplace(target,'',opciones);
  if encon = 0 then begin
    //MsgBox('No found: %s', [target]);
    if MsgYesNo('No found: %s. Continue from start?', [target]) = 1 then begin
      sedit.CaretX := 1;
      sedit.CaretY := 1;
    end;
  end;
end;
procedure TSynEditor.ReplaceDialog_Replace(Sender: TObject);
var
  encon, r : integer;
  buscado : string;
  opciones: TSynSearchOptions;
begin
  buscado := ReplaceDialog1.FindText;
  opciones := [ssoFindContinue];
//  opciones := [];
  if not(frDown in ReplaceDialog1.Options) then opciones += [ssoBackwards];
  if frMatchCase in ReplaceDialog1.Options then opciones += [ssoMatchCase];
  if frWholeWord in ReplaceDialog1.Options then opciones += [ssoWholeWord];
  if frEntireScope in ReplaceDialog1.Options then opciones += [ssoEntireScope];
  if frReplaceAll in ReplaceDialog1.Options then begin
    //se ha pedido reemplazar todo
    encon := sedit.SearchReplace(buscado,ReplaceDialog1.ReplaceText,
                              opciones+[ssoReplaceAll]);  //reemplaza
    msgbox('%d occurrences were replaced.',[encon]);
    exit;
  end;
  //reemplazo con confirmación
  ReplaceDialog1.CloseDialog;
  encon := sedit.SearchReplace(buscado,'',opciones);  //búsqueda
  while encon <> 0 do begin
      //pregunta
      r := Application.MessageBox(Pchar('Replace this?'),
                Pchar('Replace'), MB_YESNOCANCEL);
      if r = IDCANCEL then exit;
      if r = IDYES then begin
        sedit.TextBetweenPoints[sedit.BlockBegin,sedit.BlockEnd] := ReplaceDialog1.ReplaceText;
      end;
      //busca siguiente
      encon := sedit.SearchReplace(buscado,'',opciones);  //búsca siguiente
  end;
  MsgBox('No found: %s', [buscado]);
end;
procedure TSynEditor.FindDialog;
//Realiza una búsqueda en el texto del editor, usando el ´diálogo de búsqeudas.
begin
  FindDialog1.OnFind:=@FindDialog_Find;
  if sedit.SelAvail then FindDialog1.FindText:=sedit.SelText;
  FindDialog1.Execute;
end;
procedure TSynEditor.ReplaceDialog;
//Realiza una búsqueda en el texto del editor, usando el ´diálogo de búsqeudas.
begin
  ReplaceDialog1.OnFind:=@ReplaceDialog_Find;
  ReplaceDialog1.OnReplace:=@ReplaceDialog_Replace;
  if sedit.SelAvail then ReplaceDialog1.FindText:=sedit.SelText;
  ReplaceDialog1.Execute;
end;
//Espejo de funciones comunes del editor
procedure TSynEditor.Cut;
begin
  sedit.CutToClipboard;
end;
procedure TSynEditor.Copy;
begin
  sedit.CopyToClipboard;
end;
procedure TSynEditor.Paste;
begin
  sedit.PasteFromClipboard;
end;
procedure TSynEditor.Undo;
//Deshace una acción en el editor
begin
  sedit.Undo;
end;
procedure TSynEditor.Redo;
//Rehace una acción en el editor
begin
  sedit.Redo;
end;
procedure TSynEditor.SelectAll;
begin
  sedit.SelectAll;
end;
//Lectura de estado
function TSynEditor.CanUndo: boolean;
//Indica si Hay Algo por deshacer
begin
  Result := sedit.CanUndo;
end;
function TSynEditor.CanRedo: boolean;
//Indica si Hay Algo por rehacer
begin
  Result := sedit.CanRedo;
end;
function TSynEditor.CanCopy: boolean;
//Indica si hay algo por copiar
begin
  Result := sedit.SelAvail;
end;
function TSynEditor.CanPaste: boolean;
//Indica si Hay Algo por pegar
begin
  Result := sedit.CanPaste;
end;
//Paneles informativos
procedure TSynEditor.RefreshPanCursor;
begin
//  if fPanCursorPos <> nil then
//    fPanCursorPos.Text:= Format('row=%d col=%d', [sedit.CaretY, sedit.CaretX]);
end;
//Inicialización
procedure TSynEditor.LoadSyntaxFromFile(XMLfile: string);
//Carga un archivo de sintaxis en el editor.
begin
  hl.LoadFromFile(XMLfile);  //carga sintaxis
  //Actualiza referencia por si existe. Al menos en Pascal debe existir.
  hl.tnDirective := hl.GetAttribIDByName('Directive');
end;
constructor TSynEditor.Create(AOwner: TComponent; nomDef0, extDef0: string;
  panTabs0: TPanel);
var
  i: Integer;
  mark: TMarkup;
begin
  //Crea diálogos
  FindDialog1:= TFindDialog.Create(nil);
  ReplaceDialog1:= TReplaceDialog.Create(nil);
  //Inicialización
  sedit := TSynEdit.Create(AOwner);// Crea un editor
  hl := TSynFacilComplet2.Create(sedit.Owner);  //crea resaltador
  hl.SelectEditor(sedit);  //inicia
  //intercepta eventos
  sedit.OnChange          := @edChange;   //Necesita interceptar los cambios
  sedit.OnStatusChange    := @edStatusChange;
  sedit.OnMouseDown       := @edMouseDown;
  sedit.OnKeyUp           := @edKeyUp;    //Para funcionamiento del completado
  sedit.OnKeyDown         := @edKeyDown;
  sedit.OnKeyPress        := @edKeyPress;
  sedit.OnUTF8KeyPress    := @edUTF8KeyPress;
  sedit.OnCommandProcessed:= @edCommandProcessed;  //Necesita para actualizar el cursor
  //guarda parámetros
  namDef := nomDef0;
  extDef := extDef0;
  ///////////////////////////////////////////////////////////////
  tabWidth := 30;  //valor por defecto
  panTabs := panTabs0;

  //configuración del editor
  sedit.Options:=[eoBracketHighlight];  //quita la línea vertical
  sedit.Options := sedit.Options + [eoSmartTabs];
  sedit.Options := sedit.Options - [eoTrimTrailingSpaces];
  sedit.Options := sedit.Options + [eoKeepCaretX];
  sedit.Options := sedit.Options + [eoTabIndent];  //permite indentar con <Tab>
  sedit.Options2:= sedit.Options2 + [eoCaretSkipTab];
  sedit.TabWidth:= 2;
  sedit.OnSpecialLineMarkup:=@edSpecialLineMarkup;
  InicEditorC1(sedit);
  sedit.Options := sedit.Options + [eoTabsToSpaces];  //permite indentar con <Tab>

  //Crea marcadores para los errores de sinatxis
  for i:=0 to MAX_NMARK do begin
    mark := TMarkup.Create(sedit);
    MarkErr[i] := mark;   //asigna referencia
    mark.MarkupInfo.Background := clNone;
    mark.MarkupInfo.Foreground := clNone;
    mark.MarkupInfo.FrameColor := clRed;
    mark.MarkupInfo.FrameEdges := sfeBottom;
    mark.MarkupInfo.FrameStyle := slsWaved;
    sedit.MarkupManager.AddMarkUp(mark);   //agrega marcador
  end;

  NewFile;        //para actualizar estado
end;
destructor TSynEditor.Destroy;
begin
  hl.UnSelectEditor;
  hl.Free;
  FreeAndNil(sedit);  //El "Owner", intentará destruirlo, por eso lo ponemos en NIL
  ReplaceDialog1.Destroy;
  FindDialog1.Destroy;
  inherited Destroy;
end;

end.
//1181
