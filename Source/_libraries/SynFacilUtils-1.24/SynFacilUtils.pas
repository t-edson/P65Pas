{
Descripción
===========
Utilidades para la creación de editores con el resaltador SynFacilSyn.

Trabaja con SynFacilCompletion 1.0 o superior
}
unit SynFacilUtils;
{$mode objfpc}{$H+}
interface
uses  Classes, SysUtils, Clipbrd, SynEdit, SynEditMarkupHighAll,
      lconvencoding, Graphics, FileUtil, Dialogs, Controls, Forms, LCLType, ComCtrls,
      SynEditKeyCmds, SynEditTypes, Menus, strUtils, LazUTF8, MisUtils, FormSelFuente,
      SynFacilCompletion;  //necesario para rutinas de manejo de sintaxis

type
  //Tipos de delimitador de línea de archivo.
  TLineEnd = (TAR_DESC,   //Tipo desconocido
             TAR_DOS,     //Tipo Windows/DOS
             TAR_UNIX,    //Tipo Unix/Linux
             TAR_MAC      //Tipo Mac OS
             );

  { TSynFacilEditor }
  TEventFile = procedure of object;
  TEventLangSelec = procedure(langName, xmlFile: string) of object;

  //Define las propiedades que debe tener un texto que se está editando
  TSynFacilEditor = class
    procedure edKeyPress(Sender: TObject; var Key: char);
    procedure edUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    procedure DoSelectLanguage(Sender: TObject);
    procedure CheckLanguageMenu(XMLfile: string);
    procedure ReplaceDialog_Find(Sender: TObject);
    procedure ReplaceDialog_Replace(Sender: TObject);
  protected
    ed          : TSynEdit;    //referencia al editor
    fPanLangName: TStatusPanel;
    mnRecents   : TMenuItem;  //Menú de archivos recientes
    mnLanguages : TMenuItem;  //Menú de lenguajes
    mnLineEnding: TMenuItem;  //Menú de Fin de línea
    mnEncoding  : TMenuItem;  //Menú de Codificación de texto
    LangPath    : string;     //ruta donde están los lengaujes
    MaxRecents  : integer;    //Máxima cantidad de archivos recientes
    //paneles con información del estado del editor
    fPanFileSaved : TStatusPanel;  //Panel para mensaje "Guardado"
    fPanCursorPos : TStatusPanel;  //Panel para mostrar posición del cursor
    //paneles para información del archivo
    fPanFileName  : TStatusPanel;  //Panel para mostrar el nombre de archivo
    fPanForEndLin : TStatusPanel;  //Panel para mostrar el tipo de delimitador de línea
    fPanCodifFile : TStatusPanel;  //Panel para mostrar la codificaión de archivo
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
    procedure SetPanCodifFile(AValue: TStatusPanel);
    procedure SetPanCursorPos(AValue: TStatusPanel);
    procedure SetPanFileName(AValue: TStatusPanel);
    procedure SetPanFileSaved(AValue: TStatusPanel);
    procedure SetPanForEndLin(AValue: TStatusPanel);
    procedure SetPanLangName(AValue: TStatusPanel);
    function GetText: string;
    procedure SetText(AValue: string);
  public
    FileName: string;    //Nombre del archivo
    DelArc  : TLineEnd;  //Tipo de delimitador de fin de línea
    CodArc  : string;    //Codificación de archivo
    linErr  : integer;   //Línea de error. SOlo usada para marcar un error
    Error   : string;    //Mensaje de error en alguna operación
    extDef  : string;    //Extensión por defecto para los archivos (txt, xml, ...)
    namDef  : string;    //Nombre por defecto para nuevos archivos
    RecentFiles: TStringList;  //Lista de archivos recientes
    hl      : TSynFacilComplet; //Resaltador.
    //Eventos
    OnChangeEditorState:TEventFile;  {Cuando cambia el estado de modificado, con opción
                          "Undo", con "Redo", con opción "Copiar", "Cortar", "Pegar"}
    OnChangeFileInform: TEventFile;  {Cuando cambia información de nombre de archivo, tipo
                           de delimitador de línea o tipo de codificación}
    OnSelectionChange : TEventFile; //Cuando cambia el área seleccionada
    OnFileOpened      : TEventFile; //Cuando se ha cargado un nuevo archivo
    OnMenLangSelected : TEventLangSelec; //Languaje seleccionado del menú, cuando se usa InitMenuLanguages().
    //Reflejo de los eventos de TSynEdit:
    OnEditChange : TNotifyEvent;
    OnMouseDown  : TMouseEvent;
    OnKeyUp      : TKeyEvent;
    OnKeyDown    : TKeyEvent;
    OnKeyPress   : TKeyPressEvent;
    OnUTF8KeyPress: TUTF8KeyPressEvent;
    //funciones comunes de un editor
    procedure NewFile(QuerySave: boolean=true); virtual;
    procedure LoadFile(arc8: string); virtual;
    procedure SaveFile; virtual;
    function OpenDialog(OpenDialog1: TOpenDialog): boolean; virtual;
    function SaveAsDialog(SaveDialog1: TSaveDialog): boolean; virtual;
    function SaveQuery: boolean; virtual;
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
    property PanFileSaved: TStatusPanel read fPanFileSaved write SetPanFileSaved;
    property PanCursorPos: TStatusPanel read fPanCursorPos write SetPanCursorPos;

    property PanFileName : TStatusPanel read fPanFileName  write SetPanFileName;
    property PanForEndLin: TStatusPanel read fPanForEndLin write SetPanForEndLin;
    property PanCodifFile: TStatusPanel read fPanCodifFile write SetPanCodifFile;
    property PanLangName : TStatusPanel read fPanLangName write SetPanLangName;

    property Text: string read GetText write SetText;  //devuelve el contenido real del editor
    procedure RefreshPanCursor;  //Refresca panel de la posición del cursor
  public  //Manejo de archivos recientes
    procedure InitMenuRecents(menRecents0: TMenuItem; RecentList: TStringList;
      MaxRecents0: integer=5);
    procedure RecentClick(Sender: TObject);
    procedure ActualMenusReciente(Sender: TObject);
    procedure AddRecentFile(arch: string);
  public  //Inicialización
    procedure InitMenuLanguages(menLanguage0: TMenuItem; LangPath0: string);
    procedure LoadSyntaxFromFile(XMLfile: string); //Carga un archivo de sintaxis
    procedure LoadSyntaxFromPath(arc: string='');  //Carga sintaxis viendo extensión de archivo
    constructor Create(ed0: TsynEdit; nomDef0, extDef0: string); virtual;
    destructor Destroy; override;
  end;

procedure InicEditorC1(ed: TSynEdit);
procedure StringToFile(const s: string; const FileName: string);
function StringFromFile(const FileName: string): string;
procedure GetFileInfo(filName: string; var lineDel: TLineEnd; var encoding: string);
function CargarArchivoLin(arc8: string; Lineas: TStrings;
                           var TipArc: TLineEnd; var CodArc: string): string;
function GuardarArchivoLin(arc0: string; Lineas: TStrings;
                           var TipArc: TLineEnd; var CodArc: string): string;
implementation
const
  szChar = SizeOf(Char);


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
         lineDel := TAR_UNIX     //solo hay #10
      else
         lineDel := TAR_DESC  //no se encontró separador
   else if Pos10 = 0 then
      //solo hay separador #13 o ninguno
      if Pos13 <> 0 then
         lineDel := TAR_MAC     //solo hay #13
      else
         lineDel := TAR_DESC  //no se encontró separador
   else if Pos10 = Pos13 + 1 then
      lineDel := TAR_DOS    //no se encontró #13#10
   else
      lineDel := TAR_DESC;  //no se reconoce delimitadores
   //Analiza codifiación
   encoding := GuessEncoding(Pbolsa);  //analiza los primeros bytes
{ TODO : Ver por qué no detectó correctaente la carga de un filName UTF-8 sin BOM }
end;
function LineEnd_To_Str(delim: TLineEnd): string;
//proporciona una descripción al tipo de delimitador
begin
  Result := 'Unknown'; //'Desconoc.';
  case delim of
    TAR_DOS : Result := 'DOS/Win';  //DOS/Windows
    TAR_UNIX: Result := 'UNIX/Linux';
    TAR_MAC : Result := 'MAC OS';
    TAR_DESC: Result := '<Unknown>'; //'Desconoc.';
  end;
end;
function Str_To_LineEnd(str: string): TLineEnd;
//proporciona una descripción al tipo de delimitador
begin
  Result := TAR_DESC;   //desconocido
  case str of
    'DOS/Win'   : Result := TAR_DOS;
    'UNIX/Linux': Result := TAR_UNIX;
    'MAC OS'    : Result := TAR_MAC;
    'Unknown'   : Result := TAR_DESC;
  end;
end;
function CargarArchivoLin(arc8: string; Lineas: TStrings;
                           var TipArc: TLineEnd; var CodArc: string): string;
{Carga el contenido de un archivo en un "TStrings". Si la codificación es diferente de
 UTF-8, hace la conversión. Esta pensado para usarse en un SynEdit.
 Además actualiza el Tipo de Delimitador de línea y la Codificación.
 Devuelve una cadena que indica si hubo conversión }
var
  arc0: String;
begin
  CodArc := '';
  arc0 := UTF8ToSys(arc8);   //pone en modo ANSI
  GetFileInfo(arc0, TipArc, CodArc);  //actualiza tipo de archivo de trabajo
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
  end else begin  //cualquier otra codificación se asume UTF-8 y no se cambia
    //En windows tal vez debería cargarse cp1252 por defecto.
    Result := 'utf8';
  end;
end;
function GuardarArchivoLin(arc0: string; Lineas: TStrings;
                           var TipArc: TLineEnd; var CodArc: string): string;
{Guarda el contenido de un "TStrings" en un archivo. Si la codificación es diferente de
 UTF-8, hace la conversión. Esta pensado para usarse en un SynEdit.
 Además usa el Tipo de Delimitador de línea para guardar el archivo.
 Devuelve una cadena con un mensaje de error, si es que lo hubiera. }
begin
  Result := '';  //sin error por defecto
  //configura tipo de separador
//  case TipArc of
//  TAR_DOS: TSynEditLines(ed.Lines).FileWriteLineEndType := sfleCrLf;
//  TAR_UNIX: TSynEditLines(ed.Lines).FileWriteLineEndType := sfleLf;
//  TAR_MAC: TSynEditLines(ed.Lines).FileWriteLineEndType := sfleCr;
//  TAR_DESCON: TSynEditLines(ed.Lines).FileWriteLineEndType := sfleCrLf;
//  end;
  case TipArc of
  TAR_DOS:  Lineas.TextLineBreakStyle := tlbsCRLF;
  TAR_UNIX: Lineas.TextLineBreakStyle := tlbsLF;
  TAR_MAC:  Lineas.TextLineBreakStyle := tlbsCR;
  TAR_DESC: Lineas.TextLineBreakStyle := tlbsCRLF;
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
  end else begin //si es otra codificación, se guarda como UTF-8
    MsgExc('Unknown file encoding');
    StringToFile(Lineas.Text,arc0);
  end;
end;
{ TSynFacilEditor }
//respuesta a eventos del editor
procedure TSynFacilEditor.edMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  RefreshPanCursor;
  linErr := 0;  //para que quite la marca de fondo del error.
                //Solo se notará cuando se refresque la línea en el editor.
  //pasa el evento
  if OnMouseDown <> nil then OnMouseDown(Sender, Button, Shift, X, Y);
end;
procedure TSynFacilEditor.edStatusChange(Sender: TObject; Changes: TSynStatusChanges);
//Cambia el estado del editor
begin
  if scSelection in changes then begin   //cambios en la selección
    if OnSelectionChange<>nil then OnSelectionChange;  //dispara eventos
    if OnChangeEditorState<>nil then OnChangeEditorState;  //para iniciar controles
  end;
end;
procedure TSynFacilEditor.edChange(Sender: TObject);
begin
  if fPanFileSaved <> nil then begin
    if GetModified then fPanFileSaved.Text:='Modified' else fPanFileSaved.Text:='Saved';
  end;
  //Ha habido cambio de contenido
  if OnChangeEditorState<>nil then OnChangeEditorState;  //para iniciar controles
  //Pasa el evento
  if OnEditChange <> nil then OnEditChange(Sender);
end;
procedure TSynFacilEditor.edCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
begin
  RefreshPanCursor;
  linErr := 0;  //para que quite la marca de fondo del error.
                //Solo se notará cuando se refresque la línea en el editor.
end;
procedure TSynFacilEditor.edKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //Pasa el evento
  if OnKeyDown <> nil then OnKeyDown(Sender, Key, Shift);
end;
procedure TSynFacilEditor.edKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //pasa el evento al resaltador por si necesita abrir el menú de completado
  hl.KeyUp(Sender, Key, Shift);
  //Pasa el evento
  if OnKeyUp <> nil then OnKeyUp(Sender, Key, Shift);
end;
procedure TSynFacilEditor.edKeyPress(Sender: TObject; var Key: char);
begin
  //Pasa evento
  if OnKeyPress <> nil then OnKeyPress(Sender, Key);
end;
procedure TSynFacilEditor.edUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char
  );
begin
  //pasa el evento al resaltador por si necesita abrir el menú de completado
  hl.UTF8KeyPress(Sender, UTF8Key);
  //Pasa el evento
  if OnUTF8KeyPress <> nil then OnUTF8KeyPress(Sender, UTF8Key);
end;
procedure TSynFacilEditor.SetModified(valor: boolean);
//Cambia el valor del campo "Modified", del editor
begin
  if ed.Modified<> valor then begin
    //se ha cambiado el estado de "Modificado"
    ed.Modified := valor;    //Fija valor
    //dispara evento
    if fPanFileSaved <> nil then begin
      if GetModified then fPanFileSaved.Text:='Modified' else fPanFileSaved.Text:='Saved';
    end;
    if OnChangeEditorState<>nil then OnChangeEditorState;
  end;
end;
function TSynFacilEditor.GetModified: boolean;
//Lee el valor del campo "Modified", del editor.
begin
  Result := ed.Modified;
end;
//"Setters" de los paneles
procedure TSynFacilEditor.SetPanCursorPos(AValue: TStatusPanel);
begin
  if FPanCursorPos=AValue then Exit;
  fPanCursorPos:=AValue;
  RefreshPanCursor;
end;
procedure TSynFacilEditor.SetPanFileSaved(AValue: TStatusPanel);
begin
  if fPanFileSaved=AValue then Exit;
  fPanFileSaved:=AValue;
  if fPanFileSaved <> nil then begin
    if GetModified then fPanFileSaved.Text:='Modified' else fPanFileSaved.Text:='Saved';
  end;
end;

procedure TSynFacilEditor.SetPanFileName(AValue: TStatusPanel);
begin
  if fPanFileName=AValue then Exit;
  fPanFileName:=AValue;
  if fPanFileName <> nil then begin
    fPanFileName.Text := SysToUTF8(FileName);
  end;
end;
procedure TSynFacilEditor.SetPanForEndLin(AValue: TStatusPanel);
begin
  if fPanForEndLin=AValue then Exit;
  fPanForEndLin:=AValue;
  if fPanForEndLin <> nil then begin
    fPanForEndLin.Text:=LineEnd_To_Str(DelArc);
  end;
end;
procedure TSynFacilEditor.SetPanCodifFile(AValue: TStatusPanel);
begin
  if fPanCodifFile=AValue then Exit;
  fPanCodifFile:=AValue;
  if fPanCodifFile <> nil then begin
    fPanCodifFile.Text:=CodArc;
  end;
end;
procedure TSynFacilEditor.SetPanLangName(AValue: TStatusPanel);
begin
  if fPanLangName=AValue then Exit;
  fPanLangName:=AValue;
  if fPanLangName<> nil then begin
    fPanLangName.Text:= hl.LangName;
  end;
end;
function TSynFacilEditor.GetText: string;
//Devuelve el contenido del editor, quitando el salto de línea final
begin
  Result := ed.Text;
  if AnsiEndsStr(LineEnding, Result) then begin
     Setlength(Result, length(Result)-length(LineEnding));
  end;
end;
procedure TSynFacilEditor.SetText(AValue: string);
//Fija el contenido del editor
begin
  ed.Text:=AValue;
end;

procedure TSynFacilEditor.NewFile(QuerySave: boolean=true);
//Inicia al editor con un nuevo nombre de archivo
//"QuerySave" indica si se debe o no preguntar por archivo modificado
begin
  if QuerySave then begin
    if SaveQuery then Exit;   //Verifica cambios
    if Error<>'' then exit;  //hubo error
  end;
  Error := '';    //limpia bandera de error
  if extDef<> '' then //genera nombre por defecto
    FileName := namDef + '.' + extDef
  else FileName := namDef;
  //verifica existencia
//  if FileExists(Arc) then   //ya existe
//     AbrirArchivo(Arc)  //lo abre
//  else begin   //no existe
//    mnArGuarClick(nil);  //Lo crea
  DelArc := TAR_DOS;  //inicia con Windows por defecto
  CodArc := 'cp1252'; //inicia en formato Windows
  ed.ClearAll;        //limpia editor
  ed.ClearUndo;       //limpia acciones "deshacer"
  SetModified(false);
  ChangeFileInform;   //actualiza
  if OnChangeEditorState<>nil then OnChangeEditorState;  //para iniciar controles
end;
procedure TSynFacilEditor.LoadFile(arc8: string);
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
  CargarArchivoLin(arc8, ed.Lines, DelArc, CodArc);
//  StatusBar1.Panels[4].Text := CodArc;  //actualiza codificación
  FileName := arc0;         //fija nombre de archivo de trabajo
  SetModified(false);  //Inicia estado
  linErr := 0;            //limpia línea marcada por si acaso
  ChangeFileInform;   //actualiza
  if OnFileOpened<>nil then OnFileOpened;  //dispara evento
  AddRecentFile(arc8);  //agrega a lista de recientes
end;
procedure TSynFacilEditor.SaveFile;
//Guarda el contenido del editor en su archivo correspondiente
//Si ocurre algún error, muestra el mensaje en pantalla y actualiza "Error".
begin
  Error := '';    //limpia bandera de error
  try
    GuardarArchivoLin(FileName, ed.Lines, DelArc, CodArc);  //guarda en formato original
    SetModified(false);
    edChange(self);  //para que actualice el panel fPanFileSaved
    //se actualiza por si acaso, se haya guardado con otro nombre
    ChangeFileInform;   //actualiza
  except
    Error := 'Error saving file: ' + FileName;
    msgErr(Error);
  end;
end;
function TSynFacilEditor.OpenDialog(OpenDialog1: TOpenDialog): boolean;
//Muestra el cuadro de diálogo para abrir un archivo, teniendo cuidado de
//pedir confirmación para grabar el contenido actual. Si hay error devuelve FALSE.
var arc0: string;
begin
  Error := '';
  if SaveQuery then exit(true);   //Verifica cambios
  if Error<>'' then exit(false);  //hubo error
  if not OpenDialog1.Execute then exit(true);    //se canceló
  arc0 := OpenDialog1.FileName;
  LoadFile(arc0);  //legalmente debería darle en UTF-8
  Result := true;   //sale sin incidencias
end;
function TSynFacilEditor.SaveAsDialog(SaveDialog1: TSaveDialog): boolean;
//Guarda el contenido del editor, permitiendo cambiar el nombre con un diálogo.
//Si se ignora la acción, devuelve "true".
//Si ocurre algún error, muestra el mensaje en pantalla y actualiza "Error".
var
  arc0: String;
  resp: TModalResult;
begin
  Result := false;
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
end;
function TSynFacilEditor.SaveQuery: boolean;
//Verifica si es necesario guardar el archivo antes de ejecutar alguna oepración con el editor.
//Si se ignora la acción, devuelve "true".
//Si ocurre algún error, muestra el mensaje en pantalla y actualiza "Error".
var resp: integer;
begin
  Result := false;
  if ed = nil then begin
    Error := 'Internal: Not initialized Editor.';
    msgErr(Error);
    exit;
  end;
  if ed.Modified then begin
    resp := MessageDlg('', Format('File %s, has been modified.' + LineEnding + 'Save?',
                               [ExtractFileName(FileName)]),
                       mtConfirmation, [mbYes, mbNo, mbCancel],0);
    if resp = mrCancel then begin
      Result := true;   //Sale con "true"
      Exit;
    end;
    if resp = mrYes then begin  //guardar
      SaveFile;  //Actualizar "Error"
    end;
  end;
end;
//Herramientas
procedure TSynFacilEditor.TabToSpaces(fSelFuente: TfrmSelFuente);
//Convierte tabulaciones a espacios.
//Requiere un formaulario de tipo TfrmSelFuente para mostrar el dialogo.
var
  i: integer;
begin
  if ed.SelAvail then begin
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
    ed.LineText:=StringReplace(ed.LineText,#9, stringOfChar(' ',ed.TabWidth), [rfReplaceAll]);
  end else If fSelFuente.optSel.Checked Then begin  //seleción
    ed.SelText:=StringReplace(ed.seltext,#9, stringOfChar(' ',ed.TabWidth), [rfReplaceAll]);;
  end else begin                           //todo
    for i := 0 to ed.Lines.Count-1 do
      ed.Lines[i] := StringReplace(ed.Lines[i],#9, stringOfChar(' ',ed.TabWidth), [rfReplaceAll]);
  End;
  ed.ClearUndo;   //porque no se puede deshacer
  if OnChangeEditorState<>nil then OnChangeEditorState;  //actualiza
end;
procedure TSynFacilEditor.TrimLines(fSelFuente: TfrmSelFuente);
//Quita espacios laterales
//Requiere un formaulario de tipo TfrmSelFuente para mostrar el dialogo.
var
  i: integer;
  f1: integer;
  f2: integer;
begin
  if ed.SelAvail then begin
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
    ed.LineText:=trim(ed.LineText);
  end else If fSelFuente.optSel.Checked Then begin  //seleción
    f1 := ed.BlockBegin.y;
    f2 := ed.BlockEnd.y;
    for i := f1 to f2-1 do
      ed.Lines[i-1] := trim(ed.Lines[i]);
  end else begin                           //todo
    for i := 0 to ed.Lines.Count-1 do
      ed.Lines[i] := trim(ed.Lines[i]);
  End;
  ed.ClearUndo;   //porque no se puede deshacer
  if OnChangeEditorState<>nil then OnChangeEditorState;  //actualiza
end;
function TSynFacilEditor.FiltLines(fSelFuente: TfrmSelFuente; sal: TStringList): boolean;
//Filtra líneas del contenido. Si se cancela la operación devuelve false.
var
  i: integer;
  s: string;
begin
  Result := true;  //por defecto
  sal := TStringList.Create;  //salida
  fSelFuente.optLin.Enabled:=false;
  if ed.SelAvail and (ed.BlockEnd.y - ed.BlockBegin.y >0) then begin
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
    for i:= ed.BlockBegin.y to ed.BlockEnd.y do
      if AnsiContainsText(ed.Lines[i],s) then sal.Add(ed.Lines[i]);
  end Else begin                           //todo
    if ed.BlockEnd.y = ed.BlockBegin.y then  //hay texto seleccionado, suguiere
      s := InputBox('Filter lines:','Enter text: ',ed.SelText)
    else
      s := InputBox('Filter lines:','Enter text: ','');
    if s = '' then exit;
    for i:= 0 to ed.Lines.Count-1 do
      if AnsiContainsText(ed.Lines[i],s) then sal.Add(ed.Lines[i]);
  End;
end;
//Funciones para cambio de Fin de Línea y Codificación
procedure TSynFacilEditor.ChangeEndLineDelim(nueFor: TLineEnd);
//Cambia el formato de salto de línea del contenido
begin
  if DelArc <> nueFor then begin  //verifica si hay cambio
    DelArc := nueFor;
    SetModified(true); //para indicar que algo ha cambiado
    ChangeFileInform;   //actualiza
  end;
end;
procedure TSynFacilEditor.InitMenuLineEnding(mnLineEnding0: TMenuItem);
//Inicia un menú con los tipos de delimitador de línea que maneja la unidad, y les
//les asigna un evento para implementar el cambio.
begin
  if mnLineEnding0 = nil then exit;
  mnLineEnding := mnLineEnding0;  //guarda referencia a menú
  //configura menú
  mnLineEnding.Caption:= 'Line Ending';
  mnLineEnding.Clear;
  //llena opciones
  AddItemToMenu(mnLineEnding, LineEnd_To_Str(TAR_UNIX), @LineEndingClick);
  AddItemToMenu(mnLineEnding, LineEnd_To_Str(TAR_DOS), @LineEndingClick);
  AddItemToMenu(mnLineEnding, LineEnd_To_Str(TAR_MAC), @LineEndingClick);
  AddItemToMenu(mnLineEnding, LineEnd_To_Str(TAR_DESC), @LineEndingClick).Enabled:=false;
  CheckOnlyOneItem(mnLineEnding, LineEnd_To_Str(delArc));   //actualiza
end;
procedure TSynFacilEditor.LineEndingClick(Sender: TObject);
var
  it: TMenuItem;
  delim: TLineEnd;
begin
  it := TMenuItem(Sender);
  delim := Str_To_LineEnd(it.Caption);
  ChangeEndLineDelim(delim);
  CheckOnlyOneItem(it); //marca menú
end;
procedure TSynFacilEditor.ChangeEncoding(nueCod: string);
//Cambia la codificación del archivo
begin
  if CodArc <> nueCod then begin
    CodArc := nueCod;
    SetModified(true); //para indicar que algo ha cambiado
    ChangeFileInform;   //actualiza
  end;
end;
procedure TSynFacilEditor.InitMenuEncoding(mnEncoding0: TMenuItem);
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
procedure TSynFacilEditor.EncodingClick(Sender: TObject);
var
  it: TMenuItem;
begin
  it := TMenuItem(Sender);
  ChangeEncoding(it.Caption);
  CheckOnlyOneItem(it); //marca menú
end;
procedure TSynFacilEditor.ChangeFileInform;
//Se debe llamar siempre que puede cambiar la información de nombre de archivo, tipo de
//delimitador de línea o tipo de codificación del archivo.
begin
  //actualiza información en los paneles
  if fPanFileName <> nil then begin
    fPanFileName.Text := SysToUTF8(FileName);
  end;
  if fPanForEndLin <> nil then begin
    fPanForEndLin.Text:=LineEnd_To_Str(DelArc);
  end;
  if fPanCodifFile <> nil then begin
    fPanCodifFile.Text:=CodArc;
  end;
  //actualiza menús
  CheckOnlyOneItem(mnLineEnding, LineEnd_To_Str(delArc));
  CheckOnlyOneItem(mnEncoding, codArc);
  //dispara evento
  if OnChangeFileInform<>nil then OnChangeFileInform;
end;
procedure TSynFacilEditor.CloseCompletionWindow;
//Cierra la ventana de completado
begin
  hl.CloseCompletionWindow;
end;
//Búsqueda y reemplazo
procedure TSynFacilEditor.FindDialog_Find(Sender: TObject);
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

  encon := ed.SearchReplace(target,'',opciones);
  if encon = 0 then begin
    //MsgBox('No found: %s', [target]);
    if MsgYesNo('No found: %s. Continue from start?', [target]) = 1 then begin
      ed.CaretX := 1;
      ed.CaretY := 1;
    end;
  end;
end;
procedure TSynFacilEditor.ReplaceDialog_Find(Sender: TObject);
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

  encon := ed.SearchReplace(target,'',opciones);
  if encon = 0 then begin
    //MsgBox('No found: %s', [target]);
    if MsgYesNo('No found: %s. Continue from start?', [target]) = 1 then begin
      ed.CaretX := 1;
      ed.CaretY := 1;
    end;
  end;
end;
procedure TSynFacilEditor.ReplaceDialog_Replace(Sender: TObject);
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
    encon := ed.SearchReplace(buscado,ReplaceDialog1.ReplaceText,
                              opciones+[ssoReplaceAll]);  //reemplaza
    msgbox('%d occurrences were replaced.',[encon]);
    exit;
  end;
  //reemplazo con confirmación
  ReplaceDialog1.CloseDialog;
  encon := ed.SearchReplace(buscado,'',opciones);  //búsqueda
  while encon <> 0 do begin
      //pregunta
      r := Application.MessageBox(Pchar('Replace this?'),
                Pchar('Replace'), MB_YESNOCANCEL);
      if r = IDCANCEL then exit;
      if r = IDYES then begin
        ed.TextBetweenPoints[ed.BlockBegin,ed.BlockEnd] := ReplaceDialog1.ReplaceText;
      end;
      //busca siguiente
      encon := ed.SearchReplace(buscado,'',opciones);  //búsca siguiente
  end;
  MsgBox('No found: %s', [buscado]);
end;
procedure TSynFacilEditor.FindDialog;
//Realiza una búsqueda en el texto del editor, usando el ´diálogo de búsqeudas.
begin
  FindDialog1.OnFind:=@FindDialog_Find;
  if ed.SelAvail then FindDialog1.FindText:=ed.SelText;
  FindDialog1.Execute;
end;
procedure TSynFacilEditor.ReplaceDialog;
//Realiza una búsqueda en el texto del editor, usando el ´diálogo de búsqeudas.
begin
  ReplaceDialog1.OnFind:=@ReplaceDialog_Find;
  ReplaceDialog1.OnReplace:=@ReplaceDialog_Replace;
  if ed.SelAvail then ReplaceDialog1.FindText:=ed.SelText;
  ReplaceDialog1.Execute;
end;
//Espejo de funciones comunes del editor
procedure TSynFacilEditor.Cut;
begin
  ed.CutToClipboard;
end;
procedure TSynFacilEditor.Copy;
begin
  ed.CopyToClipboard;
end;
procedure TSynFacilEditor.Paste;
begin
  ed.PasteFromClipboard;
end;
procedure TSynFacilEditor.Undo;
//Deshace una acción en el editor
begin
  ed.Undo;
end;
procedure TSynFacilEditor.Redo;
//Rehace una acción en el editor
begin
  ed.Redo;
end;
procedure TSynFacilEditor.SelectAll;
begin
  ed.SelectAll;
end;
//Lectura de estado
function TSynFacilEditor.CanUndo: boolean;
//Indica si Hay Algo por deshacer
begin
  Result := ed.CanUndo;
end;
function TSynFacilEditor.CanRedo: boolean;
//Indica si Hay Algo por rehacer
begin
  Result := ed.CanRedo;
end;
function TSynFacilEditor.CanCopy: boolean;
//Indica si hay algo por copiar
begin
  Result := ed.SelAvail;
end;
function TSynFacilEditor.CanPaste: boolean;
//Indica si Hay Algo por pegar
begin
  Result := ed.CanPaste;
end;
procedure TSynFacilEditor.DoSelectLanguage(Sender: TObject);
//Se ha seleccionado un lenguaje desde el menú.
var
  XMLfile: String;
  it: TMenuItem;
begin
  it := TMenuItem(Sender);
  XMLfile := LangPath + RightStr(it.Caption, length(it.Caption)-1 ) + '.xml';
  hl.LoadFromFile(XMLfile);  //carga la sintaxis indicada
  if fPanLangName<> nil then begin
    fPanLangName.Text:= hl.LangName;
  end;
  CheckOnlyOneItem(it); //marca menú
  if OnMenLangSelected<>nil then OnMenLangSelected(hl.LangName, XMLfile);
end;
procedure TSynFacilEditor.CheckLanguageMenu(XMLfile: string);
//Marca el ítem del menú de lenguaje que corresponde al nombre de archivo indicado.
var
  XML: String;
begin
  if mnLanguages = nil then exit;
  XML := ExtractFileName(XMLfile);  //por si tenía ruta
  XML := ChangeFileExt(XML,'');
  CheckOnlyOneItem(mnLanguages, XML);
end;
//Paneles informativos
procedure TSynFacilEditor.RefreshPanCursor;
begin
  if fPanCursorPos <> nil then
    fPanCursorPos.Text:= Format('row=%d col=%d', [ed.CaretY, ed.CaretX]);
end;
//Manejo de archivos recientes
procedure TSynFacilEditor.InitMenuRecents(menRecents0: TMenuItem; RecentList: TStringList;
                                     MaxRecents0: integer=5);
//Configura un menú, con el historial de los archivos abiertos recientemente
//"nRecents", es el número de archivos recientes que se guardará
var
  i: Integer;
begin
  mnRecents := menRecents0;
  RecentFiles := RecentList;  //gaurda referencia a lista
  MaxRecents := MaxRecents0;
  //configura menú
  mnRecents.Caption:= '&Recents';
  mnRecents.OnClick:=@ActualMenusReciente;
  for i:= 1 to MaxRecents do begin
    AddItemToMenu(mnRecents, '&'+IntToStr(i), @RecentClick);
  end;
end;
procedure TSynFacilEditor.RecentClick(Sender: TObject);
//Se selecciona un archivo de la lista de recientes
begin
  if SaveQuery then Exit;   //Verifica cambios
  LoadFile(MidStr(TMenuItem(Sender).Caption,4,150));
end;
procedure TSynFacilEditor.ActualMenusReciente(Sender: TObject);
{Actualiza el menú de archivos recientes con la lista de los archivos abiertos
recientemente. }
var
  i: Integer;
begin
  if mnRecents = nil then exit;
  if RecentFiles = nil then exit;
  //proteciión
  if RecentFiles.Count = 0 then begin
    mnRecents[0].Caption:='No files';
    mnRecents[0].Enabled:=false;
    for i:= 1 to mnRecents.Count-1 do begin
      mnRecents[i].Visible:=false;
    end;
    exit;
  end;
  //hace visible los ítems
  mnRecents[0].Enabled:=true;
  for i:= 0 to mnRecents.Count-1 do begin
    if i<RecentFiles.Count then
      mnRecents[i].Visible:=true
    else
      mnRecents[i].Visible:=false;
  end;
  //pone etiquetas a los menús, incluyendo un atajo numérico
  for i:=0 to RecentFiles.Count-1 do begin
    mnRecents[i].Caption := '&'+IntToStr(i+1)+' '+RecentFiles[i];
  end;
end;
procedure TSynFacilEditor.AddRecentFile(arch: string);
//Agrega el nombre de un archivo reciente
var hay: integer; //bandera-índice
    i: integer;
begin
  if RecentFiles = nil then exit;
  //verifica si ya existe
  hay := -1;   //valor inicial
  for i:= 0 to RecentFiles.Count-1 do
    if RecentFiles[i] = arch then hay := i;
  if hay = -1 then  //no existe
    RecentFiles.Insert(0,arch)  //agrega al inicio
  else begin //ya existe
    RecentFiles.Delete(hay);     //lo elimina
    RecentFiles.Insert(0,arch);  //lo agrega al inicio
  end;
  while RecentFiles.Count>MaxRecents do  //mantiene tamaño máximo
    RecentFiles.Delete(MaxRecents);
end;
//Inicialización
procedure TSynFacilEditor.InitMenuLanguages(menLanguage0: TMenuItem; LangPath0: string);
//Inicia un menú con la lista de archivos XML (que representan a lenguajes) que hay
//en una carpeta en particular y les asigna un evento.
var
  Hay: Boolean;
  SR : TSearchRec;
begin
  if menLanguage0 = nil then exit;
  mnLanguages := menLanguage0;  //guarda referencia a menú
  LangPath := LangPath0;        //guarda ruta
  if (LangPath<>'') and (LangPath[length(LangPath)] <> DirectorySeparator) then
     LangPath+=DirectorySeparator;
  //configura menú
  mnLanguages.Caption:= '&Languages';
  //explora archivos
  Hay := FindFirst(LangPath + '*.xml', faAnyFile - faDirectory, SR) = 0;
  while Hay do begin
     //encontró archivo
    AddItemToMenu(mnLanguages, '&'+ChangeFileExt(SR.name,''),@DoSelectLanguage);
    Hay := FindNext(SR) = 0;
  end;
end;
procedure TSynFacilEditor.LoadSyntaxFromFile(XMLfile: string);
//Carga un archivo de sintaxis en el editor.
begin
  hl.LoadFromFile(XMLfile);  //carga sintaxis
  if fPanLangName<> nil then begin
    fPanLangName.Text:= hl.LangName;
  end;
  //verifica si se puede marcar en el menú
  if mnLanguages = nil then exit;   //no se ha confogurado menú
  if LangPath = ExtractFilePath(XMLfile) then begin
    //La ruta correponde a la definida para el menú.
    CheckLanguageMenu(XMLfile);  //actualiza menú y panel
  end else begin
    //es una ruta distinta
  end;
end;
procedure TSynFacilEditor.LoadSyntaxFromPath(arc: string = '');
//Carga la sintaxis de un archivo, buscando el archivo XML, apropiado en la ruta
//de lengaujes definida con InitMenuLanguages().
//Si no se indica el nombre del archivo, se usará el archivo actual
var
  XML: String;
begin
  if arc='' then begin
    arc := FileName;
  end;
  XML := hl.LoadSyntaxFromPath(arc,LangPath);
  //marca menú
  if XML<>'' then begin  //encontró
    if fPanLangName<> nil then begin
      fPanLangName.Text:= hl.LangName;
    end;
    CheckLanguageMenu(XML);  //actualiza menú
    exit;
  end;
  //No encontró archivo XML apropiado
  //Carga una sintaxis básica para limpiar la que pudiera haber
  hl.ClearMethodTables;           //limpìa tabla de métodos
  hl.ClearSpecials;               //para empezar a definir tokens
  //crea tokens por contenido
  hl.DefTokIdentif('[$A-Za-z_]', '[A-Za-z0-9_]*');
  hl.DefTokContent('[0-9]', '[0-9.]*', hl.tnNumber);
  hl.DefTokDelim('"','"', hl.tnString);
  hl.Rebuild;  //reconstruye
  CheckLanguageMenu('');  //actualiza menú
  if fPanLangName<> nil then begin
    fPanLangName.Text:= 'No language';
  end;
end;
constructor TSynFacilEditor.Create(ed0: TsynEdit; nomDef0, extDef0: string);
begin
  //Crea diálogos
  FindDialog1:= TFindDialog.Create(nil);
  ReplaceDialog1:= TReplaceDialog.Create(nil);
  //Inicialización
  ed := ed0;
  hl := TSynFacilComplet.Create(ed.Owner);  //crea resaltador
  hl.SelectEditor(ed);  //inicia
  //intercepta eventos
  ed.OnChange          := @edChange;   //Necesita interceptar los cambios
  ed.OnStatusChange    := @edStatusChange;
  ed.OnMouseDown       := @edMouseDown;
  ed.OnKeyUp           := @edKeyUp;    //Para funcionamiento del completado
  ed.OnKeyDown         := @edKeyDown;
  ed.OnKeyPress        := @edKeyPress;
  ed.OnUTF8KeyPress    := @edUTF8KeyPress;
  ed.OnCommandProcessed:= @edCommandProcessed;  //Necesita para actualizar el cursor
//  RecentFiles := TStringList.Create;
  MaxRecents := 1;   //Inicia con 1
  //guarda parámetros
  namDef := nomDef0;
  extDef := extDef0;
  NewFile;   //Inicia editor con archivo vacío
end;
destructor TSynFacilEditor.Destroy;
begin
  hl.UnSelectEditor;
  hl.Free;
  ReplaceDialog1.Destroy;
  FindDialog1.Destroy;
  inherited Destroy;
end;

end.

