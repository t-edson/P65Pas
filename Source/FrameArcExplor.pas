{
FrameArcExplor
===============
Por Tito Hinostroza 28/09/2014

Descripción
===========
Explorador de archivo implementado en un solo TreeView. Permite navegar entre carpetas
y archivos, a modo de explorador.
Permite implementar las funciones principales de un explorador de archivos.
Se ha diseñado para ser controlado externamente, pero incluye dos menús contextuales
que implementan las funciones principales de administración de archivos como crear,
renombrea o eliminar archivos o carpetas.
Para activar los menús contextuales por defecto, se debe activar las variables:

* InternalPopupFolder -> TRUE, para activar el menú contextual interno de carpetas
* InternalPopupFiles -> TRUE, para activar el menú contextual interno de archivos

Los eventos son:

* OnMenuOpenFile    : TevClickOnFile;  //Se seleccionó "Abrir" desde el menú
* OnDoubleClickFile : TevClickOnFile;  //Se hizo doble click en un archivo
* OnRightClickFile  : TevClickOnFile;  //Click derecho en archivo
* OnRightClickFolder: TevClickOnFile;  //Click derecho en carpeta
* OnKeyEnterOnFile  : TevClickOnFile;  //Se presionó la tecla enter en un archivo

Cuando se usa el menú contextual interno, conviene iniciar las variables:

* NewFileName  : string;       //nombre por defecto de archivo nuevo
* NewFolderName: string;       //nombre pord efecto de carpeta nueva

Para definir el filtro del explorador se puede usar un código similar a este:

  ExplorArch.Filter.Items.Add('*.txt,*.def');  //los filtros se separan por comas
  ExplorArch.Filter.Items.Add('*');  //para seleccionar todos
  ExplorArch.Filter.ItemIndex:=0;    //selecciona la primera opción por defecto

}
unit FrameArcExplor;
{$mode objfpc}{$H+}
interface
uses
  Classes, {$IFDEF MSWINDOWS} Windows, {$ENDIF} SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls,
  LCLType, Menus, Masks, LazUTF8, Dialogs, Graphics, Globales, strutils,
  MisUtils;
var
  NEW_FILE_NAME: string;
  FOLDER_NAME  : string;
  TXT_EMPTY    : string;
  TXT_FOLD     : string;
  TXT_UNFOLD   : string;
  TXT_NOTDELFOL: string;
  TXT_DELFILE  : string;

type
  TNodeType = (ntyDrive, ntyFile, ntyFolder);

  { TExplorNode }

  TExplorNode = class(TTreeNode)  //tipo de nodo personalizado para el arbol
  public
    NodType : TNodeType;
    function Path: string;       //ruta del archivo o carpeta
    function IsFile: boolean;    //indica si el nodo es archivo (no carpeta)
    function IsFolder: boolean;  //indica si el nodo es carpeta
//    function IsDrive: boolean;  //indica si el nodo es una unidad
  end;

  TEvArcExp_Abrir= procedure(arc0: string) of object;
  TevClickOnFile = procedure(nod: TExplorNode) of object;

  { TfrmArcExplor }

  TfrmArcExplor = class(TFrame)
  published
    Filter: TComboBox;
    ImageList2: TImageList;
    MenuItem1: TMenuItem;
    mnFilCreCopFrom: TMenuItem;
    mnFilRefrescar: TMenuItem;
    mnFolChanName: TMenuItem;
    mnFolDelete: TMenuItem;
    mnFolRefresh: TMenuItem;
    MenuItem3: TMenuItem;
    mnFolNewFolder: TMenuItem;
    mnFilChanName: TMenuItem;
    mnFilDelete: TMenuItem;
    mnFolExpandCol: TMenuItem;
    mnFolOpenInExplor: TMenuItem;
    mnFolNewFile: TMenuItem;
    mnFilOpen: TMenuItem;
    PopupFolder: TPopupMenu;
    PopupFile: TPopupMenu;
    TreeView1: TTreeView;
    procedure mnFilCreCopFromClick(Sender: TObject);
    procedure mnFilOpenClick(Sender: TObject);
    procedure mnFilChanNameClick(Sender: TObject);
    procedure mnFilRefrescarClick(Sender: TObject);
    procedure mnFolOpenInExplorClick(Sender: TObject);
    procedure mnFilDeleteClick(Sender: TObject);
    procedure mnFolChanNameClick(Sender: TObject);
    procedure mnFolDeleteClick(Sender: TObject);
    procedure mnFolExpandColClick(Sender: TObject);
    procedure mnFolNewFileClick(Sender: TObject);
    procedure mnFolNewFolderClick(Sender: TObject);
    procedure mnFolRefreshClick(Sender: TObject);
    procedure PopupFolderPopup(Sender: TObject);
    procedure TreeView1CreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure TreeView1DblClick(Sender: TObject);
    procedure TreeView1EditingEnd(Sender: TObject; Node: TTreeNode;
      Cancel: Boolean);
    procedure TreeView1Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FilterChange(Sender: TObject);
    procedure TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

{    procedure acPArcAbrirExecute(Sender: TObject);
    procedure acPArcAbrNueExecute(Sender: TObject);
    procedure acPArcCamNomExecute(Sender: TObject);
    procedure acPArcElimExecute(Sender: TObject);
    procedure acPArcNueCarExecute(Sender: TObject);
    procedure acPArcNueConExecute(Sender: TObject);
    procedure acPArcNueEncExecute(Sender: TObject);
    procedure acPArcRefresExecute(Sender: TObject);}
  private
    FTextColor: TColor;
    NombNodEdi: String;    //nombre actual de nodo en edición
    procedure ActualPanelArc;
    function AddNodeTo(ParentNode: TTreeNode; const S: string): TExplorNode;
    procedure ExpandirNodArc(Node: TTreeNode; expan: Boolean);
    procedure LeeFilt(lfil: TStringList);
    procedure LeerDirectorio(Item0: TTreeNode; filtro: TStringList;
      expandir: boolean=false);
    function NodRuta(rut: string): TExplorNode;
    procedure SetTextColor(AValue: TColor);
    procedure TreeView1AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
  public
    NewFileName  : string;       //nombre por defecto de archivo nuevo
    NewFolderName: string;       //nombre pord efecto de carpeta nueva
    InternalPopupFolder: boolean; //perimte activar el menú contextual interno
    InternalPopupFile: boolean;   //perimte activar el menú contextual interno
    curNod       : TExplorNode;   //Nodo actual. Se usa como variable temporal
    //Eventos
    OnMenuOpenFile    : TevClickOnFile;  //Se seleccionó "Abrir" desde el menú
    OnDoubleClickFile : TevClickOnFile;  //Se hizo doble click en un archivo
    OnRightClickFile  : TevClickOnFile;  //Click derecho en archivo
    OnRightClickFolder: TevClickOnFile;  //Click derecho en carpeta
    OnKeyEnterOnFile  : TevClickOnFile;  //Se presionó la tecla enter en un archivo
    OnCloseFile       : TevClickOnFile;  //Se pide cerrar un archivo
//    OnKeyDown         : TKeyEvent;       //TEcla pulsada
    property TextColor: TColor read FTextColor write SetTextColor;
    function SelectedNode: TExplorNode;
    function SelectedFile: TExplorNode;
    procedure LocateFileOnTree(arch8: string);
    constructor Create(AOwner: TComponent) ; override;
    procedure SetLanguage;
  end;

implementation
{$R *.lfm}
procedure TfrmArcExplor.SetLanguage;
//Rutina de traducción
begin
  {$I ..\language\tra_FrameArcExplor.pas}
  //Inicia propiedades
  NewFileName := NEW_FILE_NAME;
  NewFolderName := FOLDER_NAME;
end;
procedure TrozaRuta(rut: string; lrut: TStringList); //devuelve una ruta trozada
begin
  if rut = '' then exit;  //no hay ruta
  if Pos('\', rut) = 0 then begin
    //solo tiene un nodo
    lrut.Add(rut);
    exit;
  end;
  //extrae los nodos de la ruta
  lrut.Delimiter:='\';
  lrut.StrictDelimiter:=true;
  lrut.DelimitedText:=rut;   //separa
end;

{ TExplorNode }
function TExplorNode.Path: string;
begin
  Result := StringReplace(GetTextPath,'/', PathDelim,[rfReplaceAll]);
//  Result := UTF8ToSys(Result);   //devuelve en codificación del sistema
end;
function TExplorNode.IsFile: boolean;
begin
  Result := NodType = ntyFile;
end;
function TExplorNode.IsFolder: boolean;
begin
  Result := NodType = ntyFolder;
end;

{ TfrmArcExplor }
//funciones básicas
function TfrmArcExplor.SelectedNode: TExplorNode;
//Lee el nodo seleccionado actualmente. Si no hay ninguno seleccionado, devuelve NIL.
var
  nod: TTreeNode;
begin
  nod := TreeView1.Selected;   //lee seleccionado
  if nod = nil then exit(nil);     //verifica
  Result := TExplorNode(nod);
end;
function TfrmArcExplor.SelectedFile: TExplorNode;
//Lee el nodo seleccionado. Si no hay ninguno seleccionado o no es archivo, devuelve NIL.
var
  nod: TExplorNode;
begin
  nod := SelectedNode;
  if nod = nil then exit(nil);     //verifica
  if not nod.IsFile then exit(nil);     //verifica
  Result := TExplorNode(nod);
end;
function TfrmArcExplor.NodRuta(rut: string): TExplorNode;
//Devuelve el nodo a partir de la ruta completa
var
  nod   : TTreeNode;
  cadNodos: TStringList;  //cadena de los nodos
  cadNod: string;
  nivel: integer;
  encontro: boolean;
begin
  Result := nil;  //valor por defecto
  cadNodos := TStringList.Create;
  try
    TrozaRuta(rut, cadNodos);
    //desenrrolla en la ruta
    nivel := 0;
    for cadNod in cadNodos do begin
      //busca nodo
      encontro := false;
      for nod in TreeView1.Items do begin
        if nod.Level = nivel then
            if UpCase(nod.Text) = UpCase(cadNod) then begin
              Result := TExplorNode(nod);
              encontro := true;
              break;  //sale del for, para buscar siguiente nivel
            end;
      end;
      if not encontro then exit;  //no vale la pena seguir buscando
      inc(nivel);
    end;
  finally
    cadNodos.Free;
  end;
end;
procedure TfrmArcExplor.SetTextColor(AValue: TColor);
begin
  FTextColor := AValue;
  Invalidate;
end;
procedure TfrmArcExplor.TreeView1AdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
begin
  with TreeView1.Canvas do begin
     if Node.Level = 0 then  begin
       Font.Style := [fsBold, fsItalic];
     end else begin
       Font.Style := [];
     end;
     font.Color:= FTextColor;
     DefaultDraw := true;   //Para que siga ejecutando la rutina de dibujo
  end;
end;
procedure TfrmArcExplor.LeeFilt(lfil: TStringList);
//Devuelve los filtros para nombres de archivos
begin
  lfil.Delimiter:=',';  //intrepreta como delimitador
  lfil.StrictDelimiter:=true;
  lfil.DelimitedText:=Filter.Text;
end;
procedure TfrmArcExplor.ExpandirNodArc(Node: TTreeNode; expan: Boolean);
//Lee el contenido de un nodo y permite expandirlo. Usa el filtro actual.
var
  filtros : TStringList;
begin
   filtros:=TStringList.Create;
   LeeFilt(filtros);  //lee los filtros
   LeerDirectorio(Node, filtros, expan);
   filtros.Free;
end;
procedure TfrmArcExplor.TreeView1Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  //solo actualiza si no se ha expandido antes
//  if (node.Count = 1) and (node.Items[0].Text = ' ') then begin
    ExpandirNodArc(TExplorNode(Node), false);
//  end;
end;
procedure TfrmArcExplor.LeerDirectorio(Item0: TTreeNode; filtro: TStringList;
                                       expandir: boolean = false);
{Lee el contenido de un directorio, y lo agrega al nodo indicado. Aplica los filtros
 indicados en "filtros" para el nombre de los archivos, no de carpetas}
  Function Coincide(txt: string): boolean;
  //verifica si la cadena coincide con alguno de los filtros
  var filt_n   : string;
  begin
    for filt_n in filtro do
      if MatchesMask(txt,filt_n) then Exit(true);
    Exit(false);
  end;
var
  SearchRec: TSearchRec;
  Item     : TTreeNode;
  nomArc   : string;
  ultFol   : TTreeNode = nil;  //última carpeta agegada
  directorio: String;
begin
  directorio := TExplorNode(Item0).Path;
  directorio := UTF8ToSys(directorio);
  TreeView1.Items.BeginUpdate;
  Screen.Cursor := crHourGlass;  //cambia puntero, para indicar trabajo
  if Item0 <> nil then Item0.DeleteChildren;
  try
    if directorio[Length(directorio)] <> PathDelim then directorio +=  PathDelim;
    if FindFirst(directorio + '*.*', faDirectory, SearchRec) = 0 then begin
      repeat
        nomArc := SysToUTF8(SearchRec.Name);
        if SearchRec.Attr and faDirectory = faDirectory then
        begin  //directorio
          if nomArc[1] <> '.' then begin
            if ultFol = nil then
              //es el primer directorio que se va a agregar.
              ultFol := TreeView1.Items.AddChild(Item0, nomArc)
            else
              //se agrega el directorio, después del último agregado
              ultFol := TreeView1.Items.InsertBehind(ultFol, nomArc);
            ultFol.HasChildren:=true;  //para que se pueda abrir
            ultFol.ImageIndex:=0;  //fija ícono
            ultFol.SelectedIndex:=0;  //fija ícono cuando está seleccionado
            TExplorNode(ultFol).NodType:=ntyFolder;  //tipo de nodo
          end;
        end else begin //archivo
          //los archivos van al final
          if Coincide(nomArc) then begin
            Item := TreeView1.Items.AddChild(Item0, nomArc);
            if AnsiEndsText('.psql',nomArc) then begin
              Item.ImageIndex:=4;       //ícono de archivo
              Item.SelectedIndex:=4;    //fija ícono cuando está seleccionado
            end else if AnsiEndsText('.pdef',nomArc) then begin
                Item.ImageIndex:=5;       //ícono de archivo
                Item.SelectedIndex:=5;    //fija ícono cuando está seleccionado
            end else if AnsiEndsText('.txt',nomArc) then begin
                Item.ImageIndex:=1;       //ícono de archivo
                Item.SelectedIndex:=1;    //fija ícono cuando está seleccionado
            end else if AnsiEndsText('.exe',nomArc) or AnsiEndsText('.bat',nomArc) then begin
                Item.ImageIndex:=7;       //ícono de archivo
                Item.SelectedIndex:=7;    //fija ícono cuando está seleccionado
            end else if AnsiEndsText('.xls',nomArc) or AnsiEndsText('.xlsx',nomArc) then begin
                Item.ImageIndex:=9;       //ícono de archivo
                Item.SelectedIndex:=9;    //fija ícono cuando está seleccionado
            end else if AnsiEndsText('.doc',nomArc) or AnsiEndsText('.docx',nomArc) then begin
                Item.ImageIndex:=10;       //ícono de archivo
                Item.SelectedIndex:=10;    //fija ícono cuando está seleccionado
            end else if AnsiEndsText('.pas',nomArc) then begin
                Item.ImageIndex:=12;       //ícono de archivo
                Item.SelectedIndex:=12;    //fija ícono cuando está seleccionado
            end else if AnsiEndsText('.bmp',nomArc) or AnsiEndsText('.jpg',nomArc) or
                        AnsiEndsText('.png',nomArc) or AnsiEndsText('.gif',nomArc)then begin
                Item.ImageIndex:=11;       //ícono de archivo
                Item.SelectedIndex:=11;    //fija ícono cuando está seleccionado
            end else begin
              Item.ImageIndex:=6;       //ícono de archivo
              Item.SelectedIndex:=6;    //fija ícono cuando está seleccionado
            end;
            TExplorNode(Item).NodType:=ntyFile;
          end;
        end;
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
    if (Item0 <> nil) and (Item0.Count=0) then
      //no hubo elementos. Agrega etiqueta
      TreeView1.Items.AddChild(Item0, TXT_EMPTY);
  finally
//    TreeView1.Items.AddChild(Item0, '<error>'); //indica error
    if expandir and (Item0 <> nil) then Item0.Expand(false);
    Screen.Cursor := crDefault;  //retorna puntero
    TreeView1.Items.EndUpdate;
  end;
end;
procedure TfrmArcExplor.ActualPanelArc;
{Actualiza el contenido del panel de archivos, refrescando todos los nodos visibles y
 expandidos}
var
  nod   : TTreeNode;
  nodEx : TExplorNode;
  nods  : string;
  filtros : TStringList;
  nodExpan: TStringList;  //nodos expandidos
begin
  filtros:=TStringList.Create;
  nodExpan:= TStringList.Create;
  LeeFilt(filtros);  //lee los filtros
  for nod in TreeView1.Items do
    if nod.Visible and nod.Expanded then begin
      //mantiene expansión
//      LeerDirectorio(nod, filtros, true);
      nodExpan.Add(TExplorNode(nod).Path);  //agrega nodo
    end;
  //refresca solo los que estaban expandidos
  for nods in nodExpan do begin
//    if nod = nil then ShowMessage('nil') else ShowMessage(nod.Text);
    nodEx := nodRuta(nods);  //toma nodo a partir de su ruta
    if nodEx = nil then break;  //no debería pasar
    LeerDirectorio(nodEx, filtros, true);
  end;
  nodExpan.Free;
  filtros.Free;
end;
procedure TfrmArcExplor.FilterChange(Sender: TObject);  //Evento del combo
begin
  ActualPanelArc;  //actualiza
end;
//Eventos de TreeView1
procedure TfrmArcExplor.TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  nodEx: TExplorNode;
begin
  if Button = mbRight then begin
    //se soltó click derecho
    nodEx := SelectedNode;   //lee seleccionado
    if nodEx = nil then exit;     //verifica
    //procesa eventos
    case nodEx.NodType of
    ntyFile  : begin
        if InternalPopupFile then PopupFile.PopUp;
      if OnRightClickFile<>nil   then OnRightClickFile(nodEx);
    end;
    ntyFolder: begin
      if InternalPopupFolder then PopupFolder.PopUp;
      if OnRightClickFolder<>nil then OnRightClickFolder(nodEx);
    end;
    end;
  end;
end;
procedure TfrmArcExplor.TreeView1CreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TExplorNode;  //define neustra clase de nodo
end;
procedure TfrmArcExplor.TreeView1DblClick(Sender: TObject); //Abre el archivo solicitado
var
  nodEx: TExplorNode;
begin
  nodEx := SelectedFile;   //lee seleccionado
  if nodEx = nil then exit;     //verifica
  if OnDoubleClickFile<>nil then OnDoubleClickFile(nodEx); //dispara evento
end;
procedure TfrmArcExplor.TreeView1EditingEnd(Sender: TObject; Node: TTreeNode; Cancel: Boolean);
//Termina la edición del nombre de un nodo
var
  NuevoNom: String;
  nodEx: TExplorNode;
begin
  NuevoNom := TExplorNode(Node).Path;
  if NombNodEdi <> NuevoNom then
    try
      RenameFile(NombNodEdi, NuevoNom);
    finally
    end;
  //refresca
  if Node.Parent<> nil then
    if node.Parent.Expanded then ExpandirNodArc(node.parent, true);   //refresca
  //selecciona el nodo
  nodEx := nodRuta(NuevoNom);  //toma a partir de su ruta, porque al refrescar se pierde la ref.
  if nodEx <> nil then NodEx.Selected:=true;
end;
procedure TfrmArcExplor.TreeView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  nodEx: TExplorNode;
  a: TPoint;
  relPos: TPoint;
begin
  //Genera el evento en el frame, porque el frame nunca tendrá enfoque
  if OnKeyDown<>nil then OnKeyDown(Sender, Key, Shift);
  //procesa teclas especiales
  if Key = VK_F2 then mnFilChanNameClick(Self);
  if Key = VK_DELETE then begin
    nodEx := SelectedNode;   //lee seleccionado
    if nodEx = nil then exit;     //verifica
    if nodEx.IsFile then mnFilDeleteClick(Self);
    if nodEx.IsFolder then mnFolDeleteClick(Self);
  end;
  if Key = VK_F5 then ActualPanelArc;
  if Key = VK_RETURN then begin
    nodEx := SelectedFile;   //lee seleccionado
    if nodEx = nil then exit;     //verifica
    if OnKeyEnterOnFile<>nil then OnKeyEnterOnFile(nodEx);
  end;
  if (Shift = [ssCtrl]) and (Key = VK_F4) then begin
    if nodEx = nil then exit;     //verifica
    if OnCloseFile<>nil then OnCloseFile(nodEx);
  end;
  if Key = VK_APPS then begin
    //Menú Contextual
    nodEx := SelectedNode;   //lee seleccionado
    relPos.x := nodEx.DisplayRect(true).Left;
    relPos.y := nodEx.DisplayRect(true).Bottom;
    a := TreeView1.ClientToScreen(relPos);
    if nodEx = nil then exit;     //verifica
    if nodEx.IsFile then PopupFile.PopUp(a.x, a.y);
    if nodEx.IsFolder then PopupFolder.PopUp(a.x, a.y);
  end;
end;
procedure TfrmArcExplor.LocateFileOnTree(arch8: string);
//Configura el árbol de archivos para ubicar la ruta del archivo indicado.
//El parámetro "arch8", debe estar en UTF-8
var nod: TTreeNode;
   cadNodos: TStringList;  //cadena de los nodos
   cadNod: string;
   nivel: integer;
   encontro: boolean;
begin
  if ExtractFilePath(arch8) = '' then begin
    //no tiene ruta, es relativo al aruta actual
    arch8 := ExtractFilePath(Application.ExeName) + arch8;
  end;
  cadNodos := TStringList.Create;
  TrozaRuta(arch8, cadNodos);
  //desenrrolla en la ruta
  nivel := 0;
  for cadNod in cadNodos do begin
    //busca nodo
    encontro := false;
    for nod in TreeView1.Items do begin
      if nod.Level = nivel then
        if UpCase(nod.Text) = UpCase(cadNod) then begin
          if TExplorNode(nod).NodType in [ntyFolder,ntyDrive] then  //es folder o unidad
            ExpandirNodArc(nod, true)   //expande para ubicar
          else  begin  //debe ser el archivo buscado
            nod.Selected:=true;
          end;
          encontro := true;
          break;  //sale del for, para buscar siguienet nivel
        end;
    end;
    if not encontro then begin
      cadNodos.Destroy;
      exit;  //no vale la pena seguir buscando
    end;
    inc(nivel);
  end;
  cadNodos.Destroy;
end;
function TfrmArcExplor.AddNodeTo(ParentNode: TTreeNode; const S: string): TExplorNode;
//Agrega un nodo al TreeView del frame. Devuelve una referencia de tipo 'TExplorNode'
begin
  Result := TExplorNode(TreeView1.Items.AddChild(ParentNode, S));
end;
constructor TfrmArcExplor.Create(AOwner: TComponent);
var
//  Nodo: TTreeNode;
  Drive: Char;
  DriveLetter: string;
  unidades: TStringList;
  Nodo: TExplorNode;
begin
  inherited Create(AOwner);
  //asigna eventos
  TreeView1.OnCreateNodeClass:=@TreeView1CreateNodeClass;
  TreeView1.OnExpanding:=@TreeView1Expanding;  //asigna evento
  TreeView1.OnDblClick:= @TreeView1DblClick;
  TreeView1.OnEditingEnd := @TreeView1EditingEnd;
  TreeView1.OnKeyDown := @TreeView1KeyDown;
  TreeView1.OnMouseUp:=@TreeView1MouseUp;
  TreeView1.Images:=ImageList2;   //asigna íconos
  TreeView1.Options:= TreeView1.Options + [tvoReadOnly];  //para que no permita editar el nodo

  //Crea lista de unidades
  unidades := TStringList.Create;
  {$IFDEF MSWINDOWS}
  for Drive := 'A' to 'Z' do
  begin
    DriveLetter := Drive + ':\';
    case GetDriveType(PChar(DriveLetter)) of
     DRIVE_REMOVABLE: unidades.Add(DriveLetter + ' Floppy Drive');
     DRIVE_FIXED:     unidades.Add(DriveLetter + ' Fixed Drive');
     DRIVE_REMOTE:    unidades.Add(DriveLetter + ' Network Drive');
     DRIVE_CDROM:     unidades.Add(DriveLetter + ' CD-ROM Drive');
     DRIVE_RAMDISK:   unidades.Add(DriveLetter + ' RAM Disk');
    end;
  end;
  {$ENDIF}
  //prepara nodos iniciales
  TreeView1.Items.Clear;
  for DriveLetter in unidades do begin
    //crea nodo de unidad
    Nodo := AddNodeTo(nil, DriveLetter[1] + ':') as TExplorNode;
    Nodo.HasChildren:=true;  //para que se expanda
    Nodo.NodType := ntyDrive;  //marca como unidad
    if DriveLetter[5] = 'C' then begin
      Nodo.ImageIndex:=3;  //fija ícono de CD
      Nodo.SelectedIndex:=3;
    end else begin
      Nodo.ImageIndex:=2;  //fija ícono de Unidad
      Nodo.SelectedIndex:=2;
    end;
  end;
  unidades.Free;
  TreeView1.OnAdvancedCustomDrawItem := @TreeView1AdvancedCustomDrawItem;
  TreeView1.Options := TreeView1.Options - [tvoThemedDraw];
  InternalPopupFolder := false;  //desactiva el menú interno
  InternalPopupFile := false;  //desactiva el menú interno
end;
//////////////////////////// Acciones /////////////////////////////////
procedure TfrmArcExplor.PopupFolderPopup(Sender: TObject);
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  if curNod.Expanded then mnFolExpandCol.Caption:= TXT_FOLD
  else mnFolExpandCol.Caption:= TXT_UNFOLD;
end;
procedure TfrmArcExplor.mnFolExpandColClick(Sender: TObject);
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  curNod.Expanded := not curNod.Expanded;
  if curNod.Expanded then mnFolExpandCol.Caption:= TXT_FOLD
  else mnFolExpandCol.Caption:= TXT_UNFOLD;
end;
procedure TfrmArcExplor.mnFolOpenInExplorClick(Sender: TObject);
var
  tmp: String;
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  //Hay problemas al abrir rutas con tildes
  tmp := curNod.Path;
//  tmp := UTF8ToSys(curNod.Path);
  Exec('explorer', '"' + tmp + '"');
end;
procedure TfrmArcExplor.mnFolNewFileClick(Sender: TObject);
var
  archivo: string;
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  if not curNod.IsFolder then exit;  //solo permite en carpetas
  //crea archivo nuevo
  try
    archivo := GetNewFileName(curNod.Path +  PathDelim + NewFilename);
    archivo := UTF8ToSys(archivo);
    if not FileExists(archivo) then
      StringToFile('', archivo);
  finally
  end;
  if curNod.Expanded then ExpandirNodArc(curNod, true);   //refresca
end;
procedure TfrmArcExplor.mnFolNewFolderClick(Sender: TObject);
var
  carpeta: string;
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  if not curNod.IsFolder then exit;  //solo permite en carpetas
  //crea carpeta
  try
    carpeta := GetNewFolderName(curNod.Path + PathDelim + NewFolderName);
    carpeta := UTF8ToSys(carpeta);
    If not DirectoryExists(carpeta) then
       CreateDir(carpeta);
  finally
  end;
  if curNod.Expanded then ExpandirNodArc(curNod, true);   //refresca
end;
procedure TfrmArcExplor.mnFolChanNameClick(Sender: TObject);
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  curNod.EditText;           //inicia edición
  NombNodEdi := curNod.Path; //Guarda nombre de nodo editado
end;
procedure TfrmArcExplor.mnFolDeleteClick(Sender: TObject);
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  MsgExc(TXT_NOTDELFOL);
end;
procedure TfrmArcExplor.mnFolRefreshClick(Sender: TObject);
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  if curNod.Visible and curNod.Expanded then
    ExpandirNodArc(curNod, true);   //mantiene expansión
end;
// File actions
procedure TfrmArcExplor.mnFilOpenClick(Sender: TObject);
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  if OnMenuOpenFile<>nil then OnMenuOpenFile(curNod);
end;
procedure TfrmArcExplor.mnFilCreCopFromClick(Sender: TObject);
var
  newFile, archivo: String;
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  if not curNod.IsFile then exit;  //no es archivo
  try
    archivo := UTF8ToSys(curNod.Path);
    if FileExists(archivo) then begin
      newFile := GetNewFileName(archivo);
      CopyFile(archivo, newFile);
    end;
  finally
  end;
  if curNod.Parent.Expanded then ExpandirNodArc(curNod.parent, true);   //refresca
end;
procedure TfrmArcExplor.mnFilChanNameClick(Sender: TObject);
begin
  mnFolChanNameClick(Sender);  //funciona comoa archivo
end;
procedure TfrmArcExplor.mnFilDeleteClick(Sender: TObject);
var
  archivo: String;
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  if not curNod.IsFile then exit;  //no es archivo
  //eliminar archivo
  if MsgYesNo(TXT_DELFILE, [curNod.Text]) <> 1 then exit;
  try
    archivo := UTF8ToSys(curNod.Path);
    if FileExists(archivo) then
//      DeleteToBin(PChar(archivo));  //envía a papelera
      DeleteFile(archivo);
  finally
  end;
  if curNod.Parent.Expanded then ExpandirNodArc(curNod.parent, true);   //refresca
end;
procedure TfrmArcExplor.mnFilRefrescarClick(Sender: TObject);
begin

end;

end.

