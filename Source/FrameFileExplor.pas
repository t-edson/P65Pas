{
FrameFileExplor
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

* NewFileName  : string;       //Nombre por defecto de archivo nuevo
* NewFolderName: string;       //Nombre por defecto de carpeta nueva

Para definir el filtro del explorador se puede usar un código similar a este:

  ExplorArch.Filter.Items.Add('*.txt,*.def');  //los filtros se separan por comas
  ExplorArch.Filter.Items.Add('*');  //para seleccionar todos
  ExplorArch.Filter.ItemIndex:=0;    //selecciona la primera opción por defecto

}
unit FrameFileExplor;
{$mode objfpc}{$H+}
interface
uses
  Classes, {$IFDEF MSWINDOWS} Windows, {$ENDIF} SysUtils, FileUtil, Forms, Controls,
  StdCtrls, ComCtrls, LCLType, Menus, Masks, LazUTF8, Dialogs, Graphics, Globales,
  strutils, MisUtils;

type
  TNodeType = (ntyDrive, ntyFile, ntyFolder);

  { TExplorNode }
  TExplorNode = class(TTreeNode)  //tipo de nodo personalizado para el arbol
  private
  public
    txtPath : string;            //Campo que guarda ubicación física en disco.
    NodType : TNodeType;         //Tipo de nodo.
    function GetPath: string;    //Ruta del archivo o carpeta
    function IsFile: boolean;    //Indica si el nodo es archivo (no carpeta)
    function IsFolder: boolean;  //Indica si el nodo es carpeta
//    function IsDrive: boolean;  //Indica si el nodo es una unidad
  end;

  TEvArcExp_Abrir= procedure(arc0: string) of object;
  TevClickOnFile = procedure(nod: TExplorNode) of object;

  { TfraArcExplor }

  TfraArcExplor = class(TFrame)
  published
    btnOpenFolder: TButton;
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
    procedure btnOpenFolderClick(Sender: TObject);
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
  private    //Funciones básicas
    function AddNodeTo(ParentNode: TTreeNode; const txt: string;
      nodType: TNodeType): TExplorNode;
    function AddNodeBehind(friendNode: TTreeNode; const txt: string;
      nodType: TNodeType): TExplorNode;
    function SelectedNode: TExplorNode;
    function SelectedFile: TExplorNode;
  public
    procedure LocateFileOnTree(arch8: string);
    function NodRuta(rut: string): TExplorNode;
  private
    FTextColor: TColor;
    NombNodEdi: String;    //nombre actual de nodo en edición
    procedure ActualPanelArc;
    procedure ExpandirNodArc(Node: TTreeNode; expan: Boolean);
    procedure LeeFilt(lfil: TStringList);
    procedure LeerDirectorio(Item0: TTreeNode; filtro: TStringList;
      expandir: boolean=false);
    procedure SetTextColor(AValue: TColor);
    procedure TreeView1AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
  public
    //Eventos
    OnMenuOpenFile    : TevClickOnFile;  //Se seleccionó "Abrir" desde el menú
    OnDoubleClickFile : TevClickOnFile;  //Se hizo doble click en un archivo
    OnRightClickFile  : TevClickOnFile;  //Click derecho en archivo
    OnRightClickFolder: TevClickOnFile;  //Click derecho en carpeta
    OnKeyEnterOnFile  : TevClickOnFile;  //Se presionó la tecla enter en un archivo
    OnCloseFile       : TevClickOnFile;  //Se pide cerrar un archivo
    OnOpenDirectory   : procedure of object;  //Se pide abrir un folder
  public
    NewFileName  : string;       //nombre por defecto de archivo nuevo
    NewFolderName: string;       //nombre pord efecto de carpeta nueva
    InternalPopupFolder: boolean; //perimte activar el menú contextual interno
    InternalPopupFile: boolean;   //perimte activar el menú contextual interno
    curNod       : TExplorNode;   //Nodo actual. Se usa como variable temporal
    property TextColor: TColor read FTextColor write SetTextColor;
    procedure Init(currPath: string);
    constructor Create(AOwner: TComponent) ; override;
  end;

implementation
{$R *.lfm}
resourcestring
  NEW_FILE_NAME   = 'newfile.pas';
  FOLDER_NAME     = 'folder';
  TXT_EMPTY       = '<empty>';
  TXT_FOLD        = 'C&ollapse';
  TXT_UNFOLD      = 'E&xpand';
  TXT_NOTDELFOL   = 'Cannot delete folders';
  TXT_DELFILE     = 'Delete file "%s"?';
  TXT_DIR_NOTEXIST = 'Directory doesn''t exist.';

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
function TExplorNode.GetPath: string;
{Devuelve una cadena con una ruta de directorio completa, construida a partir de la
estructura de los nodos.
Esta rutina es similar a TTreeNode.GetTextPath() pero no se usa esa rutina, porque esa
rutina reconstruye la ruta a partir del "Caption" de todos los nodos, así que se fuerza
a usar nodos con nombres exactos de directorios.
Noostros en esta rutina usamos un campo adicional (TExplorNode.txtPath) para guardar
información de directorio, mientras que podemos poner cualquier valor en el título del
nodo.}
var
  Node: TTreeNode;
begin
  Result := '';
  Node := Self;
  while Assigned(Node) do begin
    if Result <> '' then begin
      Result := DirectorySeparator + Result;
    end;
    //Result := Node.Text + Result;
    Result := TExplorNode(Node).txtPath + Result;
    Node := Node.Parent;
  end;
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

{ TfraArcExplor }
//Funciones básicas
function TfraArcExplor.AddNodeTo(parentNode: TTreeNode; const txt: string;
                                 nodType: TNodeType): TExplorNode;
{Agrega un nodo al TreeView del frame. El nodo se agrega como nodo hijo de "ParentNode".
Devuelve una referencia de tipo 'TExplorNode'.
Esta función y AddNodeBehind() deberían ser las únicas rutinas para agregar nodos a
TreeView1.
}
begin
  Result := TExplorNode(TreeView1.Items.AddChild(parentNode, txt));
  Result.txtPath := txt;    //Por defecto
  Result.NodType := nodType;
end;
function TfraArcExplor.AddNodeBehind(friendNode: TTreeNode; const txt: string;
                                 nodType: TNodeType): TExplorNode;
{Agrega un nodo al TreeView del frame. El nodo se agrega al lado de "friendNode".
Devuelve una referencia de tipo 'TExplorNode'.
Esta función y AddNodeBehind() deberían ser las únicas rutinas para agregar nodos a
TreeView1.
}
begin
  Result := TExplorNode(TreeView1.Items.InsertBehind(friendNode, txt));
  Result.txtPath := txt;    //Por defecto
  Result.NodType := nodType;
end;
function TfraArcExplor.SelectedNode: TExplorNode;
//Lee el nodo seleccionado actualmente. Si no hay ninguno seleccionado, devuelve NIL.
var
  nod: TTreeNode;
begin
  nod := TreeView1.Selected;   //lee seleccionado
  if nod = nil then exit(nil);     //verifica
  Result := TExplorNode(nod);
end;
function TfraArcExplor.SelectedFile: TExplorNode;
//Lee el nodo seleccionado. Si no hay ninguno seleccionado o no es archivo, devuelve NIL.
var
  nod: TExplorNode;
begin
  nod := SelectedNode;
  if nod = nil then exit(nil);     //verifica
  if not nod.IsFile then exit(nil);     //verifica
  Result := TExplorNode(nod);
end;
procedure TfraArcExplor.LocateFileOnTree(arch8: string);
//Configura el árbol de archivos para ubicar la ruta del archivo indicado.
//El parámetro "arch8", debe estar en UTF-8
   function ExtractNodePath(var fPath: string; nod: TExplorNode): boolean;
   {Returns TRUE if the full Path of "nod" can be extracted of "fPath".}
   var
     nodpath: string;
   begin
     nodpath := nod.GetPath;
     if copy(fPath, 1, length(nodpath)) = nodpath then begin
       fPath := copy(fPath, length(nodpath)+2);  //Extract path including "/"
       exit(true);
     end else begin
       exit(false);
     end;
   end;
   function ExtractNodeText(var fPath: string; nod: TExplorNode): boolean;
   {Returns TRUE if the text of "nod" can be extracted of "fPath".}
   var
     nodpath: string;
   begin
     nodpath := nod.TxtPath;
     if copy(fPath, 1, length(nodpath)) = nodpath then begin
       fPath := copy(fPath, length(nodpath)+2);  //Extract path including "/"
       exit(true);
     end else begin
       exit(false);
     end;
   end;
   function ExtractNodePathInChildren(var fPath: string; nod: TExplorNode): TTreeNode;
   {Similar to ExtractNodePath, but find in the children nodes of "nod". If found returns
   the node where it's found, otherwise returns NIL.}
   var
     nod2: TTreeNode;
   begin
     if nod = nil then exit(nil);
     if not nod.HasChildren then exit(nil);
     ExpandirNodArc(nod, true);
     nod2 := nod.GetFirstChild;
     while nod2 <> nil do begin
       if ExtractNodeText(fPath, TExplorNode(nod2)) then begin
         exit(nod2);
       end;
       nod2 := nod2.GetNextSibling
     end;
     exit(nil);
   end;
   function LocateInNode(nod: TExplorNode; fPath: string): boolean;
   var
     nod2: TTreeNode;
   begin
     if ExtractNodePath(fPath, nod) then begin
       //Could be
       while fPath<>'' do begin
         nod2 := ExtractNodePathInChildren(fPath, nod);
         if nod2 = nil then begin
           break;
         end else begin
           nod := TExplorNode(nod2);
           continue;
         end;
       end;
       if fPath='' then begin
         nod.Selected := true;
         exit(true);
       end else begin
         exit(false);
       end;
     end else begin
       exit(false);
     end;
   end;
var
  nod: TTreeNode;
begin
  if ExtractFilePath(arch8) = '' then begin
    //no tiene ruta, es relativo al aruta actual
    arch8 := ExtractFilePath(Application.ExeName) + arch8;
  end;
//  cadNodos := TStringList.Create;
//  TrozaRuta(arch8, cadNodos);
//  //desenrrolla en la ruta
//  nivel := 0;
//  for cadNod in cadNodos do begin
//    //busca nodo
//    encontro := false;
//    for nod in TreeView1.Items do begin
//      if nod.Level = nivel then
//        if UpCase(nod.Text) = UpCase(cadNod) then begin
//          if TExplorNode(nod).NodType in [ntyFolder,ntyDrive] then  //es folder o unidad
//            ExpandirNodArc(nod, true)   //expande para ubicar
//          else  begin  //debe ser el archivo buscado
//            nod.Selected:=true;
//          end;
//          encontro := true;
//          break;  //sale del for, para buscar siguienet nivel
//        end;
//    end;
//    if not encontro then begin
//      cadNodos.Destroy;
//      exit;  //no vale la pena seguir buscando
//    end;
//    inc(nivel);
//  end;
//  cadNodos.Destroy;
   //Find in one root node
   nod := TreeView1.Items[0];
   while nod <> nil do begin
     if LocateInNode(TExplorNode(nod), arch8) then break;
     nod := nod.GetNextSibling
   end;
end;
function TfraArcExplor.NodRuta(rut: string): TExplorNode;
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
procedure TfraArcExplor.SetTextColor(AValue: TColor);
begin
  TreeView1.ExpandSignColor := AValue;
  TreeView1.DisabledFontColor := AValue;
  TreeView1.SeparatorColor := AValue;
  TreeView1.TreeLineColor := AValue;

  FTextColor := AValue;
  Invalidate;
end;
procedure TfraArcExplor.TreeView1AdvancedCustomDrawItem(
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
procedure TfraArcExplor.LeeFilt(lfil: TStringList);
//Devuelve los filtros para nombres de archivos
begin
  lfil.Delimiter:=',';  //intrepreta como delimitador
  lfil.StrictDelimiter:=true;
  lfil.DelimitedText:=Filter.Text;
end;
procedure TfraArcExplor.ExpandirNodArc(Node: TTreeNode; expan: Boolean);
//Lee el contenido de un nodo y permite expandirlo. Usa el filtro actual.
var
  filtros : TStringList;
begin
   filtros:=TStringList.Create;
   LeeFilt(filtros);  //lee los filtros
   LeerDirectorio(Node, filtros, expan);
   filtros.Free;
end;
procedure TfraArcExplor.TreeView1Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  //solo actualiza si no se ha expandido antes
//  if (node.Count = 1) and (node.Items[0].Text = ' ') then begin
    ExpandirNodArc(TExplorNode(Node), false);
//  end;
end;
procedure TfraArcExplor.LeerDirectorio(Item0: TTreeNode; filtro: TStringList;
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
  Item     : TExplorNode;
  nomArc   : string;
  ultFol   : TExplorNode = nil;  //última carpeta agegada
  directorio: String;
begin
  directorio := TExplorNode(Item0).GetPath;
  directorio := UTF8ToSys(directorio);
  TreeView1.Items.BeginUpdate;
  Screen.Cursor := crHourGlass;  //cambia puntero, para indicar trabajo
  if Item0 <> nil then Item0.DeleteChildren;
  try
    if directorio[Length(directorio)] <> PathDelim then directorio +=  PathDelim;
    if FindFirst(directorio + '*', faDirectory, SearchRec) = 0 then begin
      repeat
        nomArc := SysToUTF8(SearchRec.Name);
        if SearchRec.Attr and faDirectory = faDirectory then
        begin  //directorio
          if nomArc[1] <> '.' then begin
            if ultFol = nil then begin
              //es el primer directorio que se va a agregar.
              ultFol := AddNodeTo(Item0, nomArc, ntyFolder)
            end else begin
              //Se agrega el directorio, después del último agregado. De esta forma se
              //mantienen a los directorios juntos.
              ultFol := AddNodeBehind(ultFol, nomArc, ntyFolder);
            end;
            ultFol.HasChildren:=true; //Para que se pueda abrir
            ultFol.ImageIndex:=0;     //Fija ícono
            ultFol.SelectedIndex:=0;  //Fija ícono cuando está seleccionado
          end;
        end else begin //archivo
          //los archivos van al final
          if Coincide(nomArc) then begin
            Item := AddNodeTo(Item0, nomArc, ntyFile);
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
          end;
        end;
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
    if (Item0 <> nil) and (Item0.Count=0) then
      //no hubo elementos. Agrega etiqueta
      AddNodeTo(Item0, TXT_EMPTY, ntyFile);
  finally
//    AddNodeTo(Item0, '<error>'); //indica error
    if expandir and (Item0 <> nil) then Item0.Expand(false);
    Screen.Cursor := crDefault;  //retorna puntero
    TreeView1.Items.EndUpdate;
  end;
end;
procedure TfraArcExplor.ActualPanelArc;
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
      nodExpan.Add(TExplorNode(nod).GetPath);  //agrega nodo
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
procedure TfraArcExplor.FilterChange(Sender: TObject);  //Evento del combo
begin
  ActualPanelArc;  //actualiza
end;
//Eventos de TreeView1
procedure TfraArcExplor.TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
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
procedure TfraArcExplor.TreeView1CreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TExplorNode;  //define neustra clase de nodo
end;
procedure TfraArcExplor.TreeView1DblClick(Sender: TObject); //Abre el archivo solicitado
var
  nodEx: TExplorNode;
begin
  nodEx := SelectedFile;   //lee seleccionado
  if nodEx = nil then exit;     //verifica
  if OnDoubleClickFile<>nil then OnDoubleClickFile(nodEx); //dispara evento
end;
procedure TfraArcExplor.TreeView1EditingEnd(Sender: TObject; Node: TTreeNode; Cancel: Boolean);
//Termina la edición del nombre de un nodo
var
  NuevoNom: String;
  nodEx: TExplorNode;
begin
  NuevoNom := TExplorNode(Node).GetPath;
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
procedure TfraArcExplor.TreeView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
procedure TfraArcExplor.Init(currPath: string);
{Hace el llenado inicial del árbol de archivos, a partir de la ruta "currPath".}
  procedure FillWindowsDrives;
  var
    Drive: Char;
    DriveLetter: string;
    unidades: TStringList;
    Nodo: TExplorNode;
  begin
    //Crea lista de unidades
    {$IFDEF MSWINDOWS}
    unidades := TStringList.Create;
    for Drive := 'A' to 'Z' do begin
      DriveLetter := Drive + ':\';
      case GetDriveType(PChar(DriveLetter)) of
       DRIVE_REMOVABLE: unidades.Add(DriveLetter + ' Floppy Drive');
       DRIVE_FIXED:     unidades.Add(DriveLetter + ' Fixed Drive');
       DRIVE_REMOTE:    unidades.Add(DriveLetter + ' Network Drive');
       DRIVE_CDROM:     unidades.Add(DriveLetter + ' CD-ROM Drive');
       DRIVE_RAMDISK:   unidades.Add(DriveLetter + ' RAM Disk');
      end;
    end;
    //Prepara nodos iniciales
    TreeView1.Items.Clear;
    for DriveLetter in unidades do begin
      //Crea nodo de unidad
      Nodo := AddNodeTo(nil, DriveLetter[1] + ':', ntyDrive);
      Nodo.HasChildren:=true; //Para que se expanda.
      if DriveLetter[5] = 'C' then begin  // E:\ CD-ROM ...
        Nodo.ImageIndex:=3;   //Fija ícono de CD
        Nodo.SelectedIndex:=3;
      end else begin
        Nodo.ImageIndex:=2;   //Fija ícono de Unidad
        Nodo.SelectedIndex:=2;
      end;
    end;
    unidades.Free;
    {$ENDIF}
  end;
var
  Nodo: TExplorNode;
begin
  btnOpenFolder.Visible := false;
  if currPath='' then begin
    //Ruta vacía
    TreeView1.Items.Clear;
    btnOpenFolder.Visible := true;
    exit;
  end;
  if not DirectoryExists(currPath) then begin
    MsgExc(TXT_DIR_NOTEXIST);
    TreeView1.Items.Clear;
    btnOpenFolder.Visible := true;
    exit;
  end;
//  FillWindowsDrives;
  //Abre la ruta especificada.
  TreeView1.Items.Clear;
  Nodo := AddNodeTo(nil, '', ntyFolder);
  Nodo.HasChildren:=true;     //Para que se expanda.
  Nodo.ImageIndex   := 13;    //Fija ícono
  Nodo.SelectedIndex:= 13;

  //Fija ruta en disco para la exploración
  Nodo.txtPath := currPath;
  //Para el nombre, toma la última parte del directorio
  Nodo.Text := Upcase(extractFileName(currPath));
end;
constructor TfraArcExplor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //asigna eventos
  TreeView1.OnCreateNodeClass:=@TreeView1CreateNodeClass;
  TreeView1.OnExpanding  :=@TreeView1Expanding;  //asigna evento
  TreeView1.OnDblClick   := @TreeView1DblClick;
  TreeView1.OnEditingEnd := @TreeView1EditingEnd;
  TreeView1.OnKeyDown    := @TreeView1KeyDown;
  TreeView1.OnMouseUp    := @TreeView1MouseUp;
  TreeView1.Images       := ImageList2;   //asigna íconos
  TreeView1.Options      := TreeView1.Options + [tvoReadOnly];  //para que no permita editar el nodo

  TreeView1.OnAdvancedCustomDrawItem := @TreeView1AdvancedCustomDrawItem;
  TreeView1.Options := TreeView1.Options - [tvoThemedDraw];
  InternalPopupFolder := false;  //desactiva el menú interno
  InternalPopupFile := false;  //desactiva el menú interno
  //Inicia propiedades
  NewFileName := NEW_FILE_NAME;
  NewFolderName := FOLDER_NAME;
end;
//////////////////////////// Acciones /////////////////////////////////
procedure TfraArcExplor.PopupFolderPopup(Sender: TObject);
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  if curNod.Expanded then mnFolExpandCol.Caption:= TXT_FOLD
  else mnFolExpandCol.Caption:= TXT_UNFOLD;
end;
procedure TfraArcExplor.mnFolExpandColClick(Sender: TObject);
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  curNod.Expanded := not curNod.Expanded;
  if curNod.Expanded then mnFolExpandCol.Caption:= TXT_FOLD
  else mnFolExpandCol.Caption:= TXT_UNFOLD;
end;
procedure TfraArcExplor.mnFolOpenInExplorClick(Sender: TObject);
var
  tmp: String;
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  //Hay problemas al abrir rutas con tildes
  tmp := curNod.GetPath;
//  tmp := UTF8ToSys(curNod.GetPath);
  Exec('explorer', '"' + tmp + '"');
end;
procedure TfraArcExplor.mnFolNewFileClick(Sender: TObject);
var
  archivo: string;
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  if not curNod.IsFolder then exit;  //solo permite en carpetas
  //crea archivo nuevo
  try
    archivo := GetNewFileName(curNod.GetPath +  PathDelim + NewFilename);
    archivo := UTF8ToSys(archivo);
    if not FileExists(archivo) then
      StringToFile('', archivo);
  finally
  end;
  if curNod.Expanded then ExpandirNodArc(curNod, true);   //refresca
  LocateFileOnTree(archivo);
end;
procedure TfraArcExplor.mnFolNewFolderClick(Sender: TObject);
var
  carpeta: string;
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  if not curNod.IsFolder then exit;  //solo permite en carpetas
  //crea carpeta
  try
    carpeta := GetNewFolderName(curNod.GetPath + PathDelim + NewFolderName);
    carpeta := UTF8ToSys(carpeta);
    If not DirectoryExists(carpeta) then
       CreateDir(carpeta);
  finally
  end;
  if curNod.Expanded then ExpandirNodArc(curNod, true);   //refresca
end;
procedure TfraArcExplor.mnFolChanNameClick(Sender: TObject);
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  curNod.EditText;           //inicia edición
  NombNodEdi := curNod.GetPath; //Guarda nombre de nodo editado
end;
procedure TfraArcExplor.mnFolDeleteClick(Sender: TObject);
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  MsgExc(TXT_NOTDELFOL);
end;
procedure TfraArcExplor.mnFolRefreshClick(Sender: TObject);
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  if curNod.Visible and curNod.Expanded then
    ExpandirNodArc(curNod, true);   //mantiene expansión
end;
// File actions
procedure TfraArcExplor.mnFilOpenClick(Sender: TObject);
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  if OnMenuOpenFile<>nil then OnMenuOpenFile(curNod);
end;
procedure TfraArcExplor.mnFilCreCopFromClick(Sender: TObject);
var
  newFile, archivo: String;
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  if not curNod.IsFile then exit;  //no es archivo
  try
    archivo := UTF8ToSys(curNod.GetPath);
    if FileExists(archivo) then begin
      newFile := GetNewFileName(archivo);
      CopyFile(archivo, newFile);
    end;
  finally
  end;
  if curNod.Parent.Expanded then ExpandirNodArc(curNod.parent, true);   //refresca
end;
procedure TfraArcExplor.btnOpenFolderClick(Sender: TObject);
begin
  if OnOpenDirectory<>nil then begin
    OnOpenDirectory();
  end;
end;
procedure TfraArcExplor.mnFilChanNameClick(Sender: TObject);
begin
  mnFolChanNameClick(Sender);  //funciona comoa archivo
end;
procedure TfraArcExplor.mnFilDeleteClick(Sender: TObject);
var
  archivo: String;
begin
  curNod := SelectedNode;
  if curNod = nil then exit;
  if not curNod.IsFile then exit;  //no es archivo
  //eliminar archivo
  if MsgYesNo(TXT_DELFILE, [curNod.Text]) <> 1 then exit;
  try
    archivo := UTF8ToSys(curNod.GetPath);
    if FileExists(archivo) then
//      DeleteToBin(PChar(archivo));  //envía a papelera
      DeleteFile(archivo);
  finally
  end;
  if curNod.Parent.Expanded then ExpandirNodArc(curNod.parent, true);   //refresca
end;
procedure TfraArcExplor.mnFilRefrescarClick(Sender: TObject);
begin

end;

end.

