unit FrameSyntaxTree;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, StdCtrls,
  ComCtrls, Menus, ActnList, ExtCtrls, ComboEx, LCLProc, Graphics,
  Globales, FormElemProperty, CompBase, FormConfig,
  FrameArcExplor, XpresElemP65, XpresAST, LexPas, MisUtils;
type
  { TfraSyntaxTree }
  TfraSyntaxTree = class(TFrame)
    acGenRefres: TAction;
    acGenGoTo: TAction;
    acGenProp: TAction;
    acGenViewDec: TAction;
    acGenExpAll: TAction;
    ActionList1: TActionList;
    ComboBoxEx1: TComboBoxEx;
    frmArcExplor1: TfrmArcExplor;
    ImageList1: TImageList;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    mnGoTo: TMenuItem;
    mnRefresh: TMenuItem;
    mnProper: TMenuItem;
    mnRefresh1: TMenuItem;
    mnRefresh2: TMenuItem;
    Panel1: TPanel;
    PopupElem: TPopupMenu;
    PopupFrame: TPopupMenu;
    TreeFilterEdit1: TTreeFilterEdit;
    TreeView1: TTreeView;
    procedure acGenExpAllExecute(Sender: TObject);
    procedure acGenGoToExecute(Sender: TObject);
    procedure acGenRefresExecute(Sender: TObject);
    procedure acGenPropExecute(Sender: TObject);
    procedure acGenViewDecExecute(Sender: TObject);
    procedure ComboBoxEx1Change(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeView1SelectionChanged(Sender: TObject);
  private
    FBackColor: TColor;
    FTextColor: TColor;
    cpx       : TCompilerBase;   //Reference to lexer
    syntaxTree: TXpTreeElements; //Reference to SyntaxTree
    function AddNodeTo(nodParent: TTreeNode; elem: TxpElement): TTreeNode;
    procedure frmArcExplor1DoubleClickFile(nod: TExplorNode);
    procedure frmArcExplor1MenuOpenFile(nod: TExplorNode);
    procedure frmElemPropertyExplore(elem: TxpElement);
    procedure RefreshByDeclar(nodMain: TTreeNode; curEle: TxpElement);
    function SelectedIsMain: boolean;
    function SelectedIsElement: boolean;
    procedure SetBackColor(AValue: TColor);
    procedure SetTextColor(AValue: TColor);
    procedure TreeView1AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
  public
    OnSelectElemen: procedure(fileSrc: string; row, col: integer) of object;
    OnOpenFile: procedure(filname: string) of object;
    OnSelecFileExplorer: procedure of object;
    //Se requiere información del archivo actual
//    OnReqCurFile: procedure(var filname: string) of object;
    function HasFocus: boolean;
    function FileSelected: string;
    property BackColor: TColor read FBackColor write SetBackColor;
    property TextColor: TColor read FTextColor write SetTextColor;
    procedure LocateFile(filname: string);
    procedure Init(Compiler: TCompilerBase);
    procedure Refresh;
    procedure SetLanguage;
  end;

implementation
{$R *.lfm}
var
  //Cadenas con los títulos de los nodos a mostrar en el árbol
  TIT_MAIN, TIT_UNIT : string;
  TIT_CONS: String;
  TIT_VARS: String;
  TIT_FUNC: String;
  TIT_TYPE: String;
  TIT_OTHER: String;

{ TfraSyntaxTree }
procedure TfraSyntaxTree.SetLanguage;
begin
  {$I ..\_language\tra_FrameSyntaxTree.pas}
  frmArcExplor1.SetLanguage;
  Refresh;
end;
procedure TfraSyntaxTree.frmArcExplor1DoubleClickFile(nod: TExplorNode);
begin
  if OnOpenFile<>nil then OnOpenFile(nod.Path);
end;
procedure TfraSyntaxTree.frmArcExplor1MenuOpenFile(nod: TExplorNode);
begin
  if OnOpenFile<>nil then OnOpenFile(nod.Path);
end;
procedure TfraSyntaxTree.Init(Compiler    : TCompilerBase);
begin
  cpx        := Compiler;
  syntaxTree := Compiler.TreeElems;
  TreeView1.ReadOnly := true;
  TreeView1.OnAdvancedCustomDrawItem := @TreeView1AdvancedCustomDrawItem;
  TreeView1.Options := TreeView1.Options - [tvoThemedDraw];
  frmElemProperty.OnExplore := @frmElemPropertyExplore;
  //Configura filtros del explorador de archivos
  frmArcExplor1.Filter.Items.Add('*.pas,*.pp,*.inc');  //los filtros se separan por comas
  frmArcExplor1.Filter.Items.Add('*');  //para seleccionar todos
  frmArcExplor1.Filter.ItemIndex:=0;    //selecciona la primera opción por defecto
  frmArcExplor1.Filter.Visible := false;
  frmArcExplor1.InternalPopupFile := true;
  frmArcExplor1.InternalPopupFolder := true;
  frmArcExplor1.OnDoubleClickFile:= @frmArcExplor1DoubleClickFile;
  frmArcExplor1.OnKeyEnterOnFile := @frmArcExplor1DoubleClickFile;
  frmArcExplor1.OnMenuOpenFile   := @frmArcExplor1MenuOpenFile;
end;
function TfraSyntaxTree.AddNodeTo(nodParent: TTreeNode; elem: TxpElement): TTreeNode;
{Agrega un elemento a un noco.}
var
  nod: TTreeNode;
  eleExp: TEleExpress;
  sen: TEleSentence;
  asmInst: TEleAsmInstr;
begin
  if elem = nil then begin
    nod := TreeView1.Items.AddChild(nodParent, '???');
    nod.Data := elem;
    Result := nod;
    exit;
  end;
  nod := TreeView1.Items.AddChild(nodParent, elem.name);
  if elem.idClass = eleConsDec then begin
    nod.ImageIndex := 23;
    nod.SelectedIndex := 23;
  end else if elem.idClass = eleVarDec then begin
    nod.ImageIndex := 24;
    nod.SelectedIndex := 24;
  end else if elem.idClass = eleTypeDec then begin
    nod.ImageIndex := 15;
    nod.SelectedIndex := 15;
  end else if elem.idClass = eleFuncDec then begin
    nod.ImageIndex := 16;
    nod.SelectedIndex := 16;
  end else if elem.idClass = eleFunc then begin
    nod.ImageIndex := 3;
    nod.SelectedIndex := 3;
  end else if elem.idClass = eleUnit then begin
    nod.ImageIndex := 6;
    nod.SelectedIndex := 6;
  end else if elem.idClass = eleBody then begin
    nod.ImageIndex := 5;
    nod.SelectedIndex := 5;
  end else if elem.idClass = eleSenten then begin
    sen := TEleSentence(elem);
    nod.Text :=   '<sentnc: ' + sen.sntTypeAsStr + '>';
    //nod.Text := '<sentence>';
    nod.ImageIndex := 12;
    nod.SelectedIndex := 12;
  end else if elem.idClass = eleAsmInstr then begin
    asmInst := TEleAsmInstr(elem);
    if asmInst.iType = itLabel then begin  //Etiquetas
      nod.ImageIndex := 22;
      nod.SelectedIndex := 22;
    end else begin
      nod.ImageIndex := 19;
      nod.SelectedIndex := 19;
    end;
  end else if elem.idClass = eleAsmOperat then begin
    nod.ImageIndex := 20;
    nod.SelectedIndex := 20;
  end else if elem.idClass = eleExpress then begin
    eleExp := TEleExpress(elem);
    if eleExp.opType = otFunct then begin
      nod.ImageIndex := 3;
      nod.SelectedIndex := 3;
    end else if eleExp.opType = otVariab then begin
      nod.ImageIndex := 2;
      nod.SelectedIndex := 2;
    end else if eleExp.opType = otConst then begin
      nod.ImageIndex := 4;
      nod.SelectedIndex := 4;
    end else begin
      nod.ImageIndex := 17;
      nod.SelectedIndex := 17;
    end;
  end else if elem.idClass = eleCondit then begin
    //sen := TEleSentence(elem);
    nod.Text :=   'condit';
    nod.ImageIndex := 21;
    nod.SelectedIndex := 21;
  end else begin
    nod.ImageIndex := 0;
    nod.SelectedIndex := 0;
  end;
  nod.Data := elem;
  Result := nod;
end;
procedure TfraSyntaxTree.frmElemPropertyExplore(elem: TxpElement);
begin
  acGenGoToExecute(self);
end;
procedure TfraSyntaxTree.RefreshByDeclar(nodMain: TTreeNode; curEle: TxpElement);
var
  elem: TxpElement;
  nodElem: TTreeNode;
begin
  //Agrega elementos
  if curEle.elements = nil then exit;
  for elem in curEle.elements do begin
      nodElem := AddNodeTo(nodMain, elem);
      RefreshByDeclar(nodElem, elem);  //Llamada recursiva
      //Expande los Body
      if elem.idClass = eleBody then nodElem.Expanded := true;
      if elem.idClass = eleSenten then nodElem.Expanded := true;
      if elem.Parent.idClass = eleSenten then nodElem.Expanded := true; //Expande instrucciones
  end;
end;
procedure TfraSyntaxTree.Refresh;
var
  nodMain: TTreeNode;
begin
  case Config.viewMode of
  vmDeclar: begin
    TreeView1.Visible := true;
    frmArcExplor1.Visible := false;

    TreeView1.Items.BeginUpdate;
    TreeView1.Items.Clear;
    nodMain := TreeView1.Items.AddChild(nil, TIT_MAIN);
    nodMain.ImageIndex := 1;
    nodMain.SelectedIndex := 1;
    nodMain.Data := syntaxTree.main;  //Elemento raiz
    RefreshByDeclar(nodMain, syntaxTree.main);
    nodMain.Expanded := true;    //Expande nodo raiz
    TreeView1.Items.EndUpdate;
  end;
  vmFileExp: begin  //Modo de explorador de archivos
    TreeView1.Visible := false;
    frmArcExplor1.Visible := true;
    frmArcExplor1.Align := alClient;

  end;
  end;
end;
function TfraSyntaxTree.SelectedIsMain: boolean;
//Indica si el nodo seleccionado es el nodo raiz
begin
  if TreeView1.Selected = nil then exit(false);
  if TreeView1.Selected.Level = 0 then exit(true);
  exit(false);
end;
function TfraSyntaxTree.SelectedIsElement: boolean;
//Indica si el nodo seleccionado es un nodo que representa a un elemeno.
var
  nod: TTreeNode;
begin
  if TreeView1.Selected = nil then exit(false);
  nod := TreeView1.Selected;
  if Config.viewMode = vmDeclar then begin
    //En modo de declaraciones, es más fácil. Todos son elementos.
    if nod.Level >= 1 then exit(true);
  end;
  exit(false);
end;
procedure TfraSyntaxTree.SetBackColor(AValue: TColor);
{Configura el color de fondo}
begin
//  if FBackColor = AValue then Exit;
  FBackColor := AValue;
  TreeView1.BackgroundColor := AValue;
  frmArcExplor1.TreeView1.BackgroundColor := AValue;
end;
procedure TfraSyntaxTree.SetTextColor(AValue: TColor);
begin
//  if FTextColor = AValue then Exit;
  FTextColor := AValue;
  frmArcExplor1.TextColor := AValue;
end;
procedure TfraSyntaxTree.TreeView1AdvancedCustomDrawItem(
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
function TfraSyntaxTree.HasFocus: boolean;
{Indica si el frame tiene el enfoque.}
begin
  if frmArcExplor1.Visible then begin
    //Modo de explorador de archivo
    Result := frmArcExplor1.TreeView1.Focused;
  end else begin
    //Modo normal
    Result := TreeView1.Focused;
  end;
end;
function TfraSyntaxTree.FileSelected: string;
{Devuelve el archivo seleccionado. Solo es válido cuando está en modo "vmFileExp"
}
begin
  if Config.viewMode = vmFileExp then begin
    if frmArcExplor1.SelectedFile = nil then begin
      Result := '';
    end else begin
      Result := frmArcExplor1.SelectedFile.Path;
    end;
  end else begin
    Result := '';
  end;
end;
procedure TfraSyntaxTree.LocateFile(filname: string);
begin
  //Ubica el archivo actual en el explorador de archivo
  if not self.Visible then exit;
  if frmArcExplor1.Visible then begin
    frmArcExplor1.LocateFileOnTree(filname);
  end;
end;
procedure TfraSyntaxTree.TreeView1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  nod: TTreeNode;
begin
  //Quita la selección, si se pulsa en una zona vacía
  nod := TreeView1.GetNodeAt(X,Y);
  if nod=nil then begin
    TreeView1.Selected := nil;
  end;
  //Abre el menú que corresponda
  if button = mbRight then begin
    if SelectedIsElement then begin
      PopupElem.PopUp;
    end else begin
      PopupFrame.PopUp;
    end;
  end;
end;
procedure TfraSyntaxTree.TreeView1SelectionChanged(Sender: TObject);
var
  elem: TxpElement;
begin
  if not frmElemProperty.Visible then exit;
  if TreeView1.Selected = nil then exit;
  if TreeView1.Selected.Data = nil then begin
    frmElemProperty.Clear;
    exit;
  end;
  elem := TxpElement(TreeView1.Selected.Data);
  frmElemProperty.Exec(cpx, elem);
end;
procedure TfraSyntaxTree.TreeView1DblClick(Sender: TObject);
begin
  acGenGoToExecute(self);
end;
procedure TfraSyntaxTree.ComboBoxEx1Change(Sender: TObject);
begin
  if Config = nil then exit;
  case ComboBoxEx1.ItemIndex of
  0: Config.viewMode := vmDeclar;
  1: Config.viewMode := vmFileExp;
  end;
  Refresh;
  if ComboBoxEx1.ItemIndex=2 then begin
    //Se seleeciona el modo de explorador de archivo
    if OnSelecFileExplorer<>nil then OnSelecFileExplorer;
  end;
end;
//////////////////////// Acciones /////////////////////
procedure TfraSyntaxTree.acGenRefresExecute(Sender: TObject);
begin
  Refresh;
end;
procedure TfraSyntaxTree.acGenGoToExecute(Sender: TObject);
var
  elem: TxpElement;
  fileName: String;
begin
  if SelectedIsElement then begin
    elem := TxpElement(TreeView1.Selected.Data);
    fileName := cpx.ctxFile(elem.srcDec);
    if OnSelectElemen <> nil  then OnSelectElemen(fileName, elem.srcDec.row, elem.srcDec.col);
  end;
end;
procedure TfraSyntaxTree.acGenExpAllExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to TreeView1.Items.Count - 1 do begin
    TreeView1.Items[i].Expanded := true;
  end;
end;
procedure TfraSyntaxTree.acGenPropExecute(Sender: TObject);
var
  elem: TxpElement;
begin
  if TreeView1.Selected = nil then exit;
  if TreeView1.Selected.Data = nil then exit;
  elem := TxpElement(TreeView1.Selected.Data);
  frmElemProperty.Exec(cpx, elem);
  frmElemProperty.Show;
end;
procedure TfraSyntaxTree.acGenViewDecExecute(Sender: TObject);
{Muestra elementos por declaración}
begin
  Config.viewMode := vmDeclar;
  Refresh;
end;

end.

