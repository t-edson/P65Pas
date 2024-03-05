unit FrameSynTree6502;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls,
  ComCtrls, Menus, ActnList, ExtCtrls, LCLProc, Graphics,
  Globales, FormElemProperty, CompBase,
  XpresElemP65, XpresAST, LexPas, MisUtils;
type
  { TfraSynxTree6502 }
  TfraSynxTree6502 = class(TFrame)
  published
    acGenRefres: TAction;
    acGenGoTo: TAction;
    acGenProp: TAction;
    acGenExpAll: TAction;
    acGenDoAnalys: TAction;
    acGenDoOptim: TAction;
    acGenDoSinth: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
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
    procedure acGenDoAnalysExecute(Sender: TObject);
    procedure acGenDoOptimExecute(Sender: TObject);
    procedure acGenDoSinthExecute(Sender: TObject);
    procedure acGenExpAllExecute(Sender: TObject);
    procedure acGenGoToExecute(Sender: TObject);
    procedure acGenRefresExecute(Sender: TObject);
    procedure acGenPropExecute(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeView1SelectionChanged(Sender: TObject);
  private
    FBackColor: TColor;
    FTextColor: TColor;
    cpx       : TCompilerBase;   //Reference to lexer
    syntaxTree: TXpTreeElements; //Reference to SyntaxTree
    frmElemProp: TfrmElemProperty;  //Formulario de propiedades
    function AddNodeTo(nodParent: TTreeNode; elem: TxpElement): TTreeNode;
    procedure frmElemPropertyExplore(elem: TxpElement);
    procedure RefreshByDeclar(nodMain: TTreeNode; curEle: TxpElement);
    function SelectedIsMain: boolean;
    function SelectedIsElement: boolean;
    procedure TreeView1AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
  public
    OnLocateElemen: procedure(fileSrc: string; row, col: integer) of object;
    OnReqAnalysis  : procedure of object;
    OnReqOptimizat : procedure of object;
    OnReqSynthesis : procedure of object;
    procedure SetBackColor(AValue: TColor);
    procedure SetTextColor(AValue: TColor);
    function HasFocus: boolean;
    property BackColor: TColor read FBackColor write SetBackColor;
    property TextColor: TColor read FTextColor write SetTextColor;
  public    //Initialization
    procedure Refresh;
    procedure Init(Compiler: TCompilerBase);
    constructor Create(AOwner: TComponent) ; override;
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

{ TfraSynxTree6502 }
function TfraSynxTree6502.AddNodeTo(nodParent: TTreeNode; elem: TxpElement): TTreeNode;
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
procedure TfraSynxTree6502.frmElemPropertyExplore(elem: TxpElement);
begin
  acGenGoToExecute(self);
end;
procedure TfraSynxTree6502.RefreshByDeclar(nodMain: TTreeNode; curEle: TxpElement);
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
function TfraSynxTree6502.SelectedIsMain: boolean;
//Indica si el nodo seleccionado es el nodo raiz
begin
  if TreeView1.Selected = nil then exit(false);
  if TreeView1.Selected.Level = 0 then exit(true);
  exit(false);
end;
function TfraSynxTree6502.SelectedIsElement: boolean;
//Indica si el nodo seleccionado es un nodo que representa a un elemeno.
var
  nod: TTreeNode;
begin
  if TreeView1.Selected = nil then exit(false);
  nod := TreeView1.Selected;
  //Todos son elementos.
  if nod.Level >= 1 then exit(true);
  exit(false);
end;
procedure TfraSynxTree6502.SetBackColor(AValue: TColor);
{Configura el color de fondo}
begin
//  if FBackColor = AValue then Exit;
  FBackColor := AValue;
  TreeView1.BackgroundColor := AValue;
end;
procedure TfraSynxTree6502.SetTextColor(AValue: TColor);
begin
//  if FTextColor = AValue then Exit;
  FTextColor := AValue;
end;
procedure TfraSynxTree6502.TreeView1AdvancedCustomDrawItem(
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
function TfraSynxTree6502.HasFocus: boolean;
{Indica si el frame tiene el enfoque.}
begin
  Result := TreeView1.Focused;
end;
procedure TfraSynxTree6502.TreeView1MouseUp(Sender: TObject;
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
procedure TfraSynxTree6502.TreeView1SelectionChanged(Sender: TObject);
var
  elem: TxpElement;
begin
  if not frmElemProp.Visible then exit;
  if TreeView1.Selected = nil then exit;
  if TreeView1.Selected.Data = nil then begin
    frmElemProp.Clear;
    exit;
  end;
  elem := TxpElement(TreeView1.Selected.Data);
  frmElemProp.Exec(cpx, elem);
end;
procedure TfraSynxTree6502.TreeView1DblClick(Sender: TObject);
begin
  acGenGoToExecute(self);
end;
//////////////////////// Acciones /////////////////////
procedure TfraSynxTree6502.acGenRefresExecute(Sender: TObject);
begin
  Refresh;
end;
procedure TfraSynxTree6502.acGenGoToExecute(Sender: TObject);
var
  elem: TxpElement;
  fileName: String;
begin
  if SelectedIsElement then begin
    elem := TxpElement(TreeView1.Selected.Data);
    fileName := cpx.ctxFile(elem.srcDec);
    if OnLocateElemen <> nil  then OnLocateElemen(fileName, elem.srcDec.row, elem.srcDec.col);
  end;
end;
procedure TfraSynxTree6502.acGenExpAllExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to TreeView1.Items.Count - 1 do begin
    TreeView1.Items[i].Expanded := true;
  end;
end;
procedure TfraSynxTree6502.acGenPropExecute(Sender: TObject);
var
  elem: TxpElement;
begin
  if TreeView1.Selected = nil then exit;
  if TreeView1.Selected.Data = nil then exit;
  elem := TxpElement(TreeView1.Selected.Data);
  frmElemProp.Exec(cpx, elem);
  frmElemProp.Show;
end;
procedure TfraSynxTree6502.acGenDoAnalysExecute(Sender: TObject);
{Require the compiler to do Only Analysis.}
begin
  if OnReqAnalysis<>nil then OnReqAnalysis();
end;
procedure TfraSynxTree6502.acGenDoOptimExecute(Sender: TObject);
{Require the compiler to do Analysis and Optimization.}
begin
  if OnReqOptimizat<>nil then OnReqOptimizat();
end;
procedure TfraSynxTree6502.acGenDoSinthExecute(Sender: TObject);
{Require the compiler to do Analysis, Optimization and Synthesis.}
begin
  if OnReqSynthesis<>nil then OnReqSynthesis();
end;
//Initialization
procedure TfraSynxTree6502.Refresh;
{Actualiza el árbol de sintaxis con el AST del compilador}
var
  nodMain: TTreeNode;
begin
  TreeView1.Visible := true;

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
procedure TfraSynxTree6502.Init(Compiler    : TCompilerBase);
begin
  cpx        := Compiler;
  syntaxTree := Compiler.TreeElems;
  TreeView1.ReadOnly := true;
  TreeView1.OnAdvancedCustomDrawItem := @TreeView1AdvancedCustomDrawItem;
  TreeView1.Options := TreeView1.Options - [tvoThemedDraw];
  frmElemProp.OnExplore := @frmElemPropertyExplore;
end;
constructor TfraSynxTree6502.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Creamos el formulario de propiedades como hijo, así que no necesitaremos destruirlo manualmente.
  frmElemProp := TfrmElemProperty.Create(self);
end;
end.
//435
