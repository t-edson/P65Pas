unit FrameLateralPanel;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  ComCtrls, Menus, ActnList, ExtCtrls, LCLProc, Graphics,
  FrameFileExplor, MisUtils;
type
  { TfraLateralPanel }
  TfraLateralPanel = class(TFrame)
    Label1: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    procedure PageControl1Change(Sender: TObject);
  private
    FBackColor: TColor;
    FTextColor: TColor;
    procedure frmArcExplor1DoubleClickFile(nod: TExplorNode);
    procedure SetBackColor(AValue: TColor);
    procedure SetTextColor(AValue: TColor);
  public
    fraArcExplor1: TfraArcExplor;
    //Eventos del explorador de archivo
    OnOpenFile: procedure(filname: string) of object;
    OnSelecFileExplorer: procedure of object;
    //Se requiere información del archivo actual
//    OnReqCurFile: procedure(var filname: string) of object;
    function HasFocus: boolean;
    property BackColor: TColor read FBackColor write SetBackColor;
    property TextColor: TColor read FTextColor write SetTextColor;
    procedure Init();
    constructor Create(AOwner: TComponent) ; override;
  end;

implementation
{$R *.lfm}
{ TfraLateralPanel }
procedure TfraLateralPanel.frmArcExplor1DoubleClickFile(nod: TExplorNode);
begin
  if OnOpenFile<>nil then OnOpenFile(nod.Path);
end;
procedure TfraLateralPanel.Init;
begin
  //Configura filtros del explorador de archivos
  fraArcExplor1.Filter.Items.Add('*.pas,*.pp,*.inc');  //los filtros se separan por comas
  fraArcExplor1.Filter.Items.Add('*');  //para seleccionar todos
  fraArcExplor1.Filter.ItemIndex:=0;    //selecciona la primera opción por defecto
  fraArcExplor1.Filter.Visible := false;
  fraArcExplor1.InternalPopupFile := true;
  fraArcExplor1.InternalPopupFolder := true;
  fraArcExplor1.OnDoubleClickFile:= @frmArcExplor1DoubleClickFile;
  fraArcExplor1.OnKeyEnterOnFile := @frmArcExplor1DoubleClickFile;
  fraArcExplor1.OnMenuOpenFile   := @frmArcExplor1DoubleClickFile;
end;
procedure TfraLateralPanel.SetBackColor(AValue: TColor);
{Configura el color de fondo}
begin
  fraArcExplor1.TreeView1.BackgroundColor := AValue;
end;
procedure TfraLateralPanel.SetTextColor(AValue: TColor);
begin
//  if FTextColor = AValue then Exit;
  FTextColor := AValue;
  fraArcExplor1.TextColor := AValue;
end;
function TfraLateralPanel.HasFocus: boolean;
{Indica si el frame tiene el enfoque.}
begin
//  if fraArcExplor1.Visible then begin
//    //Modo de explorador de archivo
//    Result := fraArcExplor1.TreeView1.Focused;
//  end else begin
//    //Modo normal
//    Result := TreeView1.Focused;
//  end;
end;
//////////////////////// Acciones /////////////////////
procedure TfraLateralPanel.PageControl1Change(Sender: TObject);
//Se ha seleccionado una página diferente
begin
  if PageControl1.ActivePage = TabSheet1 then begin
    //Es el explrador de archivos.
    if OnSelecFileExplorer<>nil then OnSelecFileExplorer();
  end;
  label1.Caption := PageControl1.ActivePage.Caption;
end;

constructor TfraLateralPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fraArcExplor1:= TfraArcExplor.Create(self);
  fraArcExplor1.Parent := TabSheet1;
  fraArcExplor1.Align := alClient;
end;

end.
//427
