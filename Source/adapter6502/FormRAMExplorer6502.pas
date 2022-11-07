unit FormRAMExplorer6502;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLType,
  ExtCtrls, StdCtrls, Menus, FrameRamExplorer6502, Analyzer, Types;
type

  { TfrmRAMExplorer6502 }

  TfrmRAMExplorer6502 = class(TForm)
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    mnZoomPos: TMenuItem;
    mnZommNeg: TMenuItem;
    panStatBar: TPanel;
    PopupMenu1: TPopupMenu;
    ScrollBox1: TScrollBox;
    shpSpecRAM: TShape;
    shpUnimplem: TShape;
    shpRamUsed: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mnZommNegClick(Sender: TObject);
    procedure mnZoomPosClick(Sender: TObject);
    procedure ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    fra: TfraRamExplorer6502;
  public
    zoom: single;
    procedure UpdateScreen(cxp0: TAnalyzer);
    procedure Exec(cxp0: TAnalyzer; zoom0: Single);
  end;

var
  frmRAMExplorer6502: TfrmRAMExplorer6502;

implementation

{$R *.lfm}

{ TfrmRAMExplorer6502 }

procedure TfrmRAMExplorer6502.FormCreate(Sender: TObject);
begin
  fra:= TfraRamExplorer6502.Create(self);
  fra.Align:=alClient;;
  fra.Parent := ScrollBox1;
  fra.Left := 0;
  fra.Top := 0;
  fra.panTitle.Visible := false;
  shpRamUsed.Brush.Color  := fra.COL_USED_CODE;
  shpSpecRAM.Brush.Color  := fra.COL_IMPLE_SFR;
  shpUnimplem.Brush.Color := fra.COL_UNIMPLEM;
end;

procedure TfrmRAMExplorer6502.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F12 then begin
    Key := 0;
    self.Hide;
  end;
  if Key = VK_ESCAPE then begin
    Key := 0;
    self.Hide;
  end;
end;

procedure TfrmRAMExplorer6502.mnZoomPosClick(Sender: TObject);
{Aumenta el Zoom}
begin
  if zoom<25 then zoom := zoom * 1.2;
  fra.Height := round(ScrollBox1.Height * zoom);
end;
procedure TfrmRAMExplorer6502.mnZommNegClick(Sender: TObject);
{Disminuye el Zoom}
begin
  if zoom>1 then zoom := zoom / 1.2;
  if zoom<1 then zoom := 1;
  fra.Height := round(ScrollBox1.Height * zoom);
end;
procedure TfrmRAMExplorer6502.ScrollBox1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
//  self.Caption := IntToSTr(WheelDelta);
  if Shift = [ssCtrl] then begin
    if WheelDelta>0 then begin
      mnZoomPosClick(self);
    end else begin
      mnZommNegClick(self);
    end;
    Handled := true;    //Para que no desplaze Scrollbars
  end;
end;
procedure TfrmRAMExplorer6502.UpdateScreen(cxp0: TAnalyzer);
{Actualiza la pantalla de este formulario.}
begin
  fra.SetCompiler(cxp0);
  fra.Invalidate;
end;
procedure TfrmRAMExplorer6502.Exec(cxp0: TAnalyzer; zoom0: Single);
begin
  zoom := zoom0;
  fra.SetCompiler(cxp0);
  Caption := 'RAM Explorer. CPUModel=' + cxp0.PICName;
  //Dimensiona ventana para mostrar
  self.Width := 360;
  self.Height := 480;
  Show;       //Para que SCrollBox1 se alínee al tamaño de "self".
  //fra.Align := alClient;
  //Define tamaño inicial de dibujo
  fra.Width := ScrollBox1.Width - 22;  //Deja espacio para Scrollbar
  fra.Height := round(ScrollBox1.Height * zoom);  //Altura inicial
//  fra.Invalidate;
end;

end.

