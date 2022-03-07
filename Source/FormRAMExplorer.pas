unit FormRAMExplorer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLType,
  ExtCtrls, StdCtrls, FrameRamExplorer, CompBase, Analyzer;
type

  { TfrmRAMExplorer }

  TfrmRAMExplorer = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    panStatBar: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fra: TfraRamExplorer;
  public
    procedure Exec(cxp0: TAnalyzer);
  end;

var
  frmRAMExplorer: TfrmRAMExplorer;

implementation

{$R *.lfm}

{ TfrmRAMExplorer }

procedure TfrmRAMExplorer.FormCreate(Sender: TObject);
begin
  fra:= TfraRamExplorer.Create(self);
  fra.Parent := self;
  fra.panTitle.Visible := false;
  Shape1.Brush.Color := $FF9090;
  Shape4.Brush.Color := $80FF80;
  Shape3.Brush.Color := clGray;
end;

procedure TfrmRAMExplorer.FormKeyDown(Sender: TObject; var Key: Word;
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

procedure TfrmRAMExplorer.Exec(cxp0: TAnalyzer);
begin
  fra.SetCompiler(cxp0);
  Caption := 'RAM Explorer. PICModel=' + cxp0.PICName;
  Show;
  self.Width := 600;
  self.Height := 480;
  fra.Align := alClient;
//  fra.Invalidate;
end;

end.

