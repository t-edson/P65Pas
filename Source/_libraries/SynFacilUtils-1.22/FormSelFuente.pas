unit FormSelFuente;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls,
  ButtonPanel;

type

  TTipFuente = (tfTod, tfSel, tfLin);
  { TfrmSelFuente }

  TfrmSelFuente = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    optTod: TRadioButton;
    optSel: TRadioButton;
    optLin: TRadioButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    cancelado: boolean;
    procedure SetLanguage(lang: string);
  end;

var
  frmSelFuente: TfrmSelFuente;
implementation

{$R *.lfm}

{ TfrmSelFuente }

procedure TfrmSelFuente.OKButtonClick(Sender: TObject);
begin
  cancelado := false;
  frmSelFuente.Close;
end;

procedure TfrmSelFuente.CancelButtonClick(Sender: TObject);
begin
  cancelado := true;
  frmSelFuente.Close;
end;

procedure TfrmSelFuente.SetLanguage(lang: string);
begin
  case lowerCase(lang) of
  'es': begin
      Caption := 'Seleccionar fuente';
      Label1.Caption:='Seleccione fuente a procesar:';
      optTod.Caption:='&Todo el texto';
      optSel.Caption:='&Selección';
      optLin.Caption:='&Línea actual';
    end;
  'en': begin
      Caption := 'Select Source';
      Label1.Caption:='Select the source to process:';
      optTod.Caption:='&All the text';
      optSel.Caption:='&Selection';
      optLin.Caption:='&Current Line';
    end;
  end;
end;


end.

