{Frame para mostrar estadísticas de uso de RAM y ROM. Se debe ubicar este frame
en el panel de estadísticas.}
unit FrameStatist6502;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics;
type

  { TfraStatist6502 }

  TfraStatist6502 = class(TFrame)
    lblRAM: TLabel;
    lblROM: TLabel;
  private
    FBackColor: TColor;
    FTextColor: TColor;
    FPanelColor: TColor;
    usedRAM, usedROM: single;
    procedure SetBackColor(AValue: TColor);
    procedure SetPanelColor(AValue: TColor);
    procedure SetTextColor(AValue: TColor);
  public
    property BackColor: TColor read FBackColor write SetBackColor ;
    property TextColor: TColor read FTextColor write SetTextColor ;
    property PanelColor: TColor read FPanelColor write SetPanelColor;
  protected
    procedure Paint; override;
  public
    procedure Update(usedRAM0, usedROM0, usedSTK0: single);
  end;

implementation
{$R *.lfm}
{ TfraStatist6502 }
procedure TfraStatist6502.SetBackColor(AValue: TColor);
begin
  if FBackColor = AValue then Exit;
  FBackColor := AValue;

end;
procedure TfraStatist6502.SetPanelColor(AValue: TColor);
begin
  //if FPanelColor = AValue then Exit;
  FPanelColor := AValue;
  self.Color := AValue; //Color de fondo

end;
procedure TfraStatist6502.SetTextColor(AValue: TColor);
begin
  if FTextColor = AValue then Exit;
  FTextColor := AValue;

end;
{Sobreescribimos el método porque no existe el evento OnPaint().
Si no funciona, usar mejor un control embebido con OnPaint como TPanel.}
procedure TfraStatist6502.Paint;
var
  cv: TCanvas;
  procedure Barra(x0, y0: integer; alt: integer; porc: Single);
  var
    alt2, dif: Integer;
    n: Int64;
  begin
    if alt<15 then exit;
    if alt>120 then alt := 100;
    //Dibuja fondo
    {$ifdef UNIX}
//    cv.Brush.Color := clForm;
    cv.Brush.Color := FBackColor;
    {$else}
//    cv.Brush.Color := clMenu;
    cv.Brush.Color := FBackColor;
    {$endif}
    cv.FillRect(x0, y0, x0 + 20, y0 +alt);
    //Dibuja barra
    n := round(porc*100);
    cv.Pen.Color := clGreen;
    if n < 40 then begin
      cv.Brush.Color := clGreen;
    end else if n < 80 then begin
      cv.Brush.Color := clYellow;
    end else begin
      cv.Brush.Color := clRed;
    end;
    alt2 := Round(alt*porc);
    dif := alt-alt2;
    cv.FillRect(x0, y0 + dif , x0 + 20, y0 + alt2 + dif);
    //Borde
    cv.Pen.Color := clGray;
    cv.Frame(x0, y0, x0 + 20, y0 +alt);
    cv.Frame(x0-2, y0-2, x0 + 22, y0 +alt+2);
    //Texto
    cv.Brush.Style := bsClear;
    cv.Font.Bold := true;
    cv.Font.Color := FTextColor;
    if n<10 then begin
      cv.TextOut(x0+2, y0 + alt div 2 - 10, IntToStr(n)+'%');
    end else if n < 100 then begin
      cv.TextOut(x0-1, y0 + alt div 2 - 10, IntToStr(n)+'%');
    end else begin
      cv.TextOut(x0-3, y0 + alt div 2 - 10, IntToStr(n)+'%');
    end;
  end;
begin
    cv := self.Canvas;
    Barra(lblRAM.Left + 5, lblRAM.Top + 20, self.Height-35, usedRAM);
    Barra(lblROM.Left + 5, lblROM.Top + 20, self.Height-35, usedROM);

  inherited Paint;
end;
procedure TfraStatist6502.Update(usedRAM0, usedROM0, usedSTK0: single);
{Refresca el control con los valores indicados.}
begin
  usedRAM := usedRAM0;
  usedROM := usedROM0;
  self.Invalidate;
end;

end.

