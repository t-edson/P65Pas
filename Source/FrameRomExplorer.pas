unit FrameRomExplorer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, StdCtrls, LCLProc,
  LCLIntf, LCLType, ExtCtrls, CPUCore, Parser;
type
  //Define a un bloque de RAM, que servirá para dibujo
  {Los bloques de RAM se usan para separar la memoria en bloques de acuerdo a
  diversos criterios}
  TRomBlock = record
    add1, add2 : word;     //Direcciones de memoria
    implemented: boolean;  //Indica si está implementado
    used       : boolean;  //Indica si el bloque está usado
  end;

  { TfraRomExplorer }

  TfraRomExplorer = class(TFrame)
    Label1: TLabel;
    panGraph: TPanel;
  private
    cxp: TCompilerBase;
    pic: TCPUCore;
    blockUse: array of TRomBlock;
    blockSta: array of TRomBlock;
    procedure DrawBlock(const marcoRam: TRect; ancMargenDir: integer; dirIni,
      dirFin, PageSize: word);
    procedure DrawBlockTxt(const marcoRam: TRect; ancMargenDir: integer;
      dirIni, dirFin, PageSize: word; lbl: string);
    procedure DibBar(const x1, x2: integer; y1, y2: integer; lbl: string);
    procedure panGraphPaint(Sender: TObject);
  public
    procedure SetCompiler(cxp0: TCompilerBase);
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
{ TfraRamExplorer }
procedure TfraRomExplorer.DibBar(const x1, x2: integer; y1, y2: integer;
                                 lbl: string);
//Dibuja una barra, en la posición: x1, x2, y, con altura "alto".
var
  altTxt, ancho, ancTxt, xt, yt, alto: Integer;
  ARect: TRect;
  TextStyle: TTextStyle;
  cv: TCanvas;
begin
  alto := y2 - y1;
  cv := panGraph.Canvas;
  if (cv.Brush.Color = clNone) and (cv.Brush.Style = bsSolid) then begin
    //Sin fondo
    cv.Frame(x1, y1, x2, y2+1);  //Corrige y2, porque Rectangle, dibuja hasta un pincel antes
  end else begin
    //Con fondo
    cv.Rectangle(x1, y1, x2, y2+1); //Corrige y2, porque Rectangle, dibuja hasta un pincel antes
  end;
  //////// Escribe etiqueta centrada /////////////
  ancho := x2 - x1;
  altTxt := cv.TextHeight(lbl);
  ancTxt := cv.TextWidth(lbl);
  ARect.Left := x1;
  ARect.Right := x2;
  ARect.Top := y1;
  ARect.Bottom := y2;
  if ancTxt> ancho then
     xt := x1  //no centra
  else
     xt := x1 + ancho div 2 -  ancTxt div 2;  //centra
  yt := y1 + alto div 2 - altTxt div 2;
//  cv.Pen.Color := clred;
  TextStyle := cv.TextStyle;
  TextStyle.EndEllipsis := true;
  TextStyle.SingleLine := false;
  cv.Brush.Style := bsClear;  //texto sin fondo
  cv.TextRect(Arect, xt, yt, lbl, TextStyle);
end;
procedure TfraRomExplorer.DrawBlockTxt(const marcoRam: TRect;
                                 ancMargenDir: integer;
                                 dirIni, dirFin, PageSize: word; lbl: string);
{Dibuja un bloque de un banco de RAM (definida en el área "marcoRam"), pone etiqueta
descriptiva y pinta con color indicativo.
El bloque a dibujar, empieza en la dirección "dirIni" y termina en "dirFin".}
var
  etiqIni, etiqFin: String;
  altTxt: integer;  //Ancho de margen para las etiquetas de dirección.
  cv: TCanvas;
  x1, x2: LongInt;
  altoByte: Double;
  y1, y2, alto: integer;
  BankMask: Word;
begin
  BankMask := PageSize-1;  //Máscara para el banco: $7F (Mid-range) o $1F (Baseline)

  x1 := marcoRam.Left;
  x2 := marcoRam.Right;
  altoByte := (marcoRam.Bottom - marcoRam.Top)/PageSize;
  y1 := round(marcoRam.Top + (dirIni and BankMask) * altoByte);
  y2 := round(marcoRam.Top + ((dirFin and BankMask)+1) * altoByte);
  alto := y2 - y1;

  cv := panGraph.Canvas;
  //Convierte direcciones a texto y1
  etiqIni := IntToHex(dirIni, 3);
  etiqFin := IntToHex(dirFin, 3);
  //Dibuja barra de fondo
  DibBar(x1+ancMargenDir, x2, y1, y2, lbl);
  //Dibuja etiquetas de dirección
  if ancMargenDir <> 0 then begin
    altTxt := cv.TextHeight(etiqFin);
    if (alto>0.8*altTxt)  then begin
      cv.TextOut(x1, y1, etiqIni);
    end;
    if (alto>1.6*altTxt)  then begin
      cv.TextOut(x1, y2-altTxt, etiqFin);
    end;
  end;
end;
procedure TfraRomExplorer.DrawBlock(const marcoRam: TRect;
                                 ancMargenDir: integer;
                                 dirIni, dirFin, PageSize: word);
{Similar a DrawBlockTxt(), pero no pone las etiquetas de dirección, ni la etiqueta
central.}
var
  cv: TCanvas;
  x1, x2: LongInt;
  altoByte: Double;
  y1, y2: integer;
  BankMask: Word;
begin
  BankMask := PageSize-1;  //Máscara para el banco: $7F (Mid-range) o $1F (Baseline)

  x1 := marcoRam.Left;
  x2 := marcoRam.Right;
  altoByte := (marcoRam.Bottom - marcoRam.Top)/PageSize;
  y1 := round(marcoRam.Top + (dirIni and BankMask) * altoByte);
  y2 := round(marcoRam.Top + ((dirFin and BankMask)+1) * altoByte);
  //Dibuja barra de fondo
  cv := panGraph.Canvas;
  cv.Rectangle(x1+ancMargenDir,
               y1, x2, y2+1);  //Corrige y2, porque Rectangle, dibuja hasta un pincel antes
end;
procedure TfraRomExplorer.panGraphPaint(Sender: TObject);
var
  bordlat, ancPag, x0, i, separ, alto, bordSup, y0: Integer;
begin
end;
procedure TfraRomExplorer.SetCompiler(cxp0: TCompilerBase);
begin
  cxp := cxp0;
  pic := cxp0.picCore;
end;
constructor TfraRomExplorer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //self.OnPaint := @Frame1Paint;
  panGraph.OnPaint := @panGraphPaint;
end;
destructor TfraRomExplorer.Destroy;
begin

  inherited Destroy;
end;

end.

