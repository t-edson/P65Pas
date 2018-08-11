unit FrameRamExplorer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, StdCtrls, LCLProc,
  LCLIntf, LCLType, ExtCtrls, Buttons, CPUCore, Parser;
type
  //Define a un bloque de RAM, que servirá para dibujo
  {Los bloques de RAM se usan para separar la memoria en bloques de acuerdo a
  diversos criterios, como el estado, el uso o el mapeo. Por eso tienen diversas
  banderas}
  TRamBlock = record
    add1, add2: word;      //Direcciones de memoria
    blkType: TCPUCellState; //Tipo de blqoue
    mapped : boolean;        //Indica si el bloque está mapeado
    used   : boolean;        //Indica si el bloque está usado
  end;

  { TfraRamExplorer }
  TfraRamExplorer = class(TFrame)
    Label1: TLabel;
    panGraph: TPanel;
    panTitle: TPanel;
    SpeedButton1: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
  private
    cxp: TCompilerBase;
    pic: TCPUCore;
    blockSta: array of TRamBlock;
    blockMap: array of TRamBlock;
    blockUse: array of TRamBlock;
    procedure SplitInStateRAM(dir1, dir2: word);
    procedure SplitInUsedRAM(dir1, dir2: word);
    procedure DrawRAMbank(const marcoRam: TRect; selected: boolean);
    procedure DibBar(const x1, x2: integer; y1, y2: integer; lbl: string);
    procedure DrawBlockTxt(const marcoRam: TRect; ancMargenDir: integer; dirIni,
      dirFin, BankSize: integer; lbl: string);
    procedure DrawBlock(const marcoRam: TRect; ancMargenDir: integer; dirIni,
      dirFin, BankSize: integer);
    procedure panGraphPaint(Sender: TObject);
  public
    OnCloseFrame: procedure of object;   //Evento de cierre
    procedure SetCompiler(cxp0: TCompilerBase);
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
{ TfraRamExplorer }
procedure TfraRamExplorer.SpeedButton1Click(Sender: TObject);
begin
  if OnCloseFrame<>nil then OnCloseFrame;
end;
procedure TfraRamExplorer.SplitInStateRAM(dir1, dir2: word);
{Explora la memoria RAM, entre las direcciones dir1, y dir2, y almacena los bloques
encontrados (de distinto campo "state"), en el arreglo "blockSta".
}
var
  n: Integer;
  i, pos1, pos2: Word;
  tipBloque: TCPUCellState;
begin
  setlength(blockSta, 1);  //abre un bloque
  n := high(blockSta);  //índice actual
  for i := dir1 to dir2 do begin
    if i = dir1 then begin
      //Define el bloque inicial
      tipBloque := pic.ram[i].state;
      pos1 := i;
      pos2 := i;
    end else begin
      if pic.ram[i].state = tipBloque then begin
        //Es del bloque anterior
        pos2 := i;   //actualiza límite
      end else begin
        //Es otro tipo de bloque.
        //Cierra el anterior
        blockSta[n].add1 := pos1;
        blockSta[n].add2 := pos2;
        blockSta[n].blkType := tipBloque;
        n := n + 1;
        setlength(blockSta, n+1);
        //Define nuevo blqoue
        tipBloque := pic.ram[i].state;
        pos1 := i;
        pos2 := i;
      end;
    end;
  end;
  //Cierra el último bloque
  blockSta[n].add1 := pos1;
  blockSta[n].add2 := pos2;
  blockSta[n].blkType := tipBloque;
end;
procedure TfraRamExplorer.SplitInUsedRAM(dir1, dir2: word);
{Explora la memoria RAM, entre las direcciones dir1, y dir2, y almacena los bloques
encontrados (de acuerdo a si están usados o no), en el arreglo "blockUse".
}
var
  n: Integer;
  i, pos1, pos2: Word;
  used: boolean;
begin
  setlength(blockUse, 1);  //abre un bloque
  n := high(blockUse);  //índice actual
  for i := dir1 to dir2 do begin
    if i = dir1 then begin
      //Define el bloque inicial
      used := pic.ram[i].used;
      pos1 := i;
      pos2 := i;
    end else begin
      if pic.ram[i].used = used then begin
        //Es del bloque anterior
        pos2 := i;   //actualiza límite
      end else begin
        //Es otro tipo de bloque.
        //Cierra el anterior
        blockUse[n].add1 := pos1;
        blockUse[n].add2 := pos2;
        blockUse[n].used:= used;
        n := n + 1;
        setlength(blockUse, n+1);
        //Define nuevo blqoue
        used := pic.ram[i].used;
        pos1 := i;
        pos2 := i;
      end;
    end;
  end;
  //Cierra el último bloque
  blockUse[n].add1 := pos1;
  blockUse[n].add2 := pos2;
  blockUse[n].used:= used;
end;

procedure TfraRamExplorer.DibBar(const x1, x2: integer; y1, y2: integer;
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
procedure TfraRamExplorer.DrawBlockTxt(const marcoRam: TRect;
  ancMargenDir: integer; dirIni, dirFin, BankSize: integer; lbl: string);
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
  BankMask := BankSize-1;  //Máscara para el banco: $7F (Mid-range) o $1F (Baseline)

  x1 := marcoRam.Left;
  x2 := marcoRam.Right;
  altoByte := (marcoRam.Bottom - marcoRam.Top)/BankSize;
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
procedure TfraRamExplorer.DrawBlock(const marcoRam: TRect;
                                 ancMargenDir: integer;
                                 dirIni, dirFin, BankSize: integer);
{Similar a DrawBlockTxt(), pero no pone las etiquetas de dirección, ni la etiqueta
central.}
var
  cv: TCanvas;
  x1, x2: LongInt;
  altoByte: Double;
  y1, y2: integer;
  BankMask: Word;
begin
  BankMask := BankSize-1;  //Máscara para el banco: $7F (Mid-range) o $1F (Baseline)

  x1 := marcoRam.Left;
  x2 := marcoRam.Right;
  altoByte := (marcoRam.Bottom - marcoRam.Top)/BankSize;
  y1 := round(marcoRam.Top + (dirIni and BankMask) * altoByte);
  y2 := round(marcoRam.Top + ((dirFin and BankMask)+1) * altoByte);
  //Dibuja barra de fondo
  cv := panGraph.Canvas;
  cv.Rectangle(x1+ancMargenDir,
               y1, x2, y2+1);  //Corrige y2, porque Rectangle, dibuja hasta un pincel antes
end;
procedure TfraRamExplorer.DrawRAMbank(const marcoRam: TRect; selected: boolean);
{Dibuja el banco de RAM completo, en el área "marcoRam", separando por bloques
de acuerdo al campo "state" }
var
  i, ancMargenDir, j: integer;
  cv: TCanvas;
  lbl: String;
  tmp: integer;
begin
  cv := panGraph.Canvas;
  //Calcula el ancho de las etqiuetas
  ancMargenDir := cv.TextWidth('XXX');
  if ancMargenDir>(marcoRam.Right - marcoRam.Left)*0.5 then begin
    ancMargenDir := 0;
  end;
  //Dibuja primero los bloques de memoria usadas
  SplitInUsedRAM(0, pic.CPUMAXRAM-1);
  cv.Pen.Color := $80FF80;
  cv.Brush.Style := bsSolid;
  for i:=0 to high(blockUse) do begin
    if blockUse[i].used then begin
      cv.Brush.Color := $80FF80;
      DrawBlock(marcoRam, ancMargenDir,
                blockUse[i].add1, blockUse[i].add2, pic.CPUMAXRAM);  //dibuja;
    end;
  end;
  //Dibuja zonas de la RAM
  cv.Pen.Width := 1;
  cv.Pen.Color := clBlack;
  //Separa bloques
  SplitInStateRAM(0, pic.CPUMAXRAM-1);
  // Dibuja los bloques
  for i:=0 to high(blockSta) do begin
    //Crea etiqueta
    case blockSta[i].blkType of
    cs_impleGPR: begin
//      cv.Brush.Color := clWhite;
      cv.Brush.Color := clNone;
      lbl := 'GPR';
    end;
    cs_unimplem: begin
      cv.Brush.Color := clGray;
      lbl := 'Uninplemented';
    end;
    end;
    tmp := pic.CPUMAXRAM;
    //Dibuja el bloque de forma normal.
    cv.Brush.Style := bsSolid;
    DrawBlockTxt(marcoRam, ancMargenDir,
            blockSta[i].add1, blockSta[i].add2, tmp, lbl);  //dibuja;
  end;
  //Verifica si está seleccionada
  if selected then begin
    cv.Pen.Width := 2;
    cv.Pen.Color := clBlue;
    cv.Frame(marcoRam.Left+ancMargenDir-1, marcoRam.Top-1, marcoRam.Right+2, marcoRam.Bottom+3);
  end;
end;
procedure TfraRamExplorer.panGraphPaint(Sender: TObject);
var
  bordlat, ancPag, x0, i, separ, alto, bordSup, y0: Integer;
begin
  ////////////////////////
  if pic = nil then exit;
  //Espaciado entre bancos
  ancPag := panGraph.width;
  bordlat := ancPag div 2;
  separ := 0;
  if ancPag>15 then begin
    Dec(ancPag,5);
    separ := 5;
  end;

  bordSup := panGraph.height div 15;  //espacio superior
  alto := panGraph.height - 2* bordSup;
//debugln('panGraph.width: %d bordLat: %d', [panGraph.width, bordlat]);
  x0 := bordlat;
  y0 := bordsup;

  for i:=0 to 0 do begin
    DrawRAMbank(Rect(x0, y0, x0+ancPag+1, y0+alto), true);
    x0 := x0 + ancPag + separ;
  end;
end;
procedure TfraRamExplorer.SetCompiler(cxp0: TCompilerBase);
begin
  pic := cxp0.picCore;
  cxp := cxp0;
end;
constructor TfraRamExplorer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //self.OnPaint := @Frame1Paint;
  panGraph.OnPaint := @panGraphPaint;
end;
destructor TfraRamExplorer.Destroy;
begin

  inherited Destroy;
end;

end.

