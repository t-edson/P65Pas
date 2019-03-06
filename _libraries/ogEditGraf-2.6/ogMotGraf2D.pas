unit ogMotGraf2D;
{
ogMotGraf2D
===========
Por Tito Hinostroza 24/09/2014

Descripción
===========
Unidad que define a un motor gráfico, para realizar dibujos en pantalla.
Contiene funciones de dibujo para las principales formas.
Maneja desplazamientos y Zoom. Incluye definiciones y funciones para dibujo de controles
simples.
Maneja 2 tipos de coordenadas:
* Coordenadas de pantalla -> Se refieren comunmente como "xp" e "yp". Se usan para representar
posiciones en la pantalla. Se amneja en pixeles.
* Coordenadas virtuales -> Se refieren a las coordenadas virtuales internas con que trabajan
los objetos gráficos. Es variable de acuerdo al desplazamientos o pantallas. Son números en
coma flotante de tipo "single".

Las coordenadas de pantalla, trabajan en pixeles para acelerar los cálculos con gráficos.
Usa el objeto canvas para la salida gráfica. No se usan las opciones de cambio de coordenadas
y desplazamientos del Canvas, para tener mayor libertad de cambiar las transformaciones
gráficas. Basado en la clase equivalente CV2D desarrollada en VB.

}
{$mode objfpc}{$H+}
interface
uses Classes, Controls, SysUtils, Graphics, FPCanvas, Types, FPimage;
const
  COL_GRIS = $808080;          //gris

Type

//Define el Tipo de dato perspectiva
Tperspectiva = record
  zoom  : Real;        //zoom de la perspectiva
  x_cam : Single;     //parámetro de desplazamiento x_cam
  y_cam : Single;     //parámetro de desplazamiento y_cam
end;
//Par ordenado de Reales.
TFPoint = record
  x: single;
  y: single;
end;

{ TMotGraf }
TMotGraf = class
  //Parámetros de la cámara (perspectiva)
  x_cam      : Single;  //Coordenadas de la camara
  y_cam      : Single;
  Zoom       : Single;  //Factor de ampliación
  //Coordenadas de desplazamiento para ubicar el centro de la pantalla
  x_des      : integer;
  y_des      : Integer;

  ImageList  : TImageList;
  constructor IniMotGraf(canvas0: Tcanvas);
  procedure FijaModoEscrit(modo:TFPPenMode);
  procedure SetPen(estilo:TFPPenStyle; ancho:Integer; color:Tcolor);
  procedure SetBrush(ColorR:TColor);
  procedure SetColor(colLin,colRel:TColor; ancho: Integer = 1); //Fija colorde línea y relleno
  procedure SetLine(colLin:TColor; width: Integer = 1); //Fija características de línea

  procedure Line(x1, y1, x2, y2:Single);
  procedure Line0(x1, y1, x2, y2: Integer);
  procedure Rectang(x1, y1, x2, y2: Single);
  procedure Rectang0(x1, y1, x2, y2: Integer);
  procedure RectangR(x1, y1, x2, y2: Single);
  procedure RectangR0(x1, y1, x2, y2: Integer);
  procedure RectRedonR(x1, y1, x2, y2: Single);
  procedure Barra(x1, y1, x2, y2: Single; colFon: TColor=-1);
  procedure Barra0(x1, y1, x2, y2: Integer; colFon: TColor);
  procedure Ellipse(x1, y1, x2, y2: Single);
  procedure RadialPie(x1, y1, x2, y2: Single; StartAngle16Deg,
    Angle16DegLength: integer);
  procedure Polygon(x1, y1, x2, y2, x3, y3: Single; x4: Single=-10000;
    y4: Single=-10000; x5: Single=-10000; y5: Single=-10000; x6: Single=-10000;
    y6: Single=-10000);
  procedure Polygon(const Points: array of TFPoint);
  //funciones para texto
  procedure SetFont(Letra: string);
  procedure SetText(color: TColor);
  procedure SetText(color: TColor; tam: single);
  procedure SetText(negrita: Boolean=False; cursiva: Boolean=False;
    subrayado: Boolean=False);
  procedure SetText(color: TColor; tam: single; Letra: String;
    negrita: Boolean=False; cursiva: Boolean=False; subrayado: Boolean=False);
  procedure Texto(x1, y1: Single; txt: String);
  procedure TextRect(x1, y1, x2, y2: Single; x0, y0: Single; const Text: string;
    const Style: TTextStyle);
  procedure TextoR(x1, y1, ancho, alto: Single; txt: String);
  function TextWidth(const txt: string): single;  //ancho del texto

  procedure SavePerspectiveIn(var p: TPerspectiva);
  procedure ReadPerspectiveFrom(p: TPerspectiva);

  procedure SetWindow(ScaleWidth, ScaleHeight: Real; xMin, xMax, yMin, yMax: Real);
  procedure Scroll(dx, dy: Integer);
  procedure ObtenerDesplaz2(xr, yr: Integer; Xant, Yant: Integer; out dx,
    dy: Single);
  procedure DrawIcon(x1, y1: Single; idx: integer);
  procedure DrawImage(im: TGraphic; x1, y1, dx, dy: Single);
  procedure DrawImageN(im: TGraphic; x1, y1: Single);
  procedure DrawImage0(im: TGraphic; x1, y1, dx, dy: Integer);
public  //Funciones de transformación
  function XPant(x: Single): Integer;    //INLINE Para acelerar las llamadas
  function YPant(y: Single): Integer;    //INLINE Para acelerar las llamadas
  procedure XYpant(xv, yv: Single; out xp, yp: Integer);
  procedure XYvirt(xp, yp: Integer; out xv, yv: Single);
  function Xvirt(xr, {%H-}yr: Integer): Single;  //INLINE Para acelerar las llamadas
  function Yvirt(xr, yr: Integer): Single;  //INLINE Para acelerar las llamadas
public  //funciones básicas para dibujo de Controles
  procedure DibBorBoton(x1,y1:Single; ancho,alto: Single);
  procedure DibFonBotonOsc(x1, y1: Single; ancho, alto: Single);
  procedure DibCheck(px, py: Single; ancho, alto: Single);
  procedure DibVnormal(x1, y1: Single; ancho, alto: Single);
  procedure DrawTrianUp(x1,y1:Single; ancho,alto: Single);
  procedure DrawTrianDown(x1,y1:Single; ancho,alto: Single);
public
  Canvas    : Tcanvas;                 //referencia al lienzo
end;

implementation

//////////////////////////////// Funciones públicas //////////////////////////////
constructor TMotGraf.IniMotGraf(canvas0: Tcanvas);
begin
   Canvas := canvas0;
    //GetClientRect frmS.hwnd, tCR
    x_des := 0;
    y_des := 0;
    //posición de cámara
    x_cam := 0;
    y_cam := 0;
    //ampliación inicial
    Zoom := 1;
End;
procedure TMotGraf.FijaModoEscrit(modo: TFPPenMode);
begin
    Canvas.Pen.Mode := modo;
End;
procedure TMotGraf.SetPen(estilo:TFPPenStyle; ancho:Integer; color:Tcolor);
//Establece el lápiz actual de dibujo
begin
   Canvas.Pen.Style := estilo;
   Canvas.pen.Width := ancho;
   Canvas.pen.Color := color;
End;
procedure TMotGraf.SetBrush(ColorR:TColor);
//Establece el relleno actual
begin
   Canvas.Brush.Style := bsSolid;  //estilo sólido
   Canvas.Brush.Color:=ColorR;
End;

procedure TMotGraf.SetColor(colLin, colRel: TColor; ancho: Integer = 1);
//Fija un color de línea y un color de relleno. La línea se fija a estilo sólido
//y el relleno también
begin
    Canvas.Pen.Style := psSolid;
    Canvas.pen.Width := ancho;
    Canvas.pen.Color := colLin;

    Canvas.Brush.Style:=bsSolid;
    Canvas.Brush.Color:=colRel;
end;
procedure TMotGraf.SetLine(colLin: TColor; width: Integer);
begin
  Canvas.Pen.Style := psSolid;
  Canvas.pen.Width := width;
  Canvas.pen.Color := colLin;
end;
//funciones para texto
procedure TMotGraf.SetFont(Letra: string);
//Permite definir el tipo de letra actual
begin
  if Letra = '' then Canvas.Font.Name:= 'MS Sans Serif';
  //'Times New Roman'
end;
procedure TMotGraf.SetText(color: TColor);
begin
  Canvas.Font.Color := color;
end;
procedure TMotGraf.SetText(color: TColor; tam: single);
//método sencillo para cambiar propiedades del texto
begin
   Canvas.Font.Color := color;
   Canvas.Font.Size := round(tam * Zoom);
end;
procedure TMotGraf.SetText(negrita:Boolean = False; cursiva: Boolean = False;
            subrayado: Boolean = False);
//Establece las características completas del texto
begin
   Canvas.Font.Bold := negrita;
   Canvas.Font.Italic := cursiva;
   Canvas.Font.Underline := subrayado;
End;
procedure TMotGraf.SetText(color: TColor; tam: single; //; nDegrees As Single, _
            Letra: String;
            negrita:Boolean = False;
            cursiva: Boolean = False;
            subrayado: Boolean = False);
//Establece las características completas del texto
begin
   Canvas.Font.Color := color;
   Canvas.Font.Size := round(tam * Zoom);
   if Letra <> '' then Canvas.Font.Name:=letra;
   Canvas.Font.Bold := negrita;
   Canvas.Font.Italic := cursiva;
   Canvas.Font.Underline := subrayado;
End;
procedure TMotGraf.Texto(x1, y1: Single; txt: String);
//Escribe un texto
begin
   Canvas.Brush.Style := bsClear;  //Fondo transparente
//   tmp := Canvas.Font.Size;  //guarda tamaño actual
//   Canvas.Font.Size := round(Canvas.Font.Size * Zoom);
   Canvas.TextOut(XPant(x1), YPant(y1), txt);
//   Canvas.Font.Size := tmp;  //restaura
   Canvas.Brush.Style := bsSolid;  //devuelve estilo de fondo
End;
procedure TMotGraf.TextRect(x1,y1,x2,y2: Single; x0, y0: Single; const Text: string;
                       const Style: TTextStyle);
//Escribe un texto
var
  Arect: TRect;
begin
   Canvas.Brush.Style := bsClear;  //Fondo transparente
//   tmp := Canvas.Font.Size;  //guarda tamaño actual
//   Canvas.Font.Size := round(Canvas.Font.Size * Zoom);
   ARect.Left   := XPant(x1);
   ARect.Top    := YPant(y1);
   ARect.Right  := XPant(x2);
   ARect.Bottom := YPant(y2);
   Canvas.TextRect(Arect, XPant(x0), YPant(y0), Text, Style);
//   Canvas.Font.Size := tmp;  //restaura
   Canvas.Brush.Style := bsSolid;  //devuelve estilo de fondo
End;
procedure TMotGraf.TextoR(x1, y1, ancho, alto: Single; txt: String);
//Escribe un texto
var r:TRect;
    //s:TTextStyle;
begin
//    Canvas.Font.Color:=clred;
   Canvas.Brush.Style := bsClear;  //Fondo transparente
   Canvas.Font.Size := round(11 * Zoom);
   r.Left := XPant(x1);
   r.Top := YPant(y1);
   r.Right := XPant(x1+ancho);     { TODO : Ver como dibujar texto no limitado }
   r.Bottom:= YPant(y1+alto);
//   s.Alignment:=taRightJustify;  //alineado a la derecha
// Canvas.TextRect(r,r.Left,r.Top,txt,s);//No permite cambia el tamaño de letra!!!!
   Canvas.TextRect(r,r.Left,r.Top,txt);
   Canvas.Brush.Style := bsSolid;  //devuelve estilo de fondo
End;
function TMotGraf.TextWidth(const txt: string): single;
begin
  Result := Canvas.TextWidth(txt) * Zoom;
end;

(*
Sub FijaTextoF(l As CFLetra)
//Establece las características de texto, por medio de una clase "CFLetra"
Dim grosor As Long
Dim nHeight As Long
    SetTextColor hdc, l.col
    If hFont <> 0 Then DeleteObject hFont
    If l.negrita Then grosor = FW_BOLD Else grosor = FW_NORMAL
    nHeight = -MulDiv(l.tam, GetDeviceCaps(hdc, LOGPIXELSY) * mZoom, 72)
    If nHeight = 0 Then nHeight = -1        //limita tamaño a un mínimo
    hFont = CreateFont(nHeight, _
        0, l.inclinacion * 10, 0, grosor, l.cursiva, l.subrayado, False, DEFAULT_CHARSET, _
        OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, PROOF_QUALITY, DEFAULT_PITCH, l.tipo)
    SelectObject hdc, hFont                 //queda pendiente eliminarlo
End;

Public Sub FijaAlineamiento(Optional alineac As Long = TA_LEFT)
    SetTextAlign hdc, alineac
End;

Public Sub ftexto(ByVal x1 As Single, ByVal y1 As Single, _
                 txt As String, Optional alineac As Long = MVW_CEN_CEN, _
                 Optional ByVal Ang As Single = 0)
//Escribe un texto, con opciones completas de alineamiento, 4 cuadrantes y 4 ejes
//Es más lenta que texto() para los casos de texto centrado
//Permite centrar con ángulo. "ang" es el ángulo de inclinación en grados
Dim x1c As Single, y1c As Single   //coordenadas corregidas
Dim ancho_txt As Single
Dim alto_txt As Single

Dim rc As RECT
    Select Case alineac
    Case MVW_SUP_DER
        x1c = (x1 - x_cam) * mZoom + x_des
        y1c = (y1 - y_cam) * mZoom + y_des
        SetTextAlign hdc, TA_RIGHT + TA_TOP
        TextOut hdc, x1c, y1c, txt, Len(txt)
    Case MVW_SUP_IZQ
        x1c = (x1 - x_cam) * mZoom + x_des
        y1c = (y1 - y_cam) * mZoom + y_des
        SetTextAlign hdc, TA_LEFT + TA_TOP
        TextOut hdc, x1c, y1c, txt, Len(txt)
    Case MVW_SUP_CEN
        x1c = (x1 - x_cam) * mZoom + x_des
        y1c = (y1 - y_cam) * mZoom + y_des
        SetTextAlign hdc, TA_CENTER + TA_TOP
        TextOut hdc, x1c, y1c, txt, Len(txt)
    Case MVW_CEN_DER
        Call LeeGeomTextoZ(txt, ancho_txt, alto_txt)
        If Ang = 0 Then
            x1 = x1
            y1 = y1 - alto_txt / 2
        Else
            Ang = Ang / 180 * PI
            x1 = x1 - alto_txt / 2 * Sin(Ang)
            y1 = y1 - alto_txt / 2 * Cos(Ang)
        End If
        x1c = (x1 - x_cam) * mZoom + x_des
        y1c = (y1 - y_cam) * mZoom + y_des
        SetTextAlign hdc, TA_RIGHT + TA_TOP
        TextOut hdc, x1c, y1c, txt, Len(txt)
    Case MVW_CEN_IZQ
        Call LeeGeomTextoZ(txt, ancho_txt, alto_txt)
        If Ang = 0 Then
            x1 = x1
            y1 = y1 - alto_txt / 2
        Else
            Ang = Ang / 180 * PI
            x1 = x1 - alto_txt / 2 * Sin(Ang)
            y1 = y1 - alto_txt / 2 * Cos(Ang)
        End If
        x1c = (x1 - x_cam) * mZoom + x_des
        y1c = (y1 - y_cam) * mZoom + y_des
        SetTextAlign hdc, TA_LEFT + TA_TOP
        TextOut hdc, x1c, y1c, txt, Len(txt)
    Case MVW_CEN_CEN
        Call LeeGeomTextoZ(txt, ancho_txt, alto_txt)
        If Ang = 0 Then
            x1 = x1 - ancho_txt / 2
            y1 = y1 - alto_txt / 2
        Else
            Ang = Ang / 180 * PI
            //****No implementado****
        End If
        x1c = (x1 - x_cam) * mZoom + x_des
        y1c = (y1 - y_cam) * mZoom + y_des
        SetTextAlign hdc, TA_LEFT + TA_TOP
        TextOut hdc, x1c, y1c, txt, Len(txt)
    Case MVW_INF_DER
        x1c = (x1 - x_cam) * mZoom + x_des
        y1c = (y1 - y_cam) * mZoom + y_des
        SetTextAlign hdc, TA_RIGHT + TA_BOTTOM
        TextOut hdc, x1c, y1c, txt, Len(txt)
    Case MVW_INF_IZQ
        x1c = (x1 - x_cam) * mZoom + x_des
        y1c = (y1 - y_cam) * mZoom + y_des
        SetTextAlign hdc, TA_LEFT + TA_BOTTOM
        TextOut hdc, x1c, y1c, txt, Len(txt)
    Case MVW_INF_CEN
        x1c = (x1 - x_cam) * mZoom + x_des
        y1c = (y1 - y_cam) * mZoom + y_des
        SetTextAlign hdc, TA_CENTER + TA_BOTTOM
        TextOut hdc, x1c, y1c, txt, Len(txt)
    End Select
End;

Public Sub texto0(x1 As Single, y1 As Single, txt As String)
//Escribe un texto "sin transformación"
Dim x1c As Single, y1c As Single  //coordenadas corregidas
    x1c = x1
    y1c = y1
    SetTextAlign hdc, TA_LEFT
    TextOut hdc, x1c, y1c, txt, Len(txt)
End;

Public Sub MultiTexto(cad As String, x1 As Single, y1 As Single, _
                                      x2 As Single, y2 As Single, _
                                      Optional color As Long = vbBlack, _
                                      Optional alinea As Long = DT_LEFT)
//Escribe texto en varias líneas, en el rectángulo indicado. Realiza saltos entre palabras
//o cuando una palabra excede el ancho del cuadro. Si una línea adicional no entra completa
//no se visualiza. Si la cadena completa no entra en el cuadro se recorta
Dim rc As RECT
    rc.Left = (x1 - x_cam) * mZoom + x_des: rc.Top = (y1 - y_cam) * mZoom + y_des
    rc.Right = (x2 - x_cam) * mZoom + x_des: rc.Bottom = (y2 - y_cam) * mZoom + y_des
    SetTextColor hdc, color
    //DrawText hdc, cad, Len(cad), rc, DT_WORDBREAK + DT_EDITCONTROL //+ DT_END_ELLIPSIS
    DrawText hdc, cad, Len(cad), rc, DT_WORDBREAK + alinea
End;

Public Sub MultiTexto0(cad As String, x1 As Single, y1 As Single, _
                                      x2 As Single, y2 As Single, _
                                      Optional color As Long = vbBlack)
//Escribe texto en varias líneas, en el rectángulo indicado. Realiza saltos entre palabras
//o cuando una palabra excede el ancho del cuadro. Si una línea adicional no entra completa
//no se visualiza. Si la cadena completa no entra en el cuadro se recorta y se le agrega "..."
Dim rc As RECT
    rc.Left = x1: rc.Top = y1
    rc.Right = x2: rc.Bottom = y2
    SetTextColor hdc, color
    DrawText hdc, cad, Len(cad), rc, DT_WORDBREAK + DT_EDITCONTROL //+ DT_END_ELLIPSIS
End;
*)
procedure TMotGraf.Line(x1, y1, x2, y2: Single);
//Dibuja una línea
begin
   Canvas.Line(XPant(x1), YPant(y1), XPant(x2), YPant(y2));
End;
procedure TMotGraf.Line0(x1, y1, x2, y2: Integer);
//Dibuja una línea , sin transformación
begin
   Canvas.Line(x1, y1, x2, y2);
End;

(*
Public Sub Circulo(x1 As Single, y1 As Single, radio As Single)
//Dibuja un círculo relleno
Dim x1c As Single, y1c As Single  //coordenadas corregidas
Dim rc As Single
    x1c = (x1 - x_cam) * mZoom + x_des
    y1c = (y1 - y_cam) * mZoom + y_des
    rc = radio * mZoom
    Ellipse hdc, (x1c - rc), (y1c - rc), (x1c + rc), (y1c + rc)
End;

Public Sub circulo0(x1 As Single, y1 As Single, radio As Single)
//Dibuja un círculo relleno, "sin transformación"
Dim x1c As Single, y1c As Single  //coordenadas corregidas
Dim rc As Single
    x1c = x1
    y1c = y1
    rc = radio
    Ellipse hdc, (x1c - rc), (y1c - rc), (x1c + rc), (y1c + rc)
End;

Public Sub Arco(x1 As Single, y1 As Single, radio As Single, _
                AngIni As Single, Ang As Single)
Dim x1c As Single, y1c As Single  //coordenadas corregidas
Dim rc As Single
Dim PT As POINTAPI
    x1c = (x1 - x_cam) * mZoom + x_des
    y1c = (y1 - y_cam) * mZoom + y_des
    rc = radio * mZoom
    BeginPath hdc
    MoveToEx hdc, x1c, y1c, ret_pt
    AngleArc hdc, x1c, y1c, rc, AngIni, Ang
    LineTo hdc, x1c, y1c
    EndPath hdc
    StrokeAndFillPath hdc
    //arc hdc,  (x1c - rc), (y1c - rc), (x1c + rc), (y1c + rc),
End;

*)

procedure TMotGraf.Rectang(x1, y1, x2, y2: Single);
//Dibuja un rectángulo
begin
    Canvas.Frame(XPant(x1), YPant(y1), XPant(x2), YPant(y2));
End;
procedure TMotGraf.Rectang0(x1, y1, x2, y2: Integer);
//Dibuja un rectángulo sin "transformación"
begin
    Canvas.Frame(x1, y1, x2, y2);
End;
procedure TMotGraf.RectangR(x1, y1, x2, y2: Single);
//Dibuja un rectángulo relleno
begin
    Canvas.Rectangle(XPant(x1), YPant(y1), XPant(x2), YPant(y2));
End;
procedure TMotGraf.RectangR0(x1, y1, x2, y2: Integer);
//Dibuja un rectángulo relleno sin "transformación"
begin
    Canvas.Rectangle(x1, y1, x2, y2);
End;
procedure TMotGraf.RectRedonR(x1, y1, x2, y2: Single);
//Dibuja un rectángulo relleno con bordes redondeados
begin
    Canvas.RoundRect(XPant(x1), YPant(y1), XPant(x2), YPant(y2), round(10 * Zoom), Round(10 * Zoom));
End;
procedure TMotGraf.Barra(x1, y1, x2, y2: Single; colFon: TColor = -1);
//Rellena un área rectangular, no rellena el borde derecho e inferior.
//Es más rápido que rellenar con Rectangle()
var rc: TRect;
begin
    rc.Left   := XPant(x1);
    rc.Top    := YPant(y1);
    rc.Right  := XPant(x2);
    rc.Bottom := YPant(y2);
    if colFon<> -1 then Canvas.Brush.Color := colFon;
    Canvas.FillRect(rc); //fondo
End;
procedure TMotGraf.Barra0(x1, y1, x2, y2: Integer; colFon: TColor);
//Rellena un área rectangular, no rellena el borde derecho e inferior.
//Es más rápido que rellenar con Rectangle()
begin
    Canvas.Brush.Color := colFon;
    Canvas.FillRect(x1,y1,x2,y2); //fondo
End;
procedure TMotGraf.Ellipse(x1, y1, x2, y2: Single);
begin
  Canvas.Ellipse(XPant(x1), YPant(y1), XPant(x2), YPant(y2));
end;
procedure TMotGraf.RadialPie(x1, y1, x2, y2: Single; StartAngle16Deg, Angle16DegLength: integer);
begin
  Canvas.RadialPie(XPant(x1), YPant(y1), XPant(x2), YPant(y2), StartAngle16Deg, Angle16DegLength);
end;

(*
Public Sub polilinea(x1 As Single, y1 As Single, _
                  x2 As Single, y2 As Single, _
                  x3 As Single, Y3 As Single, _
                  Optional x4 As Single = -10000, Optional y4 As Single = -10000, _
                  Optional x5 As Single = -10000, Optional y5 As Single = -10000, _
                  Optional x6 As Single = -10000, Optional y6 As Single = -10000)
//Dibuja un polígono usando llamadas a la API de Windows
Dim Ptos3(1 To 7) As Tpunto      //puntos
Dim ptos(1 To 7) As POINTAPI    //arreglo de puntos a dibujar
Dim nPtos As Long
Dim x1c As Single, y1c As Single
Dim i As Integer
 Ptos3(1).X = x1: Ptos3(1).Y = y1
 Ptos3(2).X = x2: Ptos3(2).Y = y2
 Ptos3(3).X = x3: Ptos3(3).Y = Y3
 nPtos = 3
 If x4 <> -10000 Then Ptos3(4).X = x4: Ptos3(4).Y = y4: nPtos = 4
 If x5 <> -10000 Then Ptos3(5).X = x5: Ptos3(5).Y = y5: nPtos = 5
 If x6 <> -10000 Then Ptos3(6).X = x6: Ptos3(6).Y = y6: nPtos = 6
 //cierra el polígono
 Ptos3(nPtos + 1).X = Ptos3(1).X
 Ptos3(nPtos + 1).Y = Ptos3(1).Y
 //transformación
 For i = 1 To nPtos + 1
     ptos(i).X = (x_des + (Ptos3(i).X - x_cam) * mZoom)
     ptos(i).Y = (y_des + (Ptos3(i).Y - y_cam) * mZoom)
 Next
 Call Polyline(hdc, ptos(1), nPtos + 1)
End;

Public Sub polilinea0(x1 As Single, y1 As Single, _
                  x2 As Single, y2 As Single, _
                  x3 As Single, Y3 As Single, _
                  Optional x4 As Single = -10000, Optional y4 As Single = -10000, _
                  Optional x5 As Single = -10000, Optional y5 As Single = -10000, _
                  Optional x6 As Single = -10000, Optional y6 As Single = -10000)
//Dibuja un polígono usando llamadas a la API de Windows, sin "transformación"
Dim Ptos3(1 To 7) As Tpunto      //puntos
Dim ptos(1 To 7) As POINTAPI    //arreglo de puntos a dibujar
Dim nPtos As Long
Dim x1c As Single, y1c As Single
Dim i As Integer
 Ptos3(1).X = x1: Ptos3(1).Y = y1
 Ptos3(2).X = x2: Ptos3(2).Y = y2
 Ptos3(3).X = x3: Ptos3(3).Y = Y3
 nPtos = 3
 If x4 <> -10000 Then Ptos3(4).X = x4: Ptos3(4).Y = y4: nPtos = 4
 If x5 <> -10000 Then Ptos3(5).X = x5: Ptos3(5).Y = y5: nPtos = 5
 If x6 <> -10000 Then Ptos3(6).X = x6: Ptos3(6).Y = y6: nPtos = 6
 //cierra el polígono
 Ptos3(nPtos + 1).X = Ptos3(1).X
 Ptos3(nPtos + 1).Y = Ptos3(1).Y
 //transformación
 For i = 1 To nPtos + 1
     ptos(i).X = Ptos3(i).X
     ptos(i).Y = Ptos3(i).Y
 Next
 Call Polyline(hdc, ptos(1), nPtos + 1)
End;

Public Sub poligono0(x1 As Single, y1 As Single, _
                  x2 As Single, y2 As Single, _
                  x3 As Single, Y3 As Single, _
                  Optional x4 As Single = -10000, Optional y4 As Single = -10000, _
                  Optional x5 As Single = -10000, Optional y5 As Single = -10000, _
                  Optional x6 As Single = -10000, Optional y6 As Single = -10000)
//Dibuja un polígono relleno usando llamadas a la API de Windows "sin transformación"
Dim Ptos3(1 To 7) As Tpunto      //puntos
Dim ptos(1 To 7) As POINTAPI    //arreglo de puntos a dibujar
Dim nPtos As Long
Dim x1c As Single, y1c As Single
Dim i As Integer
 Ptos3(1).X = x1: Ptos3(1).Y = y1
 Ptos3(2).X = x2: Ptos3(2).Y = y2
 Ptos3(3).X = x3: Ptos3(3).Y = Y3
 nPtos = 3
 If x4 <> -10000 Then Ptos3(4).X = x4: Ptos3(4).Y = y4: nPtos = 4
 If x5 <> -10000 Then Ptos3(5).X = x5: Ptos3(5).Y = y5: nPtos = 5
 If x6 <> -10000 Then Ptos3(6).X = x6: Ptos3(6).Y = y6: nPtos = 6
 //cierra el polígono
 Ptos3(nPtos + 1).X = Ptos3(1).X
 Ptos3(nPtos + 1).Y = Ptos3(1).Y
 //transformación
 For i = 1 To nPtos + 1
     ptos(i).X = Ptos3(i).X
     ptos(i).Y = Ptos3(i).Y
 Next
 Call Polygon(hdc, ptos(1), nPtos + 1)   //dibuja borde
End;
*)
procedure TMotGraf.Polygon(x1, y1, x2, y2, x3, y3 : Single;
                  x4: Single = -10000; y4: Single = -10000;
                  x5: Single = -10000; y5: Single = -10000;
                  x6: Single = -10000; y6: Single = -10000);
//Dibuja un polígono relleno.
var
  Ptos: array of TPoint;    //arreglo de puntos a dibujar
  nPtos: integer;
begin
  //calcula número de puntos
  If x4 = -10000 Then nPtos := 3
  else if x5 = -10000 Then nPtos := 4
       else If x6 = -10000 Then nPtos := 5
            else nPtos := 6;
  SetLength(Ptos, nPtos);   //dimensiona
  //Llena arreglo
  Ptos[0].x := XPant(x1); Ptos[0].y := YPant(y1);
  Ptos[1].x := XPant(x2); Ptos[1].y := YPant(y2);
  Ptos[2].x := XPant(x3); Ptos[2].y := YPant(y3);
  If x4 <> -10000 Then begin
    Ptos[3].x := XPant(x4); Ptos[3].y := YPant(y4);
  end;
  If x5 <> -10000 Then begin
    Ptos[4].x := XPant(x5); Ptos[4].y := YPant(y5);
  end;
  If x6 <> -10000 Then begin
    Ptos[5].x := XPant(x6); Ptos[5].y := YPant(y6);
  end;
  Canvas.Polygon(Ptos);   //dibuja
End;
procedure TMotGraf.Polygon(const Points: array of TFPoint);
//Dibuja un polígono relleno.
var
  Ptos: array of TPoint;    //arreglo de puntos a dibujar
  i: Integer;
begin
  SetLength(Ptos, high(Points)+1);   //dimensiona
  //transforma puntos
  for i:= 0 to high(Points) do begin
    Ptos[i].x := XPant(Points[i].x);
    Ptos[i].y := YPant(Points[i].y);
  end;
  Canvas.Polygon(Ptos);   //dibuja
End;

//*****************************FUNCIONES DE TRANSFORMACIÓN********************************
//Las siguientes funciones son por así decirlo, "estandar".
//Cuando se creen otras clases de dispositivo interfase gráfica deberían tener también estas
//funciones que son siempre necesarias.
function TMotGraf.XPant(x:Single): Integer; INLINE;    //INLINE Para acelerar las llamadas
//Función de la geometría del motor. Da la transformación lineal de la coordenada x.
begin
   XPant := Round((x - x_cam) * Zoom + x_des);
end;
function TMotGraf.YPant(y:Single): Integer; INLINE;    //INLINE Para acelerar las llamadas
//Función de la geometría del motor. Da la transformación lineal de la coordenada y.
begin
   YPant := Round((y - y_cam) * Zoom + y_des);
end;
procedure TMotGraf.XYpant(xv, yv: Single; out xp, yp: Integer);
//Devuelve las coordenadas de pantalla para un punto virtual (x,y,z).
begin
    xp := Xpant(xv);
    yp := Ypant(yv);
End;
function TMotGraf.Xvirt(xr, yr: Integer): Single;  //INLINE Para acelerar las llamadas
//Obtiene la coordenada X virtual (del punto X,Y,Z ) a partir de unas coordenadas de pantalla
begin
    Xvirt := (xr - x_des) / Zoom + x_cam;
End;
function TMotGraf.Yvirt(xr, yr: Integer): Single;  //INLINE Para acelerar las llamadas
//Obtiene la coordenada Y virtual (del punto X,Y,Z ) a partir de unas coordenadas de pantalla
begin
    Yvirt := (yr - y_des) / Zoom + y_cam;
End;
procedure TMotGraf.XYvirt(xp, yp: Integer; out xv, yv: Single);
//Devuelve las coordenadas virtuales xv,yv a partir de unas coordenadas de pantalla
//(o del ratón). Equivale a intersecar un plano
//paralelo al plano XY con la línea de mira del ratón en pantalla.
begin
    xv := Xvirt(xp, yp);
    yv := Yvirt(yp, yp);
End;
procedure TMotGraf.SetWindow(ScaleWidth, ScaleHeight: Real;
               xMin, xMax, yMin, yMax: Real);
//Fija las coordenadas de pantalla de manera que se ajusten a las nuevas que se dan
//Recibe coordenadas virtuales
var zoomX: Real;
    zoomY: Real;
    dxcen: Real; //Desplazamiento en x para centrar
    dycen: Real; //Desplazamiento en y para centrar
begin
   If xMax <= xMin Then Exit;
   If yMax <= yMin Then Exit;
   //calcula el zoom por efecto de dX
   zoomX := ScaleWidth / (xMax - xMin);
   //calcula el zoom por efecto de dY
   zoomY := ScaleHeight / (yMax - yMin);
   //toma el zoom menor, en caso de relación de aspecto diferente de 1
   If zoomY > zoomX Then   //toma el zoom de x
      begin
        Zoom := zoomX;
        dxcen := 0;
        dycen := (ScaleHeight / Zoom - (yMax - yMin)) / 2;   //para centrar en vertical
      end
   Else  //zoomX > zoomy    ,toma el zoom de y
      begin
        Zoom := zoomY;
        dycen := 0;
        dxcen := (ScaleWidth / Zoom - (xMax - xMin)) / 2;   //para centrar en horizontal
      end;
   //fija las coordenadas de cámara
   x_cam := xMin + x_des / Zoom - dxcen;
   y_cam := yMin + y_des / Zoom - dycen;
End;
procedure TMotGraf.Scroll(dx, dy: Integer);
//Desplaza el escenario (el punto de rotación siempre está en el centro de la pantalla)
begin
  y_cam := y_cam - dy;
  x_cam := x_cam - dx;
End;
procedure TMotGraf.ObtenerDesplaz2(xr, yr: Integer; Xant, Yant: Integer;
  out dx, dy: Single);
//Obtiene los desplazamientos dx, dy para los objetos gráficos en base a
//los movimientos del ratón. Sólo desplaza en 2D
begin
    //desplazamiento en plano XY en caso alfa=0, fi=0
    dx := (xr - Xant) / Zoom;
    dy := (yr - Yant) / Zoom;
End;
(*
Public Function LeeGeomTextoZ(cad As String, ancho As Single, alto As Single)
//Devuelve el ancho y alto del texto. El ancho y alto del texto debe ser independiente
//del zoom, pero como despues de llamar a FijaTexto se altera el tamaño del font
//se requiere la corrección por el factor del zoom.
Dim szText As SIZE
Dim res As Long
    res = GetTextExtentPoint32(hdc, cad, Len(cad), szText)
    ancho = szText.cx / mZoom
    alto = szText.cy / mZoom
End Function

Public Function LeeGeomTexto0(cad As String, ancho As Single, alto As Single)
//Devuelve el ancho y alto del texto. No considera el zoom.
Dim szText As SIZE
Dim res As Long
    res = GetTextExtentPoint32(hdc, cad, Len(cad), szText)
    ancho = szText.cx
    alto = szText.cy
End Function

Public Function NCaracTextAncho(cad As String, ancho As Long) As Long
//Devuelve el número de caracteres de una línea que entran en un ancho determinado
Dim s As SIZE
Dim entran As Long
    GetTextExtentExPoint hdc, cad, Len(cad), ancho, entran, ByVal 0&, s
    NCaracTextAncho = entran
End Function

*)
//////////////////////////////  FUNCIONES DE PERSPECTIVA  //////////////////////////////
procedure TMotGraf.SavePerspectiveIn(var p: TPerspectiva);
//guarda sus datos de perspectiva en una variable perspectiva
begin
  p.x_cam := x_cam;
  p.y_cam := y_cam;
  p.zoom := Zoom;
End;
procedure TMotGraf.ReadPerspectiveFrom(p: TPerspectiva);
//lee sus datos de perspectiva de una variable perspectiva
begin
  x_cam := p.x_cam;
  y_cam := p.y_cam;
  Zoom := p.Zoom;
End;

//*********************************************************************************
procedure TMotGraf.DrawIcon(x1, y1: Single; idx: integer);
//Dibuja una de las imágenes alamcenadas en la propiedad ImageList
begin
  ImageList.Draw(Canvas, XPant(x1),YPant(y1), idx);
end;
procedure TMotGraf.DrawImage(im: TGraphic; x1, y1, dx, dy: Single);
//Dibuja imagen. Debe recibir el ancho y alto de la imagen
//en pixeles. Estos valores "ancho" y "alto" no se puede obtener
//de manera directa con im.Width e im.Height, porque vienen en unidades
//HIMETRIC, y se requeriría acceder al método Scale() para la conversión.
//
var r:TRect;
begin
   if im = nil then exit;
    r.Left:=XPant(x1);
    r.Top :=YPant(y1);
    r.Right :=r.Left + round(dx * Zoom);
    r.Bottom:=r.Top + round(dy * Zoom);
    Canvas.StretchDraw(r,im);
End;
procedure TMotGraf.DrawImageN(im: TGraphic; x1, y1: Single);
//Igual a DibujarImagen() pero no hace escalamiento de la imagen, solo por el Zoom.
var r:TRect;
begin
  if im = nil then exit;
//   Canvas.Draw(XPant(x1), YPant(y1),im);
  r.Left:=XPant(x1);
  r.Top :=YPant(y1);
  r.Right :=r.Left + round(im.Width * Zoom); //se probó quitándole 1, pero así cuadra mejor
  r.Bottom:=r.Top + round(im.Height * Zoom);
  Canvas.StretchDraw(r,im);
End;
procedure TMotGraf.DrawImage0(im: TGraphic; x1, y1, dx, dy: Integer);
//Dibuja imagen sin transformación
var r:TRect;
begin
  if im = nil then exit;
//    Canvas.Draw(x1, y1,im);
   r.Left:=x1;
   r.Top :=y1;
   r.Right:=x1+dx;
   r.Bottom:=y1+dy;
   Canvas.StretchDraw(r,im);
//   Canvas.StretchDraw(x1,y1,dx,dy,im); //por algún motivo no funciona{ TODO : ???? Debería funcionar }
End;

////////////////////////  Funciones de Dibujo de Controles /////////////////////////
procedure TMotGraf.DibBorBoton(x1, y1: Single; ancho, alto: Single);
//Dibuja el borde de los botones
begin
   SetColor(clGray, clWhite, 1);
   Canvas.RoundRect(XPant(x1), YPant(y1), XPant(x1+ancho), YPant(y1+alto),
                    round(6 * Zoom), Round(6 * Zoom));
end;
procedure TMotGraf.DibFonBotonOsc(x1, y1: Single; ancho, alto: Single);
//Dibuja el fondo de los botones
begin
   SetColor(clGray, clScrollBar, 1);
   Canvas.RoundRect(XPant(x1), YPant(y1), XPant(x1+ancho), YPant(y1+alto),
                    round(6 * Zoom), Round(6 * Zoom));
end;
procedure TMotGraf.DibVnormal(x1, y1: Single; ancho, alto: Single);
//Dibuja una V en modo normal. Usado para dibujar el ícono de los botones
var xm: Single;
begin
    SetPen(psSolid,2,clGray);
    xm := x1 + round(ancho/2);  //se redondea antes (ancho/2), para evitar vavriación en la
                                //posición, al dibujar en diferentes posiciones.
    Line(x1, y1, xm, y1+alto);
    Line(xm,y1+alto,x1+ancho,y1);
end;
procedure TMotGraf.DibCheck(px, py: Single; ancho, alto: Single);
//Dibuja una marca de tipo "Check". Útil para implementar el control "Check"
var xm: Single;
begin
    SetPen(psSolid,2,clGray);
    xm := round(ancho/4);
    Line(px     , py + 3, px + xm, py + alto);
    Line(px + xm, py + alto, px + ancho, py );
End;
procedure TMotGraf.DrawTrianUp(x1, y1: Single; ancho, alto: Single);
//Dibuja un pequeño triángulo apuntando hacia arriba
var
  Ptos: array of TPoint;    //arreglo de puntos a dibujar
begin
  SetLength(Ptos, 3);   //dimensiona
  //Llena arreglo
  Ptos[0].x := XPant(x1);         Ptos[0].y := YPant(y1+alto);
  Ptos[1].x := XPant(x1+ancho);   Ptos[1].y := YPant(y1+alto);
  Ptos[2].x := XPant(x1+ancho/2); Ptos[2].y := YPant(y1);
  Canvas.Polygon(Ptos);   //dibuja
end;
procedure TMotGraf.DrawTrianDown(x1, y1: Single; ancho, alto: Single);
//Dibuja un pequeño triángulo apuntando hacia abajo
var
  Ptos: array of TPoint;    //arreglo de puntos a dibujar
begin
  SetLength(Ptos, 3);   //dimensiona
  //Llena arreglo
  Ptos[0].x := XPant(x1);         Ptos[0].y := YPant(y1);
  Ptos[1].x := XPant(x1+ancho);   Ptos[1].y := YPant(y1);
  Ptos[2].x := XPant(x1+ancho/2); Ptos[2].y := YPant(y1+alto);
  Canvas.Polygon(Ptos);   //dibuja
end;

end.

