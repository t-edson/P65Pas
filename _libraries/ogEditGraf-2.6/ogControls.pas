{Unidad que define controles de formulario quse se dibujan en modo gráfico.
Esta unidad aún está incompleta y requiere reviisón y ampliación.}
unit ogControls;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Controls, Types, ExtCtrls, fgl, Graphics,
  ogDefObjGraf, ogMotGraf2D;
type
  //Procedimiento-evento para evento Click en Botón
  TEvenBTclk = procedure(estado: Boolean) of object;

  TTipBot =
   (BOT_CERRAR,   //botón cerrar
    BOT_EXPAND,   //botón expandir/contraer
    BOT_CHECK,    //check
    BOT_REPROD);   //reproducir/detener

  TSBOrientation =
   (SB_HORIZONT,    //horizontal
    SB_VERTICAL);   //vertical

  { TogButton }
  { Objeto TogButton - Permite gestionar los botones}
  TogButton = class(TObjVsible)
    estado     : Boolean;   //Permite ver el estado del botón o el check
    drawBack   : boolean;   //indica si debe dibujar el fondo
    constructor Create(mGraf: TMotGraf; tipo0: TTipBot; EvenBTclk0: TEvenBTclk);
    procedure Dibujar;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
  private
    tipo       : TTipBot;
    OnClick: TEvenBTclk
  end;

  { TogCheckBox }   //////////No implementado
  TogCheckBox = class(TObjVsible)
    estado     : Boolean;   //Permite ver el estado del botón o el check
    procedure Dibujar;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
  private
    tipo     : TTipBot;
    //OnClick  : TEvenBTclk
  end;

  { TogScrollBar }
  //Este ScrollBar está diseñado para manejara desplazamientos con valores discretos
  TogScrollBar = class(TObjVsible)
    valMin   : integer;   //valor mínimo
    valMax   : integer;   //valor máximo
    valCur   : integer;   //valor actual
    step     : integer;   //valor del paso
    page     : integer;      //cantidad de elementos por página
    pulsado  : boolean;   //bandera para temporización
    constructor Create(mGraf: TMotGraf; tipo0: TSBOrientation; EvenBTclk0: TEvenBTclk);
    destructor Destroy; override;
    procedure Scroll(delta: integer);  //desplaza el valor del cursor
    procedure Dibujar;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
    procedure procButDown(estado: Boolean);
    procedure procButUp(estado: Boolean);
    procedure ProcTic(Sender: TObject);
  private
    tipo       : TSBOrientation;
    butUp      : TogButton;
    butDown    : TogButton;
    OnClick    : TEvenBTclk;
    clock      : TTimer;     //temporizador para leer salida del proceso
    ticCont    : integer;    //contador
  end;

  TogButtons = specialize TFPGObjectList<TogButton>;       //Para gestionar los botones
  TogScrollBars = specialize TFPGObjectList<TogScrollBar>; //Para gestionar barras de desplazamiento

  { TObjGrafCtrls }
  {Objeto gráfico que además permite incluir controles}
  TObjGrafCtrls = class(TObjGraf)
  public //Manejo de controles
    Buttons    : TogButtons;    //Lista para contener botones
    ScrollBars : TogScrollBars; //Lista para contener barras de desplazamiento
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
       xp, yp: Integer; solto_objeto: Boolean); override;
    function AddButton(ancho0, alto0: Integer; tipo0: TTipBot;
      EvenBTclk0: TEvenBTclk): TogButton;
    function AddScrollBar(ancho0, alto0: Integer; tipo0: TSBOrientation;
      EvenBTclk0: TEvenBTclk): TogScrollBar;
    procedure Draw; override;
  public
    constructor Create(mGraf: TMotGraf); override;
    destructor Destroy; override;
  end;

implementation
{ TogCheckBox }
procedure TogCheckBox.Dibujar;
//Dibuja el botón de acuerdo a su tipo y estado
begin
  case tipo of
  BOT_CERRAR: begin
//       v2d.DibFonBoton(fx,fy,15,15);
       v2d.DibVnormal(x+2, y+2,10,5);
       v2d.DibVnormal(x+2, y+12,10,-5);
     end;
  BOT_EXPAND:
      if estado then begin
//         v2d.DibFonBoton(fx,fy,15,15);
//         v2d.DibVnormal(fx+2,fy+7,10,-5);
//         v2d.DibVnormal(fx+2,fy+11,10,-5);
         v2d.SetColor(COL_GRIS, COL_GRIS, 1);
         v2d.Polygon(x+3      , y + height-5,
                      x+width-3, y + height-5,
                      x+width/2, y + 4);
      end else begin
//         v2d.DibFonBoton(fx,fy,15,15);
//         v2d.DibVnormal(fx+2,fy+2,10,5);
//         v2d.DibVnormal(fx+2,fy+6,10,5);
        v2d.SetColor(COL_GRIS, COL_GRIS, 1);
        v2d.Polygon(x+3      , y + 5,
                     x+width-3, y + 5,
                     x+width/2, y + height - 4);
      end;
  BOT_CHECK: begin  //botón check
     if estado then begin   //dibuja solo borde
        v2d.DibBorBoton(x, y,15,15);
     end else begin         //dibuja con check
        v2d.DibBorBoton(x, y,15,15);
        v2d.DibCheck(x+2, y+2,10,8);
     end;
    end;
  BOT_REPROD: begin  //botón reproducir
     if estado then begin   //dibuja solo borde
       v2d.SetColor(clBlack, TColor($E5E5E5), 1);
       v2d.RectRedonR(x, y, x+width, y+height);
       v2d.SetColor(clBlack, clBlack, 1);
       v2d.RectangR(x+6, y+6, x+width-6, y+height-6);
     end else begin         //dibuja con check
       v2d.SetColor(clBlack, TColor($E5E5E5), 1);
       v2d.RectRedonR(x, y, x+width, y+height);
       v2d.SetColor(clBlack, clBlack, 1);
       v2d.Polygon(x+ 6, y+3,
                    x+18, y + height/2,
                    x+ 6, y + height - 4);
     end;
    end;
  end;
end;
procedure TogCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState; xp,
  yp: Integer);
begin

end;
{ TogButton }
constructor TogButton.Create(mGraf: TMotGraf; tipo0: TTipBot;
  EvenBTclk0: TEvenBTclk);
begin
   inherited Crear(mGraf, 16, 16);    //crea
   tipo := tipo0;
   OnClick := EvenBTclk0;
   estado := FALSE;   //inicia en 0 (check no marcado, o botón por contraer)
   drawBack := true;
end;
procedure TogButton.Dibujar;
//Dibuja el botón de acuerdo a su tipo y estado
begin
  case tipo of
  BOT_CERRAR: begin
       if drawBack then v2d.DibBorBoton(x, y,width,height);
       v2d.DibVnormal(x+2, y+ 2, 10, 5);
       v2d.DibVnormal(x+2, y+12, 10,-5);
     end;
  BOT_EXPAND:
      if estado then begin
        if drawBack then v2d.DibBorBoton(x, y,width,height);
//         v2d.DibVnormal(fx+2,fy+7,10,-5);
//         v2d.DibVnormal(fx+2,fy+11,10,-5);
         v2d.SetColor(COL_GRIS, COL_GRIS, 1);
         v2d.DrawTrianUp(x+2, y+4, width-4, height-10);
      end else begin
         if drawBack then v2d.DibBorBoton(x, y,width,height);
//         v2d.DibVnormal(fx+2,fy+2,10,5);
//         v2d.DibVnormal(fx+2,fy+6,10,5);
        v2d.SetColor(COL_GRIS, COL_GRIS, 1);
        v2d.DrawTrianDown(x+2, y+5,width-4,height-10);
      end;
  BOT_CHECK: begin  //botón check
     if estado then begin   //dibuja solo borde
        v2d.DibBorBoton(x,y,15,15);
     end else begin         //dibuja con check
        v2d.DibBorBoton(x,y,15,15);
        v2d.DibCheck(x+2,y+2,10,8);
     end;
    end;
  BOT_REPROD: begin  //botón reproducir
     if estado then begin   //dibuja solo borde
       v2d.SetColor(clBlack, TColor($E5E5E5), 1);
       v2d.RectRedonR(x,y,x+width, y+height);
       v2d.SetColor(clBlack, clBlack, 1);
       v2d.RectangR(x+6,y+6,x+width-6, y+height-6);
     end else begin         //dibuja con check
       v2d.SetColor(clBlack, TColor($E5E5E5), 1);
       v2d.RectRedonR(x,y,x+width, y+height);
       v2d.SetColor(clBlack, clBlack, 1);
       v2d.Polygon(x+6, y+3,
                    x+18, y + height/2,
                    x+6, y + height - 4);
     end;
    end;
  end;
end;
procedure TogButton.MouseUp(Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
begin
   if LoSelec(xp,yp) then begin    //se soltó en el botón
      //cambia el estado, si aplica
      if tipo in [BOT_EXPAND, BOT_CHECK, BOT_REPROD] then estado := not estado;
      if Assigned(OnClick) then
         OnClick(estado);    //ejecuta evento
   end;
end;
{ TogScrollBar }
constructor TogScrollBar.Create(mGraf: TMotGraf; tipo0: TSBOrientation;
  EvenBTclk0: TEvenBTclk);
begin
  inherited Crear(mGraf, 16, 50);    //crea
  clock := TTimer.Create(nil);
  clock.interval:=250;  //ciclo de conteo
  clock.OnTimer:=@ProcTic;
  tipo := tipo0;
  OnClick := EvenBTclk0;
  valMin:=0;
  valMax:=255;
  pulsado := false;   //limpia bandera para detectar pulsado contínuo.
  ticCont := 0;       //inicia contador
  case tipo of
  SB_HORIZONT: begin
      width:=80; height:=19;  {se usa 19 de width porque así tendremos a los botones con
                             16, que es el tamño en el que mejor se dibuja}
    end;
  SB_VERTICAL: begin
      width:=19; height:=80;
    end;
  end;
  //crea botones
  butUp := TogButton.Create(mGraf,BOT_EXPAND, @procButUp);
  butUp.drawBack:=false;
  butUp.estado:=true;
  butDown := TogButton.Create(mGraf,BOT_EXPAND, @procButDown);
  butDown.drawBack:=false;
  butDown.estado:=false;
end;
destructor TogScrollBar.Destroy;
begin
  butUp.Destroy;
  butDown.Destroy;
  clock.Free;  //destruye temporizador
  inherited Destroy;
end;
procedure TogScrollBar.Scroll(delta: integer);
//Desplaza el cursor en "delta" unidades.
begin
  valCur += delta;
//   if valCur  > valMax then valCur := valMax;
  if valCur + page - 1 > valMax then valCur := valMax - page + 1;
  if valCur < valMin then valCur := valMin;
  if OnClick<>nil then OnClick(true);
end;
procedure TogScrollBar.procButDown(estado: Boolean);
begin
  Scroll(1);
end;
procedure TogScrollBar.procButUp(estado: Boolean);
begin
  Scroll(-1);
end;
procedure TogScrollBar.ProcTic(Sender: TObject);
begin
  //Se verifica si los botones están pulsados
  inc(ticCont);

end;
procedure TogScrollBar.Dibujar;
const
  ALT_MIN_CUR = 10;
var
  altBot: Single;
  y2: Extended;
  facPag: Single;
  espCur: Single;
  altCur: Single;
  yIni, yFin: Single;
  yDesp: SIngle;
begin
  case tipo of
  SB_HORIZONT: begin
    end;
  SB_VERTICAL: begin
      v2d.SetPen(psSolid, 1, clScrollBar);
      v2d.SetBrush(clMenu);
      v2d.rectangR(x,y,x+width,y+height);  //fondo

      butUp.x:=x+1;
      butUp.width:=width-3;
      butUp.y:=y;

      butDown.x:=x+1;
      butDown.width:=width-3;
      butDown.y:=y+height-butDown.height;

      butUp.Dibujar;
      butDown.Dibujar;
      //dibuja líneas
      altBot := butUp.height;
      y2 := y + height;
      yIni := y+altBot;
      yFin := y2-altBot;
      v2d.SetPen(psSolid, 1, clScrollBar);
      v2d.SetBrush(clScrollBar);
      v2d.Line(x,yIni,x+width,yIni);
      v2d.Line(x,yFin,x+width,yFin);
      //dibuja cursor
      facPag := page/(valMax-valMin+1);  //factor de página
      espCur := yFin-yIni;  //espacio disponible para desplazamiento del cursor
      if espCur > ALT_MIN_CUR then begin
         altCur := facPag * espCur;
         if altCur<ALT_MIN_CUR then altCur:= ALT_MIN_CUR;
         //dibuja cursor
         yDesp := (valCur-valMin)/(valMax-valMin+1)*espCur;
         if espCur<=altCur then exit;   //protección
         yIni := yIni + yDesp*(espCur-altCur)/(espCur-facPag * espCur);
         if yIni+altCur > yFin then exit;   //protección
         v2d.RectangR(x,yIni,x+width,yIni+altCur);
      end;
    end;
  end;
end;
procedure TogScrollBar.MouseUp(Button: TMouseButton; Shift: TShiftState; xp,
  yp: Integer);
begin
  pulsado := false;   //limpia bandera
  ticCont := 0;
  //pasa eventos
//  butUp.MouseUp(Button, Shift, xp, yp);
//  butDown.MouseUp(Button, Shift, xp, yp);
end;
procedure TogScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState; xp,
  yp: Integer);
begin
  pulsado := true;   //marca bandera
  //pasa eventos como si fueran MouseUP, porque las barras de desplazamiento deben
  //responder rápidamente.
  butUp.MouseUp(Button, Shift, xp, yp);
  butUp.estado:=true;     //para que no cambie el ícono
  butDown.MouseUp(Button, Shift, xp, yp);
  butDown.estado:=false;  //para que no cambie el ícono
end;
procedure TObjGrafCtrls.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; xp, yp: Integer; solto_objeto: Boolean);
var
  bot: TogButton;
  sbar : TogScrollBar;
begin
  inherited;
  if Proceso then exit;  //Ya procesaron el evento
  if Button = mbLeft then begin        //soltó izquierdo
     //pasa evento a los controles
     for bot in Buttons do bot.MouseUp(Button, Shift, xp, yp);
     for sbar in ScrollBars do sbar.MouseUp(Button, Shift, xp, yp);
  end;
end;
//Manejo de controles
function TObjGrafCtrls.AddButton(ancho0, alto0: Integer; tipo0: TTipBot;
  EvenBTclk0: TEvenBTclk): TogButton;
//Agrega un botón al objeto.
begin
  Result := TogButton.Create(v2d, tipo0, EvenBTclk0);
  Result.width := ancho0;
  Result.height := alto0;
  Buttons.Add(Result);
end;
function TObjGrafCtrls.AddScrollBar(ancho0, alto0: Integer; tipo0: TSBOrientation;
  EvenBTclk0: TEvenBTclk): TogScrollBar;
//Agrega un botón al objeto.
begin
  Result := TogScrollBar.Create(v2d, tipo0, EvenBTclk0);
  Result.width := ancho0;
  Result.height := alto0;
  ScrollBars.Add(Result);
end;
procedure TObjGrafCtrls.Draw;
var
  bot  : TogButton;
  sbar : TogScrollBar;
begin
  inherited;  //Dibujo normal
  //Dibuja controles
  for bot in Buttons do bot.Dibujar;     //Dibuja Buttons
  for sbar in ScrollBars do sbar.Dibujar;
end;

constructor TObjGrafCtrls.Create(mGraf: TMotGraf);
begin
  inherited Create(mGraf);
  Buttons    := TogButtons.Create(True);     //Crea lista con administración de objetos
  ScrollBars := TogScrollBars.Create(True);
end;

destructor TObjGrafCtrls.Destroy;
begin
  ScrollBars.Free;
  Buttons.Free;
  inherited Destroy;
end;

end.

