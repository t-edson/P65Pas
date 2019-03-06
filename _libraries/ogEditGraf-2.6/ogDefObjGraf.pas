{Unidad ogDefObjGraf
====================
Por Tito Hinostroza 24/09/2014

Descripcion
===========
Define a los objetos gráficos primarios que serán usados por los objetos de mayor nivel
a usar en un editor de objetos gráficos.
El objeto TObjGraf, es el objeto base del que deben derivarse los objetos más específicos
que se dibujarán en pantalla.
Se incluyen también la definición de puntos de control, que permiten redimensionar al
objeto; y de botones que pueden incluirse en los objetos graficos.
En esta unidad solo deben estar definidos los objetos básicos, los que se pueden usar en
muchas aplicaciones. Los más específicos se deben poner en otra unidad.
No se recomienda modificar esta unidad para adecuar los objetos gráficos a la aplicación.
Si se desea manjar otra clase de objetos generales, es mejor crear otra clase general a
partir de TObjGraf.
La jerarquía de clases es:

TObjVisible ----------------------------------------> TObjGraf ---> Derivar objetos aquí
              |                                          |
               --> TPtoCtrl --(Se incluyen en)-----------
              |                                          |
               --> TogButton --(Se pueden incluir en)----
              |                                          |
               --> TogScrollBar -(Se pueden incluir en)--

}
unit ogDefObjGraf;
{$mode objfpc}{$H+}
interface
uses
  Classes, Controls, SysUtils, Fgl, Graphics, GraphType, Types, math, ExtCtrls,
  LCLProc, ogMotGraf2D;

const
  ANCHO_MIN = 0;    //Ancho mínimo de objetos gráficos en pixels (Coord Virtuales)
  ALTO_MIN = 0;     //Alto mínimo de objetos gráficos en Twips (Coord Virtuales)

type
  TBehave = (
    behav1D,  //De una dimensión (línea)
    behav2D   //De dos dimensiones
  );
  { TObjVsible }
  //Clase base para todos los objetos visibles
  TObjVsible = class
  private
    procedure Setx(AValue: Single);
    procedure Sety(AValue: Single);
  protected
    fx,fy     : Single;    //coordenadas virtuales
    v2d       : TMotGraf;  //motor gráfico
    Xant,Yant : Integer;   //coordenadas anteriores
  public  //Contenedor de la forma (Cuadro de selección)
    //Puntos de Inicio y Fin
    {Estos puntos definen la geometría cuando se trata de una forma: behav1D}
    startX, startY, endX, endY: Single;
    //Cuadro de selección
    Width     : Single;    //ancho
    Height    : Single;    //alto
  public
    Id        : Integer;   //Identificador del Objeto. No usado por la clase. Se deja para facilidad de identificación.
    Selected  : Boolean;   //indica si el objeto está seleccionado
    Visible   : boolean;   //indica si el objeto es visible
    procedure Crear(mGraf: TMotGraf; ancho0, alto0: Integer);  //no es constructor
    procedure Locate(x0, y0: Single); virtual; //Fija posición  ¿Realmente es útil?
    function LoSelec(xr, yr: Integer): Boolean;
    function StartMove(xr, yr: Integer): Boolean;
    property x: Single read fx write Setx;
    property y: Single read fy write Sety;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TPosicPCtrol = (   //Tipo de desplazamiento de punto de control
    TD_SIN_POS,  //sin posición. No se reubicará automáticamente
    //Puntos de dimensionamiento en 2D
    TD_SUP_IZQ,  //superior izquierda, desplaza ancho (por izquierda) y alto (por arriba)
    TD_SUP_CEN,  //superior central, desplaza alto por arriba
    TD_SUP_DER,  //superior derecha, desplaza ancho (por derecha) y alto (por arriba)

    TD_CEN_IZQ,  //central izquierda, desplaza ancho (por izquierda)
    TD_CEN_DER,  //central derecha, desplaza ancho (por derecha)

    TD_INF_IZQ,  //inferior izquierda
    TD_INF_CEN,  //inferior central
    TD_INF_DER  //inferior izquierda

    );

  TPtoCtrl = class;
  TPtoConx = class;
  //Eventos para dimensionar forma
  TEvReqDimen2D = procedure(newX, newY, newWidth, newHeight: Single) of object;
  TEvPCReqPosiiton = procedure(target: TPtoCtrl; dx, dy: Single; wishX, wishY: Single) of object;
  TEvPCconnect = procedure(pCtl: TPtoCtrl; pCnx: TPtoConx) of object;

  TPtoCtrlIco = (pciSquare, pciCircle);
  TObjGraf = class;

  { TPtoCtrl }
  {Define al objeto Punto de Control.}
  TPtoCtrl = class(TObjVsible)
  private
    function GetQuadrant: byte;
    procedure SetQuadrant(AValue: byte);
  public
    idIcon     : TPtoCtrlIco;   {Forma gráfica del punto de control.}
    relPosition: TPosicPCtrol;  {Posición del Punto de Control con respecto a su objeto
                                contenedor.}
    mousePtr   : TCursor;       //Tipo de puntero del mouse
    Parent     : TObjGraf;      //Referencia al objeto contenedor
    OnChangePosition: TEvPCReqPosiiton;  //Requiere dimensionamiento en modo 1D
    OnConnect  : TEvPCconnect;  //Se conecta a un punto de conexión
    OnDisconnect:TEvPCconnect; //Se desconecta de un punto de conexión
    ConnectedTo: TPtoConx;     //Punto de conexión al cual se encuentra conectado
    procedure Disconnect;
    property Quadrant: byte read GetQuadrant write SetQuadrant;
    procedure Draw();
    procedure StartMove(xr, yr: Integer; xIni, yIni, widthIni, heighIni: Single);
    procedure MouseMove(xr, yr: Integer);  //Dimensiona las variables indicadas
    function LoSelec(xp, yp: Integer):boolean;
    procedure LocateInParent;
  public //Inicialización
    x0, y0  : Single;        //Posición inicial de la forma al iniciar el control
    width0, height0: Single; //Dimensiones iniciales de la forma al iniciar el control
    constructor Create(Parent0: TObjGraf; PosicPCtrol: TPosicPCtrol;
      mousePtr0: TCursor; ChangePosition: TEvPCReqPosiiton); reintroduce;
  end;
  TPtosControl = specialize TFPGObjectList<TPtoCtrl>;  //Lista para gestionar los puntos de control

  { TPtoConx }
  {Define al objeto Punto de Conexión.}
  TPtoConx = class(TObjVsible)
  public
    xFac, yFac: Single;  //Posición con respecto al objeto contenedor (procentaje de ancho y alto)
    procedure Draw;
    procedure Mark;
    procedure StartMove(xr, yr: Integer; xIni, yIni, widthIni, heightIni: Single);
    procedure Mover(xr, yr: Integer);  //Dimensiona las variables indicadas
    function LoSelec(xp, yp: Integer; accuracy: integer=0): boolean;
    procedure Locate(x0, y0: Single); override;
  private
    pointerTyp : Integer;  //Tipo de puntero
  public
    Marked     : boolean;  //Indica que el punto debe marcarse porqu el ratón pasó por encima
    ptosControl: TPtosControl; //Puntos de control a los que se encuentar enganchado.
    Parent     : TObjGraf;     //Reference to object container
    data       : TObject;      //Unused field. Can be used for teh user.
    procedure ConnectTo(pCtl: TPtoCtrl);
    procedure DisconnectFrom(pCtl: TPtoCtrl);
    procedure Disconnect;
  public //Inicialización
    x0, y0, width0, height0: Single;  //valores objetivo para las dimensiones
    constructor Create(mGraf: TMotGraf);
    destructor Destroy; override;
  end;
  TPtosConex = specialize TFPGObjectList<TPtoConx>;  //Lista para gestionar los puntos de control

  TEventSelec = procedure(obj: TObjGraf) of object; //Procedimiento-evento para seleccionar
  TEventPtrChange = procedure(TipPunt: Integer) of object; //Procedimiento-evento para cambiar puntero

  { TObjGraf }
  {Este es el Objeto padre de todos los objetos gráficos visibles que son administrados por
   el motor de edición.}
  TObjGraf = class(TObjVsible)
  protected
    function GetXCent: Single;  //Coordenada X central del objeto.
    procedure SetXcent(AValue: Single);
    function GetYCent: Single;  //Coordenada Ycentral del objeto
    procedure SetYCent(AValue: Single);
  private
    procedure PtoCtl_ChangePosition(target: TPtoCtrl; dx, dy: Single; wishX,
      wishY: Single);
  public
    behav : TBehave;  //Indica si la forma es de 1D o 2D.
    Name        : String;    //Identificación del objeto
    Marked      : Boolean;   //Indica que está marcado, porque el ratón pasa por encima
    DibSimplif  : Boolean;   //Indica que se está en modo de dibujo simplificado
    Highlight   : Boolean;   //Indica si permite el resaltado del objeto
    SizeLocked  : boolean;   //Protege al objeto de redimensionado
    PosLocked   : Boolean;   //Indica si el objeto está bloqueado para movimiento
    SelLocked   : Boolean;   //Indica si el objeto está bloqueado para selección
    FillColor   : TColor;    //Color de relleno
    Proceso     : Boolean;   //Bandera
    Resizing    : boolean;   //Indica que el objeto está dimensionándose
    Erased      : boolean;   //Bandera para eliminar al objeto
    property Xcent: Single read GetXCent write SetXcent;
    property YCent: Single read GetYCent write SetYCent;
    procedure Selec;         //Método único para seleccionar al objeto
    procedure Deselec;       //Método único para quitar la selección del objeto
    procedure Delete;        //Método para eliminar el objeto
    function IsSelectedBy(xr, yr:integer): Boolean; virtual;
    procedure Draw; virtual;  //Dibuja el objeto gráfico
    procedure StartMove(xr, yr : Integer);
    procedure MouseMove(xr, yr : Integer; nobjetos : Integer); virtual;
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
       xp, yp: Integer); virtual;  //Metodo que funciona como evento mouse_down
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
       xp, yp: Integer; solto_objeto: Boolean); virtual;
    procedure MouseOver(Sender: TObject; Shift: TShiftState; xp, yp: Integer); virtual;
    procedure MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
                 MousePos: TPoint; var Handled: Boolean); virtual;
  public
    Tipo        : Integer;   //Tipo de objeto. No usado por la librería. Queda para el usuario.
    Data        : string;    //Dato adicional. No usado por la librería. Queda para el usuario.
    Obj         : pointer;   //Dato adicional. No usado por la librería. Queda para el usuario.
  public //Posición y Tamaño
    procedure ReLocate(newX, newY: Single; UpdatePCtrls: boolean=true); virtual;
    procedure ReSize(newWidth, newHeight: Single; UpdatePCtrls: boolean=true);
      virtual; //Reconstruye la geometría del objeto
    procedure ReLocateSize(newX, newY, newWidth, newHeight: Single;
      UpdatePCtrls: boolean=true);
  public //Eventos de la clase
    OnRelocate: procedure of object;
    OnResize  : procedure of object;
    OnSelec   : TEventSelec;
    OnDeselec : TEventSelec;
    OnCamPunt : TEventPtrChange;
  public //Puntos de Control
    curPntCtl   : TPtoCtrl;  //Punto de Control actual
  public
    {Los puntos de control son los que se pueden mover independientemente y tienen
    efecto sobre la posición y/o el tamaño de la forma.}
    //Puntos de control por defecto
    pcTOP_LEF: TPtoCtrl;
    pcTOP_CEN: TPtoCtrl;
    pcTOP_RIG: TPtoCtrl;
    pcCEN_LEF: TPtoCtrl;
    pcCEN_RIG: TPtoCtrl;
    pcBOT_LEF: TPtoCtrl;
    pcBOT_CEN: TPtoCtrl;
    pcBOT_RIG: TPtoCtrl;
    pcBEGIN  : TPtoCtrl;
    pcEND    : TPtoCtrl;
    PtosControl1: TPtosControl;  //Lista de puntos de control en modo 1D
    PtosControl2: TPtosControl;  //Lista de puntos de control en modo 2D
    function SelecPtoControl(xp, yp: integer): TPtoCtrl;
    function AddPtoControl1D(PosicPCtrol: TPosicPCtrol; mousePtr: TCursor): TPtoCtrl;
    function AddPtoControl2D(PosicPCtrol: TPosicPCtrol; mousePtr: TCursor): TPtoCtrl;
  public //Puntos de conexión
    ShowPtosConex: boolean;   //Indica si se mostrarán los puntos de conexión
    PtosConex  : TPtosConex;  //Lista de puntos de conexión
    function AddPtoConex(xOff, yOff: Single): TPtoConx; virtual;
    function SelectConnectionPoint(xp, yp: integer; accuracy: integer=0): TPtoConx;
    function MarkConnectionPoint(xp, yp: integer; accuracy: integer = 0): TPtoConx;
    procedure ClearMarkConnectionPoints;
    function ConnectionPointMarked: TPtoConx;
  public //Inicialización
    constructor Create(mGraf: TMotGraf); virtual;
    destructor Destroy; override;
  end;

  function PointSelectSegment(xp, yp, x0, y0, x1, y1: integer): Boolean;

implementation
const
  ANC_PCT2 = 5;       //mitad del ancho de punto de control
  ANC_PCN2 = 4;

function PointSelectSegment(xp, yp, x0, y0, x1, y1: integer): Boolean;
{Indica si el punto (xp, yp) selecciona un segmento de recta con una tolerancia
de 5 pixeles.
El segmento se define con los puntos (x0, y0) y (x1, y1).
Las coordenadas son de pantalla.}
const
  DSEL = 5;
var
  dx, dy: Int16;
   //tolerancia en pixeles
begin
  {No debería ser necesario actualizar las coordenadas de pantalla de P0 y P1, ya que
  si esta recta se mostró en pantalla, es porque se actualizaron sus coordenadas de
  pantalla:
  v2d.XYpant(P0);
  v2d.XYpant(P1);
  }
  if x0 = x1 then begin  //Caso recta vertical
     if abs(x0 - xp)>DSEL then exit(false);  //excede distancia horizontal
     if y0 = y1 then begin  //Caso de un punto
       Result := (abs(y0 - yp) < DSEL);
     end else begin //Caso de recta vertical común
       if y0 > y1 then begin  //P0 arriba
          Result := (yp<y0+DSEL) and (yp>y1-DSEL);
       end else begin               //P1 arriba
          Result := (yp<y1+DSEL) and (yp>y0-DSEL);
       end;
     end;
  end else if x0 < x1 then begin  //P0 a la izquierda
     if xp<x0-DSEL then exit(false);  //escapa de límite
     if xp>x1+DSEL then exit(false);  //escapa de límite
     //Simplifica la comparación, viendo solo una distancia vertical
//     a := (y1 - y0)/(x1 - x0);  //pendiente
//     b := y0 - a*x0;  //Define ecuación de la recta y=ax+b
//     Result := abs(a*xp + b - yp) < DSEL;
     //Forma alternativa, sin divisiones
     dx := x1 - x0;   //siempre positivo
     dy := y1 - y0;   //positivo o negativo
     if abs(dy)<dx then begin
       Result := abs( (xp - x0)*dy - (yp-y0)*dx ) < DSEL * dx;
     end else begin //abs(dy), es mayor a dx
       Result := abs( (xp - x0)*dy - (yp-y0)*dx ) < DSEL * abs(dy);
     end;
  end else begin                        //P1 a la izquierda
     if xp<x1-DSEL then exit(false);  //escapa de límite
     if xp>x0+DSEL then exit(false);  //escapa de límite
     //Define ecuación de la recta y=ax+b
//     a := (y0 - y1)/(x0 - x1);  //pendiente
//     b := y1 - a*x1;
//     Result := abs(a*xp + b - yp) < DSEL;
      dx := x0 - x1;   //siempre positivo
      dy := y0 - y1;   //positivo o negativo
      if abs(dy)<dx then begin
        Result := abs( (xp - x1)*dy - (yp-y1)*dx ) < DSEL * dx;
      end else begin //abs(dy), es mayor a dx
        Result := abs( (xp - x1)*dy - (yp-y1)*dx ) < DSEL * abs(dy);
      end;
  end;
end;

{ TObjVsible }
procedure TObjVsible.Crear(mGraf: TMotGraf; ancho0, alto0: Integer);
begin
  v2d := mGraf;
  width:=ancho0;
  height :=alto0;
  visible := true;
end;
procedure TObjVsible.Setx(AValue: Single);
begin
  if fx=AValue then Exit;
  fx:=AValue;
end;
procedure TObjVsible.Sety(AValue: Single);
begin
  if fy=AValue then Exit;
  fy:=AValue;
end;
procedure TObjVsible.Locate(x0, y0: Single);
begin
  fx := x0;
  fy := y0;
end;
function TObjVsible.LoSelec(xr, yr: Integer): Boolean;
//Indica si las coordenadas de ratón seleccionan al botón en su posición actual
var xv, yv: Single;    //coordenadas virtuales
begin
    v2d.XYvirt(xr, yr, xv, yv);
    Result := False;    //valor por defecto
    If (xv > fx - 2) And (xv < fx + width + 2) And
       (yv > fy - 2) And (yv < fy + height + 2) Then
        Result := True;
end;
function TObjVsible.StartMove(xr, yr: Integer): Boolean;
begin
  Result := false;  //por el momento, no devuelve valor
  if not visible then exit;    //validación
  //captura posición actual, para calcular los desplazamientos
  Xant := xr;
  Yant := yr;
end;
constructor TObjVsible.Create;
begin
  inherited Create;
end;
destructor TObjVsible.Destroy;
begin
  inherited Destroy;
end;
// TPtoCtrl
function TPtoCtrl.GetQuadrant: byte;
{Devuelve el cuadrante, en el sentido geométrico, de la posición del Putno de Control con respecto al centro de
su objeto contenedor :
             |
       2     |     1
             |
   <-------------------->
             |
       3     |     4
             |
   }
begin
  case relPosition of
  TD_SUP_IZQ: exit(2);
  TD_SUP_CEN: exit(1);
  TD_SUP_DER: exit(1);

  TD_CEN_IZQ: exit(2);
  TD_CEN_DER: exit(4);

  TD_INF_IZQ: exit(3);
  TD_INF_CEN: exit(3);
  TD_INF_DER: exit(4);
  else
    exit(1);
  end;
end;
procedure TPtoCtrl.SetQuadrant(AValue: byte);
begin
  case AValue of
  1: relPosition := TD_SUP_DER;
  2: relPosition := TD_SUP_IZQ;
  3: relPosition := TD_INF_IZQ;
  4: relPosition := TD_INF_DER;
  end
end;
procedure TPtoCtrl.Disconnect;
{Desconecta la el punto de control al punto de conexión que pudiera estar ligado.}
begin
  if ConnectedTo<>nil then begin
     ConnectedTo.DisconnectFrom(self);
     ConnectedTo := nil;
  end;
end;
procedure TPtoCtrl.Draw();
//Dibuja el Punto de control en la posición definida
var xp, yp: Integer;
begin
   if not visible then exit;    //validación
   if  self.idIcon = pciCircle  then begin
      v2d.XYpant(fx, fy, xp, yp);      //obtiene coordenadas de pantalla
      v2d.SetBrush(clNavy);
      v2d.Canvas.Ellipse(xp - ANC_PCT2, yp - ANC_PCT2,
                         xp + ANC_PCT2, yp + ANC_PCT2);
   end else begin
     v2d.XYpant(fx, fy, xp, yp);      //obtiene coordenadas de pantalla
     v2d.Barra0(xp - ANC_PCT2, yp - ANC_PCT2,
                xp + ANC_PCT2, yp + ANC_PCT2, clNavy);  //siempre de tamaño fijo
   end;
end;
procedure TPtoCtrl.StartMove(xr, yr: Integer; xIni, yIni, widthIni, heighIni: Single);
//Procedimiento para procesar el evento StartMove del punto de control
begin
   if not visible then exit;    //validación
   inherited StartMove(xr,yr);
   {Captura los valores iniciales de la geometría, apra poder operar sobre esas
   dimensiones cuando se intente hacer los dimensionamientos.}
   x0 := xIni;
   y0 := yIni;
   width0 := widthIni;
   height0 := heighIni;
   Disconnect;
end;
procedure TPtoCtrl.MouseMove(xr, yr: Integer);
//Realiza el cambio de las variables indicadas de acuerdo al tipo de control y a
//las variaciones indicadas (dx, dy)
var
  dx, dy, wishX, wishY: Single;
begin
  if not visible then exit;    //validación
  dx := (xr - Xant) / v2d.Zoom;     //obtiene desplazamiento absoluto
  dy := (yr - Yant) / v2d.Zoom;     //obtiene desplazamiento absoluto
  {Comunica que se requiere cambiar la posición del punto de control, para que el
  objeto gráfico padre tome la decisión sobre el cambio}
  v2d.XYvirt(xr, yr, wishX, wishY);
  OnChangePosition(Self, dx, dy, wishX, wishY);
end;
function TPtoCtrl.LoSelec(xp, yp: Integer): boolean;
//Indica si las coordenadas lo selecciona
var xp0, yp0 : Integer; //corodenadas virtuales
begin
   LoSelec := False;
   if not visible then exit;    //validación
   v2d.XYpant(fx, fy, xp0, yp0);   //obtiene sus coordenadas en pantalla
   //compara en coordenadas de pantalla
   If (xp >= xp0 - ANC_PCT2) And (xp <= xp0 + ANC_PCT2) And
      (yp >= yp0 - ANC_PCT2) And (yp <= yp0 + ANC_PCT2) Then
        LoSelec := True;
End;
procedure TPtoCtrl.LocateInParent;
{Ubica al Punto de control en su posición respectiva con respecto al objeto padre.}
begin
 case relPosition of
 TD_SUP_IZQ:  //superior izquierda, desplaza ancho (por izquierda) y height (por arriba)
   Locate(Parent.x, Parent.y);
 TD_SUP_CEN:  //superior central, desplaza Parent.height por arriba
   Locate(Parent.x+Parent.width/2,Parent.y);
 TD_SUP_DER:  //superior derecha, desplaza ancho (por derecha) y Parent.height (por arriba)
   Locate(Parent.x+Parent.width,Parent.y);

 TD_CEN_IZQ:  //central izquierda, desplaza ancho (por izquierda)
   Locate(Parent.x,Parent.y+Parent.height/2);
 TD_CEN_DER:  //central derecha, desplaza ancho (por derecha)
   Locate(Parent.x+Parent.width,Parent.y+Parent.height/2);

 TD_INF_IZQ:  //inferior izquierda
   Locate(Parent.x,Parent.y+Parent.height);
 TD_INF_CEN:  //inferior central
   Locate(Parent.x+Parent.width/2,Parent.y+Parent.height);
 TD_INF_DER:   //inferior izquierda
   Locate(Parent.x+Parent.width,Parent.y+Parent.height);
 else
   //otra ubicación no lo reubica
 end;
end;
constructor TPtoCtrl.Create(Parent0: TObjGraf; PosicPCtrol: TPosicPCtrol;
  mousePtr0: TCursor; ChangePosition: TEvPCReqPosiiton);
begin
  inherited Crear(Parent0.v2d, 2*ANC_PCT2, 2*ANC_PCT2);    //crea
  Parent    := Parent0;
  relPosition := PosicPCtrol;  //Dónde aparecerá en el objeto
  mousePtr  := mousePtr0;    //El puntero del mouse
  OnChangePosition := ChangePosition;
  visible   := true;             //lo hace visible
  fx :=0;
  fy :=0;
end;
//////////////////////////////  TPtoConx //////////////////////////////
procedure TPtoConx.Draw;
//Draw the Connection point.
var xp, yp: Integer;
begin
  if not visible then exit;    //validación
  v2d.XYpant(fx, fy, xp, yp);      //obtiene coordenadas de pantalla
  v2d.SetLine(clBlue);
  v2d.Line0(xp - ANC_PCN2+1, yp - ANC_PCN2+1, xp + ANC_PCN2, yp + ANC_PCN2);
  v2d.Line0(xp - ANC_PCN2+1, yp + ANC_PCN2-1, xp + ANC_PCN2, yp - ANC_PCN2);
end;
procedure TPtoConx.Mark;
{Draw a Connection point highlighted.}
var xp, yp: Integer;
begin
  if not visible then exit;    //validación
  v2d.XYpant(fx, fy, xp, yp);      //obtiene coordenadas de pantalla
  v2d.SetLine(clBlue, 2);
  v2d.rectang(xp - ANC_PCN2-1, yp - ANC_PCN2-1, xp + ANC_PCN2+2, yp + ANC_PCN2+2);
end;
procedure TPtoConx.StartMove(xr, yr: Integer; xIni, yIni, widthIni, heightIni: Single);
//Procedimiento para procesar el evento StartMove del punto de control
begin
  if not visible then exit;    //validación
  inherited StartMove(xr,yr);
  //captura los valores iniciales de las dimensiones
  x0 := xIni;
  y0 := yIni;
  width0 := widthIni;
  height0 := heightIni;
end;
procedure TPtoConx.Mover(xr, yr: Integer);
//Realiza el cambio de las variables indicadas de acuerdo al tipo de control y a
//las variaciones indicadas (dx, dy)
begin
  if not visible then exit;    //validación
//  dx := (xr - Xant) / v2d.Zoom;     //obtiene desplazamiento absoluto
//  dy := (yr - Yant) / v2d.Zoom;     //obtiene desplazamiento absoluto
//  Xant := xr; Yant := yr;   //actualiza coordenadas
end;
function TPtoConx.LoSelec(xp, yp: Integer; accuracy: integer = 0): boolean;
//Indica si las coordenadas lo selecciona
var xp0, yp0 : Integer; //corodenadas virtuales
begin
  LoSelec := False;
  if not visible then exit;    //validación
  v2d.XYpant(fx, fy, xp0, yp0);   //obtiene sus coordenadas en pantalla
  //compara en coordenadas de pantalla
  if (xp >= xp0 - ANC_PCN2-accuracy) and (xp <= xp0 + ANC_PCN2+accuracy) and
     (yp >= yp0 - ANC_PCN2-accuracy) and (yp <= yp0 + ANC_PCN2+accuracy) then
       LoSelec := True;
end;
procedure TPtoConx.Locate(x0, y0: Single);
var
  pctl: TPtoCtrl;
begin
  inherited Locate(x0, y0);
  //Mueve puntos de control enganchados
  for pctl in ptosControl do begin
     {Se llama al evento simulando un movimiento por Ratón. Esto solo funcioanará en
     Puntos de Control 1D.
     Se pudo haber hecho solo: pctl.x := x; pctl.y := y;
     Pero esto no actualizaría la geometría de la forma}
     pctl.OnChangePosition(pctl, 0, 0, x, y);
  end;
end;
procedure TPtoConx.ConnectTo(pCtl: TPtoCtrl);
{Conecta a un punto de control.}
begin
  ptosControl.Add(pCtl);
  pCtl.ConnectedTo := self;
  if pCtl.OnConnect<>nil then pCtl.OnConnect(pCtl, self);
end;
procedure TPtoConx.DisconnectFrom(pCtl: TPtoCtrl);
{Se desconecta de un punto de control.}
begin
  pCtl.ConnectedTo := nil;
  ptosControl.Remove(pCtl);
  if pCtl.OnDisconnect<>nil then pCtl.OnDisconnect(pCtl, self);
end;
procedure TPtoConx.Disconnect;
{Se desconecta de todos los puntos de control a los que se enuentra conectado.}
var
  pCtl: TPtoCtrl;
begin
  //Usa while porque va a eliminar elementor
  while ptosControl.Count>0 do begin
    pCtl := ptosControl[0];
    DisconnectFrom(pCtl)
  end;
end;
constructor TPtoConx.Create(mGraf: TMotGraf);
begin
  inherited Crear(mGraf, 2*ANC_PCT2, 2*ANC_PCT2);    //crea
  visible := true;             //lo hace visible
  fx :=0;
  fy :=0;
  pointerTyp := crSizeNW;  //No se usa
  {Crea lista para los puntos de control 1D que engancha. Pero solo gaurdará referencias
   no eliminará los objetos.}
  ptosControl:= TPtosControl.Create(false);
end;
destructor TPtoConx.Destroy;
var
  pctl: TPtoCtrl;
begin
  //Se desconecta de todos los puntos de control que pudieran estar conectados a
  //este punto de conexión.
  for pctl in ptosControl do begin
    pCtl.ConnectedTo := nil;
    //Hacer DisconnectFrom(pctl) no es necesario y generará error por la forma como se explora a la lista
  end;
  ptosControl.Destroy;
  inherited Destroy;
end;

{ TObjGraf }
function TObjGraf.GetXCent: Single;
begin
   Result := fx + width / 2;
end;
procedure TObjGraf.SetXcent(AValue: Single);
begin
  Locate(AValue-width/2, y);
end;
function TObjGraf.GetYCent: Single;
begin
   Result := fy + height / 2;
end;
procedure TObjGraf.SetYCent(AValue: Single);
begin
  Locate(x, AValue-height/2);
end;
procedure TObjGraf.Selec;
begin
   if Selected then exit;    //ya está Selected
   Selected := true; //se marca como Selected
   //Llama al evento que selecciona el objeto. El editor debe responder
   if Assigned(OnSelec) then OnSelec(self);   //llama al evento
   { TODO : Aquí se debe activar los controles para dimensionar el objeto }
end;
procedure TObjGraf.Deselec;
begin
   if not Selected then exit;    //ya está Selected
   Selected := false; //se marca como selccionado
   //Llama al evento que selecciona el objeto. El editor debe responder
   if Assigned(OnDeselec) then OnDeselec(self);  //llama al evento
   { TODO : Aquí se debe desactivar los controles para dimensionar el objeto }
end;
procedure TObjGraf.Delete;
begin
  //Marca para eliminarse
  Erased := true;
end;
procedure TObjGraf.StartMove(xr, yr: Integer);
//Procedimiento para procesar el evento StartMove de los objetos gráficos
//Se ejecuta al inicio de movimiento al objeto
begin
  Xant := xr; Yant := yr;
  Proceso := False;
  if not Selected then exit;   //para evitar que responda antes de seleccionarse
  //Busca si algún punto de control lo procesa
  curPntCtl := SelecPtoControl(xr,yr);
  if curPntCtl <> NIL  then begin
      curPntCtl.StartMove(xr, yr, fx, fy, width, height);     //prepara para movimiento fy dimensionamiento
      Proceso := True;      //Marcar para indicar al editor fy a Mover() que este objeto procesará
                            //el evento fy no se lo pasé a los demás que pueden estar seleccionados.
      Resizing := True; //Marca bandera
   end else begin
     //No se mueve ningún punto de control
     //Desconecta Puntos de Control 1D, por si estaban ligados.
     pcBEGIN.Disconnect;
     pcEND.Disconnect;
   end;
  { TODO : Verificar por qué, a veces se puede iniciar el movimiento del objeto cuando el puntero está en modo de dimensionamiento. }
end;
procedure TObjGraf.MouseMove(xr, yr: Integer; nobjetos: Integer);
{Metodo que funciona como evento MouseMove al objeto.
"nobjetos" es la cantidad de objetos que se mueven. Ususalmente es sólo uno}
var dx , dy: Single;
begin
//     If ArrastBoton Then Exit;       //Arrastrando botón  { TODO : Revisar }
//     If ArrastFila Then Exit;        //Arrastrando botón  { TODO : Revisar }
     If Selected Then begin
        v2d.ObtenerDesplaz2( xr, yr, Xant, Yant, dx, dy);
        if Proceso then begin
            //Algún elemento del objeto ha procesado el evento de movimiento
            if curPntCtl <> nil then begin
               //Hay un punto de control procesando el evento MouseMove
               if not SizeLocked then
                 curPntCtl.MouseMove(xr, yr);   //permite dimensionar el objeto
            end;
        end else begin //ningún elemento del objeto lo ha procesado, pasamos a mover todo el objeto
            ReLocate(fx + dx, fy + dy);  //reubica los elementos
            Proceso := False;
        end;
        Xant := xr; Yant := yr;
     End;
end;
function TObjGraf.IsSelectedBy(xr, yr:integer): Boolean;
//Devuelve verdad si la coordenada de pantalla xr,yr cae en un punto tal
//que "lograria" la seleccion de la forma.
var xv , yv : Single; //corodenadas virtuales
begin
    v2d.XYvirt(xr, yr, xv, yv);
    IsSelectedBy := False; //valor por defecto
    //verifica área de selección
    if (xv > fx - 1) And (xv < fx + width + 1) And (yv > fy - 1) And (yv < fy + height + 1) then begin
      IsSelectedBy := True;
    end;
    if Selected then begin   //Ub objeto seleccionado, tiene un área mayor de selección
      if SelecPtoControl(xr,yr) <> NIL then IsSelectedBy := True;
    end;
End;
procedure TObjGraf.Draw;
const tm = 3;
var
  pct  : TPtoCtrl;
  pcn : TPtoConx;
begin
  //---------------Draw mark --------------
  if Marked and Highlight then begin
    v2d.SetPen(psSolid, 2, clBlue);   //RGB(128, 128, 255)
    v2d.rectang(fx - tm, fy - tm, fx + width + tm, fy + height + tm);
  end;
  //--------------- Draw selection state--------------
  if Selected Then begin
    if behav = behav1D then begin
       for pct in PtosControl1 do pct.Draw;   //Dibuja puntos de control
    end else if behav = behav2D then begin
       for pct in PtosControl2 do pct.Draw;   //Dibuja puntos de control
    end;
  end;
  //Draw Connection Points
  if ShowPtosConex then begin
     for pcn in PtosConex do pcn.Draw;
  end;
  //if MarkConnectPoints then begin
    for pcn in PtosConex do if pcn.Marked then pcn.Mark;
  //end
end;
procedure TObjGraf.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
//Metodo que funciona como evento "MouseDown"
begin
//  CapturoEvento := NIL;
  Proceso := False;
  If IsSelectedBy(xp, yp) Then begin  //sólo responde instantáneamente al caso de selección
    If Not Selected Then Selec;
    Proceso := True;{ TODO : Verificar si es útil la bandera "Proceso" }
  End;
End;
procedure TObjGraf.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; xp, yp: Integer; solto_objeto: Boolean);
//Metodo que funciona como evento MouseUp
//la bandera "solto_objeto" indica que se ha soltado el objeto despues de estarlo arrastrando
begin
    Proceso := False;
    //verifica si cae de un arrastre
    If solto_objeto And Selected Then begin
        Proceso := True; Exit;    //no quita la selección
    end;
    //Se soltó el ratón
    If Button = mbLeft Then  begin          //soltó izquierdo
    end else If Button = mbRight Then begin //soltó derecho
        If IsSelectedBy(xp, yp) Then
            Proceso := True;
    end;
    //Restaura puntero si estaba dimensionándose por si acaso
    if Resizing then begin
       if not curPntCtl.LoSelec(xp,yp) then //se salio del foco
          if Assigned(OnCamPunt) then OnCamPunt(crDefault);  //pide retomar el puntero
       Resizing := False;    //quita bandera, por si estaba Resizing
       exit;
    end;
end;
procedure TObjGraf.MouseOver(Sender: TObject; Shift: TShiftState; xp, yp: Integer);
//Respuesta al evento MouseMove. Se debe recibir cuando el Mouse pasa por encima del objeto
var pc: TPtoCtrl;
begin
    if not Selected then Exit;
    //Aquí se supone que tomamos el control porque está Selected
    //Procesa el cambio de puntero.
    if Assigned(OnCamPunt) then begin
        pc := SelecPtoControl(xp,yp);
        if pc<> NIL then
           OnCamPunt(pc.mousePtr)  //cambia a supuntero
        else
           OnCamPunt(crDefault);
    end;
end;
procedure TObjGraf.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin

end;
//Posición y Tamaño
procedure TObjGraf.ReLocate(newX, newY: Single; UpdatePCtrls: boolean = true);
{Se usa para cambiar SOLAMENTE la ubicación del objeto}
var
  pCnx: TPtoConx;
begin
  fx := newX;
  fy := newY;
  //Reubica todos los puntos de control
  if UpdatePCtrls then begin
    pcTOP_LEF.LocateInParent;
    pcTOP_CEN.LocateInParent;
    pcTOP_RIG.LocateInParent;
    pcCEN_LEF.LocateInParent;
    pcCEN_RIG.LocateInParent;
    pcBOT_LEF.LocateInParent;
    pcBOT_CEN.LocateInParent;
    pcBOT_RIG.LocateInParent;
    pcBEGIN.LocateInParent;
    pcEND.LocateInParent;
  end;
  //Reubica todos los puntos de conexión
  for pCnx in PtosConex do begin
    pCnx.Locate(x + width * pCnx.xFac,
                y + height * pCnx.yFac);
  end;
  if OnRelocate<>nil then OnRelocate;
end;
procedure TObjGraf.ReSize(newWidth, newHeight: Single; UpdatePCtrls: boolean = true);
{Se usa para cambiar SOLAMENTE el tamaño del objeto}
var
  pCnx: TPtoConx;
begin
  //Protección
  if newWidth < ANCHO_MIN then begin
     newWidth := ANCHO_MIN;
  end;
  if newHeight < ALTO_MIN then begin
     newHeight := ALTO_MIN;
  end;
  //Actualiza por casos
  if (newWidth<>Width) and (newHeight<>Height) then begin
    //Cambian ancho y alto
    Width := newWidth;
    Height := newHeight;
  end else if newWidth<>Width then begin
    //Solo cambia el ancho
    Width := newWidth;
  end else if newHeight<>Height then begin
    //Solo cambia el alto
    Height := newHeight;
  end else begin
    //No cambia nada
    exit;
  end;
  //Reubica todos los puntos de control
  if UpdatePCtrls then begin
  //pcTOP_LEF.LocateInParent;  //Este es el único punto que no cambia su posición al redimensionar la forma
    pcTOP_CEN.LocateInParent;
    pcTOP_RIG.LocateInParent;
    pcCEN_LEF.LocateInParent;
    pcCEN_RIG.LocateInParent;
    pcBOT_LEF.LocateInParent;
    pcBOT_CEN.LocateInParent;
    pcBOT_RIG.LocateInParent;
    pcBEGIN.LocateInParent;
    pcEND.LocateInParent;
  end;
  //Posiciona proporcionalmente a los puntos de conexión
  //debugln('fdx=%f fdy=%f', [fdx, fdy]);
  for pCnx in PtosConex do begin
    pCnx.Locate(x + width * pCnx.xFac,
                y + height * pCnx.yFac);
  end;
  if OnResize<>nil then OnResize;
end;
procedure TObjGraf.ReLocateSize(newX, newY, newWidth, newHeight: Single;
                                UpdatePCtrls: boolean = true);
//Se usa para atender los requerimientos de los puntos de control cuando quieren
//cambiar el tamaño y/o la posición del objeto.
var
  changeLocation, changeSize: Boolean;
begin
  //Protección
  if newWidth < ANCHO_MIN then begin
     newWidth := ANCHO_MIN;
     newX := fx;  //Mantiene X, por si acaso
  end;
  if newHeight < ALTO_MIN then begin
     newHeight := ALTO_MIN;
     newY := fy;  //Mantiene Y, por si acaso
  end;
  changeLocation := (newX<>fx) or (newY<>fy);
  changeSize := (newWidth<>width) or (newHeight<>Height);

  if changeLocation then begin
     ReLocate(newX, newY, UpdatePCtrls);       //Reubica
  end;
  if changeSize then begin
     ReSize(newWidth, newHeight, UpdatePCtrls);       //Reubica
  end;
end;
procedure TObjGraf.PtoCtl_ChangePosition(target: TPtoCtrl; dx, dy: Single; wishX, wishY: Single
  );
{Un punto de control está solicitando reposicionamiento, lo que se suponse afecta
a la posición o el dimensionameinto de la forma.}
  procedure ReadQuadrant;
  begin
    if          (pcBEGIN.x < pcEND.x) and (pcBEGIN.y < pcEND.y) then begin
      pcBEGIN.Quadrant := 2;
      pcEND.Quadrant := 4;
    end else if (pcBEGIN.x > pcEND.x) and (pcBEGIN.y < pcEND.y) then begin
      pcBEGIN.Quadrant := 1;
      pcEND.Quadrant := 3;
    end else if (pcBEGIN.x < pcEND.x) and (pcBEGIN.y > pcEND.y) then begin
      pcBEGIN.Quadrant := 3;
      pcEND.Quadrant := 1;
    end else if (pcBEGIN.x > pcEND.x) and (pcBEGIN.y > pcEND.y) then begin
      pcBEGIN.Quadrant := 4;
      pcEND.Quadrant := 2;
    end;
  end;
var
  newX, newY, newWidth, newHeight: Single;
begin
  case behav of
  behav1D: begin
    //Desplazamiento en una dimensión
    //Ubica el cuadrante del punto de control
    if target = pcBEGIN then begin
      //Se mueve el punto de inicio
      pcBEGIN.Locate(wishX, wishY);  //Mueve el punto de control
    end else if target = pcEND then begin
      //Se mueve el punto final
      pcEND.Locate(wishX, wishY);  //Mueve el punto de control
    end;
    ReadQuadrant;  //Reubica cuandrantes de pcBEGIN y pcEND
    newX := min(pcBEGIN.x, pcEND.x);
    newY := min(pcBEGIN.y, pcEND.y);
    newWidth := abs(pcBEGIN.x - pcEND.x);
    newHeight := abs(pcBEGIN.y - pcEND.y);
    ReLocateSize(newX, newY, newWidth, newHeight, false);
  end;
  behav2D: begin
    //Desplazamiento en dos dimensiones
    {Cambia posición y tamaño, de acuerdo al tipo de desplazamiento (deducido de la
     posición) del punto de control.}
    case target.relPosition of
    TD_SUP_IZQ: ReLocateSize(target.x0+dx, target.y0+dy, target.width0-dx, target.height0-dy);
    TD_SUP_CEN: ReLocateSize(target.x0   , target.y0+dy, target.width0   , target.height0-dy);
    TD_SUP_DER: ReLocateSize(target.x0   , target.y0+dy, target.width0+dx, target.height0-dy);

    TD_CEN_IZQ: ReLocateSize(target.x0+dx, target.y0   , target.width0-dx, target.height0);
    TD_CEN_DER: ReLocateSize(target.x0   , target.y0   , target.width0+dx, target.height0);

    TD_INF_IZQ: ReLocateSize(target.x0+dx, target.y0   , target.width0-dx, target.height0+dy);
    TD_INF_CEN: ReLocateSize(target.x0   , target.y0   , target.width0   , target.height0+dy);
    TD_INF_DER: ReLocateSize(target.x0   , target.y0   , target.width0+dx, target.height0+dy);
    end;
  end;
  end;
end;
function TObjGraf.AddPtoControl1D(PosicPCtrol: TPosicPCtrol; mousePtr: TCursor): TPtoCtrl;
//Agrega un punto de control, que trabajará en formas 1D
begin
  Result := TPtoCtrl.Create(self, PosicPCtrol, mousePtr, @PtoCtl_ChangePosition);
  PtosControl1.Add(Result);
end;
function TObjGraf.AddPtoControl2D(PosicPCtrol: TPosicPCtrol; mousePtr: TCursor): TPtoCtrl;
//Agrega un punto de control, que trabajará en formas 2D
begin
  Result := TPtoCtrl.Create(self, PosicPCtrol, mousePtr, @PtoCtl_ChangePosition);
  PtosControl2.Add(Result);
end;
function TObjGraf.SelecPtoControl(xp, yp:integer): TPtoCtrl;
//Indica si selecciona a algún punto de control y devuelve la referencia.
var pdc: TPtoCtrl;
begin
  Result := Nil;      //valor por defecto
  if behav = behav1D then begin
     for pdc in PtosControl1 do begin
         if pdc.LoSelec(xp,yp) then begin
             Result := pdc;
             exit;
         end;
     end;
  end else if behav = behav2D then begin
    for pdc in PtosControl2 do begin
        if pdc.LoSelec(xp,yp) then begin
            Result := pdc;
            exit;
        end;
    end;
  end;
end;
//Puntos de conexión
function TObjGraf.AddPtoConex(xOff, yOff: Single): TPtoConx;
begin
  Result := TPtoConx.Create(v2d);
  Result.xFac := xOff/Width;
  Result.yFac := yOff/Height;
  //Actualiza coordenadas absolutas
  Result.x := x + xOff;
  Result.y := x + yOff;
  Result.Parent := self;
  PtosConex.Add(Result);
end;
function TObjGraf.SelectConnectionPoint(xp, yp: integer; accuracy: integer = 0): TPtoConx;
{Indica si las coordenadas de pantalla, seleccionan a un Punto de conexión. De ser así
devuelve la referencia al punto de COnexión, de otra forma devuevlve NIL.}
var
  pcnx: TPtoConx;
begin
  Result := Nil;      //valor por defecto
  for pcnx in PtosConex do begin
     if pcnx.LoSelec(xp, yp, accuracy) then begin
        Result := pcnx;
        exit;
     end;
  end;
end;
function TObjGraf.MarkConnectionPoint(xp, yp: integer; accuracy: integer = 0): TPtoConx;
{Explore the object and mark (set flag .Mark) the Connection point selected.
Only one Connection point can be marked. Return the Connection point selected.}
var
  pcnx: TPtoConx;
  found: Boolean;
begin
  Result := nil;      //valor por defecto
  found := false;
  for pcnx in PtosConex do begin
     if found then begin
       //Already found. Clear because only one point can be selected.
       pcnx.Marked := false;
     end else begin
       //Find
       if pcnx.LoSelec(xp, yp, accuracy) then begin
         pcnx.Marked := true;
         Result := pcnx;
         found := true;
       end else begin
         //Clear flag in case it was set before
         pcnx.Marked := false;
       end;
     end;
  end;
end;
procedure TObjGraf.ClearMarkConnectionPoints;
{Clear the mark for all the Connection points of the object}
var
  pcnx: TPtoConx;
begin
  for pcnx in PtosConex do begin
    pcnx.Marked := false;
  end;
end;
function TObjGraf.ConnectionPointMarked: TPtoConx;
{Return the Connection point marked is one exists, otherwise return NIL.}
var
  pcnx: TPtoConx;
begin
  for pcnx in PtosConex do begin
    if pcnx.Marked then exit(pcnx);
  end;
  exit(nil);
end;

//Inicialización
constructor TObjGraf.Create(mGraf: TMotGraf);
begin
  inherited Create;
  erased := false;
  v2d := mGraf;   //asigna motor gráfico
  visible := true;
  width := 100;   //width por defecto
  height := 100;    //height por defecto
  fx := 100;
  fy := 100;
  PtosControl1:= TPtosControl.Create(True);   //Crea lista con administración de objetos
  PtosControl2:= TPtosControl.Create(True);   //Crea lista con administración de objetos
  PtosConex  := TPtosConex.Create(true);
  Selected   := False;
  Marked    := False;
  Proceso    := false;
  DibSimplif := false;
  Highlight  := true;
  //Crea puntos de control estándar. Luego se pueden eliminar fy crear nuevos o modificar
  //estos puntos de control.
  pcTOP_LEF  := AddPtoControl2D(TD_SUP_IZQ, crSizeNW);
  pcTOP_CEN := AddPtoControl2D(TD_SUP_CEN, crSizeNS);
  pcTOP_RIG := AddPtoControl2D(TD_SUP_DER, crSizeNE);

  pcCEN_LEF := AddPtoControl2D(TD_CEN_IZQ, crSizeWE);
  pcCEN_RIG := AddPtoControl2D(TD_CEN_DER, crSizeWE);

  pcBOT_LEF := AddPtoControl2D(TD_INF_IZQ, crSizeNE);
  pcBOT_CEN := AddPtoControl2D(TD_INF_CEN, crSizeNS);
  pcBOT_RIG := AddPtoControl2D(TD_INF_DER, crSizeNW);
  //Crea puntos de control para formas 1D
  pcBEGIN   := AddPtoControl1D(TD_SUP_IZQ, crSize);
  pcEND     := AddPtoControl1D(TD_INF_DER, crSize);
  pcBEGIN.idIcon := pciCircle;
  pcEND.idIcon := pciCircle;
  //Comportamiento por defecto
  behav := behav2D;
end;
destructor TObjGraf.Destroy;
begin
  //Se desconecta los Ptos de Control 1D.
  pcBEGIN.Disconnect;
  pcEND.Disconnect;
  //Elimina Puntos de control
  PtosControl1.Free;
  PtosControl2.Free;
  PtosConex.Free;
  inherited Destroy;
end;

end.

