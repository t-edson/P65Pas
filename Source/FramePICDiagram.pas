{
Define el frame que implementa la interfaz gráfica donde se muestra el PIC y al que
se le pueden agreagr compoentes electrónicos adicionales como leds, o pantallas LCD.

                                                Creado por Tito Hinostroza 05/2018.
                                                Modif. por Tito Hinostroza 08/2018.
}
unit FramePICDiagram;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, fgl, Types, Forms, Controls, ExtCtrls, Graphics,
  Menus, ActnList, LCLProc, ogMotEdicion, ogMotGraf2D, ogDefObjGraf, CPUCore,
  Parser, MisUtils;
type
  { TPinGraph }
  {Objeto que modela a un pin físico de un componente electrónico.
  Se penso en usar el mismo tipo TpicCore.pines[] y usar ese arreglo como contenedor
  de objetos TPinGraph, pero considerando que los pines están fuertemente asociados a
  un punto de conexión, se decidió crearlo como una extensión de TPtoConx, así se
  simplifica considerablemente la administración.}
  TPinGraph = class(TPtoConx)
  private  //Parámetros del modelo interno del Pin
    {Cuando es un componente común. se leerán estos parámetros, como modelo
    del pin.}
    vThev: single;
    rThev: single;
    procedure GetModel(out vThev0, rThev0: Single);  //Devuelve parámetros del modelo eléctrico
    procedure SetModel(vThev0, rThev0: Single);  //Fija parámetros del modelo eléctrico
  private  //Valores de voltaje e impedancia del nodo al que se encuentra conectado
    vNod: single;
    rNod: single;
    procedure SetNodePars(vNod0, rNod0: Single);   //Fija parámetros debido al nodoconectado
  private  //Campos adicionales cuando es pin de un PIC
    {Se necesita una referencia al PIC cuando este pin es parte de un PIC.}
    pic   : TCPUCore;
    nPin  : integer;    //Número de pin (del encapsulado)
  private  //Propiedades geométricas
    x1, y1, x2, y2: Single; //Cordenadas cuando se representa como rectángulo
    lbl   : string;     //Etiqueta
    xLbl, yLbl: Single; //Posición de la etiqueta
  public
    //procedure GetThevNod(out vThev0, rThev0: Single);//Devuelve parámetros del modelo eléctrico
    procedure SetLabel(xl, yl: Single; txt: string; align: TAlignment =
      taLeftJustify);
    constructor Create(mGraf: TMotGraf);  //OJO: No es override
  end;
  TPicPinList = specialize TFPGObjectList<TPinGraph>;  //Lista para gestionar los puntos de control

  { TOgComponent }
  {Incluye propiedades de los componentes para este editor gráfico.}
  TOgComponent = class(TObjGraf)
  private
  public
    Ref: string;  //Nomenclatura única del componente: R1, R2, CI1
    ShowRef: boolean;
    xRef, yRef: Single;  //Ubicación relativa de la etiqueta Ref
//    pins: TPicPinList;  //Lista de pines
    function AddPtoConex(xOff, yOff: Single): TPinGraph; override;
    function AddPin(xCnx, yCnx, x1, y1, x2, y2: Single): TPinGraph;
    constructor Create(mGraf: TMotGraf); override;
    destructor Destroy; override;
  end;

  TNode = class;

  { TOgConector }
  TOgConector = class(TOgComponent)
  private
    OnConnect: procedure of object;
    OnDisconnect: procedure of object;
    nodParent: TNode;
    PaintBox : TPaintBox;  //Referencia al PaintBox donde se dibuja
    ptos: array of TFPoint;
    procedure PCtlConnect(pCtl: TPtoCtrl; pCnx: TPtoConx);
    procedure PCtlDisconnect(pCtl: TPtoCtrl; pCnx: TPtoConx);
    function ConnectedTo(ogCon: TOgConector): boolean;
  public
    function LoSelecciona(xr, yr: Integer): Boolean; override;
    procedure Draw; override;
    constructor Create(mGraf: TMotGraf); override;
    destructor Destroy; override;
  end;
  TConnectorList = specialize TFPGObjectList<TOgConector>;  //Lista para gestionar los puntos de control

  { TNode }
  TNode = class
  private
    vt, rt: Single;
    connectorList: TConnectorList;
    pinList      : TPicPinList;  //Lista de pines conectadas al nodo
    procedure UpdateModel;
    function Contains(ogCon: TOgConector): boolean;
    function ConnectedTo(ogCon: TOgConector): boolean;
    procedure AddConnector(ogCon: TOgConector);
  public
    constructor Create;
    destructor Destroy; override;
  end;
  TNodeList = specialize TFPGObjectList<TNode>;  //Lista de nodos

  { TOgPic }
  //Define el objeto gráfico PIC
  TOgPic = class(TOgComponent)
  private
    pic: TCPUCore;   //referencia al PIC
    xpin: Single;  //Posición X del Pin
    nPinsDiag: Integer;  //Número de pines a dibujar
    nPinsSide: Integer;
  public
    procedure SetPic(pic0: TCPUCore);
    procedure Draw; override;
    constructor Create(mGraf: TMotGraf); override;
    destructor Destroy; override;
  end;

  { TOgLogicState }
  //Define el objeto gráfico LogicState
  TOgLogicState = class(TOgComponent)
  private
    ptos: array of TFPoint;
    pin: TPinGraph;
  public
    //procedure SetState(Value: boolean);
    procedure Draw; override;
    constructor Create(mGraf: TMotGraf); override;
    destructor Destroy; override;
  end;

  { TOgLedRed }
  //Define el objeto Diodo Led
  TOgLedRed = class(TOgComponent)
  private
    pin: TPinGraph;
  public
    //procedure SetState(Value: boolean);
    procedure Draw; override;
    constructor Create(mGraf: TMotGraf); override;
    destructor Destroy; override;
  end;

  { TOg7Segment }
  //Define el objeto Display de 7 segmentos
  TOg7Segment = class(TOgComponent)
  private
    pinA: TPinGraph;
    pinB: TPinGraph;
    pinC: TPinGraph;
    pinD: TPinGraph;
    pinE: TPinGraph;
    pinF: TPinGraph;
    pinG: TPinGraph;
  public
    //procedure SetState(Value: boolean);
    procedure Draw; override;
    constructor Create(mGraf: TMotGraf); override;
    destructor Destroy; override;
  end;

  { TOgResisten }
  //Define el objeto Resistencia (Resistor)
  TOgResisten = class(TOgComponent)
  private
  public
    //procedure SetState(Value: boolean);
    procedure Draw; override;
    constructor Create(mGraf: TMotGraf); override;
    destructor Destroy; override;
  end;

  { TfraPICDiagram }
  TfraPICDiagram = class(TFrame)
    acAddLogTog: TAction;
    acGenDelObject: TAction;
    acGenConnTo: TAction;
    acAddConn: TAction;
    acAddLed: TAction;
    acAddResis: TAction;
    acAdd7SegComC: TAction;
    acGenReconn: TAction;
    ActionList1: TActionList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    mnReconn: TMenuItem;
    mnConnect: TMenuItem;
    mnReset: TMenuItem;
    mnRun: TMenuItem;
    MenuItem3: TMenuItem;
    mnAddLogicTog: TMenuItem;
    mnStepOver: TMenuItem;
    mnDelete: TMenuItem;
    PaintBox1: TPaintBox;
    PopupMenu1: TPopupMenu;
    procedure acAdd7SegComCExecute(Sender: TObject);
    procedure acAddConnExecute(Sender: TObject);
    procedure acAddLedExecute(Sender: TObject);
    procedure acAddLogTogExecute(Sender: TObject);
    procedure acAddResisExecute(Sender: TObject);
    procedure acGenConnToExecute(Sender: TObject);
    procedure acGenDelObjectExecute(Sender: TObject);
    procedure acGenReconnExecute(Sender: TObject);
  private  //Nombres y referencias
    procedure connectorChange;
    function ExistsName(AName: string): boolean;
    function UniqueName(NameBase: string): string;
    function ExistsRef(ARef: string): TOgComponent;
    function UniqueRef(RefBase: string): string;
  private  //Manejo de nodos
    nodeList: TNodeList;
    procedure AddConnectorToNodes(ogCon: TOgConector);
    procedure UpdateNodeList;
  private
    Fpic: TCPUCore;
    ogPic: TOgPic;
    motEdi: TModEdicion;
    procedure ConnectAction(Sender: TObject);
    procedure fraPICDiagramKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure motEdi_MouseUpRight(Shift: TShiftState; x, y: integer);
    procedure motEdi_MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure motEdi_MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    procedure Refrescar;
    procedure SetCompiler(cxp0: TCompilerBase);
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
const
  SEP_PIN = 20;   //Separación entre pines
  LON_PIN = 10;   //Longitud de pin
  //Colores pro defecto
  COL_CI  = $404040;  //Circuitos Integrados
  COL_GND = $404040;  //GND
  COL_VCC = clRed;    //Voltajes VCC
  COL_HIM = $A0A0A0;  //Alta impedancia
  COL_RES = $9FE7F9;  //Cuerpo de resistencias
const  //ID para componentes
  ID_PINGRAF = 1;    //Un pin que desciende de un Punto de conexión
  ID_COMPON  = 10;   //Componentes en general
  ID_PIC     = 11;
  ID_CONNEC  = 12;
  ID_LEDRES  = 13;
  ID_RESIST  = 14;
  ID_7SEGME  = 15;
  ID_TOG_LOG = 16;

function GetThevCol(vt, rt: Single): TColor;
{Devuelve un color que representa el estado de un circuito de Thevening.}
begin
  if rt>1e+6 then begin
    //Se considera alta impedancia
    exit(COL_HIM);
  end else begin
    //Tiene potencial
    if vt > 2.5 then begin
      exit(COL_VCC);
    end else begin
      exit(COL_GND);
    end;
  end;
end;
function ResParallel(r1, r2: Single): Single; inline;
begin
  if r1+r2=0 then begin
    exit(0)
  end else begin
    exit(r1*r2/(r1+r2));
  end;
end;
{ TNode }
procedure TNode.UpdateModel;
{Devuelve los parámetros de Thevening del nodo. Esto es útil para leer el voltaje del
nodo en cualquier momento, que es parte de un análisis común por nodos.}
var
  pin: TPinGraph;
  r0 : Single;
  v1, r1, v2, r2: Single;
  nResistor, nSource: integer;  //Contadores y banderas
begin
  //Casos especiales
  if pinList.Count = 0 then begin
    //Nodo sin conexiones
    vt := 0;
    rt := 1e+9;  //Alta impedancia
  end else if pinList.Count = 1 then begin
    //Conectado a un solo pin
    pinList[0].GetModel(vt, rt);  //Mismo modelo del nodo
  end else begin
    //Conectado a varios pines
    {Va simplificando por un lado las que son fuentes (en v1, r1)
    y por otro las que son solo resistencias (en r0). La idea es que al final
    se tenga:

          +--[R1]---
          |
         [V1]
          |
         ---

    Y por el otro:

      ----+
          |
         [R0]
          |
         ---
    }
    nResistor := 0;
    nSource   := 0;
    for pin in pinList do begin
      pin.GetModel(v2, r2);
      if v2 = 0 then begin
        //Es resistencia pura
        if nResistor=0 then begin
          //Primera resistencia
          r0 := r2;  //toma su valor
        end else begin
          r0 := ResParallel(r0, r2);  //Acumula en paralelo
        end;
        inc(nResistor);  //Lleva la cuenta
      end else begin
        //Es fuente con resistencia
        if nSource=0 then begin
          //Primera fuente
          v1 := v2;  //Toma su valor
          r1 := r2;
        end else begin
          if r1+r2 = 0 then begin
            //Hay conexión directa de dos fuentes
            {Se pone valor de voltaje promedio para evitar la indeterminación, pero
            ralemnte debería generarse un error o advertencia.}
            v1 := (v1+v2)/2;
          end else begin
            v1 := v2 + (v1-v2)*r2/(r1+r2);
          end;
          r1 := ResParallel(r1, r2);
        end;
        inc(nSource);  //lleva la cuenta
      end;
    end;
    //Ya se han explorado todos los pines. Ahora analzia lso casos
    if nSource=0 then begin
      //Todas son resistncias
      vt := 0;
      rt := r0;  //Resistencia acumulada
    end else if nResistor=0 then begin
      //Todas son fuentes thevening
      vt := v1;  //Voltaje acumulado
      rt := r1;  //Resistencia acumulada
    end else begin
      //Caso general: Fuentes de thevening con resistencia:
      vt := v1*r0/(r0 + r1);
      rt := ResParallel(r0, r1);
    end;
  end;
//debugln('Nodo actualizado: %d fuentes, %d resist.', [nSource, nResistor]);
  {Ahora que ya se tienen los valores de voltaje e impedancia del nodo, pasa esa
  información a todos los pines de los componentes conectados, para uniformizar estados}
  for pin in pinList do begin
    pin.SetNodePars(vt, rt);
  end;
end;
function TNode.Contains(ogCon: TOgConector): boolean;
{Indica si el conector está en la lista de conectores del nodo.}
var
  c: TOgConector;
begin
  for c in connectorList do begin
    if c = ogCon then exit(true);
  end;
  //No está
  exit(false);
end;
function TNode.ConnectedTo(ogCon: TOgConector): boolean;
{Indica si el conector indicado, está eléctricamente conectado a este nodo.}
var
  c: TOgConector;
begin
  //Verifia si etsá conectado a alguno de los conectores del nodo
  for c in connectorList do begin
    if c.ConnectedTo(ogCon) then exit(true);
  end;
  //No está conectado a ningún conector
  exit(false);
end;
procedure TNode.AddConnector(ogCon: TOgConector);
{Agrega un connector a la lista de conectores del nodo. Se supone que todo los
conectores de un nodo están eléctrticamenet conectados.}
  procedure AddPinOf(pCtl: TPtoCtrl);
  var
    pin: TPinGraph;
  begin
    if pCtl.ConnectedTo<>nil then begin
      if pCtl.ConnectedTo.Id = ID_PINGRAF then begin
        pin := TPinGraph(pCtl.ConnectedTo);
        if pinList.IndexOf(pin) = -1 then begin
          //No existe el pin, lo agrega
          pinList.Add(pin);
        end;
      end;
    end;
  end;
begin
  connectorList.Add(ogCon);
  ogCon.nodParent := self;  //Guarda referencia
  //También se guardan los pines a los que se encuentra conectado
  AddPinOf(ogCon.pcBEGIN);
  AddPinOf(ogCon.pcEND);
end;
constructor TNode.Create;
begin
  connectorList:= TConnectorList.Create(false);
  pinList := TPicPinList.Create(false);
end;
destructor TNode.Destroy;
begin
  pinList.Destroy;
  connectorList.Destroy;
  inherited Destroy;
end;
{ TPinGraph }
procedure TPinGraph.GetModel(out vThev0, rThev0: Single);
{Devuelve el equivalente de Thevening del pin. Equivale a devolver el modelo eléctrico
del pin, cuando está desconectado.}
begin
  if pic = nil then begin
    //No pertenece a un PIC, lee directamente sus parámetros
    vThev0 := vThev;
    rThev0 := rThev;
  end else begin
    //Es pin de un pic
//    pic.GetPinThev(nPin, vThev0, rThev0);
  end;
end;
procedure TPinGraph.SetModel(vThev0, rThev0: Single);
begin
  if pic = nil then begin
    //No pertenece a un PIC, lee directamente sus parámetros
    vThev := vThev0;
    rThev := rThev0;
  end else begin
    //Es pin de un pic. No se puede cambiar
  end;
end;
procedure TPinGraph.SetNodePars(vNod0, rNod0: Single);
{Fija los valores de voltaje que debe tener el pin, y la impedancia que debe ver,
por el efecto de estar conectado a algún nodo.
Se supone que ya se ha hecho el cálculo de voltaje/impedancia em el nodo.}
begin
  if pic = nil then begin
    //No pertenece a un PIC
    vNod := vNod0;
    rNod := rNod0;
  end else begin
    //Es parte de un PIC
//    pic.SetNodePars(nPin, vNod0, rNod0);
  end;
end;
procedure TPinGraph.SetLabel(xl, yl: Single; txt: string;
                             align: TAlignment = taLeftJustify);
begin
  lbl := txt;
  yLbl := yl;
  case align of
  taLeftJustify: xLbl := xl;  //Justificado a la
  taRightJustify: xLbl := xl - v2d.TextWidth(txt);
  end
end;
constructor TPinGraph.Create(mGraf: TMotGraf);
begin
  inherited Create(mGraf);
  {Se usa un ID porque identificar un objeto por ID es más rápido que usar
  la comparación con: <variable> IS <Alguna Clase>.}
  id := ID_PINGRAF;
end;
{ TOgComponent }
function TOgComponent.AddPtoConex(xOff, yOff: Single): TPinGraph;
{Reescribimos nuestra propia función porque no vamos a agregar objetos TPtoConx,
sino objetos TPinGraph.}
begin
  Result := TPinGraph.Create(v2d);
  ////// Esta sección es similar al del método virtual AddPtoConex //////
  Result.xFac := xOff/Width;
  Result.yFac := yOff/Height;
  //Actualiza coordenadas absolutas
  Result.x := x + xOff;
  Result.y := x + yOff;
  Result.Parent := self;
  PtosConex.Add(Result);
end;
function TOgComponent.AddPin(xCnx, yCnx, //Coord. del punto de conexión
                             x1, y1, x2, y2: Single): TPinGraph;
var
  pin: TPinGraph;
begin
  pin := AddPtoConex(xCnx, yCnx);
  pin.v2d := self.v2d;
  pin.x1 := x1;
  pin.y1 := y1;
  pin.x2 := x2;
  pin.y2 := y2;
  pin.nPin := PtosConex.Count;
  Result := pin;
end;
constructor TOgComponent.Create(mGraf: TMotGraf);
begin
  Id := ID_COMPON;
  inherited Create(mGraf);
end;
destructor TOgComponent.Destroy;
  begin
    inherited Destroy;
  end;
{ TOgConector }
procedure TOgConector.PCtlConnect(pCtl: TPtoCtrl; pCnx: TPtoConx);
{Un punto de control se conecta a un punto de conexión }
begin
   if OnConnect<>nil then OnConnect();
end;
procedure TOgConector.PCtlDisconnect(pCtl: TPtoCtrl; pCnx: TPtoConx);
{Un punto de control se desconecta a un punto de conexión }
begin
  if OnDisconnect<>nil then OnDisconnect();
end;
function TOgConector.ConnectedTo(ogCon: TOgConector): boolean;
{Verifica si hay conexión entre este conector y "ogCon"}
  function ConnectedSameConexionPoint(p1, p2: TPtoCtrl): boolean;
  {Indica si los puntos de control indicados, están conectados al mismo Punto
  de Conexión.}
  begin
    if p1.ConnectedTo = nil then exit(false);  //No está conectado a nada
    if p2.ConnectedTo = nil then exit(false);  //No está conectado a nada
    if p1.ConnectedTo = p2.ConnectedTo then exit(true) else exit(false);
  end;
begin
  if ConnectedSameConexionPoint(pcBEGIN, ogCon.pcBEGIN) then exit(true);
  if ConnectedSameConexionPoint(pcBEGIN, ogCon.pcEND) then exit(true);
  if ConnectedSameConexionPoint(pcEND, ogCon.pcBEGIN) then exit(true);
  if ConnectedSameConexionPoint(pcEND, ogCon.pcEND) then exit(true);
  exit(false);
end;
function TOgConector.LoSelecciona(xr, yr: Integer): Boolean;
var
  x0, y0, x1, y1: Integer;
begin
  v2d.XYpant(pcBEGIN.x, pcBEGIN.y,  x0, y0);
  v2d.XYpant(pcEND.x, pcEND.y, x1, y1);
  Result := PointSelectSegment(xr, yr, x0, y0, x1, y1 );
end;
procedure TOgConector.Draw;
const
  ANCHO1 = 7;
  ANCHO2 = 50;
  ALTO1 = 10;
  ALTO2 = 30;
var
  col: TColor;
  pct  : TPtoCtrl;
  pcn : TPtoConx;
  x1, y1: Single;
  pMouse: TPoint;
begin
  //Descripción
  //v2d.SetText(clBlack, 11,'', true);
  //v2d.Texto(X + 2, Y -20, 'Conector');
  //Cuerpo
  col := GetThevCol(nodParent.vt, nodParent.rt);  //Se supone que el nodo padre ya está actualizado
  v2d.SetPen(psSolid, 1, col);
  v2d.Linea(pcBEGIN.x, pcBEGIN.y, pcEND.x, pcEND.y);
  //Implementamos nosotros el remarcado y selección, para personalizar mejor
  //---------------Draw mark --------------
  if Marked and Highlight then begin
    //Resaltado
    v2d.SetPen(psSolid, 2, clBlue);   //RGB(128, 128, 255)
    v2d.Linea(pcBEGIN.x, pcBEGIN.y, pcEND.x, pcEND.y);
    //Marcador de Voltaje
    v2d.SetPen(psSolid, 1, clBlack);   //RGB(128, 128, 255)
    v2d.SetBrush(clYellow);
    pMouse := PaintBox.ScreenToClient(Mouse.CursorPos);
    x1 := v2d.Xvirt(pMouse.x, pMouse.y);
    y1 := v2d.Yvirt(pMouse.x, pMouse.y);
//    v2d.RectangR(x1, y1, x1+10, y1+20);
    ptos[0].x := x1;
    ptos[0].y := y1;
    ptos[1].x := x1+ANCHO1;
    ptos[1].y := y1-ALTO1;
    ptos[2].x := x1+ANCHO1;
    ptos[2].y := y1-ALTO2;
    ptos[3].x := x1+ANCHO2;
    ptos[3].y := y1-ALTO2;
    ptos[4].x := x1+ANCHO2;
    ptos[4].y := y1-ALTO1;
    ptos[5].x := x1+20;
    ptos[5].y := y1-ALTO1;
    v2d.Polygon(ptos);

    v2d.SetText(True, False, False);
    v2d.SetText(clBlack);
    v2d.Texto(x1+ANCHO1+3, y1 - ALTO2+2, Format('%.2fV', [nodParent.vt]));
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
constructor TOgConector.Create(mGraf: TMotGraf);
begin
  inherited Create(mGraf);
  Id := ID_CONNEC;
  setlength(ptos, 6);
  pcBEGIN.OnConnect := @PCtlConnect;
  pcEND.OnConnect := @PCtlConnect;
  pcBEGIN.OnDisconnect := @PCtlDisconnect;
  pcEND.OnDisconnect := @PCtlDisconnect;
end;
destructor TOgConector.Destroy;
begin
  inherited Destroy;
end;
{ TOgPic }
procedure TOgPic.SetPic(pic0: TCPUCore);
{Fija el dispositivo de trabajo y prepara las estructuras que
definen la geometría del componente, de modo que el dibujo sea rápido.}
var
  newHeight, i: Integer;
  ypin: Single;
  pin: TPinGraph;
begin
  pic := pic0;  //Actualiza referencia
  Name := pic0.Model;
  //Define geometría del cuerpo del PIC
  if pic.Npins <= 6 then begin
    nPinsDiag := 6;
  end else if pic.Npins <=8 then begin
    nPinsDiag := 8;
  end else if pic.Npins <=14 then begin
    nPinsDiag := 14;
  end else if pic.Npins <=18 then begin
    nPinsDiag := 18;
  end else if pic.Npins <=28 then begin
    nPinsDiag := 28;
  end else if pic.Npins <=40 then begin
    nPinsDiag := 40;
  end else begin
    //Caso de muchos pines
    nPinsDiag := 40;
  end;
  nPinsSide := nPinsDiag div 2;  //Pines pro lado
  newHeight := nPinsSide * SEP_PIN; //Altura del chip
  //Actualiza tamaño. Se debe hacer antes de calcular las posiciones de los Ptos. de Conexión.
  ReSize(Width, newHeight);
  {Calcula posiciones relativas de los pines asumiendo un formato de encapsulado DIL.
  Se crearán también los puntos de conexión en cada uno de los pines}
  PtosConex.Clear;  //Se aprovechará para crear puntos de conexión
  //Pines de la izquierda
  ypin := SEP_PIN/2;   //posición inicial
  xpin := -LON_PIN+3;
  for i:=1 to nPinsSide do begin
    //Pin
    pin := AddPin(xpin, ypin-1,
                  xpin, ypin-5, xpin+LON_PIN+1, ypin+5);
    pin.SetLabel(5, ypin-8, pic.pines[i].GetLabel);
    pin.pic := pic0;  //guarda referencia el PIC
    //pin.lValue := @;
    //Calcula siguiente posición
    ypin := ypin + SEP_PIN;
  end;
  //Pines de la derecha
  ypin := SEP_PIN/2 + (nPinsSide-1) * SEP_PIN;   //posición inicial
  xpin := width-3;
  for i:=nPinsSide+1 to nPinsDiag do begin
    //Pin
    pin := AddPin(xpin+LON_PIN-1, ypin,
                  xpin, ypin-5, xpin+LON_PIN-1, ypin+5);
    pin.SetLabel(xpin-2, ypin-8, pic.pines[i].GetLabel, taRightJustify);
    pin.pic := pic0;  //guarda referencia el PIC
    //Calcula siguiente posición
    ypin := ypin - SEP_PIN;
  end;
  //Actualiza posición.
  Relocate(x, y);  //Se mantiene la posición, pero se hace para actualizar a los puntos de conexión
end;
procedure TOgPic.Draw;
const
  RAD_MARK = 15;  //Radio de la marca superior del chip
var
  ancho, rt, vt, xMed: Single;
  pin : TPinGraph;
  pCnx: TPtoConx;
begin
  if pic= nil then begin
    v2d.SetPen(psSolid, 1, clBlack);
    v2d.SetBrush(clGray);
    v2d.RectangR(x, y, x+Width, y+Height);
  end else begin //Caso normal
    xMed := x + width/2;
    //Dibuja título
    ancho := v2d.TextWidth(Name);
    v2d.SetText(True, False, False);
    v2d.Texto(xMed - ancho/2 , y - 18, Name);
    //Dibuja cuerpo
    v2d.SetText(False, False, False);
    v2d.SetText($D0D0D0);
    v2d.SetPen(psSolid, 1, clGray);
    v2d.SetBrush(COL_CI);
    v2d.RectangR(x, y, x+Width, y+Height);  //fondo
    v2d.SetBrush($202020);
    v2d.RadialPie(xMed-RAD_MARK ,y-RAD_MARK, xMed+RAD_MARK,y+RAD_MARK,2880,2880);
    //Dibuja los pines
    for pCnx in self.PtosConex do begin
      pin := TPinGraph(pCnx);
      //En el PIC, los pines se pintan con el color del modelo interno
      pin.GetModel(vt, rt);
      v2d.SetBrush(GetThevCol(vt,rt));  //Rellena de acuerdo al estado
      v2d.rectangR(x+pin.x1, y+pin.y1, x+pin.x2, y+pin.y2);
      v2d.Texto(x+pin.xLbl, y+pin.yLbl, pin.lbl);
    end;
  end;
  inherited;
end;
constructor TOgPic.Create(mGraf: TMotGraf);
begin
  inherited Create(mGraf);
  ID := ID_PIC;
  Width := 140;
  Height := 180;
//  pcTOP_CEN.Visible := false;
//  pcBOT_CEN.Visible := false;
//  pcCEN_LEF.Visible := false;
//  pcCEN_RIG.Visible := false;
  SizeLocked := true;
//  ShowPtosConex:=true;  //Muestra los puntos de conexión
end;
destructor TOgPic.Destroy;
begin
  inherited Destroy;
end;
{ TOgLogicState }
procedure TOgLogicState.Draw;
var
  ancho: Single;
begin
  //Dibuja título
  ancho := v2d.TextWidth(Name);
  v2d.SetText(COL_GND);
  v2d.SetText(True, False, False);
  v2d.Texto(x + width/2 - ancho/2 , y - 18, Name);
  //Dibuja cuerpo
  v2d.SetPen(psSolid, 1, clBlack);
  if pin.vThev>2.5 then begin
    v2d.SetBrush(clRed)
  end else begin
    v2d.SetBrush(clGray);
  end;
  //v2d.RectangR(x, y, x+Width, y+Height);
  ptos[0].x := x;
  ptos[0].y := y;
  ptos[1].x := x+20;
  ptos[1].y := y;
  ptos[2].x := x+30;
  ptos[2].y := y+10;
  ptos[3].x := x+20;
  ptos[3].y := y+20;
  ptos[4].x := x;
  ptos[4].y := y+20;
  v2d.Polygon(ptos);
  inherited;
end;
constructor TOgLogicState.Create(mGraf: TMotGraf);
begin
  inherited Create(mGraf);
  id := ID_TOG_LOG;
  setlength(ptos, 5);
  Width  := 30;
  Height := 20;
  pcTOP_CEN.Visible := false;
  pcBOT_CEN.Visible := false;
  pcCEN_LEF.Visible := false;
  pcCEN_RIG.Visible := false;
  SizeLocked := true;
  pin := AddPin(30, 10, 0, 0, 0, 0);
  pin.rThev :=  0;
  pin.vThev :=  5;  //voltios
  //ShowPtosConex:=true;
end;
destructor TOgLogicState.Destroy;
begin
  inherited Destroy;
end;
{ TOgLedRed }
procedure TOgLedRed.Draw;
var
  ancho, x2, y2, yled: Single;
begin
  x2:=x+width;
  yled := y + 40;
  y2:=y+height;
  //Dibuja título
  ancho := v2d.TextWidth(Name);
  v2d.SetText(COL_GND);
  v2d.SetText(True, False, False);
  v2d.Texto(x + width/2 - ancho/2 , y - 18, Name);
  //Verifica valor lógico

  //FState
  //Dibuja cuerpo
  v2d.SetPen(psSolid, 2, COL_GND);
  //Línea vertioal y conexión a tierra
  v2d.Linea(x+12, y, x+12, y2);
  v2d.Linea(x+5, y2, x+19, y2);
  //Resistencia
  v2d.SetPen(psSolid, 1, COL_GND);
  v2d.SetBrush(COL_RES);
  v2d.RectangR(x+5, y+10, x2-5, y+35);
  //Símbolo circular
  if pin.vNod>2 then v2d.SetBrush(clRed)
  else v2d.SetBrush(clGray);
  v2d.Ellipse(x, yled, x+width, yled+24);
  v2d.Ellipse(x+3, yled+3, x+width-3, yled+24-3);
  inherited;
end;
constructor TOgLedRed.Create(mGraf: TMotGraf);
begin
  inherited Create(mGraf);
  id := ID_LEDRES;
  Width  := 24;
  Height := 70;
  pcTOP_CEN.Visible := false;
  pcBOT_CEN.Visible := false;
  pcCEN_LEF.Visible := false;
  pcCEN_RIG.Visible := false;
  SizeLocked := true;
  pin := AddPin(12, 0, 0, 0, 0, 0);
  pin.SetModel(0, 470); //0V, 470ohms. Por ahora se modela así
  pin.lbl := 'VLed';
  //ShowPtosConex:=true;
end;
destructor TOgLedRed.Destroy;
begin
  inherited Destroy;
end;
{ TOg7Segment }
procedure TOg7Segment.Draw;
var
  ancho, x2, y2, x1, y1, y3: Single;
  pCnx: TPtoConx;
  pin: TPinGraph;
begin
  //Dibuja título
  ancho := v2d.TextWidth(Name);
  v2d.SetText(COL_GND);
  v2d.SetText(True, False, False);
  v2d.Texto(x + width/2 - ancho/2 , y - 18, Name);
  //Dibuja cuerpo
  v2d.SetPen(psSolid, 2, COL_GND);
  v2d.SetBrush(COL_HIM);
  v2d.RectangR(x, y, x+Width, y+Height);
  //Segmentos
  x1 := x +10;
  x2 := x+width-10;
  y1 := y + 10;
  y2 := y + 50;
  y3 := y + 90;
  //Segment A
  if pinA.vNod>2 then begin
    v2d.SetPen(psSolid, 1, clRed); v2d.SetBrush(clRed);
  end else begin
    v2d.SetPen(psSolid, 1, $808080); v2d.SetBrush($808080);
  end;
  v2d.RectangR(x1, y1-3, x2, y1+3);
  //Segment B
  if pinB.vNod>2 then begin
    v2d.SetPen(psSolid, 1, clRed); v2d.SetBrush(clRed);
  end else begin
    v2d.SetPen(psSolid, 1, $808080); v2d.SetBrush($808080);
  end;
  v2d.RectangR(x2-3, y1+3, x2+3, y2-3);
  //Segment C
  if pinC.vNod>2 then begin
    v2d.SetPen(psSolid, 1, clRed); v2d.SetBrush(clRed);
  end else begin
    v2d.SetPen(psSolid, 1, $808080); v2d.SetBrush($808080);
  end;
  v2d.RectangR(x2-3, y2+3, x2+3, y3-3);
  //Segment D
  if pinD.vNod>2 then begin
    v2d.SetPen(psSolid, 1, clRed); v2d.SetBrush(clRed);
  end else begin
    v2d.SetPen(psSolid, 1, $808080); v2d.SetBrush($808080);
  end;
  v2d.RectangR(x1, y3-3, x2, y3+3);
  //Segment E
  if pinE.vNod>2 then begin
    v2d.SetPen(psSolid, 1, clRed); v2d.SetBrush(clRed);
  end else begin
    v2d.SetPen(psSolid, 1, $808080); v2d.SetBrush($808080);
  end;
  v2d.RectangR(x1-3, y2+3, x1+3, y3-3);
  //Segment F
  if pinF.vNod>2 then begin
    v2d.SetPen(psSolid, 1, clRed); v2d.SetBrush(clRed);
  end else begin
    v2d.SetPen(psSolid, 1, $808080); v2d.SetBrush($808080);
  end;
  v2d.RectangR(x1-3, y1+3, x1+3, y2-3);
  //Segment G
  if pinG.vNod>2 then begin
    v2d.SetPen(psSolid, 1, clRed); v2d.SetBrush(clRed);
  end else begin
    v2d.SetPen(psSolid, 1, $808080); v2d.SetBrush($808080);
  end;
  v2d.RectangR(x1, y2-3, x2, y2+3);

  //conexión a tierra
  v2d.SetPen(psSolid, 1, COL_GND);
  y2 := y + height + 10;
  v2d.Linea(x+30, y+height, x+30, y2);
  v2d.Linea(x+24, y2, x+36, y2);
  //Dibuja los pines
  v2d.SetPen(psSolid, 1, COL_GND);
  for pCnx in PtosConex do begin
    pin := TPinGraph(pCnx);
    //En el PIC, los pines se pintan con el color del modelo interno
    v2d.SetBrush(clWhite);  //Rellena de acuerdo al estado
    v2d.Linea(pin.x, pin.y, pin.x+7, pin.y);
    v2d.Texto(x+pin.xLbl, y+pin.yLbl, pin.lbl);
  end;
  inherited;
end;
constructor TOg7Segment.Create(mGraf: TMotGraf);
begin
  inherited Create(mGraf);
  id := ID_7SEGME;
  Width  := 60;
  Height := 100;
  pcTOP_CEN.Visible := false;
  pcBOT_CEN.Visible := false;
  pcCEN_LEF.Visible := false;
  pcCEN_RIG.Visible := false;
  SizeLocked := true;
  pinA := AddPin(-7, 5, 0, 0, 0, 0);
  pinA.SetModel(0, 680);
  pinA.lbl := 'A';
  pinB := AddPin(-7,20, 0, 0, 0, 0);
  pinB.SetModel(0, 680);
  pinB.lbl := 'B';
  pinC := AddPin(-7,35, 0, 0, 0, 0);
  pinC.SetModel(0, 680);
  pinC.lbl := 'C';
  pinD := AddPin(-7,50, 0, 0, 0, 0);
  pinD.SetModel(0, 680);
  pinD.lbl := 'D';
  pinE := AddPin(-7,65, 0, 0, 0, 0);
  pinE.SetModel(0, 680);
  pinE.lbl := 'E';
  pinF := AddPin(-7,80, 0, 0, 0, 0);
  pinF.SetModel(0, 680);
  pinF.lbl := 'F';
  pinG := AddPin(-7,95, 0, 0, 0, 0);
  pinG.SetModel(0, 680);
  pinG.lbl := 'G';
  //ShowPtosConex:=true;
end;
destructor TOg7Segment.Destroy;
begin
  inherited Destroy;
end;
{ TOgResisten }
procedure TOgResisten.Draw;
var
  ancho, x2, y2: Single;
begin
  x2:=x+width;
  y2:=y+height;
  //Dibuja título
  ancho := v2d.TextWidth(Name);
  v2d.SetText(COL_GND);
  v2d.SetText(True, False, False);
  v2d.Texto(x + width/2 - ancho/2 , y - 18, Name);
  //Verifica valor lógico

  //Línea vertioal y conexión a tierra
  v2d.SetPen(psSolid, 2, COL_GND);
  v2d.Linea(x+12, y, x+12, y2);
  v2d.Linea(x+5, y2, x+19, y2);
  //Resistencia
  v2d.SetPen(psSolid, 1, COL_GND);
  v2d.SetBrush(COL_RES);
  v2d.RectangR(x+5, y+20, x2-5, y+50);
  inherited;
end;
constructor TOgResisten.Create(mGraf: TMotGraf);
var
  pin: TPinGraph;
begin
  inherited Create(mGraf);
  id := ID_RESIST;
  Width  := 24;
  Height := 70;
  pcTOP_CEN.Visible := false;
  pcBOT_CEN.Visible := false;
  pcCEN_LEF.Visible := false;
  pcCEN_RIG.Visible := false;
  SizeLocked := true;
  pin := AddPin(12, 0, 0, 0, 0, 0);
  pin.rThev := 1000;
  pin.vThev := 0;
  //ShowPtosConex:=true;
end;
destructor TOgResisten.Destroy;
begin
  inherited Destroy;
end;
{ TfraPICDiagram }
procedure TfraPICDiagram.Refrescar;
var
  nod: TNode;
begin
  {Aqui debería hacerse la actualización del PIC, y de los otros elementos que se mueven
  con reloj.}
  //pic.ExecNCycle()
  {Actualiza el estado de los Nodos porqu se supone que los voltajes o resisetncias de
  los componentes pueden haber cambiado (Como los pines de salida del PIC).}
  if nodeList <> nil then begin
    for nod in nodeList do begin
      nod.UpdateModel;
    end;
  end; //Protección
  motEdi.Refrescar;
end;
procedure TfraPICDiagram.SetCompiler(cxp0: TCompilerBase);
begin
  Fpic := cxp0.picCore;
  //Inicia dispositivo
  ogPic.SetPic(cxp0.picCore);
  //Al fijar el PIC, se elimina y crea un nuevo PIC,por ello hay que llamar a UpdateNodeList().
  UpdateNodeList;
end;
procedure TfraPICDiagram.fraPICDiagramKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  MsgBox('fraPICDiagramKeyDown');
end;
procedure TfraPICDiagram.ConnectAction(Sender: TObject);
var
  mnItem: TMenuItem;
  oc: TOgConector;
  a: TStringDynArray;
  comp1, comp2: TOgComponent;
  nPin1, nPin2: LongInt;
  pCnx1, pCnx2: TPtoConx;
  pin1 , pin2: TPinGraph;
  id1, id2: String;
begin
  if Sender is TMenuItem then begin
    mnItem := TMenuItem(Sender);
    //Agrega conector
    acAddConnExecute(self);
    oc := TOgConector(motEdi.Selected);
    //Ubica nodo Inicial
//    MsgBox(mnItem.Hint);
    a := Explode('-', mnItem.Hint);
    id1 := a[0];
    id2 := a[1];
    a := Explode('.', id1);
    comp1 := ExistsRef(a[0]);
    nPin1 := StrToInt(a[1]);
    pCnx1 := comp1.PtosConex[nPin1-1];
    if not(pCnx1 is TPinGraph) then exit;
    pin1 := TPinGraph(pCnx1);
    //Ubica nodo final
    a := Explode('.', id2);
    comp2 := ExistsRef(a[0]);
    nPin2 := StrToInt(a[1]);
    pCnx2 := comp2.PtosConex[nPin2-1];
    if not(pCnx2 is TPinGraph) then exit;
    pin2 := TPinGraph(pCnx2);
    //Ahora se conecta el conector a los Punttos de Conexión
    pin1.ConnectTo(oc.pcBEGIN);
    pin1.Locate(pin1.x, pin1.y); //Actualiza el "enganche"
    pin2.ConnectTo(oc.pcEND);
    pin2.Locate(pin2.x, pin2.y); //Actualiza el "enganche"
    //oc.Selec;          //Selecciona el conector
    Refrescar;
  end;
end;
//Manejo de nodos
procedure TfraPICDiagram.AddConnectorToNodes(ogCon: TOgConector);
{Agrega un conector al nodo que corresponda (al que contiene conectores que están
unidos eléctricamente a "conn"), o crea un nuevo nodo.}
var
  nod, newNode: TNode;
begin
  for nod in nodeList do begin
    if nod.Contains(ogCon) then begin
      //Ya lo contiene en su lista. No hay nada que hacer.
      exit;
    end else if nod.ConnectedTo(ogCon) then begin
      //Está eléctricamente conectado, pero no está en la lista
      nod.AddConnector(ogCon);  //Lo agrega
      exit;
    end;
  end;
  //No pertenece a ningún nodo existente.
  newNode := TNode.Create;  //Se crea con sus listas de conectores y pines vacías
  newNode.AddConnector(ogCon);
  nodeList.Add(newNode);
end;
procedure TfraPICDiagram.UpdateNodeList;
{Actualiza la lista de nodos a partir de los conectores existentes. Esta tarea
es importante para realizar el análisis correcto del voltaje e impedancia del nodo.
Como esta tarea puede ser algo pesada, por optimización se debe realizar solo cuando
se pueda producir cambios en los nodos (creación, eliminación, conexión y desconexión)}
var
  og: TObjGraf;
  ogCon: TOgConector;
begin
  nodeList.Clear;
  //Explora objetos gráfiocs
  for og in motEdi.objetos do begin
    if og is TOgConector then begin
      ogCon := TOgConector(og);
      AddConnectorToNodes(ogCon);
    end;
  end;
//debugln('Lista de Nodos:');
//for nod in nodeList do begin
//  debugln('  Nodo con '+IntToStr(nod.connectorList.Count)+' conectores.');
//end;
end;
function TfraPICDiagram.ExistsName(AName: string): boolean;
{Indica si existe algún componente con el nombre AName}
var
  og: TObjGraf;
begin
  for og in motEdi.objetos do begin
    if og.Name = AName then exit(true);
  end;
  exit(false);
end;
function TfraPICDiagram.UniqueName(NameBase: string): string;
{Obtiene un nombre único tomando como base la cadena "NameBase", de modo que si
en "NameBase" se indica "Nombre", se generará los nombres Nombre1, Nombre2, ... }
var
  n: Integer;
begin
  n := 1;   //Empieza con este valor
  Result := NameBase + IntToStr(n);  //Nombre tentativo
  While ExistsName(Result) do begin
    Inc(n);
    Result := NameBase + IntToStr(n);
  end;
end;
function TfraPICDiagram.ExistsRef(ARef: string): TOgComponent;
{Indica si existe algún componente con la referencia Aref. Si no existe devuelve NIL.}
var
  og: TObjGraf;
begin
  for og in motEdi.objetos do begin
    if not(og is TOgComponent) then continue;
    if TOgComponent(og).Ref = ARef then exit(TOgComponent(og));
  end;
  exit(Nil);
end;
function TfraPICDiagram.UniqueRef(RefBase: string): string;
{Obtiene una referencia única tomando como base la cadena "RefBase", de modo que si
en "RefBase" se indica "R", se generará los nombres R1, R2, R3, ... }
var
  n: Integer;
begin
  n := 1;   //Empieza con este valor
  Result := RefBase + IntToStr(n);  //Nombre tentativo
  While ExistsRef(Result)<>nil do begin
    Inc(n);
    Result := RefBase + IntToStr(n);
  end;
end;
procedure TfraPICDiagram.motEdi_MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LogInp: TOgLogicState;
  //pCnx: TPtoConx;
  //oc: TOgConector;
  //xv, yv: Single;
begin
  if motEdi.seleccion.Count = 1 then begin
    //Hay uno seleccionado
    if motEdi.Selected.LoSelecciona(X,Y) then begin
      //Click sobre un objeto seleccionado
      if motEdi.Selected is TOgLogicState then begin
        LogInp := TOgLogicState(motEdi.Selected);
        LogInp.pin.vThev := 5;
        Refrescar;
        //MsgBox('TOggle');
      end;
    end;
  end;
  //Verifica si se inicia la conexión de un pin
//  if Button = mbLeft then begin
//    pCnx := motEdi.ConnectionPointMarked;
//    if pCnx <> nil then begin
//      //Se soltó en con un punto de conexión marcado
//      oc := TOgConector.Create(motEdi.v2d);  //Crea objeto
//      oc.behav := behav1D;    //De tipo conector
//      motEdi.AddGraphObject(oc);  //Lo agrega al editor
//      //Ahora se conecta un nodo (Punto de control) al Pto. de Conexión
//      pCnx.ConnectTo(oc.pcBEGIN);
//      pCnx.Locate(pCnx.x, pCnx.y); //Actualiza el "enganche"
//      oc.Selec;          //Selecciona el conector
//      motEdi.v2d.XYvirt(X, Y, xv, yv);  //Obtiene coordenadas del mouse
//      //oc.pcEND.Locate(xv+50, yv+50);   //Posiciona Punto final del conector
//      oc.pcEND.OnChangePosition(oc.pcEND, 0, 0, xv+20, yv+20);
//      //oc.ReSize(oc.Width, oc.Height);
//      motEdi.CapturoEvento := oc;   //Indica al motor de edición que el conector se está dimensionando
//      motEdi.EstPuntero := EP_DIMEN_OBJ;  //Pone editor en modo "Dimensionando"
//      motEdi.Refrescar;            //Actualiza pantalla
//    end;
//  end;
end;
procedure TfraPICDiagram.motEdi_MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LogInp: TOgLogicState;
begin
  if motEdi.seleccion.Count = 1 then begin
    //Hay un componente seleccionado
    if motEdi.Selected.LoSelecciona(X,Y) then begin
      if motEdi.Selected is TOgLogicState then begin
        LogInp := TOgLogicState(motEdi.Selected);
        LogInp.pin.vThev := 0;
      end;
    end;
  end;
end;
procedure TfraPICDiagram.motEdi_MouseUpRight(Shift: TShiftState; x, y: integer);
  procedure VisibActionsAdd(State: boolean);
  begin
    acAddLogTog.Visible := State;
    acAddLed.Visible    := State;
    acAddConn.Visible   := State;
    acAddResis.Visible  := State;
    acAdd7SegComC.Visible := State;
  end;
var
  og: TObjGraf;
  it, it2: TMenuItem;
  pin2, pin1: TPinGraph;
  comp1, comp2: TOgComponent;
  pCnx, pCnx2: TPtoConx;
begin
  //Verifica el estado para activar acciones
  acGenDelObject.Visible := motEdi.seleccion.Count>0;
  if motEdi.seleccion.Count = 0 then begin
    //Ninguno seleccionado
    mnReset.Visible   := true;
    mnRun.Visible     := true;
    mnStepOver.Visible:= false;
    //mnAddLogicTog.Visible := true;
    VisibActionsAdd(true);
  end else if (motEdi.seleccion.Count = 1) and (motEdi.Selected is TOgComponent) then begin
    //Hay un componente seleccionado
    comp1 := TOgComponent(motEdi.Selected);  //Componente fuente
    mnReset.Visible   := true;
    mnRun.Visible     := true;
    mnStepOver.Visible:= true;
    //mnAddLogicTog.Visible := false;
    VisibActionsAdd(false);
  end else begin
    //Se ha seleccionado otra cosa o hay varios seleccionados
    mnReset.Visible   := false;
    mnRun.Visible     := false;
    mnStepOver.Visible:= false;
    //mnAddLogicTog.Visible := false;
    VisibActionsAdd(false);
  end;
  //Verifica la funcionalidad del menú de "Conectar a"
  //Verifica si se está marcado un punto de Conexión
  pCnx := motEdi.ConnectionPointMarked;
  if pCnx = nil then begin
    mnConnect.Visible := false;
  end else begin
    mnConnect.Visible := true;
    //mnAddLogicTog.Visible := false;  //Para que no confunda
    VisibActionsAdd(false);
    //Ubica componente de origen
    if not(pCnx.Parent is TOgComponent) then exit;
    comp1 := TOgComponent(pCnx.Parent);
    pin1 := TPinGraph(pCnx); //El Pto. de Conex. debe ser un pin
    if (comp1 = nil) or (pin1=nil) then exit;  //Protección
    mnConnect.Caption := Format('Connect %s to', [pin1.lbl]);
    //Actualiza menú de Conexión, con objetos gráficos
    mnConnect.Clear;
    for og in motEdi.objetos do begin
      if not(og is TOgComponent) then continue;
      if og is TOgConector then continue;;
      it := AddItemToMenu(mnConnect, og.Name, nil);
      comp2 := TOgComponent(og);
      for pCnx2 in comp2.PtosConex do begin
        pin2 := TPinGraph(pCnx2);
        if pin2.lbl = 'NC' then continue;  //No conectado
        if pin2 = nil then continue;
        it2 := AddItemToMenu(it, pin2.lbl, @ConnectAction);
        it2.Hint := comp1.Ref + '.' + IntToStr(pin1.nPin)+'-'+
                    comp2.Ref + '.' + IntToStr(pin2.nPin);
      end;
    end;
  end;
  //Muestra
  PopupMenu1.PopUp;
end;
constructor TfraPICDiagram.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //crea motor de edición
  motEdi := TModEdicion.Create(PaintBox1);
  nodeList := TNodeList.Create(true);
  //agrega objeto
  ogPic := TOgPic.Create(motEdi.v2d);
  ogPic.Ref := 'CI1';
  ogPic.Highlight := false;

  motEdi.AddGraphObject(ogPic);
  OnKeyDown := @fraPICDiagramKeyDown;
  motEdi.OnMouseDownLeft := @motEdi_MouseDown;
  motEdi.OnMouseUp       := @motEdi_MouseUp;
  motEdi.OnMouseUpRight  := @motEdi_MouseUpRight;
end;
destructor TfraPICDiagram.Destroy;
begin
  nodeList.Destroy;
  {Marca "nodeList" en NIL porque cuando se destruye "motEdi", si es que hay connectores,
  unidos a algún componente, estos intentarán llamar a "UpdateNodeList", cuando se
  desconecten, al momento de destruirse, y "UpdateNodeList" intentaría acceder a "nodeList"}
  nodeList := nil;
  motEdi.Destroy;
  inherited Destroy;
end;
procedure TfraPICDiagram.connectorChange;
begin
  if nodeList = nil then exit;  //Protección
  UpdateNodeList;
end;
/////////////////////// Acciones /////////////////////////
procedure TfraPICDiagram.acGenConnToExecute(Sender: TObject);
{Connecta el Pin de un objeto a otro.
No se implementa aquí porque se implementa de forma dinámica.}
begin
end;
procedure TfraPICDiagram.acAddLogTogExecute(Sender: TObject);
{Agrega un Objeto Gráfico LogicToggle}
var
  logTog: TOgLogicState;
begin
  logTog := TOgLogicState.Create(motEdi.v2d);
  logTog.Highlight := false;
  logTog.Name := UniqueName('Logic');
  logTog.Ref := UniqueRef('LG');  //Genera nombe único
  motEdi.AddGraphObject(logTog);
  logTog.Selec;
  Refrescar;
end;
procedure TfraPICDiagram.acAddResisExecute(Sender: TObject);
{Agrega un Objeto Gráfico Resistencia}
var
  res: TOgResisten;
begin
  res := TOgResisten.Create(motEdi.v2d);
  res.Highlight := false;
  res.Name := UniqueName('R');
  res.Ref := UniqueRef('R');  //Genera nombe único
  motEdi.AddGraphObject(res);
  res.Selec;
  Refrescar;
end;
procedure TfraPICDiagram.acAddLedExecute(Sender: TObject);
{Agrega un Objeto Gráfico Led}
var
  led: TOgLedRed;
begin
  led := TOgLedRed.Create(motEdi.v2d);
  led.Highlight := false;
  led.Name := UniqueName('Led');
  led.Ref := UniqueRef('D');  //Genera nombe único
  motEdi.AddGraphObject(led);
  led.Selec;
  Refrescar;
end;
procedure TfraPICDiagram.acAddConnExecute(Sender: TObject);
var
  conn: TOgConector;
begin
  conn :=  TOgConector.Create(motEdi.v2d);
  conn.behav := behav1D;    //De tipo conector
  //conn.Highlight := false;
  conn.Name := UniqueName('Connector');
  conn.Ref := UniqueRef('CN');  //Genera nombe único
  conn.OnConnect := @connectorChange;
  conn.OnDisconnect := @connectorChange;
  conn.PaintBox := PaintBox1;  //Necesita esta referencia
  motEdi.AddGraphObject(conn);
  conn.Selec;
  UpdateNodeList;
  Refrescar;
end;
procedure TfraPICDiagram.acAdd7SegComCExecute(Sender: TObject);
{Agrega un Objeto "Display" de 7 segmentos}
var
  led: TOg7Segment;
begin
  led := TOg7Segment.Create(motEdi.v2d);
  led.Highlight := false;
  led.Name := UniqueName('V7S');
  led.Ref := UniqueRef('V7S');  //Genera nombe único
  motEdi.AddGraphObject(led);
  led.Selec;
  Refrescar;
end;
procedure TfraPICDiagram.acGenDelObjectExecute(Sender: TObject);
{Elimina un Objeto Gráfico.}
begin
  if ogPic.Selected then begin
    MsgExc('Cannot delete PIC device.');
    ogPic.Deselec;
  end;
  //Elimina elementos seleccionados
  motEdi.DeleteSelected;
  UpdateNodeList;
end;
procedure TfraPICDiagram.acGenReconnExecute(Sender: TObject);
{Reconecta componentes del diagrama, de acuerdo a las coordenadas de los conectores.}
var
  og: TObjGraf;
  ogCon: TOgConector;
  xp, yp: Integer;
  selPntCnx: TPtoConx;
begin
  nodeList.Clear;
  //Explora objetos gráfiocs
  for og in motEdi.objetos do begin
    if og is TOgConector then begin
      ogCon := TOgConector(og);
      if ogCon.pcBEGIN.ConnectedTo = nil then begin
        motEdi.v2d.XYpant(ogCon.pcBEGIN.x, ogCon.pcBEGIN.y, xp, yp);
        selPntCnx := motEdi.SelectPointOfConexion(xp, yp, 2);
        if selPntCnx <> nil then selPntCnx.ConnectTo(ogCon.pcBEGIN);
      end;
      if ogCon.pcEND.ConnectedTo = nil then begin
        motEdi.v2d.XYpant(ogCon.pcEND.x, ogCon.pcEND.y, xp, yp);
        selPntCnx := motEdi.SelectPointOfConexion(xp, yp, 2);
        if selPntCnx <> nil then selPntCnx.ConnectTo(ogCon.pcEND);
      end;
    end;
  end;
  UpdateNodeList;
end;

end.

