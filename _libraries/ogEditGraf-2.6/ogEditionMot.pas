{Unidad ogMotEdicion
====================
Por Tito Hinostroza 24/09/2014

Descripción
============
Define la clase TModEdicion para la implementación de una interfaz de objetos gráficos
en un Editor.
Los objetos a manejar deben derivarse de la clase TObjGraf.
Se debe indicar el control TPaint que se usará como salida gráfica.
Trabaja en pixels para acelerar la velocidad de gráficos.
Basado en la clase equivalente en el proyecto SQLGraf en Visual Basic.
}
unit ogEditionMot;
{$mode objfpc}{$H+}
INTERFACE
uses
  Classes, Forms, Controls, ExtCtrls, SysUtils, Graphics, Fgl, LCLIntf,
  LCLType, GraphType, Dialogs, LCLProc, ogMotGraf2D, ogDefObjGraf;

const
  CUR_DEFEC = crDefault;   //Cursor por defecto

  ZOOM_MAX_CONSULT = 5  ;  //Define el zoom máximo que se permite en un diagrama
  ZOOM_MIN_CONSULT = 0.1;  //Define el zoom mínimo que se permite en un diagrama

  ZOOM_INC_FACTOR = 1.15;  //Factor de ampliación del zoom
  DESPLAZ_MENOR   = 10;
  ACCURACY_SNAP   = 5;     //Precisión del encaje
type
  TPointerState = (
      EP_NORMAL,      //No se está realizando ninguna operación
      EP_SELECMULT,   //Esta en modo de selección múltiple
      EP_MOV_OBJS,    //Indica que se esta moviendo una o mas objetos
      EP_DESP_PANT,   //Indica desplazamiento con ratón + <Shift> + <Ctrl>
      EP_DIMEN_OBJ,   //Indica que se está dimensionando un objeto
      EP_RAT_ZOOM);   //Indica que se está en un proceso de Zoom

  TlistObjGraf = specialize TFPGObjectList<TObjGraf>;   //Lista de "TObjTabla"

  TEvMouse = procedure(Shift: TShiftState; x,y:integer) of object;
  TOnObjetosElim = procedure of object;

  { TEditionMot }
  TEditionMot = class
  protected
    procedure MouseDownRight(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
                        xp, yp: Integer); virtual;
    procedure MouseDownLeft(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
                        xp, yp: Integer); virtual;
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
                        xp, yp: Integer); virtual;
    procedure VerifyForMove(xp, yp: Integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; xp,  yp: Integer); virtual;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; xp, yp: Integer);
    procedure Paint(Sender: TObject);
    procedure PBMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public  //Eventos
    OnObjetosElim : TOnObjetosElim;   //Cuando se elminan uno o más objetos
    OnMouseUp     : TMouseEvent;      //Cuando se suelta el botón
    OnMouseUpRight: TEvMouse;
    OnMouseUpLeft : TEvMouse;
    OnMouseDown   : TMouseEvent;
    OnMouseDownRight: TMouseEvent;
    OnMouseDownLeft : TMouseEvent;
    OnMouseMove   : TMouseMoveEvent;
    OnDblClick    : TNotifyEvent;
    OnObjectsMoved: procedure of object;
  public
    EstPuntero   : TPointerState; //Estado del puntero
    ParaMover    : Boolean;       //Bandera de control para el inicio del movimiento
    CapturoEvento: TObjGraf;      //Referencia a objeto que capturo el movimiento
    objetos      : TlistObjGraf;
    seleccion    : TlistObjGraf;
    curPntCtl    : TPtoCtrl;      //Punto de control actual en movimiento
    Modif    : Boolean;    //Bandera para indicar Diagrama Modificado
    PBox     : TPaintBox;  //Control de Salida
    v2d      : TMotGraf;   //salida gráfica
    procedure AddGraphObject(og: TObjGraf; AutoPos: boolean=true);
    procedure DeleteAll;
    procedure DeleteSelected;
    procedure DeleteGraphObject(obj: TObjGraf);
    procedure PBDblClick(Sender: TObject);
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function ObjPorNombre(nom: string): TObjGraf;
    procedure Refresh;
  protected
    //Coordenadas del raton
    x_pulso: integer;
    y_pulso: integer;
    //Perspectivas
    PFinal : TPerspectiva;  //Almacena la perspectiva a la que se quiere llegar
    x_cam_a: Single;  //Coordenadas anteriores de x_cam
    y_cam_a: Single;
    procedure InicMover;
    function MarkConnectionPoint(xp, yp: Integer): TPtoConx;
  protected  // Funciones para administrar los elementos visibles y seleccion por teclado
    function NumberOfVisible: Integer;
    function FirstVisible: TObjGraf;
    function LastVisible: TObjGraf;
    function SiguienteVisible(c: TObjGraf): TObjGraf;
    function AnteriorVisible(c: TObjGraf): TObjGraf;
    procedure SeleccionarSiguiente;
    procedure SeleccionarAnterior;
  public // Funciones de visualización
    procedure AmpliarClick(factor: single=ZOOM_INC_FACTOR; xrZoom: integer=0;
      yrZoom: integer=0);
    procedure ReducirClick(factor: single=ZOOM_INC_FACTOR; xrZoom: integer=0;
      yrZoom: integer=0);
  public //Funciones de selección
    procedure SelectAll;
    procedure UnselectAll;
    function Selected: TObjGraf;
    function ConnectionPointMarked: TPtoConx;
    function SelectSomeObject(xp, yp: Integer): TObjGraf;
    function SelectPointOfConexion(xp, yp: Integer; snap_adj: integer =
      ACCURACY_SNAP): TPtoConx;
  protected  //Resaltado de Objetos
    lastMarked   : TObjGraf;      //Nombre del objeto marcado
    lastCnxPnt   : TPtoConx;
    function VerifyMouseMove(X, Y: Integer): TObjGraf;
  protected  //Desplazamiento de pantalla
    procedure Desplazar(dx, dy: integer);
    procedure ScrollDown(desp: Double=DESPLAZ_MENOR);
    procedure ScrollUp(desp: Double=DESPLAZ_MENOR);
    procedure ScrollRight(desp: Double=DESPLAZ_MENOR);
    procedure ScrollLeft(desp: Double=DESPLAZ_MENOR);
    procedure ScrollDesp(dx, dy: integer);
  protected  //Funciones del Rectángulo de Selección
    x1Sel    : integer;
    y1Sel    : integer;
    x2Sel    : integer;
    y2Sel    : integer;
    x1Sel_a  : integer;
    y1Sel_a  : integer;
    x2Sel_a  : integer;
    y2Sel_a  : integer;
    procedure DibujRecSeleccion;
    procedure InicRecSeleccion(X, Y: Integer);
    function RecSeleccionNulo: Boolean;
    function enRecSeleccion(X, Y: Single): Boolean;
  public // Eventos para atender requerimientos de objetos "TObjGraf"
    procedure ObjGraf_Select(obj: TObjGraf);     //Respuesta a Evento
    procedure ObjGraf_Unselec(obj: TObjGraf);    //Respuesta a Evento
    procedure ObjGraf_SetPointer(Punt: integer); //Respuesta a Evento
  public  //Inicialización
    constructor Create(PB0: TPaintBox);
    destructor Destroy; override;
  end;

implementation

procedure TEditionMot.MouseDownRight(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; xp, yp: Integer);
{Se ejecuta al pulsar el botón derecho del Mouse. No se incluye el caso para
 cuando se pulsa con <Shift>+<Conrol>.}
var
  ogs: TObjGraf;
begin
  ogs := SelectSomeObject(xp, yp);  //verifica si selecciona a un objeto
  if ogs = nil Then begin  //Ninguno Selected
      UnselectAll;
      Refresh;
      EstPuntero := EP_SELECMULT;  //inicia seleccion multiple
      InicRecSeleccion(x_pulso, y_pulso);
  end else begin //Selecciona a uno, pueden haber otros seleccionados
      if ogs.Selected Then  begin  //Se marcó sobre un Selected
//          if Shift = [] Then UnselectAll;
          ogs.MouseDown(Sender, Button, Shift, xp, yp);  //Pasa el evento
          exit;
      end;
      //Se selecciona a uno que no tenía selección
      if Shift = [ssRight] Then  //Sin Control ni Shift
        UnselectAll;
      ogs.MouseDown(Sender, Button, Shift, xp, yp);  //Pasa el evento
      Refresh;
       //ParaMover = True       ;  //listo para mover
  end;
end;
procedure TEditionMot.MouseDownLeft(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; xp, yp: Integer);
{Se ejecuta al pulsar el botón izquierdo del Mouse. No se incluye el caso para
 cuando se pulsa con <Shift>+<Conrol>.}
var
  ogs: TObjGraf;
begin
  ogs := SelectSomeObject(xp, yp);  //verifica si selecciona a un objeto
  if ogs = nil then  begin  //No selecciona a ninguno
      UnselectAll;
      Refresh;
      EstPuntero := EP_SELECMULT;  //inicia seleccion multiple
      InicRecSeleccion(x_pulso, y_pulso);
  end else begin     //selecciona a uno, pueden haber otros seleccionados
      if ogs.Selected Then begin //Se marcó sobre un Selected
          //No se quita la selección porque puede que se quiera mover
          //varios objetos seleccionados. Si no se mueve, se quitará la
          //selección en MouseUp
          //If Shift = 0 Then Call UnselectAll
          ogs.MouseDown(Sender, Button, Shift, xp, yp);  //Pasa el evento
          ParaMover := True;  //listo para mover
          exit;               //Se sale sin desmarcar
      end;
      //Se selecciona a uno que no tenía selección
      if Shift = [ssLeft] then  //Sin Control ni Shift
         UnselectAll;
      ogs.MouseDown(Sender, Button, Shift, xp, yp);  //Pasa el evento
      ParaMover := True;            //Listo para mover
  end;
end;
procedure TEditionMot.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
begin
//    if OnMouseDown<>nil then OnMouseDown(Sender, Button, Shift, Xp, Yp);
    x_pulso := xp;
    y_pulso := yp;
    InicMover;   //por si acaso, para iniciar movimiento
    if Shift >= [ssCtrl, ssShift]  Then begin   //Contiene <Shift>+<Ctrl>
        //Inicia estado de ZOOM. Puede convertirse en EP_DESP_PANT
        //si luego se genera el evento Move()
        EstPuntero := EP_RAT_ZOOM;
    end else begin
        //Caso sin <Shift>+<Ctrl>
        if Button = mbRight then begin
          MouseDownRight(Sender, Button, Shift, xp, yp);
          if OnMouseDownRight<>nil then OnMouseDownRight(Sender, Button, Shift, xp, yp);
        end;
        if Button = mbLeft then begin
          MouseDownLeft(Sender, Button, Shift, xp, yp);
          if OnMouseDownLeft<>nil then OnMouseDownLeft(Sender, Button, Shift, xp, yp);
        end;
    end;
    if OnMouseDown<>nil then OnMouseDown(Sender, Button, Shift, xp, xp);
end;
procedure TEditionMot.Paint(Sender: TObject);
var
  o:TObjGraf;
begin
//  If s = NIL Then
    PBox.canvas.Brush.Color := clWhite; //rgb(255,255,255);
    PBox.canvas.FillRect(PBox.ClientRect); //fondo
    If EstPuntero = EP_SELECMULT Then DibujRecSeleccion;
    //Dibuja objetos
    for o In objetos do begin
      o.Draw;
    end;
end;
procedure TEditionMot.PBMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  ogs: TObjGraf;
begin
  ogs := SelectSomeObject(MousePos.x, MousePos.y);  //verifica si selecciona a un objeto
  if ogs=nil then begin
    //debe desplazar la pantalla
  end else begin
    //lo selecciona, pero debe ver si está Selected
    if ogs.Selected then begin
       ogs.MouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
    end else begin

    end;
  end;
end;
procedure TEditionMot.Refresh;  //   Optional s: TObjGraf = Nothing
begin
  PBox.Invalidate;
end;
procedure TEditionMot.InicMover;
//Procedimiento que inicia un desplazamiento de la pantalla. Se debe llamar cada vez que se puede
//iniciar el proceso de desplazamiento
begin
    x_cam_a := v2d.x_cam;
    y_cam_a := v2d.y_cam;
end;
function TEditionMot.SelectSomeObject(xp, yp: Integer): TObjGraf;
//Rutina principal para determinar la selección de objetos. Si (xp,yp)
//selecciona a algún objeto, devuelve la referencia, sino devuelve "NIL"
var
  i: Integer;
  s: TObjGraf;
begin
  //Verifica primero entre los que están seleccionados
  Result := NIL; //valor por defecto
  //Explora objetos priorizando los que están encima
  for i := seleccion.Count-1 downTo 0 do begin
    s := seleccion[i];
    If not s.SelLocked and s.IsSelectedBy(xp, yp) Then begin
        Result:= s;
        Exit;
    End;
  end;
  //Explora objetos priorizando los que están encima
  for i := objetos.Count-1 downTo 0 do begin
    s := objetos[i];
    If not s.SelLocked and s.IsSelectedBy(xp, yp) Then begin
        Result := s;
        Exit;
    End;
  end;
End;
function TEditionMot.SelectPointOfConexion(xp, yp: Integer;
                     snap_adj: integer = ACCURACY_SNAP): TPtoConx;
{Indica si la coordenada indicada seleciona (está cerca) a algún punto de conexión
de cualquier objeto gráfico.
Si encuentra algún Punto de Conexión cerca, devuelve la referencia.}
var
  og: TObjGraf;
  pcnx: TPtoConx;
begin
  Result := nil;
  for og In objetos do begin
    pcnx := og.SelectConnectionPoint(xp, yp, snap_adj);
    if pcnx<>nil then exit(pcnx);
  end;
end;
function TEditionMot.MarkConnectionPoint(xp, yp: Integer): TPtoConx;
{Mark the Connection point selected by (X,Y) if some is selected, in wich case
return the Connection point selected, otherwise returns NIL.}
var
  og: TObjGraf;
  pCnx: TPtoConx;
begin
  Result := nil;
  for og In objetos do begin
    pCnx := og.MarkConnectionPoint(xp, yp, 5);
    if pCnx<>nil then begin
      Result := pCnx;
    end;
  end;
end;
function TEditionMot.ConnectionPointMarked: TPtoConx;
{Return de Connection point marked is some exists, otherwise returms NIL}
var
  og: TObjGraf;
  pCnx: TPtoConx;
begin
  for og In objetos do begin
    pCnx := og.ConnectionPointMarked;
    if pCnx<>nil then exit(pCnx);
  end;
  exit(nil);
end;

procedure TEditionMot.VerifyForMove(xp, yp: Integer);
{Si se empieza el movimiento, selecciona primero algun elemento que
pudiera estar debajo del puntero y actualiza "EstPuntero".
Solo se debe ejecutar una vez al inicio del movimiento, para ello se
usa la bandera ParaMover, que debe ponerse a FALSE aquí.}
var
  s: TObjGraf;
begin
    for s In seleccion  do begin  //da prioridad a los elementos seleccionados
      if s.PosLocked then continue;
      s.StartMove(xp, yp);      //llama al evento inic_mover para cada objeto
      if s.Proceso Then begin  //este objeto proceso el evento
          CapturoEvento := s;
          if s.Resizing then begin
            EstPuntero := EP_DIMEN_OBJ;
            curPntCtl := s.curPntCtl;
          end else begin
            EstPuntero := EP_NORMAL;
          end;
          ParaMover := False;    //para que ya no se llame otra vez
          Exit;
      end;
    end;
    for s In objetos do begin
      if s.PosLocked then continue;
      s.StartMove(xp, yp);    //llama al evento inic_mover para cada objeto
      if s.Proceso Then begin   //este objeto proceso el evento
          CapturoEvento := s;
          if s.Resizing then EstPuntero := EP_DIMEN_OBJ else EstPuntero := EP_NORMAL;
          EstPuntero := EP_NORMAL;
          ParaMover := False;   //para que ya no se llame otra vez
          exit;
      end;
    end;
    //Ningún objeto ha capturado, el evento, asumimos que se debe realizar
    //el desplazamiento simple de los objetos seleccionados
//Debug.Print "   VerifParaMover: EP_MOV_OBJS"
    EstPuntero := EP_MOV_OBJS;
    CapturoEvento := nil;      //ningún objeto capturo el evento
    ParaMover := False;        //para que ya no se llame otra vez
end;
procedure TEditionMot.MouseMove(Sender: TObject; Shift: TShiftState;
  xp,  yp: Integer);
var
  s: TObjGraf;
  selPntCnx: TPtoConx;
begin
  if OnMouseMove<>nil then OnMouseMove(Sender, Shift, xp, yp);
  If Shift = [ssCtrl, ssShift, ssRight] Then  //<Shift>+<Ctrl> + <Botón derecho>
     begin
      EstPuntero := EP_DESP_PANT;
      ScrollDesp(x_pulso - xp, y_pulso - yp);
      Refresh;
      Exit;
     End;
  If ParaMover = True Then VerifyForMove(xp, yp);
  If EstPuntero = EP_SELECMULT then begin  //modo seleccionando multiples formas
      x2Sel := xp;
      y2Sel := yp;
      //verifica los que se encuentran seleccionados
      if objetos.Count < 100 Then begin//sólo anima para pocos objetos
          for s In objetos do begin
            if s.SelLocked then continue;
            if enRecSeleccion(s.XCent, s.YCent) And Not s.Selected Then begin
              s.Selec;
            End;
            if Not enRecSeleccion(s.XCent, s.YCent) And s.Selected Then begin
              s.Deselec;
            end;
          end;
      End;
      Refresh
  end Else If EstPuntero = EP_MOV_OBJS then begin  //mueve la selección
      Modif := True;
      for s in seleccion do
          s.MouseMove(xp,yp, seleccion.Count);
      Refresh;
  end Else If EstPuntero = EP_DIMEN_OBJ then begin
      selPntCnx := SelectPointOfConexion(xp, yp);
      if selPntCnx <> nil then begin
         //Engancha la coordenada de pantalla al punto de control
         v2d.XYpant(selPntCnx.x, selPntCnx.y, xp, yp);
      end;
      //Se está dimensionando un objeto, moviendo un punto de control
      CapturoEvento.MouseMove(xp, yp, seleccion.Count);
      Refresh;
  end else begin
      if CapturoEvento <> NIL then begin
         CapturoEvento.MouseMove(xp, yp, seleccion.Count);
         Refresh;
      end else begin  //Movimiento simple
          s := VerifyMouseMove(xp, yp);
          if s <> NIL then s.MouseOver(Sender, Shift, xp, yp);  //pasa el evento
      end;
  end;
end;
procedure TEditionMot.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; xp, yp: Integer);
var o: TObjGraf;
  selPntCnx: TPtoConx;
begin
    //Verifica si la selección es NULA
    If (EstPuntero = EP_SELECMULT) And RecSeleccionNulo Then EstPuntero := EP_NORMAL;
    //Procesa de acuerdo al estado
    Case EstPuntero of
    EP_RAT_ZOOM  : begin   //------ Zoom con el Ratón ------
        If Button = mbLeft Then AmpliarClick(1.2, xp, yp) ;  //<Shift> + <Ctrl> + click izquierdo
        If Button = mbRight Then ReducirClick(1.2, xp, yp) ;  //<Shift> + <Ctrl> + click derecho
//      EstPuntero = EP_NORMAL   //Legalmente debería ponerse a normal. Pero si se
                                 //hace, es posible que un click consecutivo muy
                                 //rápido, no dispare el evento MouseDown (dispara
                                 //el DblClick en su lugar), y se desactivaría el
                                 //modo ZOOM lo que es molesto.
      end;
    EP_DESP_PANT : begin    //------ Desplazamiento de Pantalla ------
        EstPuntero := EP_NORMAL;
      end;
    EP_MOV_OBJS  : begin    //------ Moviendo Objetos ------
//Debug.Print "Esatado EP_MOV_OBJS"
        For o In seleccion do  //Pasa el evento a la selección
            o.MouseUp(Sender, Button, Shift, xp, yp, EstPuntero = EP_MOV_OBJS);
        EstPuntero := EP_NORMAL;  //fin de movimiento
        Refresh;
        //Genera eventos. Los objetos movidos se pueden determinar a partir de la selección.
        if OnObjectsMoved<>nil then OnObjectsMoved;
      end;
    EP_SELECMULT : begin //------ En selección múltiple, Botón izquierdo o derecho
        if objetos.Count > 100 Then begin  //Necesita actualizar porque la selección múltiple es diferente
          for o in objetos do
            if enRecSeleccion(o.XCent, o.YCent) And Not o.Selected Then o.Selec;
        end;
        EstPuntero := EP_NORMAL;
      end;
    EP_NORMAL    : begin //------ En modo normal
        o := SelectSomeObject(xp, yp);  //verifica si selecciona a un objeto
        If Button = mbRight Then //----- solto derecho -------------------
          begin
(*            If o = NIL Then  //Ninguno Selected
                RaiseEvent ClickDerDiag    //Genera evento
            Else    ;  //Hay uno que lo selecciona, o más???
                If Not o.Seleccionado Then Call o.SoltoRaton(Button, Shift, xr, yr)    ;  //Pasa el evento
                RaiseEvent ClickDerSel     //Genera evento
            End If*)
          end
        else If Button = mbLeft Then begin //----- solto izquierdo -----------
            If o = NIL Then    //No selecciona a ninguno
//                Call UnselectAll
            else begin         //Selecciona a alguno
                If Shift = [] Then UnselectAll;
                o.Selec;   //selecciona
                o.MouseUp(Sender, Button, Shift, xp, yp, false);
                Refresh;
                //verifica si el objeto está piddiendo que lo eliminen
                if o.Erased then begin
                  DeleteGraphObject(o);
                  Refresh;
                end;
            End;
            CapturoEvento := NIL;      //inicia bandera de captura de evento
            ParaMover := False;        //por si aca
        end;
      end;
    EP_DIMEN_OBJ : begin  //Se soltó mientras se estaba dimensionado un objeto
        //Pasa evento a objeto que se estaba dimensionando
        CapturoEvento.MouseUp(Sender, Button, Shift, xp, yp, false);
        //termina estado
        EstPuntero := EP_NORMAL;
        CapturoEvento := NIL;      //inicia bandera de captura de evento
        ParaMover := False;        //por si aca
        //Verifica el enganche de los puntos de conexión
        selPntCnx := SelectPointOfConexion(xp, yp);
        if selPntCnx <> nil then begin
           //Engancha la coordenada de pantalla al punto de control
          selPntCnx.ConnectTo(curPntCtl);
        end;
      end;
    End;
    if OnMouseUp<>nil then OnMouseUp(Sender, Button, Shift, xp, yp);
    if Button = mbRight then
      if OnMouseUpRight<> nil then OnMouseUpRight(Shift, xp,yp);  //evento
    if Button = mbLeft then
      if OnMouseUpLeft<> nil then OnMouseUpLeft(Shift, xp,yp);  //evento
End;
procedure TEditionMot.KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//Procesa el evento KeyDown()
//var
//  og: TObjGraf;
begin
  If Shift = [] Then begin  //********************* Teclas normales ***********************
      //If tec = 13 Then PropiedSeleccion ;  //Debe procesarlo el diagrama
      If Key = VK_DELETE Then DeleteSelected;  //DELETE
      If Key = 9 Then begin
          SeleccionarSiguiente;  //TAB
          Key := 0;   //Para que no se pase el enfoque a otro control
      end;
      If Key = 27 Then begin  //ESCAPE
          UnselectAll;
          Refresh;
      end;
      If seleccion.Count = 0 Then begin  //si no hay objetos seleccionados
          If Key = 37 Then ScrollRight(DESPLAZ_MENOR)  ;  //derecha
          If Key = 39 Then ScrollLeft(DESPLAZ_MENOR);  //izquierda
          If Key = 40 Then ScrollUp(DESPLAZ_MENOR)   ;  //arriba
          If Key = 38 Then ScrollDown(DESPLAZ_MENOR)    ;  //abajo
      end else begin  //hay seleccionados
//          If Key = 37 Then begin  //derecha
//              For og In seleccion do begin
//                  If Not og.PosLocked Then og.x := og.X - DESPLAZ_MENOR;
//              end;
//              Refresh;
//          end;
//          If Key = 39 Then begin  //izquierda
//              For og In seleccion do begin
//                  If Not og.PosLocked Then og.X := og.X + DESPLAZ_MENOR;
//              end;
//              Refresh;
//          end;
//          If Key = 40 Then begin  //arriba
//              For og In seleccion do begin
//                  If Not og.PosLocked Then og.Y := og.Y + DESPLAZ_MENOR;
//              end;
//              Refresh;
//          end;
//          If Key = 38 Then begin  //abajo
//              For og In seleccion do begin
//                  If Not og.PosLocked Then og.Y := og.Y - DESPLAZ_MENOR;
//              end;
//              Refresh;
//          end;
      end;
  end else If Shift = [ssShift] Then begin //**********************Shift + ************************
      If Key = 9 Then begin
          SeleccionarAnterior;  //TAB
          Key := 0;   //Para que no se pase el enfoque a otro control
      end;
//  end else If Shift = [ssCtrl] Then begin  //**********************Ctrl + ************************
//      If Key = 107 Then Call AmpliarClick      ;  //+
//      If Key = 109 Then Call ReducirClick      ;  //-
//      If Key = 37 Then Call ScrollRight(DESPLAZ_MAYOR)   ;  //derecha
//      If Key = 39 Then Call ScrollLeft(DESPLAZ_MAYOR) ;  //izquierda
//      If Key = 40 Then Call ScrollUp(DESPLAZ_MAYOR)    ;  //arriba
//      If Key = 38 Then Call ScrollDown(DESPLAZ_MAYOR)     ;  //abajo
//  end else If Shift = [ssShift, ssCtrl] Then  begin  //******************Shift + Ctrl*************************
//    picSal.MousePointer := vbSizeAll;  //indica modo Zoom + desplazamiento
  end;
end;
procedure TEditionMot.AddGraphObject(og: TObjGraf; AutoPos: boolean = true);
//Agrega un objeto grafico al editor. El objeto gráfico debe haberse creado previamente,
//y ser de tipo TObjGraf o un descendiente. "AutoPos", permite posicionar automáticamente
//al objeto en pantalla, de modo que se evite ponerlo siempre en la misma posición.
var
  x: single;
  y: single;
begin
  Modif := True;        //Marca el editor como modificado
  //Posiciona tratando de que siempre aparezca en pantalla
  if AutoPos Then begin  //Se calcula posición
    x := v2d.Xvirt(100, 100) + 30 * objetos.Count Mod 400;
    y := v2d.Yvirt(100, 100) + 30 * objetos.Count Mod 400;
    og.ReLocate(x,y);
  end;
  //configura eventos para ser controlado por este editor
  og.OnSelec   := @ObjGraf_Select;    //referencia a procedimiento de selección
  og.OnDeselec := @ObjGraf_Unselec;  //referencia a procedimiento de "de-selección"
  og.OnCamPunt := @ObjGraf_SetPointer;    //procedimiento para cambiar el puntero
//  Refresh(s)   ;                //Refresca objeto
  objetos.Add(og);                //agrega elemento
end;
procedure TEditionMot.DeleteGraphObject(obj: TObjGraf);  //elimina un objeto grafico
var
  pCnx: TPtoConx;
begin
  Modif := True;  //Marca documento como modificado
  obj.Deselec;    //por si acaso
  //Quita posibles conexiones a este objeto
  for pCnx in obj.PtosConex do begin
    pCnx.Disconnect;
  end;
  //Elimina de la lista
  objetos.Remove(obj);
  obj := nil;
  if OnObjetosElim<>nil then OnObjetosElim;
End;
procedure TEditionMot.PBDblClick(Sender: TObject);
begin
  if OnDblClick<>nil then OnDblClick(Sender);
end;
procedure TEditionMot.DeleteAll;
//Elimina todos los objetos gráficos existentes
begin
  if objetos.Count=0 then exit;  //no hay qué eliminar
  //elimina
  UnselectAll;  //por si acaso hay algun simbolo Selected
  objetos.Clear;    //limpia la lista de objetos
  EstPuntero := EP_NORMAL;
  ParaMover := false;
  CapturoEvento := nil;
  lastMarked := nil;     //por si había alguno marcado
  Modif := true;    //indica que se modificó
//    DeleteGraphObject(o);
  PBox.Cursor := CUR_DEFEC;        //define cursor
  if OnObjetosElim<>nil then OnObjetosElim;
End;
procedure TEditionMot.DeleteSelected;
//Elimina la selección.
var
  og: TObjGraf;
  tmp: TOnObjetosElim;
begin
  tmp := OnObjetosElim;  //guarda evento
  OnObjetosElim := nil; //para evitar llamar muchas veces
  //For og In seleccion  do  //explora todos
  //  DeleteGraphObject(og);
  while seleccion.Count>0 do begin
    og := seleccion[0];
    DeleteGraphObject(og);
  end;
  Refresh;
  OnObjetosElim := tmp;  //restaura
  if OnObjetosElim<>nil then OnObjetosElim;  //llama evento
end;
function  TEditionMot.ObjPorNombre(nom: string): TObjGraf;
//Devuelve la referecnia a un objeto, dado el nombre. Si no encuentra, devuelve NIL.
var s: TObjGraf;
begin
  Result := nil;   //valor por defecto
  if nom = '' then exit;
  For s In objetos do
    if s.Name = nom then begin
       Result := s;
       break;
    end;
End;
{
//Respuesta al evento doble click
Public Sub DblClick()
Dim s: TObjGraf
    Set s = SeleccionaAlguno(x_pulso, y_pulso)
    If s = NIL Then    ;  //En diagrama
        Exit Sub
    Else                    ;  //En objeto
        RaiseEvent DblClickObj(s)
    End If
End Sub
}
// Funciones para administrar los elementos visibles y seleccion por teclado
function TEditionMot.NumberOfVisible: Integer;
//devuelve el número de objetos visibles
var
  v: TObjGraf;
  tmp: Integer;
begin
  tmp := 0;
  For v in objetos do begin
    if v.visible then Inc(tmp);
  end;
  Result := tmp;
end;
function TEditionMot.FirstVisible: TObjGraf;
 //devuelve el primer objeto visible
var
  i: integer;
begin
  for i:=0 to objetos.Count-1 do begin
    if objetos[i].visible then begin
      Result := objetos[i];
      exit;
    end;
  end;
End;
function TEditionMot.LastVisible: TObjGraf;
 //devuelve el último objeto visible
var
  i: Integer;
begin
  for i:=objetos.Count-1 downto 0 do begin
    if objetos[i].visible then begin
      Result := objetos[i];
      exit;
    end;
  end;
end;
function TEditionMot.SiguienteVisible(c: TObjGraf): TObjGraf;
//devuelve el siguiente objeto visible en el orden de creación
var
  i: Integer;
begin
    //busca su orden dentro de los objetos
    For i := 0 To objetos.Count-1 do begin
      if objetos[i] = c Then break;
    end;
    //calcula el siguiente elemento
    repeat
      Inc(i);
      If i >= objetos.Count Then begin  //se ha llegado al final del conjunto
        Result := FirstVisible;
        Exit;
      end;
    until objetos[i].visible;
    //selecciona el siguiente visible
    Result := objetos[i];
end;
function TEditionMot.AnteriorVisible(c: TObjGraf): TObjGraf;
//devuelve el anterior objeto visible en el orden de creación
var
  i: Integer;
begin
    //busca su orden dentro de los objetos
    For i := 0 To objetos.Count-1 do begin
      If objetos[i] = c Then break;
    end;
    //calcula el elemento anterior
    repeat
      Dec(i);
      If i < 0 Then begin  //se ha llegado al inicio
        Result := LastVisible;
        Exit;
      End;
    until objetos[i].visible;
    //selecciona el siguiente visible
    Result := objetos[i];
End;
procedure TEditionMot.SeleccionarSiguiente;
//Selecciona el siguiente elemento visible en el orden de creación.
//Si no hay ninguno seleccionado, selecciona el primero
var
  s: TObjGraf;
begin
    if NumberOfVisible() = 0 Then exit;
    if seleccion.Count = 1 Then begin  //hay uno Selected
        s := seleccion[0];   //toma el Selected
        s := SiguienteVisible(s);
        UnselectAll;
        s.Selec;
    end else begin     //hay cero o más de uno Selected
        s := FirstVisible;  //selecciona el primero
        UnselectAll;
        s.Selec;
    end;
    Refresh;
end;
procedure TEditionMot.SeleccionarAnterior;
//Selecciona el anterior elemento visible en el orden de creación.
//Si no hay ninguno seleccionado, selecciona el ultimo
var
  s: TObjGraf;
begin
    if NumberOfVisible() = 0 Then exit;
    if seleccion.Count = 1 then begin     //hay uno Selected
        s := seleccion[0];    //toma el Selected
        s := AnteriorVisible(s);
        UnselectAll;
        s.Selec;
    end else begin               //hay cero o más de uno Selected
        s := LastVisible;   //selecciona el ultimo
        UnselectAll;
        s.Selec;
    end;
    Refresh;
end;
// Funciones de visualización
procedure TEditionMot.AmpliarClick(factor: single = ZOOM_INC_FACTOR;
                        xrZoom: integer = 0; yrZoom: integer = 0);
var
  xv0, yv0, xv1, yv1: Single;
begin
  //Calcula coordenadas virtuales del punto donde se pide ampliar
  v2d.XYvirt(xrZoom, yrZoom, xv0, yv0);     //convierte
  if v2d.zoom < ZOOM_MAX_CONSULT then
      v2d.zoom := v2d.zoom * factor;
  if (xrZoom <> 0) Or (yrZoom <> 0) then begin  //se ha especificado una coordenada central
      //Calcula coordenadas virtuales del mismo (xrZoom, yrZoom) punto después de la ampliaicón
      v2d.XYvirt(xrZoom, yrZoom, xv1, yv1);
      //Corrige posición para que se mantenga la misma coordenada virtual
      v2d.x_cam -= (xv1-xv0);
      v2d.y_cam -= (yv1-yv0);
  end;
  v2d.SavePerspectiveIn(Pfinal);  //para que no se regrese al ángulo inicial
  Refresh;
End;
procedure TEditionMot.ReducirClick(factor: single = ZOOM_INC_FACTOR;
                        xrZoom: integer = 0; yrZoom: integer = 0);
var
  xv1, yv1, xv0, yv0: Single;
begin
  //Calcula coordenadas virtuales del punto donde se pide reducir
  v2d.XYvirt(xrZoom, yrZoom, xv0, yv0);     //convierte
  if v2d.zoom > ZOOM_MIN_CONSULT then
      v2d.zoom := v2d.zoom / factor;
  if (xrZoom <> 0) Or (yrZoom <> 0) then begin  //se ha especificado una coordenada central
      //Calcula coordenadas virtuales del mismo (xrZoom, yrZoom) punto después de la ampliaicón
      v2d.XYvirt(xrZoom, yrZoom, xv1, yv1);
      //Corrige posición para que se mantenga la misma coordenada virtual
      v2d.x_cam -= (xv1-xv0);
      v2d.y_cam -= (yv1-yv0);
  end;
  v2d.SavePerspectiveIn(Pfinal)  ;  //para que no se regrese al ángulo inicial
  Refresh;
End;
// Funciones de selección
procedure TEditionMot.SelectAll;
var s: TObjGraf;
begin
    For s In objetos do s.Selec; //selecciona todos
End;
procedure TEditionMot.UnselectAll;
var s: TObjGraf;
begin
  For s In objetos do //no se explora "seleccion" porque se modifica con "s.Deselec"
    if s.Selected then s.Deselec;
//  seleccion.Clear; //No se puede limpiar simplemente la lista. Se debe llamar a s.Deselec
End;
function  TEditionMot.Selected: TObjGraf;
//Devuelve el objeto seleccionado. Si no hay ninguno seleccionado, devuelve NIL.
begin
  Result := nil;   //valor por defecto
  if seleccion.Count = 0 then exit;  //no hay
  //hay al menos uno
  Result := seleccion[seleccion.Count-1];  //devuelve el único o último
End;
// Resaltado de Objetos
function TEditionMot.VerifyMouseMove(X, Y: Integer): TObjGraf;
//Anima la marcación de los objetos cuando el ratón pasa encima de ellos
//Devuelve referencia al objeto por el que pasa el cirsor
var sel: TObjGraf;
  pCnx: TPtoConx;
begin
    sel := SelectSomeObject(X, Y);    //verifica si selecciona a un objeto
    Result := sel;  //Devuelve referencia
    //Se refresca la pantalla optimizando
    if sel = NIL then begin  //No hay ninguno por marcar
      if lastMarked <> NIL then begin
         //Si ya había uno marcado, se actualiza el dibujo y la bandera
         lastMarked.Marked := False;  //se desmarca
         lastMarked := NIL;
         Refresh;
      end;
      PBox.Cursor := CUR_DEFEC;   //restaura cursor
    end else begin   //Hay uno por marcar
      if lastMarked = NIL then begin
         //No había ninguno marcado
         lastMarked := sel;      //guarda
         sel.Marked := True;    //lo marca
         Refresh;            //y se dibuja
      end else begin  //ya había uno marcado
         if lastMarked = sel Then  //es el mismo
            //no se hace nada
         else begin    //había otro marcado
            lastMarked.Marked := False;  //se desmarca
            lastMarked := sel ;   //actualiza
            sel.Marked := True;
            Refresh;          //y se dibuja
         end;
        end;
    end;
    {Verifica si se pasa por un Punto de Control y lo pone como "Marked = true".
    Aquí se usa una técncia distinta. Se exploran todos los puntos de conexión y
    se marca solo el que es seleccionado por (x,y), poniendo los demas desmarcados.
    Tal vez se debería elegir la misma técnica para el marcado de objetos }
    pCnx := MarkConnectionPoint(X, Y);
    if lastCnxPnt <> pCnx then begin
      //Refresh only when changes
      Refresh;
    end;
    lastCnxPnt := pCnx;
end;
//Desplazamiento de pantalla
procedure TEditionMot.Desplazar(dx, dy: integer);
begin
//Procedimiento "estandar" para hacer un desplazamiento de la pantalla
//Varía los parámetros de la perspectiva "x_cam" e "y_cam"
    v2d.Scroll(dx, dy);
    v2d.SavePerspectiveIn(Pfinal);  //para que no se regrese al valor inicial
end;
procedure TEditionMot.ScrollDown(desp: Double = DESPLAZ_MENOR) ;  //abajo
//Genera un desplazamiento en la pantalla haciendolo independiente del
//factor de ampliación actual
var
    z: Single ;  //zoom
begin
    z := v2d.zoom;
    Desplazar(0, round(desp / z));
    Refresh;
end;
procedure TEditionMot.ScrollUp(desp: Double = DESPLAZ_MENOR) ;  //arriba
//Genera un desplazamiento en la pantalla haciendolo independiente del
//factor de ampliación actual
var
    z: Single ;  //zoom
begin
    z := v2d.zoom;
    Desplazar(0, round(-desp / z));
    Refresh;
end;
procedure TEditionMot.ScrollRight(desp: Double = DESPLAZ_MENOR) ;  //derecha
//Genera un desplazamiento en la pantalla haciendolo independiente del
//factor de ampliación actual
var
    z: Single ;  //zoom
begin
    z := v2d.zoom;
    Desplazar(round(desp / z), 0);
    Refresh;
end;
procedure TEditionMot.ScrollLeft(desp: Double = DESPLAZ_MENOR) ;  //izquierda
//Genera un desplazamiento en la pantalla haciendolo independiente del
//factor de ampliación actual
var
    z: Single ;  //zoom
begin
    z := v2d.zoom;
    Desplazar(round(-desp / z), 0);
    Refresh;
end;
procedure TEditionMot.ScrollDesp(dx, dy: integer);
//Desplazamiento de la pantalla
begin
//PBox.Canvas.TextOut(0,30,'dx=' + FloatToStr(dx) + '  ');
    v2d.x_cam := round(x_cam_a + dx / v2d.zoom);
    v2d.y_cam := round(y_cam_a + dy / v2d.zoom);
    v2d.SavePerspectiveIn(Pfinal);  //para que no se regrese al valor inicial
End;
// Funciones del Rectángulo de Selección
procedure TEditionMot.DibujRecSeleccion;
//Dibuja por métodos gráficos el rectángulo de selección en pantalla
begin
    v2d.SetPen(psDot, 1, clGreen);
    v2d.rectang0(x1Sel, y1Sel, x2Sel, y2Sel);

    x1Sel_a := x1Sel; y1Sel_a := y1Sel;
    x2Sel_a := x2Sel; y2Sel_a := y2Sel;
End;
procedure TEditionMot.InicRecSeleccion(X, Y: Integer);
//Inicia el rectángulo de selección, con las coordenadas
begin
    x1Sel:= X; y1Sel := Y;
    x2Sel := X; y2Sel := Y;
    x1Sel_a := x1Sel;
    y1Sel_a := y1Sel;
    x2Sel_a := x2Sel;
    y2Sel_a := y2Sel;
End;
function TEditionMot.RecSeleccionNulo: Boolean;
 //Indica si el rectángulo de selección es de tamaño NULO o despreciable
begin
    If (x1Sel = x2Sel) And (y1Sel = y2Sel) Then
        RecSeleccionNulo := True
    Else
        RecSeleccionNulo := False;
End;
function TEditionMot.enRecSeleccion(X, Y: Single): Boolean;
//Devuelve verdad si (x,y) esta dentro del rectangulo de seleccion.
var xMin, xMax: Integer;   //coordenadas mínimas y máximas del recuadro
    yMin, yMax: Integer;
    xx1, yy1: Single;
    xx2, yy2: Single;
begin
    //guarda coordenadas mínimas y máximas
    If x1Sel < x2Sel Then begin
        xMin := x1Sel;
        xMax := x2Sel;
    end Else begin
        xMin := x2Sel;
        xMax := x1Sel;
    End;
    If y1Sel < y2Sel Then begin
        yMin := y1Sel;
        yMax := y2Sel;
    end Else begin
        yMin := y2Sel;
        yMax := y1Sel;
    End;

    v2d.XYvirt(xMin, yMin, xx1, yy1);
    v2d.XYvirt(xMax, yMax, xx2, yy2);

    //verifica si está en región
    If (X >= xx1) And (X <= xx2) And (Y >= yy1) And (Y <= yy2) Then
        enRecSeleccion := True
    Else
        enRecSeleccion := False;
End;
(*
Public Sub CopiarAPortapapeles()
;  //Copia la selección en un archivo temporal y en el portapapeles.
Dim s: TObjGraf
Dim nar: Integer
    If seleccion.Count = 0 Then Exit Sub
    ;  //Generar archivo con contenido de copia
    nar = FreeFile
    Open CarpetaTmp & "\bolsa.txt" For Output: #nar
    For Each s In seleccion  ;  //explora todos
        Call s.EscCadenaObjeto(nar)
    Next s
    Close #nar
    ;  //copia al portapapeles
    Clipboard.Clear
    Clipboard.SetText LeeArchivo(CarpetaTmp & "\bolsa.txt")
    Call Refrescar
End Sub

Public Sub PegarDePortapapeles()
;  //Pega la selección en el reporte indicado
Dim nar: Integer
Dim Linea: String
Dim v: TObjGraf
Dim IDog: Integer         ;  //identificador de objeto gráfico
Dim error: String
    ;  //empieza a leer archivo
    nar = FreeFile
    If Dir(CarpetaTmp & "\bolsa.txt") = "" Then Exit Sub
    Open CarpetaTmp & "\bolsa.txt" For Input: #nar
    Call DeseleccionarTodos
    While Not EOF(nar)
        Line Input #nar, Linea
        If Linea Like "<OG??>" Then     ;  //Objeto gráfico
            IDog = Val(Mid$(Linea, 4, 2))
            Set v = AgregarObjGrafico(IDog, 1)
            If v = NIL Then Exit Sub   ;  //Hubo error
            error = v.LeeCadenaObjeto(nar)    ;  //lee los datos del objeto
            If error <> "" Then MsgBox error
            Seleccionar v
        End If
    Wend
    Close #nar
    Call Refrescar
End Sub

Public Sub GuardarPerspectiva(nar: Integer)
;  //Escribe datos de perspectiva en disco
    Print #nar, "<PERS>"   ;  //Marcador
    ;  //guarda parámetro de visualización
    Print #nar, N2f(v2d.zoom) & w & w & w & _
                N2f(v2d.x_cam) & w & N2f(v2d.y_cam) & ",,,,,"
    Print #nar, "</PERS>"   ;  //Marcador final
End Sub

Public Sub LeePerspectiva(nar: Integer)
;  //lee datos de perspectiva. No lee marcador inicial
Dim a(): String
Dim tmp: String
    ;  //lee perspectiva inicial
    Line Input #nar, tmp   ;  //lee parámetros
    a = Split(tmp, w)
    v2d.zoom = f2N(a(0))
    v2d.x_cam = f2N(a(3)): v2d.y_cam = f2N(a(4))
    Line Input #nar, tmp   ;  //lee marcador de fin
    Call v2d.GuardarPerspectivaEn(Pfinal)   ;  //para que no se regrese
End Sub

;  //--------------- Funciones de Búsqueda---------------
Public Sub InicBuscar(bus: String, _
                      Optional ambito: Integer = AMB_TODO, _
                      Optional ignCaja: Boolean = True, _
                      Optional palComp: Boolean = False)
;  //Inicia una búsqueda definiendo sus parámetros.
;  //La cadena "bus" debe ser de una sola línea.
;  //El parámetro "ambito" no se usa. Se mantiene por compatibilidad.
    PosEnc = 1    ;  //Fija posición inicial para buscar
    CadBus = bus        ;  //Guarda cadena de búsqueda
    CajBus = ignCaja    ;  //Guarda parámetro de caja
    PalCBus = palComp
End Sub

Public Function BuscarSig(): String
;  //Realiza una búsqueda iniciada con "InicBuscar"
;  //La búsqueda se hace a partir de la posición donde se dejó en la última búsqueda.
;  //Devuelve la cadena de búsqueda.
    ;  //Protecciones
    If objetos.Count = 0 Then Exit Function
    ;  //búsqueda
    If PalCBus Then ;  //Debe ser palabra completa
;  //        p = BuscarCadPos(CadBus, PosEnc, PosBus2, CajBus)
;  //        Do While p.xt <> 0
;  //            p2 = PosSigPos(p, Len(CadBus))
;  //            If EsPalabraCompleta(p, p2) Then Exit Do
;  //            PosEnc = p2
;  //            p = BuscarCadPos(CadBus, PosEnc, PosBus2, CajBus)
;  //        Loop
    Else    ;  //Búsqueda normal
;  //        p = BuscarCadPos(CadBus, PosEnc, PosBus2, CajBus)
;  //        If p.xt <> 0 Then p2 = PosSigPos(p, Len(CadBus))
    End If
    ;  //verifica si encontró
;  //    If p.xt <> 0 Then
;  //        Redibujar = True       ;  //para no complicarnos, dibuja todo
;  //        If haysel Then Call LimpSelec
;  //        ;  //Selecciona cadena
;  //        posCursorA p
;  //        Call FijarSel0      ;  //Fija punto base
;  //        posCursorA2 p2
;  //        Call ExtenderSel ;  //Extiende selección
;  //        BuscarSig = CadBus  ;  //devuelve cadena
;  //    Else
;  //        BuscarSig = CadBus  ;  //devuelve cadena
;  //        MsgBox "No se encuentra el texto: ;  //" & CadBus & ";  //", vbExclamation
;  //    End If
End Function
*)
// Eventos para atender requerimientos de objetos "TObjGraf"
procedure TEditionMot.ObjGraf_Select(obj: TObjGraf);
//Agrega un objeto gráfico a la lista "selección". Este método no debe ser llamado directamente.
//Si se quiere seleccionar un objeto se debe usar la forma objeto.Selec.
begin
//    If obj.Seleccionado Then Exit;  //Ya está Selected. No debe ser necesario
  seleccion.Add(obj);      { TODO : Verificar si se puede manejar bien el programa sin usar la propiedad "NombreObj"}
End;
procedure TEditionMot.ObjGraf_Unselec(obj: TObjGraf);
//Quita un objeto gráfico de la lista "selección". Este método no debe ser llamado directamente.
//Si se quiere quitar la seleccion a un objeto se debe usar la forma objeto.Deselec.
begin
//    If not obj.Seleccionado Then Exit;
  seleccion.Remove(obj);
End;
procedure TEditionMot.ObjGraf_SetPointer(Punt: integer);
//procedimiento que cambia el puntero del mouse. Es usado para proporcionar la los objetos "TObjGraf"
//la posibilidad de cambiar el puntero.
begin
  PBox.Cursor := Punt;        //define cursor
end;
//Inicialización
constructor TEditionMot.Create(PB0: TPaintBox);
//Metodo de inicialización de la clase Editor. Debe indicarse el
//PaintBox de salida donde se controlarán los objetos gráficos.
begin
  PBox := PB0;  //asigna control de salida
  //intercepta eventos
  PBox.OnMouseUp:=@MouseUp;
  PBox.OnMouseDown:=@MouseDown;
  PBox.OnMouseMove:=@MouseMove;
  PBox.OnMouseWheel:=@PBMouseWheel;
  PBox.OnDblClick:=@PBDblClick;
  PBox.OnPaint:=@Paint;

  //Inicia motor
  v2d := TMotGraf.IniMotGraf(PBox.Canvas);   //Inicia motor gráfico
  v2d.SetFont('MS Sans Serif');   //define tipo de letra
  objetos := TlistObjGraf.Create(TRUE);   //crea lista con "posesión" de objetos
  seleccion := TlistObjGraf.Create(FALSE);   //crea lista sin posesión", porque la
                                        //administración la hará "objetos".
  EstPuntero := EP_NORMAL;
  ParaMover := false;
  CapturoEvento := NIL;
  lastMarked := NIL;
  Modif := False;   //Inicialmente no modificado

  PBox.Cursor := CUR_DEFEC;        //define cursor
end;
destructor TEditionMot.Destroy;
begin
  seleccion.Free;
  objetos.Free;  //limpia lista y libera objetos apuntados
  v2d.Free;      //Libera
  //resatura eventos
  PBox.OnMouseUp:=nil;
  PBox.OnMouseDown:=nil;
  PBox.OnMouseMove:=nil;
  PBox.OnPaint:=nil;
  inherited;     //llama al destructor
end;

end.

