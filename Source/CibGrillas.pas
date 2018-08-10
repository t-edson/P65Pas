{Unidad con clases para mejorar las que presenta "UtilsGrilla".}
unit CibGrillas;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LCLIntf, Graphics, Controls, StdCtrls, fpexprpars, Grids,
  LCLType, BasicGrilla, UtilsGrilla, MisUtils;
type
  //Evento que genera el fin de la edición de una celda
  TEvSalida = (
    evsNulo,       //USado para cancelar el fin de la edición
    evsTecEnter,   //sale por tecla <Enter>
    evsTecTab,     //sale por tecla <Tab>
    evsTecDer,    //Direccional derecha
    evsTecEscape,  //sale por tecla <Escape>
    evsEnfoque     //sale porque se pierde el enfoque
  );

  TEvIniEditarCelda = procedure(col, fil: integer; txtInic: string) of object;
  TEvFinEditarCelda = procedure(var eveSal:TEvSalida; col, fil: integer;
                                var ValorAnter, ValorNuev: string) of object;
  TEvLeerColorFondo = function(col, fil: integer): TColor of object;

  { TGrillaEdic }
  {Define a una grilla de tipo "TUtilGrillaFil" que facilita la edición de los campos
  de la grilla asociada. Esta clase maneja un TEdit, como control para la edición del
  contenido de una celda, permitiendo cancelar la edición (sin modificar la celda),
  pulsando simplemente <Escape>. También se incluye un control de lista, para que pueda
  servir a modo de menú contextual, al momento de realizar la edición de la celda.
  Para interactuar con al edición, se incluyen los eventos:
   * OnIniEditarCelda;
   * OnFinEditarCelda;
  TGrillaEdic, no usa las opciones de edición, de TStringGRid (con los eventos
  OnGetEditText y OnEditingDone), sino que implementa sus propias rutinas de edición.
  Se diseño así porque se ha detectado muchos porblemas en las rutinas
  OnGetEditText() y OnEditingDone() de TStringGrid, como que sus parámetros no son
  apropiados y sobre todo, que se generan llamadas múltiples a estos eventos, cuando se
  usaba EditingDone().
  }
  TGrillaEdic = class(TUtilGrilla)
  private
    ColClick, RowClick: Longint;
    colIniCelda, filIniCelda: Integer;
    procedure edGrillaExit(Sender: TObject);
    procedure grillaSelection(Sender: TObject; aCol, aRow: Integer);
    procedure UbicarControles(r: TRect);
  protected
    edGrilla : TEdit;  //Editor para los campos de tipo texto
    lstGrilla: TListBox;  //Lista para la selección de valores
    valIniCelda: string;
    procedure edGrillaKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure edGrillaKeyPress(Sender: TObject; var Key: char);
    procedure grillaKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure grillaKeyPress(Sender: TObject; var Key: char); override;
    procedure grillaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure grillaMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  public
    OnIniEditarCelda : TEvIniEditarCelda;  //Inicia la edición de una celda
    OnFinEditarCelda : TEvFinEditarCelda;  //Finaliza la edición de una celda
    procedure IniciarEdicion(txtInic: string); virtual;
    procedure TerminarEdicion(eventSalida: TEvSalida; ValorAnter, ValorNuev: string
      ); virtual;
    function EnEdicion: boolean;
    procedure NumerarFilas;
    procedure ValidaFilaGrilla(f: integer);
  public //Inicialización
    constructor Create(grilla0: TStringGrid); override;
    destructor Destroy; override;
  end;

  { TGrillaEdicFor }
  {Agrega a TGrillaEdic capacidades para colorear las filas, en base a una condición
   de alguna(s) celdas.}
  TGrillaEdicFor = class(TGrillaEdic)
  private
    FOpSelMultiFila: boolean;
    procedure DibCeldaIcono(aCol, aRow: Integer; const aRect: TRect);
    procedure DibCeldaTexto(aCol, aRow: Integer; const aRect: TRect);
    procedure DibCeldaTextoEncab(aCol, aRow: Integer; const aRect: TRect);
    function EsFilaSeleccionada(const f: integer): boolean;
    procedure grillaDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure SetOpSelMultiFila(AValue: boolean);
  public
    ImageList: TImageList;   //referencia a un TInageList, para los íconos
    OnLeerColorFondo: TEvLeerColorFondo;
    property OpSelMultiFila: boolean  //activa el dimensionamiento de columnas
             read FOpSelMultiFila write SetOpSelMultiFila;
    constructor Create(grilla0: TStringGrid); override;
  end;

implementation

{ TGrillaEdic }
procedure TGrillaEdic.UbicarControles(r: TRect);
begin
  //Ubica editor
  edGrilla.Left   := grilla.Left + r.left + 1;
  edGrilla.Top    := grilla.Top + r.Top + 1;
  edGrilla.Width  := r.Right - r.Left;;
  edGrilla.Visible:= true;
  //Ubica lista, por si se le desse usar
  lstGrilla.Left := edGrilla.Left;
  lstGrilla.Top := edGrilla.Top+ edGrilla.Height;
  lstGrilla.Width := r.Right - r.Left + 50;
  lstGrilla.Height := 120; //r.Bottom - r.Top;
end;
procedure TGrillaEdic.edGrillaKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then begin
    TerminarEdicion(evsTecEscape, valIniCelda, edGrilla.Text);   //termina edición
  end else if Key = #13 then begin
//    Key := #0;
    TerminarEdicion(evsTecEnter, valIniCelda, edGrilla.Text);   //termina edición
  end;
end;
procedure TGrillaEdic.edGrillaKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_TAB then begin
    TerminarEdicion(evsTecTab, valIniCelda, edGrilla.Text);   //termina edición
  end;
  if Key = VK_RIGHT then begin
    if edGrilla.SelStart = length(edGrilla.Text) then begin
      TerminarEdicion(evsTecDer, valIniCelda, edGrilla.Text);   //termina edición
    end;
  end;
end;
procedure TGrillaEdic.edGrillaExit(Sender: TObject);
begin
  {En general, el enfoque se puede perder por diversos motivos, pero lo más común
   es que se haya hecho "click" en alguna otra parte de la grilla. }
  TerminarEdicion(evsEnfoque, valIniCelda, edGrilla.Text);   //termina edición
end;
procedure TGrillaEdic.grillaSelection(Sender: TObject; aCol, aRow: Integer);
begin
  if not EnEdicion then exit;  //no está en modo edición
  TerminarEdicion(evsTecEscape, valIniCelda, edGrilla.Text);   //termina edición
end;
procedure TGrillaEdic.IniciarEdicion(txtInic: string);
{Inicia la edición del valor de una celda}
begin
//debugln('IniciarEdicion');
  if not cols[grilla.Col].editable then exit;
  valIniCelda := grilla.Cells[grilla.Col, grilla.Row];
  colIniCelda := grilla.Col;   //guarda coordenadas de edición
  filIniCelda := grilla.Row;   //guarda coordenadas de edición
  edGrilla.Text := txtInic;
  UbicarControles(grilla.CellRect(grilla.Col, grilla.Row));
  edGrilla.OnExit:=@edGrillaExit;   {Para evitar que el editor quede visible al cambiar el
                                     enfoque.}
  if cols[grilla.Col].tipo = ugTipNum then edGrilla.Alignment := taRightJustify
  else edGrilla.Alignment := taLeftJustify;
  edGrilla.Visible:=true;
  if edGrilla.Visible then edGrilla.SetFocus;
  edGrilla.SelStart:=length(edGrilla.Text);  //quita la selección
  if OnIniEditarCelda<>nil then
    OnIniEditarCelda(grilla.Col, grilla.Row, txtInic);
end;
procedure TGrillaEdic.TerminarEdicion(eventSalida: TEvSalida; ValorAnter, ValorNuev: string);
begin
//debugln('---TerminarEdicion');
  if OnFinEditarCelda<>nil then begin
    OnFinEditarCelda(eventSalida, colIniCelda, filIniCelda, ValorAnter, ValorNuev);
    if eventSalida = evsNulo then exit;   //Se canceló el fin de la edición
  end;
  //Se porcede a terminar la edición
  edGrilla.OnExit := nil;  {Para evitar llamada recursiva de este evento. Se debe hacer
                            antes de ocultarlo.}
  edGrilla.Visible:=false;
  lstGrilla.Visible:=false;
  if grilla.Visible then grilla.SetFocus;    //retorna enfoque a la grilla
  case eventSalida of
  evsTecEnter: begin
    grilla.Cells[colIniCelda, filIniCelda] := ValorNuev;  //acepta valor
    AdelantarAFilaVis(grilla);   //pasa a siguiente línea
  end;
  evsTecTab: begin
    grilla.Cells[colIniCelda, filIniCelda] := ValorNuev;  //acepta valor
    MovASiguienteColVis(grilla);   //pasa a siguiente columna
  end;
  evsTecDer: begin
    grilla.Cells[colIniCelda, filIniCelda] := ValorNuev;  //acepta valor
    MovASiguienteColVis(grilla);   //pasa a siguiente columna
  end;
  evsEnfoque: begin
    grilla.Cells[colIniCelda, filIniCelda] := ValorNuev;  //acepta valor
  end;
  end;
end;
function TGrillaEdic.EnEdicion: boolean;
{Indica si la grilla está en mod de edición.}
begin
  Result := edGrilla.Visible;
end;
procedure TGrillaEdic.NumerarFilas;
var
  f: Integer;
begin
  grilla.BeginUpdate;
  f := 1;
  for f := 1 to grilla.RowCount-1 do begin
    grilla.Cells[0, f] := IntToStr(f);
  end;
  grilla.EndUpdate();
end;
procedure TGrillaEdic.ValidaFilaGrilla(f: integer);
{Valida una fila de la grilla, para ver si es consistente, para ser ingresado como
registro nuevo a la grilla.}
var
  col: TugGrillaCol;
begin
  MsjError := '';
  for col in cols do begin
    colError:=col.idx;  {Se asigna primero la columna de error, para dar posibilidad a la
                        rutina de validación, el poder cambiarla en caso de que lo crea
                        conveniente.}
    col.ValidateStr(f);
    if MsjError <> '' then exit;
  end;
end;
//Estas rutinas de teclado, determinan cuando se inicia o no la edición de una celda.
procedure TGrillaEdic.grillaKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited grillaKeyDown(Sender, Key, Shift);
  //Se filtran las teclas que permitirán la edición.
  if (Key = VK_DELETE) and (Shift = []) then begin
    IniciarEdicion('');
  end else if Key = VK_F2 then begin
    IniciarEdicion(grilla.Cells[grilla.Col, grilla.Row]);
  end else begin
    //Estas teclas no se reconcoen aquí, pero puede que grillaKeyPress(), si lo haga.
  end;
end;
procedure TGrillaEdic.grillaKeyPress(Sender: TObject; var Key: char);
begin
  inherited grillaKeyPress(Sender, Key);
  if Key in ['0'..'9','a'..'z','A'..'Z','+','$'] then begin
    IniciarEdicion(Key);
    Key := #0;  //Para no dejar pasar accesos directos Botones.  Se detectó un error en unas pruebas
  end else begin
    Key := #0;   //para no entrar en modo de edición.
  end;
end;
procedure TGrillaEdic.grillaMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Longint;
begin
  inherited grillaMouseDown(Sender, Button, Shift, X, Y);
  grilla.MouseToCell(X, Y, ACol, ARow );
  if ACol = 0 then begin
    //En la columna fija, selecciona la fila.
    if ARow >= grilla.FixedRows then begin
      grilla.Row:=ARow;
    end;
  end;
  //Guarda coordenadas de la celda pulsad
  ColClick := grilla.Col;
  RowClick := grilla.Row;
end;
procedure TGrillaEdic.grillaMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Longint;
begin
  inherited grillaMouseUp(Sender, Button, Shift, X, Y);
  //También se puede iniciar la edición con el Mouse
  if Button = mbLeft then begin
    grilla.MouseToCell(X, Y, ACol, ARow );
    if ARow<grilla.FixedRows then exit;   //no en el encabezado
    if ACol<grilla.FixedCols then exit;   //no en el encabezado
    if (ColClick = ACol) and (RowClick = ARow) then begin
      //Click en la celda seleccionada.
      IniciarEdicion(grilla.Cells[grilla.Col, grilla.Row]);
    end;
  end;
end;
constructor TGrillaEdic.Create(grilla0: TStringGrid);
begin
  inherited Create(grilla0);
  //Crea el control de edición
  edGrilla := TEdit.Create(nil);
  edGrilla.Parent := grilla.Parent;  //Ubica como contenedor al mismo contnedor de la grilla
  edGrilla.Color:=TColor($40FFFF);
  edGrilla.Visible:=false;
  edGrilla.TabStop:=false;
  //Crea la lista contextual para mostrar opciones
  //Por defecto la lista no se mostrará. Debe controlarse por la aplicaicón.
  lstGrilla := TListBox.Create(nil);
  lstGrilla.Parent := grilla.Parent;  //Ubica como contenedor al mismo contnedor de la grilla
  lstGrilla.Color:=TColor($40FFFF);
  lstGrilla.Visible:=false;
  //La opción de edición, l amanejamos aquí
  grilla.Options:=grilla.Options-[goEditing];
  grilla.OnSelection:=@grillaSelection;
  //Configura eventos
  edGrilla.OnKeyPress:=@edGrillaKeyPress;
  edGrilla.OnKeyDown:=@edGrillaKeyDown;
end;
destructor TGrillaEdic.Destroy;
begin
  lstGrilla.Destroy;
  edGrilla.Destroy;
  inherited Destroy;
end;
{ TGrillaEdicFor }
procedure TGrillaEdicFor.SetOpSelMultiFila(AValue: boolean);
begin
  FOpSelMultiFila:=AValue;
  if grilla<>nil then begin
    //Ya tiene asignada una grilla
    if AValue then grilla.RangeSelectMode := rsmMulti
    else grilla.RangeSelectMode := rsmSingle;
  end;
end;
procedure TGrillaEdicFor.DibCeldaIcono(aCol, aRow: Integer; const aRect: TRect);
{Dibuja un ícono alineado en la celda "aRect" de la grilla "Self.grilla", usando el
alineamiento de Self.cols[].}
var
  cv: TCanvas;
  txt: String;
  ancTxt: Integer;
  icoIdx: Integer;
begin
  cv := grilla.Canvas;  //referencia al Lienzo
  if ImageList = nil then exit;
  //Es una celda de tipo ícono
  txt := grilla.Cells[ACol,ARow];
  if not TryStrToInt(txt, icoIdx) then begin //obtiene índice
    icoIdx := -1
  end;
  case cols[aCol].alineam of
    taLeftJustify: begin
      ImageList.Draw(cv, aRect.Left+2, aRect.Top+2, icoIdx);
    end;
    taCenter: begin
      ancTxt := ImageList.Width;
      ImageList.Draw(cv, aRect.Left + ((aRect.Right - aRect.Left) - ancTxt) div 2,
                   aRect.Top + 2, icoIdx);
    end;
    taRightJustify: begin
      ancTxt := ImageList.Width;
      ImageList.Draw(cv, aRect.Right - ancTxt - 2, aRect.Top+2, icoIdx);
    end;
  end;
end;
procedure TGrillaEdicFor.DibCeldaTextoEncab(aCol, aRow: Integer; const aRect: TRect);
{Dibuja un texto para una celda en el encabezado.}
var
  cv: TCanvas;
  txt: String;
begin
  cv := grilla.Canvas;  //referencia al Lienzo
  txt := grilla.Cells[ACol,ARow];
  cv.TextOut(aRect.Left + 2, aRect.Top + 2, txt);
end;
procedure TGrillaEdicFor.DibCeldaTexto(aCol, aRow: Integer; const aRect: TRect);
{Dibuja un texto alineado en la celda "aRect" de la grilla "Self.grilla", usando el
alineamiento de Self.cols[].}
var
  cv: TCanvas;
  txt: String;
  ancTxt: Integer;
  colum: TugGrillaCol;
begin
  cv := grilla.Canvas;  //referencia al Lienzo
  txt := grilla.Cells[ACol,ARow];
  colum := cols[aCol];
  if (colum.tipo = ugTipNum) and (colum.formato<>'') then begin   //Hay formato
    try
       txt := Format(colum.formato, [f2N(txt)]);
    except
       txt := '###';
    end;
  end;
  //escribe texto con alineación
  case colum.alineam of
    taLeftJustify: begin
      cv.TextOut(aRect.Left + 2, aRect.Top + 2, txt);
    end;
    taCenter: begin
      ancTxt := cv.TextWidth(txt);
      cv.TextOut(aRect.Left + ((aRect.Right - aRect.Left) - ancTxt) div 2,
                 aRect.Top + 2, txt );
    end;
    taRightJustify: begin
      ancTxt := cv.TextWidth(txt);
      cv.TextOut(aRect.Right - ancTxt - 3, aRect.Top + 2, txt);
    end;
  end;
end;
function TGrillaEdicFor.EsFilaSeleccionada(const f: integer): boolean;
{Indica si la fila "f", está seleccionada.
Se puede usar esta función para determinar las filas seleccionadas de la grilla (en el
caso de que la selección múltiple esté activada), porque hasta la versión actual,
SelectedRange[], puede contener rangos duplicados, si se hace click dos veces en la misma
fila, así que podría dar problemas si se usa SelectedRange[], para hallar las filas
seleccionadas.}
var
  i: Integer;
  sel: TGridRect;
begin
  if not FOpSelMultiFila then begin
    //Caso de selección simple
    exit(f = grilla.Row);
  end;
  //Selección múltiple
  for i:=0 to grilla.SelectedRangeCount-1 do begin
    sel := grilla.SelectedRange[i];
    if (f >= sel.Top) and (f <= sel.Bottom) then exit(true);
  end;
  //No está en ningún rango de selección
  exit(false);
end;
procedure TGrillaEdicFor.grillaDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
{Rutina personalziad para el dibujo de la celda}
var
  cv: TCanvas;           //referencia al lienzo
  atrib: integer;
begin
  cv := grilla.Canvas;  //referencia al Lienzo
  if gdFixed in aState then begin
    //Es una celda fija
    cv.Font.Color := clBlack;
    cv.Font.Style := [];
    cv.Brush.Color := clBtnFace;
    cv.FillRect(aRect);   //fondo
    DibCeldaTextoEncab(aCol, aRow, aRect);
  end else begin
    //Es una celda común
    cv.Font.Color := TColor(PtrUInt(grilla.Objects[1, aRow]));
    if grilla.Objects[2, aRow]=nil then begin
      //Sin atributos
      cv.Font.Style := [];
    end  else begin
      //Hay atributos de texto
      atrib := PtrUInt(grilla.Objects[2, aRow]);
      if (atrib and 1) = 1 then cv.Font.Style := cv.Font.Style + [fsUnderline];
      if (atrib and 2) = 2 then cv.Font.Style := cv.Font.Style + [fsItalic];
      if (atrib and 4) = 4 then cv.Font.Style := cv.Font.Style + [fsBold];
    end;
    if OpResaltFilaSelec and EsFilaSeleccionada(aRow) then begin
      //Fila seleccionada. (Debe estar activada la opción "goRowHighligh", para que esto funcione bien.)
      cv.Brush.Color := clBtnFace;
    end else begin
      if OnLeerColorFondo<>nil then
        cv.Brush.Color := OnLeerColorFondo(aCol, aRow)
      else
        cv.Brush.Color := clWhite;
    end;
    cv.FillRect(aRect);   //fondo
    if cols[aCol].tipo = ugTipIco then
      DibCeldaIcono(aCol, aRow, aRect)
    else
      DibCeldaTexto(aCol, aRow, aRect);
    // Dibuja ícono
{    if (aCol=0) and (aRow>0) then
      ImageList16.Draw(grilla.Canvas, aRect.Left, aRect.Top, 19);}
    //Dibuja borde en celda seleccionada
    if gdFocused in aState then begin
      cv.Pen.Color := clRed;
      cv.Pen.Style := psDot;
      cv.Frame(aRect.Left, aRect.Top, aRect.Right-1, aRect.Bottom-1);  //dibuja borde
    end;
  end;
end;
constructor TGrillaEdicFor.Create(grilla0: TStringGrid);
begin
  inherited Create(grilla0);
  grilla.DefaultDrawing := false;
  grilla.OnDrawCell := @grillaDrawCell;
end;

end.

