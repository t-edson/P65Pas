{Implemnenta la ventana de mensajes}
unit FrameMessagesWin;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Grids, Graphics,
  ExtCtrls, StdCtrls, Menus, Clipbrd, EpikTimer, CompBase, Globales, LexPas,
  adapterBase, UtilsGrilla, BasicGrilla, MisUtils;
type

  { TUtilGrillaFil2 }

  TUtilGrillaFil2 = class(TUtilGrillaFil)
    BackSelColor: TColor;   //Color de fondo de la selección
    procedure grillaDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState); override;
  public //Inicialización
    constructor Create(grilla0: TStringGrid); override;
  end;

  { TfraMessagesWin }
  TfraMessagesWin = class(TFrame)
    chkInform: TCheckBox;
    chkWarns: TCheckBox;
    chkErrors: TCheckBox;
    ImgMessages: TImageList;
    lblInform: TLabel;
    lblWarns: TLabel;
    lblRAM: TLabel;
    lblROM: TLabel;
    lblErrors: TLabel;
    mnCopyRow: TMenuItem;
    mnClearAll: TMenuItem;
    panStatis: TPanel;
    Panel2: TPanel;
    grilla: TStringGrid;
    PanGrilla: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure chkErrorsChange(Sender: TObject);
    procedure chkInformChange(Sender: TObject);
    procedure chkWarnsChange(Sender: TObject);
    procedure grillaDblClick(Sender: TObject);
    procedure mnClearAllClick(Sender: TObject);
    procedure mnCopyRowClick(Sender: TObject);
    procedure PanGrillaResize(Sender: TObject);
    procedure panStatisDblClick(Sender: TObject);
  private
    cxp       : TAdapterBase;
    FBackColor: TColor;
    FBackSelColor: Tcolor;
    FPanelColor: TColor;
    FTextColor: TColor;
    FTextErrColor: TColor;
    UtilGrilla: TUtilGrillaFil2;
    nVis, nWar, nErr: Integer;
    eTimer    : TEpikTimer;  //Counter for mesaure compiling
    procedure CountMessages;
    procedure SetBackColor(AValue: TColor);
    procedure SetBackSelColor(AValue: Tcolor);
    procedure SetPanelColor(AValue: TColor);
    procedure SetTextColor(AValue: TColor);
    procedure SetTextErrColor(AValue: TColor);
  public
    HaveErrors: boolean;
    OnDblClickMessage: procedure(fileSrc: string; row, col: integer) of object;
    OnStatisDBlClick: procedure of object;
    property BackColor: TColor read FBackColor write SetBackColor ;
    property TextColor: TColor read FTextColor write SetTextColor ;
    property TextErrColor: TColor read FTextErrColor write SetTextErrColor;
    property BackSelColor: Tcolor read FBackSelColor write SetBackSelColor;
    property PanelColor: TColor read FPanelColor write SetPanelColor;
    procedure FilterGrid;
    procedure GetFirstError(out msg: string; out filname: string; out row,
      col: integer);
    procedure GetErrorIdx(f: integer; out msg: string; out filname: string; out
      row, col: integer);
    function IsErroridx(f: integer): boolean;
    procedure InitCompilation(cxp0: TAdapterBase; InitMsg: boolean);
    procedure EndCompilation(showSummary: boolean = true);
    procedure AddError(errTxt, fname: string; row, col: integer);
    procedure AddInformation(infTxt, fname: string; row, col: integer);
    procedure AddWarning(warTxt, fname: string; row, col: integer);
  public //Inicialización
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
    procedure SetLanguage;
  end;

implementation
{$R *.lfm}
const
  ROW_HEIGH = 19;
  //Constantes para identificar íconos
  ICO_INF = '0';
  ICO_WAR = '1';
  ICO_ERR = '2';
var
  //Variables para índice de columnas
  GCOL_ICO: integer;  //ícono
  GCOL_TXT: integer;  //texto a mostrar en la grilla
  GCOL_FILE: integer;  //archivo (incluyendo ruta)
  GCOL_ROW: integer;  //número de fila
  GCOL_COL: integer;  //número ed columna
  GCOL_MSG: integer;  //mensaje

var  //Cadenas de traducción
  MSG_INICOMP: string;
  MSG_WARN   : string;
  MSG_WARNS  : string;
  MSG_ERROR  : string;
  MSG_ERRORS : string;
  MSG_COMPIL : string;
{ TUtilGrillaFil2 }
procedure TUtilGrillaFil2.grillaDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
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
    DibCeldaTexto(aCol, aRow, aRect);
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
      cv.Brush.Color := BackSelColor;
    end else begin
      cv.Brush.Color := TColor(PtrUInt(grilla.Objects[0, aRow]));
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
//      cv.Pen.Color := clRed;
//      cv.Pen.Style := psDot;
//      cv.Frame(aRect.Left, aRect.Top, aRect.Right-1, aRect.Bottom-1);  //dibuja borde
    end;
  end;
end;
constructor TUtilGrillaFil2.Create(grilla0: TStringGrid);
begin
  inherited Create(grilla0);
  BackSelColor := clBtnFace;
end;
{ TfraMessagesWin }
procedure TfraMessagesWin.SetLanguage;
begin
 {$I ..\_language\tra_FrameMessagesWin.pas}
end;
procedure TfraMessagesWin.AddInformation(infTxt, fname: string; row, col: integer);
var
  f: Integer;
begin
  f := grilla.RowCount;
  grilla.RowCount := f + 1;
  grilla.Cells[GCOL_ICO, f] := ICO_INF;  //ícono
  grilla.Cells[GCOL_TXT, f] := infTxt;
  grilla.Cells[GCOL_FILE, f] := '';
  grilla.Cells[GCOL_ROW, f] := '-1';
  grilla.Cells[GCOL_COL, f] := '-1';
  grilla.Cells[GCOL_MSG, f] := infTxt;
  grilla.RowHeights[f] := ROW_HEIGH;
  UtilGrilla.FijColorFondo(f, FBackColor);  //Color de fondo de la fila
  UtilGrilla.FijColorTexto(f, FTextColor);
end;
procedure TfraMessagesWin.AddWarning(warTxt, fname: string; row, col: integer);
var
  f: Integer;
begin
  f := grilla.RowCount;
  grilla.RowCount := f + 1;
  grilla.Cells[GCOL_ICO, f] := ICO_WAR;  //ícono
  grilla.Cells[GCOL_TXT, f] := ExtractFileNameOnly(fName) +
    '['+ IntToStr(row) + ',' + IntToStr(col) + '] '+ MSG_WARN + ': ' + warTxt;
  grilla.Cells[GCOL_FILE, f] := fName;
  grilla.Cells[GCOL_ROW, f] := IntToStr(row);
  grilla.Cells[GCOL_COL, f] := IntToStr(col);
  grilla.Cells[GCOL_MSG, f] := warTxt;   //El texto tal cual
  grilla.RowHeights[f] := ROW_HEIGH;
  UtilGrilla.FijColorFondo(f, FBackColor);  //Color de fondo de la fila
  UtilGrilla.FijColorTexto(f, FTextColor);
end;
procedure TfraMessagesWin.AddError(errTxt, fname: string; row, col: integer);
var
  f: Integer;
begin
  f := grilla.RowCount;
  grilla.RowCount := f + 1;
  grilla.Cells[GCOL_ICO, f] := ICO_ERR;  //ícono
  grilla.Cells[GCOL_TXT, f] := ExtractFileNameOnly(fName) +
    '['+ IntToStr(row) + ',' + IntToStr(col) + '] ' + MSG_ERROR + ': ' + errTxt;
  grilla.Cells[GCOL_FILE, f] := fName;
  grilla.Cells[GCOL_ROW, f] := IntToStr(row);
  grilla.Cells[GCOL_COL, f] := IntToStr(col);
  grilla.Cells[GCOL_MSG, f] := errTxt;   //El texto tal cual
  grilla.RowHeights[f] := ROW_HEIGH;
  UtilGrilla.FijColorFondo(f, FBackColor);  //Color de fondo de la fila
  UtilGrilla.FijColorTexto(f, FTextErrColor);  //Color del texto de la fila
  HaveErrors := true;  //marca bandera
end;
procedure TfraMessagesWin.chkInformChange(Sender: TObject);
begin
  FilterGrid;
end;
procedure TfraMessagesWin.chkErrorsChange(Sender: TObject);
begin
  FilterGrid;
end;
procedure TfraMessagesWin.chkWarnsChange(Sender: TObject);
begin
  FilterGrid;
end;
procedure TfraMessagesWin.grillaDblClick(Sender: TObject);
var
  row, col: Longint;
  arc: String;
begin
  if grilla.Row = -1 then exit;
  arc := grilla.Cells[GCOL_FILE, grilla.row];
  TryStrToInt(grilla.Cells[GCOL_ROW, grilla.row], row);
  TryStrToInt(grilla.Cells[GCOL_COL, grilla.row], col);
  if arc<>'' then begin
    if OnDblClickMessage<>nil then OnDblClickMessage(arc, row, col);
  end;
end;
procedure TfraMessagesWin.mnCopyRowClick(Sender: TObject);
{Copia la fila seleccionada al portapapeles.}
begin
  if grilla.Row = -1 then exit;
  Clipboard.AsText := grilla.Cells[2, grilla.Row];
end;
procedure TfraMessagesWin.mnClearAllClick(Sender: TObject);
{Limpia la lista de mensajes.}
begin
  grilla.RowCount := 1;   //Limpia Grilla
end;
procedure TfraMessagesWin.PanGrillaResize(Sender: TObject);
begin
  grilla.ColWidths[2] := PanGrilla.Width-50;
end;
procedure TfraMessagesWin.panStatisDblClick(Sender: TObject);
begin
  if OnStatisDBlClick<>nil then OnStatisDBlClick;
end;
procedure TfraMessagesWin.CountMessages;
var
  f: Integer;
begin
  //Cuenta advertencias, errores, y filas visibles
  nVis := 0;
  nWar := 0;
  nErr := 0;
  for f:=1 to grilla.RowCount -1 do begin
    if grilla.Cells[GCOL_ICO, f] = ICO_WAR then Inc(nWar);
    if grilla.Cells[GCOL_ICO, f] = ICO_ERR then Inc(nErr);
    if grilla.RowHeights[f] > 0 then inc(nVis);
  end;
end;
procedure TfraMessagesWin.SetBackColor(AValue: TColor);
begin
  FBackColor := AValue;
  grilla.Color := AValue;
end;
procedure TfraMessagesWin.SetBackSelColor(AValue: Tcolor);
begin
  FBackSelColor := AValue;
  UtilGrilla.BackSelColor := AValue;
end;
procedure TfraMessagesWin.SetPanelColor(AValue: TColor);
begin
  FPanelColor := AValue;
  //Paneles
  panStatis.Color := AValue;   //No muy útil si vamos a poner un Frame encima.
  panel2.Color := AValue;
  PanGrilla.Color := AValue;
  Splitter1.Color := AValue;
  Splitter2.Color := AValue;
end;
procedure TfraMessagesWin.SetTextColor(AValue: TColor);
begin
  if FTextColor = AValue then Exit;
  FTextColor := AValue;
  //Paneles
//  chkInform.Font.Color := Avalue;  No se pude cambiar direcatmente a los CheckBox
//  chkWarns.Font.Color := Avalue;
//  chkErrors.Font.Color := Avalue;
  lblInform.Font.Color := AVAlue;
  lblWarns.Font.Color := AVAlue;
  lblErrors.Font.Color := AVAlue;
  Pangrilla.Font.Color := AVAlue;

  lblRAM.Font.Color := Avalue;
  lblROM.Font.Color := Avalue;
end;
procedure TfraMessagesWin.SetTextErrColor(AValue: TColor);
begin
  if FTextErrColor = AValue then Exit;
  FTextErrColor := AValue;
end;
procedure TfraMessagesWin.GetFirstError(out msg: string; out filname: string;
                                        out row, col: integer);
{Devuelve información sobre el primer error de la lista de errores.
Si el error no tiene información de ubicación, devuelve -1 en "row" y "col".
Si no encuentra algún error, devuelve cadena vacía en "msg".}
var
  f: Integer;
begin
  for f:=1 to grilla.RowCount -1 do begin
    if IsErroridx(f) then begin
      GetErrorIdx(f, msg, filname, row, col);
      exit;
    end;
  end;
  //No encontró
  msg := '';
end;
procedure TfraMessagesWin.GetErrorIdx(f: integer; out msg: string; out
  filname: string; out row, col: integer);
{Obtiene el error de índice "f".}
begin
  msg := grilla.Cells[GCOL_MSG, f];
  filname := grilla.Cells[GCOL_FILE, f];
  TryStrToInt(grilla.Cells[GCOL_ROW, f], row);
  TryStrToInt(grilla.Cells[GCOL_COL, f], col);
end;
function TfraMessagesWin.IsErroridx(f: integer): boolean;
{Indica si la fila de la grilla contiene un error.}
begin
  result := grilla.Cells[GCOL_ICO, f] = ICO_ERR;
end;
procedure TfraMessagesWin.FilterGrid;
var
  f: integer;
  icoId: String;
begin
  grilla.BeginUpdate;
  for f:=1 to grilla.RowCount -1 do begin
    icoId := grilla.Cells[GCOL_ICO, f];
    case icoId of
    ICO_INF: begin
      if chkInform.Checked then grilla.RowHeights[f] := ROW_HEIGH
      else grilla.RowHeights[f] := 0;
    end;
    ICO_WAR: begin
      if chkWarns.Checked then grilla.RowHeights[f] := ROW_HEIGH
      else grilla.RowHeights[f] := 0;
    end;
    ICO_ERR: begin
      if chkErrors.Checked then grilla.RowHeights[f] := ROW_HEIGH
      else grilla.RowHeights[f] := 0;
    end;
    end;
  end;
  CountMessages;
  if nVis = 0 then begin
    grilla.Visible := false;
  end else begin
    grilla.Visible := true;
    grilla.Row := PrimeraFilaVis(grilla);
  end;
  grilla.EndUpdate;
end;
procedure TfraMessagesWin.InitCompilation(cxp0: TAdapterBase; InitMsg: boolean
  );
{Limpia grilla e inicia banderas para empezar a recibir mensajes.}
begin
  cxp := cxp0;   //Guarda referencia
  grilla.RowCount := 1;   //Limpia Grilla
  cxp.OnWarning:= @AddWarning;  //Inicia evento
  cxp.OnError  := @AddError;
  cxp.OnInfo   := @AddInformation;

  eTimer.Clear;
  eTimer.Start;   //Star counting time
  if InitMsg then AddInformation(cxp.CompilerName + ': ' + MSG_INICOMP, '', 0, 0);
  HaveErrors := false;  //limpia bandera
end;
procedure TfraMessagesWin.EndCompilation(showSummary: boolean = true);
{Escribe un mensaje final del tiempo de compilación usado y la cantidad de advertencias
y errores en el Panel de mensajes. También incluye información sobre los recursos usados
(RAM) y filtra los mensajes de acuerdo a lo que indican los "CheckBox".}
var
  infWar, infErr: String;
begin
  if not showSummary then begin
     //Solo filtra los posibles mensajes recibidos.
    FilterGrid;
    exit;
  end;
  //Construye información adicional
  CountMessages;
  if nWar = 1 then begin
    infWar := '1 ' + MSG_WARN;
  end else begin
    infWar := IntToStr(nWar) + ' ' + MSG_WARNS;
  end;
  if nErr = 1 then begin
    infErr := '1 ' + MSG_ERROR;
  end else begin
    infErr := IntToStr(nErr) + ' ' + MSG_ERRORS;
  end;
  eTimer.Stop;  //Stop counter
  AddInformation(MSG_COMPIL + IntToStr(round(eTimer.Elapsed*1000)) + ' msec. <<' +
                 infWar + ', ' + infErr + '>>', '', 0, 0);

  //Actualiza estadísticas de uso
  if nErr=0 then begin     //No hay error
    AddInformation(cxp.RAMusedStr, '', 0, 0);
  end;
  FilterGrid;
  //Posiciona al final
  if grilla.RowCount>1 then begin
    grilla.Row := grilla.RowCount -1;
  end;
end;
//Inicialización
constructor TfraMessagesWin.Create(AOwner: TComponent);
var
  enc: TugGrillaCol;
begin
  inherited Create(AOwner);
  //Configura Grilla de mensajes
  UtilGrilla:= TUtilGrillaFil2.Create(grilla);
  grilla.FixedCols := 0;
  UtilGrilla.IniEncab;
  ///////// Definición de columnas //////////
  UtilGrilla.AgrEncabNum('' , 30).visible := false;  //Columna fija
  enc := UtilGrilla.AgrEncabIco('Ícono'  ,  24); //Ícono centrado
  GCOL_ICO := enc.idx;
  enc := UtilGrilla.AgrEncabTxt('Texto', 500) ;
  GCOL_TXT := enc.idx;
  enc := UtilGrilla.AgrEncabTxt('Archivo', 120);
  GCOL_FILE := enc.idx;
  enc.visible := false;
  enc := UtilGrilla.AgrEncabNum('Fila'   ,  20);
  GCOL_ROW := enc.idx;
  enc.visible := false;
  enc := UtilGrilla.AgrEncabNum('Columna',  20);
  GCOL_COL := enc.idx;
  enc.visible := false;
  enc := UtilGrilla.AgrEncabNum('Mensaje',  20);
  GCOL_MSG := enc.idx;
  enc.visible := false;
  /////////////////////////////////////////////
  UtilGrilla.FinEncab;
  UtilGrilla.OpDimensColumnas := true;
  UtilGrilla.OpDimensColumnas := true;
  UtilGrilla.ImageList := ImgMessages;  //Íconos a usar
  grilla.RowHeights[0] := 0;  //oculta envabezado
  grilla.Visible := false;
  eTimer := TEpikTimer.Create(nil);  //Used for precision time measure
end;
destructor TfraMessagesWin.Destroy;
begin
  eTimer.Destroy;
  UtilGrilla.Destroy;
  inherited Destroy;
end;

end.

