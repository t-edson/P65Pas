{
DESCRIPCIÓN
Incluye la definición del objeto TUtilGrilla, con rutinas comunes para el manejo de
grillas.
}
unit UtilsGrilla;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, Types, Grids, Clipbrd, Menus, Controls,
  Graphics, LCLProc, LCLType, BasicGrilla, MisUtils;
const
  ALT_FILA_DEF = 22;          //Altura por defecto para las grillas de datos
type

  TugTipoCol = (
    ugTipText,    //columna de tipo texto
    ugTipChar,    //columna de tipo caracter
    ugTipNum,     //columna de tipo numérico
    ugTipBol,     //columna de tipo booleano
    ugTipIco,     //columna de tipo ícono
    ugTipDatTim   //columna de tipo fecha-hora
  );

  //Acción a realizar sobre una columna
  TugColAction = (
    ucaRead,    //Leer el valor de la columna
    ucaWrite,   //Escribir un valor en la columna
    ucaValid,   //Validar si un valor, del tipo nativo (número, boolean, ...), es legal.
    ucaValidStr //Validar si una cadena (que se desea asignar a la celda) es legal.
  );
  TugColRestric = (
    ucrNotNull,   //Columna no nula
    ucrUnique     //Unicidad
  );

  {Procedimiento asociado a una columna, para permitir realizar una validación del valor
  que se desea grabar en la columna. Se espera que el valor devuelto, sea el mnesaje de
  error, en caso de que el valor deseado no sea apropiado parala columna.}
  TProcValidacion = function(fil: integer; nuevValor: string): string of object;
  {Procedimientos asociados a una columna, para permitir realizar acciones diversas,
  como la asignación o la validación. }
  TugProcColActionStr = function(actType: TugColAction; AValue: string;
                        col, row: integer): string of object;
  TugProcColActionChr = function(actType: TugColAction; ValidStr: string;
                        col, row: integer; AValue: char): char of object;
  TugProcColActionNum = function(actType: TugColAction; ValidStr: string;
                        col, row: integer; AValue: double): double of object;
  TugProcColActionBool = function(actType: TugColAction; ValidStr: string;
                        col, row: integer; AValue: boolean): boolean of object;
  TugProcColActionDatTim = function(actType: TugColAction; ValidStr: string;
                        col, row: integer; AValue: TDateTime): TDateTime of object;
  { TugGrillaCol }
  {Representa a una columna de la grilla}
  TugGrillaCol = class
    nomCampo: string;     //Nombre del campo de la grilla
    ancho   : integer;    //Ancho físico de la columna de la grilla
    visible : boolean;    //Permite coultar la columna
    alineam : TAlignment; //Alineamiento del campo
    iEncab  : integer;    //índice a columna de la base de datos o texto
    tipo    : TugTipoCol; //Tipo de columna
    idx     : integer;    //Índice dentro de su grilla contenedora (0->la columna de la izquierda)
  public  //campos adicionales
    grilla  : TStringGrid; //Referencia a la grilla de trabajo
    editable: boolean;    //Indica si se puede editar
    valDefec: string;     //Valor por defecto
    formato : string;     //Formato para mostrar una celda, cuando es numérica.
    restric : set of TugColRestric;
  private
    nullStr: string;   //Cadena nula. Se usa como variable auxiliar.
    function GetValStr(iRow: integer): string;
    procedure SetValStr(iRow: integer; AValue: string);
    function GetValChr(iRow: integer): char;
    procedure SetValChr(iRow: integer; AValue: char);
    function GetValNum(iRow: integer): Double;
    procedure SetValNum(iRow: integer; AValue: Double);
    function GetValBool(iRow: integer): boolean;
    procedure SetValBool(iRow: integer; AValue: boolean);
    function GetValDatTim(iRow: integer): TDateTime;
    procedure SetValDatTim(iRow: integer; AValue: TDateTime);
  public //Manejo de lectura y asignación de valores
    {Los siguientes campos, deben asignarse a una función que implemente las acciones
     TugColAction, de acuerdo al tipo del campo. Si no se implementan estos
    procedimientos, no se podrá hacer uso de las propiedades ValStr[] y ValNum[], y
    tampoco se podrá usar la rutina de validación ValidateStr[]. }
    procActionStr   : TugProcColActionStr;
    procActionChr   : TugProcColActionChr;
    procActionNum   : TugProcColActionNum;
    procActionBool  : TugProcColActionBool;
    procActionDatTim: TugProcColActionDatTim;
    //Valor como cadena
    property ValStr[iRow: integer]: string read GetValStr write SetValStr;
    //Valor como cadena
    property ValChr[iRow: integer]: char read GetValChr write SetValChr;
    //Valor como número
    property ValNum[iRow: integer]: double read GetValNum write SetValNum;
    //Valor como BOOLEANO
    property ValBool[iRow: integer]: boolean read GetValBool write SetValBool;
    //Valor como DateTime
    property ValDatTim[iRow: integer]: TDateTime read GetValDatTim write SetValDatTim;
    //Métodos para leer/fijar valor del campo
    procedure SetValue(iRow: integer;  valString: string);
    function GetValue(iRow: integer): string;
    //Rutinas de validación
    procedure ValidateStr(row: integer; NewStr: string);
    procedure ValidateStr(row: integer);
  end;
  TGrillaDBCol_list =   specialize TFPGObjectList<TugGrillaCol>;

  TEvMouseGrilla = procedure(Button: TMouseButton; row, col: integer) of object;

  { TUtilGrillaBase }
  {Este es el objeto principal de la unidad. TUtilGrilla, permite administrar una grilla
   de tipo TStringGrid, agregándole funcionalidades comunes, como el desplazamiento de
   teclado o la creación sencilla de encabezados. Para trabajar con una grilla se tiene
   dos formas:

  1. Asociándola a una grilla desde el inicio:

     UtilGrilla.IniEncab;
     UtilGrilla.AgrEncab('CAMPO1' , 40);  //Con 40 pixeles de ancho
     UtilGrilla.AgrEncab('CAMPO2' , 60);  //Con 60 pixeles de ancho
     UtilGrilla.AgrEncab('CAMPO3' , 35, -1, taRightJustify); //Justificado a la derecha
     UtilGrilla.FinEncab;

  2. Sin asociarla a una UtilGrilla:

     UtilGrilla.IniEncab;
     UtilGrilla.AgrEncab('CAMPO1' , 40);  //Con 40 pixeles de ancho
     UtilGrilla.AgrEncab('CAMPO2' , 60);  //Con 60 pixeles de ancho
     UtilGrilla.AgrEncab('CAMPO3' , 35, -1, taRightJustify); //Justificado a la derecha
     UtilGrilla.FinEncab;

  En esta segunda forma, se debe asociar posteriormente a la UtilGrilla, usando el método:
     UtilGrilla.AsignarGrilla(MiGrilla);

  , haciendo que la grilla tome los encabezados que se definieron en "UtilGrilla". De esta
  forma se pueden tener diversos objetos TUtilGrilla, para usarse en un solo objeto
  TStringGrid.
  }
  TUtilGrillaBase = class
  protected  //campos privados
    FMenuCampos   : boolean;
    popX, popY    : integer;     //posición donde se abre el menú contextual
    procedure SetMenuCampos(AValue: boolean);
    procedure grillaKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure grillaKeyPress(Sender: TObject; var Key: char); virtual;
    procedure grillaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure grillaMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure grillaPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure itClick(Sender: TObject);
  private  //Getters and Setters
    FOpOrdenarConClick: boolean;
    FOpAutoNumeracion: boolean;
    FOpDimensColumnas: boolean;
    FOpEncabezPulsable: boolean;
    FOpResaltarEncabez: boolean;
    FOpResaltFilaSelec: boolean;
    function procDefActionBool(actType: TugColAction; ValidStr: string;
             col, row: integer; AValue: boolean): boolean;
    function procDefActionChr(actType: TugColAction; ValidStr: string;
             col, row: integer; AValue: char): char;
    function procDefActionDatTim(actType: TugColAction; ValidStr: string;
             col, row: integer; AValue: TDateTime): TDateTime;
    function procDefActionNum(actType: TugColAction; ValidStr: string;
             col, row: integer; AValue: double): double;
    function procDefActionStr(actType: TugColAction; AValue: string;
             col, row: integer): string;
    procedure SetOpDimensColumnas(AValue: boolean);
    procedure SetOpAutoNumeracion(AValue: boolean);
    procedure SetOpEncabezPulsable(AValue: boolean);
    procedure SetOpOrdenarConClick(AValue: boolean);
    procedure SetOpResaltarEncabez(AValue: boolean);
    procedure SetOpResaltFilaSelec(AValue: boolean);
  public
    grilla     : TStringGrid;  //referencia a la grila de trabajo
    cols       : TGrillaDBCol_list;  //Información sobre las columnas
    PopupHeadInt: TPopupMenu; //Menú contextual interno, para mostrar/ocultar campos
    PopUpCells : TPopupMenu;  //Menú para las celdas
    OnKeyDown  : TKeyEvent; {Se debe usar este evento en lugar de usar directamente
                            el evento de la grilla, ya que TGrillaDB, usa ese evento.}
    OnKeyPress : TKeyPressEvent; {Se debe usar este evento en lugar de usar directamente
                            el evento de la grilla, ya que TGrillaDB, usa ese evento.}
//    OnDblClick: TKeyPressEvent; {Se debe usar este evento en lugar de usar directamente
//                          el evento de la grilla, ya que TGrillaDB, usa ese evento.}
    OnMouseDown    : TMouseEvent;
    OnMouseUp      : TMouseEvent;
    OnMouseUpCell  : TEvMouseGrilla;
    OnMouseUpHeader: TEvMouseGrilla;
    OnMouseUpFixedCol: TEvMouseGrilla;
    OnMouseUpNoCell: TEvMouseGrilla;
    //Definición de encabezados
    procedure IniEncab;
    function AgrEncab(titulo: string; ancho: integer; indColDat: int16=-1;
      alineam: TAlignment=taLeftJustify): TugGrillaCol; virtual;
    function AgrEncabTxt(titulo: string; ancho: integer; indColDat: int16=-1
      ): TugGrillaCol; virtual;
    function AgrEncabChr(titulo: string; ancho: integer; indColDat: int16=-1
      ): TugGrillaCol; virtual;
    function AgrEncabNum(titulo: string; ancho: integer; indColDat: int16=-1
      ): TugGrillaCol; virtual;
    function AgrEncabBool(titulo: string; ancho: integer; indColDat: int16=-1
      ): TugGrillaCol; virtual;
    function AgrEncabDatTim(titulo: string; ancho: integer; indColDat: int16=-1
      ): TugGrillaCol; virtual;
    procedure FinEncab(actualizarGrilla: boolean=true);
    procedure AsignarGrilla(grilla0: TStringGrid); virtual;  //Configura grilla de trabajo
  public //Opciones de la grilla
    property MenuCampos: boolean     //Activa el menú contextual
             read FMenuCampos write SetMenuCampos;
    property OpDimensColumnas: boolean  //activa el dimensionamiento de columnas
             read FOpDimensColumnas write SetOpDimensColumnas;
    property OpAutoNumeracion: boolean  //activa el autodimensionado en la columna 0
             read FOpAutoNumeracion write SetOpAutoNumeracion;
    property OpResaltarEncabez: boolean  //Resalta el encabezado, cuando se pasa el mouse
             read FOpResaltarEncabez write SetOpResaltarEncabez;
    property OpEncabezPulsable: boolean  //Permite pulsar sobre los encabezados como botones
             read FOpEncabezPulsable write SetOpEncabezPulsable;
    property OpOrdenarConClick: boolean  //Ordenación de filas pulsando en los encabezados
             read FOpOrdenarConClick write SetOpOrdenarConClick;
    property OpResaltFilaSelec: boolean  //Resaltar fila seleccionada
             read FOpResaltFilaSelec write SetOpResaltFilaSelec;
  public //campos auxiliares
    MsjError : string;   //Mensaje de error
    colError : integer;  //Columna con error
    procedure DimensColumnas;
    function BuscarColumna(nombColum: string): TugGrillaCol;
    procedure CopiarCampo;  //copia valor de la celda al portapapeles
    procedure CopiarFila;   //copia valor de la fila al portapapeles
    function PegarACampo: boolean;  //pega del protapapeles al campo
  public //Constructor y destructor
    constructor Create(grilla0: TStringGrid); virtual;
    destructor Destroy; override;
  end;

const
  MAX_UTIL_FILTROS = 10;

type
  TUtilProcFiltro = function(const f: integer):boolean of object;
  {Se crea una clase derivada, para agregar funcionalidades de filtro, y de búsqueda.}
  TUtilGrilla = class(TUtilGrillaBase)
  private
    procedure FiltrarFila(f: integer);
  public
    {Se usa una matriz estática para almacenar a los filtros, para hacer el proceso,
    de filtrado más rápido, ya que se iterará por cada fila de la grilla .}
    filtros: array[0..MAX_UTIL_FILTROS-1] of TUtilProcFiltro;
    numFiltros: integer;
    filVisibles: integer;
    procedure LimpiarFiltros;
    function AgregarFiltro(proc: TUtilProcFiltro): integer;
    procedure Filtrar;
    function BuscarTxt(txt: string; col: integer): integer;
  public
    constructor Create(grilla0: TStringGrid); override;
  end;

  { TUtilGrillaFil }
  {Similar a "TUtilGrilla", pero está orientada a trabajar con datos como filas, más que
  como celdas.
  Además permite cambiar atributos de las filas, como color de fondo, color de texto, etc.
  Para almacenar los atributos de las filas, no crea nuevas variables, sino que usa la
  propiedad "Object", de las celdas, usando las columnas como campos de propiedades para
  la fila. El uso de las columnas es como se indica:
    * Colunna 0-> Almacena el color de fondo de la fila.
    * Colunna 1-> Almacena el color del texto de la fila.
    * Colunna 2-> Almacena los atributos del texto de la fila.
  Por lo tanto se deduce que para manejar estas propiedades, la grilla debe tener las
  columnas necesarias.
  }
  TUtilGrillaFil = class(TUtilGrilla)
  private
    FOpSelMultiFila: boolean;
  protected
    procedure DibCeldaTexto(aCol, aRow: Integer; const aRect: TRect);
    procedure DibCeldaIcono(aCol, aRow: Integer; const aRect: TRect);
    procedure SetOpSelMultiFila(AValue: boolean);
    procedure grillaDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState); virtual;
  public //Opciones de la grilla
    property OpSelMultiFila: boolean  //activa el dimensionamiento de columnas
             read FOpSelMultiFila write SetOpSelMultiFila;
  public
    ImageList: TImageList;   //referecnia a un TInageList, para los íconos
    function AgrEncabIco(titulo: string; ancho: integer; indColDat: int16=-1
      ): TugGrillaCol;
    procedure AsignarGrilla(grilla0: TStringGrid); override;  //Configura grilla de trabajo
    procedure FijColorFondo(fil: integer; color: TColor);  //Color de fondo de la fila
    procedure FijColorFondoGrilla(color: TColor);
    procedure FijColorTexto(fil: integer; color: TColor);  //Color del texto de la fila
    procedure FijColorTextoGrilla(color: TColor);
    procedure FijAtribTexto(fil: integer; negrita, cursiva, subrayadao: boolean);  //Atributos del texto de la fila
    procedure FijAtribTextoGrilla(negrita, cursiva, subrayadao: boolean);  //Atributos del texto de la fila
    function EsFilaSeleccionada(const f: integer): boolean;
  public //Inicialización
    constructor Create(grilla0: TStringGrid); override;
  end;

implementation


{ TugGrillaCol }
function TugGrillaCol.GetValStr(iRow: integer): string;
begin
  Result := procActionStr(ucaRead, nullStr, idx, iRow);
end;
procedure TugGrillaCol.SetValStr(iRow: integer; AValue: string);
begin
  procActionStr(ucaWrite, AValue, idx, iRow);
end;
function TugGrillaCol.GetValChr(iRow: integer): char;
begin
  Result := procActionChr(ucaRead, nullStr, idx, iRow, ' ');
end;
procedure TugGrillaCol.SetValChr(iRow: integer; AValue: char);
begin
  procActionChr(ucaWrite, nullStr, idx, iRow, AValue);
end;
function TugGrillaCol.GetValNum(iRow: integer): Double;
begin
  Result := procActionNum(ucaRead, nullStr, idx, iRow, 0);
end;
procedure TugGrillaCol.SetValNum(iRow: integer; AValue: Double);
begin
  procActionNum(ucaWrite, nullStr, idx, iRow, AValue);
end;
function TugGrillaCol.GetValBool(iRow: integer): boolean;
begin
  Result := procActionBool(ucaRead, nullStr, idx, iRow, false);
end;
procedure TugGrillaCol.SetValBool(iRow: integer; AValue: boolean);
begin
  procActionBool(ucaWrite, nullStr, idx, iRow, AValue);
end;
function TugGrillaCol.GetValDatTim(iRow: integer): TDateTime;
begin
  Result := procActionDatTim(ucaRead, nullStr, idx, iRow, 0);
end;
procedure TugGrillaCol.SetValDatTim(iRow: integer; AValue: TDateTime);
begin
  procActionDatTim(ucaWrite, nullStr, idx, iRow, AValue);
end;
procedure TugGrillaCol.SetValue(iRow: integer; valString: string);
{Fija el valor de la columna en la fila "iRow", a partir de una cadena.}
begin
  case self.tipo of
  ugTipText  : ValStr[iRow]   := valString;
  ugTipBol   : ValBool[iRow]  := f2B(valString);
  ugTipNum   : ValNum[iRow]   := f2N(valString);
  ugTipDatTim: ValDatTim[iRow]:= f2D(valString);
  else
    //Faltan otros tipos
    grilla.Cells[idx, iRow] := valString;
  end;
end;
function TugGrillaCol.GetValue(iRow: integer): string;
{Lee el valor del campo y lo devuelve como cadena, independientemente del tipo que sea.}
begin
  case self.tipo of
  ugTipText  : Result := ValStr[iRow];
  ugTipBol   : Result := B2f(ValBool[iRow]);
  ugTipNum   : Result := N2f(ValNum[iRow]);
  ugTipDatTim: Result := D2f(ValDatTim[iRow]);
  else
    //Faltan otros tipos
    Result := grilla.Cells[idx, iRow];
  end;

end;
procedure TugGrillaCol.ValidateStr(row: integer; NewStr: string);
{Verifica si el valor indicado, como cadena, es legal para ponerlo en la celda que
corresponde. Es aplicable incluisve a celdas que no son del tipo cadena, porque el tipo
básico de las celdas del TStringGrid, es cadema.}
begin
  case tipo of
  ugTipText: begin
    if procActionStr<>nil then procActionStr(ucaValidStr, NewStr, idx, row);
  end;
  ugTipChar: begin
    if procActionChr<>nil then procActionChr(ucaValidStr, NewStr, idx, row, ' ');
  end;
  ugTipNum: begin
    if procActionNum<>nil then procActionNum(ucaValidStr, NewStr, idx, row, 0);
  end;
  ugTipBol: begin
    if procActionBool<>nil then procActionBool(ucaValidStr, NewStr, idx, row, false);
  end;
  ugTipDatTim: begin
    if procActionDatTim<>nil then procActionDatTim(ucaValidStr, NewStr, idx, row, 0);
  end;
  end;
end;
procedure TugGrillaCol.ValidateStr(row: integer);
{Versión que valida el valor que ya existe en la celda.}
begin
  ValidateStr(row, grilla.Cells[idx, row]);
end;
procedure TUtilGrillaBase.grillaKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT] then begin
    //Corrige un comportamiento anómalo de la selección: Cuando se tienen seleccionados
    //varios rangos y se mueve la selecció, los otros rangos no desaparecen.
    if grilla.SelectedRangeCount>1 then begin
      grilla.ClearSelections;
    end;
  end;
  ProcTeclasDireccion(grilla, Key, SHift, ALT_FILA_DEF);
  //Dispara evento
  if OnKeyDown<>nil then OnKeyDown(Sender, Key, Shift);
end;
procedure TUtilGrillaBase.grillaKeyPress(Sender: TObject; var Key: char);
begin
  //Dispara evento
  if OnKeyPress<>nil then OnKeyPress(Sender, Key);
end;
procedure TUtilGrillaBase.grillaMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if OnMouseDown<>nil then OnMouseDown(Sender, Button, Shift, X, Y);
end;
procedure TUtilGrillaBase.grillaMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  it: TMenuItem;
  i: Integer;
  col, row, uf: integer;
  MaxYCoordCeldas: LongInt;
begin
  //Pasa el evento
  if OnMouseUp<>nil then OnMouseUp(Sender, Button, Shift, X, Y);
//debugln('MouseCoordY=' + IntToStr(grilla.MouseCoord(X,y).y));
  //Primero analiza si se está más allá de las celdas existentes.
  {Detectar si el puntero está en una zona sin celdas, no es sencillo, con las funciones
  de TSTringGrid. Se probó con MouseToCell(), MouseToGridZone(), MouseToLogcell(), pero
  todas ellas fallaron.}
  //Valida si escapa de la últiam fila mostrada.
  uf := UltimaFilaVis(grilla);
  if uf<>-1 then begin
    MaxYCoordCeldas := grilla.CellRect(0, uf).Bottom;
//    debugln('MaxYCoordCeldas=%d', [MaxYCoordCeldas]);
    if Y>MaxYCoordCeldas then begin
      //Está fuera de la celda
      if OnMouseUpNoCell<>nil then OnMouseUpNoCell(Button, -1, -1);
      exit;
    end;
  end;
//Debe estar dentro de alguna de las celdas
  grilla.MouseToCell(X, Y, Col, Row );   {Verifica la elda en donde se soltó el mouse.
                                    Aunque MouseToCell(), parece indciar más bien, la celda
                                    seleccionada, cuando se soltó el mouse.}
  if (Row < grilla.FixedRows) and (Col>=grilla.FixedCols) then begin
    //Es el encabezado
    if OnMouseUpHeader<>nil then OnMouseUpHeader(Button, row, col);
    if FMenuCampos and (Button = mbRight) then begin
      {Se ha configurado un menú contextual para los campos.}
      //Configura el menú
      PopupHeadInt.Items.Clear;
      for i:=0 to cols.Count-1 do begin
        it := TMenuItem.Create(PopupHeadInt.Owner);
        it.Caption:=cols[i].nomCampo;
        it.Checked := cols[i].visible;
        it.OnClick:=@itClick;
        PopupHeadInt.Items.Add(it);
      end;
      //Muestra
      popX := Mouse.CursorPos.x;
      popY := Mouse.CursorPos.y;
      PopupHeadInt.PopUp(popX, popY);
    end;
  end else if Col<grilla.FixedCols then begin
    if OnMouseUpFixedCol<>nil then OnMouseUpFixedCol(Button, row, col);
    //En columnas fijas
  end else begin
    //Es una celda común
    if OnMouseUpCell<>nil then OnMouseUpCell(Button, row, col);
    if Button = mbRight then begin
      //Implementa la selección con botón derecho
      grilla.Row:=row;
      grilla.Col:=col;
      if PopUpCells<>nil then PopUpCells.PopUp;
    end;
  end;
end;
procedure TUtilGrillaBase.grillaPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  MyTextStyle: TTextStyle;
begin
  //Activa el alineamiento
  if aRow = 0 then exit;  //no cambia el encabezado
  if aCol>=cols.Count then exit;
  if cols[aCol].alineam <> taLeftJustify then begin
    MyTextStyle := grilla.Canvas.TextStyle;
    MyTextStyle.Alignment := cols[aCol].alineam;
    grilla.Canvas.TextStyle := MyTextStyle;
  end;
end;
procedure TUtilGrillaBase.itClick(Sender: TObject);
{Se hizo click en un ítem del menú de campos}
var
  it: TMenuItem;
  i: Integer;
begin
  it := TMenuItem(Sender);   //debe ser siempre de este tipo
  it.Checked := not it.Checked;
  //Actualiza visibilidad, de acuerdo al menú contextual
  for i:=0 to PopupHeadInt.Items.Count-1 do begin
    cols[i].visible := PopupHeadInt.Items[i].Checked;
  end;
  DimensColumnas;  //dimesiona grillas
  PopupHeadInt.PopUp(popX, popY);  //abre nuevamente, para que no se oculte
end;
procedure TUtilGrillaBase.DimensColumnas;
var
  c: Integer;
begin
  if grilla=nil then exit;
  for c:=0 to cols.Count-1 do begin
    grilla.Cells[c,0] := cols[c].nomCampo;
    if cols[c].visible then  //es visible. Se le asigna ancho
      grilla.ColWidths[c] := cols[c].ancho
    else                //se oculat poniéndole ancho cero.
      grilla.ColWidths[c] := 0;
  end;
end;
procedure TUtilGrillaBase.SetMenuCampos(AValue: boolean);
begin
  //if FMenuCampos=AValue then Exit;
  FMenuCampos:=AValue;
  if grilla<>nil then begin
    //Ya tiene grilla asignada
    if AValue=true then begin
      //Se pide activar el menú contextual
      if PopupHeadInt<>nil then PopupHeadInt.Destroy;  //ya estaba creado
      PopupHeadInt := TPopupMenu.Create(grilla);
    end else begin
      //Se pide desactivar el menú contextual
      if PopupHeadInt<>nil then PopupHeadInt.Destroy;
      PopupHeadInt := nil;
    end;
  end;
end;
procedure TUtilGrillaBase.SetOpDimensColumnas(AValue: boolean);
begin
  FOpDimensColumnas:=AValue;
  if grilla<>nil then begin
    //Ya tiene asignada una grilla
    if AValue then grilla.Options:=grilla.Options + [goColSizing]
    else grilla.Options:=grilla.Options - [goColSizing];
  end;
end;
procedure TUtilGrillaBase.SetOpAutoNumeracion(AValue: boolean);
begin
  FOpAutoNumeracion:=AValue;
  if grilla<>nil then begin
    //Ya tiene asignada una grilla
    if AValue then grilla.Options:=grilla.Options+[goFixedRowNumbering]
    else grilla.Options:=grilla.Options - [goFixedRowNumbering];
  end;
end;
procedure TUtilGrillaBase.SetOpResaltarEncabez(AValue: boolean);
begin
  FOpResaltarEncabez:=AValue;
  if grilla<>nil then begin
    //Ya tiene asignada una grilla
    if AValue then grilla.Options:=grilla.Options+[goHeaderHotTracking]
    else grilla.Options:=grilla.Options - [goHeaderHotTracking];
  end;
end;
procedure TUtilGrillaBase.SetOpEncabezPulsable(AValue: boolean);
begin
  FOpEncabezPulsable:=AValue;
  if grilla<>nil then begin
    //Ya tiene asignada una grilla
    if AValue then grilla.Options:=grilla.Options+[goHeaderPushedLook]
    else grilla.Options:=grilla.Options - [goHeaderPushedLook];
  end;
end;
procedure TUtilGrillaBase.SetOpOrdenarConClick(AValue: boolean);
begin
  FOpOrdenarConClick:=AValue;
  if grilla<>nil then begin
    //Ya tiene asignada una grilla
    grilla.ColumnClickSorts:=AValue;
  end;
end;
procedure TUtilGrillaBase.SetOpResaltFilaSelec(AValue: boolean);
begin
  FOpResaltFilaSelec:=AValue;
  if grilla<>nil then begin
    //Ya tiene asignada una grilla
    if AValue then grilla.Options:=grilla.Options+[goRowHighlight]
    else grilla.Options:=grilla.Options - [goRowHighlight];
  end;
end;
function TUtilGrillaBase.procDefActionStr(actType: TugColAction;
  AValue: string; col, row: integer): string;
var
  f: Integer;
  colum: TugGrillaCol;
begin
  case actType of
  ucaRead: begin  //Se pide leer un valor de la grilla
    Result := grilla.Cells[col, row];  //es cadena
  end;
  ucaWrite: begin  //Se pide escribir un valor en la grilla
    grilla.Cells[col, row] := AValue;  //es cadena
  end;
  ucaValid, ucaValidStr: begin  //Ambas Validaciones son equivalentes.
    colum := cols[col];
    if (ucrNotNull in colum.restric) and (AValue = '') then begin
      MsjError:='Campo "' + colum.nomCampo + '" no puede ser nulo.';
      exit;
    end;
    if ucrUnique in colum.restric then begin  //Unicidad
      for f:=1 to grilla.RowCount-1 do begin
        if f = row then continue;  //No se debe verificar la misma fila
        if grilla.Cells[col, f] = AValue then begin
          MsjError := 'Campo: "' + colum.nomCampo + '" debe ser único.';
          exit;
        end;
      end;
    end;
  end;
  end;
end;
function TUtilGrillaBase.procDefActionChr(actType: TugColAction;
  ValidStr: string; col, row: integer; AValue: char): char;
begin
  case actType of
  ucaRead: begin  //Se pide leer un valor de la grilla
    Result := grilla.Cells[col, row][1];  //es cadena
  end;
  ucaWrite: begin  //Se pide escribir un valor en la grilla
    grilla.Cells[col, row] := AValue;  //es cadena
  end;
  ucaValid: begin   //Se pide validación en tipo nativo
    //Todos los valores son válidos
    exit;
  end;
  ucaValidStr: begin  //Se pide validación
    if length(ValidStr) <> 1 then begin
      MsjError:='Campo "' + cols[col].nomCampo + '" debe ser de un caracter.';
    end;
  end;
  end;
end;
function TUtilGrillaBase.procDefActionNum(actType: TugColAction;
  ValidStr: string; col, row: integer; AValue: double): double;
var
  n: Double;
begin
  case actType of
  ucaRead: begin  //Se pide leer un valor de la grilla
    if grilla.Cells[col, row] = '' then begin
      Result := 0;   //Para cadenas nulas, se asume cero
    end else begin
      Result := StrToFloat(grilla.Cells[col, row]);  //es cadena
    end;
  end;
  ucaWrite: begin  //Se pide escribir un valor en la grilla
    grilla.Cells[col, row] := FloatToStr(AValue);  //Se escribe como cadena.
  end;
  ucaValid: begin   //Se pide validación en tipo nativo
    //Todos los valores son válidos
    exit;
  end;
  ucaValidStr: begin  //Se pide validación en cadena
    if not TryStrToFloat(ValidStr, n) then begin  //debe ser convertible a flotante
      //Hay error en la conversión
      if (ValidStr<>'') and (ValidStr[1] in ['a'..'z','A'..'Z','_']) then begin
        //Parece ser una cadena
        MsjError := 'Campo "' + cols[col].nomCampo + '" debe ser numérico.';
      end else begin
        MsjError := 'Error en formato de: "' + cols[col].nomCampo + '"';
      end;
      exit;
    end;
  end;
  end;
end;
function TUtilGrillaBase.procDefActionBool(actType: TugColAction;
  ValidStr: string; col, row: integer; AValue: boolean): boolean;
begin
  case actType of
  ucaRead: begin  //Se pide leer un valor de la grilla
    Result := (grilla.Cells[col, row] = 'V');  //es cadena
  end;
  ucaWrite: begin  //Se pide escribir un valor en la grilla
    if AValue then
      grilla.Cells[col, row] := 'V'  //Se escribe como cadena.
    else
      grilla.Cells[col, row] := 'F'; //Se escribe como cadena.
  end;
  ucaValid: begin   //Se pide validación en tipo nativo
    //Todos los valores son válidos
    exit;
  end;
  ucaValidStr: begin  //Se pide validación en cadena
    if (ValidStr <> 'V') and (ValidStr <> 'F') then begin
      MsjError := 'Error en formato de: "' + cols[col].nomCampo + '"';
      exit;
    end;
  end;
  end;
end;
function TUtilGrillaBase.procDefActionDatTim(actType: TugColAction;
  ValidStr: string; col, row: integer; AValue: TDateTime): TDateTime;
var
  n: TDateTime;
begin
  case actType of
  ucaRead: begin  //Se pide leer un valor de la grilla
    Result := StrToDateTime(grilla.Cells[col, row]);  //es cadena
  end;
  ucaWrite: begin  //Se pide escribir un valor en la grilla
    grilla.Cells[col, row] := DateTimeToStr(AValue);  //Se escribe como cadena.
  end;
  ucaValid: begin   //Se pide validación en tipo nativo
    //Todos los valores son válidos
    exit;
  end;
  ucaValidStr: begin  //Se pide validación en cadena
    if not TryStrToDateTime(ValidStr, n) then begin  //debe ser convertible a flotante
      MsjError := 'Error en formato de: "' + cols[col].nomCampo + '"';
      exit;
    end;
  end;
  end;
end;
procedure TUtilGrillaBase.IniEncab;
{Inicia el proceso de agregar encabezados a la grilla.}
begin
  cols.Clear;   //Limpia información de columnas
end;
function TUtilGrillaBase.AgrEncab(titulo: string; ancho: integer; indColDat: int16 =-1;
    alineam: TAlignment = taLeftJustify): TugGrillaCol;
{Agrega una celda de encabezado a la grilla y devuelve el campo creado. Esta
función debe ser llamada después de inicializar los enbezados con IniEncab.
Sus parámetros son:
* titulo -> Es el título que aparecerá en la fila de encabezados.
* ancho -> Ancho en pixeles de la columna a definir.
* indColDat -> Número de columna, de una fuente de datos, de donde se leerá este campo}
var
  col: TugGrillaCol;
begin
  //Agrega información de campo
  col := TugGrillaCol.Create;
  col.nomCampo:= titulo;
  col.ancho   := ancho;
  col.visible := true;  //visible por defecto
  col.alineam := alineam;
  col.iEncab  := indColDat;
  col.tipo    := ugTipText;  //texto por defecto
  col.idx     := cols.Count;
  col.editable:= true;   //editable por defecto
  col.grilla  := grilla;  //referencia a grilla
  cols.Add(col);
  Result := col;  //columna usada
end;
function TUtilGrillaBase.AgrEncabTxt(titulo: string; ancho: integer;
  indColDat: int16=-1): TugGrillaCol;
{Crea encabezado de tipo texto. Devuelve el número de columna usada. }
begin
  Result := AgrEncab(titulo, ancho, indColDat);
  {Agrega una rutina para procesar las acciones de esta columna numérica. Así se podrá
   hacer uso del campo ValStr[] y de la rutina ValidateStr(), de TugGrillaCol.
   Si esta rutina es insuficiente, siempre se puede procesar por uno mismo el evento,
   personalizándolo de acuerdo a las necesidades particulares, respetando la forma de
   trabajo.}
  Result.procActionStr:=@procDefActionStr;
end;
function TUtilGrillaBase.AgrEncabChr(titulo: string; ancho: integer;
  indColDat: int16): TugGrillaCol;
begin
  Result := AgrEncab(titulo, ancho, indColDat);
  Result.tipo := ugTipChar;
  Result.procActionChr:=@procDefActionChr;
end;
function TUtilGrillaBase.AgrEncabNum(titulo: string; ancho: integer;
  indColDat: int16=-1): TugGrillaCol;
{Crea encabezado de tipo numérico. Devuelve el número de columna usada. }
begin
  Result := AgrEncab(titulo, ancho, indColDat, taRightJustify);
  Result.tipo := ugTipNum;
  {Agrega una rutina para procesar las acciones de esta columna . Así se podrá
   hacer uso del campo ValNum[] y de la rutina ValidateStr(), de TugGrillaCol.
   Si esta rutina es insuficiente, siempre se puede procesar por uno mismo el evento,
   personalizándolo de acuerdo a las necesidades particulares, respetando la forma de
   trabajo.}
  Result.procActionNum:=@procDefActionNum;
end;
function TUtilGrillaBase.AgrEncabBool(titulo: string; ancho: integer;
  indColDat: int16): TugGrillaCol;
begin
  Result := AgrEncab(titulo, ancho, indColDat);
  Result.tipo := ugTipBol;
  Result.procActionBool:=@procDefActionBool;
end;
function TUtilGrillaBase.AgrEncabDatTim(titulo: string; ancho: integer;
  indColDat: int16): TugGrillaCol;
begin
  Result := AgrEncab(titulo, ancho, indColDat);
  Result.tipo := ugTipDatTim;
  Result.procActionDatTim:=@procDefActionDatTim;
end;
procedure TUtilGrillaBase.FinEncab(actualizarGrilla: boolean = true);
begin
  if actualizarGrilla and (grilla<>nil) then begin
      //Configura las columnas
      grilla.FixedCols:= 1;  //columna de cuenta de filas
      grilla.RowCount := 1;
      grilla.ColCount:=cols.Count;   //Hace espacio
      DimensColumnas;
  end;
end;
procedure TUtilGrillaBase.AsignarGrilla(grilla0: TStringGrid);
{Asigna una grilla al objeto GrillaDB. Al asignarle una nueva grilla, la configura
de acuerdo a los encabezados definidos para este objeto. Se define esta rutina de forma
separada al constructor para poder ejecutarla posteroiormente y tener la posibilidad de
poder cambiar de grilla. Poder cambiar de grilla, nos permite reutilizar una misma grilla
para mostrar información diversa.
Si solo se va a trabajar con una grilla. No es necesario usar este método. Bastará con la
definición de ña grilla en el constructor.}
var
  c: TugGrillaCol;
begin
  grilla := grilla0;
  if cols.Count>0 then begin  //se han definido columnas
    FinEncab(true);  //configura columnas de la grilla
    for c in cols do c.grilla := grilla;   //Actualiza las referencias
  end;
  //Actualiza menú contextual
  SetMenuCampos(FMenuCampos);
  //Actualiza opciones
  SetOpDimensColumnas(FOpDimensColumnas);
  SetOpAutoNumeracion(FOpAutoNumeracion);
  SetOpResaltarEncabez(FOpResaltarEncabez);
  SetOpEncabezPulsable(FOpEncabezPulsable);
  SetOpOrdenarConClick(FOpOrdenarConClick);
  SetOpResaltFilaSelec(FOpResaltFilaSelec);
  //Configura eventos
  grilla.OnPrepareCanvas:=@grillaPrepareCanvas;
  grilla.OnKeyDown:=@grillaKeyDown;
  grilla.OnKeyPress:=@grillaKeyPress;
  grilla.OnMouseUp:=@grillaMouseUp;
  grilla.OnMouseDown:=@grillaMouseDown;
end;
function TUtilGrillaBase.BuscarColumna(nombColum: string): TugGrillaCol;
{Busca una columna por su nombre. Si no la encuentra, devuelve NIL.}
var
  col: TugGrillaCol;
begin
  for col in cols do begin
    if col.nomCampo = nombColum then exit(col);
  end;
  exit(nil);
end;
procedure TUtilGrillaBase.CopiarCampo;
begin
  if (grilla.Row = -1) or  (grilla.Col = -1) then exit;
  Clipboard.AsText:=grilla.Cells[grilla.Col, grilla.Row];
end;
procedure TUtilGrillaBase.CopiarFila;
var
  tmp: String;
  c: Integer;
begin
  if (grilla.Row = -1) or  (grilla.Col = -1) then exit;
  tmp := grilla.Cells[1, grilla.Row];
  for c:=2 to grilla.ColCount-1 do tmp := tmp + #9 + grilla.Cells[c, grilla.Row];
  Clipboard.AsText:=tmp;
end;
function TUtilGrillaBase.PegarACampo: boolean;
{Pega el valor del portapapeles en la celda. Si hubo cambio, devuelve TRUE.}
  function PegaEnCelda(col, row: integer; txt: string): boolean;
  {Pega un valor en la celda indicada, si es diferente.
  Si produce cambios, devuelve TRUE.}
  begin
    Result := false;   //Por defecto, no hay cambios
    if col>grilla.ColCount-1 then exit;;
    if row>grilla.RowCount-1 then exit;
    if not cols[col].editable then exit;
    //Quita caracteres raros al final
    while (txt<>'') and (txt[length(txt)] in [#13,#10,#32]) do
       delete(txt, length(txt), 1);
    if grilla.Cells[col, row] <> txt then begin
      grilla.Cells[col, row] := txt;
      Result := true;  //Hubo cambios
    end;
  end;
var
  txt: String;
  c, cIni: Integer;
  campos: TStringDynArray;
begin
  if (grilla.Row = -1) or  (grilla.Col = -1) then exit;
  txt := Clipboard.AsText;
  //Verifica, si vienen varios campos, en la cadena (Como suele copiar Excel)
  if pos(#9, txt) <> 0 then begin
    //Hay varios campos en el protapapeles
    Result := false;
    campos := explode(#9, txt);  //separa
    cIni := grilla.Col;  //Columna inicial
    for c := 0 to high(campos) do begin  //pega en las columnas
      if PegaEnCelda(cIni + c, grilla.Row, campos[c]) then begin
        Result:= true;  //hubo cambios
      end;
    end;
  end else begin
    //Hay un solo campo
    Result := PegaEnCelda(grilla.Col, grilla.Row, txt);
  end;
end;
//Constructor y destructor
constructor TUtilGrillaBase.Create(grilla0: TStringGrid);
begin
  cols:= TGrillaDBCol_list.Create(true);
  //Configura grilla
  if grilla0<>nil then AsignarGrilla(grilla0);
end;
destructor TUtilGrillaBase.Destroy;
begin
  cols.Destroy;
  //Elimina menú. Si se ha creado
  if PopupHeadInt<>nil then PopupHeadInt.Destroy;
  inherited Destroy;
end;
{ TUtilGrilla }
procedure TUtilGrilla.LimpiarFiltros;
{Elimina todos los filtros}
begin
   numFiltros := 0;
end;
function TUtilGrilla.AgregarFiltro(proc: TUtilProcFiltro): integer;
{Agrega un filtro al arreglo. Devuelve el índice.}
begin
  if numFiltros+1>MAX_UTIL_FILTROS then exit;
  filtros[numFiltros] := proc;
  Result := numFiltros;
  inc(numFiltros);
end;
procedure TUtilGrilla.FiltrarFila(f: integer);
var
  n: Integer;
begin
  //Los filtros se aplican en modo AND, es decir si alguno falla, se oculta
  for n:=0 to numFiltros-1 do begin
    if not Filtros[n](f) then begin
      grilla.RowHeights[f] := 0;
      exit;
    end;
  end;
  //Paso por todos los filtros
  inc(filVisibles);
  grilla.RowHeights[f] := ALT_FILA_DEF;
end;
procedure TUtilGrilla.Filtrar;
{Ejecuta un filtrado, de las filsa de la grilla, usando los filtros, previamente
agregados a TUtilGrilla. Actualza "filVisibles".}
var
  fil: Integer;
begin
  if grilla=nil then exit;  //protección
  if numFiltros = 0 then begin  //sin filtros
    grilla.BeginUpdate;
    for fil:=1 to grilla.RowCount-1 do begin
      grilla.RowHeights[fil] := ALT_FILA_DEF;
    end;
    grilla.EndUpdate();
    exit;
  end;
  grilla.BeginUpdate;
  filVisibles := 0;
  for fil:=1 to grilla.RowCount-1 do begin
    FiltrarFila(fil);
  end;
  grilla.EndUpdate();
  grilla.row := PrimeraFilaVis(grilla);   //selecciona el primero
end;
function TUtilGrilla.BuscarTxt(txt: string; col: integer): integer;
{Realiza la búsqueda de un texto, de forma literal, en la columna indicada. devuelve el
número de fila donde se encuentra. SI no encuentra, devuelve -1.
No busca en los encabezados.}
var
  f: Integer;
begin
  for f:=grilla.FixedRows to grilla.RowCount-1 do begin
    if grilla.Cells[col, f] = txt then exit(f);
  end;
  exit(-1);
end;
constructor TUtilGrilla.Create(grilla0: TStringGrid);
begin
  inherited Create(grilla0);
  LimpiarFiltros;
end;
{ TUtilGrillaFil }
procedure TUtilGrillaFil.DibCeldaIcono(aCol, aRow: Integer; const aRect: TRect);
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
procedure TUtilGrillaFil.DibCeldaTexto(aCol, aRow: Integer; const aRect: TRect);
{Dibuja un texto alineado en la celda "aRect" de la grilla "Self.grilla", usando el
alineamiento de Self.cols[].}
var
  cv: TCanvas;
  txt: String;
  ancTxt: Integer;
begin
  cv := grilla.Canvas;  //referencia al Lienzo
  txt := grilla.Cells[ACol,ARow];
  //escribe texto con alineación
  case cols[aCol].alineam of
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
      cv.TextOut(aRect.Right - ancTxt - 2, aRect.Top + 2, txt);
    end;
  end;
end;
function TUtilGrillaFil.EsFilaSeleccionada(const f: integer): boolean;
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
procedure TUtilGrillaFil.grillaDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  cv: TCanvas;           //referencia al lienzo
  atrib: integer;
begin
  cv := grilla.Canvas;  //referencia al Lienzo
  //txt := grilla.Cells[ACol,ARow];
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
      cv.Brush.Color := clBtnFace;
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
      cv.Pen.Color := clRed;
      cv.Pen.Style := psDot;
      cv.Frame(aRect.Left, aRect.Top, aRect.Right-1, aRect.Bottom-1);  //dibuja borde
    end;
  end;
end;
procedure TUtilGrillaFil.SetOpSelMultiFila(AValue: boolean);
begin
  FOpSelMultiFila:=AValue;
  if grilla<>nil then begin
    //Ya tiene asignada una grilla
    if AValue then grilla.RangeSelectMode := rsmMulti
    else grilla.RangeSelectMode := rsmSingle;
  end;
end;
function TUtilGrillaFil.AgrEncabIco(titulo: string; ancho: integer;
  indColDat: int16): TugGrillaCol;
{Agrega una columna de tipo ícono.}
begin
  Result := AgrEncab(titulo, ancho, indColDat);
  Result.tipo := ugTipIco;
end;
procedure TUtilGrillaFil.AsignarGrilla(grilla0: TStringGrid);
begin
  inherited;
  SetOpSelMultiFila(FOpSelMultiFila);
  //Trabaja con su propia rutina de dibujo
  grilla.DefaultDrawing:=false;
  grilla.OnDrawCell:=@grillaDrawCell;
end;
procedure TUtilGrillaFil.FijColorFondo(fil: integer; color: TColor);
{Fija el color de fondo de la fila indicada. Por defecto es negro.}
begin
  //El color de fondo se almacena en la colunma 1
  if grilla.ColCount<2 then exit;  //protección
  grilla.Objects[0, fil] := TObject(PtrUInt(color));
end;
procedure TUtilGrillaFil.FijColorFondoGrilla(color: TColor);
{Fija el color de fondo de toda la grilla.}
var
  f: Integer;
begin
  if grilla.ColCount<2 then exit;  //protección
  for f:=grilla.FixedRows to grilla.RowCount-1 do begin
    grilla.Objects[0, f] := TObject(PtrUInt(color));
  end;
end;
procedure TUtilGrillaFil.FijColorTexto(fil: integer; color: TColor);
{Fija el color del texto de la fila indicada. Por defecto es negro.}
begin
  //El color de fondo se almacena en la colunma 2
  if grilla.ColCount<3 then exit;  //protección
  grilla.Objects[1, fil] := TObject(PtrUInt(color));
end;
procedure TUtilGrillaFil.FijColorTextoGrilla(color: TColor);
{Fija el color del texto de toda la grilla.}
var
  f: Integer;
begin
  if grilla.ColCount<3 then exit;  //protección
  for f:=grilla.FixedRows to grilla.RowCount-1 do begin
    grilla.Objects[1, f] := TObject(PtrUInt(color));
  end;
end;
procedure TUtilGrillaFil.FijAtribTexto(fil: integer; negrita, cursiva,
  subrayadao: boolean);
{Fija lo satributos del texto de la fila indicada. Por defecto no tiene atributos.}
begin
  //Los atributos se almacenan en la colunma 3
  if grilla.ColCount<4 then exit;  //protección
  grilla.Objects[2, fil] := TObject(ord(negrita)*4+ord(cursiva)*2+ord(subrayadao));
end;
procedure TUtilGrillaFil.FijAtribTextoGrilla(negrita, cursiva,
  subrayadao: boolean);
var
  f: Integer;
begin
  if grilla.ColCount<4 then exit;  //protección
  for f:=grilla.FixedRows to grilla.RowCount-1 do begin
    grilla.Objects[2, f] := TObject(ord(negrita)*4+ord(cursiva)*2+ord(subrayadao));
  end;
end;
constructor TUtilGrillaFil.Create(grilla0: TStringGrid);
begin
  inherited Create(grilla0);
  OpResaltFilaSelec:=true;  //Por defecto trabaja en modo fila
end;

end.

