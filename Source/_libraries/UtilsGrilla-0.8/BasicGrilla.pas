{Rutinas útiles para el manejo de grillas.}
unit BasicGrilla;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, types, LCLType, StdCtrls, Grids, Forms, LCLProc,
  Controls, MisUtils;
type
{ TListaCompletado }
{Representa a una lista con opciones de autocompletado. Usada para llenar a un campo
de tipo TEdit, con ayuda contextual}
TListaCompletado = class
  procedure EditCtrl_DblClick(Sender: TObject);
  procedure EditCtrl_Change(Sender: TObject);
  procedure listBox_DblClick(Sender: TObject);
  procedure listBox_Exit(Sender: TObject);
  procedure listBox_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
    );
  procedure listBox_KeyPress(Sender: TObject; var Key: char);
public
  listBox: TListBox;
  EditCtrl: TEdit;
  OnSelect: procedure of object;      //Para cuando se selecciona un ítem
  OnEditChange: procedure of object;  //Cuando se modifica el control de edición
  OnLlenarLista: procedure of object; //Cuando se solicita llenar al lista
private
  Grilla  : TStringGrid;
  nCol    : integer;
  procedure EditCtrl_Exit(Sender: TObject);
  procedure EditCtrl_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  procedure ElegirDeLista;
public //construcción/inicialización
  procedure Inic(EditCtrl0: TEdit; Grilla0: TStringGrid; nCol0: integer);
  constructor Create;
  destructor Destroy; override;
end;

function CumpleFiltro(const cad: string; const buscar: string): boolean;
//Funciones para el tratamiento de grillas
function PrimeraFilaVis(grilla0: TStringGrid): integer;
function UltimaFilaVis(grilla0: TStringGrid): integer;
function FilaVisAnterior(grilla0: TStringGrid): integer;
function FilaVisSiguiente(grilla0: TStringGrid): integer;
procedure RetrocederAFilaVis(grilla0: TStringGrid);
procedure AdelantarAFilaVis(grilla0: TStringGrid);
procedure MovASiguienteColVis(grilla0: TStringGrid);
procedure MovASiguienteFilVis(grilla0: TStringGrid);

function FiltrarGrilla(grilla0: TStringGrid; buscar: string;
                       campoBusq0, alturaFil0: integer): integer;
procedure ProcTeclasDireccion(grilla0: TStringGrid; var Key: Word;
  Shift: TShiftState; alt_fila: integer);
function AnalizarCeldas(grilla0: TStringGrid): string;
function InformSeleccionGrilla(grilla0: TStringGrid): string;
//Funciones para mover datos de StringList a StringGrid y viceversa
procedure StringListAGrilla(list: TStringList; grilla: TStringGrid; sep:char = #129);
procedure GrillaAStringList(grilla: TStringGrid; list: TStringList; sep:char = #129);
//Funciones para listas
procedure FiltrarEnListBox(gr: TStringGrid;
  colFiltro1: integer     ; const valFiltro1: string; lista: TListBox;
  colFiltro2: integer = -1; const valFiltro2: string = '');

implementation
function CumpleFiltro(const cad: string; const buscar: string): boolean;
  function ValidaFiltFil1(const buscar1: string): boolean;
  begin
    if Pos(buscar1, Upcase(trim(cad))) <> 0 then begin
      exit(true);
    end else begin  //no coincide
      exit(false);
    end;
  end;
  function ValidaFiltFil2(const buscar1, buscar2: string): boolean;
  begin
    if Pos(buscar1, Upcase(trim(cad))) <> 0 then begin
      //coincide la primera palabra, vemos la segunda
      if Pos(buscar2, Upcase(trim(cad))) <> 0 then begin
        exit(true);
      end else begin  //no coincide
        exit(false);
      end;
    end else begin  //no coincide
      exit(false);
    end;
  end;
const
  MAX_PAL_BUS = 40;  //tamaño máximo de las palabras de búsqueda, cuando hay más de una
var
  p: integer;
  pal1: String;
  pal2: String;
  patron: String;
begin
  patron := trim(Upcase(buscar));  //simplifica
  //Valida
  if patron = '' then begin
    //Cadena vacía coincide con todo
    Result := true;
    exit;
  end;
  //Hay término(s) de búsqueda
  p := pos(' ', patron);
  if p = 0 then begin
    //solo hay una palabra de búsqueda
    Result := ValidaFiltFil1(patron);
  end else begin
    //Hay dos o mas palabras de búsqueda
    pal1 := copy(patron,1,p-1);
    while patron[p]= ' ' do
      inc(p);  //Salta espacios. No debería terminar en espacio, porque ya se le aplicó trim()
    pal2 := copy(patron, p, MAX_PAL_BUS);  //solo puede leer hasta 80 caracteres
    Result := ValidaFiltFil2(pal1, pal2);
  end;
end;
//Funciones para el tratamiento de grillas
function PrimeraFilaVis(grilla0: TStringGrid): integer;
{Devuelve la primera fila visible de la grilla.}
var
  f: Integer;
begin
  for f:=grilla0.FixedRows to grilla0.rowcount -1 do begin
    if grilla0.RowHeights[f] <> 0 then exit(f);
  end;
  exit(-1);
end;
function UltimaFilaVis(grilla0: TStringGrid): integer;
{Devuelve la última fila visible de la grilla.}
var
  f: Integer;
begin
  for f:= grilla0.rowcount -1 downto 0 do begin
    if grilla0.RowHeights[f] <> 0 then exit(f);
  end;
  exit(-1);
end;
function FilaVisAnterior(grilla0: TStringGrid): integer;
{Fila visible anterior. Si no hay devuelve -1}
var
  f: Integer;
begin
  f := grilla0.Row;
  if f<=1 then exit(-1);
  repeat
    dec(f);
  until (f<1) or (grilla0.RowHeights[f] <> 0);
  if f<1 then  //no encontró fila anterior
    exit(-1)
  else  //encontró una fila anterior no oculta
    exit(f);
end;
function FilaVisSiguiente(grilla0: TStringGrid): integer;
{Fila visible anterior. Si no hay devuelve -1}
var
  f: Integer;
begin
  f := grilla0.Row;
  if f>=grilla0.RowCount-1 then exit(-1);
  repeat
    inc(f);
  until (f>grilla0.RowCount-1) or (grilla0.RowHeights[f] <> 0);
  if f>grilla0.RowCount-1 then  //no encontró fila anterior
    exit(-1)
  else  //encontró una fila posterior no oculta
    exit(f);
end;
procedure RetrocederAFilaVis(grilla0: TStringGrid);
{Retrocede la fila seleccionada de la grilla, una posición de celda visible.}
var
  f: Integer;
begin
  f := FilaVisAnterior(grilla0);
  if f<>-1 then grilla0.Row := f;
end;
procedure AdelantarAFilaVis(grilla0: TStringGrid);
{Adelanta la fila seleccionada de la grilla, una posición de celda visible.}
var
  f: Integer;
begin
  f := FilaVisSiguiente(grilla0);
  if f<>-1 then grilla0.Row := f;
end;
function PrimeraColVis(grilla0: TStringGrid): integer;
{Devuelve la primera columna visible de la grilla.}
var
  c: Integer;
begin
  for c:=grilla0.FixedCols to grilla0.ColCount -1 do begin
    if grilla0.ColWidths[c] <> 0 then exit(c);
  end;
  exit(-1);
end;
function UltimaColVis(grilla0: TStringGrid): integer;
{Devuelve la primera columna visible de la grilla.}
var
  c: Integer;
begin
  for c:=grilla0.ColCount-1 downto 1 do begin
    if grilla0.ColWidths[c] <> 0 then exit(c);
  end;
  exit(-1);
end;
procedure MovASiguienteColVis(grilla0: TStringGrid);
{Adelanta la columna seleccionada de la grilla, una posición de celda visible.}
var
  c: Integer;
begin
  c := grilla0.Col;
  if c>=grilla0.ColCount-1 then exit;
  repeat
    inc(c);
  until (c>grilla0.ColCount-1) or (grilla0.ColWidths[c] <> 0);
  if c>grilla0.ColCount-1 then  //no encontró columna siguiente
    //deja la misma fila seleccionada
  else  //encontró una columna posterior no oculta
    grilla0.Col := c;
end;
procedure MovASiguienteFilVis(grilla0: TStringGrid);
{Mueve la selección a la siguienet fila visible.}
var
  sig: Integer;
begin
  sig := FilaVisSiguiente(grilla0);
  if sig = -1 then exit;
  grilla0.Row := sig;
end;

function FiltrarGrilla(grilla0: TStringGrid; buscar: string;
                       campoBusq0, alturaFil0: integer): integer;
{Filtra el contenido de la grilla, de acuerdo a una palabra curPass.}
  function ProcesarCad(const cad: string): string;
  {Procesa una cadena y la deja lista para compraciones}
  begin
    Result := Upcase(trim(cad));
    //Esta rutina puede ser lenta
    Result := StringReplace(Result, 'Á', 'A', [rfReplaceAll]);
    Result := StringReplace(Result, 'É', 'E', [rfReplaceAll]);
    Result := StringReplace(Result, 'Í', 'I', [rfReplaceAll]);
    Result := StringReplace(Result, 'Ó', 'O', [rfReplaceAll]);
    Result := StringReplace(Result, 'Ú', 'U', [rfReplaceAll]);
    Result := StringReplace(Result, 'á', 'A', [rfReplaceAll]);
    Result := StringReplace(Result, 'é', 'E', [rfReplaceAll]);
    Result := StringReplace(Result, 'í', 'I', [rfReplaceAll]);
    Result := StringReplace(Result, 'ó', 'O', [rfReplaceAll]);
    Result := StringReplace(Result, 'ú', 'U', [rfReplaceAll]);
  end;
  function ValidaFiltFil1(grilla: TStringGrid; const f: integer; altura: integer; const buscar1: string): boolean;
  var
    tmp: String;
  begin
    tmp := ProcesarCad(grilla.Cells[campoBusq0, f]);
    if Pos(buscar1, tmp) <> 0 then begin
      grilla.RowHeights[f] := altura;
      exit(true);
    end else begin  //no coincide
      grilla.RowHeights[f] := 0;
      exit(false);
    end;
  end;
  function ValidaFiltFil2(grilla: TStringGrid; const f: integer; altura: integer; const buscar1, buscar2: string): boolean;
  var
    tmp: String;
  begin
    tmp := ProcesarCad(grilla.Cells[campoBusq0, f]);
    if Pos(buscar1, tmp) <> 0 then begin
      //coincide la primera palabra, vemos la segunda
      if Pos(buscar2, tmp) <> 0 then begin
        grilla.RowHeights[f] := altura;
        exit(true);
      end else begin  //no coincide
        grilla.RowHeights[f] := 0;
        exit(false);
      end;
    end else begin  //no coincide
      grilla.RowHeights[f] := 0;
      exit(false);
    end;
  end;

const
  MAX_PAL_BUS = 40;  //tamaño máximo de las palabras de búsqueda, cuando hay más de una
var
  f: Integer;
  p: integer;
  pal1, pal2: string[MAX_PAL_BUS];
begin
  //Realiza el filtrado de ítems
  if grilla0=nil then exit(0);
  Result := 0;
  buscar := ProcesarCad(buscar);  //simplifica
  //Valida
  if buscar = '' then begin
    //Cadena vacía coincide con todo
    grilla0.BeginUpdate;
    for f:=1 to grilla0.RowCount-1 do begin
      grilla0.RowHeights[f] := alturaFil0;
    end;
    grilla0.EndUpdate();
    exit(grilla0.RowCount-1);
  end;
  //Hay término(s) de búsqueda
  grilla0.BeginUpdate;
  p := pos(' ', buscar);
  if p = 0 then begin
    //solo hay una palabra de búsqueda
    for f:=1 to grilla0.RowCount-1 do begin
      if ValidaFiltFil1(grilla0, f, alturaFil0, buscar) then inc(Result);
    end;
  end else begin
    //Hay dos o mas palabras de búsqueda
    pal1 := copy(buscar,1,p-1);
    while buscar[p]= ' ' do
      inc(p);  //Salta espacios. No debería terminar en espacio, porque ya se le aplicó trim()
    pal2 := copy(buscar, p, MAX_PAL_BUS);  //solo puede leer hasta 80 caracteres
    for f:=1 to grilla0.RowCount-1 do begin
      if ValidaFiltFil2(grilla0, f, alturaFil0, pal1, pal2) then inc(Result);;
    end;
  end;
  grilla0.EndUpdate();
  grilla0.row := PrimeraFilaVis(grilla0);   //selecciona el primero
end;
procedure ProcTeclasDireccion(grilla0: TStringGrid; var Key: Word;
  Shift: TShiftState; alt_fila: integer);
{Procesa las teclas direccionales para realizar un desplazamiento correcto, en la grilla,
 aún cuando las filas puedan estar filtradas.}
  procedure QuitarSeleccion;
  {Quita la selección de rango y deja solo una celda seleccionada.}
  begin
    grilla0.Selection := Rect(grilla0.Col, grilla0.Row, grilla0.Col, grilla0.Row);
  end;
var
  n: Integer;
  nfilvis: Integer;
begin
  if Key = VK_UP then begin
    if Shift = [] then begin
      //Caso normal, deja que lo procese la grilla
    end else if Shift = [ssCtrl] then begin
      grilla0.row := PrimeraFilaVis(grilla0);
      QuitarSeleccion;   //por alguna razón, la selección múltiple puede quedar activa
      Key := 0;
    end else if Shift = [ssShift, ssCtrl] then begin
      grilla0.row := PrimeraFilaVis(grilla0);
      Key := 0;
    end;
  end else if Key = VK_DOWN then begin
    if Shift = [] then begin
      //Caso normal, deja que lo procese la grilla
    end else if Shift = [ssCtrl] then begin
      grilla0.row := UltimaFilaVis(grilla0);
      QuitarSeleccion;   //por alguna razón, la selección múltiple puede quedar activa
      Key := 0;
    end else if Shift = [ssShift, ssCtrl] then begin
      grilla0.row := UltimaFilaVis(grilla0);
      Key := 0;
    end;
  end else if Key = VK_LEFT then begin
    if Shift = [] then begin
      //Caso normal, deja que lo procese la grilla
    end else if Shift = [ssCtrl] then begin
      grilla0.col := PrimeraColVis(grilla0);
      QuitarSeleccion;   //por alguna razón, la selección múltiple puede quedar activa
      Key := 0;
    end else if Shift = [ssShift, ssCtrl] then begin
      grilla0.col := PrimeraColVis(grilla0);
      Key := 0;
    end;
  end else if Key = VK_RIGHT then begin
    if Shift = [] then begin
      //Caso normal, deja que lo procese la grilla
    end else if Shift = [ssCtrl] then begin
      grilla0.col := UltimaColVis(grilla0);
      QuitarSeleccion;   //por alguna razón, la selección múltiple puede quedar activa
      Key := 0;
    end else if Shift = [ssShift, ssCtrl] then begin
      grilla0.col := grilla0.ColCount-1;
      Key := 0;
    end;
  end else if Key = VK_HOME then begin
    if Shift = [] then begin
      grilla0.col := PrimeraColVis(grilla0);;
      Key := 0;
    end else if Shift = [ssCtrl] then begin
      grilla0.row := PrimeraFilaVis(grilla0);
      grilla0.col := PrimeraColVis(grilla0);
      QuitarSeleccion;   //por alguna razón, la selección múltiple puede quedar activa
    end;
  end else if Key = VK_END then begin
    if Shift = [] then begin
      grilla0.col := UltimaColVis(grilla0);
      Key := 0;
    end else begin
      grilla0.row := UltimaFilaVis(grilla0);
      grilla0.col := UltimaColVis(grilla0);
      Key := 0;
    end;
  end else if (Shift = []) and (Key = VK_PRIOR) then begin
    nfilvis := (grilla0.Height-50) div alt_fila;
    for n:=1 to nfilvis do RetrocederAFilaVis(grilla0);
    Key := 0;
  end else if (Shift = []) and (Key = VK_NEXT) then begin
    nfilvis := (grilla0.Height-50) div alt_fila;
    for n:=1 to nfilvis do AdelantarAFilaVis(grilla0);
    Key := 0;
  end;
end;
function AnalizarCeldas(grilla0: TStringGrid): string;
{Analiza las celdas seleccionadas y devuelve una cadena con información sobre estas celdas.}
  function EsFormatoMoneda(cad: string; var valor: double): boolean;
  {Indica si el texto está en el formato de  moneda. De ser así, lo ocnvierte a número}
  var
    l: Integer;
  begin
    l := length(DefaultFormatSettings.CurrencyString);
    if copy(cad, 1, l) <> DefaultFormatSettings.CurrencyString then
       exit(false);  //no es
    //Puede ser
    cad := copy(cad, l+1, length(cad));
    cad := StringReplace(cad, DefaultFormatSettings.ThousandSeparator, '', [rfReplaceAll]);
    Result := TryStrToFloat(cad, valor);
  end;
  function EsNumero(cad: string; var valor: double): boolean;
  {Indica si el texto está en el formato de  moneda. De ser así, lo ocnvierte a número}
  begin
    Result := TryStrToFloat(cad, valor);
  end;

var
  sum: Double;
  col1: integer;
  fil1, fil2: integer;
  f: integer;
  valor: double;
  cel1: String;
begin
  if grilla0.Selection.Top = grilla0.Selection.Bottom then
     exit('');  //solo hay una fila seleccionada
  //Trata de identificar el tipo de contenido de la selección
  col1 := grilla0.Selection.Left;
  fil1 := grilla0.Selection.Top;
  fil2 := grilla0.Selection.Bottom;
  if fil1 = -1 then exit('');
  cel1 := grilla0.Cells[col1,fil1];
  //Intenta hacer una suma de las celdas
  if EsFormatoMoneda(cel1, valor{%H-}) then begin
    sum := 0;
    for f:=fil1 to fil2 do begin
      if EsFormatoMoneda(grilla0.Cells[col1, f],valor) then begin
        sum := sum + valor;
      end;
    end;
    Result := 'Sum=' + FloatToStrF(sum, ffCurrency, 6, 2);
  end else if EsNumero(cel1, valor) then begin
    sum := 0;
    for f:=fil1 to fil2 do begin
      if EsNumero(grilla0.Cells[col1, f],valor) then begin
        sum := sum + valor;
      end;
    end;
    Result := 'Sum=' + FloatToStr(sum);
  end else begin
    sum := 0;
    for f:=fil1 to fil2 do begin
      if grilla0.Cells[col1, f] <> '' then begin
        sum := sum + 1;
      end;
    end;
    Result := 'Cta=' + FloatToStr(sum);
  end;
end;
function InformSeleccionGrilla(grilla0: TStringGrid): string;
var
  i, cont: Integer;
  sum  : double;
  sel: TGridRect;
  c, f: LongInt;
  errConv: Boolean;
  val: double;
begin
  cont := 0;
  sum := 0;
  errConv := false;
  for i:=0 to grilla0.SelectedRangeCount-1 do begin
    sel := grilla0.SelectedRange[i];
    for c := sel.Left to sel.Right do begin
      if grilla0.ColWidths[c] = 0 then continue;
      for f := sel.top to sel.Bottom do begin
        if grilla0.RowHeights[f] = 0 then continue;
        if TryStrToFloat(grilla0.Cells[c, f], val) then begin
          sum := sum + val;
        end else begin
          errConv := true;
        end;
        inc(cont);
      end;
    end;
  end;
  if errConv then begin  //Hubo error de convrsión
    Result := Format('Cuenta=%d', [cont]);
  end else begin
    Result := Format('Cuenta=%d   Suma=%n   Promedio=%n', [cont, sum, sum/cont]);
  end;
end;
//Funciones para mover datos de StringList a StringGrid y viceversa
procedure StringListAGrilla(list: TStringList; grilla: TStringGrid; sep:char = #129);
{Pasa los datos de una lista a Una grilla. Cada fila de la lista, representará a una
fila de la grilla. Los campos en la lista, deben estar separados por "sep".
No se tocan las filas fijas de la grilla. Los datos se llenan únicamente en las filas que
no son fijas.
Notar que si la grilla tiene menos columnas que las requeridas por la lista, no se
dimensionará la grilla ni se escribirán los datos de las columnas adicionales.}
var
  f: Integer;
  a: TStringDynArray;
  c: Integer;
  lin: String;
  ncol: Integer;
begin
  grilla.BeginUpdate;
  grilla.RowCount:=grilla.FixedRows+list.Count;
  f := grilla.FixedRows;  //fila inicial
  for lin in list do begin
    a:=Explode(sep, lin);
    for c:=grilla.FixedCols to grilla.ColCount-1 do begin
      ncol := c-grilla.FixedCols;
      if ncol<=high(a) then
         grilla.Cells[c, f] := a[ncol];
    end;
    f := f + 1;
  end;
  grilla.EndUpdate();
end;
procedure GrillaAStringList(grilla: TStringGrid; list: TStringList; sep:char = #129);
{Pasa los datos de una grilla a un StringList. Solo mueve los datos de las filas y columnas
que no son fijas. La lista se dimensionará de acuerdo a la cantidad de datos de la grilla.}
var
  f: Integer;
  c: Integer;
  lin: string;
begin
  list.Clear;
  for f:=grilla.FixedRows to grilla.RowCount-1 do begin
    //fusiona los campos en una sola cadena
    lin := '';
    for c:=grilla.FixedCols to grilla.ColCount-1 do begin
      lin := lin + grilla.Cells[c,f] + sep;
    end;
    delete(lin, length(lin), 1);
    //Agrega cadena con separadores
    list.Add(lin);
  end;
end;
//Funciones para listas
procedure FiltrarEnListBox(gr: TStringGrid;
  colFiltro1: integer     ; const valFiltro1: string; lista: TListBox;
  colFiltro2: integer = -1; const valFiltro2: string = '');
{Filtra los valores de una grilla}
  function CumpleFiltro2(f: integer): boolean;
  begin
    if colFiltro2 = -1 then exit(true);
    //Hay filtro
    if gr.Cells[colFiltro2, f] = valFiltro2 then exit(true)
    else exit(false);
  end;
var
  f: Integer;
  nombre: String;
begin
  //debugln('Filtrando: ' + valFiltro1);
  lista.Items.BeginUpdate;
  lista.Clear;
  if gr = nil then exit;
  for f:=1 to gr.RowCount-1 do begin
    nombre := gr.Cells[colFiltro1, f];
    if CumpleFiltro(nombre , valFiltro1) and CumpleFiltro2(f) then
      lista.AddItem(nombre, nil);
  end;
  if lista.Count>0 then
    lista.ItemIndex:=0;  //selecciona el primer elemento
  lista.Items.EndUpdate;
end;
{ TListaCompletado }
procedure TListaCompletado.ElegirDeLista;
{Elige el elemento seleccioando de la lista}
var
  nomb: String;
begin
  if ListBox.ItemIndex<>-1 then begin
    nomb := ListBox.Items[ListBox.ItemIndex];
    EditCtrl.Text := nomb;
    listBox.Visible:=false;
    if OnSelect<>nil then OnSelect;
  end;
end;
procedure TListaCompletado.EditCtrl_DblClick(Sender: TObject);
begin
  EditCtrl.SelectAll;
  EditCtrl_Change(self);  //para que se abra la lista
end;
procedure TListaCompletado.EditCtrl_Change(Sender: TObject);
begin
  if OnEditChange<>nil then OnEditChange;
  if not EditCtrl.Focused then begin
    exit;  //Para evitar que se active cuando lo modifica el ListBox
  end;
  {if EditCtrl.Text = '' then begin
    ListBox.Visible:=false;
    exit;
  end;}
  ListBox.Left:=EditCtrl.Left;
  ListBox.Top:=EditCtrl.Top + EditCtrl.Height;
  listBox.Width:=EditCtrl.Width;
  if OnLlenarLista<>nil then OnLlenarLista  //Se da prioridad al evento
  else FiltrarEnListBox(grilla, nCol, EditCtrl.Text, ListBox);
  ListBox.Visible:=true;
  //ListBox.SetFocus;
end;
procedure TListaCompletado.EditCtrl_KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin
    listBox.Visible:=false;
    EditCtrl.SetFocus;
  end else if Key = VK_DOWN then begin
    if ListBox.Visible then ListBox.SetFocus;
  end else if Key = VK_RETURN then begin
    ElegirDeLista;
    Key := 0;
  end;
end;
procedure TListaCompletado.EditCtrl_Exit(Sender: TObject);
var
  tieneEnfoque: TWinControl;
begin
  //Identifica al Form padre, usando "EditCtrl"
  if EditCtrl.Owner is TForm then begin
    tieneEnfoque := TForm(EditCtrl.Owner).ActiveControl;
  end else begin
    tieneEnfoque := nil;
  end;
  //Verifica si el que tiene el enfoque es el "listBox"
  if tieneEnfoque <> listBox then begin
    //Si el enfoque pasó a otro control que no sea la lista
    ListBox.Visible:=false;  //protección
  end;
end;
procedure TListaCompletado.listBox_DblClick(Sender: TObject);
var
  nomb: String;
begin
  if ListBox.ItemIndex<>-1 then begin
    nomb := ListBox.Items[ListBox.ItemIndex];
    EditCtrl.Text := nomb;
    listBox.Visible:=false;
    if OnSelect<>nil then OnSelect;
  end;
end;
procedure TListaCompletado.listBox_Exit(Sender: TObject);
begin
  listBox.Visible:=false;
end;
procedure TListaCompletado.listBox_KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin
    listBox.Visible:=false;
    EditCtrl.SetFocus;
  end;
end;
procedure TListaCompletado.listBox_KeyPress(Sender: TObject; var Key: char);
begin
  if Key = #8 then begin
    EditCtrl.Text := copy(EditCtrl.Text, 1, length(EditCtrl.Text)-1);
    if OnLlenarLista<>nil then OnLlenarLista  //Se da prioridad al evento
    else FiltrarEnListBox(grilla, nCol, EditCtrl.Text, ListBox);
    if EditCtrl.Text = '' then begin
      listBox.Visible:=false;
      EditCtrl.SetFocus;
    end;
  end else if Key = #27 then begin
    listBox.Visible:=false;
  end else if Key = #13 then begin
    ElegirDeLista;
  end else begin
    EditCtrl.Text := EditCtrl.Text + Key;
    if OnLlenarLista<>nil then OnLlenarLista  //Se da prioridad al evento
    else FiltrarEnListBox(grilla, nCol, EditCtrl.Text, ListBox);
  end;
end;
procedure TListaCompletado.Inic(EditCtrl0: TEdit; Grilla0: TStringGrid; nCol0: integer);
{Inicia las propiedades del objeto. Se debe llamar inmediátamente después de la
 construcción.}
begin
  listBox.Parent := EditCtrl0.Parent;   //toma al mismo padre, como su padre
  EditCtrl := EditCtrl0;
  Grilla   := Grilla0;
  nCol     := nCol0;
  //Configura lista
  ListBox.Visible:=false;
  ListBox.Height:=120;
  ListBox.OnKeyDown  := @listBox_KeyDown;
  ListBox.OnKeyPress := @listBox_KeyPress;
  listBox.OnExit     := @listBox_Exit;
  listBox.OnDblClick := @listBox_DblClick;
  listBox.BringToFront;
  EditCtrl.OnChange  := @EditCtrl_Change;
  EditCtrl.OnDblClick:= @EditCtrl_DblClick;
  EditCtrl.OnKeyDown := @EditCtrl_KeyDown;
  EditCtrl.OnExit:=@EditCtrl_Exit;
end;
constructor TListaCompletado.Create;
begin
  listBox:= TListBox.Create(nil);  //crea la lista sin propietario. Lo destruiremos nosotros.
end;
destructor TListaCompletado.Destroy;
begin
  listBox.Destroy;
  inherited Destroy;
end;

end.

