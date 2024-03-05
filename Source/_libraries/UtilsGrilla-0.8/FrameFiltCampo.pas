{Frame que contiene utilidades a realizar sobre grillas TSTringGrid. Actualmente solo se
ha implementado la opción de búsqueda.
Para realizar su trabajo, se debe asociar a un TStringGrid cualquiera. Notar que no es
necesario usar el objeto TUtilGrilla o TUtilGrillaFil.
Para facilitar el uso de este frame, se ha definido que no tenga más dependencias
especiales que BasicGrilla.
Además no se debe interceptar eventos de la grilla, a menos que sea estrcitamente necesario.
Estos e hace previeniendo el caso de que la grilla necesite usar sus eventos o estos estén
siendo usados por alguna otra utilidad.

La forma de utilizar el frame, es insertándolo en el formulario o panel, a modo de
ToolBar. Luego configurarlo:

fraUtilsGrilla1.Inic(grilla);
fraUtilsGrilla1.AgregarColumnaFiltro('Por Código', 1);
fraUtilsGrilla1.AgregarColumnaFiltro('Por Nombre', 2);

También se debe llamar a fraUtilsGrilla1.GridKeyPress(Key), en el evento KeyPress() de la
grilla, si se desea que la búsqueda se haga con solo pulsar una tecla en la grilla.

                                                   Por Tito Hinostroza 28/04/016.
}
unit FrameFiltCampo;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, LCLProc,
  Graphics, Buttons, LCLType, Grids, fgl, UtilsGrilla;
type
  TFiltroGrilla = class
    etiq: string;   //etiqueta  a mostrar
    campo: integer; //campo a usar como filtro
  end;
  TFiltroGrilla_list = specialize TFPGObjectList<TFiltroGrilla>;
  //Modo de filtrado
  TModFiltrado = (
    mfilNone,    //Sin filtro (pasa todo)
    mfil1Pal,    //COincidencia con 1 palabra
    mfil2Pal,    //Coincidencia con 2 palabras
    mfilIgualN,  //Comparación con valor
    mfilMayorN,  //Comparación con valor
    mfilMenorN,  //Comparación con valor
    mfilMayorIN,  //Comparación con valor
    mfilMenorIN,  //Comparación con valor
    mfilDiferN   //Comparación con valor
  );
  { TfraFiltCampo }
  TfraFiltCampo = class(TFrame)
    ComboBox2: TComboBox;
    Edit1: TEdit;
    Panel1: TPanel;
    btnFind: TSpeedButton;
    btnClose: TSpeedButton;

    procedure btnCloseClick(Sender: TObject);

    procedure ComboBox2Change(Sender: TObject);
    function Filtro(const f: integer):boolean;
    procedure Edit1Change(Sender: TObject);
    procedure Edit1Enter(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FrameResize(Sender: TObject);
  private
    grilla :TStringGrid;  //grilla asociada
    proteger: boolean;
    filtros : TFiltroGrilla_list;
    campoAfiltrar: integer;
    modFiltro: TModFiltrado;  //Modo del filtro
    buscar1, buscar2: string;  //palabras de búsqueda
    buscarN: double;  //búsqueda por cantidad
    griFiltrar: TUtilGrilla;
    procedure ActualizarVisibilidadBotones;
    procedure fraFiltCampoCambiaFiltro;
    procedure ModoConTexto(txt0: string);
    procedure ModoSinTexto;
    procedure PreparaFiltro;
    function PreparaCad(const cad: string): string;
  public
    msjeBuscar  : string;   //mensajae inicial que aparecerá en el cuadro de texto
    incluirCateg: boolean;
    txtBusq     : string;  //Texto que se usa para la búsqueda
    OnCambiaFiltro: procedure of object;  //Cuando cambia algún parámetro del filtro
    procedure AgregarColumnaFiltro(NomCampo: string; ColCampo: integer);
    procedure GridKeyPress(var Key: char);  //Para dejar al frame procesar el evento KeyPress de la grilla
    function SinTexto: boolean;
    procedure LeerCamposDeGrilla(cols: TGrillaDBCol_list; indCampoDef: integer);
    procedure Activar(txtIni: string);
    procedure SetFocus; override;
    function Focused: Boolean; override;
  public  //Inicialización
    procedure Inic(grilla0: TStringGrid); virtual;
    procedure Inic(gri: TUtilGrilla; campoDef: integer=-1); virtual;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
{ TfraFiltCampo }
procedure TfraFiltCampo.ActualizarVisibilidadBotones;
begin
  if txtBusq = '' then begin
    //No tiene contenido
    btnFind.Visible:=true;
    btnClose.Visible:=false;
  end else begin
    //Tiene contendio
    btnFind.Visible:=false;
    btnClose.Visible:=true;
  end;
  //ComboBox2.Enabled := filtros.Count>0;
end;
procedure TfraFiltCampo.ModoSinTexto;
//Pone al control de edición en modo "Esperando a que se escriba."
begin
  proteger := true;
  Edit1.Font.Italic := true;
  Edit1.Font.Color := clGray;
  Edit1.Text := msjeBuscar;
  proteger := false;
end;
procedure TfraFiltCampo.ModoConTexto(txt0: string);
begin
  proteger := true;
  Edit1.Font.Italic := false;
  Edit1.Font.Color := clBlack;
  Edit1.Text:=txt0;
  proteger := false;
end;
function TfraFiltCampo.SinTexto: boolean;
{Indica si el control está sin texto de búsqueda.}
begin
  Result := txtBusq='';
end;
procedure TfraFiltCampo.AgregarColumnaFiltro(NomCampo: string;
  ColCampo: integer);
//Agrega un campo para usar como filtro a la lista
var
  fil: TFiltroGrilla;
begin
  fil := TFiltroGrilla.Create;
  fil.etiq  := NomCampo;
  fil.campo := ColCampo;
  filtros.Add(fil);
  ComboBox2.AddItem(NomCampo, nil);
  ComboBox2.ItemIndex:=0;  //selecciona el primero
end;
procedure TfraFiltCampo.btnCloseClick(Sender: TObject);  //Limpia texto
begin
  Edit1.Text:= '';
end;
function TfraFiltCampo.PreparaCad(const cad: string): string;
{Procesa una cadena y la deja lista para compraciones}
begin
  Result := Upcase(trim(cad));
  //Esta parte  puede ser lenta
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
function TfraFiltCampo.Filtro(const f: integer): boolean;
{Rutina que implementa un filtro, de acuerdo al formato requerido por TUtilGrilla.
Esta rutina es llamada por cada fila de la grilla.}
  function ValidaFiltFil1(const busc: string; const buscar1: string): boolean;
  begin
    if Pos(buscar1, busc) <> 0 then begin
      exit(true);
    end else begin  //no coincide
      exit(false);
    end;
  end;
  function ValidaFiltFil2(const busc: string; const buscar1, buscar2: string): boolean;
  begin
    if Pos(buscar1, busc) <> 0 then begin
      //coincide la primera palabra, vemos la segunda
      if Pos(buscar2, busc) <> 0 then begin
        exit(true);
      end else begin  //no coincide
        exit(false);
      end;
    end else begin  //no coincide
      exit(false);
    end;
  end;
var
  tmp: String;
  n: Double;
begin
  case modFiltro of
  mfilNone: exit(true);   //siempre pasa
  mfil1Pal: begin  //búsqueda de una palabra
    tmp := PreparaCad(grilla.Cells[campoAfiltrar, f]);
    exit(ValidaFiltFil1(tmp, buscar1));
  end;
  mfil2Pal: begin  //búsqueda de dos palabras
    tmp := PreparaCad(grilla.Cells[campoAfiltrar, f]);
    exit(ValidaFiltFil2(tmp, buscar1, buscar2));
    //se podría acelerar, si se evita pasar los parámetros "buscar1" y "buscar2".
  end;
  mfilIgualN: begin  //igual a número
    if not TryStrToFloat(grilla.Cells[campoAfiltrar, f], n) then exit(false);
    Result := (n = buscarN);
  end;
  mfilDiferN: begin
    if not TryStrToFloat(grilla.Cells[campoAfiltrar, f], n) then exit(false);
    Result := (n <> buscarN);
  end;
  mfilMenorN: begin  //igual a número
    if not TryStrToFloat(grilla.Cells[campoAfiltrar, f], n) then exit(false);
    Result := (n < buscarN);
  end;
  mfilMayorN: begin  //igual a número
    if not TryStrToFloat(grilla.Cells[campoAfiltrar, f], n) then exit(false);
    Result := (n > buscarN);
  end;
  mfilMenorIN: begin  //igual a número
    if not TryStrToFloat(grilla.Cells[campoAfiltrar, f], n) then exit(false);
    Result := (n <= buscarN);
  end;
  mfilMayorIN: begin  //igual a número
    if not TryStrToFloat(grilla.Cells[campoAfiltrar, f], n) then exit(false);
    Result := (n >= buscarN);
  end;
  end;
end;
procedure TfraFiltCampo.GridKeyPress(var Key: char);
{Debe ser llamado en el evento OnKeyPress, de la grilla, si se desee que el Frame tome el
 control de este evento, iniciando el filtrado con la tecla pulsada.}
begin
  if Key = #13 then exit;   //este código, no debe ser considerado como tecla de edición
  Edit1.SetFocus;
  Edit1.Text := Key;  //sobreescribe lo que hubiera
  Edit1.SelStart:=length(Edit1.Text);
end;
procedure TfraFiltCampo.PreparaFiltro;
{Prepara }
const
  MAX_PAL_BUS = 40;  //tamaño máximo de las palabras de búsqueda, cuando hay más de una
var
  p: Integer;
  buscar, numStr: String;
begin
  ActualizarVisibilidadBotones;
  if (ComboBox2.ItemIndex = -1) or (grilla = nil) or (txtBusq='') then begin
    modFiltro := mfilNone;  //otra manera de decir que no hay filtro
  end else if txtBusq[1] in ['=','>','<'] then begin
    //Búsqueda por número
    if length(txtBusq)<2 then begin  //muy pocos caracteres
      modFiltro := mfilNone;
      exit;
    end;
    campoAfiltrar := filtros[ComboBox2.ItemIndex].campo;
    //Identifica operador y número
    case txtBusq[1] of
    '=': begin
      modFiltro := mfilIgualN;  //otra manera de decir que no hay filtro
      numStr := copy(txtBusq, 2, length(txtBusq));
    end;
    '<': begin
      if txtBusq[2] = '>' then begin
        numStr := copy(txtBusq, 3, length(txtBusq));
        modFiltro := mfilDiferN;
      end else if txtBusq[2] = '=' then begin
        numStr := copy(txtBusq, 3, length(txtBusq));
        modFiltro := mfilMenorIN;
      end else begin
        numStr := copy(txtBusq, 2, length(txtBusq));
        modFiltro := mfilMenorN;
      end;
    end;
    '>': begin
      if txtBusq[2] = '=' then begin
        numStr := copy(txtBusq, 3, length(txtBusq));
        modFiltro := mfilMayorIN;
      end else begin
        numStr := copy(txtBusq, 2, length(txtBusq));
        modFiltro := mfilMayorN;
      end;
    end;
    end;
    //Lee número a comparar
    if not TryStrToFloat(numStr, buscarN) then begin
      modFiltro := mfilNone;
      exit;
    end;
  end else begin
    buscar := PreparaCad(txtBusq);  //simplifica
    campoAfiltrar := filtros[ComboBox2.ItemIndex].campo;
    p := pos(' ', buscar);
    if p = 0 then begin
      //solo hay una palabra de búsqueda
      modFiltro := mfil1Pal;  //otra manera de decir que no hay filtro
      buscar1 := buscar;
    end else begin
      //Hay dos o mas palabras de búsqueda
      modFiltro := mfil2Pal;  //otra manera de decir que no hay filtro
      buscar1 := copy(buscar,1,p-1);
      while buscar[p]= ' ' do
        inc(p);  //Salta espacios. No debería terminar en espacio, porque ya se le aplicó trim()
      buscar2 := copy(buscar, p, MAX_PAL_BUS);  //solo puede leer hasta 40 caracteres
    end;
  end;
end;
procedure TfraFiltCampo.ComboBox2Change(Sender: TObject);
begin
  PreparaFiltro;
  if OnCambiaFiltro<>nil then OnCambiaFiltro;
end;
procedure TfraFiltCampo.Edit1Change(Sender: TObject);
begin
  if proteger then exit;   //para no actualizar
  txtBusq := Edit1.Text;
  PreparaFiltro;
  if OnCambiaFiltro<>nil then OnCambiaFiltro;
end;
procedure TfraFiltCampo.Edit1Enter(Sender: TObject);
begin
  //if (txtBusq='') and (Edit1.Text=msjeBuscar) then begin
    ModoConTexto(txtBusq);
  //end;
end;
procedure TfraFiltCampo.Edit1Exit(Sender: TObject);
begin
  if txtBusq = '' then begin    //No tiene contenido
    ModoSinTexto;
  end;
  ActualizarVisibilidadBotones;
end;
procedure TfraFiltCampo.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //Pasa el evento
  if OnKeyDown<>nil then OnKeyDown(Sender, Key, Shift);
end;
procedure TfraFiltCampo.FrameResize(Sender: TObject);
begin
    Panel1.Height:=Edit1.Height;
    Panel1.Left:=0;
    Panel1.Top :=0;
    Panel1.Width:=self.Width - ComboBox2.Width;
    ComboBox2.Top:=0;
    ComboBox2.Left := self.Width - ComboBox2.Width;
    //Edit1.Align:=alClient;
    Edit1.Left := 0;  //a la izquierda, dentro del panel
    Edit1.Width := Panel1.Width - btnClose.Width;  {Se supone que solo habrá un botón
                                                    visible, así que esto debe funcionar}
    Edit1.Top:=2;
end;
procedure TfraFiltCampo.LeerCamposDeGrilla(cols: TGrillaDBCol_list; indCampoDef: integer);
{Configura todos los campos definidos, menos el 0, como campos para la búsqueda. Solo
se puede aplicar cuando se ha definido un objeto TfraUtilsGrilla, mediante UsarFrameUtils()
}
var
  c: Integer;
begin
  //Agrega filtro de todos los campos, menos el primero
  for c:=1 to cols.Count-1 do begin
    AgregarColumnaFiltro('Por ' + cols[c].nomCampo, c);
  end;
  ComboBox2.ItemIndex:=indCampoDef;
end;
procedure TfraFiltCampo.fraFiltCampoCambiaFiltro;
begin
  if griFiltrar<>nil then griFiltrar.Filtrar;
end;
procedure TfraFiltCampo.Activar(txtIni: string);
{Activa el panel de búsqueda, dándole el enfoque, y poniendo un texto debúsqueda inicial. }
begin
    Edit1.Text:=txtIni;
  if Edit1.Visible then Edit1.SetFocus;
  Edit1.SelStart:=2;
end;
procedure TfraFiltCampo.SetFocus;
begin
//  inherited SetFocus;
  try
    edit1.SetFocus;
  except
  end;
end;
function TfraFiltCampo.Focused: Boolean;
begin
  Result := inherited Focused;
  if Edit1.Focused then Result := true;
end;
procedure TfraFiltCampo.Inic(grilla0: TStringGrid);
{Prepara al frame para iniciar su trabajo. Notar que para evitar conflictos, se ha
definido que no se intercepten los eventos de la grilla, en este Frame.}
begin
  grilla := grilla0;
  ComboBox2.Clear;
  filtros.Clear;
end;
procedure TfraFiltCampo.Inic(gri: TUtilGrilla; campoDef: integer = -1);
{Asocia al frame para trabajar con un objeto TUtilGrilla. "campoDef" es el campo por
defecto que se usará cuando se muestre el filtro.}
begin
  Inic(gri.grilla);
  if campoDef>=0 then begin
    LeerCamposDeGrilla(gri.cols, campoDef);
  end;
  gri.AgregarFiltro(@Filtro);  //agrega su filtro
  {Crea un manejador de evento temporal, para ejecutar el filtro. Este evento puede
  luego reasignarse, de acuerdo a necesida, ya que no es vital. }
  griFiltrar := gri;
  OnCambiaFiltro:=@fraFiltCampoCambiaFiltro;
end;
constructor TfraFiltCampo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnResize:=@FrameResize;
  filtros := TFiltroGrilla_list.Create(true);
  msjeBuscar := 'Texto a buscar';  //Inicia mensaje
  ActualizarVisibilidadBotones;
  ModoSinTexto;
end;
destructor TfraFiltCampo.Destroy;
begin
  filtros.Destroy;
  inherited Destroy;
end;

end.

