{Unidad con definiciones básicas para las clases MiConfigINI y MiConfigXML.
Aquí se define la clase TMiConfigBasic, que incluye los métodos para crear las
asociaciones entre las variables (propiedades) y los controles.
También incluye las rutinas para mover datos entre los controles y las variables.

En teoría, se podría usar esta clase, si no fuera necesario guardar datos a disco,
solamente entre controles y variables:

+------------+                              +------------+
|            | <----PropertiesToWindow----  |            |
| Controles  |                              | Variables  |
|            |  ----WindowToProperties----> |            |
+------------+                              +------------+

Pero como es común salvar los datos a disco, se le debe agregar las funcionalidades
de accesos a disco, como se hace en las unidades MiConfigINI y MiConfigXML.

Las asociaciones entre variables y controles, se hacen con los métodos:
Asoc_Int, Asoc_Dbl, Asoc_Str, ... que están sobrecargados para manejar diversos controles
o crear la asociación sin control.
Solo se han creado asociaciones entre tipos comunes y controles comunes.

Para agregar un nuevo tipo de asociación, en esta unidad se debe:
  1. Crear el identificador de la nueva asociación en el tipo TTipPar.
  2. Crear el nuevo método de asociación (Asoc_XXX) o sobrecargar uno existente.
  3. Actualizar el método TMiConfigBasic.PropertyWindow(), con el nuevo tipo.
Luego también debe implementarse el acceso a disco, para esta nueva asociación, en las
unidades MiConfigINI y MiConfigXML.

                                                 Por Tito Hinostroza 29/07/2016
}
unit MiConfigBasic;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, StdCtrls, Spin, Graphics, MisUtils, EditBtn, Dialogs,
  ExtCtrls, Grids, ColorBox, fgl;
type
  //Tipos de asociaciones
  TTipPar = (
   tp_Int             //Entero sin asociación
  ,tp_Int_TEdit       //entero asociado a TEdit
  ,tp_Int_TSpinEdit   //entero asociado a TSpinEdit
  ,tp_Int_TRadioGroup  //entero asociado a TRadioGroup

  ,tp_Dbl             //Double sin asociación
  ,tp_Dbl_TEdit       //Double asociado a TEdit
  ,tp_Dbl_TFloatSpinEdit   //Double asociado a TFloatSpinEdit

  ,tp_Str             //String sin asociación
  ,tp_Str_TEdit       //string asociado a TEdit
  ,tp_Str_TEditButton //string asociado a TEditButton (ancestro de TFileNameEdit, TDirectoryEdit, ...)
  ,tp_Str_TCmbBox     //string asociado a TComboBox

  ,tp_Bol             //Boleano sin asociación
  ,tp_Bol_TCheckBox   //booleano asociado a CheckBox
  ,tp_Bol_TRadBut     //Booleano asociado a TRadioButton

  ,tp_Enum            //Enumerado sin asociación
  ,tp_Enum_TRadBut    //Enumerado asociado a TRadioButton
  ,tp_Enum_TRadGroup  //Enumerado asociado a TRadioGroup

  ,tp_TCol_TColBut    //TColor asociado a TColorButton
  ,tp_TCol_TColBox    //TColor asociado a TColorBox

  ,tp_StrList         //TStringList sin asociación
  ,tp_StrList_TListBox //StringList asociado a TListBox
  ,tp_StrList_TStringGrid //StringList asociado a TStringGrid
  );

  //Objeto de asociación variable-control

  { TParElem }

  TParElem = class
  private  //Getters and setters
    pCtl: TComponent;  //referencia al control
    radButs: array of TRadioButton;  //referencia a controles TRadioButton (se usan en conjunto)
    minEnt, maxEnt: integer;  //valores máximos y mínimos para variables enteras
    minDbl, maxDbl: Double;  //valores máximos y mínimos para variables Double
    function GetAsBoolean: Boolean;
    function GetAsInteger: integer;
    procedure SetAsBoolean(AValue: Boolean);
    procedure SetAsInteger(AValue: integer);
    function GetAsDouble: double;
    procedure SetAsDouble(AValue: double);
    function GetAsString: string;
    procedure SetAsString(AValue: string);
    function GetAsInt32: Int32;
    procedure SetAsInt32(AValue: Int32);
    function GetAsTColor: TColor;
    procedure SetAsTColor(AValue: TColor);
  public
    pVar   : pointer;  //referencia a la variable
    lVar   : integer;  //tamaño de variable. (Cuando no sea conocido)
    tipPar : TTipPar;  //tipo de par agregado
    etiqVar: string;   //etiqueta usada para grabar la variable en archivo INI o XML
    categ  : integer;  //Categoría. Usada para leer selectivamente con
    //Campos para configurar la grilla, cuando se use
    HasHeader  : boolean;  //Si incluye encabezado
    HasFixedCol: boolean;   //Si tiene una columna fija
    ColCount   : byte;     //Cantidad de columnas para la grilla
    OnPropertyToWindow: procedure of object;
    OnWindowToProperty: procedure of object;
    OnFileToProperty: procedure of object;   //después de guardar el elemento a disco
    OnPropertyToFile: procedure of object;   //antes de guardar el elemento a disco
  public  //valores por defecto
    defInt: integer;   //valor entero por defecto al leer de archivo
    defDbl: Double;    //valor double por defecto al leer de archivo
    defStr: string;    //valor string por defecto al leer de archivo
    defBol: boolean;   //valor booleano por defecto al leer de archivo
    defCol: TColor;    //valor TColor por defecto al leer de archivo
  public  //propiedades para facilitar el acceso a pVar^, usando diversos tipos
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property AsDouble: double read GetAsDouble write SetAsDouble;
    property AsString: string read GetAsString write SetAsString;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsInt32: Int32 read GetAsInt32 write SetAsInt32;
    property AsTColor: TColor read GetAsTColor write SetAsTColor;
  end;
  TParElem_list = specialize TFPGObjectList<TParElem>;

  { TMiConfigBasic }
  TMiConfigBasic = class
  protected
    valInt: integer;  //valor entero de salida
    valDbl: Double;  //valor double de salida
    listParElem : TParElem_list;
    procedure PropertyWindow(r: TParElem; PropToWindow: boolean);
  public  //Rutinas de movimientos entre: Controles <-> Propiedades <-> Archivo
    OnPropertiesChanges: procedure of object; //Cuando se actualizan las propiedades
    function PropertiesToWindow: boolean; virtual;
    function WindowToProperties: boolean; virtual;

  protected  //Rutinas de validación
    function EditValidateInt(edit: TEdit; min: integer=MaxInt; max: integer=-MaxInt): boolean;
    function EditValidateDbl(edit: TEdit; min: Double=0; max: Double=1e6): boolean;
  public   //Métodos para asociar pares: variable-control
    function Asoc_Int(etiq: string; ptrInt: pointer; defVal: integer): TParElem;
    function Asoc_Int(etiq: string; ptrInt: pointer; edit: TEdit;
                      defVal: integer; minVal, maxVal: integer): TParElem;
    function Asoc_Int(etiq: string; ptrInt: pointer; spEdit: TSpinEdit;
                      defVal: integer): TParElem;
    function Asoc_Int(etiq: string; ptrInt: pointer; radGroup: TRadioGroup;
                      defVal: integer): TParElem;
    //---------------------------------------------------------------------
    function Asoc_Dbl(etiq: string; ptrDbl: PDouble; defVal: double): TParElem;
    function Asoc_Dbl(etiq: string; ptrDbl: PDouble; edit: TEdit;
                      defVal: double; minVal, maxVal: double): TParElem;
    function Asoc_Dbl(etiq: string; ptrDbl: PDouble; spEdit: TFloatSpinEdit;
                      defVal: double): TParElem;
    //---------------------------------------------------------------------
    function Asoc_Str(etiq: string; ptrStr: pointer;  defVal: string): TParElem;
    function Asoc_Str(etiq: string; ptrStr: pointer; edit: TCustomEdit;
                      defVal: string): TParElem;
    function Asoc_Str(etiq: string; ptrStr: pointer; edit: TCustomEditButton;
                      defVal: string): TParElem;
    function Asoc_Str(etiq: string; ptrStr: pointer; cmbBox: TComboBox;
                      defVal: string): TParElem;
    //---------------------------------------------------------------------
    function Asoc_Bol(etiq: string; ptrBol: pointer;  defVal: boolean): TParElem;
    function Asoc_Bol(etiq: string; ptrBol: pointer; chk: TCheckBox;
                      defVal: boolean): TParElem;
    function Asoc_Bol(etiq: string; ptrBol: pointer;
                      radButs: array of TRadioButton;  defVal: boolean): TParElem;
    //---------------------------------------------------------------------
    function Asoc_Enum(etiq: string; ptrEnum: pointer; EnumSize: integer; defVal: integer): TParElem;
    function Asoc_Enum(etiq: string; ptrEnum: pointer; EnumSize: integer;
                       radButs: array of TRadioButton;  defVal: integer): TParElem;
    function Asoc_Enum(etiq: string; ptrEnum: pointer; EnumSize: integer;
                       radGroup: TRadioGroup;  defVal: integer): TParElem;
    //---------------------------------------------------------------------
    function Asoc_TCol(etiq: string; ptrTCol: pointer; colBut: TColorButton;
                             defVal: TColor): TParElem;
    function Asoc_TCol(etiq: string; ptrTCol: pointer; colBut: TColorBox;
                             defVal: TColor): TParElem;
    //---------------------------------------------------------------------
    function Asoc_StrList(etiq: string; ptrStrList: pointer): TParElem;
    function Asoc_StrList_TListBox(etiq: string; ptrStrList: pointer; lstBox: TlistBox): TParElem;
  public
    MsjErr: string;    //mensaje de error
    ctlErr: TParElem;  //elemento con error
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TParElem }
function TParElem.GetAsInteger: integer;
begin
  Result := Integer(Pvar^);
end;
procedure TParElem.SetAsInteger(AValue: integer);
begin
  //if FAsInteger=AValue then Exit;
  Integer(Pvar^) := AValue;
end;
function TParElem.GetAsDouble: double;
begin
  Result := Double(Pvar^);
end;
procedure TParElem.SetAsDouble(AValue: double);
begin
  Double(Pvar^) := AValue;
end;
function TParElem.GetAsString: string;
begin
  Result := string(Pvar^);
end;
procedure TParElem.SetAsString(AValue: string);
begin
  string(Pvar^) := AValue;
end;
function TParElem.GetAsTColor: TColor;
begin
  Result := TColor(Pvar^);
end;
procedure TParElem.SetAsTColor(AValue: TColor);
begin
  TColor(Pvar^) := AValue;
end;

function TParElem.GetAsBoolean: Boolean;
begin
  Result := boolean(Pvar^);
end;
procedure TParElem.SetAsBoolean(AValue: Boolean);
begin
  boolean(Pvar^) := AValue;
end;
function TParElem.GetAsInt32: Int32;
begin
  Result := Int32(Pvar^);
end;
procedure TParElem.SetAsInt32(AValue: Int32);
begin
  Int32(pVar^) := AValue;
end;

{ TMiConfigBasic }
procedure TMiConfigBasic.PropertyWindow(r: TParElem; PropToWindow: boolean);
{Implementa el movimiento de datos entre las propiedades y los controles de la ventana
Permite leer o escribir una propiedad, desde o hacia un comtrol}
var
  n, j: Integer;
  list: TStringList;
  gr: TStringGrid;
  spEd: TSpinEdit;
  spFloatEd: TFloatSpinEdit;
  rdGr: TRadioGroup;
begin
  if r.pVar = nil then exit;   //se inició con NIL
  case r.tipPar of
  tp_Int:; //no tiene control asociado
  tp_Int_TEdit:
    if PropToWindow then begin  //entero en TEdit
        //carga entero
        TEdit(r.pCtl).Text:=IntToStr(r.AsInteger);
    end else begin
        if not EditValidateInt(TEdit(r.pCtl),r.minEnt, r.MaxEnt) then
          exit;   //hubo error. con mensaje en "msjErr"
        r.AsInteger := valInt;  //guarda
    end;
  tp_Int_TSpinEdit:
    if PropToWindow then begin  //entero en TSpinEdit
       //carga entero
       TSpinEdit(r.pCtl).Value:= r.AsInteger;
    end else begin
       spEd := TSpinEdit(r.pCtl);
       r.AsInteger := spEd.Value;
    end;
  tp_Int_TRadioGroup:
    if PropToWindow then begin  //entero en TSpinEdit
       //carga entero
       TRadioGroup(r.pCtl).ItemIndex := r.AsInteger;
    end else begin
       rdGr := TRadioGroup(r.pCtl);
       r.AsInteger := rdGr.ItemIndex;
    end;
  //---------------------------------------------------------------------
  tp_Dbl:;
  tp_Dbl_TEdit:
    if PropToWindow then begin
        //carga double
        TEdit(r.pCtl).Text:=FloatToStr(r.AsDouble);
    end else begin
      if not EditValidateDbl(TEdit(r.pCtl),r.minDbl, r.MaxDbl) then
        exit;   //hubo error. con mensaje en "msjErr"
      r.AsDouble := valDbl;  //guarda
    end;
  tp_Dbl_TFloatSpinEdit:
    if PropToWindow then begin
        //carga double
        TFloatSpinEdit(r.pCtl).Value := r.AsDouble;
    end else begin
      spFloatEd := TFloatSpinEdit(r.pCtl);
      //las validaciones de rango las hace el mismo control
      r.AsDouble := spFloatEd.Value;
    end;
  //---------------------------------------------------------------------
  tp_Str:; //no tiene control asociado
  tp_Str_TEdit:
    if PropToWindow then begin  //cadena en TEdit
       //carga cadena
       TEdit(r.pCtl).Text := r.AsString;
    end else begin
       r.AsString := TEdit(r.pCtl).Text;
    end;
  tp_Str_TEditButton:
    if PropToWindow then begin
      //carga cadena
      TEditButton(r.pCtl).Text := r.AsString;
    end else begin
      r.AsString := TEditButton(r.pCtl).Text;
    end;
  tp_Str_TCmbBox:
    if PropToWindow then begin  //cadena en TComboBox
       //carga cadena
       TComboBox(r.pCtl).Text := r.AsString;
    end else begin
       r.AsString := TComboBox(r.pCtl).Text;
    end;
  //---------------------------------------------------------------------
  tp_Bol:; //no tiene control asociado
  tp_Bol_TCheckBox:
    if PropToWindow then begin //boolean a TCheckBox
       TCheckBox(r.pCtl).Checked := r.AsBoolean;
    end else begin
       r.AsBoolean := TCheckBox(r.pCtl).Checked;
    end;
  tp_Bol_TRadBut:
    if PropToWindow then begin //Enumerado a TRadioButtons
        if 1<=High(r.radButs) then begin
          if r.AsBoolean then r.radButs[1].checked := true  //activa primero
          else r.radButs[0].checked := true  //activa segundo
        end;
    end else begin
        //busca el que está marcado
        if high(r.radButs)>=1 then begin
           if r.radButs[1].checked then r.AsBoolean := true
           else r.AsBoolean := false;
        end;
    end;
  //---------------------------------------------------------------------
  tp_Enum:;  //no tiene control asociado
  tp_Enum_TRadBut:
    if PropToWindow then begin //Enumerado a TRadioButtons
        if r.lVar = 4 then begin  //enumerado de 4 bytes
          n := r.AsInt32;  //convierte a entero
          if n<=High(r.radButs) then
            r.radButs[n].checked := true;  //lo activa
        end else begin  //tamño no implementado
          msjErr := dic('Enumerated type no handled.');
          exit;
        end;
    end else begin
        //busca el que está marcado
        for j:=0 to high(r.radButs) do begin
           if r.radButs[j].checked then begin
             //debe fijar el valor del enumerado
             if r.lVar = 4 then begin  //se puede manejar como entero
               r.AsInt32 := j;  //guarda
               break;
             end else begin  //tamaño no implementado
               msjErr := dic('Enumerated type no handled.');
               exit;
             end;
           end;
        end;
    end;
  tp_Enum_TRadGroup:
    if PropToWindow then begin
        if r.lVar = 4 then begin  //enumerado de 4 bytes
          n := r.AsInt32;  //convierte a entero
          if n<TRadioGroup(r.pCtl).Items.Count then
            TRadioGroup(r.pCtl).ItemIndex:=n; //activa
        end else begin  //tamño no implementado
          msjErr := dic('Enumerated type no handled.');
          exit;
        end;
    end else begin
       //debe fijar el valor del enumerado
       if r.lVar = 4 then begin  //se puede manejar como entero
         r.AsInt32 := TRadioGroup(r.pCtl).ItemIndex;  //lee
       end else begin  //tamaño no implementado
         msjErr := dic('Enumerated type no handled.');
         exit;
       end;
    end;
  //---------------------------------------------------------------------
  tp_TCol_TColBut:
    if PropToWindow then begin //Tcolor a TColorButton
       TColorButton(r.pCtl).ButtonColor := r.AsTColor;
    end else begin
       r.AsTColor := TColorButton(r.pCtl).ButtonColor;
    end;
  tp_TCol_TColBox:
    if PropToWindow then begin //Tcolor a TColorButton
       TColorBox(r.pCtl).Selected := r.AsTColor;
    end else begin
       r.AsTColor := TColorBox(r.pCtl).Selected;
    end;
  //---------------------------------------------------------------------
  tp_StrList:; //no tiene control asociado
  tp_StrList_TListBox:
    if PropToWindow then begin  //lista en TlistBox
       //carga lista
       list := TStringList(r.Pvar^);
       TListBox(r.pCtl).Clear;
       for j:=0 to list.Count-1 do
         TListBox(r.pCtl).AddItem(list[j],nil);
    end else begin
      list := TStringList(r.Pvar^);
      list.Clear;
      for j:= 0 to TListBox(r.pCtl).Count-1 do
        list.Add(TListBox(r.pCtl).Items[j]);
    end;
  tp_StrList_TStringGrid:
    if PropToWindow then begin  //lista en TStringGrid
       //carga lista
       list := TStringList(r.Pvar^);
       gr := TStringGrid(r.pCtl);
       gr.Clear;
       gr.BeginUpdate;
       if r.HasFixedCol then gr.FixedCols:=1 else gr.FixedCols:=0;
       gr.ColCount:=r.ColCount;  //fija número de columnas
       if r.HasHeader then begin
         //Hay encabezado
         gr.RowCount:=list.Count+1;  //deja espacio para encabezado
         for j:=0 to list.Count-1 do begin
           gr.Cells[0,j+1] := list[j];
         end;
       end else  begin
         //No hay encabezado
         gr.RowCount:=list.Count;
         for j:=0 to list.Count-1 do begin
           gr.Cells[0,j] := list[j];
         end;
       end;
       gr.EndUpdate();
    end else begin
      //????????
    end;
  else  //no se ha implementado bien
    msjErr := dic('Design error.');
    exit;
  end;
end;
function TMiConfigBasic.PropertiesToWindow: boolean;
{Muestra en los controles, las variables asociadas
Si encuentra error devuelve FALSE, y el mensaje de error en "MsjErr", y el elemento
con error en "ctlErr".}
var
  r: TParElem;
begin
  msjErr := '';
  for r in listParElem do begin
    PropertyWindow(r, true);
    if msjErr<>'' then begin
      ctlErr := r;  //guarda la referencia al elemento, en caso de que haya error
    end;
    if r.OnPropertyToWindow<>nil then r.OnPropertyToWindow;
  end;
  Result := (msjErr='');
end;
function TMiConfigBasic.WindowToProperties: boolean;
{Lee en las variables asociadas, los valores de loc controles
Si encuentra error devuelve FALSE, y el mensaje de error en "MsjErr", y el elemento
con error en "ctlErr".}
var
  r: TParElem;
begin
  msjErr := '';
  for r in listParElem do begin
    PropertyWindow(r, false);
    if msjErr<>'' then begin
      ctlErr := r;  //guarda la referencia al elemento, en caso de que haya error
    end;
    if r.OnWindowToProperty<>nil then r.OnWindowToProperty;
  end;
  //Terminó con éxito. Actualiza los cambios
  if OnPropertiesChanges<>nil then OnPropertiesChanges;
  Result := (msjErr='');  //si hubo error, se habrá actualizado "ctlErr"
end;
//Rutinas de validación
function TMiConfigBasic.EditValidateInt(edit: TEdit; min: integer; max: integer): boolean;
{Valida el contenido de un TEdit, para ver si se puede convertir a un valor entero.
Si no se puede convertir, devuelve FALSE, devuelve el mensaje de error en "MsjErr", y
pone el TEdit con enfoque.
Si se puede convertir, devuelve TRUE, y el valor convertido en "valInt".}
var
  tmp : string;
  c : char;
  signo: string;
  larMaxInt: Integer;
  n: Int64;
begin
  Result := false;
  //validaciones previas
  larMaxInt := length(IntToStr(MaxInt));
  tmp := trim(edit.Text);
  if tmp = '' then begin
    MsjErr:= dic('Field must contain a value.');
    if edit.visible and edit.enabled then edit.SetFocus;
    exit;
  end;
  if tmp[1] = '-' then begin  //es negativo
    signo := '-';  //guarda signo
    tmp := copy(tmp, 2, length(tmp));   //quita signo
  end;
  for c in tmp do begin
    if not (c in ['0'..'9']) then begin
      MsjErr:= dic('Only numeric values are allowed.');
      if edit.visible and edit.enabled then edit.SetFocus;
      exit;
    end;
  end;
  if length(tmp) > larMaxInt then begin
    MsjErr:= dic('Numeric value is too large.');
    if edit.visible and edit.enabled then edit.SetFocus;
    exit;
  end;
  //lo leemos en Int64 por seguridad y validamos
  n := StrToInt64(signo + tmp);
  if n>max then begin
    MsjErr:= dic('The maximun allowed value is: %d', [max]);
    if edit.visible and edit.enabled then edit.SetFocus;
    exit;
  end;
  if n<min then begin
    MsjErr:= dic('The minimun allowed value is: %d', [min]);
    if edit.visible and edit.enabled then edit.SetFocus;
    exit;
  end;
  //pasó las validaciones
  valInt:=n;  //actualiza valor
  Result := true;   //tuvo éxito
end;
function TMiConfigBasic.EditValidateDbl(edit: TEdit; min: Double; max: Double): boolean;
{Valida el contenido de un TEdit, para ver si se puede convertir a un valor Double.
Si no se puede convertir, devuelve FALSE, devuelve el mensaje de error en "MsjErr", y
pone el TEdit con enfoque.
Si se puede convertir, devuelve TRUE, y el valor convertido en "valDbl".}
var
  d: double;
begin
  Result := false;
  //intenta convertir
  if not TryStrToFloat(edit.Text, d) then begin
    MsjErr:= dic('Wrong float number.');
    if edit.visible and edit.enabled then edit.SetFocus;
    exit;
  end;
  //validamos
  if d>max then begin
    MsjErr:= dic('The maximun allowed value is: %f', [max]);
    if edit.visible and edit.enabled then edit.SetFocus;
    exit;
  end;
  if d<min then begin
    MsjErr:= dic('The minimun allowed value is: %f', [min]);
    if edit.visible and edit.enabled then edit.SetFocus;
    exit;
  end;
  //pasó las validaciones
  valDbl:=d;  //actualiza valor
  Result := true;   //tuvo éxito
end;
//Métodos de asociación
function TMiConfigBasic.Asoc_Int(etiq: string; ptrInt: pointer; defVal: integer
  ): TParElem;
//Agrega una variable Entera para guardarla en el archivo.
var
  r: TParElem;
begin
  r := TParElem.Create;
  r.pVar   := ptrInt;  //toma referencia
  r.tipPar := tp_Int;  //tipo de par
  r.etiqVar:= etiq;
  r.defInt := defVal;
  listParElem.Add(r);
  Result := r;
end;
function TMiConfigBasic.Asoc_Int(etiq: string; ptrInt: pointer; edit: TEdit;
  defVal: integer; minVal, maxVal: integer): TParElem;
//Agrega un par variable entera - Control TEdit
begin
  Result := Asoc_Int(etiq, ptrInt, defVal);
  Result.pCtl   := edit;    //toma referencia
  Result.tipPar := tp_Int_TEdit;  //tipo de par
  Result.minEnt := minVal;    //protección de rango
  Result.maxEnt := maxVal;    //protección de rango
end;
function TMiConfigBasic.Asoc_Int(etiq: string; ptrInt: pointer;
  spEdit: TSpinEdit; defVal: integer): TParElem;
//Agrega un par variable entera - Control TSpinEdit
begin
  Result := Asoc_Int(etiq, ptrInt, defVal);
  Result.pCtl   := spEdit;    //toma referencia
  Result.tipPar := tp_Int_TSpinEdit;  //tipo de par
end;
function TMiConfigBasic.Asoc_Int(etiq: string; ptrInt: pointer;
  radGroup: TRadioGroup; defVal: integer): TParElem;
//Agrega un par variable entera - Control TRadioGroup
begin
  Result := Asoc_Int(etiq, ptrInt, defVal);
  Result.pCtl   := radGroup;    //toma referencia
  Result.tipPar := tp_Int_TRadioGroup;  //tipo de par
end;
//---------------------------------------------------------------------
function TMiConfigBasic.Asoc_Dbl(etiq: string; ptrDbl: PDouble; defVal: double
  ): TParElem;
var
  r: TParElem;
begin
  r := TParElem.Create;
  r.pVar   := ptrDbl;  //toma referencia
  r.tipPar := tp_Dbl;  //tipo de par
  r.etiqVar:= etiq;
  r.defDbl := defVal;
  listParElem.Add(r);
  Result := r;
end;
function TMiConfigBasic.Asoc_Dbl(etiq: string; ptrDbl: PDouble; edit: TEdit;
  defVal: double; minVal, maxVal: double): TParElem;
//Agrega un par variable double - Control TEdit
begin
  Result := Asoc_Dbl(etiq, ptrDbl, defVal);
  Result.pCtl   := edit;    //toma referencia
  Result.tipPar := tp_Dbl_TEdit;  //tipo de par
  Result.minDbl := minVal;    //protección de rango
  Result.maxDbl := maxVal;    //protección de rango
end;
function TMiConfigBasic.Asoc_Dbl(etiq: string; ptrDbl: PDouble;
  spEdit: TFloatSpinEdit; defVal: double): TParElem;
begin
  Result := Asoc_Dbl(etiq, ptrDbl, defVal);
  Result.pCtl   := spEdit;    //toma referencia
  Result.tipPar := tp_Dbl_TFloatSpinEdit;  //tipo de par
end;
//---------------------------------------------------------------------
function TMiConfigBasic.Asoc_Str(etiq: string; ptrStr: pointer; defVal: string
  ): TParElem;
//Agrega una variable String para guardarla en el archivo.
var
  r: TParElem;
begin
  r := TParElem.Create;
  r.pVar   := ptrStr;  //toma referencia
  r.tipPar := tp_Str;  //tipo de par
  r.etiqVar:= etiq;
  r.defStr := defVal;
  listParElem.Add(r);
  Result := r;
end;
function TMiConfigBasic.Asoc_Str(etiq: string; ptrStr: pointer;
  edit: TCustomEdit; defVal: string): TParElem;
//Agrega un par variable string - Control TEdit
begin
  Result := Asoc_Str(etiq, ptrStr, defVal);
  Result.pCtl   := edit;    //toma referencia
  Result.tipPar := tp_Str_TEdit;  //tipo de par
end;
function TMiConfigBasic.Asoc_Str(etiq: string; ptrStr: pointer;
  edit: TCustomEditButton; defVal: string): TParElem;
//Agrega un par variable string - Control TEditButton
begin
  Result := Asoc_Str(etiq, ptrStr, defVal);
  Result.pCtl   := edit;    //toma referencia
  Result.tipPar := tp_Str_TEditButton;  //tipo de par
end;
function TMiConfigBasic.Asoc_Str(etiq: string; ptrStr: pointer;
  cmbBox: TComboBox; defVal: string): TParElem;
//Agrega un par variable string - Control TEdit
begin
  Result := Asoc_Str(etiq, ptrStr, defVal);
  Result.pCtl   := cmbBox;   //toma referencia
  Result.tipPar := tp_Str_TCmbBox;  //tipo de par
end;
//---------------------------------------------------------------------
function TMiConfigBasic.Asoc_Bol(etiq: string; ptrBol: pointer; defVal: boolean
  ): TParElem;
var
  r: TParElem;
begin
  r := TParElem.Create;
  r.pVar   := ptrBol;  //toma referencia
  r.tipPar := tp_Bol;  //tipo de par
  r.etiqVar:= etiq;
  r.defBol := defVal;
  listParElem.Add(r);
  Result := r;
end;
function TMiConfigBasic.Asoc_Bol(etiq: string; ptrBol: pointer; chk: TCheckBox;
  defVal: boolean): TParElem;
//Agrega un para variable booleana - Control TCheckBox
begin
  Result := Asoc_Bol(etiq, ptrBol, defVal);
  Result.pCtl   := chk;    //toma referencia
  Result.tipPar := tp_Bol_TCheckBox;  //tipo de par
end;
function TMiConfigBasic.Asoc_Bol(etiq: string; ptrBol: pointer;
  radButs: array of TRadioButton; defVal: boolean): TParElem;
//Agrega un par variable Enumerated - Controles TRadioButton
//Solo se permiten enumerados de hasta 32 bits de tamaño
var
  i: Integer;
begin
  Result := Asoc_Bol(etiq, ptrBol, defVal);
//  Result.pCtl   := ;    //toma referencia
  Result.tipPar := tp_Bol_TRadBut;  //tipo de par
  //guarda lista de controles
  setlength(Result.radButs, high(radButs)+1);  //hace espacio
  for i:=0 to high(radButs) do
    Result.radButs[i]:= radButs[i];
end;
//---------------------------------------------------------------------
function TMiConfigBasic.Asoc_Enum(etiq: string; ptrEnum: pointer;
  EnumSize: integer; defVal: integer): TParElem;
var
  r: TParElem;
begin
  r := TParElem.Create;
  r.pVar   := ptrEnum;  //toma referencia
  r.lVar   := EnumSize;  //necesita el tamaño para modificarlo luego
  r.tipPar := tp_Enum;  //tipo de par
  r.etiqVar:= etiq;
  r.defInt := defVal;   //se maneja como entero
  listParElem.Add(r);
  Result := r;
end;
function TMiConfigBasic.Asoc_Enum(etiq: string; ptrEnum: pointer;
  EnumSize: integer; radButs: array of TRadioButton; defVal: integer): TParElem;
//Agrega un par variable Enumerated - Controles TRadioButton
//Solo se permiten enumerados de hasta 32 bits de tamaño
var
  i: Integer;
begin
  Result := Asoc_Enum(etiq, ptrEnum, EnumSize, defVal);
//  Result.pCtl   := ;    //toma referencia
  Result.tipPar := tp_Enum_TRadBut;  //tipo de par
  //guarda lista de controles
  setlength(Result.radButs, high(radButs)+1);  //hace espacio
  for i:=0 to high(radButs) do
    Result.radButs[i]:= radButs[i];
end;
function TMiConfigBasic.Asoc_Enum(etiq: string; ptrEnum: pointer; EnumSize: integer;
  radGroup: TRadioGroup; defVal: integer): TParElem;
//Agrega un par variable Enumerated - Control TRadioGroup
//Solo se permiten enumerados de hasta 32 bits de tamaño
begin
  Result := Asoc_Enum(etiq, ptrEnum, EnumSize, defVal);
  Result.pCtl   := radGroup;  //toma referencia a control
  Result.tipPar := tp_Enum_TRadGroup;  //tipo de par
end;
//---------------------------------------------------------------------
function TMiConfigBasic.Asoc_TCol(etiq: string; ptrTCol: pointer;
  colBut: TColorButton; defVal: TColor): TParElem;
//Agrega un par variable TColor - Control TColorButton
var
  r: TParElem;
begin
  r := TParElem.Create;
  r.pVar   := ptrTCol;    //toma referencia
  r.pCtl   := colBut;    //toma referencia a control
  r.tipPar := tp_TCol_TColBut;  //tipo de par
  r.etiqVar:= etiq;
  r.defCol := defVal;
  listParElem.Add(r);
  Result := r;
end;
function TMiConfigBasic.Asoc_TCol(etiq: string; ptrTCol: pointer; colBut: TColorBox;
                         defVal: TColor): TParElem;
//Agrega un par variable TColor - Control TColorButton
var
  r: TParElem;
begin
  r := TParElem.Create;
  r.pVar   := ptrTCol;    //toma referencia
  r.pCtl   := colBut;    //toma referencia a control
  r.tipPar := tp_TCol_TColBox;  //tipo de par
  r.etiqVar:= etiq;
  r.defCol := defVal;
  listParElem.Add(r);
  Result := r;
end;
//---------------------------------------------------------------------
function TMiConfigBasic.Asoc_StrList(etiq: string; ptrStrList: pointer
  ): TParElem;
//Agrega una variable TStringList para guardarla en el archivo. El StrinList, debe estar
//ya creado, sino dará error.
var
  r: TParElem;
begin
  r := TParElem.Create;
  r.pVar   := ptrStrList;  //toma referencia
//  r.pCtl   := colBut;    //toma referencia
  r.tipPar := tp_StrList;  //tipo de par
  r.etiqVar:= etiq;
//  r.defCol := defVal;
  listParElem.Add(r);
  Result := r;
end;
function TMiConfigBasic.Asoc_StrList_TListBox(etiq: string;
  ptrStrList: pointer; lstBox: TlistBox): TParElem;
var
  r: TParElem;
begin
  r := TParElem.Create;
  r.pVar   := ptrStrList;  //toma referencia
  r.pCtl   := lstBox;    //toma referencia
  r.tipPar := tp_StrList_TlistBox;  //tipo de par
  r.etiqVar:= etiq;
//  r.defCol := defVal;
  listParElem.Add(r);
  Result := r;
end;
constructor TMiConfigBasic.Create;
begin
  listParElem := TParElem_list.Create(true);
end;
destructor TMiConfigBasic.Destroy;
begin
  listParElem.Destroy;
  inherited Destroy;
end;

end.

