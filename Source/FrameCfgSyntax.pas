{Frame para la configuración de los elementos de la sintaxis de los editores de texto.
A diferencia de los otros frames, este no trabaja a la manera común, que sería asociar
propiedades a controles, con rutinas de MiConfig.
Aquí se leen directamente las propiedades de los archivos XML de sintaxis, y se cargan
en la lista synLangList. Allí se modifican y solo cuando se pulsa "Aplicar", se vuelca
nuevamente el contenido a disco, sobreescribiendo todo el archivo.
}
unit FrameCfgSyntax;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, LazUTF8, LazFileUtils, Forms, Controls, StdCtrls,
  LCLProc, Graphics, MisUtils, fgl, Types, LCLIntf, Dialogs, SynFacilBasic,
  strutils;
type

  { TSynParam }
  {Registro para modelar a un parámetro (color de texto, de fondo, ....) de un
  atributo. Un atributo contiene varios parámetros.
  El objeto TParamPos, realmente no guarda copia del valor de un parámetro, sino solo
  referencias de posición al objeto lines[], que es el único contenedor del archivo de
  sintaxis. Cuando se actualiza TParamPos, se actualiza directamente el contenido de
  Lines[].
  Se podría pensar que guardar las referencias a lines[] y "nlin", podría ser redundante
  porque todos de los parámetros, están en la misma línea, pero eso es solo cierto para
  los parámetros de un atributo. Los parámetros peuden aparecer también, fuera de los
  atributos (por ejemplo el parámetro "pOpenOnKeyUp"), y ocupar líneas diferentes.
  }
  TSynAttribute = class;

  TSynParam = class
    parName: string;      //Nombre del parámetro
    lines  : TStringList; //Referencia a archivo de sintaxis
    nlin   : integer;     //Índice de línea
    OnModified: procedure(paramModf: TSynParam) of object;  //Cuando se modifica
    function Exist: boolean;
    function ReadString: string;
    function ReadColor: TColor;
    function ReadBool: boolean;
    procedure WriteString(value: string);
    procedure WriteColor(color: TColor);
    procedure WriteBool(value: boolean);
  private
    procedure ReadParamPos(parLabel: string; out p1, p2: integer);
  public
    procedure SetSourcePosition(parName0: string; lines0: TStringList;
      nlin0: integer);
  end;
  TSynParamList = specialize TFPGObjectList<TSynParam>;

  { TSynAttribute }
  {Modela a un atributo (Identificadores, Cadenas, Números, etc.)
  Un archivo de sinatxis contiene varios atributos.
  Un atributo contiene a varios parámetros.}
  TSynAttribute = class
  private
    function GetName: string;
    procedure SetName(AValue: string);
  public
    pName   : TSynParam;
    pTextColor: TSynParam;
    pBackColor: TSynParam;
    pBold   : TSynParam;
    pItalic : TSynParam;
    pUnder  : TSynParam;
  public  //Inicialización
    constructor Create; virtual;
    destructor Destroy; override;
  end;
  TSynAttributeList = specialize TFPGObjectList<TSynAttribute>;

  { TSynLang }
  {Almacena al archivo de sintaxis, y alguna posiciones importantes para poder
   modificarlo. No usa documentos XML, sino que lo maneja el archivo como líneas de
  texto para manteenr el formato, en las líneas no editadas.}
  TSynLang = class
  private
    lines : TStringList;  //Contenedor del archivo de sintaxis
    linComplet: integer;  //línea donde esta <Completion>
    linLangua : integer;  //línea donde esta <Language>
    pName: TSynParam;
    pOpenOnKeyUp: TSynParam;
  public
    filName: string;
    Attributes: TSynAttributeList;  //Lista de atributos
  public
    procedure ReadFromFile(fil: string);
    procedure SaveToFile;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TSynLangList = specialize TFPGObjectList<TSynLang>;

  { TfraCfgSyntax }
  TfraCfgSyntax = class(TFrame)
    chkBold: TCheckBox;
    chkAutoComp: TCheckBox;
    chkItalic: TCheckBox;
    chkUnder: TCheckBox;
    colTextCol: TColorButton;
    colBackCol: TColorButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListBox1: TListBox;
    procedure chkAutoCompChange(Sender: TObject);
    procedure chkBoldChange(Sender: TObject);
    procedure chkItalicChange(Sender: TObject);
    procedure chkUnderChange(Sender: TObject);
    procedure colTextColColorChanged(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    pathSyn: string;
    curLang: TSynLang;   //Sintaxis actual
    curAttr: TSynAttribute;  //Atributo actual
    synLangList: TSynLangList;
    function AddSyntax(synFile: string): TSynLang;
  public
    procedure SaveChanges;
    function GetPropertiesForTheme: string;
    procedure SetPropertiesForTheme(themeFile: string);
  public  //Inicialización
    procedure LoadSyntaxFiles(pathSyn0: string);
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
{ TSynParam }
procedure TSynParam.ReadParamPos(parLabel: string; out p1, p2: integer);
{Lee la ubicación (p1 y p2) del parámetro de nombre "parLabel", en la línea lines[nlin].
 Si no enecuentra, pone p1:=0}
  function BuscarFinDe(cadBusq: string; const lin: string): integer;
  {Busca la cadena "cadBusq" en "lin". Si encuentra, devuelve la posición al final de
  la cadena de búsqueda, saltando espacios.
  Si no encuentra, devuelve 0.}
  var
    p: SizeInt;
  begin
    p := pos(cadBusq, lin);
    if p=0 then begin
      //No enceontró
      exit(0);
    end else begin
      //Encontró la cadena
      p := p + length(cadBusq);  //Para que pase
      //salta blancos
      while (p<=length(lin)) and (lin[p] in [' ',#9]) do begin
        inc(p);
      end;
      //No debería fallar, si ya se cargó (validó) la sintaxis
      if p>length(lin) then begin
        exit(0);
      end;
      //Termina apuntando a la siguiente posición no vacía
      exit(p);
    end;
  end;
var
  carStr: Char;
  lin, cadBuscar: String;
begin
  lin := lines[nlin];
  parName := parLabel;   //actualzia nombre
  //Busca el inicio del parámetro
  lin := UpCase(lin);  //Para realizar la búsqueda sin considera caja
  cadBuscar := UpCase(parLabel + '=');  //construye cadena de búsqueda
  p1 := BuscarFinDe(cadBuscar, lin);
  if p1 <> 0 then begin
    //Debería seguir comilla o doble comilla
    carStr := lin[p1];
    if not (carStr in ['''', '"']) then begin
      p1 := 0;
      exit;
    end;
    //Busca el final de cadena
    p1 := p1 + 1;
    p2 := posEx(carStr, lin, p1+1)-1;
  end;
end;
function TSynParam.Exist: boolean;
var
  p1, p2: integer;
begin
  //Esta verifiación no es muy eficiente, así que usarla con cuidado
  ReadParamPos(parName, p1, p2);  //actualiza p1 y p2
  Result := p1<>0;
end;
function TSynParam.ReadString: string;
var
  p1, p2: integer;
begin
  ReadParamPos(parName, p1, p2);  //actualiza p1 y p2
  if p1 = 0 then exit('');
  Result := copy(lines[nlin], p1, p2 - p1 + 1);
end;
function TSynParam.ReadColor: TColor;
  function EsHexa(txt: string; out num: integer): boolean;
  //Convierte un texto en un número entero. Si es numérico devuelve TRUE
  var
    i: integer;
  begin
    Result := true;  //valor por defecto
    num := 0; //valor por defecto
    for i:=1 to length(txt) do begin
      if not (txt[i] in ['0'..'9','a'..'f','A'..'F']) then exit(false);  //no era
    end;
    //todos los dígitos son numéricos
    num := StrToInt('$'+txt);
  end;
var
  cad: String;
begin
  cad := ReadString;
  if cad='' then exit(clBlack);
  Result := clBlack;  //Color por defecto
  Result := ColorFromStr(cad);
end;
function TSynParam.ReadBool: boolean;
begin
  Result := UpCase(ReadString)='TRUE';
end;
procedure TSynParam.WriteString(value: string);
var
  lin, newline: String;
  p1, p2: integer;
begin
  ReadParamPos(parName, p1, p2);  //actualiza p1 y p2
  if p1 = 0 then exit;
  lin := lines[nlin];
  newline := copy(lin, 1, p1-1)+ value + copy(lin, p2+1, length(lin));
  lines[nlin] := newline;
  if OnModified<>nil then OnModified(self);  //Para que se actuliazen los otros parámetros
end;
procedure TSynParam.WriteColor(color: TColor);
var
  value: String;
  r, g, b: Integer;
begin
  r := color and $FF;
  g := (color >> 8) and $FF;
  b := (color >> 16) and $FF;
  value := '#' + IntToHex(r,2) + IntToHex(g,2) + IntToHex(b,2);
  WriteString(value);
end;
procedure TSynParam.WriteBool(value: boolean);
begin
  if Value then WriteString('True') else WriteString('False');
end;
procedure TSynParam.SetSourcePosition(parName0: string; lines0: TStringList; nlin0: integer);
{Configura la ubicación del parámetro, para que pueda encontrar su valro, cuando
necesite leerlo o modificarlo}
begin
  parName:= parName0;
  lines := lines0;
  nlin  := nlin0;
end;
{ TSynAttribute }
function TSynAttribute.GetName: string;
begin
  Result := pName.ReadString;
end;
procedure TSynAttribute.SetName(AValue: string);
begin
  pName.WriteString(AValue);
end;
//Inicialización
constructor TSynAttribute.Create;
begin
  //Crea y ubica a sus atributos
  pName     := TSynParam.Create;
  pTextColor:= TSynParam.Create;
  pBackColor:= TSynParam.Create;
  pBold     := TSynParam.Create;
  pItalic   := TSynParam.Create;
  pUnder    := TSynParam.Create;
end;
destructor TSynAttribute.Destroy;
begin
  pName.Destroy;
  pTextColor.Destroy;
  pBackColor.Destroy;
  pBold.Destroy;
  pItalic.Destroy;
  pUnder.Destroy;
  inherited Destroy;
end;
{ TSynLang }
procedure TSynLang.ReadFromFile(fil: string);
var
  lin: String;
  i: Integer;
  att: TSynAttribute;
begin
  filName := fil;
  lines.LoadFromFile(fil);
  linComplet := 0;
  for i := 0 to lines.Count-1 do begin
    lin := lines[i];
    if AnsiContainsText(lin, '<Completion') then begin
      linComplet := i;
      pOpenOnKeyUp.SetSourcePosition('OpenOnKeyUp', lines, i );
    end else if AnsiContainsText(lin, '<Language') then begin
      linLangua := i;
      pName.SetSourcePosition('Name', lines, i);
    end else if AnsiContainsText(lin, '<Attribute') then begin
      //Crea el atributo
      att := TSynAttribute.Create;
      att.pName     .SetSourcePosition('Name'     , lines, i);
      att.pTextColor.SetSourcePosition('ForeCol'  , lines, i);
      att.pBackColor.SetSourcePosition('BackCol'  , lines, i);
      att.pBold     .SetSourcePosition('Bold'     , lines, i);
      att.pItalic   .SetSourcePosition('Italic'   , lines, i);
      att.pUnder    .SetSourcePosition('Underline', lines, i);
      Attributes.Add(att);
    end;
  end;
end;
procedure TSynLang.SaveToFile;
{Vuelca el contenido de todo el archivo de este TSynLang, a disco. Las propiedades ya
deben haber sido actualizadas en lines[]}
begin
  lines.SaveToFile(filName);
end;
constructor TSynLang.Create;
begin
  pName       := TSynParam.Create;
  pOpenOnKeyUp:= TSynParam.Create;
  lines := TStringList.Create;
  Attributes:= TSynAttributeList.Create(true);
end;
destructor TSynLang.Destroy;
begin
  pOpenOnKeyUp.Destroy;
  pName.Destroy;
  lines.Destroy;
  Attributes.Destroy;
  inherited Destroy;
end;
{ TfraCfgSyntax }
procedure TfraCfgSyntax.chkAutoCompChange(Sender: TObject);
begin
  if curLang = nil then exit;
  curLang.pOpenOnKeyUp.WriteBool(chkAutoComp.Checked);
end;
procedure TfraCfgSyntax.chkBoldChange(Sender: TObject);
begin
  if curAttr = nil then exit;
  curAttr.pBold.WriteBool(chkBold.Checked);
end;
procedure TfraCfgSyntax.chkItalicChange(Sender: TObject);
begin
  if curAttr = nil then exit;
  curAttr.pItalic.WriteBool(chkItalic.Checked);
end;
procedure TfraCfgSyntax.chkUnderChange(Sender: TObject);
begin
  if curAttr = nil then exit;
  curAttr.pUnder.WriteBool(chkUnder.Checked);
end;
procedure TfraCfgSyntax.colTextColColorChanged(Sender: TObject);
begin
  if curAttr = nil then exit;
  curAttr.pTextColor.WriteColor(colTextCol.ButtonColor);
end;
function  TfraCfgSyntax.GetPropertiesForTheme: string;
{Devuelve en una cadena, las propiedades que se deben guardar como parte de un tema,
como son los colores.
Se usa para obtener información de algunas propiedades para guardarlas como parte de
un tema.}
var
  synLang: TSynLang;
  att: TSynAttribute;
begin
  Result := '';
  for synLang in synLangList do begin
    Result := Result + 'f:' + synLang.filName + LineEnding;
    for att in synLang.Attributes do begin
      //Agrega una línea por atributo
      Result := Result + att.GetName + #9 +
                   I2f(att.pTextColor.ReadColor) + #9 +
                   I2f(att.pBackColor.ReadColor) + #9 +
                   B2f(att.pBold.ReadBool) + #9 +
                   B2f(att.pItalic.ReadBool) + #9 +
                   B2f(att.pUnder.ReadBool) + #9 +
                   #9 + #9 + #9 + //para amplaición
                LineEnding;
    end;
  end;
end;
procedure TfraCfgSyntax.SetPropertiesForTheme(themeFile: string);
{Fija las propiedades que lee GetPropertiesForTheme(), a partir del contenido de un
archivo}
  procedure SetAttribute(fil: string; attribLine: string);
  var
    synLang: TSynLang;
    att: TSynAttribute;
    campos: TStringDynArray;
  begin
    for synLang in synLangList do begin
      if Upcase(ExtractFileNameOnly(synLang.filName)) = Upcase(ExtractFileNameOnly(fil)) then begin
        //Encontró al synLang, que corresponde al archivo
        campos := Explode(#9, attribLine);  //separa campos
        //Ahora debe ubicar al atributo que corresponde "attribLine"
        for att in synLang.Attributes do begin
          if att.GetName = campos[0] then begin
            //Encontró al atributo. Ahora lee los parámetros
            att.pTextColor.WriteColor(f2I(campos[1]));
            att.pBackColor.WriteColor(f2I(campos[2]));
            att.pBold.WriteBool      (f2B(campos[3]));
            att.pItalic.WriteBool    (f2B(campos[4]));
            att.pUnder.WriteBool     (f2B(campos[5]));
          end;
        end;
        //synLang.Attributes;
      end;
    end;
  end;
var
  lin, SyntaxInf, fileNam: String;
  isSyntaxInf: Boolean;
  lineas: TStringList;
begin
  lineas:= TStringList.Create;
  try
    lineas.LoadFromFile(themeFile);
    isSyntaxInf := false;
    SyntaxInf := '';
    for lin in lineas do begin
      if copy(lin,1,2) = 'f:' then isSyntaxInf := true;
      if lin = '' then isSyntaxInf := false;
      if isSyntaxInf then begin
        //Es una línea Propiedad
        if copy(lin,1,2) = 'f:' then begin
          //Es el inicio de un archivo
          fileNam := copy(lin, 3);
        end else begin
          //Debe ser un atributo
          SetAttribute(fileNam, lin);
        end;
        SyntaxInf := SyntaxInf + lin + LineEnding;
      end;
    end;
  finally
    lineas.Destroy;
  end;
end;
procedure TfraCfgSyntax.ComboBox1Change(Sender: TObject);
var
  att: TSynAttribute;
begin
  if ComboBox1.ItemIndex = -1 then begin
    curLang := nil;
    exit;
  end;
  //Actualiza curSynLang
  curLang := synLangList[ComboBox1.ItemIndex];
  //Llena las propiedades
  ListBox1.Clear;
  if curLang.linComplet= 0 then begin
    //No tiene inforrmación de completado
    chkAutoComp.Enabled := false;
  end else begin //Sí tiene completado
    chkAutoComp.Enabled := true;
    chkAutoComp.Checked := curLang.pOpenOnKeyUp.ReadBool;
  end;
  //Llena los atributos encontrados
  for att in curLang.Attributes do begin
    ListBox1.AddItem(att.pName.ReadString, att);  //Guarda referencia al objeto
  end;
  if ListBox1.Count>0 then begin
    ListBox1.ItemIndex := 0;
    ListBox1Click(self);   //Actualiza
  end;
end;
procedure TfraCfgSyntax.ListBox1Click(Sender: TObject);
{Se selecciona un atributo de la lista de atributos.}
var
  att: TSynAttribute;
  Exist: Boolean;
begin
  if ListBox1.ItemIndex = -1 then begin
    curAttr := nil;
    exit;
  end;
  curAttr := nil;  //Se poene en NIl, para evitar disparar eventos en los botones
  att := TSynAttribute(ListBox1.Items.Objects[ListBox1.ItemIndex]);

  //  MsgBox(att.pName.ReadString);
  Exist := att.pTextColor.Exist;
  colTextCol.Enabled := Exist;
  Label2.Enabled := Exist;
  if Exist then colTextCol.ButtonColor := att.pTextColor.ReadColor;

  Exist := att.pBackColor.Exist;
  colBackCol.Enabled := Exist;
  Label4.Enabled := Exist;
  if Exist then  colBackCol.ButtonColor := att.pBackColor.ReadColor;

  Exist := att.pBold.Exist;
  chkBold.Enabled := Exist;
  if Exist then chkBold.Checked := att.pBold.ReadBool;

  Exist := att.pItalic.Exist;
  chkItalic.Enabled := Exist;
  if Exist then chkItalic.Checked := att.pItalic.ReadBool;

  Exist := att.pUnder.Exist;
  chkUnder.Enabled := Exist;
  if Exist then chkUnder.Checked := att.pUnder.ReadBool;
  //Actualiza al final "curAttr".
  curAttr := att;
end;
function TfraCfgSyntax.AddSyntax(synFile: string): TSynLang;
{Agrega una sintaxis a la lista de sintaxis. Devuelve la referecnia a la sinatxis}
var
  synLan: TSynLang;
begin
  try
    //Agrega sintaxis a la lista
    synLan := TSynLang.Create;
    synLan.ReadFromFile(synFile);
    synLangList.add(synLan);
    Result := synLan;
  except
    Result := nil;
  end;
end;
procedure TfraCfgSyntax.LoadSyntaxFiles(pathSyn0: string);
//Carga el contendio de los archivos de sintaxis en "synLangList".
var
  directorio, nomArc: String;
  SearchRec: TSearchRec;
  synt: TSynLang;
begin
  pathSyn := pathSyn0;
  ComboBox1.Clear;
  directorio := pathSyn;
  if FindFirst(directorio + DirectorySeparator + '*.xml', faDirectory, SearchRec) = 0 then begin
    repeat
      nomArc := SysToUTF8(SearchRec.Name);
      if SearchRec.Attr and faDirectory = faDirectory then begin
        //directorio
      end else begin //archivo
        //Agrega la sintaxis
        synt := AddSyntax(directorio + DirectorySeparator + nomArc);
        //Argega nombre de archivo, sin extensión
        nomArc := copy(nomArc, 1, length(nomArc)-4);  //quita extensión
        delete(nomArc,1, 6);  //quita parte inicial
        ComboBox1.AddItem(synt.pName.ReadString, nil);
      end;
    until FindNext(SearchRec) <> 0;
    //Ya no hay más archivos
    FindClose(SearchRec);
  end;
  //Actualiza
  if ComboBox1.Items.Count = 0 then exit;
  ComboBox1.ItemIndex := 0;
  ComboBox1Change(self);
end;
procedure TfraCfgSyntax.SaveChanges;
var
  synLang: TSynLang;
begin
  for synLang in synLangList do begin
    synLang.SaveToFile;
  end;
end;
constructor TfraCfgSyntax.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  synLangList:= TSynLangList.Create(true);
end;
destructor TfraCfgSyntax.Destroy;
begin
  synLangList.Destroy;
  inherited Destroy;
end;

end.

