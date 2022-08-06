{Modelo de formulario de configuración que usa dos Frame de configuración}
unit FormConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, SynEdit, Forms, Controls, Graphics,
  Dialogs, Buttons, StdCtrls, ExtCtrls, ComCtrls, ColorBox, LCLType,
  FrameCfgSynEdit, Globales, FrameCfgSyntax, FrameCfgExtTool, fgl,
  MiConfigBasic, MiConfigXML, MisUtils, adapterBase;
type
  //Tipo de Barra de herramientas
  TStyleToolbar = (stb_SmallIcon, stb_BigIcon);
  //Lista de adaptadores registrados
  TAdaptadores = specialize TFPGObjectList<TAdapterBase>;
  { TConfig }
  TConfig = class(TForm)
  published
    BitAplicar: TBitBtn;
    BitCancel: TBitBtn;
    BitAceptar: TBitBtn;
    butSaveCurThem: TButton;
    butLoadTheme: TButton;
    chkLoadLast: TCheckBox;
    chkShowErrMsg: TCheckBox;
    colCodExplBack: TColorBox;
    colCodExplText: TColorBox;
    colMessPanBack: TColorBox;
    colMessPanErr: TColorBox;
    colPanelsCol: TColorBox;
    colMessPanSel: TColorBox;
    colMessPanText: TColorBox;
    colPanTextCol: TColorBox;
    colSplitCol: TColorBox;
    cmbLanguage: TComboBox;
    txtThemLoaded: TEdit;
    grpTabEdiState: TRadioGroup;
    ImageList1: TImageList;
    Label2: TLabel;
    lblThemLoad: TLabel;
    lblCodExplCol1: TLabel;
    lblCodExplCol2: TLabel;
    lblMessPan1: TLabel;
    lblMessPan2: TLabel;
    lblMessPan3: TLabel;
    lblMessPan4: TLabel;
    lblPanelCol: TLabel;
    lblSplitCol: TLabel;
    lblSplitCol1: TLabel;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    grpToolbarSta: TRadioGroup;
    grpFilType: TRadioGroup;
    sclEnvThemes: TScrollBox;
    sclExtTool: TScrollBox;
    sclEdiAppear: TScrollBox;
    sclCompiler: TScrollBox;
    sclCmpExtra1: TScrollBox;
    sclCmpExtra2: TScrollBox;
    sclCmpExtra3: TScrollBox;
    sclEdiSyntax: TScrollBox;
    sclEnvExtra1: TScrollBox;
    sclEdiExtra1: TScrollBox;
    sclEnvFilExp: TScrollBox;
    sclEditor: TScrollBox;
    sclEnvMesPan: TScrollBox;
    sclEnviron: TScrollBox;
    tabEditor: TTabSheet;
    tabEdiAppear: TTabSheet;
    tabCmpExtra1: TTabSheet;
    tabCompiler: TTabSheet;
    tabEnviron: TTabSheet;
    tabExtTool: TTabSheet;
    tabEnvMesPan: TTabSheet;
    tabEnvFilExp: TTabSheet;
    tabEdiSyntax: TTabSheet;
    tabEdiExtra1: TTabSheet;
    tabCmpExtra2: TTabSheet;
    tabCmpExtra3: TTabSheet;
    tabEnvExtra1: TTabSheet;
    tabEnvThemes: TTabSheet;
    TreeView1: TTreeView;
    procedure BitAceptarClick(Sender: TObject);
    procedure BitAplicarClick(Sender: TObject);
    procedure butLoadThemeClick(Sender: TObject);
    procedure butSaveCurThemClick(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    //Inicialización
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FViewPanRight: boolean;
    FViewPanMsg: boolean;
    FViewStatusbar: Boolean;
    FViewPanLeft: boolean;
    FViewToolbar: boolean;
    ItemIni   : TTreeNode;
    ConfigPages: array of TConfigPage;
    procedure cfgFilePropertiesChanges;
    procedure FillTree;
    procedure SetViewPanRight(AValue: boolean);
    procedure SetViewPanMsg(AValue: boolean);
    procedure SetViewStatusbar(AValue: Boolean);
    procedure SetViewPanLeft(AValue: boolean);
    procedure SetViewToolbar(AValue: boolean);
  private     //Páginas de configuración
    pEnviron, pEnvThemes, pEnvFilExp, pEnvMesPan, pEnvExtra1,
    pEditor, pEdiAppear, pEdiSyntax, pEdiExtra1,
    pCompiler, pCmpExtra1, pCmpExtra2, pCmpExtra3,
    pExtTool: TConfigPage;

  public  //Configuraciones de entorno
    language   : string;  //Lenguaje
    LoadLast   : boolean; //Cargar el último archivo editado

    PanelsCol  : TColor;  //Color de los paneles de toda la IDE.
    SplitterCol: TColor;  //Color de separadores
    PanTextCol : TColor;  //Color del texto mostrado en la barra de herramientas

    StateToolbar: TStyleToolbar;
    PanLeftWidth: integer;  //Ancho del panel del árbol de sintaxis.
    PanRightWidth:integer;  //Ancho del editor de ensamblador.
    //Propiedades sin control
    filesClosed : string;  {Lista de archivos cargados. Usado para restaurar los archivos
                          abiertos al abrir nuevamente el programa.}
    winXpos   : integer;  //Coordenada X de la ventana pricipal.
    winYpos   : integer;  //Coordenada Y de la ventana pricipal.
    winWidth  : integer;
    winHeight : integer;
    winState  : TWindowState;
    //Propiedades de temas
    themLoaded  : string; //Nombre del último tema cargado
    //Propiedades del explorador de archivos
    FilExplBack : TColor;
    FilExplText : TColor;
    FilExpFiltyp: integer;
    //Propiedades del panel de mensajes
    MessPanBack: TColor;  //Color de fondo del panel de mensajes
    MessPanText: TColor;  //Color del texto del panel de mensajes
    MessPanErr : TColor;  //Color del texto de error del panel de mensajes
    MessPanSel : TColor;  //Color del fonde de la selección del panel de mensajes
  public //Configuraciones de Editor
    TabEdiMode : integer;  //Estado de pestañas del editor
    ShowErMsg   : boolean; //Muestra diálogo con mensaje de error
    property ViewStatusbar: Boolean read FViewStatusbar write SetViewStatusbar;
    property ViewToolbar: boolean read FViewToolbar write SetViewToolbar;
    property ViewPanMsg: boolean read FViewPanMsg write SetViewPanMsg;
    property ViewPanRight: boolean read FViewPanRight write SetViewPanRight;
    property ViewPanLeft: boolean read FViewPanLeft write SetViewPanLeft;
  public  //COnfiguraciones apariencia del editor
    fraCfgSynEdit: TfraCfgSynEdit;
  public
    fraCfgSyntax : TfraCfgSyntax;
    fraCfgExtTool: TfraCfgExtTool;
    OnPropertiesChanges: procedure of object;
    adaptadores: TAdaptadores;
    procedure RegisterAdapter(adapt: TAdapterBase);
    procedure ActivateAdapter(adapt: TAdapterBase);
    procedure Init;
    procedure Mostrar;
    procedure SaveToFile;
  end;

var
  Config: TConfig;

implementation
{$R *.lfm}
{ TConfig }
resourcestring
  THEME_FILE_DESC  = 'Theme file';
  MSG_ALLFILES     = 'All files';
  LABEL_THEM_NONE   = 'None';
  // Environment Settings
  CPAGE_ENVIRON    = 'Environment';
  CPAGE_ENV_FILEXP = 'File Explorer';
  CPAGE_ENV_THEMES = 'Themes';
  CPAGE_ENV_MESPAN = 'Message Panel';
  CPAGE_ENV_EXTRA1 = 'Extra Subpanel 1'; //Disponible para uso por el compilador
  // Editor Settings
  CPAGE_EDITOR     = 'Editor' ;
  CPAGE_EDI_APPEAR = 'Appearance';
  CPAGE_EDI_SYNTAX = 'Syntax';
  CPAGE_EDI_EXTRA1 = 'Extra Subpanel 1';    //Disponible para uso por el compilador
  // Compiler Settings
  CPAGE_COMPIL     = 'Extra Panel';      //Disponible para uso por el compilador
  CPAGE_CMP_EXTRA1 = 'Extra Subpanel 1';     //Disponible para uso por el compilador
  CPAGE_CMP_EXTRA2 = 'Extra Subpanel 2'; //Disponible para uso por el compilador
  CPAGE_CMP_EXTRA3 = 'Extra Subpanel 3'; //Disponible para uso por el compilador
  // External Tool
  CPAGE_EXTOOL     = 'External Tool';

procedure TConfig.FillTree;
  function AddConfigPage(title: string; ParentNode: TTreeNode;
            ImageIndex: integer; tabSht: TTabSheet; scrlBox: TScrollBox): TConfigPage;
  {Agrega una página de configuración, que consiste en un nodo en el TTreeView lateral y
  un TTabSheet asociado del PageControl.
  Devuelve el TreeNode creado}
  var
    configPage: TConfigPage;
    Item: TTreeNode;
    n: SizeInt;
  begin
    //Agrega nodo en el TTreeView
    Item := TreeView1.Items.AddChild(ParentNode, title);
    Item.ImageIndex    := ImageIndex;  //cambia ícono del nodo
    Item.SelectedIndex := ImageIndex;
    //Llena ConfigPage
    configPage.treeNode := Item;
    configPage.tabsheet := tabSht;
    configPage.scrollBox := scrlBox;
    configPage.extra := false;
    //Agrega al arreglo
    n := length(ConfigPages);
    SetLength(ConfigPages, n+1);
    ConfigPages[n] := configPage;
    //Devuelve Tree node creado
    exit(configPage);
  end;
  function AddConfigExtra(title: string; ParentNode: TTreeNode;
            ImageIndex: integer; tabSht: TTabSheet; scrlBox: TScrollBox): TConfigPage;
  {Similar a AddConfigPage(), pero configura el ConfigPage para marcarlo como que es
  adicional o extra, y que se deja disponible para el complador pueda usarla}
  begin
    Result := AddConfigPage(title, ParentNode, ImageIndex, tabSht, scrlBox);
    Result.extra := true;
  end;
var
  SR: TRawByteSearchRec;
  Hay: Boolean;
begin
  TreeView1.Items.Clear;
  //Configuración de entorno
  pEnviron := AddConfigPage(CPAGE_ENVIRON,nil, 0, tabEnviron , sclEnviron);
  ItemIni := pEnviron.treeNode;   //ítem inicial
    pEnvThemes := AddConfigPage (CPAGE_ENV_THEMES, pEnviron.treeNode, 0, tabEnvThemes, sclEnvThemes); //Temas
    pEnvFilExp := AddConfigPage (CPAGE_ENV_FILEXP, pEnviron.treeNode, 0, tabEnvFilExp, sclEnvFilExp); //Explorador de archivos
    pEnvMesPan := AddConfigPage (CPAGE_ENV_MESPAN, pEnviron.treeNode, 0, tabEnvMesPan, sclEnvMesPan); //Panel de mensajes
    pEnvExtra1 := AddConfigExtra(CPAGE_ENV_EXTRA1, pEnviron.treeNode, 0, tabEnvExtra1, sclEnvExtra1); //Extra panel 1
  pEnviron.treeNode.Expanded := true;
  //Configuración del Editor principal (editores)
  pEditor  := AddConfigPage(CPAGE_EDITOR, nil, 0, tabEditor   , sclEditor   );
    pEdiAppear := AddConfigPage (CPAGE_EDI_APPEAR, pEditor.treeNode, 0, tabEdiAppear, sclEdiAppear); //Apariencia
    pEdiSyntax := AddConfigPage (CPAGE_EDI_SYNTAX, pEditor.treeNode, 0, tabEdiSyntax, sclEdiSyntax); //Editor-Sintaxis
    pEdiExtra1 := AddConfigExtra(CPAGE_EDI_EXTRA1, pEditor.treeNode, 0, tabEdiExtra1, sclEdiExtra1); //Acciones extra
  pEditor.treeNode.Expanded := true;
  //Configuración del Compilador
  pCompiler := AddConfigExtra(CPAGE_COMPIL,nil, 0, tabCompiler , sclCompiler );
    pCmpExtra1 := AddConfigExtra(CPAGE_CMP_EXTRA1, pCompiler.treeNode, 0, tabCmpExtra1, sclCmpExtra1); //Extra panel 1
    pCmpExtra2 := AddConfigExtra(CPAGE_CMP_EXTRA2, pCompiler.treeNode, 0, tabCmpExtra2, sclCmpExtra2); //Extra panel 2
    pCmpExtra3 := AddConfigExtra(CPAGE_CMP_EXTRA3, pCompiler.treeNode, 0, tabCmpExtra3, sclCmpExtra3); //Extra panel 3
  pCompiler.treeNode.Expanded := true;
  //Herramientas externas
  pExtTool := AddConfigPage(CPAGE_EXTOOL, nil, 0, tabExtTool, sclExtTool);

  //Termina llenado.
  ItemIni.Selected := true;
  TreeView1Click(self);
  //Lee lista de temas
  //cmbThemes.Items.Clear;
  //cmbThemes.Items.Add(LABEL_THEM_NONE);
  //Hay := FindFirst(patThemes + DirectorySeparator + '*.theme',faAnyFile - faDirectory, SR) = 0;
  //while Hay do begin
  //   //Encontró archivo, lee sus extensiones
  //   cmbThemes.Items.Add(ExtractFileNameWithoutExt(SR.name));
  //   Hay := FindNext(SR) = 0;  //Busca siguiente
  //end;
  //cmbThemes.ItemIndex := 0;
end;
procedure TConfig.TreeView1Click(Sender: TObject);
var
  i: Integer;
begin
  //Busca el nodo seleccionado para activar la página que corresponde.
  for i:=0 to length(ConfigPages)-1 do begin
    if TreeView1.Selected = ConfigPages[i].treeNode then begin
      //Encontró el nodo seleccionado en la lista de páginas de config.
      PageControl1.ActivePage := ConfigPages[i].tabsheet;  //Activa
    end;
  end;
end;
procedure TConfig.TreeView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    TreeView1Click(self);
  end;
end;
procedure TConfig.BitAceptarClick(Sender: TObject);
begin
  bitAplicarClick(Self);
  if cfgFile.MsjErr<>'' then exit;  //hubo error
  self.Close;  //sale si no hay error
end;
procedure TConfig.BitAplicarClick(Sender: TObject);
var
  filTheme: String;
begin
  //Verifica primero si hay tema, para cargarlo antes que nada
//  if cmbThemes.ItemIndex > 0 then begin
//    filTheme := patThemes + DirectorySeparator + cmbThemes.Text + '.theme';
//    //Lee de archivo, solo las propiedades marcadas con categoría 1.
//    if not cfgFile.FileToPropertiesCat(filTheme, 1) then begin
//      MsgErr(cfgFile.MsjErr);
//    end;
//    //Mueva valor de las propiedades, a los controles.
//    if not cfgFile.PropertiesToWindowCat(1) then begin
//      MsgErr(cfgFile.MsjErr);
//    end;
//    //Las propiedades de colores de las sintaxis se leen del final del archivo
//    //Actualiza las propiedades leídas del tema, en fraCfgSyntax
//    fraCfgSyntax.SetPropertiesForTheme(filTheme);
//  end;
  //Guarda primero los posibles cambios en la sintaxis, para tener actualizado los
  //archivos de sintaxis, cuando se dispare "OnPropertiesChanges".
  fraCfgSyntax.SaveChanges;
  //Proceso normal
  cfgFile.WindowToProperties;
  if cfgFile.MsjErr<>'' then begin
    MsgErr(cfgFile.MsjErr);
    exit;
  end;
  //Cambiará toda la interfaz incluyendo el idioma, y se recargará el archivo de sintaxis
  SaveToFile;
end;
procedure TConfig.butLoadThemeClick(Sender: TObject);
var
  filTheme: String;
begin
  OpenDialog1.InitialDir := patThemes;
  OpenDialog1.Filter:= THEME_FILE_DESC + '|*.theme|' + MSG_ALLFILES + '|*.*';
  if not OpenDialog1.Execute then exit;    //se canceló
  filTheme := OpenDialog1.FileName;
  if not FileExists(filTheme) then begin
    MsgExc('Theme file doesn''t exist.');
    exit;
  end;
  //Actualiza nombre de tema
  txtThemLoaded.Text := filTheme;

  //Lee de archivo, solo las propiedades marcadas con categoría 1.
  if not cfgFile.FileToPropertiesCat(filTheme, 1) then begin
    MsgErr(cfgFile.MsjErr);
  end;
  //Mueva valor de las propiedades, a los controles.
  if not cfgFile.PropertiesToWindowCat(1) then begin
    MsgErr(cfgFile.MsjErr);
  end;
  //Las propiedades de colores de las sintaxis se leen del final del archivo
  //Actualiza las propiedades leídas del tema, en fraCfgSyntax
  fraCfgSyntax.SetPropertiesForTheme(filTheme);

end;
procedure TConfig.butSaveCurThemClick(Sender: TObject);
{Se pide guardar la configuración actual como tema.}
var
  themeName, filTem: string;
  f: text;
begin
  themeName := InputBox('New theme', 'Theme name:', '');
  if themeName = '' then exit;
  filTem := patThemes + DirectorySeparator + themeName + '.theme';
  if FileExists(filTem) then begin
    if MsgYesNo('Theme exists. Overwrite?') <> 1 then exit;
  end;
//  msgbox(synColorsInform);
  //Saca una copia del archivo de configuración
  CopyFile(cfgFile.ReadFileName, filTem);
  //Y le agrega información sobre la sintaxis, al final
  AssignFile(f, filTem);
  Append(f);   //Abre para agregar
  Writeln (f, '<!-->');  //agrega como comentario
  Writeln (f, fraCfgSyntax.GetPropertiesForTheme);
  Writeln (f, '<-->');
  CloseFile(f);

end;
procedure TConfig.cfgFilePropertiesChanges;
begin
  if OnPropertiesChanges<>nil then OnPropertiesChanges;
end;
procedure TConfig.SetViewPanMsg(AValue: boolean);
begin
  if FViewPanMsg = AValue then Exit;
  FViewPanMsg := AValue;
  cfgFilePropertiesChanges;
end;
procedure TConfig.SetViewStatusbar(AValue: Boolean);
begin
  if FViewStatusbar = AValue then Exit;
  FViewStatusbar := AValue;
  cfgFilePropertiesChanges;
end;
procedure TConfig.SetViewPanLeft(AValue: boolean);
begin
  if FViewPanLeft = AValue then Exit;
  FViewPanLeft := AValue;
  cfgFilePropertiesChanges;
end;
procedure TConfig.SetViewPanRight(AValue: boolean);
begin
  if FViewPanRight = AValue then Exit;
  FViewPanRight := AValue;
  cfgFilePropertiesChanges
end;
procedure TConfig.SetViewToolbar(AValue: boolean);
begin
  if FViewToolbar = AValue then Exit;
  FViewToolbar := AValue;
  cfgFilePropertiesChanges;
end;
procedure TConfig.RegisterAdapter(adapt: TAdapterBase);
{Regsitra el adaptador de un compilador nuevo, para poder usar sus frames de
configuración.}
begin
  //Agrega a la lista de adaptadores
  adaptadores.Add(adapt);
  //Llama a rutina de creación para que cree sus Frames de configuración.
  adapt.ConfigCreate(self, pEnvExtra1, pEdiExtra1,
    pCompiler, pCmpExtra1, pCmpExtra2, pCmpExtra3);
end;
procedure TConfig.ActivateAdapter(adapt: TAdapterBase);
{Inicia el estado de las páginas extras y solicita al adaptador que configure estas
páginas.}
  procedure HidePage(pag: TConfigPage);
  {Oculta una página de configuración y todos los frames de configuración que pueda
  contener}
  var
    i: Integer;
  begin
    pag.treeNode.Visible := false;    //Oculta el acceso, no la página
    //Ahora oculta todos los Frames que se han creado en la página.
    for i:=0 to pag.scrollBox.ControlCount-1 do begin
      //Todos deben ser "frames".
      pag.scrollBox.Controls[i].Visible := false;
    end;
  end;
begin
  //Primero oculta todos las páginas que se ofrecen a los compiladores.
  HidePage(pEnvExtra1);
  HidePage(pEdiExtra1);
  HidePage(pCompiler );
  HidePage(pCmpExtra1);
  HidePage(pCmpExtra2);
  HidePage(pCmpExtra3);
  //Llama al adaptador para que active las que va a usar.
  adapt.ConfigActivate;
  //Reinicia ítem seleccionado.
  ItemIni.Selected := true;
end;
procedure TConfig.Init;
//Inicia el formulario de configuración. Debe llamarse antes de usar el formulario y
//después de haber cargado todos los frames.
var
  s: TParElem;
  adapt: TAdapterBase;
begin
  ///////////////////////////////////////////////////////
  ///////// Configuraciones de Entorno
  ///////////////////////////////////////////////////////
  s:=cfgFile.Asoc_Str ('language'    , @language   , cmbLanguage , 'en - English');
  s:=cfgFile.Asoc_Bol ('chkLoadLast' , @LoadLast   , chkLoadLast , true);
  s:=cfgFile.Asoc_Enum('grpToolbarSta',@StateToolbar, SizeOf(TStyleToolbar), grpToolbarSta, 1);
  s:=cfgFile.Asoc_TCol('PanelsCol'  , @PanelsCol  , colPanelsCol, clDefault);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_TCol('SplitterCol' , @SplitterCol, colSplitCol , clDefault);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_TCol('TextPanel'   , @PanTextCol , colPanTextCol, clGray);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  //Propiedades sin control
  s:=cfgFile.Asoc_Str ('filesClosed' , @filesClosed  , '');
  s:=cfgFile.Asoc_Bol ('VerPanMensaj', @FViewPanMsg  , true);
  s:=cfgFile.Asoc_Bol ('VerStatusbar', @ViewStatusbar, true);
  s:=cfgFile.Asoc_Bol ('VerBarHerram', @FViewToolbar , true);
  s:=cfgFile.Asoc_Bol ('ViewSynTree' , @FViewPanLeft , true);
  s:=cfgFile.Asoc_Int ('SynTreeWidth', @PanLeftWidth , 130);
  s:=cfgFile.Asoc_Int ('EditAsmWidth', @PanRightWidth, 300);
  s:=cfgFile.Asoc_Bol ('ViewPanAssem', @FViewPanRight, true);
  s:=cfgFile.Asoc_Int ('winXpos'     , @winXpos  , 50);
  s:=cfgFile.Asoc_Int ('winYpos'     , @winYpos  , 50);
  s:=cfgFile.Asoc_Int ('winWidth'    , @winWidth , 800);
  s:=cfgFile.Asoc_Int ('winHeight'   , @winHeight, 600);
  s:=cfgFile.Asoc_Enum('winState'    , @winState , SizeOf(TWindowState), 0);

  ///////// Configuraciones del Temas
  s:=cfgFile.Asoc_Str('themLoaded'  ,@themLoaded, txtThemLoaded, '');

  ///////// Configuraciones del Explorador de Archivos
  s:=cfgFile.Asoc_TCol('CodExplBack',@FilExplBack, colCodExplBack, clWindow);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_TCol('CodExplText',@FilExplText, colCodExplText, clDefault);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_Int ('grpFiltypes',@FilExpFiltyp, grpFiltype, 0);

  ///////// Configuraciones del Panel de mensajes
  s:=cfgFile.Asoc_TCol('MessPanBack',@MessPanBack, colMessPanBack, clWindow);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_TCol('MessPanText',@MessPanText, colMessPanText, clDefault);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_TCol('MessPanErr', @MessPanErr , colMessPanErr , clRed);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_TCol('MessPanSel', @MessPanSel , colMessPanSel , clBtnFace);
  s.categ := 1;   //marca como propiedad de tipo "Tema"

  ///////////////////////////////////////////////////////
  ///////// Configuraciones del Editor
  ///////////////////////////////////////////////////////

  s:=cfgFile.Asoc_Bol('ShowErMsg'  , @ShowErMsg, chkShowErrMsg, true);
  s:=cfgFile.Asoc_Int('TabEdiState', @TabEdiMode, grpTabEdiState, 0);

  ///////// Configuraciones de apariencia
  fraCfgSynEdit.Iniciar('Edit', cfgFile);

  //Configuración de Sintaxis
  fraCfgSyntax.LoadSyntaxFiles(patSyntax);

  ///////////////////////////////////////////////////////
  ///////// Configuración de Herramienta Externa
  fraCfgExtTool.Init('ExternTool', cfgFile);

  ///////// Configuraciones de los adaptadores registrados
  for adapt in adaptadores do begin
     adapt.ConfigInit(cfgFile);
  end;
  //////////////////////////////////////////////////
  cfgFile.OnPropertiesChanges := @cfgFilePropertiesChanges;
  if not cfgFile.FileToProperties then begin
    MsgErr(cfgFile.MsjErr);
  end;
end;
procedure TConfig.Mostrar;
//Muestra el formulario para configurarlo
begin
  Showmodal;
end;
procedure TConfig.SaveToFile;
begin
  if not cfgFile.PropertiesToFile then begin
    MsgErr(cfgFile.MsjErr);
  end;
end;
//Inicialización
procedure TConfig.FormCreate(Sender: TObject);
begin
  adaptadores:= TAdaptadores.Create(false);
  //Crea frames de configruación a usar
  fraCfgSynEdit := TfraCfgSynEdit.Create(self);
  fraCfgSynEdit.Parent := sclEdiAppear; // tabEdiAppear;
  fraCfgSynEdit.Left := 0;
  fraCfgSynEdit.Top := 0;

  fraCfgSyntax := TfraCfgSyntax.Create(self);
  fraCfgSyntax.Parent := sclEdiSyntax;
  fraCfgSyntax.Left := 0;
  fraCfgSyntax.Top := 0;

  fraCfgExtTool := TfraCfgExtTool.Create(self);
  fraCfgExtTool.Parent := sclExtTool;
  fraCfgExtTool.Left := 0;
  fraCfgExtTool.Top := 0;

  // Tareas adicionales
  cfgFile.VerifyFile;
  FillTree;
end;
procedure TConfig.FormShow(Sender: TObject);
begin
  if not cfgFile.PropertiesToWindow then begin
    MsgErr(cfgFile.MsjErr);
  end;
end;
procedure TConfig.FormDestroy(Sender: TObject);
begin
  adaptadores.Destroy;
end;

end.

