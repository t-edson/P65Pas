{Modelo de formulario de configuración que usa dos Frame de configuración}
unit FormConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ExtCtrls, ComCtrls, ColorBox, LCLType, Spin,
  FrameCfgSynEdit, Globales, FrameCfgSyntax, FrameCfgExtTool, MiConfigXML,
  MiConfigBasic, MisUtils;
type
  //Tipo de Barra de herramientas
  TStyleToolbar = (stb_SmallIcon, stb_BigIcon);
  //Tipo de declaración de variables
  TVarDecType = (dvtDBDb,  //Estilo DB/Db/DW
                 dvtEQU    //Estilo usando macros y EQU
                 );
  //Niveles de optimización
  TOptimLev = (olvFool,   //Nivel básico de optimización
               olvSmart   //Nivel mayor de optimización
               );
  TTreeViewMode = (vmGroups,   //Muestra por grupos
                   vmDeclar,   //Muestra en el orden de declaración
                   vmFileExp   //Muestra el explorador de archivos
                   );
  { TConfig }
  TConfig = class(TForm)
    BitAplicar: TBitBtn;
    BitCancel: TBitBtn;
    BitAceptar: TBitBtn;
    butSaveCurThem: TButton;
    chkOptBnkAftIF: TCheckBox;
    chkAutSynChk: TCheckBox;
    chkIncVarName: TCheckBox;
    chkLoadLast: TCheckBox;
    chkOptBnkBefPro: TCheckBox;
    chkOptBnkAftPro: TCheckBox;
    chkReuProcVar: TCheckBox;
    chkOptRetProc: TCheckBox;
    chkShowErrMsg: TCheckBox;
    chkIncComment2: TCheckBox;
    chkExcUnused: TCheckBox;
    chkIncDecVar: TCheckBox;
    chkIncAddress: TCheckBox;
    chkIncComment: TCheckBox;
    chkIncHeadMpu: TCheckBox;
    chkViewHScroll: TCheckBox;
    chkViewVScroll: TCheckBox;
    cmbFontName: TComboBox;
    colCodExplBack: TColorBox;
    colCodExplText: TColorBox;
    colMessPanBack: TColorBox;
    colMessPanErr: TColorBox;
    colMessPanPan: TColorBox;
    colMessPanSel: TColorBox;
    colMessPanText: TColorBox;
    colSplitCol: TColorBox;
    cmbThemes: TComboBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    grpTabEdiState: TRadioGroup;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lblCodExplCol1: TLabel;
    lblCodExplCol2: TLabel;
    lblMessPan1: TLabel;
    lblMessPan2: TLabel;
    lblMessPan3: TLabel;
    lblMessPan4: TLabel;
    lblPanelCol: TLabel;
    lblSplitCol: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    grpOptimLev: TRadioGroup;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    grpFilType: TRadioGroup;
    spFontSize: TSpinEdit;
    tabEditor: TTabSheet;
    tabEdiColor: TTabSheet;
    tabEnsamb: TTabSheet;
    tabCompiler: TTabSheet;
    tabEnviron: TTabSheet;
    tabExtTool: TTabSheet;
    tabMessPan: TTabSheet;
    tabCodeExp: TTabSheet;
    tabEdiSyntax: TTabSheet;
    TreeView1: TTreeView;
    procedure BitAceptarClick(Sender: TObject);
    procedure BitAplicarClick(Sender: TObject);
    procedure butSaveCurThemClick(Sender: TObject);
    procedure chkIncDecVarChange(Sender: TObject);
    procedure SetLanguage;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FViewPanAssem: boolean;
    FViewPanMsg: boolean;
    FViewStatusbar: Boolean;
    FViewSynTree: boolean;
    FViewToolbar: boolean;
    ItemIni   : TTreeNode;
    procedure cfgFilePropertiesChanges;
    procedure FillTree;
    procedure SetViewPanAssem(AValue: boolean);
    procedure SetViewPanMsg(AValue: boolean);
    procedure SetViewStatusbar(AValue: Boolean);
    procedure SetViewSynTree(AValue: boolean);
    procedure SetViewToolbar(AValue: boolean);
  public  //Configuraciones generales
    language  : string;   //Lenguaje
  public //Configuraciones de Editor
    TipLet     : string;    //tipo de letra
    TamLet     : integer;   //tamaño de letra
    VerBarDesV : boolean;   //ver barras de desplazamiento
    VerBarDesH : boolean;   //ver barras de desplazamiento
    TabEdiMode: integer;  //Estado de pestañas del editor
    AutSynChk : boolean;  //Verificación automática de sintaxis
    property ViewStatusbar: Boolean read FViewStatusbar write SetViewStatusbar;
    property ViewToolbar: boolean read FViewToolbar write SetViewToolbar;
    property ViewPanMsg: boolean read FViewPanMsg write SetViewPanMsg;
    property ViewPanAssem: boolean read FViewPanAssem write SetViewPanAssem;
    property ViewSynTree: boolean read FViewSynTree write SetViewSynTree;
  public  //Configuraciones de entorno
    StateToolbar: TStyleToolbar;
    SynTreeWidth: integer;   //Ancho del panel del árbol ed sintaxis
    viewMode  : TTreeViewMode;
    CodExplBack: TColor;
    CodExplText: TColor;
    cexpFiltype   : integer;
    MessPanBack: TColor;  //Color de fondo del panel de mensajes
    MessPanText: TColor;  //Color del texto del panel de mensajes
    MessPanErr : TColor;  //Color del texto de error del panel de mensajes
    MessPanSel : TColor;  //Color del fonde de la selección del panel de mensajes
    PanelsCol : TColor;   //Color de los panels del Panel de Mensages
    SplitterCol: TColor;  //Color de separadores
    LoadLast   : boolean; //Cargar el último archivo editado
    filesClosed: string;  {Lista de archivos cargados. Usado para restaurar los archivos
                          abiertos al abrir nuevamente el programa.}
  public  //Configuraciones para ensamblador
    IncHeadMpu: boolean;  //Incluye encabezado con información del MPU
    IncVarDec : boolean;  //Incluye declaración de varaibles
    VarDecType: TVarDecType;  //tipo de declaración de variables
    IncAddress: boolean;  //Incluye dirección física en el código desensamblado
    IncComment: boolean;  //Incluye comentarios en el código desensamblado
    IncComment2: boolean; //Incluye comentarios detallados en el código desensamblado
    ExcUnused : boolean;  //Excluye declaración de variables no usadas
    IncVarName: boolean;  //Reemplaza dirección con etiqueta de variables
    //Configuracions del compilador
    ShowErMsg   : boolean;
    OptimLev    : TOptimLev;
    OptBnkAftIF : boolean;
    OptBnkBefPro: boolean;
    OptBnkAftPro: boolean;
    ReuProcVar  : boolean;
    OptRetProc  : boolean;
    procedure ConfigEditor(ed: TSynEdit);
  public
    fraCfgSynEdit: TfraCfgSynEdit;
    fraCfgSyntax : TfraCfgSyntax;
    fraCfgExtTool: TfraCfgExtTool;
    OnPropertiesChanges: procedure of object;
    procedure Iniciar;
    procedure Mostrar;
    procedure SaveToFile;
  end;

var
  Config: TConfig;

implementation
{$R *.lfm}
{ TConfig }
  {$I ..\language\tra_FormConfig.pas}
procedure TConfig.FormCreate(Sender: TObject);
begin
  fraCfgSynEdit := TfraCfgSynEdit.Create(self);
  fraCfgSynEdit.Parent := tabEdiColor;
  fraCfgSynEdit.Left := 5;
  fraCfgSynEdit.Top := 5;

  fraCfgSyntax := TfraCfgSyntax.Create(self);
  fraCfgSyntax.Parent := tabEdiSyntax;
  fraCfgSyntax.Left := 5;
  fraCfgSyntax.Top := 5;

  fraCfgExtTool := TfraCfgExtTool.Create(self);
  fraCfgExtTool.Parent := tabExtTool;
  fraCfgExtTool.Left := 5;
  fraCfgExtTool.Top := 5;

  cfgFile.VerifyFile;
end;
procedure TConfig.FillTree;
var
  Item, SubItem: TTreeNode;
  SR: TRawByteSearchRec;
  Hay: Boolean;
begin
  TreeView1.Items.Clear;
  //Environment
  Item := TreeView1.Items.AddChild(nil, TIT_CFG_ENVIRON);
  Item.ImageIndex:=0;
  Item.SelectedIndex := 0;
  ItemIni := Item;   //Item inicial
    SubItem := TreeView1.Items.AddChild(Item, TIT_CFG_CODEXP);
    SubItem.ImageIndex:=0;    //cambia ícono del nodo
    SubItem.SelectedIndex := 0;
    SubItem := TreeView1.Items.AddChild(Item, TIT_CFG_MESPAN);
    SubItem.ImageIndex:=0;    //cambia ícono del nodo
    SubItem.SelectedIndex := 0;
  Item.Expanded := true;
  //Configura Editor
  Item := TreeView1.Items.AddChild(nil, TIT_CFG_EDITOR);
  Item.ImageIndex:=0;
  Item.SelectedIndex := 0;
    //Editor-Colores
    SubItem := TreeView1.Items.AddChild(Item, TIT_CFG_EDICOL);
    SubItem.ImageIndex:=0;    //cambia ícono del nodo
    SubItem.SelectedIndex := 0;
    //Editor-Sintaxis
    SubItem := TreeView1.Items.AddChild(Item, TIT_CFG_SYNTAX);
    SubItem.ImageIndex:=0;    //cambia ícono del nodo
    SubItem.SelectedIndex := 0;
  Item.Expanded := true;
  //Assembler
  Item := TreeView1.Items.AddChild(nil, TIT_CFG_ASSEMB);
  Item.ImageIndex:=0;    //cambia ícono del nodo
  Item.SelectedIndex := 0;
  Item := TreeView1.Items.AddChild(nil, TIT_CFG_COMPIL);
  Item.ImageIndex:=0;    //cambia ícono del nodo
  Item.SelectedIndex := 0;
  Item := TreeView1.Items.AddChild(nil, TIT_CFG_EXTOOL);
  Item.ImageIndex:=0;    //cambia ícono del nodo
  Item.SelectedIndex := 0;

  ItemIni.Selected := true;
  TreeView1Click(self);
  //Lee lista de temas
  cmbThemes.Items.Clear;
  cmbThemes.Items.Add(LABEL_THEM_NONE);
  Hay := FindFirst(patThemes + DirectorySeparator + '*.theme',faAnyFile - faDirectory, SR) = 0;
  while Hay do begin
     //Encontró archivo, lee sus extensiones
     cmbThemes.Items.Add(ExtractFileNameWithoutExt(SR.name));
     Hay := FindNext(SR) = 0;  //Busca siguiente
  end;
  cmbThemes.ItemIndex := 0;
end;
procedure TConfig.TreeView1Click(Sender: TObject);
var
  nodStr: String;
begin
  nodStr := TreeView1.Selected.Text;
  if nodStr = TIT_CFG_EDITOR  then PageControl1.ActivePage := tabEditor;
  if nodStr = TIT_CFG_ENVIRON then PageControl1.ActivePage := tabEnviron;
  if nodStr = TIT_CFG_CODEXP  then PageControl1.ActivePage := tabCodeExp;
  if nodStr = TIT_CFG_MESPAN  then PageControl1.ActivePage := tabMessPan;
  if nodStr = TIT_CFG_EDICOL  then PageControl1.ActivePage := tabEdiColor;
  if nodStr = TIT_CFG_SYNTAX  then PageControl1.ActivePage := tabEdiSyntax;
  if nodStr = TIT_CFG_ASSEMB  then PageControl1.ActivePage := tabEnsamb;
  if nodStr = TIT_CFG_COMPIL  then PageControl1.ActivePage := tabCompiler;
  if nodStr = TIT_CFG_EXTOOL  then PageControl1.ActivePage := tabExtTool;
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
  if cmbThemes.ItemIndex > 0 then begin
    filTheme := patThemes + DirectorySeparator + cmbThemes.Text + '.theme';
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
  //Guarda primero, para tener actualizado los archivos de sintaxis, cuando se dispare
  //"OnPropertiesChanges"
  fraCfgSyntax.SaveChanges;
  //Proceso normal
  cfgFile.WindowToProperties;
  if cfgFile.MsjErr<>'' then begin
    MsgErr(cfgFile.MsjErr);
    exit;
  end;
  //Cambiará toda la interafaz incluyendo el idioma, y se recargará el archivo de sintaxis
  SaveToFile;
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
procedure TConfig.chkIncDecVarChange(Sender: TObject);
begin
  RadioGroup2.Enabled := chkIncDecVar.Checked;
  chkExcUnused.Enabled := chkIncDecVar.Checked;
end;
procedure TConfig.Iniciar;
//Inicia el formulario de configuración. Debe llamarse antes de usar el formulario y
//después de haber cargado todos los frames.
var
  s: TParElem;
begin
  //Configuraciones de Entorno
  s:=cfgFile.Asoc_Str ('language'   , @language, ComboBox1, 'en - English');
  s:=cfgFile.Asoc_Enum('StateStatusbar', @StateToolbar, SizeOf(TStyleToolbar), RadioGroup1, 1);
  s:=cfgFile.Asoc_Bol ('chkLoadLast',@LoadLast   , chkLoadLast   , true);
  s:=cfgFile.Asoc_Str ('filesClosed', @filesClosed, '');
  s:=cfgFile.Asoc_TCol('SplitterCol',@SplitterCol, colSplitCol, clDefault);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_TCol('MessPanPan', @PanelsCol , colMessPanPan , clDefault);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_Bol('VerPanMensaj', @FViewPanMsg  , true);
  s:=cfgFile.Asoc_Bol('VerStatusbar', @ViewStatusbar, true);
  s:=cfgFile.Asoc_Bol('VerBarHerram', @FViewToolbar , true);
  s:=cfgFile.Asoc_Bol('ViewSynTree',  @FViewSynTree , true);
  s:=cfgFile.Asoc_Int('SynTreeWidth', @SynTreeWidth , 130);
  s:=cfgFile.Asoc_Bol('ViewPanAssem', @FViewPanAssem, true);
  s:=cfgFile.Asoc_Enum('viewMode',  @viewMode   , SizeOf(TTreeViewMode), 0);
  //Configuraciones del Panel de mensajes
  s:=cfgFile.Asoc_TCol('MessPanBack',@MessPanBack, colMessPanBack, clWindow);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_TCol('MessPanText',@MessPanText, colMessPanText, clDefault);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_TCol('MessPanErr', @MessPanErr , colMessPanErr , clRed);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_TCol('MessPanSel', @MessPanSel , colMessPanSel , clBtnFace);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  //Configuraciones del Explorador de Cödigo
  s:=cfgFile.Asoc_TCol('CodExplBack',@CodExplBack, colCodExplBack, clWindow);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_TCol('CodExplText',@CodExplText, colCodExplText, clDefault);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_Int ('grpFiltypes',@cexpFiltype,  grpFiltype, 0);

  //Configuraciones del Editor
  s:=cfgFile.Asoc_Int('TamLet', @TamLet, spFontSize, 10);
  s.categ := 1;
  cmbFontName.Items.Clear;
  cmbFontName.Items.Add('Courier New');
  cmbFontName.Items.Add('DejaVu Sans Mono');
  cmbFontName.Items.Add('Fixedsys');
  cmbFontName.Items.Add('Lucida Console');
  cmbFontName.Items.Add('Consolas');
  cmbFontName.Items.Add('Cambria');
  s:=cfgFile.Asoc_Str('TipLet', @TipLet, cmbFontName, 'Courier New');
  s.categ := 1;
  s:=cfgFile.Asoc_Bol('VerBarDesV', @VerBarDesV, chkViewVScroll, true);
  s:=cfgFile.Asoc_Bol('VerBarDesH', @VerBarDesH, chkViewHScroll, false);
  s:=cfgFile.Asoc_Int('TabEdiState', @TabEdiMode, grpTabEdiState, 0);
  s:=cfgFile.Asoc_Bol('AutSynChk',  @AutSynChk  , chkAutSynChk , false);
  //Configuraciones del Editor-Colores
  fraCfgSynEdit.Iniciar('Edit', cfgFile);
  //Configuración del Editor-Sintaxis
  fraCfgSyntax.LoadSyntaxFiles(patSyntax);

  //Configuraciones de Ensamblador
  cfgFile.Asoc_Bol('IncHeadMpu', @IncHeadMpu , chkIncHeadMpu , false);
  cfgFile.Asoc_Bol('IncDecVar' , @IncVarDec  , chkIncDecVar  , true);
  cfgFile.Asoc_Enum('VarDecType',@VarDecType , Sizeof(TVarDecType), RadioGroup2, 1);
  cfgFile.Asoc_Bol('IncAddress', @IncAddress , chkIncAddress , true);
  cfgFile.Asoc_Bol('IncComment', @IncComment , chkIncComment , false);
  cfgFile.Asoc_Bol('IncComment2',@IncComment2, chkIncComment2, false);
  cfgFile.Asoc_Bol('ExcUnused' , @ExcUnused  , chkExcUnused  , true);
  cfgFile.Asoc_Bol('IncVarName', @IncVarName , chkIncVarName , true);
  //Configuraciones del compilador
  cfgFile.Asoc_Bol('ShowErMsg' , @ShowErMsg, chkShowErrMsg, true);
  cfgFile.Asoc_Enum('OptimLev' , @OptimLev, Sizeof(TOptimLev), grpOptimLev, 1);
  cfgFile.Asoc_Bol('OptBnkAftIF' , @OptBnkAftIF , chkOptBnkAftIF , true);
  cfgFile.Asoc_Bol('OptBnkBefPro', @OptBnkBefPro, chkOptBnkBefPro, true);
  cfgFile.Asoc_Bol('OptBnkAftPro', @OptBnkAftPro, chkOptBnkAftPro, true);
  cfgFile.Asoc_Bol('ReuProcVar'  , @ReuProcVar, chkReuProcVar, false);
  cfgFile.Asoc_Bol('OptRetProc'  , @OptRetProc, chkOptRetProc, true);
  //Configuración de Herramienta Externa
  fraCfgExtTool.Init('ExternTool', cfgFile);
  //////////////////////////////////////////////////
  cfgFile.OnPropertiesChanges := @cfgFilePropertiesChanges;
  if not cfgFile.FileToProperties then begin
    MsgErr(cfgFile.MsjErr);
  end;
  chkIncDecVarChange(self);   //para actualizar
end;
procedure TConfig.FormShow(Sender: TObject);
begin
  if not cfgFile.PropertiesToWindow then begin
    MsgErr(cfgFile.MsjErr);
  end;
end;
procedure TConfig.ConfigEditor(ed: TSynEdit);
//Configura un editor con las opciones definidas aquí
begin
  fraCfgSynEdit.ConfigEditor(ed);
  //tipo de texto
  if TipLet <> '' then ed.Font.Name:=TipLet;
  if (TamLet > 6) and (TamLet < 32) then ed.Font.Size:=Round(TamLet);
  //Barras de desplazamiento
  if VerBarDesV and VerBarDesH then  //barras de desplazamiento
    ed.ScrollBars:= ssBoth
  else if VerBarDesV and not VerBarDesH then
    ed.ScrollBars:= ssVertical
  else if not VerBarDesV and VerBarDesH then
    ed.ScrollBars:= ssHorizontal
  else
    ed.ScrollBars := ssNone;
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
procedure TConfig.SetViewSynTree(AValue: boolean);
begin
  if FViewSynTree = AValue then Exit;
  FViewSynTree := AValue;
  cfgFilePropertiesChanges;
end;
procedure TConfig.SetViewPanAssem(AValue: boolean);
begin
  if FViewPanAssem = AValue then Exit;
  FViewPanAssem := AValue;
  cfgFilePropertiesChanges
end;
procedure TConfig.SetViewToolbar(AValue: boolean);
begin
  if FViewToolbar = AValue then Exit;
  FViewToolbar := AValue;
  cfgFilePropertiesChanges;
end;
procedure TConfig.Mostrar;
//Muestra el formulario para configurarlo
begin
  cmbThemes.ItemIndex := 0;   //Para evitar confusión
  Showmodal;
end;
procedure TConfig.SaveToFile;
begin
  if not cfgFile.PropertiesToFile then begin
    MsgErr(cfgFile.MsjErr);
  end;
end;

end.

