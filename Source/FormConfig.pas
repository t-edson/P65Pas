{Modelo de formulario de configuración que usa dos Frame de configuración}
unit FormConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, SynEdit, Forms, Controls, Graphics,
  Dialogs, Buttons, StdCtrls, ExtCtrls, ComCtrls, ColorBox, LCLType,
  FrameCfgSynEdit, Globales, FrameCfgSyntax, FrameCfgExtTool,
  FrameCfgAfterChg6502, FrameCfgCompiler6502, FrameCfgAsmOut6502, MiConfigXML,
  MiConfigBasic, MisUtils;
type
  //Tipo de Barra de herramientas
  TStyleToolbar = (stb_SmallIcon, stb_BigIcon);
  { TConfig }
  TConfig = class(TForm)
    BitAplicar: TBitBtn;
    BitCancel: TBitBtn;
    BitAceptar: TBitBtn;
    butSaveCurThem: TButton;
    chkLoadLast: TCheckBox;
    chkShowErrMsg: TCheckBox;
    colCodExplBack: TColorBox;
    colCodExplText: TColorBox;
    colMessPanBack: TColorBox;
    colMessPanErr: TColorBox;
    colPanels: TColorBox;
    colMessPanSel: TColorBox;
    colMessPanText: TColorBox;
    colPanTextCol: TColorBox;
    colSplitCol: TColorBox;
    cmbThemes: TComboBox;
    cmbLanguage: TComboBox;
    Edit1: TEdit;
    grpTabEdiState: TRadioGroup;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblCodExplCol1: TLabel;
    lblCodExplCol2: TLabel;
    lblMessPan1: TLabel;
    lblMessPan2: TLabel;
    lblMessPan3: TLabel;
    lblMessPan4: TLabel;
    lblPanelCol: TLabel;
    lblSplitCol: TLabel;
    lblSplitCol1: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    grpToolbarSta: TRadioGroup;
    grpFilType: TRadioGroup;
    scrlEdiColor: TScrollBox;
    scrlEnvExt1: TScrollBox;
    tabEditor: TTabSheet;
    tabEdiColor: TTabSheet;
    tabCompAsm: TTabSheet;
    tabCompiler: TTabSheet;
    tabEnviron: TTabSheet;
    tabExtTool: TTabSheet;
    tabMessPan: TTabSheet;
    tabCodeExp: TTabSheet;
    tabEdiSyntax: TTabSheet;
    tabAftEdit: TTabSheet;
    tabCompExt2: TTabSheet;
    tabCompExt3: TTabSheet;
    tabEnvExt1: TTabSheet;
    TreeView1: TTreeView;
    procedure BitAceptarClick(Sender: TObject);
    procedure BitAplicarClick(Sender: TObject);
    procedure butSaveCurThemClick(Sender: TObject);
    procedure SetLanguage;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FViewPanRight: boolean;
    FViewPanMsg: boolean;
    FViewStatusbar: Boolean;
    FViewPanLeft: boolean;
    FViewToolbar: boolean;
    ItemIni   : TTreeNode;
    procedure cfgFilePropertiesChanges;
    procedure FillTree;
    procedure SetViewPanRight(AValue: boolean);
    procedure SetViewPanMsg(AValue: boolean);
    procedure SetViewStatusbar(AValue: Boolean);
    procedure SetViewPanLeft(AValue: boolean);
    procedure SetViewToolbar(AValue: boolean);
  public  //Configuraciones generales
    language  : string;   //Lenguaje
    winXpos   : integer;  //Coordenada X de la ventana pricipal.
    winYpos   : integer;  //Coordenada Y de la ventana pricipal.
    winWidth  : integer;
    winHeight : integer;
    winState  : TWindowState;
  public  //Configuraciones de entorno
    LoadLast   : boolean; //Cargar el último archivo editado

    PanelsCol  : TColor;  //Color de los paneless del Panel de Mensages
    SplitterCol: TColor;  //Color de separadores
    PanTextCol : TColor;  //Color del texto mostrado en la barra de herramientas

    StateToolbar: TStyleToolbar;
    PanLeftWidth: integer;  //Ancho del panel del árbol de sintaxis.
    PanRightWidth: integer;  //Ancho del editor de ensamblador.
    CodExplBack: TColor;
    CodExplText: TColor;
    cexpFiltype: integer;
    filesClosed: string;  {Lista de archivos cargados. Usado para restaurar los archivos
                          abiertos al abrir nuevamente el programa.}
    //Porpiedades del panel de mensajes
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
  public  //Configuraciones después de editar
    fraCfgAfterChg6502: TfraCfgAfterChg6502;
  public  //Configuraciones para ensamblador
    fraCfgAsmOut6502: TfraCfgAsmOut6502;
  public  //Configuracions del compilador
    fraCfgCompiler6502: TfraCfgCompiler6502;
  public
    fraCfgSyntax : TfraCfgSyntax;
    fraCfgExtTool: TfraCfgExtTool;
    OnPropertiesChanges: procedure of object;
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
  LABEL_THEM_NONE   = 'None';
  // Environment Settings
  TIT_CFG_ENVIRON   = 'Environment';
  TIT_CFG_FILEXP    = 'File Explorer';
  TIT_CFG_MESPAN    = 'Message Panel';
  TIT_CFG_ENV_EXT1  = 'Extra Panel 1'; //Disponible para uso por el compilador
  // Editor Settings
  TIT_CFG_EDITOR    = 'Editor' ;
  TIT_CFG_EDI_APR   = 'Appearance';
  TIT_CFG_EDI_SYN   = 'Syntax';
  TIT_CFG_EDI_AFT   = 'After Edit';    //Disponible para uso por el compilador
  // Compiler Settings
  TIT_CFG_COMPIL    = 'Compiler';      //Disponible para uso por el compilador
  TIT_CFG_CMP_ASM   = 'Assembler';     //Disponible para uso por el compilador
  TIT_CFG_CMP_EXT2  = 'Extra Panel 2'; //Disponible para uso por el compilador
  TIT_CFG_CMP_EXT3  = 'Extra Panel 3'; //Disponible para uso por el compilador
  // External Tool
  TIT_CFG_EXTOOL    = 'External Tool';

procedure TConfig.SetLanguage;
begin
  fraCfgSynEdit.SetLanguage;
  fraCfgExtTool.SetLanguage;
  fraCfgSyntax.SetLanguage;
  FillTree;
end;
procedure TConfig.FormCreate(Sender: TObject);
begin
  fraCfgSynEdit := TfraCfgSynEdit.Create(self);
  fraCfgSynEdit.Parent := scrlEdiColor; // tabEdiColor;
  fraCfgSynEdit.Left := 0;
  fraCfgSynEdit.Top := 0;

  fraCfgSyntax := TfraCfgSyntax.Create(self);
  fraCfgSyntax.Parent := tabEdiSyntax;
  fraCfgSyntax.Left := 5;
  fraCfgSyntax.Top := 5;

  fraCfgExtTool := TfraCfgExtTool.Create(self);
  fraCfgExtTool.Parent := tabExtTool;
  fraCfgExtTool.Left := 5;
  fraCfgExtTool.Top := 5;

  //Frames de configuración para todos los compiladores soportados.
  fraCfgAfterChg6502 := TfraCfgAfterChg6502.Create(self);
  fraCfgAfterChg6502.Parent := tabAftEdit;
  fraCfgAfterChg6502.Left := 0;
  fraCfgAfterChg6502.Top := 0;

  fraCfgCompiler6502 := TfraCfgCompiler6502.Create(self);
  fraCfgCompiler6502.Parent := tabCompiler;
  fraCfgCompiler6502.Left := 0;
  fraCfgCompiler6502.Top := 0;

  fraCfgAsmOut6502 := TfraCfgAsmOut6502.Create(self);
  fraCfgAsmOut6502.Parent := tabCompAsm;
  fraCfgAsmOut6502.Left := 0;
  fraCfgAsmOut6502.Top := 0;

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
    //Explorador de archivos
    SubItem := TreeView1.Items.AddChild(Item, TIT_CFG_FILEXP);
    SubItem.ImageIndex:=0;    //cambia ícono del nodo
    SubItem.SelectedIndex := 0;
    //Panel de mensajes
    SubItem := TreeView1.Items.AddChild(Item, TIT_CFG_MESPAN);
    SubItem.ImageIndex:=0;    //cambia ícono del nodo
    SubItem.SelectedIndex := 0;
    //Extra panel 1
    SubItem := TreeView1.Items.AddChild(Item, TIT_CFG_ENV_EXT1);
    SubItem.ImageIndex:=0;    //cambia ícono del nodo
    SubItem.SelectedIndex := 0;
  Item.Expanded := true;
  //Configura Editor
  Item := TreeView1.Items.AddChild(nil, TIT_CFG_EDITOR);
  Item.ImageIndex:=0;
  Item.SelectedIndex := 0;
    //Editor-Colores
    SubItem := TreeView1.Items.AddChild(Item, TIT_CFG_EDI_APR);
    SubItem.ImageIndex:=0;    //cambia ícono del nodo
    SubItem.SelectedIndex := 0;
    //Editor-Sintaxis
    SubItem := TreeView1.Items.AddChild(Item, TIT_CFG_EDI_SYN);
    SubItem.ImageIndex:=0;    //cambia ícono del nodo
    SubItem.SelectedIndex := 0;
    //Acciones después de editar
    SubItem := TreeView1.Items.AddChild(Item, TIT_CFG_EDI_AFT);
    SubItem.ImageIndex:=0;    //cambia ícono del nodo
    SubItem.SelectedIndex := 0;
  Item.Expanded := true;
  //Compilador
  Item := TreeView1.Items.AddChild(nil, TIT_CFG_COMPIL);
  Item.ImageIndex:=0;    //cambia ícono del nodo
  Item.SelectedIndex := 0;
    //Assembler
    SubItem := TreeView1.Items.AddChild(Item, TIT_CFG_CMP_ASM);
    SubItem.ImageIndex:=0;    //cambia ícono del nodo
    SubItem.SelectedIndex := 0;
    //Extra panel 1
    SubItem := TreeView1.Items.AddChild(Item, TIT_CFG_CMP_EXT2);
    SubItem.ImageIndex:=0;    //cambia ícono del nodo
    SubItem.SelectedIndex := 0;
    //Extra panel 2
    SubItem := TreeView1.Items.AddChild(Item, TIT_CFG_CMP_EXT3);
    SubItem.ImageIndex:=0;    //cambia ícono del nodo
    SubItem.SelectedIndex := 0;
  Item.Expanded := true;
  //Herramientas externas
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
  if nodStr = TIT_CFG_ENVIRON then PageControl1.ActivePage := tabEnviron;
  if nodStr = TIT_CFG_FILEXP  then PageControl1.ActivePage := tabCodeExp;
  if nodStr = TIT_CFG_MESPAN  then PageControl1.ActivePage := tabMessPan;
  if nodStr = TIT_CFG_ENV_EXT1 then PageControl1.ActivePage := tabEnvExt1;

  if nodStr = TIT_CFG_EDITOR  then PageControl1.ActivePage := tabEditor;
  if nodStr = TIT_CFG_EDI_APR  then PageControl1.ActivePage := tabEdiColor;
  if nodStr = TIT_CFG_EDI_AFT  then PageControl1.ActivePage := tabAftEdit;
  if nodStr = TIT_CFG_EDI_SYN  then PageControl1.ActivePage := tabEdiSyntax;

  if nodStr = TIT_CFG_COMPIL  then PageControl1.ActivePage := tabCompiler;
  if nodStr = TIT_CFG_CMP_ASM then PageControl1.ActivePage := tabCompAsm;
  if nodStr = TIT_CFG_CMP_EXT2  then PageControl1.ActivePage := tabCompExt2;
  if nodStr = TIT_CFG_CMP_EXT3  then PageControl1.ActivePage := tabCompExt3;

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
procedure TConfig.FormShow(Sender: TObject);
begin
  if not cfgFile.PropertiesToWindow then begin
    MsgErr(cfgFile.MsjErr);
  end;
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
procedure TConfig.Init;
//Inicia el formulario de configuración. Debe llamarse antes de usar el formulario y
//después de haber cargado todos los frames.
var
  s: TParElem;
begin
  ///////////////////////////////////////////////////////
  ///////// Configuraciones de Entorno
  ///////////////////////////////////////////////////////
  s:=cfgFile.Asoc_Str ('language'    , @language   , cmbLanguage , 'en - English');
  s:=cfgFile.Asoc_Bol ('chkLoadLast' , @LoadLast   , chkLoadLast , true);
  s:=cfgFile.Asoc_Enum('grpToolbarSta',@StateToolbar, SizeOf(TStyleToolbar), grpToolbarSta, 1);
  s:=cfgFile.Asoc_TCol('MessPanels'  , @PanelsCol  , colPanels   , clDefault);
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

  ///////// Configuraciones del Explorador de Cödigo
  s:=cfgFile.Asoc_TCol('CodExplBack',@CodExplBack, colCodExplBack, clWindow);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_TCol('CodExplText',@CodExplText, colCodExplText, clDefault);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_Int ('grpFiltypes',@cexpFiltype, grpFiltype, 0);

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

  //Configuración por cada compilador
  fraCfgAfterChg6502.Init('AftChg6502', cfgFile);

  ///////////////////////////////////////////////////////
  ///////// Configuraciones del compilador
  ///////////////////////////////////////////////////////

  fraCfgCompiler6502.Init('Compiler6502', cfgFile);
  ///////////////////////////////////////////////////////
  ///////// Configuraciones de Ensamblador
  fraCfgAsmOut6502.Init('AsmOutput6502', cfgFile);

  ///////////////////////////////////////////////////////
  ///////// Configuración de Herramienta Externa
  fraCfgExtTool.Init('ExternTool', cfgFile);

  //////////////////////////////////////////////////
  cfgFile.OnPropertiesChanges := @cfgFilePropertiesChanges;
  if not cfgFile.FileToProperties then begin
    MsgErr(cfgFile.MsjErr);
  end;
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

