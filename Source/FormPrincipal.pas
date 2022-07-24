{                                   PicPas.
Compilador en Pascal para micorocntroladores PIC de la serie 16.

                                        Por Tito Hinostroza   22/08/2015 }
unit FormPrincipal;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, SynEdit, SynEditTypes, LazUTF8, Forms, Controls, Dialogs,
  Menus, ComCtrls, ActnList, StdActns, ExtCtrls, LCLIntf, LCLType, LCLProc,
  StdCtrls, Graphics, MisUtils, CompBase,  //Para tener acceso a TCompilerBase
  Compiler_PIC16, FrameLateralPanel, FormConfig, Globales, PicPasProject,
  EditView, FrameEditView, FrameMessagesWin, adapter6502, FrameCfgExtTool,
  ParserASM_6502, Analyzer, adapterBase;
type
  { TfrmPrincipal }
  TfrmPrincipal = class(TForm)
  published
    acArcOpen: TAction;
    acArcSaveAs: TAction;
    acArcSave: TAction;
    acArcNewFile: TAction;
    acArcQuit: TAction;
    acSearFind: TAction;
    acSearFindNxt: TAction;
    acSearReplac: TAction;
    acEdCopy: TEditCopy;
    acEdCut: TEditCut;
    acEdPaste: TEditPaste;
    acEdRedo: TAction;
    acEdSelecAll: TAction;
    acEdUndo: TAction;
    acArcNewProj: TAction;
    acArcCloseProj: TAction;
    acArcCloseFile: TAction;
    acSearFindPrv: TAction;
    acToolSel_kickc: TAction;
    acToolSel_P65pas: TAction;
    acViewPanRight: TAction;
    acToolExt4: TAction;
    acToolExt5: TAction;
    acToolExt2: TAction;
    acToolExt3: TAction;
    acToolExt1: TAction;
    acToolConfig: TAction;
    acViewToolbar: TAction;
    acViewMsgPan: TAction;
    ActionList: TActionList;
    acViewStatbar: TAction;
    acViewPanLeft: TAction;
    CoolBar1: TCoolBar;
    FindDialog1: TFindDialog;
    ImgActions32: TImageList;
    ImgActions16: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    extraMenu1: TMenuItem;
    extraMenu2: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    extraMenu3: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    panRightPanel: TPanel;
    Separator1: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem8: TMenuItem;
    mnSamples: TMenuItem;
    mnView: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    mnExit: TMenuItem;
    MenuItem9: TMenuItem;
    mnFile: TMenuItem;
    mnFind: TMenuItem;
    mnEdit: TMenuItem;
    mnTools: TMenuItem;
    mnRecents: TMenuItem;
    panMessages: TPanel;
    PopupEdit: TPopupMenu;
    PopupCompiler: TPopupMenu;
    ReplaceDialog1: TReplaceDialog;
    splLeft: TSplitter;
    Splitter2: TSplitter;
    splRight: TSplitter;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolBar4: TToolBar;
    ToolBar5: TToolBar;
    ToolBar6: TToolBar;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    ToolButton33: TToolButton;
    ToolButton34: TToolButton;
    ToolButton36: TToolButton;
    ToolButton37: TToolButton;
    ToolButton38: TToolButton;
    ToolButton39: TToolButton;
    ToolButton40: TToolButton;
    ToolButton41: TToolButton;
    ToolButton43: TToolButton;
    procedure acArcCloseFileExecute(Sender: TObject);
    procedure acArcCloseProjExecute(Sender: TObject);
    procedure acArcOpenExecute(Sender: TObject);
    procedure acArcSaveAsExecute(Sender: TObject);
    procedure acArcSaveExecute(Sender: TObject);
    procedure acArcNewFileExecute(Sender: TObject);
    procedure acArcNewProjExecute(Sender: TObject);
    procedure acArcQuitExecute(Sender: TObject);
    procedure acSearFindExecute(Sender: TObject);
    procedure acSearFindNxtExecute(Sender: TObject);
    procedure acSearFindPrvExecute(Sender: TObject);
    procedure acSearReplacExecute(Sender: TObject);
    procedure acEdiRedoExecute(Sender: TObject);
    procedure acEdiSelecAllExecute(Sender: TObject);
    procedure acEdiUndoExecute(Sender: TObject);
    procedure acEdRedoExecute(Sender: TObject);
    procedure acEdSelecAllExecute(Sender: TObject);
    procedure acEdUndoExecute(Sender: TObject);
    procedure acToolConfigExecute(Sender: TObject);
    procedure acToolExt1Execute(Sender: TObject);
    procedure acToolExt2Execute(Sender: TObject);
    procedure acToolExt3Execute(Sender: TObject);
    procedure acToolExt4Execute(Sender: TObject);
    procedure acToolExt5Execute(Sender: TObject);
    procedure acToolFindDecExecute(Sender: TObject);
    procedure acToolSel_P65pasExecute(Sender: TObject);
    procedure acToolSel_kickcExecute(Sender: TObject);
    procedure acViewPanRightExecute(Sender: TObject);
    procedure acViewPanLeftExecute(Sender: TObject);
    procedure acViewStatbarExecute(Sender: TObject);
    procedure acViewToolbarExecute(Sender: TObject);
    procedure acViewMsgPanExecute(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure fraEdit_ChangeEditorState(ed: TSynEditor);
    procedure DoSelectSample(Sender: TObject);
    procedure editChangeFileInform;
    procedure ReplaceDialog1Replace(Sender: TObject);
    procedure ToolBar5PaintButton(Sender: TToolButton; State: integer);
  published   //Form events
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    tic         : integer;  //Contador para temporización
    ticSynCheck : integer;  //Contador para temporizar la verifiación ed sintaxis
    actSynCheck : Boolean;  //Activa la verificación de sinatxis
    PopupEdit_N : integer; //Contador de ítems de menú
    curProj     : TPicPasProject; //Proyecto actual
    fraEditView1: TfraEditView;   //Panel de editores
    fraLeftPanel: TfraLateralPanel; //Panel lateral para explorador de archivos y Árbol de sintaxis
    fraMessages : TfraMessagesWin;
    procedure comp_AfterCheckSyn;
    procedure comp_BeforeCheckSyn;
    procedure comp_BeforeCompile;
    procedure comp_AfterCompile;
    procedure ConfigExtTool_RequirePar(var comLine: string);
    procedure fraEdit_RequireSetCompletion(ed: TSynEditor);
    procedure fraMessagesStatisDBlClick;
    procedure fraLeftPanel_selecFileExplorer;
    procedure fraEdit_RequireSynEditConfig(ed: TsynEdit);
    procedure ConfigChanged;
    procedure fraEdit_SelectEditor;
    procedure fraMessagesDblClickMessage(fileSrc: string; row, col: integer);
    procedure fraLeftPanel_OpenFile(filname: string);
    procedure MarkErrors;
    procedure ShowErrorInDialogBox;
    procedure UpdateIDE(CompName: string);
  public     //Compilers adapters
    currComp   : TAdapterBase;   //Compialdor actual
    adapter6502: TAdapter6502;  //Adaptador para compilado 6502
  public
    procedure SetLanguage(idLang: string);
  end;

var
  frmPrincipal: TfrmPrincipal;
var
  MSG_MODIFIED, MSG_SAVED, MSG_NOFILES, MSG_NOFOUND_ : string;
  MSG_REPTHIS, MSG_N_REPLAC, MSG_SYNFIL_NOF, MSG_FILSAVCOMP: string;
  MSG_PROJECT : String;

implementation
{$R *.lfm}
{ TfrmPrincipal }
procedure TfrmPrincipal.SetLanguage(idLang: string);
{Este método es usado para cambiar el idioma. La idea es llamar al método SetLanguage()
de todas las unidades accesibles desde aquí. A las unidades a las que no se tiene acceso,
se les llama de forma indirecta, mediante las unidades que si son accesibles.
Se puede poner este método (y los demás SetLanguage() ) en la seccion INITIALIZATION de
las unidades, pero se pone como un método independiente, para poder realizar cambio de
idioma, en tiempo de ejecución.}
begin
  if curLanguage = idLang then
    exit;  //no ha habido cambio de idioma
  curLanguage := idLang;
  Config.SetLanguage;
  fraMessages.SetLanguage;
  Compiler_PIC16.SetLanguage;
  ParserASM_6502.SetLanguage;
  //ParserDirec_PIC16.SetLanguage;
  {$I ..\_language\tra_FormPrincipal.pas}
end;
procedure TfrmPrincipal.fraLeftPanel_OpenFile(filname: string);
{El explorador de código, solicita abrir un archivo.}
begin
  fraEditView1.LoadFile(filname);
  Config.SaveToFile;  //guarda la configuración actual
end;
procedure TfrmPrincipal.fraLeftPanel_selecFileExplorer;
{Se ha seleccionado el modo de explorador de archivo,}
var
  ed: TSynEditor;
begin
  //Ubica el archivo actual en el explorador.
  ed := fraEditView1.ActiveEditor;
  if (ed<>nil) and (ed.FileName<>'') then begin
     //¿Vale la pena hacer esto?
     //fraLeftPanel.LocateFile(ed.FileName);
  end;
end;
procedure TfrmPrincipal.fraEdit_ChangeEditorState(ed: TSynEditor);
{Se produjo un cambio en el estado del editor actual. Estos cambios pueden ser:
- Modificación del editor actual "ed" (caracter agregado, eliminado, ...).
- Se acaba de seleccionar una ventana de edición.
- Se ha cambiado el compilador actual.
- El usuario graba el archivo del editor actual.
- Se ha producido una grabación automática del archivo porque se está compilando.).
}
begin
  {Activamos el contador de verificación de sintaxis, por si se necesita hacer, ya que
   ha habido un cambio en el archivo actual en edición.}
  if actSynCheck then ticSynCheck := 0;  //Reinicia cuenta.
  acArcSave.Enabled := ed.Modified;
  acEdUndo.Enabled  := ed.CanUndo;
  acEdRedo.Enabled  := ed.CanRedo;
  //Para estas acciones no es necesario controlarlas, porque son acciones pre-determinadas
//  acEdiCortar.Enabled := edit.canCopy;
//  acEdiCopiar.Enabled := edit.canCopy;
//  acEdiPegar.Enabled  := edit.CanPaste;
  ed.ClearMarkErr;  //Quita la marca de error que pudiera haber
end;
procedure TfrmPrincipal.fraEdit_SelectEditor;
{Se ha cambiado el estado de los editores: Se ha cambiado la selección, se ha
agregado o eliminado alguno.}
var
  ed: TSynEditor;
begin
  //Se trata de realizar solo las tareas necesarias. Para no cargar el proceso.
  if fraEditView1.Count = 0 then begin
    //No hay ventanas de edición abiertas
//    fraEditView1.Visible := false;
    acArcSaveAs.Enabled := false;
    acEdSelecAll.Enabled := false;

    acArcSave.Enabled := false;
    acEdUndo.Enabled  := false;
    acEdRedo.Enabled  := false;

    StatusBar1.Panels[3].Text := '';
    StatusBar1.Panels[4].Text := '';
  end else begin
    //Hay ventanas de edición abiertas
    ed := fraEditView1.ActiveEditor;
    acArcSaveAs.Enabled := true;
    acEdSelecAll.Enabled := true;

    fraEdit_ChangeEditorState(ed);  //Actualiza botones

    StatusBar1.Panels[3].Text := ed.CodArc;  //Codificación
    StatusBar1.Panels[4].Text := ed.FileName;  //Nombre de archivo
  end;
  editChangeFileInform;
end;
procedure TfrmPrincipal.fraEdit_RequireSynEditConfig(ed: TsynEdit);
{Se pide actualizar la configuración de un editor.}
begin
  ed.PopupMenu := PopupEdit;
  Config.fraCfgSynEdit.ConfigEditor(ed);
end;
procedure TfrmPrincipal.fraEdit_RequireSetCompletion(ed: TSynEditor);
{Solicita configurar el completado de código al resaltador.}
begin
  //Pasa requerimiento al compilador actual
  currComp.SetCompletion(ed);
end;
procedure TfrmPrincipal.fraMessagesStatisDBlClick;
//Doble clcik en la sección de estadísticas
begin

end;
procedure TfrmPrincipal.ConfigExtTool_RequirePar(var comLine: string);
{Se pide reemplazar parámetros en línea de comandos de Herramienta externa.}
begin
  comLine := StringReplace(comLine, '$(hexFile)', currComp.hexFilePath, [rfReplaceAll, rfIgnoreCase]);
  comLine := StringReplace(comLine, '$(mainFile)', currComp.mainFilePath, [rfReplaceAll, rfIgnoreCase]);
  comLine := StringReplace(comLine, '$(mainPath)', ExtractFileDir(currComp.mainFilePath), [rfReplaceAll, rfIgnoreCase]);
  comLine := StringReplace(comLine, '$(picModel)', currComp.CPUname, [rfReplaceAll, rfIgnoreCase]);
end;
procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin

  fraLeftPanel := TfraLateralPanel.Create(self);
  fraLeftPanel.Parent := self;
  //Configura panel de mensajes
  fraMessages := TfraMessagesWin.Create(self);
  fraMessages.Parent := panMessages;  //Ubica
  fraMessages.Align := alClient;
  fraMessages.OnDblClickMessage := @fraMessagesDblClickMessage;
  fraMessages.OnStatisDBlClick  := @fraMessagesStatisDBlClick;
  //Configura panel de edición
  fraEditView1 := TfraEditView.Create(self);
  fraEditView1.Parent := self;
  fraEditView1.OnChangeEditorState    := @fraEdit_ChangeEditorState;
  fraEditView1.OnSelectEditor         := @fraEdit_SelectEditor;
  fraEditView1.OnRequireSynEditConfig := @fraEdit_RequireSynEditConfig;
  fraEditview1.OnRequireSetCompletion := @fraEdit_RequireSetCompletion;
  //Guarda cantidad de ítems en menú contextul del editor para dar esa información a los
  //adaptadores.
  PopupEdit_N := PopupEdit.Items.Count;
  actSynCheck := true;
end;
procedure TfrmPrincipal.FormDestroy(Sender: TObject);
begin
  adapter6502.Destroy;
end;
procedure TfrmPrincipal.FormShow(Sender: TObject);
var
  Hay: Boolean;
  SR: TSearchRec;
begin
  //Alineamiento de panel izquierdo
  fraLeftPanel.Align := alLeft;
  fraLeftPanel.Visible := true;
  splLeft.Align := alLeft;
  AnchorTo(splLeft, akLeft, fraLeftPanel);
  //Alineamieanto de Panel derecho
  panRightPanel.Align := alRight;
  splRight.Align := alRight;
  //Frame de editores
  fraEditView1.Align := alClient;
  fraEditView1.tmpPath := patTemp;   //fija ruta de trabajo

  /////////// Crea adaptadores para compiladores soportados ///////////
  adapter6502:= TAdapter6502.Create(fraEditView1, panRightPanel);
  adapter6502.Init(fraLeftPanel.PageControl1, ImgActions16, ImgActions32, ActionList);
  adapter6502.OnBeforeCompile  := @comp_BeforeCompile;
  adapter6502.OnAfterCompile   := @comp_AfterCompile;
  adapter6502.OnBeforeCheckSyn := @comp_BeforeCheckSyn;
  adapter6502.OnAfterCheckSyn  := @comp_AfterCheckSyn;
  Config.RegisterAdapter(adapter6502);    //Registra adaptador en configuración
  //Fija compilador por defecto
  acToolSel_P65pasExecute(self);
  ///////////////////////////////////////////////////////

  Config.Init;   //necesario para poder trabajar
  Config.OnPropertiesChanges := @ConfigChanged;
  Config.fraCfgExtTool.OnReplaceParams := @ConfigExtTool_RequirePar;
  //Configura Panel lateral
  fraLeftPanel.OnOpenFile := @fraLeftPanel_OpenFile;
  fraLeftPanel.OnSelecFileExplorer := @fraLeftPanel_selecFileExplorer;
  fraLeftPanel.Init;
  //Termina configuración
  fraEditView1.InitMenuRecents(mnRecents, Config.fraCfgSynEdit.ArcRecientes);  //inicia el menú "Recientes"
  //Carga lista de ejemplos
  Hay := FindFirst(patSamples + DirectorySeparator + '*.pas', faAnyFile - faDirectory, SR) = 0;
  while Hay do begin
     //encontró archivo
    AddItemToMenu(mnSamples, '&'+ChangeFileExt(SR.name,''),@DoSelectSample);
    Hay := FindNext(SR) = 0;
  end;
  ConfigChanged;   //primera actualización

  //Carga últimos archivos abiertos
  if Config.LoadLast then fraEditView1.LoadListFiles(Config.filesClosed);
  Timer1.Enabled := true;
end;
procedure TfrmPrincipal.DoSelectSample(Sender: TObject);
//Se ha seleccionado un archivo de ejemplo.
var
  SamFil: String;
  it: TMenuItem;
begin
  it := TMenuItem(Sender);
  SamFil := patSamples + DirectorySeparator + it.Caption + '.pas';
  SamFil := StringReplace(SamFil,'&','',[rfReplaceAll]);
  //Carga archivo
  fraEditView1.LoadFile(SamFil);
end;
procedure TfrmPrincipal.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  lstClosedFiles: string;
begin
  if curProj<>nil then begin
    if not curProj.Close then begin
      CanClose := False;  //Cancela
      exit;
    end;
    curProj.Destroy;
  end;
  if fraEditView1.CloseAll(lstClosedFiles) then begin
     CanClose := false;   //cancela
  end else begin
    //Se va a cerrar. Guarda la lista de archivos quee staban abiertos
    Config.filesClosed := lstClosedFiles;
  end;
end;
procedure TfrmPrincipal.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //Guarda posición y tamaño de ventana
  Config.winState  := self.WindowState;
  if Config.winState = wsNormal then begin
    //Solo en este modo se actualizan las coordenadas
    Config.winXpos   := self.Left;
    Config.winYpos   := self.Top ;
    Config.winHeight := self.Height;
    Config.winWidth  := self.Width;
  end;
  Config.PanRightWidth := panRightPanel.Width;

  Config.PanLeftWidth := fraLeftPanel.Width;   //Guarda ancho
  Config.SaveToFile;  //guarda la configuración actual
end;
procedure TfrmPrincipal.Timer1Timer(Sender: TObject);
{Evento temporizado de Timer1. Cada 100 mseg.}
var
  ed: TSynEditor;
begin
  inc(tic);
  inc(ticSynCheck);
  if (tic mod 5 = 0) and StatusBar1.Visible then begin
    //Cada 0.5 seg, se actualiza la barra de estado
    if fraEditView1.Count = 0 then begin
      //No hay editores
       StatusBar1.Panels[0].Text := ''
    end else begin
      //Hay archivos abiertos
      ed := fraEditView1.ActiveEditor;
      //Actualiza Barra de estado
      if ed.Modified then
        StatusBar1.Panels[0].Text := MSG_MODIFIED
      else
        StatusBar1.Panels[0].Text := MSG_SAVED;
      //Actualiza cursor
      StatusBar1.Panels[1].Text := Format('%d,%d', [ed.SynEdit.CaretX, ed.SynEdit.CaretY]);
    end;
  end;
  if (ticSynCheck = 5) and (fraEditView1.Count>0 ) then begin
    {Se cumplió el tiempo para iniciar la verificación automática de sintaxis y hay
    archivos abiertos.}
//debugln('--Verif. Syntax.' + TimeToStr(now) + ':');
    currComp.CheckSyntax();
  end;
end;
procedure TfrmPrincipal.ToolBar5PaintButton(Sender: TToolButton; State: integer
);
{Paint the button for Select Compiler.}
var
  but: TToolButton;
  bRect : TRect;
  cv : TCanvas;
  txtAlt, yArr: integer;
begin

  but := ToolBar5.Buttons[0];
  bRect := but.BoundsRect;
  cv := but.Canvas;
  txtAlt := cv.TextHeight('X');
  //Dibuja flecha
  yArr := (but.height div 2) - 4;
  cv.Pen.Color := Config.PanTextCol;
  cv.Line(2,yArr  , 8, yArr);
  cv.Line(2,yArr+1, 8, yArr+1);

  cv.Line(3,yArr+2, 7, yArr+2);
  cv.Line(3,yArr+3, 7, yArr+3);

  cv.Line(4,yArr+4, 6, yArr+4);
  cv.Line(4,yArr+5, 6, yArr+5);

  cv.Line(5,yArr+6, 5, yArr+6);
  cv.Line(5,yArr+7, 5, yArr+7);

  //Dibuja ícono y texto
  ImgActions16.Draw(cv, bRect.Left+11, bRect.Top + (but.height div 2) - 8, 14);
  cv.Brush.Style := bsClear;  //Texto sin fondo
  cv.Font.Color := Config.PanTextCol;
  cv.Textout(31, but.height div 2 - (txtAlt div 2), but.Caption);
end;
procedure TfrmPrincipal.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  i: Integer;
begin
  //Carga archivo arrastrados
  for i:=0 to high(FileNames) do begin
    fraEditView1.LoadFile(FileNames[i]);
  end;
end;
procedure TfrmPrincipal.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  {Realmente, todo este código podrái ir dentro de de CodeTool.KeyDown.}
  if (Shift = [ssCtrl]) and (Key = VK_TAB) then begin
    if fraEditView1.HasFocus then begin
      if fraEditView1.Count>1 then begin
        fraEditView1.SelectNextEditor;
      end else begin
        //Debe haber solo una ventana
        if panRightPanel.Visible then panRightPanel.SetFocus;
      end;
    end else if panRightPanel.Focused then begin
      fraEditView1.SetFocus;
    end;
  end;
  if (Shift = [ssShift, ssCtrl]) and (Key = VK_TAB) then begin
    if fraEditView1.HasFocus then fraEditView1.SelectPrevEditor;
  end;
  if (Shift = [ssCtrl]) and (Key = VK_F4) then begin
    if fraEditView1.HasFocus then acArcCloseFileExecute(self);
//*** Comportamiento en el explorador de archivos.
//    if fraLeftPanel.HasFocus and (fraLeftPanel.FileSelected<>'') then
//       //Hay un archivo seleccionado
//       if fraEditView1.SelectEditor(fraLeftPanel.FileSelected) then begin
//         //Está abierto
//         curNode := fraLeftPanel.FileSelected;  //Guarda nodo seleccionado
//         acArcCloseFileExecute(self);  //Cierra archivo actual
//         fraLeftPanel.LocateFile(curNode);  //Restaura nodo seleccionado, porque
//         //Despues de cerrar
//         if fraLeftPanel.fraArcExplor1.TreeView1.Visible then
//           fraLeftPanel.fraArcExplor1.TreeView1.SetFocus;
//       end;
    Shift := []; Key := 0;  //para qie no pase
  end;
  //Pasa evento a COde Tool
  currComp.CTKeyDown(Sender, Key, Shift);
end;
procedure TfrmPrincipal.fraMessagesDblClickMessage(fileSrc: string; row,
  col: integer);
begin
  fraEditView1.SelectOrLoad(fileSrc, row, col, false);
end;
procedure TfrmPrincipal.ConfigChanged;
//Se han cambiado las opciones de configuración.
  procedure SetStateActionsProject(state: boolean);
  begin
    acArcSave.Enabled := state;
    acArcCloseProj.Enabled := state;
    acEdUndo.Enabled := state;
    acEdRedo.Enabled := state;
    acEdCut.Enabled := state;
    acEdCopy.Enabled := state;
    acEdPaste.Enabled := state;
  end;
  procedure SetToolbarSmall(tb: TToolBar);
  begin
    tb.ButtonHeight:=22;
    tb.ButtonWidth:=22;
    tb.Height:=26;
    tb.Images:=ImgActions16;
  end;
  procedure SetToolbarBig(tb: TToolBar);
  begin
    tb.ButtonHeight:=38;
    tb.ButtonWidth:=38;
    tb.Height:=42;
    tb.Images:=ImgActions32;
  end;
var
  cad: String;
  i: Integer;
  tool : TExternTool;
begin
  SetLanguage(copy(Config.language, 1, 2));
  if curProj = nil then begin
    SetStateActionsProject(false);
//    exit;
  end else begin
    SetStateActionsProject(true);
  end;
  //Posición y tamaño de ventana
  self.WindowState := Config.winState;
  if Config.winState = wsMinimized then begin
    self.WindowState := wsNormal;
  end;

  if Config.winState = wsNormal then begin
    self.Left   := Config.winXpos;
    self.Top    := Config.winYpos;
    self.Height := Config.winHeight;
    self.Width  := Config.winWidth;
  end;
  //Visibilidad del Panel izquierdo (explorador de archivo)
  fraLeftPanel.Visible := Config.ViewPanLeft;
  fraLeftPanel.Width   := Config.PanLeftWidth;
  splLeft.Visible := Config.ViewPanLeft;
  acViewPanLeft.Checked := Config.ViewPanLeft;

  //Visibilidad de La Barra de Estado
  StatusBar1.Visible := Config.ViewStatusbar;
  acViewStatbar.Checked:= Config.ViewStatusbar;

  //Visibilidad de la Barra de Herramientas
  CoolBar1.Visible    := Config.ViewToolbar;
  acViewToolbar.Checked:= Config.ViewToolbar;

  //Visibilidad del Panel de Mensajes
  panMessages.Visible:= Config.ViewPanMsg;
  Splitter2.Visible  := Config.ViewPanMsg;
  acViewMsgPan.Checked:= Config.ViewPanMsg;

  //Visibilidad del Panel derecho (Ensamblador)
  panRightPanel.Visible:= Config.ViewPanRight;
  splRight.Visible     := Config.ViewPanRight;
  acViewPanRight.Checked := Config.ViewPanRight;
  panRightPanel.Width  := Config.PanRightWidth;

  //Tamaño de la Barra de Herramientas
  case Config.StateToolbar of
  stb_SmallIcon: begin
    SetToolbarSmall(Toolbar2);
    SetToolbarSmall(Toolbar3);
    SetToolbarSmall(Toolbar4);
    SetToolbarSmall(Toolbar5);
    SetToolbarSmall(Toolbar6);
    CoolBar1.AutosizeBands;  //Update size
  end;
  stb_BigIcon: begin
    SetToolbarBig(ToolBar2);
    SetToolbarBig(ToolBar3);
    SetToolbarBig(ToolBar4);
    SetToolbarBig(ToolBar5);
    SetToolbarBig(ToolBar6);
    CoolBar1.AutosizeBands;  //Update size
  end;
  end;
  //Configura Explorador de código
  fraLeftPanel.BackColor := Config.FilExplBack;;
  fraLeftPanel.TextColor := Config.FilExplText;
  fraLeftPanel.fraArcExplor1.Filter.ItemIndex := Config.FilExpFiltyp;
  fraLeftPanel.fraArcExplor1.FilterChange(self);
  //Configura Visor de Mensajes
  fraMessages.BackColor := Config.MessPanBack;
  fraMessages.TextColor := Config.MessPanText;
  fraMessages.TextErrColor := Config.MessPanErr;
  fraMessages.BackSelColor := Config.MessPanSel;
  fraMessages.PanelColor := Config.PanelsCol;

  //Set color to Toolbars
  ToolBar2.Color := Config.PanelsCol;
  ToolBar3.Color := Config.PanelsCol;
  ToolBar4.Color := Config.PanelsCol;
  ToolBar5.Color := Config.PanelsCol;
  ToolBar6.Color := Config.PanelsCol;

  //Set color to Coolbars
  CoolBar1.Color := Config.PanelsCol;
  CoolBar1.Bands[0].Color := Config.PanelsCol;
  CoolBar1.Bands[1].Color := Config.PanelsCol;
  CoolBar1.Bands[2].Color := Config.PanelsCol;
  CoolBar1.Bands[3].Color := Config.PanelsCol;
  CoolBar1.Bands[4].Color := Config.PanelsCol;

  fraEditView1.Panel1.Color := Config.PanelsCol;
  //fraEditView1.Color :=  Config.PanelsCol;
  //Color de separadores
  Splitter2.Color := Config.SplitterCol;
  splLeft.Color := Config.SplitterCol;
  splRight.Color := Config.SplitterCol;
  //Solicita configura los editores activos
  fraEditView1.UpdateSynEditConfig;
  fraEditView1.TabViewMode := Config.TabEdiMode;
  //Configura accesos a herramientas externas.
  //Solo es aplicable a las primeras 5 herramientas
  acToolExt1.Visible := false;
  acToolExt2.Visible := false;
  acToolExt3.Visible := false;
  acToolExt4.Visible := false;
  acToolExt5.Visible := false;
  //Notifica al adaptador actual por si lo necesita
  currComp.NotifyConfigChanged( Config.MessPanBack,
    Config.MessPanText, Config.MessPanErr, Config.MessPanSel, Config.fraCfgSynEdit);
  for i:=0 to config.fraCfgExtTool.ExternTools.Count-1 do begin
    cad := config.fraCfgExtTool.ExternTools[i];
    tool.ReadFromString(cad);  //lee campos
    case i of
    0: if tool.ShowInTbar then begin
         acToolExt1.Visible := true;
         acToolExt1.Caption:= tool.name;
         acToolExt1.Hint := tool.name;
       end;
    1: if tool.ShowInTbar then begin
         acToolExt2.Visible := true;
         acToolExt2.Caption:= tool.name;
         acToolExt2.Hint := tool.name;
       end;
    2: if tool.ShowInTbar then begin
         acToolExt3.Visible := true;
         acToolExt3.Caption:= tool.name;
         acToolExt3.Hint := tool.name;
       end;
    3: if tool.ShowInTbar then begin
         acToolExt4.Visible := true;
         acToolExt4.Caption:= tool.name;
         acToolExt4.Hint := tool.name;
       end;
    4: if tool.ShowInTbar then begin
         acToolExt5.Visible := true;
         acToolExt5.Caption:= tool.name;
         acToolExt5.Hint := tool.name;
       end;
    end;
  end;
end;
procedure TfrmPrincipal.editChangeFileInform;
{Actualiza la barra de título, de acuerdo al estado}
var
  ed: TSynEditor;
begin
  ed := fraEditView1.ActiveEditor;
  if curProj= nil then begin
    //Modo de archivos. Actualiza nombre de archivo
    if fraEditView1.Count = 0 then begin
      Caption := NOM_PROG + ' - ' + VER_PROG  + ' - ' +MSG_NOFILES;
    end else begin  //Hay varios
      if ed.FileName='' then
        Caption := NOM_PROG + ' - ' + VER_PROG  + ' - ' + ed.Caption
      else
        Caption := NOM_PROG + ' - ' + VER_PROG  + ' - ' + ed.FileName;
    end;
  end else begin
    //Hay un proyecto abierto
    Caption := NOM_PROG + ' - ' + VER_PROG  + ' - ' + MSG_PROJECT + curProj.name;
  end;
  if (ed<>nil) and (ed.FileName<>'') then begin
//*** Verificar si es necesario
//     fraLeftPanel.LocateFile(ed.FileName);
  end;
end;
procedure TfrmPrincipal.FindDialog1Find(Sender: TObject);
var
  encon  : integer;
  buscado : string;
  opciones: TSynSearchOptions;
  curEdit: TSynEdit;
begin
  if fraEditView1.ActiveEditor = nil then exit;
  curEdit := fraEditView1.ActiveEditor.SynEdit;
  buscado := FindDialog1.FindText;
  opciones := [];
  if not(frDown in FindDialog1.Options) then opciones += [ssoBackwards];
  if frMatchCase in FindDialog1.Options then opciones += [ssoMatchCase];
  if frWholeWord in FindDialog1.Options then opciones += [ssoWholeWord];
  if frEntireScope in FindDialog1.Options then opciones += [ssoEntireScope];
  encon := curEdit.SearchReplace(buscado,'',opciones);
  if encon = 0 then
     MsgBox(MSG_NOFOUND_, [buscado]);
end;
procedure TfrmPrincipal.ReplaceDialog1Replace(Sender: TObject);
var
  encon, r : integer;
  buscado : string;
  opciones: TSynSearchOptions;
  curEdit: TSynEdit;
begin
  if fraEditView1.ActiveEditor = nil then exit;
  curEdit := fraEditView1.ActiveEditor.SynEdit;
  buscado := ReplaceDialog1.FindText;
  opciones := [ssoFindContinue];
  if not(frDown in ReplaceDialog1.Options) then opciones += [ssoBackwards];
  if frMatchCase in ReplaceDialog1.Options then opciones += [ssoMatchCase];
  if frWholeWord in ReplaceDialog1.Options then opciones += [ssoWholeWord];
  if frEntireScope in ReplaceDialog1.Options then opciones += [ssoEntireScope];
  if frReplaceAll in ReplaceDialog1.Options then begin
    //se ha pedido reemplazar todo
    encon := curEdit.SearchReplace(buscado,ReplaceDialog1.ReplaceText,
                              opciones+[ssoReplaceAll]);  //reemplaza
    MsgBox(MSG_N_REPLAC, [IntToStr(encon)]);
    exit;
  end;
  //reemplazo con confirmación
  ReplaceDialog1.CloseDialog;
  encon := curEdit.SearchReplace(buscado,'',opciones);  //búsqueda
  while encon <> 0 do begin
      //pregunta
      r := Application.MessageBox(pChar(MSG_REPTHIS), '', MB_YESNOCANCEL);
      if r = IDCANCEL then exit;
      if r = IDYES then begin
        curEdit.TextBetweenPoints[curEdit.BlockBegin,curEdit.BlockEnd] := ReplaceDialog1.ReplaceText;
      end;
      //busca siguiente
      encon := curEdit.SearchReplace(buscado,'',opciones);  //búsca siguiente
  end;
  MsgBox(MSG_NOFOUND_, [buscado]);
end;
procedure TfrmPrincipal.comp_BeforeCheckSyn;
begin
  fraMessages.InitCompilation(currComp, false);  //Limpia mensajes pero no pone mesaje inicial.
end;
procedure TfrmPrincipal.comp_AfterCheckSyn;
begin
  if fraMessages.HaveErrors then MarkErrors;
  fraMessages.EndCompilation(false);        //No muestra los resúmenes
end;
procedure TfrmPrincipal.comp_BeforeCompile;
{Se ha iniciado el proceso de compilación del compilador actual.}
begin
  fraMessages.InitCompilation(currComp, true);  //Limpia mensajes
  actSynCheck := false; //Desactiva alguna Verif. de sintaxis, en camino.
end;
procedure TfrmPrincipal.comp_AfterCompile;
{Ha terminado el proceso de compilación del compilador actual.}
begin
  actSynCheck := true; //Restaura las verifiaciones de sintaxis
  {Desactiva alguna Verif. de sintaxis, en camino, porque si se ha terminado
  de compilar, ya no tiene sentido hacer una verificación de sintaxis.
  Si bien con "actSynCheck" desactivado no se ejecutará la verificación de sintaxis,
  puede que haya quedado una cuenta en camino de "ticSynCheck" y generaría una
  verificación de sintaxis. Por eso fijamos "ticSynCheck" a un valor alto. }
  ticSynCheck := 1000;
  //Muestra y marca posibles errores
  if fraMessages.HaveErrors then begin
    fraMessages.EndCompilation;
    ShowErrorInDialogBox;
    MarkErrors;
    exit;
  end;
  fraMessages.EndCompilation(true);  //Muestra resúmenes.
end;
/////////////////// Acciones de Archivo /////////////////////
procedure TfrmPrincipal.acArcNewFileExecute(Sender: TObject);
begin
  fraEditView1.NewPasFile;
  fraEditView1.ActiveEditor.SynEdit.Text := currComp.SampleCode;
  fraEditView1.SetFocus;
end;
procedure TfrmPrincipal.acArcNewProjExecute(Sender: TObject);
begin
  if curProj<>nil then begin
    if not curProj.Close then exit;
    curProj.Destroy;
  end;
  curProj := TPicPasProject.Create;
  curProj.Open;
  fraEditView1.NewPasFile;
end;
procedure TfrmPrincipal.acArcOpenExecute(Sender: TObject);
begin
  fraEditView1.OpenDialog;
  fraEditView1.SetFocus;
  Config.SaveToFile;  //para que guarde el nombre del último archivo abierto
end;
procedure TfrmPrincipal.acArcCloseFileExecute(Sender: TObject);
begin
  fraEditView1.CloseEditor;
  fraEditView1.SetFocus;
end;
procedure TfrmPrincipal.acArcCloseProjExecute(Sender: TObject);
begin
  if curProj<>nil then begin
    if not curProj.Close then exit;
    curProj.Destroy;
  end;
end;
procedure TfrmPrincipal.acArcSaveExecute(Sender: TObject);
begin
  fraEditView1.SaveFile;
end;
procedure TfrmPrincipal.acArcSaveAsExecute(Sender: TObject);
begin
  fraEditView1.SaveAsDialog;
end;
procedure TfrmPrincipal.acArcQuitExecute(Sender: TObject);
begin
  frmPrincipal.Close;
end;
//////////// Acciones de Edición ////////////////
procedure TfrmPrincipal.acEdiUndoExecute(Sender: TObject);
begin
  if fraEditView1.ActiveEditor= nil then exit;
  fraEditView1.ActiveEditor.Undo;
end;
procedure TfrmPrincipal.acEdRedoExecute(Sender: TObject);
begin
  fraEditView1.Redo;
end;
procedure TfrmPrincipal.acEdSelecAllExecute(Sender: TObject);
begin
  fraEditView1.SelectAll;
end;
procedure TfrmPrincipal.acEdUndoExecute(Sender: TObject);
begin
  fraEditView1.Undo;
end;
procedure TfrmPrincipal.acEdiRedoExecute(Sender: TObject);
begin
  if fraEditView1.ActiveEditor= nil then exit;
  fraEditView1.ActiveEditor.Redo;
end;
procedure TfrmPrincipal.acEdiSelecAllExecute(Sender: TObject);
begin
  if fraEditView1.ActiveEditor= nil then exit;
  fraEditView1.ActiveEditor.SelectAll;
end;
//////////// Acciones de Búsqueda ////////////////
procedure TfrmPrincipal.acSearFindExecute(Sender: TObject);
begin
  FindDialog1.Execute;
end;
procedure TfrmPrincipal.acSearFindNxtExecute(Sender: TObject);
begin
  FindDialog1Find(self);
end;
procedure TfrmPrincipal.acSearFindPrvExecute(Sender: TObject);
begin
  if frDown in FindDialog1.Options then begin
    FindDialog1.Options := FindDialog1.Options - [frDown];  //Quita
    FindDialog1Find(self);
    FindDialog1.Options := FindDialog1.Options + [frDown];  //Restaura
  end else begin
    FindDialog1Find(self);
  end;
end;
procedure TfrmPrincipal.acSearReplacExecute(Sender: TObject);
begin
  ReplaceDialog1.Execute;
end;
//////////// Acciones de Ver ///////////////
procedure TfrmPrincipal.acViewStatbarExecute(Sender: TObject);
begin
  Config.ViewStatusbar:=not Config.ViewStatusbar;
end;
procedure TfrmPrincipal.acViewToolbarExecute(Sender: TObject);
begin
  Config.ViewToolbar:= not Config.ViewToolbar;
end;
procedure TfrmPrincipal.acViewMsgPanExecute(Sender: TObject);
begin
  Config.ViewPanMsg:= not Config.ViewPanMsg;
end;
procedure TfrmPrincipal.acViewPanLeftExecute(Sender: TObject);
begin
  Config.ViewPanLeft := not Config.ViewPanLeft;
end;
procedure TfrmPrincipal.acViewPanRightExecute(Sender: TObject);
begin
  Config.ViewPanRight := not Config.ViewPanRight;
end;
//////////// Acciones de Herramientas ///////////////
procedure TfrmPrincipal.acToolConfigExecute(Sender: TObject);
begin
  Config.Mostrar;
end;
//Seleccion del compilador
procedure TfrmPrincipal.UpdateIDE(CompName: string);
{Termina de hacer las configuraciones finales de la IDE al elegir un compilador.}
begin
  {Solicita la actualización de los resaltadores de sintaxis (con completado) y la
  herramienta Codetools para todos los editores abiertos.}
  fraEditView1.UpdateSynEditCompletion;
  //Actualiza barra de estado
  StatusBar1.Panels[2].Text := CompName;
  //Actualiza texto de lista desplegable de Barra de herramientas
  {Agrega espacios al final del nombre para que las rutinas de dimensionamiento no
  recorten el texto porque s eha movido a la derecha para poner el ícono.}
  ToolButton39.Caption := copy(CompName + '     ',1,18);
  CoolBar1.AutosizeBands;  //Update size
  //Para compilar de nuevo si está en modo de correccíón de Sintaxis
  if fraEditView1.ActiveEditor <> nil then begin
     fraEdit_ChangeEditorState(fraEditView1.ActiveEditor);
  end;
end;
procedure TfrmPrincipal.acToolSel_P65pasExecute(Sender: TObject);
{Se pide seleccionar el compilador P65pas}
begin
  currComp := adapter6502;         //Apunta a compilador
  //Actualiza lista de compiladores
  acToolSel_P65pas.Checked := true;
  acToolSel_kickc.Checked := false;
  //Actualiza configuración
  Config.ActivateAdapter(currComp);
  //Configura menús y Toolbar
  currComp.setMenusAndToolbar(extraMenu1, extraMenu2, extraMenu3, ToolBar4, PopupEdit,
    PopupEdit_N);
  //Termina configuración
  UpdateIDE(adapter6502.CompilerName);
end;
procedure TfrmPrincipal.acToolSel_kickcExecute(Sender: TObject);
{Se pide seleccionar el compilador PicPas}
begin
  currComp := adapter6502;         //Apunta a compilador
  //Actualiza lista de compiladores
  acToolSel_P65pas.Checked := false;
  acToolSel_kickc.Checked := true;
  //Actualiza configuración
  Config.ActivateAdapter(currComp);
  //Configura menús y Toolbar
  currComp.setMenusAndToolbar(extraMenu1, extraMenu2, extraMenu3, ToolBar4, PopupEdit,
    PopupEdit_N);
  //Termina configuración
  UpdateIDE(adapter6502.CompilerName+'65C02');
end;
procedure TfrmPrincipal.acToolExt1Execute(Sender: TObject);
begin
  Config.fraCfgExtTool.ExecTool(0);
end;
procedure TfrmPrincipal.acToolExt2Execute(Sender: TObject);
begin
  Config.fraCfgExtTool.ExecTool(1);
end;
procedure TfrmPrincipal.acToolExt3Execute(Sender: TObject);
begin
  Config.fraCfgExtTool.ExecTool(2);
end;
procedure TfrmPrincipal.acToolExt4Execute(Sender: TObject);
begin
  Config.fraCfgExtTool.ExecTool(3);
end;
procedure TfrmPrincipal.acToolExt5Execute(Sender: TObject);
begin
  Config.fraCfgExtTool.ExecTool(4);
end;
procedure TfrmPrincipal.acToolFindDecExecute(Sender: TObject);
{Ubica la declaración del elemento}
begin
  if fraEditView1.Count=0 then exit;
  currComp.GoToDeclaration;
end;
//Adicionales
procedure TfrmPrincipal.MarkErrors;
{Marca los errores del panel de mensajes, en la ventana activa del editor.
Los erroes solo se marcarán si es que se udican en la ventana activa del editor.}
var
  msg, filname: string;
  row, col, f: integer;
  ed: TSynEditor;
begin
  ed := fraEditView1.ActiveEditor;
  if fraMessages.HaveErrors then begin
    //Obtiene las coordenadas de los errores
     for f:=1 to fraMessages.grilla.RowCount -1 do begin
       if fraMessages.IsErroridx(f) then begin
         fraMessages.GetErrorIdx(f, msg, filname, row, col);  //obtiene información del error
         if (msg<>'') and (filname = ed.FileName) then begin
           //Hay error en el archivo actual
           ed.MarkError(Point(col, row));
         end;
       end;
     end;

//     fraMessages.GetFirstError(msg, filname, row, col);
//     if (msg<>'') and (filname = ed.FileName) then begin
//       //Hay error en el archivo actual
//       ed.MarkError(Point(col, row));
//     end;
  end;
end;
procedure TfrmPrincipal.ShowErrorInDialogBox;
{Verifica si se ha generado al menos un error en el Panel de mensajes. De ser así,
marca la linea del error en color rojo (cuando es posible) y muestra una Caja de
Diálogo con el mensaje de error.}
  procedure HighlightErrorLine(ed: TSynEditor; nLin, nCol: integer);
  {Pinta la línea de error "nLin" de color rojo, en el editor "ed". También osiciona el
  cursor en la coordenada: (nCol, nLin).}
  begin
    fraEditView1.SetFocus;
    //posiciona curosr
  //  ed.SynEdit.CaretY := nLin; //primero la fila
  //  ed.SynEdit.CaretX := nCol;
    ed.SynEdit.LogicalCaretXY := Point(nCol, nLin);
    //Define línea con error
    ed.linErr := nLin;
    ed.SynEdit.Invalidate;  //refresca
  end;
var
  msg, filname: string;
  row, col: integer;
begin
    fraMessages.GetFirstError(msg, filname, row, col);
    if msg='' then exit;
    //Selecciona posición de error en el Editor
    if filname <> '' Then begin
        fraEditView1.SelectOrLoad(filname);  //Selecciona o abre
         //Ya lo tenemos cargado
        If row <> -1 Then begin
           HighlightErrorLine(fraEditView1.ActiveEditor, row, col);
        end;
        if Config.ShowErMsg Then MsgErr(msg);
    end else begin   //no hay archivo de error
      if Config.ShowErMsg Then MsgErr(msg);
    end;
End;

end.
//1285
