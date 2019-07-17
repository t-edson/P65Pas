{                                   PicPas.
Compilador en Pascal para micorocntroladores PIC de la serie 16.

                                        Por Tito Hinostroza   22/08/2015 }
unit FormPrincipal;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, SynEdit, SynEditTypes, LazUTF8, Forms, Controls, Dialogs,
  Menus, ComCtrls, ActnList, StdActns, ExtCtrls, LCLIntf, LCLType, LCLProc,
  StdCtrls, SynFacilHighlighter, SynFacilUtils, MisUtils, XpresBas, CompBase,  //Para tener acceso a TCompilerBase
  Compiler_PIC16,
  FrameSyntaxTree, FormConfig, Globales,
  PicPasProject, FrameEditView, FrameMessagesWin, XpresElementsPIC, CodeTools,
  FrameCfgExtTool, FormDebugger, FormRAMExplorer;
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
    acToolTestPic10: TAction;
    acToolTestUnit: TAction;
    acToolSelPIC16: TAction;
    acToolSelPIC10: TAction;
    acToolSelPIC17: TAction;
    acToolASMDebug: TAction;
    acViewAsmPan: TAction;
    acToolRamExp: TAction;
    acToolExt4: TAction;
    acToolExt5: TAction;
    acToolExt2: TAction;
    acToolExt3: TAction;
    acToolExt1: TAction;
    acToolFindDec: TAction;
    acToolListRep: TAction;
    acToolConfig: TAction;
    acToolCompil: TAction;
    acToolPICExpl: TAction;
    acToolComEjec: TAction;
    acViewToolbar: TAction;
    acViewMsgPan: TAction;
    ActionList: TActionList;
    acViewStatbar: TAction;
    acViewSynTree: TAction;
    edAsm: TSynEdit;
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
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem24: TMenuItem;
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
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
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
    splSynTree: TSplitter;
    Splitter2: TSplitter;
    splEdPas: TSplitter;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    butSelCompiler: TToolButton;
    ToolButton22: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
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
    procedure acToolComEjecExecute(Sender: TObject);
    procedure acToolCompilExecute(Sender: TObject);
    procedure acToolASMDebugExecute(Sender: TObject);
    procedure acToolListRepExecute(Sender: TObject);
    procedure acToolConfigExecute(Sender: TObject);
    procedure acToolExt1Execute(Sender: TObject);
    procedure acToolExt2Execute(Sender: TObject);
    procedure acToolExt3Execute(Sender: TObject);
    procedure acToolExt4Execute(Sender: TObject);
    procedure acToolExt5Execute(Sender: TObject);
    procedure acToolFindDecExecute(Sender: TObject);
    procedure acToolRamExpExecute(Sender: TObject);
    procedure acToolSelPIC16Execute(Sender: TObject);
    procedure acToolTestPic16Execute(Sender: TObject);
    procedure acToolTestUnitExecute(Sender: TObject);
    procedure acViewAsmPanExecute(Sender: TObject);
    procedure acViewSynTreeExecute(Sender: TObject);
    procedure acViewStatbarExecute(Sender: TObject);
    procedure acViewToolbarExecute(Sender: TObject);
    procedure acViewMsgPanExecute(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure fraEdit_ChangeEditorState(ed: TSynEditor);
    procedure DoSelectSample(Sender: TObject);
    procedure editChangeFileInform;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ReplaceDialog1Replace(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure butSelCompilerClick(Sender: TObject);
  private
    Compiler16  : TCompiler_PIC16;
    Compiler    : TCompilerBase;
    tic         : integer;  //Contador para temporización
    ticSynCheck : integer;  //Contador para temporizar la verifiación ed sintaxis
    curProj     : TPicPasProject; //Proyecto actual
    hlAssem     : TSynFacilSyn;   //resaltador para ensamblador
    fraEditView1: TfraEditView;   //Panel de editores
    fraSynTree  : TfraSyntaxTree; //Árbol de sintaxis
    fraMessages : TfraMessagesWin;
    CodeTool    : TCodeTool;
    procedure CompileFile(filName: string; verifErr: boolean);
    procedure ConfigExtTool_RequirePar(var comLine: string);
    procedure Compiler16_AfterCompile;
    procedure Compiler16_RequireFileString(FilePath: string; var strList: TStrings);
    procedure fraEdit_RequireSetCompletion(ed: TSynEditor);
    procedure fraMessagesStatisDBlClick;
    procedure fraSynTreeSelecFileExplorer;
    procedure fraEdit_RequireSynEditConfig(ed: TsynEdit);
    procedure ChangeAppearance;
    procedure fraEdit_SelectEditor;
    procedure fraMessagesDblClickMessage(const srcPos: TSrcPos);
    procedure fraSynTreeOpenFile(filname: string);
    procedure fraSynTreeSelectElemen(var elem: TxpElement);
    procedure LoadAsmSyntaxEd;
    procedure MarcarError(ed: TSynEditor; nLin, nCol: integer);
    procedure MarkErrors;
    procedure VerificarError;
  public
    frmDebug: TfrmDebugger;
    procedure SetLanguage(idLang: string);
  end;

var
  frmPrincipal: TfrmPrincipal;
var
  MSG_MODIFIED, MSG_SAVED, MSG_NOFILES, MSG_NOFOUND_ : string;
  MSG_REPTHIS, MSG_N_REPLAC, MSG_SYNFIL_NOF, MSG_FILSAVCOMP: string;
  MSG_BASEL_COMP: string;
  MSG_MIDRAN_COMP: string;
  MSG_ENMIDR_COMP: String;
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
  fraSynTree.SetLanguage;
  fraEditView1.SetLanguage;
  fraMessages.SetLanguage;
  frmDebug.SetLanguage;
  Compiler_PIC16.SetLanguage;
  //ParserAsm_PIC16.SetLanguage;
  //ParserDirec_PIC16.SetLanguage;
  {$I ..\language\tra_FormPrincipal.pas}
  acToolSelPIC10.Caption := MSG_BASEL_COMP;
  acToolSelPIC16.Caption := MSG_MIDRAN_COMP;
  acToolSelPIC17.Caption := MSG_ENMIDR_COMP;
end;
procedure TfrmPrincipal.fraSynTreeSelectElemen(var elem: TxpElement);
begin
  fraEditView1.SelectOrLoad(elem.srcDec, false);
end;
procedure TfrmPrincipal.fraSynTreeOpenFile(filname: string);
{El explorador de código, solicita abrir un archivo.}
begin
  fraEditView1.LoadFile(filname);
  Config.SaveToFile;  //guarda la configuración actual
end;
procedure TfrmPrincipal.fraSynTreeSelecFileExplorer;
{Se ha seleccionado el modo de explorador de archivo,}
var
  ed: TSynEditor;
begin
  //Ubica el archivo actual en el explorador.
  ed := fraEditView1.ActiveEditor;
  if (ed<>nil) and (ed.FileName<>'') then begin
     fraSynTree.LocateFile(ed.FileName);
  end;
end;
procedure TfrmPrincipal.Compiler16_RequireFileString(FilePath: string; var strList: TStrings);
{El compilador está solicitando acceder a un STringList, con el contenido de "FilePath",
para evitar tener que leerlo de disco, y ahcer más rápido el acceso.}
var
  i: Integer;
  ed: TSynEditor;
begin
  i := fraEditView1.SearchEditorIdx(FilePath);
  if i <> -1 then begin
    //Tiene el archivo abierto. Pasa la referencia.
    ed := fraEditView1.editors[i];
    if Compiler.Compiling then ed.SaveFile;   //En compilación guarda siempre los archivos afectados
    strList := ed.SynEdit.Lines;
  end;
end;
procedure TfrmPrincipal.fraEdit_ChangeEditorState(ed: TSynEditor);
{Se produjo una modificación en el editor "ed"}
begin
  if not Compiler.Compiling then begin
    //En compilación no se activa la verificación automática de sintaxis
    ticSynCheck := 0;  //reinicia cuenta
  end;
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
  Config.ConfigEditor(ed);
end;
procedure TfrmPrincipal.fraEdit_RequireSetCompletion(ed: TSynEditor);
{Solicita configurar el completado de código al resaltador.}
begin
  CodeTool.SetCompletion(ed);
  //¿Y si el archivo no es Pascal?
end;
procedure TfrmPrincipal.fraMessagesStatisDBlClick;
//Doble clcik en la sección de estadísticas
begin

end;
procedure TfrmPrincipal.Compiler16_AfterCompile;
{Se genera después de realizar la compilación.}
begin
  //Refresca el árbol de sintaxis, para actualizar la estructura del árbol de sintaxis
  if fraSynTree.Visible then begin
    fraSynTree.Refresh;
  end;
end;
procedure TfrmPrincipal.ConfigExtTool_RequirePar(var comLine: string);
{Se pide reemplazar parámetros en línea de comandos de Herramienta externa.}
begin
  comLine := StringReplace(comLine, '$(hexFile)', Compiler.hexFilePath, [rfReplaceAll, rfIgnoreCase]);
  comLine := StringReplace(comLine, '$(mainFile)', Compiler.mainFilePath, [rfReplaceAll, rfIgnoreCase]);
  comLine := StringReplace(comLine, '$(mainPath)', ExtractFileDir(Compiler.mainFilePath), [rfReplaceAll, rfIgnoreCase]);
  comLine := StringReplace(comLine, '$(picModel)', Compiler.PICName, [rfReplaceAll, rfIgnoreCase]);
end;
procedure TfrmPrincipal.LoadAsmSyntaxEd;
{Carga archivo de sinatxis para el editor de ASM}
var
  synFile: String;
begin
  synFile := patSyntax + DirectorySeparator + 'P65Pas_Asm.xml';
  if FileExists(synFile) then begin
    hlAssem.LoadFromFile(synFile);
  end else begin
    MsgErr(MSG_SYNFIL_NOF, [synFile]);
  end;
end;
procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  //Es necesario crear solo una instancia del compilador.
  Compiler16 := TCompiler_PIC16.Create;  //Crea una instancia del compilador
  Compiler := Compiler16;  //Inicializa variable Compiler
  fraSynTree := TfraSyntaxTree.Create(self);
  fraSynTree.Parent := self;
  //configura panel de mensajes
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
  //Configura Árbol de sintaxis
  fraSynTree.OnSelectElemen := @fraSynTreeSelectElemen;
  fraSynTree.OnOpenFile := @fraSynTreeOpenFile;
  fraSynTree.OnSelecFileExplorer := @fraSynTreeSelecFileExplorer;
  //Carga un resaltador a la ventana de ensamblador
  hlAssem := TSynFacilSyn.Create(self);
  edAsm.Highlighter := hlAssem;
  LoadAsmSyntaxEd;
  CodeTool := TCodeTool.Create(fraEditView1);
  //Configura eventos de los compiladores
  Compiler16.OnRequireFileString := @Compiler16_RequireFileString;
  Compiler16.OnAfterCompile      := @Compiler16_AfterCompile;
  //Crea dinámicamente para poder inciailizarlo con comodidad
  frmDebug:= TfrmDebugger.Create(self);
end;
procedure TfrmPrincipal.FormDestroy(Sender: TObject);
begin
  frmDebug.Destroy;
  CodeTool.Destroy;
  hlAssem.Free;
  Compiler16.Destroy;
end;
procedure TfrmPrincipal.FormShow(Sender: TObject);
var
  Hay: Boolean;
  SR: TSearchRec;
begin
  fraSynTree.Align := alLeft;
  fraSynTree.Visible := true;
  splSynTree.Align := alLeft;
  AnchorTo(splSynTree, akLeft, fraSynTree);
  edAsm.Align := alRight;
  InicEditorC1(edAsm);
  splEdPas.Align := alRight;
  fraEditView1.Align := alClient;
  fraEditView1.tmpPath := patTemp;   //fija ruta de trabajo
  Config.Iniciar;   //necesario para poder trabajar
  Config.OnPropertiesChanges := @ChangeAppearance;
  Config.fraCfgExtTool.OnReplaceParams := @ConfigExtTool_RequirePar;
  CodeTool.SetCompiler(Compiler);
  fraSynTree.Init(Compiler.TreeElems);
  //Termina configuración
  fraEditView1.InitMenuRecents(mnRecents, Config.fraCfgSynEdit.ArcRecientes);  //inicia el menú "Recientes"
  ChangeAppearance;   //primera actualización
  //Carga lista de ejemplos
  Hay := FindFirst(patSamples + DirectorySeparator + '*.pas', faAnyFile - faDirectory, SR) = 0;
  while Hay do begin
     //encontró archivo
    AddItemToMenu(mnSamples, '&'+ChangeFileExt(SR.name,''),@DoSelectSample);
    Hay := FindNext(SR) = 0;
  end;
  //Inicia encabezado
  //Carga último archivo
  if Config.LoadLast then fraEditView1.LoadListFiles(Config.filesClosed);
  acToolSelPIC16Execute(self);  //Fija compilador por defecto
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
  Config.SynTreeWidth := fraSynTree.Width;   //Guarda ancho
  Config.SaveToFile;  //guarda la configuración actual
end;
procedure TfrmPrincipal.Timer1Timer(Sender: TObject);
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
  if Config.AutSynChk and (ticSynCheck = 5) then begin
    //Se cumplió el tiempo para iniciar la verificación automática de sintaxis
//    debugln('--Verif. Syntax.' + TimeToStr(now));
    if fraEditView1.Count>0 then begin
      //Hay archivo abiertos
      ed := fraEditView1.ActiveEditor;
      if (ed.SynEdit.Lines.Count <=1) and  (trim(ed.Text)='') then begin
        //Verifica rápidamente si hay texto en el editor
         fraMessages.InitCompilation(Compiler, false);  //Limpia mensajes
        exit;
      end;
      fraMessages.InitCompilation(Compiler, false);  //Limpia mensajes
      if Config.AutCompile then begin  //Compilación y enlace
        CompileFile(ed.FileName, false);  //No verifica error para evitar que el cursor se mueva mientras se escribe.
        edAsm.BeginUpdate(false);
        edAsm.Lines.Clear;
        Compiler.DumpCode(edAsm.Lines, (Config.AsmType = dvtASM),
                          Config.IncVarDec , Config.ExcUnused,
                          Config.IncAddress, true, Config.IncVarName );
        edAsm.EndUpdate;
      end else begin
        Compiler.Compile(ed.FileName, false);  //Solo compilación para verificar sintaxis
      end;
      //Puede haber generado error, los mismos que deben haberse mostrado en el panel.
      MarkErrors;  //Resalta errores, si están en el editor actual
      fraMessages.FilterGrid;  //Para que haga visible la lista de mensajes
    end;
  end;
end;
procedure TfrmPrincipal.butSelCompilerClick(Sender: TObject);
begin
  //Para facilitar el acceso al menú de compialdores
  butSelCompiler.DropdownMenu.PopUp;
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
var
  curNode: String;
begin
  {Realmente, todo este código podrái ir dentro de de CodeTool.KeyDown.}
  if (Shift = [ssCtrl]) and (Key = VK_TAB) then begin
    if fraEditView1.HasFocus then begin
      if fraEditView1.Count>1 then begin
        fraEditView1.SelectNextEditor;
      end else begin
        //Debe haber solo una ventana
        if edAsm.Visible then edAsm.SetFocus;
      end;
    end else if edAsm.Focused then begin
      fraEditView1.SetFocus;
    end;
  end;
  if (Shift = [ssShift, ssCtrl]) and (Key = VK_TAB) then begin
    if fraEditView1.HasFocus then fraEditView1.SelectPrevEditor;
  end;
  if (Shift = [ssCtrl]) and (Key = VK_F4) then begin
    if fraEditView1.HasFocus then acArcCloseFileExecute(self);
    if fraSynTree.HasFocus and (fraSynTree.FileSelected<>'') then
       //Hay un archivo seleccionado
       if fraEditView1.SelectEditor(fraSynTree.FileSelected) then begin
         //Está abierto
         curNode := fraSynTree.FileSelected;  //Guarda nodo seleccionado
         acArcCloseFileExecute(self);  //Cierra archivo actual
         fraSynTree.LocateFile(curNode);  //Restaura nodo seleccionado, porque
         //Despues de cerrar
         if fraSynTree.frmArcExplor1.TreeView1.Visible then
           fraSynTree.frmArcExplor1.TreeView1.SetFocus;
       end;
    Shift := []; Key := 0;  //para qie no pase
  end;
  //Pasa evento a COde Tool
  CodeTool.KeyDown(Sender, Key, Shift);
end;
procedure TfrmPrincipal.fraMessagesDblClickMessage(const srcPos: TSrcPos);
begin
  fraEditView1.SelectOrLoad(srcPos, false);
end;
procedure TfrmPrincipal.ChangeAppearance;
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
  //Visibilidad del explorador de código
  fraSynTree.Visible := Config.ViewSynTree;
  fraSynTree.Width   := Config.SynTreeWidth;
  splSynTree.Visible := Config.ViewSynTree;
  acViewSynTree.Checked := Config.ViewSynTree;

  //Visibilidad de La Barra de Estado
  StatusBar1.Visible := Config.ViewStatusbar;
  acViewStatbar.Checked:= Config.ViewStatusbar;

  //Visibilidad de la Barra de Herramientas
  ToolBar1.Visible   := Config.ViewToolbar;
  acViewToolbar.Checked:= Config.ViewToolbar;

  //Visibilidad del Panel de Mensajes
  panMessages.Visible:= Config.ViewPanMsg;
  Splitter2.Visible  := Config.ViewPanMsg;
  acViewMsgPan.Checked:= Config.ViewPanMsg;

  //Visibilidad del Visor de Ensamblador
  edAsm.Visible      := Config.ViewPanAssem;
  splEdPas.Visible   := Config.ViewPanAssem;
  acViewAsmPan.Checked:= Config.ViewPanAssem;
  //Tamaño de la Barra de Herramientas
  case Config.StateToolbar of
  stb_SmallIcon: begin
    ToolBar1.ButtonHeight:=22;
    ToolBar1.ButtonWidth:=22;
    ToolBar1.Height:=26;
    ToolBar1.Images:=ImgActions16;
  end;
  stb_BigIcon: begin
    ToolBar1.ButtonHeight:=38;
    ToolBar1.ButtonWidth:=38;
    ToolBar1.Height:=42;
    ToolBar1.Images:=ImgActions32;
  end;
  end;
  //Configura Explorador de código
  fraSynTree.BackColor := Config.CodExplBack;;
  fraSynTree.TextColor := Config.CodExplText;
  fraSynTree.frmArcExplor1.Filter.ItemIndex := Config.cexpFiltype;
  fraSynTree.frmArcExplor1.FilterChange(self);
  //Configura Visor de Mensajes
  fraMessages.BackColor := Config.MessPanBack;
  fraMessages.TextColor := Config.MessPanText;
  fraMessages.TextErrColor := Config.MessPanErr;
  fraMessages.BackSelColor := Config.MessPanSel;

  fraMessages.PanelColor := Config.PanelsCol;
  ToolBar1.Color := Config.PanelsCol;
  fraEditView1.Panel1.Color := Config.PanelsCol;
  //fraEditView1.Color :=  Config.PanelsCol;
  //Color de separadores
  Splitter2.Color := Config.SplitterCol;
  splSynTree.Color := Config.SplitterCol;
  splEdPas.Color := Config.SplitterCol;
  //Configura editor ASM
  Config.ConfigEditor(edAsm);
  LoadAsmSyntaxEd;
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
     fraSynTree.LocateFile(ed.FileName);
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
procedure TfrmPrincipal.CompileFile(filName: string; verifErr: boolean);
begin
  fraMessages.InitCompilation(Compiler, true);  //Limpia mensajes
  Compiler.incDetComm   := Config.IncComment2;   //Visualización de mensajes
  Compiler.OptBnkAftIF  := Config.OptBnkAftIF;
  Compiler.OptReuProVar := Config.ReuProcVar;
  Compiler.OptRetProc   := Config.OptRetProc;
  ticSynCheck := 1000; //Desactiva alguna Verif. de sintaxis, en camino.
  Compiler.Compiling := true;   //Activa bandera
  Compiler.Compile(filName, true);
  Compiler.Compiling := false;
  if fraMessages.HaveErrors then begin
    fraMessages.EndCompilation;
    if verifErr then VerificarError;
    MarkErrors;
    exit;
  end;
  fraMessages.EndCompilation;
end;
/////////////////// Acciones de Archivo /////////////////////
procedure TfrmPrincipal.acArcNewFileExecute(Sender: TObject);
begin
  fraEditView1.NewPasFile;
  with fraEditView1.ActiveEditor.SynEdit.Lines do begin
    Add('////////////////////////////////////////////');
    Add('// New program created in ' + DateToStr(now) + '}');
    Add('////////////////////////////////////////////');
    Add('program NewProgram;');
    if compiler = Compiler16 then begin
      Add('uses Commodore64;');
    end else begin  //Para los otros casos
      Add('uses Commodore64;');
    end;
    Add('//Declarations here');
    Add('  ');
    Add('begin');
    Add('  ');
    Add('  //Code here');
    Add('  ');
    Add('end.');
  end;
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
procedure TfrmPrincipal.acViewSynTreeExecute(Sender: TObject);
begin
  Config.ViewSynTree := not Config.ViewSynTree;
end;
procedure TfrmPrincipal.acViewAsmPanExecute(Sender: TObject);
begin
  Config.ViewPanAssem := not Config.ViewPanAssem;
end;
//////////// Acciones de Herramientas ///////////////
procedure TfrmPrincipal.acToolCompilExecute(Sender: TObject);
{Compila el contenido del archivo actual}
var
  filName: String;
begin
  if fraEditView1.ActiveEditor=nil then exit;
  self.SetFocus;
  filName := fraEditView1.ActiveEditor.FileName;
  if filName='' then begin
    //No tiene nombre. No debería pasar, porque "fraEditView1" debe generar nombres.
    if fraEditView1.SaveAsDialog then begin
      MsgExc(MSG_FILSAVCOMP);
      exit;
    end;
  end;

  CompileFile(filName, true);
  //Genera código ensamblador
  edAsm.BeginUpdate(false);
  edAsm.Lines.Clear;
  Compiler.DumpCode(edAsm.Lines, (Config.AsmType = dvtASM),
                    Config.IncVarDec, Config.ExcUnused,
                    Config.IncAddress, true, Config.IncVarName );
  edAsm.EndUpdate;
end;
procedure TfrmPrincipal.acToolComEjecExecute(Sender: TObject);
{Compila y ejecuta en la ventana de simulación}
begin
  acToolCompilExecute(self);
  if Compiler.CompiledUnit then exit;  //No es programa
  if not fraMessages.HaveErrors then begin
     frmDebug.Exec(Compiler);
     frmDebug.acGenRunExecute(self);
  end;
end;
procedure TfrmPrincipal.acToolASMDebugExecute(Sender: TObject);
begin
  frmDebug.Exec(Compiler);
end;
procedure TfrmPrincipal.acToolRamExpExecute(Sender: TObject);
begin
   frmRAMExplorer.Exec(Compiler);
end;
procedure TfrmPrincipal.acToolListRepExecute(Sender: TObject);
{Muestra un conteo de instrucciones.}
var
  edit: TSynEditor;
begin
  fraEditView1.NewLstFile;
  edit := fraEditView1.ActiveEditor;
  edit.SynEdit.BeginUpdate;
  Compiler.GenerateListReport(edit.SynEdit.Lines);
  edit.SynEdit.EndUpdate;
end;
procedure TfrmPrincipal.acToolTestUnitExecute(Sender: TObject);
{Inicia la prueba de unidades de las carpetas /device10, /device16 y /device17.}
  procedure TestDevicesUnits(var nFil: integer);
  var
    SearchRec: TSearchRec;
    directorio, nomArc: String;
  begin
    directorio := compiler.devicesPath;
    if FindFirst(directorio + DirectorySeparator + 'PIC*.pas', faDirectory, SearchRec) = 0 then begin
      repeat
        inc(nFil);
        nomArc := SysToUTF8(SearchRec.Name);
        if SearchRec.Attr and faDirectory = faDirectory then begin
          //directorio
        end else begin //archivo
          //Unidad de PIC
          nomArc := directorio + DirectorySeparator +  nomArc;
          DebugLn('Compiling: '+ nomArc);
          CompileFile(nomArc, true);
          if Compiler.HayError then break;
        end;
        fraMessages.AddInformation(Format('%d files processed...', [nFil]));
        Application.ProcessMessages;   //Para refrescar ventanas
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
  end;
var
  nFiles: Integer;
begin
  nFiles := 0;
  //Prueba Unidades de PIC16
  acToolSelPIC16Execute(self);  //Elige compilador
  TestDevicesUnits(nFiles);
  if Compiler.HayError then exit;
  MsgBox('%d files tested OK.', [nFiles]);
end;
procedure TfrmPrincipal.acToolTestPic16Execute(Sender: TObject);
  procedure TestUnits(var nFil: integer);
  var
    SearchRec: TSearchRec;
    directorio, nomArc: String;
  begin
    directorio := patApp + DirectorySeparator + 'testcode10';
    if FindFirst(directorio + DirectorySeparator + '*.pas', faDirectory, SearchRec) = 0 then begin
      repeat
        inc(nFil);
        nomArc := SysToUTF8(SearchRec.Name);
        if SearchRec.Attr and faDirectory = faDirectory then begin
          //directorio
        end else begin //archivo
          //Unidad de PIC
          nomArc := directorio + DirectorySeparator +  nomArc;
          DebugLn('Compiling: '+ nomArc);
          CompileFile(nomArc, true);
          if Compiler.HayError then break;
        end;
        fraMessages.AddInformation(Format('%d files processed...', [nFil]));
        Application.ProcessMessages;   //Para refrescar ventanas
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
  end;
var
  nFiles: Integer;
begin
  nFiles := 0;
  acToolSelPIC16Execute(self);
  TestUnits(nFiles);
  if Compiler.HayError then exit;
  MsgBox('%d files tested OK.', [nFiles]);
end;
procedure TfrmPrincipal.acToolConfigExecute(Sender: TObject);
begin
  Config.Mostrar;
end;
procedure TfrmPrincipal.acToolSelPIC16Execute(Sender: TObject);
begin
  Compiler := Compiler16;
  acToolSelPIC10.Checked := false;
  acToolSelPIC16.Checked := true;
  acToolSelPIC17.Checked := false;
  StatusBar1.Panels[2].Text := MSG_MIDRAN_COMP;
  //Para compilar de nuevo si está en modo de correccíón de Sintaxis
  if fraEditView1.ActiveEditor <> nil then begin
     fraEdit_ChangeEditorState(fraEditView1.ActiveEditor);
  end;
  //Para recargar CodeTools en todos los editores abiertos
  CodeTool.SetCompiler(Compiler);
  fraEditView1.UpdateSynEditCompletion;
  //Inicia árbol de sintaxis
  fraSynTree.Init(Compiler.TreeElems);
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
  CodeTool.GoToDeclaration;
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
procedure TfrmPrincipal.VerificarError;
//Verifica si se ha producido algún error en el preprocesamiento y si lo hay
//Ve la mejor forma de msotrarlo
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
           MarcarError(fraEditView1.ActiveEditor, row, col);
        end;
        if Config.ShowErMsg Then MsgErr(msg);
    end else begin   //no hay archivo de error
      if Config.ShowErMsg Then MsgErr(msg);
    end;
End;
procedure TfrmPrincipal.MarcarError(ed: TSynEditor; nLin, nCol: integer);
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

end.

