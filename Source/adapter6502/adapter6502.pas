{Adapter for supporting the P65Pas compiler in the IDE.
This is a mandatory file and the entry point for suppporting an controling a compiler
from the IDE.
All compilers supported must have an adapter and a unit like this.}
unit adapter6502;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, ComCtrls, Controls, ActnList, Menus, ExtCtrls, Graphics,
  Forms, SynEdit, adapterBase, CodeTools6502, Compiler_PIC16, LexPas,
  FrameEditView, Globales, FrameCfgSynEdit, MisUtils, SynFacilHighlighter,
  EditView, MiConfigXML, FrameStatist6502, FrameSynTree6502,
  FormAdapter6502, FrameCfgAfterChg6502, FrameCfgCompiler6502, FormDebugger6502,
  FormRAMExplorer6502, FrameCfgAsmOut6502;
type
  { TAdapter6502 }
  TAdapter6502 = class(TAdapterBase)
  public
    const COMP_NAME = 'P65Pas';
  private //Referencias a páginas de configuración.
    {Estas son las páginas que ofrece el form. de configuración para que el adaptador
    pueda usar. Aunque formalmente deberían ser objetos TTabSheet, en realidad son
    objetos TScrollbox, porque se están usanddo estos controles como contenedores dentro
    de cada página del TPageControl, para odrecer la facilidad de "scroll" cuando el
    frame que se registra es más grande que el tamaño que define frmConfig.}
    pEnvExt1 : TConfigPage;
    pEdiExt1 : TConfigPage;
    pCompiler: TConfigPage;
    pCompExt1: TConfigPage;
    pCompExt2: TConfigPage;
    pCompExt3: TConfigPage;
    procedure CompilerMessageBox(txt: string; mode: integer);
    procedure ReadCompilerSettings(var pars: string);
  private
    //Herramienta de completado y manejo de código
    CodeTool    : TCodeTool;
    //Compilador.
    Compiler    : TCompiler_PIC16;
    //Referencia al frame de edición
    fraEditView1: TfraEditView;
    //Referencia al editor lateral (Ensamblador)
    panRightPanel: TPanel;
    edAsm        : TSynEdit;
    hlAssem      : TSynFacilSyn;   //resaltador para ensamblador
    procedure LoadAsmSyntaxEd;
  private     //Herramientas adicionales
    fraStatis     : TfraStatist6502;  //Frame de estadísticas
    fraSynTree    : TfraSynxTree6502; //Frame de árbol de sintaxis
    adapterForm   : TfrmAdapter6502;   //Formulario principal
    frmDebug      : TfrmDebugger6502;
    frmRAMExplorer: TfrmRAMExplorer6502;
  private    //Otros
    Compiling   : Boolean;  //Bandera. Indica que se ha pedido ejceutar una compilación.
    procedure Compiler_RequireFileString(FilePath: string;
      var strList: TStrings);
    procedure CompilerError(errTxt: string; const srcPos: TSrcPos);
    procedure CompilerInfo(infTxt: string; const srcPos: TSrcPos);
    procedure CompilerWarning(warTxt: string; const srcPos: TSrcPos);
    procedure fraSynTreeLocateElemen(fileSrc: string; row, col: integer);
    procedure UpdateTools;
  public      //Acciones adicionales de "adapterForm"
    procedure CompileAndExec(Sender: TObject);
    procedure ASMDebug(Sender: TObject);
    procedure acRamExpExecute(Sender: TObject);
    procedure ListReport;
    procedure FindDeclarat;
    procedure TestUnit;
  public      //Información
    function CompilerName: string; override;
    function hexFilePath: string; override;
    function mainFilePath: string; override;
    function CPUname: string; override;
    function RAMusedStr: string; override;
    function SampleCode: string; override;
  public      //Manejo de Codetool
    procedure SetCompletion(ed: TSynEditor); override;
    procedure CTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure GoToDeclaration; override;
  public      //Ejecución
    nErrors: Integer;
    procedure Compile; override;
    procedure CheckSyntax; override;
//    procedure UpdateCompletionForEditors; override;
    procedure NotifyConfigChanged(MessPanBack, MessPanText, MessPanErr,
      MessPanSel: TColor; mainEditorCfg: TfraCfgSynEdit); override;
  public      //Frames de configuración
    fraCfgAfterChg: TfraCfgAfterChg6502;
    fraCfgCompiler: TfraCfgCompiler6502;
    fraCfgAsmOut  : TfraCfgAsmOut6502;
  public      //Inicialización
    procedure Init(pagControl: TPageControl; imgList16, imglist32: TImageList;
      actList: TActionList);
    procedure ConfigCreate(frmConfig: TComponent; EnvExt1, EdiExt1,
      _Compiler, CompExt1, CompExt2, CompExt3: TConfigPage); override;
    procedure ConfigInit(cfgFile: TMiConfigXML); override;
    procedure ConfigActivate; override;
    procedure setMenusAndToolbar(menu1, menu2, menu3: TMenuItem; toolbar: TToolBar;
      popupEdit: TPopupMenu; popupEditCount: integer); override;
    constructor Create(fraEdit0: TfraEditView; panRightPanel0: TPanel);
    destructor Destroy; override;
  end;
resourcestring
  MSG_SYNFIL_NOF = 'Syntax file not found: %s';

implementation
{ TAdapter6502 }
procedure TAdapter6502.Compiler_RequireFileString(FilePath: string;
  var strList: TStrings);
{El compilador está solicitando acceder a un STringList, con el contenido de "FilePath",
para evitar tener que leerlo de disco, y hacer más rápido el acceso.}
var
  i: Integer;
  ed: TSynEditor;
begin
  i := fraEditView1.SearchEditorIdx(FilePath);
  if i <> -1 then begin
    //Tiene el archivo abierto. Pasa la referencia.
    ed := fraEditView1.editors[i];
    if Compiling then begin
      //En compilación guarda siempre los archivos afectados.
      ed.SaveFile;
    end else begin
      {En verificación de sintaxis no es coveniente. Puede resultar molesto al usuario.
      A menos que tenga activada alguna opción de "Autosave".}
    end;
    strList := ed.sedit.Lines;
  end;
end;
procedure TAdapter6502.CompilerError(errTxt: string; const srcPos: TSrcPos);
var
  fName: String;
begin
  inc(nErrors);  //Lleva la cuenta
  fName := compiler.ctxFile(srcPos);
  if OnError<>nil then OnError(errTxt, fname, srcPos.row, srcPos.col);
end;
procedure TAdapter6502.CompilerWarning(warTxt: string; const srcPos: TSrcPos);
var
  fName: String;
begin
  fName := compiler.ctxFile(srcPos);
  if OnWarning<>nil then OnWarning(warTxt, fname, srcPos.row, srcPos.col);
end;
procedure TAdapter6502.fraSynTreeLocateElemen(fileSrc: string; row, col: integer
  );
{Se pide localizar la posición en archivo de un elemento}
begin
  fraEditView1.SelectOrLoad(fileSrc, row, col, false);
end;
procedure TAdapter6502.CompilerInfo(infTxt: string; const srcPos: TSrcPos);
var
  fName: String;
begin
  fName := compiler.ctxFile(srcPos);
  if OnInfo<>nil then OnInfo(infTxt, fname, srcPos.row, srcPos.col);
end;
//Acciones adicionales de "adapterForm"
procedure TAdapter6502.CompileAndExec(Sender: TObject);
{Compila y ejecuta en la ventana de simulación}
begin
  Compile;
  if Compiler.IsUnit then exit;  //No es programa
  if nErrors=0 then begin
     frmDebug.Exec(Compiler);
     frmDebug.acGenRunExecute(self);
  end;
end;
procedure TAdapter6502.ASMDebug(Sender: TObject);
begin
    frmDebug.Exec(Compiler);
end;
procedure TAdapter6502.acRamExpExecute(Sender: TObject);
begin
     frmRAMExplorer.Exec(Compiler, 1);
end;
procedure TAdapter6502.ListReport;
var
  edit: TSynEditor;
begin
  fraEditView1.NewLstFile;
  edit := fraEditView1.ActiveEditor;
  edit.sedit.BeginUpdate;
  Compiler.GenerateListReport(edit.sedit.Lines);
  edit.sedit.EndUpdate;
end;
procedure TAdapter6502.FindDeclarat;
begin
  if fraEditView1.Count=0 then exit;
  CodeTool.GoToDeclaration;
end;
procedure TAdapter6502.TestUnit;
{Inicia la prueba de unidades de las carpetas /device10, /device16 y /device17.}
//  procedure TestDevicesUnits(var nFil: integer);
//  var
//    SearchRec: TSearchRec;
//    directorio, nomArc: String;
//  begin
//    directorio := compiler.devicesPath;
//    if FindFirst(directorio + DirectorySeparator + 'PIC*.pas', faDirectory, SearchRec) = 0 then begin
//      repeat
//        inc(nFil);
//        nomArc := SysToUTF8(SearchRec.Name);
//        if SearchRec.Attr and faDirectory = faDirectory then begin
//          //directorio
//        end else begin //archivo
//          //Unidad de PIC
//          nomArc := directorio + DirectorySeparator +  nomArc;
//          DebugLn('Compiling: '+ nomArc);
//          CompileFile(nomArc, true);
//          if Compiler.HayError then break;
//        end;
//        fraMessages.AddInformation(Format('%d files processed...', [nFil]));
//        Application.ProcessMessages;   //Para refrescar ventanas
//      until FindNext(SearchRec) <> 0;
//      FindClose(SearchRec);
//    end;
//  end;
//var
//  nFiles: Integer;
begin
//  nFiles := 0;
//  //Prueba Unidades de PIC16
//  acToolSelPIC16Execute(self);  //Elige compilador
//  TestDevicesUnits(nFiles);
//  if Compiler.HayError then exit;
//  MsgBox('%d files tested OK.', [nFiles]);
end;
//Información
function TAdapter6502.CompilerName: string;
begin
  exit(COMP_NAME);
end;
function TAdapter6502.hexFilePath: string;
begin
  exit(compiler.hexFilePath);
end;
function TAdapter6502.mainFilePath: string;
begin
  exit(compiler.mainFilePath);
end;
function TAdapter6502.CPUname: string;
begin
  exit(compiler.PICName)
end;
function TAdapter6502.RAMusedStr: string;
begin
  exit(compiler.RAMusedStr);
end;
function TAdapter6502.SampleCode: string;
begin
  exit(
  '////////////////////////////////////////////' + LineEnding +
  '// New program created in ' + DateToStr(now) + '}' + LineEnding +
  '////////////////////////////////////////////' + LineEnding +
  'program NewProgram;' + LineEnding +
  'uses Commodore64;' + LineEnding +
  '//Declarations here' + LineEnding +
  '  ' + LineEnding +
  'begin' + LineEnding +
  '  ' + LineEnding +
  '  //Code here' + LineEnding +
  '  ' + LineEnding +
  'end.'
  );
end;
//Manejo de Codetool
procedure TAdapter6502.SetCompletion(ed: TSynEditor);
begin
  CodeTool.SetCompletion(ed);
end;
procedure TAdapter6502.CTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  CodeTool.KeyDown(Sender, Key, Shift);
end;
procedure TAdapter6502.GoToDeclaration;
begin
  CodeTool.GoToDeclaration;
end;
//Ejecución
procedure TAdapter6502.UpdateTools;
{Actualiza las herramientas adicionales que se incluyen con el compilador.}
var
  usedRAM, usedROM, usedSTK, zoom: single;
begin
  //Actualiza el árbol de sintaxis
  fraSynTree.Refresh;
  //Actualiza frame de estadísticas de uso
  if nErrors=0 then begin
    //No hay error
    compiler.GetResourcesUsed(usedRAM, usedROM, usedSTK);
    fraStatis.Update(usedRAM, usedROM, usedSTK);
  end else begin
    //Hubo errores
    fraStatis.Update(0, 0, 0);
  end;
  //Actualiza ventana de ensamblador.
  edAsm.BeginUpdate(false);
  edAsm.Lines.Clear;
  Compiler.DumpCode(edAsm.Lines, (fraCfgAsmOut.AsmType = dvtASM),
                    fraCfgAsmOut.IncVarDec , fraCfgAsmOut.ExcUnused,
                    fraCfgAsmOut.IncAddress, true, fraCfgAsmOut.IncVarName );
  edAsm.EndUpdate;
  //Actualiza explorador de RAM
  zoom := frmRAMExplorer.zoom; //Zoom actual
  if frmRAMExplorer.Visible then frmRAMExplorer.UpdateScreen(compiler);
end;
procedure TAdapter6502.ReadCompilerSettings(var pars: string);
{Read the compiler setting from the Setting frame and add them to "pars".}
begin
  if fraCfgCompiler.ReuProcVar then AddLine(pars, '-Ov');  //Reusar variables de proced.
  if fraCfgCompiler.OptRetProc then AddLine(pars, '-Or');  //Optimizar Retorno de proced.
  AddLine(pars, '-Fu"' + fraCfgCompiler.unitPathExpanded + '"');    //Agrega esta ruta a las rutas de unidades del compilador.
  if fraCfgAsmOut.IncComment  then AddLine(pars, '-Ac');   //Comentario detallado
end;
procedure TAdapter6502.Compile;
{Ejecuta el compilador para generar un archivo binario de salida.}
var
  ed: TSynEditor;
  pars: string = '';
begin
  //Validación de editor disponible
  if fraEditView1.ActiveEditor=nil then exit;
  ed := fraEditView1.ActiveEditor;
  if ed.FileName='' then begin
    //No tiene nombre. No debería pasar, porque "fraEditView1" debe generar nombres.
    if fraEditView1.SaveAsDialog then begin
      MsgExc('File must be saved before compiling.' {MSG_FILSAVCOMP});
      exit;
    end;
  end;
  //Lee configuración de compilación
  ReadCompilerSettings(pars);
  //Inicio de compilación
  nErrors := 0;
  if OnBeforeCompile<>nil then OnBeforeCompile();
  Compiling := true;   //Activa bandera para saber que queremos compilar.
  Compiler.Exec(ed.FileName, '', pars);
  Compiling := false;
  if OnAfterCompile<>nil then OnAfterCompile();
  UpdateTools;
end;
procedure TAdapter6502.CheckSyntax;
{Ejecuta el compilador para realizar una verificación de sintaxis.}
var
  ed: TSynEditor;
  pars: string = '';
begin
  //Validación de editor disponible
  if fraEditView1.ActiveEditor=nil then exit;
  ed := fraEditView1.ActiveEditor;
  if ed.FileName='' then exit;
  //Verifica rápidamente si hay texto en el editor
  if (ed.sedit.Lines.Count<=1) and (trim(ed.Text)='') then exit;
  //Lee configuración de verif. de sintaxis.
  case fraCfgAfterChg.actAfterChg of
    0: begin  //No actions
      //pars := '-Cn' + LineEnding + '-Dn';  //<No action>
      exit;
    end;
    1: begin  //Do Only Analysis
      AddLine(pars, '-Ca');
      AddLine(pars, '-Dn');  //Disable directive messages
      ReadCompilerSettings(pars);
    end;
    2: begin  //Do Analysis and Optimization.
      AddLine(pars, '-Cao');
      AddLine(pars, '-Dn');  //Disable directive messages
      ReadCompilerSettings(pars);
    end;
    3: begin  //Do complete compilation.
      AddLine(pars, '-C');
      AddLine(pars, '-Dn');  //Disable directive messages
      ReadCompilerSettings(pars);
    end;
  end;
  //Inicio de compilación
  nErrors := 0;
  if OnBeforeCheckSyn<>nil then OnBeforeCheckSyn();
  Compiler.Exec(ed.FileName, '', pars);
  if OnAfterCheckSyn<>nil then OnAfterCheckSyn();
  UpdateTools;
end;
procedure TAdapter6502.NotifyConfigChanged(MessPanBack, MessPanText,
          MessPanErr, MessPanSel: TColor; mainEditorCfg: TfraCfgSynEdit);
{Se está notificando al adaptador que ha habido un cambio en la configuración y que
probablemnete modifica la apariencia de la IDE. Probablemente se hayan modificado los
atributos de los resaltadores de sintaxis. Puede que se hayan modificado, también, alguna
de las propiedades que usa este adaptador.}
begin
  //Actualizamos la apariencia del árbol de sintaxis, usando colores de la configuración
  //global, en vez de manejar nuestro propio Frame de configuración.
  fraSynTree.SetBackColor(MessPanBack);
  fraSynTree.SetTextColor(MessPanText);
  //Configuramos nuestro editor ASM usando la misma configuración que el editor principal.
  {Otra opción sería crear nuestro propio Frame de configuración, pero mejor usamos el
  de la IDE y aprovechamos sus facilidades de manejo de temas.}
  mainEditorCfg.ConfigEditor(edAsm);
  LoadAsmSyntaxEd;
end;
procedure TAdapter6502.CompilerMessageBox(txt: string; mode: integer);
{Se pide mostrar un cuadro de diálogo con mensaje.}
begin
  if mode = 0 then MsgBox(txt);
  if mode = 1 then MsgExc(txt);
  if mode = 2 then MsgErr(txt);
end;
//Inicialización
procedure TAdapter6502.LoadAsmSyntaxEd;
{Carga archivo de sinatxis para el editor de ASM}
var
  synFile: String;
begin
  //Carga sintaxis de la carpeta de sintaxis
  synFile := patSyntax + DirectorySeparator + 'P65Pas_Asm.xml';
  if FileExists(synFile) then begin
    hlAssem.LoadFromFile(synFile);
  end else begin
    MsgErr(MSG_SYNFIL_NOF, [synFile]);
  end;
end;
procedure TAdapter6502.Init(pagControl: TPageControl; imgList16,
  imglist32: TImageList; actList: TActionList);
{Inicializa el adaptador. Eso implica preparar la IDE para que soporte a este nuevo
compilador que se está registrando.
Solo se debe ejecutar esta rutina una vez al inicio.
Parámetros:
  * pagControl -> Es el contenedor de la barra lateral izquierda (donde está el navegador
  de archivos), donde se crearán las herramientas que ofrece este compilador en esta
  barra. Lo más común es solo crear el árbol de sintaxis.
  * imgList16, imgList32 -> Son las listas de imágenes del formulario principal donde se
  deben registrar los íconos para ser usados por el menú y la barra de herramientas.
  * actList -> Control TActionList del formulario principal a donde se insertarán las
  acciones.
  * mainMenu -> MEnú principal de la IDE. En ese menú se creará una entrada adicional
  para acceder a las opciones del compilador.
}
var
  tab: TTabSheet;
begin
  //Configura idioma
  curLanguage := 'en';
  Compiler_PIC16.SetLanguage;
  //Agrega los íconos de "adapterForm" a los ImageList
  adapterForm.AddActions(imgList16, imgList32, actList, COMP_NAME);
  //Agrega las herramientas de este compilador
  tab := pagControl.AddTabSheet;    //Agrega nuevo panel
  tab.Name := 'SyntaxTree_'+ COMP_NAME;
  tab.Caption := 'Syntax Tree';
  tab.Hint := COMP_NAME;     //Lo marca aquí para saber que es de este compilador.
  fraSynTree.Parent := tab;
  fraSynTree.Visible := true;
  fraSynTree.Align := alClient;
  fraSynTree.OnLocateElemen  := @fraSynTreeLocateElemen;
//  //Asigna referencias a formularios de configuración
//  fraCfgAfterChg := frmCfgAfterChg0;
//  fraCfgCompiler := fraCfgCompiler0;
//  fraCfgAsmOut   := fraCfgAsmOut0;
  //COnfigura editor de ensamblador
  edAsm.Parent := panRightPanel;
  edAsm.Align := alClient;
  edAsm.Highlighter := hlAssem;
  InicEditorC1(edAsm);
end;
procedure TAdapter6502.ConfigCreate(frmConfig: TComponent; EnvExt1, EdiExt1,
  _Compiler, CompExt1, CompExt2, CompExt3: TConfigPage);
{Se pide crear los frames de configuración, que se usarán en el formulario
de configuración general de la IDE.
Notar que al igual que este adaptador, otros adaptadores también deben crear sus
frames de configuración para que sus configuraciones se guarden en disco.
Notar también que frames de distintos compiladores pueden crearse en la misma página
y en la misma posición, pero solo uno se hará visible cuando se elija el compilador
de trabajo y se llame  a ConfigInit.}
begin
  //Guarda las referencias de las páginas disponibles, para usarlas en ConfigInit().
  pEnvExt1  := EnvExt1;
  pEdiExt1  := EdiExt1;
  pCompiler := _Compiler;
  pCompExt1 := CompExt1;
  pCompExt2 := CompExt2;
  pCompExt3 := CompExt3;

  //Crea los frames que vamos a usar en las páginas apropiadas
  fraCfgAfterChg := TfraCfgAfterChg6502.Create(frmConfig);
  fraCfgAfterChg.Parent := pEdiExt1.scrollBox;
  fraCfgAfterChg.Left := 0;
  fraCfgAfterChg.Top := 0;
  pEdiExt1.treeNode.Text := 'After Edit';

  fraCfgCompiler := TfraCfgCompiler6502.Create(frmConfig);
  fraCfgCompiler.Parent := pCompiler.scrollBox;
  fraCfgCompiler.Left := 0;
  fraCfgCompiler.Top := 0;
  pCompiler.treeNode.Text := 'Compiler';

  fraCfgAsmOut := TfraCfgAsmOut6502.Create(frmConfig);
  fraCfgAsmOut.Parent := pCompExt1.scrollBox;
  fraCfgAsmOut.Left := 0;
  fraCfgAsmOut.Top := 0;
  pCompExt1.treeNode.Text := 'Assembler';
end;
procedure TAdapter6502.ConfigInit(cfgFile: TMiConfigXML);
{Se pide inicializar los frames de configuración}
begin
  //Configuración de todos los formularios de configuración creados y registrados.
  fraCfgAfterChg.Init('AftChg6502', cfgFile);
  fraCfgCompiler.Init('Compiler6502', cfgFile);
  fraCfgAsmOut.Init('AsmOutput6502', cfgFile);
  //Aprovechamos para inicializar "patUnits" ahora que ya tenemos creado "fraCfgCompiler".
  //CodeTool.patUnits := fraCfgCompiler.unitPath;
  CodeTool.fraCfgCompiler := fraCfgCompiler;
end;
procedure TAdapter6502.ConfigActivate;
{Se pide activar los frames creados en las páginas elegidas del formualario de
configuración.}
begin
  //Partimos de que todas las páginas extras se han ocultado
  //Mostramos las páginas que vamos a usar
  pEdiExt1 .treeNode.visible := true; //Muestra el acceso
  pCompiler.treeNode.visible := true;
  pCompExt1.treeNode.visible := true;
  //Mostramos también los frames que corresponden a este adaptador
  fraCfgAfterChg.Visible := true;
  fraCfgCompiler.Visible := true;
  fraCfgAsmOut.Visible := true;
end;
procedure TAdapter6502.setMenusAndToolbar(menu1, menu2, menu3: TMenuItem;
  toolbar: TToolBar; popupEdit: TPopupMenu; popupEditCount: integer);
begin
  adapterForm.setMenusAndToolbar(menu1, menu2, menu3, toolbar, popupEdit, popupEditCount);

  //Carga lista de ejemplos
//  Hay := FindFirst(patSamples + DirectorySeparator + '*.pas', faAnyFile - faDirectory, SR) = 0;
//  while Hay do begin
//     //encontró archivo
//    AddItemToMenu(mnSamples, '&'+ChangeFileExt(SR.name,''),@DoSelectSample);
//    Hay := FindNext(SR) = 0;
//  end;

end;
//procedure TfrmPrincipal.DoSelectSample(Sender: TObject);
////Se ha seleccionado un archivo de ejemplo.
//var
//  SamFil: String;
//  it: TMenuItem;
//begin
//  it := TMenuItem(Sender);
//  SamFil := patSamples + DirectorySeparator + it.Caption + '.pas';
//  SamFil := StringReplace(SamFil,'&','',[rfReplaceAll]);
//  //Carga archivo
//  fraEditView1.LoadFile(SamFil);
//end;

constructor TAdapter6502.Create(fraEdit0: TfraEditView; panRightPanel0: TPanel);
begin
  inherited Create;
  fraEditView1 := fraEdit0;
  Compiler:= TCompiler_PIC16.Create;
  //COnfigura eventos
  Compiler.OnRequireFileString:=@Compiler_RequireFileString;
  Compiler.OnError           := @CompilerError;
  Compiler.OnWarning         := @CompilerWarning;
  Compiler.OnInfo            := @CompilerInfo;
  Compiler.OnMessageBox      := @CompilerMessageBox;
  //Configura CodeTool
  CodeTool  := TCodeTool.Create(fraEdit0);
  CodeTool.Init(compiler);  //Asigna compilador
  //Crea frame de estadísticas
  fraStatis  := TfraStatist6502.Create(nil);
  //Crea frame del árbol de sintaxis
  fraSynTree := TfraSynxTree6502.Create(nil);
  fraSynTree.Init(Compiler);    //Conecta al compilador
  //Crea formulario principal
  adapterForm:= TfrmAdapter6502.Create(nil);
  adapterForm.adapter := self;  //Actualiza referencia
  //Crea formulario de depuración
  frmDebug    := TfrmDebugger6502.Create(nil);
  //Crea formulario explorador de RAM
  frmRAMExplorer:= TfrmRAMExplorer6502.Create(nil);
  //Guarda referencia a editor lateral.
  panRightPanel := panRightPanel0;
  //Crea editor y resaltador
  edAsm := TSynEdit.Create(panRightPanel);
  hlAssem := TSynFacilSyn.Create(edAsm);
  LoadAsmSyntaxEd;
end;
destructor TAdapter6502.Destroy;
begin
  hlAssem.Free;
  frmRAMExplorer.Destroy;
  frmDebug.Destroy;
  adapterForm.Destroy;
  fraSynTree.Destroy;
  fraStatis.Destroy;
  CodeTool.Destroy;
  Compiler.Destroy;
  inherited Destroy;
end;

end.

