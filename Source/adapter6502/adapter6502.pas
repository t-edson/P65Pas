{Adapter for supporting the P65Pas compiler in the IDE.
This is a mandatory file and the entry point for suppporting an controling a compiler
from the IDE.
All compilers supported must have an adapter and a unit like this.}
unit adapter6502;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, ComCtrls, Controls, ActnList, Menus, adapterBase,
  CodeTools6502, Compiler_PIC16, LexPas, FrameEditView, Globales, MisUtils,
  FrameStatist6502, FrameSynTree6502, FormAdapter6502, FrameCfgAfterChg6502,
  FrameCfgCompiler6502, FormDebugger6502, FormRAMExplorer6502;
type
  { TAdapter6502 }
  TAdapter6502 = class(TAdapterBase)
  public
    const COMP_NAME = 'P65Pas';
  private
    //Herramienta de completado y manejo de código
    CodeTool    : TCodeTool;
    //Compilador.
    Compiler    : TCompiler_PIC16;
    //Referencia al frame de edición
    fraEditView1: TfraEditView;
  private     //Herramientas adicionales
    fraStatis     : TfraStatist6502;  //Frame de estadísticas
    fraSynTree    : TfraSynxTree6502; //Frame de árbol de sintaxis
    adapterForm   : TfrmAdapter6502;   //Formulario principal
    frmDebug      : TfrmDebugger6502;
    frmRAMExplorer: TfrmRAMExplorer6502;
  private    //Otros
    Compiling   : Boolean;  //Bandera. Indica que se ha pedido ejceutar una compilación.
    procedure Compiler16_RequireFileString(FilePath: string;
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
    procedure UpdateCompletionForEditors; override;
    procedure DumpCode(lins: TSTrings); override;
  public      //Frames de configuración
    //Por practicidad, estos frames se deben instanciar en el formulario de configuración
    //de la IDE.
    fraCfgAfterChg: TfraCfgAfterChg6502;
    fraCfgCompiler: TfraCfgCompiler6502;
  public      //Inicialización
    procedure Init(pagControl: TPageControl; imgList16, imglist32: TImageList;
      actList: TActionList; mainMenu: TMainMenu;
      frmCfgAfterChg0: TfraCfgAfterChg6502; fraCfgCompiler0: TfraCfgCompiler6502);
    constructor Create(fraEdit0: TfraEditView);
    destructor Destroy; override;
  end;

implementation
{ TAdapter6502 }
procedure TAdapter6502.Compiler16_RequireFileString(FilePath: string;
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
    strList := ed.SynEdit.Lines;
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
     frmRAMExplorer.Exec(Compiler);
end;
procedure TAdapter6502.ListReport;
var
  edit: TSynEditor;
begin
//  fraEditView1.NewLstFile;
//  edit := fraEditView1.ActiveEditor;
//  edit.SynEdit.BeginUpdate;
//  Compiler.GenerateListReport(edit.SynEdit.Lines);
//  edit.SynEdit.EndUpdate;
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
  usedRAM, usedROM, usedSTK: single;
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
//  edAsm.BeginUpdate(false);
//  edAsm.Lines.Clear;
//  currComp.DumpCode(edAsm.Lines);
//  edAsm.EndUpdate;
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
  if fraCfgCompiler.ReuProcVar then AddLine(pars, '-Ov');   //Reusar variables de proced.
  if fraCfgCompiler.OptRetProc then AddLine(pars, '-Or');   //Optimizar Retorno de proced.
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
  if (ed.SynEdit.Lines.Count<=1) and (trim(ed.Text)='') then exit;
  //Lee configuración de verif. de sintaxis.
  case fraCfgAfterChg.actAfterChg of
    0: pars := '-Cn' + LineEnding + '-Dn';  //<No action>
    1: pars := '-Ca' + LineEnding + '-Dn';  //Do Only Analysis
    2: pars := '-Cao'+ LineEnding + '-Dn'; //Do Analysis and Optimization.
    3: pars := '-C'  + LineEnding + '-Dn';   //Do complete compilation.
  end;
  //Inicio de compilación
  nErrors := 0;
  if OnBeforeCheckSyn<>nil then OnBeforeCheckSyn();
  Compiler.Exec(ed.FileName, '', pars);
  if OnAfterCheckSyn<>nil then OnAfterCheckSyn();
  UpdateTools;
end;
procedure TAdapter6502.UpdateCompletionForEditors;
begin
  fraEditView1.UpdateSynEditCompletion;
end;
procedure TAdapter6502.DumpCode(lins: TSTrings);
begin
//  Compiler.DumpCode(lins,AsmMode, IncVarDec, ExcUnused, incAdrr, incCom, incVarNam);
end;
//Inicialización
procedure TAdapter6502.Init(pagControl: TPageControl; imgList16,
  imglist32: TImageList; actList: TActionList; mainMenu: TMainMenu;
  frmCfgAfterChg0: TfraCfgAfterChg6502; fraCfgCompiler0: TfraCfgCompiler6502);
{Inicializa el adaptador. Eso implica preparar la IDE para que soporte a este nuevo
compilador que se está registrando.
Solo se debe ejecutar esta rutina una ve al inicio.
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
  //Agrega los íconos de "adapterForm" a los ImageList
  adapterForm.AddActions(imgList16, imgList32, actList, COMP_NAME, mainMenu);
  //Agrega las herramientas de este compilador
  tab := pagControl.AddTabSheet;    //Agrega nuevo panel
  tab.Name := 'SyntaxTree_'+ COMP_NAME;
  tab.Caption := 'Syntax Tree';
  tab.Hint := COMP_NAME;     //Lo marca aquí para saber que es de este compilador.
  fraSynTree.Parent := tab;
  fraSynTree.Visible := true;
  fraSynTree.Align := alClient;
  fraSynTree.OnLocateElemen  := @fraSynTreeLocateElemen;
  //Asigna referencias a formularios de configuración
  fraCfgAfterChg := frmCfgAfterChg0;
  fraCfgCompiler := fraCfgCompiler0;
end;
constructor TAdapter6502.Create(fraEdit0: TfraEditView);
begin
  inherited Create;
  fraEditView1 := fraEdit0;
  Compiler:= TCompiler_PIC16.Create;
  //COnfigura eventos
  Compiler.OnRequireFileString:= @Compiler16_RequireFileString;
  Compiler.OnError         := @CompilerError;
  Compiler.OnWarning       := @CompilerWarning;
  Compiler.OnInfo          := @CompilerInfo;
  //Configura CodeTool
  CodeTool  := TCodeTool.Create(fraEdit0);
  CodeTool.SetCompiler(compiler);  //Asigna compilador
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
end;
destructor TAdapter6502.Destroy;
begin
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

