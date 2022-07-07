{Adapter for supporting the P65Pas compiler in the IDE.
This is a mandatory file and the entry point for suppporting an controling a compiler
from the IDE.
All compilers supported must have an adapter and a unit like this.}
unit adapter6502;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, ComCtrls, Controls, ActnList, Menus, adapterBase,
  CodeTools6502, Compiler_PIC16, LexPas, FrameEditView, MisUtils,
  FrameStatist6502, FrameSynTree6502, FormAdapter6502;
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
    //Frame de estadísticas
    fraStatis   : TfraStatist6502;
    //Frame de árbol de sintaxis
    fraSynTree  : TfraSynxTree6502;
    //Formulario principal
    adapterForm: TfrmAdapter6502;
    //Otros
    Compiling   : Boolean;  //Bandera. Indica que se ha pedido ejceutar una compilación.
    procedure Compiler16_RequireFileString(FilePath: string;
      var strList: TStrings);
    procedure CompilerError(errTxt: string; const srcPos: TSrcPos);
    procedure CompilerInfo(infTxt: string; const srcPos: TSrcPos);
    procedure CompilerWarning(warTxt: string; const srcPos: TSrcPos);
    procedure fraSynTreeLocateElemen(fileSrc: string; row, col: integer);
    procedure UpdateTools;
  public      //Respuesta a acciones de "adapterForm"
    procedure acCompilExecute(Sender: TObject);
    procedure acCompExExecute(Sender: TObject);
    procedure acChecksynExecute(Sender: TObject);
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
  public      //Inicialización
    procedure Init(pagControl: TPageControl; imgList16, imglist32: TImageList;
      actList: TActionList; mainMenu: TMainMenu);
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
//Respuesta a acciones de "adapterForm"
procedure TAdapter6502.acCompilExecute(Sender: TObject);
{Ejecuta acción "Compilar".}
begin
  Compile; //Config.getParamsCompiling('6502'));
end;
procedure TAdapter6502.acCompExExecute(Sender: TObject);
begin

end;
procedure TAdapter6502.acChecksynExecute(Sender: TObject);
{Hace una verifiación de sintaxis del contenido actual}
begin
  CheckSyntax; //Config.getParamsAfterEdit('6502'));
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
  //Inicio de compilación
  nErrors := 0;
  if OnBeforeCompile<>nil then OnBeforeCompile();
  Compiling := true;   //Activa bandera para saber que queremos compilar.
  Compiler.Exec(ed.FileName, '', ''); //*** outFile y pars deben leerese de la configuración
  Compiling := false;
  if OnAfterCompile<>nil then OnAfterCompile();
  UpdateTools;
end;
procedure TAdapter6502.CheckSyntax;
{Ejecuta el compilador para realizar una verificación de sintaxis.}
var
  ed: TSynEditor;
begin
  //Validación de editor disponible
  if fraEditView1.ActiveEditor=nil then exit;
  ed := fraEditView1.ActiveEditor;
  if ed.FileName='' then exit;
  //Verifica rápidamente si hay texto en el editor
  if (ed.SynEdit.Lines.Count<=1) and (trim(ed.Text)='') then begin
    exit;
  end;
  //Inicio de compilación
  nErrors := 0;
  if OnBeforeCheckSyn<>nil then OnBeforeCheckSyn();
  Compiler.Exec(ed.FileName, '', '');
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
procedure TAdapter6502.Init(pagControl: TPageControl; imgList16, imglist32: TImageList;
  actList: TActionList; mainMenu: TMainMenu);
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

end;
destructor TAdapter6502.Destroy;
begin
  adapterForm.Destroy;
  fraSynTree.Destroy;
  fraStatis.Destroy;
  CodeTool.Destroy;
  Compiler.Destroy;
  inherited Destroy;
end;

end.

