{Adapter for supporting the P65Pas compiler in the IDE.
This is a mandatory file and the entry point for suppporting an controling a compiler
from the IDE.
All compilers supported must have an adapter and a unit like this.}
unit adapter6502;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, ComCtrls, adapter, CodeTools6502, Compiler_PIC16, LexPas,
  FrameEditView, FrameStatist6502, FrameSynTree6502;
type
  { TAdapter6502 }
  TAdapter6502 = class(TAdapter)
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
    //Otros
    Compiling   : Boolean;  //Bandera. Indica que se ha pedido ejceutar una compilación.
    procedure Compiler16_RequireFileString(FilePath: string;
      var strList: TStrings);
    procedure CompilerAfterCompile;
    procedure CompilerError(errTxt: string; const srcPos: TSrcPos);
    procedure CompilerInfo(infTxt: string; const srcPos: TSrcPos);
    procedure CompilerWarning(warTxt: string; const srcPos: TSrcPos);
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
    procedure Exec(srcFile, outFile: string; pars: string); override;
    procedure CheckSyntax(srcFile: string; pars: string); override;
    procedure UpdateCompletionForEditors; override;
    procedure DumpCode(lins: TSTrings); override;
  public      //Inicialización
    procedure Init(pagControl: TPageControl);
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
procedure TAdapter6502.CompilerAfterCompile;
var
  usedRAM, usedROM, usedSTK: single;
begin
  if OnAfterCompile<>nil then OnAfterCompile();
  fraSynTree.Refresh;    //Actualiza el árbol de sintaxis
  //Actualiza frame de estadísticas de uso
  if nErrors=0 then begin
    //No hay error
    compiler.GetResourcesUsed(usedRAM, usedROM, usedSTK);
    fraStatis.Update(usedRAM, usedROM, usedSTK);
  end else begin
    //Hubo errores
    fraStatis.Update(0, 0, 0);
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
procedure TAdapter6502.CompilerInfo(infTxt: string; const srcPos: TSrcPos);
var
  fName: String;
begin
  fName := compiler.ctxFile(srcPos);
  if OnInfo<>nil then OnInfo(infTxt, fname, srcPos.row, srcPos.col);
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
procedure TAdapter6502.Exec(srcFile, outFile: string; pars: string);
{Ejecuta el compilador para generar un archivo binario de salida.}
begin
  nErrors := 0;
  Compiling := true;   //Activa bandera para saber que queremos compilar.
  Compiler.Exec(srcFile, outFile, pars);
  Compiling := false;
end;
procedure TAdapter6502.CheckSyntax(srcFile: string; pars: string);
{Ejecuta el compilador para realizar una verificación de sintaxis.}
begin
  Compiler.Exec(srcFile, '', pars);
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
procedure TAdapter6502.Init(pagControl: TPageControl);
{Inicializa el adaptador. Eso implica preparar la IDE para que soporte a este nuevo
compilador que se está registrando.}
var
  tab: TTabSheet;
begin
  //Agrega las herramientas de este compilador
  tab := pagControl.AddTabSheet;    //Agrega nuevo panel
  //tab.Name := 'SyntaxTree_'+ COMP_NAME;
  tab.Caption := 'Syntax Tree';
  tab.Hint := COMP_NAME;     //Lo marca aquí para saber que es de este compilador.
  fraSynTree.Parent := tab;
  fraSynTree.Visible := true;

end;
constructor TAdapter6502.Create(fraEdit0: TfraEditView);
begin
  inherited Create;
  fraEditView1 := fraEdit0;
  Compiler:= TCompiler_PIC16.Create;
  //COnfigura eventos
  Compiler.OnRequireFileString:= @Compiler16_RequireFileString;
  Compiler.OnAfterCompile  := @CompilerAfterCompile;
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
end;
destructor TAdapter6502.Destroy;
begin
  fraSynTree.Destroy;
  fraStatis.Destroy;
  CodeTool.Destroy;
  Compiler.Destroy;
  inherited Destroy;
end;

end.

