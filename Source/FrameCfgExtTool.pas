unit FrameCfgExtTool;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, LazUTF8, Forms, Controls, StdCtrls, LCLProc,
  Graphics, MisUtils, Types, LCLIntf, Dialogs, Buttons, EditBtn, Globales,
  SynFacilBasic, MiConfigXML, process;
type
  //Representa una herramienta exterrna

  { TExternTool }
  TExternTool = object
  public
    name   : string;  //Nombre de la herramienta
    path   : string;  //Ruta del ejecutable
    ComLine: string;  //Línea de comandos
    WaitOnExit: boolean;  //Esperar hasta que termine
    ShowInTbar: boolean;  //Mostrar en barra de herramientas
    procedure ReadFromString(const str: string);
    function ToString: string;
  end;

  { TfraCfgExtTool }
  TfraCfgExtTool = class(TFrame)
    butAdd: TBitBtn;
    butRemove: TBitBtn;
    butTest: TButton;
    chkWaitExit: TCheckBox;
    chkShowTBar: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    txtName: TEdit;
    txtComLine: TEdit;
    txtPath: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    ListBox1: TListBox;
    procedure butAddClick(Sender: TObject);
    procedure butRemoveClick(Sender: TObject);
    procedure butTestClick(Sender: TObject);
    procedure chkShowTBarChange(Sender: TObject);
    procedure chkWaitExitChange(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure txtComLineChange(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
    procedure txtPathChange(Sender: TObject);
  private
    curTool: TExternTool;  //Atributo actual
    NoEvents: boolean;   //bandera
    procedure ControlsToListBox;
    procedure EstadoCampos(estado: boolean);
    procedure fraCfgExtToolPaint(Sender: TObject);
    procedure ListBoxToControls;
  public
    ExternTools: TStringList;  //Lista de archivos recientes
    OnReplaceParams: procedure(var comLine: string) of object;
    procedure ExecTool(idx: integer);
    procedure Execute(const tool: TExternTool);
  public  //Inicialización
    procedure Init(section: string; cfgFile: TMiConfigXML);
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
resourcestring
  ER_FAIL_EXEC_ = 'Fail executing: %s';
  PRE_TOOL_NAME = 'Tool'        ;

{ TExternTool }
procedure TExternTool.ReadFromString(const str: string);
var
  a: TStringDynArray;
begin
  a := Explode(#9, str);
  name := a[0];
  path := a[2];
  ComLine := a[3];
  WaitOnExit := f2B(a[4]);
  ShowInTbar := f2B(a[5]);
end;
function TExternTool.ToString: string;
begin
  //La cadena a almacenar en la lista, incluye todos los campos, pero se formatea
  //para que solo aparezca el nombre.
  Result := name + #9 +
            space(24) + #9 +  //Con esto se ocultan los otros campos
            path + #9 +
            ComLine + #9 +
            B2f(WaitOnExit) + #9 +
            B2f(ShowInTbar) + #9 +
            '' + #9 +   //Campo de ampliación
            '' + #9 +   //Campo de ampliación
            '' + #9 +   //Campo de ampliación
            '' + #9 +   //Campo de ampliación
            '';         //Campo de ampliación
end;
{TfraCfgExtTool}
procedure TfraCfgExtTool.Execute(const tool: TExternTool);
{Ejecutar la herramienta externa indicada. }
var
  p: TProcess;
  ComLine, Path: string;
begin
  p := TProcess.Create(nil); //Crea proceso
  if tool.WaitOnExit then p.Options:= p.Options + [poWaitOnExit];
  Path := tool.path;
  ComLine := tool.ComLine;
  if OnReplaceParams <> nil then begin
    OnReplaceParams(Path);  //Reemplaza parámetros
    OnReplaceParams(ComLine);  //Reemplaza parámetros
  end;
  p.CommandLine := Path + ' ' + ComLine;
//  p.Executable:=path;
//  p.Parameters.Clear;
//  p.Parameters.Add(ComLine);
  try
    p.Execute;
  except
    MsgBox(ER_FAIL_EXEC_, [p.Executable]);
  end;
  p.Free;
end;
procedure TfraCfgExtTool.ExecTool(idx: integer);
{Ejecuta la herramienta, de índice "idx"}
var
  cad: String;
  tool: TExternTool;
begin
  if (idx<0) or (idx>ExternTools.Count-1) then exit;
  cad := ExternTools[idx];
  tool.ReadFromString(cad);
  Execute(tool);
end;
procedure TfraCfgExtTool.butTestClick(Sender: TObject);
{Prueba la herramienta externa, que se tiene actualmente en ListBox1.}
var
  cad: String;
  tool: TExternTool;
begin
  if ListBox1.ItemIndex = -1 then exit;
  cad := ListBox1.Items[ListBox1.ItemIndex];
  tool.ReadFromString(cad);
  Execute(tool);
end;
procedure TfraCfgExtTool.butAddClick(Sender: TObject);
var
  r: TExternTool;
begin
  r.name := PRE_TOOL_NAME + IntToStr(ListBox1.Count+1);
  r.path := patApp;
  r.ComLine := '';
  r.ShowInTbar := false;
  r.WaitOnExit := false;
  ListBox1.AddItem(r.ToString, nil);
end;
procedure TfraCfgExtTool.butRemoveClick(Sender: TObject);
begin
  if ListBox1.ItemIndex=-1 then exit;
  ListBox1.Items.Delete(ListBox1.ItemIndex);
end;
procedure TfraCfgExtTool.EstadoCampos(estado: boolean);
begin
  label1.Enabled := estado;
  label2.Enabled := estado;
  label3.Enabled := estado;
  label5.Enabled := estado;
  txtName.Enabled := estado;
  txtPath.Enabled := estado;
  txtComLine.Enabled := estado;
  chkWaitExit.Enabled := estado;
  chkShowTBar.Enabled := estado;
  butTest.Enabled := estado;
end;
procedure TfraCfgExtTool.fraCfgExtToolPaint(Sender: TObject);
begin
  ListBox1Click(self);  //Inicia
end;
procedure TfraCfgExtTool.ListBoxToControls;
{Mueve el contendio de curTool, a los controles}
var
  cad: TCaption;
begin
  cad := ListBox1.Items[ListBox1.ItemIndex];
  curTool.ReadFromString(cad);
  NoEvents := true;  //protege
  txtName.Text        := curTool.name;
  txtPath.Text        := curTool.path;
  txtComLine.Text     := curTool.ComLine;
  chkWaitExit.Checked := curTool.WaitOnExit;
  chkShowTBar.Checked := curTool.ShowInTbar;
  debugln('ListBoxToControls');
  NoEvents := false;
end;
procedure TfraCfgExtTool.ControlsToListBox;
{Mueve el contenido de los controles a la lista ListBox1.}
begin
  if NoEvents then exit;  //Se deshabilitaron eventos
  if ListBox1.ItemIndex = -1 then begin
    exit;
  end;
  curTool.name       := txtName.Text;
  curTool.path       := txtPath.Text;
  curTool.ComLine    := txtComLine.Text;
  curTool.WaitOnExit := chkWaitExit.Checked;
  curTool.ShowInTbar := chkShowTBar.Checked;
  ListBox1.Items[ListBox1.ItemIndex] := curTool.ToString;
end;
procedure TfraCfgExtTool.ListBox1Click(Sender: TObject);
{Se selecciona una Herramienta de la lista de herramientas.}
begin
  if ListBox1.ItemIndex = -1 then begin
    txtName.Text := '';
    txtPath.Text := '';;
    txtComLine.Text := '';
    EstadoCampos(false);
    exit;
  end;
  ListBoxToControls;
  EstadoCampos(true);
end;
procedure TfraCfgExtTool.txtNameChange(Sender: TObject);
begin
  ControlsToListBox;
end;
procedure TfraCfgExtTool.txtPathChange(Sender: TObject);
begin
  ControlsToListBox;
end;
procedure TfraCfgExtTool.txtComLineChange(Sender: TObject);
begin
  ControlsToListBox;
end;
procedure TfraCfgExtTool.chkWaitExitChange(Sender: TObject);
begin
  ControlsToListBox;
end;
procedure TfraCfgExtTool.chkShowTBarChange(Sender: TObject);
begin
  ControlsToListBox;
end;
procedure TfraCfgExtTool.Init(section: string; cfgFile: TMiConfigXML);
begin
//  cfgFile.Asoc_StrList(section+ '/extern_tools', @ExternTools);
  cfgFile.Asoc_StrList_TListBox(section+ '/extern_tools', @ExternTools, ListBox1);
  self.OnPaint := @fraCfgExtToolPaint;
end;
constructor TfraCfgExtTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ExternTools := TStringList.Create;  //crea lista
  NoEvents := false;
end;
destructor TfraCfgExtTool.Destroy;
begin
  FreeAndNil(ExternTools);
  inherited Destroy;
end;

end.

