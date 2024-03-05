unit FormDebugger6502;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Grids, ActnList, Menus, LCLType, CompBase,
  P65c02utils, CPUCore, FrameRamExplorer6502, FrameRegisters6502,
  FrameRegWatcher6502, FrameAsm6502, MisUtils, Analyzer;
type
  { TfrmDebugger6502 }
  TfrmDebugger6502 = class(TForm)
    acGenReset: TAction;
    acGenStep: TAction;
    acGenStepIn: TAction;
    acGenSetPC: TAction;
    acGenExecHer: TAction;
    acGenRun: TAction;
    acGenPause: TAction;
    acGenSetBrkPnt: TAction;
    acGenClearCC: TAction;
    acGenAddWatch: TAction;
    ActionList1: TActionList;
    Image1: TImage;
    ImageList32: TImageList;
    ImageList16: TImageList;
    lstMessages: TListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Separator1: TMenuItem;
    MenuItem7: TMenuItem;
    PanASM: TPanel;
    PanWatcher: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    panRAM: TPanel;
    PopupAsmGrid: TPopupMenu;
    PopupMenu2: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter6: TSplitter;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    Timer2: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure acGenAddWatchExecute(Sender: TObject);
    procedure acGenClearCCExecute(Sender: TObject);
    procedure acGenExecHerExecute(Sender: TObject);
    procedure acGenSetBrkPntExecute(Sender: TObject);
    procedure acGenStepExecute(Sender: TObject);
    procedure acGenStepInExecute(Sender: TObject);
    procedure acGenPauseExecute(Sender: TObject);
    procedure acGenResetExecute(Sender: TObject);
    procedure acGenRunExecute(Sender: TObject);
    procedure acGenSetPCExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Panel3Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    defHeight: LongInt;
    margInstrc: Integer;
    fraRamExp: TfraRamExplorer6502;
    fraPicReg: TfraPicRegisters;
    fraRegWat: TfraRegWatcher;
    fraPicAsm: TfraPicAsm;
    milsecRefresh: integer;   //Periodo de refresco en milisegunod
    nCyclesPerClk: integer;   //Número de ciclos a ejecutar por pasada
    curVarName : string;
    cxp: TAnalyzer;
    pic: TCPUCore;
    procedure fraPicAsmCPUerror(txt: string);
    procedure picExecutionMsg(message: string);
    procedure RefreshScreen(SetGridRow: boolean = true);
  public
    procedure Exec(cxp0: TAnalyzer);
  end;

var
  frmDebugger6502: TfrmDebugger6502;

implementation
{$R *.lfm}
{ TfrmDebugger6502 }
procedure TfrmDebugger6502.Timer1Timer(Sender: TObject);
{temporizador para el editor de diagramas }
var
  stopped: boolean;
begin
  if pic = nil then exit;
//  consoleTickStart;
  pic.ExecNCycles(nCyclesPerClk, stopped);
  if stopped then begin
    acGenPauseExecute(self);
  end;
//  consoleTickCount('');
end;
procedure TfrmDebugger6502.Timer2Timer(Sender: TObject);
{Temporizador para los otros frames menos fraPicDia.}
begin
  fraPicReg.Refrescar;
  if fraRamExp.Visible then fraRamExp.panGraph.Invalidate;
  if fraRegWat.Visible then fraRegWat.Refrescar;
//  if fraPicDia.Visible then fraPicDia.Refrescar;
  if fraPicAsm.Visible then fraPicAsm.Refrescar(true);
  StatusBar1.Panels[1].Text := 'Clock Cycles = ' + IntToStr(pic.nClck);
  StatusBar1.Panels[2].Text := 'Time  = ' +
            FormatDateTime('hh:mm:ss.zzz', pic.nClck / pic.frequen / 86400 );
end;
procedure TfrmDebugger6502.RefreshScreen(SetGridRow: boolean = true);
{Refresca los paneles de la pantalla, con información actual del PIC}
begin
  fraPicReg.Refrescar;
  if fraRamExp.Visible then fraRamExp.panGraph.Invalidate;
  if fraRegWat.Visible then fraRegWat.Refrescar;
  if fraPicAsm.Visible then fraPicAsm.Refrescar(SetGridRow);
  StatusBar1.Panels[1].Text := 'Clock Cycles = ' + IntToStr(pic.nClck);
  StatusBar1.Panels[2].Text := 'Time  = ' +
            FormatDateTime('hh:mm:ss.zzz', pic.nClck / pic.frequen / 86400 );
end;
procedure TfrmDebugger6502.picExecutionMsg(message: string);
var
  i: Integer;
begin
  lstMessages.AddItem(message, nil);
  if lstMessages.Count>100 then begin
    //Limita la cantidad de mensajes
    lstMessages.Items.BeginUpdate;
    for i:=1 to 10 do begin
      lstMessages.Items.Delete(0);
    end;
    lstMessages.AddItem('Too many messages. STOP command sent.', nil);
    lstMessages.Items.EndUpdate;
    pic.CommStop := true;  //Manda comando para detener
  end;
end;
procedure TfrmDebugger6502.fraPicAsmCPUerror(txt: string);
{El panel de Ensamblador ha generado un error de CPU.}
begin
  lstMessages.AddItem(txt, nil);
end;
procedure TfrmDebugger6502.Exec(cxp0: TAnalyzer);
{Inicia el prcceso de depuración, mostrando la ventana.}
begin
  cxp := cxp0;
  pic := cxp0.picCore;

  //Muestra Frames
  fraRamExp.SetCompiler(cxp);
  fraPicReg.SetCompiler(cxp);
  fraRegWat.SetCompiler(cxp);
  fraPicAsm.SetCompiler(cxp);

//  pic.AddBreakpoint(0);
  pic.OnExecutionMsg := @picExecutionMsg;
  acGenResetExecute(self);
  StatusBar1.Panels[0].Text := pic.Model + ' at ' + IntToStr(pic.frequen) + ' Hz';
  ///// Calcula parámetros de refresco, para la ejecución en tiempo real //////////
  {La idea de la ejecución en tiempo real, es ejecutar un paquete de instrucciones
  (ciclos) por bloques y luego aprovechar el tiempo muerto que queda por haber ejecutado
  todas las instrucciones en menor tiempo.}
  milsecRefresh := 50;   //Fija un periodo de refresco inicial
  Timer1.Interval := milsecRefresh;
  Timer2.Interval := 250;  //Los controles adicionales se pueden refrescar despacio
  {Calcula cuántos ciclos debe ejecutar por refresco. Aún cuando el resultado de la
  fórmula sea exacto, la función ExecNCycles() usada para ejecutar un grupo de ciclos
  no siempre ejecuta los ciclos solicitados exactamente.}
  nCyclesPerClk := round(int64(pic.frequen) * milsecRefresh / 1000);
  /////////////////////////////////////////////////////////////////////////////////
  fraRegWat.acClearAllExecute(self);
  fraRegWat.acAddVarsExecute(self);  //agrega varaibles por defecto
  self.Show;
end;
procedure TfrmDebugger6502.FormCreate(Sender: TObject);
  procedure CreaMenuConAccion(itemMenu: TMenuItem; accion: TBasicAction; TheOwner: TComponent);
  {Agrega un nuevo ítem a un menú, a partir de una acción.}
  var
    mn: TMenuItem;
  begin
    mn :=  TMenuItem.Create(TheOwner);
    mn.Action := accion;
    itemMenu.Add(mn);
  end;
begin
  fraRamExp:= TfraRamExplorer6502.Create(self);
  fraRamExp.Parent := panRAM;
  fraRamExp.Align := alClient;

  fraPicReg:= TfraPicRegisters.Create(self);
  fraPicReg.Parent := Panel3;
  fraPicReg.Align := alClient;

  fraRegWat := TfraRegWatcher.Create(self);
  fraRegWat.Parent := PanWatcher;
  fraRegWat.Align := alClient;

  fraPicAsm:= TfraPicAsm.Create(self);
  fraPicAsm.Parent := PanASM;
  fraPicAsm.Align := alClient;
  fraPicAsm.OnCPUerror  := @fraPicAsmCPUerror;
  //Asigna menú contextual al Panel de ensamblador
  fraPicAsm.StringGrid1.PopupMenu := PopupAsmGrid;

  //Altura de fila de la grilla por defecto
  defHeight := 20;
  //Margen para mostrar las instrucciones en la grilla
  margInstrc := 32;
  //Configura Toolbar
//  ToolBar1.ButtonHeight:=38;
//  ToolBar1.ButtonWidth:=38;
//  ToolBar1.Height:=42;
//  ToolBar1.Images:=ImgActions32;
end;
procedure TfrmDebugger6502.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then begin
    MsgBox('Hola');
  end;
end;
procedure TfrmDebugger6502.Panel3Click(Sender: TObject);
begin

end;
procedure TfrmDebugger6502.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  acGenPauseExecute(self);
end;
////////////////////// Acciones ////////////////////
procedure TfrmDebugger6502.acGenSetPCExecute(Sender: TObject);
//Fija el puntero del programa en la instrucción seleccionada en el panel de ensamblador.
begin
  fraPicAsm.SetPC;
end;
procedure TfrmDebugger6502.acGenExecHerExecute(Sender: TObject);
{Ejecuta una instrucción hasta la dirección seleccionada.}
begin
  fraPicAsm.ExecHere;
  RefreshScreen;
end;
procedure TfrmDebugger6502.acGenSetBrkPntExecute(Sender: TObject);
{Pone o quita un Punto de Interrupción en la posición indicada}
var
  pc: word;
begin
  fraPicAsm.SetBrkPnt;
  RefreshScreen(false);  //¿Es necesario?
end;
procedure TfrmDebugger6502.acGenStepExecute(Sender: TObject);
{Ejecuta una instrucción sin entrar a subrutinas}
begin
  fraPicAsm.StepOver;  //Ejecutamos usando el panel de ensamblador.
  RefreshScreen;
end;
procedure TfrmDebugger6502.acGenStepInExecute(Sender: TObject);
{Ejecuta una instrucción, entrando al código de las subrutinas.}
begin
  fraPicAsm.StepIn;   //Ejecutamos usando el panel de ensamblador.
  RefreshScreen;
end;
procedure TfrmDebugger6502.acGenResetExecute(Sender: TObject);
var
  pc: Integer;
begin
  //pic.Reset;   //No hace reset para no perder el programa
  pic.Reset(false);

  Timer1.Enabled := false;
  Timer2.Enabled := false;
  acGenRun.Enabled := true;
  acGenPause.Enabled := false;
  lstMessages.Clear;
  lstMessages.AddItem('Restarting.', nil);
  //Start at the begin of code
  pc := cxp.GeneralORG;
//  while (pc < high(pic.ram)) and (pic.ram[pc].used <> ruCode) do begin
//    pc := pc + 1;  //Incrementa
//  end;
  while (pc < high(pic.ram)) and (pic.ram[pc].name <> '__main__') do begin
    pc := pc + 1;  //Incrementa
  end;
  if pc = high(pic.ram) then begin
    MsgExc('Executable code entry not found.');
  end;
  pic.WritePC(pc);
  RefreshScreen;
end;
procedure TfrmDebugger6502.acGenRunExecute(Sender: TObject);
{Ejecuta el programa, desde la posición actual}
var
  stopped: boolean;
begin
  pic.CommStop := false;   //Por si acaso
  //Ejecuta la primera instrucción para pasar en caso de que haya Puntos de Interrupción
  pic.Exec;
  {Hace una primera ejecución, porque la primera ejecución del Timer, va a demorar.}
  pic.ExecNCycles(nCyclesPerClk, stopped);
  if stopped then begin
    //Bastó una sola pasada, para llegar a algún obstáculo
    RefreshScreen;
    exit;
  end;
  //Programa la ejecución temporizada
  Timer1.Enabled := true;
  Timer2.Enabled := true;
  acGenRun.Enabled := false;
  acGenPause.Enabled := true;
  RefreshScreen;
  lstMessages.AddItem('Running program.', nil);
end;
procedure TfrmDebugger6502.acGenPauseExecute(Sender: TObject);
{Detiene el programa en el punto actual.}
begin
  Timer1.Enabled := false;
  Timer2.Enabled := false;
  acGenRun.Enabled := true;
  acGenPause.Enabled := false;
  RefreshScreen;
  lstMessages.AddItem('Execution Paused.', nil);
end;
procedure TfrmDebugger6502.acGenClearCCExecute(Sender: TObject);
{Reinica el contador de ciclos.}
begin
  pic.nClck := 0;
  RefreshScreen(false);
end;
procedure TfrmDebugger6502.acGenAddWatchExecute(Sender: TObject);
{Agrega un vigilante en la varible "curVarName"}
begin
  fraRegWat.AddWatch(curVarName);
end;

end.
//352
