unit FramePicRegisters;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, StdCtrls, LCLProc,
  LCLIntf, LCLType, Grids, ExtCtrls, CompBase,
  CPUCore, P6502utils;
type

  { TfraPicRegisters }

  TfraPicRegisters = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
  private
    cxp: TCompilerBase;
    //Acceso a los registros importantes
    WREGptr: ^byte;
    STATptr: ^byte;
    procedure ShowRegister(reg: byte; gri: TStringGrid);
  public
    procedure SetCompiler(cxp0: TCompilerBase);
    procedure Refrescar;
    //procedure SetRegisters();
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
{ TfraPicRegisters }
procedure TfraPicRegisters.ShowRegister(reg: byte; gri: TStringGrid);
{Muestra el contenido de un registro en la fila indciada de la grilla.}
begin
  gri.BeginUpdate;
  gri.Cells[0,1] := IntToStr(reg);
  gri.Cells[1,1] := IntToHex(reg, 2);
  if (reg and %00000001)<>0 then gri.Cells[9, 1] := '1' else gri.Cells[9, 1] := '0';
  if (reg and %00000010)<>0 then gri.Cells[8, 1] := '1' else gri.Cells[8, 1] := '0';
  if (reg and %00000100)<>0 then gri.Cells[7, 1] := '1' else gri.Cells[7, 1] := '0';
  if (reg and %00001000)<>0 then gri.Cells[6, 1] := '1' else gri.Cells[6, 1] := '0';
  if (reg and %00010000)<>0 then gri.Cells[5, 1] := '1' else gri.Cells[5, 1] := '0';
  if (reg and %00100000)<>0 then gri.Cells[4, 1] := '1' else gri.Cells[4, 1] := '0';
  if (reg and %01000000)<>0 then gri.Cells[3, 1] := '1' else gri.Cells[3, 1] := '0';
  if (reg and %10000000)<>0 then gri.Cells[2, 1] := '1' else gri.Cells[2, 1] := '0';
  gri.EndUpdate;
end;
procedure TfraPicRegisters.SetCompiler(cxp0: TCompilerBase);
{Fija el compilador actual.}
var
  pic : TCPUCore;
begin
  cxp := cxp0;
  pic := cxp0.picCore;
  //Configura registros de acuerdo al tipo de arquitectura del compilador
  WREGptr := nil;
  STATptr := nil;
  //Obtiene referencias a los registros importantes
  WREGptr := @(TP6502(pic).W);
//  StringGrid2.Cells[2,0] := 'IRP';
//  StringGrid2.Cells[3,0] := 'RP1';
//  StringGrid2.Cells[4,0] := 'RP0';
  STATptr := @(TP6502(pic).SR);
end;
procedure TfraPicRegisters.Refrescar;
{Refresca valores de los registros}
begin
  ShowRegister(WREGptr^, StringGrid1);
  ShowRegister(STATptr^, StringGrid2);
end;
constructor TfraPicRegisters.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;
destructor TfraPicRegisters.Destroy;
begin
  inherited Destroy;
end;

end.

