program P65Pas;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, FormPrincipal, FrameSynTree6502, Globales, FormConfig,
  PicPasProject, FrameEditView, FrameMessagesWin, FormElemProperty,
  FrameCfgExtTool, FormDebugger6502, FormRAMExplorer6502, ParserDirec, Analyzer,
  LexPas, CompContexts, ParserASM_6502, XpresElemP65, FrameLateralPanel,
  adapter6502, FrameStatist6502, FormAdapter6502, FrameCfgAfterChg6502,
  FrameCfgCompiler6502, FrameCfgAsmOut6502;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.CreateForm(TConfig, Config);
//  Application.CreateForm(TfraCfgGeneral, fraCfgGeneral);
  Application.Run;
end.

