program P65Pas;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, FormPrincipal, FrameSynTree6502, Globales, FormConfig,
  PicPasProject, FrameEditView, FrameMessagesWin, FormElemProperty,
  FrameCfgExtTool, FormDebugger, FormRAMExplorer, ParserDirec, Analyzer, LexPas,
  CompContexts, ParserASM_6502, XpresElemP65, adapter6502,
  FrameCfgChkSyntax6502, FrameStatist6502, FormAdapter6502;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.CreateForm(TConfig, Config);
  Application.CreateForm(TfrmDebugger, frmDebugger);
  Application.CreateForm(TfrmRAMExplorer, frmRAMExplorer);
//  Application.CreateForm(TfraCfgGeneral, fraCfgGeneral);
  Application.Run;
end.

