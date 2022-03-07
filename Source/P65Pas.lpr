program P65Pas;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, etpackage, lazcontrols, FormPrincipal, FrameSyntaxTree, Globales,
  FormConfig, PicPasProject, FrameEditView, FrameMessagesWin, FormElemProperty,
  FrameCfgExtTool, FormDebugger, FormRAMExplorer, ParserDirec, Analyzer, LexPas,
  CompContexts, ParserASM_6502, XpresElemP65;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.CreateForm(TConfig, Config);
  Application.CreateForm(TfrmElemProperty, frmElemProperty);
  Application.CreateForm(TfrmDebugger, frmDebugger);
  Application.CreateForm(TfrmRAMExplorer, frmRAMExplorer);
//  Application.CreateForm(TfraCfgGeneral, fraCfgGeneral);
  Application.Run;
end.

