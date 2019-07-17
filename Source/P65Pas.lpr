program P65Pas;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, FormPrincipal, FrameSyntaxTree, XpresElementsPIC,
  Globales, FormConfig, PicPasProject, FrameEditView, FrameMessagesWin,
  FormElemProperty, ParserAsm_PIC16, FrameCfgExtTool, FormDebugger,
  FormRAMExplorer, ParserDirec, CompMain;

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

