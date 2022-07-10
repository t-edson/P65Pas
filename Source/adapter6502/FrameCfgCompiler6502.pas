unit FrameCfgCompiler6502;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, MiConfigXML;
type
  //Niveles de optimización
  TOptimLev = (olvFool,   //Nivel básico de optimización
               olvSmart   //Nivel mayor de optimización
               );

  { TfraCfgCompiler6502 }
  TfraCfgCompiler6502 = class(TFrame)
    chkOptBnkAftIF: TCheckBox;
    chkOptBnkAftPro: TCheckBox;
    chkOptBnkBefPro: TCheckBox;
    chkOptRetProc: TCheckBox;
    chkReuProcVar: TCheckBox;
    GroupBox1: TGroupBox;
    grpOptimLev: TRadioGroup;
  private

  public
    OptimLev    : TOptimLev;
    OptBnkAftIF : boolean;
    OptBnkBefPro: boolean;
    OptBnkAftPro: boolean;
    ReuProcVar  : boolean;
    OptRetProc  : boolean;
    procedure Init(section: string; cfgFile: TMiConfigXML);
  end;

implementation
{$R *.lfm}

procedure TfraCfgCompiler6502.Init(section: string; cfgFile: TMiConfigXML);
begin
  cfgFile.Asoc_Enum(section+ '/OptimLev'    , @OptimLev, Sizeof(TOptimLev), grpOptimLev, 1);
  cfgFile.Asoc_Bol (section+ '/OptBnkAftIF' , @OptBnkAftIF , chkOptBnkAftIF , true);
  cfgFile.Asoc_Bol (section+ '/OptBnkBefPro', @OptBnkBefPro, chkOptBnkBefPro, true);
  cfgFile.Asoc_Bol (section+ '/OptBnkAftPro', @OptBnkAftPro, chkOptBnkAftPro, true);
  cfgFile.Asoc_Bol (section+ '/ReuProcVar'  , @ReuProcVar, chkReuProcVar, false);
  cfgFile.Asoc_Bol (section+ '/OptRetProc'  , @OptRetProc, chkOptRetProc, true);
end;

end.

