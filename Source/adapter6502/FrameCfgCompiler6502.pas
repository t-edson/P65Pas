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
    chkOptRetProc: TCheckBox;
    chkRemUnOpcod: TCheckBox;
    chkReuProcVar: TCheckBox;
    chkForToRepeat: TCheckBox;
    txtUnitpath: TEdit;
    grpOptimLev: TRadioGroup;
    Label1: TLabel;
  private

  public
    unitPath    : string;
    ForToRepeat : boolean;
    OptimLev    : TOptimLev;
    ReuProcVar  : boolean;
    OptRetProc  : boolean;
    RemUnOpcod  : boolean;
    function unitPathExpanded: string;
    procedure Init(section: string; cfgFile: TMiConfigXML);
  end;

implementation
{$R *.lfm}

function TfraCfgCompiler6502.unitPathExpanded: string;
{Devuelve la propiedad "unitPath" expandiendo las posibles variables que puede
contener.}
var
  apppath, tmp: string;
begin
  apppath := ExtractFileDir(Application.ExeName);
  if DirectorySeparator = '\' then begin  //Estamos en Windows.
    tmp := StringReplace(unitPath, '/', DirectorySeparator, [rfReplaceAll, rfIgnoreCase]);
  end else begin
    tmp := unitPath;
  end;
  tmp := StringReplace(tmp, '{AppPath}', apppath, [rfReplaceAll, rfIgnoreCase]);
  exit(tmp);
end;

procedure TfraCfgCompiler6502.Init(section: string; cfgFile: TMiConfigXML);
begin
  cfgFile.Asoc_Str (section+ '/unitPath'    , @unitPath    , txtUnitpath, '{AppPath}/comp_p65pas/units/');
  cfgFile.Asoc_Bol (section+ '/ForToRepeat' , @ForToRepeat , chkForToRepeat  , true);
  cfgFile.Asoc_Enum(section+ '/OptimLev'    , @OptimLev    , Sizeof(TOptimLev),grpOptimLev, 1);
  cfgFile.Asoc_Bol (section+ '/ReuProcVar'  , @ReuProcVar  , chkReuProcVar   , false);
  cfgFile.Asoc_Bol (section+ '/OptRetProc'  , @OptRetProc  , chkOptRetProc   , true);
  cfgFile.Asoc_Bol (section+ '/RemUnOpcod'  , @RemUnOpcod  , chkRemUnOpcod   , true);

end;


end.

