unit FrameCfgAfterChg6502;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, MiConfigXML;

type

  { TfraCfgAfterChg6502 }

  TfraCfgAfterChg6502 = class(TFrame)
    grpAfterEdit_6502: TRadioGroup;
  private

  public
    actAfterChg: integer;
    procedure Init(section: string; cfgFile: TMiConfigXML);
  end;

implementation

{$R *.lfm}

{ TfraCfgAfterChg6502 }

procedure TfraCfgAfterChg6502.Init(section: string; cfgFile: TMiConfigXML);
begin
    cfgFile.Asoc_Int(section+ '/actAfterChg', @actAfterChg, grpAfterEdit_6502, 1);
end;

end.

