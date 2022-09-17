unit FrameCfgAsmOut6502;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, MiConfigXML;
type
  { TfraCfgAsmOut6502 }
  TfraCfgAsmOut6502 = class(TFrame)
    chkExcUnused: TCheckBox;
    chkIncAddress: TCheckBox;
    chkIncComment: TCheckBox;
    chkIncDecVar: TCheckBox;
    chkIncVarName: TCheckBox;
    RadioGroup2: TRadioGroup;
    procedure chkIncDecVarChange(Sender: TObject);
  private

  public
    asmOutType : integer;  //Tipo de Salida
    IncVarDec  : boolean;  //Incluye declaración de variables
    IncAddress : boolean;  //Incluye dirección física en el código desensamblado
    IncComment : boolean;  //Incluye comentarios en el código desensamblado
    ExcUnused  : boolean;  //Excluye declaración de variables no usadas
    IncVarName : boolean;  //Reemplaza dirección con etiqueta de variables
    procedure Init(section: string; cfgFile: TMiConfigXML);
  end;

implementation

{$R *.lfm}

{ TfraCfgAsmOut6502 }

procedure TfraCfgAsmOut6502.chkIncDecVarChange(Sender: TObject);
begin
  RadioGroup2.Enabled := chkIncDecVar.Checked;
  chkExcUnused.Enabled := chkIncDecVar.Checked;
end;

procedure TfraCfgAsmOut6502.Init(section: string; cfgFile: TMiConfigXML);
begin
  cfgFile.Asoc_Bol (section+'/IncDecVar' , @IncVarDec  , chkIncDecVar  , true);
  cfgFile.Asoc_Int (section+'/OutAsmType', @asmOutType , RadioGroup2, 0);
  cfgFile.Asoc_Bol (section+'/IncAddress', @IncAddress , chkIncAddress , true);
  cfgFile.Asoc_Bol (section+'/IncComment', @IncComment , chkIncComment , false);
//  cfgFile.Asoc_Bol (section+'/IncComment2',@IncComment2, chkIncComment2, false);
  cfgFile.Asoc_Bol (section+'/ExcUnused' , @ExcUnused  , chkExcUnused  , true);
  cfgFile.Asoc_Bol (section+'/IncVarName', @IncVarName , chkIncVarName , true);
  chkIncDecVarChange(self);   //Para actualizar
end;

end.

