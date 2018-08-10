{Unidad con definiciones báscias de un proyecto de PicPas.}
unit PicPasProject;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, MisUtils, SynEdit;
type
  TPICProjType = (
    pptNormalProj,   //Proyecto Normal
    pptSingleFile    //Proyecto de un solo archivo
  );
  { TPicPasProject }
  TPicPasProject = class
  private
    procedure SetLanguage(lang: string);
  public
    name: string;   //Nombre del proyecto
    picTarget: string;  //Modelo de PIC destino
    projFile: string;   //archivo del proyecto
    modified: boolean;
    projType: TPICProjType;
    procedure Open();
    function Close: boolean;
    procedure Save;
  public
    constructor Create;
  end;

implementation
var
  DEF_PROJ_NAME: string;
{ TPicPasProject }
procedure TPicPasProject.Open;
{Abre un nuevo proyecto}
begin
  modified := false;
end;
function TPicPasProject.Close: boolean;
{Cierra el proyecto. Si se cancela la acción devuevle FALSE.}
var
  Res: Byte;
begin
  //Verifica
  if modified then begin
    Res := MsgYesNoCancel('Current project, have been modified. ¿Save?');
    if Res = 3 then exit(false);
    if Res = 1 then begin
      Save;
    end;
  end;
  //Cierra el proyecto
  exit(true);
end;
procedure TPicPasProject.Save;
{Guarda el proyecto}
begin

end;

constructor TPicPasProject.Create;
begin
  //Nombre del proyecto
  name := DEF_PROJ_NAME;
  picTarget := '16F84A';
  projFile := name + '.ppr';
  modified := false;
end;

procedure TPicPasProject.SetLanguage(lang: string);
begin
  case lowerCase(lang) of
  'en': begin
      DEF_PROJ_NAME := 'Project1';
      dicClear;  //ya está en inglés
    end;
  'es': begin
      DEF_PROJ_NAME := 'Proyecto1';
      dicSet('Current project, have been modified. ¿Save?','El projecto ha sido modificado. ¿Guardar?');
    end;
  'qu': begin
      DEF_PROJ_NAME := 'Proyecto1';
      dicSet('Current project, have been modified. ¿Save?','El projecto ha sido modificado. ¿Guardar?');
    end;
  end;
end;

end.

