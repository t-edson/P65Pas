{Unidad que implementa a la clase TParserDirec, que sirve como contenedor para
implementar las funcionaliddes adicionales del procesamiento de directivas.
}
unit ParserDirec_PIC16;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, ParserDirec, ParserAsm_PIC16;

type

  { TParserDirec }
  TParserDirec = class(TParserAsm)
  private
  public
    procedure ClearMacros;
  end;

  procedure SetLanguage;

implementation
procedure SetLanguage;
begin
  ParserAsm_PIC16.SetLanguage;
end;
{ TParserDirec }
procedure TParserDirec.ClearMacros;
begin
  inherited;
  //Agrega nuevas instrucciones
end;

end.
