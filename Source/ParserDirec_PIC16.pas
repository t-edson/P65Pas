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
    procedure ProcCOMMODORE64;
  public
    Commodore64: boolean;
    procedure ClearMacros;
  end;

  procedure SetLanguage;

implementation
procedure SetLanguage;
begin
  ParserAsm_PIC16.SetLanguage;
end;

procedure TParserDirec.ProcCOMMODORE64;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  Commodore64 := true;  //Activa modo Commodore64
end;

{ TParserDirec }
procedure TParserDirec.ClearMacros;
begin
  inherited;
  //Agrega nuevas instrucciones
  //Agrega nuevas instrucciones
  AddInstruction('COMMODORE64', @ProcCOMMODORE64);
end;

end.
