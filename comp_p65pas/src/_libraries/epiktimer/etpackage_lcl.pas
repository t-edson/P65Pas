unit etpackage_lcl;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources;
  
procedure Register;


implementation

uses
  LazarusPackageIntf, EpikTimer;


procedure Register;
begin
  RegisterComponents('System', [TEpikTimer]);
end;

initialization
  {$I epiktimer.lrs}

end.

