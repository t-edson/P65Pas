{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit etpackage_dsgn; 

interface

uses
  etpackage_lcl, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('etpackage_lcl', @etpackage_lcl.Register); 
end; 

initialization
  RegisterPackage('etpackage_dsgn', @Register); 
end.
