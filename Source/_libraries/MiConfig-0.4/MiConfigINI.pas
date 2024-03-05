{
MiConfigIni 0.1b
=============
Por Tito Hinostroza 29/07/2016

Descripción
===========
Unidad con rutinas de lectura/escritura de propiedades en archivos INI. Permite crear
fácilmente, una ventana de configuración, con las opciones: ACEPTAR y CANCELAR.
Está basado en la librería ConfigFrame, pero a diferencia de esta, aquí las propiedades
no se separan en "frames", sino que todas las propiedades se manejan en un mismo objeto.
Para alamacenar las propiedades, se debe crear un objeto TMiConfigINI. Sin embargo,
la unidad crea por defecto, una isntancia de TMiConfigINI, llamada "cfgFile", que toma
como nombre <nombre del proyecto>.ini
Tiene como dependencia a la librería MisUtils.

Por Tito Hinostroza 29/07/2016
}
unit MiConfigINI;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Graphics, Forms, IniFiles, MisUtils, MiConfigBasic;
type
  { TMiConfigINI }
  {Clase base que es usada para manejar los campos de configuración.}
  TMiConfigINI = class(TMiConfigBasic)
  private
    fileName    : string;   //archivo XML
    function DefaultFileName: string;
    procedure FileProperty(iniCfg: TIniFile; const r: TParElem; FileToProp: boolean);
  public
    secINI: string;   //sección donde se guardarán los datos en un archivo INI
    procedure VerifyFile;
    function FileToProperties: boolean; virtual;
    function PropertiesToFile: boolean; virtual;
  public  //Constructor y Destructor
    constructor Create(INIfile0: string);
    destructor Destroy; override;
  end;

var
  cfgFile : TMiConfigINI;   //Default INI Config file

implementation
//Funciones de uso interno
function CodeStr(s:string): string;
{Protege a una cadena para que no pierda los espacios laterales si es que los tiene,
porque el el archivo INI se pierden. Además codifica el caracter "=", porque es
reservado en el archvio INI}
begin
  Result := '.'+s+'.';
  Result := StringReplace(Result, '=', #25, [rfReplaceAll]);  //protege caracter
  Result := StringReplace(Result, LineEnding, #26, [rfReplaceAll]);  //protege caracter
end;
function DecodeStr(s:string): string;
{Quita la protección a una cadena que ha sido guardada en un archivo INI}
begin
  Result:=copy(s,2,length(s)-2);
  Result := StringReplace(Result, #25, '=', [rfReplaceAll]);  //protege caracter
  Result := StringReplace(Result, #26, LineEnding, [rfReplaceAll]);  //protege caracter
end;
{ TMiConfigINI }
function TMiConfigINI.DefaultFileName: string;
{Devuelve el nombre pro defecto del archvio de configuración}
begin
  Result := ChangeFileExt(Application.ExeName,'.ini');
end;
procedure TMiConfigINI.VerifyFile;
//Verifica si el archivo INI "FileName" existe. Si no, muestra un mensaje y lo crea.
var
  F: textfile;
begin
  if not FileExists(fileName) then begin
    MsgErr('No INI file found: %s', [fileName]);
    //crea uno vacío para leer las opciones por defecto
    AssignFile(F, fileName);
    Rewrite(F);
    CloseFile(F);
  end;
end;
procedure TMiConfigINI.FileProperty(iniCfg: TIniFile; const r: TParElem; FileToProp: boolean);
{Permite leer o escribir una propiedad en el archivo XML}
var
  n, j: Integer;
  list: TStringList;
  strlst: TStringList;
  c: TColor;
begin
  if r.pVar = nil then exit;   //se inició con NIL
  case r.tipPar of
  tp_Int, tp_Int_TEdit, tp_Int_TSpinEdit, tp_Int_TRadioGroup:
    if FileToProp then begin  //lee entero
      r.AsInteger := iniCfg.ReadInteger(secINI, r.etiqVar, r.defInt);
    end else begin
      iniCfg.WriteInteger(secINI, r.etiqVar, r.AsInteger);
    end;
  //---------------------------------------------------------------------
  tp_Dbl, tp_Dbl_TEdit, tp_Dbl_TFloatSpinEdit:
    if FileToProp then begin
      r.AsDouble := iniCfg.ReadFloat(secINI, r.etiqVar, r.defDbl);
    end else begin
      iniCfg.WriteFloat(secINI, r.etiqVar, r.AsDouble);
    end;
  //---------------------------------------------------------------------
  tp_Str, tp_Str_TEdit, tp_Str_TEditButton, tp_Str_TCmbBox:
    if FileToProp then begin  //lee cadena
      r.AsString := DecodeStr(iniCfg.ReadString(secINI, r.etiqVar, '.'+r.defStr+'.'));
    end else begin
      iniCfg.WriteString(secINI, r.etiqVar, CodeStr(r.AsString));
    end;
  //---------------------------------------------------------------------
  tp_Bol, tp_Bol_TCheckBox, tp_Bol_TRadBut:
    if FileToProp then begin  //lee booleano
      r.AsBoolean := iniCfg.ReadBool(secINI, r.etiqVar, r.defBol);
    end else begin
      iniCfg.WriteBool(secINI, r.etiqVar, r.AsBoolean);
    end;
  //---------------------------------------------------------------------
  tp_Enum, tp_Enum_TRadBut, tp_Enum_TRadGroup:
    if FileToProp then begin  //lee enumerado como entero
       if r.lVar = 4 then begin  //tamaño común de las variable enumeradas
         r.AsInt32 := iniCfg.ReadInteger(secINI, r.etiqVar, r.defInt);
       end else begin  //tamaño no implementado
         msjErr := dic('Enumerated type no handled.');
         exit;
       end;
    end else begin
      if r.lVar = 4 then begin
        iniCfg.WriteInteger(secINI, r.etiqVar, r.AsInt32);  //como entero de 4 bytes
      end else begin  //tamaño no implementado
        msjErr := dic('Enumerated type no handled.');
        exit;
      end;
    end;
  //---------------------------------------------------------------------
  tp_TCol_TColBut, tp_TCol_TColBox:
    if FileToProp then begin  //lee TColor
      r.AsTColor := iniCfg.ReadInteger(secINI, r.etiqVar, r.defCol);
    end else begin
      c := r.AsTColor;
      iniCfg.WriteInteger(secINI, r.etiqVar, c);
    end;
  tp_StrList, tp_StrList_TListBox:
    if FileToProp then  begin //lee TStringList
      list := TStringList(r.Pvar^);
      iniCfg.ReadSection(secINI+'_'+r.etiqVar, list);
      //decodifica cadena
      for n:=0 to list.Count-1 do list[n] := DecodeStr(list[n]);
    end else begin
      strlst := TStringList(r.Pvar^);
      iniCfg.EraseSection(secINI+'_'+r.etiqVar);
      for j:= 0 to strlst.Count-1 do begin
        iniCfg.WriteString(secINI+'_'+r.etiqVar,
                           CodeStr(strlst[j]),'');
      end;
    end;
  else  //no se ha implementado bien
    msjErr := dic('Design error.');
    exit;
  end;
end;
function TMiConfigINI.FileToProperties: boolean;
{Lee de disco las propiedades registradas
Si encuentra error devuelve FALSE, y el mensaje de error en "MsjErr", y el elemento
con error en "ctlErr".}
var
  r: TParElem;
  iniCfg: TIniFile;
begin
  if not FileExists(fileName) then begin
    ctlErr := nil;
    MsjErr := dic('INI file does not exist.');  //errro
    exit(false);  //para que no intente leer
  end;
  try
    iniCfg := TIniFile.Create(fileName);
  except
    ctlErr := nil;
    MsjErr := dic('Error reading INI file: %s', [fileName]);
    iniCfg.Free;
    exit(false);
  end;
  msjErr := '';
  for r in listParElem do begin
    FileProperty(iniCfg, r, true);
    if msjErr<>'' then begin
      ctlErr := r;  //elemento que produjo el error
      iniCfg.Free;  //libera
      exit(false);   //sale con error
    end;
    if r.OnFileToProperty<>nil then r.OnFileToProperty;
  end;
  //Terminó con éxito. Actualiza los cambios
  if OnPropertiesChanges<>nil then OnPropertiesChanges;
  ctlErr := nil;
  iniCfg.Free;  //libera
  exit(true);   //sale sin error
end;
function TMiConfigINI.PropertiesToFile: boolean;
{Guarda en disco las propiedades registradas
Si encuentra error devuelve FALSE, y el mensaje de error en "MsjErr", y el elemento
con error en "ctlErr".}
var
  r: TParElem;
  iniCfg: TIniFile; //
begin
  if FileExists(fileName) then begin  //ve si existe
     if FileIsReadOnly(fileName) then begin
       ctlErr := nil;
       MsjErr := dic('INI file is only read.');
       exit(false);
     end;
  end;
  try
    iniCfg := TIniFile.Create(fileName);
  except
    ctlErr := nil;
    MsjErr := dic('Error writing INI file: %s', [fileName]);
    exit(false);
  end;
  msjErr := '';
  for r in listParElem do begin
    if r.OnPropertyToFile<>nil then r.OnPropertyToFile;  //se ejecuta antes
    FileProperty(iniCfg, r, false);
    if msjErr<>'' then begin
      ctlErr := r;   //elemento que produjo el error
      iniCfg.Free;   //libera
      exit(false);   //sale con error
    end;
  end;
  ctlErr := nil;
  iniCfg.Free;    //libera
  exit(true);     //sin error
end;
//Constructor y Destructor
constructor TMiConfigINI.Create(INIfile0: string);
begin
  inherited Create;
  fileName := INIfile0;
  secINI := 'config';  //sección por defecto en archivo INI
end;
destructor TMiConfigINI.Destroy;
begin
  inherited Destroy;
end;

initialization
  cfgFile := TMiConfigINI.Create(cfgFile.DefaultFileName);

finalization
  cfgFile.Destroy;
end.

