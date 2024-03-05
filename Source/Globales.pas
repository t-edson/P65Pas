{
Unidad con declaraciones globales del proyecto
}
unit Globales;
{$mode objfpc}{$H+}
interface
uses  Classes, SysUtils, Forms, SynEdit, SynEditKeyCmds, MisUtils,
      lclType, FileUtil, LazLogger, Menus, EpikTimer;

const
  NOM_PROG = 'P65Pas';   //nombre de programa
  {$I ../version.txt}   //versión del programa

var
   //Variables globales
//   MsjError    : String;    //Bandera - Mensaje de error
   //Rutas sin "/" final
   patApp     : string;     //Ruta de la aplicación
   patTemp    : string;     //Ruta para los archivos temporales del frame de edición.
   patSyntax  : string;     //Ruta de los archivos de sintaxis
   patThemes  : string;     //Ruta de los archivos de temas

   archivoEnt  : string;    //Archivo de entrada
   MostrarError: Boolean;   //Bandera para mostrar mensajes de error.
   ActConsSeg  : Boolean;   //Activa consultas en segundo plano

   LastProject: string; // the name of the file that will be compiled

/////////////// Campos para manejo del diccionario //////////
var
 curLanguage: string;  //identificador del lenguaje

procedure StartCountElapsed;
procedure EndCountElapsed(msg: string);

function Trans(const strEn, strEs, strQu, strDe, strUk, strRu, strFr: string): string;
//////////////////////////////////////////////////////
function NombDifArc(nomBase: String): String;
procedure AddLine(var baseStr: string; newStr: string);

implementation
var
  ET : TEpikTimer;
const
  WA_DIR_NOEXIST = 'Directory: %s no found. It will be created';
  ER_CANN_READDI = 'Cannot read or create directories.';

procedure StartCountElapsed;
begin
  ET.Clear;
  ET.Start;
end;
procedure EndCountElapsed(msg: string);
begin
  ET.Stop;
  debugln(msg + IntToStr(round(ET.Elapsed*1000))+'ms');
end;

function Trans(const strEn, strEs, strQu, strDe, strUk, strRu, strFr: string): string;
  function ClearLangId(str: string): string;
  {Limpia la cadena del caracter identificador de lenguaje, de la forma:
  #en=
  que se puede usar al inicio de una cadena.}
  begin
     if str='' then exit('');
     if length(str)<4 then exit(str);
     if (str[1] = '#') and (str[4] = '=') then begin
       delete(str, 1, 4);
       exit(str);
     end else begin
       exit(str);
     end;
  end;
begin
  case LowerCase(curLanguage) of
  'en': begin
     Result := ClearLangId(strEn);
  end;
  'es': begin
     Result := ClearLangId(strEs);
     if Result = '' then Result := ClearLangId(strEn);
  end;
  'qu': begin
     Result := ClearLangId(strQu);
     if Result = '' then Result := strEs;
  end;  //por defecto
  'de': begin
     Result := ClearLangId(strDe);
     if Result = '' then Result := ClearLangId(strEn);
  end;  //por defecto
  'uk': begin
     Result := ClearLangId(strUk);
     if Result = '' then Result := ClearLangId(strEn); //por defecto
  end;
  'ru': begin
     Result := ClearLangId(strRu);
     if Result = '' then Result := ClearLangId(strEn); //por defecto
  end;
  'fr': begin
     Result := ClearLangId(strFr);
     if Result = '' then Result := ClearLangId(strEn); //por defecto
  end;
  else  //Por defecto Inglés
    Result := ClearLangId(strEn);
  end;
end;
function NombDifArc(nomBase: String): String;
{Genera un nombre diferente de archivo, tomando el nombre dado como raiz.}
const MAX_ARCH = 10;
var i : Integer;    //Número de intentos con el nombre de archivo de salida
    cadBase : String;   //Cadena base del nombre base
    extArc: string;    //extensión

  function NombArchivo(i: integer): string;
  begin
    Result := cadBase + '-' + IntToStr(i) + extArc;
  end;

begin
   Result := nomBase;  //nombre por defecto
   extArc := ExtractFileExt(nomBase);
   if ExtractFilePath(nomBase) = '' then exit;  //protección
   //quita ruta y cambia extensión
   cadBase := ChangeFileExt(nomBase,'');
   //busca archivo libre
   for i := 0 to MAX_ARCH-1 do begin
      If not FileExists(NombArchivo(i)) then begin
        //Se encontró nombre libre
        Exit(NombArchivo(i));  //Sale con nombre
      end;
   end;
   //todos los nombres estaban ocupados. Sale con el mismo nombre
End;
procedure AddLine(var baseStr: string; newStr: string);
{Agrega una nueva línea a una cadena, verificando primero si está vacía}
begin
  if length(baseStr)=0 then baseStr := newStr
  else baseStr := baseStr + LineEnding + newStr;
end;

initialization
  //inicia directorios de la aplicación
  patApp      := ExtractFilePath(Application.ExeName);  //incluye el '\' final
  patTemp     := patApp + 'temp';
  patSyntax   := patApp + 'syntax';
  patThemes   := patApp + 'themes';
  archivoEnt  := '';    //archivo de entrada
  //verifica existencia de carpetas de trabajo
  try
    if not DirectoryExists(patTemp) then begin
       msgexc(WA_DIR_NOEXIST, [patTemp]);
       CreateDir(patTemp);
    end;
    if not DirectoryExists(patSyntax) then begin
       msgexc(WA_DIR_NOEXIST, [patSyntax]);
       CreateDir(patSyntax);
    end;
    if not DirectoryExists(patThemes) then begin
       msgexc(WA_DIR_NOEXIST, [patThemes]);
      CreateDir(patThemes);
    end;

  except
    msgErr(ER_CANN_READDI);
  end;
  ET := TEpikTimer.Create(nil);  //Used for precision time measure
finalization
  //Por algún motivo, la unidad HeapTrc indica que hay gotera de memoria si no se liberan
  //estas cadenas:
  patApp :=  '';
  patTemp := '';
  patSyntax := '';
  ET.Destroy;
end.

