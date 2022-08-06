unit CompGlobals;
{$mode ObjFPC}{$H+}
interface
uses  Classes, SysUtils, EpikTimer, LazLogger;

const
  NOM_PROG = 'P65Pas';   //nombre de programa
  {$I ../version.txt}   //versión del programa

var
   ActConsSeg  : Boolean;     //Activa consultas en segundo plano
   archivoEnt  : string;      //archivo de entrada
   //Esta propiedad tal vez deba estar junto a las demás opciones del compilador.
   unitPaths   : TStringList; //Lista de rutas donde buscar unidades.
/////////////// Campos para manejo del diccionario //////////
var
 curLanguage: string;  //identificador del lenguaje

procedure StartCountElapsed;
procedure EndCountElapsed(msg: string);

function Trans(const strEn, strEs, strQu, strDe, strUk, strRu, strFr: string): string;
//////////////////////////////////////////////////////
function LeerParametros: boolean;
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
function LeerParametros: boolean;
{lee la linea de comandos
 Si hay error devuelve TRUE}
var
   par : String;
   i   : Integer;
begin
   Result := false;    //valor por defecto
   //valores por defecto
   archivoEnt := '';
   ActConsSeg := False;
   //Lee parámetros de entrada
   par := ParamStr(1);
   if par = '' then begin
     debugln('Nombre de archivo vacío.');
     Result := true;
     exit;  //sale con error
   end;
   if par[1] = '/' then begin  //es parámetro
      i := 1;  //para que explore desde el principio
   end else begin  //es archivo
      archivoEnt := par;  //el primer elemento es el archivo de entrada
      i := 2;  //explora siguientes
   end;
   while i <= ParamCount do begin
      par := ParamStr(i);
      If par[1] = '/' Then begin
         Case UpCase(par) of
            '/CONSEG': ActConsSeg := True;
            '/NOCONSEG': ActConsSeg := False;
         Else begin
                debugln('Error. Parámetro desconocido: ' + par);
                Result := true;
                exit;  //sale con error
              End
         End
      end Else begin
//         archivoSal := par;
      End;
      inc(i);  //pasa al siguiente
   end;
End;
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
  archivoEnt  := '';    //archivo de entrada

  ET := TEpikTimer.Create(nil);  //Used for precision time measure
  unitPaths   := TStringList.Create;

finalization
  unitPaths.Destroy;
  ET.Destroy;
end.

