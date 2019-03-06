{
MisUtils 0.6
============
Por Tito Hinostroza 06/02/2017
* Se agrega la función LoadPNGToImageList()
* Se agrega la función AddStringToFile().
* Se agrega una versión simple el método Join().

MisUtils 0.5b
============
 Por Tito Hinostroza 13/05/2015
* Se agregan las funciones DT2Number() and Number2DT().
* Se agrega la función StringLike().
* Se elimina la variable global msjError, ya que se encontró casos de duplicidad de
nombre con la variable de error global de la aplicación. Además se está evitando usar
variables globales.
* Se agrega la función TrimEndLine() para quitar un salto de línea al final de una
cadena.
* Se modifica f2N, para fijar siempre el punto decimal como ".".
* Se modifica f2S(), proque se detectó problemas en Win32.

 Descripción
 ============
 Librería de funciones útiles para mostrar mensajes en pantalla, para guardar datos en
 archivos, para crear aplicaciones en varios idiomas y algunas utilidades adicionales.
 }
unit MisUtils;

{$mode objfpc}{$H+}

interface

uses  Classes, SysUtils, Forms, Graphics, Dialogs, process, Controls, lclType,
  LazFileUtils, Masks, types, dateutils, strutils, Menus, LCLProc, LCLIntf;

var
//  msjError  : string;       //mensaje de error de la aplicación
  dictionary: TstringList;  //diccionario para el manejo de mensajes
  TranslateMsgs: boolean;   //activa la traducción del mensaje
//funciones para mostrar mensajes
procedure MsgExc(txt: string; Caption: string = '');
procedure MsgExc(Fmt: String; const Args: array of const);
procedure MsgErr(txt: string; Caption: string = '');
procedure MsgErr(Fmt: String; const Args: array of const);
//function MsgBox(txt: PChar; Caption: string = ''; flags: longint = 0): integer;
function MsgBox(txt: String; Caption: string = ''; flags: longint = 0): integer;
procedure MsgBox(Fmt : String; const Args : Array of const);
function MsgYesNo(txt: string): byte;
function MsgYesNo(Fmt: string; const Args: array of const): byte;
function MsgYesNoCancel(txt: string): byte;
function MsgYesNoCancel(Fmt: string; const Args: array of const): byte;
//funciones diversas
function Explode(delimiter:string; str:string):TStringDynArray;
function Join(delimiter: char; const a: TStringDynArray): string;
function Exec(com, par: string; WaitOnExit: boolean = false): boolean;
procedure AnchorTo(Ctl: TControl; Side: TAnchorKind; Sibling: TControl;
  Space: integer = 0; Internal: Boolean = false);
procedure TrimEndLine(var cad: string);
function StringLike(const str: string; mask: string): boolean;
procedure StringToFile(const s: string; const FileName: string);
function StringFromFile(const FileName: string): string;
function AddStringToFile(txt: string; const FileName: string): boolean;
//Utilidades para menús
function AddItemToMenu(menu: TMenuItem; txt: string; evento: TNotifyEvent): TMenuItem;
procedure CheckOnlyOneItem(item: TMenuItem);
procedure CheckOnlyOneItem(Menu: TMenuItem; Caption: string);
function LoadPNGToImageList(imagList16: TImageList; imgFile: string): Integer;
//Genera un nombre distinto de archivo
function GetNewFileName(nomBase: String; maxNumFile: integer = 10): String;
//Genera un nombre distinto de carpeta
function GetNewFolderName(nomBase: String; maxNumFile: integer = 10): String;
//Conversion de tipos a cadena
function I2f(n: Integer):String;
Function f2I(s : String): Integer;
Function f2I(s : WideString): Integer;
function N2f(n: Double):String;
Function f2N(s : String): Double;
Function f2N(s : WideString): Double;
Function B2f(b : Boolean) : String;
Function f2B(s : String) : Boolean;
Function D2f(d : TDateTime): String;
Function f2D(s : String) : TDateTime;
Function f2D(s : WideString) : TDateTime;
Function S2f(s : String) : String;
function f2S(s : String) : String;

function DT2Number(const dt: TDateTime): Int64;
function Number2DT(n: Int64): TDateTime;
function T2f(const dt: TDateTime): string;
function f2T(hex: string): TDateTime;

//Funciones del diccionario
procedure dicClear;  //limpia el diccionario
procedure dicSet(key, value: string);  //fija una entrada del diccionario
procedure dicDel(key: string);  //limpia una entrada del diccionario
procedure TransCapCtrls(TheForm: TForm; Caption, value: string);  //traduce un mensaje de un control
function dic(key: string): string;     //lee un mensaje traducido
function dic(Fmt : String; const Args : Array of const): string; //lee un mensaje traducido
//manejo de consola
procedure console(Fmt : String; const Args : Array of const);  //muestra mensaje en consola
procedure consoleTickStart;  //inicia contador de tiempo
procedure consoleTickCount(msg: string);  //muestra diferencia de tiempo


implementation
const
  szChar = SizeOf(Char);

var
  timeCnt: types.DWORD;  //contador para medir intervalos de tiempo

procedure MsgExc(txt: string; Caption: string = '');
//Mensaje de exclamación
begin
  if TranslateMsgs then txt := dic(txt);
  Application.MessageBox(PChar(txt), PChar(Caption), MB_ICONEXCLAMATION);
end;
procedure MsgExc(Fmt: String; const Args: array of const);
var
  txt: String;
begin
  if TranslateMsgs then Fmt := dic(Fmt);
  txt := Format(Fmt, Args);
  Application.MessageBox(Pchar(txt), '', MB_ICONEXCLAMATION);
end;
procedure MsgErr(txt: string; Caption: string = '');
//Mensaje de error
begin
  if TranslateMsgs then txt := dic(txt);
  Application.MessageBox(PChar(txt), PChar(Caption), MB_ICONERROR);
end;
procedure MsgErr(Fmt: String; const Args: array of const);
var
  txt: String;
begin
  if TranslateMsgs then Fmt := dic(Fmt);
  txt := Format(Fmt, Args);
  Application.MessageBox(Pchar(txt), '', MB_ICONERROR);
end;
{function MsgBox(txt: PChar; Caption: string = ''; flags: longint = 0): integer;
begin
  if TranslateMsgs then txt := dic(txt);
  Result := Application.MessageBox(txt, PChar(Caption), flags);
end;}
function MsgBox(txt: String; Caption: string = ''; flags: longint = 0): integer;
begin
  if TranslateMsgs then txt := dic(txt);
  Result := Application.MessageBox(Pchar(txt), PChar(Caption), flags);
end;
procedure MsgBox(Fmt: String; const Args: array of const);
var
  txt: String;
begin
  if TranslateMsgs then Fmt := dic(Fmt);
  txt := Format(Fmt, Args);
  Application.MessageBox(Pchar(txt), '', 0);
end;
function MsgYesNo(txt: string): byte;
//Muestra un mensaje en pantalla con los botones Yes - No
//Devuelve 1, si para la opción Yes
//Devuelve 2, si para la opción No
var
  r: Integer;
begin
  Result := 0;  //Valor por defecto
  if TranslateMsgs then txt := dic(txt);
  r := Application.MessageBox(PChar(txt),'',MB_YESNO + MB_ICONQUESTION);
  if r = IDYES then exit(1);
  if r = IDNO  then exit(2);
end;
function MsgYesNo(Fmt: string; const Args: array of const): byte;
//Muestra un mensaje en pantalla con los botones Yes - No
//Devuelve 1, si para la opción Yes
//Devuelve 2, si para la opción No
var
  r: Integer;
  txt: String;
begin
  Result := 0;  //Valor por defecto
  if TranslateMsgs then Fmt := dic(Fmt);
  txt := Format(Fmt, Args);
  r := Application.MessageBox(PChar(txt),'',MB_YESNO + MB_ICONQUESTION);
  if r = IDYES then exit(1);
  if r = IDNO  then exit(2);
end;
function MsgYesNoCancel(txt: string): byte;
//Muestra un mensaje en pantalla con los botones Yes - No - Cancel
//Devuelve 1, si para la opción Yes
//Devuelve 2, si para la opción No
//Devuelve 3, si para la opción Cancel
var
  r: Integer;
begin
  Result := 0;  //Valor por defecto
  if TranslateMsgs then txt := dic(txt);
  r := Application.MessageBox(PChar(txt),'',MB_YESNOCANCEL + MB_ICONQUESTION);
  if r = IDYES then exit(1);
  if r = IDNO  then exit(2);
  if r = IDCANCEL  then exit(3);
end;
function MsgYesNoCancel(Fmt: string; const Args: array of const): byte;
//Muestra un mensaje en pantalla con los botones Yes - No - Cancel
//Devuelve 1, si para la opción Yes
//Devuelve 2, si para la opción No
//Devuelve 3, si para la opción Cancel
var
  r: Integer;
  txt: String;
begin
  Result := 0;  //Valor por defecto
  if TranslateMsgs then Fmt := dic(Fmt);
  txt := Format(Fmt, Args);
  r := Application.MessageBox(PChar(txt),'',MB_YESNOCANCEL + MB_ICONQUESTION);
  if r = IDYES then exit(1);
  if r = IDNO  then exit(2);
  if r = IDCANCEL  then exit(3);
end;
//funciones diversas
function Explode(delimiter:string; str:string):TStringDynArray;
var
  p, n, dsize:integer;
begin
  n := 0;
  dsize := length(delimiter);
  while true do begin
    p := pos(delimiter,str);
    if p > 0 then begin
      inc(n);
      SetLength(Result,n);
      Result[n-1] := copy(str,1,p-1);
      delete(str,1,p+dsize-1);
    end else break;
  end;
  inc(n);
  SetLength(Result,n);
  Result[n-1] := str;
end;
function Join(delimiter: char; const a: TStringDynArray): string;
var
  i: Integer;
begin
{
linea := #9 + #9 + 'a'+#9 + 'b' + #9;
debugln('linea ini=|' + linea + '|');
debugln('long ini=' + IntToStr(length(linea)));
a := explode(#9, linea);
linea := join(a, #9);
debugln('linea fin=|' + linea + '|');
debugln('long fin=' + IntToStr(length(linea)));
}
  Result := '';
  for i:=0 to high(a) do begin
    if i=0 then
      Result := a[0]
    else
      Result := Result  + delimiter + a[i];
  end;
end;
function Exec(com, par: string; WaitOnExit: boolean = false): boolean;
//Ejecuta un programa. Devuelve FALSE si hubo error
var
  p    : TProcess;   //el proceso a manejar
begin
  Result := true;
  p := TProcess.Create(nil); //Crea proceso
  if WaitOnExit then p.Options:= p.Options + [poWaitOnExit];
  //p.CommandLine := SysToUTF8(com);
  p.Executable:=com;
  p.Parameters.Clear;
  p.Parameters.Add(par);
  try
    p.Execute;
  except
    Result := false;
    MsgBox('Fallo al iniciar aplicativo: '+ p.Executable);;
  end;
  p.Free;
end;
procedure AnchorTo(Ctl: TControl; Side: TAnchorKind; Sibling: TControl;
  Space: integer = 0; Internal: Boolean = false);
{Utilidad para facilitar el anclaje a un control vecino, o a un contenedor.
Es una versión de AnchorToNeighbour(), ampliada. La idea es que se alínie
un control al lado del otro. Si "Internal" es true, el alineamiento se hará
en sentido opuesto}
begin
    Ctl.AnchorSide[Side].Control:=Sibling;  //define vecino
    case Side of
    akLeft: begin
      Ctl.BorderSpacing.Left:=Space;
      if Internal then Ctl.AnchorSide[Side].Side:=asrLeft
      else Ctl.AnchorSide[Side].Side:=asrRight;
    end;
    akTop: begin
      Ctl.BorderSpacing.Top:=Space;
      if Internal then Ctl.AnchorSide[Side].Side:=asrTop
      else Ctl.AnchorSide[Side].Side:=asrBottom;
    end;
    akRight: begin
      Ctl.BorderSpacing.Right:=Space;
      if Internal then Ctl.AnchorSide[Side].Side:=asrRight
      else Ctl.AnchorSide[Side].Side:=asrLeft;
    end;
    akBottom: begin
      Ctl.BorderSpacing.Bottom:=Space;
      if Internal then Ctl.AnchorSide[Side].Side:=asrBottom
      else Ctl.AnchorSide[Side].Side:=asrTop;
    end;
    end;
    Ctl.Anchors:=Ctl.Anchors+[Side];  //agrega bandera de anclaje
end;
procedure TrimEndLine(var cad: string);
{Verifica si la cadena incluye un salto de línea al final y de ser así, lo quita}
var
  lSalto: Integer;
begin
  lSalto := length(LineEnding);
  if length(cad)<lSalto then exit;  //no puede contener salto
  if RightStr(cad, lSalto) = LineEnding then begin
    //Contiene el salto
    delete(cad, length(cad)-lSalto +1 ,lSalto);
  end;
end;
function StringLike(const str: string; mask: string): boolean;
{Utilidad para comparación de cadenas al estilo de VB. El patrón de comparación es
"mask" y tiene los siguientes comodines:
'?' -> coincide con cualquier caracter.
'*' -> coincide con cualquier texto.
'#' -> coincide con cualquier caracter numércio.
'[]' -> indica un conjunto de cacacteres.
}
var
  msk: TMask;
begin
  mask := StringReplace(mask, '#', '[0-9]', [rfReplaceAll]);
  msk := Tmask.Create(mask);
  Result := msk.Matches(str);
  msk.Destroy;
end;
procedure StringToFile(const s: string; const FileName: string);
///Guarda una cadena a un archivo. El archivo debe estar la codificaión del sistema.
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    FileStream.WriteBuffer(Pointer(s)^, (Length(s) * szChar));
  finally
    FreeAndNil(FileStream);
  end;
end;
function StringFromFile(const FileName: string): string;
//Lee un archivo como una cadena.
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(Result, (FileStream.Size div szChar));
    FileStream.ReadBuffer(Pointer(Result)^, FileStream.Size);
  finally
    FreeAndNil(FileStream);
  end;
end;
function AddStringToFile(txt: string; const FileName: string): boolean;
{Escribe una cadena de texto a un archivo. }
var
  f : Textfile;
begin
  Result := False;
  AssignFile(f, FileName);
  try
    if FileExists(FileName) = False then begin
      Rewrite(f)
    end else begin
      Append(f);
    end;
    Writeln(f, txt);
    Result := True;
  finally
    CloseFile(f);
  end;
end;//Utilidades para menús
function AddItemToMenu(menu: TMenuItem; txt: string; evento: TNotifyEvent
  ): TMenuItem;
//Agrega un ítema un menú. Devuelve la refrecnia ál nuevo ítem agregado.
var
  item: TMenuItem;
begin
  item := TMenuItem.Create(nil);
  item.Caption:= txt;  //nombre
  item.OnClick:=evento;
  menu.Add(item);
  Result := item;
end;
procedure CheckOnlyOneItem(item: TMenuItem);
//Marca un ítem de un menú y deja los demás desmarcados
var
  MenuPadre: TMenuItem;
  i: Integer;
begin
  MenuPadre := item.Parent;
  if MenuPadre= nil then exit;
  for i:=0 to MenuPadre.Count-1 do  //limpia todos
    MenuPadre.Items[i].Checked := false;
  item.Checked:=true;  //marca el ítem
end;
procedure CheckOnlyOneItem(Menu: TMenuItem; Caption: string);
//Marca un ítem de un menú (usando su etiqueta) y deja los demás desmarcados.
//Ignora la caja y el símbolo "&".
var
  i: Integer;
  capItem: String;
  it: TMenuItem;
begin
  if Menu = nil then exit;  //proteción
  //busca el ítem por su etiqueta
  it := nil;
  Caption := UpCase(Caption);
  for i:=0 to Menu.Count-1 do begin
    capItem := Upcase(Menu.Items[i].Caption);
    capItem := StringReplace(capItem,'&','',[rfReplaceAll]);
    if capItem = Caption then begin
      it := Menu.Items[i];
      break;
    end;
  end;
  if it = nil then exit;   //no encontró
  CheckOnlyOneItem(it);   //marca
end;
function LoadPNGToImageList(imagList16: TImageList; imgFile: string): Integer;
{Rutina para cargar un archivo PNG, en un ImageList. Devuelve el índice de la imagen}
var
  pngbmp: TPortableNetworkGraphic;
begin
  if not FileExists(imgFile) then exit(-1);
  pngbmp:=TPortableNetworkGraphic.Create;
  pngbmp.LoadFromFile(imgFile);
  Result:= imagList16.Add(pngbmp, nil);
  pngbmp.Destroy;
end;
function GetNewFileName(nomBase: String; maxNumFile: integer = 10): String;
{Generate a different file name, using the base name.
}
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
   for i := 0 to maxNumFile-1 do begin
      If not FileExists(NombArchivo(i)) then begin
        //Se encontró nombre libre
        Exit(NombArchivo(i));  //Sale con nombre
      end;
   end;
   //All names were used. Return the same name.
End;
function GetNewFolderName(nomBase: String; maxNumFile: integer = 10): String;
{Genera un nombre diferente de archivo, tomando el nombre dado como raiz.}
var i : Integer;    //Número de intentos con el nombre de archivo de salida
    cadBase : String;   //Cadena base del nombre base

  function NombFolder(i: integer): string;
  begin
    Result := cadBase + '-' + IntToStr(i);
  end;

begin
   Result := nomBase;  //nombre por defecto
//   cadBase := ExtractFilePath(nomBase);
   cadBase := nomBase;
   if cadBase = '' then exit;  //protección
   //busca archivo libre
   for i := 0 to maxNumFile-1 do begin
      If not DirectoryExists(NombFolder(i)) then begin
        //Se encontró nombre libre
        Exit(NombFolder(i));  //Sale con nombre
      end;
   end;
   //todos los nombres estaban ocupados. Sale con el mismo nombre
End;

//############## Funciones de conversión de datos para acceso a disco ############
function I2f(n: Integer): String;
begin
   Result := IntToStr(n);
end;

function f2I(s: String): Integer;
begin
   Result := StrToInt(s);
end;
function f2I(s: WideString): Integer;
begin
  Result := StrToInt(AnsiString(s));
end;

function N2f(n: Double):String;
//Convierte número a cadena para guardar en disco. Independiente de la configuración regional
begin
    Result := FloatToStr(n);
End;

function f2N(s: String): Double;
//Convierte cadena de disco a número. Independiente de la configuración regional
begin
  DefaultFormatSettings.DecimalSeparator:='.';   //para uniformizar el formato
  Result := StrToFloat(s);     //usa siempre el punto decimal
End;
function f2N(s: WideString): Double;
begin
  DefaultFormatSettings.DecimalSeparator:='.';   //para uniformizar el formato
  Result := StrToFloat(AnsiString(s));     //usa siempre el punto decimal
end;
function B2f(b: Boolean): String;
//Convierte Boleean a cadena para guardar en disco.
begin
    If b Then Result := 'V' Else Result := 'F';
End;
function f2B(s: String): Boolean;
//Convierte cadena de disco a Boleean
begin
    If s = 'V' Then exit(True) else exit(False);
End;
function D2f(d: TDateTime): String;
//Convierte fecha a cadena para guardar en disco.
var
  s: string;
begin
  DateTimeToString(s,'yyyy:mm:dd:hh:nn:ss',d);
  Result :=  s;
End;
function f2D(s: String): TDateTime;
//Convierte cadena de disco a fecha.
var a: TStringDynArray;
begin
  a := explode(':',s);
  Result := EncodeDateTime(StrToInt(a[0]), StrToInt(a[1]), StrToInt(a[2]),
                           StrToInt(a[3]), StrToInt(a[4]), StrToInt(a[5]), 0);
End;
function f2D(s: WideString): TDateTime;
var
  a: TStringDynArray;
begin
  a := explode(':', AnsiString(s));
  Result := EncodeDateTime(StrToInt(a[0]), StrToInt(a[1]), StrToInt(a[2]),
                           StrToInt(a[3]), StrToInt(a[4]), StrToInt(a[5]), 0);
end;

function S2f(s : String) : String;
//Convierte cadena a formato para guardar en disco, en una línea.
begin
  //Inicialmente se trabajó con ReplaceText() aquí, pero daba cadena vacía en Win32
  Result := StringReplace(s, LineEnding, #1, [rfReplaceAll]);
end;
function f2S(s : String) : String;
//Convierte cadena leída de disco a cadena multilínea.
begin
  //Inicialmente se trabajó con ReplaceText() aquí, pero daba cadena vacía en Win32
  Result := StringReplace(s, #1, LineEnding, [rfReplaceAll]);
end;

function DT2Number(const dt: TDateTime): Int64;
{Convierte fecha-hora en número entero (no incluye milisegundos). Esta función se creó
como reemplazo a DateTimeToUnix(), ya que en la presente versión de Lazarus, tiene
errores de redondeo.}
var
  hh, nn, ss, MilliSecond: word;
begin
  DecodeTime(dt, hh, nn, ss, MilliSecond);
  Result := trunc(dt)*86400 + hh * 3600 + nn * 60 + ss;
end;
function Number2DT(n: Int64): TDateTime;
{Función opuesta de DT2Number()}
var
  day, hh, nn, ss: Int64;
begin
  day := n div 86400;
  n := n mod 86400;
  hh := n div 3600;
  n := n mod 3600;
  nn := n div 60;
  ss := n mod 60;
  Result := EncodeTime(hh,nn,ss,0) + day;
end;
function T2f(const dt: TDateTime): string;
{Codifica una fecha-hora en una cadena compacta, usando hexadecimal. Usaulmente para
una fecha generará solo 8 caracteres.}
var
  n: Int64;
begin
  n := DT2Number(dt);
  if n=0 then
    Result := '0'
  else if n<=$FF then
    Result := IntTohex(n,2)
  else if n<=$FFF then
    Result := IntTohex(n,3)
  else if n<=$FFFF then
    Result := IntTohex(n,4)
  else if n<=$FFFFF then
    Result := IntTohex(n,5)
  else if n<=$FFFFFF then
    Result := IntTohex(n,6)
  else if n<=$FFFFFFF then
    Result := IntTohex(n,7)
  else if n<=$FFFFFFFF then
    Result := IntTohex(n,8)
  else
    Result := IntTohex(n,9);
end;
function f2T(hex: string): TDateTime;
{Restaura la cadena convertida por DT2f}
var
  m: Int64;
begin
  m := StrToInt64('$'+hex);
  Result := Number2DT(m);
end;

procedure dicClear;
//Limpia el diccionario, de modo que no se traducirá ningún mensaje
begin
  dictionary.Clear;
end;
procedure dicSet(key, value: string);
//Fija o agrega una entrada al diccionario
begin
  //los símbolos "=", no se pueden ingresar
  key := StringReplace(key, '=', #31, [rfReplaceAll]);
  dictionary.values[key]:=value;
end;
procedure dicDel(key: string);
//Limpia una entrada del diccionario
begin
  dictionary.values[key]:='';
end;

procedure TransCapCtrls(TheForm: TForm; Caption, value: string);
//Traduce la etiqueta de un control de un formulario
var
  c: TControl;
  i : integer;
begin
   for i := 0 to TheForm.ControlCount-1 do begin
     c := theForm.Controls[i];
     if c.Caption = Caption then c.Caption := value;
   end;
end;

function dic(key: string): string;
//Devuelve un mensaje en el lenguaje definido, dada la clave.
//La clave no puede tener el signo "="
begin
  key := StringReplace(key, #31, '=', [rfReplaceAll]);  //codifica la clave
  Result := dictionary.Values[key];
  //si no encuentra, devuelve la misma clave
  if Result = '' then Result := key;
end;
function dic(Fmt: String; const Args: array of const): string;
var
  txt: String;
begin
  txt := dic(Fmt);  //busca
  Result := Format(txt, Args);  //completa
end;
procedure console(Fmt: String; const Args: array of const);
begin
  debugln(Format(Fmt, Args));  //completa
end;
procedure consoleTickStart;
//Inicia el contador de milisegundos
begin
  timeCnt:=GetTickCount;
end;
procedure consoleTickCount(msg: string);
//Muestra la diferencia de tiempo transcurrido, e inicia otra cuenta
begin
  debugln(msg + ':' + IntToStr(GetTickCount-timeCnt) + 'mseg');
  timeCnt := GetTickCount;
end;

Initialization
  //crea diccionario
  dictionary := TStringList.Create;
  TranslateMsgs := false;
Finalization

  dictionary.Destroy;
end.

