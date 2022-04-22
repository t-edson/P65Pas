{Part of the compiler with functionalities for:
 - Contexts managment.
 - Info, Warning and Error generation.
                                          Created by Tito Hinostroza  01/09/2020
}
unit CompContexts;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LexPas, LCLProc;

type
  { TContexts }
  //Extructura para manejar diversas fuentes de datos de contexto
  TContexts = class
  private
    idCount: integer;        //Contador para obtener el índice de un contexto
  public  //Events
    OnNewLine: procedure(lin: string) of object;
  public //Information for current context
    curCtx   : TContext;       //referencia al contexto de entrada actual
    //Control for state
    function GetCtxState: TContextState;
    procedure SetCtxState(pc: TContextState);
    //Control for position
    function GetSrcPos: TSrcPos;
    procedure SetSrcPos(const srcPos: TSrcPos);
  public //Information for any context
    function ctxId(fileSrc: string): integer;
    function ctxFile(idCtx: integer): string;
    function ctxFile(const srcPos: TSrcPos): string;
    function ctxFileName(const srcPos: TSrcPos): string;
    function ctxFileDir(const srcPos: TSrcPos): string;
  protected  //Context manage
    ctxList: TContextList;   //Lista de contextos de entrada
    function AddContext: TContext;
    procedure NewContextFromText(txt: string; fileSrc: String);
    procedure NewContextFromFile(filSrc: String; out notFound: boolean);
    procedure NewContextFromTStrings(lins: Tstrings; filSrc: String);
    procedure RemoveContext;
    procedure ClearContexts;      //Deletes all contexts.
  public //Scan functions
    token    : string;     //Current Token
    tokType  : TTokenKind; //Current Token type
    //function tokL: string; //token actual en minúscula
    function atEol: Boolean; inline;
    function atEof: Boolean;
    procedure SkipWhites;
    procedure SkipWhitesNoEOL;
    procedure Next;       //Go to the next token.
    procedure GotoEOL;    //Go to the EOL position.
  public  //Context debugging
    procedure ShowContexts;
    procedure ShowCurContInformat;
  public    //Errors and warnings
    curLocation: TxpEleLocation;   {Current location for scan. This tells the compiler
                                   where it's scanning. It useful because some declarations
                                   have to interpret in different way according to the
                                   location.}
    HayError: boolean;             //Flag for errors
    OnWarning: procedure(warTxt: string; const srcPos: TSrcPos) of object;
    OnError  : procedure(errTxt: string; const srcPos: TSrcPos) of object;
    OnInfo   : procedure(infTxt: string) of object;
    procedure ClearError;
    //Rutinas de generación de mensajes
    procedure GenInfo(msg: string);
    //Rutinas de generación de advertencias
    procedure GenWarn(msg: string; const srcPos: TSrcPos);
    procedure GenWarn(msg: string; const Args: array of const; const srcPos: TSrcPos);
    procedure GenWarn(msg: string);
    procedure GenWarn(msg: string; const Args: array of const);
    //Rutinas de generación de error
    procedure GenError(msg: string; const srcPos: TSrcPos);
    procedure GenError(msg: String; const Args: array of const; const srcPos: TSrcPos);
    procedure GenError(msg: string);
    procedure GenError(msg: String; const Args: array of const);
  public  //Initialization
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TContexts }
//Information for current context
function TContexts.GetCtxState: TContextState;
//Devuelve Contexto actual y su posición
begin
  if curCtx = nil then begin
    //Aún no hay Contexto definido
  end else begin
    curCtx.GetContextState(Result);
  end;
end;
procedure TContexts.SetCtxState(pc: TContextState);
//Fija Contexto actual y su posición
begin
  curCtx := ctxList[pc.idCtx];
  curCtx.SetContextState(pc);
  //Update current token and type
  token := curCtx.ReadToken;
  tokType := curCtx.tokType;
end;
function TContexts.GetSrcPos: TSrcPos;
{Devuelve un objeto TSrcPos, en la posición actual.}
begin
  Result.idCtx := curCtx.idCtx;
  Result.Row := curCtx.row0;
  Result.Col := curCtx.col0;
end;
procedure TContexts.SetSrcPos(const srcPos: TSrcPos);
begin
  curCtx := ctxList[srcPos.idCtx];
  if curCtx = nil then begin
    //No tiene un Contexto actual
  end else begin
    curCtx.row0 := srcPos.row;
    curCtx.col0 := srcPos.col;
  end;
end;
//Information for any context
function TContexts.ctxId(fileSrc: string): integer;
{Returns the ID of a context whose fileSrc attribute is the indicated.}
var
  i: Integer;
begin
  fileSrc := UpCase(fileSrc);
  for i:=0 to ctxList.Count-1 do begin
    if UpCase(ctxList[i].fileSrc) = fileSrc then exit(i);
  end;
  //Not found
  exit(-1);
end;
function TContexts.ctxFile(idCtx: integer): string;
{Returns the file name for some context.}
var
  ct: TContext;
begin
  if idCtx<0 then exit('');
  ct := ctxList[idCtx];
  exit(ct.fileSrc);
end;
function TContexts.ctxFile(const srcPos: TSrcPos): string;
{Returns the file name for some context, receiving a TSrcPos.}
var
  ct: TContext;
begin
  if srcPos.idCtx<0 then exit('');
  ct := ctxList[srcPos.idCtx];
  exit(ct.fileSrc);
end;
function TContexts.ctxFileName(const srcPos: TSrcPos): string;
{Returns the file name (like file1.pas ) from the source file of a context}
var
  ct: TContext;
begin
  if srcPos.idCtx<0 then exit('');
  ct := ctxList[srcPos.idCtx];
  exit(ExtractFileName(ct.fileSrc));
end;
function TContexts.ctxFileDir(const srcPos: TSrcPos): string;
{Returns the file directory (like C:\dir1 ) from the source file of a context}
var
  ct: TContext;
begin
  if srcPos.idCtx<0 then exit('');
  ct := ctxList[srcPos.idCtx];
  exit(ExtractFileDir(ct.fileSrc));
end;
//Context manage
function TContexts.AddContext: TContext;
{Add a context to "ctxList" and returns the reference.
Punto único para agregar un contexto}
begin
  inherited;
  Result := TContext.Create;  //Creates Context.
  Result.retPos := GetCtxState; //Keep return position.
  Result.onGenError := @GenError;
  ctxList.Add(Result);        //Register Context.
  idCount := ctxList.Count-1; //Calculate de index.
  Result.idCtx := idCount;    //Set reference to index.
end;
procedure TContexts.NewContextFromText(txt: string; fileSrc: String);
{Create a new context from a text and set "curCtx" to the new context created.
Parameter "filSrc" is a optional reference to a file name, asociated to "txt".}
begin
  curCtx := AddContext;
  {$ifdef debug_mode}
  debugln('  +Nex context from Txt:'+filSrc);
  {$endif}
  curCtx.SetSource(txt);      //Inicia con texto
  curCtx.fileSrc := fileSrc;  {Se guarda el nombre del archivo actual, solo para poder procesar
                               las funciones $NOM_ACTUAL y $DIR_ACTUAL}
  //Actualiza token actual
  token := curCtx.ReadToken;  //lee el token
  tokType := curCtx.tokType;  //lee atributo
end;
procedure TContexts.NewContextFromFile(filSrc: String; out notFound: boolean);
{Create a new context from a file and set "curCtx" to the new context created.
If the file is not found, returns TRUE in "notFound".}
begin
  notFound := false;
  If not FileExists(filSrc)  Then  begin  //ve si existe
    notFound := true;
    exit;
  end;
  curCtx := AddContext;
  {$ifdef debug_mode}
  debugln('  +Nex context from File:'+filSrc);
  {$endif}
  curCtx.SetSourceF(filSrc);   //Inicia con archivo
  //Actualiza token actual
  token := curCtx.ReadToken;    //lee el token
  tokType := curCtx.tokType;  //lee atributo
end;
procedure TContexts.NewContextFromTStrings(lins: Tstrings; filSrc: String);
{Create a new context from a TStrings and set "curCtx" to the new context created.
Parameter "filSrc" is a optional reference to a file name, asociated to "lins".}
begin
  curCtx := AddContext;
  {$ifdef debug_mode}
  debugln('  +Nex context from File:'+filSrc);
  {$endif}
  curCtx.SetSource(lins);     //Inicia con archivo contenido en TStrings
  curCtx.fileSrc :=  filSrc;  //Guarda nombre de archivo, solo como referencia.
  //actualiza token actual
  token := curCtx.ReadToken;  //lee el token
  tokType := curCtx.tokType;  //lee atributo
end;
procedure TContexts.RemoveContext;
//Elimina el contexto de entrada actual. Deja apuntando al anterior en la misma posición.
var
  retPos: TContextState;
begin
  if ctxList.Count = 0 then begin
    //No hay contextos abiertos
    curCtx := nil;   //por si acaso
    exit;  //no se puede quitar más
  end;
  {$ifdef debug_mode}
  debugln('  -Context deleted:'+ curCtx.arc);
  {$endif}
  //Hay al menos un contexto abierto
  retPos := curCtx.retPos;  //guarda dirección de retorno
  //ctxList.Delete(ctxList.Count-1);  //elimina contexto superior
  ctxList.Remove(curCtx);
  if ctxList.Count = 0 then begin
    //No quedan contextos abiertos
    curCtx := nil;
  end else begin
    //Queda al menos un contexto anterior
    curCtx := ctxList[retPos.idCtx]; //Recover last context
    SetCtxState(retPos);             //Recover last position
  end;
end;
procedure TContexts.ClearContexts;  //Limpia todos los contextos
begin
  ctxList.Clear;  //Elimina todos los Contextos de entrada
  curCtx := nil;    //Por si acaso
  idCount := 0;   //Inicia contador
end;
//Scan functions
function TContexts.atEol: Boolean;
begin
  exit(tokType = tkEol);
  {Note "curCtx.Eol" won't work correctly, because it respond to "fcol", not
  to "col0".}
end;
function TContexts.atEof: Boolean;
begin
  //A complete verificaction must consider: if nlin = 0 then ..
  exit((curCtx.row0 = curCtx.nlin) and  //Last line
       (curCtx.col0 = curCtx.curSize));  //At EOL
  {Note "curCtx.Eof" won't work correctl, because it respond to (frow, fcol), no to
  (row0, col0).}
end;
procedure TContexts.SkipWhites;
{Salta los blancos incluidos los saltos de línea}
begin
  while atEof or  //Considera también, para poder auto-cerrar contextos
       (curCtx.tokType in [tkSpace, tkEol, tkComment]) do
  begin
      if atEof then begin
        if curCtx.autoClose then begin
          RemoveContext;  //cierra automáticamente
        end else begin
          break;  //Sale del WHILE
        end;
      end;
      if curCtx.Next then begin   //hubo cambio de línea
        if OnNewLine<>nil then OnNewLine(curCtx.CurLine);
      end;
  end;
  //Actualiza token actual
  token := curCtx.ReadToken;    //lee el token
  tokType := curCtx.tokType;  //lee atributo
end;
procedure TContexts.SkipWhitesNoEOL;
{Salta los blancos sin incluir los saltos de línea}
begin
  while not atEof and (curCtx.tokType in [tkSpace, tkComment]) do
  begin
      if curCtx.Next then begin   //hubo cambio de línea
        if OnNewLine<>nil then OnNewLine(curCtx.CurLine);
      end;
  end;
  //actualiza token actual
  token := curCtx.ReadToken;    //lee el token
  tokType := curCtx.tokType;  //lee atributo
end;
procedure TContexts.Next;
begin
  if curCtx.Next then begin   //hubo cambio de línea
    if OnNewLine<>nil then OnNewLine(curCtx.CurLine);
  end;
  if atEof and curCtx.autoClose then begin
    //Se debe cerrar automáticamente
    RemoveContext;
  end;
  //actualiza token actual
  token := curCtx.ReadToken;    //lee el token
  tokType := curCtx.tokType;  //lee atributo
end;
procedure TContexts.GotoEOL;
{Move lexer cursor to the end of the current line.}
begin
  curCtx.setEOL;   //Move fcol to end of the line.
  curCtx.Next;     //Update current token to tkEOL
end;
//function TContexts.tokL: string; inline;
////Devuelve el token actual, ignorando la caja.
//begin
//  Result:=lowercase(token);
//end;
procedure TContexts.ShowContexts;
{Función para depuración. Muestra el contenido de los contextos existentes.}
var ct: TContext;
begin
  debugln('=== Openend contexts ===');
  for ct in ctxList do begin
    debugln('   ' + ct.fileSrc);
  end;
end;
procedure TContexts.ShowCurContInformat;
var
  typStr: string;
begin
  case curCtx.typ of
  TC_ARC: typStr := 'TC_ARC';
  TC_TXT: typStr := 'TC_TXT';
  end;
  debugln('===Current Context ===');
  debugln('  arc=' + curCtx.fileSrc);
  debugln('  typ=%s pos=[%d,%d]', [typStr, curCtx.row, curCtx.col]);
//  debugln('  curlines=' + curCon.curLines.Text);
end;
//Errors and warnings
procedure TContexts.ClearError;
{Limpia la bandera de errores. Tomar en cuenta que solo se debe usar para iniciar el
procesamiento de errores. Limpiar errores en medio de la compilación, podría hacer que
se pierda el rastro de errores anteriores, y que inclusive, la compilación termine sin
error, aún cuando haya generado errores intermedios.
Como norma, se podría decir que solo se debe usar, después de haber proecsado un posible
error anterior.}
begin
  HayError := false;
end;
procedure TContexts.GenInfo(msg: string);
begin
  if OnInfo<>nil then OnInfo(msg);
end;
procedure TContexts.GenWarn(msg: string; const srcPos: TSrcPos);
{Genera un mensaje de advertencia en la posición indicada.}
begin
  if OnWarning<>nil then OnWarning(msg, srcPos);
end;
procedure TContexts.GenWarn(msg: string; const Args: array of const; const srcPos: TSrcPos);
begin
  GenWarn(Format(msg, Args), srcPos);
end;
procedure TContexts.GenWarn(msg: string);
{Genera un mensaje de Advertencia, en la posición actual del contexto. }
var
  srcPos: TSrcPos;
begin
  if curCtx = nil then begin
    srcPos.row := -1;
    srcPos.col := -1;
    srcPos.idCtx := 0;
    GenWarn(msg, srcPos);
  end else begin
    GenWarn(msg, GetSrcPos);
  end;
end;
procedure TContexts.GenWarn(msg: string; const Args: array of const);
{Genera un mensaje de Advertencia, en la posición actual del contexto. }
begin
  GenWarn(Format(msg, Args));
end;
procedure TContexts.GenError(msg: string; const srcPos: TSrcPos);
{Genera un mensaje de error en la posición indicada.}
begin
  //Protección
  if curCtx = nil then begin
    HayError := true;
    exit;
  end;
  if curCtx.FixErrPos then begin
    //El contexto actual, tiene configurado una posición fija para los errores
    msg := curCtx.PreErrorMsg + msg;  //completa mensaje
    if OnError<>nil then OnError(msg, curCtx.PreErrPosit);

  end else begin
    if OnError<>nil then OnError(msg, srcPos);
  end;
  HayError := true;
end;
procedure TContexts.GenError(msg: String; const Args: array of const; const srcPos: TSrcPos);
{Versión con parámetros de GenError.}
begin
  GenError(Format(msg, Args), srcPos);
end;
procedure TContexts.GenError(msg: string);
{Función de acceso rápido para Perr.GenError(). Pasa como posición a la posición
del contexto actual. Realiza la traducción del mensaje también.}
var
  srcPos: TSrcPos;
begin
  if curCtx = nil then begin
    srcPos.row := -1;
    srcPos.col := -1;
    srcPos.idCtx := 0;
    GenError(msg, srcPos);
  end else begin
    GenError(msg, GetSrcPos);
  end;
end;
procedure TContexts.GenError(msg: String; const Args: array of const);
{Genera un mensaje de error eb la posición actual del contexto.}
begin
  GenError(Format(msg, Args));
end;
//Initialization
constructor TContexts.Create;
begin
  ctxList := TContextList.Create(true);  //crea contenedor de Contextos, con control de objetos.
  curCtx := nil;
end;
destructor TContexts.Destroy;
begin
  ctxList.Free;
  inherited Destroy;
end;

end.

