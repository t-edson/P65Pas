{                               SynFacilBasic
Unidad con rutinas básicas de SynFacilSyn.
Incluye la definición de la clase base: TSynFacilSynBase, que es la clase padre
de TSYnFacilSyn.
Además icnluye la definición del tipo "tFaTokContent" y el procesamiento de
expresiones regulares que son usadas por TSynFacilSyn.

                                 Por Tito Hinostroza  02/12/2014 - Lima Perú
}
unit SynFacilBasic;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, Classes, SynEditHighlighter, strutils, Graphics, DOM, LCLIntf,
  LCLProc, SynEditHighlighterFoldBase, SynEditTypes;

type
  ///////// Definiciones para manejo de tokens por contenido ///////////

  //Tipo de expresión regular soportada. Las exp. regulares soportadas son
  //simples. Solo incluyen literales de cadena o listas.
  tFaRegExpType = (
    tregTokPos,   //Posición de token
    tregString,   //Literal de cadena: "casa"
    tregChars,    //Lista de caracteres: [A-Z]
    tregChars01,  //Lista de caracteres: [A-Z]?
    tregChars0_,  //Lista de caracteres: [A-Z]*
    tregChars1_   //Lista de caracteres: [A-Z]+
  );

  //Acciones a ejecutar en las comparaciones
  tFaActionOnMatch = (
    aomNext,    //pasa a la siguiente instrucción
    aomExit,    //termina la exploración
    aomMovePar, //Se mueve a una posición específica
    aomExitpar  //termina la exploración retomando una posición específica.
  );

  //Estructura para almacenar una instrucción de token por contenido
  tFaTokContentInst = record
    Chars    : array[#0..#255] of ByteBool; //caracteres
    Text     : string;             //cadena válida
    tokPos   : integer;  //Cuando se usa posición del token
    expTyp   : tFaRegExpType;      //tipo de expresión
    aMatch   : integer;  //atributo asignado en caso TRUE
    aFail    : integer;  //atributo asignado en caso TRUE
    //Campos para ejecutar instrucciones, cuando No cumple
    actionFail : tFaActionOnMatch;
    destOnFail : integer;  //posición destino
    //Campos para ejecutar instrucciones, cuando cumple
    actionMatch: tFaActionOnMatch;
    destOnMatch: integer;  //posición destino

    posFin     : integer;  //para guardar posición
  end;
  tFaTokContentInstPtr = ^tFaTokContentInst;

  ESynFacilSyn = class(Exception);   //excepción del resaltador

  { tFaTokContent }
  //Estructura para almacenar la descripción de los token por contenido
  tFaTokContent = class
    TokTyp   : integer;        //tipo de token por contenido
    CaseSensitive: boolean;     //Usado para comparación de literales de cadena
    Instrucs : array of tFaTokContentInst;  //Instrucciones del token por contenido
    nInstruc : integer;      //Cantidad de instrucciones
    procedure Clear;
    procedure AddInstruct(exp: string; ifTrue: string = ''; ifFalse: string = '';
      atMatch: integer = - 1; atFail: integer = - 1);
    procedure AddRegEx(exp: string; Complete: boolean=false);
  private
    function AddItem(expTyp: tFaRegExpType; ifMatch, ifFail: string): integer;
    procedure AddOneInstruct(var exp: string; ifTrue: string; ifFalse: string;
      atMatch: integer = -1; atFail: integer = -1);
  end;

  ///////// Definiciones básicas para el resaltador ///////////

  //Identifica si un token es el delimitador inicial
  TFaTypeDelim =(tdNull,     //no es delimitado
                 tdUniLin,   //es delimitador inicial de token delimitado de una línea
                 tdMulLin,   //es delimitador inicial de token delimitado multilínea
                 tdConten1,  //es delimitador inicial de token por contenido 1
                 tdConten2,  //es delimitador inicial de token por contenido 2
                 tdConten3,  //es delimitador inicial de token por contenido 3
                 tdConten4); //es delimitador inicial de token por contenido 4
  //Tipos de coloreado de bloques
  TFaColBlock = (cbNull,     //sin coloreado
                 cbLevel,    //colorea bloques por nivel
                 cbBlock);   //colorea bloques usando el color definido para cada bloque

  TFaProcMetTable = procedure of object;   //Tipo de procedimiento para procesar el token de
                                         //acuerdo al caracter inicial.
  TFaProcRange = procedure of object;      //Procedimiento para procesar en medio de un rango.

  TFaSynBlock = class;   //definición adelantada

  //Descripción de tokens especiales (identificador o símbolo)
  TTokSpec = record
    txt   : string;        //palabra clave (puede cambiar la caja y no incluir el primer caracter)
    orig  : string;        //palabra clave tal cual se indica
    TokPos: integer;       //posición del token dentro de la línea
    tTok  : integer;       //tipo de token
    typDel: TFaTypeDelim;  {indica si el token especial actual, es en realidad, el
                            delimitador inicial de un token delimitado o por contenido}
    dEnd  : string;        //delimitador final (en caso de que sea delimitador)
    pRange: TFaProcRange;  //procedimiento para procesar el token o rango(si es multilinea)
    folTok: boolean;       //indica si el token delimitado, tiene plegado
    chrEsc: char;          //Caracter de escape de token delimitado. Si no se usa, contiene #0.
    //propiedades para manejo de bloques y plegado de código
    openBlk   : boolean;      //indica si el token es inicio de bloque de plegado
    BlksToOpen: array of TFaSynBlock;  //lista de referencias a los bloques que abre
    closeBlk  : boolean;      //indica si el token es fin de bloque de plegado
    BlksToClose: array of TFaSynBlock; //lista de referencias a los bloques que cierra
    OpenSec   : boolean;      //indica si el token es inicio de sección de bloque
    SecsToOpen: array of TFaSynBlock;  //lista de bloques de los que es inicio de sección
    firstSec  : TFaSynBlock; //sección que se debe abrir al abrir el bloque
  end;

  TEvBlockOnOpen = procedure(blk: TFaSynBlock; var Cancel: boolean) of object;

  TArrayTokSpec = array of TTokSpec;
  //clase para manejar la definición de bloques de sintaxis
  TFaSynBlock = class
    name        : string;    //nombre del bloque
    index       : integer;   //indica su posición dentro de TFaListBlocks
    showFold    : boolean;   //indica si se mostrará la marca de plegado
    parentBlk   : TFaSynBlock; //bloque padre (donde es válido el bloque)
    BackCol     : TColor;    //color de fondo de un bloque
    IsSection   : boolean;   //indica si es un bloque de tipo sección
    UniqSec     : boolean;   //índica que es sección única
    CloseParent : boolean;   //indica que debe cerrar al blqoue padre al cerrarse
    OnBeforeOpen      : TEvBlockOnOpen;  //evento de apertura de bloque
    OnBeforeClose     : TEvBlockOnOpen;  //evento de cierre de bloque
  end;

  TPtrATokEspec = ^TArrayTokSpec;     //puntero a tabla
  TPtrTokEspec = ^TTokSpec;     //puntero a tabla

  //Guarda información sobre un atributo de un nodo XML
  TFaXMLatrib = record  //atributo XML
    hay: boolean;    //bandera de existencia
    val: string;     //valor en cadena
    n  : integer;    //valor numérico
    bol: boolean;    //valor booleando (si aplica)
    col: TColor;     //valor de color (si aplica)
  end;

  { TSynFacilSynBase }
  //Clase con métodos básicos para el resaltador
  TSynFacilSynBase = class(TSynCustomFoldHighlighter)
  protected
    fLine      : PChar;         //Puntero a línea de trabajo
    tamLin     : integer;       //Tamaño de línea actual
    fProcTable : array[#0..#255] of TFaProcMetTable;   //tabla de métodos
    fAtriTable : array[#0..#255] of integer;   //tabla de atributos de tokens
    posIni     : Integer;       //índice a inicio de token
    posFin     : Integer;       //índice a siguiente token
    fStringLen : Integer;       //Tamaño del token actual
    fToIdent   : PChar;         //Puntero a identificador
    fTokenID   : integer;      //Id del token actual
    charIni    : char;          //caracter al que apunta fLine[posFin]
    posTok     : integer;       //para identificar el ordinal del token en una línea

    CaseSensitive: boolean;     //Para ignorar mayúscula/minúscula
    charsIniIden: Set of char;  //caracteres iniciales de identificador
    lisTmp     : TStringList;   //lista temporal
    fSampleSource: string;      //código de muestra
    function GetSampleSource: String; override;
  protected   //identificadores especiales
    CharsIdentif: array[#0..#255] of ByteBool; //caracteres válidos para identificadores
    tc1, tc2, tc3, tc4: tFaTokContent;
    //Tablas para identificadores especiales
    mA, mB, mC, mD, mE, mF, mG, mH, mI, mJ,
    mK, mL, mM, mN, mO, mP, mQ, mR, mS, mT,
    mU, mV, mW, mX, mY, mZ:  TArrayTokSpec;  //para mayúsculas
    mA_,mB_,mC_,mD_,mE_,mF_,mG_,mH_,mI_,mJ_,
    mK_,mL_,mM_,mN_,mO_,mP_,mQ_,mR_,mS_,mT_,
    mU_,mV_,mW_,mX_,mY_,mZ_:  TArrayTokSpec;  //para minúsculas
    m_, mDol, mArr, mPer, mAmp, mC3 : TArrayTokSpec;
    mSym        :  TArrayTokSpec;   //tabla de símbolos especiales
    mSym0       :  TArrayTokSpec;   //tabla temporal para símbolos especiales.
    TabMayusc   : array[#0..#255] of Char;     //Tabla para conversiones rápidas a mayúscula
  protected  //funciones básicas
    function BuscTokEspec(var mat: TArrayTokSpec; cad: string; out n: integer;
      TokPos: integer = 0): boolean;
    function ToListRegex(list: TFaXMLatrib): string;
    function dStartRegex(tStart, tCharsStart: TFaXMLatrib): string;
    procedure VerifDelim(delim: string);
    procedure ValidAsigDelim(delAct, delNue: TFaTypeDelim; delim: string);
    procedure ValidateParamStart(Start: string; var ListElem: TStringList);
    function KeyComp(var r: TTokSpec): Boolean;
    function CreaBuscTokEspec(var mat: TArrayTokSpec; cad: string; out i: integer;
      TokPos: integer = 0): boolean;
    //procesamiento de XML
    procedure CheckXMLParams(n: TDOMNode; listAtrib: string);
    function ReadXMLParam(n: TDOMNode; nomb: string): TFaXMLatrib;
  protected   //Métodos para tokens por contenido
    procedure metTokCont(const tc: tFaTokContent); //inline;
    procedure metTokCont1;
    procedure metTokCont2;
    procedure metTokCont3;
    procedure metTokCont4;
  protected  //Procesamiento de otros elementos
    procedure metIdent;
    procedure metIdentUTF8;
    procedure metNull;
    procedure metSpace;
    procedure metSymbol;
  public     //Funciones públicas
    procedure DefTokIdentif(dStart, Content: string );
  public     //Atributos y sus propiedades de acceso
    //Atributos predefinidos
    tkEol     : TSynHighlighterAttributes;
    tkSymbol  : TSynHighlighterAttributes;
    tkSpace   : TSynHighlighterAttributes;
    tkIdentif : TSynHighlighterAttributes;
    tkNumber  : TSynHighlighterAttributes;
    tkKeyword : TSynHighlighterAttributes;
    tkString  : TSynHighlighterAttributes;
    tkComment : TSynHighlighterAttributes;
    //ID para los tokens
    tnEol     : integer;  //id para los tokens salto de línea
    tnSymbol  : integer;  //id para los símbolos
    tnSpace   : integer;  //id para los espacios
    tnIdentif : integer;  //id para los identificadores
    tnNumber  : integer;  //id para los números
    tnKeyword : integer;  //id para las palabras claves
    tnString  : integer;  //id para las cadenas
    tnComment : integer;  //id para los comentarios
    {Se crea el contenedor adicional Attrib[], para los atributos, porque aunque ya se
    tiene Attribute[] en TSynCustomHighlighter, este está ordenado pro defecto y no
    ayuda en ubicar a los attributos por su índice}
    Attrib: array of TSynHighlighterAttributes;
    function NewTokAttrib(TypeName: string; out TokID: integer
      ): TSynHighlighterAttributes;
    function NewTokType(TypeName: string; out TokAttrib: TSynHighlighterAttributes
      ): integer;
    function NewTokType(TypeName: string): integer;
    procedure CreateAttributes;  //limpia todos loa atributos
    function GetAttribByName(txt: string): TSynHighlighterAttributes;
    function GetAttribIDByName(txt: string): integer;
    function IsAttributeName(txt: string): boolean;
    protected
    function ProcXMLattribute(nodo: TDOMNode): boolean;
  public //Inicializacoón
    constructor Create(AOwner: TComponent); override;
  end;

function ExtractRegExp(var exp: string; out str: string; out listChars: string): tFaRegExpType;
function ExtractRegExpN(var exp: string; out RegexTyp: tFaRegExpType ): string;
function ReplaceEscape(str: string): string;
function ColorFromStr(cad: string): TColor;
implementation
const
    //Mensajes de error generales
//    ERR_START_NO_EMPTY = 'Parámetro "Start" No puede ser nulo';
//    ERR_EXP_MUST_BE_BR = 'Expresión debe ser de tipo [lista de caracteres]';
//    ERR_TOK_DELIM_NULL = 'Delimitador de token no puede ser nulo';
//    ERR_NOT_USE_START = 'No se puede usar "Start" y "CharsStart" simultáneamente.';
//    ERR_PAR_START_CHARS = 'Se debe definir el parámetro "Start" o "CharsStart".';
//    ERR_TOK_DEL_IDE_ERR = 'Delimitador de token erróneo: %s (debe ser identificador)';
//    ERR_IDEN_ALREA_DEL = 'Identificador "%s" ya es delimitador inicial.';
//    ERR_INVAL_ATTR_LAB = 'Atributo "%s" no válido para etiqueta <%s>';
//    ERR_BAD_PAR_STR_IDEN = 'Parámetro "Start" debe ser de la forma: "[A-Z]", en identificadores';
//    ERR_BAD_PAR_CON_IDEN = 'Parámetro "Content" debe ser de la forma: "[A-Z]*", en identificadores';

    ERR_START_NO_EMPTY = 'Parameter "Start" can not be null';
    ERR_EXP_MUST_BE_BR = 'Expression must be like: [list of chars]';
    ERR_TOK_DELIM_NULL = 'Token delimiter can not be null';
    ERR_NOT_USE_START = 'Cannot use "Start" and "CharsStart" simultaneously.';
    ERR_PAR_START_CHARS = 'It must be defined "Start" or "CharsStart" parameter.';
    ERR_TOK_DEL_IDE_ERR = 'Bad Token delimiter: %s (must be identifier)';
    ERR_IDEN_ALREA_DEL = 'Identifier "%s" is already a Start delimiter.';
    ERR_INVAL_ATTR_LAB = 'Invalid attribute "%s" for label <%s>';
    ERR_BAD_PAR_STR_IDEN = 'Parameter "Start" must be like: "[A-Z]", in identifiers';
    ERR_BAD_PAR_CON_IDEN = 'Parameter "Content" must be like: "[A-Z]*", in identifiers';

    //Mensajes de tokens por contenido
//    ERR_EMPTY_INTERVAL = 'Error: Intervalo vacío.';
//    ERR_EMPTY_EXPRES = 'Expresión vacía.';
//    ERR_EXPECTED_BRACK = 'Se esperaba "]".';
//    ERR_UNSUPPOR_EXP_ = 'Expresión no soportada.';
//    ERR_INC_ESCAPE_SEQ = 'Secuencia de escape incompleta.';
//    ERR_SYN_PAR_IFFAIL_ = 'Error de sintaxis en parámetro "IfFail": ';
//    ERR_SYN_PAR_IFMATCH_ = 'Error de sintaxis en parámetro "IfMarch": ';
    ERR_EMPTY_INTERVAL = 'Error: Empty Interval.';
    ERR_EMPTY_EXPRES = 'Empty expression.';
    ERR_EXPECTED_BRACK = 'Expected "]".';
    ERR_UNSUPPOR_EXP_ = 'Unsupported expression: ';
    ERR_INC_ESCAPE_SEQ = 'Incomplete Escape sequence';
    ERR_SYN_PAR_IFFAIL_ = 'Syntax error on Parameter "IfFail": ';
    ERR_SYN_PAR_IFMATCH_ = 'Syntax error on Parameter "IfMarch": ';

var
  bajos: string[128];
  altos: string[128];

function copyEx(txt: string; p: integer): string;
//Versión sobrecargada de copy con 2 parámetros
begin
  Result := copy(txt, p, length(txt));
end;
//Funciones para el manejo de expresiones regulares
function ExtractChar(var txt: string; out escaped: boolean; convert: boolean): string;
//Extrae un caracter de una expresión regular. Si el caracter es escapado, devuelve
//TRUE en "escaped"
//Si covert = TRUE, reemplaza el caracter compuesto por uno solo.
var
  c: byte;
begin
  escaped := false;
  Result := '';   //valor por defecto
  if txt = '' then exit;
  if txt[1] = '\' then begin  //caracter escapado
    escaped := true;
    if length(txt) = 1 then  //verificación
      raise ESynFacilSyn.Create(ERR_INC_ESCAPE_SEQ);
    if txt[2] in ['x','X'] then begin
      //caracter en hexadecimal
      if length(txt) < 4 then  //verificación
        raise ESynFacilSyn.Create(ERR_INC_ESCAPE_SEQ);
      if convert then begin    //toma caracter hexdecimal
        c := StrToInt('$'+copy(txt,3,2));
        Result := Chr(c);
      end else begin  //no tranforma
        Result := copy(txt, 1,4);
      end;
      txt := copyEx(txt,5);
    end else begin //se supone que es de tipo \A
      //secuencia normal de dos caracteres
      if convert then begin  //hay que convertirlo
        Result := txt[2];
      end else begin  //lo toma tal cual
        Result := copy(txt,1,2);
      end;
      txt := copyEx(txt,3);
    end;
  end else begin   //caracter normal
    Result := txt[1];
    txt := copyEx(txt,2);
  end;
end;
function ExtractChar(var txt: string): char;
//Versión simplificada de ExtractChar(). Extrae un caracter ya convertido. Si no hay
//más caracteres, devuelve #0
var
  escaped: boolean;
  tmp: String;
begin
  if txt = '' then Result := #0
  else begin
    tmp := ExtractChar(txt, escaped, true);
    Result := tmp[1];  //se supone que siempre será de un solo caracter
  end;
end;
function ExtractCharN(var txt: string): string;
//Versión simplificada de ExtractChar(). Extrae un caracter sin convertir.
var
  escaped: boolean;
begin
  Result := ExtractChar(txt, escaped, false);
end;
function ReplaceEscape(str: string): string;
{Reemplaza las secuencias de escape por su caracter real. Las secuencias de
escape recnocidas son:
* Secuencia de 2 caracteres: "\#", donde # es un caracter cualquiera, excepto"x".
  Esta secuencia equivale al caracter "#".
* Secuencia de 4 caracteres: "\xHH" o "\XHH", donde "HH" es un número hexadecimnal.
  Esta secuencia representa a un caracter ASCII.

Dentro de las expresiones regulares de esta librería, los caracteres: "[", "*", "?",
"*", y "\", tienen significado especial, por eso deben "escaparse".

"\\" -> "\"
"\[" -> "["
"\*" -> "*"
"\?" -> "?"
"\+" -> "+"
"\x$$" -> caracter ASCII $$
}
begin
  Result := '';
  while str<>'' do
    Result += ExtractChar(str);
end;
function EscapeText(str: string): string;
//Comvierte los caracteres que pueden tener significado especial en secuencias de
//escape para que se procesen como caracteres normales.
begin
  str := StringReplace(str, '\', '\\',[rfReplaceAll]);  //debe hacerse primero
  str := StringReplace(str, '[', '\[',[rfReplaceAll]);
  str := StringReplace(str, '*', '\*',[rfReplaceAll]);
  str := StringReplace(str, '?', '\?',[rfReplaceAll]);
  str := StringReplace(str, '+', '\+',[rfReplaceAll]);
  Result := str;
end;
function PosChar(ch: char; txt: string): integer;
//Similar a Pos(). Devuelve la posición de un caracter que no este "escapado"
var
  f: SizeInt;
begin
  f := Pos(ch,txt);
  if f=1 then exit(1);   //no hay ningún caracter antes.
  while (f>0) and (txt[f-1]='\') do begin
    f := PosEx(ch, txt, f+1);
  end;
  Result := f;
end;
function ExtractRegExp(var exp: string; out str: string; out listChars: string): tFaRegExpType;
{Extrae parte de una expresión regular y devuelve el tipo. Esta función se basa en
que toda expresión regular se puede reducir a literales de cadenas o listas (con o
sin cuantificador).
En los casos de listas de caracteres, expande los intervalos de tipo: A..Z, reemplaza
las secuencias de escape y devuelve la lista en "listChars".
En el caso de que sea un literal de cadena, reemplaza las secuencias de escape y
devuelve la cadena en "str".
Soporta todas las formas definidas en "tFaRegExpType".
Si encuentra error, genera una excepción.}
  procedure ValidateInterval(var cars: string);
  {Valida un conjunto de caracteres, expandiendo los intervalos de tipo "A-Z", y
  remplazando las secuencias de escape como: "\[", "\\", "\-", ...
  El caracter "-", se considera como indicador de intervalo, a menos que se encuentre
  en el primer o ùltimo caracter de la cadena, o esté escapado.
  Si hay error genera una excepción.}
  var
    c, car1, car2: char;
    car: string;
    tmp: String;
    Invert: Boolean;
    carsSet: set of char;
  begin
    //reemplaza intervalos
    if cars = '' then
      raise ESynFacilSyn.Create(ERR_EMPTY_INTERVAL);
    //Verifica si es lista invertida
    Invert := false;
    if cars[1] = '^' then begin
      Invert := true;        //marca
      cars := copyEx(cars,2);  //quita "^"
    end;
    //Procesa contenido, reemplazando los caracteres escapados.
    //Si el primer caracter es "-". lo toma literal, sin asumir error.
    car1 := ExtractChar(cars);   //Extrae caracter convertido. Se asume que es inicio de intervalo.
    tmp := car1;  //inicia cadena para acumular.
    car := ExtractCharN(cars);   //Eextrae siguiente. Sin convertir porque puede ser "\-"
    while car<>'' do begin
      if car = '-' then begin
        //es intervalo
        car2 := ExtractChar(cars);   //caracter final
        if car2 = #0 then begin
          //Es intervalo incompleto, podría genera error, pero mejor asumimos que es el caracter "-"
          tmp += '-';
          break;  //sale por que se supone que ya no hay más caracteres
        end;
        //se tiene un intervalo que hay que reemplazar
        for c := Chr(Ord(car1)+1) to car2 do  //No se incluye "car1", porque ya se agregó
          tmp += c;
      end else begin  //simplemente acumula
        car1 := ExtractChar(car);   //Se asume que es inicio de intervalo. No importa perder "car"
        tmp += car1;  //Es necesario, porque puede estar escapado
      end;
      car := ExtractCharN(cars);  //extrae siguiente
    end;
    cars := StringReplace(tmp, '%HIGH%', altos,[rfReplaceAll]);
    cars := StringReplace(cars, '%ALL%', bajos+altos,[rfReplaceAll]);
    //Verifica si debe invertir lista
    if Invert then begin
      //Convierte a conjunto
      carsSet := [];
      for c in cars do carsSet += [c];
      //Agrega caracteres
      cars := '';
      for c := #1 to #255 do  //no considera #0
        if not (c in carsSet) then cars += c;
    end;
  end;
var
  tmp: string;
  lastAd: String;
begin
  if exp= '' then
    raise ESynFacilSyn.Create(ERR_EMPTY_EXPRES);
  //Verifica la forma TokPos=1
  if UpCase(copy(exp,1,7)) = 'TOKPOS=' then begin
    //Caso especial de la forma TokPos=n
    str := copy(exp,8,2);  //Aquí se devuelve "n"
    exp := '';    //ya no quedan caracteres
    Result := tregTokPos;
    exit;
  end;
  //Reemplaza secuencias conocidas que equivalen a listas.
  if copy(exp,1,2) = '\d' then begin
    exp := '[0-9]' + copyEx(exp,3);
  end else if copy(exp,1,2) = '\D' then begin
    exp := '[^0-9]' + copyEx(exp,3);
  end else if copy(exp,1,2) = '\a' then begin
    exp := '[A-Za-z]' + copyEx(exp,3);
  end else if copy(exp,1,2) = '\w' then begin
    exp := '[A-Za-z0-9_]' + copyEx(exp,3);
  end else if copy(exp,1,2) = '\W' then begin
    exp := '[^A-Za-z0-9_]' + copyEx(exp,3);
  end else if copy(exp,1,2) = '\s' then begin
    exp := ' ' + copyEx(exp,3);
  end else if copy(exp,1,2) = '\S' then begin
    exp := '[^ ]' + copyEx(exp,3);
  end else if copy(exp,1,2) = '\t' then begin
    exp := '\x09' + copyEx(exp,3);
  end else if copy(exp,1,1) = '.' then begin
    exp := '[\x01-\xFF]' + copyEx(exp,2);
  end;
  //analiza la secuencia
  if (exp[1] = '[') and (length(exp)>1) then begin    //Es lista de caracteres
    //Captura interior del intervalo.
    exp := CopyEx(exp,2);
    listChars := '';
    tmp := ExtractCharN(exp);   //No convierte para no confundir "\]"
    while (exp<>'') and (tmp<>']') do begin
      listChars += tmp;
      tmp := ExtractCharN(exp);  //No convierte para no confundir "\]"
    end;
    if (tmp<>']') then   //no se encontró ']'
      raise ESynFacilSyn.Create(ERR_EXPECTED_BRACK);
    //la norma es tener aquí, el contenido de la lista, pero manteniendo los caracteres escapados
    ValidateInterval(listChars);  //puede simplificar "listChars". También puede generar excepción
    if exp = '' then begin   //Lista de tipo "[ ... ]"
      Result := tregChars;
    end else if exp[1] = '*' then begin  //Lista de tipo "[ ... ]* ... "
      exp := copyEx(exp,2);    //extrae parte procesada
      Result := tregChars0_
    end else if exp[1] = '?' then begin  //Lista de tipo "[ ... ]? ... "
      exp := copyEx(exp,2);    //extrae parte procesada
      Result := tregChars01
    end else if exp[1] = '+' then begin  //Lista de tipo "[ ... ]+ ... "
      exp := copyEx(exp,2);    //extrae parte procesada
      Result := tregChars1_
    end else begin
      //No sigue ningún cuantificador, podrías er algún literal
      Result := tregChars;  //Lista de tipo "[ ... ] ... "
    end;
  end else if (length(exp)=1) and (exp[1] in ['*','?','+','[']) then begin
    //Caso especial, no se usa escape, pero no es lista, ni cuantificador. Se asume
    //caracter único
    listChars := exp;  //'['+exp+']'
    exp := '';    //ya no quedan caracteres
    Result := tregChars;
    exit;
  end else begin
    //No inicia con lista. Se puede suponer que inicia con literal cadena.
    {Pueden ser los casos:
      Caso 0) "abc"    (solo literal cadena, se extraerá la cadena "abc")
      Caso 1) "abc[ ... "  (válido, se extraerá la cadena "abc")
      Caso 2) "a\[bc[ ... " (válido, se extraerá la cadena "a[bc")
      Caso 3) "abc* ... "  (válido, pero se debe procesar primero "ab")
      Caso 4) "ab\\+ ... " (válido, pero se debe procesar primero "ab")
      Caso 5) "a? ... "    (válido, pero debe transformarse en lista)
      Caso 6) "\[* ... "   (válido, pero debe transformarse en lista)
    }
    str := '';   //para acumular
    tmp := ExtractCharN(exp);
    lastAd := '';   //solo por seguridad
    while tmp<>'' do begin
      if tmp = '[' then begin
        //Empieza una lista. Caso 1 o 2
        exp:= '[' + exp;  //devuelve el caracter
        str := ReplaceEscape(str);
{        if length(str) = 1 then begin  //verifica si tiene un caracter
          listChars := str;       //'['+str+']'
          Result := tregChars;   //devuelve como lista de un caracter
          exit;
        end;}
        Result := tregString;   //es literal cadena
        exit;  //sale con lo acumulado en "str"
      end else if (tmp = '*') or (tmp = '?') or (tmp = '+') then begin
        str := copy(str, 1, length(str)-length(lastAd)); //no considera el último caracter
        if str <> '' then begin
          //Hay literal cadena, antes de caracter y cuantificador. Caso 3 o 4
          exp:= lastAd + tmp + exp;  //devuelve el último caracter agregado y el cuantificador
          str := ReplaceEscape(str);
          if length(str) = 1 then begin  //verifica si tiene un caracter
            listChars := str;       //'['+str+']'
            Result := tregChars;   //devuelve como lista de un caracter
            exit;
          end;
          Result := tregString;   //es literal cadena
          exit;
        end else begin
          //Hay caracter y cuantificador. . Caso 5 o 6
          listChars := ReplaceEscape(lastAd);  //'['+lastAd+']'
          //de "exp" ya se quitó: <caracter><cuantificador>
          if          tmp = '*' then begin  //Lista de tipo "[a]* ... "
            Result := tregChars0_
          end else if tmp = '?' then begin  //Lista de tipo "[a]? ... "
            Result := tregChars01
          end else if tmp = '+' then begin  //Lista de tipo "[a]+ ... "
            Result := tregChars1_
          end;   //no hay otra opción
          exit;
        end;
      end;
      str += tmp;   //agrega caracter
      lastAd := tmp;  //guarda el último caracter agregado
      tmp := ExtractCharN(exp);  //siguiente caracter
    end;
    //Si llega aquí es porque no encontró cuantificador ni lista (Caso 0)
    str := ReplaceEscape(str);
{    if length(str) = 1 then begin  //verifica si tiene un caracter
      listChars := str;       //'['+str+']'
      Result := tregChars;   //devuelve como lista de un caracter
      exit;
    end;}
    Result := tregString;
  end;
end;
function ExtractRegExpN(var exp: string; out RegexTyp: tFaRegExpType): string;
{Extrae parte de una expresión regular y la devuelve como cadena . Actualiza el
tipo de expresión obtenida en "RegexTyp".
No Reemplaza las secuencias de excape ni los intervalos, devuelve el texto tal cual}
var
  listChars, str: string;
  exp0: String;
  tam: Integer;
begin
  exp0 := exp;   //guarda expresión tal cual
  RegexTyp := ExtractRegExp(exp, str, listChars);
  tam := length(exp0) - length(exp);  //ve diferencia de tamaño
  Result := copy(exp0, 1, tam)
end;
function ColorFromStr(cad: string): TColor;
//Convierte una cadena a Color
  function EsHexa(txt: string; out num: integer): boolean;
  //Convierte un texto en un número entero. Si es numérico devuelve TRUE
  var i: integer;
  begin
    Result := true;  //valor por defecto
    num := 0; //valor por defecto
    for i:=1 to length(txt) do begin
      if not (txt[i] in ['0'..'9','a'..'f','A'..'F']) then exit(false);  //no era
    end;
    //todos los dígitos son numéricos
    num := StrToInt('$'+txt);
  end;
var
  r, g, b: integer;
begin
  if (cad<>'') and (cad[1] = '#') and (length(cad)=7) then begin
    //es código de color. Lo lee de la mejor forma
    EsHexa(copy(cad,2,2),r);
    EsHexa(copy(cad,4,2),g);
    EsHexa(copy(cad,6,2),b);
    Result:=RGB(r,g,b);
  end else begin  //constantes de color
    case UpCase(cad) of
    'WHITE'      : Result :=rgb($FF,$FF,$FF);
    'SILVER'     : Result :=rgb($C0,$C0,$C0);
    'GRAY'       : Result :=rgb($80,$80,$80);
    'BLACK'      : Result :=rgb($00,$00,$00);
    'RED'        : Result :=rgb($FF,$00,$00);
    'MAROON'     : Result :=rgb($80,$00,$00);
    'YELLOW'     : Result :=rgb($FF,$FF,$00);
    'OLIVE'      : Result :=rgb($80,$80,$00);
    'LIME'       : Result :=rgb($00,$FF,$00);
    'GREEN'      : Result :=rgb($00,$80,$00);
    'AQUA'       : Result :=rgb($00,$FF,$FF);
    'TEAL'       : Result :=rgb($00,$80,$80);
    'BLUE'       : Result :=rgb($00,$00,$FF);
    'NAVY'       : Result :=rgb($00,$00,$80);
    'FUCHSIA'    : Result :=rgb($FF,$00,$FF);
    'PURPLE'     : Result :=rgb($80,$00,$80);

    'MAGENTA'    : Result :=rgb($FF,$00,$FF);
    'CYAN'       : Result :=rgb($00,$FF,$FF);
    'BLUE VIOLET': Result :=rgb($8A,$2B,$E2);
    'GOLD'       : Result :=rgb($FF,$D7,$00);
    'BROWN'      : Result :=rgb($A5,$2A,$2A);
    'CORAL'      : Result :=rgb($FF,$7F,$50);
    'VIOLET'     : Result :=rgb($EE,$82,$EE);
    end;
  end;
end;

{ tFaTokContent }
procedure tFaTokContent.Clear;
begin
  CaseSensitive := false;   //por defecto
  nInstruc := 0;
  setLength(Instrucs,0);
end;
function tFaTokContent.AddItem(expTyp: tFaRegExpType; ifMatch, ifFail: string): integer;
//Agrega un ítem a la lista Instrucs[]. Devuelve el número de ítems.
//Configura el comportamiento de la instrucción usando "ifMatch".
var
  ifMatch0, ifFail0: string;

  function extractIns(var txt: string): string;
  //Extrae una instrucción (identificador)
  var
    p: Integer;
  begin
    txt := trim(txt);
    if txt = '' then exit('');
    p := 1;
    while (p<=length(txt)) and (txt[p] in ['A'..'Z']) do inc(p);
    Result := copy(txt,1,p-1);
    txt := copyEx(txt, p);
//    Result := copy(txt,1,p);
//    txt := copyEx(txt, p+1);
  end;
  function extractPar(var txt: string; errMsg: string): integer;
  //Extrae un valor numérico
  var
    p, p0: Integer;
    sign: Integer;
  begin
    txt := trim(txt);
    if txt = '' then exit(0);
    if txt[1] = '(' then begin
      //caso esperado
      p := 2;  //explora
      if not (txt[2] in ['+','-','0'..'9']) then  //validación
        raise ESynFacilSyn.Create(errMsg + ifFail0);
      sign := 1;  //signo por defecto
      if txt[2] = '+' then begin
        p := 3;  //siguiente caracter
        sign := 1;
        if not (txt[3] in ['0'..'9']) then
          raise ESynFacilSyn.Create(errMsg + ifFail0);
      end;
      if txt[2] = '-' then begin
        p := 3;  //siguiente caracter
        sign := -1;
        if not (txt[3] in ['0'..'9']) then
          raise ESynFacilSyn.Create(errMsg + ifFail0);
      end;
      //Aquí se sabe que en txt[p], viene un númaro
      p0 := p;   //guarda posición de inicio
      while (p<=length(txt)) and (txt[p] in ['0'..'9']) do inc(p);
      Result := StrToInt(copy(txt,p0,p-p0)) * Sign;  //lee como número
      if txt[p]<>')' then raise ESynFacilSyn.Create(errMsg + ifFail0);
      inc(p);
      txt := copyEx(txt, p+1);
    end else begin
      raise ESynFacilSyn.Create(errMsg + ifFail0);
    end;
  end;
  function HavePar(var txt: string): boolean;
  //Verifica si la cadena empieza con "("
  begin
    Result := false;
    txt := trim(txt);
    if txt = '' then exit;
    if txt[1] = '(' then begin   //caso esperado
      Result := true;
    end;
  end;

var
  inst: String;
  n: Integer;
begin
  ifMatch0 := ifMatch;  //guarda valor original
  ifFail0 := ifFail;    //guarda valor original
  inc(nInstruc);
  n := nInstruc-1;  //último índice
  setlength(Instrucs, nInstruc);
  Instrucs[n].expTyp := expTyp;    //tipo
  Instrucs[n].actionMatch := aomNext;  //valor por defecto
  Instrucs[n].actionFail  := aomExit; //valor por defecto
  Instrucs[n].destOnMatch:=0;         //valor por defecto
  Instrucs[n].destOnFail:= 0;         //valor por defecto
  Result := nInstruc;
  //Configura comportamiento
  if ifMatch<>'' then begin
    ifMatch := UpCase(ifMatch);
    while ifMatch<>'' do begin
      inst := extractIns(ifMatch);
      if inst = 'NEXT' then begin  //se pide avanzar al siguiente
        Instrucs[n].actionMatch := aomNext;
      end else if inst = 'EXIT' then begin  //se pide salir
        if HavePar(ifMatch) then begin  //EXIT con parámetro
          Instrucs[n].actionMatch := aomExitpar;
          Instrucs[n].destOnMatch := n + extractPar(ifMatch, ERR_SYN_PAR_IFMATCH_);
        end else begin   //EXIT sin parámetros
          Instrucs[n].actionMatch := aomExit;
        end;
      end else if inst = 'MOVE' then begin
        Instrucs[n].actionMatch := aomMovePar;  //Mover a una posición
        Instrucs[n].destOnMatch := n + extractPar(ifMatch, ERR_SYN_PAR_IFMATCH_);
      end else begin
        raise ESynFacilSyn.Create(ERR_SYN_PAR_IFMATCH_ + ifMatch0);
      end;
      ifMatch := Trim(ifMatch);
      if (ifMatch<>'') and (ifMatch[1] = ';') then  //quita delimitador
        ifMatch := copyEx(ifMatch,2);
    end;
  end;
  if ifFail<>'' then begin
    ifFail := UpCase(ifFail);
    while ifFail<>'' do begin
      inst := extractIns(ifFail);
      if inst = 'NEXT' then begin  //se pide avanzar al siguiente
        Instrucs[n].actionFail := aomNext;
      end else if inst = 'EXIT' then begin  //se pide salir
        if HavePar(ifFail) then begin  //EXIT con parámetro
          Instrucs[n].actionFail := aomExitpar;
          Instrucs[n].destOnFail := n + extractPar(ifFail, ERR_SYN_PAR_IFFAIL_);
        end else begin   //EXIT sin parámetros
          Instrucs[n].actionFail := aomExit;
        end;
      end else if inst = 'MOVE' then begin
        Instrucs[n].actionFail := aomMovePar;  //Mover a una posición
        Instrucs[n].destOnFail := n + extractPar(ifFail, ERR_SYN_PAR_IFFAIL_);
      end else begin
        raise ESynFacilSyn.Create(ERR_SYN_PAR_IFFAIL_ + ifFail0);
      end;
      ifFail := Trim(ifFail);
      if (ifFail<>'') and (ifFail[1] = ';') then  //quita delimitador
        ifFail := copyEx(ifFail,2);
    end;
  end;
end;
procedure tFaTokContent.AddOneInstruct(var exp: string; ifTrue: string; ifFalse: string;
      atMatch: integer=-1; atFail: integer=-1);
{Agrega una y solo instrucción al token por contenido. Si encuentra más de una
instrucción, genera una excepción. Si se pone ifTrue en blnnco, se asumirá 'next',
si se pone "ifFalse" en blanco, se se asumirá 'exit'.
Este es el punto de entrada único para agregar una instrucción de Regex a
tFaTokContent}
var
  list: String;
  str: string;
  n: Integer;
  c: Char;
  expr: string;
  t: tFaRegExpType;
begin
  if exp='' then exit;
  //analiza
  expr := exp;   //guarda, porque se va a trozar
  t := ExtractRegExp(exp, str, list);
  case t of
  tregChars,    //Es de tipo lista de caracteres [...]
  tregChars01,  //Es de tipo lista de caracteres [...]?
  tregChars0_,  //Es de tipo lista de caracteres [...]*
  tregChars1_:  //Es de tipo lista de caracteres [...]+
    begin
      n := AddItem(t, ifTrue, ifFalse)-1;  //agrega
      Instrucs[n].aMatch:= atMatch;
      Instrucs[n].aFail := atFail;
      //Configura caracteres de contenido
      for c := #0 to #255 do Instrucs[n].Chars[c] := False;
      for c in list do Instrucs[n].Chars[c] := True;
    end;
  tregString: begin      //Es de tipo texto literal
      n := AddItem(t, ifTrue, ifFalse)-1;  //agrega
      Instrucs[n].aMatch:= atMatch;
      Instrucs[n].aFail := atFail;
      //configura cadena
      if CaseSensitive then Instrucs[n].Text := str
      else Instrucs[n].Text := UpCase(str);  //ignora caja
    end;
  tregTokPos: begin
      n := AddItem(t, ifTrue, ifFalse)-1;  //agrega
      Instrucs[n].aMatch:= atMatch;
      Instrucs[n].aFail := atFail;
      //configura cadena
      Instrucs[n].tokPos:= StrToInt(str);  //Orden de token
    end;
  else
    raise ESynFacilSyn.Create(ERR_UNSUPPOR_EXP_ + expr);
  end;
end;
procedure tFaTokContent.AddInstruct(exp: string; ifTrue: string=''; ifFalse: string='';
      atMatch: integer=-1; atFail: integer=-1);
//Agrega una instrucción para el procesamiento del token por contenido.
//Solo se debe indicar una instrucción, de otra forma se generará un error.
var
  expr: String;
begin
  expr := exp;   //guarda, porque se va a trozar
  AddOneInstruct(exp, ifTrue, ifFalse, atMatch, atFail);  //si hay error genera excepción
  //Si llegó aquí es porque se obtuvo una expresión válida, pero la
  //expresión continua.
  if exp<>'' then begin
    raise ESynFacilSyn.Create(ERR_UNSUPPOR_EXP_ + expr);
  end;
end;
procedure tFaTokContent.AddRegEx(exp: string; Complete: boolean = false);
{Agrega una expresión regular (un conjunto de instrucciones sin opciones de control), al
token por contenido. Las expresiones regulares deben ser solo las soportadas.
Ejemplos son:  "[0..9]*[\.][0..9]", "[A..Za..z]*"
Las expresiones se evalúan parte por parte. Si un token no coincide completamente con la
expresión regular, se considera al token, solamente hasta el punto en que coincide.
Si se produce algún error se generará una excepción.}
var
  dToStart: Integer;
begin
  if Complete then begin
    //Cuando no coincide completamente, retrocede hasta el demimitador incial
    dToStart := 0;  //distamcia al inicio
    while exp<>'' do begin
      AddOneInstruct(exp,'','exit(-'+ IntToStr(dToStart) + ')');
      Inc(dToStart);
    end;
  end else begin
    //La coinicidencia puede ser parcial
    while exp<>'' do begin
      AddOneInstruct(exp,'','');  //en principio, siempre debe coger una expresión
    end;
  end;
end;

{ TSynFacilSynBase }
function TSynFacilSynBase.GetSampleSource: String;
begin
  Result := fSampleSource;
end;
//funciones básicas
function TSynFacilSynBase.BuscTokEspec(var mat: TArrayTokSpec; cad: string;
                         out n: integer; TokPos: integer = 0): boolean;
//Busca una cadena en una matriz TArrayTokSpec. Si la ubica devuelve el índice en "n".
var i : integer;
begin
  Result := false;
  if TokPos = 0 then begin //búsqueda normal
    for i := 0 to High(mat) do begin
      if mat[i].txt = cad then begin
        n:= i;
        exit(true);
      end;
    end;
  end else begin  //búsqueda con TokPos
    for i := 0 to High(mat) do begin
      if (mat[i].txt = cad) and (TokPos = mat[i].TokPos) then begin
        n:= i;
        exit(true);
      end;
    end;
  end;
end;
function TSynFacilSynBase.ToListRegex(list: TFaXMLatrib): string;
//Reemplaza el contenido de una lista en foramto XML (p.ej. "A..Z") al formato de
//listas de expresiones regulares; "[A-Z]"
//Los caracteres "..", cambian a "-" y el caracter "-", cambia a "\-"
var
  tmp: String;
begin
  tmp := StringReplace(list.val, '-', '\-',[rfReplaceAll]);
  tmp := StringReplace(tmp, '..', '-',[rfReplaceAll]);
  Result := '[' + tmp + ']';  //completa con llaves
end;
function TSynFacilSynBase.dStartRegex(tStart, tCharsStart: TFaXMLatrib): string;
//Lee los parámetros XML "Start" y "CharsStart"; y extrae el delimitador inicial
//a usar en formato de Expresión Regular.
begin
  //validaciones
  if tStart.hay and tCharsStart.hay then begin
    //No es un caso válido que se den los dos parámetros
    raise ESynFacilSyn.Create(ERR_NOT_USE_START);
  end;
  if not tStart.hay and not tCharsStart.hay then begin
    //Tampoco es un caso válido que no se de ninguno.
    raise ESynFacilSyn.Create(ERR_PAR_START_CHARS);
  end;
  //Hay uno u otro parámetro definido
  if tStart.hay then begin
    Result := EscapeText(tStart.val);  //protege a los caracteres especiales
  end else if tCharsStart.hay then begin
    Result := ToListRegex(tCharsStart);  //convierte a expresión regular como [a..z]
  end;
end;
procedure TSynFacilSynBase.VerifDelim(delim: string);
//Verifica la validez de un delimitador para un token delimitado.
//Si hay error genera una excepción.
var c:char;
    tmp: string;
begin
  //verifica contenido
  if delim = '' then
    raise ESynFacilSyn.Create(ERR_TOK_DELIM_NULL);
  //verifica si inicia con caracter de identificador.
  if  delim[1] in charsIniIden then begin
    //Empieza como identificador. Hay que verificar que todos los demás caracteres
    //sean también de identificador, de otra forma no se podrá reconocer el token.
    tmp := copy(delim, 2, length(delim) );
    for c in tmp do
      if not CharsIdentif[c] then begin
        raise ESynFacilSyn.Create(format(ERR_TOK_DEL_IDE_ERR,[delim]));
      end;
  end;
end;
procedure TSynFacilSynBase.ValidateParamStart(Start: string; var ListElem: TStringList);
{Valida si la expresión del parámetro es de tipo <literal> o [<lista de cars>], de
otra forma generará una excepción.
Si es de tipo <literal>, valida que sea un delimitador válido.
Devuelve en "ListElem" una lista con con los caracteres (En el caso de [<lista de cars>])
o un solo elemento con una cadena (En el caso de <literal>). Por ejemplo:
Si Start = 'cadena', entonces se tendrá: ListElem = [ 'cadena' ]
Si Start = '[1..5]', entonces se tendrá: ListElem = ['0','1','2','3','4','5']
Si encuentra error, genera excepción.}
var
  t: tFaRegExpType;
  listChars: string;
  str: string;
  c: Char;
begin
  if Start= '' then raise ESynFacilSyn.Create(ERR_START_NO_EMPTY);
  t := ExtractRegExp(Start, str, listChars);
  ListElem.Clear;
  if Start<>'' then  //la expresión es más compleja
    raise ESynFacilSyn.Create(ERR_EXP_MUST_BE_BR);
  if t = tregChars then begin
    for c in listChars do begin
      ListElem.Add(c);
    end;
  end else if t = tregString then begin  //lista simple o literal cadena
    VerifDelim(str);   //valida reglas
    lisTmp.Add(str);
  end else //expresión de otro tipo
    raise ESynFacilSyn.Create(ERR_EXP_MUST_BE_BR);
end;
procedure TSynFacilSynBase.ValidAsigDelim(delAct, delNue: TFaTypeDelim; delim: string);
//Verifica si la asignación de delimitadores es válida. Si no lo es devuelve error.
begin
  if delAct = tdNull then  exit;  //No estaba inicializado, es totalente factible
  //valida asignación de delimitador
  if (delAct in [tdUniLin, tdMulLin]) and
     (delNue in [tdUniLin, tdMulLin]) then begin
    raise ESynFacilSyn.Create(Format(ERR_IDEN_ALREA_DEL,[delim]));
  end;
end;
function TSynFacilSynBase.KeyComp(var r: TTokSpec): Boolean; inline;
{Compara rápidamente una cadena con el token actual, apuntado por "fToIden".
 El tamaño del token debe estar en "fStringLen"}
var
  i: Integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(r.txt) = fStringLen then begin  //primera comparación
    if (r.TokPos <> 0) and (r.TokPos<>posTok) then exit(false);  //no coincide
    Result := True;  //valor por defecto
    for i := 1 to fStringLen do begin
      if TabMayusc[Temp^] <> r.txt[i] then exit(false);
      inc(Temp);
    end;
  end else  //definitívamente es diferente
    Result := False;
end;
function TSynFacilSynBase.CreaBuscTokEspec(var mat: TArrayTokSpec; cad: string;
                                       out i:integer; TokPos: integer = 0): boolean;
{Busca o crea el token especial indicado en "cad". Si ya existe, devuelve TRUE y
 actualiza "i" con su posición. Si no existe. Crea el token especial y devuelve la
 referencia en "i". Se le debe indicar la tabla a buscar en "mat"}
var
  r:TTokSpec;
begin
  if not CaseSensitive then cad:= UpCase(cad);  //cambia caja si es necesario
  if BuscTokEspec(mat, cad, i, TokPos) then exit(true);  //ya existe, devuelve en "i"
  //no existe, hay que crearlo. Aquí se definen las propiedades por defecto
  r.txt:=cad;         //se asigna el nombre
  r.TokPos:=TokPos;   //se asigna ordinal del token
  r.tTok:=-1;        //sin tipo asignado
  r.typDel:=tdNull;   //no es delimitador
  r.dEnd:='';         //sin delimitador final
  r.pRange:=nil;      //sin función de rango
  r.folTok:=false;    //sin plegado de token
  r.chrEsc := #0;       //sin caracter de escape
  r.openBlk:=false;    //sin plegado de bloque
  r.closeBlk:=false;    //sin plegado de bloque
  r.OpenSec:=false;    //no es sección de bloque
  r.firstSec:=nil;     //inicialmente no abre ningún bloque

  i := High(mat)+1;   //siguiente posición
  SetLength(mat,i+1); //hace espacio
  mat[i] := r;        //copia todo el registro
  //sale indicando que se ha creado
  Result := false;
end;
//procesamiento de XML
function TSynFacilSynBase.ReadXMLParam(n: TDOMNode; nomb:string): TFaXMLatrib;
//Explora un nodo para ver si existe un atributo, y leerlo. Ignora la caja.
var
  i: integer;
  cad: string;
  atri: TDOMNode;
  function EsEntero(txt: string; out num: integer): boolean;
  //convierte un texto en un número entero. Si es numérico devuelve TRUE
  var i: integer;
  begin
    Result := true;  //valor por defecto
    num := 0; //valor por defecto
    for i:=1 to length(txt) do begin
      if not (txt[i] in ['0'..'9']) then exit(false);  //no era
    end;
    //todos los dígitos son numéricos
    num := StrToInt(txt);
  end;
begin
  Result.hay := false; //Se asume que no existe
  Result.val:='';      //si no encuentra devuelve vacío
  Result.bol:=false;   //si no encuentra devuelve Falso
  Result.n:=0;         //si no encuentra devuelve 0
  for i:= 0 to n.Attributes.Length-1 do begin
    atri := n.Attributes.Item[i];
    if UpCase(AnsiString(atri.NodeName)) = UpCase(nomb) then begin
      Result.hay := true;          //marca bandera
      Result.val := AnsiString(atri.NodeValue);  //lee valor
      Result.bol := UpCase(atri.NodeValue) = 'TRUE';  //lee valor booleano
      cad := trim(AnsiString(atri.NodeValue));  //valor sin espacios
      //lee número
      if (cad<>'') and (cad[1] in ['0'..'9']) then  //puede ser número
        EsEntero(cad,Result.n); //convierte
      //Lee color
      Result.col := ColorFromStr(cad);
    end;
  end;
end;
procedure TSynFacilSynBase.CheckXMLParams(n: TDOMNode; listAtrib: string);
//Valida la existencia completa de los nodos indicados. Si encuentra alguno más
//genera excepción. Los nodos deben estar separados por espacios.
var i,j   : integer;
    atri  : TDOMNode;
    nombre, tmp : string;
    hay   : boolean;
begin
  //Carga lista de atributos
  lisTmp.Clear;  //usa lista temproal
  lisTmp.Delimiter := ' ';
  //StringReplace(listSym, #13#10, ' ',[rfReplaceAll]);
  lisTmp.DelimitedText := listAtrib;
  //Realiza la verificación
  for i:= 0 to n.Attributes.Length-1 do begin
    atri := n.Attributes.Item[i];
    nombre := UpCase(AnsiString(atri.NodeName));
    //verifica existencia
    hay := false;
    for j:= 0 to lisTmp.Count -1 do begin
      tmp := trim(lisTmp[j]);
      if nombre = UpCase(tmp) then begin
         hay := true; break;
      end;
    end;
    //verifica si no existe
    if not hay then begin   //Este atributo está demás
      raise ESynFacilSyn.Create(format(ERR_INVAL_ATTR_LAB,[atri.NodeName, n.NodeName]));
    end;
  end;
end;
////Métodos para tokens por contenido
procedure TSynFacilSynBase.metTokCont(const tc: tFaTokContent); //inline;
//Procesa tokens por contenido
var
  n,i : Integer;
  posFin0: Integer;
  nf  : Integer;
  tam1: Integer;
  inst: tFaTokContentInstPtr;
begin
  fTokenID := tc.TokTyp;   //No debería ser necesario ya que se asignará después.
  inc(posFin);  //para pasar al siguiente caracter
  n := 0;
  while n<tc.nInstruc do begin
    inst := @tc.Instrucs[n];  //Para acceso rápido
    inst^.posFin := posFin;  //guarda posición al iniciar
    case inst^.expTyp of
    tregTokPos: begin   //TokPos=?
        //Se verifica posición de token
      //verifica la coincidencia
      if inst^.tokPos = posTok then begin //cumple
        if inst^.aMatch<>-1 then fTokenID := inst^.aMatch;  //pone atributo
        case inst^.actionMatch of
        aomNext:;   //no hace nada, pasa al siguiente elemento
        aomExit: break;    //simplemente sale
        aomExitpar: begin  //sale con parámetro
          nf := inst^.destOnMatch;   //lee posición final
          posFin := tc.Instrucs[nf].posFin;  //Debe moverse antes de salir
          break;
        end;
        aomMovePar: begin  //se mueve a una posición
          n := inst^.destOnMatch;   //ubica posición
          continue;
        end;
        end;
      end else begin      //no cumple
        if inst^.aFail<>-1 then fTokenID := inst^.aFail;  //pone atributo
        case inst^.actionFail of
        aomNext:;   //no hace nada, pasa al siguiente elemento
        aomExit: break;    //simplemente sale
        aomExitpar: begin  //sale con parámetro
          nf := inst^.destOnFail;   //lee posición final
          posFin := tc.Instrucs[nf].posFin;  //Debe moverse antes de salir
          break;
        end;
        aomMovePar: begin  //se mueve a una posición
          n := inst^.destOnFail;   //ubica posición
          continue;
        end;
        end;
      end;
    end;

    tregString: begin  //texo literal
        //Rutina de comparación de cadenas
        posFin0 := posFin;  //para poder restaurar
        i := 1;
        tam1 := length(inst^.Text)+1;  //tamaño +1
        if CaseSensitive then begin  //sensible a caja
          while (i<tam1) and (inst^.Text[i] = fLine[posFin]) do begin
            inc(posFin);
            inc(i);
          end;
        end else begin  //Ignora mayúcula/minúscula
          while (i<tam1) and (inst^.Text[i] = TabMayusc[fLine[posFin]]) do begin
            inc(posFin);
            inc(i);
          end;
        end;
        //verifica la coincidencia
        if i = tam1 then begin //cumple
          if inst^.aMatch<>-1 then fTokenID := inst^.aMatch;  //pone atributo
          case inst^.actionMatch of
          aomNext:;   //no hace nada, pasa al siguiente elemento
          aomExit: break;    //simplemente sale
          aomExitpar: begin  //sale con parámetro
            nf := inst^.destOnMatch;   //lee posición final
            posFin := tc.Instrucs[nf].posFin;  //Debe moverse antes de salir
            break;
          end;
          aomMovePar: begin  //se mueve a una posición
            n := inst^.destOnMatch;   //ubica posición
            continue;
          end;
          end;
        end else begin      //no cumple
          if inst^.aFail<>-1 then fTokenID := inst^.aFail;  //pone atributo
          posFin := posFin0;   //restaura posición
          case inst^.actionFail of
          aomNext:;   //no hace nada, pasa al siguiente elemento
          aomExit: break;    //simplemente sale
          aomExitpar: begin  //sale con parámetro
            nf := inst^.destOnFail;   //lee posición final
            posFin := tc.Instrucs[nf].posFin;  //Debe moverse antes de salir
            break;
          end;
          aomMovePar: begin  //se mueve a una posición
            n := inst^.destOnFail;   //ubica posición
            continue;
          end;
          end;
        end;
      end;
    tregChars: begin   //conjunto de caracteres: [ ... ]
        //debe existir solo una vez
        if inst^.Chars[fLine[posFin]] then begin
          //cumple el caracter
          if inst^.aMatch<>-1 then fTokenID := inst^.aMatch;  //pone atributo
          inc(posFin);  //pasa a la siguiente instrucción
          //Cumple el caracter
          case inst^.actionMatch of
          aomNext:;   //no hace nada, pasa al siguiente elemento
          aomExit: break;    //simplemente sale
          aomExitpar: begin  //sale con parámetro
            nf := inst^.destOnMatch;   //lee posición final
            posFin := tc.Instrucs[nf].posFin;  //Debe moverse antes de salir
            break;
          end;
          aomMovePar: begin  //se mueve a una posición
            n := inst^.destOnMatch;   //ubica posición
            continue;
          end;
          end;
        end else begin
          //no se encuentra ningún caracter de la lista
          if inst^.aFail<>-1 then fTokenID := inst^.aFail;  //pone atributo
          case inst^.actionFail of
          aomNext:;   //no hace nada, pasa al siguiente elemento
          aomExit: break;    //simplemente sale
          aomExitpar: begin  //sale con parámetro
            nf := inst^.destOnFail;   //lee posición final
            posFin := tc.Instrucs[nf].posFin;  //Debe moverse antes de salir
            break;
          end;
          aomMovePar: begin  //se mueve a una posición
            n := inst^.destOnFail;   //ubica posición
            continue;
          end;
          end;
        end;
    end;
    tregChars01: begin   //conjunto de caracteres: [ ... ]?
        //debe existir cero o una vez
        if inst^.Chars[fLine[posFin]] then begin
          inc(posFin);  //pasa a la siguiente instrucción
        end;
        //siempre cumplirá este tipo, no hay nada que verificar
        if inst^.aMatch<>-1 then fTokenID := inst^.aMatch;  //pone atributo
        case inst^.actionMatch of
        aomNext:;   //no hace nada, pasa al siguiente elemento
        aomExit: break;    //simplemente sale
        aomExitpar: begin  //sale con parámetro
          nf := inst^.destOnMatch;   //lee posición final
          posFin := tc.Instrucs[nf].posFin;  //Debe moverse antes de salir
          break;
        end;
        aomMovePar: begin  //se mueve a una posición
          n := inst^.destOnMatch;   //ubica posición
          continue;
        end;
        end;
    end;
    tregChars0_: begin   //conjunto de caracteres: [ ... ]*
        //debe exitir 0 o más veces
        while inst^.Chars[fLine[posFin]] do begin
          inc(posFin);
        end;
        //siempre cumplirá este tipo, no hay nada que verificar
        if inst^.aMatch<>-1 then fTokenID := inst^.aMatch;  //pone atributo
        //¿No debería haber código aquí también?
      end;
    tregChars1_: begin   //conjunto de caracteres: [ ... ]+
        //debe existir una o más veces
        posFin0 := posFin;  //para poder comparar
        while inst^.Chars[fLine[posFin]] do begin
          inc(posFin);
        end;
        if posFin>posFin0 then begin   //Cumple el caracter
          if inst^.aMatch<>-1 then fTokenID := inst^.aMatch;  //pone atributo
          case inst^.actionMatch of
          aomNext:;   //no hace nada, pasa al siguiente elemento
          aomExit: break;    //simplemente sale
          aomExitpar: begin  //sale con parámetro
            nf := inst^.destOnMatch;   //lee posición final
            posFin := tc.Instrucs[nf].posFin;  //Debe moverse antes de salir
            break;
          end;
          aomMovePar: begin  //se mueve a una posición
            n := inst^.destOnMatch;   //ubica posición
            continue;
          end;
          end;
        end else begin   //No cumple
          if inst^.aFail<>-1 then fTokenID := inst^.aFail;  //pone atributo
          case inst^.actionFail of
          aomNext:;   //no hace nada, pasa al siguiente elemento
          aomExit: break;    //simplemente sale
          aomExitpar: begin  //sale con parámetro
            nf := inst^.destOnFail;   //lee posición final
            posFin := tc.Instrucs[nf].posFin;  //Debe moverse antes de salir
            break;
          end;
          aomMovePar: begin  //se mueve a una posición
            n := inst^.destOnFail;   //ubica posición
            continue;
          end;
          end;
        end;
      end;
    end;
    inc(n);
  end;
end;
procedure TSynFacilSynBase.metTokCont1; //Procesa tokens por contenido 1
begin
  metTokCont(tc1);
end;
procedure TSynFacilSynBase.metTokCont2; //Procesa tokens por contenido 2
begin
  metTokCont(tc2);
end;
procedure TSynFacilSynBase.metTokCont3; //Procesa tokens por contenido 3
begin
  metTokCont(tc3);
end;
procedure TSynFacilSynBase.metTokCont4; //Procesa tokens por contenido 3
begin
  metTokCont(tc4);
end;
//Procesamiento de otros elementos
procedure TSynFacilSynBase.metIdent;
//Procesa el identificador actual
begin
  inc(posFin);  {debe incrementarse, para pasar a comparar los caracteres siguientes,
                 o de otra forma puede quedarse en un lazo infinito}
  while CharsIdentif[fLine[posFin]] do inc(posFin);
  fTokenID := tnIdentif;  //identificador común
end;
procedure TSynFacilSynBase.metIdentUTF8;
//Procesa el identificador actual. considerando que empieza con un caracter UTF8 (dos bytes)
begin
  inc(posFin);  {es UTF8, solo filtra por el primer caracter (se asume que el segundo
                 es siempre válido}
  inc(posFin);  {debe incrementarse, para pasar a comparar los caracteres siguientes,
                 o de otra forma puede quedarse en un lazo infinito}
  while CharsIdentif[fLine[posFin]] do inc(posFin);
  fTokenID := tnIdentif;  //identificador común
end;
procedure TSynFacilSynBase.metNull;
//Procesa la ocurrencia del cacracter #0
begin
  fTokenID := tnEol;   //Solo necesita esto para indicar que se llegó al final de la línae
end;
procedure TSynFacilSynBase.metSpace;
//Procesa caracter que es inicio de espacio
begin
  fTokenID := tnSpace;
  repeat  //captura todos los que sean espacios
    Inc(posFin);
  until (fLine[posFin] > #32) or (posFin = tamLin);
end;
procedure TSynFacilSynBase.metSymbol;
begin
  inc(posFin);
  while (fProcTable[fLine[posFin]] = @metSymbol)
  do inc(posFin);
  fTokenID := tnSymbol;
end;
//Funciones públicas
procedure TSynFacilSynBase.DefTokIdentif(dStart, Content: string );
{Define token para identificadores. Los parámetros deben ser intervalos.
El parámetro "dStart" deben ser de la forma: "[A..Za..z]"
El parámetro "charsCont" deben ser de la forma: "[A..Za..z]*"
Si los parámetros no cumplen con el formato se generará una excepción.
Se debe haber limpiado previamente con "ClearMethodTables"}
var
  c : char;
  t : tFaRegExpType;
  listChars: string;
  str: string;
begin
  /////// Configura caracteres de inicio
  if dStart = '' then exit;   //protección
  t := ExtractRegExp(dStart, str, listChars);
  if (t <> tregChars) or (dStart<>'') then  //solo se permite el formato [ ... ]
    raise ESynFacilSyn.Create(ERR_BAD_PAR_STR_IDEN);
  //Agrega evento manejador en caracteres iniciales
  charsIniIden := [];  //inicia
  for c in listChars do begin //permite cualquier caracter inicial
    if c<#128 then begin  //caracter normal
      fProcTable[c] := @metIdent;
      charsIniIden += [c];  //agrega
    end else begin   //caracter UTF-8
      fProcTable[c] := @metIdentUTF8;
      charsIniIden += [c];  //agrega
    end;
  end;
  /////// Configura caracteres de contenido
  t := ExtractRegExp(Content, str, listChars);
  if (t <> tregChars0_) or (Content<>'') then  //solo se permite el formato [ ... ]*
    raise ESynFacilSyn.Create(ERR_BAD_PAR_CON_IDEN);
  //limpia matriz
  for c := #0 to #255 do begin
    CharsIdentif[c] := False;
    //aprovecha para crear la tabla de mayúsculas para comparaciones
    if CaseSensitive then
      TabMayusc[c] := c
    else begin  //pasamos todo a mayúscula
      TabMayusc[c] := UpCase(c);
    end;
  end;
  //marca las posiciones apropiadas
  for c in listChars do CharsIdentif[c] := True;
end;
//Manejo de atributos
function TSynFacilSynBase.NewTokAttrib(TypeName: string; out TokID: integer
  ): TSynHighlighterAttributes;
{Crea un nuevo atributo y lo agrega al resaltador. Este debe ser el único punto de
entrada, para crear atributos en SynFacilSyn. En tokID, se devuelve el ID del nuevo tipo.
No hay funciones para eliminar atributs creados.}
var
  n: Integer;
begin
  Result := TSynHighlighterAttributes.Create(TypeName);
  n := High(Attrib)+1;   //tamaño
  setlength(Attrib, n + 1);  //incrementa tamaño
  Attrib[n] := Result;  //guarda la referencia
  tokID := n;           //devuelve ID
  AddAttribute(Result);   //lo registra en el resaltador
end;
function TSynFacilSynBase.NewTokType(TypeName: string; out
  TokAttrib: TSynHighlighterAttributes): integer;
{Crea un nuevo tipo de token, y devuelve la referencia al atributo en "TokAttrib".}
begin
  TokAttrib := NewTokAttrib(TypeName, Result);
end;

function TSynFacilSynBase.NewTokType(TypeName: string): integer;
{Versión simplificada de NewTokType, que devuelve directamente el ID del token}
begin
  NewTokAttrib(TypeName, Result);
end;

procedure TSynFacilSynBase.CreateAttributes;
//CRea los atributos por defecto
begin
  //Elimina todos los atributos creados, los fijos y los del usuario.
  FreeHighlighterAttributes;
  setlength(Attrib, 0);  //limpia
  { Crea los atributos que siempre existirán. }
  tkEol     := NewTokAttrib('Eol', tnEol);      //atributo de nulos
  tkSymbol  := NewTokAttrib('Symbol', tnSymbol);   //atributo de símbolos
  tkSpace   := NewTokAttrib('Space', tnSpace);    //atributo de espacios.
  tkIdentif := NewTokAttrib('Identifier', tnIdentif); //Atributo para identificadores.
  tkNumber  := NewTokAttrib('Number', tnNumber);   //atributo de números
  tkNumber.Foreground := clFuchsia;
  tkKeyword := NewTokAttrib('Keyword',tnKeyword);      //atribuuto de palabras claves
  tkKeyword.Foreground:=clGreen;
  tkString  := NewTokAttrib('String', tnString);   //atributo de cadenas
  tkString.Foreground := clBlue;
  tkComment := NewTokAttrib('Comment', tnComment);  //atributo de comentarios
  tkComment.Style := [fsItalic];
  tkComment.Foreground := clGray;
end;
function TSynFacilSynBase.GetAttribByName(txt: string): TSynHighlighterAttributes;
{Devuelve la referencia de un atributo, recibiendo su nombre. Si no lo encuentra
devuelve NIL.}
var
  i: Integer;
begin
  txt := UpCase(txt);   //ignora la caja
  //También lo puede buscar en Attrib[]
  for i:=0 to AttrCount-1 do begin
    if Upcase(Attribute[i].Name) = txt then begin
        Result := Attribute[i];  //devuelve índice
        exit;
    end;
  end;
  //No se encontró
  exit(nil);
end;
function TSynFacilSynBase.GetAttribIDByName(txt: string): integer;
{Devuelve el identificador de un atributo, recibiendo su nombre. Si no lo encuentra
devuelve -1.}
var
  i: Integer;
begin
  txt := UpCase(txt);   //ignora la caja
  //Se tiene que buscar en Attrib[], proque allí están con los índices cprrectos
  for i:=0 to AttrCount-1 do begin
    if Upcase(Attrib[i].Name) = txt then begin
        Result := i;  //devuelve índice
        exit;
    end;
  end;
  //No se encontró
  exit(-1);
end;

function TSynFacilSynBase.IsAttributeName(txt: string): boolean;
//Verifica si una cadena corresponde al nombre de un atributo.
begin
  //primera comparación
  if GetAttribByName(txt) <> nil then exit(true);
  //puede que haya sido "NULL"
  if UpCase(txt) = 'NULL' then exit(true);
  //definitivamente no es
  Result := False;
end;
function TSynFacilSynBase.ProcXMLattribute(nodo: TDOMNode): boolean;
//Verifica si el nodo tiene la etiqueta <ATTRIBUTTE>. De ser así, devuelve TRUE y lo
//procesa. Si encuentra error, genera una excepción.
var
  tName: TFaXMLatrib;
  tBackCol: TFaXMLatrib;
  tForeCol: TFaXMLatrib;
  tFrameCol: TFaXMLatrib;
  tFrameEdg: TFaXMLatrib;
  tFrameSty: TFaXMLatrib;
  tStyBold: TFaXMLatrib;
  tStyItal: TFaXMLatrib;
  tStyUnder: TFaXMLatrib;
  tStyStrike: TFaXMLatrib;
  tStyle: TFaXMLatrib;
  tipTok: TSynHighlighterAttributes;
  Atrib: TSynHighlighterAttributes;
  tokId: integer;
begin
  if UpCase(nodo.NodeName) <> 'ATTRIBUTE' then exit(false);
  Result := true;  //encontró
  ////////// Lee parámetros //////////
  tName    := ReadXMLParam(nodo,'Name');
  tBackCol := ReadXMLParam(nodo,'BackCol');
  tForeCol := ReadXMLParam(nodo,'ForeCol');
  tFrameCol:= ReadXMLParam(nodo,'FrameCol');
  tFrameEdg:= ReadXMLParam(nodo,'FrameEdg');
  tFrameSty:= ReadXMLParam(nodo,'FrameSty');
  tStyBold := ReadXMLParam(nodo,'Bold');
  tStyItal := ReadXMLParam(nodo,'Italic');
  tStyUnder:= ReadXMLParam(nodo,'Underline');
  tStyStrike:=ReadXMLParam(nodo,'StrikeOut');
  tStyle   := ReadXMLParam(nodo,'Style');
  CheckXMLParams(nodo, 'Name BackCol ForeCol FrameCol FrameEdg FrameSty '+
                         'Bold Italic Underline StrikeOut Style');
  ////////// cambia atributo //////////
  if IsAttributeName(tName.val)  then begin
    tipTok := GetAttribByName(tName.val);   //tipo de atributo
  end else begin
    //No existe, se crea.
    tipTok := NewTokAttrib(tName.val, tokId);
  end;
  //obtiene referencia
  Atrib := tipTok;
  //asigna la configuración del atributo
  if Atrib <> nil then begin
     if tBackCol.hay then Atrib.Background:=tBackCol.col;
     if tForeCol.hay then Atrib.Foreground:=tForeCol.col;
     if tFrameCol.hay then Atrib.FrameColor:=tFrameCol.col;
     if tFrameEdg.hay then begin
       case UpCase(tFrameEdg.val) of
       'AROUND':Atrib.FrameEdges:=sfeAround;
       'BOTTOM':Atrib.FrameEdges:=sfeBottom;
       'LEFT':  Atrib.FrameEdges:=sfeLeft;
       'NONE':  Atrib.FrameEdges:=sfeNone;
       end;
     end;
     if tFrameSty.hay then begin
       case UpCase(tFrameSty.val) of
       'SOLID': Atrib.FrameStyle:=slsSolid;
       'DASHED':Atrib.FrameStyle:=slsDashed;
       'DOTTED':Atrib.FrameStyle:=slsDotted;
       'WAVED': Atrib.FrameStyle:=slsWaved;
       end;
     end;
     if tStyBold.hay then begin  //negrita
        if tStyBold.bol then Atrib.Style:=Atrib.Style+[fsBold]
        else Atrib.Style:=Atrib.Style-[fsBold];
     end;
     if tStyItal.hay then begin  //cursiva
        if tStyItal.bol then Atrib.Style:=Atrib.Style+[fsItalic]
        else Atrib.Style:=Atrib.Style-[fsItalic];
     end;
     if tStyUnder.hay then begin  //subrayado
        if tStyUnder.bol then Atrib.Style:=Atrib.Style+[fsUnderline]
        else Atrib.Style:=Atrib.Style-[fsUnderline];
     end;
     if tStyStrike.hay then begin //tachado
        if tStyStrike.bol then Atrib.Style:=Atrib.Style+[fsStrikeOut]
        else Atrib.Style:=Atrib.Style-[fsStrikeOut];
     end;
     if tStyle.hay then begin  //forma alternativa
       Atrib.Style:=Atrib.Style-[fsBold]-[fsItalic]-[fsUnderline]-[fsStrikeOut];
       if Pos('b', tStyle.val)<>0 then Atrib.Style:=Atrib.Style+[fsBold];
       if Pos('i', tStyle.val)<>0 then Atrib.Style:=Atrib.Style+[fsItalic];
       if Pos('u', tStyle.val)<>0 then Atrib.Style:=Atrib.Style+[fsUnderline];
       if Pos('s', tStyle.val)<>0 then Atrib.Style:=Atrib.Style+[fsStrikeOut];
     end;
  end;
end;
constructor TSynFacilSynBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  setlength(Attrib, 0);
end;

var
  i: integer;
initialization
  //prepara definición de comodines
  bajos[0] := #127;
  for i:=1 to 127 do bajos[i] := chr(i);  //todo menos #0
  altos[0] := #128;
  for i:=1 to 128 do altos[i] := chr(i+127);

end.

