{Pascal lexer, using the language defined in the compilers PicPas and P65pas.
                                          Created by Tito Hinostroza  08/03/2020
}
unit LexPas;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, LCLProc;
type

  //Context types
  tTypCon = (
    TC_ARC,     //File type context.
    TC_TXT      //Text type context.
  );

  //Primary location for elements
  { TODO : ¿Debe estar aquí? Formalmente esta clasific. pertenece al nivel de elementos. }
  TxpEleLocation = (
                locMain,       //En el programa principal.
                locInterface,  //En INTERFACE de una unidad.
                locImplement   //En IMPLEMENTATION de una unidad.
  );

  TTokenKind = (
    //Pascal Lexer token
    tkNull     ,  //Not defined token. Single-line token.
    tkEol      ,  //End of line. Single-line token.
    tkSymbol   ,  //Symbol
    tkSpace    ,  //Space token. Consider only characters #9 and #32. Single-line token.
    tkIdentifier, //Identifier. Single-line token.
    tkLitNumber,  //Literal number
    tkString   ,  //Literal String
    tkComment  ,  //Comment. Multi-line token.
    tkOperator ,  //Operators
    tkDirective,  //
//    tkAsm      ,  //
    tkExpDelim ,  //
    tkBlkDelim ,  //
    tkChar     ,  //
    tkKeyword  ,  //Reserved words
    tkDirDelim ,  //Delimitador de directiva. Usado solo para directivas.
    tkOthers      //
  );

  { TScanner }
  {Define a basic lexer that can explore source text, character form character.
  This lexer only can read chars and identifies special positions of the
  source text.
  Attribute "curLines" is a reference to the source text.
  Source text is viewed as a list of lines.
  Each line is considered as a set of characters and a additional next-line character.
  So a common line like "HOLA", is mapped as:
  +---+---+---+---+---+
  | 1 | 2 | 3 | 4 | 5 |
  +---+---+---+---+---+
  | H | O | L | A |#13|
  +---+---+---+---+---+
                    |
                   EOL

  A final line like "HI", is mapped as:
  +---+---+---+
  | 1 | 2 | 3 |
  +---+---+---+
  | H | I |#13|
  +---+---+---+
            |
         EOL,EOF

  The initial position for cursor of the scanner is:
      (frow=1, fcol=1).
  The ending position is:
      (frow=nlin, fcol=length_last_line)
  ReadChar() can returns:
   - A valid character of the text.
   - A empty string, if the cursor points to a EOL character or the source text is empty.
  }
  TScanner = class
  private
    {Attributes for store the state of the scanner. Used to restore a position of the
    scanner after reading chars. Note that the source text, and related information
    is not saved, only the scanner cursor position. }
    frow_    : integer;
    fcol_    : integer;
    curLine_ : string;
    curSize_ : integer;
  public  //Fast versions of some functions.
    function _Bol: boolean; inline;
    function _Eol: boolean; inline;
    function _Eof: boolean; inline;
    function _FirstLine: boolean; inline;
    function _LastLine: boolean; inline;
    procedure _setRow(row0: integer); inline;
    procedure _setCol(col0: integer); inline;
    function _ReadChar: char; inline;
    procedure _NextChar; inline;
    procedure _setEOL;      //Set cursor at the end of current line
  public
    //Cursor position.
    frow     : integer;    //From 1 to "nlin".
    fcol     : integer;    //From 1 to "curSize".
    //Current information
    curLine  : string;     //Stores the current line. Depends on "frow".
    curSize  : integer;    //Length of the current line. Depends on "curLine". It's value is length(curLine)+1, because it consider the \n as a one more character.
    //Information of the source text.
    curLines : TStrings;   //Reference to the current StringList, used to scan. This class doesn't have a container for the input text.
    nlin     : LongInt;    //Number of lines of the source.
  public  //Scanner state
    procedure SaveScannerState;
    procedure RestoreScannerState;
  public  //Check positions
    function Empty: boolean; inline;  //Returns TRUE if ther is not source text.
    function Bol: boolean;  //Returns TRUE if cursor is at the begin of the current line.
    function Eol: boolean;  //Returns TRUE if cursor is at the end of the current line.
    function FirstLine: boolean;
    function LastLine: boolean;
    function Bof: boolean; inline; //Returns TRUE if cursor is at the begin of file.
    function Eof: boolean; inline; //Returns TRUE if cursor is at the end of file.
  public  //Set positions
    procedure setRow(row0: integer);  //Set the current row
    procedure setCol(col0: integer);  //Set the current col
    procedure setEOL;      //Set cursor at the end of current line
    procedure setBOF;      //Set cursor at the begin of file
    procedure setEOF;      //Set cursor at the end of file
  public  //Scan functions
    procedure NextChar;        //Move cursor to the next position
    function ReadChar: char; inline;  //Returns the char pointed by cursor
    procedure SetText(strList: TStringList);  //Sets the source text.
    //constructor Create;
    //destructor Destroy; override;
  end;

  { TSrcPos }
  { Position in the source code. Used to:
   - Locate syntax elements in source code.
   - Define postion for errors or warnings.
  As all the syntax elements must have a TSrcPos field, the size of this object
  is keeping small.
  This position can be used too, for return the scanner to a specific state of
  exploration (and no need to save a TContextState record). It will require to
  restore the position again, and make a single token scan at that position to
  restore all the attributes of the Context. }
  TSrcPos = object
    idCtx  : integer;  //Id for the context.
    //Attributes for position.
    row    : integer;  //Row number
    col    : integer;  //Column number
    function RowColString: string;
    function EqualTo(const target: TSrcPos): boolean;
  end;
  TSrcPosArray = array of TSrcPos;

  { TContextState }
  {Record for store the state of the context. Used to restore a position of the
  scanner after reading tokens. Note that the source text, and related information
  is not saved, only the scanner cursor position. }
  TContextState = record
    idCtx   : integer;     //Id for the context.
    //Attributes of TScannerState
    frow    : integer;
    fcol    : integer;
    curLine : string;
    curSize : integer;
    //Additional atributes.
    row0    : integer;
    col0    : integer;
    tokType : TTokenKind;
    tokPrec : integer;
    tokPrecU: integer;    //Precedence when "tokType" is "tkOperator" and operator can be used as unary operator.
  end;

  { TContext }
  {Estructura que define a un objeto contexto. Un contexto es un objeto que sirve como
  entrada de datos, en donde se puede cargar un texto, y luego leerlo token por token
  de forma simple.}
  TContext = class(TScanner)
  private
    fLexerState: TContextState;  //almacenamiento temporal
  public  //State of the scanner
    //Position for start of current token
    row0     : integer;    //From 1 to "nlin". ?
    col0     : integer;    //From 1 to "curSize". Set to 0 at the beginning. ?
    //Information for current token
    tokType  : TTokenKind; //Current token kind.
    tokPrec  : integer;    //Precedence when "tokType" is "tkOperator".
    tokPrecU : integer;    //Precedence when "tokType" is "tkOperator" and operator can be used as unary operator.
    //The rest of the state are defined by TScanner: frow, fcol, curLine, ...
  public  //Control for current position
    procedure GetContextState(out c: TContextState);
    procedure SetContextState(const c: TContextState);
    procedure SaveContextState;    //Guarda el estado actual del lexer
    procedure RestoreContextState; //Restaura el estado actual del lexer
    //Current cursor position.
    property row: integer read frow;
    property col: integer read fcol;
  public  //Containers for content
    intLines : TStringList;  {Internal containers for text, when not specified an external
                             TStringList. Always created.}
  public  //Scan functions
    OnDecodeNext: function: boolean of object;
    function DecodeNext: boolean;
    function Next: boolean;      //Pasa al siguiente token
    function ReadToken: string;
    procedure SkipWhites;
    procedure SkipWhitesNoEOL;
  public
    idCtx    : integer;     //Unique identifier for the context.
    retPos   : TContextState; //Return position to parent context.
    typ      : tTypCon;     //Context type.
    fileSrc  : String;      //Nombre de archivo fuente, incluyendo la ruta. En caso de que el contexto corresponda a uno.
    data     : TObject;     //Campo para información adiciconal que se desee almacenar.
    autoReturn: boolean;    {Indica que, al finalizar la exploración, se debe retornar al
                            contexto que hizo la llamada.}
    autoRemove: boolean;    {Indica que, al finalizar la exploración, se debe eliminar
                            este contexto. Solo es válido cuando se activa "autoReturn".}
    function ReadSource: string;    //Lee el contenido del contexto en un string
  public  //Error generation
    onGenError: procedure(msg: string) of object;  //Generates error
  public  //Manage of the Pre-error.
    {The Pre-error is a technique that affect the generation of errors in this context.
    When it's activated, it means that if an error is generated when scannig this context,
    the error will be located at the position indicated by "PreErrPosit", instead of the
    normal position.
    }
    FixErrPos: boolean;     {Indica que los mensajes de error, deben apuntar a una
                             posición fija, y no a la posición en donde se detecta el error.}
    PreErrPosit: TSrcPos;   //Posición a usar para el error, cuando se activa FixErrPos.
    PreErrorMsg: string;    {Mensaje previo al mensaje de error, cuando el errror se
                             genere en este contexto.}
  public  //Métodos de inicialización
    function IniCont:Boolean;
    procedure StartScan;
    procedure SetSource(txt : string);   //Fija el contenido del contexto con cadena
    procedure SetSource(lins: Tstrings; MakeCopy: boolean = false); //Fija contenido a partir de una lista
    procedure SetSourceF(file0: string);  //Fija el contenido del contexto con archivo
    constructor Create;
    destructor Destroy; override;
  end;

  //Define una lista de Contextos
  TContextList = specialize TFPGObjectList<TContext>;
var
  srcPosNull: TSrcPos;  //Object TScrcPos NULL.

implementation
{ TScanner }
//Fast versions of some functions.
function TScanner._Bol: boolean;
begin
  exit(fcol = 1);
end;
function TScanner._Eol: boolean;
{Basic version of Eol() with no validation.}
begin
  exit(fcol = curSize);  //TRUE also at EOF.
end;
function TScanner._Eof: boolean;
begin
  exit(_LastLine and _Eol);
end;
function TScanner._FirstLine: boolean;
{Basic version of FirstLine() with no validation.}
begin
  exit(frow=1);
end;
function TScanner._LastLine: boolean;
//Basic version of _LastLine().
begin
  exit(frow=nlin);
end;
procedure TScanner._setRow(row0: integer);
{Basic version of setRow() with no validations.}
begin
  //Set current row, updating related attributes.
  frow := row0;
  curLine := curLines[frow-1];
  curSize := length(curLine)+1;  //Consider an extra char for the EOL, even in the last line.
end;
procedure TScanner._setCol(col0: integer);
{Basic version of setCol() with no validation.}
begin
  fcol := col0;
end;
procedure TScanner._NextChar;
begin
  if _EOL then begin
    //At the end of the current line.
    if _LastLine then begin  //The last line
      //Cannot advance to a NextChar line. Keep position (EOF)
    end else begin
      //There is a NextChar line
      _setRow(frow+1);
      _setCol(1);
    end;
  end else begin
    //A normal position
    inc(fcol);
  end;
end;
procedure TScanner._setEOL;
begin
  fcol := curSize;
end;
function TScanner._ReadChar: char;
begin
  if _Eol then exit(#13);  //End of line
  exit(curLine[fcol]);    //Common character
end;
//Scanner state
procedure TScanner.SaveScannerState;
begin
  frow_    := frow;
  fcol_    := fcol;
  curLine_ := curLine;
  curSize_ := curSize;
end;
procedure TScanner.RestoreScannerState;
begin
  frow    := frow_;
  fcol    := fcol_;
  curLine := curLine_;
  curSize := curSize_;
end;
function TScanner.Empty: boolean;
begin
  exit(nlin=0);
end;
//Check positions
function TScanner.Bol: boolean;
begin
  if Empty then exit(true);
  exit(_Bol);
end;
function TScanner.Eol: boolean;
{Returns TRUE is the cursor is located on the end of the current line.}
begin
  if Empty then exit(false);  //Empty file.
  exit (_Eol);  //TRUE also at EOF.
end;
function TScanner.FirstLine: boolean;
begin
  if Empty then exit(false);  //Empty file.
  exit(_FirstLine);
end;
function TScanner.LastLine: boolean;
begin
  if Empty then exit(false);  //Empty file.
  exit(_LastLine);
end;
function TScanner.Bof: boolean;
begin
  if Empty then exit(true);  //Empty file.
  exit(_FirstLine and _Bol);
end;
function TScanner.Eof: boolean;
{Returns TRUE if the cursor is located to the end of the source or the source
is empty.}
begin
  if Empty then exit(true);
  exit(_LastLine and _Eol);
  {Other opcion would be (frow>nlin) however it's not possible by definition, except
  when source is Empty() (already checked).}
end;
//Set positions
procedure TScanner.setRow(row0: integer);
begin
  //Check for empty source text
  if Empty then begin
    frow := 1;
    exit;
  end;
  //Validate limit
  if row0>nlin then row0 := nlin;
  //Set row
  _setRow(row0);
end;
procedure TScanner.setCol(col0: integer);
begin
  //Check for empty source text
  if Empty then begin
    fcol := 1;
    exit;
  end;
  //Validate limit
  if col0>curSize then col0 := curSize;
  //Set current column
  _setCol(col0);
end;
procedure TScanner.setEOL;
begin
  if Empty then exit;  //Empty file.
  _setEOL;
end;
procedure TScanner.setBOF;
begin
  _setRow(1);
  _setCol(1);
end;
procedure TScanner.setEOF;
begin
  if Empty then setBOF; //Empty source text
  _setRow(nlin);     //Point to the last line
  _setCol(curSize);  //Point to an nonexistent "\n".
end;
//Scan functions
procedure TScanner.NextChar;
{Move cursor to next position.}
begin
  if Empty then exit;  //Empty source
  _NextChar;
end;
function TScanner.ReadChar: char;
begin
  if Empty then exit(#0); //Empty source
  exit(_ReadChar);    //Common character
end;
procedure TScanner.SetText(strList: TStringList);
begin
  curLines := strList;   //Just copy the reference.
end;
{ TSrcPos }
function TSrcPos.RowColString: string;
begin
  Result := '[' + IntToStr(Row) + ',' + IntToStr(Col)+']';
end;
function TSrcPos.EqualTo(const target: TSrcPos): boolean;
begin
  Result := (idCtx = target.idCtx) and
            (row    = target.row) and
            (col    = target.col);
end;
{ TContext }
function TContext.IniCont: Boolean;
//Devuelve verdadero si se está al inicio del Contexto (fila 1, columna 1)
begin
  Result := (row = 1) and (col = 1);
end;
procedure TContext.SkipWhites;
//Coge los blancos iniciales, saltos de línea y comentarios del contexto de entrada.
//Si no encuentra algun blanco al inicio, devuelve falso
begin
  while toktype in [tkSpace , tkEol, tkComment] do
  begin
    DecodeNext; //Formally, must be OnDecodeNext() but decoding spaces is similar.
  end;
end;
procedure TContext.SkipWhitesNoEOL;
//Get initial whites from input context. Doesn't consider EOL as white.
//If not whites are found, returns FALSE.
begin
  while toktype = tkSpace do begin
    DecodeNext; //Formally, must be OnDecodeNext() but decoding spaces is similar.
  end;
end;
function TContext.DecodeNext: boolean;
{Decode the token in the current position, indicated by (frow, fcol), and returns:
 - Token type in "tokType".
 - Token precedence in "tokPrec", when "tokType" is "tkOperator".
 - Start of next token in (frow, fcol).
 - Value TRUE if the current line has changed.

 Token precedence is the common in Pascal:
 6)    ~, not, unary "+", unary "-", @, **  (high precedence)
 5)    *, /, div, mod, and, shl, shr, &
 4)    |, !, +, -, or, xor
 3)    =, <>, <, <=, >, >=, in
 2)    :=, +=, -=, *=, /=  (low precedence)

}

var
  iden: String;
begin
  if _Eof then begin
    tokType := tkNull;
    exit(false);
  end else if _Eol then begin
    tokType := tkEol;
    if _LastLine then begin
      //Cannot advance to a NextChar line. Keep position (EOF)
    end else begin
      //In a common line
      _setRow(frow+1);
      _setCol(1);
    end;
    exit(true);
  end;
  case curLine[fcol] of
  #32, #9: begin
    repeat
      inc(fcol);
    until _Eol or not(curline[fcol] in [#32, #9]);
    tokType := tkSpace;
    //Leaves (frow, fcol) in the begin of the next token.
  end;
  '0'..'9': begin
    repeat
      inc(fcol);
    until _Eol or not(curline[fcol] in ['0'..'9']);
    tokType := tkLitNumber;
  end;
  '$': begin
    repeat
      inc(fcol);
    until _Eol or not(curline[fcol] in ['0'..'9','A'..'F','a'..'f']);
    tokType := tkLitNumber;
  end;
  '%': begin
    repeat
      inc(fcol);
    until _Eol or not(curline[fcol] in ['0','1']);
    tokType := tkLitNumber;
  end;
  'A','a': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if iden = 'AND' then begin
      tokType := tkOperator;
      tokPrec := 5;
    end else if iden = 'ARRAY' then begin
      tokType := tkKeyword;
    end else if iden = 'ASM' then begin
      tokType := tkKeyword;
    end else begin
      tokType := tkIdentifier;
    end;
  end;
  'B','b': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if iden = 'BEGIN' then begin
      tokType := tkKeyword;
    end else begin
      tokType := tkIdentifier;
    end;
  end;
  'C','c': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if iden = 'CASE' then begin
      tokType := tkKeyword;
    end else if iden = 'CONST' then begin
      tokType := tkKeyword;
    end else begin
      tokType := tkIdentifier;
    end;
  end;
  'D','d': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'DIV') then begin
      tokType := tkOperator;
      tokPrec := 5;
    end else tokType := tkIdentifier;
  end;
  'E','e': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'END') or (iden='ELSE') or (iden='ELSIF') then tokType := tkBlkDelim
    else tokType := tkIdentifier;
  end;
  'I','i': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'IN') then begin
      tokType := tkOperator;
      tokPrec := 3;
    end else if iden = 'INTERFACE' then begin
      tokType := tkKeyword;
    end else if iden = 'IMPLEMENTATION' then begin
      tokType := tkKeyword;
    end else tokType := tkIdentifier;
  end;
  'M','m': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'MOD') then begin
      tokType := tkOperator;
      tokPrec := 5;
    end else tokType := tkIdentifier;
  end;
  'N','n': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'NOT') then begin
      tokType := tkOperator;
      //tokPrec := 6;
      tokPrecU := 6;
    end else tokType := tkIdentifier;
  end;
  'O','o': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'OR') then begin
      tokType := tkOperator;
      tokPrec := 4;
    end else tokType := tkIdentifier;
  end;
  'P','p': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'PROCEDURE') then begin
      tokType := tkKeyword;
    end else tokType := tkIdentifier;
  end;
  'T','t': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'TYPE') then begin
      tokType := tkKeyword;
    end else tokType := tkIdentifier;
  end;
  'U','u': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'UNTIL') then tokType := tkBlkDelim
    else tokType := tkIdentifier;
  end;
  'V','v': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'VAR') then tokType := tkKeyword
    else tokType := tkIdentifier;
  end;
  'X','x': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'XOR') then begin
      tokType := tkOperator;
      tokPrec := 4;
    end else tokType := tkIdentifier;
  end;
  'F'..'H','J'..'L','Q'..'S','W','Y','Z','_',
  'f'..'h','j'..'l','q'..'s','w','y','z': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    tokType := tkIdentifier;
  end;
  '+','-': begin
    _NextChar;
    if not _Eol and (curLine[fcol]='=') then begin  //+=, -=
      _NextChar;
      tokType := tkOperator;
      tokPrec := 2;
    end else begin   //+, -
      tokType := tkOperator;
      tokPrec := 4;
      tokPrecU := 6;
    end;
  end;
  '~': begin
    _NextChar;
    tokType := tkOperator;
    tokPrec := 4;
    tokPrecU := 6;
  end;
  '*': begin
    _NextChar;
    if not _Eol and (curLine[fcol]='=') then begin  //*=
      _NextChar;
      tokType := tkOperator;
      tokPrec := 2;
    end else if not _Eol and (curLine[fcol]='*') then begin  //**
      _NextChar;
      tokType := tkOperator;
      tokPrec := 6;
    end else begin  //*
      tokType := tkOperator;
      tokPrec := 5;
    end;
  end;
  '/': begin
    _NextChar;
    if not _Eol and (curLine[fcol]='/') then begin  //Comment
      repeat _NextChar until _Eol;
      tokType := tkComment;
    end else if not _Eol and (curLine[fcol]='=') then begin // /=
      _NextChar;
      tokType := tkOperator;
      tokPrec := 2;
    end else begin  // /
      tokType := tkOperator;
      tokPrec := 5;
    end;
  end;
  '\': begin  //Not Pascal standard operators.
    _NextChar;
    tokType := tkOperator;
    tokPrec := 5;
  end;
  '.': begin  //Not Pascal standard operators.
    _NextChar;
    tokType := tkOperator;
    tokPrec := 6;
  end;
  '=': begin
    _NextChar;
    tokType := tkOperator;
    tokPrec := 3;
  end;
  '@','^': begin  //Special operators
    _NextChar;
    tokType := tkOperator;
    tokPrec := 6;
    tokPrecU := 6;
  end;
  '>': begin
    _NextChar;
    if not _Eol and (curLine[fcol] = '=') then begin  // >=
      _NextChar;
      tokType := tkOperator;
      tokPrec := 3;
    end else if not _Eol and (curLine[fcol] = '>') then begin  //SHR
      _NextChar;
      tokType := tkOperator;
      tokPrec := 5;
    end else begin  //>
      tokType := tkOperator;
      tokPrec := 3;
    end;
  end;
  '<': begin
    _NextChar;
    if not _Eol and (curLine[fcol] in ['=','>']) then begin  //<=, <>
      _NextChar;
      tokType := tkOperator;
      tokPrec := 3;
    end else if not _Eol and (curLine[fcol] = '<') then begin  //SHL
      _NextChar;
      tokType := tkOperator;
      tokPrec := 5;
    end else begin  //<
      tokType := tkOperator;
      tokPrec := 3;
    end;
  end;
  ';': begin
    _NextChar;
    tokType := tkExpDelim;
  end;
  ':': begin
    _NextChar;
    if _ReadChar = '=' then begin  //:=
      _NextChar;
      tokType := tkOperator;
      tokPrec := 2;
    end else begin  // :
      tokType := tkOthers;
    end;
  end;
  '(',')',',','[',']': begin
    _NextChar;
    tokType := tkOthers;
  end;
  '''': begin
    repeat inc(fcol); until _Eol or (curline[fcol] = '''');
    if _Eol then begin
      onGenError('Unclosed string.');  //Don't stop scanning
    end else begin
      _NextChar;  //Go to next character
    end;
    tokType := tkString;
  end;
  '#': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['0'..'9']);
    tokType := tkChar;
  end;
  '{': begin
    _NextChar;
    if _ReadChar='$' then begin
      //Is directive
      repeat _NextChar;
      until _Eof or (_ReadChar = '}');
      if _Eof then begin
        onGenError('Unclosed directive.');  //Don't stop scanning
      end else begin
        _NextChar;  //Go to next character
      end;
      tokType := tkDirective;
    end else begin
      //Multiline comment
      repeat _NextChar;
      until _Eof or (_ReadChar = '}');
      if _Eof then begin
        onGenError('Unclosed comment.');  //Don't stop scanning
      end else begin
        _NextChar;  //Go to next character
      end;
      tokType := tkComment;
    end;
  end;
  else
    //Unkmown token.
    tokType := tkNull;
    _NextChar;
  end;
  exit(false);
end;
function TContext.Next: boolean;
{Decode the token in the current position of the cursor (frow,fcol) and returns:
- Start of the current token in (row0, col0).
- Token type in "tokType".
- Start of next token in (frow, fcol).
- Value TRUE if the current line has changed.
}
var
  tmp: String;
begin
  row0 := frow;
  col0 := fcol;
  if Empty then exit;
  if OnDecodeNext = nil then begin
    Result := DecodeNext;  //Decode token in (frow, fcol)
  end else begin
    //A hook has been introduced to the lexer
    Result := OnDecodeNext();  //Decode token in (frow, fcol)
  end;
end;
function TContext.ReadToken: string;
{Read the current token in "token", that is defined by (row0, col0) and (frow, fcol).}
var
  r: Integer;
  tmp: String;
  line: string;
begin
  if tokType in [tkNull, tkSpace, tkEol, tkComment] then begin
    //For optimization, returns empty in these token types.
    exit('');
  end;
  if row0 = frow then begin  //It's a single line token
    exit( copy(curLine, col0, (fcol-col0)) );
  end else begin //It's a multiline token
    line := curLines[row0-1];
    tmp := copy(line, col0, length(line) );
    for r:=row0+1 to frow-1 do begin
      tmp += LineEnding + curLines[r-1];
    end;
    line := curLines[frow-1];
    tmp += LineEnding + copy(line, 1, fcol);
    exit(tmp);
  end;
end;
procedure TContext.GetContextState(out c: TContextState);
begin
  c.idCtx := idCtx;
  //Scanner attributes
  c.frow    := frow;
  c.fcol    := fcol;
  c.curLine := curLine;
  c.curSize := curSize;
  //Adittional attributes
  c.row0    := row0;
  c.col0    := col0;
  c.tokType := tokType;
  c.tokPrec := tokPrec;
  c.tokPrecU:= tokPrecU;
end;
procedure TContext.SetContextState(const c: TContextState);
begin
  //idCtx   := c.idCtx;  Wouldn't be needed.
  //Scanner attributes
  frow    := c.frow;
  fcol    := c.fcol;
  curLine := c.curLine;
  curSize := c.curSize;
  //Adittional attributes
  row0    := c.row0;
  col0    := c.col0;
  tokType := c.tokType;
  tokPrec := c.tokPrec;
  tokPrecU:= c.tokPrecU;
end;
procedure TContext.SaveContextState;
//Guarda el estado actual del lexer en la variable interna "fLexerState".
//Este estado incluye las coordenadas actuales de lectura en el Lexer.
begin
  GetContextState(fLexerState);
end;
procedure TContext.RestoreContextState;
//Copia el estado del lexer grabado en "fLexerState". Se debe ejecutar siempre
//después de SaveLexerState().
begin
  SetContextState(fLexerState);
end;
function TContext.ReadSource: string;
//Devuelve el contenido del contexto en una cadena.
begin
  Result := curLines.text;
end;
procedure TContext.StartScan;
{Reset the scanner to start working.}
begin
  nlin := curLines.Count; //Number of lines of source.
  //Set cursor
  setRow(1);
  setCol(1);
  row0 := 1;
  col0 := 1;
  //Updates the first token.
  Next;
end;
//Métodos de inicialización
procedure TContext.SetSource(txt: string);
//Fija el contenido del contexto con una cadena. Puede ser de varias líneas.
begin
  typ := TC_TXT;          //indica que contenido es Texto
  //guarda en lista interna.
  if txt='' then begin
    //cadena vacía, crea una línea vacía
    intLines.Clear;
  end else begin
    intLines.Text := txt;
  end;
  curLines := intLines;  //Apunta a almacenamiento interno
  fileSrc := '';        //There is not file source defined.
  StartScan;            //Updates first cursor position.
end;
procedure TContext.SetSource(lins: Tstrings; MakeCopy: boolean = false);
//Fija el contenido del contexto con una lista TStringList. Usa la referencia, no copia.
begin
  typ := TC_TXT;         //indica que contenido es Texto
  if MakeCopy then begin  //crea copia
    intLines.Clear;
    intLines.AddStrings(lins); //carga líneas, de la lista
    curLines := intLines; //apunta a almacenamiento interno
  end else begin
    curLines := lins;    //apunta a la fuente externa. No la copia.
  end;
  fileSrc := '';        //There is not file source defined.
  StartScan;            //Updates first cursor position.
end;
procedure TContext.SetSourceF(file0: string);
//Fija el contenido del contexto con un archivo
begin
  typ := TC_ARC;         //indica que contenido es Texto
  intLines.LoadFromFile(file0);
  curLines := intLines;  //apunta a almacenamiento interno
  fileSrc := file0;     //Takes file name.
  StartScan;            //Updates first cursor position.
end;
constructor TContext.Create;
begin
inherited;   //solo se pone por seguridad, ya que no es necesario.
  intLines := TStringList.Create;    //crea lista de cadenas para almacenar el texto
  nlin := 0;
  SetSource('');  //para iniciar con algo en donde leer
end;
destructor TContext.Destroy;
begin
//  lex.Free;     //libera lexer
  intLines.Free;     //libera lista
  inherited Destroy;
end;

Initialization
  srcPosNull.idCtx := -1;

end.

