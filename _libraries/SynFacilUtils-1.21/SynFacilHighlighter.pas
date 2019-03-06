{                               TSynFacilSyn
Unidad principal de SynfacilSyn.

Queda pendiente incluir el procesamiento de los paréntesis en las expresiones regulares,
como una forma sencilla de definir bloques de Regex, sin tener que usar la definición
avanzada. También se podría ver si se puede mejorar el soporte de Regex, sobre todo para el
caso de expresiones como ".*a".


                                    Por Tito Hinostroza  15/09/2015 - Lima Perú
}
unit SynFacilHighlighter;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Graphics, SynEditHighlighter, DOM, XMLRead,
  Dialogs, Fgl, strings, Lazlogger, SynEditHighlighterFoldBase, LCLIntf,
  SynFacilBasic;
const
  COL_TRANSPAR = $FDFEFF;  //color transparente
type

  //Para manejo del plegado
  TFaListBlocks = specialize TFPGObjectList<TFaSynBlock>;   //lista de bloques

  //Descripción de token. Usado solamente para el trabajo del método ExploreLine()
  TFaTokInfo = record
     txt    : string;        //texto del token
     TokPos : integer;       //posición del token dentro de la línea
     TokTyp : integer;       //tipo de token
     IsIDentif: boolean;     //para saber si es identificador
     posIni : integer;       //posición de inicio en la línea
     length : integer;       //tamaño del token (en bytes)
     curBlk : TFaSynBlock;   //referencia al bloque del token
  end;
  TATokInfo = array of TFaTokInfo;

  //Permite leer el estado actual del resaltador. Considera la posición actual de la
  //exploración y el estado del rango, NO CONSIDERA el estado de los bloques de
  //plegado. Se usa cuando se hace trabajar al resaltador como analizador léxico.
  TFaLexerState = record
    //propiedades fijadas al inicio de la línea y no cambian en toda la línea.
//    fLine      : PChar;         //puntero a línea de trabajo.
//    tamLin     : integer;       //tamaño de línea actual
    LineText   : string;        //línea de trabajo
    LineIndex  : integer;       //el número de línea actual
    //propiedades que van cambiando conforme se avanza en la exploración de la línea
    posTok     : integer;       //para identificar el ordinal del token en una línea
    BlkToClose : TFaSynBlock;   //bandera-variable para posponer el cierre de un bloque
    posIni     : Integer;       //índice a inicio de token
    posFin     : Integer;       //índice a siguiente token
    fRange     : ^TTokSpec;    //para trabajar con tokens multilínea
    fTokenID   : integer;      //Id del token actual
  end;

  { TSynFacilSyn }

  TSynFacilSyn = class(TSynFacilSynBase)
  protected  //Variables internas
    lisBlocks  : TFaListBlocks; //lista de bloques de sintaxis
    delTok     : string;        //delimitador del bloque actual
    folTok     : boolean;       //indica si hay "folding" que cerrar en token delimitado actual
    chrEsc     : char;          //indica si hay caracter de escape en token delimitado actual (#0 si no hay)
    nTokenCon  : integer;       //cantidad de tokens por contenido
    fRange     : TPtrTokEspec;  //para trabajar con tokens multilínea
    CloseThisBlk: TFaSynBlock;   //bandera-variable para posponer el cierre de un bloque
    OnFirstTok : procedure of object;
    procedure SetTokContent(tc: tFaTokContent; dStart: string;
      TypDelim: TFaTypeDelim; typToken: integer);
    //Manejo de bloques
    procedure StartBlock(ABlockType: Pointer; IncreaseLevel: Boolean); inline;
    procedure EndBlock(DecreaseLevel: Boolean); inline;
    procedure StartBlockAndFirstSec(const blk, firstSec: TfaSynBlock);
    procedure StartBlockFa(const blk: TfaSynBlock);
    procedure EndBlockFa(const blk: TfaSynBlock);
    function TopBlock: TFaSynBlock;
    function TopBlockOpac: TFaSynBlock;
  protected  //Funciones de bajo nivel
    function CreaBuscIdeEspec(out mat: TPtrATokEspec; cad: string; out i: integer;
      TokPos: integer = 0): boolean;
    function CreaBuscSymEspec(out mat: TPtrATokEspec; cad: string; out i: integer;
      TokPos: integer = 0): boolean;
    function CreaBuscEspec(out tok: TPtrTokEspec; cad: string; TokPos: integer
      ): boolean;
    procedure TableIdent(iden: string; out mat: TPtrATokEspec; out
      met: TFaProcMetTable);
    procedure FirstXMLExplor(doc: TXMLDocument);
    function ProcXMLBlock(nodo: TDOMNode; blqPad: TFaSynBlock): boolean;
    function ProcXMLSection(nodo: TDOMNode; blqPad: TFaSynBlock): boolean;
  public    //funciones públicas de alto nivel
//    Err       : string;         //Mensaje de error
    LangName  : string;         //Nombre del lengauje
    Extensions: String;         //Extensiones de archivo
    MainBlk   : TFaSynBlock;    //Bloque global
    MulTokBlk : TFaSynBlock;    //Bloque reservado para bloques multitokens
    ColBlock  : TFaColBlock;    //Coloreado por bloques
    procedure ClearMethodTables; //Limpia la tabla de métodos
    //Definición de tokens por contenido
    function DefTokContent(dStart: string; typToken: integer): tFaTokContent;
    procedure DefTokContent(dStart, Content: string; typToken: integer;
      Complete: boolean = false);
    //Manejo de identificadores especiales
    procedure ClearSpecials;        //Limpia identif, y símbolos especiales
    procedure AddIdentSpec(iden: string; tokTyp: integer; TokPos: integer=0);
    procedure AddIdentSpecList(listIden: string; tokTyp: integer; TokPos: integer=0);
    procedure AddKeyword(iden: string);
    procedure AddSymbSpec(symb: string; tokTyp: integer; TokPos: integer=0);
    procedure AddSymbSpecList(listSym: string; tokTyp: integer; TokPos: integer=0);
    procedure DefTokDelim(dStart, dEnd: string; tokTyp: integer;
      tipDel: TFaTypeDelim=tdUniLin; havFolding: boolean=false; chrEscape: char=#0);
    procedure RebuildSymbols;
    procedure LoadFromStream(Stream: TStream); virtual;                                    //load highlighter from a stream
    procedure LoadFromResourceName(Instance: THandle; const ResName: String); virtual; //load highlighter from a resource
    procedure LoadFromFile(const Filename: string); virtual;                                      //Para cargar sintaxis
    procedure Rebuild; virtual;

    procedure AddIniBlockToTok(dStart: string; TokPos: integer; blk: TFaSynBlock);
    procedure AddFinBlockToTok(dEnd: string; TokPos: integer; blk: TFaSynBlock);
    procedure AddIniSectToTok(dStart: string; TokPos: integer; blk: TFaSynBlock);
    procedure AddFirstSectToTok(dStart: string; TokPos: integer; blk: TFaSynBlock);
    function CreateBlock(blkName: string; showFold: boolean=true;
      parentBlk: TFaSynBlock=nil): TFaSynBlock;
    function AddBlock(dStart, dEnd: string; showFold: boolean=true;
      parentBlk: TFaSynBlock=nil): TFaSynBlock;
    function AddSection(dStart: string; showFold: boolean=true;
      parentBlk: TFaSynBlock=nil): TFaSynBlock;
    function AddFirstSection(dStart: string; showFold: boolean=true;
      parentBlk: TFaSynBlock=nil): TFaSynBlock;
    //Funciones para obtener información de bloques
    function SearchBlock(blk: string; out Success: boolean): TFaSynBlock;
    function NestedBlocks: Integer;
    function NestedBlocksBegin(LineNumber: integer): Integer;
    function SearchBeginBlock(level: integer; PosY: integer): integer;
    function SearchEndBlock(level: integer; PosY: integer): integer;
    procedure SearchBeginEndBlock(level: integer; PosX, PosY: integer; out
      pIniBlock, pEndBlock: integer);
    function TopCodeFoldBlock(DownIndex: Integer=0): TFaSynBlock;
    function SetHighlighterAtXY(XY: TPoint): boolean;
    function ExploreLine(XY: TPoint; out toks: TATokInfo; out CurTok: integer
      ): boolean;
    procedure GetBlockInfoAtXY(XY: TPoint; out blk: TFaSynBlock; out level: integer
      );
    function GetBlockInfoAtXY(XY: TPoint; out blk: TFaSynBlock; out
      BlockStart: TPoint; out BlockEnd: TPoint): boolean;
  private   //procesamiento de identificadores especiales
    //métodos para identificadores especiales
    procedure metA;
    procedure metB;
    procedure metC;
    procedure metD;
    procedure metE;
    procedure metF;
    procedure metG;
    procedure metH;
    procedure metI;
    procedure metJ;
    procedure metK;
    procedure metL;
    procedure metM;
    procedure metN;
    procedure metO;
    procedure metP;
    procedure metQ;
    procedure metR;
    procedure metS;
    procedure metT;
    procedure metU;
    procedure metV;
    procedure metW;
    procedure metX;
    procedure metY;
    procedure metZ;
    procedure metA_;
    procedure metB_;
    procedure metC_;
    procedure metD_;
    procedure metE_;
    procedure metF_;
    procedure metG_;
    procedure metH_;
    procedure metI_;
    procedure metJ_;
    procedure metK_;
    procedure metL_;
    procedure metM_;
    procedure metN_;
    procedure metO_;
    procedure metP_;
    procedure metQ_;
    procedure metR_;
    procedure metS_;
    procedure metT_;
    procedure metU_;
    procedure metV_;
    procedure metW_;
    procedure metX_;
    procedure metY_;
    procedure metZ_;
    procedure metUnd;
    procedure metDol;
    procedure metArr;
    procedure metPer;
    procedure metAmp;
    procedure metC3;
  protected   //procesamiento de otros elementos
    procedure ProcTokenDelim(const d: TTokSpec);
    procedure metIdentEsp(var mat: TArrayTokSpec);
    procedure metSimbEsp;
    //funciones rápidas para la tabla de métodos (símbolos especiales)
    procedure metSym1Car;
    //funciones rápidas para la tabla de métodos (tokens delimitados)
    procedure metUniLin1;
    procedure metFinLinea;
    //funciones llamadas en medio de rangos
    procedure ProcEndLine;
    procedure ProcRangeEndSym;
    procedure ProcRangeEndSym1;
    procedure ProcRangeEndIden;
  private   //Utilidades para analizador léxico
    function GetState: TFaLexerState;
    procedure SetState(state: TFaLexerState);
  public //Utilidades para analizador léxico
    function GetX: Integer; inline; //devuelve la posición X actual del resaltador
    function GetY: Integer; inline; //devuelve la posición Y actual del resaltador
    function GetXY: TPoint;  //devuelve la posición actual del resaltador
    property Range: TPtrTokEspec read fRange write fRange;
    property State: TFaLexerState read GetState write SetState;
  public     //métodos OVERRIDE
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;
    function  GetEol: Boolean; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetToken: String; override;
    function GetTokenPos: Integer; override;
    function GetTokenKind: integer; override;
    procedure ResetRange; override;
    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
{//published   //Se crean accesos a las propiedades
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifAttri write fIdentifAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property KeywordAttri: TSynHighlighterAttributes read fKeywordAttri write fKeywordAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;}
  end;

implementation

uses
  Resource;

const
{
    ERR_IDENTIF_EMPTY = 'Identificador vacío.';
    ERR_INVAL_CHAR_IDEN_ = 'Caracter no válido para Identificador: ';
    ERR_IDENTIF_EXIST = 'Ya existe identificador: ';
    ERR_EMPTY_SYMBOL = 'Símbolo vacío';
    ERR_EMPTY_IDENTIF = 'Identificador vacío';
    ERR_SYMBOL_EXIST = 'Ya existe símbolo.';
    ERR_MUST_DEF_CHARS = 'Debe indicarse atributo "CharsStart=" en etiqueta <IDENTIFIERS ...>';
    ERR_MUST_DEF_CONT = 'Debe indicarse atributo "Content=" en etiqueta <IDENTIFIERS ...>';
    ERR_INVAL_LAB_BLK = 'Etiqueta "%S" no válida para etiqueta <BLOCK ...>';
    ERR_INVAL_LAB_TOK = 'Invalid label "%s" for <TOKEN ...>';
    ERR_INVAL_LAB_SEC = 'Etiqueta "%S" no válida para etiqueta <SECTION ...>';
    ERR_INCOMP_TOK_DEF_ = 'Definición incompleta de token: ';
    ERR_UNKNOWN_LABEL = 'Etiqueta no reconocida <%s>;
    ERR_INVAL_LBL_IDEN = 'Etiqueta "%s" no válida para etiqueta <IDENTIFIERS ...>';
    ERR_INVAL_LBL_IN_LBL = 'Etiqueta "%s" no válida para etiqueta <SYMBOLS ...>';
    ERR_BLK_NO_DEFINED = 'No se encuentra definido el bloque: ';
    ERR_MAX_NUM_TOKCON = 'Máximo número de tokens por contenido superado';
    ERR_UNKNOWN_ATTRIB = 'Atributo "%s" no existe.'
    ERROR_LOADING_ = 'Error cargando: ';
  }
    ERR_IDENTIF_EMPTY = 'Empty identifier.';
    ERR_INVAL_CHAR_IDEN_ = 'Invalid character for identifier: ';
    ERR_IDENTIF_EXIST = 'Identifier already exists: ';
    ERR_EMPTY_SYMBOL = 'Empty Symbol';
    ERR_EMPTY_IDENTIF = 'Empty Identifier';
    ERR_SYMBOL_EXIST = 'Symbol already exists.';
    ERR_MUST_DEF_CHARS = 'It must be indicated "CharsStart=" in label <IDENTIFIERS ...>';
    ERR_MUST_DEF_CONT = 'It must be indicated "Content=" in label <IDENTIFIERS ...>';
    ERR_INVAL_LAB_BLK = 'Invalid label "%s" for <BLOCK ...>';
    ERR_INVAL_LAB_TOK = 'Invalid label "%s" for <TOKEN ...>';
    ERR_INVAL_LAB_SEC = 'Invalid label "%s" for <SECTION ...>';
    ERR_INCOMP_TOK_DEF_ = 'Incomplete token definition: ';
    ERR_UNKNOWN_LABEL = 'Unknown label <%s>';
    ERR_INVAL_LBL_IDEN = 'Invalid label "%s", for label <IDENTIFIERS ...>';
    ERR_INVAL_LBL_IN_LBL = 'Invalid label "%s", for label <SYMBOLS ...>';
    ERR_BLK_NO_DEFINED = 'Undefined block: ';
    ERR_MAX_NUM_TOKCON = 'Maximun numbers of tokens by Content Added.';
    ERR_UNKNOWN_ATTRIB = 'Attribute "%s" doesn''t exist.';
    ERROR_LOADING_ = 'Error loading: ';

  { TSynFacilSyn }

//**************** Funciones de bajo nivel ****************
function TSynFacilSyn.CreaBuscIdeEspec(out mat: TPtrATokEspec; cad: string;
                                       out i:integer; TokPos: integer = 0): boolean;
{Busca o crea el identificador especial indicado en "cad". Si ya existe, devuelve
 TRUE, y actualiza "i" con su posición. Si no existe, crea el token especial y devuelve
 la referencia en "i". En "mat" devuelve la referencia a la tabla que corresponda al
 identificador. Puede generar una excepción si el identificador no empieza con un
 caracter válido}
var
  met: TFaProcMetTable;
  c: Char;
begin
  Result := false;  //valor por defecto
  TableIdent(cad, mat, met);  //busca tabla y método (Puede generar excepción)
  //Verifica si existe
  if CreaBuscTokEspec(mat^, copy(cad,2,length(cad)), i, TokPos) then begin
    exit(true);  //Ya existe
  end;
  //No existía, pero se creó. Ahora hay que actualizar la tabla de métodos
  mat^[i].orig:=cad;  //guarda el identificador original
  c := cad[1]; //primer caracter
  if CaseSensitive then begin //sensible a la caja
    fProcTable[c] := met;
  end else begin
    fProcTable[LowerCase(c)] := met;
    fProcTable[UpCase(c)] := met;
  end;
end;
function TSynFacilSyn.CreaBuscSymEspec(out mat: TPtrATokEspec; cad: string;
                                       out i:integer; TokPos: integer = 0): boolean;
{Busca o crea el símbolo especial indicado en "cad". Si ya existe, devuelve TRUE, y
 actualiza "i" con su posición. Si no existe, crea el token especial y devuelve la referencia
 en "i". En "mat" devuelve la referencia a la tabla que corresponda al símbolo (por ahora
 siempre será mSymb0).}
begin
  Result := false;  //valor por defecto
  mat := @mSym0;  //no hace falta buscarlo
  //Verifica si existe
  if CreaBuscTokEspec(mSym0, cad, i, TokPos) then
    exit(true);  //Ya existe.
  //No existía, pero se creó.
end;
function TSynFacilSyn.CreaBuscEspec(out tok: TPtrTokEspec; cad: string;
                                          TokPos: integer): boolean;
{Busca o crea un token especial (identificador o símbolo), con texto "cad" y posición en
 "TokPos". Si ya existe, devuelve TRUE, y su referencia en "tok". Si no existe, crea el
 token especial y devuelve su referencia en "tok". Puede generar excepción.}
var
  mat: TPtrATokEspec;
  i: integer;
begin
  if cad[1] in charsIniIden then begin  //delimitador es identificador
    Result := CreaBuscIdeEspec(mat, cad, i, TokPos); //busca o crea
    if not Result then
      mat^[i].tTok:=tnIdentif;  //es token nuevo, hay que darle atributo por defecto
  end else begin   //el delimitador inicial es símbolo
    Result := CreaBuscSymEspec(mat, cad, i, TokPos);  //busca o crea
    if not Result then
      mat^[i].tTok:=tnSymbol;  //es token nuevo, hay que darle atributo por defecto
  end;
  tok := @mat^[i];   //devuelve referencia a token especial
end;

procedure TSynFacilSyn.TableIdent(iden: string; out mat: TPtrATokEspec;
  out met: TFaProcMetTable);
{Devuelve una referencia a la tabla que corresponde a un identificador y el método
que debe procesarlo. Si no encuentra una tabla apropiada para el identificador
(caracter inicial no válido) genera una excepción}
var
  c: char;
begin
  if iden = '' then raise ESynFacilSyn.Create(ERR_IDENTIF_EMPTY);
  c := iden[1]; //primer caracter
  mat :=nil; met := nil;   //valores por defecto
  if CaseSensitive then begin //sensible a la caja
    case c of
    'A': begin mat:= @mA;  met := @metA; end;
    'B': begin mat:= @mB;  met := @metB; end;
    'C': begin mat:= @mC;  met := @metC; end;
    'D': begin mat:= @mD;  met := @metD; end;
    'E': begin mat:= @mE;  met := @metE; end;
    'F': begin mat:= @mF;  met := @metF; end;
    'G': begin mat:= @mG;  met := @metG; end;
    'H': begin mat:= @mH;  met := @metH; end;
    'I': begin mat:= @mI;  met := @metI; end;
    'J': begin mat:= @mJ;  met := @metJ; end;
    'K': begin mat:= @mK;  met := @metK; end;
    'L': begin mat:= @mL;  met := @metL; end;
    'M': begin mat:= @mM;  met := @metM; end;
    'N': begin mat:= @mN;  met := @metN; end;
    'O': begin mat:= @mO;  met := @metO; end;
    'P': begin mat:= @mP;  met := @metP; end;
    'Q': begin mat:= @mQ;  met := @metQ; end;
    'R': begin mat:= @mR;  met := @metR; end;
    'S': begin mat:= @mS;  met := @metS; end;
    'T': begin mat:= @mT;  met := @metT; end;
    'U': begin mat:= @mU;  met := @metU; end;
    'V': begin mat:= @mV;  met := @metV; end;
    'W': begin mat:= @mW;  met := @metW; end;
    'X': begin mat:= @mX;  met := @metX; end;
    'Y': begin mat:= @mY;  met := @metY; end;
    'Z': begin mat:= @mZ;  met := @metZ; end;
    'a': begin mat:= @mA_; met := @metA_;end;
    'b': begin mat:= @mB_; met := @metB_;end;
    'c': begin mat:= @mC_; met := @metC_;end;
    'd': begin mat:= @mD_; met := @metD_;end;
    'e': begin mat:= @mE_; met := @metE_;end;
    'f': begin mat:= @mF_; met := @metF_;end;
    'g': begin mat:= @mG_; met := @metG_;end;
    'h': begin mat:= @mH_; met := @metH_;end;
    'i': begin mat:= @mI_; met := @metI_;end;
    'j': begin mat:= @mJ_; met := @metJ_;end;
    'k': begin mat:= @mK_; met := @metK_;end;
    'l': begin mat:= @mL_; met := @metL_;end;
    'm': begin mat:= @mM_; met := @metM_;end;
    'n': begin mat:= @mN_; met := @metN_;end;
    'o': begin mat:= @mO_; met := @metO_;end;
    'p': begin mat:= @mP_; met := @metP_;end;
    'q': begin mat:= @mQ_; met := @metQ_;end;
    'r': begin mat:= @mR_; met := @metR_;end;
    's': begin mat:= @mS_; met := @metS_;end;
    't': begin mat:= @mT_; met := @metT_;end;
    'u': begin mat:= @mU_; met := @metU_;end;
    'v': begin mat:= @mV_; met := @metV_;end;
    'w': begin mat:= @mW_; met := @metW_;end;
    'x': begin mat:= @mX_; met := @metX_;end;
    'y': begin mat:= @mY_; met := @metY_;end;
    'z': begin mat:= @mZ_; met := @metZ_;end;
    //adicionales
    '_': begin mat:= @m_  ;met := @metUnd;end;
    '$': begin mat:= @mDol;met := @metDol;  end;
    '@': begin mat:= @mArr;met := @metArr;  end;
    '%': begin mat:= @mPer;met := @metPer;  end;
    '&': begin mat:= @mAmp;met := @metAmp;  end;
    end;
  end else begin  //no es sensible a la caja
    case c of
    'A','a': begin mat:= @mA;  met:= @metA;  end;
    'B','b': begin mat:= @mB;  met:= @metB; end;
    'C','c': begin mat:= @mC;  met:= @metC; end;
    'D','d': begin mat:= @mD;  met:= @metD; end;
    'E','e': begin mat:= @mE;  met:= @metE; end;
    'F','f': begin mat:= @mF;  met:= @metF; end;
    'G','g': begin mat:= @mG;  met:= @metG; end;
    'H','h': begin mat:= @mH;  met:= @metH; end;
    'I','i': begin mat:= @mI;  met:= @metI; end;
    'J','j': begin mat:= @mJ;  met:= @metJ; end;
    'K','k': begin mat:= @mK;  met:= @metK; end;
    'L','l': begin mat:= @mL;  met:= @metL; end;
    'M','m': begin mat:= @mM;  met:= @metM; end;
    'N','n': begin mat:= @mN;  met:= @metN; end;
    'O','o': begin mat:= @mO;  met:= @metO; end;
    'P','p': begin mat:= @mP;  met:= @metP; end;
    'Q','q': begin mat:= @mQ;  met:= @metQ; end;
    'R','r': begin mat:= @mR;  met:= @metR; end;
    'S','s': begin mat:= @mS;  met:= @metS; end;
    'T','t': begin mat:= @mT;  met:= @metT; end;
    'U','u': begin mat:= @mU;  met:= @metU; end;
    'V','v': begin mat:= @mV;  met:= @metV; end;
    'W','w': begin mat:= @mW;  met:= @metW; end;
    'X','x': begin mat:= @mX;  met:= @metX; end;
    'Y','y': begin mat:= @mY;  met:= @metY; end;
    'Z','z': begin mat:= @mZ;  met:= @metZ; end;
    '_'    : begin mat:= @m_  ;met:= @metUnd;end;
    '$'    : begin mat:= @mDol;met:= @metDol;end;
    '@'    : begin mat:= @mArr;met:= @metArr;end;
    '%'    : begin mat:= @mPer;met:= @metPer;end;
    '&'    : begin mat:= @mAmp;met:= @metAmp;end;
    #$C3   : begin mat:= @mC3; met:= @metC3; end;  //página 195 de UTF-8
    end;
  end;
  //verifica error
  if mat = nil then begin
    raise ESynFacilSyn.Create(ERR_INVAL_CHAR_IDEN_+iden);
  end;
end;
procedure TSynFacilSyn.SetTokContent(tc: tFaTokContent; dStart: string;
   TypDelim: TFaTypeDelim; typToken: integer);
//Configura la definición de un token por contenido. De ser así devuelve TRUE y
//actualiza la tabla de métodos con el método indicado. Puede generar excepción.
var
  tmp: string;
  tok: TPtrTokEspec;
begin
  ValidateParamStart(dStart, lisTmp);   //Valida parámetro "dStart", y devuelve en lista.
  tc.TokTyp:= typToken;  //atributo inicial
  tc.CaseSensitive := CaseSensitive;  //toma el mismo comportamiento de caja
  /////// Configura detección de inicio
  {Si es rango de caracteres, agrega cada caracter como símbolo especial, aunque parezca
  ineficiente. Pero de esta forma se podrán procesar tokens por contenido que empiecen
  con el mismo caracter. Además, de ser posible, la función Rebuild() optimizará luego
  el procesamiento.}
  for tmp in lisTmp do begin
    CreaBuscEspec(tok, tmp, 0);  //busca o crea
    //actualiza sus campos. Cambia, si ya existía
    tok^.tTok:=typToken;   //no se espera usar este campo, sino  "tc.TokTyp"
    tok^.typDel:=TypDelim;  //solo es necesario marcarlo como que es por contenido
  end;
end;
procedure TSynFacilSyn.ClearMethodTables;
{Limpia la tabla de métodos, usada para identificar a los tokens de la sintaxis.
 También limpia las definiciones de tokens por contenido.
 Proporciona una forma rápida de identificar la categoría de token.}
var i: Char;
begin
  lisBlocks.Clear;   //inicia lista de bloques
  nTokenCon := 0;    //inicia contador de tokens por contenido
  tc1.Clear;
  tc2.Clear;
  tc3.Clear;
  tc4.Clear;
  for i := #0 to #255 do
    case i of
      //caracteres blancos, son fijos
      #1..#32 : fProcTable[i] := @metSpace;
      //fin de línea
      #0      : fProcTable[i] := @metNull;   //Se lee el caracter de marca de fin de cadena
      else //los otros caracteres (alfanuméricos o no)
        fProcTable[i] := @metSymbol;  //se consideran símbolos
    end;
end;
//definición de tokens por contenido
function TSynFacilSyn.DefTokContent(dStart: string;
  typToken: integer): tFaTokContent;
{Crea un token por contenido, y devuelve una referencia al token especial agregado.
Se debe haber limpiado previamente la tabla de métodos con "ClearMethodTables"
Solo se permite definir hasta 4 tokens por contenido. Puede generar excepción}
begin
  if nTokenCon = 0 then begin       //está libre el 1
    SetTokContent(tc1, dStart, tdConten1, typToken);
    Result := tc1;   //devuelve referencia
    inc(nTokenCon);
  end else if nTokenCon = 1 then begin //está libre el 2
    SetTokContent(tc2, dStart, tdConten2, typToken);
    Result := tc2;   //devuelve referencia
    inc(nTokenCon);
  end else if nTokenCon = 2 then begin //está libre el 3
    SetTokContent(tc3, dStart, tdConten3, typToken);
    Result := tc3;   //devuelve referencia
    inc(nTokenCon);
  end else if nTokenCon = 3 then begin //está libre el 4
    SetTokContent(tc4, dStart, tdConten4, typToken);
    Result := tc4;   //devuelve referencia
    inc(nTokenCon);
  end else begin //las demás declaraciones, generan error
    raise ESynFacilSyn.Create(ERR_MAX_NUM_TOKCON);
  end;
end;
procedure TSynFacilSyn.DefTokContent(dStart, Content: string;
  typToken: integer; Complete:boolean = false);
{Versión simplificada para crear tokens por contenido sencillos. El parámetro
"Content", se debe ingresar com expresión regular. Un ejemplo sencillo sería:
  hlt.DefTokContent('[0-9]','[0-9]*');
Se debe haber limpiado previamente la tabla de métodos con "ClearMethodTables"
Solo se permite definir hasta 4 tokens}
var
  p: tFaTokContent;
begin
  p := DefTokContent(dStart, typToken);
  p.AddRegEx(Content, Complete);   //agrega contenido como expresión regular
end;
//manejo de identificadores y símbolos especiales
procedure TSynFacilSyn.ClearSpecials;
//Limpia la lista de identificadores especiales y de símbolos delimitadores.
begin
  //ídentificadores
  SetLength(mA,0); SetLength(mB,0); SetLength(mC,0); SetLength(mD,0);
  SetLength(mE,0); SetLength(mF,0); SetLength(mG,0); SetLength(mH,0);
  SetLength(mI,0); SetLength(mJ,0); SetLength(mK,0); SetLength(mL,0);
  SetLength(mM,0); SetLength(mN,0); SetLength(mO,0); SetLength(mP,0);
  SetLength(mQ,0); SetLength(mR,0); SetLength(mS,0); SetLength(mT,0);
  SetLength(mU,0); SetLength(mV,0); SetLength(mW,0); SetLength(mX,0);
  SetLength(mY,0); SetLength(mZ,0);
  SetLength(mA_,0); SetLength(mB_,0); SetLength(mC_,0); SetLength(mD_,0);
  SetLength(mE_,0); SetLength(mF_,0); SetLength(mG_,0); SetLength(mH_,0);
  SetLength(mI_,0); SetLength(mJ_,0); SetLength(mK_,0); SetLength(mL_,0);
  SetLength(mM_,0); SetLength(mN_,0); SetLength(mO_,0); SetLength(mP_,0);
  SetLength(mQ_,0); SetLength(mR_,0); SetLength(mS_,0); SetLength(mT_,0);
  SetLength(mU_,0); SetLength(mV_,0); SetLength(mW_,0); SetLength(mX_,0);
  SetLength(mY_,0); SetLength(mZ_,0);
  SetLength(m_,0); SetLength(mDol,0); SetLength(mArr,0);
  SetLength(mPer,0); SetLength(mAmp,0); SetLength(mC3,0);
  //símbolos
  SetLength(mSym,0);
  SetLength(mSym0,0);  //limpia espacio temporal
end;
procedure TSynFacilSyn.AddIdentSpec(iden: string; tokTyp: integer;
  TokPos: integer);
//Método público para agregar un identificador especial cualquiera.
//Si el identificador no inicia con caracter válido, o ya existe, genera una excepción.
var i: integer;
    mat: TPtrATokEspec;
begin
  if iden = '' then raise ESynFacilSyn.Create(ERR_EMPTY_IDENTIF);
  //Verifica si existe
  if CreaBuscIdeEspec(mat, iden, i, TokPos) then begin  //puede generar excepción
    //Genera error, porque el identif. ya existe
    raise ESynFacilSyn.Create(ERR_IDENTIF_EXIST+iden);
  end;
  //se ha creado uno nuevo
  mat^[i].tTok:=tokTyp;  //solo cambia atributo
end;
procedure TSynFacilSyn.AddIdentSpecList(listIden: string; tokTyp: integer;
  TokPos: integer);
//Permite agregar una lista de identificadores especiales separados por espacios.
//Puede gernerar excepción, si algún identificador está duplicado o es erróneo.
var
  iden   : string;
  i      : integer;
begin
  //Carga identificadores
  lisTmp.Clear;
  lisTmp.Delimiter := ' ';
  //StringReplace(listIden, #13#10, ' ',[rfReplaceAll]);
  lisTmp.DelimitedText := listIden;
  for i:= 0 to lisTmp.Count -1 do
    begin
      iden := trim(lisTmp[i]);
      if iden = '' then continue;
      AddIdentSpec(iden, tokTyp, TokPos);  //puede generar excepción
    end;
end;
procedure TSynFacilSyn.AddKeyword(iden: string);
//Método público que agrega un identificador "Keyword" a la sintaxis
//Si el identificador es erróneso (caracter inicial no válido) o ya existe, genera
//una excepción.
begin
  AddIdentSpec(iden, tnKeyword);
end;
procedure TSynFacilSyn.AddSymbSpec(symb: string; tokTyp: integer;
  TokPos: integer);
//Método público para agregar un símbolo especial cualquiera.
//Si el símbolo ya existe, genera una excepción.
var i: integer;
    mat: TPtrATokEspec;
begin
  if symb = '' then raise ESynFacilSyn.Create(ERR_EMPTY_SYMBOL);
  //Verifica si existe
  if CreaBuscSymEspec(mat, symb, i, TokPos) then begin //busca o crea
    //Genera error, porque el símbolo. ya existe
    raise ESynFacilSyn.Create(ERR_SYMBOL_EXIST);
  end;
  //se ha creado uno nuevo
  mat^[i].tTok:=tokTyp;  //solo cambia atributo
end;
procedure TSynFacilSyn.AddSymbSpecList(listSym: string; tokTyp: integer;
  TokPos: integer);
//Permite agregar una lista de símbolos especiales separados por espacios.
//Puede gernerar excepción, si algún símbolo está duplicado.
var
  iden   : string;
  i      : integer;
begin
  //Carga identificadores
  lisTmp.Clear;
  lisTmp.Delimiter := ' ';
  //StringReplace(listSym, #13#10, ' ',[rfReplaceAll]);
  lisTmp.DelimitedText := listSym;
  for i:= 0 to lisTmp.Count -1 do
    begin
      iden := trim(lisTmp[i]);
      if iden = '' then continue;
      AddSymbSpec(iden, tokTyp, TokPos);  //puede generar excepción
    end;
end;
//definición de tokens delimitados
procedure TSynFacilSyn.DefTokDelim(dStart, dEnd: string; tokTyp: integer;
  tipDel: TFaTypeDelim; havFolding: boolean; chrEscape: char);
{Función genérica para agregar un token delimitado a la sintaxis. Si encuentra error,
genera una excepción}
var
  tok  : TPtrTokEspec;
  tmp, tmpnew: String;
  procedure ActProcRange(var r: TTokSpec);
  //Configura el puntero pRange() para la función apropiada de acuerdo al delimitador final.
  begin
    if r.typDel = tdNull then begin  //no es delimitador
      r.pRange:=nil;
      exit;
    end;
    if r.dEnd = '' then exit;
    if r.dEnd = #13 then begin   //Como comentario de una línea
      //no puede ser multilínea
      r.pRange := @ProcEndLine;
      exit;
    end;
    //los siguientes casos pueden ser multilínea
    if r.dEnd[1] in charsIniIden then begin //es identificador
      r.pRange:=@ProcRangeEndIden;
    end else begin  //es símbolo
      if length(r.dEnd) = 1 then begin
        r.pRange:=@ProcRangeEndSym1;  //es más óptimo
      end else begin
        r.pRange:=@ProcRangeEndSym;
      end;
    end;
  end;
begin
  if dEnd='' then dEnd := #13;  //no se permite delimitador final vacío
  ValidateParamStart(dStart, lisTmp);
  dEnd := ReplaceEscape(dEnd);  //convierte secuencias de escape
  VerifDelim(dEnd);
  //configura token especial
  for tmp in lisTmp do begin
    if (tmp<>'') and (tmp[1]='^') then begin
      tmpnew := copy(tmp,2,length(tmp));
      CreaBuscEspec(tok, tmpnew, 1); //busca o crea
    end else begin
      if copy(tmp,1,2) = '\^' then begin  //caracter escapado
        tmpnew := '^' + copy(tmp,3,length(tmp));
        CreaBuscEspec(tok, tmpnew, 0); //busca o crea
      end else begin
        CreaBuscEspec(tok, tmp, 0); //busca o crea
      end;
    end;
    //actualiza sus campos. Cambia, si ya existía
    tok^.dEnd  :=dEnd;
    tok^.typDel:=tipDel;
    tok^.tTok  :=tokTyp;
    tok^.folTok:=havFolding;
    tok^.chrEsc:=chrEscape;
    ActProcRange(tok^);  //completa .pRange()
  end;
end;
procedure TSynFacilSyn.RebuildSymbols;
{Crea entradas en la tabla de métodos para procesar los caracteres iniciales de los
 símbolos especiales. Así se asegura que se detectarán siempre.
 Se debe llamar después de haber agregado los símbolos especiales a mSym[]}
var
  i,j   : integer;
  maximo: integer;
  aux   : TTokSpec;
  c     : char;
begin
  {Ordena mSym[], poniendo los de mayor tamaño al inicio, para que la búsqueda considere
   primero a los símbolos de mayor tamaño}
  maximo := High(mSym);
  for i:=0 to maximo-1 do
    for j:=i+1 to maximo do begin
      if (length(mSym[i].txt) < length(mSym[j].txt)) then begin
        aux:=mSym[i];
        mSym[i]:=mSym[j];
        mSym[j]:=aux;
      end;
    end;
  //muestra los símbolos especiales que existen
  {$IFDEF DebugMode}
  DebugLn('------ delimitadores símbolo, ordenados --------');
  for tokCtl in mSym do DebugLn('  delim: '+ tokCtl.cad );
  DebugLn('---------actualizando tabla de funciones----------');
  {$ENDIF}
  {Captura los caracteres válidos para delimitadores y asigna las funciones
   para el procesamiento de delimitadores, usando el símbolo inicial}
  i := 0;
  while i <= High(mSym) do begin
    c := mSym[i].txt[1];   //toma primer caracter
    if fProcTable[c] <> @metSimbEsp then begin  //prepara procesamiento de delimitador
      {$IFDEF DebugMode}
      DebugLn('  puntero a funcion en: [' + c + '] -> @metSimbEsp');
      {$ENDIF}
      fProcTable[c] := @metSimbEsp;  //prepara procesamiento de delimitador
    end;
    { Para hacerlo más óptimo se debería usar una matriz para cada símbolo, de
     la misma forma a como se hace con los identificadores.}
    inc(i);
  end;
end;
procedure TSynFacilSyn.FirstXMLExplor(doc: TXMLDocument);
{Hace la primera exploración al archivo XML, para procesar la definición de Symbolos
 e Identificadores. Si encuentra algún error, genera una excepción.
 Si no encuentra definición de Identificadores, crea una definición por defecto}
var
  nodo, atri   : TDOMNode;
  i,j            : integer;
  nombre         : string;
  defIDENTIF     : boolean;  //bandera
  tipTok         : integer;
  tExt, tName, tCasSen, tColBlk: TFaXMLatrib;
  tCharsStart, tContent, tAtrib: TFaXMLatrib;
  tTokPos: TFaXMLatrib;
begin
  defIDENTIF := false;
//  defSIMBOLO := false;
  //////////// explora atributos del lenguaje//////////
  tExt  := ReadXMLParam(doc.DocumentElement, 'Ext');
  tName := ReadXMLParam(doc.DocumentElement, 'Name');
  tCasSen :=ReadXMLParam(doc.DocumentElement, 'CaseSensitive');
  tColBlk :=ReadXMLParam(doc.DocumentElement, 'ColorBlock');
  //carga atributos leidos
  CheckXMLParams(doc.DocumentElement, 'Ext Name CaseSensitive ColorBlock');
  LangName := tName.val;
  Extensions := tExt.val;
  CaseSensitive := tCasSen.bol;
  case UpCase(tColBlk.val) of  //coloreado de bloque
  'LEVEL': ColBlock := cbLevel;
  'BLOCK': ColBlock := cbBlock;
  else ColBlock:= cbNull;
  end;

  ////////////// explora nodos ////////////
  for i:= 0 to doc.DocumentElement.ChildNodes.Count - 1 do begin
     // Lee un Nodo o Registro
     nodo := doc.DocumentElement.ChildNodes[i];
     nombre := UpCase(AnsiString(nodo.NodeName));
     if nombre = 'IDENTIFIERS' then begin
       defIDENTIF := true;      //hay definición de identificadores
       ////////// Lee parámetros //////////
       tCharsStart  := ReadXMLParam(nodo,'CharsStart');
       tContent:= ReadXMLParam(nodo,'Content');
       CheckXMLParams(nodo, 'CharsStart Content'); //valida
       ////////// verifica los atributos indicados
       if tCharsStart.hay and tContent.hay then  //lo normal
         DefTokIdentif(ToListRegex(tCharsStart), ToListRegex(tContent)+'*')   //Fija caracteres
       else if not tCharsStart.hay and not tContent.hay then  //etiqueta vacía
         DefTokIdentif('[A-Za-z$_]', '[A-Za-z0-9_]*')  //def. por defecto
       else if not tCharsStart.hay  then
         raise ESynFacilSyn.Create(ERR_MUST_DEF_CHARS)
       else if not tContent.hay  then
         raise ESynFacilSyn.Create(ERR_MUST_DEF_CONT);
       ////////// explora nodos hijos //////////
       for j := 0 to nodo.ChildNodes.Count-1 do begin
         atri := nodo.ChildNodes[j];
         nombre := UpCase(AnsiString(atri.NodeName));
         if nombre = 'TOKEN' then begin  //definición completa
           //lee atributos
           tAtrib:= ReadXMLParam(atri,'Attribute');
           tTokPos:= ReadXMLParam(atri,'TokPos');  //posición de token
           CheckXMLParams(atri, 'Attribute TokPos'); //valida
           tipTok := GetAttribIDByName(tAtrib.val);
           if tipTok = -1 then begin
             raise ESynFacilSyn.Create(Format(ERR_UNKNOWN_ATTRIB, [tAtrib.val]));
           end;
           //crea los identificadores especiales
           AddIdentSpecList(AnsiString(atri.TextContent), tipTok, tTokPos.n);
         end else if IsAttributeName(nombre) then begin  //definición simplificada
           //lee atributos
           tTokPos:= ReadXMLParam(atri,'TokPos');  //posición de token
           CheckXMLParams(atri, 'TokPos'); //valida
           //Crea los identificadores especiales
           AddIdentSpecList(AnsiString(atri.TextContent), GetAttribIDByName(nombre), tTokPos.n);
         end else if nombre = '#COMMENT' then begin
           //solo para evitar que de mensaje de error
         end else begin
           raise ESynFacilSyn.Create(Format(ERR_INVAL_LBL_IDEN, [atri.NodeName]));
         end;
       end;
     end else if nombre = 'SYMBOLS' then begin
//       defSIMBOLO := true;      //hay definición de símbolos
       ////////// Lee atributos, pero no se usan. Es solo protocolar.
       tCharsStart:= ReadXMLParam(nodo,'CharsStart');
       tContent   := ReadXMLParam(nodo,'Content');
       ////////// explora nodos hijos //////////
       for j := 0 to nodo.ChildNodes.Count-1 do begin
         atri := nodo.ChildNodes[j];
         nombre := UpCase(AnsiString(atri.NodeName));
         if nombre = 'TOKEN' then begin  //definición completa
           //lee atributos
           tAtrib := ReadXMLParam(atri,'Attribute');
           tTokPos:= ReadXMLParam(atri,'TokPos');  //posición de token
           CheckXMLParams(atri, 'Attribute TokPos'); //valida
           tipTok := GetAttribIDByName(tAtrib.val);
           //crea los símbolos especiales
           AddSymbSpecList(AnsiString(atri.TextContent), tipTok, tTokPos.n);
         end else if IsAttributeName(nombre) then begin  //definición simplificada
           //lee atributos
           tTokPos:= ReadXMLParam(atri,'TokPos');  //posición de token
           CheckXMLParams(atri, 'TokPos'); //valida
           //crea los símbolos especiales
           AddSymbSpecList(AnsiString(atri.TextContent), GetAttribIDByName(nombre), tTokPos.n);
         end else if nombre = '#COMMENT' then begin
           //solo para evitar que de mensaje de error
         end else begin
           raise ESynFacilSyn.Create(Format(ERR_INVAL_LBL_IN_LBL, [atri.NodeName]));
         end;
       end;
     end else if nombre = 'SAMPLE' then begin  //Cödigo de muestra
       fSampleSource := AnsiString(nodo.TextContent);  //Carga texto
     end else if ProcXMLattribute(nodo) then begin
       //No es necesario hacer nada
     end;
     //ignora las otras etiquetas, en esta pasada.
  end;
  //verifica configuraciones por defecto
  if not defIDENTIF then //no se indicó etiqueta IDENTIFIERS
    DefTokIdentif('[A-Za-z$_]', '[A-Za-z0-9_]*');  //def. por defecto
//  if not defSIMBOLO then //no se indicó etiqueta SYMBOLS
end;
procedure TSynFacilSyn.LoadFromStream(Stream: TStream);
//Carga una sintaxis desde archivo
var
  doc     : TXMLDocument;
  nodo    : TDOMNode;
  i, j    : integer;
  nombre  : string;
  subExp     : string;
  p : tFaTokContent;
  t : tFaRegExpType;
  dStart: String;
  tipTok  : integer;
  tStart, tEnd, tContent, tAtrib : TFaXMLatrib;
  tRegex, tCharsStart, tMultiline, tFolding : TFaXMLatrib;
  tMatch: TFaXMLatrib;
  match: Boolean;
  nodo2: TDOMNode;
  tIfTrue,tIfFalse, tText: TFaXMLatrib;
  list: String;
  tEscape,tAtTrue,tAtFalse: TFaXMLatrib;
  chrEscape: Char;
begin
  {$IFDEF DebugMode}
  DebugLn('');
  DebugLn(' === Cargando archivo de sintaxis ===');
  {$ENDIF}
  ClearSpecials;     //limpia tablas de identif. y simbolos especiales
  CreateAttributes;  //Limpia todos los atributos y crea los predefinidos.
  ClearMethodTables; //Limpia tabla de caracter inicial, y los bloques
  try
    ReadXMLFile(doc, Stream);  //carga archivo de lenguaje
    ////////Primera exploración para capturar elementos básicos de la sintaxis/////////
    FirstXMLExplor(doc);  //Hace la primera exploración. Puede generar excepción.
    ///////////Segunda exploración para capturar elementos complementarios///////////
    //inicia exploración
    for i:= 0 to doc.DocumentElement.ChildNodes.Count - 1 do begin
       // Lee un Nodo o Registro
       nodo := doc.DocumentElement.ChildNodes[i];
       nombre := UpCase(AnsiString(nodo.NodeName));
       if (nombre = 'IDENTIFIERS') or (nombre = 'SYMBOLS') or
          (nombre = 'ATTRIBUTE') or (nombre = 'COMPLETION') or
          (nombre = 'SAMPLE') or(nombre = '#COMMENT') then begin
         //No se hace nada. Solo se incluye para evitar error de "etiqueta desconocida"
//     end else if IsAttributeName(nombre)  then begin
       end else if nombre =  'KEYWORD' then begin
         //forma corta de <TOKEN ATTRIBUTE='KEYWORD'> lista </TOKEN>
         AddIdentSpecList(AnsiString(nodo.TextContent), tnKeyword);  //Carga Keywords
       end else if (nombre = 'TOKEN') or
                   (nombre = 'COMMENT') or (nombre = 'STRING') then begin
         //Lee atributo
         if nombre = 'TOKEN' then begin   //Es definición formal de token
           tAtrib    := ReadXMLParam(nodo,'Attribute');
           tipTok := GetAttribIDByName(tAtrib.val);
           if tipTok = -1 then begin
             raise ESynFacilSyn.Create(Format(ERR_UNKNOWN_ATTRIB, [tAtrib.val]));
           end;
         end else begin   //Es definición simplificada
           tipTok := GetAttribIDByName(nombre);
           if tipTok = -1 then begin
             raise ESynFacilSyn.Create(Format(ERR_UNKNOWN_ATTRIB, [nombre]));
           end;
         end;
         //Lee los otros parámetros
         tStart    := ReadXMLParam(nodo,'Start');
         tEnd      := ReadXMLParam(nodo,'End');
         tCharsStart:= ReadXMLParam(nodo,'CharsStart');
         tContent  := ReadXMLParam(nodo,'Content');
         tRegex    := ReadXMLParam(nodo,'Regex');
         tMultiline:=ReadXMLParam(nodo,'Multiline');  //Falso, si no existe
         tFolding  := ReadXMLParam(nodo,'Folding');    //Falso, si no existe
         tMatch    := ReadXMLParam(nodo,'RegexMatch'); //Tipo de coincidencia
         tEscape   := ReadXMLParam(nodo,'Escape'); //
         if (nombre = 'COMMENT') and not tEnd.hay then tEnd.hay := true; //por compatibilidad
         //verifica tipo de definición
         if tContent.hay then begin //Si hay "Content", es token por contenido
           CheckXMLParams(nodo, 'Start CharsStart Content Attribute');
           dStart := dStartRegex(tStart, tCharsStart);  //extrae delimitador inicial
           p := DefTokContent(dStart, tipTok);
           //define contenido
           p.AddInstruct(ToListRegex(tContent)+'*','','');
         end else if tRegex.hay then begin //definición de token por contenido con Regex
           CheckXMLParams(nodo, 'Start CharsStart Regex Attribute RegexMatch');
           match := UpCase(tMatch.val)='COMPLETE';
           if tStart.hay or tCharsStart.hay then begin //modo con delimitador
             dStart := dStartRegex(tStart, tCharsStart);  //extrae delimitador inicial
             p := DefTokContent(dStart, tipTok);
             p.AddRegEx(tRegex.val, match);  //agrega la otra parte de la expresión
           end else begin  //modo simplificado: <token regex="" />
             subExp := ExtractRegExpN(tRegex.val, t);  //extrae primera expresión
             if t = tregChars1_ then begin  //[A-Z]+
               //Esta forma, normalmente no sería válida, pero se puede dividir
               //en las formas [A-Z][A-Z]*, y así sería válida
               list := copy(subExp, 1, length(subExp)-1);  //quita "+"
               subExp := list;  //transforma en lista simple
               tRegex.val := list + '*' + tRegex.val;  //completa
             end;
             p := DefTokContent(subExp, tipTok);
             p.AddRegEx(tRegex.val, match);  //agrega la otra parte de la expresión
           end;
         end else if tEnd.hay then begin //definición de token delimitado
           CheckXMLParams(nodo, 'Start CharsStart End Attribute Multiline Folding Escape');
           dStart := dStartRegex(tStart, tCharsStart);  //extrae delimitador inicial
           if (tEscape.hay) and (tEscape.val<>'') then chrEscape:= tEscape.val[1]
           else chrEscape := #0;
           //no se espera que DefTokDelim(), genere error aquí.
           if tMultiline.bol then  //es multilínea
             DefTokDelim(dStart, tEnd.val, tipTok, tdMulLin, tFolding.bol, chrEscape)
           else  //es de una sola líneas
             DefTokDelim(dStart, tEnd.val, tipTok, tdUniLin, tFolding.bol, chrEscape);
         end else begin  //definición incompleta
           if tStart.hay or tCharsStart.hay then begin //se ha indicado delimitador inicial
             dStart := dStartRegex(tStart, tCharsStart);  //extrae delimitador
             p := DefTokContent(dStart, tipTok);  //crea un token por contenido
             //Hasta aquí se creó un token por contenido. Explora sub-nodos
             for j := 0 to nodo.ChildNodes.Count-1 do begin
               nodo2 := nodo.ChildNodes[j];
               if UpCAse(nodo2.NodeName)='REGEX' then begin  //instrucción
                 tText   := ReadXMLParam(nodo2,'Text');
                 tIfTrue := ReadXMLParam(nodo2,'IfTrue');
                 tIfFalse:= ReadXMLParam(nodo2,'IfFalse');
                 tAtTrue := ReadXMLParam(nodo2,'AtTrue');
                 tAtFalse:= ReadXMLParam(nodo2,'AtFalse');
                 CheckXMLParams(nodo2, 'Text IfTrue IfFalse AtTrue AtFalse');
                 //Agrega la instrucción
                 p.AddInstruct(tText.val, tIfTrue.val, tIfFalse.val,
                       GetAttribIDByName(tAtTrue.val), GetAttribIDByName(tAtFalse.val));
               end else if UpCase(nodo2.NodeName)='#COMMENT' then begin
                 //solo lo deja pasar,para no generar error
               end else begin
                 raise ESynFacilSyn.Create(Format(ERR_INVAL_LAB_TOK,[nodo2.NodeName]));
               end;
             end;
           end else begin
             raise ESynFacilSyn.Create(ERR_INCOMP_TOK_DEF_ + '<' + nombre + '...');
           end;
         end;
       end else if ProcXMLBlock(nodo, nil) then begin  //bloques válidos en cualquier parte
         //No es necesario hacer nada
       end else if ProcXMLSection(nodo, nil) then begin //secciones en "MAIN"
         //No es necesario hacer nada
       end else begin
          raise ESynFacilSyn.Create(Format(ERR_UNKNOWN_LABEL,[nombre]));
       end;
    end;
    Rebuild;  //prepara al resaltador
    doc.Free;  //libera
  except
    on e: Exception do begin
      //completa el mensaje
      e.Message:=ERROR_LOADING_ + 'stream' + #13#10 + e.Message;
      doc.Free;
      raise   //genera de nuevo
    end;
  end;
end;
procedure TSynFacilSyn.LoadFromResourceName(Instance: THandle; const ResName: String);
var
  rs: TResourceStream;
begin
  rs := TResourceStream.Create(Instance, ResName, PChar(RT_RCDATA));
  try
    rs.Position := 0;
    LoadFromStream(rs);
  finally
    rs.Free;
  end;
end;
function TSynFacilSyn.ProcXMLBlock(nodo: TDOMNode; blqPad: TFaSynBlock): boolean;
//Verifica si el nodo tiene la etiqueta <BLOCK>. De ser así, devuelve TRUE y lo procesa.
//Si encuentra error, genera una excepción.
var
  i: integer;
  tStart, tFolding, tName, tParent : TFaXMLatrib;
  tBackCol, tTokPos: TFaXMLatrib;
  blq : TFaSynBlock;
  nodo2  : TDOMNode;
  Success: boolean;
  tEnd: TFaXMLatrib;
  tCloParnt: TFaXMLatrib;
begin
  if UpCase(nodo.NodeName) <> 'BLOCK' then exit(false);
  Result := true;  //encontró
  //Lee atributos
  tStart    := ReadXMLParam(nodo,'Start');
  tEnd      := ReadXMLParam(nodo,'End');
  tName     := ReadXMLParam(nodo,'Name');
  tFolding  := ReadXMLParam(nodo,'Folding');    //Falso, si no existe
  tParent   := ReadXMLParam(nodo,'Parent');
  tBackCol  := ReadXMLParam(nodo,'BackCol');
  tCloParnt := ReadXMLParam(nodo,'CloseParent');
  //validaciones
  if not tFolding.hay then tFolding.bol:=true;  //por defecto
  if not tName.hay then tName.val:='Blk'+IntToStr(lisBlocks.Count+1);
  CheckXMLParams(nodo, 'Start End Name Folding Parent BackCol CloseParent');
  if tParent.hay then begin //se especificó blqPad padre
    blqPad := SearchBlock(tParent.val, Success);  //ubica blqPad
    if not Success then raise ESynFacilSyn.Create(ERR_BLK_NO_DEFINED + tParent.val);
  end;
  //crea el blqoue, con el bloque padre indicado, o el que viene en el parámetro
  blq := CreateBlock(tName.val, tFolding.bol, blqPad);
  if tStart.hay then AddIniBlockToTok(tStart.val, 0, blq);
  if tEnd.hay   then AddFinBlockToTok(tEnd.val, 0, blq);
  if tBackCol.hay then begin //lee color
    if UpCase(tBackCol.val)='TRANSPARENT' then blq.BackCol:= COL_TRANSPAR
    else blq.BackCol:= tBackCol.col;
  end;
  if tCloParnt.hay then begin
    blq.CloseParent:=tCloParnt.bol;
  end;
  ////////// explora nodos hijos //////////
  for i := 0 to nodo.ChildNodes.Count-1 do begin
    nodo2 := nodo.ChildNodes[i];
    if UpCAse(nodo2.NodeName)='START' then begin  //definición alternativa de delimitador
      tTokPos := ReadXMLParam(nodo2,'TokPos');
      CheckXMLParams(nodo2, 'TokPos');
      //agrega la referecnia del bloque al nuevo token delimitador
      AddIniBlockToTok(trim(AnsiString(nodo2.TextContent)), tTokPos.n, blq);
    end else if UpCAse(nodo2.NodeName)='END' then begin  //definición alternativa de delimitador
      tTokPos := ReadXMLParam(nodo2,'TokPos');
      CheckXMLParams(nodo2, 'TokPos');
      //agrega la referecnia del bloque al nuevo token delimitador
      AddFinBlockToTok(trim(AnsiString(nodo2.TextContent)), tTokPos.n, blq);
    end else if ProcXMLSection(nodo2, blq) then begin  //definición de sección
      //No es necesario procesar
    end else if ProcXMLBlock(nodo2, blq) then begin  //definición de bloque anidado
      //No es necesario procesar
    end else if UpCase(nodo2.NodeName) = '#COMMENT' then begin
      //solo para evitar que de mensaje de error
    end else begin
      raise ESynFacilSyn.Create(Format(ERR_INVAL_LAB_BLK,[nodo2.NodeName]));
    end;
  end;
end;
function TSynFacilSyn.ProcXMLSection(nodo: TDOMNode; blqPad: TFaSynBlock): boolean;
//Verifica si el nodo tiene la etiqueta <SECCION>. De ser así, devuelve TRUE y lo procesa.
//Si encuentra error, genera una excepción.
var
  i: integer;
  tStart, tFolding, tName, tParent : TFaXMLatrib;
  blq : TFaSynBlock;
  tBackCol, tUnique: TFaXMLatrib;
  nodo2  : TDOMNode;
  tStartPos: TFaXMLatrib;
  tFirstSec: TFaXMLatrib;
  tTokenStart: TFaXMLatrib;
  Success: boolean;
begin
  if UpCase(nodo.NodeName) <> 'SECTION' then exit(false);
  Result := true;  //encontró
  //lee atributos
  tStart    := ReadXMLParam(nodo,'Start');
  tTokenStart:= ReadXMLParam(nodo,'TokenStart');
  tName     := ReadXMLParam(nodo,'Name');
  tFolding  := ReadXMLParam(nodo,'Folding');    //Falso, si no existe
  tParent   := ReadXMLParam(nodo,'Parent');
  tBackCol  := ReadXMLParam(nodo,'BackCol');
  tUnique   := ReadXMLParam(nodo,'Unique');
  tFirstSec := ReadXMLParam(nodo,'FirstSec');
  //validaciones
  if not tFolding.hay then tFolding.bol:=true;  //por defecto
  if not tName.hay then tName.val:='Sec'+IntToStr(lisBlocks.Count+1);
  CheckXMLParams(nodo, 'Start TokenStart Name Folding Parent BackCol Unique FirstSec');
  if tParent.hay then begin //se especificó blqPad padre
    blqPad := SearchBlock(tParent.val, Success);  //ubica blqPad
    if not Success then raise ESynFacilSyn.Create(ERR_BLK_NO_DEFINED + tParent.val);
  end;
  //crea la sección, con el bloque padre indicado, o el que viene en el parámetro
  blq := CreateBlock(tName.val, tFolding.bol, blqPad);
  blq.IsSection:=true;
  if tStart.hay then begin   //configuración normal con "Start"
    if tFirstSec.hay then begin  //hay primera sección
      AddFirstSectToTok(tStart.val, 0, blq)
    end else begin               //sección normal
      AddIniSectToTok(tStart.val, 0, blq);
    end;
  end else if tTokenStart.hay  then begin  //configuración indicando nombre de token
    {Se usará la misma función AddIniSectToTok(), para encontrar al token, pero
     formalmente debería usarse una función especial para ubicar al token usnado
     su nombre}
    AddIniSectToTok(tTokenStart.val, 0, blq);
  end;
  if tBackCol.hay then begin
    if UpCase(tBackCol.val)='TRANSPARENT' then blq.BackCol:= COL_TRANSPAR
    else blq.BackCol:= tBackCol.col;   //lee color
  end;
  if tUnique.hay then blq.UniqSec:=tUnique.bol;  //lee Unique
  ////////// explora nodos hijos //////////
  for i := 0 to nodo.ChildNodes.Count-1 do begin
      nodo2 := nodo.ChildNodes[i];
      if UpCAse(nodo2.NodeName)='START' then begin  //definición alternativa de delimitador
        tStartPos := ReadXMLParam(nodo2,'StartPos');
        CheckXMLParams(nodo2, 'StartPos');
        //agrega la referecnia del bloque al nuevo token delimitador
        AddIniSectToTok(trim(AnsiString(nodo2.TextContent)), tStartPos.n, blq);
      end else if ProcXMLSection(nodo2, blq) then begin  //definición de sección
        //No es necesario procesar
      end else if ProcXMLBlock(nodo2, blq) then begin  //definición de bloque anidado
        //No es necesario procesar
      end else if UpCase(nodo2.NodeName) = '#COMMENT' then begin
        //solo para evitar que de mensaje de error
      end else begin
        raise ESynFacilSyn.Create(Format(ERR_INVAL_LAB_SEC,[nodo2.NodeName]));
      end;
  end;
end;
procedure TSynFacilSyn.LoadFromFile(const Filename: string);
var
  fs: TFileStream;
begin
  try
    fs := TFileStream.Create(Filename, fmOpenRead);
    fs.Position := 0;

    LoadFromStream(fs);
    fs.Free;
  except
    on e: Exception do begin
      //completa el mensaje
      e.Message:=ERROR_LOADING_ + Filename + #13#10 + e.Message;
      fs.Free;
      raise   //genera de nuevo
    end;
  end;
end;
procedure TSynFacilSyn.Rebuild;
{Configura los tokens delimitados de acuerdo a la sintaxis definida actualmente, de
 forma que se optimice el procesamiento.
 Todos los tokens que se procesen aquí, deben tener delimitador inicial símbolo}
var
  i,j     : integer;
  r       : TTokSpec;
  dSexc   : boolean;  //indica que el delimitador inicial es exclusivo.

  function delStart1Exclus(cad0: string): boolean;
  {Indica si el token símbolo es de 1 caracter de ancho y no otro token símbolo que empiece
   con ese caracter}
  var i: integer;
      cad: string;
  begin
    Result := true;  //se asume que si es exclusivo
    if length(cad0)<>1 then exit(false);
    for i := 0 to High(mSym0) do begin
      cad := mSym0[i].txt;
      if cad  <> cad0 then begin  //no considera al mismo
        if cad0[1] = cad[1] then exit(false);  //no es
      end;
    end;
  end;

begin
  {$IFDEF DebugMode}
  DebugLn('---------símbolos leidos: mSym0[]----------');
  for i:=0 to High(mSym0) do
    DebugLn('  bloque: '+ mSym0[i].txt + ',' + StringReplace(mSym0[i].dEnd, #13,#25,[]));
  DebugLn('---------simplificando símbolos----------');
  {$ENDIF}
  //Explora los símbolos para optimizar el procesamiento
  setlength(mSym,0);  //limpia, porque vamos a reconstruir
  for i := 0 to High(mSym0) do begin
    r := mSym0[i];
    dSexc:=delStart1Exclus(r.txt);  //ve si es de 1 caracter y exclusivo
    if          dSexc and (r.typDel=tdConten1) then begin
      //Token por contenido, que se puede optimizar
      {$IFDEF DebugMode}
      DebugLn('  [' + r.txt[1] + '] -> @metTokCont1 (Token Por Conten. inicio exclusivo)');
      {$ENDIF}
      fProcTable[r.txt[1]] := @metTokCont1;
    end else if dSexc and (r.typDel=tdConten2) then begin
      //Token por contenido, que se puede optimizar
      {$IFDEF DebugMode}
      DebugLn('  [' + r.txt[1] + '] -> @metTokCont2 (Token Por Conten. inicio exclusivo)');
      {$ENDIF}
      fProcTable[r.txt[1]] := @metTokCont2;
    end else if dSexc and (r.typDel=tdConten3) then begin
      //Token por contenido, que se puede optimizar
      {$IFDEF DebugMode}
      DebugLn('  [' + r.txt[1] + '] -> @metTokCont3 (Token Por Conten. inicio exclusivo)');
      {$ENDIF}
      fProcTable[r.txt[1]] := @metTokCont3;
    end else if dSexc and (r.typDel=tdConten4) then begin
      //Token por contenido, que se puede optimizar
      {$IFDEF DebugMode}
      DebugLn('  [' + r.txt[1] + '] -> @metTokCont4 (Token Por Conten. inicio exclusivo)');
      {$ENDIF}
      fProcTable[r.txt[1]] := @metTokCont4;
    end else if dSexc and (r.typDel=tdUniLin) and (r.txt=r.dEnd) and (r.chrEsc=#0) then begin
      //Caso típico de cadenas. Es procesable por nuestra función "metUniLin1"
      {$IFDEF DebugMode}
      DebugLn('  [' + r.txt[1] + '] -> @metUniLin1 (uniLin c/delims iguales de 1 car)');
      {$ENDIF}
      fProcTable[r.txt[1]] := @metUniLin1;
      fAtriTable[r.txt[1]] := r.tTok; //para que metUniLin1() lo pueda recuperar
    //busca tokens una línea con delimitador de un caracter
    end else if dSexc and (r.typDel=tdUniLin) and (r.dEnd=#13) then begin
      //Caso típico de comentarios. Es procesable por nuestra función "metFinLinea"
      {$IFDEF DebugMode}
      DebugLn('  [' + r.txt[1] + '] -> @metFinLinea (uniLin con dStart de 1 car y dEnd = #13)');
      {$ENDIF}
      fProcTable[r.txt[1]] := @metFinLinea;
      fAtriTable[r.txt[1]] := r.tTok; //para que metFinLinea() lo pueda recuperar
      { TODO : Se podría crear un procedimiento para manejar bloques multilíneas
       con delimitador inicial exclusivo y así optimizar su procesamiento porque puede
       tornarse pesado en la forma actual. }
    end else if dSexc and (r.typDel=tdNull) and not r.openBlk and not r.closeBlk and
                not r.OpenSec then begin
      //Es símbolo especial de un caracter, exclusivo, que no es parte de token delimitado
      //ni es inicio o fin de bloque
      {$IFDEF DebugMode}
      DebugLn('  [' + r.txt[1] + '] -> @metSym1Car (símbolo simple de 1 car)');
      {$ENDIF}
      fProcTable[r.txt[1]] := @metSym1Car;
      fAtriTable[r.txt[1]] := r.tTok; //para que metSym1Car() lo pueda recuperar
    end else begin //no se puede simplificar.
      //Lo agrega a la tabla de símbolos para búsqueda normal.
      CreaBuscTokEspec(mSym, r.txt, j);  //No puede usar CreaBuscSymEspec(), porque usa mSymb0
      mSym[j] := r;  //actualiza o agrega
    end
  end;
  //termina el proceso
  RebuildSymbols;
  if CurrentLines <> nil then begin //Hay editor asignado
    ScanAllRanges;  {Necesario, porque se ha reconstruido los TTokSpec y
                       los valores de "fRange" de las líneas, están "perdidos"}
  end;
  {$IFDEF DebugMode}
  DebugLn('--------------------------------');
  {$ENDIF}
end;
/////////// manejo de bloques
procedure TSynFacilSyn.StartBlock(ABlockType: Pointer; IncreaseLevel: Boolean); inline;
//Procedimiento general para abrir un bloque en el resaltador
begin
//  if IsCollectingNodeInfo then // llutti
//  begin // llutti
//    CollectNodeInfo(False, ABlockType, IncreaseLevel); // llutti
//  end; // llutti
  StartCodeFoldBlock(ABlockType, IncreaseLevel);
//  CodeFoldRange.Add(ABlockType, IncreaseLevel);
end;
procedure TSynFacilSyn.EndBlock(DecreaseLevel: Boolean); inline;
//Procedimiento general para cerrar un bloque en el resaltador
begin
//  BlockType := TopCodeFoldBlockType; // llutti
//  if IsCollectingNodeInfo then // llutti
//  begin // llutti
//    CollectNodeInfo(True, BlockType, DecreaseLevel); // llutti
//  end; // llutti
  EndCodeFoldBlock(DecreaseLevel);
//  CodeFoldRange.Pop(DecreaseLevel);
end;
procedure TSynFacilSyn.StartBlockAndFirstSec(const blk, firstSec: TfaSynBlock);
{Abre un bloque TfaSynBlock, verifica si tiene una primera sección para abrir.}
var Cancel: boolean;
begin
  Cancel := false;
  if blk.OnBeforeOpen<>nil then blk.OnBeforeOpen(blk, Cancel);
  if Cancel then exit;
  StartBlock(blk, blk.showFold);
  //verifica si hay primera sección para abrir
  if firstSec <> nil then
    StartBlock(firstSec, firstSec.showFold);
end;
procedure TSynFacilSyn.StartBlockFa(const blk: TfaSynBlock);
{Abre un bloque TfaSynBlock.}
var Cancel: boolean;
begin
  Cancel := false;
  if blk.OnBeforeOpen<>nil then blk.OnBeforeOpen(blk, Cancel);
  if Cancel then exit;
  StartBlock(blk, blk.showFold);
end;
procedure TSynFacilSyn.EndBlockFa(const blk: TfaSynBlock);
{Cierra un bloque TfaSynBlock. El parámetro blk, no debería ser necesario, puesto que
se supone que siemprer se cerrará el último abierto.}
var Cancel: boolean;
begin
  Cancel := false;
  if blk.OnBeforeClose<>nil then blk.OnBeforeClose(blk, Cancel);
  if Cancel then exit;
  EndBlock(blk.showFold);
end;
function TSynFacilSyn.TopBlock: TFaSynBlock;
//Función genérica para devolver el último bloque abierto. Si no hay ningún bloque
//abierto, devuelve "MainBlk".
//Es una forma personalizada de TopCodeFoldBlockType()
var
  Fold: TSynCustomCodeFoldBlock;
begin
  Fold := CodeFoldRange.Top;  //CodeFoldRange nunca denería ser NIL
  if Fold = nil then
    Result := MainBlk  //está en el primer nivel
  else begin
    Result := TFaSynBlock(Fold.BlockType);
    if Result = nil then
      Result := MainBlk;  //protección
  end;
end;
function TSynFacilSyn.TopBlockOpac: TFaSynBlock;
//Función genérica para devolver el último bloque abierto con color de fondo.
var
  Fold: TSynCustomCodeFoldBlock;
begin
  //profundiza hasta encontrar un bloque con color opaco
   Fold := CodeFoldRange.Top;
   while (Fold <> nil) and (Fold.BlockType<>nil) and
         (TFaSynBlock(Fold.BlockType).BackCol=COL_TRANSPAR) do begin
     Fold := Fold.Parent;
   end;
   //si no encontró devuelve el bloque principal
   if (Fold = nil) or (Fold.BlockType=nil) then begin
     Result := MainBlk
   end else begin
     Result := TFaSynBlock(Fold.BlockType);
   end;
end;
function TSynFacilSyn.SearchBlock(blk: string; out Success: boolean): TFaSynBlock;
{Busca un bloque por su nombre. Se ignora la caja.
 Si la búsqueda tuvo éxito, pone la bandera "Success" en TRUE}
var i: integer;
begin
  Result := nil;  //valor por defecto. Es un valor "válido".
  Success := false;
  if UpCase(blk) = 'NONE' then begin
    Success := true;
    exit;
  end;
  if UpCase(blk) = 'MAIN' then begin
    Result := MainBlk;
    Success := true;
    exit;
  end;
  for i := 0 to lisBlocks.Count-1 do
    if Upcase(lisBlocks[i].name) = Upcase(blk) then begin
       Result := lisBlocks[i];  //devuelve referencia
       Success := true;
       exit;
    end;
  //no se encontró el blqPad pedido
end;

procedure TSynFacilSyn.ProcTokenDelim(const d: TTokSpec);
{Procesa un posible token delimitador. Debe llamarse después de que se ha reconocido
 el token especial y el puntero apunte al siguiente token.}
  procedure CheckForOpenBlk(const d: TTokSpec); //inline;
  {Abre el bloque actual, verificando si está en el bloque valído}
  var
      CurBlk: TFaSynBlock;
      CurBlk_Parent: TFaSynBlock;
      blkToOpen: TFaSynBlock;
  begin
    CurBlk := TopBlock();  //lee bloque superior (el actual)
    if CurBlk.IsSection then begin
      //Estamos en un bloque de sección
      CurBlk_Parent := TopCodeFoldBlock(1);  {Lee bloque superior, en donde se ha abierto, que no
                                              siempe coincidirá con CurBlk.parentBlk }
      for blkToOpen in d.BlksToOpen do begin
        //verifica si se cumplen condiciones para abrir el bloque
        if blkToOpen.parentBlk = nil then begin //se abre en cualquier parte
          {Un bloque al mismo nivel de una sección, la cierra siempre}
          EndBlockFa(CurBlk);  //cierra primero la sección anterior
          StartBlockAndFirstSec(blkToOpen, d.firstSec);
          break;   //sale
        end else if blkToOpen.parentBlk = CurBlk  then begin
          //Corresponde abrir el bloque dentro de esta sección
          {No se verifica si hay sección anterior porque estamos en una sección y se sabe
          que un sección no puede contener otras secciones.}
          StartBlockAndFirstSec(blkToOpen, d.firstSec);
          break;     //sale
        end else if blkToOpen.parentBlk = CurBlk_Parent then begin
          {No correspondía abrir en esa sección, pero se aplica al bloque padre,
          entonces está al mismo nivel que la sección actual y debería cerrarla primero}
          EndBlockFa(CurBlk);  //cierra primero la sección anterior
          StartBlockAndFirstSec(blkToOpen, d.firstSec);
          break;     //sale
        end;
      end;
    end else begin
      //Estamos dentro de un bloque común
      for blkToOpen in d.BlksToOpen do begin
        //verifica si se cumplen condiciones para abrir el bloque
        if blkToOpen.parentBlk = nil then begin //se abre en cualquier parte
          StartBlockAndFirstSec(blkToOpen, d.firstSec);
          break;   //sale
        end else if blkToOpen.parentBlk = CurBlk then begin
          //Corresponde abrir en este bloque
          StartBlockAndFirstSec(blkToOpen, d.firstSec);
          break;     //sale
        end;
      end;
    end;
  end;
  function CheckForOpenSec(const d: TTokSpec): boolean; //inline;
  {Abre una sección si se cunplen las condiciones. De ser el caso, cierra primero una
   posible sección previa. Si abre la sección devuelve TRUE. }
  var
      curBlk: TFaSynBlock;
      CurBlk_Parent: TFaSynBlock;
      SecToOpen: TFaSynBlock;
  begin
    CurBlk := TopBlock();  //lee bloque superior (el actual)
    if curBlk.IsSection then begin //verifica si está en un bloque de sección
      //Estamos en un bloque de sección. ¿Será de alguna de las secciones que maneja?
      CurBlk_Parent := TopCodeFoldBlock(1);  {Lee bloque superior, en donde se ha abierto, que no
                                              siempe coincidirá con CurBlk.parentBlk }
      for SecToOpen in d.SecsToOpen do begin
        //verifica si se cumplen condiciones para abrir el bloque
        if SecToOpen.parentBlk = nil then begin //se abre en cualquier parte
          {Una sección al mismo nivel de una sección, la cierra siempre}
          if (SecToOpen=curBlk) and curBlk.UniqSec then exit(false); //verificación
          EndBlockFa(CurBlk);  //cierra primero la sección anterior
          StartBlockFa(SecToOpen);  //abre una nueva sección
          exit(true);  //sale con TRUE
        end else if SecToOpen.parentBlk = CurBlk_Parent then begin
          //Está en el bloque para el que se ha definido
          //Debe cerrar primero la sección anterior, porque está al mismo nivel
          if (SecToOpen=curBlk) and curBlk.UniqSec then exit(false); //verificación
          EndBlockFa(curBlk);  //cierra primero la sección anterior
          StartBlockFa(SecToOpen);  //abre una nueva sección
          exit(true);  //sale con TRUE
        end else if SecToOpen.parentBlk = CurBlk_Parent.parentBlk then begin
          //Está en el bloque para el que se ha definido, pero hay abierta otra sección
          //Debe cerrar primero la sección anterior, y la anterior.
//          if (SecToOpen=curBlk) and curBlk.UniqSec then exit(false); //verificación
          EndBlockFa(curBlk);  //cierra primero la sub-sección anterior
          EndBlockFa(curBlk);  //cierra primero la sección anterior
          StartBlockFa(SecToOpen);  //abre una nueva sección
          exit(true);  //sale con TRUE
        end else if SecToOpen.parentBlk = CurBlk then begin
          //Está en el bloque que se ha definido como padre
          StartBlockFa(SecToOpen);  //abre una nueva sección dentro de la sección
        end;
      end;
      Result := false;  //no abrió
    end else begin
      //No está en un bloque de sección, entonces debe estar en un bloque (aunque sea MainBlk)
      //verifica si corresponde abrir esta sección
      for SecToOpen in d.SecsToOpen do begin
        //verifica si se cumplen condiciones para abrir el bloque
        if SecToOpen.parentBlk = nil then begin //se abre en cualquier parte
          StartBlockFa(SecToOpen);
          exit(true);  //sale con TRUE
        end else if SecToOpen.parentBlk = curBlk then begin
          //Corresponde abrir en este bloque
          StartBlockFa(SecToOpen);
          exit(true);  //sale con TRUE
        end;
      end;
      Result := false;  //no abrió
    end;
  end;
  procedure CheckForCloseBlk(const BlksToClose: array of TFaSynBlock); //inline;
  {Verifica si el bloque más reciente del plegado, está en la lista de bloques
   que cierra "d.BlksToClose". De ser así cierra el bloque}
  var
    CurBlk: TFaSynBlock;
    CurBlk_Parent: TFaSynBlock;
    BlkToClose: TFaSynBlock;
  begin
    CurBlk := TopBlock();  //lee bloque superior
    //verifica si estamos en medio de una sección
    if CurBlk.IsSection then begin //verifica si es bloque de sección
      {Es sección, el bloque actual debe ser el bloque padre, porque por definición
      los tokens no cierran secciones (a menos que abran bloques).}
      CurBlk_Parent := TopCodeFoldBlock(1);  //lee bloque superior
      for BlkToClose in BlksToClose do begin
        if BlkToClose = CurBlk_Parent  then begin  //coincide
          //Antes de cerrar el blqoque padre, debe cerrar la sección actual
          EndBlockFa(CurBlk);  //cierra primero la sección
//            EndBlockFa(CurBlk_Parent.showFold)  //cierra bloque
          CloseThisBlk := BlkToClose; //marca para cerrar en el siguuiente token
          break;
        end;
      end;
    end else begin
      //Estamos dentro de un bloque común
      for BlkToClose in BlksToClose do begin
        if BlkToClose = CurBlk  then begin  //coincide
//            EndBlockFa(CurBlk_Parent.showFold)  //cierra bloque
          CloseThisBlk := BlkToClose; //marca para cerrar en el siguuiente token
          break;
        end;
      end;
    end;
  end;

var
  abrioSec: Boolean;
begin
  case d.typDel of
  tdNull: begin       //token que no es delimitador de token
      fTokenID := d.tTok; //no es delimitador de ningún tipo, pone su atributo
      //un delimitador común puede tener plegado de bloque
      if d.closeBlk then begin //verifica primero, si es cierre de algún bloque
        CheckForCloseBlk(d.BlksToClose);  //cierra primero
      end;
      abrioSec := false;
      if d.OpenSec then //Verifica primero si es bloque de sección
        abrioSec := CheckForOpenSec(d);
      if not abrioSec then  //prueba si abre como bloque
        if d.openBlk then CheckForOpenBlk(d); //verifica como bloque normal
    end;
  tdUniLin: begin  //delimitador de token de una línea.
      //Se resuelve siempre en la misma línea.
      fTokenID := d.tTok;   //asigna token
      delTok := d.dEnd;   //para que esté disponible al explorar la línea actual.
      folTok := false;    //No tiene sentido el plegado, en token de una línea.
      chrEsc := d.chrEsc; //caracter de escape
      if posFin=tamLin then exit;  //si está al final, necesita salir con fTokenID fijado.
      d.pRange;  //ejecuta función de procesamiento
      //Se considera que este tipo de tokens, puede ser también inicio o fin de bloque
      if d.closeBlk then begin //verifica primero, si es cierre de algún bloque
        CheckForCloseBlk(d.BlksToClose);  //cierra primero
      end;
      abrioSec := false;
      if d.OpenSec then //Verifica primero si es bloque de sección
        abrioSec := CheckForOpenSec(d);
      if not abrioSec then  //prueba si abre como bloque
        if d.openBlk then CheckForOpenBlk(d); //verifica como bloque normal
    end;
  tdMulLin: begin  //delimitador de token multilínea
      //Se pueden resolver en la línea actual o en las siguientes líneas.
      fTokenID := d.tTok;   //asigna token
      delTok := d.dEnd;    //para que esté disponible al explorar las sgtes. líneas.
      folTok := d.folTok;  //para que esté disponible al explorar las sgtes. líneas.
      chrEsc := d.chrEsc;  //caracter de escape
      if folTok then StartBlockFa(MulTokBlk);  //abre al inicio del token
      fRange := @d;    //asigna rango apuntando a este registro
      if posFin=tamLin then exit;  //si está al final, necesita salir con fTokenID fijado.
      d.pRange;  //ejecuta función de procesamiento
    end;
  tdConten1: begin  //delimitador de token por contenido 1
      dec(posFin);    //ajusta para que se procese correctamente
      metTokCont1;    //este método se encarga
    end;
  tdConten2: begin  //delimitador de token por contenido 1
      dec(posFin);    //ajusta para que se procese correctamente
      metTokCont2;    //este método se encarga
    end;
  tdConten3: begin  //delimitador de token por contenido 1
      dec(posFin);    //ajusta para que se procese correctamente
      metTokCont3;    //este método se encarga
    end;
  tdConten4: begin  //delimitador de token por contenido 1
      dec(posFin);    //ajusta para que se procese correctamente
      metTokCont4;    //este método se encarga
    end;
  else
    fTokenID := d.tTok; //no es delimitador, solo toma su atributo.
  end;
end;
//Rutinas de procesamiento de Identificadores especiales
procedure TSynFacilSyn.metIdentEsp(var mat: TArrayTokSpec); //inline;
//Procesa el identificador actual con la matriz indicada
var i: integer;
begin
  repeat inc(posFin)
  until not CharsIdentif[fLine[posFin]];
  fStringLen := posFin - posIni - 1;  //calcula tamaño - 1
  fToIdent := Fline + posIni + 1;  //puntero al identificador + 1
  fTokenID := tnIdentif;  //identificador común
  for i := 0 to High(mat) do begin
    if KeyComp(mat[i])  then begin
      ProcTokenDelim(mat[i]); //verifica si es delimitador
      exit;
    end;
  end;
end;
procedure TSynFacilSyn.metA;begin metIdentEsp(mA);end;
procedure TSynFacilSyn.metB;begin metIdentEsp(mB);end;
procedure TSynFacilSyn.metC;begin metIdentEsp(mC);end;
procedure TSynFacilSyn.metD;begin metIdentEsp(mD);end;
procedure TSynFacilSyn.metE;begin metIdentEsp(mE);end;
procedure TSynFacilSyn.metF;begin metIdentEsp(mF);end;
procedure TSynFacilSyn.metG;begin metIdentEsp(mG);end;
procedure TSynFacilSyn.metH;begin metIdentEsp(mH);end;
procedure TSynFacilSyn.metI;begin metIdentEsp(mI);end;
procedure TSynFacilSyn.metJ;begin metIdentEsp(mJ);end;
procedure TSynFacilSyn.metK;begin metIdentEsp(mK);end;
procedure TSynFacilSyn.metL;begin metIdentEsp(mL);end;
procedure TSynFacilSyn.metM;begin metIdentEsp(mM);end;
procedure TSynFacilSyn.metN;begin metIdentEsp(mN);end;
procedure TSynFacilSyn.metO;begin metIdentEsp(mO);end;
procedure TSynFacilSyn.metP;begin metIdentEsp(mP);end;
procedure TSynFacilSyn.metQ;begin metIdentEsp(mQ);end;
procedure TSynFacilSyn.metR;begin metIdentEsp(mR);end;
procedure TSynFacilSyn.metS;begin metIdentEsp(mS);end;
procedure TSynFacilSyn.metT;begin metIdentEsp(mT);end;
procedure TSynFacilSyn.metU;begin metIdentEsp(mU);end;
procedure TSynFacilSyn.metV;begin metIdentEsp(mV);end;
procedure TSynFacilSyn.metW;begin metIdentEsp(mW);end;
procedure TSynFacilSyn.metX;begin metIdentEsp(mX);end;
procedure TSynFacilSyn.metY;begin metIdentEsp(mY);end;
procedure TSynFacilSyn.metZ;begin metIdentEsp(mZ);end;

procedure TSynFacilSyn.metA_; begin metIdentEsp(mA_);end;
procedure TSynFacilSyn.metB_; begin metIdentEsp(mB_);end;
procedure TSynFacilSyn.metC_; begin metIdentEsp(mC_);end;
procedure TSynFacilSyn.metD_; begin metIdentEsp(mD_);end;
procedure TSynFacilSyn.metE_; begin metIdentEsp(mE_);end;
procedure TSynFacilSyn.metF_; begin metIdentEsp(mF_);end;
procedure TSynFacilSyn.metG_; begin metIdentEsp(mG_);end;
procedure TSynFacilSyn.metH_; begin metIdentEsp(mH_);end;
procedure TSynFacilSyn.metI_; begin metIdentEsp(mI_);end;
procedure TSynFacilSyn.metJ_; begin metIdentEsp(mJ_);end;
procedure TSynFacilSyn.metK_; begin metIdentEsp(mK_);end;
procedure TSynFacilSyn.metL_; begin metIdentEsp(mL_);end;
procedure TSynFacilSyn.metM_; begin metIdentEsp(mM_);end;
procedure TSynFacilSyn.metN_; begin metIdentEsp(mN_);end;
procedure TSynFacilSyn.metO_; begin metIdentEsp(mO_);end;
procedure TSynFacilSyn.metP_; begin metIdentEsp(mP_);end;
procedure TSynFacilSyn.metQ_; begin metIdentEsp(mQ_);end;
procedure TSynFacilSyn.metR_; begin metIdentEsp(mR_);end;
procedure TSynFacilSyn.metS_; begin metIdentEsp(mS_);end;
procedure TSynFacilSyn.metT_; begin metIdentEsp(mT_);end;
procedure TSynFacilSyn.metU_; begin metIdentEsp(mU_);end;
procedure TSynFacilSyn.metV_; begin metIdentEsp(mV_);end;
procedure TSynFacilSyn.metW_; begin metIdentEsp(mW_);end;
procedure TSynFacilSyn.metX_; begin metIdentEsp(mX_);end;
procedure TSynFacilSyn.metY_; begin metIdentEsp(mY_);end;
procedure TSynFacilSyn.metZ_; begin metIdentEsp(mZ_);end;
procedure TSynFacilSyn.metDol;begin metIdentEsp(mDol);end;
procedure TSynFacilSyn.metArr;begin metIdentEsp(mArr);end;
procedure TSynFacilSyn.metPer;begin metIdentEsp(mPer);end;
procedure TSynFacilSyn.metAmp;begin metIdentEsp(mAmp);end;
procedure TSynFacilSyn.metC3;begin metIdentEsp(mC3);end;
procedure TSynFacilSyn.metUnd;begin metIdentEsp(m_);end;
//Rutina única de procesamiento de Símbolos especiales
procedure TSynFacilSyn.metSimbEsp;
//Procesa un caracter que es inicio de símbolo y podría ser origen de un símbolo especial.
var i: integer;
  nCarDisp: Integer;
begin
  fTokenID := tnSymbol;  //identificador inicial por defecto
  //prepara para las comparaciones
  nCarDisp := tamLin-posIni;   //calcula caracteres disponibles hasta fin de línea
  fToIdent := Fline + posIni;  //puntero al identificador. Lo guarda para comparación
  //hay un nuevo posible delimitador. Se hace la búsqueda
  for i := 0 to High(mSym) do begin  //se empieza con los de mayor tamaño
    //fijamos nuevo tamaño para comparar
    fStringLen := length(mSym[i].txt);  //suponemos que tenemos esta cantidad de caracteres
    if fStringLen > nCarDisp then continue;  //no hay suficientes, probar con el siguiente
    if KeyComp(mSym[i]) then begin
      //¡Es símbolo especial!
      inc(posFin,fStringLen);  //apunta al siguiente token
      ProcTokenDelim(mSym[i]); //verifica si es delimitador
      exit;   //sale con el atributo asignado
    end;
  end;
  {No se encontró coincidencia.
   Ahora debemos continuar la exploración al siguiente caracter}
  posFin := posIni + 1;  //a siguiente caracter, y deja el actual como: fTokenID := tkSymbol
end;
//Funciones rápidas para la tabla de métodos (símbolos especiales)
procedure TSynFacilSyn.metSym1Car;
//Procesa tokens símbolo de un caracter de ancho.
begin
  fTokenID := fAtriTable[charIni];   //lee atributo
  Inc(posFin);  //pasa a la siguiente posición
end;
//Funciones rápidas para la tabla de métodos (tokens delimitados)
procedure TSynFacilSyn.metUniLin1;
//Procesa tokens de una sola línea y con delimitadores iguales y de un solo caracter.
begin
  fTokenID := fAtriTable[charIni];   //lee atributo
  Inc(posFin);  {no hay peligro en incrementar porque siempre se llama "metUniLin1" con
                 el caracter actual <> #0}
  while posFin <> tamLin do begin
    if fLine[posFin] = charIni then begin //busca fin de cadena
      Inc(posFin);
      if (fLine[posFin] <> charIni) then break;  //si no es doble caracter
    end;
    Inc(posFin);
  end;
end;
procedure TSynFacilSyn.metFinLinea;
//Procesa tokens de una sola línea que va hasta el fin de línea.
begin
  fTokenID := fAtriTable[charIni];   //lee atributo
  posFin := tamLin;  //salta rápidamente al final
end;
//Funciones llamadas por puntero y/o en medio de rangos. Estas funciones son llamadas
//cuando se procesa un token especial que es inicio de token delimitado y no ha sido
//optimizado para usar los métodos rápidos.
procedure TSynFacilSyn.ProcEndLine;
//Procesa hasta encontrar el fin de línea.
begin
  posFin := tamLin;  //salta rápidamente al final
end;
procedure TSynFacilSyn.ProcRangeEndSym;
{Procesa la línea actual buscando un delimitador símbolo (delTok).
 Si lo encuentra pone fRange a NIL. El tipo de token, debe estar ya asignado.}
var p: PChar;
begin
  //busca delimitador final
  p := strpos(fLine+posFin,PChar(delTok));
  if p = nil then begin   //no se encuentra
     posFin := tamLin;  //apunta al fin de línea
  end else begin  //encontró
     posFin := p + length(delTok) - fLine;
     fRange := nil;               //no necesario para tokens Unilínea
     if folTok then CloseThisBlk := MulTokBlk; //marca para cerrar en el siguuiente token
  end;
end;
procedure TSynFacilSyn.ProcRangeEndSym1;
{Procesa la línea actual buscando un delimitador símbolo de un caracter.
 Si lo encuentra pone fRange a NIL. El tipo de token, debe estar ya asignado.}
var p: PChar;
begin
  //busca delimitador final
  if chrEsc=#0 then begin  //no hay caracter de escape
    p := strscan(fLine+posFin,delTok[1]);
  end else begin  //debe filtrar el caracter de escape
    p := strscan(fLine+posFin,delTok[1]);
    while (p<>nil) and ((p-1)^=chrEsc) do begin
      p := strscan(p+1,delTok[1]);
    end;
  end;
  if p = nil then begin   //no se encuentra
     posFin := tamLin;  //apunta al fin de línea
  end else begin  //encontró
     posFin := p + 1 - fLine;
     fRange := nil;              //no necesario para tokens Unilínea
     if folTok then CloseThisBlk := MulTokBlk; //marca para cerrar en el siguiente token
  end;
end;
procedure TSynFacilSyn.ProcRangeEndIden;
{Procesa la línea actual buscando un delimitador identificador (delTok).
 Si lo encuentra pone fRange a rsUnknown. El tipo de token, debe estar ya asignado.}
var p: Pchar;
    c1, c2: char;
begin
  //busca delimitador final
  if CaseSensitive then
    p := strpos(fLine+posFin,PChar(delTok))
  else
    p := stripos(fLine+posFin,PChar(delTok));
  while p <> nil do begin   //definitivamente no se encuentra
    //verifica si es inicio de identificador
    c1:=(p-1)^;  {Retrocede. No debería haber problema en retroceder siempre, porque se
                  supone que se ha detectado el delimitador inicial, entonces siempre habrá
                  al menos un caracter}
    c2:=(p+length(delTok))^;   //apunta al final, puede ser el final de línea #0
    if (c1 in charsIniIden) or CharsIdentif[c1] or CharsIdentif[c2] then begin
      //está en medio de un identificador. No es válido.
      if CaseSensitive then
        p := strpos(p+length(delTok),PChar(delTok))  //busca siguiente
      else
        p := stripos(p+length(delTok),PChar(delTok));  //busca siguiente
    end else begin  //es el identificador buscado
      posFin := p + length(delTok) - fLine;  //puede terminar apuntándo a #0
      fRange := nil;               //no necesario para tokens Unilínea
      if folTok then CloseThisBlk := MulTokBlk; //marca para cerrar en el siguuiente token
      exit;
    end;
  end;
  //definitívamente no se encuentra
  posFin := tamLin;  //apunta al fin de línea
end;
///////////////////////////////////////////////////////////////////////////////////
procedure TSynFacilSyn.AddIniBlockToTok(dStart: string; TokPos: integer; blk: TFaSynBlock);
//Agrega a un token especial, la referencia a un bloque, en la parte inicial.
//Si hay error, genera excepción.
var n: integer;
    tok : TPtrTokEspec;
begin
  VerifDelim(dStart);  //puede generar excepción
  CreaBuscEspec(tok, dStart, TokPos); //busca o crea. Puede generar excepción
  //agrega referencia
  tok^.openBlk:=true;
  n:=High(tok^.BlksToOpen)+1;  //lee tamaño
  setlength(tok^.BlksToOpen,n+1);  //aumenta
  tok^.BlksToOpen[n]:=blk;  //escribe referencia
end;
procedure TSynFacilSyn.AddFinBlockToTok(dEnd: string; TokPos: integer; blk: TFaSynBlock);
//Agrega a un token especial, la referencia a un bloque, en la parte final.
//Si hay error, genera excepción.
var n: integer;
    tok : TPtrTokEspec;
begin
  VerifDelim(dEnd);  //puede generar excepción
  CreaBuscEspec(tok, dEnd, TokPos); //busca o crea. Puede generar excepción
  //agrega referencia
  tok^.closeBlk:=true;
  n:=High(tok^.BlksToClose)+1;  //lee tamaño
  setlength(tok^.BlksToClose,n+1);  //aumenta
  tok^.BlksToClose[n]:=blk;  //escribe referencia
end;
procedure TSynFacilSyn.AddIniSectToTok(dStart: string; TokPos: integer; blk: TFaSynBlock);
//Agrega a un token especial, la referencia a una sección.
//Si hay error, genera excepción.
var n: integer;
    tok : TPtrTokEspec;
begin
  VerifDelim(dStart);  //puede generar excepción
  CreaBuscEspec(tok, dStart, TokPos); //busca o crea. Puede generar excepción
  //agrega referencia
  tok^.OpenSec:=true;
  n:=High(tok^.SecsToOpen)+1;  //lee tamaño
  setlength(tok^.SecsToOpen,n+1);  //aumenta
  tok^.SecsToOpen[n]:=blk;  //escribe referencia
end;
procedure TSynFacilSyn.AddFirstSectToTok(dStart: string; TokPos: integer; blk: TFaSynBlock);
//Agrega a un token especial, la referencia a una sección.
var
  tok : TPtrTokEspec;
begin
  VerifDelim(dStart);  //puede generar excepción
  CreaBuscEspec(tok, dStart, TokPos); //busca o crea. Puede generar excepción
  //agrega referencia
  tok^.firstSec := blk; //agrega referencia
end;
function TSynFacilSyn.CreateBlock(blkName: string; showFold: boolean = true;
                                  parentBlk: TFaSynBlock = nil): TFaSynBlock;
//Crea un bloque en el resaltador y devuelve una referencia al bloque creado.
var blk : TFaSynBlock;
begin
  Result := nil;    //valor por defecto
  //if blkName = '' //No se verifica el nombre del bloque
  //Crea bloque
  blk:= TFaSynBlock.Create;
  blk.name     :=blkName;     //nombre de bloque
  blk.index    :=lisBlocks.Count; //calcula su posición
  blk.showFold := showFold;   //inidca si se muestra la marca de plegado
  blk.parentBlk:= parentBlk;  //asigna bloque padre
  blk.BackCol  := clNone;     //inicialmente sin color
  blk.IsSection:= false;
  blk.UniqSec  := false;
  blk.CloseParent :=false;

  lisBlocks.Add(blk);        //agrega a lista
  Result := blk;             //devuelve referencia
end;
function TSynFacilSyn.AddBlock(dStart, dEnd: string; showFold: boolean = true;
                               parentBlk: TFaSynBlock = nil): TFaSynBlock;
{Función pública para agregar un bloque a la sintaxis. Si encuentra error, genera una
excepción}
var blk : TFaSynBlock;
begin
  Result := nil;    //valor por defecto
  //Crea bloque
  blk:= CreateBlock('',showFold,parentBlk);
  Result := blk;           //devuelve referencia
  //procesa delimitador inicial
  AddIniBlockToTok(dStart, 0, blk);  //agrega referencia
  //procesa delimitador final
  AddFinBlockToTok(dEnd, 0, blk);  //agrega referencia
end;
function TSynFacilSyn.AddSection(dStart: string; showFold: boolean = true;
                                 parentBlk: TFaSynBlock = nil): TFaSynBlock;
{Función pública para agregar una sección a un bloque a la sintaxis. Si encuentra
genera una excepción}
var blk : TFaSynBlock;
begin
  Result := nil;    //valor por defecto
  //verificaciones
  if parentBlk = nil then begin
    parentBlk := MainBlk;  //NIL significa que es válido en el bloque principal
  end;
  //Crea bloque
  blk:= CreateBlock('',showFold,parentBlk);
  blk.IsSection:=true;
  Result := blk;           //devuelve referencia
  //procesa delimitador inicial
  AddIniSectToTok(dStart, 0, Blk);  //agrega referencia
end;
function TSynFacilSyn.AddFirstSection(dStart: string; showFold: boolean = true;
                                      parentBlk: TFaSynBlock = nil): TFaSynBlock;
{Función pública para agregar una sección que se abre siempre al inicio de un bloque.
Si encuentra error, genera una excepción}
var
  blk : TFaSynBlock;
begin
  Result := nil;    //valor por defecto
  //Una sección es también un bloque. Crea bloque
  blk:= CreateBlock('',showFold,parentBlk);
  blk.IsSection:=true;
  Result := blk;           //devuelve referencia
  //procesa delimitador inicial
  AddFirstSectToTok(dStart, 0, blk);
end;
//funciones para obtener información de bloques
function TSynFacilSyn.NestedBlocks: Integer;
//Devuelve la cantidad de bloques anidados en la posición actual. No existe un contador
//en el resaltador para este valor (solo para bloques con marca de pleagdo visible).
var
  Fold: TSynCustomCodeFoldBlock;
begin
  Result:=-1;  //para compensar el bloque que se crea al inicio
  if (CodeFoldRange<>nil) then begin
    Fold := CodeFoldRange.Top;
    while Fold <> nil do begin
//if Fold.BlockType = nil then debugln('--NIL') else debugln('--'+TFaSynBlock(Fold.BlockType).name);
      inc(Result);
      Fold := Fold.Parent;
    end;
  end;
end;
function TSynFacilSyn.NestedBlocksBegin(LineNumber: integer): Integer;
//Devuelve la cantidad de bloques anidados al inicio de la línea.
begin
  if LineNumber = 0 then  //primera línea
    Result := 0
  else begin
    SetRange(CurrentRanges[LineNumber - 1]);
    Result := NestedBlocks;
  end;
end;
function TSynFacilSyn.TopCodeFoldBlock(DownIndex: Integer): TFaSynBlock;
//Función pública para TopCodeFoldBlockType() pero no devuelve puntero.
begin
  Result := TFaSynBlock(TopCodeFoldBlockType(DownIndex));
  if Result = nil then   //esto solo podría pasar en el bloque principal
    Result := MainBlk;
end;
function TSynFacilSyn.SetHighlighterAtXY(XY: TPoint): boolean;
//Pone al resaltador en una posición específica del texto, como si estuviera
//haciendo la exploración normal. Así se puede leer el estado.
//La posición XY, empieza en (1,1). Si tuvo exito devuelve TRUE.
var
  PosX, PosY: integer;
//  Line: string;
  Start: Integer;
begin
  Result := false;  //valor por defecto
  //validaciónes
  PosY := XY.Y -1;
  if (PosY < 0) or (PosY >= CurrentLines.Count) then exit;
  PosX := XY.X;
  if (PosX <= 0) then exit;
{  Line := CurrentLines[PosY];
  //validación
  if PosX >= Length(Line)+1 then begin
    //Está al final o más. Simula el estado al final de la línea
    //Este bloque se puede quitar
    SetLine(Line);
    SetRange(CurrentRanges[PosY]);   //carga estado de rango al final
    fTokenId := tkEol;        //marca final
    posFin := length(Line)+1;
    posIni := posFin;
    //posTok := ??? no se puede regenerar sin explorar
    Result := TRUE;
    exit;
  end;}
  //explora línea
  StartAtLineIndex(PosY);   //posiciona y hace el primer Next()
  while not GetEol do begin
    Start := GetTokenPos + 1;
    if (PosX >= Start) and (PosX < posFin+1) then begin
      //encontró
      //Token := GetToken;  //aquí se puede leer el token
      Result := TRUE;
      exit;
    end;
    Next;
  end;
  //No lo ubicó. Está más allá del fin de línea
  Result := TRUE;
end;
function TSynFacilSyn.ExploreLine(XY: TPoint; out toks: TATokInfo;
                                  out CurTok: integer): boolean;
//Explora la línea en la posición indicada. Devuelve la lista de tokens en toks[].
//También indica el orden del token actual.
//La posición XY, empieza en (1,1). Si tuvo exito devuelve TRUE.
var
  PosX, PosY: integer;
  idx: Integer;
begin
  Result := false;  //valor por defecto
  CurTok :=-1;       //valor por defecto
  idx := 0;
  setlength(toks,12);  //tamaño inicial
  //validaciónes
  PosY := XY.Y -1;
  if (PosY < 0) or (PosY >= CurrentLines.Count) then exit;
  PosX := XY.X;
  if (PosX <= 0) then exit;
  //explora línea
  StartAtLineIndex(PosY);   //posiciona y hace el primer Next()
  while not GetEol do begin
    //hay token
    if idx>high(toks) then
      setlength(toks, idx+12);  //aumenta espacio
    toks[idx].TokPos:=idx;
    toks[idx].txt := GetToken;
    toks[idx].length:=posFin - posIni;  //tamaño del token
    toks[idx].TokTyp:=fTokenID;
    toks[idx].posIni:=PosIni;
    toks[idx].IsIDentif:= (fLine[posIni] in charsIniIden);  //puede ser Keyword, o cualquier otro identificador
    toks[idx].curBlk := TopCodeFoldBlock(0);  //lee el rango
    Inc(idx);  //actualiza tamaño

    if (PosX > PosIni) and (PosX < posFin+1) then begin
      //encontró
      CurTok := idx-1;  //devuelve índice del token
      Result := TRUE;
    end;
    Next;
  end;
  //agrega el token final
  setlength(toks, idx+1);  //recorta al tamaño necesario
  toks[idx].TokPos:=idx;
  toks[idx].txt := GetToken;
  toks[idx].TokTyp:=fTokenID;
  toks[idx].posIni:=PosIni;
  toks[idx].curBlk := TopCodeFoldBlock(0);  //lee el rango
  Inc(idx);  //actualiza tamaño
  //verifica si lo ubicó.
  if CurTok = -1 then begin
    //No lo ubicó. Está más allá del fin de línea
    CurTok := idx-1;  //devuelve índice del token
    Result := TRUE;
  end;
end;
function TSynFacilSyn.SearchBeginBlock(level: integer; PosY: integer): integer;
//Busca en la linea "PosY", el inicio del bloque con nivel "level". Si no lo encuentra
//en esa línea, devuelve -1, indicando que debe estar en la línea anterior.
var
  niv1, niv2: Integer;   //niveles anterior y posterior
  ultApert : integer;    //posición de última apertura
begin
  ultApert := -1; //valor por defecto
  niv1 := NestedBlocksBegin(PosY); //Verifica el nivel al inicio de la línea
  //explora línea
  StartAtLineIndex(PosY);   //posiciona y hace el primer Next()
  while not GetEol do begin
    niv2 := NestedBlocks;   //lee nivel después de hacer Next()
    if (niv1 < level) and (niv2>=level) then begin
      ultApert:= posIni+1;   //último cambio de nivel que incluye al nivel pedido (posición inicial)
    end;
    niv1 := niv2;
    Next;
  end;
  //Terminó de explorar.
  Result := ultApert;
end;
function TSynFacilSyn.SearchEndBlock(level: integer; PosY: integer): integer;
//Busca en la linea "PosY", el fin del bloque con nivel "level". Si no lo encuentra
//en esa línea, devuelve MAXINT, indicando que debe estar en la línea siguiente.
var
  niv1, niv2: Integer;   //niveles anterior y posterior
begin
  Result := MAXINT; //valor por defecto
  niv1 := NestedBlocksBegin(PosY); //Verifica el nivel al inicio de la línea
  //explora línea
  StartAtLineIndex(PosY);   //posiciona y hace el primer Next()
  while not GetEol do begin
    niv2 := NestedBlocks;   //lee nivel después de hacer Next()
    if (niv1 >= level) and (niv2 < level) then begin
      Result := posIni+1; //cambio de nivel que incluye al nivel pedido
      exit;       //ya tiene los datos requeridos
    end;
    niv1 := niv2;
    Next;
  end;
  //Terminó de explorar y no encontró el cierre de blqoue.
  //hace la verificación del último token
  niv2 := NestedBlocks;   //lee nivel después del último Next()
  if (niv1 >= level) and (niv2 < level) then begin
    Result:= posIni+1; //cambio de nivel que incluye al nivel pedido
    exit;       //ya tiene los datos requeridos
  end;
  //Ya verificó el último token y no encontró el cierre. Sale com MAXINT
end;
procedure TSynFacilSyn.SearchBeginEndBlock(level: integer; PosX, PosY: integer;
                                      out pIniBlock, pEndBlock: integer);
//Explora una línea y devuelve el punto en la línea en que se abre y cierra el bloque de la
//posición PosX. "level" debe indicar el nivel del bloque buscado.
//Si no encuentra el inicio del bloque en la línea, devuelve -1
var
  niv1, niv2: Integer;   //niveles anterior y posterior
  Despues: boolean;      //bandera para indicar si se alcanzó al token
begin
  pIniBlock := -1; //valor por defecto
  pEndBlock := MAXINT;  //valor por defecto
  Despues := false;
  niv1 := NestedBlocksBegin(PosY); //Verifica el nivel al inicio de la línea
  //explora línea
  StartAtLineIndex(PosY);   //posiciona y hace el primer Next()
  while not GetEol do begin
    niv2 := NestedBlocks;   //lee nivel después de hacer Next()
    //verifica cambio de nivel
    if Despues then begin  //ya pasó al token actual
      if (niv1 >= level) and (niv2 < level) then begin
        pEndBlock:= posIni+1; //cambio de nivel que incluye al nivel pedido
        exit;       //ya tiene los datos requeridos
      end;
    end else begin        //aún no pasa al token actual
      if (niv1 < level) and (niv2>=level) then begin
        pIniBlock:= posIni+1;   //último cambio de nivel que incluye al nivel pedido (posición inicial)
      end;
    end;
    //verifica
    if (PosX >= posIni + 1) and (PosX < posFin+1) then begin
      //llegó a la posición pedida
      Despues := true;
//      exit;    //Sale con el último "pIniBlock"
    end;
    niv1 := niv2;
    Next;
  end;
  //terminó de explorar la línea y no encontró el cierre de bloque
  if Despues then begin  //ya pasó al token actual, pero no encontró el cierre
    //hace la verificación del último token
    niv2 := NestedBlocks;   //lee nivel después del último Next()
    if (niv1 >= level) and (niv2 < level) then begin
      pEndBlock:= posIni+1; //cambio de nivel que incluye al nivel pedido
      exit;       //ya tiene los datos requeridos
    end;
  end else begin      //aún no pasa al token actual
    //No lo ubicó. PosX está más allá del fin de línea. Sale con el último "pIniBlock"
  end;
end;
procedure TSynFacilSyn.GetBlockInfoAtXY(XY: TPoint; out blk: TFaSynBlock;
                                        out level: integer);
//Da información sobre el bloque en la posición indicada.
begin
  SetHighlighterAtXY(XY);        //posiciona
  blk := TopBlock();  //lee bloque
  //level := CodeFoldRange.CodeFoldStackSize; no considera los que tienen IncreaseLevel=FALSE
  level := NestedBlocks;
end;
function TSynFacilSyn.GetBlockInfoAtXY(XY: TPoint; out blk: TFaSynBlock;
  out BlockStart: TPoint; out BlockEnd: TPoint): boolean;
//Da información sobre el bloque en la posición indicada. Si hay error devuelve FALSE.
//BlockStart y BlockEnd, tienen sus coordenadas empezando en 1.
var
  nivel: Integer;
//  PosY: integer;
begin
  Result := SetHighlighterAtXY(XY);        //posiciona
  if Result=false then begin  //hubo error
    blk := nil;
    exit;
  end;
  blk := TopBlock();  //lee bloque
  //busca coordenadas del bloque
  nivel := NestedBlocks;   //ve el nivel actual
  BlockStart.y := XY.y;
  BlockEnd.y := XY.y;
  SearchBeginEndBlock(nivel, XY.x, BlockStart.y-1,BlockStart.x, BlockEnd.x);
  //busca posición de inicio
  if BlockStart.x = -1 then begin  //no se encontró en la línea actual
    while (BlockStart.y>1) do begin
      Dec(BlockStart.y);  //busca en la anterior
      BlockStart.x := SearchBeginBlock(nivel, BlockStart.y-1);
      if BlockStart.x<>-1 then break;  //encontró
    end;
  end;
  //busca posición de fin de bloque
  if BlockEnd.x = MAXINT then begin  //no se encontró en la línea actual
    while (BlockEnd.y < CurrentLines.Count) do begin
      Inc(BlockEnd.y);  //busca en la anterior
      BlockEnd.x := SearchEndBlock(nivel, BlockEnd.y-1);
      if BlockEnd.x<>MAXINT then break;  //encontró
    end;
//    if BlockEnd.x = MAXINT then   //llegó al final, y no encontró el final
//      BlockEnd.x := length(CurrentLines[BlockEnd.y-1])+1;
  end;
end;
//Utilidades para analizadores léxicos
function TSynFacilSyn.GetX: Integer; inline;
begin
  Result:=posIni+1;  //corrige
end;
function TSynFacilSyn.GetY: Integer; inline;
begin
  Result:=lineIndex+1;  //corrige
end;
function TSynFacilSyn.GetXY: TPoint;
//Devuelve las coordenadas de la posicón actual de exploración.
//El inicio del texto inicia en (1,1)
begin
  Result.x:=posIni+1;  //corrige
end;
function TSynFacilSyn.GetState: TFaLexerState;
//Devuelve el estado actual del resaltador, pero sin considerar el estado de los bloque,
//solo el estado de tokens y rangos.
{ TODO : Tal vez deba incluirse también a "FIsInNextToEOL" }
begin
  //Propiedades fijadas al inicio de la línea y no cambian en toda la línea.
//  Result.fLine    := fLine;
//  Result.tamLin   := tamLin;
  Result.LineText := fLine;  //copia cadena indirectamente, porque "LineText", no es accesible
  Result.LineIndex:= LineIndex;  //define a la posición vertical
  //propiedades que van cambiando conforme se avanza en la exploración de la línea
  Result.posTok   := posTok;
  Result.BlkToClose:= CloseThisBlk;
  Result.posIni   := posIni;   //define la posición horizontal
  Result.posFin   := posFin;
  Result.fRange   := fRange;
  Result.fTokenID := fTokenID;
end;
procedure TSynFacilSyn.SetState(state: TFaLexerState);
//Configura el estado actual del resaltador, pero sin considerar el estado de los bloque,
//solo el estado de tokens y rangos.
//Al cambiar el estado actual del resaltador, se pierde el estado que tenía.
begin
  //Propiedades fijadas al inicio de la línea y no cambian en toda la línea.
  //fLine      := state.fLine;
  //tamLin     := state.tamLin;
  //LineText   := state.LineText;
  //LineIndex  := state.LineIndex;
  Setline(state.LineText, state.LineIndex);   {Como "LineText" y "LineIndex" no son
           accesibles, se usa SetLine(), y de paso se actualiza "fLine" y "tamLin"}
  //propiedades que van cambiando conforme se avanza en la exploración de la línea
  posTok     := state.posTok;
  CloseThisBlk := state.BlkToClose;
  posIni     := state.posIni;
  posFin     := state.posFin;
  fRange     := state.fRange;
  fTokenID   := state.fTokenID;
end;

// ******************** métodos OVERRIDE ********************//
procedure TSynFacilSyn.SetLine(const NewValue: String; LineNumber: Integer);
{Es llamado por el editor, cada vez que necesita actualizar la información de coloreado
 sobre una línea. Despues de llamar a esta función, se espera que GetTokenEx, devuelva
 el token actual. Y también después de cada llamada a "Next".}
begin
  inherited;
  fLine := PChar(NewValue); //solo copia la dirección para optimizar
//debugln('SetLine('+ IntToStr(LineNumber)+'): ' + fLine );
  tamLin := length(NewValue);
  posTok := 0;  //inicia contador
  posFin := 0;  //apunta al primer caracter
  CloseThisBlk := nil;   //inicia bandera
  Next;
end;
procedure TSynFacilSyn.Next;
{Es llamado por SynEdit, para acceder al siguiente Token. Y es ejecutado por cada token de
 la línea en curso.
 En nuestro caso "posIni" debe quedar apuntando al inicio del token y "posFin" debe
 quedar apuntando al inicio del siguiente token o al caracter NULL (fin de línea).}
begin
  //verifica si hay cerrado de bloque pendiente del token anterior
  if CloseThisBlk<>nil then begin
    EndBlockFa(CloseThisBlk);
    if CloseThisBlk.CloseParent then begin
      //debe cerrar también al padre
      EndBlockFa(TopBlock);
    end;
    CloseThisBlk := nil;
  end;
  Inc(posTok);  //lleva la cuenta del orden del token
//  if posTok=1 then begin
//    if OnFirstTok<>nil then OnFirstTok;
//  end;

  posIni := posFin;   //apunta al primer elemento
  if fRange = nil then begin
      charIni:=fLine[posFin]; //guardar para tenerlo disponible en el método que se va a llamar.
      fProcTable[charIni];    //Se ejecuta la función que corresponda.
  end else begin
    if posFin = tamLin then begin  //para acelerar la exploración
      fTokenID:=tnEol;
      exit;
    end;
    {Debe actualizar el estado del rango porque las líneas no necesariamente se exploran
     consecutivamente}
    fTokenID:=fRange^.tTok;  //tipo de token
    delTok := fRange^.dEnd;  //delimitador de rango
    folTok := fRange^.folTok; //bandera para cerrar plegado
    fRange^.pRange;   //ejecuta método
  end;
end;
function TSynFacilSyn.GetEol: Boolean;
begin
  Result := fTokenId = tnEol;
end;
procedure TSynFacilSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := posFin - posIni;
  TokenStart := FLine + posIni;
end;
function TSynFacilSyn.GetTokenAttribute: TSynHighlighterAttributes;
{Debe devolver el atributo para el token actual. El token actual se actualiza con
 cada llamada a "Next", (o a "SetLine", para el primer token de la línea.)
 Esta función es la que usa SynEdit para definir el atributo del token actual}
var topblk: TFaSynBlock;
begin
  Result := Attrib[fTokenID];  //podría devolver "tkEol"
  if Result<> nil then begin
    //verifica coloreado de bloques
    case ColBlock of
    cbLevel: begin  //pinta por nivel
        Result.Background:=RGB(255- CodeFoldRange.CodeFoldStackSize*25,255- CodeFoldRange.CodeFoldStackSize*25,255);
      end;
    cbBlock: begin  //pinta por tipo de bloque
        topblk := TopBlockOpac;  //bloque con color
        //asigna color
        Result.Background:=topblk.BackCol;
      end;
    end;
  end;
end;
function TSynFacilSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
{Este método es llamado por la clase "TSynCustomHighlighter", cuando se accede a alguna de
 sus propiedades:  CommentAttribute, IdentifierAttribute, KeywordAttribute, StringAttribute,
 SymbolAttribute o WhitespaceAttribute.}
begin
  case Index of
    SYN_ATTR_COMMENT   : Result := tkComment;
    SYN_ATTR_IDENTIFIER: Result := tkIdentif;
    SYN_ATTR_KEYWORD   : Result := tkKeyword;
    SYN_ATTR_WHITESPACE: Result := tkSpace;
    SYN_ATTR_STRING    : Result := tkString;
    SYN_ATTR_SYMBOL    : Result := tkSymbol;
    else Result := nil;
  end;
end;
function TSynFacilSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := posFin - posIni;
  SetString(Result, (FLine + posIni), Len);
end;
function TSynFacilSyn.GetTokenPos: Integer;
begin
  Result := posIni;
end;
function TSynFacilSyn.GetTokenKind: integer;
begin
  Result := fTokenId;
end;
{Implementación de las funcionalidades de rango}
procedure TSynFacilSyn.ResetRange;
begin
  inherited;
  fRange := nil;
end;
function TSynFacilSyn.GetRange: Pointer;
begin
  CodeFoldRange.RangeType := fRange;
  Result := inherited GetRange;
  //debugln('  GetRange: ' + fLine + '=' + IntToStr(Integer(fRange)) );
end;
procedure TSynFacilSyn.SetRange(Value: Pointer);
begin
//debugln(' >SetRange: ' + fLine + '=' + IntToStr(PtrUInt(Value)) );
  inherited SetRange(Value);
  fRange := CodeFoldRange.RangeType;
end;
constructor TSynFacilSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  lisTmp := TStringList.Create;   //crea lista temporal
  tc1:= tFaTokContent.Create;
  tc2:= tFaTokContent.Create;
  tc3:= tFaTokContent.Create;
  tc4:= tFaTokContent.Create;

  CaseSensitive := false;
  fRange := nil;     //inicia rango

  ClearSpecials;     //Inicia matrices
  CreateAttributes;  //crea los atributos
  lisBlocks:=TFaListBlocks.Create(true);  //crea lista de bloques con control
  //Crea bloque global
  MainBlk   := TFaSynBlock.Create;
  MainBlk.name:='Main';   //Nombre especial
  MainBlk.index:=-1;
  MainBlk.showFold:=false;
  MainBlk.parentBlk:=nil;  //no tiene ningún padre
  MainBlk.BackCol:=clNone;
  MainBlk.UniqSec:=false;
  MainBlk.CloseParent:=false;  //No tiene sentido porque este bloque no tiene padre
  //Crea bloque para tokens multilínea
  MulTokBlk  := TFaSynBlock.Create;
  MulTokBlk.name:='MultiToken';
  MulTokBlk.index:=-2;
  MulTokBlk.showFold:=true;  //Dejar en TRUE, porque así trabaja
  MulTokBlk.parentBlk:=nil;
  MulTokBlk.BackCol:=clNone;
  MulTokBlk.UniqSec:=false;
  MulTokBlk.CloseParent:=false;

  ClearMethodTables;   //Crea tabla de funciones
  DefTokIdentif('[A-Za-z$_]','[A-Za-z0-9_]*');
end;
destructor TSynFacilSyn.Destroy;
begin
  MulTokBlk.Free;
  MainBlk.Free;
  lisBlocks.Destroy;        //libera
  tc1.Destroy;
  tc2.Destroy;
  tc3.Destroy;
  tc4.Destroy;
  lisTmp.Destroy;
  //no es necesario destruir los attrributos, porque  la clase ya lo hace
  inherited Destroy;
end;

end.

