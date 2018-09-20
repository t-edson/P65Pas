{Esta unidad Define a la clase "TParserDirecBase" que es una capa que se coloca sobre
TCompilerBase para implementar las funcionalidades del "Parser" para las directivas.}
unit ParserDirec;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, math, Graphics, Parser, SynFacilHighlighter, XpresBas,
  MisUtils, Globales, XpresElementsPIC;
type  //Tipos para manejo de expresiones
  TDirDatType = (ddtNumber, ddtString);

  { TDirOperand }
  //Tipo expresión u operando. Se usa para manejo de evaluación aritmética.
  TDirOperand = object
  private
    FvalStr: string;
    FvalNum: Double;
    function GetvalNum: Double;
    procedure SetvalNum(AValue: Double);
    function GetvalStr: string;
    procedure SetvalStr(AValue: string);
  public  //Campos principales
    datTyp: TDirDatType;  //Tipo de dato
    property valNum: Double read GetvalNum write SetvalNum ;    //Valor numérico de la expresión
    property valStr: string read GetvalStr write SetvalStr;  //Valor cadena de la expresión
    procedure SetBool(value: boolean);
  End;

  TDirEveReadNum =  function: Single of object;
  TDirEveWriteNum =  procedure(AValue: Single) of object;
  TDirEveReadStr =  function: String of object;
  TDirEveWriteStr =  procedure(AValue: String) of object;
  TDirEveCallProc = procedure of object;

  { TDirVar }
  //Define a una variable.
  TDirVar= class
  private  //Eventos
    {Estos eventos se usan cuando se requiere direccionar, la lectura/escritura de
    valores de la expresión.}
    OnReadNum: TDirEveReadNum;
    OnWriteNum: TDirEveWriteNum;
    OnReadStr: TDirEveReadStr;
    OnWriteStr: TDirEveWriteStr;
  private
    Fvalor: TDirOperand;
    function Getvalor: TDirOperand;
    procedure Setvalor(AValue: TDirOperand);
  public
    nomb: string;   //Nombre de la variable
    property valor: TDirOperand read Getvalor write Setvalor;
    procedure ReflectToNumber(ReadNum: TDirEveReadNum; WriteNum: TDirEveWriteNum);
    procedure ReflectToString(ReadStr: TDirEveReadStr; WriteStr: TDirEveWriteStr);
    //property datTyp: TDirDatType read Fvalor.datTyp write Fvalor.datTyp;
  end;
  TDirVar_list = specialize TFPGObjectList<TDirVar>;

type
  //Identifica a una macro
  TDirMacro = class
    name  : string;
    value : string;
    posDef: TSrcPos;   //posición del contexto
  end;
  TDirMacro_list = specialize TFPGObjectList<TDirMacro>;

  //Identifica a una instrucción
  TDirInstruc = class
    name   : string;
    OnCall : TDirEveCallProc;
  end;
  TDirInstruc_list = specialize TFPGObjectList<TDirInstruc>;


  { TParserDirecBase }
  TParserDirecBase = class(TCompilerBase)
  private  //Parser and Expressions evaluation
    tokIni : integer;  //Posición inicial del token actual
    dirOperator: Integer;
    dirDelimiter: integer;
    WaitForEndIF: integer;
    function GetIdent: string;
    function tokType: integer;
    function CogOperando: TDirOperand;
    procedure IniExplorDirec(out lin: string);
    function CogCarERR(car: char): Boolean;
    function CogExpresion(jerar: Integer): TDirOperand;
    function CogExpresionPar: TDirOperand;
    function CogIdentif(out s: string): boolean;
    function CogNumero(var n: Double): boolean;
    function cogOperador: String;
    function jerOp(operad: String): Integer;
    function Evaluar(Op1: TDirOperand; opr: String; Op2: TDirOperand): TDirOperand;
  private  //Instructions implementation
    function modeStr: string;
    function ScanIFDEF(out tok: string): boolean;
    procedure ProcCLEAR_STATE_RAM;
    procedure ProcSET_STATE_RAM;
    procedure ProcERROR;
    procedure ProcINFO;
    procedure ProcWARNING;
    procedure ProcSET;
    procedure ProcMSGBOX;
    procedure ProcMSGERR;
    procedure ProcMSGWAR;
    procedure ProcMODE;
    procedure ProcDEFINE(lin: string);
    procedure ProcIF(lin: string; negated: boolean);
    procedure ProcELSE;
    procedure ProcENDIF;
    procedure ProcIFDEF(lin: string; negated: boolean);
    procedure ProcOUTPUTHEX(lin: string);
    procedure ProcINCLUDE(lin: string; var ctxChanged: boolean);
    procedure ProcORG;
    procedure ProcFREQUENCY;
  private  //Access to system variables
    function read_CURRBLOCK: String;
    function read_PIC_FREQUEN: Single;
    function read_ORG: Single;
    function read_PIC_MAXFREQ: Single;
    function read_PIC_MODEL: string;
    function read_SYN_MODE: String;
    procedure write_PIC_FREQUEN(AValue: Single);
    procedure write_ORG(AValue: Single);
    procedure write_PIC_MAXFREQ(AValue: Single);
    procedure write_PIC_MODEL(AValue: string);
    procedure write_SYN_MODE(AValue: String);
  private  //Macros
    macroList : TDirMacro_list;  //List of macros
    procedure NewMacro(macName, macValue: string);
    function DefinedMacro(macName: string): boolean;
    function DefinedMacro(macName: string; out dmac: TDirMacro): boolean;
  private  //Instructions and Variables
    instList : TDirInstruc_list;  //List of instruction accepted
    varsList : TDirVar_list;  //List of variables
    function DefinedInstruc(insName: string; out dins: TDirInstruc): boolean;
    function DefinedVar(cad: string; out dvar: TDirVar): boolean;
    function AsigVariable(VarName: string; const value: TDirOperand): TDirVar;
  public   //Public
    lexDir : TSynFacilSyn;  //lexer para analizar directivas
    procedure skipWhites;
    procedure GenErrorDir(msg: string);
    procedure GenErrorDir(msg: string; const Args: array of const);
    procedure AddInstruction(instName: string; callProc: TDirEveCallProc);
    procedure AddSysVariableNumber(varName: string; ReadNum: TDirEveReadNum;
                                                    WriteNum: TDirEveWriteNum);
    procedure AddSysVariableString(varName: string; ReadStr: TDirEveReadStr;
                                                    WriteStr: TDirEveWriteStr);
    procedure ProcDIRline(const AsmLin: string; out ctxChanged: boolean);
    procedure DefLexDirectiv;
    procedure ClearMacros;
  public   //Initialization
    constructor Create; override;
    destructor Destroy; override;
  end;
var
  ER_UNKNO_DEVIC: String;  //Error message for Unknown Device

  procedure SetLanguage;

implementation
var
  ER_UNKNO_DIREC, ER_ERROR_DIREC: String;
  ER_UNEXP_ELSE, ER_ENDIF_NOFOU, ER_UNEXP_ENDIF : string;
  ER_MODE_UNKNOWN,
  ER_ERROR_FREQ, ER_IDENT_EXPEC, ER_EXPEC_EQUAL,
  ER_SYNTAX_ERRO, ER_SYNTAX_ERR_: string;
  ER_EXPECTED_BR: String;
  ER_FILE_NO_FND_, ER_ERIN_NUMBER_, ER_UNKNW_IDENT_: String;
  ER_DIVIDE_ZERO, ER_EVA_ZER_ZER, ER_OPE_NOT_IMP_: String;
  ER_EXPECT_CAR_: String;
  ER_TOOHIGHFRE: String;

procedure SetLanguage;
begin
//  ParserAsm_PIC16.SetLanguage;
{$I ..\language\tra_ParserDirec.pas}
end;
{ TDirOperand }
function TDirOperand.GetvalNum: Double;
begin
  if datTyp = ddtNumber then begin
    Result := FvalNum;
  end else begin  //es cadena
    //Trata de obtener su valor numérico
    if not TryStrToFloat(FvalStr , Result) then exit(0);
  end;
end;
procedure TDirOperand.SetvalNum(AValue: Double);
begin
  datTyp := ddtNumber;   //fuerza a que sea numéro
  FvalNum := AValue;
end;
function TDirOperand.GetvalStr: string;
begin
  if datTyp = ddtString then begin
    Result := FvalStr;
  end else begin  //es número
    Result := FloatToStr(FvalNum);
  end;
end;
procedure TDirOperand.SetvalStr(AValue: string);
begin
  datTyp := ddtString;   //fuerza a que sea string
  FvalStr := AValue;
end;
procedure TDirOperand.SetBool(value: boolean);
{Asigna un valro booleano al operando. En realidad, no hay valores booleando, así
que se usará los números 0 o 1}
begin
  if value then valNum := 1 else valNum := 0;
end;
{ TDirVar }
function TDirVar.Getvalor: TDirOperand;
begin
  //Primero actualiza en caso de que este enlazada.
  {Se actualizan ambos tipos porque, si por ejemplo, el tipo es numérico, pero se pide
  como cadena, se debe tener actualizdo su valor numérico, por si se hace una conversión.
  Habría que ver, si esta es la forma más óptima, de implementarlo.}
  if OnReadNum<>nil then Fvalor.valNum := OnReadNum();
  if OnReadStr<>nil then Fvalor.valStr := OnReadStr();
  //Ahora devuelve valor actualizado
  Result := Fvalor;
end;
procedure TDirVar.Setvalor(AValue: TDirOperand);
begin
  Fvalor := AValue;  //actualiza
  //Llama a eventos para actualziar valor reflejado
  if OnWriteNum<>nil then OnWriteNum(Fvalor.FvalNum);
  if OnWriteStr<>nil then OnWriteStr(Fvalor.FvalStr);
end;
procedure TDirVar.ReflectToNumber(ReadNum: TDirEveReadNum;
  WriteNum: TDirEveWriteNum);
{Define que la variable esté reflejada a otra variable numérica, mediante eventos.}
begin
  Fvalor.datTyp := ddtNumber;  //fija como número
  OnReadNum := ReadNum;        //asigna eventos
  OnWriteNum := WriteNum;
end;
procedure TDirVar.ReflectToString(ReadStr: TDirEveReadStr;
  WriteStr: TDirEveWriteStr);
begin
  Fvalor.datTyp := ddtString;  //fija como cadena
  OnReadStr := ReadStr;        //asigna eventos
  OnWriteStr := WriteStr;
end;
{ TParserDirecBase }
//Parser and Expressions evaluation
function TParserDirecBase.GetIdent: string;
begin
  if tokType = lexDir.tnSpace then
    lexDir.Next;  //quita espacios
  //verifica
  if lexDir.GetTokenKind <> lexDir.tnIdentif then begin
    GenErrorDir(ER_IDENT_EXPEC);
    exit;
  end;
  Result := lexDir.GetToken;
  lexDir.Next;  //toma identificador
end;
function TParserDirecBase.tokType: integer;
begin
  Result := lexdir.GetTokenKind;
end;
function TParserDirecBase.CogOperando: TDirOperand;
{Coge un operando en la posición actual del contexto. Si no enceuntra
el operando o es erróneo, genera Error.}
var
  cad , tmp: String;
  num : Double;
  exp : TDirOperand;
  mac: TDirMacro;
  p: TFaLexerState;
  dvar: TDirVar;
  delim: Char;
begin
  skipWhites;   //quita blancos iniciales
  if lexDir.GetEol then begin
    Result.datTyp := ddtString;
    Result.FvalStr := '';
    exit;
  end;
  if CogNumero(num) then begin
    if HayError then exit;  //pudo haber error en número
    Result.valNum := num;   //fija tipo a número
  end else if lexDir.GetTokenKind = lexDir.tnString then begin
    //Es cadena
    tmp := lexDir.GetToken;
    delim := tmp[1];
    tmp := copy(tmp, 2, length(tmp)-2);  //quita delimitadores
    if delim='"' then begin
      //Es cadena con comilla doble
      tmp := StringReplace(tmp, '\n', LineEnding, [rfReplaceAll]);
      tmp := StringReplace(tmp, '\r', chr($0D), [rfReplaceAll]);
      tmp := StringReplace(tmp, '\t', #9, [rfReplaceAll]);
    end;
    Result.valStr := tmp;
    lexDir.Next;
  end else if CogIdentif(cad) then begin
    {Es un identificador}
    //Busca si es macro
    if DefinedMacro(cad, mac) then begin
      //Es una macro. Hay que expandirla
      p := lexDir.State;  //guarda estado de lexer
      lexDir.SetLine(mac.value, 0);  //inicia otra explroación en el contenido de la macro
      Result := CogExpresion(0);
      lexDir.State := p;  //restaura estado del lexer, para que siga la expresión
      exit;
    end;
    //Busca si es una variable
    cad := UpCase(cad);
    for dvar in varsList do begin
      if UpCase(dvar.nomb) = cad then begin
        Result := dvar.valor;
        exit;
      end;
    end;
    //No es variable, ni macro, busca si es función
    case cad of
    'ABS': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := abs(exp.valNum);
      exit;  //sale sin error
    end;
    'SGN': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := Sign(exp.valNum);
      exit;  //sale sin error
    end;
    'SIN': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := sin(exp.valNum);
      exit;  //sale sin error
    end;
    'COS': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := cos(exp.valNum);
      exit;  //sale sin error
    end;
    'TAN': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := tan(exp.valNum);
      exit;  //sale sin error
    end;
    'LOG': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := ln(exp.valNum);
      exit;  //sale sin error
    end;
    'ROUND': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := round(exp.valNum);
      exit;  //sale sin error
    end;
    'TRUNC': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := trunc(exp.valNum);
      exit;  //sale sin error
    end;
    'LENGTH': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := length(exp.valStr);
      exit;  //sale sin error
    end;
    'UPCASE': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valStr := upcase(exp.valStr);
      exit;  //sale sin error
    end;
    'LOWCASE': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valStr := LowerCase(exp.valStr);
      exit;  //sale sin error
    end;
    end;
    //No es variable ni función.
    GenErrorDir(ER_UNKNW_IDENT_, [cad]);
  end else If lexDir.GetToken = '(' Then begin
    Result := CogExpresionPar;
    exit;  //Puede salir con error
  end else If lexDir.GetToken = '-' Then begin
    //Puede ser número negativo
    lexDir.Next;  //toma el signo
    Result := CogOperando();
    if HayError then exit;
    if Result.datTyp <> ddtNumber then begin
      GenErrorDir(ER_SYNTAX_ERRO);
      exit;  //no devuelve nada
    end;
    //Es un número
    Result.valNum := -Result.valNum;
    exit;  //Puede salir con error
  end else begin
    //Debe ser otra cosa
    GenErrorDir(ER_SYNTAX_ERRO);
    exit;  //no devuelve nada
  end;
end;
procedure TParserDirecBase.IniExplorDirec(out lin: string);
(*Inicia la exploración del token de directiva. Extrae el delimitador final "}", y
posiciona al lexer justo despues del delimitador inicial "{$". Devuelve la línea
procesada en "lin" (sin delimitador final). Además inicia tokIni
*)
var
  dlin: Integer;
  p: TSrcPos;
begin
  //Fija el inicio del token actual (Esto es válido, porque las directivas son "unilineas")
  tokIni := Cin.curCon.lex.GetX - 1;
  //Usa SynFacilSyn como lexer para analizar texto
  lin := Cin.tok;
  dlin := length(lin);
  if lin[dlin] = '}'  then begin
    delete(lin, dlin, 1);  //quita delimitador final de directiva
  end else begin
    //Es un error, pero es salvable.
    //Ubicamos el error, "manualmente", porque aún no hemos explorado con el lexer.
    p := cIn.ReadSrcPos;
    p.col := tokIni + dlin + 1;  //columna al final
    GenErrorPos(ER_EXPECTED_BR, [], p);
  end;
  //Inicia exploración con el lexer "lexDir"
  lexDir.SetLine(lin, 0);  //inicia cadena
  lexDir.Next;  //Salta el "{$"
  skipWhites;
end;
function TParserDirecBase.CogCarERR(car: char): Boolean;
{Coge el caracter indicado. Si no lo encuentra genera error y devuelve FALSE.}
begin
  if lexDir.GetToken=car then begin
    //Es el caracter buscado
    lexDir.Next;
    exit(true);
  end else begin
    GenErrorDir(ER_EXPECT_CAR_, [car]);
    exit(false);
  end;
end;
function TParserDirecBase.CogExpresion(jerar: Integer): TDirOperand;
{ Evaluador de expresiones. Toma una expresión completa, en la posición actual del
contenido. Si no encuentra una expresión, genera error. }
var Op1, Op2 : TDirOperand;
    opr, opr2 : String;
    jerOpr, jerOpr2: Integer;
    pos1, pos2: TFaLexerState;
begin
    skipWhites;  //quita blancos iniciales
    Op1 := CogOperando;  //error
    if HayError then exit;
    opr := cogOperador;
    if opr = '' Then begin
      Result := Op1;
      Exit
    End;
    jerOpr := jerOp(opr);     //Hay operador, tomar su jerarquía
    //-------------------------- ¿Delimitada por jerarquía? ---------------------
    if jerOpr <= jerar then begin  //es menor que la que sigue, expres.
      Result := Op1;  //solo devuelve el único operando que leyó
      Exit;
    End;
    while opr <> '' do begin
        pos1 := lexDir.State;    //Guarda por si lo necesita
        Op2 := CogOperando;
        if HayError then exit;
        pos2 := lexDir.State;    //Guarda por si lo necesita
        opr2 := cogOperador;
        If opr2 <> '' Then begin  //Hay otro operador
            jerOpr2 := jerOp(opr2);
            //¿Delimitado por jerarquía de operador?
            If jerOpr2 <= jerar Then begin  //sigue uno de menor jerarquía, hay que salir
                lexDir.State:= pos2;   //antes de coger el operador
                Result := Evaluar(Op1, opr, Op2);
                Exit;
            End;
            If jerOpr2 > jerOpr Then begin    //y es de mayor jerarquía, retrocede
                lexDir.State:= pos1;        //retrocede
                Op2 := CogExpresion(jerOpr);        //evalua primero
                opr2 := cogOperador;    //actualiza el siguiente operador
            End;
        End;

        Op1 := Evaluar(Op1, opr, Op2);    //evalua resultado
        if HayError then exit;
        opr := opr2;
        jerOpr := jerOp(opr);    //actualiza operador anterior
    end;
    Result := Op1;
end;
function TParserDirecBase.CogExpresionPar: TDirOperand;
{Coge una expresión que debe estar encerrada entre paréntesis. Puede genera error}
begin
  if not CogCarERR('(') then exit;  //sale con error
  Result := CogExpresion(0);
  if HayError then exit;  //sale con error
  skipWhites;
  if not CogCarERR(')') then exit;  //sale con error
end;
function TParserDirecBase.CogNumero(var n: Double): boolean;
{Veririfca si lo que sigues es un número y de ser así, intenta tomarlo.
Puede generar error al convertir el número}
var
  m: Longint;
begin
  if lexdir.GetTokenKind <> lexdir.tnNumber then exit(false);
  if lexdir.GetToken[1] = '$' then begin
    //Formato hexadecimal
    if not TryStrToInt(lexdir.GetToken, m) then begin
      GenErrorDir(ER_ERIN_NUMBER_, [lexDir.GetToken]);
      exit(false);
    end;
    n := m;
  end else begin
    if not TryStrToFloat(lexdir.GetToken, n) then begin
      GenErrorDir(ER_ERIN_NUMBER_, [lexDir.GetToken]);
      exit(false);
    end;
  end;
  lexdir.Next;
  Result := true;  //indica que hubo número
end;
function TParserDirecBase.CogIdentif(out s: string): boolean;
{Veririfca si lo que sigues es un identificador y de ser así, intenta tomarlo.}
begin
  if tokType = lexDir.tnSpace then
    lexDir.Next;  //quita espacios
  //verifica
  if lexDir.GetTokenKind <> lexDir.tnIdentif then begin
    exit(false);
  end;
  s := lexDir.GetToken;
  lexDir.Next;  //toma identificador
  exit(true);
end;
function TParserDirecBase.cogOperador: String;
{Coge un operador en la posición del contexto actual. Si no encuentra
 devuelve cadena vacía y no coge caracteres, salvo espacios iniciales.}
begin
  Result := '';
  skipWhites;     //quita blancos iniciales
  Case UpCase(lexDir.GetToken) of //completa con operador de más caracteres
  '+': begin
         Result := lexDir.GetToken;
         lexDir.next;
        end;
  '-': begin
         Result := lexDir.GetToken;
         lexDir.next;
      end;
  '*': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '/': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '\': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '%': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '^': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  //Operadores de comparación
  '=': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '<>': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '>': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '<': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '>=': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '<=': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  'AND': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  'OR': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  'XOR': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  'NOT': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  end;
end;
function TParserDirecBase.jerOp(operad: String): Integer;
//Devuelve la jerarquía de un operador ver documentación técnica.
begin
    case operad of
    'OR','XOR'             : Result := 2;
    'AND'                  : Result := 3;
    '=', '<>', '>', '<', '>=', '<=': Result := 4;
    '+', '-'               : Result := 5;
    '*', '/', '\', '%'     : Result := 6;
    '^', 'NOT'             : Result := 8;
    else
      Result := 0;
    end;
End;
function TParserDirecBase.Evaluar(Op1: TDirOperand; opr: String; Op2: TDirOperand): TDirOperand;
//Devuelve el resultado y tipo de una operación
begin
    Case opr of
    '': begin     //Sin operador. Y se supone sin Op2
          //no hay nada que hacer, ya está en la pila
          Result := Op1;
        end;
    '+': begin
           //Puede ser concatenación o suma
           if (Op1.datTyp = ddtString) or (Op2.datTyp = ddtString) then begin
             //Al menos uno es cadena
             Result.valStr := Op1.valStr + Op2.valStr;  //cadena
           end else begin
             //Son dos números, los suma
             Result.valNum := Op1.valNum + Op2.valNum;  //número
           end;
         end;
    '-': begin
          Result.valNum := Op1.valNum - Op2.valNum;
         end;
    '*': begin
          Result.valNum := Op1.valNum * Op2.valNum;
         end;
    '/': begin
          if Op2.valNum = 0 Then
              GenErrorDir(ER_DIVIDE_ZERO)
          else begin   //error
              Result.valNum := Op1.valNum / Op2.valNum;
          End;
         end;
    '\': begin
          if Op2.valNum = 0 then
              GenErrorDir(ER_DIVIDE_ZERO)
          else begin   //error
              Result.valNum := round(Op1.valNum) div round(Op2.valNum);
          end;
         end;
    '%': begin
          if Op2.valNum = 0 then
              GenErrorDir(ER_DIVIDE_ZERO)
          else begin   //error
              Result.valNum := round(Op1.valNum) mod round(Op2.valNum);
          end;
         end;
    '^': begin
          if (Op2.valNum = 0) and (Op2.valNum = 0) then
              GenErrorDir(ER_EVA_ZER_ZER)
          else begin   //error
              Result.valNum := power(Op1.valNum, Op2.valNum);
          end;
         end;
    //Operadores de comparación
    '=': begin
          //Puede ser concatenación o suma
          if (Op1.datTyp = ddtString) or (Op2.datTyp = ddtString) then begin
            //Al menos uno es cadena, compara cadenas
            Result.SetBool(Op1.valStr = Op2.valStr);
          end else begin
            //Son dos números, compara valores
            Result.SetBool(Op1.valNum = Op2.valNum);
          end;
         end;
    '<>': begin
          //Puede ser concatenación o suma
          if (Op1.datTyp = ddtString) or (Op2.datTyp = ddtString) then begin
            //Al menos uno es cadena, compara cadenas
            Result.SetBool(Op1.valStr <> Op2.valStr);
          end else begin
            //Son dos números, compara valores
            Result.SetBool(Op1.valNum <> Op2.valNum);
          end;
         end;
    '>': begin
          //Puede ser concatenación o suma
          if (Op1.datTyp = ddtString) or (Op2.datTyp = ddtString) then begin
            //Al menos uno es cadena, compara cadenas
            Result.SetBool(Op1.valStr > Op2.valStr);
          end else begin
            //Son dos números, compara valores
            Result.SetBool(Op1.valNum > Op2.valNum);
          end;
         end;
    '<': begin
          //Puede ser concatenación o suma
          if (Op1.datTyp = ddtString) or (Op2.datTyp = ddtString) then begin
            //Al menos uno es cadena, compara cadenas
            Result.SetBool(Op1.valStr < Op2.valStr);
          end else begin
            //Son dos números, compara valores
            Result.SetBool(Op1.valNum < Op2.valNum);
          end;
         end;
    '>=': begin
          //Puede ser concatenación o suma
          if (Op1.datTyp = ddtString) or (Op2.datTyp = ddtString) then begin
            //Al menos uno es cadena, compara cadenas
            Result.SetBool(Op1.valStr >= Op2.valStr);
          end else begin
            //Son dos números, compara valores
            Result.SetBool(Op1.valNum >= Op2.valNum);
          end;
         end;
    '<=': begin
          //Puede ser concatenación o suma
          if (Op1.datTyp = ddtString) or (Op2.datTyp = ddtString) then begin
            //Al menos uno es cadena, compara cadenas
            Result.SetBool(Op1.valStr <= Op2.valStr);
          end else begin
            //Son dos números, compara valores
            Result.SetBool(Op1.valNum <= Op2.valNum);
          end;
         end;
    //Operadores lógicos
    //Falta ...
    else begin
        GenErrorDir(ER_OPE_NOT_IMP_, [opr]);
        Exit;
         End;
    end;
end;
//Instructions implementation
function TParserDirecBase.modeStr: string;
begin
  case mode of
  modPascal: Result := 'modPascal';
  modPicPas: Result := 'modPicPas';
  else
    Result := 'Unknown';
  end;
end;
function TParserDirecBase.ScanIFDEF(out tok: string): boolean;
{Explora el texto, hasta encontrar la directiva $ENDIF o $ELSE.  Si llega al
 final del contexto, sin encontrar alguna de estas directivas, devuelve FALSE.}
var
  tmp, direc: string;
begin
  while not cIn.Eof do begin
//    debugln(cIn.tok);
    if cIn.tokType = tnDirective then begin
      //Podría ser el delimitador buscado
      IniExplorDirec(tmp);
      direc := UpperCase(lexDir.GetToken);
      if (direc = 'ENDIF') or (direc='ELSE') then begin
        //Encontró el delimitador
        tok := direc;
        cIn.Next;  //toma el token
        exit(true);  //y continúa
      end;
    end;
    cIn.Next;
  end;
  //No encontró
  exit(false);
end;
procedure TParserDirecBase.ProcOUTPUTHEX(lin: string);
var
  filPath: String;
begin
  lexDir.Next;  //pasa al siguiente
  filPath := CogExpresion(0).valStr;
  if HayError then Exit;
  filPath := ExpandRelPathTo(mainFile, filPath);  //Completa ruta, si es relativa
  //Por simplicidad se permite realizar esto en la primera y segunda pasada
  //Auqnue lo más práctico sería en la segunda pasada donde se genera el HEX final.
  hexfile := filPath;
end;
procedure TParserDirecBase.ProcDEFINE(lin: string);
var
  Ident, value: String;
begin
  lexDir.Next;  //pasa al siguiente
  Ident := GetIdent;
  if HayError then exit;
  skipWhites;
  if lexDir.GetEol then begin
    //Se definió un identificador sin valor
    NewMacro(Ident, '');
  end else begin
    //Sigue algo más
    if lexDir.GetToken <> '=' then begin
      GenErrorDir(ER_EXPEC_EQUAL);
      exit;
    end;
    lexDir.Next;  //toma símbolo
    skipWhites;
    if lexDir.GetEol then begin
      GenErrorDir(ER_SYNTAX_ERRO);
    end;
    //Toma definición
    value := copy(lin, lexDir.GetX, length(lin));
    NewMacro(Ident, value);
  end;
end;
procedure TParserDirecBase.ProcIFDEF(lin: string; negated: boolean);
  function EvaluateExp(const Ident: string): boolean;
  {Evalúa el resultado de la expresión de la directiva $IFDEF.
  Debería ejecutarse solo una vez, en ProcIFDEF(()}
  var
    xDirec: TxpEleDIREC;
    ele: TxpElement;
    dvar: TDirVar;
  begin
    if FirstPass then begin
      //Agrega el nodo para guardar información para la segunda pasada
      xDirec := TxpEleDIREC.Create;
      xDirec.srcDec := cIn.ReadSrcPos;   //guarda posición de aparición
      TreeDirec.AddElement(xDirec, false);  //Agrega sin verificación de nombre
      //Evalúa
      Result := (DefinedMacro(Ident) or DefinedVar(Ident, dvar)) xor negated;
      //Guarda resultado
      xDirec.ifDefResult := Result;
    end else begin
      {En al segunda pasada, ya no se evalúa, porque la segunda pasada, ya no se
      hace en el orden del código fuente, y se pierde la secuencia de directivas.}
      for ele in TreeDirec.curNode.elements do begin
        //Busca la directiva de la dirección actual (ubicada en la primera pasada)
        if ele.srcDec.EqualTo(cIn.ReadSrcPos) then begin
          //Encontró
          Result := TxpEleDIREC(ele).ifDefResult;
          exit;
        end;
      end;
      //No encontró. Esto no debería pasar.
      MsgErr('Implementation error.');
    end;
  end;
var
  Ident, direc: String;
begin
  lexDir.Next;  //pasa al siguiente
  Ident := GetIdent;
  if HayError then exit;
  skipWhites;
  if lexDir.GetEol then begin
    //Esto es lo normal. Buscamos el identificador
    if EvaluateExp(Ident) then begin
      //Está definido
      inc(WaitForEndIF);  //marca bandera para esperar
    end else begin
      //No está definido, no se debe compilar hasta un {$ENDIF} o un {$ELSE}
      cIn.Next;  //toma token {$IDEF  }
      //Explora, sin compilar, hasta encontrar directiva delimitadora.
      if not ScanIFDEF(direc) then begin
        //Llegó al final del código fuente, sin encontrar el ENDIF
        GenErrorDir(ER_ENDIF_NOFOU);
        exit;
      end;
      //Encontró token delimitador
      //Si es $ENDIF, no hay problema, todo termina allí, pero si es un else:
      if direc='ELSE' then begin
        inc(WaitForEndIF);  //marca bandera para esperar
      end;
    end;
  end else begin
    //Sigue algo más. No se esperaba.
    GenErrorDir(ER_SYNTAX_ERRO);
  end;
end;
procedure TParserDirecBase.ProcIF(lin: string; negated: boolean);
  function EvaluateExp: boolean;
  {Evalúa el resultado de la expresión de la directiva $IFDEF.
  Debería ejecutarse solo una vez, en ProcIFDEF(()}
  var
    xDirec: TxpEleDIREC;
    ele: TxpElement;
    varValue: TDirOperand;
  begin
    if FirstPass then begin
      //Agrega el nodo para guardar información para la segunda pasada
      xDirec := TxpEleDIREC.Create;
      xDirec.srcDec := cIn.ReadSrcPos;   //guarda posición de aparición
      TreeDirec.AddElement(xDirec, false);  //Agrega sin verificación de nombre
      //Evalúa
      varValue := CogExpresion(0);
      //No debería seguir nada más
      if not lexDir.GetEol then begin
        GenErrorDir(ER_SYNTAX_ERRO);
        exit;
      end;
      if varValue.datTyp = ddtNumber then begin
        //En números, cualquier valor <>0 se considera verdadero
        Result := (varValue.valNum<>0) xor negated;
      end else begin
        //En cadenas, cualquier cadena no nula se considera verdadero
        Result := (varValue.valStr<>'') xor negated;
      end;
      //Guarda resultado
      xDirec.ifDefResult := Result;
    end else begin
      {En al segunda pasada, ya no se evalúa, porque la segunda pasada, ya no se
      hace en el orden del código fuente, y se pierde la secuencia de directivas.}
      for ele in TreeDirec.curNode.elements do begin
        //Busca la directiva de la dirección actual (ubicada en la primera pasada)
        if ele.srcDec.EqualTo(cIn.ReadSrcPos) then begin
          //Encontró
          Result := TxpEleDIREC(ele).ifDefResult;
          exit;
        end;
      end;
      //No encontró. Esto no debería pasar.
      MsgErr('Implementation error.');
    end;
  end;
var
  direc: String;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  if lexDir.GetEol then begin
    GenErrorDir(ER_SYNTAX_ERRO);
    exit;
  end;
  //Se supone que sigue una expresión
  if EvaluateExp then begin
    //Es verdadero
    inc(WaitForEndIF);  //marca bandera para esperar
  end else begin
    //No es verdadero, no se debe compilar hasta un {$ENDIF} o un {$ELSE}
    cIn.Next;  //toma token {$IDEF  }
    //Explora, sin compilar, hasta encontrar directiva delimitadora.
    if not ScanIFDEF(direc) then begin
      //Llegó al final del código fuente, sin encontrar el ENDIF
      GenErrorDir(ER_ENDIF_NOFOU);
      exit;
    end;
    //Encontró token delimitador
    //Si es $ENDIF, no hay problema, todo termina allí, pero si es un else:
    if direc='ELSE' then begin
      inc(WaitForEndIF);  //marca bandera para esperar
    end;
  end;
end;
procedure TParserDirecBase.ProcELSE;
var
  direc: string;
begin
  if WaitForEndIF>0 then begin
    {Estamos dentro de un IF, que se supone dio verdadero, de otra forma, no llegaría
    por aquí. De ser así, el ELSE debe ser falso.}
    cIn.Next;  //toma token {$ELSE}
    //Explora, sin compilar, hasta encontrar directiva delimitadora.
    if not ScanIFDEF(direc) then begin
      //Llegó al final del código fuente, sin encontrar el ENDIF
      GenErrorDir(ER_ENDIF_NOFOU);
      exit;
    end;
    //Encontró token delimitador
    //Si es $ENDIF, no hay problema, todo termina allí, pero si es un else:
    if direc='ELSE' then begin
      GenErrorDir(ER_UNEXP_ELSE);
      exit;
    end;
    //Encontró un $ENDIF
    dec(WaitForEndIF);  //lleva la cuenta
  end else begin
    //No se esperaba
    GenErrorDir(ER_UNEXP_ENDIF);
    exit;
  end;
end;
procedure TParserDirecBase.ProcENDIF;
begin
  if WaitForEndIF>0 then begin
    //Se es peraba el delimitador
    Dec(WaitForEndIF);  //para que ya no siga buscando
  end else begin
    //No se esperaba
    GenErrorDir(ER_UNEXP_ENDIF);
    exit;
  end;
end;
procedure TParserDirecBase.ProcINCLUDE(lin: string; var ctxChanged: boolean);
{Implementa la inclusión de un archivo externo en el código}
var
  filPath: string;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  //Toma el restante de la cadena
  filPath := copy(lin, lexDir.GetX);
  //Completa ruta, si es relativa
  if (pos('/', filPath)=0) and (pos('\', filPath)=0) then begin
    //No incluye información de ruta. Asume que está en la misma ruta.
    filPath := ExtractFileDir(mainFile) + DirectorySeparator + filPath;
  end;
  if not FileExists(filPath) then begin
    GenErrorDir(ER_FILE_NO_FND_, [filPath]);
    exit;
  end;
  //Ya se tiene el archivo
  cIn.Next;  //pasa la directiva
  cIn.NewContextFromFile(filPath);  //Pasa a explorar contenido del archivo
  cIn.curCon.autoClose := true;   //Para que se cierre, al finalizar
  //cIn.curCon.FixErrPos := true;   //Para que se ignore la posición de los errores
  //cIn.curCon.ErrPosition := p;    //Posición a usar para ubicar el error
  //cIn.curCon.PreErrorMsg := 'Macro '+mac.name+': ';
  ctxChanged := true;   //Marca bandera para indciar que se ha cambiado de contexto

end;
procedure TParserDirecBase.ProcFREQUENCY;
var
  f: Longint;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  if tokType <> lexDir.tnNumber then begin
    GenErrorDir(ER_ERROR_DIREC);
    exit;
  end;
  if not TryStrToInt(lexDir.GetToken, f) then begin
    GenErrorDir(ER_ERROR_FREQ);
    exit;
  end;
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  case UpperCase(lexDir.GetToken) of
  'KHZ': f := f * 1000;
  'MHZ': f := f * 1000000;
  else
    GenErrorDir(ER_ERROR_DIREC);
    exit;
  end;
  if f>picCore.MaxFreq then begin
    GenErrorDir(ER_TOOHIGHFRE);
    exit;
  end;
  picCore.frequen:=f; //asigna frecuencia
end;
procedure TParserDirecBase.ProcORG;
var
  Ident: String;
  valOrg: Longint;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  if lexDir.GetTokenKind = lexDir.tnNumber then begin
    //Es un valor numérico
    if not TryStrToInt(lexDir.GetToken, valOrg) then begin
      GenErrorDir(ER_ERIN_NUMBER_, [lexDir.GetToken]);
      exit;
    end;
    //Ya se tiene el valor numérico
    GeneralORG := valOrg;  //Carga directamente en variable global
    lexDir.Next;
    skipWhites;
    //No debe seguir nada
    if not lexDir.GetEol then begin
      GenErrorDir(ER_ERIN_NUMBER_, [lexDir.GetToken]);
      exit;
    end;
    exit;
  end else begin
    GenErrorDir(ER_ERIN_NUMBER_, [Ident]);
    exit;
  end;
end;
procedure TParserDirecBase.ProcSET_STATE_RAM;
var
  txtMsg: String;
begin
  lexDir.Next;  //pasa al siguiente
  txtMsg := CogExpresion(0).valStr;
  if HayError then exit;
  picCore.SetStatRAMCom(txtMsg);
  if picCore.MsjError<>'' then GenErrorDir(picCore.MsjError);
end;
procedure TParserDirecBase.ProcCLEAR_STATE_RAM;
{Limpia el estado de la memoria RAM}
begin
   picCore.DisableAllRAM;
end;
procedure TParserDirecBase.ProcINFO;
var
  txtMsg: String;
begin
  lexDir.Next;  //pasa al siguiente
  txtMsg := CogExpresion(0).valStr;
  if HayError then Exit;
  //Solo muestra en compilación y en la primera pasada
  if Compiling and FirstPass then GenInfo(txtMsg);
end;
procedure TParserDirecBase.ProcWARNING;
var
  txtMsg: String;
begin
  lexDir.Next;  //pasa al siguiente
  txtMsg := CogExpresion(0).valStr;
  if HayError then Exit;
  //Solo muestra en compilación y en la primera pasada
  if Compiling and FirstPass then GenWarn(txtMsg);
end;
procedure TParserDirecBase.ProcERROR;
var
  txtMsg: String;
begin
  lexDir.Next;  //pasa al siguiente
  txtMsg := CogExpresion(0).valStr;
  if HayError then Exit;
  //Solo muestra en compilación y en la primera pasada
  if Compiling and FirstPass then GenError(txtMsg);
end;
procedure TParserDirecBase.ProcSET;
//Asigna valor a una varaible
var
  varName, unitInf: String;
  varValue: TDirOperand;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  if lexDir.GetEol then begin
    GenErrorDir(ER_SYNTAX_ERRO);
    exit;
  end;
  if lexDir.GetTokenKind <> lexDir.tnIdentif then begin
    GenErrorDir(ER_IDENT_EXPEC);
    exit;
  end;
  varName :=  lexDir.GetToken;  //lee identificador
  lexDir.Next;
  skipWhites;
  if not CogCarERR('=') then exit;  //sale con error
  varValue := CogExpresion(0);
  if HayError then exit;
  unitInf := lexDir.GetToken;  //Puede que haya unidades
  //Esta facilidad adicional es útil para casos en que se expresa la frecuencia.
  if upcase(unitInf) = 'KHZ' then begin
    lexDir.Next;  //Lo reconcoe
    varValue.SetvalNum(varValue.GetvalNum * 1000);
  end else if upcase(unitInf) = 'MHZ' then begin
    lexDir.Next;  //Lo reconcoe
    varValue.SetvalNum(varValue.GetvalNum * 1000000);
  end;
  if not lexdir.GetEol then begin
    GenErrorDir(ER_SYNTAX_ERRO);
    exit;
  end;
  if not lexdir.GetEol then begin
    GenErrorDir(ER_SYNTAX_ERRO);
    exit;
  end;
  AsigVariable(varName, varValue);
end;
procedure TParserDirecBase.ProcMSGBOX;
var
  txtMsg: String;
begin
  lexDir.Next;  //pasa al siguiente
  txtMsg := CogExpresion(0).valStr;
  if HayError then Exit;
  //Solo muestra en compilación y en la primera pasada
  if Compiling and FirstPass then msgbox(txtMsg);
end;
procedure TParserDirecBase.ProcMSGERR;
var
  txtMsg: String;
begin
  lexDir.Next;  //pasa al siguiente
  txtMsg := CogExpresion(0).valStr;
  if HayError then Exit;
  //Solo muestra en compilación y en la primera pasada
  if Compiling and FirstPass then MsgErr(txtMsg);
end;
procedure TParserDirecBase.ProcMSGWAR;
var
  txtMsg: String;
begin
  lexDir.Next;  //pasa al siguiente
  txtMsg := CogExpresion(0).valStr;
  if HayError then Exit;
  //Solo muestra en compilación y en la primera pasada
  if Compiling and FirstPass then MsgExc(txtMsg);
end;
procedure TParserDirecBase.ProcMODE;
var
  txtMode: String;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  txtMode := UpCase(lexDir.GetToken);
  if txtMode = 'PICPAS' then begin
    self.mode := modPicPas;
  end else if txtMode = 'PASCAL' then begin
    self.mode := modPascal;
  end else begin
    GenErrorDir(ER_MODE_UNKNOWN, [txtMode]);
    exit;
  end;
end;
//Access to system variables
function TParserDirecBase.read_PIC_MODEL: string;
begin
  Result := picCore.Model;
end;
procedure TParserDirecBase.write_PIC_MODEL(AValue: string);
begin
  picCore.Model := AValue;
end;
function TParserDirecBase.read_PIC_FREQUEN: Single;
begin
  Result := picCore.frequen;
end;
procedure TParserDirecBase.write_PIC_FREQUEN(AValue: Single);
begin
  picCore.frequen := round(AValue);
end;
function TParserDirecBase.read_PIC_MAXFREQ: Single;
begin
  Result := PICCore.MaxFreq;
end;
procedure TParserDirecBase.write_PIC_MAXFREQ(AValue: Single);
begin
  PICCore.MaxFreq := round(AValue);
end;
function TParserDirecBase.read_ORG: Single;
begin
  Result := picCore.iRam;
end;
procedure TParserDirecBase.write_ORG(AValue: Single);
begin
  picCore.iRam:= round(AValue);
end;
function TParserDirecBase.read_SYN_MODE: String;
begin
  case mode of
  modPicPas: Result := 'PicPas';
  modPascal: Result := 'Pascal';
  else
      Result := '';
  end;
end;
procedure TParserDirecBase.write_SYN_MODE(AValue: String);
begin

end;
function TParserDirecBase.read_CURRBLOCK: String;
var
  parentNod: TxpEleCodeCont;
begin
  if TreeElems.curNode.idClass = eltBody then begin
    //Solo aquí puede haber bloques
    parentNod := TreeElems.CurCodeContainer; //Se supone que nunca debería fallar
    if parentNod.CurrBlock = nil then
      exit('')
    else
      exit(parentNod.CurrBlock.idStr);
  end else begin
    Result := '';
  end;
end;
//Macros
procedure TParserDirecBase.NewMacro(macName, macValue: string);
{Agrega una nueva macro a la lista de macros}
var
  mac: TDirMacro;
begin
  mac := TDirMacro.Create;
  mac.name := macName;
  mac.value := macValue;
  //Ubica la posición del contexto
  mac.posDef := cIn.ReadSrcPos;
  macroList.Add(mac);
end;
function TParserDirecBase.DefinedMacro(macName: string): boolean;
{Indicate if a macro has been defined.}
var
  mac: TDirMacro;
begin
  macName := UpCase(macName);
  for mac in macroList do begin
    if UpCase(mac.name) = macName then begin
      exit(true);  //encontró
    end;
  end;
//No se encontró
  exit(false);
end;
function TParserDirecBase.DefinedMacro(macName: string; out dmac: TDirMacro): boolean;
begin
  macName := UpCase(macName);
  for dmac in macroList do begin
    if UpCase(dmac.name) = macName then begin
      exit(true);  //encontró
    end;
  end;
//No se encontró
  exit(false);
end;
//Instructions and Variables
procedure TParserDirecBase.AddInstruction(instName: string;
  callProc: TDirEveCallProc);
{Add a new instruction to the Directive engine.}
var
  dins: TDirInstruc;
begin
  dins := TDirInstruc.Create;
  dins.name := instName;
  dins.OnCall := callProc;
  instList.Add(dins);
end;
function TParserDirecBase.DefinedInstruc(insName: string; out dins: TDirInstruc): boolean;
begin
    insName := UpCase(insName);
    for dins in instList do begin
      if UpCase(dins.name) = insName then begin
        exit(true);  //encontró
      end;
    end;
  //No se encontró
    exit(false);
end;
function TParserDirecBase.DefinedVar(cad: string; out dvar: TDirVar): boolean;
{Indica si un identificador corresponde a una variable. Devuelve la referencia a la
variable encontrada.}
begin
  cad := UpCase(cad);
  for dvar in varsList do begin
    if UpCase(dvar.nomb) = cad then begin
      exit(true);
    end;
  end;
  exit(false);
end;
function TParserDirecBase.AsigVariable(VarName: string; const value: TDirOperand): TDirVar;
{Asigna un valor numérico o de cadena a una variable. Si no existe la crea.
Devuelve la referencia a la variable asignada.}
begin
  //Busca variable
  if DefinedVar(VarName, Result) then begin
    //Encontró la variable
    Result.valor := value;
    exit(Result);
  end;
  //No se encontró, se debe crear
  Result := TDirVar.Create;
  Result.nomb := VarName;
  Result.valor := value;
  varsList.Add(Result);
end;
//Public
procedure TParserDirecBase.skipWhites;
begin
  if tokType = lexDir.tnSpace then
    lexDir.Next;  //quita espacios
end;
procedure TParserDirecBase.GenErrorDir(msg: string);
{Genera un error corrigiendo la posición horizontal}
var
  p: TSrcPos;
begin
  p := cIn.ReadSrcPos;
  p.col := tokIni + lexDir.GetX;  //corrige columna
  GenErrorPos(msg, [], p);
end;
procedure TParserDirecBase.GenErrorDir(msg: string; const Args: array of const);
var
  p: TSrcPos;
begin
  p := cIn.ReadSrcPos;
  p.col := tokIni + lexDir.GetX;  //corrige columna
  GenErrorPos(msg, Args, p);
end;
procedure TParserDirecBase.AddSysVariableNumber(varName: string;
  ReadNum: TDirEveReadNum; WriteNum: TDirEveWriteNum);
{Add a new system variable of type number, to the Directive engine.}
var
  dvar: TDirVar;
begin
  dvar :=  TDirVar.Create;
  dvar.nomb := varName;
  dvar.ReflectToNumber(ReadNum, WriteNum);
  varsList.Add(dvar);
end;
procedure TParserDirecBase.AddSysVariableString(varName: string;
  ReadStr: TDirEveReadStr; WriteStr: TDirEveWriteStr);
var
  dvar: TDirVar;
begin
  dvar :=  TDirVar.Create;
  dvar.nomb := varName;
  dvar.ReflectToString(ReadStr, WriteStr);
  varsList.Add(dvar);
end;
procedure TParserDirecBase.ProcDIRline(const AsmLin: string; out ctxChanged: boolean);
{Procesa una directiva, que ha sido definida, para que solo ocupe una sola línea,
para simplificar el procesamiento, ya que si las macros ocupan más de una línea,
complican tremendamente la exploración del lexer y la ubicación de errores.
Sin embargo, las directivas de tipo $IFDEF ... o ELSE ...  se pueden procesar aquí,
leyendo varias líneas sucesivas del código fuente.}
var
  lin: String;
  dmac: TDirMacro;
  p: TSrcPos;
  dvar: TDirVar;
  dins: TDirInstruc;
begin
  ctxChanged := false;
  IniExplorDirec(lin);
  if tokType <> lexDir.tnIdentif then begin
    GenErrorDir(ER_ERROR_DIREC);
    exit;
  end;
  //sigue identificador
  case UpperCase(lexDir.GetToken) of
  'FREQUENCY' : ProcFREQUENCY;
  'ORG'       : ProcORG;
  'INCLUDE'   : ProcINCLUDE(lin, ctxChanged);
  'OUTPUTHEX' : ProcOUTPUTHEX(lin);
  'DEFINE'    : ProcDEFINE(lin);
  'IFDEF'     : ProcIFDEF(lin, false);
  'IFNDEF'    : ProcIFDEF(lin, true);
  'IF'        : ProcIF(lin, false);
  'IFNOT'     : ProcIF(lin, true);
  'ELSE'      : ProcELSE;
  'ENDIF'     : ProcENDIF;
  'MODE'      : ProcMODE;
  'MSGBOX'    : ProcMSGBOX;
  'MSGERR'    : ProcMSGERR;
  'MSGWAR'    : ProcMSGWAR;
  'INFO'      : ProcINFO;
  'WARNING'   : ProcWARNING;
  'ERROR'     : ProcERROR;
  'SET'       : ProcSET;
  'CLEAR_STATE_RAM': ProcCLEAR_STATE_RAM;
  'SET_STATE_RAM'  : ProcSET_STATE_RAM;
  else
    //Puede ser una instrucción, macro o variable
    if DefinedInstruc(lexDir.GetToken, dins) then begin
      dins.OnCall();
    end else if DefinedMacro(lexDir.GetToken, dmac) then begin
      p := cIn.ReadSrcPos;   //Guarda posición del token
      cIn.Next;  //pasa la directiva
      cIn.NewContextFromTxt(
        dmac.value, //Pasa a explorar contenido de la macro como cadena
        dmac.posDef.fil {Fija el archivo de definiición de la macro.}
      );
      cIn.curCon.autoClose := true;   //Para que se cierre, al finalizar
      cIn.curCon.FixErrPos := true;   //Para que se ignore la posición de los errores
      cIn.curCon.ErrPosition := p;    //Posición a usar para ubicar el error
      cIn.curCon.PreErrorMsg := 'Macro '+dmac.name+': ';
      ctxChanged := true;  //Marca bandera para indciar que se ha cambiado de contexto
    end else if DefinedVar(lexDir.GetToken, dvar) then begin
      //Es variable
      p := cIn.ReadSrcPos;   //Guarda posición del token
      cIn.Next;  //pasa la directiva
      cIn.NewContextFromTxt(
        dvar.valor.valStr, //Pasa a explorar valor de la variable como texto
        '' {Fija el archivo de definiición.}
      );
      cIn.curCon.autoClose := true;   //Para que se cierre, al finalizar
      cIn.curCon.FixErrPos := true;   //Para que se ignore la posición de los errores
      cIn.curCon.ErrPosition := p;    //Posición a usar para ubicar el error
      cIn.curCon.PreErrorMsg := 'Variable '+dvar.nomb+': ';
      ctxChanged := true;  //Marca bandera para indciar que se ha cambiado de contexto
    end else begin
      GenErrorDir(ER_UNKNO_DIREC, [lexDir.GetToken]);
      exit;
    end;
  end;
end;
procedure TParserDirecBase.DefLexDirectiv;
(*Define la sintaxis del lexer que se usará para analizar las directivas. La que
 debe estar entre los símbolo {$ ... }
*)
begin
  lexDir.ClearSpecials;               //para empezar a definir tokens
  lexDir.CreateAttributes;            //Limpia atributos
  lexDir.ClearMethodTables;

  dirOperator := lexDir.NewTokType('operator');
  dirDelimiter:= lexDir.NewTokType('delimiter');
  lexDir.Attrib[lexDir.tnSymbol].Background := clRed;
  lexDir.Attrib[dirOperator].Background := clGreen;
  //solo se requiere identificadores y números
  lexDir.DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');
  lexDir.DefTokContent('[0-9]', '[0-9.]*', lexDir.tnNumber);
  lexDir.DefTokContent('[$]','[0-9A-Fa-f]*', lexDir.tnNumber);
  lexDir.DefTokDelim('''', '''', lexDir.tnString);  //cadenas
  lexDir.DefTokDelim('"', '"', lexDir.tnString);  //cadenas

  lexDir.AddSymbSpec('=', dirOperator);
  lexDir.AddSymbSpec('+=', dirOperator);
  lexDir.AddSymbSpec('<>', dirOperator);
  lexDir.AddSymbSpec('>', dirOperator);
  lexDir.AddSymbSpec('<', dirOperator);
  lexDir.AddSymbSpec('>=', dirOperator);
  lexDir.AddSymbSpec('<=', dirOperator);

  lexDir.AddIdentSpec('AND', dirOperator);
  lexDir.AddIdentSpec('OR', dirOperator);
  lexDir.AddIdentSpec('XOR', dirOperator);
  lexDir.AddIdentSpec('NOT', dirOperator);

  lexDir.AddSymbSpec('+', dirOperator);
  lexDir.AddSymbSpec('-', dirOperator);
  lexDir.AddSymbSpec('*', dirOperator);
  lexDir.AddSymbSpec('/', dirOperator);
  lexDir.AddSymbSpec('\', dirOperator);
  lexDir.AddSymbSpec('%', dirOperator);
  lexDir.AddSymbSpec('^', dirOperator);

  lexDir.AddSymbSpec('(', dirDelimiter);
  lexDir.AddSymbSpec(')', dirDelimiter);
  lexDir.AddSymbSpec('{$', dirDelimiter);
  lexDir.AddSymbSpec('}', dirDelimiter);
  lexDir.Rebuild;
end;
procedure TParserDirecBase.ClearMacros;
begin
  macroList.Clear;
  WaitForEndIF := 0;
  GeneralORG := 0;   //ORG por defecto en 0
  instList.Clear;

  //Create system variables
  varsList.Clear;
  AddSysVariableString('PIC_MODEL'   , @read_PIC_MODEL  , @write_PIC_MODEL);
  AddSysVariableNumber('PIC_FREQUEN' , @read_PIC_FREQUEN, @write_PIC_FREQUEN);
  AddSysVariableNumber('PIC_MAXFREQ' , @read_PIC_MAXFREQ, @write_PIC_MAXFREQ);
  AddSysVariableNumber('PIC_ORG'     , @read_ORG        , @write_ORG);
  AddSysVariableString('SYN_MODE'    , @read_SYN_MODE   , @write_SYN_MODE);
  AddSysVariableString('CURRBLOCK'   , @read_CURRBLOCK  , nil);
end;
//Initialization
constructor TParserDirecBase.Create;
begin
  inherited Create;
  lexDir := TSynFacilSyn.Create(nil);  //crea lexer para analzar directivas
  macroList := TDirMacro_list.Create(true);
  varsList := TDirVar_list.Create(true);
  instList := TDirInstruc_list.Create(true);
  DefLexDirectiv;
end;
destructor TParserDirecBase.Destroy;
begin
  instList.Destroy;
  varsList.Destroy;
  macroList.Destroy;
  lexDir.Destroy;
  inherited Destroy;
end;

end.

