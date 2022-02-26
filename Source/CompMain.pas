unit CompMain;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Types, CompBase, Globales, XpresElemP65,
  XpresAST, LexPas, ParserASM_6502, StrUtils, CPUCore, LCLProc;
type

  { TCompMain }
  TCompMain = class(TCompilerBase)
  public    //Access to CPU hardware.
    picCore    : TCPUCore;   //Objeto PIC Core. This is an abstraction. Real CPU is not yet specified.
    devicesPath: string;     //path to untis for this device
    function PICName: string; virtual; abstract;
    function RAMmax: integer; virtual; abstract;
  private
    function AddConstDeclar(constName: string; srcPos: TSrcPos; out
      typesCreated: integer): TEleConsDec;
    procedure AnalyzeFOR;
    procedure AnalyzeIF;
    procedure AnalyzeREPEAT;
    procedure AnalyzeWHILE;
    procedure MoveInternalTypes(node: TxpElement; declarSec: TxpElement;
      declarPos: Integer);
  protected
    function IsUnit: boolean;
    function StartOfSection: boolean;
    procedure CompileLastEnd;
  protected  //Elements processing
    procedure GetAdicVarDeclar(xType: TEleTypeDec; out aditVar: TAdicVarDec; out
      typesCreated: integer);
    procedure ReadProcHeader(out procName: String; out retType: TEleTypeDec; out
      srcPos: TSrcPos; out pars: TxpParFuncArray; out IsInterrupt,
  IsForward: Boolean);
    procedure CompileVarDeclar;
    procedure CompileConstDeclar;
    procedure CompileTypeDeclar(elemLocat: TxpEleLocation);
    function GetTypeDeclar(out decStyle: TTypDeclarStyle; out
      TypeCreated: boolean): TEleTypeDec;
    function GetTypeDeclarSimple(): TEleTypeDec;

    function VerifyEND: boolean;
    function GetCondition(out ex: TEleExpress; ConstBool: boolean = false): boolean;
    function OpenContextFrom(filePath: string): boolean;
    function CompileStructBody: boolean;
    procedure AnalyzeSentence;
    procedure AnalyzeCurBlock;
    procedure CompileProcDeclar;
    procedure CompileInlineDeclar(elemLocat: TxpEleLocation);
    procedure CompileUsesDeclaration;
    procedure DoAnalyzeUnit(uni: TxpElement);
    procedure DoAnalyzeProgram;
  public
    {Indica que TCompiler, va a acceder a un archivo, peor está pregunatndo para ver
     si se tiene un Stringlist, con los datos ya caragdos del archivo, para evitar
     tener que abrir nuevamente al archivo.}
    OnRequireFileString: procedure(FilePath: string; var strList: TStrings) of object;
  end;
  procedure SetLanguage;

implementation
var
  ER_INV_MEMADDR, ER_EXP_VAR_IDE, ER_NUM_ADD_EXP, ER_CON_EXP_EXP, ER_EQU_EXPECTD,
  ER_IDEN_EXPECT, ER_NOT_IMPLEM_, ER_SEM_COM_EXP, ER_INV_ARR_SIZ, ER_ARR_SIZ_BIG,
  ER_IDE_TYP_EXP, ER_IDE_CON_EXP, ER_EQU_COM_EXP, ER_DUPLIC_IDEN,
  ER_BOOL_EXPECT, ER_EOF_END_EXP, ER_ELS_UNEXPEC, ER_END_EXPECTE, ER_NOT_AFT_END,
  ER_INST_NEV_EXE,ER_UNKN_STRUCT, ER_DUPLIC_FUNC_, ER_FIL_NOFOUND, ER_PROG_NAM_EX,
  ER_VARIAB_EXPEC, ER_ONL_BYT_WORD, ER_UNKNOWN_IDE_
  : string;
procedure SetLanguage;
begin
  CompBase.SetLanguage;
  {$I ..\_language\tra_CompMain.pas}
end;
function TCompMain.StartOfSection: boolean;
var
  tokL: String;
begin
  tokL := lowercase(token);
  Result := (tokL ='var') or (tokL ='const') or
            (tokL ='type') or (tokL ='procedure') or (tokL ='inline');
end;
procedure TCompMain.CompileLastEnd;
{Compila la parte de final de un programa o una unidad}
var
  tokL: String;
begin
  if atEof then begin
    GenError(ER_EOF_END_EXP);
    exit;       //sale
  end;
  tokL := lowercase(token);
  if tokL <> 'end' then begin  //verifica si termina el programa
    if tokL = 'else' then begin
      //Precisa un poco más en el error
      GenError(ER_ELS_UNEXPEC);
      exit;       //sale
    end else begin
      GenError(ER_END_EXPECTE);
      exit;       //sale
    end;
  end;
  Next;   //coge "end"
  //Debería seguir el punto
  if not CaptureTok('.') then exit;
  //no debe haber más instrucciones
  ProcComments;
  if not atEof then begin
    GenError(ER_NOT_AFT_END);
    exit;       //sale
  end;
end;

//Elements processing
procedure TCompMain.GetAdicVarDeclar(xType: TEleTypeDec; out aditVar: TAdicVarDec;
          out typesCreated: integer);
{Verify aditional settings for var declarations, after the type definition. These settings
can be:
 ABSOLUTE <literal address or variable/constant identifier>
 REGISTER/REGISTERA/REGISTERX/REGISTERY

All aditional settings are returned in "aditVar".
IMPORTANT: "xType" can change when is an unspecified size array ( like []byte ) and an
initialization is provided. No new type elements are created or destroyed.
If aditional type are created for the init value, "typesCreated" specify the number of
type created.
}
  function ReadAddres(tok: string): word;
  {Lee una dirección de RAM a partir de una cadena numérica.
  Puede generar error.}
  var
    n: LongInt;
  begin
    //COnvierte cadena (soporta binario y hexadecimal)
    if not TryStrToInt(tok, n) then begin
      //Podría fallar si es un número muy grande
      GenError(ER_INV_MEMADDR);
      {%H-}exit;
    end;
    callValidRAMaddr(n);  //Validate address
    if HayError then exit(0);
    Result := n;
  end;
var
  xvar: TEleVarDec;
  n: integer;
  tokL, newConstName: String;
  ele: TxpElement;
  xcon, constDec: TEleConsDec;
  consTyp: TEleTypeDec;
  nItems: Int64;
begin
  aditVar.hasAdic  := decNone;       //Bandera
  aditVar.hasInit  := false;
  aditVar.absVar   := nil;         //Por defecto
  tokL := lowercase(token);
  if (tokL = 'absolute') or (token = '@') then begin
    // Hay especificación de dirección absoluta ////
    aditVar.hasAdic := decAbsol;    //marca bandera
    Next;
    ProcComments;
    if tokType = tkLitNumber then begin
      if (token[1]<>'$') and ((pos('e', token)<>0) or (pos('E', token)<>0)) then begin
        //La notación exponencial, no es válida.
        GenError(ER_INV_MEMADDR);
        exit;
      end;
      n := pos('.', token);   //no debe fallar
      if n=0 then begin
        //Número entero sin parte decimal
        aditVar.absAddr := ReadAddres(token);
        Next;  //Pasa con o sin error, porque esta rutina es "Pasa siempre."
      end else begin
        //Puede ser el formato <dirección>.<bit>, en un solo token, que no es válido.
        GenError('Syntax error.');
        exit;
      end;
    end else if tokType = tkIdentifier then begin
      //Should be variable or constant
      ele := TreeElems.FindFirst(token);  //Identify element
      if ele = nil then begin
        //Unidentified element.
        GenError(ER_UNKNOWN_IDE_, [token]);
        Next;  //Pasa con o sin error, porque esta rutina es "Pasa siempre."
        exit;
      end;
      if ele.idClass = eleConsDec then begin  //Is constant
        xcon := TEleConsDec(ele);
        AddCallerToFromCurr(ele);
        aditVar.absAddr := xcon.value.ValInt; { TODO : Faltaría verificar que "xcon" sea de tipo numérico }
        Next;    //Pasa al siguiente
      end else if ele.idClass = eleVarDec then begin  //Is mapped to a variable
        xvar := TEleVarDec(ele);
        aditVar.absVar := xvar;  //Guarda referencia
        aditVar.absOff := 0;
        //La dirección final se asignará al asignar RAM.
        Next;    //Pasa al siguiente.
        //Se implementará una rutina muy simple para los casos .Low y .High.
        if token='.' then begin
          Next;
          if UpCase(token) = 'LOW' then begin
            Next;
          end else if UpCase(token) = 'HIGH' then begin
            aditVar.absOff := 1;
            Next;
          end else begin
            GenError('Expected ".low" or ".high".');
            Next;  //Pasa con o sin error, porque esta rutina es "Pasa siempre."
            exit;
          end;
        end;
      end else begin
        GenError(ER_EXP_VAR_IDE);
        Next;  //Pasa con o sin error, porque esta rutina es "Pasa siempre."
        exit;
      end;
    end else begin   //error
      GenError(ER_NUM_ADD_EXP);
      Next;    //pasa siempre
      exit;
    end;
  end else if tokL = 'register' then begin
    //Es de tipo registro
    aditVar.hasAdic := decRegis;    //marca bandera
    Next;
    ProcComments;
  end else if tokL = 'registera' then begin
    // Es de tipo registro
    aditVar.hasAdic := decRegisA;    //marca bandera
    Next;
    ProcComments;
  end else if tokL = 'registerx' then begin
    // Es de tipo registro
    aditVar.hasAdic := decRegisX;    //marca bandera
    Next;
    ProcComments;
  end else if tokL = 'registery' then begin
    // Es de tipo registro
    aditVar.hasAdic := decRegisY;    //marca bandera
    Next;
    ProcComments;
  end;
  //Verifica compatibilidad de tamaños
  if aditVar.hasAdic in [decRegisA, decRegisX, decRegisY] then begin
    //Solo pueden ser de tamaño byte
    if not xType.IsByteSize then begin
      GenError('Only byte-size types can be a specific register.');
      exit;
    end;
  end;
  //Puede seguir una sección de inicialización: var: char = 'A';
  ProcComments;
  if token = '=' then begin
    aditVar.hasInit := true;
    Next;   //lo toma
    ProcComments;
    //Aquí debe seguir el valor inicial constante.
    newConstName := 'C-'+IntToStr(TreeElems.curNode.Elements.Count);
    constDec := AddConstDeclar(newConstName, GetSrcPos, typesCreated);  //Leemos como constante
    if HayError then exit;
    //Ya se tiene el valor constante para inicializar variable.
    if aditVar.hasAdic in [decRegis, decRegisA, decRegisX, decRegisY] then begin
      GenError('Cannot initialize register variables.');
      exit;
    end;
  end else begin
    //No hay asignación inicial.
    aditVar.hasInit := false;
  end;
  //Valida el caso de areglos dinámicos
  if (xType.catType = tctArray) and xType.isDynam and not aditVar.hasInit then begin
    //Es un arreglo dinámico. Debió inicializarse.
    GenError(ER_EQU_EXPECTD);
    exit;
  end;
  {Ya se validó la pertinencia de la inicialización y ya se tiene el operando de
  inicialización en "constDec". Ahora toca validar la compatibilidad de los tipos.}
  //Por ahora solo se permite inicializar arreglos.
  if aditVar.hasInit then begin
    consTyp := constDec.Typ;
    if (xType.catType = tctArray) then begin
      nItems := consTyp.consNitm.value.ValInt;
      //Arrays have some particular behaviour
      //Validation for category
      if consTyp.catType <> tctArray then begin
        GenError('Expected an array.');
        exit;
      end;
      //Here we assure xType and consTyp are both arrays.
      //If dynamic, the size depends on initial value
      if xType.isDynam then begin
        //Dynamic size
        xType.consNitm.value.ValInt := nItems;   //Update array size
        //Type name could be updated too.
        //xType.name := GenArrayTypeName(xType.itmType.name, nItems);
      end;
      //Validation for item types.
      if xType.itmType <> consTyp.itmType then begin
        GenError('Item type doesn''t match for initialize array.');
        exit;
      end;
      //Validation for size. Must have the same size to simplify creating and calling new types.
      if xType.nItems < nItems then begin
        GenError('Too many items to initialize array.');
      end else if xType.nItems > nItems then begin
        GenError('Too few items to initialize array.');
      end ;
      //Validate type compatibility
      {Until here we have validated that xType and consTyp and both arrays,
      have the same item type and compatible sizes, so they are compatible to
      initialization.
      }
    end else begin
      //Other types are simple to validate
      if consTyp <> xType then begin
        GenError('Cannot initialize. Expected type "%s". Got "%s".', [xType.name, consTyp.name]);
        exit;
      end;
    end;
    //Assign constant init value.
    aditVar.constDec := constDec;
  end;
end;
procedure TCompMain.ReadProcHeader(out procName: String; out retType: TEleTypeDec;
  out srcPos: TSrcPos; out pars: TxpParFuncArray; out IsInterrupt, IsForward: Boolean);
{Hace el procesamiento del encabezado de la declaración de una función/procedimiento.
Devuelve la referencia al objeto TxpEleFun creado, en "fun".
Conviene separar el procesamiento del enzabezado, para poder usar esta rutina, también,
en el procesamiento de unidades.}
  procedure ReadFunctionParams(out funPars: TxpParFuncArray);
  //Lee la declaración de parámetros de una función.
  const
    BLOCK_SIZE = 10;  //Tamaño de bloque.
  var
    typ: TEleTypeDec;
    itemList: TStringDynArray;
    srcPosArray: TSrcPosArray;
    i, curSize, n, typesCreated: Integer;
    adicVarDec: TAdicVarDec;
  begin
    SkipWhites;
    if EOBlock or EOExpres or (token = ':') then begin
      //No tiene parámetros
      setlength(funPars, 0);
    end else begin
      //Debe haber parámetros. Prepara espacio para leer.
      curSize := BLOCK_SIZE;    //Tamaño inicial de bloque
      setlength(funPars, curSize);  //Tamaño inicial
      n := 0;
      //Inicia lectura
      if not CaptureTok('(') then exit;
      SkipWhites;
      repeat
        //if tokL = 'register' then begin
        //  IsRegister := regA;  //Asumimos que es A
        //  Next;
        //  SkipWhites;
        //end;
        getListOfIdent(itemList, srcPosArray);
        if HayError then begin  //precisa el error
          GenError(ER_IDEN_EXPECT);
          exit;
        end;
        if not CaptureTok(':') then exit;
        typ := GetTypeDeclarSimple;  //lee tipo
        if HayError then exit;
//        /////// Allows new types definition here /////
//        typ := GetTypeDeclar(decStyle, TypeCreated);  //lee tipo
//        if HayError then exit;
//        if typeCreated then begin
//          //Es un tipo nuevo.
//          typ.location := curLocation;   //Ubicación del tipo (Interface/Implementation/...)
//          TreeElems.AddElement(typ); {Tendría que agregarse después de abrir el elemento función, no ahora}
//        end else begin
//          //El tipo ya existe en el arbol de sintaxis
//        end;
//        ///////////////////////////////////////////////
        ProcComments;
        GetAdicVarDeclar(typ, adicVarDec, typesCreated);
        if adicVarDec.hasInit then begin
          //As we don't permit initialization, we won't have changes on "typ".
          GenError('Cannot initialize parameters.');
          exit;
        end;
        //Ya tiene los datos de los parámetros
        for i:= 0 to high(itemList) do begin
          funPars[n].name  := itemList[i];
          funPars[n].srcPos:= srcPosArray[i];
          funPars[n].typ   := typ;
          funPars[n].adicVar := adicVarDec;
          //Prepara siguiente lectura
          inc(n);
          if n >= curSize then begin
            curSize += BLOCK_SIZE;   //Incrementa tamaño en bloque
            setlength(funPars, curSize);  //hace espacio en bloque
          end;
        end;
        //Busca delimitador
        if token = ';' then begin
          Next;   //toma separador
          SkipWhites;
        end else begin
          //no sigue separador de parámetros,
          //debe terminar la lista de parámetros
          //¿Verificar EOBlock or EOExpres ?
          break;
        end;
      until false;
      //busca paréntesis final
      if not CaptureTok(')') then exit;
      //Asigna tamaño final
      setlength(funPars, n);
    end;
  end;
var
  tokL: String;
begin
  //Toma información de ubicación, al inicio del procedimiento
  SkipWhites;
  srcPos := GetSrcPos;
  //Ahora debe haber un identificador
  if tokType <> tkIdentifier then begin
    GenError(ER_IDEN_EXPECT);
    exit;
  end;
  //Lee nombre de función
  procName := token;
  Next;  //lo toma
  //Captura los parámetros en "pars"
  ReadFunctionParams(pars);
  if HayError then exit;

  //Verifica si es función
  SkipWhites;
  if token = ':' then begin
    Next;
    SkipWhites;
    //Es función
    retType := GetTypeDeclarSimple;  //lee tipo
    if HayError then exit;
  end else begin
    retType := typNull;
  end;
  if not CaptureTok(';') then exit;
  //Verifica si es INTERRUPT
  SkipWhites;
  tokL := lowercase(token);
  if tokL = 'interrupt' then begin
    Next;
    IsInterrupt := true;
    if not CaptureTok(';') then exit;
  end else begin
    IsInterrupt := false;
  end;
  if tokL = 'forward' then begin
    Next;
    IsForward := true;
    if not CaptureTok(';') then exit;
  end else begin
    IsForward := false;
  end;
  ProcComments;  //Quita espacios. Puede salir con error
end;
function TCompMain.GetTypeDeclar(out decStyle: TTypDeclarStyle;
                                 out TypeCreated: boolean): TEleTypeDec;
{Extrae la sección de declaración de tipo (De una variable, parámetro o de una definición
de tipo). Se debe llamar justo cuando empieza esta declaración de tipo. Se puede usar en
los siguientes casos;

VAR variable: <DECLARACIÓN DE TIPO>;

PROCEDURE nombre(param: <DECLARACIÓN DE TIPO>);

TYPE nombre_tipo = <DECLARACIÓN DE TIPO>;

El parámetro "decStyle" devuelve el estilo de declaración (Ver comentario de
TTypDeclarStyle).

Si la declaración es de estilo:

ttdDirect -> Devuelve la referencia al tipo, directamente. No crea otro tipo
             Por ejemplo, la siguiente declaración:
               a: Alguntipo;
             Devuelve una referencia al tipo "Alguntipo".

ttdDeclar -> Crea un tipo nuevo con la definición del nuevo tipo especificado, y devuelve
             la referencia a ese tipo, pero no lo agrega al arbol de sintaxis (ACtualmente si lo agrega), porque
             espera validaciones posteriores.
             Por ejemplo, la siguiente declaración.
               a: Array[0..5] of char;
             Creará un tipo nuevo con la definición: "array[0..5] of char".
             El tipo nuevo devuelto tiene nombre vacío y debe ser actualizado luego.

Entre "decStyle" y el "catType" del tipo devuelto (Ver comentario de TxpCatType),
debería quedar completamente especificada la declaración del tipo.

"TypeCreated" indicates when a new Type instance was created in the AST).
In general, creating new types is avoided when exists other type equivalent.

If some problems happens, Error is generated and the NIL value is returned.
}
  procedure ReadSizeInBrackets(arrTyp: TEleTypeDec; dynam: boolean = false);
  {Lee el tamaño de un arreglo especificado entre corchetes: [], y crea el campo
  constante "length" en el tipo "arrTyp".
  También actualiza el valor de "arrTyp.consNitm" y "arrTyp.isDynam".
  Si se pone "dynam" a TRUE, se asume que el tamño es dinámico}
  var
    sizExp: TEleExpress;
    consDec: TEleConsDec;
  begin
    if dynam then begin  //Special case
      //Creates constant element "length" to returns array size
      consDec := AddConstantAndOpen('length', typNull, GetSrcPos);  //No type defined here.
      if HayError then exit;  //Can be duplicated
      sizExp := GenExpressionConstByte('0', 0, GetSrcPos);
      consDec.typ := sizExp.Typ;
      consDec.value := sizExp.value;
      consDec.evaluated := true;
      TreeElems.CloseElement;  //Close element "length"
      arrTyp.isDynam := true;
      arrTyp.consNitm := consDec;  //Update reference to the size.
      exit;
    end;
    Next; //Toma '['. Se asume que ya se identificó
    ProcComments;
    if token = ']' then begin
      //Short declaration without size specified: []byte ;
      Next;
      //Creates constant element "length" to returns array size
      consDec := AddConstantAndOpen('length', typNull, GetSrcPos);  //No type defined here.
      if HayError then exit;  //Can be duplicated
      sizExp := GenExpressionConstByte('0', 0, GetSrcPos);
      consDec.typ := sizExp.Typ;
      consDec.value := sizExp.value;
      consDec.evaluated := true;
      TreeElems.CloseElement;  //Close element "length"
      arrTyp.isDynam := true;
      arrTyp.consNitm := consDec;  //Update reference to the size.
    end else begin
      {Note this section of code is similar to TCompMain.CompileConstDeclar().}
      //Creates constant element "length" to returns array size
      consDec := AddConstantAndOpen('length', typNull, GetSrcPos);  //No type defined here.
      if HayError then exit;  //Can be duplicated
      //Debe seguir una expresión constante, que no genere código
      sizExp := GetExpression(0);  //Add expression to current node os AST
      if HayError then exit;
      consDec.typ := sizExp.Typ;
      if sizExp.Sto = stConst then begin
        consDec.value := sizExp.value;
        consDec.evaluated := sizExp.evaluated;  //Could be constant, not evaluated yet.
      end else begin
        GenError(ER_CON_EXP_EXP);
        exit;
      end;
      TreeElems.CloseElement;  //Close element "length"
      //Límites físicos predefinidos
      if (sizExp.Typ <> typByte) and (sizExp.Typ <> typWord) then begin
        GenError('Only byte and word types allowed here.');
        exit;
      end;
      //{ TODO : "length" podría no estar evaluado, entonces no tendría sentido valdiar así.}
      //if siz.value.valInt<0 then begin
      //  GenError(ER_INV_ARR_SIZ);
      //  exit(0);
      //end;
      //if siz.value.valInt>$ffff then begin
      //  //Límite físico definido
      //  GenError(ER_ARR_SIZ_BIG);
      //  exit(0);
      //end;
      //Ya se tiene el tamaño del arreglo, se sigue con la declaración.
      if not CaptureTok(']') then exit;  //toma "]"
      arrTyp.isDynam := false;
      arrTyp.consNitm := consDec;  //Update reference to the size.
    end;
  end;
  procedure ArrayDeclaration(const srcpos: Tsrcpos; out itemTyp: TEleTypeDec);
  {Procesa la declaración de un tipo arreglo. No agrega el tipo al árbol de sintaxis.
  Se asume que ya se ha identificado el inicio de la declaración de un arreglo,
  sea en su forma larga: "ARRAY[] OF BYTE" o en su forma corta: []BYTE }
  var
    itemDecStyle: TTypDeclarStyle;
    itemTypeCreated: boolean;
    arrTyp: TEleTypeDec;
  begin
    arrTyp := TEleTypeDec(TreeElems.curNode);  //Get Reference to the array.
    if token = '[' then begin
      //Declaración corta
      ReadSizeInBrackets(arrTyp);  //Lee tamaño
      if HayError then exit;
    end else begin
      //Declaración larga: ARRAY[tamaño] OF <tipo>
      Next; //Toma 'ARRAY'. Se asume que ya se identificó.
      ProcComments;
      if token = '[' then begin  //Tamaño espefificado o puede ser []
        ReadSizeInBrackets(arrTyp);  //Lee tamaño
        if HayError then exit;
      end else begin
        //No se especifica tamaño. Puede ser la forma corta: ARRAY OF
        ReadSizeInBrackets(arrTyp, true);
        if HayError then exit;
      end;
      ProcComments;
      if not CaptureStr('OF') then begin
        GenError('Expected "OF"', srcPos);
        exit;
      end;
    end;
    //Lee el tipo que sigue. Llamada RECURSIVA.
    itemTyp := GetTypeDeclar(itemDecStyle, itemTypeCreated);
    if HayError then exit;
    if itemTypeCreated then begin
      //No support for nested declaration by now.
      GenError('Too complex type declaration');
      exit;
    end;
  end;
  function ObjectDeclaration(const srcpos: Tsrcpos): TEleTypeDec;
  {Procesa la declaración de un tipo objeto y devuelve el tipo, ya creado para su
  validación. No agrega el tipo al árbol de sintaxis.
  Se asume que ya se ha identificado el inicio de la declaración de un objeto.}
  var
    xtyp, atTyp: TEleTypeDec;
    i, offs: Integer;
    srcPosArray: TSrcPosArray;
    varNames: TStringDynArray;
  begin
    //Declaración: OBJECT ... END
    Next; //Toma 'OBJECT'. Se asume que ya se identificó.
    ProcComments;
    {When declaring objets, we don't complicate in find previous declarations (rare).
    We alwalys create a new type}
    xtyp := CreateEleTypeObject(GenerateUniqName('Obj'), srcPos);
    TypeCreated  := true;     //Activate flag
    Result := xtyp;  //By default
    //Start reading attributes
    try
      offs := 0;   //Addres offset
      while not atEof and (lowercase(token) <> 'end') do begin
        if lowercase(token) = 'procedure' then begin
          //Es un método. ¿No se debería crear mejor dentro del nodo tipo?
          Next;    //lo toma
          CompileProcDeclar;
          if HayError then exit;
        end else begin
          //Debe ser un campo
          //Esta parte es como la de la declaración de variables pero simplificada.
          SetLength(varNames, 0);
          //Procesa variables a,b,c : int;
          getListOfIdent(varNames, srcPosArray);
          if HayError then begin  //precisa el error
            GenError(ER_EXP_VAR_IDE);
            exit;
          end;
          //usualmente debería seguir ":"
          if token = ':' then begin
            //Debe seguir, el tipo de la variable
            Next;  //lo toma
            ProcComments;
            //Lee el tipo de la variable.
            atTyp := GetTypeDeclarSimple;   //Solo tipos simples por ahora
            if HayError then exit;
            //Reserva espacio para las variables
            for i := 0 to high(varNames) do begin
//              attr := TTypAttrib.Create;
//              attr.name:=varNames[i];
//              attr.typ := atTyp;
//              attr.offs := offs;
//              xtyp.attribs.Add(attr);
              offs += atTyp.size;
            end;
          end else begin
            GenError(ER_SEM_COM_EXP);
            exit;
          end;
          if not CaptureDelExpres then exit;
          ProcComments;
        end;
      end;
    finally
      //Unique exit point
      if HayError then begin
        //There was an error
        xtyp.Destroy;
        TypeCreated := false;
        Result := nil;
      end;
    end;
    //Capture final "END"
    if not CaptureStr('END') then exit;
  end;
  function PointerDeclaration(const srcpos: TSrcPos): TEleTypeDec;
  {Procesa la declaración de un tipo puntero y devuelve el tipo, ya creado para su
  validación.
  Se asume que ya se ha identificado el inicio de la declaración de un puntero,
  sea en su forma larga: "POINTER TO BYTE" o en su forma corta: ^BYTE }
  var
    reftyp, xtyp: TEleTypeDec;
    typName: String;
  begin
//    if token = '^' then begin
//      //Declaración corta
//      Next;  //Toma '^'
//    {end else begin
//      //Declaración larga: POINTER TO <tipo>
//      Next; //Toma 'POINTER'. Se asume que ya se identificó.
//      ProcComments;
//      if not CaptureStr('TO') then exit;}
//    end;
//    //Por ahora solo permitiremos identificadores de tipos
//    if not FindSysEleType(token, reftyp) then begin
//      //No es un tipo del sistema, pero puede ser un tipo prdefinido
//      reftyp := TreeElems.FindType(token); //Busca elemento
////      reftyp := GetTypeDeclar(decStyle2, TypeCreated2); //Recursive definition
//      if HayError then exit;
//      if reftyp = nil then begin
//        GenError('Expected a type identifier.');
//        exit;
//      end;
//    end;
//    //Verify type existence
//    if not TreeElems.ExistsPointerType(reftyp, xtyp) then begin
//      typName := GenPointerTypeName(refTyp.name);
//      xtyp := CreateEleTypePtr(typName, srcPos, reftyp);
//      if HayError then exit;
//      TypeCreated  := true;     //Activate flag
//    end;
//    Next;   //take type name
//    Result := xtyp;
  end;
var
  typName, tokL: String;
  typ, itemTyp: TEleTypeDec;
  ele: TxpElement;
  srcPos: TSrcPos;
begin
  Result := nil;
  TypeCreated := false;
  ProcComments;
  //Analiza el tipo declarado
  srcPos := GetSrcPos;  //Inicio de declaración
  tokL := lowercase(token);
  if (tokL = 'array') or (token = '[') then begin
    //Es declaración de arreglo
    decStyle := ttdDeclar;  //Es declaración elaborada
    //typ := AddTypeAndOpen('!', -1, tctArray, t_object, srcPos);  //No name by now
    typ := TreeElems.AddElementTypeAndOpen(srcPos, '<undef>', -1, tctArray, t_object);
    if HayError then exit;  //Can be duplicated
    TypeCreated := true;
    ArrayDeclaration(srcpos, itemTyp);
    if HayError then exit(nil);     //Sale para ver otros errores

    typ.itmType := itemTyp; //Item type
    callDefineArray(typ);   //Define operations to array
    TreeElems.CloseElement;  //Close type
  end else if {(tokL = 'pointer') or }(token = '^') then begin
    //Es declaración de puntero
    decStyle := ttdDeclar;  //Es declaración elaborada
    typ := PointerDeclaration(srcpos);
    if HayError then exit(nil);     //Sale para ver otros errores
  end else if (tokL = 'object') then begin
    //Es declaración de objeto
    decStyle := ttdDeclar;  //Es declaración elaborada
    typ := ObjectDeclaration(srcpos);
    if HayError then exit(nil);     //Sale para ver otros errores
  end else if tokType = tkIdentifier then begin
    //Es un identificador de tipo
    typName := token;
    decStyle := ttdDirect;  //Es directo. Se refiere
    TypeCreated := false;
    {Se pensó usar GetOperandIdent(), para identificar al tipo, pero no está preparado
    para procesar tipos y no se espera tanta flexibilidad. Así que se hace "a mano".}
    ele := TreeElems.FindFirstType(typName);
    if ele = nil then begin
      //No identifica a este elemento
      GenError('Unknown identifier: %s', [typName]);
      exit(nil);
    end;
    if ele.idClass = eleTypeDec then begin
      //Es un tipo
      Next;   //toma identificador
      typ := TEleTypeDec(ele);
      //AddCallerToFromCurr(typ);  Poner la llamada aquí, complica el manejo posteriormente.
      if typ.copyOf<>nil then typ := typ.copyOf;  {Apunta al tipo copia. Esto es útil para
                     lograr una mejor compatibilidad cuando se usan Tipos en parámetros de
                     procedimientos.}
    end else begin
      GenError(ER_IDE_TYP_EXP);
      exit(nil);
    end;
  end else begin
    decStyle := ttdDirect;  //Es directo
    GenError(ER_IDE_TYP_EXP);
    exit(nil);
  end;
  {No se capturan delimitadores aquí porque la declaración de tipos, en un contexto más
  general, puede ser seguida de una inicialización.}
  //if not CaptureDelExpres then exit(nil);
  //ProcComments;
  exit(typ);
end;
function TCompMain.GetTypeDeclarSimple(): TEleTypeDec;
{Similar a GetTypeDeclar(), pero solo permite la referencia a tipos simples. No permite la
declaración de nuevos tipos, como: ARRAY OF ...}
var
  decStyle: TTypDeclarStyle;
  TypeCreated: boolean;
begin
  if TreeElems.curCodCont=nil then TreeElems.curCodCont:=typByte; {Ver comentario de CompileVarDeclar()}
  Result := GetTypeDeclar(decStyle, TypeCreated);  //lee tipo
  if HayError then exit;
  if TypeCreated then begin
    //No se permiten declaraciones elaboradas aquí.
    GenError('Only simple types expected here.');
    Result.Destroy;  //Destroy the new type created
    Result := nil;
  end;
end;
procedure TCompMain.CompileTypeDeclar(elemLocat: TxpEleLocation);
{Compila la sección de declaración de un tipo, y genera un elemento TxpEleType, en el
árbol de sintaxis:  TYPE sometype = <declaration>;
}
var
  etyp, reftyp: TEleTypeDec;
  srcpos: TSrcPos;
  decStyle: TTypDeclarStyle;
  typeCreated: boolean;
  typName: String;
begin
  ProcComments;
  if tokType <> tkIdentifier then begin
    GenError(ER_IDEN_EXPECT);
    exit;
  end;
  //hay un identificador
  srcpos := GetSrcPos;
  typName := token;
  Next;
  ProcComments;
  if not CaptureTok('=') then exit;
  if TreeElems.curCodCont=nil then TreeElems.curCodCont:=typByte; {Ver comentario de CompileVarDeclar()}
  etyp := GetTypeDeclar(decStyle, typeCreated);
  if HayError then exit;
  //Analiza la declaración
  if decStyle = ttdDirect then begin
    //Es un tipo referenciado directamente. Algo como TYPE fool = byte;
    //Para este caso, nosotros creamos un tipo nuevo, en  modo copia.
    reftyp := etyp;  //Referencia al tipo { TODO : ¿Y si el tipo es ya una copia? }
    //etyp := CreateEleType(typName, srcPos, reftyp.size, reftyp.catType, reftyp.group);
    etyp := TreeElems.AddElementTypeAndOpen(srcPos, typName, reftyp.size, reftyp.catType, reftyp.group);
    if HayError then exit;  //Can be duplicated
    AddCallerToFromCurr(reftyp);  //Llamada al tipo usado.  { TODO : Validar si se manejan bien las llamadas a los tipos copia. }
    TreeElems.CloseElement;  //Close type
    {Crea la copia del tipo del sistema, que básicamente es el mismo tipo, solo que
    con otro nombre y que además, ahora, está en el árbol de sintaxis, por lo tanto
    tiene otras reglas de alcance.}
    etyp.copyOf := reftyp;        //Indica que es una copia
    etyp.location := elemLocat;   //Ubicación del tipo (Interface/Implementation/...)
  end else begin
    {Las declaraciones elaboradas de tipo (Algo como TYPE fool = array[] of ...),
    crean siempre nuevos tipos.}
    //Validación de duplicidad de nombre.
    if NameExistsIn(UpCase(typName), TreeElems.curNode.elements) then begin
      GenError(ER_DUPLIC_IDEN, [etyp.name], srcpos);
      exit;
    end;
    etyp.name := typName;       //Actualiza el nombre con el nombre de esta definición.
    etyp.srcDec := srcpos;
    etyp.location := elemLocat; //Ubicación del tipo (Interface/Implementation/...)
  end;
  {Valida que solo se manejen tipos de tamaño estático, porque la inicialización de
  variables (que usen este tipo) puede modificar el nombre y tamaño de arreglos de
  tamaño no especificado, y eso cambia la esencia de declarar un tipo con un nombre.
  }
  if (etyp.catType = tctArray) and (etyp.nItems = -1) then begin
    GenError('Array types must have a specified size.');
    exit;
  end; ;
  if not CaptureDelExpres then exit;
  ProcComments;
end;
function TCompMain.AddConstDeclar(constName: string; srcPos:TSrcPos;
                                 out typesCreated: integer): TEleConsDec;
{Add a constant declaration, to the current node of the AST. Aditional types could be
added first. Returns the declaration created.
"typesCreated" returns the number of types created at the first level.
If used in constant declaration, must be called after reading the "=" token.}
var
  cons: TEleConsDec;
  ele: TxpElement;
  posBefore: Integer;
  init: TEleExpress;
begin
  cons := AddConstantAndOpen(constName, typNull, srcPos);
  if HayError then exit;  //Can be duplicated
  //Debe seguir una expresión constante, que no genere código
  init := GetExpression(0);
  if HayError then exit;
  cons.typ := init.Typ;   //Update constant type.
  if init.opType = otConst then begin
    //A simple value. We can initialize the constant.
    cons.value := init.value;
    cons.evaluated := init.evaluated;
  end else begin
    {Puede que siga una expresión "otFunct" como la llamada a una función que
    devolverá a una constante, como en el caso:
    CONST := word(1);
    Pero como no podemos definir el resultado de esa expresión en este nivel, lo
    dejamos pasar.
    }
    //GenError(ER_CON_EXP_EXP);
    //exit;
  end;
  //Other types (no otConst) will be evaluated later.
  TreeElems.CloseElement;  //Close constant.
  //Move types created to this level
  typesCreated := 0;  //Start count.
  for ele in cons.elements do begin
    //Only explore the first level.
    if ele.idClass = eleTypeDec then begin  //It's a type, we need to move
      //Position before of the last element (constant declaration).
      posBefore := TreeElems.curNode.elements.Count - 1;
      //Move the element to this level
      TreeElems.ChangeParentTo(TreeElems.curNode, ele, posBefore);
      inc(typesCreated);  //We expect no more than one type is created in this level.
      { TODO : Hay que estudiar mejor este caso para ver si se pueden generar más de untipo nuevo en el primer nivel. }
    end;
  end;
  exit(cons);
end;
procedure TCompMain.CompileConstDeclar;
var
  consNames: array of string;  //nombre de variables
  srcPosArray: TSrcPosArray;
  n: integer;
begin
  SetLength(consNames, 0);
  //Procesa lista de constantes a,b,cons ;
  getListOfIdent(consNames, srcPosArray);
  if HayError then begin  //precisa el error
    GenError(ER_IDE_CON_EXP);
    exit;
  end;
  if length(consNames)>1 then begin
    GenError('Only one constant can be initialized.');
    exit;
  end;
  //puede seguir "=" o identificador de tipo
  if token = '=' then begin
    Next;  //Pass to the next.
    //Create constant
    AddConstDeclar(consNames[0], srcPosArray[0], n);
  end else begin
    GenError(ER_EQU_COM_EXP);
    exit;
  end;
  if not CaptureDelExpres then exit;
  ProcComments;
  //puede salir con error
end;
procedure TCompMain.CompileVarDeclar;
{Compila la declaración de variables en el nodo actual.
"IsInterface", indica el valor que se pondrá al as variables, en la bandera "IsInterface" }
  function GenTypeName(const xtyp: TEleTypeDec; const baseName: string): string;
  {Genera un nombre para un tipo de acuerdo a su definición. Se supone que no es un tipo
  "atomic".
  Se generan nombres representativos, para poder luego verificar la existencia de tipos
  compatibles. Así por ejemplo si se crea un ARRAY OF char y luego se crea otro ARRAY OF
  char con el mismo tamaño, se preferirá usar el tipo ya creado que además es compatible.}
  begin
    //Genera nombre con caracter inválido para que no se pueda confundir con los que declara el usuario.
    case xtyp.catType of
    tctAtomic : //caso inútil
       exit(xtyp.name);
    tctArray  :  //Arreglo de algo
      exit(GenArrayTypeName(xtyp.itmType.name, xtyp.nItems));
    tctPointer:  //Puntero a algo
      exit(GenPointerTypeName(xtyp.ptrType.name));
    tctObject :  {Objeto. No se da un nombre fijo porque los objetos son muy variables. Podría
                  intentarse con obj-<tipos de campos>, pero podría ser muy largo y poco útil
                  para buscar compatibilidades de tipo}
      exit(PREFIX_OBJ + '-' + baseName); //Caso particular
    else  //Solo pasaría cuando hay ampliaciones
      exit('???-' + baseName);
    end;
  end;
var
  varNames: array of string;  //nombre de variables
  srcPosArray: TSrcPosArray;
  i, typesCreated: Integer;
  xvar: TEleVarDec;
  adicVarDec: TAdicVarDec;
  xtyp: TEleTypeDec;
  decStyle: TTypDeclarStyle;
  typeCreated: boolean;
  nItems: Int64;
begin
  SetLength(varNames, 0);
  //Procesa variables a,b,c : int;
  getListOfIdent(varNames, srcPosArray);
  if HayError then begin  //precisa el error
    GenError(ER_EXP_VAR_IDE);
    exit;
  end;
  //usualmente debería seguir ":"
  if token = ':' then begin
    //Debe seguir, el tipo de la variable
    Next;  //lo toma
    ProcComments;
    //Lee el tipo de la variable.
    {Primero fijamos un contenedor de código cualquiera en TreeElems.curCodCont, para
    que GetTypeDeclar() crea que está dentro de una declaración y así pueda ubicar bien a
    sus elmentos cuando use FindFirst().}
    if TreeElems.curCodCont=nil then TreeElems.curCodCont:=typByte;
    xtyp := GetTypeDeclar(decStyle, typeCreated);
    if HayError then exit;
    //Lee información adicional de la declaración (ABSOLUTE, REGISTER, initial value)
    ProcComments;
    GetAdicVarDeclar(xtyp, adicVarDec, typesCreated);  //Could create new types
    if HayError then begin
      //Cannot destroy, directly, the type created, because the creation has added variables to the AST.
      //if typeCreated then xtyp.Destroy;
      exit;
    end;
    //Verifica si hay asignación de valor inicial.
    ProcComments;
    {Elimina la llamada agregada a la variable, porque se van a agregar llamadas más
    específicas desde la(s) variable(s) declaradas.}
    if adicVarDec.absVar<>nil then begin
      adicVarDec.absVar.RemoveLastCaller;
    end;
    {Aquí, finalmente, se tiene el tipo completo en su estructura porque, si había un
    arreglo no dimensionado, como:  foo []CHAR = 'HELLO'; ya se verificó la
    inicialización.}
    if typeCreated then begin
      //A new type is created in the definition of the variable.
      if adicVarDec.hasInit then begin
        //A initialization exists.
        if typesCreated=1 then begin
          {La inicialización también ha creado un tipo en el AST. Esto se produce en un
          caso de arreglo dinámico, como:
            VAR myarray: ARRAY[] OF char  //Tipo creado en la definición
               = 'abc';          //Tipo creado en la inicialización.
          }
          {Podríamos eliminar un tipo y dejar solo uno, pero para no tener que lidiar con
          la eliminación de un elemento del AST (y sus "callers"). }
          //Actualizamos para que apunte al tipo de la inicialización, porque ya tiene una llamada creada.
          xtyp := adicVarDec.constDec.typ;
        end else begin
          {La inicialización no ha creado un tipo nuevo. Debe ser porque se ha
          se ha reusado el tipo "xtyp". Puede ser algo como:
            VAR myarray: ARRAY[3] OF char  //Tipo creado en la definición
               = 'abc';          //Usa el mismo tipo de la definición.
          }
          //En este caso, solo nos queda darle el nombre aporopiado a xtyp:
          nItems := xtyp.consNitm.value.ValInt;
          xtyp.name := GenArrayTypeName(xtyp.itmType.name, nItems);
        end;
      end
    end;
    //Aquí ya se tiene el tipo creado y en el árbol de sintaxis
    //Reserva espacio para las variables
    for i := 0 to high(varNames) do begin
      xvar := AddVariableAndOpen(varNames[i], xtyp, srcPosArray[i]);
      if HayError then break;        //Sale para ver otros errores
      //Inicia campos
      xvar.adicPar := adicVarDec;    //Actualiza propiedades adicionales
      xvar.location := curLocation;  //Actualiza bandera
      //Updte storage
      case adicVarDec.hasAdic of
      decRegis : xvar.storage := stRegister;
      decRegisA: xvar.storage := stRegistA;
      decRegisX: xvar.storage := stRegistX;
      decRegisY: xvar.storage := stRegistY;
      decAbsol : xvar.storage := stRamFix;  //Will have a fixed memory address.
      decNone  : xvar.storage := stRamFix;  //Will have a fixed memory address.
      end;
      //Agrega la llamada al tipo de la variable.
      AddCallerToFromCurr(xtyp);  //Llamada al tipo usado (Lo usa cada variable declarada.)
      //Agrega llamada a variable cuando se use en ABSOLUTE.
      if adicVarDec.absVar<>nil then begin
        AddCallerToFromCurr(adicVarDec.absVar);
      end;
      //Agrega llamada a constante cuando se use al inicializar
      if adicVarDec.hasInit then begin
        AddCallerToFromCurr(adicVarDec.constDec);
      end;
      TreeElems.CloseElement;  //Close variable
    end;
  end else begin
    GenError(ER_SEM_COM_EXP);
    exit;
  end;
  if not CaptureDelExpres then exit;
  ProcComments;
  //Puede salir con error.
end;
procedure TCompMain.CompileProcDeclar;
{Compila la declaración de procedimientos. Tanto procedimientos como funciones
 se manejan internamente como funciones.}
  function FindProcInInterface(procName: string; const pars: TxpParFuncArray;
                               const srcPos: TSrcPos): TEleFunDec;
  {Explore the current context to verify (and validate) the existence, in the INTERFACE
  section, of a function declared in the IMPLEMENTATION section.
  If found the function, returns the reference. Otherwise returns NIL.
  Also validate the duplicity in the same IMPLEMENTATION section.}
  var
    ele : TxpElement;
    uname: String;
    fun: TEleFun;
    funInterface: TEleFunDec;
  begin
    {Se supone que esta exploración solo se hará en la primera pasada, así que no hay
    problema, en hacer una exploración común.}
    //debugln('Buscando declaración de %s en nodo %s desde 0 hasta %d', [fun.name, ParentElems.name, ParentElems.elements.Count-2]);
    Result := nil;
    uname := upcase(procName);
    {This a doble-scanning, to detect: Existence of declaration in INTERFACE, and
    name duplicity in IMPLEMENTATION. }
    for ele in TreeElems.curNode.elements do begin
      if ele.uname = uname then begin
        //Match the name.
        if ele.location = locInterface then begin
          //Is an INTERFACE element.
          if ele.idClass = eleFuncDec then begin
            //Para las declaraciones, se debe comparar los parámetros
            funInterface := TEleFunDec(ele);
            if funInterface.SameParamsType(pars) then begin
              Result := funInterface;
              //Continue exploring to verify duplicity.
            end;
          end else begin
            //The same name of other element, is conflict.
            GenError('Identifier "%s" already defined', [uname]);
            exit;
          end;
        end else if ele.location = locImplement then begin
          //Is an IMPLEMENTATION element.
          if ele.idClass = eleFunc then begin
            //Para las funciones, se debe comparar los parámetros
            fun := TEleFun(ele);
            if fun.SameParamsType(pars) then begin
              {Two similar functions in the same IMPLEMENTATION scope.}
              GenError(ER_DUPLIC_FUNC_,[procName], srcPos);
              exit(nil);
            end;
          end else begin
            //The same name of other element, is conflict.
            GenError('Identifier "%s" already defined', [uname]);
            exit;
          end;
        end;
      end;
    end;
    //Doesn't found.
  end;
  function FindProcAsForwawd(procName: string; const pars: TxpParFuncArray;
                               const srcPos: TSrcPos): TEleFunDec;
  {Explore the current context to verify (and validate) the existence of a functon
  declared as FORWARD, of any ohter function (No FORWARD).
  If found the function, returns the reference. Otherwise returns NIL.
  Also validate the duplicity of the function.}
  var
    uname: String;
    ele : TxpElement;
    fun: TEleFun;
    fundec: TEleFunDec;
  begin
    Result := nil;
    uname := upcase(procName);
    //Busca en el entorno para ver si está duplicada o con FORWARD
    for ele in TreeElems.curNode.elements do begin
      if ele.uname = uname then begin
        //Match the name.
        if ele.idClass = eleFuncDec then begin
          //Must be a FORWARD
          fundec := TEleFunDec(ele);
          //if fun.IsForward ?
          if fundec.SameParamsType(pars) then begin
            Result := fundec;  //Return FORWARD function
          end;
          //Continue exploring to validate
        end else if ele.idClass = eleFunc then begin
          //Para las funciones, se debe comparar los parámetros
          fun := TEleFun(ele);
          if fun.SameParamsType(pars) then begin
            //Is not FORWARD, must be duplicated:
            GenError(ER_DUPLIC_FUNC_,[procName], srcPos);
            exit;
          end;
        end else begin
          //The same name of other element, is conflict.
          GenError('Identifier "%s" already defined', [uname]);
          exit;
        end;
      end;
    end;
  end;
var
  funDec, funInterface, funForward: TEleFunDec;
  fun: TEleFun;
  bod: TEleBody;
  IsInterrupt, IsForward: Boolean;
  procName, tokL: String;
  retType: TEleTypeDec;
  srcPos: TSrcPos;
  pars: TxpParFuncArray;
begin
  case curLocation of
  locInterface: begin
    //Las declaraciones en INTERFACE son sencillas. Solo son caebceras.
    ReadProcHeader(procName, retType, srcPos, pars, IsInterrupt, IsForward);
    if HayError then exit;
    if IsForward then begin
      GenError('Cannot use FORWARD in INTERFACE section.');
      exit;
    end;
    if TreeElems.FunctionExistInCur(procName, pars) then begin
      GenError(ER_DUPLIC_FUNC_,[procName], srcPos);
      exit;
    end;
    funDec := AddFunctionDEC(procName, retType, srcPos, pars, IsInterrupt);
    funDec.location := curLocation;  //Set location
    funDec.callType := ctUsrNormal;  //Normal function
    exit;  //No more task required.
  end;
  locImplement:  begin
    //Se compila para implementación.
    {Este proceso es más complejo. La idea es compilar el enzabezado de cualquier función,
    y luego comparar para ver si corresponde a una implementación o no. Si es
    implementación, se elimina el nodo creado y se trabaja con el de la declaración.}
    ReadProcHeader(procName, retType, srcPos, pars, IsInterrupt, IsForward);
    if HayError then exit;
    //Verifica si es implementación de una función en la INTERFACE o no.
    funInterface := FindProcInInterface(procName, pars, srcPos);
    if HayError then exit;
    if funInterface<>nil then begin
      //Es una implementación normal
      fun := AddFunctionIMP(procName, retType, srcPos, funInterface, true);
    end else begin
      //Debe ser una función privada. No declarada en INTERFACE.
      //Ya verificamos que no hay conflicto en IMPLEMENTATION.
      fun := AddFunctionUNI(procName, retType, srcPos, pars, IsInterrupt, true);
    end;
    fun.location := curLocation;
    fun.callType := ctUsrNormal;  //Normal function
  end;
  locMain: begin
    //Es una compilación en el programa principal. ¿Y si es FORWARD?
    ReadProcHeader(procName, retType, srcPos, pars, IsInterrupt, IsForward);
    if HayError then exit;
    if IsForward then begin
      if TreeElems.FunctionExistInCur(procName, pars) then begin
        GenError(ER_DUPLIC_FUNC_,[procName], srcPos);
        exit;
      end;
      funDec := AddFunctionDEC(procName, retType, srcPos, pars, IsInterrupt);
      funDec.location := locMain;  //Set location
      funDec.callType := ctUsrNormal;  //Normal function
      funDec.IsForward := true;    //Mark as Forward
      exit;  //No more task required.
    end;
    //This is no-FORWARD function
    funForward := FindProcAsForwawd(procName, pars, srcPos);
    if HayError then exit;
    if funForward<>nil then begin
      //It's an implementation
      fun := AddFunctionIMP(procName, retType, srcPos, funForward, true);
    end else begin
      //It's a common function
      fun := AddFunctionUNI(procName, retType, srcPos, pars, IsInterrupt, true);
    end;
    fun.location := curLocation;
    fun.callType := ctUsrNormal;  //Normal function
  end;
  else
    GenError(ER_NOT_IMPLEM_, ['locMain in TCompMain.CompileProcDeclar()']);
  end;
  //Aquí ya se tiene "fun" abierta, validada y apuntando a la declaración.
  //Empiezan las declaraciones VAR, CONST, PROCEDURE, TYPE
  while StartOfSection do begin
    tokL := lowercase(token);
    if tokL = 'var' then begin
      Next;    //lo toma
      while not StartOfSection and (lowercase(token) <>'begin') do begin
        CompileVarDeclar;
        if HayError then exit;;
      end;
    end else if tokL = 'const' then begin
      Next;    //lo toma
      while not StartOfSection and (lowercase(token) <>'begin') do begin
        CompileConstDeclar;
        if HayError then exit;;
      end;
//    end else if lowercase(token) = 'procedure' then begin
//      Next;    //lo toma
//      CompileProcDeclar;
    end else begin
      GenError('Expected VAR, CONST or BEGIN.');
      exit;
    end;
  end;
  if UpCase(token) <> 'BEGIN' then begin
    GenError('Expected "begin", "var", "type" or "const".');
    exit;
  end;
  //Ahora empieza el cuerpo de la función o las declaraciones
  bod := TreeElems.AddElementBodyAndOpen(GetSrcPos);  //Open Body node
  Next;   //Takes "BEGIN"
  AnalyzeCurBlock;
  TreeElems.CloseElement;  //Close Body node
  bod.srcEnd := GetSrcPos; //End of body
  if HayError then exit;
  TreeElems.CloseElement; //Close Function body
  if not CaptureStr('END') then exit;
  if not CaptureTok(';') then exit;
  ProcComments;  //Quita espacios. Puede salir con error
end;
procedure TCompMain.CompileInlineDeclar(elemLocat: TxpEleLocation);
{Compila la declaración de procedimientos INLINE. Tanto procedimientos como funciones
 INLINE se manejan internamente como funciones.
 IsImplementation, se usa para cuando se está compilando en la sección IMPLEMENTATION.}
begin
//  {Este método, solo se ejecutará en la primera pasada, en donde todos los procedimientos
//  se codifican al inicio de la memoria, y las variables y registros se ubican al
//  inicio de la memoria RAM, ya que lo que importa es simplemente recabar información
//  del procedimiento, y no tanto codificarlo. }
//  CallResetRAM;   //Limpia RAM y FLASH, y fija CurrBank
//  case elemLocat of
//  locInterface: begin
//    //Los procedimientos en INTERFACE, no se procesan aquí. Se procesan en CompileUnit().
//  end;
//  locImplement:  begin
//    //Se compila para implementación.
//    {Este proceso es más complejo. La idea es compilar el encabezado de cualquier función,
//    y luego comparar para ver si corresponde a una implementación o no. Si es
//    implementación, se elimina el nodo creado y se trabaja con el de la declaración.}
//    ReadInlineHeader(procName, retType, srcPos, pars);
//    if HayError then exit;
//    //Verifica si es implementación de una función en la INTERFACE o no.
//    ParentElems := TreeElems.curNode.elements;  //Para comparar
//    {Se supone que esta exploración solo se hará en la primera pasada, así que no hay
//    problema, en hacer una exploración común.}
//    //debugln('Buscando declaración de %s en nodo %s desde 0 hasta %d', [fun.name, ParentElems.name, ParentElems.elements.Count-2]);
//    Found := false;
//    uname := upcase(procName);
//    for ele in ParentElems do begin
//      if ele.location = locInterface then begin
//        //Es elemento de INTERFACE
//        if ele.uname = uname then begin
//          //Hay coincidencia de nombre
//          if ele.idClass = eleFunc then begin
//            //Para las funciones, se debe comparar los parámetros
//            fun := TxpEleInlin(ele);
//            if fun.SameParamsType(pars) then begin
//              Found := true;
//              break;
//            end;
//          end else begin
//            //Si tiene el mismo nombre que cualquier otro elemento, es conflicto
//            GenError('Identifier "%s" already defined', [uname]);
//            exit;
//          end;
//        end;
//      end else begin
//        {Debe ser elemento de IMPLEMENTATION, no hay otra opción porque se supone que
//        estamos en la sección de IMPLEMENTATION, así que el Parent, debe ser una unidad.}
//        GenErrorPos(ER_DUPLIC_FUNC_,[procName], srcPos);  //Está duplicada en IMPLEMENTATION
//        exit;
//      end;
//    end;
//    if Found then begin
//      //Es una implementación. No vale la pena tener otro nodo.
//      TreeElems.OpenElement(fun);  //Abre el nodo anterior
//    end else begin
//      //Debe ser una función privada. No declarada en Interface.
//      //La creamos con seguridad porque ya verificamos que no hay conflicto en IMPLEMENTATION.
//      fun := AddInline(procName, retType, srcPos, pars, CallFunctParam, CallFunctCall);
//      //Un caso especial de proced. declarado solo en IMPLEMENTATION.
//      fun.location := locImplement;
//    end;
//  end;
//  locMain: begin
//    //Es una compilación en el programa principal. ¿Y si es FORWARD?
//    ReadInlineHeader(procName, retType, srcPos, pars);  //Procesa el encabezado
//    if HayError then exit;
//    if TreeElems.InlineExistInCur(procName, pars) then begin
//      GenErrorPos(ER_DUPLIC_FUNC_,[procName], srcPos);
//      exit;
//    end;
//    fun := AddInline(procName, retType, srcPos, pars, CallFunctParam, CallFunctCall);
//    //Aquí estamos en el entorno de la función.
//    fun.location := locMain;
//  end
//  else
//    GenError(ER_NOT_IMPLEM_, ['locMain in TCompMain.CompileInlineDeclar()']);
//  end;
//  //Aquí ya se tiene "fun" abierta, validada y apuntando a la declaración.
//  //Empiezan las declaraciones VAR, CONST, PROCEDURE, TYPE
//  while StartOfSection do begin
//    if tokL = 'var' then begin
//      Next;    //lo toma
//      while not StartOfSection and (tokL <>'begin') do begin
//        CompileVarDeclar;
//        if HayError then exit;;
//      end;
//    end else if tokL = 'const' then begin
//      Next;    //lo toma
//      while not StartOfSection and (tokL <>'begin') do begin
//        CompileConstDeclar;
//        if HayError then exit;;
//      end;
////    end else if tokL = 'procedure' then begin
////      Next;    //lo toma
////      CompileProcDeclar;
//    end else begin
//      GenError('Expected VAR, CONST or BEGIN.');
//      exit;
//    end;
//  end;
//  if tokL <> 'begin' then begin
//    GenError('Expected "begin", "var", "type" or "const".');
//    exit;
//  end;
//  //Ahora empieza el cuerpo de la función o las declaraciones
//  fun.posCtx := PosAct;  //Guarda posición para la segunda compilación
//  bod := CreateBody;   //crea elemento del cuerpo de la función
//  bod.srcDec := GetSrcPos;
//  TreeElems.AddElementAndOpen(bod);  //Abre nodo Body
//  CompileInlineBody(fun);
//  TreeElems.CloseElement;  //Cierra Nodo Body
//  TreeElems.CloseElement; //cierra espacio de nombres de la función
//  bod.srcEnd := GetSrcPos;  //Fin de cuerpo
////  fun.adrReturn := pic.iRam-1;  //Guarda dirección del i_RETURN
//  if not CaptureTok(';') then exit;
//  ProcComments;  //Quita espacios. Puede salir con error
end;
function TCompMain.VerifyEND: boolean;
{Compila la parte final de la estructura, que en el modo PicPas, debe ser el
 delimitador END. Si encuentra error, devuelve FALSE.}
begin
  Result := true;   //por defecto
  if mode = modPicPas then begin
    //En modo PicPas, debe haber un delimitador de bloque
    if not CaptureStr('END') then exit(false);
  end;
end;
function TCompMain.GetCondition(out ex: TEleExpress; ConstBool: boolean=false): boolean;
{Create a Condition Block and read a boolean expression in "ex".
If parameter ConstBool is activated, a constant expression, evaluated to TRUE, is
inserted.
If there is some error, returns FALSE.}
var
  elem: TEleCondit;
begin
  //Create condition block
  elem := TEleCondit.Create;
  elem.name := 'cond';
  elem.srcDec := GetSrcPos;
  TreeElems.AddElementAndOpen(elem);
  if ConstBool then begin
    //Generates a boolean constant.
    GenExpressionConstBool('else', true, GetSrcPos);
  end else begin
    //Get the boolean expression
    OnExprStart;
    ex := GetExpression(0);
    if HayError then exit(false);
    OnExprEnd(pexSTRUC);
    if ex.Typ <> typBool then begin
      GenError(ER_BOOL_EXPECT);
      exit(false);
    end;
    ProcComments;
  end;
  TreeElems.CloseElement;  //Close condition block
  exit(true);  //No hay error
end;
function TCompMain.OpenContextFrom(filePath: string): boolean;
{Abre un contexto con el archivo indicado. Si lo logra abrir, devuelve TRUE.}
var
  strList: TStrings;
  notFound: boolean;
begin
  //Primero ve si puede obteenr acceso directo al contenido del archivo
  if OnRequireFileString<>nil then begin
    //Hace la consulta a través del evento
    strList := nil;
    OnRequireFileString(filePath, strList);
    if strList=nil then begin
      //No hay acceso directo al contenido. Carga de disco
      //debugln('>disco:'+filePath);
      NewContextFromFile(filePath, notFound);
      Result := not notFound;  //El único error es cuando no se encuentra el archivo.
    end else begin
      //Nos están dando el acceso al contenido. Usamos "strList"
      NewContextFromTStrings(strList, filePath);
      Result := true;
    end;
  end else begin
    //No se ha establecido el evento. Carga de disco
    //debugln('>disco:'+filePath);
    NewContextFromFile(filePath, notFound);
    Result := not notFound;  //El único error es cuando no se encuentra el archivo.
  end;
end;
function TCompMain.CompileStructBody: boolean;
{Compila el cuerpo de un THEN, ELSE, WHILE, ... considerando el modo del compilador.
Si se genera error, devuelve FALSE. }
begin
  //Este es el modo normal. Genera código.
  if mode = modPascal then begin
    //En modo Pascal se espera una instrucción
    AnalyzeSentence;
  end else begin
    //En modo normal
    AnalyzeCurBlock;
  end;
  if HayError then exit(false);
  //Salió sin errores
  exit(true);
end;
procedure TCompMain.AnalyzeIF;
{Compile an IF structure.}
var
  ex: TEleExpress;
begin
  if not GetCondition(ex) then exit;
  if not CaptureStr('THEN') then exit; //toma "then"
  //Compile the THEN part.
  TreeElems.AddElementBlockAndOpen(GetSrcPos);  //Open block
  if not CompileStructBody then exit;
  TreeElems.CloseElement;  //Close block
  //Compila los ELSIF que pudieran haber
  while UpCase(token) = 'ELSIF' do begin
    Next;   //toma "elsif"
    if not GetCondition(ex) then exit;
    if not CaptureStr('THEN') then exit;  //toma "then"
    //Compila el cuerpo pero sin código
    TreeElems.AddElementBlockAndOpen(GetSrcPos);  //Open block
    if not CompileStructBody then exit;
    TreeElems.CloseElement;  //Close block
  end;
  //Compila el ELSE final, si existe.
  if Upcase(token) = 'ELSE' then begin
    //Hay bloque ELSE, pero no se ejecutará nunca
    Next;   //Takes  "else"
    //An "else" is similar to a ELSIF true
    GetCondition(ex, true);  //Create condiiton block with a fixed TRUE constant.
    TreeElems.AddElementBlockAndOpen(GetSrcPos);  //Open block
    if not CompileStructBody then exit;
    TreeElems.CloseElement;  //Close block
    if not VerifyEND then exit;
  end else begin
    VerifyEND;
  end;
end;
procedure TCompMain.AnalyzeWHILE;
{Compile a WHILE structure.}
var
  ex: TEleExpress;
begin
  if not GetCondition(ex) then exit;  //Condición
  if not CaptureStr('DO') then exit;  //toma "do"
  //Aquí debe estar el cuerpo del "while"
  TreeElems.AddElementBlockAndOpen(GetSrcPos);  //Open block
  if not CompileStructBody then exit;
  TreeElems.CloseElement;  //Close block
  if not VerifyEND then exit;
end;
procedure TCompMain.AnalyzeREPEAT;
{Compile the WHILE structure.}
var
  ex: TEleExpress;
begin
  TreeElems.AddElementBlockAndOpen(GetSrcPos);  //Open block
  AnalyzeCurBlock;
  TreeElems.CloseElement;  //Close block
  if HayError then exit;
  SkipWhites;
  if not CaptureStr('UNTIL') then exit; //toma "until"
  if not GetCondition(ex) then exit;
end;
procedure TCompMain.AnalyzeFOR;
{Compila uan extructura FOR}
var
  Op1, Op2, idx: TEleExpress;
begin
  {Get the first asignment: i:=0; }
  //This section is similar to AnalyzeSentence().
  Op1 := GetExpression(0);
  if HayError then exit;
  if Op1.name<>'_set' then begin
    GenError('Expected ":=".', Op1.srcDec);
    exit;
  end;
  //We have an asigment here
  idx := TEleExpress(Op1.elements[0]);  //Get index variable (like "i").
  if idx.opType <> otVariab then begin
    GenError(ER_VARIAB_EXPEC);
    exit;
  end;
  if (idx.Typ<>typByte) and (idx.Typ<>typWord) then begin
    GenError(ER_ONL_BYT_WORD);
    exit;
  end;
  SkipWhites;
  //Ya se tiene la asignación inicial
  if not CaptureStr('TO') then exit;
  //Toma expresión Final
  Op2 := GetExpression(0);
  if HayError then exit;
  if Op2.Typ <> idx.Typ then begin
    GenError('Expected expression of type %s', [idx.Typ.name], Op2.srcDec);
    exit;
  end;
  SkipWhites;
  if not CaptureStr('DO') then exit;  //toma "do"
  //Aquí debe estar el cuerpo del "for"
  TreeElems.AddElementBlockAndOpen(GetSrcPos);  //Open block
  if not CompileStructBody then exit;
  TreeElems.CloseElement;  //Close block
  if not VerifyEND then exit;
end;
procedure TCompMain.MoveInternalTypes(node: TxpElement;
             declarSec: TxpElement; declarPos: Integer);
{Explore all the types declared in "node" at any deep level and move it to
the current declaartion section.}
var
  ele: TxpElement;
begin
  if node.elements= nil then exit;
  for ele in node.elements do begin
    //Explore the first level.
    if ele.idClass = eleTypeDec then begin  //It's a type, we need to move
      //Move the element to declaration section
      TreeElems.ChangeParentTo(declarSec, ele, declarPos);
    end;
    //Explore in deeper level
    MoveInternalTypes(ele, declarSec, declarPos);
  end;
end;
procedure TCompMain.AnalyzeSentence;
{Compile one Pascal sentence. One sentence can be:
 1. Assigment sentence.
 2. Procedure call.
 3. Function operand.
 4. BEGIN ... END block.
 5. IF sentence.
 6. LOOP instruction (WHILE, REPEAT, FOR)
 7. CASE sentence.
 Sentence doesn't include delimiter.
 Can generate Error.
 }
var
  tokUp: String;
  ele, declarSec: TxpElement;
  ex: TEleExpress;
  declarPos: Integer;
  snt: TxpEleSentence;
begin
  if not (TreeElems.curNode.idClass in [eleBody, eleBlock]) then begin
    //No debería pasar, porque las instrucciones solo pueden estar en eleBody
    GenError('Syntax error.');
    exit;
  end;
  ProcComments;
  if tokType in [tkIdentifier, tkKeyword] then begin
    tokUp := Upcase(token);
    if tokUp = 'BEGIN' then begin
      //Es bloque
      Next;  //toma "begin"
      TreeElems.AddElementBlockAndOpen(GetSrcPos);  //Open block
      AnalyzeCurBlock;   //Recursive call
      TreeElems.CloseElement;  //Close block
      if HayError then exit;
      if not CaptureStr('END') then exit;
      ProcComments;
      //puede salir con error
    end else if tokUp = 'IF' then begin
      TreeElems.AddElementSentAndOpen(GetSrcPos, sntIF);  //Open sentence
      Next;         //Takes "if"
      AnalyzeIF;
      if HayError then begin
        exit;
      end else if TreeElems.curNode.idClass <> eleSenten then begin
        //Something went wrong
        GenError('Syntax error.');
        exit;
      end;
      TreeElems.CloseElement;  //Close sentence
    end else if tokUp = 'WHILE' then begin
      TreeElems.AddElementSentAndOpen(GetSrcPos, sntWHILE);  //Open sentence
      Next;         //Takes "while"
      AnalyzeWHILE;
      if HayError then begin
        exit;
      end else if TreeElems.curNode.idClass <> eleSenten then begin
        //Something went wrong
        GenError('Syntax error.');
        exit;
      end;
      TreeElems.CloseElement;  //Close sentence
    end else if tokUp = 'REPEAT' then begin
      TreeElems.AddElementSentAndOpen(GetSrcPos, sntREPEAT);  //Open sentence
      Next;         //Takes "until"
      AnalyzeREPEAT;
      if HayError then begin
        exit;
      end else if TreeElems.curNode.idClass <> eleSenten then begin
        //Something went wrong
        GenError('Syntax error.');
        exit;
      end;
      TreeElems.CloseElement;  //Close sentence
    end else if tokUp = 'FOR' then begin
      TreeElems.AddElementSentAndOpen(GetSrcPos, sntFOR);  //Open sentence
      Next;         //Takes "until"
      AnalyzeFOR;
      if HayError then begin
        exit;
      end else if TreeElems.curNode.idClass <> eleSenten then begin
        //Something went wrong
        GenError('Syntax error.');
        exit;
      end;
      TreeElems.CloseElement;  //Close sentence
    end else if tokUp = 'ASM' then begin  //ASM block
      TreeElems.AddElementSentAndOpen(GetSrcPos, sntAsmBlock);  //Open sentence
      vParserASM_6502.ProcessASMBlock(self);
      //In case there is not support for ASM blocks, this code can bypass the block.
      //repeat Next; until atEof or (UpCase(token)='END');
      //if atEof then begin
      //  GenError('Unclosed ASM block.');
      //end;
      //Next;
      TreeElems.CloseElement;  //Close sentence
    end else begin
      //Could be Assigment sentence, Procedure call or Function operand.
      snt := TreeElems.AddElementSentAndOpen(GetSrcPos, sntAssign); //Open sentence
      //Parse expression
      GetExpression(0);
      //Validate expression
      if HayError then begin
        exit;
      end else if TreeElems.curNode.idClass <> eleSenten then begin
        //Something went wrong
        GenError('Syntax error.');
        exit;
      end;
      //Check for possible types generated to move to the declaration section.
      declarSec := TreeElems.curCodCont.Parent;  //Declaraion section.
      declarPos := TreeElems.curCodCont.Index;  //Position before of the body.
      MoveInternalTypes(TreeElems.curNode, declarSec, declarPos);
      //Take the first (should be the unique) element.
      ele :=TreeElems.curNode.elements[0];
      //We expected one expression element.
      if ele.idClass = eleExpress then begin
        //The expected element
        ex := TEleExpress(ele);
        if ex.opType = otFunct then begin
          //Should be a procedure or function call.
          if ex.fcallOp then begin   //It comes from an operator
            //Only assignment ':=' is allowed.
            if ex.name <> '_set' then begin
               GenError('Expressions are not allowed here.', ex.srcDec);
            end;
          end else begin             //Should be a function call
            snt.sntType := sntProcCal;   //Update type.
          end;
        end else begin
          //Returns a type. Should be an expression
          GenError('Invalid sentence.', ex.srcDec);
        end;
      end else begin  //Maybe a simple operand.
        GenError('Expression expected.');
      end;
      TreeElems.CloseElement;  //Close sentence
    end;
  end else begin
    //Any other thing.
    GenError('Syntax error.');
  end;
  //Can terminate with error.
end;
procedure TCompMain.AnalyzeCurBlock;
{Compila el bloque de código actual hasta encontrar un delimitador de bloque, o fin
de archivo. }
begin
  ProcComments;
  while not atEof and (tokType<>tkBlkDelim) do begin
    //se espera una expresión o estructura
    AnalyzeSentence;
    if HayError then exit;   //aborta
    //se espera delimitador
    if atEof then break;  //sale por fin de archivo
    //busca delimitador
    ProcComments;
    //Puede terminar con un delimitador de bloque
    if tokType=tkBlkDelim then break;
    //Pero lo común es que haya un delimitador de expresión
    if not CaptureTok(';') then exit;
    ProcComments;  //Puede haber Directivas o ASM también
  end;
end ;
function TCompMain.IsUnit: boolean;
{Indica si el archivo del contexto actual, es una unidad. Debe llamarse}
begin
  ProcCommentsNoExec;  //Solo es validación, así que no debe ejecutar nada
  //Busca UNIT
  if lowercase(token) = 'unit' then begin
    curCtx.StartScan;   //retorna al inicio
    exit(true);
  end;
  curCtx.StartScan;   //retorna al inicio
  exit(false);
end;
//Compilación de secciones
procedure TCompMain.CompileUsesDeclaration;
{Compila la unidad indicada.}
var
  uni: TEleUnit;
  uPath: String;
  uName: String;
  p: TContextState;
begin
  if lowercase(token) = 'uses' then begin
    Next;  //pasa al nombre
    //Toma una a una las unidades
    repeat
      ProcComments;
      //ahora debe haber un identificador
      if tokType <> tkIdentifier then begin
        GenError(ER_IDEN_EXPECT);
        exit;
      end;
      //hay un identificador de unidad
      uName := token;
      uni := CreateUnit(uName);
      //Verifica si existe ya el nombre de la unidad
      if NameExistsIn(uni.uname, TreeElems.curNode.elements) then begin
        GenError('Identifier duplicated: %s.', [uName]);
        uni.Destroy;
        exit;
      end;
      uni.srcDec := GetSrcPos;   //guarda posición de declaración
      uName := uName + '.pas';  //nombre de archivo
{----}TreeElems.AddElementAndOpen(uni);
      //Ubica al archivo de la unidad
      p := GetCtxState;   //Se debe guardar la posición antes de abrir otro contexto
      //Primero busca en la misma ubicación del archivo fuente
      uPath := ExtractFileDir(mainFile) + DirectorySeparator + uName;
      if OpenContextFrom(uPath) then begin
        uni.srcFile := uPath;   //Gaurda el archivo fuente
      end else begin
        //No lo encontró, busca en la carpeta de dispositivos
        uPath := devicesPath + DirectorySeparator + uName;
        if OpenContextFrom(uPath) then begin
          uni.srcFile := uPath;   //Gaurda el archivo fuente
        end else begin
           //No lo encontró, busca en la carpeta de librerías
           uPath := patUnits + DirectorySeparator + uName;
           if OpenContextFrom(uPath) then begin
             uni.srcFile := uPath;   //Gaurda el archivo fuente
           end else begin
             //No lo encuentra
             GenError(ER_FIL_NOFOUND, [uName]);
             exit;
           end;
        end;
      end;
      //Aquí ya se puede realizar otra exploración, como si fuera el archivo principal
      DoAnalyzeUnit(uni);
      SetCtxState(p);
      if HayError then exit;  //El error debe haber guardado la ubicación del error
{----}TreeElems.CloseElement; //cierra espacio de nombres de la función
      Next;  //toma nombre
      SkipWhites;
      if token <> ',' then break; //sale
      Next;  //toma la coma
    until false;
    if not CaptureDelExpres then exit;
  end;
end;
procedure TCompMain.DoAnalyzeUnit(uni: TxpElement);
{Realiza la compilación de una unidad}
var
  elem: TxpElement;
  fundec: TEleFunDec;
  tokL: String;
begin
//debugln('   Ini Unit: %s-%s',[TreeElems.curNode.name, ExtractFIleName(curCon.fileSrc)]);
  ClearError;
  ProcComments;
  //Busca UNIT
  if lowercase(token) = 'unit' then begin
    Next;  //pasa al nombre
    ProcComments;
    if atEof then begin
      GenError('Name of unit expected.');
      exit;
    end;
    if UpCase(token)<>uni.uname then begin
      GenError('Name of unit doesn''t match file name.');
      exit;
    end;
    Next;  //Toma el nombre y pasa al siguiente
    if not CaptureDelExpres then exit;
  end else begin
    GenError('Expected: UNIT');
    exit;
  end;
  ProcComments;
  if lowercase(token) <> 'interface' then begin
    GenError('Expected: INTERFACE');
    exit;
  end;
  Next;   //toma
  ProcComments;
  curLocation := locInterface;
  if atEof then begin
    GenError('Expected "uses", "var", "type", "const" or "implementation".');
    exit;
  end;
  ProcComments;
  //Busca USES
  CompileUsesDeclaration;
  if atEof then begin
    GenError('Expected "var", "type" or "const".');
    exit;
  end;
  curLocation := locInterface;  //Restore right location
  ProcComments;
//  Cod_StartProgram;  //Se pone antes de codificar procedimientos y funciones
  if HayError then exit;
  //Empiezan las declaraciones
  while StartOfSection do begin
    tokL := lowercase(token);
    if tokL = 'var' then begin
      Next;    //lo toma
      while not StartOfSection and (lowercase(token) <>'implementation') do begin
        CompileVarDeclar;  //marca como "IsInterface"
        if HayError then exit;;
      end;
    end else if tokL = 'type' then begin
      Next;    //lo toma
      while not StartOfSection and (lowercase(token) <>'implementation') do begin
        CompileTypeDeclar(locInterface);
        if HayError then exit;
      end;
    end else if tokL = 'const' then begin
      Next;    //lo toma
      while not StartOfSection and (lowercase(token)<>'implementation') do begin
        CompileConstDeclar;
        if HayError then exit;;
      end;
    end else if tokL = 'procedure' then begin
      Next;    //lo toma
      CompileProcDeclar;
      if HayError then exit;
    end else begin
      GenError(ER_NOT_IMPLEM_, [token]);
      exit;
    end;
  end;
  ProcComments;
  if lowercase(token) <> 'implementation' then begin
    GenError('Expected: IMPLEMENTATION');
    exit;
  end;
  Next;   //toma
  /////////////////  IMPLEMENTATION /////////////////////
  ProcComments;
  //Explora las declaraciones e implementaciones
  curLocation := locImplement;
  //Empiezan las declaraciones
  while StartOfSection do begin
    if tokL = 'var' then begin
      Next;    //lo toma
      while not StartOfSection and (tokL <>'end') do begin
        CompileVarDeclar;
        if HayError then exit;;
      end;
    end else if tokL = 'const' then begin
      Next;    //lo toma
      while not StartOfSection and (tokL <>'end') do begin
        CompileConstDeclar;
        if HayError then exit;;
      end;
    end else if tokL = 'procedure' then begin
      Next;    //lo toma
      CompileProcDeclar;  //Compila en IMPLEMENTATION
      if HayError then exit;
    end else begin
      GenError(ER_NOT_IMPLEM_, [token]);
      exit;
    end;
  end;
  //Verifica si todas las funciones de INTERFACE, se implementaron
  for elem in TreeElems.curNode.elements do if elem.idClass = eleFuncDec then begin
    fundec := TEleFunDec(elem);
    if fundec.implem = nil then begin
      GenError('Function %s not implemented.', [fundec.name], fundec.srcDec);
      exit;
    end;
  end;
  CompileLastEnd;
  if HayError then exit;
//  //procesa cuerpo
//  ResetRAM;  {No es tan necesario, pero para seguir un orden y tener limpio
//                     también, la flash y memoria, después de algún psoible procedimiento.}
//  if tokL = 'begin' then begin
//    bod := CreateBody;
//    bod.srcDec := GetSrcPos;
//    Next;   //coge "begin"
//    //Guardamos la ubicación física, real, en el archivo, después del BEGIN
//    bod.posCtx := PosAct;
//    //codifica el contenido
//    AnalyzeCurBlock;   //compila el cuerpo
//    if HayError then exit;

//    _SLEEP();   //agrega instrucción final
//  end else begin
//    GenError('Expected "begin", "var", "type" or "const".');
//    exit;
//  end;
//  Cod_EndProgram;
//debugln('   Fin Unit: %s-%s',[TreeElems.curNode.name, ExtractFIleName(curCon.fileSrc)]);
end;
procedure TCompMain.DoAnalyzeProgram;
{Performs the Analysis (Lexical, syntactic and semantic).
Input: The current context.
Output: The AST.}
var
  bod: TEleBody;
  elem: TxpElement;
  fundec: TEleFunDec;
  tokL: String;
begin
  ClearError;
  ProcComments;
  //Busca PROGRAM
  tokL := lowercase(token);
  if tokL = 'unit' then begin
    //Se intenta compilar una unidad
    GenError('Expected a program. No a unit.');
    exit;
  end else if tokL = 'program' then begin
    Next;  //pasa al nombre
    ProcComments;
    if atEof then begin
      GenError(ER_PROG_NAM_EX);
      exit;
    end;
    Next;  //Toma el nombre y pasa al siguiente
    if not CaptureDelExpres then exit;
  end;
  if atEof then begin
    GenError('Expected "program", "begin", "var", "type" or "const".');
    exit;
  end;
  ProcComments;
  //Busca USES
  if HayError then exit;  //CompileUsesDeclaration, va a limpiar "HayError"
  CompileUsesDeclaration;
  if atEof then begin
    GenError('Expected "begin", "var", "type" or "const".');
    exit;
  end;
  ProcComments;
  callStartProgram;  //Se pone antes de codificar procedimientos y funciones
  curLocation := locMain;
  if HayError then exit;
  //Empiezan las declaraciones
  while StartOfSection do begin
    tokL := lowercase(token);
    if tokL = 'var' then begin
      Next;    //lo toma
      while not StartOfSection and (lowercase(token) <>'begin') do begin
        CompileVarDeclar;
        if HayError then exit;
      end;
    end else if tokL = 'type' then begin
      Next;    //lo toma
      while not StartOfSection and (lowercase(token) <>'begin') do begin
        CompileTypeDeclar(locMain);
        if HayError then exit;
      end;
    end else if tokL = 'const' then begin
      Next;    //lo toma
      while not StartOfSection and (lowercase(token) <>'begin') do begin
        CompileConstDeclar;
        if HayError then exit;
      end;
    end else if tokL = 'procedure' then begin
      Next;    //lo toma
      CompileProcDeclar;
      if HayError then exit;
    end else if tokL = 'inline' then begin
      Next;    //lo toma
      CompileInlineDeclar(locMain);
      if HayError then exit;
    end else begin
      GenError(ER_NOT_IMPLEM_, [token]);
      exit;
    end;
  end;
  //Procesa cuerpo
  if Upcase(token) <> 'BEGIN' then begin
    GenError('Expected "begin", "var", "type" or "const".');
    exit;
  end;
  bod := TreeElems.AddElementBodyAndOpen(GetSrcPos);  //Abre nodo Body
  Next;   //Takes "BEGIN"
  AnalyzeCurBlock;   //Compiles the body
  TreeElems.CloseElement;   //No debería ser tan necesario.
  bod.srcEnd := GetSrcPos;
  if HayError then exit;
  //Verifica si todas las funciones FORWARD, se implementaron
  for elem in TreeElems.curNode.elements do if elem.idClass = eleFuncDec then begin
    fundec := TEleFunDec(elem);
    if fundec.implem = nil then begin
      GenError('Function %s not implemented.', [fundec.name], fundec.srcDec);
      exit;
    end;
  end;
  CompileLastEnd;  //Compila el "END." final
  if HayError then exit;
  //_RTS();   //agrega instrucción final
  callEndProgram;
end;

end.

