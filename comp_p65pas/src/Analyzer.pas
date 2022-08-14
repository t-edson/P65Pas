unit Analyzer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Types, CompBase, XpresElemP65,
  LexPas, ParserASM_6502, CPUCore, CompGlobals;
type

  { TAnalyzer }
  TAnalyzer = class(TCompilerBase)
  public    //Access to CPU hardware.
    picCore    : TCPUCore;   //Objeto PIC Core. This is an abstraction. Real CPU is not yet specified.
    function PICName: string; virtual; abstract;
    function RAMmax: integer; virtual; abstract;
  private
    function GetConstValue(typExpec: TEleTypeDec; out mainTypCreated: TEleTypeDec
      ): TEleExpress;
    procedure AnalyzeEXIT(exitSent: TEleSentence);
    procedure AnalyzeFOR;
    procedure AnalyzeIF;
    procedure AnalyzeREPEAT;
    procedure AnalyzeWHILE;
    procedure MoveInternalTypes(node: TxpElement; declarSec: TxpElement;
      declarPos: Integer);
  protected
    function GetUnitDeclaration: boolean;
    function StartOfSection: boolean;
    procedure CompileLastEnd;
  protected  //Elements processing
    procedure GetAdicVarDeclar(var varTyp: TEleTypeDec; out aditVar: TAdicVarDec;
      out mainTypCreated: TEleTypeDec);
    procedure ReadProcHeader(out procName: String; out retType: TEleTypeDec; out
      srcPos: TSrcPos; out pars: TxpParFuncArray; out IsInterrupt,
  IsForward: Boolean);
    procedure AnalyzeVarDeclar;
    procedure AnalyzeConstDeclar;
    procedure AnalyzeTypeDeclar(elemLocat: TxpEleLocation);
    function GetTypeDeclar(out decStyle: TTypDeclarStyle; out TypeCreated: boolean;
      location: TTypeLocat = tlCurrNode): TEleTypeDec;
    function GetTypeDeclarSimple(): TEleTypeDec;
    function GetTypeDeclarColon(out decTyp: TEleTypeDec; out
      decStyle: TTypDeclarStyle; out TypeCreated: boolean): boolean;
    function VerifyEND: boolean;
    function GetCondition(out ex: TEleExpress; ConstBool: boolean = false): boolean;
    function OpenContextFrom(filePath: string): boolean;
    function AnalyzeStructBody: boolean;
    procedure AnalyzeSentence;
    procedure AnalyzeCurBlock;
    procedure AnalyzeProcDeclar;
    procedure AnalyzeInlineDeclar(elemLocat: TxpEleLocation);
    procedure AnalyzeUsesDeclaration;
    procedure DoAnalyzeUnit(uni: TxpElement);
    procedure DoAnalyzeProgram;
    procedure DoAnalyze;
  public
    {Indica que TCompiler, va a acceder a un archivo, peor está preguntando para ver
     si se tiene un Stringlist, con los datos ya caragdos del archivo, para evitar
     tener que abrir nuevamente al archivo.}
    OnRequireFileString: procedure(FilePath: string; var strList: TStrings) of object;
  end;

implementation
resourcestring
  ER_INV_MEMADDR  = 'Invalid memory address.';
  ER_EXP_VAR_IDE  = 'Identifier of variable expected.';
  ER_NUM_ADD_EXP  = 'Numeric address expected.';
  ER_CON_EXP_EXP  = 'Constant expression expected.';
  ER_EQU_EXPECTD  = '"=" expected.'               ;
  ER_IDEN_EXPECT  = 'Identifier expected.'        ;
  ER_NOT_IMPLEM_  = 'Not implemented: "%s"'       ;
  ER_SEM_COM_EXP  = '":" or "," expected.'        ;
  ER_INV_ARR_SIZ  = 'Invalid array size.';
  ER_ARR_SIZ_BIG  = 'Array size to big.';
  ER_IDE_TYP_EXP  = 'Identifier of type expected.';
  ER_IDE_CON_EXP  = 'Identifier of constant expected.';
  ER_EQU_COM_EXP  = '"=" or "," expected.';
  ER_DUPLIC_IDEN  = 'Duplicated identifier: "%s"';
  ER_BOOL_EXPECT  = 'Boolean expression expected.';
  ER_EOF_END_EXP  = 'Unexpected end of file. "end" expected.';
  ER_ELS_UNEXPEC  = '"else" unexpected.';
  ER_END_EXPECTE  = '"end" expected.';
  ER_NOT_AFT_END  = 'Syntax error. Nothing should be after "END."';
  ER_INST_NEV_EXE = 'Instruction will never execute.';
  ER_UNKN_STRUCT  = 'Unknown structure.'          ;
  ER_DUPLIC_FUNC_ = 'Duplicated function: %s'     ;
  ER_FIL_NOFOUND  = 'File not found: %s'         ;
  ER_PROG_NAM_EX  = 'Program name expected.'      ;
  ER_VARIAB_EXPEC = 'Variable expected.'         ;
  ER_ONL_BYT_WORD = 'Only BYTE or WORD index is allowed in FOR.';
  ER_UNKNOWN_IDE_ = 'Unknown identifier: %s'    ;

function TAnalyzer.StartOfSection: boolean;
var
  tokL: String;
begin
  tokL := lowercase(token);
  Result := (tokL ='var') or (tokL ='const') or
            (tokL ='type') or (tokL ='procedure') or (tokL ='inline');
end;
procedure TAnalyzer.CompileLastEnd;
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
function TAnalyzer.GetConstValue(typExpec: TEleTypeDec;
                                 out mainTypCreated: TEleTypeDec): TEleExpress;
{Add a constant expression, to the current node of the AST. Returns the declaration
created.
"typExpec" is the expected type of the constant. If it's NIL it's ignored.

Aditional types could be added first (when analyzing constant like Arrays). If new types
are created they are added to the parent node, before the current node.

Parameter "mainTypCreated" returns the main type created (When at least one type is
created). Some constant like: [[1],[2],[3]] creates two types.
When no new types are created, "mainTypCreated" returns NIL.
}
var
  init: TEleExpress;
  ntyp, typesCreated, indexNode: Integer;
  parentNod, typDecElem: TxpElement;
begin

  //Debe seguir una expresión constante, que no genere código
  typesCreated := 0;  //Default value
  ntyp := nTypesCreated;   //Set for count
  if typExpec=nil then begin
    init := GetExpression(0);
    if HayError then exit(nil);
  end else begin  //With type verification
    if typExpec.catType = tctArray then begin
      //Literal array. We read in the format (<item>,<item>,... )
      ProcComments;
      if token = '(' then begin   //Common Pascal syntax ( ... )
        init := GetConstantArray(')');
        TreeElems.OpenElement(init.Parent);  //Returns to parent because GetConstantArray() has created an opened a node.
        if HayError then exit(nil);
      end else if token = '[' then begin  //Alternate syntax [ ... ]
        init := GetConstantArray(']');
        TreeElems.OpenElement(init.Parent);  //Returns to parent because GetConstantArray() has created an opened a node.
        if HayError then exit(nil);
      end else if tokType = tkString then begin  //Alternate syntax "abc" onlcy for char.
        init := GetConstantArrayStr(false);
        TreeElems.OpenElement(init.Parent);  //Returns to parent because GetConstantArray() has created an opened a node.
        if HayError then exit(nil);
      end else begin
        GenError('Expected "("');
        exit(nil);
      end;
    end else begin
      init := GetExpression(0);
      if HayError then exit(nil);
    end;
    if typExpec.isDynam then begin
      //Dynamic arrays cannot be validated directly.
    end else begin
      if typExpec<>init.Typ then begin
        GenError('Expected a constant of type %s, got %s', [typExpec.name, init.Typ.name]);
      end;
    end;
  end;
  {We don't verify "init.Sto = stConst" here because some constant declarations
  can be expressions "ofFunt" with storage "stNone" like :
        = CONST1 + CONST2;
        = word(1);
        = @(variable1);
  Validation for constant will be done in Optimization level.}
  //This is an accurate measure for the types created.
  typesCreated := nTypesCreated-ntyp;  //Calculate types created.
  //This is not an accurate way to obtain the main type creaded.
  if typesCreated>0 then begin
    {This will work because GetExpression() and GetOperand() creates types at the parent
    level.}
    parentNod :=  TreeElems.curNode.Parent;
    indexNode :=  TreeElems.curNode.Index;
    typDecElem := parentNod.elements[indexNode-1];
    if typDecElem.idClass = eleTypeDec then begin
      mainTypCreated := TEleTypeDec(typDecElem);
    end else begin  //Wouldn't happens.
      mainTypCreated := nil;
      GenError('Design error');
      exit;
    end;
  end else begin
    mainTypCreated := nil;
  end;
  exit(init);
end;
procedure TAnalyzer.GetAdicVarDeclar(var varTyp: TEleTypeDec; out aditVar: TAdicVarDec;
          out mainTypCreated: TEleTypeDec);
{Verify aditional settings for var declarations, after the type definition. These settings
can be:
 ABSOLUTE <literal address or variable/constant identifier>
 REGISTER/REGISTERA/REGISTERX/REGISTERY

All aditional settings are returned in "aditVar".
IMPORTANT: "varTyp" can be destroyed when is an unspecified size array ( like []byte ) and
an initialization is provided.
If aditional type are created for the init value, "typeCreated" specifies the number of
types created.
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
  tokL: String;
  ele: TxpElement;
  xcon: TEleConsDec;
  consTyp: TEleTypeDec;
  nItems: Int64;
  consIni: TEleExpress;
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
        aditVar.absAddr := xcon.value^.ValInt; { TODO : Faltaría verificar que "xcon" sea de tipo numérico }
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
    if not varTyp.IsByteSize then begin
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
    consIni := GetConstValue(varTyp, mainTypCreated);  //Leemos como constante
    if HayError then exit;
    consTyp := consIni.Typ;
    //Ya se tiene el valor constante para inicializar variable.
    if aditVar.hasAdic in [decRegis, decRegisA, decRegisX, decRegisY] then begin
      GenError('Cannot initialize register variables.');
      exit;
    end;
  end else begin
    //No hay asignación inicial.
    aditVar.hasInit := false;
  end;
  //Validate initialization for dynamic arrays.
  if (varTyp.catType = tctArray) then begin
    if varTyp.isDynam then begin
      //Dynamic array
      if not aditVar.hasInit then begin
        //Es un arreglo dinámico. Debió inicializarse.
        GenError(ER_EQU_EXPECTD);
        exit;
      end;
      //Has initialization. Validates.
      if consTyp.catType <> tctArray then begin
        GenError('Expected an array.');
        exit;
      end;
      //Here we assure "varTyp" and "consTyp" are both arrays.
      //Validation for item types.
      if varTyp.itmType <> consTyp.itmType then begin
        //GenError('Item type doesn''t match for initialize array.');
        GenError('Cannot initialize. Expected array of "%s". Got array of "%s".',
                 [varTyp.itmType.name, consTyp.itmType.name]);
        exit;
      end;
      //Both are arrays of the same item type.
//      TreeElems.DeleteTypeNode(varTyp);  //We don't need this type *** Genera error en la síntesis si se elimina.
      varTyp := consTyp;  //Use the same array type declaration.
      aditVar.constDec := consIni;
      exit;
    end;
    if aditVar.hasInit then begin
      nItems := consTyp.consNitm.value^.ValInt;
      //Validation for category
      if consTyp.catType <> tctArray then begin
        GenError('Expected an array.');
        exit;
      end;
      //both are arrays. Validation for item types.
      if varTyp.itmType <> consTyp.itmType then begin
        GenError('Item type doesn''t match for initialize array.');
        exit;
      end;
      //Validation for size. Must have the same size to simplify creating and calling new types.
      if varTyp.nItems < nItems then begin
        GenError('Too many items to initialize array.');
      end else if varTyp.nItems > nItems then begin
        GenError('Too few items to initialize array.');
      end ;
      //Validate type compatibility
      //First validation
      if consTyp <> varTyp then begin
        GenError('Expected type "%s". Got "%s".', [varTyp.name, consTyp.name]);
        exit;
      end;
      //Assign constant init value.
      aditVar.constDec := consIni;
    end;
  end else begin  //No array
    if aditVar.hasInit then begin
      if consTyp <> varTyp then begin
        GenError('Cannot initialize. Expected type "%s". Got "%s".', [varTyp.name, consTyp.name]);
        exit;
      end;
      //Assign constant init value.
      aditVar.constDec := consIni;
    end;
  end;
  {Ya se validó la pertinencia de la inicialización y ya se tiene el operando de
  inicialización en "consIni". Ahora toca validar la compatibilidad de los tipos.}
  //Por ahora solo se permite inicializar arreglos.
  if aditVar.hasInit then begin
    if (varTyp.catType = tctArray) then begin
    end else begin
    end;
  end;
end;
procedure TAnalyzer.ReadProcHeader(out procName: String; out retType: TEleTypeDec;
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
    typ, typeCreated: TEleTypeDec;
    itemList: TStringDynArray;
    srcPosArray: TSrcPosArray;
    i, curSize, n: Integer;
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
        if not getListOfIdent(itemList, srcPosArray) then begin
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
        GetAdicVarDeclar(typ, adicVarDec, typeCreated);
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
function TAnalyzer.GetTypeDeclar(out decStyle: TTypDeclarStyle;
                                 out TypeCreated: boolean;
                                 location: TTypeLocat = tlCurrNode): TEleTypeDec;
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

"TypeCreated" indicates when a new Type instance was created in the AST (The main Type
element). In general, the extraction of type definition can create several types, like in:
VAR a: object x:array[3] of byte; end;
However, there are always a main type (like the object in the example).
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
      consDec := AddConstDeclarByte('length', 0);
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
      consDec := AddConstDeclarByte('length', 0);
      arrTyp.isDynam := true;
      arrTyp.consNitm := consDec;  //Update reference to the size.
    end else begin
      {Note this section of code is similar to TAnalyzer.GetConstValue().}
      //Creates constant element "length" to returns array size
      consDec := AddConsDecAndOpen('length', typNull, GetSrcPos);  //No type defined here.
      if HayError then exit;  //Can be duplicated
      //Debe seguir una expresión constante, que no genere código
      sizExp := GetExpression(0);  //Add expression to current node os AST
      if HayError then exit;
      consDec.typ := sizExp.Typ;
      if sizExp.Sto = stConst then begin
        consDec.value := @sizExp.value;
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
          AnalyzeProcDeclar;
          if HayError then exit;
        end else begin
          //Debe ser un campo
          //Esta parte es como la de la declaración de variables pero simplificada.
          //Procesa variables a,b,c : int;
          if not getListOfIdent(varNames, srcPosArray) then begin  //precisa el error
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
  procedure PointerDeclaration(const srcpos: TSrcPos; out refTyp: TEleTypeDec);
  {Procesa la declaración de un tipo puntero y devuelve el tipo, ya creado para su
  validación.
  Se asume que ya se ha identificado el inicio de la declaración de un puntero,
  sea en su forma larga: "POINTER TO BYTE" o en su forma corta: ^BYTE }
  var
    refDecStyle: TTypDeclarStyle;
    refTypCreated: boolean;
  begin
    if token = '^' then begin
      //Declaración corta
      Next;  //Toma '^'
    end else begin
      //Declaración larga: POINTER TO <tipo>
      Next; //Toma 'POINTER'. Se asume que ya se identificó.
      ProcComments;
      if not CaptureStr('TO') then exit;
    end;
//    reftyp := TreeElems.FindType(token); //Busca elemento
    reftyp := GetTypeDeclar(refDecStyle, refTypCreated); //Recursive definition
    if refTypCreated then begin
      //Por ahora solo permitiremos identificadores de tipos
      GenError('Too complex type definition.');
      exit;
    end;
  end;
var
  typName, tokL: String;
  typ, itemTyp, refType: TEleTypeDec;
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
    typ := OpenTypeDec(srcPos, '', -1, tctArray, t_object, location);
      if HayError then exit;  //Can be duplicated
      TypeCreated := true;
      ArrayDeclaration(srcpos, itemTyp);
      if HayError then exit(nil);     //Sale para ver otros errores
      typ.itmType := itemTyp; //Item type
      callDefineArray(typ);   //Define operations to array
      //Update name *** ¿Podría estar duplicado?
      typ.name := GenArrayTypeName(itemTyp.name, typ.consNitm.value^.ValInt);
    CloseTypeDec(typ);  //Close type
  end else if {(tokL = 'pointer') or }(token = '^') then begin
    //Es declaración de puntero
    decStyle := ttdDeclar;  //Es declaración elaborada
    typ := OpenTypeDec(srcPos, '', -1, tctPointer, t_object, location);
      if HayError then exit;  //Can be duplicated
      TypeCreated := true;

      PointerDeclaration(srcpos, refType);
      if HayError then exit(nil);     //Sale para ver otros errores
      typ.ptrType := refType;
      callDefinePointer(typ);
      typ.name := GenPointerTypeName(refType.name);
    CloseTypeDec(typ);  //Close type
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
function TAnalyzer.GetTypeDeclarSimple(): TEleTypeDec;
{Similar a GetTypeDeclar(), pero solo permite la referencia a tipos simples. No permite la
declaración de nuevos tipos, como: ARRAY OF ...}
var
  decStyle: TTypDeclarStyle;
  TypeCreated: boolean;
begin
  if TreeElems.curCodCont=nil then TreeElems.curCodCont:=typByte; {Ver comentario de AnalyzeVarDeclar()}
  Result := GetTypeDeclar(decStyle, TypeCreated);  //lee tipo
  if HayError then exit;
  if TypeCreated then begin
    //No se permiten declaraciones elaboradas aquí.
    GenError('Only simple types expected here.');
    Result.Destroy;  //Destroy the new type created
    Result := nil;
  end;
end;
function TAnalyzer.GetTypeDeclarColon(out decTyp: TEleTypeDec;
                                 out decStyle: TTypDeclarStyle;
                                 out TypeCreated: boolean): boolean;
{Obtiene el tipo declarado después de dos puntos en "decTyp". Si no encuentra dos puntos,
devuelve NIL en "decTyp". Si se produce algún error, devuelve FALSE.
"TypeCreated" indica que el tipo devuelto (el tipo principal de la declaración y no otros
tipos que pudieron crearse también) ha generado la creación de un nuevo tipo en el AST.
El tipo creado, cuando se crea alguno, se coloca en el nodo padre antes del nodo actual.
}
begin
  if token = ':' then begin  //Check if type exists
    //Debe seguir, el tipo
    Next;  //Takes ":"
    ProcComments;
    decTyp := GetTypeDeclar(decStyle, TypeCreated, tlParentNode);  //Can create a Type element at this level.
    if HayError then exit(false);
    ProcComments;
    if TypeCreated then begin
      //Se creó al menos un tipo.
      {Por completar. *****
      Aquí podríamos verificar si los tipos creados, ya existen, para ir eliminándolos,
      aprovechando que no tienen referencias aún.
      Como pueden ser varios, se puede ubicar tomando el índice del nodo actual, antes de
      llamar a GetTypeDeclar() y ver cuántos nodos se han creado.
      Para arreglos se puede usar el nombre como identificador.
      }
    end;
  end else begin
    //No problem. Type is optional.
    decTyp := nil;
  end;
  exit(true);
end;
procedure TAnalyzer.AnalyzeTypeDeclar(elemLocat: TxpEleLocation);
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
  if TreeElems.curCodCont=nil then TreeElems.curCodCont:=typByte; {Ver comentario de AnalyzeVarDeclar()}
  etyp := GetTypeDeclar(decStyle, typeCreated);
  if HayError then exit;
  //Analiza la declaración
  if decStyle = ttdDirect then begin
    //Es un tipo referenciado directamente. Algo como TYPE fool = byte;
    //Para este caso, nosotros creamos un tipo nuevo, en  modo copia.
    reftyp := etyp;  //Referencia al tipo { TODO : ¿Y si el tipo es ya una copia? }
    //etyp := CreateEleType(typName, srcPos, reftyp.size, reftyp.catType, reftyp.group);
    etyp := TreeElems.AddTypeDecAndOpen(srcPos, typName, reftyp.size, reftyp.catType, reftyp.group);
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
procedure TAnalyzer.AnalyzeConstDeclar;
  procedure GetIntialization(consTyp: TEleTypeDec; out consIni: TEleExpress;
                             out mainTypCreated: TEleTypeDec);
  {Get the constant intiialization for the declaration.}
  begin
    if token = '=' then begin
      Next;  //Pass to the next.
      consIni := GetConstValue(consTyp, mainTypCreated);
    end else begin
      GenError(ER_EQU_COM_EXP);
      exit;
    end;
  end;
var
  consNames: array of string;  //nombre de variables
  srcPosArray: TSrcPosArray;
  consDec: TEleConsDec;
  consIni: TEleExpress;
  consTyp, mainTypCreated: TEleTypeDec;
  decStyle: TTypDeclarStyle;
  consTypCreated: boolean;
begin
  //Procesa lista de constantes a,b,cons ;
  if not getListOfIdent(consNames, srcPosArray) then begin
    GenError(ER_IDE_CON_EXP);
    exit;
  end;
  if length(consNames)=1 then begin  //Pascal behaviour. Only one constant.
    //Create constant declaration
    consDec := AddConsDecAndOpen(consNames[0], typNull, srcPosArray[0]);  //Open constant dec
    if HayError then exit;  //Can be duplicated
    //Puede seguir "=" o ":" <identificador de tipo>
    if not GetTypeDeclarColon(consTyp, decStyle, consTypCreated) then exit;
    //Si no había tipo; "consTyp" será NIL.
    GetIntialization(consTyp, consIni, mainTypCreated);
    if HayError then begin
      exit;
    end;
    if consTyp<>nil then AddCallerToFromCurr(consTyp);  //Add caller
    consDec.typ := consIni.Typ;   //Update constant type.
    if consIni.Sto = stConst then begin
      //A simple value. We can initialize the constant.
      consDec.value := @consIni.value;
      consDec.evaluated := consIni.evaluated;
    end;
    //Other types (no otConst) will be evaluated later.

    TreeElems.CloseElement;  //Close constant declaration.

  end else begin
    GenError('Only one constant can be initialized.');
    exit;
  end;
  if not CaptureDelExpres then exit;
  ProcComments;
  //Puede salir con error
end;
procedure TAnalyzer.AnalyzeVarDeclar;
{Compila la declaración de variables en el nodo actual.
"IsInterface", indica el valor que se pondrá al as variables, en la bandera "IsInterface" }
  procedure UpdateVarDec(xvar: TEleVarDec; varTyp: TEleTypeDec;
                      const adicVarDec: TAdicVarDec);
  begin
    xvar.typ := varTyp;   //Update type
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
  end;
  procedure GetAdicVarDeclar2(varTyp: TEleTypeDec; out aditVar: TAdicVarDec;
            out mainTypCreated: TEleTypeDec);
  {Versión de GetAdicVarDeclar() que no permite declaracionea adicionales.}
  begin
    GetAdicVarDeclar(varTyp, aditVar, mainTypCreated);  //Could create new types
    if HayError then exit;
    {Like Free Pascal, we don't allow REGISTER, ABSOLUTE or initialization, for more
    than one variable.}
    if aditVar.hasAdic in [decRegis, decRegisA, decRegisX, decRegisY] then begin
      GenError('Cannot define REGISTER for more than one variable');
      exit;
    end;
    if aditVar.hasAdic = decAbsol then begin
      GenError('Cannot define ABSOLUTE for more than one variable');
      exit;
    end;
    if aditVar.hasInit then begin
      GenError('Cannot initialize more than one variable');
      exit;
    end;
  end;

var
  varNames: array of string;  //nombre de variables
  srcPosArray: TSrcPosArray;
  i: Integer;
  varTyp, mainTypCreated: TEleTypeDec;
  adicVarDec: TAdicVarDec;
  varDec: TEleVarDec;
  decStyle: TTypDeclarStyle;
  varTypCreated: boolean;
begin
  {La primera sección de este código es similar a TAnalyzer.AnalyzeConstDeclar().
  Se ha tratado de mantener la uniformidad para reutilizar el algoritmo hasta donde
  lo permitan las similitudes.}
  //Procesa lista de variables a,b,c : int;
  if not getListOfIdent(varNames, srcPosArray) then begin
    GenError(ER_EXP_VAR_IDE);
    exit;
  end;
  if length(varNames)=1 then begin //Unique variable declaration
    varDec := AddVarDecAndOpen(varNames[0], typNull, srcPosArray[0]);  //We don`t have the type here
    if HayError then exit;        //Sale para ver otros errores
    //Must follow ":".
    if not GetTypeDeclarColon(varTyp, decStyle, varTypCreated) then exit;
    if varTyp = nil then begin
      GenError(ER_SEM_COM_EXP);
      exit;
    end;
    //Lee información adicional de la declaración (ABSOLUTE, REGISTER, initial value)
    GetAdicVarDeclar(varTyp, adicVarDec, mainTypCreated);  //Could create new types
    if HayError then begin
      //Cannot destroy, directly, the type created, because the creation has added variables to the AST.
      //if varTypCreated then varTyp.Destroy;
      exit;
    end;
    AddCallerToFromCurr(varTyp);
    UpdateVarDec(varDec, varTyp, adicVarDec);
    TreeElems.CloseElement;  //Close variable
  end else begin  //Multiple variables declaration
    //Add Variable declarations element to the AST
    for i := 0 to high(varNames) do begin
      varDec := AddVarDecAndOpen(varNames[i], typNull, srcPosArray[i]);  //We don`t have the type here
      if HayError then break;        //Sale para ver otros errores
      //Updates "varTyp" and "adicVarDec" and add callers.
      if i=0 then begin  //Lee solo para la primera variable
        //Must follow ":".
        if not GetTypeDeclarColon(varTyp, decStyle, varTypCreated) then exit;
        if varTyp = nil then begin
          GenError(ER_SEM_COM_EXP);
          exit;
        end;
        GetAdicVarDeclar2(varTyp, adicVarDec, mainTypCreated);  //Could create new types
        if HayError then exit;
      end;
      ProcComments;
      {Aquí, finalmente, se tiene el tipo completo en su estructura porque, si había un
      arreglo no dimensionado, como:  foo []CHAR = 'HELLO'; ya se verificó la
      inicialización.}
      AddCallerToFromCurr(varTyp);
      UpdateVarDec(varDec, varTyp, adicVarDec);
      TreeElems.CloseElement;  //Close variable
    end;
  end;
  if not CaptureDelExpres then exit;
  ProcComments;
  //Puede salir con error.
end;
procedure TAnalyzer.AnalyzeProcDeclar;
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
        AnalyzeVarDeclar;
        if HayError then exit;;
      end;
    end else if tokL = 'const' then begin
      Next;    //lo toma
      while not StartOfSection and (lowercase(token) <>'begin') do begin
        AnalyzeConstDeclar;
        if HayError then exit;;
      end;
//    end else if lowercase(token) = 'procedure' then begin
//      Next;    //lo toma
//      AnalyzeProcDeclar;
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
  bod := TreeElems.AddBodyAndOpen(GetSrcPos);  //Open Body node
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
procedure TAnalyzer.AnalyzeInlineDeclar(elemLocat: TxpEleLocation);
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
//        AnalyzeVarDeclar;
//        if HayError then exit;;
//      end;
//    end else if tokL = 'const' then begin
//      Next;    //lo toma
//      while not StartOfSection and (tokL <>'begin') do begin
//        AnalyzeConstDeclar;
//        if HayError then exit;;
//      end;
////    end else if tokL = 'procedure' then begin
////      Next;    //lo toma
////      AnalyzeProcDeclar;
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
function TAnalyzer.VerifyEND: boolean;
{Compila la parte final de la estructura, que en el modo PicPas, debe ser el
 delimitador END. Si encuentra error, devuelve FALSE.}
begin
  Result := true;   //por defecto
  if mode = modPicPas then begin
    //En modo PicPas, debe haber un delimitador de bloque
    if not CaptureStr('END') then exit(false);
  end;
end;
function TAnalyzer.GetCondition(out ex: TEleExpress; ConstBool: boolean=false): boolean;
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
    AddExpressionConstBool('else', true, GetSrcPos);
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
function TAnalyzer.OpenContextFrom(filePath: string): boolean;
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
function TAnalyzer.AnalyzeStructBody: boolean;
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
procedure TAnalyzer.AnalyzeIF;
{Compile an IF structure.}
var
  ex: TEleExpress;
begin
  if not GetCondition(ex) then exit;
  if not CaptureStr('THEN') then exit; //toma "then"
  //Compile the THEN part.
  TreeElems.AddElementBlockAndOpen(GetSrcPos);  //Open block
  if not AnalyzeStructBody then exit;
  TreeElems.CloseElement;  //Close block
  //Compila los ELSIF que pudieran haber
  while UpCase(token) = 'ELSIF' do begin
    Next;   //toma "elsif"
    if not GetCondition(ex) then exit;
    if not CaptureStr('THEN') then exit;  //toma "then"
    //Compila el cuerpo pero sin código
    TreeElems.AddElementBlockAndOpen(GetSrcPos);  //Open block
    if not AnalyzeStructBody then exit;
    TreeElems.CloseElement;  //Close block
  end;
  //Compila el ELSE final, si existe.
  if Upcase(token) = 'ELSE' then begin
    //Hay bloque ELSE, pero no se ejecutará nunca
    Next;   //Takes  "else"
    //An "else" is similar to a ELSIF true
    GetCondition(ex, true);  //Create condiiton block with a fixed TRUE constant.
    TreeElems.AddElementBlockAndOpen(GetSrcPos);  //Open block
    if not AnalyzeStructBody then exit;
    TreeElems.CloseElement;  //Close block
    if not VerifyEND then exit;
  end else begin
    VerifyEND;
  end;
end;
procedure TAnalyzer.AnalyzeWHILE;
{Compile a WHILE structure.}
var
  ex: TEleExpress;
begin
  if not GetCondition(ex) then exit;  //Condición
  if not CaptureStr('DO') then exit;  //toma "do"
  //Aquí debe estar el cuerpo del "while"
  TreeElems.AddElementBlockAndOpen(GetSrcPos);  //Open block
  if not AnalyzeStructBody then exit;
  TreeElems.CloseElement;  //Close block
  if not VerifyEND then exit;
end;
procedure TAnalyzer.AnalyzeREPEAT;
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
procedure TAnalyzer.AnalyzeFOR;
{Compila uan extructura FOR}
  procedure CreateCondition(idx: TEleExpress);
  {Create a condition getting de expression of the <TO ... DO> section.}
  var
    elem: TEleCondit;
    Op2, _lequ, xvar: TEleExpress;
    funSet: TEleFunBase;
  begin
    //Create and open condition
    elem := TEleCondit.Create;
    elem.name := 'cond';
    elem.srcDec := GetSrcPos;
    TreeElems.AddElementAndOpen(elem);

    //Create the _lequ() expression: i<n.
    _lequ := CreateExpression('_lequ', typNull, otFunct, GetSrcPos);
    funSet := MethodFromBinOperator(idx.Typ, '<=', idx.Typ);
    if funSet = nil then begin   //Operator not found
      GenError('Undefined operation: %s %s %s', [idx.Typ.name, '<', idx.Typ.name]);
      exit;
    end;
    _lequ.rfun := funSet;
    //Add the new expression
    TreeElems.AddElementAndOpen(_lequ);
    //Create a new variable for the expression "i<..."
    xvar := CreateExpression(idx.name, idx.Typ, otVariab, GetSrcPos);
    xvar.SetVariab(idx.rvar);
    TreeElems.AddElement(xvar);

    Op2 := GetExpression(0);

    TreeElems.CloseElement;      //Close _lequ()

    TreeElems.CloseElement;      //Close condition
    if HayError then exit;
    if Op2.Typ <> idx.Typ then begin
      GenError('Expected expression of type %s', [idx.Typ.name], Op2.srcDec);
      exit;
    end;
    SkipWhites;
  end;
  procedure CReateInc(idx: TEleExpress);
  {Add a sentence Inc() applied to the index variable "idx".}
  var
    Op1, xvar: TEleExpress;
  begin
    //Open sentence
    TreeElems.AddElementSentAndOpen(GetSrcPos, sntProcCal);
    //Use reference eleFunInc" to access directly to function Inc().
    Op1 := CreateExpression(eleFunInc.name, eleFunInc.retType, otFunct, GetSrcPos);
    Op1.rfun := eleFunInc;
    TreeElems.AddElementAndOpen(Op1);  //Open Inc()
    //Create a new variable for the expression "i<..."
    xvar := CreateExpression(idx.name, idx.Typ, otVariab, GetSrcPos);
    xvar.SetVariab(idx.rvar);
    TreeElems.AddElement(xvar);
    TreeElems.CloseElement;  //Close Inc()
    //Close Sentence
    TreeElems.CloseElement;
  end;
var
  Op1, idx: TEleExpress;
begin
  {Get the first asignment: i:=0; }
  TreeElems.AddElementBlockAndOpen(GetSrcPos);  //Open block for first assigment.
  Op1 := GetExpression(0);
  TreeElems.CloseElement;  //Close block
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
  CreateCondition(idx);
  if not CaptureStr('DO') then exit;  //toma "do"
  //Aquí debe estar el cuerpo del "for"
  TreeElems.AddElementBlockAndOpen(GetSrcPos);  //Open block
  if not AnalyzeStructBody then exit;
  //Agrega instrucción de incremento.
  CreateInc(idx);
  //Close block
  TreeElems.CloseElement;
  if not VerifyEND then exit;
end;
procedure TAnalyzer.AnalyzeEXIT(exitSent: TEleSentence);
  function GetExitExpression(out oper: TEleExpress): boolean;
  {Get the argument of the Exit instruction. If not an expression follows, returns FALSE}
  begin
    if (tokType=tkBlkDelim) or (token=';') then begin
      //Not expression specified
      exit(false);
    end else begin
      //Must follow an expression
      oper := GetExpression(0);
    end;
    exit(true);
  end;
var
  parentNod: TxpElement;
  func: TEleFun;
  oper: TEleExpress;
  prog: TEleProg;
begin
  ProcComments;
  //Detect if an expression must follow
  parentNod := TreeElems.curCodCont.Parent;  //Se supone que nunca debería fallar
  //posExit := GetSrcPos;
  if parentNod.idClass = eleProg then begin
    prog := TEleProg(parentNod);
    //It's the main body
    if GetExitExpression(oper) then begin
      GenError('Main program cannot return a value.');
    end;
    //Lleva el registro de las llamadas a exit()
    prog.RegisterExitCall(exitSent);
  end else if parentNod.idClass = eleFunc then begin
    func := TEleFun(parentNod);
    if func.retType = typNull then begin
      //Is Procedure
        if GetExitExpression(oper) then begin
          GenError('Procedures doesn''t return a value.');
        end;
    end else begin
      //Is Function
      if GetExitExpression(oper) then begin
        //The expected. Check type match.
        if HayError then exit;
        if oper.Typ <> func.retType then begin
          GenError('Expected a "%s" expression.', [func.retType.name]);
        end;
        //Detect dependence when returning value
        if oper.Typ.OnRequireWR<>nil then oper.Typ.OnRequireWR;
      end else begin
        GenWarn('Expected return value.');
      end;
    end;
    //Lleva el registro de las llamadas a exit()
    func.RegisterExitCall(exitSent);
  end;
end;
procedure TAnalyzer.MoveInternalTypes(node: TxpElement;
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
procedure TAnalyzer.AnalyzeSentence;
{Analyze one Pascal sentence. One sentence can be:
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
  snt, exitSent: TEleSentence;
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
    end else if tokUp = 'EXIT' then begin  //EXIT instruction.
      exitSent := TreeElems.AddElementSentAndOpen(GetSrcPos, sntExit);  //Open sentence
      Next;
      AnalyzeEXIT(exitSent);
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
      declarSec := TreeElems.curCodCont.Parent;  //Declaration section.
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
procedure TAnalyzer.AnalyzeCurBlock;
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
function TAnalyzer.GetUnitDeclaration: boolean;
{Indica si el archivo del contexto actual, es una unidad. Debe llamarse al inico de la
exploración del archivo.}
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
procedure TAnalyzer.AnalyzeUsesDeclaration;
{Compila la unidad indicada.}
var
  uni: TEleUnit;
  uPath: String;
  uName: String;
  p: TContextState;
  ufound: Boolean;
begin
  if lowercase(token) = 'uses' then begin
    Next;  //Go to the name
    //Takes each unit
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
        uni.srcFile := uPath;   //Guarda el archivo fuente
      end else begin
         //No lo encontró, busca en las carpetas de librerías.
         ufound := false;
         for upath in unitPaths do begin
           if OpenContextFrom(uPath + uName) then begin
             uni.srcFile := uPath + uName;  //Guarda el archivo fuente
             ufound := true;
             break;
           end;
         end;
         if not ufound then begin
           GenError(ER_FIL_NOFOUND, [uName]);
           exit;
         end;
      end;
      //Aquí ya se puede realizar otra exploración, como si fuera el archivo principal
      DoAnalyzeUnit(uni);
      SetCtxState(p); //*** No debería ser necesario salvar y restaurar estados si se usa "autoReturn" como se hace en TParserDirecBase.ProcINCLUDE().
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
procedure TAnalyzer.DoAnalyzeUnit(uni: TxpElement);
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
  AnalyzeUsesDeclaration;
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
        AnalyzeVarDeclar;  //marca como "IsInterface"
        if HayError then exit;;
      end;
    end else if tokL = 'type' then begin
      Next;    //lo toma
      while not StartOfSection and (lowercase(token) <>'implementation') do begin
        AnalyzeTypeDeclar(locInterface);
        if HayError then exit;
      end;
    end else if tokL = 'const' then begin
      Next;    //lo toma
      while not StartOfSection and (lowercase(token)<>'implementation') do begin
        AnalyzeConstDeclar;
        if HayError then exit;;
      end;
    end else if tokL = 'procedure' then begin
      Next;    //lo toma
      AnalyzeProcDeclar;
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
        AnalyzeVarDeclar;
        if HayError then exit;;
      end;
    end else if tokL = 'const' then begin
      Next;    //lo toma
      while not StartOfSection and (tokL <>'end') do begin
        AnalyzeConstDeclar;
        if HayError then exit;;
      end;
    end else if tokL = 'procedure' then begin
      Next;    //lo toma
      AnalyzeProcDeclar;  //Compila en IMPLEMENTATION
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
end;
procedure TAnalyzer.DoAnalyzeProgram;
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
  if HayError then exit;  //AnalyzeUsesDeclaration, va a limpiar "HayError"
  AnalyzeUsesDeclaration;
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
        AnalyzeVarDeclar;
        if HayError then exit;
      end;
    end else if tokL = 'type' then begin
      Next;    //lo toma
      while not StartOfSection and (lowercase(token) <>'begin') do begin
        AnalyzeTypeDeclar(locMain);
        if HayError then exit;
      end;
    end else if tokL = 'const' then begin
      Next;    //lo toma
      while not StartOfSection and (lowercase(token) <>'begin') do begin
        AnalyzeConstDeclar;
        if HayError then exit;
      end;
    end else if tokL = 'procedure' then begin
      Next;    //lo toma
      AnalyzeProcDeclar;
      if HayError then exit;
    end else if tokL = 'inline' then begin
      Next;    //lo toma
      AnalyzeInlineDeclar(locMain);
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
  bod := TreeElems.AddBodyAndOpen(GetSrcPos);  //Abre nodo Body
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
procedure TAnalyzer.DoAnalyze;
{Performs the Analysis (Lexical, syntactic and semantic).
Input: The current context.
Output: The AST.}
begin
  if IsUnit then begin
    DoAnalyzeUnit(TreeElems.main);
  end else begin
    DoAnalyzeProgram;    //puede dar error
  end;
end;

end.

