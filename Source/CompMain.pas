unit CompMain;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Types, CompBase, XpresElementsPIC, Globales,
  CompOperands, XpresTypesPIC, XpresBas;
type

  { TCompMain }
  TCompMain = class(TCompilerBase)
  protected
    function StartOfSection: boolean;
    procedure CompileLastEnd;
    procedure AssignRAMtoVar(xvar: TxpEleVar; shared: boolean = false);
    procedure CreateLocalVarsAndPars;
    procedure CreateGlobalVars;
  protected  //Elements processing
    procedure GetAdicVarDeclar(xType: TxpEleType; out aditVar: TAdicVarDec);
    procedure ReadProcHeader(out procName: String; out retType: TxpEleType; out
      srcPos: TSrcPos; out pars: TxpParFuncArray; out IsInterrupt,
  IsForward: Boolean);
    procedure ReadInlineHeader(out procName: String; out retType: TxpEleType;
      out srcPos: TSrcPos; out pars: TxpParInlinArray);
    procedure CompileVarDeclar;
    procedure CompileGlobalConstDeclar;
    procedure CompileTypeDeclar(elemLocat: TxpEleLocation; typName: string = ''
      );
    function GetTypeDeclar(out decStyle: TTypDeclarStyle; out
      TypeCreated: boolean): TxpEleType;
    function GetTypeDeclarSimple(): TxpEleType;

    function VerifyEND: boolean;
    function GetExpressionBool: boolean;
    procedure Tree_AddElement(elem: TxpElement);
    function OpenContextFrom(filePath: string): boolean;
    function CompileStructBody(GenCode: boolean): boolean;
    function CompileConditionalBody: boolean;
    function CompileNoConditionBody(GenCode: boolean): boolean;
    procedure CompileInstruction;
    procedure CompileInstructionDummy;
    procedure CompileCurBlock;
    procedure CompileCurBlockDummy;
    function IsUnit: boolean;
    procedure CompileInlineBody(fun: TxpEleInlin);
    procedure CompileProcDeclar;
    procedure CompileInlineDeclar(elemLocat: TxpEleLocation);
    procedure CompileUnit(uni: TxpElement);
    procedure CompileUsesDeclaration;
    procedure CompileProgram;
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
  WA_UNUSED_VAR_
  : string;
procedure SetLanguage;
begin
  CompBase.SetLanguage;
  {$I ..\language\tra_CompMain.pas}
end;
function TCompMain.StartOfSection: boolean;
begin
  Result := (cIn.tokL ='var') or (cIn.tokL ='const') or
            (cIn.tokL ='type') or (cIn.tokL ='procedure') or (cIn.tokL ='inline');
end;
procedure TCompMain.CompileLastEnd;
{Compila la parte de final de un programa o una unidad}
begin
  if cIn.Eof then begin
    GenError(ER_EOF_END_EXP);
    exit;       //sale
  end;
  if cIn.tokL <> 'end' then begin  //verifica si termina el programa
    if cIn.tokL = 'else' then begin
      //Precisa un poco más en el error
      GenError(ER_ELS_UNEXPEC);
      exit;       //sale
    end else begin
      GenError(ER_END_EXPECTE);
      exit;       //sale
    end;
  end;
  cIn.Next;   //coge "end"
  //Debería seguir el punto
  if not CaptureTok('.') then exit;
  //no debe haber más instrucciones
  ProcComments;
  if not cIn.Eof then begin
    GenError(ER_NOT_AFT_END);
    exit;       //sale
  end;
end;

//Elements processing
procedure TCompMain.GetAdicVarDeclar(xType: TxpEleType; out aditVar: TAdicVarDec) ;
{Verify aditional settings for var declarations, after the type definition. These settings
can be:
 ABSOLUTE <literal address or variable/constant identifier>
 REGISTER/REGISTERA/REGISTERX/REGISTERY

All aditional settings are returned in "aditVar".
IMPORTANT: "xType" can change when is an unspecified size array ( like []byte ) and an
initialization is provided. No new type elements are created or destroyed.
 . }
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
  xvar: TxpEleVar;
  n: integer;
  Op, OpInit: TOperand;
begin
  aditVar.srcDec   := cIn.PosAct;  //Posición de inicio de posibles parámetros adic.
  aditVar.hasAdic  := decNone;       //Bandera
  aditVar.hasInit  := false;
  aditVar.absVar   := nil;         //Por defecto
  if (cIn.tokL = 'absolute') or (cIn.tok = '@') then begin
    // Hay especificación de dirección absoluta ////
    aditVar.hasAdic := decAbsol;    //marca bandera
    cIn.Next;
    ProcComments;
    if cIn.tokType = tnNumber then begin
      if (cIn.tok[1]<>'$') and ((pos('e', cIn.tok)<>0) or (pos('E', cIn.tok)<>0)) then begin
        //La notación exponencial, no es válida.
        GenError(ER_INV_MEMADDR);
        exit;
      end;
      n := pos('.', cIn.tok);   //no debe fallar
      if n=0 then begin
        //Número entero sin parte decimal
        aditVar.absAddr := ReadAddres(cIn.tok);
        cIn.Next;  //Pasa con o sin error, porque esta rutina es "Pasa siempre."
      end else begin
        //Puede ser el formato <dirección>.<bit>, en un solo token, que es válido.
        GenError('Syntax error.');
        exit;
      end;
    end else if cIn.tokType = tnIdentif then begin
      //Puede ser variable
      GetOperandIdent(Op, opmGetter); //
      if HayError then exit;
      if Op.Sto <> stVariab then begin
        GenError(ER_EXP_VAR_IDE);
        cIn.Next;  //Pasa con o sin error, porque esta rutina es "Pasa siempre."
        exit;
      end;
      //Mapeado a variable. Notar que puede ser una variable temporal, si se usa: <var_byte>.0
      xvar := Op.rVar;
      if Op.rVarBase=nil then begin
        aditVar.absVar := Op.rVar;  //Guarda referencia
      end else begin
        {Es un caso como "<Variab.Base>.0", conviene devolver la referencia a <Variab.Base>,
        en lugar de a la variable "<Variable Base>.0", considerando que:
        * GetOperandIdent() usa <Variable Base>, para registrar la llamada.
        * Esta referencia se usará luego para ver variables no usadas en
          TCompiler.CompileLinkProgram().}
        aditVar.absVar := Op.rVarBase;  //Guarda referencia
      end;
      //Ya tiene la variable en "xvar".
      aditVar.absAddr := xvar.addr;  //debe ser absoluta
      if aditVar.absAddr = ADRR_ERROR then begin
        //No se puede obtener la dirección.
        GenError('Cannot locate variable at: %s', [xvar.name]);
  //      GenError('Internal Error: TxpEleVar.AbsAddr.');
        exit;
      end;
    end else begin   //error
      GenError(ER_NUM_ADD_EXP);
      cIn.Next;    //pasa siempre
      exit;
    end;
  end else if cIn.tokL = 'register' then begin
    // Es de tipo registro
    aditVar.hasAdic := decRegis;    //marca bandera
    cIn.Next;
    ProcComments;
  end else if cIn.tokL = 'registera' then begin
    // Es de tipo registro
    aditVar.hasAdic := decRegisA;    //marca bandera
    cIn.Next;
    ProcComments;
  end else if cIn.tokL = 'registerx' then begin
    // Es de tipo registro
    aditVar.hasAdic := decRegisX;    //marca bandera
    cIn.Next;
    ProcComments;
  end else if cIn.tokL = 'registery' then begin
    // Es de tipo registro
    aditVar.hasAdic := decRegisY;    //marca bandera
    cIn.Next;
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
  if cIn.tok = '=' then begin
    aditVar.hasInit := true;
    cIn.Next;   //lo toma
    ProcComments;
    //Aquí debe seguir el valor inicial
    OpInit := GetExpression(0);
    if HayError then exit;
    if OpInit.Sto <> stConst then begin
      GenError(ER_CON_EXP_EXP);
      exit;
    end;
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
  if (xType.catType = tctArray) and (xType.nItems = -1) and not aditVar.hasInit then begin
    //Es un arreglo dinámico. Debió inicializarse.
    GenError(ER_EQU_EXPECTD);
    exit;
  end;
  {Ya se validó la pertinencia de la inicialización y ya se tiene el operando de
  inicialización. Ahora toca validar la compatibilidad de los tipos.}
  //Por ahora solo se permite inicializar arreglos.
  if aditVar.hasInit then begin
    if (xType.catType = tctArray) then begin
      //Arrays have some particular behaviour
      if (xType.itmType = typChar) then begin
        //Special case for Char arrays. They can be initializaed with string.
        if OpInit.Typ = typChar then begin
          //Caso especial. Se puede considerar un array[1] of char
          OpInit.SetAsConst(xType);  //Set as constant array of char
          OpInit.StringToArrayOfChar( chr(OpInit.valInt) );  //Set ítems from string
//        end else if OpInit.Typ = typString then begin
//          OpInit.SetAsConst(xType);  //Set as constant array of char
//          OpInit.StringToArrayOfChar( OpInit.valStr );  //Set ítems from string
        end;
      end;
      //Validation for category
      if OpInit.Typ.catType <> tctArray then begin
        GenError('Expected an array.');
        exit;
      end;
      //Here we assure xType and OpInit are both arrays.
      //If dynamic, the size depends o initial value
      if xType.nItems= -1 then begin
        //Dynamic size
        xType.nItems := OpInit.nItems;   //Update array size
        //Type name needs to be updated too.
        xType.name := GenArrayTypeName(xType.itmType.name, OpInit.nItems);
        //NOTE that type name (and structrure) has changed.
      end;
      //Validation for item types.
      if xType.itmType <> OpInit.Typ.itmType then begin
        GenError('Item type doesn''t match for initialize array.');
        exit;
      end;
      //Validation for size.
      if xType.nItems < OpInit.nItems then begin
        GenError('Too many items to initialize array.');
        //exit
      end;
      //Validate type compatibility
      {We could be tempted to compare xType and OpInit.Typ here but this cannot be done,
      because OpInit.Typ will be (most of the times) always different from xType,
      because xType exists but it hasn't been added to the Syntax Tree, so OpInit.Typ
      had never the chance to find it, when creating.
      However, until here we have validated that xType and OpInit.Typ and both arrays,
      have the same item type and compatible sizes, so they are compatible to
      initialization.
      }
    end else begin
      //Other types are simple to validate
      if OpInit.Typ <> xType then begin
        GenError('Cannot initialize. Expected type "%s". Got "%s".', [xType.name, OpInit.Typ.name]);
        exit;
      end;
    end;
    //Assign constant value
    aditVar.iniVal := OpInit.Value;
  end;
end;
procedure TCompMain.ReadProcHeader(out procName: String; out retType: TxpEleType;
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
    typ: TxpEleType;
    itemList: TStringDynArray;
    srcPosArray: TSrcPosArray;
    i, curSize, n: Integer;
    adicVarDec: TAdicVarDec;
  begin
    cIn.SkipWhites;
    if EOBlock or EOExpres or (cIn.tok = ':') then begin
      //No tiene parámetros
      setlength(funPars, 0);
    end else begin
      //Debe haber parámetros. Prepara espacio para leer.
      curSize := BLOCK_SIZE;    //Tamaño inicial de bloque
      setlength(funPars, curSize);  //Tamaño inicial
      n := 0;
      //Inicia lectura
      if not CaptureTok('(') then exit;
      cin.SkipWhites;
      repeat
        //if cIn.tokL = 'register' then begin
        //  IsRegister := regA;  //Asumimos que es A
        //  cin.Next;
        //  cin.SkipWhites;
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
        GetAdicVarDeclar(typ, adicVarDec);
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
        if cIn.tok = ';' then begin
          cIn.Next;   //toma separador
          cIn.SkipWhites;
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
begin
  //Toma información de ubicación, al inicio del procedimiento
  cIn.SkipWhites;
  srcPos := cIn.ReadSrcPos;
  //Ahora debe haber un identificador
  if cIn.tokType <> tnIdentif then begin
    GenError(ER_IDEN_EXPECT);
    exit;
  end;
  //Lee nombre de función
  procName := cIn.tok;
  cIn.Next;  //lo toma
  //Captura los parámetros en "pars"
  ReadFunctionParams(pars);
  if HayError then exit;

  //Verifica si es función
  cIn.SkipWhites;
  if cIn.tok = ':' then begin
    cIn.Next;
    cIn.SkipWhites;
    //Es función
    retType := GetTypeDeclarSimple;  //lee tipo
    if HayError then exit;
  end else begin
    retType := typNull;
  end;
  if not CaptureTok(';') then exit;
  //Verifica si es INTERRUPT
  cIn.SkipWhites;
  if cIn.tokL = 'interrupt' then begin
    cIn.Next;
    IsInterrupt := true;
    if not CaptureTok(';') then exit;
  end else begin
    IsInterrupt := false;
  end;
  if cIn.tokL = 'forward' then begin
    cIn.Next;
    IsForward := true;
    if not CaptureTok(';') then exit;
  end else begin
    IsForward := false;
  end;
  ProcComments;  //Quita espacios. Puede salir con error
end;
procedure TCompMain.ReadInlineHeader(out procName: String; out retType: TxpEleType;
  out srcPos: TSrcPos; out pars: TxpParInlinArray);
{Hace el procesamiento del encabezado de la declaración de una función INLINE.
Devuelve la referencia al objeto TxpEleInline creado, en "fun".
Conviene separar el procesamiento del enzabezado, para poder usar esta rutina, también,
en el procesamiento de unidades.}
  procedure ReadInlineParams(out funPars: TxpParInlinArray);
  //Lee la declaración de parámetros de una función Inline.
  const
    BLOCK_SIZE = 10;  //Tamaño de bloque.
  var
    typ: TxpEleType;
    itemList: TStringDynArray;
    srcPosArray: TSrcPosArray;
    i, curSize, n: Integer;
    sto: TStoOperand;
  begin
    cIn.SkipWhites;
    SetLength(itemList, 0);
    if EOBlock or EOExpres or (cIn.tok = ':') then begin
      //no tiene parámetros
      //No tiene parámetros
      setlength(funPars, 0);
    end else begin
      //Debe haber parámetros. Prepara espacio para leer.
      curSize := BLOCK_SIZE;    //Tamaño inicial de bloque
      setlength(funPars, curSize);  //Tamaño inicial
      n := 0;
      //Inicia lectura
      if not CaptureTok('(') then exit;
      cin.SkipWhites;
      repeat
        if cIn.tokL = 'var' then begin
          cin.Next;
          cin.SkipWhites;
          sto := stVariab;
        end else if cIn.tokL = 'const' then begin
          cin.Next;
          cin.SkipWhites;
          sto := stConst;
        end else if cIn.tokL = 'expr' then begin
          cin.Next;
          cin.SkipWhites;
          sto := stExpres;
        end else begin
          GenError('Syntax error.');
          exit;
        end;
        //Lee lista de parámetros de tipo: a,b,c : byte;
        getListOfIdent(itemList, srcPosArray);
        if HayError then begin  //precisa el error
          GenError(ER_IDEN_EXPECT);
          exit;
        end;
        if not CaptureTok(':') then exit;
        typ := GetTypeDeclarSimple;  //lee tipo
        if HayError then exit;
        //Ya tiene los nombres y el tipo
        //Crea el parámetro como una variable local
        for i:= 0 to high(itemList) do begin
          funPars[n].name  := itemList[i];
          funPars[n].srcPos:= srcPosArray[i];
          funPars[n].typ   := typ;
          funPars[n].sto   := sto;
          //Prepara siguiente lectura
          inc(n);
          if n >= curSize then begin
            curSize += BLOCK_SIZE;   //Incrementa tamaño en bloque
            setlength(funPars, curSize);  //hace espacio en bloque
          end;
        end;
        //Busca delimitador
        if cIn.tok = ';' then begin
          cIn.Next;   //toma separador
          cIn.SkipWhites;
        end else begin
          //No sigue separador de parámetros,
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
begin
  //Toma información de ubicación, al inicio del procedimiento
  cIn.SkipWhites;
  srcPos := cIn.ReadSrcPos;
  //Ahora debe haber un identificador
  if cIn.tokType <> tnIdentif then begin
    GenError(ER_IDEN_EXPECT);
    exit;
  end;
  //Lee nombre de función
  procName := cIn.tok;
  cIn.Next;  //lo toma
  //Captura los parámetros en "pars"
  ReadInlineParams(pars);
  if HayError then exit;

  //Verifica si es función
  cIn.SkipWhites;
  if cIn.tok = ':' then begin
    cIn.Next;
    cIn.SkipWhites;
    //Es función
    retType := GetTypeDeclarSimple;  //lee tipo
    if HayError then exit;
  end else begin
    retType := typNull;
  end;
  if not CaptureTok(';') then exit;
  ProcComments;  //Quita espacios. Puede salir con error
end;
function TCompMain.GetTypeDeclar(out decStyle: TTypDeclarStyle;
                                       out TypeCreated: boolean): TxpEleType;
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
             la referencia a ese tipo, pero no lo agrega al arbol de sintaxis, porque
             espera validaciones posteriores.
             Por ejemplo, la siguiente declaración.
               a: Array[0..5] of char;
             Creará un tipo nuevo con la definición: "array[0..5] of char".
             El tipo nuevo devuelto tiene nombre vacío y debe ser actualizado luego.

Entre "decStyle" y el "catType" del tipo devuelto (Ver comentario de TxpCatType),
debería quedar completamente especificada la declaración del tipo.

"TypeCreated" indicates when a new Type instance was created (and will be destroyed or
added to the syntax Tree).
In general, creating new types is avoided when exists other type equivalent.

If some problems happens, Error is generated and the NIL value is returned.
}
  function ReadSizeInBrackets: integer;
  {Lee el tamaño de un arreglo especificado entre corchetes: []. Si no se
  especifica el tamaño. Devuelve -1.}
  begin
    //Declaración simplificada: []<tipo>
    cIn.Next; //Toma '['. Se asume que ya se identificó
    ProcComments;
    if cIn.tok = ']' then begin
      //Declaración corta de arreglo sin indicar el tamaño: []byte ;
      cIn.Next;
      exit(-1);  //Indica que es dinámico.
    end else begin
      //Se espera tamaño de arreglo
      //Debe seguir una expresión constante, que no genere código
      res := GetExpression(0);
      if HayError then exit(0);
      if res.Sto <> stConst then begin
        GenError(ER_CON_EXP_EXP);
        exit(0);
      end;
      //Límites físicos predefinidos
//      if res.Typ <> typByte then begin
//        GenError('Only byte type allowed here.');
//        exit(0);
//      end;
      if res.valInt<0 then begin
        GenError(ER_INV_ARR_SIZ);
        exit(0);
      end;
      if res.valInt>=$10000 then begin
        //Límite físico definido
        GenError(ER_ARR_SIZ_BIG);
        exit;
      end;
      //Ya se tiene el tamaño del arreglo, se sigue con la declaración.
      if not CaptureTok(']') then exit(0);  //toma "]"
      exit(res.valInt);  //Guarda número de elementos.
    end;
  end;
  function ArrayDeclaration(const srcpos: Tsrcpos): TxpEleType;
  {Procesa la declaración de un tipo arreglo y devuelve el tipo, ya creado para su
  validación. No agrega el tipo al árbol de sintaxis.
  Se asume que ya se ha identificado el inicio de la declaración de un arreglo,
  sea en su forma larga: "ARRAY[] OF BYTE" o en su forma corta: []BYTE }
  var
    itemDecStyle: TTypDeclarStyle;
    itemTyp, xtyp: TxpEleType;
    nElem: Integer;
    typName: String;
    itemTypeCreated: boolean;
  begin
    if cIn.tok = '[' then begin
      //Declaración corta
      nElem := ReadSizeInBrackets();  //Lee tamaño
      if HayError then exit;
    end else begin
      //Declaración larga: ARRAY[tamaño] OF <tipo>
      cIn.Next; //Toma 'ARRAY'. Se asume que ya se identificó.
      ProcComments;
      if cIn.tok = '[' then begin  //Tamaño espefificado o puede ser []
        nElem := ReadSizeInBrackets();  //Lee tamaño
        if HayError then exit;
      end else begin
        //No se especifica tamaño. Puede ser la forma corta: ARRAY OF
        nElem := -1;
      end;
      ProcComments;
      if not CaptureStr('of') then exit(nil);
    end;
    //Lee el tipo que sigue. Llamada RECURSIVA.
    itemTyp := GetTypeDeclar(itemDecStyle, itemTypeCreated);
    if HayError then exit(nil);
    if itemTypeCreated then begin
      //No support for nested declaration
      itemTyp.Destroy;
      GenError('Too complex type declaration');
      exit;
    end;
    if itemTyp.nItems = -1 then begin  //Sin tamaño
      //ARRAY[] OF ARRAY[] OF ... No se soporta por ahora.
      GenError('Dynamic array not allowed here.');
      exit(nil);
    end;
    //Se supone que ahora solo tenemos un tipo simple en "itemTyp".
    //Ya se tiene la información para crear un nuevo tipo array.
    if not TreeElems.ExistsArrayType(itemTyp, nElem, xtyp) then begin
      //There is not a similar type. We create a new type.
      typName := GenArrayTypeName(itemTyp.name, nElem);
      {typName could change later if not size specified for this array but it's initialized.
      Something like: []byte = (1,2,3);  }
      xtyp := CreateEleTypeArray(typName, srcPos, itemTyp, nElem);
      TypeCreated  := true;     //Activate flag
    end;
    Result := xtyp;
  end;
  function ObjectDeclaration(const srcpos: Tsrcpos): TxpEleType;
  {Procesa la declaración de un tipo objeto y devuelve el tipo, ya creado para su
  validación. No agrega el tipo al árbol de sintaxis.
  Se asume que ya se ha identificado el inicio de la declaración de un objeto.}
  var
    xtyp, atTyp: TxpEleType;
    i, offs: Integer;
    srcPosArray: TSrcPosArray;
    varNames: TStringDynArray;
    attr: TTypAttrib;
  begin
    //Declaración: OBJECT ... END
    cIn.Next; //Toma 'OBJECT'. Se asume que ya se identificó.
    ProcComments;
    {When declaring objets, we don't complicate in find previous declarations (rare).
    We alwalys create a new type}
    xtyp := CreateEleTypeObject(GenerateUniqName('Obj'), srcPos);
    TypeCreated  := true;     //Activate flag
    Result := xtyp;  //By default
    //Start reading attributes
    try
      offs := 0;   //Addres offset
      while not cIn.Eof and (cIn.tokl <> 'end') do begin
        if cIn.tokL = 'procedure' then begin
          //Es un método. ¿No se debería crear mejor dentro del nodo tipo?
          cIn.Next;    //lo toma
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
          if cIn.tok = ':' then begin
            //Debe seguir, el tipo de la variable
            cIn.Next;  //lo toma
            ProcComments;
            //Lee el tipo de la variable.
            atTyp := GetTypeDeclarSimple;   //Solo tipos simples por ahora
            if HayError then exit;
            //Reserva espacio para las variables
            for i := 0 to high(varNames) do begin
              attr := TTypAttrib.Create;
              attr.name:=varNames[i];
              attr.typ := atTyp;
              attr.offs := offs;
              xtyp.attribs.Add(attr);
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
    if not CaptureStr('end') then exit;
  end;
  function PointerDeclaration(const srcpos: TSrcPos): TxpEleType;
  {Procesa la declaración de un tipo puntero y devuelve el tipo, ya creado para su
  validación.
  Se asume que ya se ha identificado el inicio de la declaración de un puntero,
  sea en su forma larga: "POINTER TO BYTE" o en su forma corta: ^BYTE }
  var
    reftyp, xtyp: TxpEleType;
    typName: String;
  begin
    if cIn.tok = '^' then begin
      //Declaración corta
      cIn.Next;  //Toma '^'
    {end else begin
      //Declaración larga: POINTER TO <tipo>
      cIn.Next; //Toma 'POINTER'. Se asume que ya se identificó.
      ProcComments;
      if not CaptureStr('to') then exit;}
    end;
    //Por ahora solo permitiremos identificadores de tipos
    reftyp := FindSysEleType(cIn.tok); //Busca elemento
    if reftyp = nil then begin
      //No es un tipo del sistema, pero puede ser un tipo prdefinido
      reftyp := TreeElems.FindType(cIn.tok); //Busca elemento
//      reftyp := GetTypeDeclar(decStyle2, TypeCreated2); //Recursive definition
      if HayError then exit;
      if reftyp = nil then begin
        GenError('Expected a type identifier.');
        exit;
      end;
    end;
    //Verify type existence
    if not TreeElems.ExistsPointerType(reftyp, xtyp) then begin
      typName := GenPointerTypeName(refTyp.name);
      xtyp := CreateEleTypePtr(typName, srcPos, reftyp);
      if HayError then exit;
      TypeCreated  := true;     //Activate flag
    end;
    cIn.Next;   //take type name
    Result := xtyp;
  end;
var
  typName: String;
  typ: TxpEleType;
  ele: TxpElement;
  srcPos: TSrcPos;
begin
  Result := nil;
  TypeCreated := false;
  ProcComments;
  //Analiza el tipo declarado
  try
    srcPos := cIn.ReadSrcPos;  //Inicio de declaración
    if (cIn.tokType = tnType) then begin
      //Caso normal. Es un tipo del sistema
      typName := cIn.tok;
      typ := FindSysEleType(typName); //Busca elemento
     if typ = nil then begin
        //Esto no debería pasar, porque el lexer indica que es un tipo del sistema.
        GenError(ER_NOT_IMPLEM_, [typName]);
        exit(nil);
      end;
      //Encontró al tipo del sistema
      cIn.Next;   //lo toma
      decStyle := ttdDirect;  //Es directo
      //El "catType" se puede leer del mismo tipo
    end else if (cIn.tokL = 'array') or (cIn.tok = '[') then begin
      //Es declaración de arreglo
      decStyle := ttdDeclar;  //Es declaración elaborada
      typ := ArrayDeclaration(srcpos);
      if HayError then exit(nil);     //Sale para ver otros errores
    end else if {(cIn.tokL = 'pointer') or }(cIn.tok = '^') then begin
      //Es declaración de puntero
      decStyle := ttdDeclar;  //Es declaración elaborada
      typ := PointerDeclaration(srcpos);
      if HayError then exit(nil);     //Sale para ver otros errores
    end else if (cIn.tokL = 'object') then begin
      //Es declaración de objeto
      decStyle := ttdDeclar;  //Es declaración elaborada
      typ := ObjectDeclaration(srcpos);
      if HayError then exit(nil);     //Sale para ver otros errores
    end else if cIn.tokType = tnIdentif then begin
      //Es un identificador de tipo
      typName := cIn.tok;
      decStyle := ttdDirect;  //Es directo
      {Se pensó usar GetOperandIdent(), para identificar al tipo, pero no está preparado
      para procesar tipos y no se espera tanta flexibilidad. Así que se hace "a mano".}
      ele := TreeElems.FindFirst(typName);
      if ele = nil then begin
        //No identifica a este elemento
        GenError('Unknown identifier: %s', [typName]);
        exit(nil);
      end;
      if ele.idClass = eltType then begin
        //Es un tipo
        cIn.Next;   //toma identificador
        typ := TxpEleType(ele);
        AddCallerTo(typ);   //lleva la cuenta
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
  finally
    if Result = nil then begin
      //Some error was happen
      if TypeCreated then begin
        typ.Destroy;  //Release object
        TypeCreated:=false;
      end;
    end;
  end;
end;
function TCompMain.GetTypeDeclarSimple(): TxpEleType;
{Similar a GetTypeDeclar(), pero solo permite la referencia a tipos simples. No permite la
declaración de nuevos tipos, como: ARRAY OF ...}
var
  decStyle: TTypDeclarStyle;
  TypeCreated: boolean;
begin
  Result := GetTypeDeclar(decStyle, TypeCreated);  //lee tipo
  if HayError then exit;
  if TypeCreated then begin
    //No se permiten declaraciones elaboradas aquí.
    GenError('Only simple types expected here.');
    Result.Destroy;  //Destroy the new type created
    Result := nil;
  end;
end;
procedure TCompMain.CompileGlobalConstDeclar;
var
  consNames: array of string;  //nombre de variables
  cons: TxpEleCon;
  srcPosArray: TSrcPosArray;
  i: integer;
begin
  SetLength(consNames, 0);
  //Procesa lista de constantes a,b,cons ;
  getListOfIdent(consNames, srcPosArray);
  if HayError then begin  //precisa el error
    GenError(ER_IDE_CON_EXP);
    exit;
  end;
  //puede seguir "=" o identificador de tipo
  if cIn.tok = '=' then begin
    cIn.Next;  //pasa al siguiente
    //Debe seguir una expresiócons constante, que no genere consódigo
    res := GetExpression(0);
    if HayError then exit;
    if res.Sto <> stConst then begin
      GenError(ER_CON_EXP_EXP);
    end;
    //Hasta aquí todo bien, crea la(s) constante(s).
    for i:= 0 to high(consNames) do begin
      //Crea constante
      cons := AddConstant(consNames[i], res.Typ, srcPosArray[i]);
      if HayError then exit;
      res.CopyConsValTo(cons); //asigna valor
    end;
//  end else if cIn.tok = ':' then begin
  end else begin
    GenError(ER_EQU_COM_EXP);
    exit;
  end;
  if not CaptureDelExpres then exit;
  ProcComments;
  //puede salir con error
end;
procedure TCompMain.CompileTypeDeclar(elemLocat: TxpEleLocation; typName: string = '');
{Compila la sección de declaración de un tipo, y genera un elemento TxpEleType, en el
árbol de sintaxis:  TYPE sometype = <declaration>;
Si se especifica typName, se obvia la extracción de la parte " nombreTipo = ", y se
toma el nombre indicado.}
var
  etyp, reftyp: TxpEleType;
  srcpos: TSrcPos;
  decStyle: TTypDeclarStyle;
  typeCreated: boolean;
begin
  ProcComments;
  if cIn.tokType <> tnIdentif then begin
    GenError(ER_IDEN_EXPECT);
    exit;
  end;
  //hay un identificador
  srcpos := cIn.ReadSrcPos;
  typName := cIn.tok;
  cIn.Next;
  ProcComments;
  if not CaptureTok('=') then exit;
  etyp := GetTypeDeclar(decStyle, typeCreated);
  if HayError then exit;
  //Analiza la declaración
  if decStyle = ttdDirect then begin
    //Es un tipo referenciado directamente. Algo como TYPE fool = byte;
    //Para este caso, nosotros creamos un tipo nuevo, en  modo copia.
    reftyp := etyp;  //Referencia al tipo { TODO : ¿Y si el tipo es ya una copia? }
    etyp := CreateEleType(typName, srcPos, reftyp.catType);
    {Crea la copia del tipo del sistema, que básicamente es el mismo tipo, solo que
    con otro nombre y que además, ahora, está en el árbol de sintaxis, por lo tanto
    tiene otras reglas de alcance.}
    etyp.copyOf := reftyp;        //Indica que es una copia
    etyp.location := elemLocat;   //Ubicación del tipo (Interface/Implementation/...)
  end else begin
    //Es una declaración elaborada de tipo. Algo como TYPE fool = array[] of ...
    if typeCreated then begin
      //El tipo ya fue creado. Solo queda verificarlo y agregarlo.
      etyp.name := typName;       //Actualiza el nombre con el nombre de esta definición.
      etyp.srcDec := srcpos;
      etyp.location := elemLocat; //Ubicación del tipo (Interface/Implementation/...)
    end else begin
      {El tipo es elaborado, pero no se ha creado. Puede ser que corresponda a una
      declaración similar. ¿Creamos un tipo nuevo o una copia?}
      //Creamos una copia porque es más fácil
      reftyp := etyp;  //Referencia al tipo { TODO : ¿Y si el tipo es ya una copia? }
      etyp := CreateEleType(typName, srcPos, reftyp.catType);
      {Crea la copia del tipo del sistema, que básicamente es el mismo tipo, solo que
      con otro nombre y que además, ahora, está en el árbol de sintaxis, por lo tanto
      tiene otras reglas de alcance.}
      etyp.copyOf := reftyp;        //Indica que es una copia
      etyp.location := elemLocat;   //Ubicación del tipo (Interface/Implementation/...)
    end;
    //Por la forma de trabajo actual,
  end;
  //Las declaraciones de tipo, crean siempre nuevos tipos.
  //Validación de duplicidad e inclusión en el árbol de sintaxis.
  if etyp.ExistsIn(TreeElems.curNode.elements) then begin
    GenErrorPos(ER_DUPLIC_IDEN, [etyp.name], etyp.srcDec);
    etyp.Destroy;   //Hay una variable creada
    exit;
  end;
  {Valida que solo se manejen tipos de tamaño estático, porque la inicialización de
  variables (que usen este tipo) puede modificar el nombre y tamaño de arreglos de
  tamaño no especificado, y eso cambia la esencia de declarar un tipo con un nombre.
  }
  if (etyp.catType = tctArray) and (etyp.nItems = -1) then begin
    GenError('Array types must have a specified size.');
    etyp.Destroy;   //Hay una variable creada
    exit;
  end; ;
  TreeElems.AddElement(etyp);  //No problem in adding, because we are in the right position of Syntax Tree.
  if not CaptureDelExpres then exit;
  ProcComments;
end;
procedure TCompMain.CompileVarDeclar;
{Compila la declaración de variables en el nodo actual.
"IsInterface", indica el valor que se pondrá al as variables, en la bandera "IsInterface" }
  function GenTypeName(const xtyp: TxpEleType; const baseName: string): string;
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
  i: Integer;
  xvar: TxpEleVar;
  adicVarDec: TAdicVarDec;
  xtyp: TxpEleType;
  decStyle: TTypDeclarStyle;
  typeCreated: boolean;
begin
  SetLength(varNames, 0);
  //Procesa variables a,b,c : int;
  getListOfIdent(varNames, srcPosArray);
  if HayError then begin  //precisa el error
    GenError(ER_EXP_VAR_IDE);
    exit;
  end;
  //usualmente debería seguir ":"
  if cIn.tok = ':' then begin
    //Debe seguir, el tipo de la variable
    cIn.Next;  //lo toma
    ProcComments;
    //Lee el tipo de la variable.
    xtyp := GetTypeDeclar(decStyle, typeCreated );
    if HayError then exit;
    //Lee información adicional de la declaración (ABSOLUTE, REGISTER, initial value)
    ProcComments;
    GetAdicVarDeclar(xtyp, adicVarDec);
    if HayError then begin
      if typeCreated then xtyp.Destroy;
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
    arreglo no diemnsionado, como:  foo []CHAR = 'HELLO'; ya se verificó la
    inicialización.}
    if typeCreated then begin
//      //Se ha creado un tipo nuevo. Verifica y agrega.
//      //xtyp.name := GenTypeName(xtyp, varNames[0]);  //Update name
//      xtyp.location := curLocation;
//      if TreeElems.DeclaredType(xtyp.name, sametype) then begin
//        {El tipo ya existe (con esa misma estructura y en ese alcance), así que mejor
//        pasamos a reutilizar ese tipo. Esta reutilización de tipos evita la creación de
//        múltiples tipos con la misma definición.}
//        xtyp.Destroy;
//        xtyp := sametype;
//        //"sametype" ya está en el árbol de sintaxis.
//      end else begin
        //Es un tipo nuevo.
        xtyp.location := curLocation;   //Ubicación del tipo (Interface/Implementation/...)
        TreeElems.AddElement(xtyp); {¿Comviene agregarlo en este espacio o en otro más accesible desde otros espacios? }
//      end;
    end else begin
      //Tipo directo. No se ha creado ningún tipo nuevo.
      //El tipo ya existe en el arbol de sintaxis
    end;
    //Aquí ya se tiene el tipo creado y en el árbol de sintaxis
    //Reserva espacio para las variables
    for i := 0 to high(varNames) do begin
      xvar := AddVariable(varNames[i], xtyp, srcPosArray[i]);
      if HayError then break;        //Sale para ver otros errores
      xvar.adicPar := adicVarDec;    //Actualiza propiedades adicionales
      xvar.location := curLocation;  //Actualiza bandera
      {Técnicamente, no sería necesario, asignar RAM a la variable aquí (y así se
      optimizaría), porque este método, solo se ejecuta en la primera pasada, y no
      es vital tener las referencias a memoria, en esta pasada.
      Pero se incluye la ásignación de RAM, por:
      * Porque el acceso con directivas, a variables del sistema
      se hace en la primera pasada, y es necesario que estas variables sean válidas.
      * Para tener en la primera pasada, un código más similar al código final.}
      if adicVarDec.hasAdic in [decNone, decAbsol] then  begin
         callCreateVarInRAM(xvar);  //Crea la variable
      end;
      //Agrega la llamada, específica, desde esta variable.
      if adicVarDec.absVar<>nil then begin
        AddCallerTo(adicVarDec.absVar, xvar);
      end
    end;
  end else begin
    GenError(ER_SEM_COM_EXP);
    exit;
  end;
  if not CaptureDelExpres then exit;
  ProcComments;
  //puede salir con error
end;
procedure TCompMain.CompileProcDeclar;
{Compila la declaración de procedimientos. Tanto procedimientos como funciones
 se manejan internamente como funciones.}
  function FindProcInInterface(procName: string; const pars: TxpParFuncArray;
                               const srcPos: TSrcPos): TxpEleFunDec;
  {Explore the current context to verify (and validate) the existence, in the INTERFACE
  section, of a function declared in the IMPLEMENTATION section.
  If found the function, returns the reference. Otherwise returns NIL.
  Also validate the duplicity in the same IMPLEMENTATION section.}
  var
    ele : TxpElement;
    uname: String;
    fun: TxpEleFun;
    funInterface: TxpEleFunDec;
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
          if ele.idClass = eltFuncDec then begin
            //Para las declaraciones, se debe comparar los parámetros
            funInterface := TxpEleFunDec(ele);
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
          if ele.idClass = eltFunc then begin
            //Para las funciones, se debe comparar los parámetros
            fun := TxpEleFun(ele);
            if fun.SameParamsType(pars) then begin
              {Two similar functions in the same IMPLEMENTATION scope.}
              GenErrorPos(ER_DUPLIC_FUNC_,[procName], srcPos);
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
                               const srcPos: TSrcPos): TxpEleFunDec;
  {Explore the current context to verify (and validate) the existence of a functon
  declared as FORWARD, of any ohter function (No FORWARD).
  If found the function, returns the reference. Otherwise returns NIL.
  Also validate the duplicity of the function.}
  var
    uname: String;
    ele : TxpElement;
    fun: TxpEleFun;
    fundec: TxpEleFunDec;
  begin
    Result := nil;
    uname := upcase(procName);
    //Busca en el entorno para ver si está duplicada o con FORWARD
    for ele in TreeElems.curNode.elements do begin
      if ele.uname = uname then begin
        //Match the name.
        if ele.idClass = eltFuncDec then begin
          //Must be a FORWARD
          fundec := TxpEleFunDec(ele);
          //if fun.IsForward ?
          if fundec.SameParamsType(pars) then begin
            Result := fundec;  //Return FORWARD function
          end;
          //Continue exploring to validate
        end else if ele.idClass = eltFunc then begin
          //Para las funciones, se debe comparar los parámetros
          fun := TxpEleFun(ele);
          if fun.SameParamsType(pars) then begin
            //Is not FORWARD, must be duplicated:
            GenErrorPos(ER_DUPLIC_FUNC_,[procName], srcPos);
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
  funDec, funInterface, funForward: TxpEleFunDec;
  fun: TxpEleFun;
  bod: TxpEleBody;
  IsInterrupt, IsForward: Boolean;
  procName: String;
  retType: TxpEleType;
  srcPos: TSrcPos;
  pars: TxpParFuncArray;
begin
  {Este método, solo se ejecutará en la primera pasada, en donde todos los procedimientos
  se codifican al inicio de la memoria, y las variables y registros se ubican al
  inicio de la memoria RAM, ya que lo que importa es simplemente recabar información
  del procedimiento, y no tanto codificarlo. }
  callResetRAM;   //Limpia RAM
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
      GenErrorPos(ER_DUPLIC_FUNC_,[procName], srcPos);
      exit;
    end;
    funDec := AddFunctionDEC(procName, retType, srcPos, pars, IsInterrupt);
    funDec.location := curLocation;  //Set location
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
      fun := AddFunctionIMP(procName, retType, srcPos, funInterface);
    end else begin
      //Debe ser una función privada. No declarada en INTERFACE.
      //Ya verificamos que no hay conflicto en IMPLEMENTATION.
      fun := AddFunctionUNI(procName, retType, srcPos, pars, IsInterrupt);
    end;
    fun.location := curLocation;
  end;
  locMain: begin
    //Es una compilación en el programa principal. ¿Y si es FORWARD?
    ReadProcHeader(procName, retType, srcPos, pars, IsInterrupt, IsForward);
    if HayError then exit;
    if IsForward then begin
      if TreeElems.FunctionExistInCur(procName, pars) then begin
        GenErrorPos(ER_DUPLIC_FUNC_,[procName], srcPos);
        exit;
      end;
      funDec := AddFunctionDEC(procName, retType, srcPos, pars, IsInterrupt);
      funDec.location := locMain;  //Set location
      funDec.IsForward := true;    //Mark as Forward
      exit;  //No more task required.
    end;
    //This is no-FORWARD function
    funForward := FindProcAsForwawd(procName, pars, srcPos);
    if HayError then exit;
    if funForward<>nil then begin
      //It's an implementation
      fun := AddFunctionIMP(procName, retType, srcPos, funForward);
    end else begin
      //It's a common function
      fun := AddFunctionUNI(procName, retType, srcPos, pars, IsInterrupt);
    end;
    fun.location := curLocation;
  end;
  else
    GenError(ER_NOT_IMPLEM_, ['locMain in TCompMain.CompileProcDeclar()']);
  end;
  //Aquí ya se tiene "fun" abierta, validada y apuntando a la declaración.
  //Empiezan las declaraciones VAR, CONST, PROCEDURE, TYPE
  while StartOfSection do begin
    if cIn.tokL = 'var' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'begin') do begin
        CompileVarDeclar;
        if HayError then exit;;
      end;
    end else if cIn.tokL = 'const' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'begin') do begin
        CompileGlobalConstDeclar;
        if HayError then exit;;
      end;
//    end else if cIn.tokL = 'procedure' then begin
//      cIn.Next;    //lo toma
//      CompileProcDeclar;
    end else begin
      GenError('Expected VAR, CONST or BEGIN.');
      exit;
    end;
  end;
  if cIn.tokL <> 'begin' then begin
    GenError('Expected "begin", "var", "type" or "const".');
    exit;
  end;
  //Ahora empieza el cuerpo de la función o las declaraciones
  fun.adrr   := callCurrRAM(); //toma dirección de inicio del código. Es solo referencial.
  fun.posCtx := cIn.PosAct;  //Guarda posición para la segunda compilación
  bod := CreateBody;   //crea elemento del cuerpo de la función
  bod.srcDec := cIn.ReadSrcPos;
  TreeElems.AddElementAndOpen(bod);  //Abre nodo Body
  callCompileProcBody(fun);
  TreeElems.CloseElement;  //Cierra Nodo Body
  TreeElems.CloseElement; //cierra espacio de nombres de la función
  bod.srcEnd := cIn.ReadSrcPos;  //Fin de cuerpo
//  fun.adrReturn := pic.iRam-1;  //Guarda dirección del i_RETURN
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
//          if ele.idClass = eltFunc then begin
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
//    if cIn.tokL = 'var' then begin
//      cIn.Next;    //lo toma
//      while not StartOfSection and (cIn.tokL <>'begin') do begin
//        CompileVarDeclar;
//        if HayError then exit;;
//      end;
//    end else if cIn.tokL = 'const' then begin
//      cIn.Next;    //lo toma
//      while not StartOfSection and (cIn.tokL <>'begin') do begin
//        CompileGlobalConstDeclar;
//        if HayError then exit;;
//      end;
////    end else if cIn.tokL = 'procedure' then begin
////      cIn.Next;    //lo toma
////      CompileProcDeclar;
//    end else begin
//      GenError('Expected VAR, CONST or BEGIN.');
//      exit;
//    end;
//  end;
//  if cIn.tokL <> 'begin' then begin
//    GenError('Expected "begin", "var", "type" or "const".');
//    exit;
//  end;
//  //Ahora empieza el cuerpo de la función o las declaraciones
//  fun.posCtx := cIn.PosAct;  //Guarda posición para la segunda compilación
//  bod := CreateBody;   //crea elemento del cuerpo de la función
//  bod.srcDec := cIn.ReadSrcPos;
//  TreeElems.AddElementAndOpen(bod);  //Abre nodo Body
//  CompileInlineBody(fun);
//  TreeElems.CloseElement;  //Cierra Nodo Body
//  TreeElems.CloseElement; //cierra espacio de nombres de la función
//  bod.srcEnd := cIn.ReadSrcPos;  //Fin de cuerpo
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
    if not CaptureStr('end') then exit(false);
  end;
end;
function TCompMain.GetExpressionBool: boolean;
{Lee una expresión booleana. Si hay algún error devuelve FALSE.}
begin
  OnExprStart;
  res := GetExpression(0);
  if HayError then exit(false);
  OnExprEnd(pexSTRUC);
  if res.Typ <> typBool then begin
    GenError(ER_BOOL_EXPECT);
    exit(false);
  end;
  ProcComments;
  exit(true);  //No hay error
end;
procedure TCompMain.Tree_AddElement(elem: TxpElement);
begin
  if FirstPass then begin
    //Caso normal. Solo aquí dede modificarse el árbol de sintaxis.
  end else begin
    //Solo se permiet agregar elementos en la primera pasada
    GenError('Internal Error: Syntax Tree modified on linking.');
  end;
end;
function TCompMain.OpenContextFrom(filePath: string): boolean;
{Abre un contexto con el archivo indicado. Si lo logra abrir, devuelve TRUE.}
var
  strList: TStrings;
begin
  //Primero ve si puede obteenr acceso directo al contenido del archivo
  if OnRequireFileString<>nil then begin
    //Hace la consulta a través del evento
    strList := nil;
    OnRequireFileString(filePath, strList);
    if strList=nil then begin
      //No hay acceso directo al contenido. Carga de disco
      //debugln('>disco:'+filePath);
      cIn.MsjError := '';
      cIn.NewContextFromFile(filePath);
      Result := cIn.MsjError='';  //El único error es cuando no se encuentra el archivo.
    end else begin
      //Nos están dando el acceso al contenido. Usamos "strList"
      cIn.NewContextFromFile(filePath, strList);
      Result := true;
    end;
  end else begin
    //No se ha establecido el evento. Carga de disco
    //debugln('>disco:'+filePath);
    cIn.MsjError := '';
    cIn.NewContextFromFile(filePath);
    Result := cIn.MsjError='';  //El único error es cuando no se encuentra el archivo.
  end;
end;
function TCompMain.CompileStructBody(GenCode: boolean): boolean;
{Compila el cuerpo de un THEN, ELSE, WHILE, ... considerando el modo del compilador.
Si se genera error, devuelve FALSE. }
begin
  if GenCode then begin
    //Este es el modo normal. Genera código.
    if mode = modPascal then begin
      //En modo Pascal se espera una instrucción
      CompileInstruction;
    end else begin
      //En modo normal
      CompileCurBlock;
    end;
    if HayError then exit(false);
  end else begin
    //Este modo no generará instrucciones
    cIn.SkipWhites;
    GenWarn(ER_INST_NEV_EXE);
    if mode = modPascal then begin
      //En modo Pascal se espera una instrucción
      CompileInstructionDummy //solo para mantener la sintaxis
    end else begin
      //En modo normal
      CompileCurBlockDummy;  //solo para mantener la sintaxis
    end;
    if HayError then exit(false);
  end;
  //Salió sin errores
  exit(true);
end;
function TCompMain.CompileConditionalBody: boolean;
{Versión de CompileStructBody(), para bloques condicionales.
Se usa para bloque que se ejecutarán de forma condicional, es decir, que no se
garantiza que se ejecute siempre. "FinalBank" indica el banco en el que debería
terminar el bloque.}
begin
  Result := CompileStructBody(true);  //siempre genera código
end;
function TCompMain.CompileNoConditionBody(GenCode: boolean): boolean;
{Versión de CompileStructBody(), para bloques no condicionales.
Se usa para bloques no condicionales, es decir que se ejecutará siempre (Si GenCode
es TRUE) o nunca (Si GenCode es FALSE);
}
begin
  //"BankChanged" sigue su curso normal
  Result := CompileStructBody(GenCode);
end;
procedure TCompMain.CompileInstruction;
{Compila una única instrucción o un bloque BEGIN ... END. Puede generar Error.
 Una instrucción se define como:
 1. Un bloque BEGIN ... END
 2. Una estrutura
 3. Una expresión
 La instrucción, no incluye al delimitador.
 }
var
  curCodCon: TxpEleCodeCont;
begin
  if TreeElems.curNode.idClass <> eltBody then begin
    //No debería pasar, porque las instrucciones solo pueden estar en eltBody
    GenError('Syntax error.');
    exit;
  end;
  curCodCon := TreeElems.CurCodeContainer;
  ProcComments;
  if cIn.tokL='begin' then begin
    //Es bloque
    cIn.Next;  //toma "begin"
    CompileCurBlock;   //llamada recursiva
    if HayError then exit;
    if not CaptureStr('end') then exit;
    ProcComments;
    //puede salir con error
  end else begin
    //Es una instrucción
    if cIn.tokType = tnStruct then begin
      if cIn.tokl = 'if' then begin
        curCodCon.OpenBlock(sbiIF);
        cIn.Next;         //pasa "if"
        callCompileIF;
        curCodCon.CloseBlock;
      end else if cIn.tokl = 'while' then begin
        curCodCon.OpenBlock(sbiWHILE);
        cIn.Next;         //pasa "while"
        callCompileWHILE;
        curCodCon.CloseBlock;
      end else if cIn.tokl = 'repeat' then begin
        curCodCon.OpenBlock(sbiREPEAT);
        cIn.Next;         //pasa "until"
        callCompileREPEAT;
        curCodCon.CloseBlock;
      end else if cIn.tokl = 'for' then begin
        curCodCon.OpenBlock(sbiFOR);
        cIn.Next;         //pasa "until"
        callCompileFOR;
        curCodCon.CloseBlock;
      end else begin
        GenError(ER_UNKN_STRUCT);
        exit;
      end;
    end else begin
      //Debe ser es una expresión de asignación o llamada a procedimiento.
      GetExpressionE(pexASIG);
    end;
    if HayError then exit;
    if callDeviceError()<>'' then begin
      //El CPU también puede dar error
      GenError(callDeviceError());
    end;
  end;
end;
procedure TCompMain.CompileInstructionDummy;
{Compila una instrucción pero sin generar código. }
var
  InvertedFromZ0: Integer;
  InvertedFromC0: Integer;
  AcumStatInZ0: Boolean;
begin
  OnReqStopCodeGen;
  InvertedFromC0 := BooleanFromC; //Guarda estado
  InvertedFromZ0 := BooleanFromZ; //Guarda estado
  AcumStatInZ0  := AcumStatInZ;

  CompileInstruction;  //Compila solo para mantener la sintaxis

  BooleanFromC := InvertedFromC0; //Restaura
  BooleanFromZ := InvertedFromZ0; //Restaura
  AcumStatInZ  := AcumStatInZ0;
  OnReqStartCodeGen;
  //puede salir con error
  { TODO : Debe limpiar la memoria flash que ocupó, para dejar la casa limpia. }
end;
procedure TCompMain.CompileCurBlock;
{Compila el bloque de código actual hasta encontrar un delimitador de bloque, o fin
de archivo. }
begin
  ProcComments;
  while not cIn.Eof and (cIn.tokType<>tnBlkDelim) do begin
    //se espera una expresión o estructura
    CompileInstruction;
    if HayError then exit;   //aborta
    //se espera delimitador
    if cIn.Eof then break;  //sale por fin de archivo
    //busca delimitador
    ProcComments;
    //Puede terminar con un delimitador de bloque
    if cIn.tokType=tnBlkDelim then break;
    //Pero lo común es que haya un delimitador de expresión
    if not CaptureTok(';') then exit;
    ProcComments;  //Puede haber Directivas o ASM también
  end;
end ;
procedure TCompMain.CompileCurBlockDummy;
{Compila un bloque pero sin geenrar código.}
var
  InvertedFromC0, InvertedFromZ0: Integer;
  AcumStatInZ0: Boolean;
begin
  OnReqStopCodeGen;
  InvertedFromC0 := BooleanFromC; //Guarda estado
  InvertedFromZ0 := BooleanFromZ; //Guarda estado
  AcumStatInZ0  := AcumStatInZ;

  CompileCurBlock;  //Compila solo para mantener la sintaxis

  BooleanFromC := InvertedFromC0; //Restaura
  BooleanFromZ := InvertedFromZ0; //Restaura
  AcumStatInZ  := AcumStatInZ0;
  OnReqStartCodeGen;
  //puede salir con error
  { TODO : Debe limpiar la memoria flash que ocupó, para dejar la casa limpia. }
end;
function TCompMain.IsUnit: boolean;
{Indica si el archivo del contexto actual, es una unidad. Debe llamarse}
begin
  ProcCommentsNoExec;  //Solo es validación, así que no debe ejecutar nada
  //Busca UNIT
  if cIn.tokL = 'unit' then begin
    cIn.curCon.SetStartPos;   //retorna al inicio
    exit(true);
  end;
  cIn.curCon.SetStartPos;   //retorna al inicio
  exit(false);
end;
procedure TCompMain.CompileInlineBody(fun: TxpEleInlin);
{Compila el cuerpo de un procedimiento INLINE.}
begin
  //Faltaría revisar la compilación para que se adecúe a la de un proc. Inline
  CompileInstructionDummy;
end;
procedure TCompMain.AssignRAMtoVar(xvar: TxpEleVar; shared: boolean = false);
var
  posAct : TPosCont;
  adicVarDec: TAdicVarDec;
begin
//debugln('  Asignando espacio a %s', [xvar.name]);
  case xvar.adicPar.hasAdic of
  decAbsol: begin
//debugln('Abs: xvar=%s at %d', [xvar.name, xvar.adicPar.absAddr]);
    {Tiene declaración absoluta. Mejor compilamos de nuevo la declaración, porque
    puede haber referencia a variables que han cambiado de ubicación, por
    optimización.
    Se podría hacer una verificación, para saber si la referencia es a direcciones
    absolutas, en lugar de a variables (o a variables temporales), y así evitar
    tener que compilar de nuevo, la declaración.}
    posAct := cIn.PosAct;   //guarda posición actual
    cIn.PosAct := xVar.adicPar.srcDec;  //Posiciona en la declaración adicional
    TreeElems.curNode := xvar.Parent;   {Posiciona el árbol, tal cual estaría en la
                                         primera pasada, para una correcta resolución
                                         de nombres}
    GetAdicVarDeclar(xvar.typ, adicVarDec);
    //No debería dar error, porque ya pasó la primera pasada
    xvar.adicPar := adicVarDec;
    cIn.PosAct := posAct;
    //Asigna RAM
    callCreateVarInRAM(xVar, shared);  //Crea la variable
    xvar.typ.DefineRegister;  //Asegura que se dispondrá de los RT necesarios
    //Puede salir con error
  end;
  decRegis, decRegisA, decRegisX, decRegisY: begin
    //Variable registro. No se asigna espacio.
  end;
  decNone: begin
    //Variable normal. Necesita ser asiganda en RAM
    callCreateVarInRAM(xVar, shared);  //Crea la variable
    //xvar.typ.DefineRegister;  //Asegura que se dispondrá de los RT necesarios
    //Puede salir con error
  end;
  end;
end;

procedure TCompMain.CreateLocalVarsAndPars;
{Create in RAM, local varisbles and parameters for functions.}
var
  elem   : TxpElement;
  xvar   : TxpEleVar;
  fun    : TxpEleFun;
begin
  //Explora primero a las funciones terminales
  for fun in usedFuncs do begin
    if not fun.IsTerminal2 then continue;
    //DebugLn('función terminal: %s con %d var.loc.', [fun.name, fun.nLocalVars]);
    //Los parámetros y variables locales aparecen como elementos de la función
    for elem in fun.elements do if elem.idClass = eltVar then begin
      xvar := TxpEleVar(elem);
      if xvar.nCalled>0 then begin
        //Asigna una dirección válida para esta variable
        AssignRAMtoVar(xvar, true);
        if HayError then exit;
      end;
    end;
    if OptReuProVar then callSetSharedUnused;   //limpia las posiciones usadas
  end;
  if OptReuProVar then callSetSharedUsed;  //Ahora marca como usados, porque ya se creó la zona de bytes compartidos
  //Explora solo a las funciones que no son terminales
  for fun in usedFuncs do begin
    if fun.IsTerminal2 then continue;
    //Los parámetros y variables locales aparecen como elementos de la función
    for elem in fun.elements do if elem.idClass = eltVar then begin
      xvar := TxpEleVar(elem);
      if xvar.nCalled>0 then begin
        //Asigna una dirección válida para esta variable
        AssignRAMtoVar(xvar);
        if HayError then exit;
      end;
    end;
  end;
end;
procedure TCompMain.CreateGlobalVars;
var
  xvar   : TxpEleVar;
begin
  //Reserva espacio para las variables (Que no son de funciones).
  for xvar in TreeElems.AllVars do begin
    if xvar.Parent.idClass = eltFunc then continue;  //Las variables de funciones ya se crearon
    //if xvar.Parent.idClass = eltUnit then continue;
    if xvar.nCalled>0 then begin
      //Asigna una dirección válida para esta variable
      AssignRAMtoVar(xvar);
      if HayError then exit;
    end else begin
      //Variable no usada
      xvar.ResetAddress;
      if xvar.Parent = TreeElems.main then begin
        //Genera mensaje solo para variables del programa principal.
        GenWarnPos(WA_UNUSED_VAR_, [xVar.name], xvar.srcDec);
      end;
    end;
  end;
end;
//Compilación de secciones
procedure TCompMain.CompileUnit(uni: TxpElement);
{Realiza la compilación de una unidad}
var
  elem: TxpElement;
  fundec: TxpEleFunDec;
begin
//debugln('   Ini Unit: %s-%s',[TreeElems.curNode.name, ExtractFIleName(cIn.curCon.arc)]);
  ClearError;
  callClearDeviceError;
  ProcComments;
  //Busca UNIT
  if cIn.tokL = 'unit' then begin
    cIn.Next;  //pasa al nombre
    ProcComments;
    if cIn.Eof then begin
      GenError('Name of unit expected.');
      exit;
    end;
    if UpCase(cIn.tok)<>uni.uname then begin
      GenError('Name of unit doesn''t match file name.');
      exit;
    end;
    cIn.Next;  //Toma el nombre y pasa al siguiente
    if not CaptureDelExpres then exit;
  end else begin
    GenError('Expected: UNIT');
    exit;
  end;
  ProcComments;
  if cIn.tokL <> 'interface' then begin
    GenError('Expected: INTERFACE');
    exit;
  end;
  cIn.Next;   //toma
  ProcComments;
  curLocation := locInterface;
  if cIn.Eof then begin
    GenError('Expected "uses", "var", "type", "const" or "implementation".');
    exit;
  end;
  ProcComments;
  //Busca USES
  CompileUsesDeclaration;
  if cIn.Eof then begin
    GenError('Expected "var", "type" or "const".');
    exit;
  end;
  ProcComments;
//  Cod_StartProgram;  //Se pone antes de codificar procedimientos y funciones
  if HayError then exit;
  //Empiezan las declaraciones
  while StartOfSection do begin
    if cIn.tokL = 'var' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'implementation') do begin
        CompileVarDeclar;  //marca como "IsInterface"
        if HayError then exit;;
      end;
    end else if cIn.tokL = 'type' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'implementation') do begin
        CompileTypeDeclar(locInterface);
        if HayError then exit;
      end;
    end else if cIn.tokL = 'const' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'implementation') do begin
        CompileGlobalConstDeclar;
        if HayError then exit;;
      end;
    end else if cIn.tokL = 'procedure' then begin
      cIn.Next;    //lo toma
      CompileProcDeclar;
      if HayError then exit;
    end else begin
      GenError(ER_NOT_IMPLEM_, [cIn.tok]);
      exit;
    end;
  end;
  ProcComments;
  if cIn.tokL <> 'implementation' then begin
    GenError('Expected: IMPLEMENTATION');
    exit;
  end;
  cIn.Next;   //toma
  /////////////////  IMPLEMENTATION /////////////////////
  ProcComments;
  //Explora las declaraciones e implementaciones
  curLocation := locImplement;
  //Empiezan las declaraciones
  while StartOfSection do begin
    if cIn.tokL = 'var' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'end') do begin
        CompileVarDeclar;
        if HayError then exit;;
      end;
    end else if cIn.tokL = 'const' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'end') do begin
        CompileGlobalConstDeclar;
        if HayError then exit;;
      end;
    end else if cIn.tokL = 'procedure' then begin
      cIn.Next;    //lo toma
      CompileProcDeclar;  //Compila en IMPLEMENTATION
      if HayError then exit;
    end else begin
      GenError(ER_NOT_IMPLEM_, [cIn.tok]);
      exit;
    end;
  end;
  //Verifica si todas las funciones de INTERFACE, se implementaron
  for elem in TreeElems.curNode.elements do if elem.idClass = eltFuncDec then begin
    fundec := TxpEleFunDec(elem);
    if fundec.implem = nil then begin
      GenErrorPos('Function %s not implemented.', [fundec.name], fundec.srcDec);
      exit;
    end;
  end;
  CompileLastEnd;
  if HayError then exit;
//  //procesa cuerpo
//  ResetRAM;  {No es tan necesario, pero para seguir un orden y tener limpio
//                     también, la flash y memoria, después de algún psoible procedimiento.}
//  if cIn.tokL = 'begin' then begin
//    bod := CreateBody;
//    bod.srcDec := cIn.ReadSrcPos;
//    cIn.Next;   //coge "begin"
//    //Guardamos la ubicación física, real, en el archivo, después del BEGIN
//    bod.posCtx := cIn.PosAct;
//    //codifica el contenido
//    CompileCurBlock;   //compila el cuerpo
//    if HayError then exit;

//    _SLEEP();   //agrega instrucción final
//  end else begin
//    GenError('Expected "begin", "var", "type" or "const".');
//    exit;
//  end;
//  Cod_EndProgram;
//debugln('   Fin Unit: %s-%s',[TreeElems.curNode.name, ExtractFIleName(cIn.curCon.arc)]);
end;
procedure TCompMain.CompileUsesDeclaration;
{Compila la unidad indicada.}
var
  uni: TxpEleUnit;
  uPath: String;
  uName: String;
  p: TPosCont;
begin
  if cIn.tokL = 'uses' then begin
    cIn.Next;  //pasa al nombre
    //Toma una a una las unidades
    repeat
      ProcComments;
      //ahora debe haber un identificador
      if cIn.tokType <> tnIdentif then begin
        GenError(ER_IDEN_EXPECT);
        exit;
      end;
      //hay un identificador de unidad
      uName := cIn.tok;
      uni := CreateUnit(uName);
      //Verifica si existe ya el nombre de la unidad
      if uni.ExistsIn(TreeElems.curNode.elements) then begin
        GenError('Identifier duplicated: %s.', [uName]);
        uni.Destroy;
        exit;
      end;
      uni.srcDec := cIn.ReadSrcPos;   //guarda posición de declaración
      uName := uName + '.pas';  //nombre de archivo
{----}TreeElems.AddElementAndOpen(uni);
      //Ubica al archivo de la unidad
      p := cIn.PosAct;   //Se debe guardar la posición antes de abrir otro contexto
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
      CompileUnit(uni);
      cIn.PosAct := p;
      if HayError then exit;  //El error debe haber guardado la ubicación del error
{----}TreeElems.CloseElement; //cierra espacio de nombres de la función
      cIn.Next;  //toma nombre
      cIn.SkipWhites;
      if cIn.tok <> ',' then break; //sale
      cIn.Next;  //toma la coma
    until false;
    if not CaptureDelExpres then exit;
  end;
end;
procedure TCompMain.CompileProgram;
{Compila un programa en el contexto actual. Empieza a codificar el código a partir de
la posición actual de memoria en el PIC (iRam).}
var
  bod: TxpEleBody;
  elem: TxpElement;
  fundec: TxpEleFunDec;
begin
  ClearError;
  callClearDeviceError;
  ProcComments;
  //Busca PROGRAM
  if cIn.tokL = 'unit' then begin
    //Se intenta compilar una unidad
    GenError('Expected a program. No a unit.');
    exit;
  end;
  if cIn.tokL = 'program' then begin
    cIn.Next;  //pasa al nombre
    ProcComments;
    if cIn.Eof then begin
      GenError(ER_PROG_NAM_EX);
      exit;
    end;
    cIn.Next;  //Toma el nombre y pasa al siguiente
    if not CaptureDelExpres then exit;
  end;
  if cIn.Eof then begin
    GenError('Expected "program", "begin", "var", "type" or "const".');
    exit;
  end;
  ProcComments;
  //Busca USES
  if HayError then exit;  //CompileUsesDeclaration, va a limpiar "HayError"
  CompileUsesDeclaration;
  if cIn.Eof then begin
    GenError('Expected "begin", "var", "type" or "const".');
    exit;
  end;
  ProcComments;
  callStartProgram;  //Se pone antes de codificar procedimientos y funciones
  curLocation := locMain;
  if HayError then exit;
  //Empiezan las declaraciones
  while StartOfSection do begin
    if cIn.tokL = 'var' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'begin') do begin
        CompileVarDeclar;
        if HayError then exit;
      end;
    end else if cIn.tokL = 'type' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'begin') do begin
        CompileTypeDeclar(locMain);
        if HayError then exit;
      end;
    end else if cIn.tokL = 'const' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'begin') do begin
        CompileGlobalConstDeclar;
        if HayError then exit;
      end;
    end else if cIn.tokL = 'procedure' then begin
      cIn.Next;    //lo toma
      CompileProcDeclar;
      if HayError then exit;
    end else if cIn.tokL = 'inline' then begin
      cIn.Next;    //lo toma
      CompileInlineDeclar(locMain);
      if HayError then exit;
    end else begin
      GenError(ER_NOT_IMPLEM_, [cIn.tok]);
      exit;
    end;
  end;
  //Procesa cuerpo
  callResetRAM;  {No es tan necesario, pero para seguir un orden y tener limpio
                     también, la flash y memoria, después de algún posible procedimiento.}
  if cIn.tokL <> 'begin' then begin
    GenError('Expected "begin", "var", "type" or "const".');
    exit;
  end;
  bod := CreateBody;
  bod.srcDec := cIn.ReadSrcPos;
  TreeElems.AddElementAndOpen(bod);  //Abre nodo Body
  cIn.Next;   //coge "begin"
  //Guardamos popsisicón en contexto para la segunda compilación
  bod.posCtx := cIn.PosAct;
  //codifica el contenido
  CompileCurBlock;   //compila el cuerpo
  TreeElems.CloseElement;   //No debería ser tan necesario.
  bod.srcEnd := cIn.ReadSrcPos;
  if HayError then exit;
  //Verifica si todas las funciones FORWARD, se implementaron
  for elem in TreeElems.curNode.elements do if elem.idClass = eltFuncDec then begin
    fundec := TxpEleFunDec(elem);
    if fundec.implem = nil then begin
      GenErrorPos('Function %s not implemented.', [fundec.name], fundec.srcDec);
      exit;
    end;
  end;
  CompileLastEnd;  //Compila el "END." final
  if HayError then exit;
  //_RTS();   //agrega instrucción final
  callEndProgram;
end;


end.

