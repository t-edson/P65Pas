{
XpresTypes
==========
Por Tito Hinostroza.

Basic definitions for the manage of elements representing types.
Here are defined objets for the manage of expressions: Types, operands, operations.
}
unit XpresTypesPIC;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl;

type  //Enumerated types

  //Groups of data types.
  TTypeGroup=(
    t_integer,  //números enteros
    t_uinteger, //enteros sin signo
    t_float,    //de coma flotante
    t_string,   //cadena de caracteres
    t_boolean,  //booleano
    t_enum      //enumerado
  );

  {Espacio para almacenar a los posibles valores de una constante.
  Debe tener campos para los tipos básicos de variable haya en "TCatType" y para valores
  estructurados}
  TConsValue = record
    ValInt  : Int64;    //Para alojar a los valores t_integer y t_uinteger
    ValFloat: extended; //Para alojar a los valores t_float
    ValBool : boolean;  //Para alojar a los valores t_boolean
    ValStr  : string;   //Para alojar a los valores t_string
    items   : array of TConsValue;  //Lista de items cuando sea array
    nItems  : integer;  //Number of items
  end;

  //Operand storage
  TStoOperand = (
    //Basic storage
    stConst   = %0000, {Operand is constant and its value is stored directly in the Operand
                       without use CPU resources. Includes evaluated constant expressions.}
    stExpres  = %0001, {Operand value is stored in RT. Generally is the result of a
                       expression, or the result of a function call.}
    stVariab  = %0010, {Operand is addressed by a constants address.}
    //Aditional storage (For pointers and arrays)
    stVarRef  = %0011, {Operand is addressed by the value of a variable. Doesn't use RT.}
    stVarConRef=%0100, {Operand is addressed by the value of a variable plus a constant
                      offset. Doesn't use RT.}
    stExpRef  = %0101,  {Operand is addressed by the value stored in IX}
    //Aditional expressions types
    stExpresA = %1000, {Operand value is stored in register A.}
    stExpresX = %1001, {Operand value is stored in register X.}
    stExpresY = %1010,  {Operand value is stored in register Y.}

    //Without storage
    stNull    = %1111 //Operand is not stored
  );
  {Almacenamiento combinado para una ROB. Se construye para poder representar dos valores
  de TStoOperand en una solo valor byte (juntando sus bits), para facilitar el uso de un
  CASE ... OF}
  TStoOperandsROB =(
    stConst_Const    = %00000000,
    stConst_Expres   = %00000001,
    stConst_Variab   = %00000010,

    stExpres_Const   = %00010000,
    stExpres_Expres  = %00010001,
    stExpres_Variab  = %00010010,

    stVariab_Const   = %00100000,
    stVariab_Expres  = %00100001,
    stVariab_Variab  = %00100010
  );


  TProcDefineVar = procedure(const varName, varInitVal: string) of object;
  {Evento para cargar un  operando en la pila.
  "OpPtr" debería ser "TOperand", pero aún no se define "TOperand".}
  TProcLoadOperand = procedure(const OpPtr: pointer) of object;

  TxpOperatorKind = (
    opkUnaryPre,   //operador Unario Pre
    opkUnaryPost,  //operador Unario Post
    opkBinary      //operador Binario
  );

type  //Type Methods

  {Event to call code for process and method (INLINE).
  "OpPtr" should be "TOperand", but "TOperand" is not yet defined.}
  TTypFieldProc = procedure(const OpPtr: pointer) of object;

  TTypField = class
    Name : string;  //Nombre del campo
    procGet : TTypFieldProc;  //routine to process when reading
    procSet : TTypFieldProc;  //routine to process when writing
  end;
  TTypFields = specialize TFPGObjectList<TTypField>;



const  //Prefixes used to name the anonym type declarations
  //Short string are used to don't afect the speed of searchings
  PREFIX_ARR = 'arr';
  PREFIX_PTR = 'ptr';
  PREFIX_OBJ = 'obj';

type
  //Types categories
  TxpCatType = (
    tctAtomic,  //Tipo básico como (byte, word, char)
    tctArray,   //Arreglo de algún otro tipo.
    tctPointer, //Puntero de otro tipo.
    tctObject   //Registro de varios campos (OBJECT <tipos> END o podría ser {...} )
  );
  {Types categories define the way a type is structured.

  ==== ATOMIC ====
  We say a type is atomic, when it cannot be expressed as a construction of other type.
  For example: CHAR or BYTE types. WORD type should be atomic too. Although a WORD can be
  expressed as an OBJECT. Here in P65Pas we define WORD as atomic.
  Declaraction for atomic types are:
  TYPE
    mytype = byte;
    mytype2 = char;
    mytype3 = mytype;  //Because "mytype" is tomic too.

  ==== ARRAY ====
  Array of some other type (atomic or not).
  Declaration for array types are:
  TYPE
    artype = ARRAY[10] OF byte;
    otherarray = artype;  //Because artype is array
    alsoarray = ARRAY OF noAtomicType;

  As an alternative notation we can use is:
  TYPE
    artype = [10]byte;

  ==== POINTER ====
  Pointer to some other type (atomic or not).
  Declaration for pointer types are:
  TYPE
    ptrtype = POINTER TO byte;
    otherptr = ptrtype;  //Because ptrtype is pointer
    alsoptr = POINTER TO noAtomicType;

  As an alternative notation we can use is:
  TYPE
    artype = ^byte;

  }

  //Type declaration style.
  TTypDeclarStyle = (
    ttdDirect,   {Like:
                      TYPE mytype = byte;
                      TYPE mytype2 = mytype;  //"mytype" could be ARRAY/POINTER/OBJECT
                }
    ttdDeclar   {Like:
                      TYPE mytype = ARRAY[30] OF char;
                      TYPE refchar = POINTER TO char; }
  );

type
  {Estos tipos están relacionados con el hardware, y tal vez deberían estar declarados
  en otra unidad. Pero se ponen aquí porque son pocos.
  La idea es que sean simples contenedores de direcciones físicas. En un inicio se pensó
  declararlos como RECORD por velocidad (para no usar memoria dinámica), pero dado que no
  se tienen requerimientos altos de velocidad en PicPas, se declaran como clases. }
  //Tipo de registro
  TPicRegType = (prtWorkReg,   //de trabajo
                 prtAuxReg,    //auxiliar
                 prtStkReg     //registro de pila
  );
  { TPicRegister }
  {Objeto que sirve para modelar a un registro del PIC (una dirección de memoria, usada
   para un fin particular)}
  TPicRegister = class
  public
    addr    : word;      //Dirección absoluta: $000 a $FFFF
    assigned: boolean;   //indica si tiene una dirección física asignada
    used    : boolean;   //Indica si está usado.
    typ     : TPicRegType; //Tipo de registro
  public
    procedure Assign(srcReg: TPicRegister);
  end;
  TPicRegister_list = specialize TFPGObjectList<TPicRegister>; //lista de registros

implementation

{ TPicRegister }
procedure TPicRegister.Assign(srcReg: TPicRegister);
begin
  addr    := srcReg.addr;
  assigned:= srcReg.assigned;
  used    := srcReg.used;
  typ     := srcReg.typ;
end;

end.

