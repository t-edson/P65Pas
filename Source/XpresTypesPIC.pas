{
XpresTypes
==========
Por Tito Hinostroza.

Definiciones básicas para el manejo de elementos que representan a tipos.
Aquí están definidas los objetos claves para el manejo de expresiones:
Los tipos, los operadores y las operaciones
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
  end;

  //Operand storage
  TStoOperand = (
    stConst  = %000, {Operand is constant and its value is stored directly in the Operand
                      without use CPU resources. Includes evaluated constant expressions.}
    stExpres = %001, {Operand value is stored in RT. Generally is the result of a
                      expression, or the result of a function call.}
    stVariab = %010, {Operand is addressed by a constants address.}
    stVarRef = %011, {Operand is addressed by the value of a variable. Doesn't use RT.}
    stVarConRef=%100,{Operand is addressed by the value of a variable plus a constant
                      offset. Doesn't use RT.}
    stExpRef = %101  {Operand is addressed by the value stored in RT}
  );
  {Almacenamiento combinado para una ROB. Se construye para poder representar dos valores
  de TStoOperand en una solo valor byte (juntando sus bits), para facilitar el uso de un
  CASE ... OF}
  TStoOperandsROB =(
    stConst_Const    = %000000,
    stConst_Expres   = %000001,
    stConst_Variab   = %000010,
    stConst_VarRef   = %000011,

    stExpres_Const   = %001000,
    stExpres_Expres  = %001001,
    stExpres_Variab  = %001010,
    stExpres_VarRef  = %001011,

    stVariab_Const   = %010000,
    stVariab_Expres  = %010001,
    stVariab_Variab  = %010010,
    stVariab_VarRef  = %010011
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
  {Evento para llamar al código de procesamiento de un campo.
  "OpPtr" debería ser "TOperand", pero aún no se define "TOperand".}
  TTypFieldProc = procedure(const OpPtr: pointer) of object;

  TTypField = class
    Name : string;  //Nombre del campo
    proc : TTypFieldProc;  //rutina de procesamiento
  end;
  TTypFields = specialize TFPGObjectList<TTypField>;

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
    assigned: boolean;  //indica si tiene una dirección física asignada
    used    : boolean;   //Indica si está usado.
    typ     : TPicRegType; //Tipo de registro
  public
    procedure Assign(srcReg: TPicRegister);
  end;
  TPicRegister_list = specialize TFPGObjectList<TPicRegister>; //lista de registros

  { TPicRegisterBit }
  {Objeto que sirve para modelar a un bit del PIC (una dirección de memoria, usada
   para un fin particular)}
  TPicRegisterBit = class
  private
    function Getbank: byte;
    function Getoffs: byte;
  public
    addr   : word;      //Dirección absoluta: $000 a $1FF
    bit    : byte;      //bit del registro
    assigned: boolean;  //indica si tiene una dirección física asignada
    used   : boolean;   //Indica si está usado.
    typ    : TPicRegType; //Tipo de registro
  public
    property offs: byte read Getoffs;   //Desplazamiento en memoria
    property bank: byte read Getbank;   //Banco del registro
    procedure Assign(srcReg: TPicRegisterBit);
  end;
  TPicRegisterBit_list = specialize TFPGObjectList<TPicRegisterBit>; //lista de registros

  //Categorías de tipos
  TxpCatType = (
    tctAtomic,  //Tipo básico
    tctArray,   //Arreglo de otro tipo
    tctPointer, //Puntero de otro tipo (Puntero corto, hasta la dirección $FF)
    tctRecord   //Registro de varios campos
  );


implementation

{ TPicRegister }
procedure TPicRegister.Assign(srcReg: TPicRegister);
begin
  addr    := srcReg.addr;
  assigned:= srcReg.assigned;
  used    := srcReg.used;
  typ     := srcReg.typ;
end;
{ TPicRegisterBit }
function TPicRegisterBit.Getbank: byte;
begin
  Result := addr >> 7;
end;
function TPicRegisterBit.Getoffs: byte;
begin
  Result := addr and $7F;  //devuelve dirección
  //Tal vez sería mejor:
  //Result := hi(addr << 1);  //devuelve dirección
end;
procedure TPicRegisterBit.Assign(srcReg: TPicRegisterBit);
begin
  addr    := srcReg.addr;
  bit     := srcReg.bit;
  assigned:= srcReg.assigned;
  used    := srcReg.used;
  typ     := srcReg.typ;
end;

end.

