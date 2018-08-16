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

type  //tipos enumerados

  //Grupos de tipo de datos
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

  //Almacenamiento de Operando
  TStoOperand = (
    stConst =%000,   {El operando es una Constante y por lo tanto su valor se almacena
                      directamente en el operando sin usar memoria del PIC. Incluyendo
                      expresiones de constantes evaluadas.}
    stVariab=%001,   {El operando es una Variable simple (atómica), y tampoco ocupa
                      espacio en la memoria física, sino que solo se guarda su dirección
                      (y número de bit para el caso de los tipos boolean o bit).}
    stExpres=%010,   {El operando es una Expresión, por lo general es el resultado de
                      algún cálculo entre variables y constantes. (incluyendo el resulatdo
                      de a una función). Se valor está siempre en los RT}
    stVarRefVar=%011,{El operando es la referencia a una variable, y esta referencia se
                      calcula en base a otras variables. No ocupa espacio a memoria,
                      porque su dirección real, se puede calcular, con parámetros
                      constantes (dirección, desplazamiento, y número de bit).}
    stVarRefExp=%100 {El operando es la referencia a una variable, y esta referencia se
                      encuentra en los RT. Para obtener la dirección real de la variable
                      se debe calcular primero la dirección, usando el valor de los RT y
                      el desplazamiento, y número de bit}
  );
  {Almacenamiento combinado para una ROB. Se construye para poder representar dos valores
  de TStoOperand en una solo valor byte (juntando sus bits), para facilitar el uso de un
  CASE ... OF}
  TStoOperandsROB =(
    stConst_Const      = %000000,
    stConst_Variab     = %000001,
    stConst_Expres     = %000010,
    stConst_VarRefVar  = %000011,
    stConst_VarRefExp  = %000100,

    stVariab_Const     = %001000,
    stVariab_Variab    = %001001,
    stVariab_Expres    = %001010,
    stVariab_VarRefVar = %001011,
    stVariab_VarRefExp = %001100,

    stExpres_Const     = %010000,
    stExpres_Variab    = %010001,
    stExpres_Expres    = %010010,
    stExpres_VarRefVar = %010011,
    stExpres_VarRefExp = %010100,

    stVarRefVar_Const     = %011000,
    stVarRefVar_Variab    = %011001,
    stVarRefVar_Expres    = %011010,
    stVarRefVar_VarRefVar = %011011,
    stVarRefVar_VarRefExp = %011100,

    stVarRefExp_Const     = %100000,
    stVarRefExp_Variab    = %100001,
    stVarRefExp_Expres    = %100010,
    stVarRefExp_VarRefVar = %100011,
    stVarRefExp_VarRefExp = %100100
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

