{
XpresElements
=============
Definiciones para el manejo de los elementos del compilador: funciones, constantes, variables.
Todos estos elementos se deberían almacenar en una estrucutura de arbol.
Por Tito Hinostroza.
 }
unit XpresElements;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, XpresTypes;

type
  //Tipos de elementos del lenguaje
  TxpElemType = (eltNone,  //sin tipo
                 eltMain,  //programa principal
                 eltVar,   //variable
                 eltFunc,  //función
                 eltCons,  //constante
                 eltType   //tipo
                );
  TFindFuncResult = (TFF_NONE, TFF_PARTIAL, TFF_FULL);

  TxpElement = class;
  TxpElements = specialize TFPGObjectList<TxpElement>;

  { TxpElement }
  //Clase base para todos los elementos
  TxpElement = class
  public
  private
//    amb  : string;      //ámbito o alcance de la constante
  public
    name : string;      //nombre de la variable
    typ  : TType;       //tipo del elemento, si aplica
    Parent: TxpElement;    //referencia al padre
    elemType: TxpElemType; //no debería ser necesario
    Used: integer;      //veces que se usa este nombre
    elements: TxpElements;  //referencia a nombres anidados, cuando sea función
    function AddElement(elem: TxpElement): TxpElement;
    function DuplicateIn(list: TObject): boolean; virtual;
    function FindIdxElemName(const eName: string; var idx0: integer): boolean;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TVarAddr = word;
  TVarBank = byte;

  //Clase para modelar al bloque principal
  { TxpEleMain }
  TxpEleMain = class(TxpElement)
    constructor Create; override;
  end;

  { TxpEleType }
  //Clase para modelar a los tipos definidos por el usuario
  { Es diferente a XpresTypes: TType, aunque no se ha hecho un anaálisis profundo }
  TxpEleType= class(TxpElement)
    //valores de la constante
    constructor Create; override;
  end;
  TxpEleTypes= specialize TFPGObjectList<TxpEleType>; //lista de variables

  { TxpEleCon }
  //Clase para modelar a las constantes
  TxpEleCon = class(TxpElement)
    //valores de la constante
    val : TConsValue;
    constructor Create; override;
  end;
  TxpEleCons = specialize TFPGObjectList<TxpEleCon>; //lista de constantes

  { TxpEleVar }
  //Clase para modelar a las variables
  TxpEleVar = class(TxpElement)
    //direción física. Usado para implementar un compilador
    addr: TVarAddr;
    bank: TVarBank;   //banco o segmento. Usado solo en algunas arquitecturas
    //Campos usados para implementar el intérprete sin máquina virtual
    //valores de la variable.
    valInt  : Int64;    //valor en caso de que sea un entero
    valUInt : Int64;    //valor en caso de que sea un entero sin signo
    valFloat: extended; //Valor en caso de que sea un flotante
    valBool  : Boolean;  //valor  en caso de que sea un booleano
    valStr  : string;    //valor  en caso de que sea una cadena
    constructor Create; override;
  end;
  TxpEleVars = specialize TFPGObjectList<TxpEleVar>; //lista de variables

  { TxpEleFun }
  //Clase para almacenar información de las funciones
  TxpEleFun = class;
  TProcExecFunction = procedure(fun: TxpEleFun) of object;  //con índice de función
  TxpEleFun = class(TxpElement)
  public
    pars: array of TType;  //parámetros de entrada
  public
    //direción física. Usado para implementar un compilador
    adrr: integer;  //dirección física
    //Campos usados para implementar el intérprete sin máquina virtual
    proc: TProcExecFunction;  //referencia a la función que implementa
    posF: TPoint;    //posición donde empieza la función en el código fuente
    procedure ClearParams;
    procedure CreateParam(parName: string; typ0: TType);
    function SameParams(Fun2: TxpEleFun): boolean;
    function ParamTypesList: string;
    function DuplicateIn(list: TObject): boolean; override;
    constructor Create; override;
  end;
  TxpEleFuns = specialize TFPGObjectList<TxpEleFun>;

  { TXpTreeElements }
  {Árbol de elementos. Solo se espera que haya una instacia de este objeto. Aquí es
  donde se guardará la referencia a todas los elementos (variables, constantes, ..)
  creados.
  Este árbol se usa también como un equivalente al NameSpace, porque se usa para
  buscar los nombres de los elementos, en una estructura en arbol}
  TXpTreeElements = class
  private
    curNode : TxpElement;  //referencia al nodo actual
    vars    : TxpEleVars;
    //variables de estado para la búsqueda con FindFirst() - FindNext()
    curFindName: string;
    curFindNode: TxpElement;
    curFindIdx: integer;
  public
    main    : TxpEleMain;  //nodo raiz
    procedure Clear;
    function AllVars: TxpEleVars;
    function CurNodeName: string;
    //funciones para llenado del arbol
    function AddElement(elem: TxpElement; verifDuplic: boolean=true): boolean;
    procedure OpenElement(elem: TxpElement);
    function ValidateCurElement: boolean;
    procedure CloseElement;
    //Métodos para identificación de nombres
    function FindFirst(const name: string): TxpElement;
    function FindNext: TxpElement;
    function FindFuncWithParams(const funName: string; const func0: TxpEleFun;
      var fmatch: TxpEleFun): TFindFuncResult;
    function FindVar(varName: string): TxpEleVar;
  public  //constructor y destructror
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TxpElement }
function TxpElement.AddElement(elem: TxpElement): TxpElement;
{Agrega un elemento hijo al elemento actual. Devuelve referencia. }
begin
  elem.Parent := self;  //actualzia referencia
  elements.Add(elem);   //agrega a la lista de nombres
  Result := elem;       //no tiene mucho sentido
end;
function TxpElement.DuplicateIn(list: TObject): boolean;
{Debe indicar si el elemento está duplicado en la lista de elementos proporcionada.}
var
  uName: String;
  ele: TxpElement;
begin
  uName := upcase(name);
  for ele in TxpElements(list) do begin
    if upcase(ele.name) = uName then begin
      exit(true);
    end;
  end;
  exit(false);
end;
function TxpElement.FindIdxElemName(const eName: string; var idx0: integer): boolean;
{Busca un nombre en su lista de elementos. Inicia buscando en idx0, hasta el final.
 Si encuentra, devuelve TRUE y deja en idx0, la posición en donde se encuentra.}
var
  i: Integer;
  uName: String;
begin
  uName := upcase(eName);
  //empieza la búsqueda en "idx0"
  for i := idx0 to elements.Count-1 do begin
    { TODO : Tal vez sería mejor usar el método de búsqueda interno de la lista,
     que es más rápido, pero trabaja con listas ordenadas. }
    if upCase(elements[i].name) = uName then begin
      //sale dejando idx0 en la posición encontrada
      idx0 := i;
      exit(true);
    end;
  end;
  exit(false);
end;
constructor TxpElement.Create;
begin
  elemType := eltNone;
end;
destructor TxpElement.Destroy;
begin
  elements.Free;  //por si contenía una lista
  inherited Destroy;
end;

{ TxpEleMain }
constructor TxpEleMain.Create;
begin
  elemType:=eltMain;
  Parent := nil;  //la raiz no tiene padre
end;

{ TxpEleCon }
constructor TxpEleCon.Create;
begin
  elemType:=eltCons;
end;

{ TxpEleVar }
constructor TxpEleVar.Create;
begin
  elemType:=eltVar;
end;

{ TxpEleType }
constructor TxpEleType.Create;
begin
  elemType:=eltType;
end;

{ TxpEleFun }
procedure TxpEleFun.ClearParams;
//Elimina los parámetros de una función
begin
  setlength(pars,0);
end;
procedure TxpEleFun.CreateParam(parName: string; typ0: TType);
//Crea un parámetro para la función
var
  n: Integer;
begin
  //agrega
  n := high(pars)+1;
  setlength(pars, n+1);
  pars[n] := typ0;  //agrega referencia
end;
function TxpEleFun.SameParams(Fun2: TxpEleFun): boolean;
{Compara los parámetros de la función con las de otra. Si tienen el mismo número
de parámetros y el mismo tipo, devuelve TRUE.}
var
  i: Integer;
begin
  Result:=true;  //se asume que son iguales
  if High(pars) <> High(Fun2.pars) then
    exit(false);   //distinto número de parámetros
  //hay igual número de parámetros, verifica
  for i := 0 to High(pars) do begin
    if pars[i] <> Fun2.pars[i] then begin
      exit(false);
    end;
  end;
  //si llegó hasta aquí, hay coincidencia, sale con TRUE
end;
function TxpEleFun.ParamTypesList: string;
{Devuelve una lista con los nombres de los tipos de los parámetros, de la forma:
(byte, word) }
var
  tmp: String;
  j: Integer;
begin
  tmp := '';
  for j := 0 to High(pars) do begin
    tmp += pars[j].name+', ';
  end;
  //quita coma final
  if length(tmp)>0 then tmp := copy(tmp,1,length(tmp)-2);
  Result := '('+tmp+')';
end;
function TxpEleFun.DuplicateIn(list: TObject): boolean;
var
  uName: String;
  ele: TxpElement;
begin
  uName := upcase(name);
  for ele in TxpElements(list) do begin
    if ele = self then Continue;  //no se compara el mismo
    if upcase(ele.name) = uName then begin
      //hay coincidencia de nombre
      if ele.elemType = eltFunc then begin
        //para las funciones, se debe comparar los parámetros
        if SameParams(TxpEleFun(ele)) then begin
          exit(true);
        end;
      end else begin
        //si tiene el mismo nombre que cualquier otro elemento, es conflicto
        exit(true);
      end;
    end;
  end;
  exit(false);
end;
constructor TxpEleFun.Create;
begin
  elemType:=eltFunc;
end;

{ TXpTreeElements }
procedure TXpTreeElements.Clear;
begin
  main.elements.Clear;  //esto debe hacer un borrado recursivo
  curNode := main;      //retorna al nodo principal
end;
function TXpTreeElements.AllVars: TxpEleVars;
{Devuelve una lista de todas las variables usadas, incluyendo las de las funciones y
 procedimientos.}
  procedure AddVars(nod: TxpElement);
  var
    ele : TxpElement;
  begin
    if nod.elements<>nil then begin
      for ele in nod.elements do begin
        if ele.elemType = eltVar then begin
          vars.Add(TxpEleVar(ele));
        end else begin
          if ele.elements<>nil then
            AddVars(ele);  //recursivo
        end;
      end;
    end;
  end;
begin
  if vars = nil then begin  //debe estar creada la lista
    vars := TxpEleVars.Create(false);
  end else begin
    vars.Clear;   //por si estaba llena
  end;
  AddVars(curNode);
  Result := vars;
end;
function TXpTreeElements.CurNodeName: string;
{Devuelve el nombre del nodo actual}
begin
  Result := curNode.name;
end;
//funciones para llenado del arbol
function TXpTreeElements.AddElement(elem: TxpElement; verifDuplic: boolean = true): boolean;
{Agrega un elemento al nodo actual. Si ya existe el nombre del nodo, devuelve false}
begin
  Result := true;
  //Verifica si hay conflicto. Solo es necesario buscar en el nodo actual.
  if verifDuplic and elem.DuplicateIn(curNode.elements) then begin
    exit(false);  //ya existe
  end;
  //agrega el nodo
  curNode.AddElement(elem);
end;
procedure TXpTreeElements.OpenElement(elem: TxpElement);
{Agrega un elemento y cambia el nodo actual. Este método está reservado para
las funciones o procedimientos}
begin
  {las funciones o procedimientos no se validan inicialmente, sino hasta que
  tengan todos sus parámetros agregados, porque pueden ser sobrecargados.}
  curNode.AddElement(elem);
  //Genera otro espacio de nombres
  elem.elements := TxpElements.Create(true);  //su propia lista
  curNode := elem;  //empieza a trabajar en esta lista
end;
function TXpTreeElements.ValidateCurElement: boolean;
{Este método es el complemento de OpenElement(). Se debe llamar cuando ya se
 tienen creados los parámetros de la función o procedimiento, para verificar
 si hay duplicidad, en cuyo caso devolverá FALSE}
begin
  //Se asume que el nodo a validar ya se ha abierto, con OpenElement() y es el actual
  if curNode.DuplicateIn(curNode.Parent.elements) then begin  //busca en el nodo anterior
    exit(false);
  end else begin
    exit(true);
  end;
end;
procedure TXpTreeElements.CloseElement;
{Sale del nodo actual y retorna al nodo padre}
begin
  if curNode.Parent<>nil then
    curNode := curNode.Parent;
end;
//Métodos para identificación de nombres
function TXpTreeElements.FindFirst(const name: string): TxpElement;
{Busca un nombre siguiendo la estructura del espacio de nombres (primero en el espacio
 actual y luego en los espacios padres).
 Si encuentra devuelve la referencia. Si no encuentra, devuelve NIL.
Este es un ejemplo, implementar de acuerdo al lenguaje y reglas de alcance.
}
  function FindFirstIn(nod: TxpElement): TxpElement;
  var
    idx0: integer;
  begin
    curFindNode := nod;  //Busca primero en el espacio actual
    {Busca con FindIdxElemName() para poder saber donde se deja la exploración y poder
     retomarla luego con FindNext().}
    idx0 := 0;  //la primera búsqueda se hace desde el inicio
    if curFindNode.FindIdxElemName(name, idx0) then begin
      //Lo encontró, deja estado listo para la siguiente búsqueda
      curFindIdx:= idx0+1;
      Result := curFindNode.elements[idx0];
      exit;
    end else begin
      //No encontró
      if nod.Parent = nil then begin
        Result := nil;
        exit;  //no hay espacios padres
      end;
      //busca en el espacio padre
      Result := FindFirstIn(nod.Parent);  //recursividad
      exit;
    end;
  end;
begin
  curFindName := name;     //actualiza para FindNext()
  Result := FindFirstIn(curNode);
end;
function TXpTreeElements.FindNext: TxpElement;
{Continúa la búsqueda iniciada con FindFirst().}
begin
  //Implementar de acuerdo al lenguaje.
  Result := nil;
end;
function TXpTreeElements.FindFuncWithParams(const funName: string; const func0: TxpEleFun;
  var fmatch: TxpEleFun): TFindFuncResult;
{Busca una función que coincida con el nombre "funName" y con los parámetros de func0
El resultado puede ser:
 TFF_NONE   -> No se encuentra.
 TFF_PARTIAL-> Se encuentra solo el nombre.
 TFF_FULL   -> Se encuentra y coninciden sus parámetros, actualiza "fmatch".
}
var
  tmp: String;
  ele: TxpElement;
  hayFunc: Boolean;
begin
  Result := TFF_NONE;   //por defecto
  hayFunc := false;
  tmp := UpCase(funName);
  for ele in curNode.elements do begin
    if (ele.elemType = eltFunc) and (Upcase(ele.name) = tmp) then begin
      //coincidencia de nombre, compara parámetros
      hayFunc := true;  //para indicar que encontró el nombre
      if func0.SameParams(TxpEleFun(ele)) then begin
        fmatch := TxpEleFun(ele);  //devuelve ubicación
        Result := TFF_FULL;     //encontró
        exit;
      end;
    end;
  end;
  //si llego hasta aquí es porque no encontró coincidencia
  if hayFunc then begin
    //Encontró al menos el nombre de la función, pero no coincide en los parámetros
    Result := TFF_PARTIAL;
    {Construye la lista de parámetros de las funciones con el mismo nombre. Solo
    hacemos esta tarea pesada aquí, porque  sabemos que se detendrá la compilación}
{    params := '';   //aquí almacenará la lista
    for i:=idx0 to high(funcs) do begin  //no debe empezar 1n 0, porque allí está func[0]
      if Upcase(funcs[i].name)= tmp then begin
        for j:=0 to high(funcs[i].pars) do begin
          params += funcs[i].pars[j].name + ',';
        end;
        params += LineEnding;
      end;
    end;}
  end;
end;
function TXpTreeElements.FindVar(varName: string): TxpEleVar;
{Busca una variable con el nombre indicado en el espacio de nombres actual}
var
  ele : TxpElement;
  uName: String;
begin
  uName := upcase(varName);
  for ele in curNode.elements do begin
    if (ele.elemType = eltVar) and (upCase(ele.name) = uName) then begin
      Result := TxpEleVar(ele);
      exit;
    end;
  end;
  exit(nil);
end;
//constructor y destructror
constructor TXpTreeElements.Create;
begin
  main:= TxpEleMain.Create;  //No debería
  main.elements := TxpElements.Create(true);  //debe tener lista
  curNode := main;  //empieza con el nodo principal como espacio de nombres actual
end;
destructor TXpTreeElements.Destroy;
begin
  main.Destroy;
  vars.Free;    //por si estaba creada
  inherited Destroy;
end;
end.

