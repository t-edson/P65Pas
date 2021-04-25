{
XpresElementsPIC
================
Definitions and implementation of the AST (Abstract Syntax Tree) structure.
This unit is based in the unit XpresElements from the framework Xpres, and is adapted
to the 6502 CPU architecture and to the Pascal dialect used here.

                                                       By Tito Hinostroza.
}
unit XpresAST;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, LCLProc, XpresElemP65, LexPas;
type  //Abstract Syntax Tree
  {Represent the state of a search with FindFirst-FindNext}
  TxpFindState = object
    Name  : string;
    Node  : TxpElement;
    Idx   : integer;
    inUnit: boolean;
  end;

  { TXpTreeElements }
  {Árbol de elementos. Se usa para el árbol de sintaxis y de directivas. Aquí es
  donde se guardará la referencia a todas los elementos (variables, constantes, ..)
  creados.
  Este árbol se usa también como para resolver nombres de elementos, en una estructura
  en arbol.}
  TXpTreeElements = class
  public  //Tree definition/events
    main      : TEleProg;  //Root node
    curNode   : TxpElement;  //Reference to current node
    curCodCont: TxpEleCodeCont;  //Reference to current body, constant or variable node opened.
    OnFindElement: procedure(elem: TxpElement) of object;  //Evento de búsqueda
  public  //Containers
    AllCons  : TEleConsDecs;
    AllVars  : TEleVarDecs;
    AllUnits : TEleUnits;
    AllFuncs : TxpEleFuns;
    AllTypes : TEleTypeDecs;
    procedure Clear;
    procedure RefreshAllUnits;
  public  //Filling the tree
    procedure AddElement(elem: TxpElement; position: integer = - 1);
    procedure AddElementAndOpen(elem: TxpElement);
    procedure AddElementToParent(elem: TxpElement; AtBegin: boolean);
    procedure OpenElement(elem: TxpElement);
    procedure CloseElement;
    procedure DeleteTypeNode(parent: TxpElement; typNode: TEleTypeDec);
    procedure ChangeParentTo(newparent, elem: TxpElement; position: integer = - 1);
    procedure InsertParentTo(newparent, elem: TxpElement);
    function AddElementBodyAndOpen(srcPos: TSrcPos): TEleBody;
    function AddElementConsAndOpen(srcPos: TSrcPos; cname: string;
      ctype: TEleTypeDec): TEleConsDec;
    function AddElementVarAndOpen(srcPos: TSrcPos; vname: string;
      vtype: TEleTypeDec): TEleVarDec;
    function AddElementTypeAndOpen(srcPos: TSrcPos; tname: string; tsize: integer;
      catType: TxpCatType; group: TTypeGroup): TEleTypeDec;
    function AddElementBlockAndOpen(srcPos: TSrcPos): TEleBlock;
    function AddElementSentAndOpen(srcPos: TSrcPos; sntType: TxpSentence): TxpEleSentence;
  public  //Element resolution (FindFirst() - FindNext())
    curFind: TxpFindState; //State variables for searching
    function FindFirst(const name: string): TxpElement;
    function FindNext: TxpElement;
    function FindNextFuncName: TEleFunBase;
    function FindFirstType(const name: string): TEleTypeDec;
    function FindNextType: TEleTypeDec;
    function FindVar(varName: string): TEleVarDec;
    function FindType(typName: string): TEleTypeDec;
  public  //Searching/Identify
    function LastNode: TxpElement;
    function BodyNode: TEleBody;
    function CurNodeName: string;
    function CurCodeContainer: TEleProgFrame;
    function ExistsArrayType(itemType: TEleTypeDec; nEle: integer;
                             out typFound: TEleTypeDec): boolean;
    function ExistsPointerType(ptrType: TEleTypeDec;
                             out typFound: TEleTypeDec): boolean;
    function GetElementBodyAt(posXY: TPoint): TEleBody;
    function GetElementAt(posXY: TPoint): TxpElement;
    function GetElementCalledAt(const srcPos: TSrcPos): TxpElement;
    function GetELementDeclaredAt(const srcPos: TSrcPos): TxpElement;
    function FunctionExistInCur(funName: string; const pars: TxpParFuncArray
      ): boolean;
  public  //Debug
    procedure print();  //Show the AST
  public  //Constructor and destructror
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TXpTreeElements }
procedure TXpTreeElements.Clear;
begin
  main.elements.Clear;  //esto debe hacer un borrado recursivo
  main.Clear;

  curNode := main;      //retorna al nodo principal
  //ELimina lista internas
  AllCons.Clear;
  AllVars.Clear;
  AllUnits.Clear;
  AllFuncs.Clear;
  AllTypes.Clear;
end;
procedure TXpTreeElements.RefreshAllUnits;
var
  ele : TxpElement;
begin
  AllUnits.Clear;   //por si estaba llena
  for ele in main.elements do begin
    if ele.idClass = eleUnit then begin
       AllUnits.Add( TEleUnit(ele) );
    end;
  end;
end;
//Filling the tree
procedure TXpTreeElements.AddElement(elem: TxpElement; position: integer = -1);
{Add a new element to the current node. Commonly elements are add at the end of the list
unless "position" is specified.
This is the unique entry point to add elements to the Syntax Tree.}
begin
  //Add the node
  if position<>-1 then curNode.AddElement(elem, position)
  else curNode.AddElement(elem);
//  if OnAddElement<>nil then OnAddElement(elem);
  //Update Lists
  case elem.idClass of
  eleConsDec : AllCons.Add(TEleConsDec(elem));
  eleVarDec  : begin
//debugln('<adding>' + elem.Parent.name + ',' + elem.name);
    AllVars.Add(TEleVarDec(elem));
    end;
  eleFunc    : AllFuncs.Add(TEleFun(elem)); //Declarations are now stored in AllFuncs.
  eleTypeDec : AllTypes.Add(TEleTypeDec(elem));
  //No se incluye el código de RefreshAllUnits() porque solo trabaja en el "main".
  end;
end;
procedure TXpTreeElements.AddElementAndOpen(elem: TxpElement);
{Add an element and change the current node to this new node. Open an element means
that all new nodes added, will be children of this node (The current node).
To open an element is useful when it will contain other nodes, like a function body.}
begin
  {Las funciones o procedimientos no se validan inicialmente, sino hasta que
  tengan todos sus parámetros agregados, porque pueden ser sobrecargados.}
  AddElement(elem);
  //Genera otro espacio de nombres
  if elem.elements = nil then elem.elements := TxpElements.Create(true);  //Create its list becuase it will contain other nodes.
  curNode := elem;  //Set new Current node.
end;
procedure TXpTreeElements.AddElementToParent(elem: TxpElement; AtBegin: boolean);
{Add element to the parent of the current element.}
var
  tmp: TxpElement;
begin
  tmp := curNode;  //Save currente node
  curNode := curNode.Parent;  //Set to parent
  if AtBegin then AddElement(elem, 0) else AddElement(elem);
  curNode := tmp;  //Restore position
end;
procedure TXpTreeElements.OpenElement(elem: TxpElement);
{Accede al espacio de nombres del elemento indicado.}
begin
  curNode := elem;  //empieza a trabajar en esta lista
end;
procedure TXpTreeElements.CloseElement;
{Close the current node and returns to the parent node.}
begin
  if curNode=curCodCont then begin //We are closing a body
    curCodCont := nil;
  end;
  if curNode.Parent<>nil then
    curNode := curNode.Parent;
end;
procedure TXpTreeElements.DeleteTypeNode(parent: TxpElement; typNode: TEleTypeDec);
{Delete a node of typw }
begin
  parent.elements.Remove(typNode);
  AllTypes.Remove(typNode);
end;
procedure TXpTreeElements.ChangeParentTo(newparent, elem: TxpElement; position: integer = -1);
{Change the current parent of "elem". The element "elem" is reinserted in the
new parent "newparent" at the position "position".}
var
  parent: TxpElement;
begin
  parent := elem.Parent;
  parent.elements.Extract(elem);  //Doesn't free "ele". No need to update lists (AllCons, AllVars, ...) because it will be reinserted.
  newparent.AddElement(elem, position);  //Reinsert here
end;
procedure TXpTreeElements.InsertParentTo(newparent, elem: TxpElement);
{Set "newparent" as parent to the element "elem" making it descend one level.
Doesn't change current node.}
var
  i: Integer;
  parent, tmp: TxpElement;
begin
  parent := elem.Parent;
  i := parent.elements.IndexOf(elem);  //Position of "elem"
  parent.elements.Extract(elem);  //Doesn't free "elem". No need to update lists (AllCons, AllVars, ...) because it will be reinserted.

  //Insert in "newparent" in the same position of "elem".
//  parent.AddElement(newparent, i);
//  if newparent.elements = nil then newparent.elements := TxpElements.Create(true);
  tmp := curNode;  //Save currente node
  curNode := parent;  //Set to parent
  AddElement(newparent, i);  //Use AddElement() to mantain the unique point for Adding nodes.
  if newparent.elements = nil then newparent.elements := TxpElements.Create(true);
  curNode := tmp;  //Restore position

  //Reinsert "elem" as child of newparent
  newparent.AddElement(elem, 0);
end;
function TXpTreeElements.AddElementBodyAndOpen(srcPos: TSrcPos): TEleBody;
{Similar to AddElementAndOpen() but create and open a Body node. Returns the Body created.
This function must be used always when creating a Body, because it mantains updated the
variable "curBody" that is used to resolve names.}
begin
  Result := TEleBody.Create;
  Result.name := TIT_BODY_ELE;
  Result.srcDec := srcPos;
  AddElementAndOpen(Result);
  curCodCont := Result;  //Update current Code container
end;
function TXpTreeElements.AddElementConsAndOpen(srcPos: TSrcPos; cname: string;
                         ctype: TEleTypeDec): TEleConsDec;
begin
  Result := TEleConsDec.Create;
  Result.name   := cname;
  Result.typ    := ctype;   //Set reference to type.
  Result.srcDec := srcPos;
  AddElementAndOpen(Result);
  curCodCont := Result;  //Update current Code container
end;
function TXpTreeElements.AddElementVarAndOpen(srcPos: TSrcPos; vname: string;
                         vtype: TEleTypeDec): TEleVarDec;
begin
  Result := TEleVarDec.Create;
  Result.name   :=vname;
  Result.typ    := vtype;   //fija  referencia a tipo
  Result.srcDec := srcPos;
  AddElementAndOpen(Result);
  curCodCont := Result;  //Update current Code container
end;
function TXpTreeElements.AddElementTypeAndOpen(srcPos: TSrcPos; tname: string;
  tsize: integer; catType: TxpCatType; group: TTypeGroup): TEleTypeDec;
begin
  Result := TEleTypeDec.Create;
  Result.name    := tname;
  Result.srcDec  := srcPos;
  Result.size    := tsize;
  Result.catType := catType;
  Result.group   := group;
  AddElementAndOpen(Result);  //Open type
  curCodCont := Result;  //Update current Code container
end;
function TXpTreeElements.AddElementBlockAndOpen(srcPos: TSrcPos): TEleBlock;
begin
  Result := TEleBlock.Create;
  Result.name := 'block';
  Result.srcDec := srcPos;
  AddElementAndOpen(Result);
end;
function TXpTreeElements.AddElementSentAndOpen(srcPos: TSrcPos; sntType: TxpSentence): TxpEleSentence;
begin
  Result := TxpEleSentence.Create;
  //Result.name := 'sent';
  Result.srcDec := srcPos;
  Result.sntType := sntType;
  AddElementAndOpen(Result);
end;
//Element resolution
function TXpTreeElements.FindNext: TxpElement;
{Realiza una búsqueda recursiva en el nodo "curFindNode", a partir de la posición,
"curFindIdx", hacia "atrás", el elemento con nombre "curFindName". También implementa
la búsqueda en unidades.
Esta rutina es quien define la resolución de nombres (alcance) en el lenguaje.}
var
  elem: TxpElement;
begin
//  debugln(' Explorando nivel: [%s] en pos: %d', [curFindNode.name, curFindIdx - 1]);
  repeat
    curFind.Idx := curFind.Idx - 1;  //Siempre salta a la posición anterior
    if curFind.Idx<0 then begin
      //No encontró, en ese nivel. Hay que ir más atrás. Pero esto se resuelve aquí.
      if curFind.Node.Parent = nil then begin
        //No hay nodo padre. Este es el nodo Main
        Result := nil;
        exit;  //aquí termina la búsqueda
      end;
      //Busca en el espacio padre
      curFind.Idx := curFind.Node.Index;  //posición actual
      curFind.Node := curFind.Node.Parent;  //apunta al padre
      if curFind.inUnit then curFind.inUnit := false;   //Sale de una unidad
      Result := FindNext();  //Recursividad IMPORTANTE: Usar paréntesis.
//      Result := nil;
      exit;
    end;
    //Verifica ahora este elemento
    elem := curFind.Node.elements[curFind.Idx];
    if curFind.inUnit and (elem.location = locImplement) then begin
      //No debería ser accesible
      continue;
    end;
    //Genera evento para indicar que está buscando.
    if OnFindElement<>nil then OnFindElement(elem);
    //Compara
    if (curFind.Name = '') or (elem.uname = curFind.Name) then begin
      //Encontró en "findSt.curFindIdx"
      Result := elem;
      //La siguiente búsqueda empezará en "findSt.curFindIdx-1".
      exit;
    end else begin
      //No tiene el mismo nombre, a lo mejor es una unidad
      if (elem.idClass = eleUnit) and not curFind.inUnit then begin   //Si es el priemr nodo de unidad
        //¡Diablos es una unidad! Ahora tenemos que implementar la búsqueda.
        curFind.inUnit := true;   //Marca, para que solo busque en un nivel
        curFind.Idx := elem.elements.Count;  //para que busque desde el último
        curFind.Node := elem;  //apunta a la unidad
        Result := FindNext();  //Recursividad IMPORTANTE: Usar paréntesis.
        if Result <> nil then begin  //¿Ya encontró?
          exit;  //Sí. No hay más que hacer aquí
        end;
        //No encontró. Hay que seguir buscando
      end;
    end;
  until false;
end;
function TXpTreeElements.FindFirst(const name: string): TxpElement;
{Routine to resolve an identifier inside the SyntaxTree, following the scope rules for
identifiers of the Pascal syntax (first the current space and then the parents spaces).
If found returns the reference to the element otherwise returns NIL.
If "name" is empty string, all the elements, of the Syntax Tree, will be scanned.}
begin
  //Busca recursivamente, a partir del espacio actual
  curFind.Name := UpCase(name);  //This value won't change in all the search
  curFind.inUnit := false;       //Set flag
  if curCodCont <> nil then begin
    {Para los cuerpos de procedimientos o de programa, se debe explorar hacia atrás a
    partir de la posición del nodo actual.}
    curFind.Idx := curCodCont.Index;   //Set index for searching. Here is the body index.
    curFind.Node := curCodCont.Parent; //Set the parent node as the node to search.
    Result := FindNext;             //Start search
  end else begin
    {La otras forma de resolución, debe ser:
    1. Declaración de constantes, cuando se definen como expresión con otras constantes
    2. Declaración de variables, cuando se definen como ABSOLUTE <variable>
    3. Declaración de tipos, cuando se refiere a otros tipos o cuando se define como objeto.
    }
    curFind.Node := curNode;  //Actualiza nodo actual de búsqueda
    {Formalmente debería apuntar a la posición del elemento actual, pero se deja
    apuntando a la posición final, sin peligro, porque, la resolución de nombres para
    constantes y variables, se hace solo en la primera pasada (con el árbol de sintaxis
    llenándose.)}
    curFind.Idx := curNode.elements.Count;
    //Busca
    Result := FindNext;
  end;
end;
function TXpTreeElements.FindNextFuncName: TEleFunBase;
{Scans recursively toward root, in the syntax tree, until find a function element with
the same name provided in a previous call to FindFirst.
Must be called after calling FindFirst() with the name of the function.
If not found, returns NIL.}
var
  ele: TxpElement;
begin
  repeat
    ele := FindNext;
  until (ele=nil) or (ele.idClass in [eleFunc, eleFuncDec]);
  //Puede que haya encontrado la función o no
  if ele = nil then exit(nil);  //No encontró
  Result := TEleFunBase(ele);   //devuelve como función
end;
function TXpTreeElements.FindFirstType(const name: string): TEleTypeDec;
{Starts the search for a element type in the syntax Tree.}
var
  ele: TxpElement;
begin
  ele := FindFirst(name);
  while (ele<>nil) and (ele.idClass <> eleTypeDec) do begin
    ele := FindNext;
  end;
  if ele = nil then exit(nil) else exit( TEleTypeDec(ele) );
end;
function TXpTreeElements.FindNextType: TEleTypeDec;
{Scan recursively toward root, in the syntax tree, until find a type element.
Must be called after calling FindFirst(). If not found, returns NIL.}
var
  ele: TxpElement;
begin
  repeat
    ele := FindNext;
  until (ele=nil) or (ele.idClass = eleTypeDec);
  //Puede que haya encontrado la función o no
  if ele = nil then exit(nil);  //No encontró
  Result := TEleTypeDec(ele);   //devuelve como función
end;
function TXpTreeElements.FindVar(varName: string): TEleVarDec;
{Busca una variable con el nombre indicado en el espacio de nombres actual}
var
  ele : TxpElement;
  uName: String;
begin
  uName := upcase(varName);
  for ele in curNode.elements do begin
    if (ele.idClass = eleVarDec) and (ele.uname = uName) then begin
      Result := TEleVarDec(ele);
      exit;
    end;
  end;
  exit(nil);
end;
function TXpTreeElements.FindType(typName: string): TEleTypeDec;
{Find a type, by name, in the current element of the Synyax Tree.}
var
  ele: TxpElement;
begin
  ele := FindFirst(typName);
//  while (ele<>nil) and (ele.idClass <> eleType) do begin
//    ele := FindNext;
//  end;
  if ele = nil then exit(nil);
  if ele.idClass = eleTypeDec then exit( TEleTypeDec(ele) ) else exit(nil);
end;
//Searching/Identification
function TXpTreeElements.LastNode: TxpElement;
{Devuelve una referencia al último nodo de "main"}
begin
  Result := main.LastNode;
end;
function TXpTreeElements.BodyNode: TEleBody;
{Devuelve la referencia al cuerpo principal del programa.}
begin
  Result := main.BodyNode;
end;
function TXpTreeElements.CurNodeName: string;
{Devuelve el nombre del nodo actual}
begin
  Result := curNode.name;
end;
function TXpTreeElements.CurCodeContainer: TEleProgFrame;
{Devuelve una referencia al Contenedor de Código actual. Si no lo identifica,
devuelve NIL}
begin
  case curNode.idClass of
  eleFunc, eleProg, eleUnit: begin
    {Este es un caso directo, porque estamos directamente en un contenedor de código.
    No es común proque en este ámbito solo están las declaraciones, no el código}
    exit( TEleProgFrame(curNode) );
  end;
  eleBody: begin
    {Este es el caso mas común porque aquí si estamos dentro de un bloque que incluye
    código.}
    //Se supone que nunca debería fallar, porque un Body siempre pertenece a un CodeCont
    exit( TEleProgFrame(curNode.Parent) );
  end;
  else
    exit(nil);
  end;
end;
function TXpTreeElements.ExistsArrayType(itemType: TEleTypeDec; nEle: integer;
  out typFound: TEleTypeDec): boolean;
{Finds an array type declaration, accesible from the current position in the syntax tree.
If found, returns TRUE and the type reference in "typFound".}
begin
  typFound := FindFirstType('');  //Any name
  while (typFound <> nil) and not typFound.IsArrayOf(itemType, nEle) do begin
    typFound := FindNextType;
  end;
  //Verify result
  Result := typFound <> nil;
end;
function TXpTreeElements.ExistsPointerType(ptrType: TEleTypeDec; out
  typFound: TEleTypeDec): boolean;
{Finds a pointer type declaration, accesible from the current position in the syntax tree.
If found, returns TRUE and the type reference in "typFound".}
begin
  typFound := FindFirstType('');  //Any name
  while (typFound <> nil) and not typFound.IsPointerTo(ptrType) do begin
    typFound := FindNextType;
  end;
  //Verify result
  Result := typFound <> nil;
end;
function TXpTreeElements.GetElementBodyAt(posXY: TPoint): TEleBody;
{Busca en el árbol de sintaxis, dentro del nodo principal, y sus nodos hijos, en qué
cuerpo (nodo Body) se encuentra la coordenada del cursor "posXY".
Si no encuentra, devuelve NIL.}
var
  res: TEleBody;

  procedure ExploreForBody(nod: TxpElement);
  var
    ele : TxpElement;
  begin
    if nod.elements<>nil then begin
      //Explora a todos sus elementos
      for ele in nod.elements do begin
        if ele.idClass = eleBody then begin
          //Encontró un Body, verifica
          if ele.posXYin(posXY) then begin
            res := TEleBody(ele);   //guarda referencia
            exit;
          end;
        end else begin
          //No es un body, puede ser un elemento con nodos hijos
          if ele.elements<>nil then
            ExploreForBody(ele);  //recursivo
        end;
      end;
    end;
  end;
begin
  //Realiza una búsqueda recursiva.
  res := nil;   //Por defecto
  ExploreForBody(main);
  Result := res;
end;
function TXpTreeElements.GetElementAt(posXY: TPoint): TxpElement;
{Busca en el árbol de sintaxis, en qué nodo Body se encuentra la coordenada del
cursor "posXY". Si no encuentra, devuelve NIL.}
var
  res: TEleBody;

  procedure ExploreFor(nod: TxpElement);
  var
    ele : TxpElement;
  begin
    if nod.elements<>nil then begin
      //Explora a todos sus elementos
      for ele in nod.elements do begin
//debugln('nod='+ele.Path);
        if ele.elements<>nil then begin
          //Tiene nodos interiors.
          ExploreFor(ele);  //Explora primero en los nodos hijos
          if res<>nil then exit;  //encontró
        end;
        //No encontró en los hijos, busca en el mismo nodo
        if ele.posXYin(posXY) then begin
          res := TEleBody(ele);   //guarda referencia
          if res<>nil then exit;  //encontró
        end;
      end;
    end;
  end;
begin
  //Realiza una búsqueda recursiva.
  res := nil;   //Por defecto
  ExploreFor(main);
  Result := res;
end;
function TXpTreeElements.GetElementCalledAt(const srcPos: TSrcPos): TxpElement;
{Explora los elementos, para ver si alguno es llamado desde la posición indicada.
Si no lo encuentra, devueleve NIL.}
var
  res: TxpElement;

  procedure ExploreForCall(nod: TxpElement);
  var
    ele : TxpElement;
  begin
    if nod.elements<>nil then begin
      //Explora a todos sus elementos
      for ele in nod.elements do begin
        if ele.IsCAlledAt(srcPos) then begin
            res := ele;   //guarda referencia
            exit;
        end else begin
          //No es un body, puede ser un eleemnto con nodos hijos
          if ele.elements<>nil then
            ExploreForCall(ele);  //recursivo
        end;
      end;
    end;
  end;
begin
  //Realiza una búsqueda recursiva.
  res := nil;   //Por defecto
  ExploreForCall(main);
  Result := res;
end;
function TXpTreeElements.GetELementDeclaredAt(const srcPos: TSrcPos): TxpElement;
{Explora los elementos, para ver si alguno es declarado en la posición indicada.}
var
  res: TxpElement;

  procedure ExploreForDec(nod: TxpElement);
  var
    ele : TxpElement;
  begin
    if nod.elements<>nil then begin
      //Explora a todos sus elementos
      for ele in nod.elements do begin
        if ele.IsDeclaredAt(srcPos) then begin
            res := ele;   //guarda referencia
            exit;
        end else begin
          //No es un body, puede ser un eleemnto con nodos hijos
          if ele.elements<>nil then
            ExploreForDec(ele);  //recursivo
        end;
      end;
    end;
  end;
begin
  //Realiza una búsqueda recursiva.
  res := nil;   //Por defecto
  ExploreForDec(main);
  Result := res;
end;
function TXpTreeElements.FunctionExistInCur(funName: string;
  const pars: TxpParFuncArray): boolean;
{Indica si la función definida por el nombre y parámetros, existe en el nodo actual.
La búsqueda se hace bajo la consideración de que dos funciones son iguales si tiene el
mismo nombre y los mismos tipos de parámetros.}
var
  ele: TxpElement;
  uname: String;
  funbas: TEleFunBase;
begin
  uname := Upcase(funName);
  for ele in curNode.elements do begin
    if ele.uname = uname then begin
      //hay coincidencia de nombre
      if ele.idClass in [eleFunc, eleFuncDec] then begin
        funbas := TEleFunBase(ele);
        //para las funciones, se debe comparar los parámetros
        if funbas.SameParamsType(pars) then begin
          exit(true);
        end;
      end else begin
        //Ssi tiene el mismo nombre que cualquier otro elemento, es conflicto
        exit(true);
      end;
    end;
  end;
  exit(false);
end;
//Debug
procedure TXpTreeElements.print();
{Función de ayuda a la depuración;}
  procedure printNode(nod: TxpElement; level: integer);
  var
    ele: TxpElement;
    expr: TEleExpress;
  begin
    for ele in nod.elements do begin
      if ele.idClass = eleExpress then begin
        expr := TEleExpress(ele);
        debugln(Space(level*2)+'ele='+expr.Name + '('+expr.StoAsStr+')');
      end else begin
        debugln(Space(level*2)+'ele='+ele.Name {+ '('+ele.Sto+')'});
      end;
      printNode(ele, level+1);
    end;
  end;
var
  ele : TxpElement;
begin
  debugln('AST('+IntToStr(main.elements.Count)+') = ');
  for ele in main.elements do begin
    debugln('  ele='+ele.Name);
    if ele.name = 'Body' then printNode(ele, 2);
  end;
  debugln('');
end;
//Constructor y destructor
constructor TXpTreeElements.Create;
begin
  main:= TEleProg.Create;  //No debería
  main.name := 'Main';
  main.elements := TxpElements.Create(true);  //debe tener lista
  AllCons  := TEleConsDecs.Create(false);   //Crea lista
  AllVars  := TEleVarDecs.Create(false);   //Crea lista
  AllFuncs := TxpEleFuns.Create(false);   //Crea lista
  AllUnits := TEleUnits.Create(false);  //Crea lista
  AllTypes := TEleTypeDecs.Create(false);  //Crea lista
  curNode := main;  //empieza con el nodo principal como espacio de nombres actual
end;
destructor TXpTreeElements.Destroy;
begin
  main.Destroy;
  AllTypes.Destroy;
  AllUnits.Destroy;
  AllFuncs.Free;
  AllVars.Free;    //por si estaba creada
  AllCons.Free;
  inherited Destroy;
end;
initialization
  //crea el operador NULL
  typNull := TEleTypeDec.Create;
  typNull.name := 'null';

finalization
  typNull.Destroy;
end.

