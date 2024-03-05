{
MiConfigUtils
===========
Por Tito Hinostroza 20/12/2016

Descripción
===========
Unidad con rutinas útiles para implementar las ventanas de configuración.
}
unit MiConfigUtils;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Graphics, Forms, ComCtrls;

type
  TlistFrames = array of TFrame;

  //Utilidades para el uso de Frames con "MiCOnfig"
  procedure HideAllFrames(form: TForm);
  procedure ShowFramePos(frm: TFrame; x, y: integer);
  //Utilidades para el uso de TTreeView con Frames con "MiCOnfig"
  function IdFromTTreeNode(node: TTreeNode): string;
  function TTreeNodeFromId(Id: string; tree: TTreeView): TTreeNode;
  function AddNodeToTreeView(tree: TTreeView; id, Caption: string): TTreeNode;
  function LinkFrameToTreeView(tree: TTreeView; id, Caption: string;
           frame: TFrame = nil): TTreeNode;
  function ShowFrameOfNode(form: Tform; node: TTreeNode; x, y: integer): boolean;

implementation

function ListOfFrames(form: TForm): TlistFrames;
//Devuelve la lista de frames del tipo TCfgFrame declarado aquí
var
  i: Integer;
  n : integer;
  f: TFrame;
begin
  SetLength(Result,0);
  for i:= 0 to form.ComponentCount-1 do begin
    if form.Components[i] is TFrame then begin
      f:=TFrame(form.Components[i]);  //obtiene referencia
      n := high(Result)+1;     //número de elementos
      setlength(Result, n+1);  //hace espacio
      Result[n] := f;          //agrega
    end;
  end;
end;
procedure HideAllFrames(form: TForm);
//Oculta todos los frames de un formulario
var
  f: TFrame;
begin
  for f in ListOfFrames(form) do
    f.visible := false;
end;
procedure ShowFramePos(frm: TFrame; x, y: integer);
//Muestra el frame en la posición indicada
begin
  frm.left:= x;
  frm.Top := y;
  frm.Visible:=true;
end;
function IdFromTTreeNode(node: TTreeNode): string;
//Returns an ID with indication of the position of a TTreeNode'.
//It has the form: 1, 1.1, 2.1.3. Only works for 3 levels.
var
  nivel: Integer;
begin
  nivel := node.Level;
  case nivel of
  0: Result := IntToStr(node.Index+1); //de un nivel
  1: Result := IntToStr(node.Parent.Index+1) + '.' +
               IntToStr(node.Index+1);
  2: Result := IntToStr(node.Parent.Parent.Index+1) + '.' +
               IntToStr(node.Parent.Index+1) + '.' +
               IntToStr(node.Index+1)
  else  //de un nivel
     Result := '';
  end;
end;
function TTreeNodeFromId(Id: string; tree: TTreeView): TTreeNode;
//Returns a TreeNode, given the ID position. If not found, returns NIL.
//Only works for 3 levels.
var
  node: TTreeNode;
begin
  for node in tree.Items do begin
    if IdFromTTreeNode(node) = Id then exit(node);
  end;
  exit(nil);
end;
function AddNodeToTreeView(tree: TTreeView; id, Caption: string): TTreeNode;
{Agrega un Frame a TTreeNode, de forma simbólica. Lo que se agrega en realidad,
es un nodo, con el nombre indicado.
El campo ID, determina la ubicación del nodo en el árbol, y es de la forma:
<nivel1>.<nivel2>.<nivel3> ... }
    function ChildCount(tv: TTreeView; nod: TTreeNode): integer;
    {Devuelve la cantidad de nodos hijos (solo en el primer nivel) de un nodo.}
    var
      node: TTreeNode;
      levChild: Integer;
    begin
      Result := 0;
      if nod = nil then levChild := 0 else levChild := nod.Level+1;
      for node in tv.Items do begin
        if (node.Level = levChild) and (node.Parent = nod) then inc(Result);
      end;
    end;
    function ChildByNumber(tv: TTreeView; nod: TTreeNode; num: integer): TTreeNode;
    {Devuelve el nodo hijo número "n" (empieza en 1) de Nod. Debe asegurarse que hay
    al menos "n" nodos hijos en el nodo indicado. De otra forma, se puede generar error.}
    var
      node: TTreeNode;
      levChild, nChild: Integer;
    begin
      nChild := 0;
      if nod = nil then levChild := 0 else levChild := nod.Level+1;
      for node in tv.Items do begin
        if (node.Level = levChild) and (node.Parent = nod) then begin
          inc(nChild);
          if nChild=num then exit(node);
        end;
      end;
      //No se encontró
      exit(nil);
    end;
    function BuscarNodoN(tv: TTreeView; raiz: TTreeNode; nNod: string): TTreeNode;
    {Busca el nodo de orden "nNod". Asegura que el nodo raiz tenga los nodos, indicados. Si no los tiene, los
    crea.}
    var
      n: LongInt;
    begin
      n := StrToInt(nNod);
      if n<=ChildCount(tv, raiz) then begin
        //No hay problema, el nodo ya existe
        Result := ChildByNumber(tv, raiz, n);
        exit;
      end else begin
        //No hay nodos hijo suficientes, hay que agregar nodos.
        while ChildCount(tv, raiz)<n do begin
          Result := tv.Items.AddChild(raiz, 'nodo');
        end;
      end;
    end;
var
  nod: TTreeNode;
  niveles: TStringList;
  niv: String;
  nodRaiz : TTreeNode;
begin
  //Ubica nodo
  nod := TTreeNodeFromId(id, tree);
  if nod=nil then begin
    //No existe el nodo. Hay que crearlo en el nivel indicado
    niveles := TStringList.Create;
    niveles.Delimiter:='.';
    niveles.DelimitedText:=id;
    nodRaiz := nil;   //inicia en nodo raiz
    for niv in niveles do begin
      nodRaiz := BuscarNodoN(tree, nodRaiz, niv);
    end;
    nod := nodRaiz;
    niveles.Destroy
  end;
  nod.Text:=Caption;
  if nod.Parent<>nil then  //para hacer al nodo visible
    nod.Parent.Expanded:=true;
  Result := nod;
end;

function LinkFrameToTreeView(tree: TTreeView; id, Caption: string;
         frame: TFrame = nil): TTreeNode;
{Crea un nodo en el TreeView y lo asocias a un Frame de configuración (Ver documentación
de MiConfig). Debe llamarse, después de crear el Frame.
El "id", debe ser único y es de la forma: "1", "2.1" o "3.2.1".
Si se indica el Frame en NIL, no se crea configura el Frame,solo se agrega el ítem}
begin
  Result := AddNodeToTreeView(tree, id, Caption);  //Crea el ítem el el TreeView
  if frame<>nil then begin
    //Agrega el ID en la etiqueta, para indicar que está asociado a ese nodo
    frame.Hint := frame.Hint + id + '|';
  end;
end;
function ShowFrameOfNode(form: Tform; node: TTreeNode; x, y: integer): boolean;
{Muestra el frame correspondiente a un nodo. La correspondencia Frame-nodo, debe
haberse indicado con LinkFrameToTreeView().
Si no encuentra el frame para mostrar, devuelve FALSE.}
var
  id: String;
  f: TFrame;
begin
  HideAllFrames(form);
  id := IdFromTTreeNode(node);
  //Ubica al Frame
  for f in ListOfFrames(form) do begin
    if pos(id+'|', f.Hint)<>0 then begin
      ShowFramePos(f, x, y);  //muestra
      exit(true);
    end;
  end;
  //No encontró
  exit(false)
end;

end.

