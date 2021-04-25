{Define a un frame que permite implementar un filtro (de los que usa UtilsGrill) de
acuerdo a un árbol.}
unit FrameFiltArbol;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, ExtCtrls, Buttons,
  ComCtrls, Grids, LCLType, UtilsGrilla, MisUtils;

type

  { TfraFiltArbol }

  TfraFiltArbol = class(TFrame)
    btnCerrPanel: TSpeedButton;
    ImageList1: TImageList;
    Panel4: TPanel;
    TreeView1: TTreeView;
    procedure btnCerrPanelClick(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure TreeView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    grilla :TStringGrid;  //grilla asociada
    griFiltrar: TUtilGrilla;
    colCat, colSubcat: TugGrillaCol;
    nombNodPrinc: string;
    procedure ProCambiaFiltro;
    function GetFiltroArbolCat: string;
    procedure SetFiltroArbolCat(AVal: string);
  public
    OnCambiaFiltro: procedure of object;
    OnSoliCerrar: procedure(Sender: TObject) of object;  //Cuando se pulsa el botón [X]
    property FiltroArbolCat: string read GetFiltroArbolCat write SetFiltroArbolCat;
    procedure LeerCategorias;
    function Filtro(const f: integer): boolean;
    procedure Inic(gri: TUtilGrilla; col1, col2: TugGrillaCol; nodPrinc: string); virtual;
  end;

implementation
{$R *.lfm}

procedure TfraFiltArbol.TreeView1DblClick(Sender: TObject);
begin
  if OnCambiaFiltro<>nil then OnCambiaFiltro();
end;
procedure TfraFiltArbol.btnCerrPanelClick(Sender: TObject);
begin
  if OnSoliCerrar<>nil then OnSoliCerrar(self);
end;
procedure TfraFiltArbol.TreeView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    TreeView1DblClick(self);
    Key := 0;
  end;
end;
function TfraFiltArbol.GetFiltroArbolCat: string;
{Devuelve el filtro (una cadena) que aplicaría el árbol de categorías.
 Si no se aplica el filtro del árbol de categorías, devuelve cadena nula, }
var
  nodSel: TTreeNode;
begin
  if not self.Visible then exit('');
  if TreeView1.Selected = nil then exit('');
  nodSel := TreeView1.Selected;
  if TreeView1.Selected.Level = 1 then begin  //Categoría seleccionada
    exit(nodSel.Text);  //nombre de nodo actual
  end else if TreeView1.Selected.Level = 2 then begin  //Categoría seleccionada
    exit(nodSel.Parent.Text + '-' + nodSel.Text);  //nombre de nodo padre
  end else begin
    exit('');
  end;
end;
procedure TfraFiltArbol.SetFiltroArbolCat(AVal: string);
{Fija una cadena como filtro, al arcbol de categorías}
var
  a: TStringDynArray;
  nod, nod2: TTreeNode;
begin
  if AVal='' then exit;
  a := Explode('-', AVal);
  if high(a) = 0 then begin
      //Solo 1 elemento
      for nod in TreeView1.Items do if nod.Level = 1 then begin
        //Selecciona la categoría
        if nod.Text = AVal then begin
          nod.Selected := true;
          exit;
        end;
      end;
  end else if high(a) = 1 then begin
      //2 elementos
      for nod in TreeView1.Items do if nod.Level = 1 then begin
        //Selecciona la categoría
        if nod.Text = a[0] then begin
          //Encontró la categoría
          for nod2 in TreeView1.Items do if nod2.Level = 2 then begin
            //Selecciona la categoría
            if nod2.Text = a[1] then begin
              nod2.Selected := true;
              exit;
            end;
          end;
        end;
      end;
  end;
end;
procedure TfraFiltArbol.ProCambiaFiltro;
begin
  if griFiltrar<>nil then griFiltrar.Filtrar;
end;
procedure TfraFiltArbol.LeerCategorias;
  function ExisteCategEnArbol(cat: string): boolean;
  var
    it : TTreeNode;
  begin
    for it in TreeView1.Items do begin
      if (it.Level = 1) and (it.Text = cat) then
        exit(true);
    end;
    exit(false);
  end;
  function ExisteSubCateg(cat, subcat: string): boolean;
  var
    it : TTreeNode;
  begin
    for it in TreeView1.Items do begin
      if (it.Level = 2) and (it.Text = subcat) and (it.Parent.Text = cat) then
        exit(true);
    end;
    exit(false);
  end;
var
  nodRaiz, nodCat, nodSub: TTreeNode;
  cat: String;
  cat2, subcat2: String;
  fil, fil2: Integer;
begin
  //Configura árbol de categorías
  TreeView1.Items.BeginUpdate;
  TreeView1.Items.Clear;
  nodRaiz := TreeView1.Items.AddChild(nil, nombNodPrinc);
  nodRaiz.ImageIndex := 0;
  nodRaiz.SelectedIndex := 0;
  for fil := 1 to grilla.RowCount-1 do begin
    cat := colCat.ValStr[fil];
    if not ExisteCategEnArbol(cat) then begin
      //No existe, crea un nuevo nodo
      nodCat := TreeView1.Items.AddChild(nodRaiz, cat);
      nodCat.ImageIndex := 1;
      nodCat.SelectedIndex := 1;
      //Llena de subcategorías
      for fil2 := 1 to grilla.RowCount-1 do begin
        cat2 := colCat.ValStr[fil2];
        subcat2 := colSubcat.ValStr[fil2];
        if (cat2 = cat) and not ExisteSubCateg(cat2, subcat2) then begin
          nodSub := TreeView1.Items.AddChild(nodCat, subcat2);
          nodSub.ImageIndex := 2;
          nodSub.SelectedIndex := 2;
        end;
      end;
      nodCat.Expanded := true;
    end;
  end;
  nodRaiz.Expanded:=true;
  nodRaiz.Selected:=true;
  TreeView1.Items.EndUpdate;
end;

function TfraFiltArbol.Filtro(const f: integer): boolean;
{Aplica el filtro de acuerdo al nodo seleccionado en el árbol de categorías.}
var
  nodSel: TTreeNode;
begin
  //if TreeView1.Selected = nil then exit(true);   //no hay seleccionado
  nodSel := TreeView1.Selected;  //nombre de nodo actual
  if nodSel.Level = 0 then begin   //Almacén seleccionado.
    exit(true);   //Pasan todos
  end else if nodSel.Level = 1 then begin  //Categoría seleccionada
    if grilla.Cells[colCat.idx, f] = nodSel.Text then exit(true)
    else exit(false);
  end else if nodSel.Level = 2 then begin  //Sub-Categoría seleccionada
    if (grilla.Cells[colCat.idx, f] = nodSel.Parent.Text) and
       (grilla.Cells[colSubcat.idx, f] = nodSel.Text) then exit(true)
    else exit(false);
  end;
end;
procedure TfraFiltArbol.Inic(gri: TUtilGrilla; col1, col2: TugGrillaCol;
  nodPrinc: string);
{Inicializa el árbol para poder funcionar como filtro}
begin
  grilla := gri.grilla;
  nombNodPrinc := nodPrinc;  //toma el nombre del nodo proncipal
  colCat := col1;
  colSubcat:= col2;
  TreeView1.Items.Clear;
  gri.AgregarFiltro(@Filtro);  //agrega su filtro
  {Crea un manejador de evento temporal, para ejecutar el filtro. Este evento puede
  luego reasignarse, de acuerdo a necesida, ya que no es vital. }
  griFiltrar := gri;
  OnCambiaFiltro:=@ProCambiaFiltro;
end;

end.

