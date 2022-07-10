{Formulario se usa como contenedor de imágenes y menús que se crearán de forma
dinámica en la IDE.
Incluye rutinas para configurar acciones y menús externos.
Es necesario crear al menos la opción "Compilar" para poder usar el compilador.
Normalmente este formulario debe permanecer oculto pues solo se usa como contenedor de
íconos/acciones, admeás de usar sus rutinas de configuración de acciones y menús.
Sin embargo, si se hace visible, este formulario puede servir también como herramienta
de depuración pues contiene un menú que da acceso a las herramientas del compilador.}
unit FormAdapter6502;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, Menus,
  ComCtrls, fgl, adapterBase, MisUtils;
type
  //Lista de acciones
  TActionsAdded = specialize TFPGObjectList<TAction>;

  { TfrmAdapter6502 }
  TfrmAdapter6502 = class(TForm)
    acToolChecksyn: TAction;
    ActionList: TActionList;
    acToolASMDebug: TAction;
    acToolComEjec: TAction;
    acToolCompil: TAction;
    acToolFindDec: TAction;
    acToolListRep: TAction;
    acToolRamExp: TAction;
    acToolTestUnit: TAction;
    ImgActions16: TImageList;
    ImgActions32: TImageList;
    MainMenu1: TMainMenu;
    procedure acToolASMDebugExecute(Sender: TObject);
    procedure acToolChecksynExecute(Sender: TObject);
    procedure acToolComEjecExecute(Sender: TObject);
    procedure acToolCompilExecute(Sender: TObject);
    procedure acToolFindDecExecute(Sender: TObject);
    procedure acToolListRepExecute(Sender: TObject);
    procedure acToolRamExpExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    actionsAdded: TActionsAdded;
    adapter: TAdapterBase;  //Referencia al adaptador padre
    procedure AddActions(imgList16, imglist32: TImageList; actList: TActionList;
      actionCat: string);
    procedure setMenusAndToolbar(menu1, menu2: TMenuItem; toolbar: TToolBar);
  end;

var
  frmAdapter6502: TfrmAdapter6502;
implementation
{$R *.lfm}
uses adapter6502;
{ TfrmAdapter6502 }
//Llamadas a eventos
procedure TfrmAdapter6502.acToolCompilExecute(Sender: TObject);
begin
  TAdapter6502(adapter).Compile;
end;
procedure TfrmAdapter6502.acToolComEjecExecute(Sender: TObject);
begin
  TAdapter6502(adapter).CompileAndExec(self);
end;
procedure TfrmAdapter6502.acToolChecksynExecute(Sender: TObject);
begin
  TAdapter6502(adapter).CheckSyntax;
end;
procedure TfrmAdapter6502.acToolRamExpExecute(Sender: TObject);
begin
  TAdapter6502(adapter).acRamExpExecute(self);
end;
procedure TfrmAdapter6502.acToolASMDebugExecute(Sender: TObject);
begin
  TAdapter6502(adapter).ASMDebug(self);
end;
procedure TfrmAdapter6502.acToolListRepExecute(Sender: TObject);
begin
  TAdapter6502(adapter).ListReport;
end;
procedure TfrmAdapter6502.acToolFindDecExecute(Sender: TObject);
begin
  TAdapter6502(adapter).FindDeclarat;
end;
//procedure TfrmAdapter6502.acToolTestUnitExecute(Sender: TObject);
//begin
//  TAdapter6502(adapter).TestUnit;
//end;

procedure TfrmAdapter6502.AddActions(imgList16, imglist32: TImageList;
                           actList: TActionList; actionCat: string);
{Copia todas las acciones de "self.ActionList" a "actList". Como las acciones a copiar
pueden tener íconos asociados, se copian también los íconos de ImgActions16 y
ImgActions32 a imgList16 y imglist32.
"actionCat" es la categoría que se pondrán a las acciones creadas para facilitar una
identificación posterior.}
var
  transImgIndexes: array of integer;
  actSrc, newAct: TAction;
  i: Integer;

  function AddActionTo(actListDest: TActionList; actSrc: TAction; categ: string): TAction;
  {Crea una nueva acción en "actListDest" y copia los datos de la acción "actSrc".
  Se debe haber inicializado transImgIndexes[]}
  begin
    Result := TACtion.Create(actListDest);
    Result.Caption := actSrc.Caption;
    Result.ActionList := actListDest;
    Result.Category := categ;
    Result.OnExecute := actSrc.OnExecute;
    if actSrc.ImageIndex = -1 then begin
      Result.ImageIndex := -1;
    end else begin
      //Coloca la nueva posición del ícono de "actSrc".
      Result.ImageIndex := transImgIndexes[actSrc.ImageIndex];
    end;
  end;

begin

  //Primero copia los íconos de nuestras listas de imágenes a "imgList16, imglist32".
  CopyIconsTo(ImgActions16, ImgActions32, imgList16, imgList32, transImgIndexes);
  actionsAdded.Clear;
  //Ahora agregamos nuestras acciones a "actList".
  for i:=0 to ActionList.ActionCount-1 do begin
      actSrc := TAction(ActionList.Actions[i]);   //Asumimos que ActionList.Actions[i] es TAction
      newAct := AddActionTo(actList, actSrc, actionCat);
      //Guardamos la acción creada.
      actionsAdded.Add(newAct);
  end;
end;
procedure AddButtons(ToolBar: TToolBar; const ButtonCaptions: array of String);
var
  i: integer;
begin
  for i := 0 to High(ButtonCaptions) do
  begin
    with TToolButton.Create(ToolBar) do
    begin
      Parent := ToolBar;
      Caption := ButtonCaptions[i];
      if (ButtonCaptions[i] = '|') then
        Style := tbsSeparator
      else
        Style := tbsButton;
      AutoSize := True;
      Left := Parent.Width; //Buttons are added from left to right, otherwise the direction might be random, usually from right to left
    end;
  end;
end;
procedure TfrmAdapter6502.setMenusAndToolbar(menu1, menu2: TMenuItem; toolbar: TToolBar);
{Configura los menús proporcionados por la IDE para uso de este adaptador.}
  procedure AddButtonTB(action: TAction);
  var
    but: TToolButton;
  begin
    but := TToolButton.Create(toolBar);
    but.Parent := toolbar;
    but.Action := action;
//    but.Style  := tbsButton;
    but.Left := toolbar.Width;  //Pone a la derecha
  end;
var
  menCompiler: TMenuItem;
  actAdded: TAction;
  i: Integer;
  but: TToolButton;
begin
  //Configuramos el menu1.
  menu1.Clear;
  menu1.Caption := 'Compiler';
  for actAdded in actionsAdded do begin
    CreaMenuConAccion(menu1, actAdded);
  end;
  menu1.Visible := true;

  //Configuramos el menu2.
  menu2.Clear;
  menu2.Visible := false;

  //Configuramos Toolbar
  for i:=toolBar.ButtonCount-1 downto 0 do toolBar.Buttons[i].Free;  //Borramos botones
  AddButtonTB(actionsAdded[0]);  //Agrega botón acToolCmpil
  AddButtonTB(actionsAdded[1]);  //Agrega botón acToolComEjec
  AddButtonTB(actionsAdded[4]);  //Agrega botón acToolASMDebug
  AddButtonTB(actionsAdded[3]);  //Agrega botón acToolRamExp
end;

//Inicialización
procedure TfrmAdapter6502.FormCreate(Sender: TObject);
begin
  actionsAdded := TActionsAdded.Create(false);
end;
procedure TfrmAdapter6502.FormDestroy(Sender: TObject);
begin
  actionsAdded.Destroy;
end;

end.

