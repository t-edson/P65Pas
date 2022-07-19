{Unit to define the base class TAdapter.
Include some routines to create actions and menus dinamically.}
unit adapterBase;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils, types, FrameEditView, FrameCfgSynEdit, MiConfigXML, Menus,
  ExtCtrls, Controls, Graphics, ComCtrls, Forms ;

type
  {Define la entidad "Página de configuración". Representa a una de las páginas o
  "TabSheet" que se tienen en el formulario de configuración.
  Se usa para ayudar en simplificar la creación de la estructura de páginas de
  configuración en el FormConfig y para facilitar la comunicación con los adapatadores
  para permitirles configurar sus propias páginas de configuración.}
  TConfigPage = object
    treeNode : TTreeNode;    //Entrada en el TTreeNode que selecciona a la página de config.
    tabsheet : TTabSheet;    //Página del PageControl donde está el "scrollBox".
    scrollBox: TScrollBox;   //Contenedor de los controles de configuración.
    extra    : boolean;      //Bandera para indicar que la página es extra y está disponible.
  end;

  {Adaptador para controlar a diversos compiladores}

  { TAdapterBase }

  TAdapterBase = class
  public     //Eventos
    OnBeforeCompile: procedure of object;  //Al iniciar la compilación (No verif. de sintaxis).
    OnAfterCompile: procedure of object;   //Al finalizar la compilación (No verif. de sintaxis).
    OnBeforeCheckSyn: procedure of object; //Al iniciar la verif. de sintaxis.
    OnAfterCheckSyn: procedure of object;  //Al finalizar la verif. de sintaxis.
    OnWarning: procedure(warTxt, fname: string; row, col: integer) of object;
    OnError  : procedure(errTxt, fname: string; row, col: integer) of object;
    OnInfo   : procedure(infTxt, fname: string; row, col: integer) of object;
  public      //Información
    function CompilerName: string; virtual; abstract;
    function hexFilePath: string; virtual; abstract;
    function mainFilePath: string; virtual; abstract;
    function CPUname: string; virtual; abstract;
    function RAMusedStr: string; virtual; abstract;
    function SampleCode: string; virtual; abstract;
  public      //Manejo de Codetool
    procedure SetCompletion(ed: TSynEditor); virtual; abstract;
    procedure CTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual; abstract;
    procedure GoToDeclaration; virtual; abstract;
  public      //Ejecución
    procedure Compile; virtual; abstract;
    procedure CheckSyntax; virtual; abstract;
    procedure NotifyConfigChanged(MessPanBack, MessPanText, MessPanErr,
      MessPanSel: TColor; mainEditorCfg: TfraCfgSynEdit); virtual; abstract;
//    procedure UpdateCompletionForEditors; virtual; abstract;
//    procedure DumpCode(lins: TSTrings); virtual; abstract;
  public      //Inicialización
    procedure ConfigCreate(frmConfig: TComponent; EnvExt1, EdiExt1,
      _Compiler, CompExt1, CompExt2, CompExt3: TConfigPage); virtual; abstract;
    procedure ConfigInit(cfgFile: TMiConfigXML); virtual; abstract;
    procedure ConfigActivate; virtual; abstract;
    procedure setMenusAndToolbar(menu1, menu2, menu3: TMenuItem; toolbar: TToolBar;
      popupEdit: TPopupMenu; popupEditCount: integer); virtual; abstract;
  end;

  //Funciones para control de menú
  procedure InicLlenadoAcciones(MenuPopup0: TPopupMenu );
  procedure CreaMenuConAccion(itemMenu: TMenuItem; accion: TBasicAction; TheOwner: TComponent);
  function MenuAccion(etiq: string; accion: TNotifyEvent; id_icon: integer = -1): TMenuItem;
  function AgregarAccion(var ordShortCut: integer; etiq: string;
                         accion: TNotifyEvent; id_icon: integer = -1): TMenuItem;
  function CreaYCargaImagen(arcPNG: string): TImage;
  function CargaPNG(imgLst16src, imgLst32src: TImageList; idx: integer;  //Origen
                    imgLst16trg, imgLst32trg: TImageList): integer;      //Destino
  procedure CopyIconsTo(imgLst16src, imgLst32src: TImageList;
                        imgList16, imgList32: TImageList;
                        out transImgIndexes: TIntegerDynArray);

implementation
var  //variables para el lleado de acciones de facturables
  idxMenu  : Integer;
  MenuPopup: TPopupMenu;
procedure InicLlenadoAcciones(MenuPopup0: TPopupMenu);
{Se usa para empezar a llenar acciones sobre un menú PopUp}
begin
  idxMenu := MenuPopup0.Items.Count;   //empieza a agregar desde el final
  MenuPopup := MenuPopup0;
end;
procedure CreaMenuConAccion(itemMenu: TMenuItem; accion: TBasicAction; TheOwner: TComponent);
{Agrega un nuevo ítem a un menú, a partir de una acción.}
var
  mn: TMenuItem;
begin
  mn :=  TMenuItem.Create(TheOwner);
  mn.Action := accion;
  itemMenu.Add(mn);
end;
function MenuAccion(etiq: string; accion: TNotifyEvent; id_icon: integer = -1): TMenuItem;
{Devuelve la referencia a un ítemd e menú, para poder agregarla a un menú.}
var
  nuevMen: TMenuItem;
begin
  nuevMen:= TMenuItem.Create(nil);
  nuevMen.Caption:=etiq;
  nuevMen.OnClick:=accion;
  nuevMen.ImageIndex:=id_icon;
  {Notar que la referencia "nuevMen", no ha sido destruida porque se supone que se usará
  para agregarla a un menú, de modo que será el propieo menú el encargado de destruirla.}
  Result := nuevMen;
end;
function AgregarAccion(var ordShortCut: integer; etiq: string;
                       accion: TNotifyEvent; id_icon: integer = -1): TMenuItem;
{Agrega una acción sobre el menú PopUp indicado. Debe llamarse después de llamar a
InicLlenadoAcciones. El ítem del menú se agrega, justo después del último ítem agregado.
Si "ordShortCut"<>-1 , se usa su valor para crear un atajo del teclado al menú, y se va
incrementando su valor. }
var
  mn: TMenuItem;
  atajo: String;
begin
  if (ordShortCut<>-1) and (ordShortCut<10) then begin
    atajo := '&' + IntToStr(ordShortCut) + '. ';  //crea atajo
    etiq := StringReplace(etiq,'&','', [rfReplaceAll]);  //quita los otros atajos
    inc(ordShortCut);   //incrmeenta
  end else begin
    atajo := '';
  end;
  mn := MenuAccion(atajo + etiq, accion, id_icon);
  MenuPopup.Items.Insert(idxMenu, mn);  //Agrega al inicio
  inc(idxMenu);
  Result := mn;
end;
function CreaYCargaImagen(arcPNG: string): TImage;
{Crea un objeto TImage, y carga una archivo PNG en él. Devuelve la referencia.}
begin
  Result := TImage.Create(nil);
  if not FileExists(arcPNG) then exit;
  Result.Picture.LoadFromFile(arcPNG);
end;
//function CargaPNG(imagList16, imagList32: TImageList; rut, nombPNG: string): integer;
//{Carga archivos PNG, de 16 y 32 pixeles, a un TImageList. Al nombre de los archivos
//se les añadirá el sufijo "_16.png" y "_32.png", para obteenr el nombre final.
//Devuelve el índice de la imagen cargada.}
//begin
//  if nombPNG = '' then exit(-1);   //protección
//  Result := LoadPNGToImageList(imagList16, rut + nombPNG + '_16.png');
//  Result := LoadPNGToImageList(imagList32, rut + nombPNG + '_32.png');
//end;
function CargaPNG(imgLst16src, imgLst32src: TImageList; idx: integer;  //Origen
                  imgLst16trg, imgLst32trg: TImageList): integer;      //Destino
{Carga archivos PNG, de 16 y 32 pixeles, desde un TImageList a otro TImageList.
Devuelve el índice de la imagen cargada.}
var
  pngbmp: TPortableNetworkGraphic;
begin
  if idx = -1 then exit(-1);  //Caso sin íconos.

  pngbmp:=TPortableNetworkGraphic.Create;

  imgLst16src.GetBitmap(idx, pngbmp);
  Result:= imgLst16trg.Add(pngbmp, nil);   //Devuelve número de imágenes

  imgLst32src.GetBitmap(idx, pngbmp);
  Result:= imgLst32trg.Add(pngbmp, nil);  //Devuelve número de imágenes

  pngbmp.Destroy;
end;
procedure CopyIconsTo(imgLst16src, imgLst32src: TImageList;
                      imgList16, imgList32: TImageList;
                      out transImgIndexes: TIntegerDynArray);
{Rutina para copiar los íconos de 16 y 32 bits de imgLst16src e imgLst32src a dos ImageList.
El parámetro "transImgIndexes" devuelve la traducción del índice de cada ícono en el
nuevo TImageList. Por ejemplo si el ícono ocupaba la posición 1 en nuestro imgLst16src,
en la lista imgList16, ocupará la posición transImgIndexes[1].
}
var
  nIconsToCopy, i: Integer;
begin
  nIconsToCopy := imgLst16src.Count;
  setlength(transImgIndexes, nIconsToCopy);  //Dimensiona para contener todos los índices.
  //Agrega los íconos de "adapterForm" a los ImageList
  for i:=0 to nIconsToCopy-1 do begin
    transImgIndexes[i] := CargaPNG(imgLst16src, imgLst32src, i, imgList16, imgList32);
  end;
end;

end.

