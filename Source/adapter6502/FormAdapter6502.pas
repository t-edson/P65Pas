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
  adapterBase;
type

  { TfrmAdapter6502 }

  TfrmAdapter6502 = class(TForm)
    acToolChecksyn: TAction;
    ActionList: TActionList;
    acToolASMDebug: TAction;
    acToolComEjec: TAction;
    acToolCompil: TAction;
    acToolFindDec: TAction;
    acToolListRep: TAction;
    acToolPICExpl: TAction;
    acToolRamExp: TAction;
    acToolTestPic10: TAction;
    acToolTestUnit: TAction;
    ImgActions16: TImageList;
    ImgActions32: TImageList;
    MainMenu1: TMainMenu;
    procedure acToolChecksynExecute(Sender: TObject);
    procedure acToolCompilExecute(Sender: TObject);
  public
    adapter: TAdapterBase;  //Referencia al adaptador padre
    procedure AddActions(imgList16, imglist32: TImageList; actList: TActionList;
       actionCat: string; mainMenu: TMainMenu);
  end;

var
  frmAdapter6502: TfrmAdapter6502;
//resourcestring
//  acToolCompil.Caption  := Trans('&Compile'                , '&Compilar'  , '&Compilay',
//                                 '&Compilieren'            , 'Компілювати', 'Компилировать',
//                                 '&Compiler');
//  acToolCompil.Hint     := Trans('Compile the source code' , 'Compila el código fuente'     , 'Pachanmanta chimpukuna kaqta compilay',
//                                 'Compiliere den Quelltext','Компілювати','Компилировать',
//                                 'Compiler le code source');
//  acToolComEjec.Caption := Trans('Compile and E&xecute'    , 'Compilar y Ej&ecutar'         , 'Compilay chaymanta &Hinay',
//                                 'Compilieren und Au&sführen','Компілювати та виконати','Компилировать и выполнить',
//                                 'Compiler et &Exécuter');
//  acToolComEjec.Hint    := Trans('Compile and Execute'     , 'Compilar y Ejecutar'          , 'Compilay chaymanta &Hinay',
//                                 'Compilieren und Ausführen','Компілювати та виконати','Компилировать и выполнить',
//                                 'Compiler et Exécuter');
//  acToolPICExpl.Caption := Trans('PIC E&xplorer'           , 'E&xplorador de PIC'           , 'PIC nisqakunata T''aqwiq',
//                                 'PIC E&xplorer','PIC оглядач','PIC обозреватель',
//                                 'E&xplorateur de PIC');
//  acToolPICExpl.Hint    := Trans('Open the PIC devices explorer','Abrir el explorador de dispos. PIC', 'Dispos. PIC nisqa t''aqwiqta kichariy',
//                                 'Öffne den PIC Geräte explorer','Відкрити PIC оглядач','Открыть PIC обозреватель',
//                                 'Ouvrir l''explorateur de modèles PIC');
//  acToolListRep.Caption := Trans('&List Report'            , '&Reporte de listado'          , '',
//                                 '','Звіт','Отчет',
//                                 '&Rapport de Compilation');
//  acToolFindDec.Caption := Trans('Find declaration' , 'Ir a la declaración' , 'Riqsichikusqan k''itiman riy',
//                                 'Finde Deklaration','Знайти декларування','Найти декларирование',
//                                 'Trouver déclaration');
//  acToolRamExp.Caption  := Trans('&RAM Explorer' , 'Explorador de &RAM' , '',
//                                 '','','',
//                                 '');
//
//  acToolASMDebug.Caption:= Trans('ASM &Debugger'        , '&Depurador de ASM'            , '',
//                                 '','ASM зневаджувач','ASM отладчик',
//                                 '&Débogueur PIC');
//  acToolASMDebug.Hint   := Trans('ASM &Debugger'        , '&Depurador de ASM'            , '',
//                                 '','ASM зневаджувач','ASM отладчик',
//                                 'Démarrer le Débogueur');
//
implementation
{$R *.lfm}
uses adapter6502;
{ TfrmAdapter6502 }
//Llamadas a eventos
procedure TfrmAdapter6502.acToolCompilExecute(Sender: TObject);
begin
  TAdapter6502(adapter).acCompilExecute(self);
end;

procedure TfrmAdapter6502.acToolChecksynExecute(Sender: TObject);
begin
  TAdapter6502(adapter).acChecksynExecute(self);
end;

procedure TfrmAdapter6502.AddActions(imgList16, imglist32: TImageList;
                           actList: TActionList; actionCat: string; mainMenu: TMainMenu);
{Copia todas las acciones de "self.ActionList" a "actList". Como las acciones a copiar
pueden tener íconos asociados, se copian también los íconos de ImgActions16 y
ImgActions32 a imgList16 y imglist32.
"actionCat" es la categoría que se pondrán a las acciones creadas.}
var
  transImgIndexes: array of integer;
  actSrc, newAct: TAction;
  i: Integer;
  menCompiler: TMenuItem;

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
  //Crea nueva entrada el menú principal de la IDE
  menCompiler:= TMenuItem.Create(nil);
  menCompiler.Caption:='Compiler';
  mainMenu.Items.Add(menCompiler);

  //Primero copia los íconos de nuestras listas de imágenes a "imgList16, imglist32".
  CopyIconsTo(ImgActions16, ImgActions32, imgList16, imgList32, transImgIndexes);
  //Ahora agregamos nuestras acciones a "actList".
  for i:=0 to ActionList.ActionCount-1 do begin
      actSrc := TAction(ActionList.Actions[i]);   //Asumimos que ActionList.Actions[i] es TAction
      newAct := AddActionTo(actList, actSrc, actionCat);
      //Aprovechamos para llenar el menú
      CreaMenuConAccion(menCompiler, newAct);
  end;
end;

end.

