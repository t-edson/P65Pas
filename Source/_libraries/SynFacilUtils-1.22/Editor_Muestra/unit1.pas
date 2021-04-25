{Programa ejemplo de uso de la librería para implementar editores "utilEditSyn".
                                        Por Tito Hinostroza   11/07/2014 }
unit Unit1;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, SynEdit, SynEditTypes, Forms, Controls, Graphics,
  Dialogs, Menus, ComCtrls, ActnList, StdActns, SynFacilUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    acArcAbrir: TAction;
    acArcGuaCom: TAction;
    acArcGuardar: TAction;
    acArcNuevo: TAction;
    acArcSalir: TAction;
    acBusBuscar: TAction;
    acBusBusSig: TAction;
    acBusReemp: TAction;
    acEdCopy: TEditCopy;
    acEdCut: TEditCut;
    acEdModCol: TAction;
    acEdPaste: TEditPaste;
    acEdRedo: TAction;
    acEdSelecAll: TAction;
    acEdUndo: TAction;
    AcHerConfig: TAction;
    ActionList: TActionList;
    acVerBarEst: TAction;
    acVerNumLin: TAction;
    acVerPanArc: TAction;
    FindDialog1: TFindDialog;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnArchivo: TMenuItem;
    mnBuscar: TMenuItem;
    mnEdicion: TMenuItem;
    mnHerram: TMenuItem;
    mnLenguajes: TMenuItem;
    mnRecientes: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    SynEdit1: TSynEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure acArcAbrirExecute(Sender: TObject);
    procedure acArcGuaComExecute(Sender: TObject);
    procedure acArcGuardarExecute(Sender: TObject);
    procedure acArcNuevoExecute(Sender: TObject);
    procedure acArcSalirExecute(Sender: TObject);
    procedure acBusBuscarExecute(Sender: TObject);
    procedure acBusBusSigExecute(Sender: TObject);
    procedure acEdiRedoExecute(Sender: TObject);
    procedure acEdiSelecAllExecute(Sender: TObject);
    procedure acEdiUndoExecute(Sender: TObject);
    procedure ChangeEditorState;
    procedure editChangeFileInform;
    procedure FindDialog1Find(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  private
    edit: TSynFacilEditor;
  public
    procedure SetLanguage(lang: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetLanguage('en');
  edit.SetLanguage('en');
  edit := TSynFacilEditor.Create(SynEdit1, 'SinNombre', 'pas');
  edit.OnChangeEditorState:=@ChangeEditorState;
  edit.OnChangeFileInform:=@editChangeFileInform;
  //define paneles
  edit.PanFileSaved := StatusBar1.Panels[0]; //panel para mensaje "Guardado"
  edit.PanCursorPos := StatusBar1.Panels[1];  //panel para la posición del cursor

  edit.PanForEndLin := StatusBar1.Panels[2];  //panel para el tipo de delimitador de línea
  edit.PanCodifFile := StatusBar1.Panels[3];  //panel para la codificación del archivo
  edit.PanLangName  := StatusBar1.Panels[4];  //panel para el lenguaje
  edit.PanFileName  := StatusBar1.Panels[5];  //panel para el nombre del archivo

  edit.NewFile;        //para actualizar estado
  edit.InitMenuRecents(mnRecientes, nil);  //inicia el menú "Recientes"
  edit.InitMenuLanguages(mnLenguajes, '..\languages');
  edit.LoadSyntaxFromPath;  //para que busque el archivo apropiado
//  edit.LoadSyntaxFromFile('.\languages\c.xml');
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if edit.SaveQuery then CanClose := false;   //cancela
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  edit.Free;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  //Carga archivo arrastrados
  if edit.SaveQuery then Exit;   //Verifica cambios
  edit.LoadFile(FileNames[0]);
  edit.LoadSyntaxFromPath;  //para que busque el archivo apropiado
end;

procedure TForm1.ChangeEditorState;
begin
  acArcGuardar.Enabled:=edit.Modified;
  acEdUndo.Enabled:=edit.CanUndo;
  acEdRedo.Enabled:=edit.CanRedo;
  //Para estas acciones no es necesario controlarlas, porque son acciones pre-determinadas
//  acEdiCortar.Enabled  := edit.canCopy;
//  acEdiCopiar.Enabled := edit.canCopy;
//  acEdiPegar.Enabled:= edit.CanPaste;
end;

procedure TForm1.editChangeFileInform;
begin
  //actualiza nombre de archivo
  Caption := 'Editor - ' + edit.FileName;
end;

procedure TForm1.FindDialog1Find(Sender: TObject);
var
  encon  : integer;
  buscado : string;
  opciones: TSynSearchOptions;
begin
  buscado := FindDialog1.FindText;
  opciones := [];
  if not(frDown in FindDialog1.Options) then opciones += [ssoBackwards];
  if frMatchCase in FindDialog1.Options then opciones += [ssoMatchCase];
  if frWholeWord in FindDialog1.Options then opciones += [ssoWholeWord];
  if frEntireScope in FindDialog1.Options then opciones += [ssoEntireScope];

  encon := SynEdit1.SearchReplace(buscado,'',opciones);
  if encon = 0 then
     ShowMessage('No found: ' + buscado);
end;

/////////////////// Acciones de Archivo /////////////////////
procedure TForm1.acArcNuevoExecute(Sender: TObject);
begin
  edit.NewFile;
  edit.LoadSyntaxFromPath;  //para que busque el archivo apropiado
end;

procedure TForm1.acArcAbrirExecute(Sender: TObject);
begin
  OpenDialog1.Filter:='Text files|*.txt|All files|*.*';
  edit.OpenDialog(OpenDialog1);
  edit.LoadSyntaxFromPath;  //para que busque el archivo apropiado
end;

procedure TForm1.acArcGuardarExecute(Sender: TObject);
begin
  edit.SaveFile;
end;

procedure TForm1.acArcGuaComExecute(Sender: TObject);
begin
  edit.SaveAsDialog(SaveDialog1);
  edit.LoadSyntaxFromPath;  //para que busque el archivo apropiado
end;

procedure TForm1.acArcSalirExecute(Sender: TObject);
begin
  Form1.Close;
end;

procedure TForm1.acBusBuscarExecute(Sender: TObject);
begin
  FindDialog1.Execute;
end;

procedure TForm1.acBusBusSigExecute(Sender: TObject);
begin
  FindDialog1Find(self);
end;

//////////// Acciones de Edición ////////////////
procedure TForm1.acEdiUndoExecute(Sender: TObject);
begin
  edit.Undo;
end;
procedure TForm1.acEdiRedoExecute(Sender: TObject);
begin
  edit.Redo;
end;
procedure TForm1.acEdiSelecAllExecute(Sender: TObject);
begin
  SynEdit1.SelectAll;
end;

procedure TForm1.SetLanguage(lang: string);
begin
  case lowerCase(lang) of
  'es': begin
      //menú principal
      mnArchivo.Caption:='&Archivo';
      mnEdicion.Caption:='&Edición';
      mnBuscar.Caption:='&Buscar';
      mnLenguajes.Caption:='&Lenguajes';
      mnHerram.Caption:='&Herramientas';

      acArcNuevo.Caption := '&Nuevo';
      acArcNuevo.Hint := 'Nueva consulta';
      acArcAbrir.Caption := '&Abrir...';
      acArcAbrir.Hint := 'Abrir archivo';
      acArcGuardar.Caption := '&Guardar';
      acArcGuardar.Hint := 'Guardar archivo';
      acArcGuaCom.Caption := 'G&uardar Como...';
      acArcGuaCom.Hint := 'Guardar como';
      acArcSalir.Caption := '&Salir';
      acArcSalir.Hint := 'Cerrar el programa';
      acEdUndo.Caption := '&Deshacer';
      acEdUndo.Hint := 'Deshacer';
      acEdRedo.Caption := '&Rehacer';
      acEdRedo.Hint := 'Reahacer';
      acEdCut.Caption := 'Cor&tar';
      acEdCut.Hint := 'Cortar';
      acEdCopy.Caption := '&Copiar';
      acEdCopy.Hint := 'Copiar';
      acEdPaste.Caption := '&Pegar';
      acEdPaste.Hint := 'Pegar';
      acEdSelecAll.Caption := 'Seleccionar &Todo';
      acEdSelecAll.Hint := 'Seleccionar todo';
      acEdModCol.Caption := 'Modo Columna';
      acEdModCol.Hint := 'Modo columna';
      acBusBuscar.Caption := 'Buscar...';
      acBusBuscar.Hint := 'Buscar texto';
      acBusBusSig.Caption := 'Buscar &Siguiente';
      acBusBusSig.Hint := 'Buscar Siguiente';
      acBusReemp.Caption := '&Remplazar...';
      acBusReemp.Hint := 'Reemplazar texto';
      acHerConfig.Caption:='Configuración';
      acHerConfig.Hint := 'Ver configuración';
    end;
  'en': begin
      //menú principal
      mnArchivo.Caption:='&File';
      mnEdicion.Caption:='&Edit';
      mnBuscar.Caption:='&Search';
      mnLenguajes.Caption:='&Lenguages';
      mnHerram.Caption:='&Tools';

      acArcNuevo.Caption := '&New';
      acArcNuevo.Hint := 'New query';
      acArcAbrir.Caption := '&Open...';
      acArcAbrir.Hint := 'Open file';
      acArcGuardar.Caption := '&Save';
      acArcGuardar.Hint := 'Save file';
      acArcGuaCom.Caption := 'Sa&ve As ...';
      acArcGuaCom.Hint := 'Save file as ...';
      acArcSalir.Caption := '&Quit';
      acArcSalir.Hint := 'Close the program';
      acEdUndo.Caption := '&Undo';
      acEdUndo.Hint := 'Undo';
      acEdRedo.Caption := '&Redo';
      acEdRedo.Hint := 'Redo';
      acEdCut.Caption := 'C&ut';
      acEdCut.Hint := 'Cut';
      acEdCopy.Caption := '&Copy';
      acEdCopy.Hint := 'Copy';
      acEdPaste.Caption := '&Paste';
      acEdPaste.Hint := 'Paste';
      acEdSelecAll.Caption := 'Select &All';
      acEdSelecAll.Hint := 'Select all';
      acEdModCol.Caption := 'Column mode';
      acEdModCol.Hint := 'Column mode';
      acBusBuscar.Caption := 'Search...';
      acBusBuscar.Hint := 'Search text';
      acBusBusSig.Caption := 'Search &Next';
      acBusBusSig.Hint := 'Search Next';
      acBusReemp.Caption := '&Replace...';
      acBusReemp.Hint := 'Replace text';
      acHerConfig.Caption := '&Settings';
      acHerConfig.Hint := 'Configuration dialog';
    end;
  end;
end;

end.

