{Programa ejemplo de uso de la librería para implementar editores "utilEditSyn".
                                        Por Tito Hinostroza   11/07/2014 }
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, SynFacilUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnUndo: TMenuItem;
    mnRedo: TMenuItem;
    MenuItem4: TMenuItem;
    mnCut: TMenuItem;
    mnPaste: TMenuItem;
    mnCopy: TMenuItem;
    mnSaveAs: TMenuItem;
    mnRecents: TMenuItem;
    MenuItem6: TMenuItem;
    mnExit: TMenuItem;
    mnNew: TMenuItem;
    mnOpen: TMenuItem;
    mnSave: TMenuItem;
    MenuItem5: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    SynEdit1: TSynEdit;
    procedure ChangeEditorState;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure mnCopyClick(Sender: TObject);
    procedure mnCutClick(Sender: TObject);
    procedure mnOpenClick(Sender: TObject);
    procedure mnNewClick(Sender: TObject);
    procedure mnPasteClick(Sender: TObject);
    procedure mnRedoClick(Sender: TObject);
    procedure mnSaveAsClick(Sender: TObject);
    procedure mnSaveClick(Sender: TObject);
    procedure mnUndoClick(Sender: TObject);
  private
    edit: TSynFacilEditor;
    ArcRecientes: TStringList;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  edit := TSynFacilEditor.Create(SynEdit1,'NoName', 'txt');
  edit.SetLanguage('en');
  edit.OnChangeEditorState:=@ChangeEditorState;
  //define paneles
  edit.PanFileSaved := StatusBar1.Panels[0]; //panel para mensaje "Guardado"
  edit.PanCursorPos := StatusBar1.Panels[1];  //panel para la posición del cursor

  edit.PanForEndLin := StatusBar1.Panels[2];  //panel para el tipo de delimitador de línea
  edit.PanCodifFile := StatusBar1.Panels[3];  //panel para la codificación del archivo
  edit.PanFileName  := StatusBar1.Panels[4];  //panel para el nombre del archivo
  //al final se debe asignar al editor
  edit.NewFile;
  ArcRecientes := TStringList.Create;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  edit.InitMenuRecents(mnRecents, ArcRecientes);  //inicia el menú "Recientes"
end;

procedure TForm1.ChangeEditorState;
begin
  mnSave.Enabled:=edit.Modified;
  mnUndo.Enabled:=edit.CanUndo;
  mnRedo.Enabled:=edit.CanRedo;

  mnCut.Enabled  := edit.canCopy;
  mnCopy.Enabled := edit.canCopy;
  mnPaste.Enabled:= edit.CanPaste;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if edit.SaveQuery then CanClose := false;   //cancela
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ArcRecientes.Free;
  edit.Free;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  //Carga archivo arrastrados
  if edit.SaveQuery then Exit;   //Verifica cambios
  edit.LoadFile(FileNames[0]);
end;

////////////// Edit menu ////////////////
procedure TForm1.mnUndoClick(Sender: TObject);
begin
  edit.Undo;
end;

procedure TForm1.mnRedoClick(Sender: TObject);
begin
  edit.Redo;
end;

procedure TForm1.mnCutClick(Sender: TObject);
begin
  edit.Cut;
end;

procedure TForm1.mnCopyClick(Sender: TObject);
begin
  edit.Copy;
end;

procedure TForm1.mnPasteClick(Sender: TObject);
begin
  edit.Paste;
end;
////////////// File menu ////////////////
procedure TForm1.mnNewClick(Sender: TObject);
begin
  edit.NewFile;
end;

procedure TForm1.mnOpenClick(Sender: TObject);
begin
  OpenDialog1.Filter:='Text files|*.txt|All files|*.*';
  edit.OpenDialog(OpenDialog1);
end;

procedure TForm1.mnSaveClick(Sender: TObject);
begin
  edit.SaveFile;
end;

procedure TForm1.mnSaveAsClick(Sender: TObject);
begin
  edit.SaveAsDialog(SaveDialog1);
end;

end.

