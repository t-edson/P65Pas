{Unidad con frame para almacenar y configurar las propiedades de un editor
 SynEdit. Las propiedades que se manejan son con respecto al coloreado.
 El frame definido, está pensado para usarse en una ventana de configuración.
 También incluye una lista para almacenamiento de los archivos recientes
                               Por Tito Hinostroza  23/11/2013
}
unit FrameCfgSynEdit;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Forms, StdCtrls, Dialogs, SynEdit, Graphics,
  Globales, MiConfigXML, MiConfigBasic, SynEditMarkupHighAll, SynEditMarkup;
type

  { TfraCfgSynEdit }
  TfraCfgSynEdit = class(TFrame)
    cbutFonPan: TColorButton;
    cbutResPalCFon: TColorButton;
    cbutResPalCTxt: TColorButton;
    cbutResPalCBor: TColorButton;
    cbutTxtPan: TColorButton;
    chkFullWord: TCheckBox;
    chkHighCurWord: TCheckBox;
    chkAutoindent: TCheckBox;
    chkVerPanVer: TCheckBox;
    chkHighCurLin: TCheckBox;
    cbutLinAct: TColorButton;
    chkVerNumLin: TCheckBox;
    chkVerMarPle: TCheckBox;
    cbutBackCol: TColorButton;
    cbutTextCol: TColorButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    procedure chkHighCurLinChange(Sender: TObject);
    procedure chkHighCurWordChange(Sender: TObject);
    procedure chkVerPanVerChange(Sender: TObject);
  public
    //configuración del editor
    MarLinAct  : boolean;   //marcar línea actual
    Autoindent : boolean;   //Autotabulación
    ResPalAct  : boolean;   //resaltar palabra actual
    ResPalCFon : TColor;    //color de fondo de la palabra actual
    ResPalCTxt : TColor;    //color de texto de la palabra actual
    ResPalCBor : TColor;    //color de borde de la palabra actual
    ResPalFWord: Boolean;    //Activa "palabra completa"

    cTxtNor    : TColor;    //color de texto normal
    cFonEdi    : TColor;    //Color de fondo del control de edición
    cFonSel    : TColor;    //color del fondo de la selección
    cTxtSel    : TColor;    //color del texto de la selección
    cLinAct    : TColor;    //color de la línea actual
    //panel vertical
    VerPanVer  : boolean;    //ver pánel vertical
    VerNumLin  : boolean;    //ver número de línea
    VerMarPle  : boolean;    //ver marcas de plegado
    cFonPan     : TColor;    //color de fondo del panel vertical
    cTxtPan     : TColor;    //color de texto del panel vertical
    ArcRecientes: TStringList;  //Lista de archivos recientes

    procedure PropToWindow;
    procedure Iniciar(section: string; cfgFile: TMiConfigXML); //Inicia el frame
    procedure ConfigEditor(ed: TSynEdit);
    procedure SetLanguage;
  public
    //genera constructor y destructor
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
//const
//  MAX_ARC_REC = 5;  //si se cambia, actualizar ActualMenusReciente()

{ TfraCfgSynEdit }
procedure TfraCfgSynEdit.SetLanguage;
//Rutina de traducción
begin
   {$I ..\language\tra_CfgSynEdit.pas}
end;
procedure TfraCfgSynEdit.Iniciar(section: string; cfgFile: TMiConfigXML);
var
  s: TParElem;
begin
  //Asigna referencia necesarias
  //crea las relaciones variable-control
  s:=cfgFile.Asoc_TCol(section+ '/cTxtNor', @cTxtNor, cbutTextCol, clBlack);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_TCol(section+ '/cFonEdi', @cFonEdi, cbutBackCol,  clWhite);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_TCol(section+ '/cLinAct', @cLinAct, cbutLinAct, clYellow);
  s.categ := 1;   //marca como propiedad de tipo "Tema"

  s:=cfgFile.Asoc_Bol(section+ '/Autoindent', @Autoindent, chkAutoindent, true);
  s:=cfgFile.Asoc_Bol(section+ '/MarLinAct' , @MarLinAct , chkHighCurLin , false);

  s:=cfgFile.Asoc_Bol(section+ '/ResPalCur' , @ResPalAct , chkHighCurWord , true);
  s.categ := 1;
  s:=cfgFile.Asoc_TCol(section+ '/ResPalCFon',@ResPalCFon, cbutResPalCFon, clSkyBlue);
  s.categ := 1;
  s:=cfgFile.Asoc_TCol(section+ '/ResPalCTxt',@ResPalCTxt, cbutResPalCTxt, clBlack);
  s.categ := 1;
  s:=cfgFile.Asoc_TCol(section+ '/ResPalCBor',@ResPalCBor, cbutResPalCBor, clSkyBlue);
  s.categ := 1;
  s:=cfgFile.Asoc_Bol(section+ '/ResPalFWord',@ResPalFWord, chkFullWord, false);

  s:=cfgFile.Asoc_Bol(section+ '/VerPanVer', @VerPanVer, chkVerPanVer, true);
  s:=cfgFile.Asoc_Bol(section+ '/VerNumLin', @VerNumLin, chkVerNumLin, false);
  s:=cfgFile.Asoc_Bol(section+ '/VerMarPle', @VerMarPle, chkVerMarPle, true);
  s:=cfgFile.Asoc_TCol(section+ '/cFonPan'  , @cFonPan  , cbutFonPan  , clWhite);
  s.categ := 1;   //marca como propiedad de tipo "Tema"
  s:=cfgFile.Asoc_TCol(section+ '/cTxtPan'  , @cTxtPan  , cbutTxtPan  , clBlack);
  s.categ := 1;   //marca como propiedad de tipo "Tema"

  cfgFile.Asoc_StrList(section+ '/recient', @ArcRecientes);
end;
procedure TfraCfgSynEdit.chkVerPanVerChange(Sender: TObject);
begin
  chkVerNumLin.Enabled:=chkVerPanVer.Checked;
  chkVerMarPle.Enabled:=chkVerPanVer.Checked;
  cbutFonPan.Enabled:=chkVerPanVer.Checked;
  cbutTxtPan.Enabled:=chkVerPanVer.Checked;
  label2.Enabled:=chkVerPanVer.Checked;
  label3.Enabled:=chkVerPanVer.Checked;
end;
procedure TfraCfgSynEdit.chkHighCurLinChange(Sender: TObject);
begin
  label1.Enabled:=chkHighCurLin.Checked;
  cbutLinAct.Enabled:=chkHighCurLin.Checked;
end;
procedure TfraCfgSynEdit.chkHighCurWordChange(Sender: TObject);
begin
  label10.Enabled:=chkHighCurWord.Checked;
  cbutResPalCFon.Enabled:=chkHighCurWord.Checked;
end;
procedure TfraCfgSynEdit.PropToWindow;
begin
   inherited;
   chkHighCurLinChange(self);  //para actualizar
   chkVerPanVerChange(self);  //para actualizar
end;
constructor TfraCfgSynEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ArcRecientes := TStringList.Create;  //crea lista
end;
destructor TfraCfgSynEdit.Destroy;
begin
  FreeAndNil(ArcRecientes);
  inherited Destroy;
end;
procedure TfraCfgSynEdit.ConfigEditor(ed: TSynEdit);
{Configura el editor con las propiedades almacenadas}
var
  marc: TSynEditMarkup;
begin
   if ed = nil then exit;  //protección

   ed.Font.Color:=cTxtNor;      //color de texto normal
   ed.Color:=cFonEdi;           //color de fondo
   if MarLinAct then          //resaltado de línea actual
     ed.LineHighlightColor.Background:=cLinAct
   else
     ed.LineHighlightColor.Background:=clNone;
   //configura panel vertical
   ed.Gutter.Visible:=VerPanVer;  //muestra panel vertical
   ed.Gutter.Parts[1].Visible:=VerNumLin;  //Número de línea
   if ed.Gutter.Parts.Count>4 then
     ed.Gutter.Parts[4].Visible:=VerMarPle;  //marcas de plegado
   ed.Gutter.Color:=cFonPan;   //color de fondo del panel
   ed.Gutter.Parts[1].MarkupInfo.Background:=cFonPan; //fondo del núemro de línea
   ed.Gutter.Parts[1].MarkupInfo.Foreground:=cTxtPan; //texto del núemro de línea

   ////////Configura el resaltado de la palabra actual //////////
   marc := ed.MarkupByClass[TSynEditMarkupHighlightAllCaret];
   if marc<>nil then begin  //hay marcador
      marc.Enabled:=ResPalAct;  //configura
      marc.MarkupInfo.Background := ResPalCFon;
      marc.MarkupInfo.FrameColor := ResPalCBor;
      marc.MarkupInfo.Foreground := ResPalCTxt;
      TSynEditMarkupHighlightAllCaret(marc).FullWord := ResPalFWord;
   end;
   ///////fija color de delimitadores () {} [] ///////////
   ed.BracketMatchColor.Foreground := clRed;
   //Opciones
   if Autoindent then begin
     ed.Options := ed.Options + [eoAutoIndent];
   end else begin
     ed.Options := ed.Options - [eoAutoIndent];
   end;
end;

end.

