unit FramePicAsm;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, StdCtrls, Grids, Graphics,
  ExtCtrls, Buttons, Menus, LCLType, Parser, CPUCore, MisUtils;
type

  { TfraPicAsm }

  TfraPicAsm = class(TFrame)
    ImageList16: TImageList;
    Label2: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem8: TMenuItem;
    mnSetPCHere: TMenuItem;
    panTitle: TPanel;
    PopupMenu1: TPopupMenu;
    SpeedButton1: TSpeedButton;
    StringGrid1: TStringGrid;
    procedure PopupMenu1Popup(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    pic: TCPUCore;
    defHeight : LongInt;  //Altura por defecto de fila
    defHeightFold: Integer;  //Altura de fila plegada
    margInstrc: Integer;
    curVarName: string;
    function FindNextLabel(row: integer): integer;
    function FindPrevLabel(row: integer): integer;
    procedure Fold(row1, row2: integer);
    function RowFolded(row: integer): boolean;
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure Unfold(row1, row2: integer);
  public
    procedure Refrescar(SetGridRow: boolean);
    procedure ResizeRow(i: integer);
    procedure SetCompiler(cxp0: TCompilerBase);
    constructor Create(AOwner: TComponent) ; override;
  end;

implementation

{$R *.lfm}

{ TfraPicAsm }
procedure TfraPicAsm.StringGrid1DrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  txt, comm, lab: String;   //Texto de la celda
  cv: TCanvas;              //Referencia al lienzo
  ramCell: ^TCPURamCell;  //Referencia a la celda RAM
  PC: Word;
  rowHeight: LongInt;
  nBytes: byte;
begin
  cv := StringGrid1.Canvas;  //referencia al Lienzo
  ramCell := @pic.ram[aRow];
  //Fija color de texto y relleno
  if gdFixed in aState then begin
    //Es una celda fija
    cv.Brush.Color := clMenu;      // le ponemos azul de fondo
    cv.Font.Color := clBlack;      // fuente blanca
    cv.Font.Style := [];     // y negrita
  end else begin
    //Es una celda común
    if aRow = StringGrid1.Row then begin
      //Fila seleccionada
      cv.Brush.Color := clMenu;  // fondo marrón
      cv.Font.Color := clBlack;    // letra negra
      cv.Font.Style := [fsBold];   // negrita
    end else begin
      //Fila sin selección
      if ramCell^.used then begin
        cv.Brush.Color := clWhite;  //fondo blanco
      end else begin  //Dirección no usada
        cv.Brush.Color := $E0E0E0;
      end;
      cv.Font.Style := [fsBold];  //negrita
    end;
  end;
  //Dibuja contenido de celda
  cv.FillRect(aRect);   //fondo
  if ACol = 0 then begin
    txt := '$'+IntToHex(aRow,3);
    cv.TextOut(aRect.Left + 2, aRect.Top + 2, txt);
  end else if ACol = 1 then begin
    //Celda normal
    txt := pic.DisassemblerAt(aRow, nBytes, true);   //desensambla
    PC := pic.ReadPC;
    //Escribe texto con alineación
    rowHeight := StringGrid1.RowHeights[Arow];
    if rowHeight = defHeight*3 then begin
      //Celda con comentario superior y etiqueta
      lab := trim(ramCell^.topLabel)+':';
      comm := trim(ramCell^.topComment);
      cv.Font.Color := clGray;
      cv.TextOut(aRect.Left + 2, aRect.Top + 2, lab);  //comentario
      cv.Font.Color := clBlue;
      cv.TextOut(aRect.Left + 2 + margInstrc, aRect.Top + defHeight+ 2, comm);  //comentario
      //Escribe instrucción
      cv.Font.Color := clGreen;   //letra verde
      cv.TextOut(aRect.Left + 2 + margInstrc, aRect.Top + defHeight*2 + 2, txt);
      if ramCell^.breakPnt then begin
        ImageList16.Draw(cv, aRect.Left + 1, aRect.Top+2 + defHeight*2, 9);
      end;
      if aRow = PC then begin  //marca
         ImageList16.Draw(cv, aRect.Left + 10, aRect.Top+2 + defHeight*2, 3);
      end;
    end else if rowHeight = defHeight*2 then begin
      //Celda con comentario superior o etiqueta
      comm := trim(ramCell^.topComment);
      if comm<>'' then begin
        cv.Font.Color := clBlue;   //letra verde
        cv.TextOut(aRect.Left + 2 + margInstrc, aRect.Top + 2, comm);  //comentario
      end else begin
        //Hay etiqueta
        lab := trim(ramCell^.topLabel)+':';
        cv.Font.Color := clGray;
        cv.TextOut(aRect.Left + 2, aRect.Top + 2, lab);  //comentario
      end;
      //Escribe instrucción
      cv.Font.Color := clGreen;   //letra verde
      cv.TextOut(aRect.Left + 2 + margInstrc, aRect.Top+2 + defHeight, txt);
      if ramCell^.breakPnt then begin
        ImageList16.Draw(cv, aRect.Left + 1, aRect.Top+2 + defHeight, 9);
      end;
      if aRow = PC then begin  //marca
         ImageList16.Draw(cv, aRect.Left + 10, aRect.Top+2 + defHeight, 3);
      end;
    end else if rowHeight = defHeightFold then begin
       //Fila plegada. Se supone que hay etiqueta
       lab := trim(ramCell^.topLabel)+': ...';
       cv.Font.Color := clGray;
       cv.TextOut(aRect.Left + 2, aRect.Top + 2, lab);  //comentario
    end else begin
      //Escribe instrucción
      cv.Font.Color := clGreen;   //letra verde
      cv.TextOut(aRect.Left + 2 + margInstrc, aRect.Top + 2, txt);
      if ramCell^.breakPnt then begin
        ImageList16.Draw(cv, aRect.Left + 1, aRect.Top+2, 9);
      end;
      if aRow = PC then begin  //marca
         ImageList16.Draw(cv, aRect.Left + 10, aRect.Top+2, 3);
      end;
    end;
  end else if ACol = 2 then begin
    //Celda normal
    cv.Font.Color := clBlue;   //letra verde
    txt := ramCell^.sideComment;  //comentario
//    if ramCell^.idFile=-1 then begin
//      txt := '';
//    end else begin
//      txt := 'IdFil=' + IntToStr(ramCell^.idFile) +
//             'row='   + IntToStr(ramCell^.rowSrc) +
//             'col='   + IntToStr(ramCell^.colSrc) ;
//    end;
    //Escribe texto con alineación
    cv.TextOut(aRect.Left + 2, aRect.Top + 2, txt);
  end;
end;
procedure TfraPicAsm.PopupMenu1Popup(Sender: TObject);
var
  txt: String;
  a: TStringDynArray;
  nBytes: byte;
begin
  if StringGrid1.Row=-1 then begin
    //acGenAddWatch.Visible := false;
      MenuItem8.Visible := false;
    exit;
  end;
  //Obtiene instrucción seleccionada
  txt := pic.DisassemblerAt(StringGrid1.Row, nBytes, true);
  //Valida si es instrucción
  a := Explode(' ', trim(txt));
  if (high(a)<>1) and (high(a)<>2) then begin
    //acGenAddWatch.Visible := false;
    MenuItem8.Visible := false;
    exit;
  end;
  //Puede ser una instrucción
  curVarName := a[1];   //toma la segunda parte
  if pos(',', curVarName)<>0 then begin
    //Toma hasta antes de la coma
    curVarName := copy(curVarName, 1, pos(',', curVarName)-1);
  end;
  curVarName := trim(curVarName);
  //acGenAddWatch.Caption := 'Add Watch on ' + curVarName;
  MenuItem8.Visible := true;
  MenuItem8.Caption := 'Add Watch on ' + curVarName;
end;
function TfraPicAsm.FindPrevLabel(row: integer): integer;
{Busca la fila anterior de la grilla que contenga una etiqueta.
La búsqued ase hace a partir de l amisma fila "row".
Si no encuentra. Deuvelve -1.}
begin
  while row > 0 do begin
    if pic.ram[row].topLabel<>'' then exit(row);
    Dec(row);  //Mira siguiente fila
  end;
  exit(-1);
end;
function TfraPicAsm.FindNextLabel(row: integer): integer;
{Busca la siguiente fila de la grilla que contenga una etiqueta.
La búsqued ase hace a partir de la siguiente fila de "row".
Si no encuentra. Deuvelve -1.}
begin
  while row < StringGrid1.RowCount-1 do begin
    Inc(row);  //Mira siguiente fila
    if pic.ram[row].topLabel<>'' then exit(row);
  end;
  exit(-1);
end;
procedure TfraPicAsm.Fold(row1, row2: integer);
//Pliega el rango de filas indicadas. No incluye a la última.
var
  i: Integer;
begin
  StringGrid1.BeginUpdate;
  StringGrid1.RowHeights[row1] := defHeightFold;  //fija altura
  for i := row1+1 to row2-1 do begin
    StringGrid1.RowHeights[i] := 0;  //oculta
  end;
  StringGrid1.EndUpdate();
end;
procedure TfraPicAsm.Unfold(row1, row2: integer);
//Despliega el rango de filas indicadas. No incluye a la última.
var
  i: Integer;
begin
  StringGrid1.BeginUpdate;
  for i := row1 to row2-1 do begin
    ResizeRow(i);
  end;
  StringGrid1.EndUpdate();
end;
procedure TfraPicAsm.StringGrid1DblClick(Sender: TObject);
var
  i1, i2: Integer;
begin
  //Toma fila actual
  i1 := StringGrid1.Row;
  if StringGrid1.RowHeights[i1] = defHeightFold then begin
    //Fila plegada. Expande
    i2 := FindNextLabel(i1);
    if i2 = -1 then exit;  //No debería pasar porque un blqoeu así no debería haberse plegado
    Unfold(i1, i2);
  end else begin
    //Fila sin plegar. Plega si es inicio de subrutina (tiene etiqueta).
    if pic.ram[i1].topLabel <> '' then begin
      i2 := FindNextLabel(i1);  //Busca fin de subrutina
      if i2 = -1 then exit;  //Es el último bloque
      Fold(i1, i2);
    end;
  end;
end;
procedure TfraPicAsm.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Longint;
begin
  if Button = mbRight then begin
    StringGrid1.MouseToCell(X, Y, ACol, ARow);
    if ACol>0 then begin
      StringGrid1.Row := ARow;
      PopupMenu1.PopUp;
    end;
  end;
end;
procedure TfraPicAsm.Refrescar(SetGridRow: boolean);
var
  pc: DWord;
  i1, i2: Integer;
begin
  if SetGridRow then begin
    pc := pic.ReadPC;
    //Verifica si está plegada
    if RowFolded(pc) then begin
      i1 := FindPrevLabel(pc);
      i2 := FindNextLabel(pc);
      if (i1 = -1) or (i2 = -1) then exit;
      Unfold(i1, i2);
    end;
    StringGrid1.Row := pc;
  end;
  StringGrid1.Invalidate;
end;
function TfraPicAsm.RowFolded(row: integer): boolean;
var
  h: LongInt;
begin
  h := StringGrid1.RowHeights[row];
  if (h = 0) or (h = defHeightFold) then exit(true);
  exit(false);
end;
procedure TfraPicAsm.ResizeRow(i: integer);
{Redimensiona la fila "i" de la grilla para que pueda contener las etiquetas que
incluye.}
begin
  if not pic.ram[i].used then begin
    StringGrid1.RowHeights[i] := defHeight;
    exit;
  end;
  //Es celda usada
  if (pic.ram[i].topComment<>'') and (pic.ram[i].topLabel<>'') then begin
    //Tiene comentario arriba y etiqueta
    StringGrid1.RowHeights[i] := 3*defHeight;
  end else if (pic.ram[i].topComment<>'') or (pic.ram[i].topLabel<>'') then begin
    //Tiene comentario arriba
    StringGrid1.RowHeights[i] := 2*defHeight;
  end else begin
    //Deja con la misma altura
    StringGrid1.RowHeights[i] := defHeight;
  end;
end;
procedure TfraPicAsm.SetCompiler(cxp0: TCompilerBase);
var
  i: Integer;
begin
  pic := cxp0.picCore;
  StringGrid1.DefaultDrawing:=false;
  StringGrid1.OnDrawCell := @StringGrid1DrawCell;
  //Dimensiona la grilla para que pueda mostrar las etIquetas
  StringGrid1.RowCount := high(pic.ram)+1;
  StringGrid1.BeginUpdate;
  for i:=0 to high(pic.ram) do ResizeRow(i);
  StringGrid1.EndUpdate();
end;
constructor TfraPicAsm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Altura de fila de la grilla por defecto
  defHeight := 20;
  defHeightFold := 21;
  //Margen para mostrar las instrucciones en la grilla
  margInstrc := 32;
  //Configura Toolbar
//  ToolBar1.ButtonHeight:=38;
//  ToolBar1.ButtonWidth:=38;
//  ToolBar1.Height:=42;
//  ToolBar1.Images:=ImgActions32;
end;

end.

