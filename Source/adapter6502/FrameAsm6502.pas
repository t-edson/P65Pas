unit FrameAsm6502;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, StdCtrls, Grids, Graphics,
  ExtCtrls, Buttons, Menus, LCLType, Analyzer, CPUCore, MisUtils;
type

  { TfraPicAsm }

  TfraPicAsm = class(TFrame)
  published
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
  private
    pic: TCPUCore;
    defHeight : LongInt;  //Altura por defecto de fila
    defHeightFold: Integer;  //Altura de fila plegada
    margInstrc: Integer;
    curVarName: string;
    function AddressFromRow(ARow: integer): Integer;
    function CurrentAddress: Integer;
    function FindNextLabel(row: integer): integer;
    function FindPrevLabel(row: integer): integer;
    procedure Fold(row1, row2: integer);
    function RowFolded(row: integer): boolean;
    function RowFromAddress(add: Integer): Integer;
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure Unfold(row1, row2: integer);
    procedure ResizeRow(i: integer);
  public  //Eventos
    OnCPUerror: procedure(txt: string) of object;
  public  //Acciones de depuración
    procedure SetPC;
    procedure SetBrkPnt;
    procedure StepIn;
    procedure StepOver;
    procedure ExecHere;
  public  //Inicialización
    procedure SetCompiler(cxp0: TAnalyzer);
    procedure Refrescar(SetGridRow: boolean);
    constructor Create(AOwner: TComponent) ; override;
  end;

implementation

{$R *.lfm}

{ TfraPicAsm }
function TfraPicAsm.AddressFromRow(ARow: integer): Integer;  inline;
//var
//  add: TObject;
begin
//  {La dirección de una fila está codificada en su campo Object[]}
//  add := StringGrid1.Objects[0, ARow];
//  Result := PtrUInt(add);  //Lee dirección física
  //La dirección de una fila está en hexadecimal en la columna o
  Result := StrToInt(StringGrid1.Cells[0,ARow]);
end;
function TfraPicAsm.RowFromAddress(add: Integer): Integer; inline;
{Obtiene la fila de la grilla que corresponde a una dirección.}
begin
  Result := pic.ram[add].rowGrid;
end;
function TfraPicAsm.CurrentAddress: Integer;  inline;
{Devuelve la dirección actualmente seleccionada en la grilla}
begin
  //La dirección de una fila está en hexadecimal en la columna o
  Result := AddressFromRow(StringGrid1.Row);
end;
procedure TfraPicAsm.StringGrid1DrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  txt, comm, lab: String;   //Texto de la celda
  cv    : TCanvas;              //Referencia al lienzo
  ramCell: ^TCPURamCell;  //Referencia a la celda RAM
  PC    : Word;
  rowHeight: LongInt;
  addr   : integer;
begin
  cv := StringGrid1.Canvas;  //referencia al Lienzo
  addr := AddressFromRow(ARow);  //Lee dirección física
  ramCell := @pic.ram[addr];
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
      if ramCell^.used = ruCode then begin
        cv.Brush.Color := clWhite;  //fondo blanco
      end else if ramCell^.used = ruData then begin
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
    //Celda para la dirección
    txt := StringGrid1.Cells[ACol, ARow];
    cv.TextOut(aRect.Left + 2, aRect.Top + 2, txt);
  end else if ACol = 1 then begin
    //Celda para el OpCode
    txt := StringGrid1.Cells[ACol, ARow];
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
      if addr = PC then begin  //marca
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
      if addr = PC then begin  //marca
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
      if addr = PC then begin  //marca
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
  if pic.ram[i].used = ruUnused then begin
    StringGrid1.RowHeights[i] := defHeight;
    exit;
  end;
  //Es celda usada
  StringGrid1.Objects[0,i] := TObject(PtrUInt(i));
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
//Acciones de depuración
procedure TfraPicAsm.SetPC;
//Fija el puntero del programa en la instrucción seleccionada.
var
  pc: Integer;
begin
  if StringGrid1.Row=-1 then exit;
  pc := AddressFromRow(StringGrid1.Row);
  pic.WritePC(pc);
  StringGrid1.Invalidate;
end;
procedure TfraPicAsm.SetBrkPnt;
{Pone o quita un Punto de Interrupción en la posición indicada}
var
  pc: word;
begin
  if StringGrid1.Row=-1 then exit;
  pc := AddressFromRow(StringGrid1.Row);
  pic.ToggleBreakpoint(pc);
end;
procedure TfraPicAsm.StepIn;
{Ejecuta una instrucción, entrando al código de las subrutinas.}
begin
  pic.MsjError := '';
  pic.Exec();
  if pic.MsjError<>'' then begin
    if OnCPUerror<>nil then OnCPUerror(pic.MsjError);
  end;
end;
procedure TfraPicAsm.StepOver;
{Ejecuta una instrucción sin entrar a subrutinas}
var
  pc: DWord;
begin
  if pic.ram[pic.ReadPC].value = 0 then begin
    //Salta memoria no usada
    pc := pic.ReadPC;
    while (pc < high(pic.ram)) and (pic.ram[pc].value = 0) do begin
      pc := pc + 1;  //Incrementa
      pic.WritePC(pc);
    end;
    //MsgBox('$%x %d', [pc, pic.ram[pc].value]);
  end else begin
    pic.MsjError := '';
    pic.ExecStep;
    if pic.MsjError<>'' then begin
      if OnCPUerror<>nil then OnCPUerror(pic.MsjError);
    end;
  end;
end;
procedure TfraPicAsm.ExecHere;
{Ejecuta una instrucción hasta la dirección seleccionada.}
var
  pc: word;
begin
  if StringGrid1.Row=-1 then exit;
  pc := AddressFromRow(StringGrid1.Row);
  pic.MsjError := '';
  pic.ExecTo(pc);  //Ejecuta hasta la sgte. instrucción, salta el i_CALL
  if pic.MsjError<>'' then begin
    if OnCPUerror<>nil then OnCPUerror(pic.MsjError);
  end;
end;
//Inicialización
procedure TfraPicAsm.SetCompiler(cxp0: TAnalyzer);
var
  addr, f, minUsed: Integer;
  nBytes: byte;
  opCode: String;
begin
  pic := cxp0.picCore;
  StringGrid1.DefaultDrawing:=false;
  StringGrid1.OnDrawCell := @StringGrid1DrawCell;
  //Dimensiona la grilla para que pueda mostrar las etIquetas
  StringGrid1.BeginUpdate;
  StringGrid1.RowCount := high(pic.ram)+1;  //Máxima cantidad de filas
  addr := 0;
  f    := 0;
  minUsed := -1;
  while addr <= high(pic.ram) do begin
    StringGrid1.Objects[0,f] := TObject(PtrUInt(addr));
    pic.ram[addr].rowGrid := f;  //Guarda referencia a la fila de la grilla en la RAM
    //Dimensiona altura de celdas
    if (pic.ram[addr].topComment<>'') and (pic.ram[addr].topLabel<>'') then begin
      //Tiene comentario arriba y etiqueta
      StringGrid1.RowHeights[f] := 3*defHeight;
    end else if (pic.ram[addr].topComment<>'') or (pic.ram[addr].topLabel<>'') then begin
      //Tiene comentario arriba
      StringGrid1.RowHeights[f] := 2*defHeight;
    end else begin
      //Deja con la misma altura
      StringGrid1.RowHeights[f] := defHeight;
    end;
    //Coloca texto y direcciones
    if pic.ram[addr].used = ruUnused then begin
      //Celda no usada
      StringGrid1.RowHeights[addr] := defHeight;
      StringGrid1.Cells[0, f] := '$'+IntToHex(addr,4);
      StringGrid1.Cells[1, f] := '';
      //StringGrid1.Cells[2, f] := '';  //Reservado para el comentario lateral
      inc(addr);
    end else if pic.ram[addr].used = ruData then begin
      //Es espacio para variable
      StringGrid1.Cells[0, f] := '$'+IntToHex(addr,4);
      StringGrid1.Cells[1, f] := '$' + IntToHex(pic.ram[addr].value, 2);
      //StringGrid1.Cells[2, f] := '';  //Reservado para el comentario lateral
      inc(addr);
    end else begin
      //Debe ser código
      if minUsed<>-1 then minUsed := addr;
      //Decodifica instrucción
      StringGrid1.Cells[0, f] := '$'+IntToHex(addr,4);
      opCode := pic.DisassemblerAt(addr, nBytes, true);  //Instrucción
      StringGrid1.Cells[1, f] := opCode;
      //StringGrid1.Cells[2, f] := '';  //Reservado para el comentario lateral
      inc(addr, nBytes);
    end;
    inc(f);
  end;
  StringGrid1.RowCount := f;  //Solo las filas usadas
  StringGrid1.EndUpdate();
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
    StringGrid1.Row := RowFromAddress(pc);
  end;
  StringGrid1.Invalidate;
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

