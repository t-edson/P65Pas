unit FrameRegWatcher6502;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Grids, ExtCtrls, StdCtrls,
  Buttons, Graphics, LCLType, Menus, LCLProc, ActnList, P65c02utils, MisUtils,
  UtilsGrilla, CibGrillas, CompBase, CPUCore, Globales, XpresElemP65, Analyzer;
type

  { TfraRegWatcher }

  TfraRegWatcher = class(TFrame)
    acClearAll: TAction;
    acAddVars: TAction;
    acAddRT: TAction;
    acDeleteRow: TAction;
    ActionList1: TActionList;
    grilla: TStringGrid;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    panTitle: TPanel;
    PopupMenu1: TPopupMenu;
    SpeedButton1: TSpeedButton;
    procedure acAddRTExecute(Sender: TObject);
    procedure acAddVarsExecute(Sender: TObject);
    procedure acClearAllExecute(Sender: TObject);
    procedure acDeleteRowExecute(Sender: TObject);
  private
    UtilGrilla: TGrillaEdicFor;
    procedure AddWatch(varAddr: word);
    function RowIsEmpty(f: integer): boolean;
    procedure grillaKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CompleteFromAddress(f: integer);
    procedure CompleteFromName(f: integer);
    procedure RefreshRow(f: integer);
    procedure UtilGrillaFinEditarCelda(var eveSal: TEvSalida; col,
      fil: integer; var ValorAnter, ValorNuev: string);
    procedure UtilGrillaKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    function UtilGrillaLeerColorFondo(col, fil: integer): TColor;
  private  //Índice de columnas
    pic: TCPUCore;
    cxp: TAnalyzer;
    //Columnas ocultas
    col_adr: word;  //Direción física
    col_bit: byte;  //Posición de bit
//    col_set: string[]
    //Columnas visibles
    COL_ADD: integer;  //Columna  de dirección
    COL_NAM: integer;  //Columna de nombre
    COL_VAL: integer;  //Columna de valor
  public
    procedure AddWatch(varName: string);
    procedure Refrescar;
  public  //Inicialización
    procedure SetCompiler(cxp0: TAnalyzer);
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
{ TfraRegWatcher }
procedure TfraRegWatcher.Refrescar;
//Refresca el valor de los registros, en base a la dirección de memoria que tengan
var
  f: Integer;
begin
  if not Visible then exit;
  grilla.BeginUpdate;
  for f := 1 to grilla.RowCount-1 do begin
      grilla.Objects[1, f] := Tobject(Pointer(0));  //inicia con texto color negro
      RefreshRow(f);
  end;
  grilla.EndUpdate();
end;
procedure TfraRegWatcher.CompleteFromAddress(f: integer);
{Refresca la fila f de la grilla, a partir del campo de dirección.
No actualiza el valor.}
var
  addrStr: String;
  addr: Longint;
begin
  addrStr := grilla.Cells[COL_ADD,f];
  if trim(addrStr) = '' then begin
     //No hay dato
     grilla.Cells[col_adr,f] := '';
     grilla.Cells[COL_NAM,f] := '#Error';
  end else begin
    //Hay dirección
    if addrStr[1] = '$' then begin
       //Debe ser hexadecimal
       if not TryStrToInt('$'+copy(addrSTr,2,10), addr) then begin
          grilla.Cells[col_adr,f] := '';
          grilla.Cells[col_bit,f] := '';
          grilla.Cells[COL_NAM,f] := '#Error';
          exit;
       end;
    end else begin
      //Debe ser decimal
      if not TryStrToInt(addrSTr, addr) then begin
         grilla.Cells[col_adr,f] := '';
         grilla.Cells[col_bit,f] := '';
         grilla.Cells[COL_NAM,f] := '#Error';
         exit;
      end;
    end;
    if (addr<0) or (addr>cxp.RAMmax) then begin
       grilla.Cells[col_adr,f] := '';
       grilla.Cells[col_bit,f] := '';
       grilla.Cells[COL_NAM,f] := '#Error';
       exit;
    end;
    //Escribe dirección y nombre
    grilla.Cells[col_adr,f] := '$'+IntToHex(addr, 3);
    grilla.Cells[col_bit,f] := '';
    grilla.Cells[COL_NAM,f] := pic.ram[addr].name;
  end;
end;
procedure TfraRegWatcher.CompleteFromName(f: integer);
{Refresca la fila f de la grilla, a partir del campo de Nombre }
var
  nameStr: String;
  addr, i: integer;
  nbit   : smallint;
begin
  nameStr := UpCase(grilla.Cells[COL_NAM,f]);
  if trim(nameStr) = '' then begin
     //No hay dato
     grilla.Cells[col_adr,f] := '';
     grilla.Cells[col_bit,f] := '';
     grilla.Cells[COL_ADD,f] := '';
  end else begin
    //Hay nombre
    addr := -1;
    nbit := -1;
    //Busca nombre de bytes
    for i:=0 to cxp.RAMmax do begin
      if UpCase(pic.ram[i].name) = nameStr then begin
        addr := i;
        break;
      end;
    end;
    if addr=-1 then begin  //No encontrado
       grilla.Cells[col_adr,f] := '';
       grilla.Cells[col_bit,f] := '';
       grilla.Cells[COL_ADD,f] := '';
       exit;
    end;
    //Encontró dirección
    grilla.Cells[col_adr,f] := '$'+IntToHex(addr,3);
    if nbit=-1 then begin
      //No hay bit
      grilla.Cells[col_bit,f] := '';
      grilla.Cells[COL_ADD,f] := '$'+IntToHex(addr,3);
    end else begin
      //Hay bit
      grilla.Cells[col_bit,f] := IntToStr(nbit);
      grilla.Cells[COL_ADD,f] := '$'+IntToHex(addr,3)+'.'+IntToStr(nbit);
    end;
  end;
end;
procedure TfraRegWatcher.RefreshRow(f: integer);
{Refresca el valor de la fila indicada. No toca ni la dirección ni el nombre.
Se usa para cuando se quiere ver la grilla actualizada.}
var
  addrStr, newValue, bitStr: String;
  addr: Longint;
  bit, valByte: byte;
begin
  //Lee dirección
  addrStr := grilla.Cells[col_adr,f];
  bitStr := grilla.Cells[col_bit,f];
  {No se hacen muchas validaciones porque se espera que el campo "col_adr"
  deba contener solo valores predecibles, ya que no son modiifcados por el
  usuario.}
  if addrStr = '' then begin
     //No hay dato
     grilla.Cells[COL_VAL, f] := '';
  end else begin
    //Debe haber dirección hexadecimal. No se hará comprobación.
    addr := StrToInt(addrStr);  //Debe incluir '$'
    //Escribe valor
    if bitStr<>'' then begin
      bit := ord(bitStr[1])-ord('0');  //convierte
      //Imprime bit
      valByte := pic.ram[addr].value;
      if (valByte and (1<<bit)) = 0 then newValue := '0' else newValue := '1';
      if grilla.Cells[COL_VAL,f] <> newValue then begin
         //Hubo cambio
         grilla.Objects[1, f] := Tobject(Pointer(255));  //Pone color
         grilla.Cells[COL_VAL,f] := newValue;
      end;
    end else begin
      //Imprime byte
      newValue := '$'+IntToHex(pic.ram[addr].value, 2);
      if grilla.Cells[COL_VAL,f] <> newValue then begin
         //Hubo cambio
         grilla.Objects[1, f] := Tobject(Pointer(255));  //Pone color
         grilla.Cells[COL_VAL,f] := newValue;
      end;
    end;
  end;
end;
procedure TfraRegWatcher.UtilGrillaFinEditarCelda(var eveSal: TEvSalida; col,
  fil: integer; var ValorAnter, ValorNuev: string);
begin
  if eveSal = evsTecEscape then exit;
//  MsgBox('Editado: %d, %d', [fil, col]);
  if col=COL_ADD then begin
     //Se editó la dirección
     grilla.Cells[col, fil] := ValorNuev;  //adelanta la escritura
     CompleteFromAddress(fil);
  end;
  if col=COL_NAM then begin
     //Se editó el nombre
     grilla.Cells[col, fil] := ValorNuev;  //adelanta la escritura
     CompleteFromName(fil);
  end;
  RefreshRow(fil);
end;
function TfraRegWatcher.RowIsEmpty(f: integer): boolean;
begin
  Result := (grilla.cells[COL_ADD, f]='') and
            (grilla.cells[COL_NAM, f]='') and
            (grilla.cells[COL_VAL, f]='');
end;
procedure TfraRegWatcher.grillaKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_UP) and                     //Se presiona flecha arriba
     (grilla.Row = grilla.RowCount-2) and  //Y pasó a la penúltima fila
     (RowIsEmpty(grilla.RowCount-1))    //y la que sigue está vacía
  then begin
     grilla.RowCount := grilla.RowCount -1;
//     MsgBox('asdsad');
  end;
end;
procedure TfraRegWatcher.UtilGrillaKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DOWN) and                    //Se presiona flecha abajo
     (grilla.Row = grilla.RowCount-1) and   //Y es la última fila
     (not RowIsEmpty(grilla.Row))
  then begin
     grilla.RowCount := grilla.RowCount + 1;  //agrega fila
  end;
end;
function TfraRegWatcher.UtilGrillaLeerColorFondo(col, fil: integer): TColor;
begin
  Result := clWhite;
end;
procedure TfraRegWatcher.AddWatch(varName: string);
{Agrega una variable para vigilar}
var
  f: Integer;
begin
  f := grilla.RowCount-1;  //última fila
  if not RowIsEmpty(f) then begin
    //Hay que agregar una fila
    grilla.RowCount := grilla.RowCount + 1;
    f := grilla.RowCount-1;
  end;
  grilla.Cells[COL_NAM,f] := varName;
  CompleteFromName(f);  //Actualiza dirección y valor
end;
procedure TfraRegWatcher.AddWatch(varAddr: word);
{Agrega una variable para vigilar, por su dirección}
var
  f: Integer;
begin
  f := grilla.RowCount-1;  //última fila
  if not RowIsEmpty(f) then begin
    //Hay que agregar una fila
    grilla.RowCount := grilla.RowCount + 1;
    f := grilla.RowCount-1;
  end;
  grilla.Cells[COL_ADD, f] := '$'+IntToHex(varAddr, 3);
  CompleteFromAddress(f);
end;
// Acciones
procedure TfraRegWatcher.acClearAllExecute(Sender: TObject);
{Limpia toda la grilla.}
begin
  grilla.RowCount := 1;  //Elimina todas
  grilla.RowCount := 2;  //Deja fila vacía
end;
procedure TfraRegWatcher.acDeleteRowExecute(Sender: TObject);
{Elimina una fila de la grilla}
var
  f: Integer;
begin
  f := grilla.Row;
  if f=-1 then exit;
  grilla.DeleteRow(f);
end;
procedure TfraRegWatcher.acAddVarsExecute(Sender: TObject);
{Agrega todas las variables usdas, del programa al inspector.}
var
  v: TEleVarDec;
  i, maxBytes: Integer;
begin
  for v in cxp.TreeElems.AllVars do begin   //Se supone que "AllVars" ya se actualizó.
      if v.nCalled = 0 then continue;
      if v.typ.IsByteSize then begin
        AddWatch(v.name);
      end else if v.typ.IsWordSize then begin
//        AddWatch(v.name+'@1');
//        AddWatch(v.name+'@0');
        AddWatch(v.addr);
        AddWatch(v.addr+1);
      end else if v.typ.IsDWordSize then begin
        AddWatch(v.addr);
        AddWatch(v.addr+1);
        AddWatch(v.addr+2);
        AddWatch(v.addr+3);
      end else if v.typ.catType = tctArray then begin
        //Arreglo
        //Agrega primer byte
        AddWatch(v.name);
        //agrega bytes siguientes
        maxBytes := v.typ.nItems * v.typ.itmType.size-1;
        //if maxBytes > 10 then
        for i:=1 to maxBytes do begin
           AddWatch(v.addr + i);
        end;
      end else if v.typ.catType = tctPointer then begin
        //Puntero corto
         AddWatch(v.addr);
      end else begin

      end;
  end;
  Refrescar;
end;
procedure TfraRegWatcher.acAddRTExecute(Sender: TObject);
{Agrega los registros de trabajo}
begin
/////// No se necesita ahora, estarutina por cuanto, los registros se manejan como
/////// simples variables.

//  for reg in cxp.ProplistRegAux do begin
//    if not reg.assigned then continue;  //puede haber registros de trabajo no asignados
//    nam := pic.ram[reg.addr].name; //debería tener nombre
//    AddWatch(nam);
//  end;
//  for rbit in cxp.ProplistRegAuxBit do begin
//    nam := pic.NameRAMbit(rbit.offs, rbit.bank, rbit.bit); //debería tener nombre
//    adStr := '0x' + IntToHex(rbit.AbsAdrr, 3);
//    lins.Add('#define' + nam + ' ' +  adStr + ',' + IntToStr(rbit.bit));
//  end;
//  Refrescar;
end;
//Inicialización
procedure TfraRegWatcher.SetCompiler(cxp0: TAnalyzer);
begin
  cxp := cxp0;
  pic := cxp0.picCore;
end;
constructor TfraRegWatcher.Create(AOwner: TComponent);
var
  tmp: TugGrillaCol;
begin
  inherited Create(AOwner);
  //grilla.Options := grilla.Options + [goEditing, goColSizing];
  UtilGrilla := TGrillaEdicFor.Create(grilla);
  UtilGrilla.IniEncab;
  //Columnas ocultas. En estas columnas se guardan lso campos qeu se usarán para refrescar toda la fila.
  //UtilGrilla.AgrEncab('' , 10).visible := false;
  tmp := UtilGrilla.AgrEncab('Add' , 35);
  tmp.visible := false;
  col_adr := tmp.idx;
  tmp := UtilGrilla.AgrEncab('Bit' , 35);
  tmp.visible := false;
  col_bit := tmp.idx;
  //Faltaría una o más columbas para el formato
  //Columnas visibles
  COL_ADD := UtilGrilla.AgrEncab('Address' , 40).idx;
  COL_NAM := UtilGrilla.AgrEncab('Name' , 60).idx;
  COL_VAL := UtilGrilla.AgrEncab('Value' , 50, -1, taRightJustify).idx;
  UtilGrilla.FinEncab;
  UtilGrilla.OnFinEditarCelda := @UtilGrillaFinEditarCelda;
  Utilgrilla.OnLeerColorFondo := @UtilGrillaLeerColorFondo;
  Utilgrilla.OnKeyDown := @UtilGrillaKeyDown;
  grilla.FixedCols := 2;
  grilla.OnKeyUp := @grillaKeyUp;

  grilla.RowCount := 2;
//  grilla.FixedCols := 0;
  grilla.Options := grilla.Options + [goColSizing];
end;
destructor TfraRegWatcher.Destroy;
begin
  UtilGrilla.Destroy;
  inherited Destroy;
end;

end.

