{Formulario para mostrar las propiedades de un elemento.}
unit FormElemProperty;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, MisUtils, XpresElementsPIC;
type

  { TfrmElemProperty }

  TfrmElemProperty = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    butDetails: TButton;
    Image1: TImage;
    ImageList1: TImageList;
    lblElemName4: TLabel;
    lblElemName5: TLabel;
    lblUsed: TLabel;
    lblElemName1: TLabel;
    lblElemName2: TLabel;
    lblElemName3: TLabel;
    txtEleLocFile: TEdit;
    txtEleName: TEdit;
    lblElemName: TLabel;
    Memo1: TMemo;
    txtEleType: TEdit;
    txtEleLocaPath: TEdit;
    procedure BitBtn2Click(Sender: TObject);
    procedure butDetailsClick(Sender: TObject);
  private
    elem: TxpElement;
    procedure SetCalledInfo(elem0: TxpElement);
  public
    OnExplore: procedure(elem0: TxpElement) of object;
    procedure Clear;
    procedure Exec(elem0: TxpElement);
  end;

var
  frmElemProperty: TfrmElemProperty;

implementation
{$R *.lfm}
{ TfrmElemProperty }
procedure TfrmElemProperty.BitBtn2Click(Sender: TObject);
begin
  if OnExplore<>nil then OnExplore(elem);
end;
procedure TfrmElemProperty.Clear;
begin
  txtEleName.Caption := 'Unknown';
  txtEleType.Caption := 'Unknown';
  txtEleLocaPath.Caption := '';
  txtEleLocFile.Caption := '';
  lblUsed.Font.Color := clGray;
  lblUsed.Caption := 'Unused';
  ImageList1.GetBitmap(13, Image1.Picture.Bitmap);
  Memo1.Text := '';
  BitBtn2.Enabled := false;
end;
procedure TfrmElemProperty.butDetailsClick(Sender: TObject);
var
  call: TxpEleCaller;
  tmp, callerStr: String;
begin
  //Detalla las llamadas hechas al elemento
  tmp := '';
  for call in elem.lstCallers do begin
    if call.caller.Parent<>nil then begin
      callerStr := call.caller.Parent.name + '-' + call.caller.name;
    end else begin
      callerStr := call.caller.name;
    end;
    tmp := tmp + 'Called by: ' + callerStr + ' ' +
           ' Pos:' + call.curPos.RowColString + LineEnding;
  end;
  MsgBox(tmp);
end;

procedure TfrmElemProperty.SetCalledInfo(elem0: TxpElement);
{Agrega información, sobre las llamadas que se hacen a un elemento }
var
  nCalled: Integer;
begin
  nCalled := elem0.nCalled;
  if nCalled = 0 then begin
    lblElemName3.Caption := 'Status';
    lblUsed.Font.Color := clGray;
    lblUsed.Caption := 'Unused';
    butDetails.Enabled := false;
  end else begin
    lblElemName3.Caption := 'Status';
    lblUsed.Font.Color := clGreen;
    lblUsed.Caption := 'Used ' + IntToStr(nCalled) + ' times.';
    butDetails.Enabled := true;
  end;
end;
procedure TfrmElemProperty.Exec(elem0: TxpElement);
var
  adicInformation, dirSolic, tmp: String;
  xcon: TxpEleCon;
  xfun: TxpEleFun;
  xbod: TxpEleBody;
  xvar: TxpEleVar;
  ecall : TxpExitCall;
begin
  if elem0 = nil then exit;
  elem := elem0;
  Image1.Stretch := true;
  Image1.Proportional := true;  // to keep width/height ratio
  adicInformation := '';
  txtEleName.Caption := elem.name;
  txtEleLocaPath.Caption := ExtractFileDir(elem.srcDec.Fil);
  txtEleLocFile.Caption := ExtractFileName(elem.srcDec.Fil) + elem.srcDec.RowColString;
  BitBtn2.Enabled := true;
  //Configura etiqueta y botón de número de llamadas al elemento
  SetCalledInfo(elem);
  //Ícono e información adicional
  if elem.idClass = eltCons then begin
    xcon := TxpEleCon(elem);
    if xcon.typ = nil then txtEleType.Caption := 'Unknown'
    else txtEleType.Caption := xcon.typ.name;

    ImageList1.GetBitmap(4, Image1.Picture.Bitmap);
    adicInformation := '';
  end else if elem.idClass = eltVar then begin
    xvar := TxpEleVar(elem);
    txtEleType.Caption := xvar.typ.name;

    ImageList1.GetBitmap(2, Image1.Picture.Bitmap);
    if xvar.typ.IsBitSize then begin
      dirSolic := IntToStr(xvar.adicPar.absAddr) + ':' + IntToStr(xvar.adicPar.absBit);
    end else begin
      dirSolic := IntToStr(xvar.adicPar.absAddr);
    end;
    adicInformation :=
           'Direcc. Solicitada: ' + dirSolic + LineEnding +
           'Direcc. Asignada: ' + xvar.AddrString;
  end else if elem.idClass = eltFunc then begin
    xfun := TxpEleFun(elem);
    if xfun.typ = nil then txtEleType.Caption := 'Unknown'
    else txtEleType.Caption := xfun.typ.name;

    ImageList1.GetBitmap(3, Image1.Picture.Bitmap);
    //Genera reporte de ExitCalls
    tmp := '';
    for ecall in xfun.lstExitCalls do begin
      tmp := tmp + 'exit() in : ' + ecall.srcPos.RowColString + ' ' +
             LineEnding;
    end;
    //Información adicional
    adicInformation := 'Address: $' + IntToHex(xfun.adrr, 3) + LineEnding +
           'Size: ' + IntToStr(xfun.srcSize) + LineEnding + tmp;
  end else if elem.idClass = eltUnit then begin
    txtEleType.Caption := 'Unit';
    ImageList1.GetBitmap(6, Image1.Picture.Bitmap);
    adicInformation := '';
  end else if elem.idClass = eltBody then begin
    xbod:= TxpEleBody(elem);
    txtEleType.Caption := 'Body';
    ImageList1.GetBitmap(12, Image1.Picture.Bitmap);
    adicInformation := 'Address: $' + IntToHex(xbod.adrr, 3) + LineEnding +
           'Begin: ' + xbod.srcDec.RowColString  + LineEnding +
           'End: ' + elem.srcEnd.RowColString;
  end else if elem.idClass = eltMain then begin
    txtEleType.Caption := 'Main';
    ImageList1.GetBitmap(1, Image1.Picture.Bitmap);
    adicInformation := '';
  end else begin
    txtEleType.Caption := 'Unknown';
    ImageList1.GetBitmap(13, Image1.Picture.Bitmap);
    adicInformation := '';
  end;
  Memo1.Text := adicInformation;
end;

end.


