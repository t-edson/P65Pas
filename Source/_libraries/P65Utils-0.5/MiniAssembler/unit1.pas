{Sample of how to create a very basic assembler tool, using the unit pic16utils.}
unit Unit1;
{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  P6502utils;

type
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    pic: TP6502;
    function CaptureComma(var lin: string): boolean;
    function ExtractNumber(var lin: string; out num: word): boolean;
    function ExtractString(var lin: string; var str: string): boolean;
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}
{ TForm1 }

function TForm1.ExtractString(var lin: string; var str: string): boolean;
var
  tmp: String;
  i: Integer;
begin
  Result := true;
  lin := trim(lin);    //trim
  if lin='' then begin
    Application.MessageBox('Expected identifier.','');
    Result := false;
    exit;
  end;
  tmp := '';
  i:=1;
  while lin[i] in ['a'..'z','A'..'Z'] do begin
    tmp += lin[i];
    inc(i);
  end;
  lin := copy(lin,i,100);
  lin := trim(lin);    //trim
  str := tmp;
  if str = '' then begin
    //No string found
    exit(false);
  end;
end;
function TForm1.ExtractNumber(var lin: string; out num: word): boolean;
var
  tmp: String;
  i: Integer;
begin
  Result := true;
  lin := trim(lin);    //trim
  if lin='' then begin
    Application.MessageBox('Expected number.','');
    Result := false;
    exit;
  end;
  tmp := '';
  i:=1;
  while lin[i] in ['$','0'..'9','x','X'] do begin
    tmp += lin[i];
    inc(i);
  end;
  lin := copy(lin,i,100);
  lin := trim(lin);    //trim
  if LowerCase( copy(tmp,1,2)) = '0x' then
     num := StrToInt('$' + copy(tmp,3,100))
  else
     num := StrToInt(tmp);
end;
function TForm1.CaptureComma(var lin: string): boolean;
begin
  Result := true;
  lin := trim(lin);    //trim
  if lin='' then begin
    Application.MessageBox('Expected comma.','');
    Result := false;
    exit;
  end;
  if lin[1]<>',' then begin
    Application.MessageBox('Expected comma.','');
    Result := false;
    exit;
  end;
  lin := copy(lin,2,100);
  lin := trim(lin);    //trim
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  l: String;
  idInst: TP6502Inst;
  Inst: String;
  lin, Par: String;
  n: word;
begin
  pic.iRam:=0;       //Start to code at $0000
  pic.MsjError:='';  //Clear error
  for lin in Memo1.Lines do begin
    l := lin;   //crea copia para modificar
    if trim(l) = '' then continue;
    if not ExtractString(l, Inst) then begin  //extract mnemonic
      Application.MessageBox('Syntax Error','');
      exit;
    end;
    //Find mnemonic, and parameters
    idInst := pic.FindOpcode(Inst);
    if idInst = i_Inval then begin
      Application.MessageBox(PChar('Invalid Opcode: '+ Inst),'');
      exit;
    end;
    //Extract parameters
    if l = '' then begin
        //No parameters. Must be Implicit
        pic.codAsm(idInst, aImplicit , 0);
        if pic.MsjError<>'' then begin
          Application.MessageBox(PChar(lin + ':' + pic.MsjError),'');
          exit;
        end;
    end else if ExtractString(l, Par) then begin  //extract mnemonic
        //It's a string
        if Par = 'A' then begin
          //Accumulator mode
          pic.codAsm(idInst, aAcumulat , 0);
          if pic.MsjError<>'' then begin
            Application.MessageBox(PChar(lin + ':' + pic.MsjError),'');
            exit;
          end;
        end else begin
          Application.MessageBox(PChar(lin + ': Syntax error' ),'');
        end;
    end else if ExtractNumber(l, n) then begin
        //There is a number
        if n<256 then begin
          //Zero page. Although could be ,X
          pic.codAsm(idInst, aZeroPage , 0);
          if pic.MsjError<>'' then begin
            Application.MessageBox(PChar(lin + ':' + pic.MsjError),'');
            exit;
          end;
        end else begin
          //Absolute. Although could be ,X
          pic.codAsm(idInst, aAbsolute , 0);
          if pic.MsjError<>'' then begin
            Application.MessageBox(PChar(lin + ':' + pic.MsjError),'');
            exit;
          end;
        end;
    end else begin
        //Not a string, nor a number, nor empty
        Application.MessageBox('Syntax Error','');
        exit;
    end;
  end;
  pic.GenHex(Application.ExeName + '.hex');
  Memo2.Lines.LoadFromFile(Application.ExeName + '.hex');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  pic := TP6502.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  pic.Destroy;
end;

end.

