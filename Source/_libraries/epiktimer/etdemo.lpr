program ETDemo;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms, main, EpikTimer, newsw, splash;

var i:Integer;

{$R *.res}

begin
  Application.Initialize;
  Form3 := TForm3.Create(nil);
  Try
  Application.ProcessMessages;
  Application.CreateForm(TForm1, Form1);
  for i:=1 to MaxStopwatches do
    Begin
      Application.CreateForm(TForm2, NewStopwatches[i]);
      Form3.Progressbar1.position:=i;
      Application.ProcessMessages;
    End;
  Finally
    Form3.free;
  End;
  Application.Run;
end.

