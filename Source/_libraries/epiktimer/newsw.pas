unit newsw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Buttons,EpikTimer;

type
  TForm2 = class(TForm)
    Button1: TBUTTON;
    Button2: TBUTTON;
    Button3: TBUTTON;
    Button4: TBUTTON;
    Button5: TBUTTON;
    Button6: TBUTTON;
    Button7: TBUTTON;
    Button8: TBUTTON;
    Edit1: TEDIT;
    Edit2: TEDIT;
    Edit3: TEDIT;
    Edit4: TEDIT;
    Edit5: TEDIT;
    Edit6: TEDIT;
    Groupbox1: TGROUPBOX;
    Groupbox2: TGROUPBOX;
    procedure Button1CLICK(Sender: TObject);
    procedure Button2CLICK(Sender: TObject);
    procedure Button3CLICK(Sender: TObject);
    procedure Button4CLICK(Sender: TObject);
    procedure Button5CLICK(Sender: TObject);
    procedure Button6CLICK(Sender: TObject);
    procedure Button7CLICK(Sender: TObject);
    procedure Button8CLICK(Sender: TObject);
    procedure Form2CREATE(Sender: TObject);
    procedure Form2DESTROY(Sender: TObject);
  private
    ET:TEpikTimer;
    LocalTimer:TimerData;
    Procedure UpdateDisplay(Msg:String);
    Procedure UpdateDisplayLocal(Msg:String);
  public
    { public declarations }
  end; 

Var

Form2:Tform2;

implementation

{ TForm2 }

procedure TForm2.UpdateDisplay(Msg:String);
begin
  edit1.text:=ET.ElapsedStr;
  Edit2.text:=ET.elapsedDHMS;
  edit4.text:=Msg+': '+ET.WallClockTime;
end;


procedure TForm2.Button2CLICK(Sender: TObject);
begin
  ET.Start;
  UpdateDisplay('Start')
end;

procedure TForm2.Button3CLICK(Sender: TObject);
begin
  ET.Stop;
  UpdateDisplay('Stop')
end;

procedure TForm2.Button4CLICK(Sender: TObject);
begin
  ET.Clear;
  UpdateDisplay('Clear')
end;

procedure TForm2.Button1CLICK(Sender: TObject);
begin
  UpdateDisplay('Elapsed')
end;

(* This group uses a locally created timer *)

procedure TForm2.UpdateDisplayLocal(Msg:String);
begin
  edit3.text:=ET.ElapsedStr(LocalTimer);
  Edit5.text:=ET.elapsedDHMS(LocalTimer);
  edit6.text:=Msg+': '+ET.WallClockTime;
end;

procedure TForm2.Button6CLICK(Sender: TObject);
begin
  ET.Start(LocalTimer);
  UpdateDisplayLocal('Start')
end;

procedure TForm2.Button7CLICK(Sender: TObject);
begin
  ET.Stop(LocalTimer);
  UpdateDisplayLocal('Stop')
end;

procedure TForm2.Button8CLICK(Sender: TObject);
begin
  ET.Clear(LocalTimer);
  UpdateDisplayLocal('Clear')
end;

procedure TForm2.Button5CLICK(Sender: TObject);
begin
 UpdateDisplayLocal('Elapsed:')
end;

procedure TForm2.Form2CREATE(Sender: TObject);
begin
  ET:=TEpikTimer.create(nil);
  ET.Clear(LocalTimer) // you have to do this if you create a local timer
end;

procedure TForm2.Form2DESTROY(Sender: TObject);
begin
  ET.Free
end;


initialization
  {$I newsw.lrs}

end.

