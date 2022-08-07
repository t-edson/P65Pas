unit main; 
{ Name: EpikTimer Demo Program
  Description: Test application
  Author: Tom Lisjac <vlx@users.sourceforge.net>

  Demonstrates the capabilities of TEpikTimer in epiktimer.pas
}

{ Copyright (C) 2003 by Tom Lisjac <vlx@users.sourceforge.net>

  This program is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU General Public License along with
  this program; if not, write to the Free Software Foundation, Inc., 59 Temple
  Place - Suite 330, Boston, MA 02111-1307, USA.
}

{
 Contributors
   Felipe Monteiro de Carvalho (felipemonteiro.carvalho@gmail.com)

 Known Issues

   - Quickly written and completely undocumented... but there's nothing
     complicated going on in this unit.

 Change log

  Initially written on 28-06-2003 TL
  Pre-release 30-06-2003 TL - Needs testing on systems without CPUID and TSC hardware

  15-11-2005
    Removed Linux unit to compile on Windows. It happens that main.pas didn´t
   use this unit.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ExtCtrls, EpikTimer, ComCtrls, Grids, newsw;

Const
  MaxStopwatches=10;
  
type

  TForm1 = class(TForm)
    Button1: TBUTTON;
    Button10: TBUTTON;
    Button11: TBUTTON;
    Button12: TBUTTON;
    Button13: TBUTTON;
    Button14: TBUTTON;
    Button15: TBUTTON;
    Button16: TBUTTON;
    Button17: TBUTTON;
    Button18: TBUTTON;
    Button2: TBUTTON;
    Button3: TBUTTON;
    Button4: TBUTTON;
    Button5: TBUTTON;
    Button6: TBUTTON;
    Button7: TBUTTON;
    Button8: TBUTTON;
    Button9: TBUTTON;
    Checkbox2: TCHECKBOX;
    Checkbox3: TCHECKBOX;
    Checkbox4: TCHECKBOX;
    Combobox1: TCOMBOBOX;
    Edit1: TEDIT;
    Edit11: TEDIT;
    Edit2: TEDIT;
    Edit3: TEDIT;
    Edit4: TEDIT;
    Edit5: TEDIT;
    Edit6: TEDIT;
    Edit7: TEDIT;
    Edit8: TEDIT;
    Edit9: TEDIT;
    Groupbox1: TGROUPBOX;
    Groupbox2: TGROUPBOX;
    Groupbox3: TGROUPBOX;
    Groupbox4: TGROUPBOX;
    Groupbox5: TGROUPBOX;
    Idletimer1: TIDLETIMER;
    Label1: TLABEL;
    Label10: TLABEL;
    Label11: TLABEL;
    Label12: TLABEL;
    Label13: TLABEL;
    Label16: TLABEL;
    Label17: TLABEL;
    Label2: TLABEL;
    Label3: TLABEL;
    Label4: TLABEL;
    Label5: TLABEL;
    Label6: TLABEL;
    Label7: TLABEL;
    Label8: TLABEL;
    Label9: TLABEL;
    Memo1: TMEMO;
    Memo2: TMEMO;
    Memo3: TMEMO;
    Memo4: TMEMO;
    Panel1: TPANEL;
    Radiogroup1: TRADIOGROUP;
    procedure Button18CLICK(Sender: TObject);
    procedure Button1CLICK(Sender: TObject);
    procedure Button2CLICK(Sender: TObject);
    procedure Button3CLICK(Sender: TObject);
    procedure Button4CLICK(Sender: TObject);
    procedure Button5CLICK(Sender: TObject);
    procedure Button6CLICK(Sender: TObject);
    procedure Button7CLICK(Sender: TObject);
    procedure Button8CLICK(Sender: TObject);
    procedure Checkbox2CLICK(Sender: TObject);
    procedure Checkbox3CLICK(Sender: TObject);
    procedure Combobox1CHANGE(Sender: TObject);
    procedure Form1CREATE(Sender: TObject);
    procedure Form1DESTROY(Sender: TObject);
    procedure Radiogroup1CLICK(Sender: TObject);

  private
     CurFreq,PrevFreq:Extended;
     CorrelationJitter:Extended;
     Iter:Integer;
  public
    ET:TEpikTimer;
  end;

var
  Form1: TForm1;
  NewStopwatches:array[1..MaxStopwatches] of Tform2;

implementation


{ TForm1 }

procedure TForm1.Button1CLICK(Sender: TObject);
Var TimerNumber:Integer;
begin
  TimerNumber:=StrtoInt(Tbutton(Sender).Caption);
  With Newstopwatches[TimerNumber] do
    Begin
      Left:=TimerNumber*8;
      Top:=TimerNumber*8;
      Caption:='Stopwatch '+Tbutton(Sender).Caption;
      visible:=true;
      show
    End
end;

procedure TForm1.Button18CLICK(Sender: TObject);
begin
  Showmessage('EpikTimer component demo program'+#10#10+
  'By Tom Lisjac <vlx@users.sourceforge.net>'+#10+
  'Additional information about this program can be found at http://theseus.sourceforge.net/fpclaz'+#10+
  'or by contacting the author.');
end;

procedure TForm1.Button2CLICK(Sender: TObject);
begin
  ET.Start;
  Edit5.text:=ET.WallClockTime;
end;

procedure TForm1.Button3CLICK(Sender: TObject);
begin
  ET.Stop;
  edit11.text:=ET.WallClockTime;
end;

procedure TForm1.Button4CLICK(Sender: TObject);
begin
  edit2.text:=ET.ElapsedStr;
  Edit3.text:=ET.elapsedDHMS;
  edit8.text:=ET.WallClockTime;
end;

procedure TForm1.Button5CLICK(Sender: TObject);
begin
  ET.Clear;
  edit1.text:=ET.WallClockTime;
  edit2.text:=ET.ElapsedStr;
  Edit3.text:=ET.elapsedDHMS;
end;

procedure TForm1.Button6CLICK(Sender: TObject);
begin
  edit4.text:=format('%.0n',[extended(ET.GetHardwareTicks)]);
  edit6.text:=format('%.0n',[extended(ET.GetSystemTicks)]);
end;

procedure TForm1.Button7CLICK(Sender: TObject);
begin
  Inc(Iter);
  ET.CalibrateCallOverheads(ET.SelectedTimebase^);
  ET.CalibrateTickFrequency(ET.SelectedTimebase^);
  With ET.SelectedTimebase^ do
  Begin
    memo2.lines.insert(0,format('%2.0d:%.0n',[Iter,extended(SleepOverhead)]));
    memo3.lines.insert(0,format('%2.0d:%.0n',[Iter,extended(TicksOverhead)]));
    memo4.lines.insert(0,format('%2.0d:%.0n',[Iter,extended(TicksFrequency)]));
    CurFreq:=TicksFrequency;
    memo1.lines.insert(0,format('%2.0d:%.0n',[Iter,CurFreq-PrevFreq]));
  End;
  PrevFreq:=CurFreq;
end;

procedure TForm1.Button8CLICK(Sender: TObject);
Var
  CorrelatedTickFrequency:TickType;
  tbdata: TimeBaseData;
begin
  CorrelatedTickFrequency:=ET.GetTimebaseCorrelation;
  Edit9.text:=format('%.0n',[extended(CorrelationJitter-CorrelatedTickFrequency)]);
  Edit7.text:=format('%.0n',[extended(CorrelatedTickFrequency)]);
  CorrelationJitter:= CorrelatedTickFrequency;
  If checkbox4.checked then begin
    tbdata := ET.HWTimebase;
    tbdata.TicksFrequency := trunc(CorrelatedTickFrequency);
    ET.HWTimebase := tbdata;
  end;
end;

procedure TForm1.Checkbox2CLICK(Sender: TObject);
begin
  ET.WantMilliseconds:=Checkbox2.checked
end;

procedure TForm1.Checkbox3CLICK(Sender: TObject);
begin
  ET.WantDays:=Checkbox3.checked
end;

procedure TForm1.Combobox1CHANGE(Sender: TObject);
begin
  ET.StringPrecision:=ComboBox1.Itemindex;
end;

procedure TForm1.Form1CREATE(Sender: TObject);
  Var S:String;
begin
  Iter:=0;
  PrevFreq:=0.0;
  CorrelationJitter:=0.0;
  ET:=TEpikTimer.create(nil);
  RadioGroup1.itemindex:=ord(ET.TimebaseSource);
  ComboBox1.itemindex:=ET.StringPrecision;
  If ET.HWCapabilityDataAvailable then S:='Yes' else S:='No';
  Label6.caption:=label6.caption+S;
  If ET.HWTickSupportAvailable then S:='Yes' else S:='No';
  Label7.caption:=label7.caption+S;
  If ET.MicrosecondSystemClockAvailable then S:='Yes' else S:='No';
  Label11.caption:=Label11.caption+S;
  Checkbox2.checked:=ET.WantMilliseconds;
  Checkbox3.checked:=ET.WantDays;
  Application.showhint:=true;
end;

procedure TForm1.Form1DESTROY(Sender: TObject);
begin
  ET.free;
end;

procedure TForm1.Radiogroup1CLICK(Sender: TObject);
begin
  ET.TimebaseSource:= TickSources(Radiogroup1.itemindex);
  Radiogroup1.itemindex:=ord(ET.TimebaseSource); // verifies the change
  Iter:=0;
end;

initialization
{$i main.lrs}

end.

