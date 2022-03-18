unit splash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls;

type
  TForm3 = class(TForm)
    Label1: TLABEL;
    Label2: TLABEL;
    Label3: TLABEL;
    Label4: TLABEL;
    Progressbar1: TPROGRESSBAR;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form3: TForm3; 

implementation

initialization
  {$I splash.lrs}

end.

