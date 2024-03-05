program test;
{$MODE PASCAL}
{$ORG $0801}
type 
  Tpos = object
    x: byte;
  end;
var
  xx: Array[10] of Byte;
  w: word;
var 
  xbyte : byte;
  location, ptr: ^byte;

procedure proc1;
var
   b: Byte;
begin
   for b:= 0 to 9 do begin
      xx[b]:= b;
   end;
end;

begin
  proc1;
  xbyte := ord('A')-64;
  location := @xbyte;
  location^ := 5;
  w := xbyte;
  w := w + 5;
end.


