program test;
{$MODE PASCAL}

var
  xx: Array[10] of Byte;+

procedure Run;
var
   b: Byte;
begin
   for b:= 0 to 9 do begin
      xx[b]:= b;
   end;
end;

begin
   Run;
end.

//{$ORG $0801}
//type 
//  Tpos = object
//  x: byte;
//  end;
////var 
////  xbyte : byte;
////  location, ptr: ^byte;
//begin
//////  xbyte := ord('A')-64;
//////  xbyte :=  func2(1);
////  location := @xbyte;
////  location += 1;
////  location^ := 5;
//end.
