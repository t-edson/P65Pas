{Test on accessing RAM video of Commodore64.}
uses Commodore64;
var
  screen: array[1024] of byte absolute 1024;
begin
  screen[13*40+0] := ord('H')-64;
  screen[13*40+1] := ord('E')-64;
  screen[13*40+2] := ord('L')-64;
  screen[13*40+3] := ord('L')-64;
  screen[13*40+4] := ord('O')-64;
  asm rts end;  
end.



//  loc: ^byte;
//  x: byte;
//
//  loc^ := 1;  

