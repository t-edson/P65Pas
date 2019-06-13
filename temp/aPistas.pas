program Hello;
uses Commodore64;
const 
  ScreenStart = $0400;
var 
  pantalla: byte absolute $0400;
procedure FillFromMem(add: word);
begin
  asm 
    LDX #0
    LDA #41
;    STA ScreenStart,x
  end 
end; 
var 
  i: byte;
  a: byte;
begin
  CLRSCR(7);
  for i:=1 to 20 do
    CHROUT('A');
  end; 
  FillFromMem($400);
  asm RTS end 
end.
