{Test code to send a message to the Commodore64
screen .}
uses Commodore64;
begin
  CHROUT('H');
  CHROUT('E');
  CHROUT('L');
  CHROUT('L');
  CHROUT('O');
  asm RTS end 
end.

