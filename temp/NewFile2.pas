{$ORG $0801}
{$COMMODORE64}
program ClearScreen;
var
  borde: byte absolute 53280;
  i: byte;
begin
  i := 0;
  while true do
    borde := i;
    inc(i);
    delay_ms(1000);
  end; 
  asm RTS end
end.
