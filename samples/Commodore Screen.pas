{Change the back color of a Commodore 64 screen}
{$ORG $0801}
{$COMMODORE64}   //Commodore 64 mode
program C64ChamgeBack;
var
  border: byte absolute 53280;
begin
  while true do
    inc(border);
    delay_ms(word(100));
  end;
end.

