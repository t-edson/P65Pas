uses Commodore64;
var
  border: byte absolute 53280;
begin
  while true do
    inc(border);
    delay_ms(word(100));
  end;
end.

