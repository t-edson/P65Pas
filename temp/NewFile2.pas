uses Commodore64;
type
  //Tpantalla = array[1024] of char; 
  Tpantalla = ^char; 
var
  border: byte absolute 53280;
  screen: Tpantalla;
  x: word;
begin
//  screen^ := 'A';
  x := 1;
//  while true do
//    inc(border);
//    delay_ms(100);
//  end; 
end. 
