{Description of the program.}
procedure ChrOUT(c: char);
begin
end;

var
k : byte;
begin
ChrOUT('H');
repeat 
  ChrOUT('H'); // this is not printed
until true; 
//asm RTS end
end.
