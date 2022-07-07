{"Hello World" P65Pas program.}
uses Commodore64;
var
  a : array[4] of char; // 5 instead of 4 or the space is not printed
  i : byte;
begin
  a[0] := 'H';
  a[1] := 'O'; 
  a[2] := 'L';
  a[3] := 'A';
  
  for i := 0 to 3 do
    ChrOUT(a[i]); 
  end;  
  asm RTS end
end.
