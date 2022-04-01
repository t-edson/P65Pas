{$ORG $0801}
var 
  xbyte : byte;
  location, ptr: ^byte;
begin
//  xbyte := ord('A')-64;
//  xbyte :=  func2(1);
  location := @xbyte;
  location += 1;
  location^ := 5;
end.
