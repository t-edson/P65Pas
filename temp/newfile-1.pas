  procedure func2(par1: byte): byte;
  begin
    exit(par1+1);
  end;
  
var 
  xbyte : byte;
begin
//  xbyte := ord('A')-64;
  xbyte :=  func2(1);
end.
