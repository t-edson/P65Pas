//uses Commodore64;
{$ORG $0801}
procedure Init;
begin
//  asm 
//	org $0801 
//  end; 
end; 
var
  x: byte;
begin
  asm 
	org $0801 
  DB $0C,$08,$0A,$00,$9E,$20,$32,$30,$36,$32
  DB $00,$00,$00
  end; 
  Init;
  x := 1;
end. 
