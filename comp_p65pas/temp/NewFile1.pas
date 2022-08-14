{$ORG $0801}
//{$BOOTLOADER $0C,$08,$0A,$00,$9E,$20, $32,$30,$36,$32,  $00,$00,$00,'JMP','COD_HL'}
{$BOOTLOADER $0C,$08,$0A,$00,$9E,'COD_4A',$00,$00,$00}
//{$BOOTLOADER 'JMP','COD_HL'}
//{$BOOTLOADER JMP}
//{$ORG $0801}
//{$BOOTLOADER C64}
var
  x: BYTE;
begin
  x := 1;
  asm 
	rts 
  end; 
end.

