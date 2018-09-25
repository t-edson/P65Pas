{$ORG $1000}
var
  borde: byte absolute 53280;
begin
  ASM
  lda #$00
  sta $d020
  sta $d021  
  END
  while true do 
    inc(borde);
    delay_ms(100);
  end; 
end.
