{Program to clear the screen of Commodore 64 using ASM blocks.}
program ClearScreen;
uses Commodore64;
begin
  ASM
     ; Taken from https://github.com/petriw/Commodore64Programming
     lda #$00
     sta $d020
     sta $d021
     tax
     lda #$20
loop: sta $0400,x
     sta $0500,x
     sta $0600,x
     sta $0700,x
     dex
     bne loop
     RTS
  END
end.

