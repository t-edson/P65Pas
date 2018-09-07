{Program to clear teh screen of Commodore 64 using ASM blocks.}
{$ORG $0801}
{$COMMODORE64}   //Commodore 64 mode
program ClearScreen;
begin
  ASM
    ; Taken from https://github.com/petriw/Commodore64Programming
    lda #$00
    sta $d020
    sta $d021
    tax
    lda #$20
    loop:   sta $0400,x
    sta $0500,x
    sta $0600,x
    sta $0700,x
    dex
    bne loop
    RTS
  END
end.

