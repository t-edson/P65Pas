uses Commodore64;
var 
  str: []char = 'HELLO';
begin
  asm 
    LDX #12    ; Select row 
    LDY #12    ; Select column 
    JSR $E50C   ; Set cursor 
    LDA #<str  ; Load lo-byte of string adress 
    LDY #>str  ; Load hi-byte of string adress 
    JSR $AB1E     ; Print string
    RTS	 
  end
end.
