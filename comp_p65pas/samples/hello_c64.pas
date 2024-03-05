{Description of the program.}
program nombre;
uses Commodore64;
var w:word;
begin
    CLRSCR;
    w := 5;
    STROUT(@'HOLA MUNDO');
    asm rts end 
end.
