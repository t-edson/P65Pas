{Test code for validation of Variable declarations and Function Systems.}
{$PROCESSOR PIC16F877A}
{$FREQUENCY 8Mhz}
{$OUTPUTHEX 'output.hex'}
uses UnitTest, PIC16F877A;
var  //simple type declarations
  vbit: bit;
  vbool: boolean;
  vchar: char;
	vbyte: byte;
  vword: word;
  vdword: dword;
begin
  SetAsOutput(pinLed);

  ///////////////////////////////////////////////////
  ///// System Function
  ///////////////////////////////////////////////////

  /////// Inc() procedure
  vchar  := #0;
  vbyte  := 0;
  vword  := 0;
  vdword := 0;
  Inc(vchar ); if vchar  = #1       then good else bad end;
  Inc(vbyte ); if vbyte  = 1        then good else bad end;
  Inc(vword ); if vword  = word(1)  then good else bad end;
  Inc(vdword); if vdword = dword(1) then good else bad end;

  vword  := $FF;
  Inc(vword ); if vword  = $100  then good else bad end;
  vdword := $FFFF;
  Inc(vdword); if vdword = $10000 then good else bad end;

  /////// Dec() procedure
  vchar  := #0;
  vbyte  := 0;
  vword  := 0;
  vdword := 0;
  Dec(vchar ); if vchar  = #255      then good else bad end;
  Dec(vbyte ); if vbyte  = $FF       then good else bad end;
  Dec(vword ); if vword  = $FFFF     then good else bad end;
  Dec(vdword); if vdword = $FFFFFFFF then good else bad end;

  vword  := $100;
  Dec(vword ); if vword  = word($FF) then good else bad end;
  vdword := $10000;
  Dec(vdword); if vdword = dword($FFFF) then good else bad end;
  
  /////// Ord() function
  vchar := 'A';
  vbyte := Ord(vchar);
  if vbyte  = 65  then good else bad end;
  vbyte := Ord('B');
  if vbyte  = 66  then good else bad end;
  vbyte := 34+Ord('B');
  if vbyte  = 100  then good else bad end;
  vbyte := (vbyte + 1)+Ord('A');
  if vbyte  = 166  then good else bad end;
  

  /////// Chr() function
  vbyte := 65;
  vchar := Chr(vbyte);
  if vchar = 'A'  then good else bad end;
  vchar := Chr(66);
  if vchar  = 'B'  then good else bad end;
  vbyte := 1;
  vchar := Chr(65+vbyte);
  if vchar  = 'B'  then good else bad end;

  /////// Bit() function
  vbit := bit(65);
  if vbit = 1  then good else bad end;
  vbit := bit(0);
  if vbit = 0  then good else bad end;

  vbyte := 65;
  vbit := bit(vbyte);
  if vbit = 1  then good else bad end;
  
  vbyte := 0;
  vbit := bit(vbyte);
  if vbit  = 0  then good else bad end;
  vbit := bit(vbyte+1);
  if vbit  = 1  then good else bad end;
  if bit(2) and 1 = 1 then good else bad end;
  
  vbool := true;
  if bit(vbool) = 1 then good else bad end;
  if bit(not vbool) = 0 then good else bad end;
  if bit(not vbool) and bit(vbool) = 0 then good else bad end;
  if bit(not vbool) or bit(vbool) = 1 then good else bad end;

  /////// Boolean() function
  vbool := boolean(65);
  if vbool  then good else bad end;
  vbool := boolean(0);
  if not vbool then good else bad end;

  vbyte := 65;
  vbool := boolean(vbyte);
  if vbool then good else bad end;
  
  vbyte := 0;
  vbool := boolean(vbyte);
  if not vbool then good else bad end;
  vbool := boolean(vbyte+1);
  if vbool then good else bad end;
  if boolean(2) and true then good else bad end;
  
  vbit := 1;
  if boolean(vbit) then good else bad end;
  if not boolean(not vbit) then good else bad end;
  if not (boolean(not vbit) and boolean(vbit) ) then good else bad end;
  if boolean(not vbit) or boolean(vbit) then good else bad end;
  
  /////// Byte() function
  vbyte := byte(65);  //byte of byte
  if vbyte = 65  then good else bad end;
  vbyte := byte('A');  //byte of char
  if vbyte = 65  then good else bad end;
  vbyte := byte($FFFF);  //byte of word
  if vbyte = $FF  then good else bad end;

  vbit := 0;
  if byte(vbit) = 0  then good else bad end;
  vbit := 1;
  if byte(vbit)+1 = 2  then good else bad end;

  vbit := 1;
  if byte(vbit and 0) = 0  then good else bad end;

  vbyte := 10;
  if byte(vbyte) = 10  then good else bad end;
  vbyte := 10;
  if byte(vbyte+5) = 15  then good else bad end;

  vchar := '0';
  if byte(vchar) = 48  then good else bad end;
  
  vword := $0102;
  if byte(vword) = 2  then good else bad end;
  if byte(vword+1) = 3  then good else bad end;

  vdword := $01020304;
  if byte(vdword) = 4 then good else bad end;
  if byte(vdword+dword(1)) = 5 then good else bad end;

  /////// Word() function
  vword := word(65);  //word of byte
  if vword = word(65) then good else bad end;
  vword := word('A');  //word of char
  if vword = word(65)  then good else bad end;
  vword := word($FFFF);  //word of word
  if vword = $FFFF  then good else bad end;
  vword := word($01020304);  //word of dword
  if vword = $0304  then good else bad end;

  vbyte := 65;
  if word(vbyte) = word(65) then good else bad end;
  if word(vbyte+1) = word(66) then good else bad end;
  vchar := 'A';  //word of char
  if word(vchar) = word(65)  then good else bad end;
  vword := $FFFF;  //word of word
  if word(vword) = $FFFF  then good else bad end;
  if word(vword+1) = word(0) then good else bad end;
  vdword := $01020304;  //word of dword
  if word(vdword) = $0304  then good else bad end;
  if word(vdword+dword(1)) = $0305  then good else bad end;

  /////// DWord() function
  vdword := dword(65);  //dword of byte
  if vdword = dword(65) then good else bad end;
  vdword := dword('A');  //dword of char
  if vdword = dword(65)  then good else bad end;
  vdword := dword($FFFF);  //dword of word
  if vdword = dword($FFFF) then good else bad end;
  vdword := dword($01020304);  //dword of dword
  if vdword = $01020304  then good else bad end;

  vbyte := 65;
  if dword(vbyte) = dword(65) then good else bad end;
  if dword(vbyte+1) = dword(66) then good else bad end;
  vchar := 'A';  //dword of char
  if dword(vchar) = dword(65)  then good else bad end;
  vword := $FFFF;  //dword of word
  if dword(vword) = dword($FFFF)  then good else bad end;
  if dword(vword+1) = dword(0) then good else bad end;
  vdword := $01020304;  //dword of dword
  if dword(vdword) = $01020304  then good else bad end;
  if dword(vdword+dword(1)) = $01020305  then good else bad end;
  
  /////// SetAsInput() function
  {Cuidado que esto puede fallar en Proteus, porque parece que no maneja
  bien los bits no implementados, y puede leer $FF u otro valor en TRISA}
//  SetAsInput(PORTA);
//  if TRISA = $3F then good else bad end;

  SetAsInput(PORTB);
  if TRISB = $FF then good else bad end;
  
  /////// SetAsOutput() function
  SetAsOutput(PORTA);
  if TRISA = 0 then good else bad end;
  SetAsOutput(PORTB);
  if TRISB = 0 then good else bad end;
  
end.

