{Test code for validation of Variable declarations and Function Systems.}
uses Commodore64;
procedure bien;
begin
  CHROUT('O');
  CHROUT('K');
  CHROUT(chr(13));
end;
procedure Mal;
begin
  CHROUT('E');
  CHROUT('R');
  CHROUT('R');
  CHROUT('O');
  CHROUT('R');
  CHROUT(chr(13));
  //Aditional RTS to exit proc. and program.
  asm RTS end   
end;
var  //simple type declarations
  vbool: boolean;
  vchar: char;
	vbyte: byte;
  vword: word;
  byteInit: byte = $10;
  wordInit: word = $1234;
var  //absolute type declarations
  abool: boolean absolute vbool;
  achar: char absolute vchar;
	abyte: byte absolute vbyte;
  aword: word absolute vword;
  byteL: byte absolute aword.low;
  byteH: byte absolute aword.high;
const
  consWord = word($0A00); 
begin

  STROUT(@'=== VAR DECLAR ==='#13#0);
  
  //Basic assigment
  vbool := false;
	if vbool then mal else bien end;
  vbool := true;
	if vbool then bien else mal end;

	vchar := ' ';
	if vchar = ' ' then bien else mal end;
  vchar := #65;
	if vchar = chr(65) then bien else mal end;
  vchar := #65;
	if vchar = 'A' then bien else mal end;

  //Initialization
  if byteInit = $10 then bien else mal end;
  if wordInit = $1234 then bien else mal end;
  	
	//Absolute position
  abool := false;
	if vbool then mal else bien end;
  abool := true;
	if vbool then bien else mal end;

  vbyte := 5;
	if abyte = 0 then mal else bien end;
  vbyte := 255;
	if abyte = 255 then bien else mal end;

  //Access to bytes of word
  vword:=$FF01;
  vbyte := vword.low;
	if vbyte = 1 then bien else mal end;
  vbyte := vword.high;
	if vbyte = 255 then bien else mal end;
  vbyte := consWord.high;
	if vbyte = 10 then bien else mal end;

  byteL := 0;
  aword := $2010;
	if byteL = $10 then bien else mal end;
	if byteH = $20 then bien else mal end;

  aword := $2010;
	if byteL = $10 then bien else mal end;

	aword := 0;
	byteL := 5;
	aword.high := aword.low;
	if aword = $0505 then bien else mal end;

	aword := 0;
  aword.low := $05;
	aword.high := aword.low;
	if aword = $0505 then bien else mal end;
	
  aword := $2010;
	if vword = $2010 then bien else mal end;
  
  //Access to bytes of dword
//  vdword := $01020304;
//  if vdword.low = 4 then bien else mal end;
//  if vdword.high = 3 then bien else mal end;
//  if vdword.extra= 2 then bien else mal end;
//  if vdword.ultra = 1 then bien else mal end;
//  if vdword.lowword = $0304 then bien else mal end;
//  if vdword.highword = $0102 then bien else mal end;
  asm RTS end 
end.

