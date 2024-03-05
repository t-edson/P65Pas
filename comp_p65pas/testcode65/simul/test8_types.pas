{Rutina de verificación para el manejo de tipos.}
uses Commodore64;
procedure good;
begin
  CHROUT('O');
  CHROUT('K');
//  CHROUT(chr(13));
  CHROUT(',');
end;
procedure bad;
begin
  CHROUT('E');
  CHROUT('R');
  CHROUT('R');
  CHROUT('O');
  CHROUT('R');
  CHROUT(chr(13));
end;
var  //tipos básicos
//  vbit  : bit;
  vbool : boolean;
	vbyte : byte;
  vchar : char;
	vword : word;
//  vdword: dword;
type  //tipos equivalentes
//  bit1   = bit;
  bool1  = boolean;
	byte1  = byte;
  char1  = char;
	word1  = word;
//  dword1 = dword; 
var  //variables de tipos equivalentes
//  vbit1  : bit1;
  vbool1 : bool1;
	vbyte1 : byte1;
  vchar1 : char1;
	vword1 : word1;
//  vdword1: dword1;
type   //Tipos punteros
  byteptr = ^byte;
  wordptr = ^word;
type  //Tipos arreglos
  Tachar = array[3] of char;
  Tabyte = array[3] of byte;
  Taword = array[3] of word;
var  //Variables arreglos
  achar: Tachar;
  abyte: Tabyte;
  aword: Taword;
var  //Variables para punteros
  ptrByte1, ptrByte2: byteptr;
  ptrWord1, ptrWord2: wordptr; 
  m, n : byte;
  x, y : word;
   
  procedure proc1(x, y: byte1): word1;
  begin
    exit( word(x+y));
  end; 
  
begin
  //////////////////////////////////////////////////////
	//////////////  Existencia básica de tipos ///////////////////
  //////////////////////////////////////////////////////
//  vbit := 1;
//  if vbit = 1 then good else bad end;
  vbool := true;
  if vbool = true then good else bad end;
	vbyte := 1;
  if vbyte = 1 then good else bad end;
  vchar:= ' ';
  if vchar = ' ' then good else bad end;
	vword:= 1;
  if vword = word(1) then good else bad end;
//	vdword:= 1;
//  if vdword = dword(1) then good else bad end;

  //Operaciones con tipos equivalentes
//  vbit1 := 1;
//  if vbit1 = 1 then good else bad end;
  vbool1 := true;
  if vbool1 = true then good else bad end;
	vbyte1 := 1;
  if vbyte1 = 1 then good else bad end;
	vbyte1 := vbyte1 + 1;
  if vbyte1 = 2 then good else bad end;
  
  vchar1:= ' ';
  if vchar1 = ' ' then good else bad end;
	vword1:= 1;
  if vword1 = word(1) then good else bad end;
  
//	vdword1:= 1;
//  if vdword1 = dword(1) then good else bad end;
//	vdword1:= 1000;
//  if vdword1 = dword(1000) then good else bad end;

  //Uso de variables de tipos equivalentes
  vword1 := 0;
  for vbyte1:=1 to 5 do 
    vword1 := vword1 + 1;
  end;
  if vword1 = word(5) then good else bad end;

  //Paso de parámetros a procedimientos  
    if proc1(1,2) = 3 then good else bad end;
  

  asm rts end; 
end.
