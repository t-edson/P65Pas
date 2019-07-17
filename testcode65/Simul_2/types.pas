{Rutina de verificación para el manejo de tipos.
Se debe simular el programa en el circuito "Test1.DSN". Se debe 
escuchar, una serie de pitidos cortos. Si se escucha un pitido 
largo, es que hubo algún error en el resultado de alguna operación.}
{$PROCESSOR PIC16F877A}
{$FREQUENCY 8Mhz}
{$OUTPUTHEX 'output.hex'}
uses UnitTest, PIC16F877A;
var  //tipos básicos
  vbit  : bit;
  vbool : boolean;
	vbyte : byte;
  vchar : char;
	vword : word;
  vdword: dword;
type  //tipos equivalentes
  bit1   = bit;
  bool1  = boolean;
	byte1  = byte;
  char1  = char;
	word1  = word;
  dword1 = dword; 
var  //variables de tipos equivalentes
  vbit1  : bit1;
  vbool1 : bool1;
	vbyte1 : byte1;
  vchar1 : char1;
	vword1 : word1;
  vdword1: dword1;
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
  SetAsOutput(pinLed);
  pinLed := 0;

  //////////////////////////////////////////////////////
	//////////////  Existencia básica de tipos ///////////////////
  //////////////////////////////////////////////////////
  vbit := 1;
  if vbit = 1 then good else bad end;
  vbool := true;
  if vbool = true then good else bad end;
	vbyte := 1;
  if vbyte = 1 then good else bad end;
  vchar:= ' ';
  if vchar = ' ' then good else bad end;
	vword:= 1;
  if vword = word(1) then good else bad end;
	vdword:= 1;
  if vdword = dword(1) then good else bad end;

  //Operaciones con tipos equivalentes
  vbit1 := 1;
  if vbit1 = 1 then good else bad end;
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
  
	vdword1:= 1;
  if vdword1 = dword(1) then good else bad end;
	vdword1:= 1000;
  if vdword1 = dword(1000) then good else bad end;

  //Uso de variables de tipos equivalentes
  vword1 := 0;
  for vbyte1:=1 to 5 do 
    vword1 := vword1 + 1;
  end;
  if vword1 = word(5) then good else bad end;

  //Paso de parámetros a procedimientos  
  //  if proc1(1,2) = 3 then good else bad end;
  // NO IMPLEMENTADO
  
  //////////////////////////////////////////////////////
  //////////////// Punteros a byte /////////////////////
  //////////////////////////////////////////////////////
  ptrByte1 := 0;    //asignación constante
  if ptrByte1 = 0 then good else bad end;
  ptrByte2 := 255;  //asignación constante
  if ptrByte2 = 255 then good else bad end;
  m := 5;
  ptrByte1 := m;    //asignación byte 
  if ptrByte1 = 5 then good else bad end;
  
  ptrByte1 := $85;
  ptrByte2 := ptrByte1;    //asignación punteros
  if ptrByte2 = $85 then good else bad end;

  //Aritmética de punteros
  ptrByte2 := ptrByte1+$10;    //asignación expresión de punteros
  if ptrByte2 = $95 then good else bad end;
  
  ptrByte2 := ptrByte1-$10;    //asignación expresión de punteros
  if ptrByte2 = $75 then good else bad end;

  inc(ptrByte2); 
  dec(ptrByte2); 
  if ptrByte2 = $75 then good else bad end;
  
  //Acceso a variables
  m := $23;
  ptrByte1 := @m;

  n := $12;
  ptrByte2 := @n;

  if ptrByte1^ = $23 then good else bad end;
  ptrByte1^ := $FF;
  if m = $ff then good else bad end;

  m := $23;  
  ptrByte2^ := ptrByte1^;
  if n = $23 then good else bad end;

  ptrByte1 := @m;
  n := ptrByte1^;
  if m = n then good else bad end;

  //Operaciones con desreferencia stVarRefVar
  m := $12;
  if ptrByte1^ = $12 then good else bad end;
  if ptrByte1^ + 1 = $13 then good else bad end;
  if ptrByte1^ - 1 = $11 then good else bad end;
  if ptrByte1^ + ptrByte1^ = $24 then good else bad end;
  if $0f and ptrByte1^  = $02 then good else bad end;
  
  //Pendientes
//  delay_ms(ptrByte1^);
//  Inc(ptrByte1^);
//  Dec(ptrByte1^);
//  ptrByte1^.bit7 := 0;
//  chr(ptrByte1^);
//  bit(ptrByte1^);
//  word(ptrByte1^);
//  dword(ptrByte1^);

  //Operaciones con desreferencia stVarRefExp
  //Se asume para esta prueba que "n", está ubicado después de "m"
  //De otar forma no funcionará, porque ptrByte1+1, fallaría
  n := $12;  
  if (ptrByte1+1)^ = $12 then good else bad end;
  if (ptrByte1+1)^ + 1 = $13 then good else bad end;
  if (ptrByte1+1)^ - 1 = $11 then good else bad end;
  {Expresión muy compleja stVarRefExp + stVarRefExp. No implementada por ahora.
 //  if (ptrByte1+1)^ + (ptrByte1+1)^ = $24 then good else bad end;  
  }
  if $0f and (ptrByte1+1)^  = $02 then good else bad end;
  
  //Pendientes
//  delay_ms((ptrByte1+1)^);
//  Inc((ptrByte1+1)^);
//  Dec((ptrByte1+1)^);
//  (ptrByte1+1)^.bit7 := 0;
//  chr((ptrByte1+1)^);
//  bit((ptrByte1+1)^);
//  word((ptrByte1+1)^);
//  dword((ptrByte1+1)^);

  //////////////////////////////////////////////////////
  //////////////// Punteros a word /////////////////////
  //////////////////////////////////////////////////////
  ptrWord1 := 0;    //asignación constante
  if ptrWord1 = 0 then good else bad end;
  ptrWord2 := 255;  //asignación constante
  if ptrWord2 = 255 then good else bad end;
  m := 5;
  ptrWord1 := m;    //asignación byte 
  if ptrWord1 = 5 then good else bad end;
  
  ptrWord1 := $85;
  ptrWord2 := ptrWord1;    //asignación punteros
  if ptrWord2 = $85 then good else bad end;

  //Aritmética de punteros
  ptrWord2 := ptrWord1+$10;    //asignación expresión de punteros
  if ptrWord2 = $95 then good else bad end;
  
  ptrWord2 := ptrWord1-$10;    //asignación expresión de punteros
  if ptrWord2 = $75 then good else bad end;

  inc(ptrWord2); 
  dec(ptrWord2); 
  if ptrWord2 = $75 then good else bad end;
  
  //Acceso a variables
  x := $23;
  ptrWord1 := @x;

  y := $12;
  ptrWord2 := @y;

  if ptrWord1^ = word($23) then good else bad end;
  ptrWord1^ := word($FF);
  if x = word($ff) then good else bad end;

  x := $23;  
  ptrWord2^ := ptrWord1^;
  if y = word($23) then good else bad end;

  ptrWord1 := @x;
  y := ptrWord1^;
  if x = y then good else bad end;

  //Operaciones con desreferencia stVarRefVar
  x := $12;
  if ptrWord1^ = word($12) then good else bad end;
  if ptrWord1^ + word(1) = word($13) then good else bad end;
  if ptrWord1^ - word(1) = word($11) then good else bad end;
  if ptrWord1^ + ptrWord1^ = word($24) then good else bad end;
//  if word($0f) and ptrWord1^  = word($02) then good else bad end;
//  
//  //Pendientes
////  delay_ms(ptrWord1^);
////  Inc(ptrWord1^);
////  Dec(ptrWord1^);
////  ptrWord1^.bit7 := 0;
////  chr(ptrWord1^);
////  bit(ptrWord1^);
////  word(ptrWord1^);
////  dword(ptrWord1^);
//
//  //Operaciones con desreferencia stVarRefExp
//  //Se asume para esta prueba que "y", está ubicado después de "x"
//  //De otar forma no funcionará, porque ptrWord1+1, fallaría
//  y := $12;  
//  if (ptrWord1+1)^ = $12 then good else bad end;
//  if (ptrWord1+1)^ + 1 = $13 then good else bad end;
//  if (ptrWord1+1)^ - 1 = $11 then good else bad end;
//  {Expresión muy compleja stVarRefExp + stVarRefExp. No implementada por ahora.
// //  if (ptrWord1+1)^ + (ptrWord1+1)^ = $24 then good else bad end;  
//  }
//  if $0f and (ptrWord1+1)^  = $02 then good else bad end;
//  
//  //Pendientes
////  delay_ms((ptrWord1+1)^);
////  Inc((ptrWord1+1)^);
////  Dec((ptrWord1+1)^);
////  (ptrWord1+1)^.bit7 := 0;
////  chr((ptrWord1+1)^);
////  bit((ptrWord1+1)^);
////  word((ptrWord1+1)^);
////  dword((ptrWord1+1)^);
//
//
  //////////////////////////////////////////////////////
  ////////////////////// Arreglos  /////////////////////
  //////////////////////////////////////////////////////
//  achar[0] := 'a';
//  if 'a' = achar[0] then good else bad end;
//  vbyte := 1;
//  achar[vbyte] := 'a';
//  if achar[vbyte] = 'a' then good else bad end;
//
//  abyte[0] := 1;
//  vbyte := 1;
//  abyte[vbyte] := 1;
//  
//  vbyte := 1;
//  aword[0] := word(5000);
//  aword[vbyte] := word(5000);
end.
