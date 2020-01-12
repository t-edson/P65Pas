{Rutina de verificación para operaciones básicas con datos de 
tipo Boolean.}
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
end;

var 
  a, b: boolean;
  vbyte: byte;
begin
  STROUT(@'=== BOOLEAN TEST ==='#13#0);

	//Pruebas con IF
	if true then bien else mal end;
	if false then mal else bien end;

  //////////////////////////////////////////////////////////
	//////////////  Operación Igualdad ///////////////////
  //////////////////////////////////////////////////////////

	//Pruebas con Boolean - constante
  if false = false then bien else mal end;
  if false = true then mal else bien end;
  if true = true then bien else mal end;
  if true = false then mal else bien end;

  if false <> false then mal else bien end;
  if true <> false then bien else mal end;

	//Pruebas con Boolean - variable
  a := true;
  b := false;
  if a = a then bien else mal end;
  if a = b then mal else bien end;  //FAIL
  if a <> a then mal else bien end;
  if a <> b then bien else mal end;

	//Pruebas con Boolean - variable - constante
  a := true;
  b := false;
  if a = true then bien else mal end;
  if a = false then mal else bien end;
  if false <> b then mal else bien end;
  if true <> b then bien else mal end;

	//Pruebas con BIT - expresión
  a := true;
  b := false;
  if a = (b and a) then mal else bien end;
//  if (b and a) = a then mal else bien end;
  if (a and true) = true then bien else mal end;
  if (a and true) = false then mal else bien end;

////  if (a and b) = (b and a) then bien else mal end;
////  if (a and b) <> (b and a) then mal else bien end;
//	
//	//Operación lógica NOT
//  if not false = true then bien else mal end;
//  if not true = false then bien else mal end;
//  a := false; 
//  if not a = true then bien else mal end;
//  if not a = false then mal  else bien end;
////  if not a = not a then bien else mal end;
//
//  //////////////////////////////////////////////////////////
//	//////////////  Operación lógica AND ///////////////////
//  //////////////////////////////////////////////////////////
//
//	//Operación lógica AND - constantes
//  if false and false = false then bien else mal end;
//  if false and true = false then bien else mal end;
//  if true and false = false then bien else mal end;
//  if true and true = true then bien else mal end;
//
//	//Operación lógica AND - variables
//  a := false; b := false;
//  if a and b = false then bien else mal end;
//  a := false; b := true;
//  if a and b = false then bien else mal end;
//  a := true; b := false;
//  if a and b = false then bien else mal end;
//  a := true; b := true;
//  if a and b = true then bien else mal end;
//
//	//Operación lógica AND - variables y constantes
//  a := false; 
//  if a and false = false then bien else mal end;
//  if false and a = false then bien else mal end;
//  if a and true = false then bien else mal end;
//  if true and a = false then bien else mal end;
//  a := true; 
//  if a and false = false then bien else mal end;
//  if false and a = false then bien else mal end;
//  if a and true = true then bien else mal end;
//  if true and a = true then bien else mal end;
//
//	//Operación lógica AND - lógica invertida
//  a := false; b := false;
//  if a and not b = false then bien else mal end;  //lógica invertida
//  if not a and b = false then bien else mal end;  //lógica invertida
////  if (not a and not b) = true then bien  else mal end;  //lógica invertida
//
//  if a and a = a then bien  else mal end;  //lógica invertida
//  if a and not a = false then bien  else mal end;  //lógica invertida
//
//  //////////////////////////////////////////////////////////
//	//////////////  Operación lógica OR ///////////////////
//  //////////////////////////////////////////////////////////
//	//Operación lógica OR - constantes
//  if false or false = false then bien else mal end;
//  if false or true = true then bien else mal end;
//  if true or false = true then bien else mal end;
//  if true or true = true then bien else mal end;
//
//	//Operación lógica OR - variables
//  a := false; b := false;
//  if a or b = false then bien else mal end;
//  a := false; b := true;
//  if a or b = true then bien else mal end;
//  a := true; b := false;
//  if a or b = true then bien else mal end;
//  a := true; b := true;
//  if a or b = true then bien else mal end;
//
//	//Operación lógica OR - variables y constantes
//  a := 0; 
//  if a or 0 = 0 then bien else mal end;
//  if 0 or a = 0 then bien else mal end;
//  if a or 1 = 1 then bien else mal end;
//  if 1 or a = 1 then bien else mal end;
//  a := 1; 
//  if a or 0 = 1 then bien else mal end;
//  if 0 or a = 1 then bien else mal end;
//  if a or 1 = 1 then bien else mal end;
//  if 1 or a = 1 then bien else mal end;
//
//	//Operación lógica OR - lógica invertida
//  a := 0; b := 0;
//  if a or not b = 1 then bien else mal end;  //lógica invertida
//  if not a or b = 1 then bien else mal end;  //lógica invertida
//  if (not a or not b) = 1 then bien  else mal end;  //lógica invertida
//
//  if a or a = a then bien  else mal end;  //lógica invertida
//  if a or not a = 1 then bien  else mal end;  //lógica invertida
//
//  //////////////////////////////////////////////////////////
//	//////////////  Operación lógica XOR ///////////////////
//  //////////////////////////////////////////////////////////
//	//Constantes
//  if 0 xor 0 = 0 then bien else mal end;
//  if 0 xor 1 = 1 then bien else mal end;
//  if 1 xor 0 = 1 then bien else mal end;
//  if 1 xor 1 = 0 then bien else mal end;
//
//	//Variables
//  a := 0; b := 0;
//  if a xor b = 0 then bien else mal end;
//  a := 0; b := 1;
//  if a xor b = 1 then bien else mal end;
//  a := 1; b := 0;
//  if a xor b = 1 then bien else mal end;
//  a := 1; b := 1;
//  if a xor b = 0 then bien else mal end;
//
//	//Variables y constantes
//  a := 0; 
//  if a xor 0 = 0 then bien else mal end;
//  if 0 xor a = 0 then bien else mal end;
//  if a xor 1 = 1 then bien else mal end;
//  if 1 xor a = 1 then bien else mal end;
//  a := 1; 
//  if a xor 0 = 1 then bien else mal end;
//  if 0 xor a = 1 then bien else mal end;
//  if a xor 1 = 0 then bien else mal end;
//  if 1 xor a = 0 then bien else mal end;
//
//	//Lógica invertida
//  a := 0; b := 0;
//  if a xor not b = 1 then bien else mal end;  //lógica invertida
//  if not a xor b = 1 then bien else mal end;  //lógica invertida
//  if (not a xor not b) = 0 then bien  else mal end;  //lógica invertida
//
//  //////////////////////////////////////////////////////////
//	//////////////// Operaciones Mixtas // //////////////////
//  //////////////////////////////////////////////////////////
//  //Operaciones con la misma variable
//  a := 0;
//  a := a;
//  if (a=a) and (a=0) then bien else mal end;
//  a := 1;
//  a := a;
//  if (a=a) and (a=1) then bien else mal end;
//   
//  a := 0;
//  a := not a;
//  if (a=1) then bien else mal end;
//
//  vbyte.bit7 := 0; 
//  vbyte.bit7 := vbyte.bit7;
//  if (vbyte.bit7=vbyte.bit7) and (vbyte.bit7=0) then bien else mal end;
//
//  vbyte.bit7 := 1; 
//  vbyte.bit7 := vbyte.bit7;
//  if (vbyte.bit7=1) then bien else mal end;
//
//  vbyte.bit7 := 0; 
//  vbyte.bit7 := not vbyte.bit7;
//  if vbyte.bit7 = 1 then bien else mal end;
//  
//  //Oepraciones con dos variables    
//  a := 0; b := 0;
//  if (a = 0) and (b = 0) then bien else mal end;
//  if (not a and not b) = 1 then bien else mal end;
//
//  a := 1; b := 0;
//  if (a = 1) and (b = 0) then bien else mal end;
//  if (a and not b) = 1 then bien else mal end;
//
//
//  a := 0; b := 0;
//  if a xor b = (not a and b or not b and a) then bien else mal end;
//  if (not a and b or not b and a) = (a xor b) then bien else mal end;
//  a := 1; b := 0;
//  if a xor b = (not a and b or not b and a) then bien else mal end;
//  if (not a and b or not b and a) = (a xor b) then bien else mal end;
//  a := 1; b := 1;
//  if a xor b = (not a and b or not b and a) then bien else mal end;
//  if (not a and b or not b and a) = (a xor b) then bien else mal end;
  asm RTS end 
end.

