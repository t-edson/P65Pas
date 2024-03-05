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
  STROUT(@'=== BOOLEAN TEST ==='#13);

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
  if a = b then mal else bien end;  
  if a <> a then mal else bien end;
  if a <> b then bien else mal end;

	//Pruebas con Boolean - variable - constante
  a := true;
  b := false;
  if a = true then bien else mal end;
  if a = false then mal else bien end;
  if false <> b then mal else bien end;
  if true <> b then bien else mal end;

  a := false;
  b := a = false;
  if b then bien else mal end;
  b := a = true;
  if b then mal else bien end;

  a := false;
  b := false;
  b := a = b;
  if b then bien else mal end;  
  
	//Pruebas con Boolean - expresión
  a := true;
  b := false;
  if a = (b and a) then mal else bien end;
  if (b and a) = a then mal else bien end;
  if (a and true) = true then bien else mal end;
  if (a and true) = false then mal else bien end;

  if (a and b) = (b and a) then bien else mal end;
  if (a and b) <> (b and a) then mal else bien end;
	
	//Operación lógica NOT
  if not false = true then bien else mal end;
  if not true = false then bien else mal end;
  a := false; 
  if not a = true then bien else mal end;
  if not a = false then mal  else bien end;
  if not a = not a then bien else mal end;

  //////////////////////////////////////////////////////////
	//////////////  Operación lógica AND ///////////////////
  //////////////////////////////////////////////////////////

	//Operación lógica AND - constantes
  if false and false = false then bien else mal end;
  if false and true = false then bien else mal end;
  if true and false = false then bien else mal end;
  if true and true = true then bien else mal end;

	//Operación lógica AND - variables
  a := false; b := false;
  if a and b = false then bien else mal end;
  a := false; b := true;
  if a and b = false then bien else mal end;
  a := true; b := false;
  if a and b = false then bien else mal end;
  a := true; b := true;
  if a and b = true then bien else mal end;

	//Operación lógica AND - variables y constantes
  a := false; 
  if a and false = false then bien else mal end;
  if false and a = false then bien else mal end;
  if a and true = false then bien else mal end;
  if true and a = false then bien else mal end;
  a := true; 
  if a and false = false then bien else mal end;
  if false and a = false then bien else mal end;
  if a and true = true then bien else mal end;
  if true and a = true then bien else mal end;

	//Operación lógica AND - lógica invertida
  a := false; b := false;
  if a and not b = false then bien else mal end;  //lógica invertida
  if not a and b = false then bien else mal end;  //lógica invertida
//  if (not a and not b) = true then bien  else mal end;  //lógica invertida

  if a and a = a then bien  else mal end;  //lógica invertida
  if a and not a = false then bien  else mal end;  //lógica invertida

  //////////////////////////////////////////////////////////
	//////////////  Operación lógica OR ///////////////////
  //////////////////////////////////////////////////////////
	//Operación lógica OR - constantes
  if false or false = false then bien else mal end;
  if false or true = true then bien else mal end;
  if true or false = true then bien else mal end;
  if true or true = true then bien else mal end;

	//Operación lógica OR - variables
  a := false; b := false;
  if a or b = false then bien else mal end;
  a := false; b := true;
  if a or b = true then bien else mal end;
  a := true; b := false;
  if a or b = true then bien else mal end;
  a := true; b := true;
  if a or b = true then bien else mal end;

	//Operación lógica OR - variables y constantes
  a := false; 
  if a or false = false then bien else mal end;
  if false or a = false then bien else mal end;
  if a or true = true then bien else mal end;
  if true or a = true then bien else mal end;
  a := true; 
  if a or false = true then bien else mal end;
  if false or a = true then bien else mal end;
  if a or true = true then bien else mal end;
  if true or a = true then bien else mal end;

	//Operación lógica OR - lógica invertida
  a := false; b := false;
  if a or not b = true then bien else mal end;  //lógica invertida
  if not a or b = true then bien else mal end;  //lógica invertida
  if (not a or not b) = true then bien  else mal end;  //lógica invertida

  if a or a = a then bien  else mal end;  //lógica invertida
  if a or not a = true then bien  else mal end;  //lógica invertida

  //////////////////////////////////////////////////////////
	//////////////  Operación lógica XOR ///////////////////
  //////////////////////////////////////////////////////////
	//Constantes
  if false xor false = false then bien else mal end;
  if false xor true = true then bien else mal end;
  if true xor false = true then bien else mal end;
  if true xor true = false then bien else mal end;

	//Variables
  a := false; b := false;
  if a xor b = false then bien else mal end;
  a := false; b := true;
  if a xor b = true then bien else mal end;
  a := true; b := false;
  if a xor b = true then bien else mal end;
  a := true; b := true;
  if a xor b = false then bien else mal end;

	//Variables y constantes
  a := false; 
  if a xor false = false then bien else mal end;
  if false xor a = false then bien else mal end;
  if a xor true = true then bien else mal end;
  if true xor a = true then bien else mal end;
  a := true; 
  if a xor false = true then bien else mal end;
  if false xor a = true then bien else mal end;
  if a xor true = false then bien else mal end;
  if true xor a = false then bien else mal end;

	//Lógica invertida
  a := false; b := false;
  if a xor not b = true then bien else mal end;  //lógica invertida
  if not a xor b = true then bien else mal end;  //lógica invertida
  if (not a xor not b) = false then bien  else mal end;  //lógica invertida

  //////////////////////////////////////////////////////////
	//////////////// Operaciones Mixtas // //////////////////
  //////////////////////////////////////////////////////////
  //Operaciones con la misma variable
  a := false;
  a := a;
  if (a=a) and (a=false) then bien else mal end;
  a := true;
  a := a;
  if (a=a) and (a=true) then bien else mal end;
   
  a := false;
  a := not a;
  if (a=true) then bien else mal end;

  //Operaciones con dos variables    
  a := false; b := false;
  if (a = false) and (b = false) then bien else mal end;
  if (not a and not b) = true then bien else mal end;

  a := true; b := false;
  if (a = true) and (b = false) then bien else mal end;
  if (a and not b) = true then bien else mal end;


  a := false; b := false;
  if a xor b = (not a and b or not b and a) then bien else mal end;
  if (not a and b or not b and a) = (a xor b) then bien else mal end;
  a := true; b := false;
  if a xor b = (not a and b or not b and a) then bien else mal end;
  if (not a and b or not b and a) = (a xor b) then bien else mal end;
  a := true; b := true;
  if a xor b = (not a and b or not b and a) then bien else mal end;
  if (not a and b or not b and a) = (a xor b) then bien else mal end;
  asm RTS end 
end.

