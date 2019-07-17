{Rutina de verificación del funcionamiento de las estructuras del 
lenguaje.
Se debe simular el programa en el circuito "Test1.DSN". Se debe 
escuchar, una serie de pitidos cortos. Si se escucha un pitido 
largo, es que hubo algún error en el resultado de alguna operación.}
{$FREQUENCY 8Mhz}
{$OUTPUTHEX 'output.hex'}
uses PIC16F84A;
var
  pinLed: bit absolute PORTB.0;
  a, i, fin: byte;
	b,c: boolean;
  procedure bien;
  begin
    pinLed := 1;
    delay_ms(30);
    pinLed := 0;
    delay_ms(70);
  end;
  procedure Mal;
  begin
    pinLed := 1;
    delay_ms(1500);
    pinLed := 0;
    asm SLEEP end
  end;
begin
  SetAsOutput(pinLed);
  pinLed := 0;

  //////////////////////////////////////////////////////////
	/////////////////////  IF-CONSTANTE //////////////////////
  //////////////////////////////////////////////////////////

	if true then bien; end; 
  if false then mal; end;
	//validación de ejecución completa de bloque	
	a := 0;
	if true then 
    inc(a);   //al inicio del bloque
    bien; 
    inc(a);		//al fin del bloque
  end; 
	if a<>2 then mal; end;
	//validación de no-ejecución completa de bloque	
	a := 0;
	if false then 
    mal;
    mal;
    mal;  //al fin del bloque
  end; 
  //validación mixta
	a := 0;
	if true then 
    inc(a);   //al inicio del bloque
    bien; 
    inc(a);		//al fin del bloque
  else
    mal;
    inc(a);		//al fin del bloque
    mal;  //al fin del bloque
  end; 
	if a<>2 then mal; end;
  //validación ELSIF 1
  a := 0;
	if true then 
    inc(a);		//al fin del bloque
    bien; 
    inc(a);		//al fin del bloque
  elsif true then 
	  mal;
	  mal;
  elsif true then 
	  mal;
	  mal;
  else
	  mal;
    mal;
  end; 
	if a<>2 then mal; end;
  //validación ELSIF 2
  a := 0;
	if false then 
	  mal;
	  mal;
  elsif true then 
    inc(a);		//al fin del bloque
    bien; 
    inc(a);		//al fin del bloque
  elsif true then 
    inc(a);		//al fin del bloque
    bien; 
    inc(a);		//al fin del bloque
  else
	  mal;
    mal;
  end; 
	if a<>2 then mal; end;
  //validación ELSIF 3
  a := 0;
	if not true then 
	  mal;
	  mal;
  elsif not true then 
	  mal;
    mal;
  else
    inc(a);		//al fin del bloque
    bien; 
    inc(a);		//al fin del bloque
  end; 
	if a<>2 then mal; end;


  //////////////////////////////////////////////////////////
	/////////////////////  IF-VARIABLE //////////////////////
  //////////////////////////////////////////////////////////
  b := true;
  if b then bien; else mal; end; 
  b := false;
  if b then mal; else bien; end; 
	//lógica invertida
  b := false;
  if not b then bien; else mal; end; 
  b := true;
  if not b then mal; else bien; end; 

	//validación de ejecución completa de bloque	
	b := true;
	a := 0;
	if b then 
    inc(a);   //al inicio del bloque
    bien; 
    inc(a);		//al fin del bloque
  end; 
	if a<>2 then mal; end;
	//validación de no-ejecución completa de bloque	
	b := false;
	if b then 
    mal;
    mal;
    mal;  //al fin del bloque
  end; 
  //validación mixta
	a := 0;
	b := true;
	if b then 
    inc(a);   //al inicio del bloque
    bien; 
    inc(a);		//al fin del bloque
  else
    mal;
    inc(a);		//al fin del bloque
    mal;  //al fin del bloque
  end; 
	if a<>2 then mal; end;
  //validación ELSIF 1
  a := 0;
	b := true;
	if b then 
    inc(a);		//al fin del bloque
    bien; 
    inc(a);		//al fin del bloque
  elsif b then 
	  mal;
	  mal;
  else
	  mal;
    mal;
  end; 
	if a<>2 then mal; end;

  //validación ELSIF 2
  a := 0;
	b := true;
	if not b then 
	  mal;
	  mal;
  elsif b then 
    inc(a);		//al fin del bloque
    bien; 
    inc(a);		//al fin del bloque
  else
	  mal;
    mal;
  end; 
	if a<>2 then mal; end;

  //validación ELSIF 3
  a := 0;
	b := true;
	if not b then 
	  mal;
	  mal;
  elsif not b then 
	  mal;
    mal;
  elsif not b then 
	  mal;
    mal;
  else
    inc(a);		//al fin del bloque
    bien; 
    inc(a);		//al fin del bloque
  end; 
	if a<>2 then mal; end;

  //////////////////////////////////////////////////////////
	/////////////////////  IF-EXPRESIÓN //////////////////////
  //////////////////////////////////////////////////////////
  b := true; c := true;

  if b and c then bien; else mal; end; 
  if b and not c then mal; else bien; end; 
	//lógica invertida
  if not (b and c) then mal; else bien; end; 
  //optimización C a Z
	if a+1>a then bien; else mal; end; 

	//validación de ejecución completa de bloque	
	a := 0;
	if b and c then 
    inc(a);   //al inicio del bloque
    bien; 
    inc(a);		//al fin del bloque
  end; 
	if a<>2 then mal; end;
	//validación de no-ejecución completa de bloque	
	if b and not c then 
    mal;
    mal;
    mal;  //al fin del bloque
  end; 
  //validación mixta
	a := 0;
	if b and c then 
    inc(a);   //al inicio del bloque
    bien; 
    inc(a);		//al fin del bloque
  else
    mal;
    inc(a);		//al fin del bloque
    mal;  //al fin del bloque
  end; 
	if a<>2 then mal; end;

  //validación ELSIF 1
  a := 0;
	if b and c then 
    inc(a);		//al fin del bloque
    bien; 
    inc(a);		//al fin del bloque
  elsif b and c then 
	  mal;
	  mal;
  else
	  mal;
    mal;
  end; 
	if a<>2 then mal; end;

  //validación ELSIF 2
  a := 0;
	b := true;
	if b and not c then 
	  mal;
	  mal;
  elsif b and c then 
    inc(a);		//al fin del bloque
    bien; 
    inc(a);		//al fin del bloque
  else
	  mal;
    mal;
  end; 
	if a<>2 then mal; end;
  //validación ELSIF 3
  a := 0;
	b := true;
	if not b and c then 
	  mal;
	  mal;
  elsif not b and c then 
	  mal;
    mal;
  elsif not b and c then 
	  mal;
    mal;
  else
    inc(a);		//al fin del bloque
    bien; 
    inc(a);		//al fin del bloque
  end; 
	if a<>2 then mal; end;

  //////////////////////////////////////////////////////////
	/////////////////////  WHILE //////////////////////
  //////////////////////////////////////////////////////////

  //Constantes
//  a := 0;    !!!!! No se puede probar un lazo infinito 
//  while true do 
//	  bien;
//  end; 

  while false do 
    mal;
    mal;
    mal;
  end;

	//Variables
//  b := true;      !!!!! No se puede probar un lazo infinito 
//  while b do 
//	  bien;
//  end; 

  b := false;
  while false do 
    mal;
    mal;
    mal;
  end;

	//Expresiones
	a := 0;
  while a<1 do 
    bien;
    inc(a);
  end; 
  if a<>1 then mal; end;

  //////////////////////////////////////////////////////////
	/////////////////////  REPEAT //////////////////////
  //////////////////////////////////////////////////////////
  a := 0;
  repeat 
    bien;
    inc(a);
  until true; 
  if a<>1 then mal; end;

//  a := 0;
//  repeat 
//    bien;
//    inc(a);
//  until false; 

	//Variables
  a := 0;
  b := true;
  repeat 
    bien;
    inc(a);
  until b; 
  if a<>1 then mal; end;

	//Expresiones
	a := 0;
  repeat
    bien;
    inc(a);
  until a>1;
  if a<>2 then mal; end;

  //////////////////////////////////////////////////////////
	/////////////////////////  FOR  //////////////////////////
  //////////////////////////////////////////////////////////
  //Constante, una iteración
  i := 0;
  a := 0;
  for i:=0 to 0 do 
 		bien;
    inc(a); 
  end;
  if a<>1 then mal; end;
  //Constante, varias iteraciones
  i := 0;
  a := 0;
  for i:=0 to 3 do 
 		bien;
    inc(a); 
  end;
  if a<>4 then mal; end;
  //Constante, sin iteraciones
  i := 0;
  a := 0;
  for i:=1 to 0 do 
 		bien;
    inc(a); 
  end;
  if a<>0 then mal; end;
  //Variable, una iteración
  i := 0; fin := 0;
  a := 0;
  for i:=0 to fin do 
 		bien;
    inc(a); 
  end;
  if a<>1 then mal; end;
  //Variable, varias iteraciones
  i := 0; fin := 3;
  a := 0;
  for i:=0 to fin do 
 		bien;
    inc(a); 
  end;
  if a<>4 then mal; end;

end.

