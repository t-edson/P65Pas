{Rutina de verificación del funcionamiento de las estructuras del 
lenguaje, en modo de compatibilidad.
Se debe simular el programa en el circuito "Test1.DSN". Se debe 
escuchar, una serie de pitidos cortos. Si se escucha un pitido 
largo, es que hubo algún error en el resultado de alguna operación.}
{$Mode Pascal}
uses Commodore64;
var
  a: byte;
	b,c: boolean;

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

begin
  //////////////////////////////////////////////////////////
	/////////////////////  IF-CONSTANTE //////////////////////
  //////////////////////////////////////////////////////////

	if true then bien; 
  if false then mal; 
	//validación de ejecución completa de bloque	
	a := 0;
	if true then begin
    inc(a);   //al inicio del bloque
    bien; 
    inc(a);		//al fin del bloque
  end; 
	if a<>2 then mal; 
  //validación mixta
	a := 0;
	if true then begin
    inc(a);   //al inicio del bloque
    bien; 
    inc(a);		//al fin del bloque
  end else begin
    mal;
    inc(a);		//al fin del bloque
    mal;  //al fin del bloque
  end; 
	if a<>2 then mal; 

  //////////////////////////////////////////////////////////
	/////////////////////  IF-VARIABLE //////////////////////
  //////////////////////////////////////////////////////////
  b := true;
  if b then bien else mal; 
  b := false;
  if b then mal else bien; 

	//validación de ejecución completa de bloque	
	b := true;
	a := 0;
	if b then begin
    inc(a);   //al inicio del bloque
    bien; 
    inc(a);		//al fin del bloque
  end; 
	if a<>2 then mal; 
	//validación de no-ejecución completa de bloque	
	b := false;
	if b then begin
    mal;
    mal;
    mal;  //al fin del bloque
  end; 
  //validación mixta
	a := 0;
	b := true;
	if b then begin
    inc(a);   //al inicio del bloque
    bien; 
    inc(a);		//al fin del bloque
  end else begin
    mal;
    inc(a);		//al fin del bloque
    mal;  //al fin del bloque
  end; 
	if a<>2 then mal; 

  //////////////////////////////////////////////////////////
	/////////////////////  IF-EXPRESIÓN //////////////////////
  //////////////////////////////////////////////////////////
  b := true; c := true;

  if b and c then bien else mal; 
  if b and not c then mal else bien;
	//lógica invertida
  if not (b and c) then mal else bien; 
  //optimización C a Z
	if a+1>a then bien else mal; 

	//validación de ejecución completa de bloque	
	a := 0;
	if b and c then begin
    inc(a);   //al inicio del bloque
    bien; 
    inc(a);		//al fin del bloque
  end; 
	if a<>2 then mal; 
	//validación de no-ejecución completa de bloque	
	if b and not c then begin
    mal;
    mal;
    mal;  //al fin del bloque
  end; 
  //validación mixta
	a := 0;
	if b and c then begin
    inc(a);   //al inicio del bloque
    bien; 
    inc(a);		//al fin del bloque
  end else begin
    mal;
    inc(a);		//al fin del bloque
    mal;  //al fin del bloque
  end; 
	if a<>2 then mal; 

  //////////////////////////////////////////////////////////
	/////////////////////  WHILE //////////////////////
  //////////////////////////////////////////////////////////

  //Constantes
//  a := 0;    !!!!! No se puede probar un lazo infinito 
//  while true do begin
//	  bien;
//  end; 

  while false do begin
    mal;
    mal;
    mal;
  end;

	//Variables
//  b := true;      !!!!! No se puede probar un lazo infinito 
//  while b do begin
//	  bien;
//  end; 

  b := false;
  while false do begin
    mal;
    mal;
    mal;
  end;

	//Expresiones
	a := 0;
  while a<1 do begin
    bien;
    inc(a);
  end; 
  if a<>1 then mal; 

  asm rts end; 
end.
