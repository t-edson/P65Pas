{Rutina de verificación para operaciones con datos de tipo byte.
Se debe simular el programa en el circuito "Test3.DSN". Se debe 
escuchar, una serie de pitidos cortos. Si se escucha un pitido 
largo, es que hubo algún error en el resultado de alguna operación.
Por: Tito Hinostroza
Modificado: 01/03/2018}
{$PROCESSOR PIC16F877A}
{$FREQUENCY 8Mhz}
{$OUTPUTHEX 'output.hex'}
uses UnitTest, PIC16F877A;
var
  a, b: byte;

begin
  SetAsOutput(pinLed);
  pinLed := 0;

	//Pruebas con IF
	if true then good else bad end;
	if false then bad else good end;

  //////////////////////////////////////////////////////////
	//////////////  Operación Igualdad ///////////////////
  //////////////////////////////////////////////////////////

	//coConst_Const
  if 0 = 0 then good else bad end;
  if 0 = 1 then bad else good end;
  if 255 = 255 then good else bad end;
  if 0 <> 0 then bad else good end;
  if 255 <> 0 then good else bad end;

	//coConst_Variab
  a := 10;
  if 10 = a then good else bad end;
  if 0 = a then bad else good end;
  if 10 <> a then bad else good end;
  a := 255;
  if 255 = a then good else bad end;

  //coVariab_Const
  a := 10;
  if a = 10 then good else bad end;
  if a = 0 then bad else good end;
  if a <> 10 then bad else good end;
  if a = 5+5 then good else bad end;

	//coVariab_Variab
  a := 0;
  b := 255;
  if a = a then good else bad end;
  if a = b then bad else good end;
  if a <> a then bad else good end;
  a := 255;
  if a = a then good else bad end;
  if a = b then good else bad end;
  if a <> a then bad else good end;

  //coConst_Expres
  a := 0;
  if 11 = a+11 then good else bad end;

  //coExpres_Const
  if a+255 = 255 then good else bad end;
  
  //coVariab_Variab
  a:= 255; b := 255;
  if a = b then good else bad end;
  
	//coExpres_Expres
  a := 10;
  if a+1 = a+1 then good else bad end;
  if a+1 = a+2 then bad else good end;

  //////////////////////////////////////////////////////////
	//////////////////  Operación Mayor /////////////////////
  //////////////////////////////////////////////////////////

	//Constante
  if 0 > 0 then bad else good end;
  if 0 > 1 then bad else good end;
  if 255 > 0 then good else bad end;
  if 255 > 15 then good else bad end;
  if 255 > 255 then bad else good end;

	//Variables
  a := 1;  b := 255;
  if a > a then bad else good end;
  if a > b then bad else good end;
  if b > b then bad else good end;
  if b > a then good else bad end;

  a := 0;  b := 255;
  if a > a then bad else good end;
  if a > b then bad else good end;
  if b > b then bad else good end;
  if b > a then good else bad end;

  a := 0;  b := 1;
  if a > a then bad else good end;
  if a > b then bad else good end;
  if b > b then bad else good end;
  if b > a then good else bad end;

  a := 0;  b := 0;
  if a > a then bad else good end;
  if a > b then bad else good end;
  if b > b then bad else good end;
  if b > a then bad else good end;

  a := 255;  b := 255;
  if a > a then bad else good end;
  if a > b then bad else good end;
  if b > b then bad else good end;
  if b > a then bad else good end;

	//Variables - constantes
  a := 1;
  if a > 0 then good else bad end;
  if a > 1 then bad else good end;
  if a > 255 then bad else good end;
  if 0 > a then bad else good end;
  if 1 > a then bad else good end;
  if 2 > a then good else bad end;
  if 255 > a then good else bad end;

	//Expresiones
  a := 10;
  if 0 > a-10 then bad else good end;
  if 0 > a+1 then bad else good end;
  if 11 > a+1 then bad else good end;
  if 12 > a+1 then good else bad end;
  if 255 > a+1 then good else bad end;

  a := 10;
  if a+1 > 0 then good else bad end;
  if a+1 > 10 then good else bad end;
  if a+1 > 11 then bad else good end;
  if a+1 > 12 then bad else good end;
  if a+1 > 255 then bad else good end;

  if a > a+1 then bad else good end;
  if a+1 > a then good else bad end;

  if a+1 > a+2 then bad else good end;
  if a+1 > a+1 then bad else good end;
  if a+2 > a+1 then good else bad end;

  //////////////////////////////////////////////////////////
	//////////////////  Operación Menor /////////////////////
  //////////////////////////////////////////////////////////
	{Las pruebas para las operaciones "menor que", no son tan exigentes porque 
   se generan a partir de "mayor que"}

	//Constante
  if 0 < 0 then bad else good end;
  if 0 < 1 then good else bad end;
  if 255 < 0 then bad else good end;

	//Variables
  a := 0;  b := 0;
  if a < a then bad else good end;
  if a < b then bad else good end;
  if b < a then bad else good end;

  a := 255;  b := 255;
  if a < a then bad else good end;
  if a < b then bad else good end;
  if b < a then bad else good end;

	//Variables - constantes
  a := 1;
  if a < 0 then bad else good end;
  if a < 1 then bad else good end;
  if a < 255 then good else bad end;
  if 0 < a then good else bad end;
  if 255 < a then bad else good end;

	//Expresiones
  a := 10;
  if a < a+1 then good else bad end;
  if a+1 < a then bad else good end;

  if a+1 < a+2 then good else bad end;
  if a+1 < a+1 then bad else good end;
  if a+2 < a+1 then bad else good end;

  //////////////////////////////////////////////////////////
	///////////////  Operación Mayor/Menor o igual ///////////
  //////////////////////////////////////////////////////////
	{Estas operaciones, son aún menos exigentes, porque son negaciones 
  de las operaciones base}

	if 5>=4 then good else bad end;
	if 5>=5 then good else bad end;
	if 5>=6 then bad else good end;

  a := 5;  b := 6;
  if a >= a then good else bad end;
  if a >= b then bad else good end;
  if b >= a then good else bad end;

	if a <= a then good else bad end;
	if a <= 6 then good else bad end;
	if a <= 4 then bad else good end;

  //////////////////////////////////////////////////////////
	/////////////////////  Operación AND /////////////////////
  //////////////////////////////////////////////////////////
  a := $FF; b:=$0F;
  //coConst_Variab
  if $00 and a = 0 then good else bad end;
  if $FF and b = $0F then good else bad end;
  //coConst_Expres
  if $FF and (b+1) = $10 then good else bad end;
  //coVariab_Const
  if b and $01 = $01 then good else bad end;
  //coVariab_Variab
  if a and b = $0F then good else bad end;
  //coVariab_Expres
  if a and (b+1) = $10 then good else bad end;
  //coExpres_Const    
  if (b+1) and $0F = $0 then good else bad end;
  //coExpres_Variab
  if (b+1) and a = $10 then good else bad end;
  //coExpres_Expres
  if (b+1) and (a+1) = $00 then good else bad end;

  //////////////////////////////////////////////////////////
	/////////////////////  Operación OR /////////////////////
  //////////////////////////////////////////////////////////
  a := $FF; b:=$0F;
  //coConst_Variab
  if $00 or a = $FF then good else bad end;
  if $00 or b = $0F then good else bad end;
  //coConst_Expres
  if $00 or (b+1) = $10 then good else bad end;
  //coVariab_Const
  if b or $01 = $0F then good else bad end;
  //coVariab_Variab
  if a or b = $FF then good else bad end;
  //coVariab_Expres
  if a or (b+1) = $FF then good else bad end;
  //coExpres_Const    
  if (b+1) or $0F = $1F then good else bad end;
  //coExpres_Variab
  if (b+1) or b = $1F then good else bad end;
  //coExpres_Expres
  if (b+1) or (a+1) = $10 then good else bad end;
  
  //////////////////////////////////////////////////////////
	/////////////////////  Operación XOR /////////////////////
  //////////////////////////////////////////////////////////
  a := $FF; b:=$0F;
  //coConst_Variab
  if $00 xor a = $FF then good else bad end;
  if $00 xor b = $0F then good else bad end;
  //coConst_Expres
  if $00 xor (b+1) = $10 then good else bad end;
  //coVariab_Const
  if b xor $01 = $0E then good else bad end;
  //coVariab_Variab
  if a xor b = $F0 then good else bad end;
  //coVariab_Expres
  if a xor (b+1) = $EF then good else bad end;
  //coExpres_Const    
  if (b+1) xor $0F = $1F then good else bad end;
  //coExpres_Variab
  if (b+1) xor b = $1F then good else bad end;
  //coExpres_Expres
  if (b+1) xor (a+1) = $10 then good else bad end;
    
  //////////////////////////////////////////////////////////
	/////////////////////  Operación NOT /////////////////////
  //////////////////////////////////////////////////////////
  a := $FF; 
  //coConst
  if not $00 = $FF then good else bad end;
  if not $0F = $F0 then good else bad end;
  //coConst
  if not a = $00 then good else bad end;

  //////////////////////////////////////////////////////////
	///////////////////  Desplaz. Derecha ////////////////////
  //////////////////////////////////////////////////////////
  //coConst_Const
  if 1>>0 = 1 then good else bad end;  
  if 2>>2 = 0 then good else bad end;  
  //coVariab_Const
  a := $ff;
  if a>>0 = $ff then good else bad end;
  if a>>1 = $7f then good else bad end;
  if a>>2 = $3f then good else bad end;
  if a>>3 = $1f then good else bad end;
  if a>>7 = $1 then good else bad end;
  if a>>8 = $0 then good else bad end;
  //coVariab_Variab
  a := $ff; 
  b := 0;
  if a>>b = $ff then good else bad end;
  b := 1;
  if a>>b = $7f then good else bad end;
  b := 8;
  if a>>b = $0 then good else bad end;
  //coExpres_Const
  a := $80;
  if (a+8)>>1 = $44 then good else bad end;
  if (a+8)>>2 = $22 then good else bad end;
  if (a+8)>>3 = $11 then good else bad end;
  if (a+8)>>4 = $08 then good else bad end;
  if (a+8)>>5 = $04 then good else bad end;
    
  //////////////////////////////////////////////////////////
	//////////////////  Desplaz. Izquierda ///////////////////
  //////////////////////////////////////////////////////////
  //coConst_Const
  if 1<<0 = 1 then good else bad end;  
  if 2<<2 = 8 then good else bad end;  
  
  //coVariab_Const
  a := $01;
  if a<<0 = $01 then good else bad end;
  if a<<1 = $02 then good else bad end;
  if a<<2 = $04 then good else bad end;
  if a<<3 = $08 then good else bad end;
  if a<<7 = $80 then good else bad end;
  if a<<8 = $00 then good else bad end;
    
  //coVariab_Variab
  a := $01;
  b := 0;
  if a<<b = $01 then good else bad end;
  b := 1;
  if a<<b = $02 then good else bad end;
  b := 8;
  if a<<b = $00 then good else bad end;
  
  //coExpres_Const
  a := $02;
  if (a+1)<<1 = $06 then good else bad end;
  if (a+1)<<2 = $0C then good else bad end;
  if (a+1)<<3 = $18 then good else bad end;
  if (a+1)<<4 = $30 then good else bad end;
  if (a+1)<<5 = $60 then good else bad end;
  
  //////////////////////////////////////////////////////////
	/////////////////////  FIN DE PRUEBAS  ///////////////////
  //////////////////////////////////////////////////////////
  pinLed := 0;
  delay_ms(50);
  pinLed := 1;
  delay_ms(70);
  pinLed := 0;
end.
