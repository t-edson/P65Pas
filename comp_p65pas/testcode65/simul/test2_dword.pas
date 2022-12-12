{Rutina de verificación para operaciones de comparación con datos 
de tipo DWORD.
Se debe simular el programa en el circuito "Test1.DSN". Se debe 
escuchar, una serie de pitidos cortos. Si se escucha un pitido 
largo, es que hubo algún error en el resultado de alguna operación.}
{$PROCESSOR PIC16F877A}
{$FREQUENCY 8Mhz}
{$OUTPUTHEX 'output.hex'}
uses UnitTest, PIC16F877A;
var

  a, b: Dword;

begin
  SetAsOutput(pinLed);
  pinLed := 0;

  //////////////////////////////////////////////////////////
	//////////////  Operación Igualdad ///////////////////
  //////////////////////////////////////////////////////////
	//coConst_Const
  if dword(0) = dword(0) then good else bad end;
  if dword(0) = dword(1) then bad else good end;
  if dword($FFFF) = dword($FFFF) then good else bad end;
  if 1265535 = 1265535 then good else bad end;

  if dword(0) <> dword(0) then bad else good end;
  if dword(255) <> dword(0) then good else bad end;
  if $12345678 <> $12345678 then bad else good end;

	//coConst_Variab
  a := $123456;
  if $123456 = a then good else bad end;
  a := 0;
  if dword(0) = a then good else bad end;
  a := $01020304;
  if dword($01020304) = a then good else bad end;
  a := $FFFFFFFF;
  if $FFFFFFFF = a then good else bad end;
  
  //coConst_Expres
  a := $123456;
  if $123457 = dword(1) + a then good else bad end;
  
  //coVariab_Const
  a := $123456;
  if a = $123456 then good else bad end;
  a := $1234;
  if a = dword($1234) then good else bad end;

  //coVariab_Variab
  a := $123456;
  b := $123456;
  if a = b then good else bad end;
  a := $1234;
  b := $1234;
  if a = b then good else bad end;
  
  //coVariab_Expres
  a := $123456;
  b := $123457;
  if b = dword(1) + a then good else bad end;
  
  //coExpres_Const
  a := $FFFFFFF;
  if dword(1) + a = $10000000 then good else bad end;

  //coExpres_Variab
  b := $10000000;
  if dword(1) + a = b then good else bad end;

  //coExpres_Expres
  a := 3141592654;
  b := 3141592654;
  if dword(1) + b = dword(1) + a then good else bad end;

  //////////////////////////////////////////////////////////
	//////////////////  Operación Mayor /////////////////////
  //////////////////////////////////////////////////////////

  //<<No implementado>>

  //////////////////////////////////////////////////////////
	//////////////////  Operación Asign.-suma /////////////////////
  //////////////////////////////////////////////////////////
  //coConst
  a := 1; 
  a += dword(1);
  if a = dword(2) then good else bad end;
  a := $ff; 
  a += dword(1);
  if a = dword($100) then good else bad end;
  a := 100000; 
  a += dword(1000);
  if a = dword(101000) then good else bad end;
  a := 800000; 
  a += 800000;
  if a = 1600000 then good else bad end;

  //coVariab
  a := 1; b := 1;
  a += b;
  if a = dword(2) then good else bad end;
  a := 100000; b := 100000;
  a += b;
  if a = 200000 then good else bad end;

  //coExpres
  a := 100000; b := 100000;
  a += (b+dword(1));
  if a = 200001 then good else bad end;

  a := 5;
  a += (a+dword(1));
  if a = dword(11) then good else bad end;
  
  //////////////////////////////////////////////////////////
	//////////////////  Operación Suma /////////////////////
  //////////////////////////////////////////////////////////

	//coConst_Const
  if dword($FFFF) + dword(1) = $10000 then good else bad end;
  if dword(1) + dword(1000) = 1001 then good else bad end;

  //coConst_Variab 
  a := $FF; 
  if dword(1) + a = dword($100)   then good else bad end; 
  
  //coConst_Expres
  a := $FF; 
  if dword(1) + (a+dword(1)) = dword($101)   then good else bad end; 
  
  //coVariab_Const
  a := 0; 
  if a + dword($FFFFFF) = dword($FFFFFF)   then good else bad end; 
  
  //coVariab_Variab
  a := 1; b := 1;
  if a+b = dword(2) then good else bad end;
  a := $FFFFFF; b:= 1;
  if b + a = $1000000 then good else bad end;
  a := 123456; b:= 123456789;
  if b + a = 123580245 then good else bad end;
  
  //coExpres_Const
  //coConst_Expres
  a := $FF; 
  if (a+dword(1)) + dword(1) = dword($101)   then good else bad end; 
end.
