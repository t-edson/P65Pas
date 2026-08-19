{Programa para la verifiación del manejo de bancos RAM.}
{$OUTPUTHEX 'output.hex'}
uses PIC16F877A, UnitTest;  
{$FREQUENCY 8Mhz}
var
  a0: byte;               //At bank 0
  b0: byte;               //At bank 0
  a1: byte absolute $A0;  //At bank 1
  b1: byte absolute $A1;  //At bank 1

begin
  SetAsOutput(pinLed);  //Inicia pin
  pinLed := 0;          //Inicia estado
 
  a0 := 1;  //Bank 0
  b0 := 3;  //Bank 0 . Must not generate bank Switch
  if a0 = 1 then good else bad end;
  if b0 = 3 then good else bad end;
  
  a0 := 1;  //Bank 0
  a1 := 3;  //Bank 1
  if a0 = 1 then good else bad end;
  if a1 = 3 then good else bad end;

  a1 := 3;  //Bank 1
  a0 := 1;  //Bank 0
  if a0 = 1 then good else bad end;
  if a1 = 3 then good else bad end;

  a1 := 1;  //Bank 1
  b1 := 2;  //Bank 1. Must not generate bank Switch
  if a1 = 1 then good else bad end;
  if b1 = 2 then good else bad end;
  
  //Expresion with several banks
  a0 := 1;  //Bank 0
  a1 := a0 + 1;  //Bank 1 and 0
  if a1 = 2 then good else bad end;

  a0 := 1;  //Bank 0
  a1 := 2;  //Bank 1
  a1 := a0 + 1 + a1;  //Bank 1 and 0
  if a1 = 4 then good else bad end;
  
end.
