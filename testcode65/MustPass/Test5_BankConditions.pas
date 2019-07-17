{Programa para la verifiación del manejo de bancos RAM, dentro de condicionales.}

{$PROCESSOR PIC16C63}
{$FREQUENCY 8Mhz}
uses PIC16C63;  //Se requiere un MCU con más de un banco 
var
  a0: byte;               //at bank0
  a1: byte absolute $A0;  //at bank 1
begin
  // NOTE: PicPas must be configured with optimization activated
  // Validation must be done, verifying the messages.
  // Correct execution of this code no necessary means the code generation is OK.

  ///////////////////////////////////////////////////
  ///// Conditional defined in Compilation Time
  ///////////////////////////////////////////////////
  {$SET NERRORS = 0}
  //Simple IF - Case TRUE.
  a0 := 0;  //Bank 0
  if true then 
    a1 := 2;  //Bank 1. Must include Bank selection 
  end;
  {$IF CURRBANK <> 1} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}

  //Simple IF - Case FALSE
  a0 := 0;  //Bank 0
  if false then 
    a1 := 2;  //Bank 1. Must not generate code.
  end;
  {$IF CURRBANK <> 0} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}

  //Complete IF - Case TRUE
  a0 := 0;  //come from bank 0
  if true then 
    a0 := 0;  //Bank 0
  else
    a1 := 2;  //Bank 1
  end;
  {$IF CURRBANK <> 0} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}

  //Complete IF - Case FALSE 
  a0 := 0;  //come from bank 0
  if false then 
    a1 := 1;  //Bank 1 
  else
    {$IF CURRBANK <> 0} 
    {$ERROR 'Error in Bank assigment'}
    {$ENDIF}
    a1 := 2;  //Bank 1
  end;
  
  //Multiple IF - Case TRUE
  a0 := 0;  //come from bank 0
  if true then 
    a0 := 1;  //Bank 0
  elsif true then
    a1 := 2;  //Bank 1
  elsif true then
    a1 := 2;  //Bank 1
  else
    a1 := 2;  //Bank 1
  end;
  {$IF CURRBANK <> 0} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}

  //Multiple IF - Case FALSE
  a0 := 0;  //come from bank 0
  if false then 
    a0 := 1;  //Bank 0
  elsif true then
    a1 := 222;  //Bank 1
  elsif true then
    a0 := 3;  //Bank 0
  else
    a0 := 4;  //Bank 0
  end;
  {$IF CURRBANK <> 1} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}
  
  ///////////////////////////////////////////////////
  ///// Conditional not defined in Compilation Time
  ///////////////////////////////////////////////////
 
  //Simple IF - Final bank defined.
  if PORTB = 1 then  //Condition in bank 0
    PORTB := 5;   //Block in bank 0
  end; 
  //Here the CurrBank  must be 0
  {$IF CURRBANK <> 0} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}

  //Simple IF - Final bank undefined.
  if PORTB = 2 then  //Condition in bank 0
    TRISB := 5;   //Block in bank 1
  end; 
  //Here the CurrBank  must be 255 (undefined)
  {$IF CURRBANK <> 255} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}
  
  //Complete IF - Final bank defined.
  if PORTB = 1 then  
    PORTB := 3;   //Block in bank 0
  else
    PORTB := 4;   //Block in bank 0
  end; 
  {$IF CURRBANK <> 0} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}

  if PORTB = 1 then  
    TRISB := 3;   //Block in bank 1
  else
    TRISB := 4;   //Block in bank 1
  end; 
  {$IF CURRBANK <> 1} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}

  //Complete IF - Final bank undefined.
  if PORTB = 1 then  
    PORTB := 3;   //Block in bank 0
  else
    TRISB := 4;   //Block in bank 1
  end; 
  {$IF CURRBANK <> 255} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}

  //Multiple IF - Final bank defined.
  if PORTB = 1 then  
    PORTB := 3;   //Block in bank 0
  elsif PORTB = 2 then 
    PORTB := 4;   //Block in bank 0
  else
    PORTB := 5;   //Block in bank 0
  end; 
  {$IF CURRBANK <> 0} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}

  //Multiple IF - Final bank defined.
  if PORTB = 1 then  
    TRISB := 3;   //Block in bank 0
  elsif TRISB = 2 then 
    TRISB := 4;   //Block in bank 0
  else
    TRISB := 5;   //Block in bank 0
  end; 
  {$IF CURRBANK <> 1} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}
  
  //Multiple IF - Final bank undefined.
  if PORTB = 1 then  
    TRISB := 4;   //Block in bank 1
  elsif PORTB = 2 then 
    PORTB := 3;   //Block in bank 0
  else
    PORTB := 5;   //Block in bank 0
  end; 
  {$IF CURRBANK <> 255} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}

  //Multiple IF - Final bank undefined.
  if PORTB = 1 then  
    PORTB := 3;   //Block in bank 0
  elsif PORTB = 2 then 
    TRISB := 4;   //Block in bank 1
  else
    PORTB := 5;   //Block in bank 0
  end; 
  {$IF CURRBANK <> 255} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}

  //Multiple IF - Final bank undefined.
  if PORTB = 1 then  
    PORTB := 3;   //Block in bank 0
  elsif PORTB = 2 then 
    PORTB := 5;   //Block in bank 0
  else
    TRISB := 4;   //Block in bank 1
  end; 
  {$IF CURRBANK <> 255} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}

  ///////////////////////////////////////////////////
  ///// WHILE defined in Compilation Time
  ///////////////////////////////////////////////////

  //WHILE false
  a0 := 0;  //Bank 0
  while false do 
    a1 := 1;  //Bank 1. Must not generate code.
  end;
  {$IF CURRBANK <> 0} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}

  //WHILE true
  a0 := 0;  //Bank 0
  while true do 
    a1 := 1;  //Bank 1. Must not generate code.
  end;
  //Verify if _BANSEL(0) is included at the endof the WHILE block
  {$IF CURRBANK <> 0} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}

  //Normal WHILE 
  a0 := 0;  //Bank 0
  while PORTB = 0 do //condition in bank 0
    a1 := 1;  //Block in bank 1. 
  end;
  //Verify if CurrBank = 0 at the end of the WHILE (because condition end in bank 0)
  {$IF CURRBANK <> 0} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}

  //Normal WHILE 
  a0 := 0;  //Bank 0
  while TRISB = 0 do //condition in bank 1
    a0 := 1;  //Block in bank 0. 
  end;
  //Verify if CurrBank = 1 at the end of the WHILE (because condition end in bank 1)
  {$IF CURRBANK <> 1} 
  {$ERROR 'Error in Bank assigment'}
  {$ENDIF}
  
end.
