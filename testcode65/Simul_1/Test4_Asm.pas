{Rutina de verificación de las rutinas en ensamblador.}
{$FREQUENCY 8Mhz}
{$OUTPUTHEX 'output.hex'}
{$Mode Pascal}
uses PIC16F84A;
var
  pinLed: bit absolute PORTB.0;
  vbyte: byte;
  vword: word;
	vbool: boolean;
  vbit: bit;
var
  abyte: byte absolute $20;
const
	CBYTE = 3;

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

  //Función para prueba de lectura y devolución de valores
  procedure SetTo8040: word;  //Devuelve $8040
  begin
    asm
      ;El resultado de un word, se devuelve en _H (parte alta) y W (parte baja)
      MOVLW $80
      MOVWF _H
      MOVLW $40
    end
  end; 

  procedure Multiplicar (multiplicando, multiplicador : byte) : word;
  var
    resultado: word;
  begin
   ASM
   ;Inicializacion de Registros
     BCF STATUS_RP0            ; RP0=0 / Trabajamos en el Banco de memoria 0.
     CLRF resultado.LOW        ; Limpia el byte bajo de la variable global resultado.
     CLRF resultado.HIGH       ; Limpia el byte alto de la variable global resultado.
	   CLRF _H
   ;Comprueba multiplicacion por cero.
     MOVLW $00
     SUBWF multiplicador,W
     BTFSC STATUS_Z
     GOTO MULT_FIN             ; Si multiplicador = 0 entonces acabar.
   ;LOOP de multiplicacion
   MULT_LOOP:
     MOVF multiplicando,W      ; Carga el multiplicador en el registro W.
     ADDWF resultado.LOW,F     ; Suma el valor de multiplicando al byte bajo de la variable global resultado
     BTFSC STATUS_C        ; Comprueba el bit CARRY del registro STATUS.
     INCF resultado.HIGH,F     ; Si CARRY es 0 resultado.LOW se ha desbordado se incrementa resultado.HIGH
     DECFSZ multiplicador,F    ; Decrementa multiplicador y comprueba si ha llegado a cero.
     GOTO MULT_LOOP            ; nuevo paso del bucle de multiplicacion.
   MULT_FIN:
   END
   exit(resultado);
  end;

begin
  SetAsOutput(pinLed);
  pinLed := 0;

asm
  ;Opcode test
  ADDWF vbyte, w
  ANDWF vbyte, w
  CLRF vbyte
  CLRW 
  COMF vbyte, f
  DECF vbyte, f
  INCF vbyte, w

  ;test jumps
  DECFSZ vbyte, w
	NOP
  INCFSZ vbyte, w
	NOP
 
  IORWF vbyte, w
  MOVF vbyte, w
  MOVWF vbyte
  RLF vbyte, w
  RRF vbyte, w
  SUBWF vbyte, w
  SWAPF vbyte, w
  XORWF vbyte, w
  BCF vbyte, 0
  BSF vbyte, 0
  BTFSC vbyte, 0
	NOP
  BTFSS vbyte, 0
  NOP
  ADDLW 0
  ANDLW 0
  ;CALL 
  CLRWDT 
  GOTO $+1
  IORLW 0
  MOVLW 0
  ;RETFIE
  ;RETLW 
	;RETURN 
  ;SLEEP 
	SUBLW 0
	XORLW 0
end
  //variable asigment
	abyte := 66;
	asm
    MOVLW 65
    MOVWF $20  ;absolute address
  end
  if abyte = 65 then bien else mal; 

  vbyte := 0;
  asm
    MOVLW 5
	  MOVWF vbyte
	end
  if vbyte = 5 then bien else mal; 

  vbool := false;
  asm BSF vbool end
  if vbool then bien else mal; 

  vbit := 1;
  asm BCF vbit end
  if vbit=0 then bien else mal; 

  vbit := 1;
  asm 
    BTFSC vbit
    BCF vbit 
  end
  if vbit=0 then bien else mal; 
	
	//constant access
  vbyte := 0;
  asm 
    MOVLW CBYTE 
    MOVWF vbyte
  end
  if vbyte = CBYTE then bien else mal; 

  vbyte := 0;
  asm 
    BSF vbyte, CBYTE
  end
  if vbyte = 8 then bien else mal; 

	//jumps
  asm 
    GOTO $+2
    SLEEP  ;stop if not jump
  end

	vbyte := 10;
  asm 
    DECFSZ vbyte, f
    GOTO $-1
  end
  if vbit=0 then bien else mal; 

	vbyte := 10;
  asm 
  label1:
    DECFSZ vbyte, f
    GOTO label1
  end
  if vbit=0 then bien else mal; 
  asm org $ end
  vbit := 1;

	vword := SetTo8040;
	if vword = $8040 then bien else mal;

	vword := multiplicar(5,10);
	if vword = word(50) then bien else mal;
end.
