{Rutina de verificación de las rutinas en ensamblador.
La primera parte de la prueba es que el compilador y el 
simulador de Commodore 64 no colapsen.
}
{$Mode Pascal}
uses Commodore64;
var
  vbyte: byte;
  vword: word;
	vbool: boolean;
var
  abyte: byte;
  regA: byte register;
const
	CBYTE = 3;
  CWORD = $1234;

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
  //Aditional RTS to exit proc. and program.
  asm RTS end   
end;

  //Función para prueba de lectura y devolución de valores
  procedure SetTo8040: word;  //Devuelve $8040
  begin
    asm
      ;El resultado de un word, se devuelve en _H (parte alta) y A (parte baja)
      LDA# $80
      STA __H
      LDA# $40
      rts
    end
  end; 

  procedure Multiplicar(fac1, fac2 : byte) : word;
  var
    resultado: word;
  begin
   ASM
   ;Inicializacion de Registros
        ; A*256 + X = FAC1 * FAC2
MUL8:
        lda #$00
        ldx #$08
        clc
m0:     bcc m1
        clc
        adc FAC2
m1:     ror
        ror FAC1
        dex
        bpl m0
        ldx FAC1
        ;Devuelve en "resultado"
        STA resultado+1
        STX resultado 
   END;
   exit(resultado);
  end;
procedure TestOpcodes;
begin
asm
  ;Opcode test
  ADC #0
  AND abyte
  ASL abyte, x
  BCC endlabel
  BCS endlabel
  BEQ endlabel
  BIT abyte
  BMI endlabel
  BNE endlabel
  BPL endlabel
  BRK 
  BVC endlabel
  BVS endlabel
  CLC
  CLD
  CLI
  CLV
  CMP 255
  CPX $10
  CPY #123
  DEC abyte
  DEX
  DEY
  EOR abyte
  INC abyte
  INX 
  INY
  JMP endlabel
  JSR endlabel
  LDA 1
  LDX 2
  LDY 3
  LSR abyte
  NOP
  ORA abyte
  PHA
  PHP
  PLA
  PLP
  ROL abyte
  ROR abyte
  RTI
  RTS
  SBC abyte
  SEC 
  SED
  SEI
  STA abyte 
  STX abyte
  STY abyte
  TAX
  TAY
  TSX
  TXA
  TXS
  TYA
endlabel:
end;
end; 
  
begin
  //Variable asigment
	abyte := 66;
	asm
    LDA# 65
    STA abyte
  end;
  if abyte = 65 then bien else mal; 

  vbyte := 0;
  asm
    LDX #5
	  STX vbyte
	end;
  if vbyte = 5 then bien else mal; 

  vbool := false;
  asm 
  LDA #$FF
  STA vbool
  end;
  if vbool then bien else mal; 

	//Constant access
  vbyte := 0;
  asm 
    LDA # CBYTE 
    STA vbyte
  end;
  if vbyte = CBYTE then bien else mal; 

  vbyte := 0;
  asm 
    LDA #$08
    ORA VBYTE
    STA VBYTE
  end;
  if vbyte = 8 then bien else mal; 

  //Indirect address mode 
  asm 
	   CLC
     LDX #3     ;Offset to point
     LDA $, X   ;Indirect X address mode
     CLC   ;Any opcode to read wuth the previous LDA
  end; 
  if regA = $18 then bien else mal; //$18 ->CLC

	//Jumps
  asm 
    JMP $+4
    BRK ;stop if not jump
  end;

  //Test Loop with offset
	vbyte := 10;
  asm 
    DEC vbyte
    BEQ $+2+3  ;2 bytes of BEQ and 3 of JMP
    JMP $-4    ;Loop to DEC vbyte
  end;
	if vbyte = $00 then bien else mal;

  //Test Loop with labels
	vbyte := 10;
  asm 
  label1:
    DEC vbyte
    BEQ salir
    JMP label1
salir:
  end;

  //Setting PC
  asm org $ end;

  //Operators
  asm 
	  LDA #CBYTE + 2
    STA vbyte
  end; 
	if vbyte = 5 then bien else mal;
 
  asm 
	  LDA #<CWORD
    STA vbyte
  end; 
	if vbyte = $34 then bien else mal;
  asm 
	  LDA #>CWORD
    STA vbyte
  end; 
	if vbyte = $12 then bien else mal;

  asm 
	  LDA #<CWORD+1
    STA vbyte
  end; 
	if vbyte = $35 then bien else mal;
  asm 
	  LDA #>CWORD-1
    STA vbyte
  end; 
	if vbyte = $11 then bien else mal;
  
  asm 
	  LDA #CWORD@0
    STA vbyte
  end; 
	if vbyte = $34 then bien else mal;
  asm 
	  LDA #CWORD@1
    STA vbyte
  end; 
	if vbyte = $12 then bien else mal;

  asm 
	  LDA #CWORD.LOW
    STA vbyte
  end; 
	if vbyte = $34 then bien else mal;
  asm 
	  LDA #CWORD.HIGH
    STA vbyte
  end; 
	if vbyte = $12 then bien else mal;
  
  //Test return value
	vword := SetTo8040;
	if vword = $8040 then bien else mal;

  //Call to subroutine
	vword := multiplicar(5,10);
	if vword = word(50) then bien else mal;
  
  asm RTS end; 
end.
