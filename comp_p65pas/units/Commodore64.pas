{Commodore64 unit    
Set P65PAS compiler to work in the Commodore 64 mode.
Include some routines from the Kernal. 
Based on information of: http://sta.c64.org/cbm64krnfunc.html
}
{$ORG $0801}
{$BOOTLOADER C64}
//Set RAM for Commodore 64
//{$CLEAR_STATE_RAM} If we clears, we'll need to define all RAM map
{$SET_DATA_ADDR ''}
{$SET_DATA_ADDR '00F7-00FE'}  //Some bytes from Zero page
{$SET_STATE_RAM '0100-01FF:SFR'} //Stack
unit Commodore64;
interface
type 
  pointer = word;
var
  screenBorder: byte absolute 53280;  
  screenBack  : byte absolute 53281;  
  screen: [1000]byte absolute $400;
  //////////// KERNAL FUNCTIONS //////////
 
  //Initialize VIC; restore default input/output to keyboard/screen; clear screen; set PAL/NTSC switch and interrupt timer.
  procedure SCINIT;
  //Initialize CIA's, SID volume; setup memory configuration; set and start interrupt timer.
  procedure IOINIT;
  //Clear Screen
  procedure CLRSCR(col: byte);
  //Clear Screen
  procedure CLRSCR;
  //Read byte from default input (for keyboard, read a line from the screen). (If not keyboard, must call OPEN and CHKIN beforehands.)
  procedure CHRIN: char;
  //Write byte to default output. (If not screen, must call OPEN and CHKOUT beforehands.)
  procedure CHROUT(c: char register);
  //Read byte from default input. 
  procedure GETIN: byte;
  
  //////////// BASIC FUNCTIONS //////////
  //Output a word Number in ASCII Decimal Digits
  procedure LINPRT(n:word);
  procedure LINPRT(n:byte);
  procedure STROUT(str: pointer);
  procedure RANDOM: byte;

  /////////// DIRECT ACCESS TO SCREEN ////////
  procedure PutChar(x: byte; y: byte registerY; c: char): word;

implementation
  procedure SCINIT;
  begin
    asm
    JSR $FF81
    end
  end; 
  
  procedure IOINIT;
  begin
    asm 
    JSR $FF84  
    end 
  end; 
  procedure CLRSCR(col: byte);
  begin
    asm
    LDA col
    STA $0286
    JSR $E544
    end
  end;
  procedure CLRSCR;
  begin
    asm
    JSR $E544
    end
  end;
  procedure CHRIN: char;
  begin
    asm
    JSR $FFCF  ;return char in A register
    end
  end; 
  
  procedure CHROUT(c: char register);
  begin
    asm 
    JSR $FFD2  ;argument already in A register
    end 
  end; 
  procedure GETIN: byte;
  {Read byte from default input. (If not keyboard, must call OPEN and CHKIN beforehands.)
  Input: <none>
  Output: A = Byte read.}
  begin
    asm 
	  JSR $FFE4 
    end 
  end; 
  
  procedure LINPRT(n:word);
  {Print the word number specified. 
  This routine, first convert the number to Floating Point Notation,
  so it's some slow.}
  begin
    asm
    LDA n.high
    LDX n.low
    JSR $BDCD 	 
    end 
  end; 
  procedure LINPRT(n:byte);
  {Print the byte number specified. 
  This routine, first convert the number to Floating Point Notation,
  so it's some slow.}
  begin
    asm
    LDA #0
    LDX n
    JSR $BDCD 	 
    end 
  end; 
  procedure STROUT(str: pointer);
  {Prints a string delimited by NULL}
  begin
    asm 
	  LDA str
    LDY str+1
    JSR $AB1E 
    end 
  end; 
  procedure RANDOM: byte;
  {Returns a pseudo-random byte.}
  begin
    asm 
	    LDA #0  ;Use internal clcck
      JSR $E09A
      LDA $64 
    end 
  end; 

  procedure PutChar(x: byte; y: byte registerY; c: char): word;
  {Write a char directly to the screen of the Commodore 64 in the specified 
  coordinates.}
  begin
    asm 
       ;--- Load Y in (H,A)
	     LDA #0
       STA __H
       TYA
       ;--- Shift (H,A) 3 times 
       ASL
       ROL __H
       ASL
       ROL __H
       ASL
       ROL __H
       ;--- Save in IX (IX <- Y * 8) 
       STA __IX.low
       LDY  __H
       STY __IX.high
       ;--- Shift (H,A) 2 times: (H,A) <- y*32 
       ASL
       ROL __H
       ASL
       ROL __H
       ;--- Add (H,A) to IX: IX <- IX + y*32 + 400
       CLC
       ADC __IX.low  ;LSB
       STA __IX.low
       LDA __H      ;MSB
       ADC __IX.high
       CLC
       ADC #$04
       STA __IX.high
       ;--- Here we have IX <- Y*40 + $400. 
       LDY x
       LDA c
       CLC  ;Prepara Index Y address mode
       STA (__IX), Y  ; Write in IX + X
    end
  end; 
  
end.
