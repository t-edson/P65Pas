{Commodore64 unit    
Set P65PAS compiler to work in the Commodore 64 mode.
Include some routines from the Kernal. 
Based on information of: http://sta.c64.org/cbm64krnfunc.html
}
{$ORG $0801}
{$COMMODORE64}
//Define some bytes from Zero page
{$SET_DATA_ADDR ''}
{$SET_DATA_ADDR '00F7-00FE'} 
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
  procedure GETIN;
  
  //////////// BASIC FUNCTIONS //////////
  //Output a word Number in ASCII Decimal Digits
  procedure LINPRT(n:word);
  procedure STROUT(str: pointer);
  procedure RANDOM: byte;
  
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
  procedure GETIN;
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
  procedure STROUT(str: pointer);
  {Prints a string delimited by NULL}
  begin
    asm 
	  LDA str.low
    LDY str.high
    JSR $AB1E 
    end 
  end; 
  procedure RANDOM: byte;
  {Returns a pseudo random byte.}
  begin
    asm 
	    LDA #0  ;Use internal clcck
      JSR $E09A
      LDA $64 
    end 
  end; 

end.
