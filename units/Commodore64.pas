{Commodore64 unit    
Set P65PAS compiler to work in the Commodore 64 mode.
Include some routines from the Kernal. 
Based on information of: http://sta.c64.org/cbm64krnfunc.html
}
{$ORG $0801}
{$COMMODORE64}
unit Commodore64;
interface
  //////////// KERNAL FUNCTIONS //////////
  
  //Initialize VIC; restore default input/output to keyboard/screen; clear screen; set PAL/NTSC switch and interrupt timer.
  procedure SCINIT;
  //Initialize CIA's, SID volume; setup memory configuration; set and start interrupt timer.
  procedure IOINIT;
  //Read byte from default input (for keyboard, read a line from the screen). (If not keyboard, must call OPEN and CHKIN beforehands.)
  procedure CHRIN: char;
  //Write byte to default output. (If not screen, must call OPEN and CHKOUT beforehands.)
  procedure CHROUT(register c: char);
  
  //////////// BASIC FUNCTIONS //////////
  //Output a word Number in ASCII Decimal Digits
  procedure LINPRT(n:word);
  
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
  
  procedure CHRIN: char;
  begin
    asm
    JSR $FFCF  ;return char in A register
    end
  end; 
  
  procedure CHROUT(register c: char);
  begin
    asm 
    JSR $FFD2  ;argument already in A register
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
end.
