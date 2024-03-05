{Commodore128 unit    
Set P65PAS compiler to work in a Commodore 128 system.
Include some routines from the Kernal. 
}
{$ORG $1C01}
{$BOOTLOADER $0C,$08,$0A,$00,$9E,'COD_4A',$00,$00,$00}
{$STRING NULL_TERMINATED}
//Set RAM for Commodore 64
//{$CLEAR_STATE_RAM} If we clears, we'll need to define all RAM map
{$SET_DATA_ADDR ''}
{$SET_DATA_ADDR '00FB-00FE'}  //Some bytes from Zero page
{$SET_STATE_RAM '0100-01FF:SFR'} //Stack
unit Commodore128;
interface
type 
  pointer = word;
var
  screenBorder: byte absolute 53280;  
  screenBack  : byte absolute 53281;  
  screen: [1000]byte absolute $400;
  //////////// KERNAL FUNCTIONS //////////
 
  //Initialize VIC; restore default input/output to keyboard/screen; clear screen; set PAL/NTSC switch and interrupt timer.
  procedure CINT;
  //Initialize CIA's, SID volume; setup memory configuration; set and start interrupt timer.
  procedure IOINIT;
  //Read byte from default input (for keyboard, read a line from the screen). (If not keyboard, must call OPEN and CHKIN beforehands.)
  procedure CHRIN: char;
  //Write byte to default output. (If not screen, must call OPEN and CHKOUT beforehands.)
  procedure CHROUT(c: char register);
  //Read byte from default input. 
  procedure GETIN: byte;
  
implementation
  procedure CINT;
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
  
end.

