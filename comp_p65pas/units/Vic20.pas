{Vic20 unit    
Set P65PAS compiler to work in the Commodore VIC-20.
Include some routines from the Kernal. 
}
{$ORG $1001}
{$BOOTLOADER $0b,$10,$0a,$00,$9e,'COD_5A',$00,$00,$00}
{$STRING NULL_TERMINATED}

//Set RAM for Commodore 64
//{$CLEAR_STATE_RAM} If we clears, we'll need to define all RAM map
{$SET_DATA_ADDR ''}
{$SET_DATA_ADDR '00F7-00FE'}  //Some bytes from Zero page
{$SET_STATE_RAM '0100-01FF:SFR'} //Stack
unit Vic20;
interface
type 
  pointer = word;
var
  //////////// SCREEN //////////
  {screeColor:
    Bit 0~2 -> Border color.
    Bit 3   -> Reverse mode: 0: on, 1: off
    Bit 4~7 -> Screen color.
  }
  screenColor: byte absolute $900F;  
  screen: [1000]byte absolute $1E00;

  //////////// SOUND  //////////
  oscilator1: byte absolute $900A;  //Bit 0~6->Frequency,  Bit 7->Activate.
  oscilator2: byte absolute $900B;  //Bit 0~6->Frequency,  Bit 7->Activate.
  oscilator3: byte absolute $900C;  //Bit 0~6->Frequency,  Bit 7->Activate.
  osc_noise : byte absolute $900D;  //Bit 0~6->Frequency,  Bit 7->Activate.
  volume    : byte absolute $900E;  //Only Low nibble.

  //////////// KERNAL FUNCTIONS //////////

  //Clear Screen
  procedure CLRSCR;
  //Read byte from default input (for keyboard, read a line from the screen). (If not keyboard, must call OPEN and CHKIN beforehands.)
  procedure CHRIN: char;
  //Write byte to default output. (If not screen, must call OPEN and CHKOUT beforehands.)
  procedure CHROUT(c: char register);
  //Read byte from default input. 
  procedure GETIN: byte;

  //Cursor position
  procedure PLOT_SET(x: byte registerX;  y: byte registerY);
  procedure PLOT_READ;
  
implementation
  
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

  procedure PLOT_SET(x: byte registerX; y: byte registerY);
  {Set screen cursor position at (x,y) position.}
  begin
    asm 
    CLC
	  JSR $FFF0 
    end 
  end; 

  procedure PLOT_READ;
  {Read cursor position. (If not keyboard, must call OPEN and CHKIN beforehands.)
  Input: <none>
  Output: register X = column number (0-21).
          register Y = row number (0-22).
  }
  begin
    asm 
    SEC
	  JSR $FFF0 
    end 
  end; 
  
end.

