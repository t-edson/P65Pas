{Commodore64 unit    
Set P65PAS compiler to work in the Commodore
}
{$ORG $0801}
{$COMMODORE64}
unit Commodore64;
interface
  procedure CHRIN: char;
  procedure CHROUT(register c: char);
  
implementation
  procedure CHRIN: char;
  begin
    asm
    JSR $FFCF  ;argument already in A register
    end
  end; 
  procedure CHROUT(register c: char);
  begin
    asm 
    JSR $FFD2  ;argument already in A register
    end 
  end; 
  
end.
