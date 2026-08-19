unit sprite1;

interface

var 
  sprite1data: [64]byte = [0,127,0,1,255,192,3,255,224,3,231,224,
	                         7,217,240,7,223,240,7,217,240,3,231,224,
	                         3,255,224,3,255,224,2,255,160,1,127,64,
	                         1,62,64,0,156,128,0,156,128,0,73,0,0,73,0,
                           0,62,0,0,62,0,0,62,0,0,28,0];
  sprite1pointer: byte absolute $07F8;
  spritesEnable: byte absolute $D015;
  sprite1xpos: byte absolute $D000;
  sprite1ypos: byte absolute $D001;
  sprite1color: byte absolute $D027;
  sprite1adres: [64]byte absolute $2000;
                             
procedure initSprite1;

implementation
    
procedure initSprite1;
var
  i,t: byte;
  pos: word;
begin
  sprite1adres := sprite1data;
 
  sprite1pointer:=$80; //128*64=$2000
  spritesEnable:=1;
  sprite1xpos:=128;
  sprite1ypos:=128;
  sprite1color:=5; //green
end;  
  
end.
