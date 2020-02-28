unit C64sprites;
interface
type 
  Tposition = object
    x,y:byte;
  end;
var 
  spritesEnable  : byte absolute $D015;  //BitMap
  spritePointers : [8]byte absolute $07F8; //Final addres = value * 64
  spritePosition : [8]Tposition absolute $D000;  //(X,Y) position
  spritePositionH: byte absolute $D010;  //Bit map
  spriteColorMode: byte absolute $D01C;  //BitMap (0->HighRes 1->Multicolor)
  spriteColor    : [8]byte absolute $D027;
  spriteWidth    : byte absolute $D01D;  //BitMap (0->Normal  1->Double Width)
  spriteHeight   : byte absolute $D017;  //BitMap (0->Normal  1->Double Height)
  spritePriority : byte absolute $D01B;  //Bitmap
  
procedure spEnable(mask: byte register);

implementation

procedure spEnable(mask: byte register);
{Enable sprite in screen.}
begin
  spritesEnable := spritesEnable or mask;
end;    

end.
