program Hello;
uses Commodore64, C64Sprites;
const
  SPRITEPUCP = [$00,$7e,$00,$03,$ff,$c0,$0f,$ff,$f0,$1f,$ff,$fc,$3f,$ff,$fc,$7f
$ff,$fe,$ff,$ff,$ff,$82,$d0,$83,$da,$d6,$db,$d2,$d7,$d3,$c6,$d7
$c7,$de,$d6,$df,$8e,$30,$8f,$ff,$ff,$ff,$ff,$ff,$ff,$7f,$ff,$fe
$3f,$ff,$fe,$3f,$ff,$fc,$0f,$ff,$f0,$03,$ff,$c0,$00,$ff,$00,$01];
var 
  sprPucp: [64]byte absolute $2000;  //Zona para almacenar el Sprite1
  regA: byte registerA;
  key: byte;
  row, i, j: byte;
  aleat: byte;
  dx0, dy0: byte;
  dx1, dy1: byte;
  dx2, dy2: byte;
  dx3, dy3: byte;
begin
  //CLRSCR;
  sprPucp := SPRITEPUCP;  //Copia datos del sprite
  spritePointers[0] := (@sprPucp) >> 6;  //apunta Sprite 0 a sprPucp.
  spritePosition[0].x := 50;
  spritePosition[0].y := 50;
  spriteColor[0]:=2; 
//  spEnable(1);

  spritePointers[1] := (@sprPucp) >> 6;  //apunta Sprite 0 a sprPucp.
  spritePosition[1].x := 70;
  spritePosition[1].y := 70;
  spriteColor[1]:=3; 
  
  spritePointers[2] := (@sprPucp) >> 6;  //apunta Sprite 0 a sprPucp.
  spritePosition[2].x := 100;
  spritePosition[2].y := 100;
  spriteColor[2]:=4; 

  spritePointers[3] := (@sprPucp) >> 6;  //apunta Sprite 0 a sprPucp.
  spritePosition[3].x := 130;
  spritePosition[3].y := 130;
  spriteColor[3]:=5; 
  
//  spriteWidth := 1;
//  spriteHeight:= 1;
  spritesEnable := $0F;
  
  screenBorder := 6;
  screenBack := 0;
  CLRSCR;
  STROUT(@'    ***ANIMANDO SPRITES***'#0);
  dx0 := RANDOM and %10000011;
  dy0 := RANDOM and %10000011;
  dx1 := RANDOM and %10000011;
  dy1 := RANDOM and %10000011;
  dx2 := RANDOM and %10000011;
  dy2 := RANDOM and %10000011;
  dx3 := RANDOM and %10000011;
  dy3 := RANDOM and %10000011;
  //if RANDOM>50 then dx1:=1 else dx1 := $FF end;
  while true do
    //spritePosition[0].x += 1;
    delay_ms(word(30));
    //Lee número aleatorio
    spritePosition[0].x += dx0;
    spritePosition[0].y += dy0;
    spritePosition[1].x += dx1;
    spritePosition[1].y += dy1;
    spritePosition[2].x += dx2;
    spritePosition[2].y += dy2;
    spritePosition[3].x += dx3;
    spritePosition[3].y += dy3;
  end; 
  asm RTS end 
end.
