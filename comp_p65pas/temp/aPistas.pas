program Hello;
uses Commodore64, C64Sprites;
const
  SPRITE1 = [$00,$00,$00,$00,$fe,$00,$03,$ff,$80,$07,$ff,$e0,$0f,$ff,$f0,$1e
$fe,$f0,$3c,$7c,$78,$3e,$fe,$f8,$3f,$ff,$f8,$3f,$ff,$f8,$1f,$01
$f0,$1f,$01,$f0,$0f,$ff,$e0,$07,$ff,$c0,$03,$ff,$80,$03,$ff,$00
$03,$03,$00,$0f,$03,$c0,$0f,$03,$c0,$00,$00,$00,$00,$00,$00,$0d];
var 
  sprPucp: [64]byte absolute $2000;  //Zona para almacenar el Sprite1
  regA: byte registerA;
  key: byte;
  i, row: byte;
  w: word;
begin
  sprPucp := SPRITE1;  //Copy date from screen
  spritePointers[0] := (@sprPucp) >> 6;  //Point to sprite.
  spritePosition[0].x := 120;
  spritePosition[0].y := 200;
  spriteColor[0]:=8; 
  spEnable(1);
  screenBorder := 6;
  screenBack := 0;
  //Prepare screen
  CLRSCR;
  for row:=0 to 24 do
    for i:=30 to 39 do
      PutChar(i, row, #160);
    end;
  end;
  for row:=0 to 24 do
    PutChar(RANDOM and 31, RANDOM and 15, #91);
  end;
  
  while true do
    //spritePosition[0].x += 1;
    delay_ms(word(10));
    key := GETIN;
    if key<>0 then 
      if key = 157 then  //Flecha izquierda
        if spritePosition[0].x > 15 then 
        spritePosition[0].x -= 5;
        end;
      end; 
      if key = 29 then  //Flecha izquierda
        if spritePosition[0].x < 250 then 
          spritePosition[0].x += 5;
        end;
      end;
      if key = 29 then  //Flecha arriba
      
      end;
//      LINPRT(word(key));
    end;  
  end; 
  asm RTS end 
end.
