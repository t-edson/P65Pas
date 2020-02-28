program Hello;
uses Commodore64, C64Sprites;
const
  SPRITEPUCP = [$00,$7e,$00,$03,$ff,$c0,$0f,$ff,$f0,$1f,$ff,$fc,$3f,$ff,$fc,$7f
                $ff,$fe,$ff,$ff,$ff,$80,$90,$83,$da,$b6,$db,$d2,$b7,$d3,$c6,$b7
                $c7,$de,$b6,$df,$8e,$30,$8f,$ff,$ff,$ff,$ff,$ff,$ff,$7f,$ff,$fe
                $3f,$ff,$fe,$3f,$ff,$fc,$0f,$ff,$f0,$03,$ff,$c0,$00,$ff,$00,$01];
var 
  sprPucp: [64]byte absolute $2000;  //Zona para almacenar el Sprite1
  regA: byte registerA;
  key: byte;
begin
  //CLRSCR;
  sprPucp := SPRITEPUCP;  //Copia datos del sprite
  spritePointers[0] := (@sprPucp) >> 6;  //apunta Sprite 0 a sprPucp.
  spritePosition[0].x := 120;
  spritePosition[0].y := 200;
  spriteColor[0]:=8; 
  spEnable(1);
  screenBorder := 6;
  screenBack := 0;
  CLRSCR;

  while true do
    //spritePosition[0].x += 1;
    delay_ms(word(10));
    GETIN;
    key := regA;
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
