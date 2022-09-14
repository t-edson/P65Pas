uses Vic20;
begin
//  clrscr;
//  screenColor := 255;
  oscilator2 := $9F;
  oscilator3 := $AF;
  volume := $f1;
  PLOT_SET(13,0);
//  CHROUT(#13);
  CHROUT('I');
  CHROUT(' ');
  CHROUT('A');
  CHROUT('M');
  CHROUT(' ');
  CHROUT('P');
  CHROUT('6');
  CHROUT('5');
  CHROUT('P');
  CHROUT('A');
  CHROUT('S');
  asm rts end;
end.
