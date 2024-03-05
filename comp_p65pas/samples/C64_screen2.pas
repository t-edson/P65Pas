////////////////////////////////////////////
// New program created in 12/09/2022}
// Contribution of https://github.com/odflor
////////////////////////////////////////////
program NewProgram;
uses Commodore64;
var
  i,c,k : word;
  r : byte;
  color: [1000]byte absolute $D800;

procedure fill1;
begin
  for i := 0 to 999 do
    r := RANDOM;
    screen[i] := 250;
    color[i] := r;
  end;
end;

procedure again;
begin
  c := c + 256;
  color[c] := r;
end;

procedure fill2;
begin
  for i := 1 to 20000 do
  c := RANDOM;
  r := RANDOM;
  color[c] := r;
  
    again;
    again;
    again;
  end;
end;

procedure fill3;
begin
  r := 0;
  
  for i := 1 to 2000 do
    c := RANDOM;
    color[c] := r;
  
    again;
    again;
    again;
  end;
end;

procedure fill4;
begin
for i := 0 to 999 do
color[i] := 0;

  for c := 1 to word(50) do
    // NOP
  end;

end;
end;

begin
  k := 0;
  screenBorder := 0;
  screenBack := 0;
  CLRSCR;
  
  while k = 0 do
    fill1;
    fill2;
    // fill3;
    fill4;
  end;
  
  asm RTS end
end.

