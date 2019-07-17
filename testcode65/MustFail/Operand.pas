{Cases of expressions not allowed.}
program nombre;
var
  x: byte;
begin
  //All Must fail
  1+x;
  x+1;
  x + 1 := 2;
  123;
end.
