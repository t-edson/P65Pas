program NewProgram;
//Declarations here
procedure DelayMs(n: word);
begin
  asm CLC end 
end; 
var 
  c: char;
begin
  DelayMs(2000);
  if c='A' then
    inc(c);
  else 
    c := #0;  
  end;  
end.
