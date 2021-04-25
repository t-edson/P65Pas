{Circular recursion}
procedure proc1; forward;

procedure proc1;
begin
  proc1;
end; 

begin
  proc1;
end. 
