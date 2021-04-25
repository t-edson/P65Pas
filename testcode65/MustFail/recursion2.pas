{Circular recursion}
procedure proc2; forward;

procedure proc1;
begin
  proc2;
end; 

procedure proc2;
begin
  proc1;
end;

begin
  proc1;
end. 
