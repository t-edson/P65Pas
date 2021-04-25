{Circular recursion 3 steps}
procedure proc3; forward;

procedure proc1;
begin
  proc3;
end; 

procedure proc2;
begin
  proc1;
end; 

procedure proc3;
begin
  proc2;
end;

begin
  proc1;
end. 
