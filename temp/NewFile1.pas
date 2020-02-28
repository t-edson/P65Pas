var
  IRQ: ^char absolute $314;

procedure IRQ_Handler: byte;
begin
end;

begin
  IRQ := @IRQ_Handler;
end.
