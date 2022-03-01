begin
  asm 
    JMP code
tmp_var:
   DB $00  ;variable temporal
code:
  LDA tmp_var
  end;

end.
