const 
//  C = 123;
  D = [1,2,3];
var
//  a: byte = 1;
//  b: []byte = [128, 64, 32, 16, 8, 4, 2, 1];
  bmp: array[1024] of byte absolute $800;
begin
//  a := D.length;  
//  bmp[a] := 5;
  //D[0] := 1;
  asm 
	Lda #>bmp
   
  end; 
end.

