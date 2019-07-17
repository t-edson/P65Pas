{Prueba de las sintaxis de la condicional IF,
en el modo Pascal.}
{$mode pascal}
var a: boolean; 
begin
  //condicional simple 
  if true then a := false;
  if a then 
   a := false;
  if a and a then 
   a := false;
  //condicional simple bloque 
  if true then  begin
	  a  := true;
	  a  := false;
  end;
  if a then  begin
	  a  := true;
	  a  := false;
  end;
  if a and a then begin
	  a  := true;
	  a  := false;
  end;
  //condicional con ELSE
  if true then 
   a := false
	else 
   a := false;

  if a then begin
   a := false;
  end else begin
   a := false;
  end;
end.
