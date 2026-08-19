{Prueba de las sintaxis de la condicional IF,
en el modo pro defecto de PicPas.}
//{$mode pascal}
var a: boolean; 
begin
  //condicional simple vacía
  if true then  

  end;
  if a then 

  end;
  if a and a then 

  end;
  //condicional simple 
  if true then  
	  a  := true;
	  a  := false;
  end;
  if a then 
	  a  := true;
	  a  := false;
  end;
  if a and a then 
	  a  := true;
	  a  := false;
  end;
  //condicional con ELSE, vacía
  if true then 
	else 
  end;
  if a then 
	else 
  end;
  if a and a then 
	else 
  end;
  //condicional con ELSE
  if true then 
	  a  := true;
	  a  := false;
	else 
	  a  := true;
	  a  := false;
  end;
  if a then 
	  a  := true;
	  a  := false;
	else 
	  a  := true;
	  a  := false;
  end;
  if a and a then 
	  a  := true;
	  a  := false;
	else 
	  a  := true;
	  a  := false;
  end;
  //condicional con ELSIF 
  if false then 
	  a  := true;
  elsif a then 
	  a  := false;
	else 
	  a  := true;
	  a  := false;
  end;

  if a then 
	  a  := true;
  elsif a then 
	  a  := false;
	else 
	  a  := true;
	  a  := false;
  end;

end.
