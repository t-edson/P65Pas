{Rutina de verificación del funcionamiento de los procedimientos y
funciones.
}
uses Commodore64;
var
  vbyte: byte;

  procedure bien;
  begin
    CHROUT('O');
    CHROUT('K');
    CHROUT(chr(13));
  end;
  procedure Mal;
  begin
    CHROUT('E');
    CHROUT('R');
    CHROUT('R');
    CHROUT('O');
    CHROUT('R');
    CHROUT(chr(13));
  end;
  
  //Procedimientos de prueba
  procedure proc1;
  begin
    vbyte := 5;
  end;

  procedure proc2(par1: byte);
  begin
    if par1 = 0 then 
      exit;
    else
      vbyte := 10;
    end;  
  end;

  //Funciones de prueba
  procedure func1: byte;
  begin
    exit(3);
  end;

  procedure func2(par1: byte): byte;
  begin
    exit(par1+1);
  end;

  procedure func2b(par1: word): word;
  begin
    exit(par1+2);
  end;

  //Prueba de devolución de variable
  procedure func2c(par1: word): word;
  begin
    exit(par1);
  end;

	//Función con varios parámetros word, byte
  procedure func3(par1: word; par2: byte): boolean;
  begin
    exit(true);
  end;

  procedure func4(par1: boolean): boolean;
  begin
    exit(not par1);
  end;

  //Funciones sobrecargadas
  procedure fun5(valor1: byte): byte;
  begin
    exit(valor1+1);
  end;

  procedure fun5(valor1: word): word;
  begin
    exit(valor1+2);
  end;

	//Función con parámetros REGISTER
  procedure fun7(valor1: byte register): byte;
  begin
	  exit(valor1 + 1);
  end; 

var 
  xbyte : byte;
  xword : word;
  xbool : boolean;
begin
  //Prueba de procedimiento
  vbyte := 0;
  Proc1;
  if vbyte = 5 then bien else mal end;

  vbyte := 1;
	proc2(0);
  if vbyte = 1 then bien else mal end;

	proc2(1);
  if vbyte = 10 then bien else mal end;

  //Prueba de función
  xbyte := func1;
  if xbyte = 3 then bien else mal end;

  xbyte := func1 + 1;
  if xbyte = 4 then bien else mal end;

  xbyte := 2 + func1 + func1 + 2;
  if xbyte = 10 then bien else mal end;

  xbyte :=  func2(1);
  if xbyte = 2 then bien else mal end;

  xbyte :=  func2(1) + func2(2);
  if xbyte = 5 then bien else mal end;

  xbyte :=  1+func2(1) + (func2(2)+func2(3));
  if xbyte = 10 then bien else mal end;

  xbyte := 10;
  xbyte :=  func2(xbyte);
  if xbyte = 11 then bien else mal end;

  xword :=  func2b(word(1));
  if xword = word(3) then bien else mal end;

  xword := 10;
  xword :=  func2b(xword);
  if xword = word(12) then bien else mal end;

  //Prueba de devolución de variable
  xword := 10;
  xword :=  func2c(xword);
  if xword = word(10) then bien else mal end;

	//Función con varios parámetros word, byte
  xbool := func4(false);
  if xbool then bien else mal end;

  xbool := func4(true);
  if not xbool then bien else mal end;

  xbool := func4(true) or func4(false);
  if xbool then bien else mal end;

  //Funciones sobrecargadas
  xword := fun5(5) + fun5(word(5));
  if xword = word(13) then bien else mal end;

	//Función con parámetros REGISTER
  xbyte :=  fun7(1);
  if xbyte = 2 then bien else mal end;
  
  //Función con llamada recursiva
  xbyte := func2(func2(200));
  if xbyte = 202 then bien else mal end;

  asm RTS end 
 
end.
