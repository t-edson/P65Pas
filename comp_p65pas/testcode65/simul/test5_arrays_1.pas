{Test file for check arrays implementation in compiler P65Pas.
This code check arrays declaration and initialization.
Written to be executed in a Commodore64 emulator.
                        By Tito Hinostroza 08/11/2020
}
uses Commodore64;
/////////////////  Types declaration ////////////////
type
  tarr1 = array[3] of byte;
  string3 = array[3] of char;
  tarr2 = [3]char;    //Alternate syntax

const 
  ATYP_NAM: string3 = ('a','b','c');
  ATYP_DEC: array[3] of char = ('1','2','3'); 
  ARRAY_ARRAY: array[3] of char = ('1','2','3'); 
//var   x,y: byte;
// z: string3 = ('a','b','c');
  //n: byte = 1; 
//var  x: array[3] of byte = [1,2,3];   
//var  myarray: ARRAY[] OF char = 'abc'; //Tipo creado en la inicialización.
//var ar: ARRAY[1] OF char = 'a'; //Tipo creado en la inicialización.
//VAR xx: array[] of byte = [1,2,3];   
//var  ca: char = 'A'; //Declaración con inicialización.
  
var
  b,c: boolean;
  arr5: array[] of byte = [1,2,3];
begin

  b := true; c := true;

  if b and c then bien else mal; 
  if b and not c then mal else bien;
	//lógica invertida
  if not (b and c) then mal else bien; 
  //optimización C a Z
	if a+1>a then bien else mal; 

	//validación de ejecución completa de bloque	
	a := 0;
	if b and c then begin
    inc(a);   //al inicio del bloque
    bien; 
    inc(a);		//al fin del bloque
  end; 
	if a<>2 then mal; 
	//validación de no-ejecución completa de bloque	
	if b and not c then begin
    mal;
    mal;
    mal;  //al fin del bloque
  end; 

end.
