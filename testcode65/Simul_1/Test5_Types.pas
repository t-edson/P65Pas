{Test file for check types declarations in compiler P65Pas.
Written to be executed in a Commodore64 emulator.
                        By Tito Hinostroza 08/11/2020
}
uses Commodore64;
/////////////////  Types declaration ////////////////
type
  tarr1 = array[3] of byte;
  //tarr2 = [3]array of char;
  
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
