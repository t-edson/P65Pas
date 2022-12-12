{Test file for check arrays implementation in compiler P65Pas.
This code check arrays when index is BYTE size.
Written to be executed in a Commodore64 emulator.
                        By Tito Hinostroza 30/08/2022
}
uses Commodore64;
//To disable Zero-age allocation. We aill allocate manually.
{$SET_DATA_ADDR ''}  
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
    //Aditional RTS to exit proc. and program.
    asm RTS end   
  end;
var  //Variables in Zero-page
  //ATENTION: Variables are mapped to fit exactly in zero page.
  ibyte0: byte absolute $02; //Free byte
  arrB0: array[2] of byte absolute $00F7;
  arrW0: array[2] of word absolute $00F9;
var  //Variables in $0801
  arrB: array[10] of byte=(1,2,3,4,5,6,7,8,9,10);
  arrW: array[3] of word=(1234, 2345, 3456);
  ibyte: byte; 
  num: byte;
  numW: word;
begin
  //Arrays in zero-page cannot be initialized in declaration.
  arrB0:= [10,20];
  arrW0:= [$1234, $5678];
  ibyte0 := 0;

  ////////////READ INDEX-BYTE CONSTANT ////////////

  //Get BYTE item with byte-index
  num := arrB0[1];  //Array in page zero
  if num=20 then bien else mal end; 
  num := arrB[1];   //Array in other page
  if num=2 then bien else mal end; 

  //Get WORD item with byte-index
  numW := arrW0[1];  //Array in page zero
  if numW=$5678 then bien else mal end; 
  numW := arrW[1];   //Array in other page
  if numW=2345 then bien else mal end; 

  //////////// READ INDEX-BYTE VARIABLE ////////////

  //Get BYTE item with byte-index
  ibyte := 1;
  num := arrB0[ibyte];  //Array in page zero
  if num=20 then bien else mal end; 
  num := arrB[ibyte];   //Array in other page
  if num=2 then bien else mal end; 

  //Get BYTE item with byte-index page zero
  ibyte0 := 1;
  num := arrB0[ibyte0];  //Array in page zero
  if num=20 then bien else mal end; 
  num := arrB[ibyte0];   //Array in other page
  if num=2 then bien else mal end; 

  //Get WORD item with byte-index
  ibyte := 1;
  numW := arrW0[ibyte];  //Array in page zero
  if numW=$5678 then bien else mal end; 
  numW := arrW[ibyte];   //Array in other page
  if numW=2345 then bien else mal end; 

  //Get WORD item with byte-index page zero
  ibyte0 := 1;
  numW := arrW0[ibyte0];  //Array in page zero
  if numW=$5678 then bien else mal end; 
  numW := arrW[ibyte0];   //Array in other page
  if numW=2345 then bien else mal end; 
 
  ////////////WRITE INDEX-BYTE CONSTANT ////////////

  //Write BYTE item with byte-index
  arrB0[1] := $12;  //Array in page zero
  if arrB0[1]=$12 then bien else mal end; 
  arrB[1] := $34;   //Array in other page
  if arrB[1] = $34 then bien else mal end; 

  //Write WORD item with byte-index
  arrW0[1] := $1234;  //Array in page zero
  if arrW0[1] = $1234 then bien else mal end; 
  arrW[1] := $3456;   //Array in other page
  if arrW[1] = $3456 then bien else mal end; 

  //////////// WRITE INDEX-BYTE VARIABLE ////////////

  //Write BYTE item with byte-index
  ibyte := 1;
  arrB0[ibyte] := $12;  //Array in page zero
  if arrB0[ibyte]=$12 then bien else mal end; 
  arrB[ibyte] := $34;   //Array in other page
  if arrB[ibyte]=$34 then bien else mal end; 

  num := $12;
  arrB0[ibyte] := num;  //Array in page zero
  if arrB0[ibyte]=$12 then bien else mal end; 
  arrB[ibyte] := num+1;   //Array in other page
  if arrB[ibyte]=$13 then bien else mal end; 

  //Write BYTE item with byte-index page zero
  ibyte0 := 1;
  arrB0[ibyte0] := $12;  //Array in page zero
  if arrB0[ibyte0]=$12 then bien else mal end; 
  arrB[ibyte0] := $34;   //Array in other page
  if arrB[ibyte0]=$34 then bien else mal end; 

  num := $20;
  arrB0[ibyte0] := num;  //Array in page zero
  if arrB0[ibyte0]=$20 then bien else mal end; 
  arrB[ibyte0] := num+1;   //Array in other page
  if arrB[ibyte0]=$21 then bien else mal end; 
  
  //Get WORD item with byte-index
  ibyte := 1;
  arrW0[ibyte] := $1234;  //Array in page zero
  if arrW0[ibyte]=$1234 then bien else mal end; 
  arrW[ibyte] := $3456;   //Array in other page
  if arrW[ibyte]=$3456 then bien else mal end; 

  numW := $1234;
  arrW0[ibyte] := numW;  //Array in page zero
  if arrW0[ibyte]=$1234 then bien else mal end; 
  arrW[ibyte] := numW+1;   //Array in other page
  if arrW[ibyte]=$1235 then bien else mal end; 
  
  //Get WORD item with byte-index page zero
  ibyte0 := 1;
  arrW0[ibyte0]:=$5678;  //Array in page zero
  if arrW0[ibyte0]=$5678 then bien else mal end; 
  arrW[ibyte0] :=1234;   //Array in other page
  if arrW[ibyte0]=1234 then bien else mal end; 

  numW := $1234;
  arrW0[ibyte0]:=numW;  //Array in page zero
  if arrW0[ibyte0]=$1234 then bien else mal end; 
  arrW[ibyte0] :=numW+1;   //Array in other page
  if arrW[ibyte0]=$1235 then bien else mal end; 

  /////////// Return instruction
  asm RTS end; 
end. 
