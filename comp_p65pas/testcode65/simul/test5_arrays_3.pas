{Test file for check arrays implementation in compiler P65Pas.
This code check arrays when index is WORD size.
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
  arrB0: array[2] of byte absolute $00F7;
  arrW0: array[2] of word absolute $00F9;
  iword0: word absolute $00FD;
var  //Variables in $0801
  arrB: array[10] of byte=(1,2,3,4,5,6,7,8,9,10);
  arrW: array[3] of word=(1234, 2345, 3456);
  iword: word;
  num: byte;
  numW: word;
begin
  //Arrays in zero-page cannot be initialized in declaration.
  arrB0:= [10,20];
  arrW0:= [$1234, $5678];
  iword0 := 0;

  //////////// INDEX-WORD CONSTANT ////////////
  // *** Need to be improved with real word index.
  //Get BYTE item with byte-index
  num := arrB0[word(1)];  //Array in page zero 
  if num=20 then bien else mal end; 
  num := arrB[word(1)];   //Array in other page
  if num=2 then bien else mal end; 

  //Get WORD item with byte-index
  numW := arrW0[word(1)];  //Array in page zero
  if numW=$5678 then bien else mal end; 
  numW := arrW[word(1)];   //Array in other page
  if numW=2345 then bien else mal end; 
 
  //////////// INDEX-WORD VARIABLE ////////////

  //Get BYTE item with word-index
  iword := 1;
  num := arrB0[iword];  //Array in page zero
  if num=20 then bien else mal end; 
  num := arrB[iword];   //Array in other page
  if num=2 then bien else mal end; 

  //Get BYTE item with word-index in zero page
  iword0 := 1;
  num := arrB0[iword0];  //Array in page zero
  if num=20 then bien else mal end; 
  num := arrB[iword0];   //Array in other page
  if num=2 then bien else mal end; 

  //Get WORD item with word-index
  iword := 1;
  numW := arrW0[iword];  //Array in page zero
  if numW=$5678 then bien else mal end; 
  numW := arrW[iword];   //Array in other page
  if numW=2345 then bien else mal end; 

  //Get WORD item with word-index in zero page
  iword0 := 1;
  numW := arrW0[iword0];  //Array in page zero
  if numW=$5678 then bien else mal end; 
  numW := arrW[iword0];   //Array in other page
  if numW=2345 then bien else mal end; 
  
  ////////////WRITE INDEX-BYTE CONSTANT ////////////

  //Write BYTE item with word-index
//  arrB0[1] := $12;  //Array in page zero
//  if arrB0[1]=$12 then bien else mal end; 
//  arrB[1] := $34;   //Array in other page
//  if arrB[1] = $34 then bien else mal end; 
//
//  //Write WORD item with word-index
//  arrW0[1] := $1234;  //Array in page zero
//  if arrW0[1] = $1234 then bien else mal end; 
//  arrW[1] := $3456;   //Array in other page
//  if arrW[1] = $3456 then bien else mal end; 
//
  //////////// WRITE INDEX-BYTE VARIABLE ////////////

  //Write BYTE item with word-index
  iword := 1;
  arrB0[iword] := $12;  //Array in page zero
  if arrB0[iword]=$12 then bien else mal end; 
  arrB[iword] := $34;   //Array in other page
  if arrB[iword]=$34 then bien else mal end; 

  num := $12;
  arrB0[iword] := num;  //Array in page zero
  if arrB0[iword]=$12 then bien else mal end; 
  arrB[iword] := num+1;   //Array in other page
  if arrB[iword]=$13 then bien else mal end; 

  //Write BYTE item with word-index page zero
  iword0 := 1;
  arrB0[iword0] := $12;  //Array in page zero
  if arrB0[iword0]=$12 then bien else mal end; 
  arrB[iword0] := $34;   //Array in other page
  if arrB[iword0]=$34 then bien else mal end; 

  num := $20;
  arrB0[iword0] := num;  //Array in page zero
  if arrB0[iword0]=$20 then bien else mal end; 
  arrB[iword0] := num+1;   //Array in other page
  if arrB[iword0]=$21 then bien else mal end; 
  
  //Get WORD item with word-index
  iword := 1;
  arrW0[iword] := $1234;  //Array in page zero
  if arrW0[iword]=$1234 then bien else mal end; 
  arrW[iword] := $3456;   //Array in other page
  if arrW[iword]=$3456 then bien else mal end; 

  numW := $1234;
  arrW0[iword] := numW;   //Array in page zero
  if arrW0[iword]=$1234 then bien else mal end; 
  arrW[iword] := numW+1;  //Array in other page
  if arrW[iword]=$1235 then bien else mal end; 
  
  //Get WORD item with word-index page zero
  iword0 := 1;
  arrW0[iword0]:=$5678;  //Array in page zero
  if arrW0[iword0]=$5678 then bien else mal end; 
  arrW[iword0] :=1234;   //Array in other page
  if arrW[iword0]=1234 then bien else mal end; 

  numW := $1234;
  arrW0[iword0]:=numW;  //Array in page zero
  if arrW0[iword0]=$1234 then bien else mal end; 
  arrW[iword0] :=numW+1;   //Array in other page
  if arrW[iword0]=$1235 then bien else mal end; 
  
  /////////// Return instruction
  asm RTS end; 
end. 
