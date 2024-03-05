{
Sample program to show the use of arrays in 
the P65Pas compiler for 6502 CPU.
By Tito Hinostroza 27/06/2019
}
program Arrays;
const
  AR1 = [1,2,3]; 
type
  //Short declaration
  Tarr5Char = [5]char;  
  //Long declaration
  Tarr10Char = array[10] of byte;
var 
  arr5: TArr5char;  //Using type array
  a , b: [3]byte;  //Type in variable declaration.
  str: []char = 'HI!';  //Initialized with string
  numbers: []byte = [1,2,3];  //Initialized with constant
  screen: [1000]byte absolute $400;
  n,i: byte;
  c: char;
begin
  arr5.clear;  //Fills zeros all memory.
  n := arr5.length;  //Number of items
  n := arr5.low;     //Lower index
  n := arr5.high;    //Higher index
//  c := arr5.item(0); //Read item
  c := arr5[0];      //Equivalent.
  for i:=0 to arr5.high do
    c := arr5[i];
  end;
  a := [$12, %1010, 12];  //Array assigment
  a := AR1;  //Array assigment
  a := b;  //Array assigment
end. 
