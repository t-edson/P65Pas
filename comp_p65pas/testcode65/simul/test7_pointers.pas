{Sample code to test pointer functionality in P65Pas compiler.
By Tito Hinostroza}
uses Commodore64;
var
  //Initializing strings.
  str1: array of char = 'HELLO';
  str2: array of char = 'HELLO'#0#1;
  str3: array of char = 'HI'#13'HELLO';
  str4: array of char = #10'HI'#13'HELLO'#0;
  str5: array of char = #13#10;
  char1: char = 'C';
  char2: char = #12;
var
  b, b2: byte;
  w, w2: word;
  pb, pb2: ^byte;
  pw, pw2: ^word;
  var1,var2: byte;
  arradd: array of word = (@var1, @var2);
  varabs1: byte absolute $5001;
  varabsw: word absolute $5002;   
  varabsw2:word absolute $5004;  //Next to varabsw
  
procedure Pass;
begin
  CHROUT('O');
  CHROUT('K');
  CHROUT(',');
end;
procedure Fail;
begin
  CHROUT('E');
  CHROUT('R');
  CHROUT('R');
  CHROUT('O');
  CHROUT('R');
  CHROUT(chr(13));
end;

begin
  ///////////////////////////////////////////////////////////
  ////////////////////// Untyped Pointers ///////////////////
  ///////////////////////////////////////////////////////////
  STROUT(@'UNTYPED POINTERS:');
  
  //Setting addresses. Only for testing syntax;
  w := $10;    //Setting byte
  w := $FFFF;  //Setting word
  w := @var1;  //Address of variable. 
  w := @Pass;  //Address of procedure. 
  w := @'Hi'#32'World';  //Address of literal. 

  //Testing address set
  w := @varabs1;
  if w = $5001 then Pass else Fail; end; 
  w := @varabs1+$20;
  if w = $5021 then Pass else Fail; end; 
  w := @varabs1+$100+$05;
  if w = $5106 then Pass else Fail; end; 
  w := word($10);
  if w = word($10) then Pass; else Fail end;  
  w := $1012;
  if w = $1012 then Pass; else Fail end;  
  
  ///////////////////////////////////////////////////////////
  //////////////////// Typed Pointer Byte ///////////////////
  ///////////////////////////////////////////////////////////
  STROUT(@#13'===POINTER TO BYTE:');

  //Setting constant
  pb := word(0); //Setting zero
  if pb = word(0) then Pass; else Fail end;  
  pb := word($10); 
  if pb = word($10) then Pass; else Fail end;  
  //Setting word
  pb := $1012;    
  if pb = $1012 then Pass; else Fail end;  
  //Setting byte variable
  w := $20;
  pb := w;        
  if pb = word($20) then Pass; else Fail end;
  //Setting word variable
  w := $2030;
  pb := w;        
  if pb = $2030 then Pass; else Fail end;  
  //Setting address and read variable
  pb := @b;       
  if pb^ = b then Pass; else Fail end;  
  b := $23;
  if pb^ = $23 then Pass else Fail end;
  //Pointer asignment
  pb := $123;
  pb2 := pb;
  if pb2 = $123 then Pass else Fail end;

  STROUT(@#13'SETTING TARGET:');
  //Write variable pointed.
  pb := @b;
  pb^ := $FF;     
  if b = $ff then Pass else Fail end;
  pb^ := $0;     //Setting target with literal
  if pb^ = $0 then Pass; else Fail end;  

  pb^ := b;       //Setting target with variable
  if pb^ = b then Pass; else Fail end;  

  //Copying values pointed
  pb := @b;
  pb2 := @b2;
  b := $23;  
  pb2^ := pb^;
  if b2 = $23 then Pass else Fail end;

  pb := @b;
  b2 := pb^;
  if b = b2 then Pass else Fail end;

  STROUT(@#13'OPERATIONS:');
  //Addition
  pb := $1000;
  pb := pb + $345;
  if pb = $1345 then Pass; else Fail end;  
  pb := $1000;
  b := $10;
  pb := pb + b;
  if pb = $1010 then Pass; else Fail end;  

  //Substraction
  pb := $1345;
  pb := pb - $345;
  if pb = $1000 then Pass; else Fail end;  
  b := $10;
  pb := $1010;
  pb := pb - b;
  if pb = $1000 then Pass; else Fail end;

  //Asignments
  pb := $FF0;
  pb += $10;
  if pb = $1000 then Pass; else Fail end;  
  pb := $FF0;
  pb += b;
  if pb = $1000 then Pass; else Fail end;  

  //Inc() and Dec()
  pb := word($ff);
  inc(pb); 
  dec(pb); 
  if pb = word($ff) then Pass else Fail end;

  STROUT(@#13'TARGET OPERATIONS:');
  //Operaciones con desreferencia stVarRefVar
  b := $12;
  pb := @b;
  if pb^ = $12 then Pass else Fail end;
  if pb^ + 1 = $13 then Pass else Fail end;
  if pb^ - 1 = $11 then Pass else Fail end;
  if pb^ + pb^ = $24 then Pass else Fail end;
  if $0f and pb^  = $02 then Pass else Fail end;
  
  //Pendientes
//  delay_ms(pb^);
//  Inc(pb^);
//  Dec(pb^);
//  pb^.bit7 := 0;
//  chr(pb^);
//  bit(pb^);
//  word(pb^);
//  dword(pb^);

  //Operaciones con desreferencia 
  w := $1234;
  pb := @w;
  if (pb+1)^ = $12 then Pass else Fail end;
  if (pb+1)^ + 1 = $13 then Pass else Fail end;
  if (pb+1)^ - 1 = $11 then Pass else Fail end;
  if (pb+1)^ + (pb+1)^ = $24 then Pass else Fail end;  
  if $0f and (pb+1)^  = $02 then Pass else Fail end;
  
  //Pendientes
//  delay_ms((pb+1)^);
//  Inc((pb+1)^);
//  Dec((pb+1)^);
//  (pb+1)^.bit7 := 0;
//  chr((pb+1)^);
//  bit((pb+1)^);
//  word((pb+1)^);
//  dword((pb+1)^);

  //////////////////////////////////////////////////////
  //////////////// Type pointer word ///////////////////
  //////////////////////////////////////////////////////
  STROUT(@#13'===POINTER TO WORD:');

  //Setting constant
  pw := word(0); //Setting zero
  if pw = word(0) then Pass; else Fail end;  
  pw := word($10); 
  if pw = word($10) then Pass; else Fail end;  
  //Setting word
  pw := $1012;    
  if pw = $1012 then Pass; else Fail end;  
  //Setting byte variable
  w := $20;
  pw := w;        
  if pw = word($20) then Pass; else Fail end;
  //Setting word variable
  w := $2030;
  pw := w;        
  if pw = $2030 then Pass; else Fail end;  
  //Setting address and read variable
  pw := @w;       
  if pw^ = w then Pass; else Fail end;  
  w := $123;
  if pw^ = $123 then Pass else Fail end;
  //Pointer asignment
  pw := $123;
  pw2 := pw;
  if pw2 = $123 then Pass else Fail end;
    
  STROUT(@#13'SETTING TARGET:');
  //Write variable pointed.
  pw := @w;
  pw^ := $FFFF;
  if w = $FFFF then Pass else Fail end;
  pw^ := word(0);     //Setting target with literal
  if pw^ = word(0) then Pass; else Fail end;  
    
  pw^ := w;       //Setting target with variable
  if pw^ = w then Pass; else Fail end;  
    
  //Copying values pointed
  pw := @w;
  pw2 := @w2;
  w := $123;  
  pw2^ := pw^;
  if w2 = $123 then Pass else Fail end;
    
  pw := @w;
  w2 := pw^;
  if w = w2 then Pass else Fail end;

  STROUT(@#13'OPERATIONS:');
  //Addition
  pw := $1000;
  pw := pw + $345;
  if pw = $1345 then Pass; else Fail end;  
  pw := $1000;
  b := $10;
  pw := pw + b;
  if pw = $1010 then Pass; else Fail end;  

  //Substraction
  pw := $1345;
  pw := pw - $345;
  if pw = $1000 then Pass; else Fail end;  
  b := $10;
  pw := $1010;
  pw := pw - b;
  if pw = $1000 then Pass; else Fail end;

  //Asignments
  pw := $FF0;
  pw += $10;
  if pw = $1000 then Pass; else Fail end;  
  pw := $FF0;
  pw += b;
  if pw = $1000 then Pass; else Fail end;  

  //Inc() and Dec()
  pw := word($75);
  inc(pw); 
  dec(pw); 
  if pw = word($75) then Pass else Fail end;
  
  STROUT(@#13'TARGET OPERATIONS:');
  w := $12;
  pw := @w;
  if pw^ = word($12) then Pass else Fail end;
  if pw^ + word(1) = word($13) then Pass else Fail end;
  if pw^ - word(1) = word($11) then Pass else Fail end;
  if pw^ + pw^ = word($24) then Pass else Fail end;
  if word($0f) and pw^  = word($02) then Pass else Fail end;
  
  //Pendientes
//  delay_ms(pw^);
//  Inc(pw^);
//  Dec(pw^);
//  pw^.bit7 := 0;
//  chr(pw^);
//  bit(pw^);
//  word(pw^);
//  dword(pw^);
//
  //Operaciones con desreferencia
  varabsw2 := $2020;
  pw := @varabsw;
//  if (pw+1)^ = $2020 then Pass else Fail end;
//  if (pw+1)^ + 1 = $2021 then Pass else Fail end;
//  if (pw+1)^ - 1 = $2019 then Pass else Fail end;
//  if (pw+1)^ + (pw+1)^ = $4040 then Pass else Fail end;  
//  if $ff and (pw+1)^ = $20 then Pass else Fail end;
//  
//  //Pendientes
////  delay_ms((pw+1)^);
////  Inc((pw+1)^);
////  Dec((pw+1)^);
////  (pw+1)^.bit7 := 0;
////  chr((pw+1)^);
////  bit((pw+1)^);
////  word((pw+1)^);
////  dword((pw+1)^);
//
//

  asm RTS end; 
end.

