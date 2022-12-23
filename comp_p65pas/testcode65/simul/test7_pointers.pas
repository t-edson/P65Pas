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
  w: word;
  pb, pb2: ^byte;
  pw: ^word;
  var1,var2: byte;
  arradd: array of word = (@var1, @var2);
  varabs1: byte absolute $101;
  varabs2: word absolute $102;

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
  if w = $101 then Pass else Fail; end;
  w := @varabs2;
  if w = $102 then Pass else Fail; end; 
  w := @varabs2+$20;
  if w = $122 then Pass else Fail; end; 
  w := @varabs2+$100+$05;
  if w = $207 then Pass else Fail; end; 
  w := word($10);
  if w = word($10) then Pass; else Fail end;  
  w := $1012;
  if w = $1012 then Pass; else Fail end;  
  
  ///////////////////////////////////////////////////////////
  ////////////////////// Typed Pointers /////////////////////
  ///////////////////////////////////////////////////////////
  STROUT(@#13'TYPED POINTERS:');

  //Testing address set
  pb := word($10); //Setting byte
  if pb = word($10) then Pass; else Fail end;  

  pb := $1012;    //Setting word
  if pb = $1012 then Pass; else Fail end;  
  
  w := $20;
  pb := w;        //Setting word variable
  if pb = word($20) then Pass; else Fail end;  

  w := $2030;
  pb := w;        //Setting word variable
  if pb = word($2030) then Pass; else Fail end;  

  pb := @b;       //Setting address or byte variable
  if pb^ = b then Pass; else Fail end;  

  b := $23;
  if pb^ = $23 then Pass else Fail end;
  
  pb^ := $FF;
  if b = $ff then Pass else Fail end;

  pw := @w;       //Setting address or word variable
  if pw^ = w then Pass; else Fail end;  

  pb := $123;
  pb2 := pb;    //asignación punteros
  if pb2 = $123 then Pass else Fail end;

  //Acceso a variables
  pb := @b;
  pb2 := @b2;
  b := $23;  
  pb2^ := pb^;
  if b2 = $23 then Pass else Fail end;

  pb := @b;
  b2 := pb^;
  if b = b2 then Pass else Fail end;
  
  
  STROUT(@#13'SETTING TARGET:');
  
  pb^ := $10;     //Setting target with literal
  if pb^ = $10 then Pass; else Fail end;  

  pb^ := b;       //Setting target with variable
  if pb^ = b then Pass; else Fail end;  

  pw^ := word($10); //Setting target with literal
  if pw^ = $10 then Pass; else Fail end;  

  pw^ := $1011;   //Setting target with literal
  if pw^ = $1011 then Pass; else Fail end;  

  pw^ := w;       //Setting target with variable
  if pw^ = w then Pass; else Fail end;  

  STROUT(@#13'OPERATIONS:');
  
  pb := $1000;
  pb := pb + $345;
  if pb = $1345 then Pass; else Fail end;  

  pb := $1000;
  pb := pb + b;
  if pb = $1010 then Pass; else Fail end;  

  pb := $1345;
  pb := pb - $345;
  if pb = $1000 then Pass; else Fail end;  

  b := $10;
  pb := $1010;
  pb := pb - b;
  if pb = $1000 then Pass; else Fail end;
    
  pb := $FF0;
  pb += $10;
  if pb = $1000 then Pass; else Fail end;  

  pb := $FF0;
  pb += b;
  if pb = $1000 then Pass; else Fail end;  

  //Aritmética de punteros
  pb := word($75);
  inc(pb); 
  dec(pb); 
  if pb = word($75) then Pass else Fail end;

  //////////////////////////////////////////////////////
  //////////////// Punteros a byte /////////////////////
  //////////////////////////////////////////////////////
  

  //Operaciones con desreferencia stVarRefVar
  b := $12;
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

  //Operaciones con desreferencia stVarRefExp
  //Se asume para esta prueba que "n", está ubicado después de "b"
  //De otar forma no funcionará, porque pb+1, fallaría
  w := $1234;
  pb := @w;
  if (pb+1)^ = $12 then Pass else Fail end;
  if (pb+1)^ + 1 = $13 then Pass else Fail end;
  if (pb+1)^ - 1 = $11 then Pass else Fail end;
  {Expresión muy compleja stVarRefExp + stVarRefExp. No implementada por ahora.
 //  if (pb+1)^ + (pb+1)^ = $24 then Pass else Fail end;  
  }
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
  //////////////// Punteros a word /////////////////////
  //////////////////////////////////////////////////////
  ptrWord1 := 0;    //asignación constante
  if ptrWord1 = 0 then Pass else Fail end;
  ptrWord2 := 255;  //asignación constante
  if ptrWord2 = 255 then Pass else Fail end;
  b := 5;
  ptrWord1 := b;    //asignación byte 
  if ptrWord1 = 5 then Pass else Fail end;
  
  ptrWord1 := $85;
  ptrWord2 := ptrWord1;    //asignación punteros
  if ptrWord2 = $85 then Pass else Fail end;

  //Aritmética de punteros
  ptrWord2 := ptrWord1+$10;    //asignación expresión de punteros
  if ptrWord2 = $95 then Pass else Fail end;
  
  ptrWord2 := ptrWord1-$10;    //asignación expresión de punteros
  if ptrWord2 = $75 then Pass else Fail end;

  inc(ptrWord2); 
  dec(ptrWord2); 
  if ptrWord2 = $75 then Pass else Fail end;
  
  //Acceso a variables
  x := $23;
  ptrWord1 := @x;

  y := $12;
  ptrWord2 := @y;

  if ptrWord1^ = word($23) then Pass else Fail end;
  ptrWord1^ := word($FF);
  if x = word($ff) then Pass else Fail end;

  x := $23;  
  ptrWord2^ := ptrWord1^;
  if y = word($23) then Pass else Fail end;

  ptrWord1 := @x;
  y := ptrWord1^;
  if x = y then Pass else Fail end;

  //Operaciones con desreferencia stVarRefVar
  x := $12;
  if ptrWord1^ = word($12) then Pass else Fail end;
  if ptrWord1^ + word(1) = word($13) then Pass else Fail end;
  if ptrWord1^ - word(1) = word($11) then Pass else Fail end;
  if ptrWord1^ + ptrWord1^ = word($24) then Pass else Fail end;
//  if word($0f) and ptrWord1^  = word($02) then Pass else Fail end;
//  
//  //Pendientes
////  delay_ms(ptrWord1^);
////  Inc(ptrWord1^);
////  Dec(ptrWord1^);
////  ptrWord1^.bit7 := 0;
////  chr(ptrWord1^);
////  bit(ptrWord1^);
////  word(ptrWord1^);
////  dword(ptrWord1^);
//
//  //Operaciones con desreferencia stVarRefExp
//  //Se asume para esta prueba que "y", está ubicado después de "x"
//  //De otar forma no funcionará, porque ptrWord1+1, fallaría
//  y := $12;  
//  if (ptrWord1+1)^ = $12 then Pass else Fail end;
//  if (ptrWord1+1)^ + 1 = $13 then Pass else Fail end;
//  if (ptrWord1+1)^ - 1 = $11 then Pass else Fail end;
//  {Expresión muy compleja stVarRefExp + stVarRefExp. No implementada por ahora.
// //  if (ptrWord1+1)^ + (ptrWord1+1)^ = $24 then Pass else Fail end;  
//  }
//  if $0f and (ptrWord1+1)^  = $02 then Pass else Fail end;
//  
//  //Pendientes
////  delay_ms((ptrWord1+1)^);
////  Inc((ptrWord1+1)^);
////  Dec((ptrWord1+1)^);
////  (ptrWord1+1)^.bit7 := 0;
////  chr((ptrWord1+1)^);
////  bit((ptrWord1+1)^);
////  word((ptrWord1+1)^);
////  dword((ptrWord1+1)^);
//
//

  
  asm RTS end; 
end.

