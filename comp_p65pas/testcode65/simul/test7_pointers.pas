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
  b: byte;
  w: word;
  pb: ^byte;
  pw: ^word;
  var1,var2: byte;
  arradd: array of word = (@var1, @var2);
  varabs1: byte absolute $101;
  varabs2: word absolute $102;

procedure Pass;
begin
  CHROUT('O');
  CHROUT('K');
  CHROUT(chr(13));
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
  STROUT(@'UNTYPED POINTERS:'#13);
  
  //Setting addresses. Only for testing syntax;
  w := $10;    //Setting byte
  w := $FFFF;  //Setting word
  w := @var1;  //Address of variable. 
  w := @Pass;  //Address of procedure. 
  w := @'Hi'#32'World';  //Address of literal. 

  //Testing address setted
  w := @varabs1;
  if w = $101 then Pass else Fail; end;
  w := @varabs2;
  if w = $102 then Pass else Fail; end; 
  w := word($10);
  if w = word($10) then Pass; else Fail end;  
  w := $1012;
  if w = $1012 then Pass; else Fail end;  
  
  ///////////////////////////////////////////////////////////
  ////////////////////// Typed Pointers /////////////////////
  ///////////////////////////////////////////////////////////
  STROUT(@'TYPED POINTERS:'#13);

  //Testing address setted
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

  pw := @w;       //Setting address or word variable
  if pw^ = w then Pass; else Fail end;  

  STROUT(@'SETTING TARGET:'#13);
  
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

  STROUT(@'OPERATIONS:'#13);
  
  pb := $1000;
  pb := pb + $345;
  if pb = $1345 then Pass; else Fail end;  

  pb := $1000;
  pb := pb + b;
  if pb = $1010 then Pass; else Fail end;  

  pb := $1345;
  pb := pb - $345;
  if pb = $1000 then Pass; else Fail end;  

  pb := $1010;
  pb := pb - b;
  STROUT(@'SUBSTRACTION WITH A VAR:');
  if pb = $1010 then Pass; else Fail end;
    
  pb := $FF0;
  pb += $10;
  if pb = $1000 then Pass; else Fail end;  

  pb := $FF0;
  pb += b;
  if pb = $1000 then Pass; else Fail end;  
  
  asm RTS end; 
end.

