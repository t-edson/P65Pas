<!--
## Donate to the project

[![paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=7LKYWG9LXNQ9C&lc=ES&item_name=Tito%20Hinostroza&item_number=2153&no_note=0&cn=Dar%20instrucciones%20especiales%20al%20vendedor%3a&no_shipping=2&currency_code=USD&bn=PP%2dDonationsBF%3abtn_donateCC_LG%2egif%3aNonHosted)
-->


P65Pas 0.6
==========

Cross-platform Pascal cross-compiler for 6502 CPU.

![P65Pas IDE](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2019/03/Sin-título.png "P65Pas IDE")

P65Pas is a Pascal compiler and IDE, which generates binary and ASM code for the 6502 CPU.

No additional libraries or software required to compile. P65Pas generates the *.prg file directly. Additionally a BASIC Commodore program (POKE's) can be generated to charge the machine code. 

P65Pas works with a simplified version of the Pascal language, that has been adapted to work with limited resources small devices.

Currently, it only supports basic types. 

It includes a complete IDE/debugger/simulator to facilitate the development.

The P65Pas compiler includes optimization options so the code obtained is generally compact.

## Installation

P65Pas doesn't need installation, and have not dependencies, except the commons of the operative system, where it's runnig.

To run, it's only needed to download the folder from GitHub. There is compiled binaries for Windows-64 version (P65Pas-win64.exe) and Ubuntu version (P65Pas-linux).

If it's required other platform, it need to be compiled from the source code.

When starting, P65Pas could generate warning messsages, if not needed folders exist.

## Hello World

P65Pas can compile to a general 6502 system, and it's not possible to do a general "Hello World" program.

If we are going to compile for Commodore 64, we can use the Commodore64 unit and use the CHROUT procedure to send chars to screen:

```
{"Hello World" P65Pas program.}
program Hello;
uses Commodore64;
begin
  ChrOUT('H');
  ChrOUT('E');
  ChrOUT('L');
  ChrOUT('L');
  ChrOUT('O');
  asm RTS end 
end.
```
This code can generate a PRG program, to send the message "Hello" on the screen of the Commodore 64.

The code "asm RTS end" line, generate a RTS assembler instruction in the program. It's needed because the PRG program is called with a JSR instruction. 

## Devices supported

Currently, only 6502 CPU is supported, but the compiler is designed to include some variants. Of course, compatible devices, like 6510, can be targeted too.

Support for differents systems (like Apple II, Atari 800XL, Commodore 64, ...) can be implemented using appropriate units.

Currently there is only a unit to support Commodore 64 system. So, to compile to Commodore 64 system, it's recommended use the unit: "Commodore64":

```
program anything;
uses Commodore64; 
begin
  ...
end. 
```

The unit Commodore64 set the compiler to start compiling in the address $0801, and includes a starting code to run the program after loading the PRG.

Moreover, this unit includes some routines (like CHROUT and CHRIN) to control C64 screen and keyboard.

## IDE

P65Pas includes an IDE integrated to the compiler, to help on developing the programs.

Some features of the IDE are:

•	Cross-platform.

•	Multiple editors windows.

•	Syntax highlighting, code folding, word, and line highlighting for Pascal and ASM.

•	Code completion, and templates for the common structures IF, REPEAT, WHILE, …

•	Shows the assembler code and the resources used.

•	Support for themes (skins).

•	Code tools for completion and navigation.

•	Check syntax in REAL TIME!!!.

•	Several setting options.

•	Translated to english, french, spanish and german.

![Tito's Terminal](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2017/06/P65Pas-0.7_en.png "P65Pas with dark skin")

![P65Pas](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2019/06/Sin-título2.png "P65Pas for Ubuntu")

![Tito's Terminal](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2018/05/P65PasMac.jpg "P65Pas for Mac")


## Debugger/Simulator

P65Pas includes a graphical debugger for ASM instructions:

![Tito's Terminal](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2017/11/P65Pas-simulator.png "P65Pas debugger-simulator")

To control the execution, the following keys can be used:

F5 -> Set a breakpoint in the current position of  the assembler code.
F6 -> Reste the device.
F7 -> Step by step into subroutine.
F8 -> Step by step over subroutine.
F9 -> Run the program in real time.


## Language Reference

### Program structure

```
program <Name>;  //optional
uses
  //Units declarations

const
  //Constants declarations

var
  //Variables declarations

//<Procedures declaration>

begin
  //Main program body
end.
```

### Unit structure

```
unit <name>;
interface
uses
  //units declaration
const
  //Constant declaration
var
  //Variable declaration

//Procedures declaration

implementation

uses
  //units declaration
const
  //Constant declaration
var
  //Variable declaration

//Procedures implementation

end.
```

### Operators

```
Operator            Precedence
=================== ==========
 NOT, sign “-“         6
 *, DIV, MOD, AND      5
 +, -, OR, XOR         4
 =, <>, <, <=, >, >=   3
 := +=                 2
 ```
 
Not all the operators are implemented in all the types.
 
### Types

Types define how variables are stored in memory.

Types can be system type or user defined type. System types are:

```
Type           Size
============== ==========
 boolean       1 bit
 byte          1 byte
 char          1 byte
 word          2 bytes
 dword         4 bytes
 ```
Numerical types are all unsigned.

Specific byte of a word, can be access using fields:

```
  word_var.Low := $ff;
  word_var.High := $ff;
```

The types "word" and "dword" are not complete implemented in the current version of the compiler.

The user can define his own custom types:

```
TYPE 
  mybyte = byte;   //Alias for byte
  string10 = array[10] of char;  //Array type
```

### Variables

Variables are defined with the VAR keyword:

```
VAR
  var1 : byte;
  large_name_variable: boolean;
  c    : char = 'A';   //Include initialization
  str  : array[] of char = 'Hi';   //Complex type
```

All variables must have a type. Type can be system type or a user defined type.

```
TYPE mytype = char;
VAR  myvar: mytype;   
```

Variables can be defined too, at an absolute memory address:

```
var
  PORTB : BYTE absolute $06;
  flag  : boolean absolute $07;
  letter: char @$07;   //"@" if the alias for ABSOLUTE
```

### Arrays

Arrays can be defined using the following syntax:

```
VAR myarray: ARRAY[10] OF byte;
```
or 
```
VAR myarray: [10]byte;
```

Both definitions are the same and declare an array of 10 items.

Arrays are indexed from 0 to n-1, where "n" is the size of the array.

When defining arrays of chars, it's possible to omit the size if it's initialized with a string.

```
VAR 
  myarray: ARRAY[] OF char = 'Hola';  
```
The size of this array will be set to the size of the string. In this case is 4 chars.

En geenral, arrays can be initialized when declaring a varaible;

```
TYPE 
  TAStr = [3]char;
  TAByte = [5]byte;
VAR
  str : TASrt = ('a','b','c');
  numb: TAByte = (1, 2); //Only the first can be  set.
```

Arrays support several methods:

array.length ->  Returns the size of te arrays.

array.low -> low index of the array. Always returns 0.

array.high -> high index of the array. Always returns n-1.

array.clear -> Clear the elements of array to its default value (Chars->#0, Byte, Word -> 0, Boolean -> false).

No dynamic arrays are supported.

### Control structures

P65Pas doens't follow the common Pascal syntax. Instead, a new Modula-2, style syntax is implemented.

The common control structures have the following forms:

```
IF <condition> THEN 
  <block of code>
END;

IF <condition> THEN 
  <block of code>
ELSE
  <block of code>
END;

IF <condition> THEN 
  <block of code>
ELSIF <condition> THEN 
  <block of code>
ELSE
  <block of code>
END;

WHILE <condition> DO
  <block of code>
END;

REPEAT
  <block of code>
UNTIL <condition>;

FOR  <variable> := <start-value> TO <end-value> DO 
  <block of code>
END;
```

### System Functions

System functions are always available in code. They don't need to be defined or included in a unit.

```
FUNCTION       DESCRIPTION
============== =================================================
delay_ms()	   Generate a time delay in miliseconds, from 0 to 65536.
Inc()          Increase a variable.
Dec()          Decrease a varaible.
Exit()         Exit from a procedure or end the program.
Ord()          Convert a char to a byte.
Chr()          Convert a byte to a char.
Byte()         Convert an expression to a byte expression.
Word()         Convert an expression to a word expression.
```

### Procedure and Functions

P65Pas use the Modula-2 syntax for procedures and functions:

Proedures are declared in the common Pascal syntax:

```
  procedure proc2(par1: byte);
  begin
    if par1 = 0 then 
      exit;
    else
      par1 := 5;
    end;  
  end;
```

Functions are declared the same, but indicating the type to return:

```
procedure TheNext(par1: byte): byte;
begin
  exit(par1 + 1);
end;
```

The return value is indicated with the exit() instruction.

### Register parameters

When using in procedures parameters, a REGISTER parameter can be included:

```
procedure QuickParameterProc(regvar: byte REGISTER);
begin
  //Be carefull if put some code here
  PORTB := regvar;
end;
```

Register parameters are fast, because they use CPU register instead of RAM for passing values.

Register parameters are defined using the following reserved words:

* REGISTER -> Use internal work register defined by the compiler. Can be used for types of one or two bytes.
* REGISTERA -> Use always the register A of the CPU. Can be used only for 1-byte size parameters.
* REGISTERX -> Use always the register X of the CPU. Can be used only for 1-byte size parameters.
* REGISTERY -> Use always the register Y of the CPU. Can be used only for 1-byte size parameters.

As register parameters use internal CPU register, they values could be lost after some instruction is executed, so is a good practice to save immediatly they value in other normal variable. So the first operation in a procedure, using a register parameter must be read this parameter.



### ASM blocks

P65Pas have a complete support for inserting ASM code inside the Pascal source.

ASM blocks must be included between the delimiters ASM and END:

```
procedure DoSomething;
begin
  x := 10;
  asm
    ;Load 3 to Acumulator 
    LDA #03
    RTS
  end
end;
```

ASM blocks are not instructions, that's why they are not finished with ";". It lets the ASM block, to be included in almost any place of the source code, like a comment.

WARNING: Changing some register used by compiler, inside an ASM block, can generate errors in compilation or in the code compiled.

Absolute and relative Labels can be used too:

```
asm 
  JMP $+3	;Jump to next instruction
  RTS
end
```

```
asm 
  ;infinite loop
label:
  NOP
  JMP label
end
```

Program variables can be accessed, using his common name:

```
var 
 byte1: byte; 
 car1: char; 
 word1: word;
begin
  //Low level clear
  asm 
    INC byte1
    INC car1
    INC word1.Low
  end
end.
```

Constant can be accessed too, using the same way.

In general, parameters of instructions can be:

- Assembler literal .
- Assembler labels.
- Pascal variables.
- Pascal constants.
- Pascal function/procedures.

The next example shows how to access to a Pascal constant:

```
const
  SMALL = $FF;
  BIG   = $FF;
begin
  //Low level clear
  asm 
    LDA #SMALL
	LDX #<BIG   ;low byte
	LDY #>BIG   ;high byte
  end
end.
```

When an instruction use an parameter bigger than a byte, the operators "<" and ">" an be used.

It's possible to use the directive ORG inside a ASM block, too:

```
  asm 
    org $-2
  end
  vbit := 1;
```

The address in ORG, can be absolute or relative. 

WARNING: Changing the PC pointer with ORG, can generate errors in the compilation or in execution.

## Directives

Directives are special instructions inserted in the source code that are interpreted and executed by the compiler when compiling the source code (in compilation time).

### Directive Programming Language

Directives have their own programmig language. It's a simple and interpreted language (with instructions, variables, operators and conditional structures) what is different from Pascal.

Some features of this programming language are:

*	It's case insensitive, like Pascal is.
*	Instructions are contained in one single line and are delimited by {$ … }
*	It's not a typed language. Variables can change their type and value in execution and different type variables can be assigned.
*	Variables don't need to be defined before using.
*	There are only two types for variables: strings and numbers.

### Variables

Variables are assigned with the instruction $SET:

```
{$SET x = 1}
{$SET y = 1 + x}
{$SET x = 'I'm now a string'}
```

$SET, is not a declaration, but an assignment. First time a variable is assigned, it's created.

Content of a variable, can be shown using instructions like $MSGBOX oo $INFO:

{$MSGBOX 'x is:' + x}

### System Variables

There are some system variables, accessible from the directives language. They are:
 
{$MSGBOX PIC_MODEL} -> Shows the PIC model defined.

{$MSGBOX PIC_FREQUEN} -> Shows the Clock frequency.

{$MSGBOX PIC_MAXFREQ} -> Shows the Max Clock frequency for the device.

{$MSGBOX SYN_MODE} -> Shows the syntax Mode of the compiler.

(*) To see the complete list, check the User Manual.

### List of Directives

The next directives are supported by P65Pas:

#### $COMMODORE64

Indicates the compiler to compile to a Commodore 64 system. 

In this mode when compiling at $0801, the compiler generates the instruction "SYS 2062" in the BASIC buffer, to run automatically the program after loading the PRG file.

#### $FREQUENCY

Specify the clock frequency, in MHz or KHz. Example:

```
{$FREQUENCY 10Mhz}
```

Frequency information is used for:

* The compiler, when needed to generate delays.
* The simulator, for Real Time simulation.

If delays are used in the program, only some frequencies are supported. They are:

1MHz, 2Mhz, 4Mhz, 8MHz, 10MHz, 12MHz, 16MHz or 20MHz.

If frequency is not specified, the default value is 4MHz.

#### $MODE

Specify the syntax mode, used by the compiler. The allowed values are:

{$MODE P65Pas} -> Default mode. Use the new syntax for the control structures.

{$MODE PASCAL} -> Clasic Pascal mode. Use the common Pascal syntax for the control structures.

#### $MSGBOX

Shows a text message in the screen:

{$MSGBOX 'Hello World'} -> Shows the message 'Hello World' in the screen.

{$MSGBOX PIC_MODEL} -> Shows the system variable PIC_MODEL, that is the PIC model defined.

{$MSGBOX PIC_FREQUEN} -> Shows the Clock frequency.

{$MSGBOX 'clock=' + PIC_FREQUEN}  -> Shows the message: "clock=8000000" (if the Frequency was set to 8MHz).

#### $MSGERR

Shows a text message in the screen, with an error icon.

#### $MSGWAR

Shows a text message in the screen, with a warning icon.

#### $INCLUDE

Includes the contents of a external file, into de source code:

```
{$INCLUDE aaa.pas}
{$INCLUDE d:\temp\aaa.txt}
x := {$INCLUDE expression.txt};
```

#### $OUTPUTHEX

Defines the name of the output binary file *.hex.

```
{$OUTPUTHEX myoutput.hex}  // Relative path
{$OUTPUTHEX d:\temp\myoutput.hex}  //Absolute path
```

When relative path is used, the file will be created in the same folder the Pascal program is.

If it's not defined the name of the *.hex file, it will be used the name of the program/unit compiled. So if the program is called "myprogram" (and the file is "myprogram.pas"), then the *.hex file will be "myprogram.hex".

Directive {$OUTPUTHEX}, can be placed in any part of the source code and can be used several times. If so, the output file will be the defined by the last directive.

#### $DEFINE

Define symbols or macros

To define a symbol we can do:

```
{$define MY_SYMBOL}
```

Once defined, it can be tested using $IFDEF directive.

To define a macro we can do:

```
{$DEFINE macro=Hello}
```

Then we can expand a macro, in the code, using the way:

{$macro}

Following, there a sample code:

```
{$DEFINE value=$FF}
uses Commodore64;
var x: byte;
begin
  x := {$value};
end.
```

#### $SET

Set a value for a variable. If variables doesn't exist, it will be created.

```
{$SET pin_out='PORTB.0'}
uses Commodore64;
begin
  SetAsOutput({$pin_out});
  {$pin_out} := 1;
end.
```

Variables can be numbers or string.

Variables supports expresions:

```
{$SET a_var = 1 + 2 * another_var + 2 ^ sin(0.5)}
```

Unlike macros, variables values are solved when assigned. Macros values, are solved when macro is referenced.

#### $IFDEF, $ELSE, $ENDIF

This directives let us to define conditional compilation blocks:

Directive $IFDEF check the existence of some macro or variable and according to that, compile or not some blocks of code.

It has two forms: 

```
{$IFDEF <identifier>} 
... 
{$ENDIF}
```

```
{$IFDEF <identifier>} 
... 
{$ELSE}
... 
{$ENDIF}
```
The next code is an example of use:

```
{$DEFINE MyPinOut=PORTB.0}
uses Commodore64;
begin
{$IFDEF MyPinOut}
{$ELSE}
  {$DEFINE MyPinOut=PORTB.1}
{$ENDIF}
  SetAsOutput({$MyPinOut});
  {$MyPinOut} := 1;
end.
```

#### $IFNDEF

This directive is the opposite version of $IFDEF.

```
{$DEFINE MyPinOut=PORTB.0}
uses Commodore64;
begin
{$IFNDEF MyPinOut}
  {$DEFINE MyPinOut=PORTB.1}
{$ENDIF}
  SetAsOutput({$MyPinOut});
  {$MyPinOut} := 1;
end.
```

#### $IF

This directives let us to define conditional compilation blocks, using expressions:

Directive $IF evaluates an expression, and according to the result, compile or omit some blocks of code.

The common syntax is: 

```
{$IF <expression>} 
... 
{$ENDIF}
```

A long way can be used too:

```
{$IF <expression>} 
... 
{$ELSE}
... 
{$ENDIF}
```
 
The following code shows an example of use:

```
{$IF value>255}
var x: word;
{$ELSE}
var x: byte;
{$ENDIF}
```

As there is not a boolean type, a boolean expression returns the number 1 when the expression is TRUE and 0 when the expression is FALSE.

On the other side, instruction {$IF} will consider as TRUE, any number different from 0, or any string not empty.

#### $IFNOT

It's the opposite version of $IF.

```
{$IFNOT value>255}
var x: byte;
{$ELSE}
var x: word;
{$ENDIF}
```

#### $CLEAR_STATE_RAM

USed to define the initial state of RAM memory. 

$CLEAR_STATE_RAM, set the state of all the RAM as unimplemented, clearing all previous setting.

It's used before of starting to define the RAM for a device, using the directives $SET_STATE_RAM and $SET_MAPPED_RAM.


## P65Pas Limitations

•	Only basic types are implemented: byte, char, boolean, word an dword(limited support).

•	Cannot declare records.

•	No recursion implemented, Because of the limited hardware resources.

•	No float point implemented.

Some of these limitations must be solved in next versions.

## Development

P65Pas is a free software (GPL license) and it's opened for the collaboration of anyone who is interested. 

There is still, much work for development or documentation, so any help will be appreciated.

## Source Code

The source code of the compiler is in the folder /Source.

To compile P65Pas, it's needed to have the following libraries:

* SynFacilUtils
* MisUtils
* MiConfig
* P65Utils 
* t-Xpres 
* UtilsGrilla
* ogEditGraf

All of them, must be availables on the GitHub. Check the versions used.

These libraries don't include package. They are only files in folders that need to be included when compiling P65Pas.

P65Pas has been compiled, using the version 1.8.0 of Lazarus. Tested in Windows, Ubuntu and Mac.

To have more information about the compiler, check the Technical Documentation (Only in spanish by now).

## Libraries

P65Pas is a new project and it's still in development and there are not dedicated libraries for the compiler. 

