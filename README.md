## Donate to the project

[![paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=7LKYWG9LXNQ9C&lc=ES&item_name=Tito%20Hinostroza&item_number=2153&no_note=0&cn=Dar%20instrucciones%20especiales%20al%20vendedor%3a&no_shipping=2&currency_code=USD&bn=PP%2dDonationsBF%3abtn_donateCC_LG%2egif%3aNonHosted)


P65Pas 1.0.0
============

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

If it's required other platform, it needs to be compiled from the source code.

The binaries for the IDE are located in root "/" of the folder downloaded:
* P65Pas-win64.exe -> Executable of the IDE for Windows.
* P65Pas-linux -> Executable of the IDE for Linux.

The binaries for the compiler are located in the folder /comp_p65pas:
* p65pas-win64.exe -> Executable of the Compiler for Windows.
* p65pas-linux -> Executable of the compiler for Linux.

The IDE binary includes the compiler too, so the compiler executable is not used when compiling from the IDE.

When starting the IDE, P65Pas could generate warning messsages, if needed folders doesn't exist.

### Folder structure

This repository includes all the files for both development and execution. It also includes the IDE and compiler files.

IDE files:

* "/" -> Includes the binaries and config files.
* "/Source" -> Source files for the IDE.
* "/syntax" -> Syntax files for highlighter and completion code.
* "/temp" -> Temporal folder for new files.
* "/themes" -> Themes files for the IDE.
* "/comp_p65pas" -> Files and binaries of the compiler.

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

![P65Pas](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2019/06/Sin-título2.png "P65Pas for Ubuntu")

![P65Pas for Mac](https://github.com/t-edson/P65Pas/blob/trunk/screenshots/PicPasMac.jpg "P65Pas for Mac")

## Debugger/Simulator

P65Pas IDE includes a complete GUI debugger for ASM instructions:

![Assembler Debugger of P65Pas](https://github.com/t-edson/P65Pas/blob/trunk/screenshots/ide_debugger.png "Assembler Debugger of P65Pas")

To control the execution, the following keys can be used:

* F5 -> Set a breakpoint in the current position of  the assembler code.
* F6 -> Reste the device.
* F7 -> Step by step into subroutine.
* F8 -> Step by step over subroutine.
* F9 -> Run the program in real time.

The debugger included in the IDE can execute the executable code step by step or in blocks.

It is based on a virtual system that includes a 6502 CPU and 64K of RAM.

The simulation environment is an empty system, composed only of CPU and RAM memory. There are no additional devices attached and no ROM routines preloaded into memory.

All that is loaded into memory is the latest compiled code from the IDE.
All 6502 CPU instructions are implemented but have not been all tested to execute as a real 6502 model. Arithmetic operations in BCD are not yet implemented for simulation.

Be careful about executing infinite loops or invalid instructions, because they could hang the program.

## Compiler

The compiler implements a simplified and adpated version of the Pascal language.

Some differences with the standard Pascal are:

* Functions and procedures are declared with PROCEDURE (Like in Modula 2).
* Conditional (IF) and loop structures (WHILE) don't use the initial BEGIN for the block (Like in Modula 2).
* Byte variables cannot be free assigned to word variables. A cast is needed.
* There is not a string type predefined. The closer data type could be an array of chars.
* Word variables have the methods .high and .low to access to individual bytes. The Lo() and Hi() functions are not implemented because .low and .high can do the same and even can be assigned like common variables.
* Arrays are declared in the form: "ARRAY[n] OF type" without specifying the initial index, because all arrays start always in the index zero.
* Arrays admit an simplified declaration "[n]type".
* Pointers and words can be free interchanged.
* REGISTER variables and parameters can be defined in the native syntax.
* Functions/Procedures can be overloaded like in modern versions of Pascal.
* Records are defined with the reserved word OBJECT and don't support variable part.
* No recursion is supported.
* No SET are impemented.

Compiler generates directly, a PRG output file without any dependency. 

When compiling for Commodore 64, the PRG output file (starting at $800) includes the isntruction "SYS 2062" in the BASIC buffer, to run automatically the program after loading the PRG file.

In normal conditions the compiled program follows the following structure:

```
<JMP instruction to the start of code>
<Local variables and procedures/functions parameters>
<Procedures/fucntions code, including Units.>
<Start of code>
<Main program>
```

The start of the program if defined with the directive $ORG. If it's not specified, the program will start at the $000 address.

Procedure and function parameters don't use the stack, they are stored as global variables. This variables can be shared when optimizing options are enabled.


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
  flag  : boolean absolute SomeOtherVariable;
  letter: char @$07;   //"@" if the alias for ABSOLUTE
```

ABSOLUTE variables cannot be initialized in the declaration, because the compiler cannot generate an output file when separated blocks of memory.

```
  c    : char = 'A' absolute $FF;  //Fails
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

In general, arrays can be initialized when declaring a variable;

```
TYPE 
  TAStr = [3]char;
  TAByte = [4]byte;
VAR
  str : TAStr = ('a','b','c');
  numb: TAByte = (1, 2, 3); //ERROR: Expected 4 items.
```

Arrays support several methods:

array.length ->  Returns the size of te arrays.

array.low -> low index of the array. Always returns 0.

array.high -> high index of the array. Always returns n-1.

array.clear -> Clear the elements of array to its default value (Chars->#0, Byte, Word -> 0, Boolean -> false).

Arrays can be constant too:

```
const
  ARRCONST = [1,2,3];
```

Assigment between arrays is possible;

```
  arrayA := arrayB;
```

The assigment is only possible if both arrays are of the same type.

No dynamic arrays are supported.

### Pointers

Pointers are declared in the Pascal common syntax:

``` 
var 
  p: ^byte;
```

Then, the pointer can be used to address some byte variable:

```
  p := @somebytevar;
  p^ := $FF;   //Write to "somebytevar".
```

Pointers can address all the 64K RAM memory, because they use 2 bytes like a word variable.

Pointers can be incremented or decremented like word variables.

### Conditional structures

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
```

All conditional blocks need the final END delimiter, even if one instructions is used in the IF body.

### Loop structures

P65pas implements the following loop structures:

```
WHILE <condition> DO
  <block of code>
END;

REPEAT
  <block of code>
UNTIL <condition>;

FOR <variable> := <start-value> TO <end-value> DO 
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
Dec()          Decrease a variable.
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

### Register variables

Variables can be declared as a REGISTER.

```
var 
  reg: byte register;  //Stored at register A
  regA: byte registerA; //Stored at register A
  regX: byte registerX; //Stored at register X
  regY: byte registerY; //Stored at register Y
```

A register variable doesn't use RAM as storage. Rather use the specified register.

Register parameters are defined using the following reserved words:

* REGISTER -> Use internal work register defined by the compiler. Can be used for types of one or two bytes.
* REGISTERA -> Use always the register A of the CPU. Can be used only for 1-byte size parameters.
* REGISTERX -> Use always the register X of the CPU. Can be used only for 1-byte size parameters.
* REGISTERY -> Use always the register Y of the CPU. Can be used only for 1-byte size parameters.

Variables declared simply as "Register" can be of size: 1 or 2 bytes. The compiler will decide wich register or RAM position will be used to storage these variables.

As the register variables keep their values in internal registers of the CPU, any operation that affects these registers will also change the content of the register variables.

```
var 
  a: boolean register;
  b: byte;
begin
  a := true;  //Set variable "a".
  b := 0;  //!!! WARNING: variable "a" has changed.
  
end.
```

Register variables can be considered as a high level way to access to CPU registers.

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
  end;
end;
```

ASM blocks are like instructions and they must be finished with ";". 

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
  BIG   = $1234;
begin
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
  end;
  vbit := 1;
```

The address in ORG, can be absolute or relative. 

WARNING: Changing the PC pointer with ORG, can generate errors in the compilation or in execution.

The directive DB allows to define a byte in a specific position of the code:

```
  asm 
    JMP code
tmp_var:
   DB $00  ;temporal variable
code:
  LDA tmp_var
  end;
```

The directive DW allows to define a word (2 bytes) in a specific position of the code.

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
{$SET x = 'I am now a string'}
```

$SET, is not a declaration, but an assignment. First time a variable is assigned, it's created.

Content of a variable, can be shown using instructions like $MSGBOX or $INFO:

{$MSGBOX 'x is:' + x}

Variables defined can also be used as part of source code like macros:

```
  {$SET valor=1+1}
  x := {$valor};   //x := 2;
```

### System Variables

There are some system variables, accessible from the directives language. They are:
 
{$MSGBOX CPU_MODEL} -> Shows the PIC model defined.

{$MSGBOX CPU_FREQUEN} -> Shows the Clock frequency.

{$MSGBOX CPU_MAXFREQ} -> Shows the Max Clock frequency for the device.

{$MSGBOX SYN_MODE} -> Shows the syntax Mode of the compiler.

(*) To see the complete list, check the User Manual.

### List of Directives

The next directives are supported by P65Pas:

#### $FREQUENCY

Specify the clock frequency, in MHz or KHz. Example:

```
{$FREQUENCY 2Mhz}
```

Frequency information is used for:

* The compiler, when needed to generate delays.
* The simulator, for Real Time simulation.

If delays are used in the program, only some frequencies are supported. They are:

1MHz, 2Mhz, 4Mhz, 8MHz, 10MHz.

If frequency is not specified, the default value is 1MHz.

#### $MODE

Specify the syntax mode, used by the compiler. The allowed values are:

{$MODE P65Pas} -> Default mode. Use the new syntax for the control structures.

{$MODE PASCAL} -> Clasic Pascal mode. Use the common Pascal syntax for the control structures.

#### $MSGBOX

Shows a text message in the screen:

{$MSGBOX 'Hello World'} -> Shows the message 'Hello World' in the screen.

{$MSGBOX CPU_MODEL} -> Shows the system variable CPU_MODEL, that is the CPU model defined.

{$MSGBOX CPU_FREQUEN} -> Shows the Clock frequency.

{$MSGBOX 'clock=' + CPU_FREQUEN}  -> Shows the message: "clock=1000000" (if the Frequency was set to 1MHz).

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

Defines the name of the output binary file *.prg.

```
{$OUTPUTHEX "myoutput.prg"}  // Relative path
{$OUTPUTHEX "d:\temp\myoutput.prg"}  //Absolute path
```

When relative path is used, the file will be created in the same folder the Pascal program is.

If it's not defined the name of the \*.prg file, it will be used the name of the program/unit compiled. So if the program is called "myprogram" (and the file is "myprogram.pas"), then the *.prg file will be "myprogram.prg".

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

#### $SET_STATE_RAM

Set the state of the RAM memory for the current CPU device.

The state of a byte of RAM can have 2 values:

* SFR: Special RAM, like Operative System o Kernel locations.
* GPR: General Purpose RAM. Used as free memory for the compiler.
* NIM: Not implemented cell.

$SET_STATE_RAM, let us to define the state of the RAM using a range of addresses.

The syntax of $SET_STATE_RAM is: 

```
{$SET_STATE_RAM <list of commands>}
```

Commands are separated by commas. One command have the syntax:

  \<Begin adrress\> - \<End address\> : \<state\>

One valid example, for this directive, would be:

```
{$SET_STATE_RAM '0100-01FF:SFR'}
```

That indicates the bytes in RAM from $0100 to $01FF are special RAM and cannot be used by the compiler.

Addresses are expressed always in hexadecimal. 

Other example is:

```
//Multiple commands in one directive
{$SET_STATE_RAM '0800-1FFF:GPR, 00F0-00FF:GPR'}  
```

By default all the 64K of RAM is defined a GPR in the virtual memory of the compiler.

IMPORTANT: The IDE maintains the previous state of $SET_STATE_RAM. It's recommended to include $CLEAR_STATE_RAM, always before any directive $SET_STATE_RAM.

WARNING: Defining RAM as NIM or SFR make the compiler don't use that locations for allocating variables, unless they are defined as ABSOLUTE.

#### $CLEAR_STATE_RAM

Used to define the initial state of RAM memory. 

$CLEAR_STATE_RAM, set the state of all the RAM as unimplemented, clearing all previous setting.

It's used before of starting to define the RAM for a device, using the directives $SET_STATE_RAM.

#### $SET_DATA_ADDR

Allow to define a separate block of RAM for placing the variables. This block is called the Primary variables address.

The syntax of $SET_DATA_ADDR is: 

```
{$SET_DATA_ADDR '<Begin adrress>-<End address>'}
```

The directive $SET_DATA_ADDR specify a range of memory where the variables will be located firstly. If this space is not enought, the program block space will be used (like it's done in the normal way).

One valid example, for this directive, would be:

```
{$SET_DATA_ADDR '0070-0075'}
```

Commomly, the compiler locates variables in the same position used for the program code, except for the ABSOLUTE variables. 

If we can define to store the variables (or part of them) in some other location. This is useful for the 6502, considering it works faster when accessing the zero page RAM.

Only one block for variables can be defined. If multiples directives $SET_DATA_ADDR are used, just the last is considered. 

It's recommendable to use $SET_DATA_ADDR at the beginning of the code. However the location is not important, because memory assignment is done after compiling all the program. 

Using $SET_DATA_ADDR with an empty string will disable the effect of a previous $SET_DATA_ADDR directive:

```
{$SET_DATA_ADDR ''}
```

Variables allocated in a separate block, are considered as ABSOLUTE variables, so they cannot be initialized in the declaration.

#### $STRING

Set options for interpreting literal arrays of chars (strings) in the source code.

By default strings are translated literally. 

This means that if a string is defined as in the following example:

```
var
  a: array of char = 'Hola';
```

The string will occupy 4 bytes in memory.

But if we use the following directive:


```
{$STRING NULL_TERMINATED}
```

The string 'Hola' will occupy 5 bytes in memory, because a NULL character will be added at the end.

To disable the directive, and return to the default mode for strings, we use:

```
{$STRING NONE}
```


#### $BOOTLOADER

Defines the initial binary code generated by the compiler. The initial code is generated at the position $0000 or at the position defined by the directive {$ORG ...}

The BOOTLOADER directive can have the following forms:

* {$BOOTLOADER NONE}

In this mode no code is generated at the beginning of the compilated code.

This mode should be used with caution because the entry point of the the compilde code must be known to execute it.

The following code shows how to use this directive.

```
{$BOOTLOADER NONE}
var
  x: byte;
begin
  x := 1;
end. 
```

The ASM code generated is:

```
      ORG $0000
x     $0000 DB 00			   ;variables zone
      $0001 A9 01    LDA #$01  ;entry point for code
      $0003 85 00    STA x 
      END
```
As we can see, the entry point is the address $0001.

* {$BOOTLOADER JMP}

This is the default bootloader created when the directive $BOOTLOADER is not specified.

This option creates a simple JMP instruction at the beginning of the code so we can always run the compiled code fron the beginning. The JMP instruction goes to the entry point of the executable code skipping the possible area for ​​variables or subroutines.

The following code shows how to use this directive.

```
{$BOOTLOADER JMP}
var
  x: byte;
begin
  x := 1;
end. 
```

The ASM code generated is:

```
            ORG $0000
      $0000 4C 04 00 JMP $0004 
x     $0003 DB 00
      $0004 A9 01    LDA #$01 
      $0006 85 03    STA x 
```

As we can see, the code can be executed from the beginning without worrying about the structure of the compiled code.

* {$BOOTLOADER C64}

It's used when compiling for the Commodore64 system.

This directive generates a simple BASIC program to call the compiled code. The BASIC program is a simple line containing: "10 SYS 2062".

This directive must be used in conjunction with the directive {$ORG $0801} in order to the bootloader code to be effective, because BASIC zone start at the address $0801.

The following code shows how to use this directive.

```
{$ORG $0801}
{$BOOTLOADER C64}
var
  x: byte;
begin
  x := 1;
end. 
```

The binary code generated for this program can be loaded and run directly from Commodore 64 BASIC after loading the PRG file.

* {$BOOTLOADER \<content\>} 

This is the more general form to define a bootloader. 

In this form we can define custom bootloaders, specifying the bytes or instructions of the code.

The argument <content> is a list of numbers or strings like: 12, $123, 'JSR', ...

The following example creates a bootloader with 3 bytes:

```
{$BOOTLOADER $0C,$08,$0A}
var
  x: byte;
begin
  x := 1;
end.
```

Bytes can be specified in decimal or hexadecimal.

In addition to numbers, which can be specified directly, there is a set of strings with a special meaning. They are:
'JMP' ->  Encodes to the 6502 JMP absolute instruction (Code $4C).
'COD_HL' -> Generates, in 2 bytes, the address of the entry point for the compiled code.
'COD_5A' -> Generates, in 5 ASCII bytes, the address of the entry point for the compiled code.
'COD_4A' -> Generates, in 4 ASCII bytes, the address of the entry point for the compiled code.

The next directive shows how to define a simple bootloader with a JMP to the origin of the compiled code, like it's generated with {$BOOTLOADER JMP}:

```
{$BOOTLOADER 'JMP','COD_HL'}
```

The next directive shows how to define a bootloader like it's generated with {$BOOTLOADER C64}:

```
{$BOOTLOADER $0C,$08,$0A,$00,$9E,'COD_4A',$00,$00,$00}
```


## Libraries

Currently there are the following units (librearies) in the directory ./comp_p65pas/units:

* Commodore64.pas -> Unit for compiling for a Commodore 64 system.
* Commodore128.pas -> Unit for compiling for a Commodore 128 system.


## P65Pas Limitations

•	Only basic types are implemented: byte, char, boolean, word an dword(limited support).

•	No recursion implemented, Because of the limited hardware resources.

•	No float point implemented.

Some of these limitations must be solved in next versions.

## Development

P65Pas is a free software (GPL license) and it's opened for the collaboration of anyone who is interested. 

There is still, much work for development or documentation, so any help will be appreciated.

## Source Code

The compiler has been developed in Object Pascal and created using the Lazarus IDE https://www.lazarus-ide.org

All the source code is divided in two projects. Both projects can be compiled independently.

* The source code of the IDE is located in the folder /Source.

* The source code of the compiler (Command line) is located in the folder /comp_p65pas/src.

When compiling the IDE, the source code of the compiler is included too so the IDE executable embeds the compiler binary too.

P65Pas has been compiled, using the version 2.0.12 of Lazarus. Tested in Windows, Ubuntu and Mac.

To have more information about the compiler internals and design, check the Technical Documentation (Only in spanish by now): \comp_p65pas\docs\.

To compile P65Pas, it's needed to have the following libraries:

* MiConfig: https://github.com/t-edson/MiConfig
* MisUtils: https://github.com/t-edson/MisUtils
* P65Utils: https://github.com/t-edson/P65Utils
* SynFacilUtils: https://github.com/t-edson/SynFacilUtils
* UtilsGrilla: https://github.com/t-edson/UtilsGrilla
* EpikTimer: https://wiki.freepascal.org/EpikTimer

All of them, are include in the folder /_libraries located in the /Source folder. 

These libraries don't include packages. They are only files in folders that are included when compiling P65Pas.

### External Dependencies

There is not external dependencies. All libraries used are included in the /Source folder.
 
