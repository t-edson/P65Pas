MisUtils 0.6
============

Useful routines for Lazarus, for showing messages and developing multilingual applications.

Inside this unit, we have this functions for showing messages:

* MsgBox() -> Shows a box with a message. Similar a ShowMessage.
* MsgExc() -> Shows a box with a message and an Icon of Exclamation. 
* MsgErr() -> Shows a box with a message and an Icon of Error. 

* MsgYesNo() -> Prompt for a Yes/No answer.
* MsgYesNoCancel() -> Prompt for a Yes/No answer, with a cancel option.

Moreover, this unit includes a diccionary for translating messages. To use it, are  declared the next functions:

* TranslateMsgs -> Flag for to enable the translation of  messages used with MsgBox(), MsgExc() and MsgErr(), MsgYesNo() and MsgYesNoCancel().
* dic() -> Translates one string using the internal dictionary.
* dicSet() -> Adds or replace an entry of the internal dictionary.
* dicDel() -> Cleans an entry of the internal dictionary.
* dicClear() -> Removes all entries of the internal dictionary.

The translation feature, for the source code, works in the following way:

* There is an internal dictionary (called "dictionary") defined in the Unit "MisUtils". It is a public field, created in the "Initialization" section.
* This dictionary contain several entries of the form (key -> value).
* The "key" field of the dictionary is a string with a text in the language that the program is codified (like: cad :=  dic('hello'); ).
* The "value" field of the dictionary is a string with a text that is the translation of the "key" text ( "hello" -> "hola").
* Thus, the dictionary contains the translation of all strings of the source (using the dictionary) for an unique language.
* Every time, it's needed to change the language, the dictionary must be filled with new "value" strings for the new language.

So, to have the translation working, the dictionary must be first filled.

How to fill the dictionary?
It's done using dicSet(). The recommended way is to use a procedure like this:

```
procedure TForm1.SetLanguage(lang: string);
begin
  case lang of
  'en': begin
    dicClear;  //if it's yet in English
  end;
  'es': begin
    dicSet('Hello','Hola');
    dicSet('Bye','Adios');
  end;
  ...
  end;
end;
```
The text used in the first parameter of dicSet(), must correspond exactly to the text used on the other parts of the source code.

After filled the dictionary, how do I put the text in my code?

The formal way is to use dic(), for every string that need to be translated. So if you have in your code:

```
   varStr := 'Hello';   
```

then you would need to change it, for:

```
   varStr := dic('Hello');  //we asume that we already have translated 'Hello' with dicSet().
```

dic() can work similar to Format() too, because have an overloaded version.

To show messages, it's possible to use the special functions of "MisUtils" like MsgBox() or MsgExc() that perform the translation without using dic().

For more information, check the sample codes.