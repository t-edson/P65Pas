{
Implementación del Generador de Código del compilador.
Esta implementación no permitirá recursividad, por las limitaciones de recursos de los
dispositivos más pequeños.
El compilador está orientado a uso de registros (solo hay uno) y memoria RAM. No se
manejan estructuras en pila.
Solo se manejan datos de tipo boolean, byte y word, y operaciones sencillas.
}
{La arquitectura definida aquí contempla:

Un registro de trabajo A, de 8 bits (el acumulador del PIC).
Dos registros auxiliares X e Y.
Tres registros de trabajo adicionales  U,E y H de 8 bits cada uno (Creados a demanda).

La forma de trabajo por tipos es:

TIPO BOOLEAN:
* Se almacenan en un byte. Cualquier valor diferente de cero se considera TRUE.
* Los resultados se devuelven en el bit Z, del registro SR.
TIPO CHAR Y BYTE:
* Se almacenan en un byte.
* Los resultados se devuelven en el registro acumulador A
TIPO WORD:
* Se almacenan en un 2 bytes.
* Los resultados se devuelven en los registros (H,A).

Opcionalmente, si estos registros ya están ocupados, se guardan primero en la pila, o se
usan otros registros auxiliares.

Despues de ejecutar alguna operación booleana que devuelva una expresión, se
actualizan las banderas: BooleanBit y BooleanInverted, que implican que:
* Si BooleanInverted es TRUE, significa que la lógica de C o Z está invertida.
* La bandera BooleanBit, indica si el resultado se deja en C o Z.

Por normas de Xpres, se debe considerar que:
* Todas las ROB reciben sus dos parámetros en las variables p1^ y p2^.
* El resultado de cualquier expresión se debe dejar indicado en el objeto "res".

Si el objeto "res" es constante, almacena directamente sus valores en:
* "valInt" para tipos enteros y enteros sin signo.
* "valBool" para el tipo booleano.
* "valStr" para el tipo string.

Las rutinas de operación, deben devolver su resultado en "res".
Para mayor información, consultar la doc. técnica.
 }
unit GenCod_PIC16;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Graphics, LCLType, LCLProc,
  SynFacilBasic, P6502utils, CPUCore, GenCodBas_PIC16,
  CompBase, Globales, XpresElemP65, LexPas, MisUtils;
type
    { TGenCod }
    TGenCod = class(TGenCodBas)
    private
//      f_byteXbyte_byte: TxpEleFun;  //índice para función
      f_byte_mul_byte_16: TEleFun;
      f_word_shift_l: TEleFun;
      procedure AddParam(var pars: TxpParFuncArray; parName: string;
        const srcPos: TSrcPos; typ0: TEleTypeDec; adicDec: TxpAdicDeclar);
      procedure arrayHigh(fun: TEleExpress);
      procedure arrayLength(fun: TEleExpress);
      procedure arrayLow(fun: TEleExpress);
      procedure byte_mul_byte_16(fun: TEleExpress);
      procedure Copy_Z_to_A;
      procedure Copy_C_to_A;
      function CreateBOMethod(clsType: TEleTypeDec; opr: string; name: string;
        parType: TEleTypeDec; retType: TEleTypeDec; pCompile: TxpCodInline
  ): TEleFun;
      function CreateUOMethod(clsType: TEleTypeDec; opr: string; name: string;
        retType: TEleTypeDec; pCompile: TxpCodInline; operTyp: TOperatorType =
  opkUnaryPre): TEleFun;
      function CreateSystemFunction(name: string; retType: TEleTypeDec;
        const srcPos: TSrcPos; const pars: TxpParFuncArray;
        pCompile: TxpCodInline): TEleFun;
      procedure DefineArray(etyp: TEleTypeDec);
      procedure DefinePointer(etyp: TEleTypeDec);
      procedure DefineShortPointer(etyp: TEleTypeDec);
      procedure fun_Addr(fun: TEleExpress);
      procedure fun_Byte(fun: TEleExpress);
//      procedure DefineArray(etyp: TxpEleType);
      procedure LoadIXmult(const idx: TEleVarDec; size: byte; offset: word);
      procedure ROB_arr_asig_arr(fun: TEleExpress);
      procedure ROB_byte_add_word(fun: TEleExpress);
      procedure ROB_word_aadd_byte(fun: TEleExpress);
      procedure ROB_word_aadd_word(fun: TEleExpress);
      procedure ROB_word_and_word(fun: TEleExpress);
      procedure ROB_word_asub_byte(fun: TEleExpress);
      procedure ROB_word_gequ_word(fun: TEleExpress);
      procedure ROB_word_great_word(fun: TEleExpress);
      procedure ROB_word_lequ_word(fun: TEleExpress);
      procedure ROB_word_less_word(fun: TEleExpress);
      procedure ROB_word_shl_byte(fun: TEleExpress);
      procedure ROB_word_shr_byte(fun: TEleExpress);
      procedure ROU_not_word(fun: TEleExpress);
      procedure ValidRAMaddr(addr: integer);
      procedure GenCodArrayGetItem(fun: TEleExpress);
      procedure GenCodArraySetItem(const OpPtr: pointer);
      procedure GenCodArrayClear(fun: TEleExpress);
      procedure ROB_bool_and_bool(fun: TEleExpress);
      procedure ROB_bool_difer_bool(fun: TEleExpress);
      procedure ROB_bool_equal_bool(fun: TEleExpress);
      procedure ROB_byte_mul_byte(fun: TEleExpress);
      procedure ROB_pointer_add_word(fun: TEleExpress);
      procedure ROB_pointer_sub_word(fun: TEleExpress);
//      procedure ROB_string_add_char(Opt: TxpOperation; SetRes: boolean);
//      procedure ROB_string_add_string(fun: TxpEleExpress);
      procedure ROB_word_add_byte(fun: TEleExpress);
      procedure ROB_word_add_word(fun: TEleExpress);
      procedure ROB_word_sub_byte(fun: TEleExpress);
      procedure ROB_word_sub_word(fun: TEleExpress);
      procedure ROU_not_bool(fun: TEleExpress);
      procedure ROU_not_byte(fun: TEleExpress);
      procedure ROU_address(fun: TEleExpress);

      procedure ROB_word_and_byte(fun: TEleExpress);
      procedure word_shift_l(fun: TEleExpress);
    protected //Boolean oeprations
      procedure ROB_bool_asig_bool(fun: TEleExpress);

    protected //Operaciones con byte
      procedure ROB_byte_asig_byte(fun: TEleExpress);
      procedure ROB_byte_aadd_byte(fun: TEleExpress);
      procedure ROB_byte_asub_byte(fun: TEleExpress);
      procedure ROB_byte_sub_byte(fun: TEleExpress);
      procedure ROB_byte_add_byte(fun: TEleExpress);
      procedure ROB_byte_and_byte(fun: TEleExpress);
      procedure ROB_byte_or_byte(fun: TEleExpress);
      procedure ROB_byte_xor_byte(fun: TEleExpress);
      procedure ROB_byte_equal_byte(fun: TEleExpress);
      procedure ROB_byte_difer_byte(fun: TEleExpress);
      procedure ROB_byte_great_byte(fun: TEleExpress);
      procedure ROB_byte_less_byte(fun: TEleExpress);
      procedure ROB_byte_gequ_byte(fun: TEleExpress);
      procedure ROB_byte_lequ_byte(fun: TEleExpress);
      procedure ROB_byte_shr_byte(fun: TEleExpress);
      procedure ROB_byte_shl_byte(fun: TEleExpress);
    private  //Operaciones con Word
      procedure ROB_word_asig_word(fun: TEleExpress);
      procedure ROB_word_asig_byte(fun: TEleExpress);
      procedure ROB_word_equal_word(fun: TEleExpress);
      procedure ROB_word_difer_word(fun: TEleExpress);
    private  //Operaciones con Char
      procedure ROB_char_asig_char(fun: TEleExpress);
      procedure ROB_char_asig_string(fun: TEleExpress);
      procedure ROB_char_equal_char(fun: TEleExpress);
      procedure ROB_char_difer_char(fun: TEleExpress);
    protected //Operaciones con punteros
      procedure ROB_pointer_add_byte(fun: TEleExpress);
      procedure ROB_pointer_sub_byte(fun: TEleExpress);
      procedure ROU_derefPointer(fun: TEleExpress; SetRes: boolean);
    private  //Funciones internas.
      procedure codif_1mseg;
      procedure codif_delay_ms(fun: TEleExpress);
      procedure expr_end(posExpres: TPosExpres);
      procedure expr_start;
      procedure fun_delay_ms(fun: TEleExpress);
      procedure fun_Exit(fun: TEleExpress);
      procedure fun_Inc(fun: TEleExpress);
      procedure fun_Dec(fun: TEleExpress);
      procedure SetOrig(fun: TEleExpress);
      procedure fun_Ord(fun: TEleExpress);
      procedure fun_Chr(fun: TEleExpress);
      procedure fun_Word(fun: TEleExpress);
    protected
      procedure Cod_StartProgram;
      procedure Cod_EndProgram;
      procedure CreateSystemElements;
    public
      procedure DefCompiler;
//      procedure DefinePointer(etyp: TxpEleType);
    end;

  procedure SetLanguage;
implementation
var
  MSG_NOT_IMPLEM, MSG_INVAL_PARTYP, MSG_UNSUPPORTED : string;
  MSG_CANNOT_COMPL, ER_INV_MEMADDR, ER_INV_MAD_DEV: string;

procedure SetLanguage;
begin
  GenCodBas_PIC16.SetLanguage;
  {$I ..\_language\tra_GenCod.pas}
end;
procedure TGenCod.Copy_Z_to_A;
{Copy the logic value of Z flag to A register (as boolean expression).}
begin
  //Result in Z. Move to A.
  BooleanFromZ := _PC;  //Activates flag, and get current address.
  _PHP;
  _PLA;
  _ANDi($02);   //If true get $02
end;
procedure TGenCod.Copy_C_to_A;
{Copy the logic value of C flag to A register (as boolean expression).}
begin
  //Result in C. Move to A.
  BooleanFromC := _PC;  //Activates flag, and get current address.
  _PHP;
  _PLA;
  _ANDi($01);
  _ASLa;  //Leaves in bit 1.
end;
////////////rutinas obligatorias
procedure TGenCod.Cod_StartProgram;
//Codifica la parte inicial del programa
begin
  //Code('.CODE');   //inicia la sección de código
end;
procedure TGenCod.Cod_EndProgram;
//Codifica la parte final del programa
begin
  //Code('END');   //inicia la sección de código
end;
procedure TGenCod.expr_start;
//Se ejecuta siempre al iniciar el procesamiento de una expresión.
begin
  //Inicia banderas de estado para empezar a calcular una expresión
  A.used := false;        //Su ciclo de vida es de instrucción
  //Guarda información de ubicación, en la ubicación actual
  pic.addPosInformation(curCtx.row, curCtx.col, curCtx.idCtx);
end;
procedure TGenCod.expr_end(posExpres: TPosExpres);
//Se ejecuta al final de una expresión, si es que no ha habido error.
begin
  if exprLevel = 1 then begin  //el último nivel
//    Code('  ;fin expres');
  end;
  //Muestra informa
end;
procedure TGenCod.ROU_not_byte(fun: TEleExpress);
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  case par.Sto of
  stConst : begin
    SetFunConst_byte(fun, (not par.value.valInt) and $FF);
  end;
  stRamFix: begin
    SetFunExpres(fun);
    _LDA(par.rvar.addr);
    _EORi($FF);
  end;
//  stExpres: begin
//    SetROUResultExpres_byte;
//    //////
//  end;
  else
    genError('Not implemented: "%s"', [fun.name]);
  end;
end;
procedure TGenCod.ROU_address(fun: TEleExpress);
{Return the address of any operand.}
var
  startAddr: integer;
begin
  case fun.Sto of
  stConst : begin
    if fun.Typ.catType = tctArray then begin
      //We allow to get the address for constant arrays, storing first in RAM.
      CreateValueInCode(fun.Typ, fun.Value, startAddr);
      SetFunConst(fun);
      fun.value.ValInt := startAddr;
    end else begin
      genError('Cannot obtain address of constant.');
    end;
  end;
  stRamFix: begin
    //Es una variable normal
    //La dirección de una variable es constante
    SetFunConst(fun);
    fun.value.valInt := fun.rVar.addr;
  end;
  stRegister: begin
    genError('Cannot obtain address of an expression.');
  end;
  else
    genError('Cannot obtain address of this operand.');
  end;
end;
procedure TGenCod.ROU_not_bool(fun: TEleExpress);
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  case par.Sto of
  stConst : begin
    //NOT for a constant is defined easily
    SetFunConst_bool(fun, not par.value.valBool);
  end;
  stRamFix: begin
    SetFunExpres(fun, logNormal);
    //We have to return logical value inverted in A
    _LDA(par.rVar.addr);   //Result of "NOT var" in Z
    Copy_Z_to_A;
  end;
//  stExpres: begin
//    SetROUResultExpres_byte;
//    //////
//  end;
  else
    genError('Not implemented: "%s"', [fun.name]);
  end;
end;
////////////Byte operations
procedure TGenCod.ROB_byte_asig_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
  parBsto: TStorage;
begin
  SetFunNull(fun);  //In Pascal an assigment doesn't return type.
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Simplify parB
  parBsto := parB.Sto;  //Save storage
  if parB.Sto = stRamVarOf then begin
     LoadToWR(parB);  //Could require IX
     if HayError then exit;
     parB.Sto := stRegister;
  end;
  if ModeRequire then begin
     {We can exit here because LoadToWR() defined is IX is used or not.
     and the remain code doesn't need more registers or temporal variables.}
    parB.Sto := parBsto;  //Restore storage just in case it has changed.
    exit;
  end;
  //Validates parA.
  if parA.opType<>otVariab then begin //The only valid type.
    GenError('Only variables can be assigned.');
    exit;
  end;
  //Implements assignment
  if parA.Sto = stRamFix then begin
    //Assignment to a common variable (constant Address)
    case parB.Sto of
    stConst: begin
      _LDAi(parB.val);
      _STA(parA.add);
    end;
    stRamFix: begin
      _LDA(parB.add);
      _STA(parA.add);
    end;
    stRegister, stRegistA: begin  //Already in A
      _STA(parA.add);
    end;
    stRegistX: begin
      _STX(parA.add);
    end;
    stRegistY: begin
      _STY(parA.add);
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto in [stRegistA, stRegister] then begin
    //Assignment to register A
    case parB.Sto of
    stConst : begin
      _LDAi(parB.val);
    end;
    stRamFix: begin
      _LDA(parB.add);
    end;
    stRegister, stRegistA: begin  //Already in A
    end;
    stRegistX: begin
      _TXA;
    end;
    stRegistY: begin
      _TYA;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto = stRegistX then begin
    //Assignment to register X
    case parB.Sto of
    stConst : begin
      _LDXi(parB.val);
    end;
    stRamFix: begin
      _LDX(parB.add);
    end;
    stRegister, stRegistA: begin  //Already in A
      _TAX;
    end;
    stRegistX: begin  //Already in X
    end;
    stRegistY: begin
      _TYA;  //Modify A
      _TAX;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto = stRegistY then begin
    //Assignment to register Y
    case parB.Sto of
    stConst : begin
      _LDYi(parB.val);
    end;
    stRamFix: begin
      _LDY(parB.add);
    end;
    stRegister, stRegistA: begin  //Already in A
      _TAY;
    end;
    stRegistX: begin
      _TXA;  //Modify A
      _TAY;
    end;
    stRegistY: begin //Already in Y
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto = stRamVarOf then begin
    //Assignment to a variable indexed
    case parB.Sto of
    stConst : begin
      _LDAi(parB.val);
      _LDX(parA.rvar.addr);  //Load address
      if parA.offs<256 then begin
        pic.codAsm(i_STA, aZeroPagX, parA.offs);
      end else begin
        pic.codAsm(i_STA, aAbsolutX, parA.offs);
      end;
    end;
    stRamFix: begin
      _LDA(parB.add);
      _LDX(parA.rvar.addr);  //Load address
      if parA.offs<256 then begin
        pic.codAsm(i_STA, aZeroPagX, parA.offs);
      end else begin
        pic.codAsm(i_STA, aAbsolutX, parA.offs);
      end;
    end;
    stRegister, stRegistA: begin  //Already in A
      _LDX(parA.rvar.addr);  //Load address
      if parA.offs<256 then begin
        pic.codAsm(i_STA, aZeroPagX, parA.offs);
      end else begin
        pic.codAsm(i_STA, aAbsolutX, parA.offs);
      end;
    end;
//    stRegistX: begin
//      _TXA;  //Modify A
//      _TAY;
//    end;
//    stRegistY: begin //Already in Y
//    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;

end;
procedure TGenCod.ROB_byte_and_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const: begin  //suma de dos constantes. Caso especial
    SetFunConst_byte(fun, parA.val and parB.val);  //puede generar error
  end;
  stConst_Variab: begin
    if parA.val = 0 then begin  //Caso especial
      SetFunConst_byte(fun, 0);  //puede generar error
      exit;
    end else if parA.val = 255 then begin  //Caso especial
      SetFunVariab(fun, parB.rVar);  //puede generar error
      exit;
    end;
    SetFunExpres(fun);
    _LDA(parB.add);
    _ANDi(parA.val);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    if parA.val = 0 then begin  //Caso especial
      SetFunConst_byte(fun, 0);  //puede generar error
      exit;
    end else if parA.val = 255 then begin  //Caso especial
      SetFunExpres(fun);  //No es necesario hacer nada. Ya está en A
      exit;
    end;
    SetFunExpres(fun);
    _ANDi(parA.val);
  end;
  stVariab_Const: begin
    if parB.val = 0 then begin  //Caso especial
      SetFunConst_byte(fun, 0);  //puede generar error
      exit;
    end else if parB.val = 255 then begin  //Caso especial
      SetFunVariab(fun, parA.rVar);  //puede generar error
      exit;
    end;
    SetFunExpres(fun);
    _LDAi(parB.val);
    _AND(parA.add);
  end;
  stVariab_Variab:begin
    SetFunExpres(fun);
    _LDA(parB.add);
    _AND(parA.add);   //leave in A
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _AND(parA.add);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    if parB.val = 0 then begin  //Caso especial
      SetFunConst_byte(fun, 0);  //puede generar error
      exit;
    end else if parA.val = 255 then begin  //Caso especial
      SetFunExpres(fun);  //No es necesario hacer nada. Ya está en A
      exit;
    end;
    SetFunExpres(fun);
    _ANDi(parB.val)
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _AND(parB.add);
  end;
  stExpres_Expres:begin
    SetFunExpres(fun);
    //p1 está en la pila y p2 en el acumulador
//    rVar := GetVarByteFromStk;
    _TAX;  //Save A
    _PLA;
    _STA(H.addr);  //Use H a temp variable.
    _TXA;  //Restore A
    _AND(H.addr);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_byte_or_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const: begin  //suma de dos constantes. Caso especial
    SetFunConst_byte(fun, parA.val or parB.val);  //puede generar error
  end;
  stConst_Variab: begin
    if parA.val = 0 then begin  //Caso especial
      SetFunVariab(fun, parB.rVar);
      exit;
    end else if parA.val = 255 then begin  //Caso especial
      SetFunConst_byte(fun, 255);
      exit;
    end;
    SetFunExpres(fun);
    _LDAi(parA.val);
    _ORA(parB.add);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    if parA.val = 0 then begin  //Caso especial
      SetFunExpres(fun);  //No es necesario hacer nada. Ya está en A
      exit;
    end else if parA.val = 255 then begin  //Caso especial
      SetFunConst_byte(fun, 255);
      exit;
    end;
    SetFunExpres(fun);
    _ORA(parA.val);
  end;
  stVariab_Const: begin
    if parB.val = 0 then begin  //Caso especial
      SetFunVariab(fun, parA.rVar);
      exit;
    end else if parA.val = 255 then begin  //Caso especial
      SetFunConst_byte(fun, 255);
      exit;
    end;
    SetFunExpres(fun);
    _LDAi(parB.val);
    _ORA(parA.add);
  end;
  stVariab_Variab:begin
    SetFunExpres(fun);
    _LDA(parA.add);
    _ORA(parB.add);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _ORA(parA.add);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    if parB.val = 0 then begin  //Caso especial
      SetFunExpres(fun);  //No es necesario hacer nada. Ya está en A
      exit;
    end else if parB.val = 255 then begin  //Caso especial
      SetFunConst_byte(fun, 255);
      exit;
    end;
    SetFunExpres(fun);
    _ORA(parB.val);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _ORA(parB.add);
  end;
//  stExpres_Expres:begin
//    SetResultExpres(fun);
//    //p1 está en la pila y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _ORA(rVar.adrByte0);
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_byte_xor_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const: begin  //suma de dos constantes. Caso especial
    SetFunConst_byte(fun, parA.val xor parB.val);  //puede generar error
  end;
  stConst_Variab: begin
    SetFunExpres(fun);
    _LDAi(parA.val);
    _EOR(parB.add)
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _EORi(parA.val);  //leave in A
  end;
  stVariab_Const: begin
    SetFunExpres(fun);
    _LDA(parA.add);   //leave in A
    _EORi(parB.val);
  end;
  stVariab_Variab:begin
    SetFunExpres(fun);
    _LDA(parA.add);   //leave in A
    _EOR(parB.add);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _EOR(parA.add);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _EORi(parB.val);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _EOR(parB.add);
  end;
//  stExpres_Expres:begin
//    SetResultExpres(fun);
//    //p1 está en la pila y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _EOR(rVar.adrByte0);
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_byte_equal_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const: begin  //compara constantes. Caso especial
    SetFunConst_bool(fun, parA.val = parB.val);
  end;
  stConst_Variab: begin
    SetFunExpres(fun, logNormal);   //Se pide Z para el resultado
    if parA.val = 0 then begin  //caso especial
      _LDA(parB.add);
    end else begin
      _LDA(parB.add);
      _CMPi(parA.val);
    end;
    Copy_Z_to_A;
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    if not AcumStatInZ then _TAX;   //Update Z, if needed.
    if parA.val = 0 then begin  //caso especial
      //Nothing
    end else begin
      _CMPi(parA.val);
    end;
    Copy_Z_to_A;
  end;
  stVariab_Const: begin
    SetFunExpres(fun, logNormal);   //Se pide Z para el resultado
    if parB.val = 0 then begin  //caso especial
      _LDA(parA.add);
    end else begin
      _LDA(parA.add);
      _CMPi(parB.val);
    end;
    Copy_Z_to_A;
  end;
  stVariab_Variab:begin
    SetFunExpres(fun, logNormal);   //Se pide Z para el resultado
    _LDA(parB.add);
    _CMP(parA.add);
    Copy_Z_to_A;
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    _CMP(parA.add);
    Copy_Z_to_A;
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    if not AcumStatInZ then _TAX;   //Update Z, if needed.
    if parB.val = 0 then begin  //caso especial
      //Nothing
    end else begin
      _CMPi(parB.val);
    end;
    Copy_Z_to_A;
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    _CMP(parB.add);
    Copy_Z_to_A;
  end;
//  stExpres_Expres:begin
//    SetROBResultExpres_bool(fun, logNormal);   //Se pide Z para el resultado
//    //la expresión p1 debe estar salvada y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _EOR(rVar.adrByte0);  //Si son iguales Z=1.
//    FreeStkRegisterByte;   //libera pila porque se usará el dato ahí contenido
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_byte_difer_byte(fun: TEleExpress);
begin
  ROB_byte_equal_byte(fun);  //usa el mismo código
  fun.Invert;  //Invierte la lógica
end;
procedure TGenCod.ROB_byte_aadd_byte(fun: TEleExpress);
{Operación de asignación suma: +=}
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Special assigment
  if parA.Sto = stRamFix then begin
    SetFunNull(fun);  //Fomalmente,  una aisgnación no devuelve valores en Pascal
    //Asignación a una variable
    case parB.Sto of
    stConst : begin
      if parB.val=0 then begin
        //Caso especial. No hace nada
      end else if parB.val=1 then begin
        //Caso especial.
        _INC(parA.add);
      end else if parB.val=2 then begin
        //Caso especial.
        _INC(parA.add);
        _INC(parA.add);
      end else begin
        _CLC;
        _LDA(parA.add);
        _ADCi(parB.val);
        _STA(parA.add);
      end;
    end;
    stRamFix: begin
      _LDA(parA.add);
      _CLC;
      _ADC(parB.add);
      _STA(parA.add);
    end;
    stRegister: begin  //ya está en A
      _CLC;
      _ADC(parA.add);
      _STA(parA.add);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else if parA.Sto = stRegister then begin
//    {Este es un caso especial de asignación a un puntero a byte dereferenciado, pero
//    cuando el valor del puntero es una expresión. Algo así como (ptr + 1)^}
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case p2^.Sto of
//    stConst : begin
//      //Asignación normal
//      if parB.val=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        kMOVWF(FSR);  //direcciona
//        _ADDWF(0, toF);
//      end;
//    end;
//    stVariab: begin
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(parB.add, toW);
//      _ADDWF(0, toF);
//    end;
//    stExpres: begin
//      //La dirección está en la pila y la expresión en A
//      aux := GetAuxRegisterByte;
//      kMOVWF(aux);   //Salva A (p2)
//      //Apunta con p1
//      rVar := GetVarByteFromStk;
//      kMOVF(rVar.adrByte0, toW);  //opera directamente al dato que había en la pila. Deja en A
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(aux, toW);
//      _ADDWF(0, toF);
//      aux.used := false;
//      exit;
//    end;
//    else
//      GenError(MSG_UNSUPPORTED); exit;
//    end;
//  end else if parA.Sto = stVarRef then begin
//    //Asignación a una variable
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case p2^.Sto of
//    stConst : begin
//      //Asignación normal
//      if parB.val=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        //Caso especial de asignación a puntero dereferenciado: variable^
//        kMOVF(parA.add, toW);
//        kMOVWF(FSR);  //direcciona
//        _ADDWF(0, toF);
//      end;
//    end;
//    stVariab: begin
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      kMOVF(parA.add, toW);
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(parB.add, toW);
//      _ADDWF(0, toF);
//    end;
//    stExpres: begin  //ya está en A
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      aux := GetAuxRegisterByte;
//      kMOVWF(aux);   //Salva A (p2)
//      //Apunta con p1
//      kMOVF(parA.add, toW);
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(aux, toW);
//      _ADDWF(0, toF);
//      aux.used := false;
//    end;
//    else
//      GenError(MSG_UNSUPPORTED); exit;
//    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.ROB_byte_asub_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Caso especial de asignación
  if parA.Sto = stRamFix then begin
    SetFunNull(fun);  //Fomalmente,  una aisgnación no devuelve valores en Pascal
    //Asignación a una variable
    case parB.Sto of
    stConst : begin
      if parB.val=0 then begin
        //Caso especial. No hace nada
      end else if parB.val=1 then begin
        //Caso especial.
        _DEC(parA.add);
      end else if parB.val=2 then begin
        //Caso especial.
        _DEC(parA.add);
        _DEC(parA.add);
      end else begin
        _SEC;
        _LDA(parA.add);
        _SBCi(parB.val);
        _STA(parA.add);
      end;
    end;
    stRamFix: begin
      _SEC;
      _LDA(parA.add);
      _SBC(parB.add);
      _STA(parA.add);
    end;
    stRegister: begin  //ya está en A
      _SEC;
      _SBC(parA.add);   //a - p1 -> a
      //Invierte
      _EORi($ff);
      _CLC;
      _ADCi(1);
      //Devuelve
      _STA(parA.add);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
//  end else if parA.Sto = stExpRef then begin
//    {Este es un caso especial de asignación a un puntero a byte dereferenciado, pero
//    cuando el valor del puntero es una expresión. Algo así como (ptr + 1)^}
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case p2^.Sto of
//    stConst : begin
//      //Asignación normal
//      if parB.val=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        kMOVWF(FSR);  //direcciona
//        _SUBWF(0, toF);
//      end;
//    end;
//    stRamFix: begin
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(parB.add, toW);
//      _SUBWF(0, toF);
//    end;
//    stRegister: begin
//      //La dirección está en la pila y la expresión en A
//      aux := GetAuxRegisterByte;
//      kMOVWF(aux);   //Salva A (p2)
//      //Apunta con p1
//      rVar := GetVarByteFromStk;
//      kMOVF(rVar.adrByte0, toW);  //opera directamente al dato que había en la pila. Deja en A
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(aux, toW);
//      _SUBWF(0, toF);
//      aux.used := false;
//      exit;
//    end;
//    else
//      GenError(MSG_UNSUPPORTED); exit;
//    end;
//  end else if parA.Sto = stVarRef then begin
//    //Asignación a una variable
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case parB.Sto of
//    stConst : begin
//      //Asignación normal
//      if parB.val=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        //Caso especial de asignación a puntero dereferenciado: variable^
//        kMOVF(parA.add, toW);
//        kMOVWF(FSR);  //direcciona
//        _SUBWF(0, toF);
//      end;
//    end;
//    stRamFix: begin
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      kMOVF(parA.add, toW);
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(parB.add, toW);
//      _SUBWF(0, toF);
//    end;
//    stRegister: begin  //ya está en A
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      aux := GetAuxRegisterByte;
//      kMOVWF(aux);   //Salva A (p2)
//      //Apunta con p1
//      kMOVF(parA.add, toW);
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(aux, toW);
//      _SUBWF(0, toF);
//      aux.used := false;
//    end;
//    else
//      GenError(MSG_UNSUPPORTED); exit;
//    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.ROB_byte_add_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
  stoo: TStoOperandsROB;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  stoo := stoOperation(parA, parB);
  case stoo of
  stConst_Const: begin
    SetFunConst_byte(fun, parA.val+parB.val);  //puede generar error
  end;
  stConst_Variab, stVariab_Const: begin
    if stoo = stVariab_Const then Exchange(parA, parB);
    if parA.val = 0 then begin
      //Caso especial
      SetFunVariab(fun, parB.rVar);  //devuelve la misma variable
      exit;
    end else if parA.val = 1 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDX(parB.add);
      _INX;
      _TXA;
      LastIsTXA := true;  //Set flag
      exit;
    end;
    SetFunExpres(fun);
    _CLC;
    _LDAi(parA.val);
    _ADC(parB.add);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _CLC;
    _ADCi(parA.val);
  end;
  stVariab_Variab:begin
    SetFunExpres(fun);
    _CLC;
    _LDA(parA.add);
    _ADC(parB.add);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _CLC;
    _ADC(parA.add);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _CLC;
    _ADCi(parB.val);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _CLC;
    _ADC(parB.add);
  end;
//  stExpres_Expres:begin
//    SetResultExpres(fun);
//    //La expresión p1 debe estar salvada y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _CLC;
//    _ADC(rVar.adrByte0);  //opera directamente al dato que había en la pila. Deja en A
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_byte_add_word(fun: TEleExpress);
begin
  fun.elements.Exchange(0,1);  //Convert to word_add_byte
  ROB_word_add_byte(fun);
  fun.elements.Exchange(0,1);
end;
procedure TGenCod.ROB_byte_sub_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const:begin  //suma de dos constantes. Caso especial
    SetFunConst_byte(fun, parA.val-parB.val);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  stConst_Variab: begin
    SetFunExpres(fun);
    _SEC;
    _LDAi(parA.val);
    _SBC(parB.add);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    requireH;
    _STA(H.addr);
    _SEC;
    _LDAi(parA.val);
    _SBC(H.addr);
  end;
  stVariab_Const: begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.add);
    _SBCi(parB.val);
  end;
  stVariab_Variab:begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.add);
    _SBC(parB.add);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _SEC;
    _SBC(parA.add);   //a - p1 -> a
    //Invierte
    _EORi($FF);
    _CLC;
    _ADCi(1);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _SEC;
    _SBCi(parB.val);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _SEC;
    _SBC(parB.add);
  end;
//  stExpres_Expres:begin
//    SetResultExpres(fun);
//    //la expresión p1 debe estar salvada y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _SEC;
//    _SBC(rVar.adrByte0);
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.byte_mul_byte_16(fun: TEleExpress);
//Routine to multiply 8 bits X 8 bits
//pasA * parB -> [H:A]  Usa registros: A,H,E,U
//Based on https://codebase64.org/doku.php?id=base:short_8bit_multiplication_16bit_product
var
  m0, m1: integer;
  fac1,  fac2: TEleVarDec;
begin
    fac1 := fun.rfun.pars[0].pvar;
    fac2 := fun.rfun.pars[1].pvar;
    requireH;
    //A*256 + X = FAC1 * FAC2
    _ldai($00);
    _ldxi($08);
    _clc;
_LABEL_pre(m0);
    _BCC_post(m1);
    _clc;
    _adc(fac2.addr);
_LABEL_post(m1);
    _RORa;
    _ror(fac1.addr);
    _dex;
    _BPL_pre(m0);
    //Returns in H,A
    _STA(H.addr);
    _LDA(fac1.addr);
    _RTS();
end;
procedure TGenCod.ROB_byte_mul_byte(fun: TEleExpress);
var
  AddrUndef: boolean;
  fmul: TEleFun;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  fmul := f_byte_mul_byte_16;
  case stoOperation(parA, parB) of
  stConst_Const: begin
    SetFunConst_word(fun, parA.val*parB.val);  //puede generar error
  end;
  stConst_Variab: begin
    if parA.val = 0 then begin
      //Caso especial
      SetFunConst_word(fun, 0);  //devuelve la misma variable
      exit;
    end else if parA.val = 1 then begin
      //Caso especial
      SetFunVariab(fun, parB.rVar);  //devuelve la misma variable
      exit;
    end else if parA.val = 2 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);
      _LDA(parB.add);
      _ASLa;
      _ROL(H.addr);
      exit;
    end else if parA.val = 4 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);
      _LDA(parB.add);
      _ASLa;
      _ROL(H.addr);
      _ASLa;
      _ROL(H.addr);
      exit;
    end else if parA.val = 8 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      _LDA(parB.add);
      //Loop
      _LDXi(3);  //Counter
      AddCallerToFromCurr(f_word_shift_l);  //Declare use
      functCall(nil, f_word_shift_l, AddrUndef);  //Use
      exit;
    end else if parA.val = 16 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      _LDA(parB.add);
      //Loop
      _LDXi(4);  //Counter
      AddCallerToFromCurr(f_word_shift_l);  //Declare use
      functCall(nil, f_word_shift_l, AddrUndef);  //Use
      exit;
    end else if parA.val = 32 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      _LDA(parB.add);
      //Loop
      _LDXi(5);  //Counter
      AddCallerToFromCurr(f_word_shift_l);  //Declare use
      functCall(nil, f_word_shift_l, AddrUndef);  //Use
      exit;
    end;
    //General case
    SetFunExpres(fun);
    _LDAi(parA.val);
    _STA(fmul.pars[0].pvar.addr);
    _LDA(parB.add);
    _STA(fmul.pars[1].pvar.addr);
    AddCallerToFromCurr(fmul);  //Declare use
    AddCallerToFromCurr(fmul.pars[0].pvar);  //Declare use
    AddCallerToFromCurr(fmul.pars[1].pvar);  //Declare use
    functCall(nil, fmul, AddrUndef);   //Code the "JSR"
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    //Es casi el mismo código de stConst_Variab
    if parA.val = 0 then begin
      //Caso especial
      SetFunConst_word(fun, 0);  //devuelve la misma variable
      exit;
    end else if parA.val = 1 then begin
      //Caso especial
      SetFunExpres(fun);  //devuelve la misma variable
      exit;
    end else if parA.val = 2 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);
      //_LDA(parB.add);
      _ASLa;
      _ROL(H.addr);
      exit;
    end else if parA.val = 4 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);
      //_LDA(parB.add);
      _ASLa;
      _ROL(H.addr);
      _ASLa;
      _ROL(H.addr);
      exit;
    end else if parA.val = 8 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      //_LDA(parB.add);
      //Loop
      _LDXi(3);  //Counter
      AddCallerToFromCurr(f_word_shift_l);  //Declare use
      functCall(nil, f_word_shift_l, AddrUndef);  //Use
      exit;
    end else if parA.val = 16 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      //_LDA(parB.add);
      //Loop
      _LDXi(4);  //Counter
      AddCallerToFromCurr(f_word_shift_l);  //Declare use
      functCall(nil, f_word_shift_l, AddrUndef);  //Use
      exit;
    end else if parA.val = 32 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      //_LDA(parB.add);
      //Loop
      _LDXi(5);  //Counter
      AddCallerToFromCurr(f_word_shift_l);  //Declare use
      functCall(nil, f_word_shift_l, AddrUndef);  //Use
      exit;
    end;
    //General case
    SetFunExpres(fun);
    //_LDAi(parA.val);
    _STA(fmul.pars[0].pvar.addr);
    _LDA(parA.val);
    _STA(fmul.pars[1].pvar.addr);
    AddCallerToFromCurr(fmul);  //Declare use
    AddCallerToFromCurr(fmul.pars[0].pvar);  //Declare use
    AddCallerToFromCurr(fmul.pars[1].pvar);  //Declare use
    functCall(nil, fmul, AddrUndef);   //Code the "JSR"
  end;
  stVariab_Const: begin
    fun.elements.Exchange(0,1);
    ROB_byte_mul_byte(fun);
    fun.elements.Exchange(0,1);
  end;
  stVariab_Variab:begin
    SetFunExpres(fun);
    _LDA(parA.add);
    _STA(fmul.pars[0].pvar.addr);
    _LDA(parB.add);
    _STA(fmul.pars[1].pvar.addr);
    AddCallerToFromCurr(fmul);  //Declare use
    AddCallerToFromCurr(fmul.pars[0].pvar);  //Declare use
    AddCallerToFromCurr(fmul.pars[1].pvar);  //Declare use
    functCall(nil, fmul, AddrUndef);   //Code the "JSR"
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    //_LDA(parA.add);
    _STA(fmul.pars[0].pvar.addr);
    _LDA(parA.add);
    _STA(fmul.pars[1].pvar.addr);
    AddCallerToFromCurr(fmul);  //Declare use
    AddCallerToFromCurr(fmul.pars[0].pvar);  //Declare use
    AddCallerToFromCurr(fmul.pars[1].pvar);  //Declare use
    functCall(nil, fmul, AddrUndef);   //Code the "JSR"
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    fun.elements.Exchange(0,1);
    ROB_byte_mul_byte(fun);  //, true);
    fun.elements.Exchange(0,1);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    fun.elements.Exchange(0,1);
    ROB_byte_mul_byte(fun);  //, true);
    fun.elements.Exchange(0,1);
  end;
//  stExpres_Expres:begin
//    SetResultExpres(fun);
//    //La expresión p1 debe estar salvada y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _CLC;
//    _ADC(rVar.adrByte0);  //opera directamente al dato que había en la pila. Deja en A
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_byte_great_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const: begin  //compara constantes. Caso especial
    SetFunConst_bool(fun, parA.val > parB.val);
  end;
  stConst_Variab: begin
    if parA.val = 0 then begin
      //0 es mayor que nada
      SetFunConst_bool(fun, false);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      SetFunExpres(fun, logInverted);
      _LDA(parB.add);
      _CMPi(parA.val); //Result in C (inverted)
      Copy_C_to_A; //Copy C to A (still inverted)
    end;
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    if parA.val = 0 then begin
      //0 es mayor que nada
      SetFunConst_byte(fun, 0);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      //Se necesita asegurar que p1, es mayo que cero.
      SetFunExpres(fun, logInverted);
      //p2, already in A
      _CMPi(parA.val); //Result in C (inverted)
      Copy_C_to_A; //Copy C to A (still inverted)
    end;
  end;
  stVariab_Const: begin
    if parB.val = 255 then begin
      //Nada es mayor que 255
      SetFunConst_bool(fun, false);
      GenWarn('Expression will always be FALSE or TRUE.');
    end else begin
      SetFunExpres(fun, logInverted);
      _LDAi(parB.val);
      _CMP(parA.add); //Result in C (inverted)
      Copy_C_to_A; //Copy C to A (still inverted)
    end;
  end;
  stVariab_Variab:begin
    SetFunExpres(fun, logInverted);
    _LDA(parB.add);
    _CMP(parA.add); //Result in C (inverted)
    Copy_C_to_A; //Copy C to A (still inverted)
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun, logInverted);
    //p2, already in A
    _CMP(parA.add); //Result in C (inverted)
    Copy_C_to_A; //Copy C to A (still inverted)
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    if parB.val = 255 then begin
      //Nada es mayor que 255
      SetFunConst_byte(fun, 0);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      SetFunExpres(fun, logNormal);
      //p1, already in A
      _CMPi(parB.val+1); //p1 >= p2+1. We've verified parB.val<255
      Copy_C_to_A; //Copy C to A
    end;
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun, logNormal);
    _CLC;   //A trick to get p1>p2 in C, after _SBC
    _SBC(parB.add);
    Copy_C_to_A; //Copy C to A
  end;
  //stExpres_Expres:begin
  //  //la expresión p1 debe estar salvada y p2 en el acumulador
  //  parA.SetAsVariab(GetVarByteFromStk, 0);  //Convierte a variable
  //  //Luego el caso es similar a stVariab_Expres
  //  ROB_byte_great_byte(fun, true);
  //  FreeStkRegisterByte;   //libera pila porque ya se usó el dato ahí contenido
  //end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_byte_less_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //A < B es lo mismo que B > A
  case stoOperation(parA, parB) of
  stExpres_Expres:begin
//    {Este es el único caso que no se puede invertir, por la posición de los operandos en
//     la pila.}
//    //la expresión p1 debe estar salvada y p2 en el acumulador
//    parA.SetAsVariab(GetVarByteFromStk);  //Convierte a variable
//    //Luego el caso es similar a stVariab_Expres
//    ROB_byte_less_byte(Opt, SetRes);
//    FreeStkRegisterByte;   //libera pila porque ya se usó el dato ahí contenido
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
  else
    //Para los otros casos, funciona
    fun.elements.Exchange(0,1);
    ROB_byte_great_byte(fun);
    fun.elements.Exchange(0,1);
  end;
end;
procedure TGenCod.ROB_byte_gequ_byte(fun: TEleExpress);
begin
  ROB_byte_less_byte(fun);
  fun.Invert;
end;
procedure TGenCod.ROB_byte_lequ_byte(fun: TEleExpress);
begin
  ROB_byte_great_byte(fun);
  fun.Invert;
end;
procedure TGenCod.ROB_byte_shr_byte(fun: TEleExpress);  //Desplaza a la derecha
var
  L2, L1: integer;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const: begin  //compara constantes. Caso especial
    SetFunConst_byte(fun, parA.val >> parB.val);
  end;
  stConst_Variab: begin
    SetFunExpres(fun);   //Se pide Z para el resultado
    _LDAi(parA.val);
    _LDX(parB.add);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _LSRa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);   //Se pide Z para el resultado
    _TAX;
    _BEQ_post(L2);
    _LDAi(parA.val);
_LABEL_pre(L1);
    _LSRa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stVariab_Const: begin
    SetFunExpres(fun);   //Se pide Z para el resultado
    //Verifica casos simples
    if parB.val = 0 then begin
      _LDA(parA.add);  //solo devuelve lo mismo en A
    end else if parB.val = 1 then begin
      _LDA(parA.add);
      _LSRa;
    end else if parB.val = 2 then begin
      _LDA(parA.add);
      _LSRa;
      _LSRa;
    end else if parB.val = 3 then begin
      _LDA(parA.add);
      _LSRa;
      _LSRa;
      _LSRa;
    end else if parB.val = 4 then begin
      _LDA(parA.add);
      _LSRa;
      _LSRa;
      _LSRa;
      _LSRa;
    end else begin
      //Caso general
      _LDA(parA.add);
      _LDXi(parB.val);
_LABEL_pre(L1);
      _LSRa;
      _DEX;
      _BNE_pre(L1);  //loop1
    end;
  end;
  stVariab_Variab:begin
    SetFunExpres(fun);   //Se pide Z para el resultado
    _LDA(parA.add);
    _LDX(parB.add);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _LSRa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);   //Se pide Z para el resultado
    _TAX;
    _BEQ_post(L2);
    _LDA(parA.add);
_LABEL_pre(L1);
    _LSRa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);   //Se pide Z para el resultado
    //Verifica casos simples
    if parB.val = 0 then begin
      //solo devuelve lo mismo en A
    end else if parB.val = 1 then begin
      _LSRa;
    end else if parB.val = 2 then begin
      _LSRa;
      _LSRa;
    end else if parB.val = 3 then begin
      _LSRa;
      _LSRa;
      _LSRa;
    end else if parB.val = 4 then begin
      _LSRa;
      _LSRa;
      _LSRa;
      _LSRa;
    end else begin
      _LDXi(parB.val);
_LABEL_pre(L1);
      _LSRa;
      _DEX;
      _BNE_pre(L1);  //loop1
    end;
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    _LDX(parB.add);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _LSRa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
//  stExpres_Expres:begin
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_byte_shl_byte(fun: TEleExpress);   //Desplaza a la izquierda
var
  L1, L2: integer;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const: begin  //compara constantes. Caso especial
    SetFunConst_byte(fun, parA.val << parB.val);
  end;
  stConst_Variab: begin
    SetFunExpres(fun);   //Se pide Z para el resultado
    _LDAi(parA.val);
    _LDX(parB.add);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _ASLa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);   //Se pide Z para el resultado
    _TAX;
    _BEQ_post(L2);
    _LDAi(parA.val);
_LABEL_pre(L1);
    _ASLa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stVariab_Const: begin
    SetFunExpres(fun);   //Se pide Z para el resultado
    //Verifica casos simples
    if parB.val = 0 then begin
      _LDA(parA.add);  //solo devuelve lo mismo en A
    end else if parB.val = 1 then begin
      _LDA(parA.add);
      _ASLa;
    end else if parB.val = 2 then begin
      _LDA(parA.add);
      _ASLa;
      _ASLa;
    end else if parB.val = 3 then begin
      _LDA(parA.add);
      _ASLa;
      _ASLa;
      _ASLa;
    end else if parB.val = 4 then begin
      _LDA(parA.add);
      _ASLa;
      _ASLa;
      _ASLa;
      _ASLa;
    end else begin
      //Caso general
      _LDA(parA.add);
      _LDXi(parB.val);
_LABEL_pre(L1);
      _ASLa;
      _DEX;
      _BNE_pre(L1);  //loop1
    end;
  end;
  stVariab_Variab:begin
    SetFunExpres(fun);   //Se pide Z para el resultado
    _LDA(parA.add);
    _LDX(parB.add);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _ASLa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);   //Se pide Z para el resultado
    _TAX;
    _BEQ_post(L2);
    _LDA(parA.add);
_LABEL_pre(L1);
    _ASLa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);   //Se pide Z para el resultado
    //Verifica casos simples
    if parB.val = 0 then begin
      //solo devuelve lo mismo en A
    end else if parB.val = 1 then begin
      _ASLa;
    end else if parB.val = 2 then begin
      _ASLa;
      _ASLa;
    end else if parB.val = 3 then begin
      _ASLa;
      _ASLa;
      _ASLa;
    end else if parB.val = 4 then begin
      _ASLa;
      _ASLa;
      _ASLa;
      _ASLa;
    end else begin
      _LDXi(parB.val);
_LABEL_pre(L1);
      _ASLa;
      _DEX;
      _BNE_pre(L1);  //loop1
    end;
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    _LDX(parB.add);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _ASLa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
//  stExpres_Expres:begin
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
////////////operaciones con Boolean
procedure TGenCod.ROB_bool_asig_bool(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  SetFunNull(fun);  //In Pascal an assigment doesn't return type.
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Validates parA.
  if parA.opType<>otVariab then begin //The only valid type.
    GenError('Only variables can be assigned.');
    exit;
  end;
  //Realiza la asignación
  if parA.Sto = stRamFix then begin
    //Assignment to a common variable (constant Address)
    case parB.Sto of
    stConst : begin
      if parB.value.valBool then begin
        //Por facilidad se usa el bit 1
        _LDAi(2);
        _STA(parA.add);
      end else begin
        _LDAi(0);
        _STA(parA.add);
      end;
    end;
    stRamFix: begin
      _LDA(parB.add);
      _STA(parA.add);
    end;
    stRegister, stRegistA: begin  //ya está en A
      _STA(parA.add);
    end;
    stRegistX: begin
      _STX(parA.add);
    end;
    stRegistY: begin
      _STY(parA.add);
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto in [stRegistA, stRegister] then begin
    //Assignment to register A
    case parB.Sto of
    stConst : begin
      if parB.value.valBool then begin
        //Por facilidad se usa el bit 1
        _LDAi(2);
      end else begin
        _LDAi(0);
      end;
    end;
    stRamFix: begin
      _LDA(parB.add);
    end;
    stRegister, stRegistA: begin  //Already in A
    end;
    stRegistX: begin
      _TXA;
    end;
    stRegistY: begin
      _TYA;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto = stRegistX then begin
    //Assignment to register X
    case parB.Sto of
    stConst : begin
      if parB.value.valBool then begin
        //Por facilidad se usa el bit 1
        _LDXi(2);
      end else begin
        _LDXi(0);
      end;
    end;
    stRamFix: begin
      _LDX(parB.add);
    end;
    stRegister, stRegistA: begin  //Already in A
      _TAX;
    end;
    stRegistX: begin  //Already in X
    end;
    stRegistY: begin
      _TYA;  //Modify A
      _TAX;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto = stRegistY then begin
    //Assignment to register Y
    case parB.Sto of
    stConst : begin
      if parB.value.valBool then begin
        //Por facilidad se usa el bit 1
        _LDYi(2);
      end else begin
        _LDYi(0);
      end;
    end;
    stRamFix: begin
      _LDY(parB.add);
    end;
    stRegister, stRegistA: begin  //Already in A
      _TAY;
    end;
    stRegistX: begin
      _TXA;  //Modify A
      _TAY;
    end;
    stRegistY: begin //Already in Y
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.ROB_bool_and_bool(fun: TEleExpress);
var
  sale0: integer;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const: begin  //Special case. Comapares constants.
    SetFunConst_bool(fun, parA.value.valBool and parB.value.valBool);
  end;
  stConst_Variab: begin
    if parA.value.valBool = false then begin  //Special case.
      SetFunConst_bool(fun, false);
    end else begin  //Special case.
      SetFunVariab(fun, parB.rVar);  //Can be problematic return "var". Formaly it should be an expression.
    end;
  end;
  stConst_Expres: begin  //Expression p2 evaluated in A
    if parA.value.valBool = false then begin  //Special case.
      SetFunConst_bool(fun, false);
      exit;
    end else if parA.value.valBool = true then begin  //Special case.
      SetFunExpres(fun);  //No needed do anything. Result already in A
      exit;
    end;
  end;
  stVariab_Const: begin
    if parB.value.valBool = false then begin  //Special case.
      SetFunConst_bool(fun, false);
      exit;
    end else if parB.value.valBool = true then begin  //Special case.
      SetFunVariab(fun, parA.rVar);  //Can be problematic return "var". Formaly it should be an expression.
      exit;
    end;
  end;
  stVariab_Variab: begin
    SetFunExpres(fun, logInverted);
    _LDX(parA.add);
    _BEQ_post(sale0);  //p1=0
    _LDX(parB.add);
_LABEL_post(sale0);
    //Here  : Z = 0 if p1<>0 and p2<>0
    //That's: Z = NOT (p1 and p2)
    Copy_Z_to_A;  //Maintain the inverted logic
  end;
  stVariab_Expres:begin   //Expresion p2 evaluated in A
    //Algorith like in "stVariab_Variab"
    SetFunExpres(fun, logInverted);
    //Obviously, the last RO was what generates the expression, so we can check "AcumStatInZ".
    if not AcumStatInZ then _TAX;   //Update Z, if needed.
    //Here, we have Z updated with the value of A
    _BEQ_post(sale0);  //p1=0
    _LDX(parA.add);
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stExpres_Const: begin   //Expresion p1 evaluated in A
    if parB.value.valBool = false then begin  //Special case.
      SetFunConst_bool(fun, false);
      exit;
    end else if parB.value.valBool = true then begin  //Special case.
      SetFunExpres(fun);  //No needed do anything. Result already in A
      exit;
    end;
  end;
  stExpres_Variab:begin  //Expresion p1 evaluated in A
    //Algorith like in "stVariab_Variab"
    SetFunExpres(fun, logInverted);
    //Obviously, the last RO was what generates the expression, so we can check "AcumStatInZ".
    if not AcumStatInZ then _TAX;   //Update Z, if needed.
    //Here, we have Z updated with the value of A
    _BEQ_post(sale0);  //p1=0
    _LDX(parB.add);
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
//  stExpres_Expres:begin
//    SetResultExpres(fun);
//    //p1 está en la pila y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _AND(rVar.adrByte0);
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_bool_equal_bool(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const: begin  //Special case. Comapares constants.
    SetFunConst_bool(fun, parA.value.valBool = parB.value.valBool);
  end;
  stConst_Variab: begin
    if parA.value.valBool = false then begin  //Special case.
      SetFunExpres(fun);
      _LDA(parB.add);  //if equals Z=1
      Copy_Z_to_A;
    end else begin  //Special case.
      SetFunExpres(fun, logInverted);
      _LDA(parB.add);  //if equals Z=0
      Copy_Z_to_A;
    end;
  end;
  stConst_Expres: begin  //Expression p2 evaluated in A
    if parA.value.valBool = false then begin  //Special case.
      SetFunExpres(fun);
      if not AcumStatInZ then _TAX;   //Update Z, if needed.
      //if equals Z=1
      Copy_Z_to_A;
    end else begin  //Special case.
      SetFunExpres(fun, logInverted);
      if not AcumStatInZ then _TAX;   //Update Z, if needed.
      //if equals Z=0
      Copy_Z_to_A;
    end;
  end;
  stVariab_Const: begin
    if parB.value.valBool = false then begin  //Special case.
      SetFunExpres(fun);
      _LDA(parA.add);  //if equals Z=1
      Copy_Z_to_A;
    end else begin  //Special case.
      SetFunExpres(fun, logInverted);
      _LDA(parA.add);  //if equals Z=0
      Copy_Z_to_A;
    end;
  end;
  stVariab_Variab: begin
    SetFunExpres(fun, logInverted);
    _LDA(parB.add);  //p2 in A.1
    _EOR(parA.add);  //Compare Oper1.1 with Oper2.1. Result in A, inverted.
  end;
  stVariab_Expres:begin   //Expresion p2 evaluated in A
    if parB.logic = logNormal then SetFunExpres(fun, logInverted)
    else SetFunExpres(fun, logNormal);
    //p2 in A.1
    _EOR(parA.add);  //Compare Oper1.1 with Oper2.1
    {Can be demonstrated the bit Z returns the operation "=" inverted. But if p2 is
    inverted, the result will have the correct logic.}
  end;
  stExpres_Const: begin   //Expresion p1 evaluated in A
    if parB.value.valBool = false then begin  //Special case.
      SetFunExpres(fun);
      if not AcumStatInZ then _TAX;   //Update Z, if needed.
      //if equals Z=1
      Copy_Z_to_A;
    end else begin  //Special case.
      SetFunExpres(fun, logInverted);
      if not AcumStatInZ then _TAX;   //Update Z, if needed.
      //if equals Z=0
      Copy_Z_to_A;
    end;
  end;
  stExpres_Variab:begin  //Expresion p1 evaluated in A
    if parA.logic = logNormal then SetFunExpres(fun, logInverted)
    else SetFunExpres(fun, logNormal);
    //p1 in A.1
    _EOR(parB.add);  //Compare Oper1.1 with Oper2.1
    {Can be demonstrated the bit Z returns the operation "=" inverted. But if p1 is
    inverted, the result will have the correct logic.}
  end;
//  stExpres_Expres:begin
//    SetResultExpres(fun);
//    //p1 está en la pila y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _AND(rVar.adrByte0);
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_bool_difer_bool(fun: TEleExpress);
begin
  ROB_bool_equal_bool(fun);
  fun.Invert;
end;
////////////operaciones con Word
procedure TGenCod.ROB_word_asig_word(fun: TEleExpress);
var
  idxVar: TEleVarDec;
  parA, parB: TEleExpress;
begin
  SetFunNull(fun);
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Validates parA.
  if parA.opType<>otVariab then begin //The only valid type.
    GenError('Only variables can be assigned.');
    exit;
  end;
  //Implements assignment
  if parA.Sto = stRamFix then begin
    case parB.Sto of
    stConst : begin
      SetFunExpres(fun);  //Realmente, el resultado no es importante
      if parB.valL = parB.valH then begin  //Lucky case
        _LDAi(parB.valL);
        _STA(parA.addL);
        _STA(parA.addH);
      end else begin  //General case
        //Caso general
        _LDAi(parB.valL);
        _STA(parA.addL);
        _LDAi(parB.valH);
        _STA(parA.addH);
      end;
    end;
    stRamFix: begin
      SetFunExpres(fun);  //Realmente, el resultado no es importante
      _LDA(parB.addL);
      _STA(parA.addL);
      _LDA(parB.addH);
      _STA(parA.addH);
    end;
    stRegister: begin   //se asume que se tiene en (H,A)
      SetFunExpres(fun);  //Realmente, el resultado no es importante
      _STA(parA.addL);
      _LDA(H.addr);
      _STA(parA.addH);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else if parA.Sto = stRegister then begin
    requireA;
    requireH;
    //Assignment to register H,A
    case parB.Sto of
    stConst : begin
      if parB.valL = parB.valH then begin  //Lucky case
        _LDAi(parB.valH);
        _STA(H.addr);  //No need to update A
      end else begin  //General case
        _LDAi(parB.valH);
        _STA(H.addr);
        _LDAi(parB.valL);
      end;
    end;
    stRamFix: begin
      _LDA(parB.addH);
      _STA(H.addr);
      _LDA(parB.addL);
    end;
    stRegister: begin  //Already in H,A
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
//  end else if parA.Sto in [stVarRef, stExpRef] then begin
//    //Asignación a una variable referenciada por variable (o IX)
//    if parA.Sto =stVarRef then
//      idxVar := parA.rVar
//    else
//      idxVar := IX;
//    if idxVar.typ.IsByteSize then begin
//      //Index is byte-size. It's easy.
//      case parB.Sto of //Operand2 to assign
//      stConsta:  begin
//        _LDAi(parB.valL);
//        _LDX(idxVar.addr);
//        pic.codAsm(i_STA, aZeroPagX, 0);
//        _LDAi(parB.valH);
//        _INX;  //Fails is cross page
//        pic.codAsm(i_STA, aZeroPagX, 0);
//      end;
//      stRamFix: begin
//        _LDA(parB.addL);
//        _LDX(idxVar.addr);
//        pic.codAsm(i_STA, aZeroPagX, 0);
//        _LDA(parB.addH);
//        _INX;  //Fails is cross page
//        pic.codAsm(i_STA, aZeroPagX, 0);
//      end;
//      stRegister: begin  //Already in A
//        _LDX(idxVar.addr);
//        pic.codAsm(i_STA, aZeroPagX, 0);
//        _LDA(H.addr);
//        _INX;  //Fails is cross page
//        pic.codAsm(i_STA, aZeroPagX, 0);
//      end;
//      else
//        GenError(MSG_UNSUPPORTED); exit;
//      end;
//    end else if idxvar.typ.IsWordSize then begin
//      //Index is word-size
//      //If it's in zero-page will be wonderful
//      if (idxvar.addr<256) then begin
//        _LDYi(0);
//        case parB.Sto of //Operand2 to assign
//        stConsta:  begin
//          _LDAi(parB.valL);
//          pic.codAsm(i_STA, aIndirecY, idxvar.addr); //Store forward
//          _LDAi(parB.valH);
//          _INY;
//          pic.codAsm(i_STA, aIndirecY, idxvar.addr); //Store forward
//        end;
//        stRamFix: begin
//          _LDA(parB.addL);
//          pic.codAsm(i_STA, aIndirecY, idxvar.addr); //Store forward
//          _LDA(parB.addH);
//          _INY;
//          pic.codAsm(i_STA, aIndirecY, idxvar.addr); //Store forward
//        end;
//        stRegister: begin  //Already in A
//          pic.codAsm(i_STA, aIndirecY, idxvar.addr); //Store forward
//          _LDA(H.addr);
//          _INY;
//          pic.codAsm(i_STA, aIndirecY, idxvar.addr); //Store forward
//        end;
//        else
//          GenError(MSG_UNSUPPORTED); exit;
//        end;
//      end else begin
//        GenError(MSG_UNSUPPORTED); exit;
////        //No problem. We create self-modifying code
////        //Add to the index, the offset
////        if parB.Sto = stRegister then _PLA;  //Save A
////        _CLC;
////        _LDA(idxVar.addr);  //Load index
////addrNextOp1 := pic.iRam + 1;  //Address next instruction
////        pic.codAsm(i_STA, aAbsolute, 0); //Store forward
////        _LDA(idxVar.addr+1);  //Load MSB
////addrNextOp2 := pic.iRam + 1;  //Address next instruction
////        pic.codAsm(i_STA, aAbsolute, 0); //Store forward
////        case parB.Sto of //Operand2 to assign
////        stConsta:  _LDAi(parB.val);
////        stRamFix: _LDA(parB.add);
////        stRegister: _PLA;  //Restore A
////        else
////          GenError(MSG_UNSUPPORTED); exit;
////        end;
////        //Modifiying instruction
////        pic.codAsm(i_STA, aAbsolute, 0); //Store forward
////        //Complete address
////        pic.ram[addrNextOp1].value := (pic.iRam - 2) and $FF;
////        pic.ram[addrNextOp1+1].value := (pic.iRam - 2)>>8 and $FF;
////        pic.ram[addrNextOp2].value := (pic.iRam - 1) and $FF;
////        pic.ram[addrNextOp2+1].value := (pic.iRam - 1)>>8 and $FF;
//      end;
//    end else begin
//      //Index could be dword or other type
//      GenError(MSG_UNSUPPORTED); exit;
//    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.ROB_word_asig_byte(fun: TEleExpress);
var
  idxVar: TEleVarDec;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  if parA.Sto = stRamFix then begin
    case parB.Sto of
    stConst : begin
      SetFunExpres(fun);  //Realmente, el resultado no es importante
      if parB.valL = 0 then begin
        _LDAi(0);  //Load once
        _STA(parA.addL);
        _STA(parA.addH);
      end else begin
        _LDAi(parB.valL);
        _STA(parA.addL);
        _LDAi(0);
        _STA(parA.addH);
      end;
    end;
    stRamFix: begin
      SetFunExpres(fun);  //Realmente, el resultado no es importante
      _LDA(parB.addL);
      _STA(parA.addL);
      _LDAi(0);
      _STA(parA.addH);
    end;
    stRegister: begin   //se asume que está en A
      SetFunExpres(fun);  //Realmente, el resultado no es importante
      _STA(parA.addL);
      _LDAi(0);
      _STA(parA.addH);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
//  end else if parA.Sto in [stVarRef, stExpRef] then begin
//    //Asignación a una variable referenciada por variable (o IX)
//    case parB.Sto of
//    stConsta, stRamFix, stRegister: begin
//      if parA.Sto =stVarRef then
//        idxVar := parA.rVar
//      else
//        idxVar := IX;
//      if idxVar.typ.IsByteSize then begin
//        //Index is byte-size. It's easy.
//        case parB.Sto of //Operand2 to assign
//        stConsta:  _LDAi(parB.val);
//        stRamFix: _LDA(parB.add);
//        stRegister: ;  //Already in A
//        end;
//        _LDX(idxVar.addr);
//        pic.codAsm(i_STA, aZeroPagX, 0);
//        pic.codAsm(i_STA, aZeroPagX, 1);
//      end else if idxvar.typ.IsWordSize then begin
//        //Index is word-size
//        //If it's in zero-page will be wonderful
//        if (idxvar.addr<256) then begin
//          _LDYi(0);
//          case parB.Sto of //Operand2 to assign
//          stConsta : _LDAi(parB.val);
//          stRamFix: _LDA(parB.add);
//          stRegister: ;  //Already in A
//          end;
//          pic.codAsm(i_STA, aIndirecY, idxvar.addr); //Store forward
//          _LDAi(0);
//          _INY;
//          pic.codAsm(i_STA, aIndirecY, idxvar.addr); //Store forward
////        end else begin
////          {We can create self-modifying code o some weird code to implement this, but
////          it's complex code. It's better using IX or the Var-index in Zero page}
//        end else begin
//          //Index could be dword or other type
//          GenError(MSG_UNSUPPORTED); exit;
//        end;
//      end else begin
//        //Index could be dword or other type
//        GenError(MSG_UNSUPPORTED); exit;
//      end;
//    end;
//    else
//      GenError(MSG_UNSUPPORTED); exit;
//    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.ROB_word_equal_word(fun: TEleExpress);
var
  sale0: integer;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const: begin  //compara constantes. Caso especial
    SetFunConst_byte(fun, ORD(parA.val = parB.val));
  end;
  stConst_Variab: begin
    SetFunExpres(fun);
    _LDAi(parA.valL);
    _CMP(parB.addL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDAi(parA.valH);
    _CMP(parB.addH);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó p2 esta en A
    SetFunExpres(fun);
//    _LDAi(parA.valL);
    _CMPi(parA.valL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDAi(parA.valH);
    _CMP(H.addr);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stVariab_Const: begin
    SetFunExpres(fun);
    _LDA(parA.addL);
    _CMPi(parB.valL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDA(parA.addH);
    _CMPi(parB.valH);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stVariab_Variab:begin
    SetFunExpres(fun);
    _LDA(parA.addL);
    _CMP(parB.addL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDA(parA.addH);
    _CMP(parB.addH);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    //_LDA(parA.addL);
    _CMP(parA.addL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDA(parA.addH);
    _CMP(H.addr);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    //_LDA(parA.addL);
    _CMPi(parB.valL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDAi(parB.valH);
    _CMP(H.addr);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    //_LDA(parA.addL);
    _CMP(parB.addL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDA(parB.addH);
    _CMP(H.addr);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
//  stExpres_Expres:begin
//    //La expresión p1, debe estar salvada y p2 en (H,A)
//    parA.SetAsVariab(GetVarWordFromStk);
//    //Luego el caso es similar a variable-expresión
//    ROB_word_equal_word(fun, SetRes);
//    FreeStkRegisterWord;
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_word_difer_word(fun: TEleExpress);
begin
  ROB_word_equal_word(fun);
  fun.Invert;
end;
procedure TGenCod.ROB_word_add_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimize
    SetFunConst_word(fun, parA.val + parB.val);
  end;
  stConst_Variab: begin
    SetFunExpres(fun);
    _CLC;
    _LDAi(parA.valL);
    _ADC(parB.addL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
    LastIsTXA := true;
    //Form 2: (Very similar)
//    _LDA(parB.addH);  //parB.add->H
//    _STA(H.addr);
//    _CLC;
//    _LDAi(parA.valL);
//    _ADC(parB.addL);
//    _BCC_post(L2);
//    _INC(H.addr);
//_LABEL_post(L2);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en (A)
    SetFunExpres(fun);
    _CLC;
    _ADCi(parA.valL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
    LastIsTXA := true;
  end;
  stVariab_Const: begin
    SetFunExpres(fun);
    _CLC;
    _LDA(parA.addL);
    _ADCi(parB.valL);
    _TAX;  //Save
    _LDA(parA.addH);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
    LastIsTXA := true;
  end;
  stVariab_Variab:begin
    SetFunExpres(fun);
    _CLC;
    _LDA(parA.addL);
    _ADC(parB.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
    LastIsTXA := true;
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en (_H,A)
    SetFunExpres(fun);
    _CLC;
    _ADC(parA.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
    LastIsTXA := true;
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _CLC;
    _ADCi(parB.valL);
    _TAX;  //Save
    _LDA(H.addr);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
    LastIsTXA := true;
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _CLC;
    _ADC(parB.addL);
    _TAX;  //Save
    _LDA(H.addr);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
    LastIsTXA := true;
  end;
//  stExpres_Expres:begin
//    SetResultExpres(fun);
//    //p1 está salvado en pila y p2 en (A)
//    parA.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
//    //Luego el caso es similar a stVariab_Expres
//    _AND(parA.add);
//    FreeStkRegisterWord;   //libera pila
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_word_add_word(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimize
    SetFunConst_word(fun, parA.val + parB.val);
  end;
  stConst_Variab: begin
    SetFunExpres(fun);
    requireH;
//    if ModeRequire then exit;
    _CLC;
    _LDAi(parA.valL);
    _ADC(parB.addL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _ADC(parB.addH);
    _STA(H.addr);
    _TXA;  //Restore A
    LastIsTXA := true;
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en (A)
    SetFunExpres(fun);
    _CLC;
    _ADCi(parA.valL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _ADC(H.addr);
    _STA(H.addr);
    _TXA;  //Restore A
    LastIsTXA := true;
  end;
  stVariab_Const: begin
    if parB.val = 0 then begin  //Special case
      SetFunVariab(fun, parA.rVar);
    end else if parB.valL = 0 then begin
      SetFunExpres(fun);
      _LDA(parA.addH);
      _ADCi(parB.valH);
      _STA(H.addr);
      _LDA(parA.addL);
    end else begin
      SetFunExpres(fun);
      _CLC;
      _LDA(parA.addL);
      _ADCi(parB.valL);
      _TAX;  //Save
      _LDA(parA.addH);
      _ADCi(parB.valH);
      _STA(H.addr);
      _TXA;  //Restore A
      LastIsTXA := true;
    end;
  end;
  stVariab_Variab:begin
    SetFunExpres(fun);
    _CLC;
    _LDA(parA.addL);
    _ADC(parB.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _ADC(parB.addH);
    _STA(H.addr);
    _TXA;  //Restore A
    LastIsTXA := true;
  end;
  stVariab_Expres:begin  //La expresión B se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _CLC;
    _ADC(parA.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _ADC(H.addr);
    _STA(H.addr);
    _TXA;  //Restore A
    LastIsTXA := true;
  end;
  stExpres_Const: begin  //La expresión A se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _CLC;
    _ADCi(parB.valL);
    _TAX;  //Save
    _LDA(H.addr);
    _ADCi(parB.valH);
    _STA(H.addr);
    _TXA;  //Restore A
    LastIsTXA := true;
  end;
  stExpres_Variab:begin  //La expresión A se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _CLC;
    _ADC(parB.addL);
    _TAX;  //Save
    _LDA(H.addr);
    _ADC(parB.addH);
    _STA(H.addr);
    _TXA;  //Restore A
    LastIsTXA := true;
  end;
//  stExpres_Expres:begin
//    SetResultExpres(fun);
//    //p1 está salvado en pila y p2 en (A)
//    parA.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
//    //Luego el caso es similar a stVariab_Expres
//    _AND(parA.add);
//    FreeStkRegisterWord;   //libera pila
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_word_sub_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const:begin  //suma de dos constantes. Caso especial
    SetFunConst_word(fun, parA.val-parB.val);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  stConst_Variab: begin
    SetFunExpres(fun);
    _SEC;
    _LDAi(parA.valL);
    _SBC(parB.addL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _SBCi(0);
    _STA(H.addr);
    _TXA;  //Restore A
    LastIsTXA := true;
  end;
//  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
//    SetResultExpres(fun);
//      AddCallerTo(H);  //Declare using register
//    _STA(H);
//    _SEC;
//    _LDA(parA.val);
//    _SBC(H);
//  end;
  stVariab_Const: begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.addL);
    _SBCi(parB.valL);
    _TAX;  //Save
    _LDA(parA.addH);
    _SBCi(0);
    _STA(H.addr);
    _TXA;  //Restore A
    LastIsTXA := true;
  end;
  stVariab_Variab:begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.addL);
    _SBC(parB.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _SBCi(0);
    _STA(H.addr);
    _TXA;  //Restore A
    LastIsTXA := true;
  end;
//  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBC(parA.add);   //a - p1 -> a
//    //Invierte
//    _EORi($FF);
//    _CLC;
//    _ADCi(1);
//  end;
//  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBCi(parB.val);
//  end;
//  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBC(parB.add);
//  end;
//  stExpres_Expres:begin
//    SetResultExpres(fun);
//    //la expresión p1 debe estar salvada y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _SEC;
//    _SBC(rVar.adrByte0);
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_word_sub_word(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const:begin  //suma de dos constantes. Caso especial
    SetFunConst_word(fun, parA.val-parB.val);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  stConst_Variab: begin
    SetFunExpres(fun);
    _SEC;
    _LDAi(parA.valL);
    _SBC(parB.addL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _SBC(parB.addH);
    _STA(H.addr);
    _TXA;  //Restore A
    LastIsTXA := true;
  end;
//  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
//    SetResultExpres(fun);
//      AddCallerTo(H);  //Declare using register
//    _STA(H);
//    _SEC;
//    _LDA(parA.val);
//    _SBC(H);
//  end;
  stVariab_Const: begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.addL);
    _SBCi(parB.valL);
    _TAX;  //Save
    _LDA(parA.addH);
    _SBCi(parB.valH);
    _STA(H.addr);
    _TXA;  //Restore A
    LastIsTXA := true;
  end;
  stVariab_Variab:begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.addL);
    _SBC(parB.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _SBC(parB.addH);
    _STA(H.addr);
    _TXA;  //Restore A
    LastIsTXA := true;
  end;
//  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBC(parA.add);   //a - p1 -> a
//    //Invierte
//    _EORi($FF);
//    _CLC;
//    _ADCi(1);
//  end;
//  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBCi(parB.val);
//  end;
//  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBC(parB.add);
//  end;
//  stExpres_Expres:begin
//    SetResultExpres(fun);
//    //la expresión p1 debe estar salvada y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _SEC;
//    _SBC(rVar.adrByte0);
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_word_aadd_byte(fun: TEleExpress);
var
  L1, L2: integer;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Special assigment
  if parA.Sto = stRamFix then begin
    SetFunNull(fun);  //Formaly, an assigment doesn't return any value in Pascal
    //Asignación a una variable
    case parB.Sto of
    stConst : begin
      if parB.val=0 then begin
        //Caso especial. No hace nada
      end else if parB.val=1 then begin
        //Caso especial.
        _INC(parA.addL);
        _BNE_post(L1);
        _INC(parA.addH);
_LABEL_post(L1);
      end else begin
        _CLC;
        _LDA(parA.addL);
        _ADCi(parB.val);
        _STA(parA.addL);
        _BCC_post(L2);
        _INC(parA.addH);
_LABEL_post(L2);
      end;
    end;
    stRamFix: begin
      _CLC;
      _LDA(parA.addL);
      _ADC(parB.add);
      _STA(parA.addL);
      _BCC_post(L2);
      _INC(parA.addH);
_LABEL_post(L2);
    end;
    stRegister: begin  //ya está en A
      _CLC;
      _ADC(parA.addL);
      _STA(parA.addL);
      _BCC_post(L2);
      _INC(parA.addH);
_LABEL_post(L2);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.ROB_word_aadd_word(fun: TEleExpress);
var
  L1, L2: integer;
  val2: DWord;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Special assigment
  if parA.Sto = stRamFix then begin
    SetFunNull(fun);  //Formaly, an assigment doesn't return any value in Pascal
    //Asignación a una variable
    case parB.Sto of
    stConst : begin
      val2 := parB.val;
      if val2=0 then begin  //Special case
        //Do nothing
      end else if val2=1 then begin  //Special case
        _INC(parA.addL);
        _BNE_post(L1);
        _INC(parA.addH);
_LABEL_post(L1);
      end else if val2 < 256 then begin
        _CLC;
        _LDA(parA.addL);
        _ADCi(parB.val);
        _STA(parA.addL);
        _BCC_post(L2);
        _INC(parA.addH);
_LABEL_post(L2);
      end else if val2 = 256 then begin
        _INC(parA.addH);
      end else if val2 = 512 then begin
        _INC(parA.addH);
        _INC(parA.addH);
      end else if (val2 and $FF) = 0 then begin
        _CLC;
        _LDAi(parB.valH);
        _ADC(parA.addH);
        _STA(parA.addH);
      end else begin
        _CLC;
        _LDA(parA.addL);
        _ADCi(parB.valL);
        _STA(parA.addL);
        _LDA(parA.addH);
        _ADCi(parB.valH);
        _STA(parA.addH);
      end;
    end;
    stRamFix: begin
      _CLC;
      _LDA(parA.addL);
      _ADC(parB.addL);
      _STA(parA.addL);
      _LDA(parA.addH);
      _ADC(parB.addH);
      _STA(parA.addH);
    end;
    stRegister: begin  //ya está en H,A
      _CLC;
      //_LDA(parA.addL);
      _ADC(parA.addL);
      _STA(parA.addL);
      _LDA(H.addr);
      _ADC(parA.addH);
      _STA(parA.addH);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.ROB_word_asub_byte(fun: TEleExpress);
var
  L1: integer;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Caso especial de asignación
  if parA.Sto = stRamFix then begin
    SetFunNull(fun);  //Fomalmente,  una aisgnación no devuelve valores en Pascal
    //Asignación a una variable
    case parB.Sto of
    stConst : begin
      if parB.val=0 then begin
        //Caso especial. No hace nada
      end else if parB.val=1 then begin
        //Caso especial.
        _LDA(parA.addL);
        _BNE_post(L1);
        _DEC(parA.addH);
_LABEL_post(L1);
        _DEC(parA.addL);
      end else begin
        _SEC;
        _LDA(parA.addL);
        _SBCi(parB.valL);
        _STA(parA.addL);
        _LDA(parA.addH);
        _SBCi(0);
        _STA(parA.addH);
      end;
    end;
    stRamFix: begin
      _SEC;
      _LDA(parA.add);
      _SBC(parB.add);
      _STA(parA.add);
    end;
    stRegister: begin  //ya está en A
      _SEC;
      _SBC(parA.add);   //a - p1 -> a
      //Invierte
      _EORi($ff);
      _CLC;
      _ADCi(1);
      //Devuelve
      _STA(parA.add);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
//  end else if parA.Sto = stExpRef then begin
//    {Este es un caso especial de asignación a un puntero a byte dereferenciado, pero
//    cuando el valor del puntero es una expresión. Algo así como (ptr + 1)^}
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case parB.Sto of
//    stConsta : begin
//      //Asignación normal
//      if parB.val=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        kMOVWF(FSR);  //direcciona
//        _SUBWF(0, toF);
//      end;
//    end;
//    stRamFix: begin
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(parB.add, toW);
//      _SUBWF(0, toF);
//    end;
//    stRegister: begin
//      //La dirección está en la pila y la expresión en A
//      aux := GetAuxRegisterByte;
//      kMOVWF(aux);   //Salva A (p2)
//      //Apunta con p1
//      rVar := GetVarByteFromStk;
//      kMOVF(rVar.adrByte0, toW);  //opera directamente al dato que había en la pila. Deja en A
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(aux, toW);
//      _SUBWF(0, toF);
//      aux.used := false;
//      exit;
//    end;
//    else
//      GenError(MSG_UNSUPPORTED); exit;
//    end;
//  end else if parA.Sto = stVarRef then begin
//    //Asignación a una variable
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case parB.Sto of
//    stConsta : begin
//      //Asignación normal
//      if parB.val=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        //Caso especial de asignación a puntero dereferenciado: variable^
//        kMOVF(parA.add, toW);
//        kMOVWF(FSR);  //direcciona
//        _SUBWF(0, toF);
//      end;
//    end;
//    stRamFix: begin
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      kMOVF(parA.add, toW);
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(parB.add, toW);
//      _SUBWF(0, toF);
//    end;
//    stRegister: begin  //ya está en A
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      aux := GetAuxRegisterByte;
//      kMOVWF(aux);   //Salva A (p2)
//      //Apunta con p1
//      kMOVF(parA.add, toW);
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(aux, toW);
//      _SUBWF(0, toF);
//      aux.used := false;
//    end;
//    else
//      GenError(MSG_UNSUPPORTED); exit;
//    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.ROB_word_gequ_word(fun: TEleExpress);
var
  L1B: integer;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const: begin  //compara constantes. Caso especial
    SetFunConst_bool(fun, parA.val >= parB.val);
  end;
  stConst_Variab: begin
    if parA.val = 65535 then begin
      //Always true
      SetFunConst_bool(fun, true);
      GenWarn('Expression will always be FALSE or TRUE.');
    end else begin
      SetFunExpres(fun, logNormal);
      //Compare MSB
      _LDAi(parA.valH);
      _CMP(parB.addH);
      _BNE_post(L1B);  //MSB1<>MSB2, quit with: C=1 -> var1>var2; C=0 -> var1<var2
      //MSB are equal,compare LSB
      _LDAi(parA.valL);
      _CMP(parB.addL);
  _LABEL_post(L1B);
      //Here if C=1, var>=var2; if C=0, var1<var
      Copy_C_to_A; //Copy C to A
    end;
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun, logNormal);
    requireE;
    _STA(E.addr);  //Sava LSB2
    //Compare MSB
    _LDAi(parA.valH);
    _CMP(H.addr);
    _BNE_post(L1B);  //MSB1<>MSB2, quit with: C=1 -> var1>var2; C=0 -> var1<var2
    //MSB are equal,compare LSB
    _LDAi(parA.valL);
    _CMP(E.addr);
_LABEL_post(L1B);
    //Here if C=1, var>=var2; if C=0, var1<var
    Copy_C_to_A; //Copy C to A
  end;
  stVariab_Const: begin
    if parB.val = 0 then begin
      //Alyway true
      SetFunConst_bool(fun, true);
      GenWarn('Expression will always be FALSE or TRUE.');
    end else begin
      SetFunExpres(fun, logNormal);
      //Compare MSB
      _LDA(parA.addH);
      _CMPi(parB.valH);
      _BNE_post(L1B);  //MSB1<>MSB2, quit with: C=1 -> var1>var2; C=0 -> var1<var2
      //MSB are equal,compare LSB
      _LDA(parA.addL);
      _CMPi(parB.valL);
  _LABEL_post(L1B);
      //Here if C=1, var>=var2; if C=0, var1<var
      Copy_C_to_A; //Copy C to A
    end;
  end;
  stVariab_Variab:begin
    SetFunExpres(fun, logNormal);
    //Compare MSB
    _LDA(parA.addH);
    _CMP(parB.addH);
    _BNE_post(L1B);  //MSB1<>MSB2, quit with: C=1 -> var1>var2; C=0 -> var1<var2
    //MSB are equal,compare LSB
    _LDA(parA.addL);
    _CMP(parB.addL);
_LABEL_post(L1B);
    //Here if C=1, var>=var2; if C=0, var1<var
    Copy_C_to_A; //Copy C to A
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun, logNormal);
    requireE;
    _STA(E.addr);  //Sava LSB2
    //Compare MSB
    _LDA(parA.addH);
    _CMP(H.addr);
    _BNE_post(L1B);  //MSB1<>MSB2, quit with: C=1 -> var1>var2; C=0 -> var1<var2
    //MSB are equal,compare LSB
    _LDA(parA.addL);
    _CMP(E.addr);
_LABEL_post(L1B);
    //Here if C=1, var>=var2; if C=0, var1<var
    Copy_C_to_A; //Copy C to A
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    if parB.val = 0 then begin
      //Alyway true
      SetFunConst_bool(fun, true);
      GenWarn('Expression will always be FALSE or TRUE.');
    end else begin
      SetFunExpres(fun, logNormal);
      requireE;
      _STA(E.addr);  //Sava LSB1
      //Compare MSB
      _LDA(H.addr);
      _CMPi(parB.valH);
      _BNE_post(L1B);  //MSB1<>MSB2, quit with: C=1 -> var1>var2; C=0 -> var1<var2
      //MSB are equal,compare LSB
      _LDA(E.addr);
      _CMPi(parB.valL);
  _LABEL_post(L1B);
      //Here if C=1, var>=var2; if C=0, var1<var
      Copy_C_to_A; //Copy C to A
    end;
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun, logNormal);
    requireE;
    _STA(E.addr);  //Sava LSB1
    //Compare MSB
    _LDA(H.addr);
    _CMP(parB.addH);
    _BNE_post(L1B);  //MSB1<>MSB2, quit with: C=1 -> var1>var2; C=0 -> var1<var2
    //MSB are equal,compare LSB
    _LDA(E.addr);
    _CMP(parB.addL);
_LABEL_post(L1B);
    //Here if C=1, var>=var2; if C=0, var1<var
    Copy_C_to_A; //Copy C to A
  end;
//  stExpres_Expres:begin
//    //la expresión p1 debe estar salvada y p2 en el acumulador
//    parA.SetAsVariab(GetVarByteFromStk, 0);  //Convierte a variable
//    //Luego el caso es similar a stVariab_Expres
//    ROB_byte_great_byte(fun, true);
//    FreeStkRegisterByte;   //libera pila porque ya se usó el dato ahí contenido
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_word_less_word(fun: TEleExpress);
begin
  ROB_word_gequ_word(fun);
  fun.Invert;
end;
procedure TGenCod.ROB_word_great_word(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  fun.elements.Exchange(0,1);
  ROB_word_less_word(fun);
  fun.elements.Exchange(0,1);
end;
procedure TGenCod.ROB_word_lequ_word(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  fun.elements.Exchange(0,1);
  ROB_word_gequ_word(fun);
  fun.elements.Exchange(0,1);
end;
procedure TGenCod.ROB_word_and_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimiza
    SetFunConst_byte(fun, parA.val and parB.val);
  end;
  stConst_Variab: begin
    SetFunExpres(fun);
    _LDAi(parA.valL);
    _AND(parB.addL);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en (A)
    SetFunExpres(fun);
    _ANDi(parA.valL);      //Deja en A
  end;
  stVariab_Const: begin
    SetFunExpres(fun);
    _LDA(parA.addL);
    _ANDi(parB.valL);
  end;
  stVariab_Variab:begin
    SetFunExpres(fun);
    _LDA(parA.addL);
    _AND(parB.addL);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en (_H,A)
    SetFunExpres(fun);
    _AND(parA.add);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _ANDi(parB.valL);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _AND(parB.addL);
  end;
//  stExpres_Expres:begin
//    SetResultExpres(fun);
//    //p1 está salvado en pila y p2 en (A)
//    parA.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
//    //Luego el caso es similar a stVariab_Expres
//    _AND(parA.add);
//    FreeStkRegisterWord;   //libera pila
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_word_and_word(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimiza
    SetFunConst_word(fun, parA.val and parB.val);
  end;
  stConst_Variab: begin
    SetFunExpres(fun);
    _LDAi(parA.valH);
    _AND(parB.addH);
    _STA(H.addr);
    _LDAi(parA.valL);
    _AND(parB.addL);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en (A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _ANDi(parA.valL);
    _PHA;  //Save LSB result
    _LDAi(parA.valH);
    _AND(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  stVariab_Const: begin
    SetFunExpres(fun);
    _LDA(parA.addH);
    _ANDi(parB.valH);
    _STA(H.addr);
    _LDA(parA.addL);
    _ANDi(parB.valL);
  end;
  stVariab_Variab:begin
    SetFunExpres(fun);
    _LDA(parA.addH);
    _AND(parB.addH);
    _STA(H.addr);
    _LDA(parA.addL);
    _AND(parB.addL);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en (_H,A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _AND(parA.addL);
    _PHA;  //Save LSB result
    _LDA(parA.addH);
    _AND(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _ANDi(parB.valL);
    _PHA;  //Save LSB result
    _LDAi(parB.valH);
    _AND(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _AND(parB.addL);
    _PHA;  //Save LSB result
    _LDA(parB.addH);
    _AND(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
//  stExpres_Expres:begin
//    SetResultExpres(fun);
//    //p1 está salvado en pila y p2 en (A)
//    parA.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
//    //Luego el caso es similar a stVariab_Expres
//    _AND(parA.add);
//    FreeStkRegisterWord;   //libera pila
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.word_shift_l(fun: TEleExpress);
{Routine to left shift.
Input:
  (H,A) -> Value to be shifted
  register X -> Number of shift. Must be greater than zero.
Output:
  (H,A) -> Result}
var
  lbl1: integer;
begin
_LABEL_pre(lbl1);
  _ASLa;
  _ROL(H.addr);
  _DEX;
  _BNE_pre(lbl1);
  _RTS;
end;
procedure TGenCod.ROB_word_shl_byte(fun: TEleExpress);
var
  i, L1, L2: Integer;
  AddrUndef: boolean;
  fInLine: boolean;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  fInLine := false;
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimiza
    SetFunConst_byte(fun, parA.val << parB.val);
  end;
  stConst_Variab: begin
    SetFunExpres(fun);
    _LDA(parA.valH);
    _STA(H.addr);  //Load high byte
    _LDA(parA.valL);
    //Loop
    _LDX(parB.add);
    _BEQ_post(L2);  //Protección to zero
//_LABEL_pre(L1);
//      _ASLa;
//      _ROL(H.addr);
//      _DEX;
//    _BNE_pre(L1);
    AddCallerToFromCurr(f_word_shift_l);  //Declare use
    functCall(nil, f_word_shift_l, AddrUndef);  //Use
_LABEL_post(L2);
  end;
  stVariab_Const: begin
    SetFunExpres(fun);
    if parB.val < 4 then begin
      _LDA(parA.addH);
      _STA(H.addr);  //Load high byte
      _LDA(parA.addL);
      for i:=1 to parB.val do begin
        _ASLa;
        _ROL(H.addr);
      end;
    end else begin
      _LDA(parA.addH);
      _STA(H.addr);  //Load high byte
      _LDA(parA.addL);
      //Loop
      _LDXi(parB.val);
      if fInLine then begin
_LABEL_pre(L1);
        _ASLa;
        _ROL(H.addr);
        _DEX;
        _BNE_pre(L1);
      end else begin
        AddCallerToFromCurr(f_word_shift_l);  //Declare use
        functCall(nil, f_word_shift_l, AddrUndef);  //Use
      end;
    end;
  end;
  stVariab_Variab:begin
    SetFunExpres(fun);
    _LDA(parA.addH);
    _STA(H.addr);  //Load high byte
    _LDA(parA.addL);
    //Loop
    _LDX(parB.add);
    _BEQ_post(L2);  //Protección to zero
    if fInLine then begin
_LABEL_pre(L1);
      _ASLa;
      _ROL(H.addr);
      _DEX;
      _BNE_pre(L1);
    end else begin
      AddCallerToFromCurr(f_word_shift_l);  //Declare use
      functCall(nil, f_word_shift_l, AddrUndef);  //Use
    end;
_LABEL_post(L2);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta A
    SetFunExpres(fun);
    _TAX;  //Counter
    _BEQ_post(L2);  //Protección to zero

    _LDA(parA.addH);
    _STA(H.addr);  //Load high byte
    _LDA(parA.addL);
    if fInLine then begin
_LABEL_pre(L1);
      _ASLa;
      _ROL(H.addr);
      _DEX;
      _BNE_pre(L1);
    end else begin
      AddCallerToFromCurr(f_word_shift_l);  //Declare use
      functCall(nil, f_word_shift_l, AddrUndef);  //Use
    end;
_LABEL_post(L2);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    if parB.val < 4 then begin
      for i:=1 to parB.val do begin
        _ASLa;
        _ROL(H.addr);
      end;
    end else begin
      _LDXi(parB.val);
      if fInLine then begin
  _LABEL_pre(L1);
        _ASLa;
        _ROL(H.addr);
        _DEX;
        _BNE_pre(L1);
      end else begin
        AddCallerToFromCurr(f_word_shift_l);  //Declare use
        functCall(nil, f_word_shift_l, AddrUndef);  //Use
      end;
    end;
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,A)
    _LDXi(parB.add);
    if fInLine then begin
_LABEL_pre(L1);
      _ASLa;
      _ROL(H.addr);
      _DEX;
      _BNE_pre(L1);
    end else begin
      AddCallerToFromCurr(f_word_shift_l);  //Declare use
      functCall(nil, f_word_shift_l, AddrUndef);  //Use
    end;
  end;
//  stExpres_Expres:begin
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROB_word_shr_byte(fun: TEleExpress);
var
  i: Integer;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimiza
    SetFunConst_byte(fun, parA.val >> parB.val);
  end;
//  stConst_Variab: begin
//    SetResultExpres(fun);
//    _LDAi(parA.val);
//    _AND(parB.addL);
//  end;
//  stConst_Expres: begin  //la expresión p2 se evaluó y esta en (A)
//    SetResultExpres(fun);
//    _ANDi(parA.valL);      //Deja en A
//  end;
  stVariab_Const: begin
    SetFunExpres(fun);
    if parB.val < 4 then begin
      _LDA(parA.addH);
      _STA(H.addr);  //Load high byte
      _LDA(parA.addL);
      for i:=1 to parB.val do begin
        _LSRa;
        _ROR(H.addr);
      end;
    end else begin
      genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end;
//  stVariab_Variab:begin
//  end;
//  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en (_H,A)
//  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    if parB.val < 4 then begin
      for i:=1 to parB.val do begin
        _LSRa;
        _ROR(H.addr);
      end;
    end else begin
      genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end;
//  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,A)
//  end;
//  stExpres_Expres:begin
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.ROU_not_word(fun: TEleExpress);
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  requireA;
  requireH;
  case par.Sto of
  stConst : begin
    SetFunConst_byte(fun, (not par.value.valInt) and $FFFF);
  end;
  stRamFix: begin
    SetFunExpres(fun);
    _LDA(par.addH);
    _EORi($FF);
    _STA(H.addr);
    _LDA(par.addL);
    _EORi($FF);
  end;
//  stExpres: begin
//    SetROUResultExpres_byte;
//    //////
//  end;
  else
    genError('Not implemented: "%s"', [fun.name]);
  end;
end;
//////////// Operaciones con Char
procedure TGenCod.ROB_char_asig_char(fun: TEleExpress);
begin
  ROB_byte_asig_byte(fun);
end;
procedure TGenCod.ROB_char_asig_string(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Solo se permite asignar constamtes cadenas de 1 caracter
  if parB.Sto <> stConst then begin
    GenError('Cannot assign to this Operand.'); exit;
    exit;
  end;
  if length(parB.value.ValStr) <> 1 then begin
    GenError('String must be 1 char size.'); exit;
    exit;
  end;
  parB.value.ValInt := ord(parB.value.ValStr[1]);  //transform
  ROB_byte_asig_byte(fun);
end;
procedure TGenCod.ROB_char_equal_char(fun: TEleExpress);
begin
  ROB_byte_equal_byte(fun);  //es lo mismo
end;
procedure TGenCod.ROB_char_difer_char(fun: TEleExpress);
begin
  ROB_byte_difer_byte(fun); //es lo mismo
end;
//////////// Operaciones con String
//procedure TGenCod.ROB_string_add_string(Opt: TxpOperation; SetRes: boolean);
//{Implementation of string is incomplete. Just created this oepration to facilitate
//initialization of ARRAY OF chars with string}
//begin
//  case stoOperation(parA, parB) of
//  stConst_Const: begin  //Special case. Comapares constants.
//    SetResultConst(typString);
//    res.valStr := parA.valStr + parB.valStr;
//  end;
//  else
//    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
//  end;
//end;
//procedure TGenCod.ROB_string_add_char(Opt: TxpOperation; SetRes: boolean);
//{Implementation of string is incomplete. Just created this oepration to facilitate
//initialization of ARRAY OF chars with string}
//begin
//  case stoOperation(parA, parB) of
//  stConst_Const: begin  //Special case. Comapares constants.
//    SetResultConst(typString);
//    res.valStr := parA.valStr + chr(parB.valInt);
//  end;
//  else
//    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
//  end;
//end;
procedure TGenCod.LoadIXmult(const idx: TEleVarDec; size: byte; offset: word);
{Load in IX register, the value of the "idx" variable multiplied by "size" and added by
"offset". Parameter "idx" must by byte-size or word-size.
Used to calculate efective address for pointers and arrays.
"idx" must be only byte or word size.}
begin
  if idx.typ.size>2 then begin
    GenError('Not supported this index or pointer type.');
    exit;
  end;
  AddCallerToFromCurr(IX);  //We declare using IX
  if size = 2 then begin
    //Multiply by 2
    if idx.typ.IsByteSize then begin
      _LDXi(0);  //Has not MSB
      _STX(IX.addr+1);   //High(IX) <- High(idx)
      _LDA(idx.addr);    //Load LSB index
      _ASLa;
      _STA(IX.addr);
      _ROL(IX.addr+1);
    end else begin
      _LDX(idx.addr+1);
      _STX(IX.addr+1);   //High(IX) <- High(idx)
      _LDA(idx.addr);    //Load LSB index
      _ASLa;
      _STA(IX.addr);
      _ROL(IX.addr+1);
    end;
    //Add offset
    if offset>0 then begin
      _CLC;
      _ADCi(lo(offset));   //LSB already in A
      _STA(IX.addr);
      _LDA(IX.addr+1);
      _ADCi(hi(offset));
      _STA(IX.addr+1);
    end;
  end else begin
    //Multiply by "n"
    GenError('Not supported this item size.');
  end;
end;
//////////// Pointer operations
procedure TGenCod.ROB_pointer_add_byte(fun: TEleExpress);
{Implementa la suma de un puntero (a cualquier tipo) y un byte.}
//var
//  ptrType: TxpEleType;
begin
//  {Guarda la referencia al tipo puntero, porque:
//  * Se supone que este tipo lo define el usuario y no se tiene predefinido.
//  * Se podrían definir varios tipos de puntero) así que no se tiene una
//  referencia estática
//  * Conviene manejar esto de forma dinámica para dar flexibilidad al lenguaje}
//  ptrType := parA.Typ;   //Se ahce aquí porque después puede cambiar parA.
//  //La suma de un puntero y un byte, se procesa, como una suma de bytes
//  ROB_word_add_byte(fun);
//  //Devuelve byte, oero debe devolver el tipo puntero
//  case fun.Sto of
//  stConsta: res.SetAsConst(ptrType);  //Cambia el tipo a la constante
//  //stRamFix: res.SetAsVariab(res.rVar);
//  {Si devuelve variable, solo hay dos posibilidades:
//   1. Que sea la variable puntero, por lo que no hay nada que hacer, porque ya tiene
//      el tipo puntero.
//   2. Que sea la variable byte (y que la otra era constante puntero 0 = nil). En este
//      caso devolverá el tipo Byte, lo cual tiene cierto sentido.}
//  stRegister: res.SetAsExpres(ptrType);  //Cambia tipo a la expresión
//  end;
end;
procedure TGenCod.ROB_pointer_add_word(fun: TEleExpress);
{Implementa la suma de un puntero (a cualquier tipo) y un byte.}
//var
//  ptrType: TxpEleType;
begin
//  {Guarda la referencia al tipo puntero, porque:
//  * Se supone que este tipo lo define el usuario y no se tiene predefinido.
//  * Se podrían definir varios tipos de puntero) así que no se tiene una
//  referencia estática
//  * Conviene manejar esto de forma dinámica para dar flexibilidad al lenguaje}
//  ptrType := parA.Typ;   //Se hace aquí porque después puede cambiar parA.
//  //La suma de un puntero y un byte, se procesa, como una suma de bytes
//  ROB_word_add_word(fun);
//  //Devuelve byte, oero debe devolver el tipo puntero
//  case fun.Sto of
//  stConsta: res.SetAsConst(ptrType);  //Cambia el tipo a la constante
//  //stRamFix: res.SetAsVariab(res.rVar);
//  {Si devuelve variable, solo hay dos posibilidades:
//   1. Que sea la variable puntero, por lo que no hay nada que hacer, porque ya tiene
//      el tipo puntero.
//   2. Que sea la variable byte (y que la otra era constante puntero 0 = nil). En este
//      caso devolverá el tipo Byte, lo cual tiene cierto sentido.}
//  stRegister: res.SetAsExpres(ptrType);  //Cambia tipo a la expresión
//  end;
end;
procedure TGenCod.ROB_pointer_sub_byte(fun: TEleExpress);
{Implementa la resta de un puntero (a cualquier tipo) y un byte.}
//var
//  ptrType: TxpEleType;
begin
//  //La explicación es la misma que para la rutina ROB_pointer_add_byte
//  ptrType := parA.Typ;
//  ROB_word_sub_byte(fun);
//  case fun.Sto of
//  stConsta  : res.SetAsConst(ptrType);
//  stRegister: res.SetAsExpres(ptrType);
//  end;
end;
procedure TGenCod.ROB_pointer_sub_word(fun: TEleExpress);
{Implementa la resta de un puntero (a cualquier tipo) y un byte.}
//var
//  ptrType: TxpEleType;
begin
//  //La explicación es la misma que para la rutina ROB_pointer_add_byte
//  ptrType := parA.Typ;
//  ROB_word_sub_word(fun);
//  case fun.Sto of
//  stConsta  : res.SetAsConst(ptrType);
//  stRegister: res.SetAsExpres(ptrType);
//  end;
end;
procedure TGenCod.ROU_derefPointer(fun: TEleExpress; SetRes: boolean);
{Implementa el operador de desreferencia "^", para Opr que se supone debe ser
 categoria "tctPointer", es decir, puntero a algún tipo de dato.}
begin
//  case parA.Sto of
//  stConsta : begin
//    //Caso especial. Cuando se tenga algo como: TPunteroAByte($FF)^
//    //Se asume que devuelve una variable de tipo Byte.
//    tmpVar := CreateTmpVar('', typByte);
//    tmpVar.addr := parA.val;  //Fija dirección de constante
//    SetROUResultVariab(fun, tmpVar);
//  end;
//  stRamFix: begin
//    //Caso común: ptrVar^
//    itemType := parA.Typ.ptrType;  //Type of pointed var
//    //By default we generate code as Setter
//    idxVar := CreateTmpVar('', typWord);
//    idxVar.addr := parA.addr;      //Var pointer as word
//    SetROUResultVarRef(idxVar, itemType);
//    //Here the Operand can be stVarRef or stExpRef
//    if OperMode = opmGetter then begin
//      //In mode Getter, we change this to stRegister, because ROP's don't like "strange" storages.
//      //Validation for WR availability, has been done before (It's suposed)
//      LoadToWR(res);  //Load to WR
//      if HayError then exit;
//      res.SetAsExpres(itemType);  //As operand is in WR, it's an expression
//    end;
//  end;
//  stRegister: begin
//    //La expresión Esta en WR, pero es una dirección, no un valor
//    SetROUResultExpRef(parA.Typ);
//  end;
//  else
////////////    genError('Not implemented: "%s"', [Opr.OperationString]);
//  end;
end;
///////////// System functions
procedure TGenCod.codif_1mseg;
//Codifica rutina de retardo de 1mseg.
var
  nCyc1m: word;
  i: Integer;
begin
  PutFwdComm(';1 msec routine.');
  nCyc1m := round(_CLOCK/1000);  //Número de ciclos necesarios para 1 mseg
  if nCyc1m < 10 then begin
    //Tiempo muy pequeño, se genera con NOP
    for i:=1 to nCyc1m div 2 do begin
      _NOP;
    end;
  end else if nCyc1m < 1275 then begin
    //Se puede lograr con bucles de 5 ciclos
    //Lazo de 5 ciclos por vuelta
    _LDXi(nCyc1m div 5);  //2 cycles
  //delay:
    _DEX;       //2 cycles (1 byte)
    _BNE(-3);   //3 cycles in loop (in same page), 2 cycles at end (2 bytes)
  end else begin
    GenError('Clock frequency %d not supported for delay_ms().', [_CLOCK]);
  end;
end;
procedure TGenCod.codif_delay_ms(fun: TEleExpress);
//Codifica rutina de retardo en milisegundos
var
  delay: Word;
  LABEL1, ZERO: integer;
begin
//  PutLabel('__delay_ms');
  PutTopComm('    ;delay routine.');
  //aux := GetAuxRegisterByte;  //Pide un registro libre
  if HayError then exit;
  {Esta rutina recibe los milisegundos en los registros en (H,A) o en (A)
  En cualquier caso, siempre usa el registros H , el acumulador "A" y un reg. auxiliar.
  Se supone que para pasar los parámetros, ya se requirió H, así que no es necesario
  crearlo.}
//  _LDXi(0);     PutComm(' ;enter when parameters in (0,A)');
//  _STX(H);
//  fun.adrr2 := pic.iRam;  {Se hace justo antes de generar código por si se crea
//                          la variable _H}
  _TAY; PutComm(';enter when parameters in (H,A)');
  //Se tiene el número en H,Y
delay:= _PC;
  _TYA;
  _BNE_post(LABEL1);  //label
  //A (and Y) is zero
  _LDA(H.addr);
  _BEQ_post(ZERO); //H is zero too (not decremented in that case)
  _DEC(H.addr);
_LABEL_post(LABEL1);
  _DEY;
  codif_1mseg;   //codifica retardo 1 mseg
  if HayError then exit;
  _JMP(delay);
_LABEL_post(ZERO);
  _RTS();
  //aux.used := false;  //libera registro
end;
procedure TGenCod.fun_delay_ms(fun: TEleExpress);
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  //Se terminó de evaluar un parámetro
/////////////  LoadToWR(res);   //Carga en registro de trabajo ******** ?????
  if HayError then exit;
  if par.Typ = typByte then begin
    //El parámetro byte, debe estar en A
    if fun.idClass = eleFunc then begin
      _JSR(TEleFun(fun).adrr);
    end else begin
      GenError('Cannot get address of %s' + fun.name);
      //_JSR($1234);
    end;
  end else if par.Typ = typWord then begin
    //El parámetro word, debe estar en (H, A)
    if fun.idClass = eleFunc then begin
      _JSR(TEleFun(fun.rfun).adrr2);
    end else begin
      GenError('Cannot get address of %s' + fun.name);
      //_JSR($1234);
    end;
  end else begin
    GenError(MSG_INVAL_PARTYP, [par.Typ.name]);
    exit;
  end;
end;
///////////// INLINE system function
procedure TGenCod.fun_Exit(fun: TEleExpress);
{Se debe dejar en los registros de trabajo, el valor del parámetro indicado.}
var
  curFunTyp: TEleTypeDec;
  parentNod: TEleProgFrame;
  curFun: TEleFun;
  posExit: TSrcPos;
  par: TEleExpress;
//  adrReturn: word;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  //TreeElems.curNode, debe ser de tipo "Body".
  parentNod := TreeElems.CurCodeContainer;  //Se supone que nunca debería fallar
  posExit := GetSrcPos;  //Guarda para el AddExitCall()
  if parentNod.idClass = eleProg then begin
    //Es el cuerpo del programa principal
    _RTS;   //Así se termina un programa en PicPas
  end else if parentNod.idClass = eleFunc then begin
    //Es el caso común, un exit() en procedimientos.
    //"parentNod" debe ser de tipo TxpEleFun
    curFun := TEleFun(parentNod);
    if curFun.IsInterrupt then begin
      GenError('Cannot use exit() in an INTERRUPT.');
      exit;
    end;
    //Codifica el retorno
    curFunTyp := curFun.retType;
    if curFunTyp = typNull then begin
      //No retorna valores. Es solo procedimiento
      _RTS;
      //No hay nada, más que hacer
    end else begin
      //El resultado de la expresión está en "par".
//***** Reemplazar "res" por "par".
      if curFunTyp <> par.Typ then begin
        GenError('Expected a "%s" expression.', [curFunTyp.name]);
        exit;
      end;
      LoadToWR(fun);  //Carga expresión en WR y genera RTS
      _RTS;
    end;
  end else begin
    //Faltaría implementar en cuerpo de TxpEleUni
    GenError('Syntax error.');
  end;
  //Lleva el registro de las llamadas a exit()
//  if FirstPass then begin
//    parentNod.AddExitCall(posExit, parentNod.CurrBlockID);  *** CurrBlockID se ha eliminado porque ya no debería ser necesario con el nueo AST
//  end;
  SetFunNull(fun);  //No es función
end;
procedure TGenCod.fun_Inc(fun: TEleExpress);
var
  LABEL1: integer;
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  case par.opType of
  otConst: begin GenError('Cannot increase a constant.');exit; end;
  otExpres: begin GenError('Cannot increase a function/procedure or expression result.'); exit; end;
  otVariab: ; //The only valid type.
  else  //Not expected to happen
    GenError('Unimplemented.'); exit;
  end;
  //otVariab
  case par.Sto of
  stConst: begin
    GenError('Cannot increase a constant.'); exit;
  end;
  stRamFix: begin  //A common variable
    if (par.Typ = typByte) or (par.Typ = typChar) then begin
      _INC(par.rvar.addr);
    end else if par.Typ = typWord then begin
      _INC(par.rVar.addr);
      _BNE_post(LABEL1);  //label
      _INC(par.rVar.addr+1);
_LABEL_post(LABEL1);
    end else if par.Typ.catType = tctPointer then begin
      //Es puntero corto
      _INC(par.rVar.addr);
    end else begin
      GenError(MSG_INVAL_PARTYP, [par.Typ.name]);
      exit;
    end;
  end;
  stRegister: begin
    if (par.Typ = typByte) or (par.Typ = typChar) then begin
      _CLC;
      _ADCi(1);
    end else if par.Typ = typWord then begin
      _CLC;
      _ADCi(1);
      _BNE_post(LABEL1);  //label
      _INC(H.addr);
_LABEL_post(LABEL1);
    end else begin
      GenError(MSG_INVAL_PARTYP, [par.Typ.name]);
      exit;
    end;
  end;
  stRegistA: begin
    _CLC;
    _ADCi(1);
  end;
  stRegistX: begin _INX; end;
  stRegistY: begin _INY; end;
  else
    genError('Not implemented "%s()" for operands "%s".',
             [fun.name, par.StoAsStr], par.srcDec);
  end;
end;
procedure TGenCod.fun_Dec(fun: TEleExpress);
var
  lbl1: integer;
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  case par.opType of
  otConst: begin GenError('Cannot decrease a constant.');exit; end;
  otExpres: begin GenError('Cannot decrease a function/procedure or expression result.'); exit; end;
  otVariab: ; //The only valid type.
  else  //Not expected to happen
    GenError('Unimplemented.'); exit;
  end;
  //otVariab
  case par.Sto of
  stConst: begin
    GenError('Cannot decrease a constant.'); exit;
  end;
  stRamFix: begin  //A common variable
    if (par.Typ= typByte) or (par.Typ = typChar) then begin
      _DEC(par.rVar.addr);
    end else if par.Typ = typWord then begin
      _LDA(par.rVar.addr);
      _BNE_post(lbl1);
      _DEC(par.rVar.addr+1);
_LABEL_post(lbl1);
      _DEC(par.rVar.addr);
    end else if par.Typ.catType = tctPointer then begin
      //Es puntero corto
      _DEC(par.rVar.addr);
    end else begin
      GenError(MSG_INVAL_PARTYP, [par.Typ.name]);
      exit;
    end;
  end;
  //stRegister: begin  //To complete.
  //end;
  stRegistA: begin
    _SEC;
    _SBCi(1);
  end;
  stRegistX: begin _DEX; end;
  stRegistY: begin _DEY; end;
  else
    genError('Not implemented "%s()" for operands "%s".',
             [fun.name, par.StoAsStr], par.srcDec);
  end;
end;
procedure TGenCod.SetOrig(fun: TEleExpress);
{Define el origen para colocar el código binario.}
// NO ES MUY ÚTIL PORQUE EL CAMBIO DE ORIGEN DEBE HACERSE ANTES DEL CÓDIGO
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  case par.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    pic.iRam := fun.value.valInt;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
  SetFunNull(fun);  //Is not function.
end;
procedure TGenCod.fun_Ord(fun: TEleExpress);
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  case par.Sto of
  stConst : begin
    if par.Typ = typChar then begin
      SetFunConst(fun);
      fun.value.valInt := par.value.ValInt;
    end else if par.Typ = typBool then begin
      SetFunConst(fun);
      if par.value.ValBool then fun.value.ValInt := 0 else fun.value.ValInt := 1;
    end else begin
      GenError('Cannot get the ordinal of %s.', [par.Typ.name]); exit;
    end;
  end;
  stRamFix: begin
    if par.Typ = typChar then begin
      //Sigue siendo variable
      SetFunVariab(fun, par.add);  //Actualiza "par"
    end else begin
      SetFunExpres(fun);   //A default operand type
      GenError('Cannot convert to ordinal.'); exit;
    end;
  end;
  stRegister: begin  //se asume que ya está en (A)
    if par.Typ = typChar then begin
      //Es la misma expresión, solo que ahora es Byte.
      SetFunExpres(fun);
    end else begin
      SetFunExpres(fun); //Set a default operand type
      GenError('Cannot convert to ordinal.'); exit;
    end;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
end;
procedure TGenCod.fun_Chr(fun: TEleExpress);
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  case par.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if par.Typ = typByte then begin
      SetFunConst(fun);
      fun.value.valInt := par.value.ValInt;
    end else if par.Typ = typWord then begin
      SetFunConst(fun);
      fun.value.valInt := par.value.valInt and $FF;
    end else begin
      GenError('Cannot convert this to char.'); exit;
    end;
  end;
  stRamFix: begin
    if par.Typ.IsByteSize then begin
      //Sigue siendo variable
      SetFunVariab(fun, par.add);
    end else if par.Typ = typWord then begin
      //Crea variable que apunte al byte bajo
      SetFunVariab(fun, par.add);
    end else begin
      SetFunExpres(fun);   //A default operand type
      GenError('Cannot convert to char.'); exit;
    end;
  end;
  stRegister: begin  //se asume que ya está en (A)
    if par.Typ = typByte then begin
      //Es la misma expresión, solo que ahora es Char.
      SetFunExpres(fun);
    end else if par.Typ = typWord then begin
      //Ya está en A el byte bajo
      SetFunExpres(fun);
    end else begin
      SetFunExpres(fun); //Set a default operand type
      GenError('Cannot convert this to char.'); exit;
    end;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
end;
procedure TGenCod.fun_Byte(fun: TEleExpress);
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  if HayError then exit;   //aborta
  case par.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if par.Typ = typByte then begin
      //ya es Byte
      SetFunConst(fun);  //It's already byte
      fun.value.valInt := par.value.ValInt;
    end else if par.Typ = typChar then begin
      SetFunConst(fun);  //It's already byte
      fun.value.valInt := par.value.ValInt;
    end else if par.Typ = typWord then begin
      SetFunConst(fun);  //It's already byte
      fun.value.valInt := par.value.valInt and $FF;
    end else begin
      GenError('Cannot convert this to byte.'); exit;
    end;
  end;
  stRamFix: begin
    if par.Typ.IsByteSize then begin
      //Es lo mismo.
      SetFunVariab(fun, par.add);  //Byte type
    end else if par.Typ = typWord then begin
      //Crea variable que apunte al byte bajo
      SetFunVariab(fun, par.add);
    end else begin
      SetFunExpres(fun);   //A default operand type
      GenError('Cannot convert to byte.'); exit;
    end;
  end;
  stRegister: begin  //se asume que ya está en (A)
    if par.Typ.IsByteSize then begin
      //Ya está en A y ya es Byte
      SetFunExpres(fun);
    end else if par.Typ = typWord then begin
      //Ya está en A el byte bajo
      SetFunExpres(fun);
    end else begin
      SetFunExpres(fun); //Set a default operand type
      GenError('Cannot convert this to byte.'); exit;
    end;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
end;
procedure TGenCod.fun_Word(fun: TEleExpress);
var
  tmpVar: TEleVarDec;
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  case par.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if par.Typ = typByte then begin
      SetFunConst(fun);
      fun.value.ValInt := par.value.ValInt;  //Copy value
    end else if par.Typ = typChar then begin
      SetFunConst(fun);
      fun.value.ValInt := par.value.ValInt;  //Copy value
    end else if par.Typ = typWord then begin
      //Already Word
      SetFunConst(fun);
      fun.value.ValInt := par.value.ValInt;  //Copy value
    end else begin
      GenError('Cannot convert this constant to word.'); exit;
    end;
  end;
  stRamFix: begin
    requireH;
    if par.Typ.IsByteSize then begin
      SetFunExpres(fun);  //No podemos devolver variable. Pero sí expresión
      _LDAi(0);
      _STA(H.addr);
      _LDA(par.rVar.addr);
    end else if par.Typ = typWord then begin
      //ya es Word
      SetFunVariab(fun, par.add);
    end else if par.Typ.IsWordSize then begin
      //Has 2 bytes long, like pointers
      SetFunVariab(fun, par.add);
      {We could generate stRegister, but we prefer generate a variable, for simplicity
      and to have the possibility of assign: word(x) := ...}
    end else begin
      SetFunExpres(fun);   //A default operand type
      GenError('Cannot convert this variable to word.'); exit;
    end;
  end;
  stRegister: begin  //se asume que ya está en (A)
    requireH;
    if par.Typ = typByte then begin
      SetFunExpres(fun);
      //Ya está en A el byte bajo
      _LDXi(0);
      _STX(H.addr);
    end else if par.Typ = typChar then begin
      SetFunExpres(fun);
      //Ya está en A el byte bajo
      _LDXi(0);
      _STX(H.addr);
    end else if par.Typ = typWord then begin
//      Ya es word
    end else begin
      GenError('Cannot convert expression to word.'); exit;
    end;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
end;
procedure TGenCod.fun_Addr(fun: TEleExpress);
{Returns the address of a datatype.}
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  case par.opType of
  otVariab: begin
    //Es una variable simple. Una variable tiene dirección fija
    if par.Sto = stRamFix then begin
      {This is a special case where the result operand type, depends on if
      par is allocated.}
      if par.allocated then begin
        SetFunConst(fun);
        fun.value.valInt := par.add;
      end else begin
        {No allocated. We keep this as an expression in order to force the
        evaluation later, when the address must be defined.}
        SetFunExpres(fun);
      end;
    end else begin
      genError('Cannot obtain address variable.');
      exit;
    end;
  end;
  otConst: begin
    SetFunConst(fun);
    if (par.typ = typByte) or (par.typ = typWord) then begin
      //For numeric constant, takes the value a as address,
      fun.value.valInt := par.Value.valInt;
    end else begin
      genError('Cannot obtain address this constant.');
      exit;
    end;
  end;
  //otExpres: begin
  { TODO : Faltaría implementar etso, después de que se defina el campo "coded" (o similar) en los TxpEleExpress para indciar cuando la función se ha implementado y codificado en memoria. }
    ////Should be a function call
    //if par.coded then begin
    //  xfun := par.fun;
    //  if xfun.codInline <> nil then begin
    //    //Inline Function
    //    genError('Cannot obtain address of a INLINE function.');
    //    exit;
    //  end else begin
    //    //Normal function
    //    SetResultConst(fun);  //Lo más cercano al POINTER de Pascal o al ADDRESS de Modula-2
    //    if xfun.coded then begin //We have a real address
    //      fun.value.valInt := xfun.adrr;
    //    end else begin
    //      //No tiene dirección. Debe ser forward (o declaración en INTERFACE).
    //      //Por ahora no se implementa. Debe ser algo como xfun.AddAddresPend(pic.iRam-2);
    //      genError('Cannot obtain address this operand.');
    //      exit;
    //    end;
    //
    //  end;
    //end;
  //end;
  else
    //Shouldn't happen
    genError('Design error.');
  end;
end;
//Routines to implement arrays and pointers
procedure TGenCod.arrayLow(fun: TEleExpress);
//Devuelve el índice mínimo de un arreglo
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  SetFunConst(fun);
  fun.value.ValInt := 0;
end;
procedure TGenCod.arrayHigh(fun: TEleExpress);
//Devuelve el índice máximo de un arreglo
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  SetFunConst(fun);
  fun.value.ValInt := par.Typ.nItems-1;
end;
procedure TGenCod.arrayLength(fun: TEleExpress);
//Devuelve la cantidad de elementos de un arreglo
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  SetFunConst(fun);
  fun.value.ValInt := par.Typ.nItems;
end;
procedure TGenCod.ROB_arr_asig_arr(fun: TEleExpress);
{Array assigment.}
var
  nItems, itSize, i: Integer;
  nBytes, des: Integer;
  itType: TEleTypeDec;
  src: Word;
  //tmpvar: TEleVarDec;
  values: array of TConsValue;
//  opr1: TxpOperator;
  startAddr, j2: integer;
  parA, parB: TEleExpress;
  buffer: TCPURam;
begin
  SetFunNull(fun);  //In Pascal an assigment doesn't return type.
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  if parA.Typ.nItems <> parB.Typ.nItems then begin
    GenError('Array sizes doesn''t match.', fun.srcDec);
    exit;
  end;
  if parA.Sto = stRamFix then begin
    nItems := parA.Typ.nItems;
    nBytes := parA.rVar.typ.size;
    itType := parA.rVar.typ.itmType;
    itSize := itType.size;
    case parB.Sto of
    stConst: begin
      if nBytes < 5 then begin
        setlength(buffer, 5);  //Temporal space for constant.
        //Just a little bytes
        WriteVaLueToRAM(@buffer, 0, parA.Typ, parB.value);
        //values := parB.Value.items;
        for i:=0 to nItems-1 do begin
          _LDAi(buffer[i].value);
          _STA(parA.add+i);
        end;
      end else if nBytes< 256 then begin
        //Several ítems, we first write Op2 in RAM.
        CreateValueInCode(parB.Typ, parB.Value, startAddr);
        //Now we have Op2 created in RAM. Lets move.
        _LDXi(nBytes);
_LABEL_pre(j2);
        pic.codAsm(i_LDA, aAbsolutX, (startAddr-1) and $FFFF);  //Fix address to fit the index loop
        pic.codAsm(i_STA, aAbsolutX, (parA.rVar.addr-1) and $FFFF);  //Fix address to fit the index loop
        _DEX;
        _BNE_pre(j2);
      end else begin
        GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
      end;
    end;
    stRamFix: begin
      if nBytes < 5 then begin
        des:=parA.rVar.addr;
        for src:=parB.rVar.addr to parB.rVar.addr+nBytes-1 do begin
          _LDA(src);
          _STA(des);
          inc(des);
        end;
      end else if nBytes< 256 then begin
        //Several ítems, we will use a loop to copy.
        //Now we have the variable created in RAM. Lets move
        _LDXi(nBytes);
_LABEL_pre(j2);
        pic.codAsm(i_LDA, aAbsolutX, (parB.rVar.addr-1) and $FFFF);  //Fix address to fit the index loop
        pic.codAsm(i_STA, aAbsolutX, (parA.rVar.addr-1) and $FFFF);  //Fix address to fit the index loop
        _DEX;
        _BNE_pre(j2);
      end else begin
        GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
      end;
    end;
//    stRegister: begin   //se asume que está en A
//      SetResultExpres(fun);  //Realmente, el resultado no es importante
//      _STA(parA.addL);
//      _LDA(0);
//      _STA(parA.addH);
//    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end else begin
    GenError('Cannot assign to this Operand.', fun.srcDec);
    exit;
  end;
end;
procedure TGenCod.GenCodArrayGetItem(fun: TEleExpress);
{Función que devuelve el valor indexado del arreglo. }
var
  arrVar, idx: TEleExpress;
  itemType: TEleTypeDec;
begin
  arrVar := TEleExpress(fun.elements[0]);
  idx := TEleExpress(fun.elements[1]);
  if arrVar.sto = stRamFix then begin
    //Applied to a variable array. The normal.
    itemType := arrVar.Typ.itmType; //Reference to the item type
    //Generate code according to the index storage.
    case idx.Sto of
    stConst: begin  //ïndice constante
      //if ModeRequire then exit;
      if arrVar.allocated then begin
        SetFunVariab(fun, arrVar.add + idx.value.valInt * itemType.size);
      end else begin
        //Not yet allocated. We keep as expression to simplify later.
        SetFunExpres(fun);
      end;
    end;
    stRamFix: begin  //Indexado por variable
      SetFunVariab_RamVarOf(fun, idx.rvar, arrVar.add); //Index by variable and an offset
      //if idx.Typ.IsByteSize then begin
      //  //Index is byte-size variable
      //  if itemType.IsByteSize then begin
      //    //And item is byte size, so it's easy to index in 6502
      //    _LDX(idx.rVar.addr);
      //    if arrVar.add<256 then begin
      //      pic.codAsm(i_LDA, aZeroPagX, arrVar.add);
      //    end else begin
      //      pic.codAsm(i_LDA, aAbsolutX, arrVar.add);
      //    end;
      //  end else if itemType.IsWordSize then begin
      //    //Item is word size. We need to multiply index by 2 and load indexed.
      //    //WARNING this is "Auto-modifiying" code.
      //    _LDA(idx.rVar.addr);  //Load index
      //    pic.codAsm(i_STA, aAbsolute, _PC + 15); //Store forward
      //    _LDAi(0);  //Load virtual MSB index
      //    pic.codAsm(i_STA, aAbsolute, _PC + 11); //Store forward
      //    pic.codAsm(i_ASL, aAbsolute, _PC + 7);  //Shift operand of LDA
      //    PIC.codAsm(i_ROL, aAbsolute, _PC + 5);  //Shift operand of LDA
      //    pic.codAsm(i_LDA, aAbsolute, 0); //Store forward (DANGER)
      //    //Could be optimized getting a Zero page index
      //  end else begin
      //    //Here we need to multiply the index by the item size.
      //    GenError('Too complex item.');
      //  end;
//    //  end else if idx.Typ.IsWordSize then begin
//    //    //Index is word-size variable
//    //    if itemType.IsByteSize then begin
//    //      //Item is byte size. We need to index with a word.
//    //      if idx.rVar.addr<256 then begin
//    //        //Index in zero page
//    //        _LDYi(0);
//    //        pic.codAsm(i_LDA, aIndirecY, idx.rVar.addr);
//    //      end else begin
//    //        //Index is out of zero page
//    //        //WARNING this is "Auto-modifiying" code.
//    //        _CLC;
//    //        _LDA(idx.rVar.addr);  //Load index
//    //        _ADCi(arrVar.add and $FF);
//    //        pic.codAsm(i_STA, aAbsolute, _PC + 12); //Store forward
//    //        _LDA(idx.rVar.addr+1);  //Load MSB index
//    //        _ADCi( arrVar.add >> 8 );
//    //        pic.codAsm(i_STA, aAbsolute, _PC + 5); //Store forward
//    //        //Modifiying instruction
//    //        pic.codAsm(i_LDA, aAbsolute, 0);
//    //      end;
//    //    end else if itemType.IsWordSize then begin
//    //      //Item is word size. We need to multiply index by 2 and load indexed.
//    //      //WARNING this is "Auto-modifiying" code.
//    //      _LDA(idx.rVar.addr);  //Load index
//    //      pic.codAsm(i_STA, aAbsolute, _PC + 16); //Store forward
//    //      _LDA(idx.rVar.addr+1);  //Load virtual MSB index
//    //      pic.codAsm(i_STA, aAbsolute, _PC + 11); //Store forward
//    //      pic.codAsm(i_ASL, aAbsolute, _PC + 7);  //Shift operand of LDA
//    //      PIC.codAsm(i_ROL, aAbsolute, _pc + 5);  //Shift operand of LDA
//    //      pic.codAsm(i_LDA, aAbsolute, 0); //Store forward
//    //    end else begin
//    //      //Here we need to multiply the index by the item size.
//    //      GenError('Too complex item.');
//    //    end;
      //end else begin
      //  GenError('Not supported this index.');
      //end;
    end;
//    stRegister: begin
//      if idx.Typ.IsByteSize then begin
//        //Change stRegister to stRamFix
//        requireE;
//        idx.SetAsVariab(E);
//        if  LastIsTXA then begin  //Move result from X
//          dec(pic.iRam);  //erase last TXA
//          _STX(E.addr);
//        end else begin  //Result is in A
//          _STA(E.addr);
//        end;
//        RTstate := nil;   //Now it's not using WR
//      end;
//    end;
    else
      GenError('Not supported this index.');
    end;
  end else begin
    GenError('Cannot index a constant array.');
  end;
end;
procedure TGenCod.GenCodArraySetItem(const OpPtr: pointer);
{Función que devuelve el valor indexado del arreglo para escritura. Devuelve el resultado
como un operando en "res".}
begin
//  if not GetIdxParArray(WithBrack, idx) then exit;
//  //Procesa
//  Op := OpPtr;
//  if Op^.Sto = stRamFix then begin
//    //Applied to a variable array. The normal.
//    arrVar := Op^.rVar;        //Reference to variable
//    itemType := arrVar.typ.itmType; //Reference to the item type
//    //Generate code according to the index storage.
//    case idx.Sto of
//    stConsta: begin  //Constant index
//        tmpVar := CreateTmpVar('', itemType);  //VAriable del mismo tipo
//        tmpVar.addr := arrVar.addr + idx.valInt * itemType.size;
//        SetResultVariab(tmpVar);
//      end;
//    stRamFix: begin  //Indexed by variable
//      //Index is byte-size variable
//      if itemType.IsByteSize then begin
//        //This is important because we know the variable can direct the current position
//        SetResultVarConRef(idx.rVar, arrVar.addr, itemType);  //Devuelve el mismo tipo
//      end else begin
//        SetResultExpRef(itemType);  //We don't need to check WR here in setter.
//        //Item is word size. We multiply index and add offset in IX.
//        LoadIXmult(idx.rVar, itemType.size, arrVar.addr);
//        if HayError then exit;
//      end;
//    end;
//    stRegister: begin  //Indexed by expression
//      //stRegister must return the address in IX register
//      if idx.Typ.IsByteSize then begin
//        //Index is byte-size expresión (Loaded in A).
//        if itemType.IsByteSize then begin
//          SetResultExpRef(itemType);
//          AddCallerToFromCurr(IX);  //We declare using IX
//          //We add A to base address of array -> IX
//          _CLC;
//          _ADCi(arrVar.addr and $FF);
//          _STA(IX.addr);  //Save
//          _LDAi(arrVar.addr >> 8);
//          _ADCi(0);
//          _STA(IX.addr+1);
//        end else begin
//          GenError('Not supported this item size.');
//        end;
//      end else if idx.Typ.IsWordSize then begin
//        //Index is word-size expresión (Loaded in H,A).
//        if itemType.IsByteSize then begin
//          SetResultExpRef(itemType);
//          AddCallerToFromCurr(IX);  //We declare using IX
//          //We add H,A to base address of array -> IX
//          _CLC;
//          _ADCi(arrVar.addr and $FF);
//          _STA(IX.addr);  //Save
//          _LDAi(arrVar.addr >> 8);
//          _ADC(H.addr);
//          _STA(IX.addr+1);
//        end else begin
//          GenError('Not supported item size.');
//        end;
//      end else begin
//        GenError('Not supported this index.');
//      end;
//    end
//    else
//      GenError('Not supported this index.');
//    end;
//  end else begin
//    GenError('Cannot index a constant array.');
//  end;
//  if WithBrack then begin
//    if not CaptureTok(']') then exit;
//  end else begin
//    if not CaptureTok(')') then exit;
//  end;
end;
procedure TGenCod.GenCodArrayClear(fun: TEleExpress);
{Used to clear all items of an array operand.}
var
  par: TEleExpress;
  n: Integer;
  LABEL1: Word;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  SetFunNull(fun);
//  //Return the same operand
//  SetResultVariab(fun, par.add);
//  fun.Typ := par.Typ;
//  fun.Sto := par.Sto;
  //Clear the array
  case par.Sto of
  stRamFix: begin
    n := par.Typ.nItems;
    if n = 0 then exit;  //Nothing to clear
    if n > 0 then begin
      if n = 1 then begin   //Just one byte
        _LDAi(0);
        _STA(par.add);
      end else if n = 2 then begin  //Es de 2 bytes
        _LDAi(0);
        _STA(par.add);
        _STA(par.add+1);
      end else if n = 3 then begin  //Es de 3 bytes
        _LDAi(0);
        _STA(par.add);
        _STA(par.add+1);
        _STA(par.add+2);
      end else if n = 4 then begin  //Es de 4 bytes
        _LDAi(0);
        _STA(par.add);
        _STA(par.add+1);
        _STA(par.add+2);
        _STA(par.add+3);
      end else if n <=256 then begin  //Tamaño pequeño
        _LDAi(0);
        _LDXi(n and $FF);
LABEL1 := _PC;
        if par.add <256 then pic.codAsm(i_STA, aZeroPagX, par.add)  //STA has not aZeroPagY
        else pic.codAsm(i_STA, aAbsolutX, par.add-1);
        _DEX;
        _BNE(LABEL1 - _PC - 2);
      end else begin  //Tamaño mayor
        //Implementa lazo, usando A como índice
        GenError('Cannot clear a big array');
      end;
    end else begin

    end;
  end;
  stConst: begin
    GenError('Cannot clear a constant array');
  end
  else
    GenError('Cannot clear this array');
  end;
end;
procedure TGenCod.DefineShortPointer(etyp: TEleTypeDec);
{Configura las operaciones que definen la aritmética de punteros.}
//var
//  opr: TxpOperator;
begin
  //Asignación desde Byte y Puntero
//  opr:=etyp.CreateBinaryOperator(':=',2,'_set');
//  opr.CreateOperation(typByte, @ROB_byte_asig_byte);
//  opr.CreateOperation(etyp   , @ROB_byte_asig_byte);
//  //Agrega a los bytes, la posibilidad de ser asignados por punteros
//  typByte.operAsign.CreateOperation(etyp, @ROB_byte_asig_byte);
//
//  opr:=etyp.CreateBinaryOperator('=',3,'equal');  //asignación
//  opr.CreateOperation(typByte, @ROB_byte_equal_byte);
//  opr:=etyp.CreateBinaryOperator('+',4,'add');  //suma
//  opr.CreateOperation(typByte, @ROB_pointer_add_byte);
//  opr:=etyp.CreateBinaryOperator('-',4,'add');  //resta
//  opr.CreateOperation(typByte, @ROB_pointer_sub_byte);
//
//  etyp.CreateUnaryPostOperator('^',6,'deref', @ROU_derefPointer);  //dereferencia
end;
procedure TGenCod.DefinePointer(etyp: TEleTypeDec);
{Set operations that defines pointers aritmethic.}
//var
//  opr: TxpOperator;
begin
  //Asignación desde Byte y Puntero
//  opr:=etyp.CreateBinaryOperator(':=',2,'_set');
//  opr.CreateOperation(typWord, @ROB_word_asig_word);
//  opr.CreateOperation(etyp   , @ROB_word_asig_word);
//  //Agrega a los word, la posibilidad de ser asignados por punteros
//  typWord.operAsign.CreateOperation(etyp, @ROB_word_asig_word);
//
//  opr:=etyp.CreateBinaryOperator('=',3,'equal');  //asignación
//  opr.CreateOperation(typWord, @ROB_word_equal_word);
//  opr:=etyp.CreateBinaryOperator('+',4,'add');  //suma
//  opr.CreateOperation(typWord, @ROB_pointer_add_word);
//  opr.CreateOperation(typByte, @ROB_pointer_add_byte);
//  opr:=etyp.CreateBinaryOperator('-',4,'add');  //resta
//  opr.CreateOperation(typWord, @ROB_pointer_sub_word);
//  opr.CreateOperation(typByte, @ROB_pointer_sub_byte);
//  opr:=etyp.CreateBinaryOperator('+=',2,'aadd');  //suma
//  opr.CreateOperation(typWord, @ROB_word_aadd_word);
//  opr.CreateOperation(typByte, @ROB_word_aadd_byte);
//
//  etyp.CreateUnaryPreOperator('@', 6, 'addr', @ROU_address); //defined in all types
//  etyp.CreateUnaryPostOperator('^',6, 'deref', @ROU_derefPointer);  //dereferencia
end;
procedure TGenCod.DefineArray(etyp: TEleTypeDec);
var
  cons: TEleConsDec;
  expr: TEleExpress;
begin
  //Create assigement method
  CreateBOMethod(etyp, ':=', '_set', etyp, typNull, @ROB_arr_asig_arr);
  //Create attribute "low" as constant.
  cons := AddConstantAndOpen('low', typNull, GetSrcPos);  //No type defined here.
  if HayError then exit;   //Can be duplicated
  expr := GenExpressionConstByte('0', 0, GetSrcPos);
  cons.typ := expr.Typ;
  cons.value.ValInt := 0;
  cons.evaluated := true;
  TreeElems.CloseElement;  //Close constant.
  //Create methods
//  CreateUOMethod(etyp, '', 'length', typByte, @arrayLength);
//  CreateUOMethod(etyp, '', 'low'   , typByte, @arrayLow);
  CreateUOMethod(etyp, '', 'high'  , typByte, @arrayHigh);
  CreateUOMethod(etyp, '', 'clear' , typNull, @GenCodArrayClear);
  CreateBOMethod(etyp, '', '_getitem', typByte, etyp.itmType, @GenCodArrayGetItem);

//  etyp.CreateField('item'  , @GenCodArrayGetItem, @GenCodArraySetItem);
//  etyp.CreateUnaryPreOperator('@', 6, 'addr', @ROU_address); //defined in all types
end;
procedure TGenCod.ValidRAMaddr(addr: integer);
{Validate a physical RAM address. If error generate error.}
begin
  if (addr<0) or (addr>$ffff) then begin
    //Debe set Word
    GenError(ER_INV_MEMADDR);
    exit;
  end;
  if not pic.ValidRAMaddr(addr) then begin
    GenError(ER_INV_MAD_DEV);
    exit;
  end;
end;
procedure TGenCod.DefCompiler;
{}
//var
//  opr: TxpOperator;
begin
  //Define métodos a usar
  OnExprStart := @expr_start;
  OnExprEnd := @expr_End;

  ///////////Crea tipos
//  ClearSystemTypes;
//  //////////////// Boolean type /////////////
//  typBool := CreateSysType('boolean',t_uinteger,1);   //de 1 byte
//  typBool.OnLoadToWR   := @bool_LoadToRT;
//  typBool.OnDefRegister:= @byte_DefineRegisters;
//  typBool.OnSaveToStk  := @byte_SaveToStk;
//  //typBool.OnReadFromStk :=
//  //////////////// Byte type /////////////
//  typByte := CreateSysType('byte',t_uinteger,1);   //de 1 byte
//  typByte.OnLoadToWR   := @byte_LoadToRT;
//  typByte.OnDefRegister:= @byte_DefineRegisters;
//  typByte.OnSaveToStk  := @byte_SaveToStk;
//  //typByte.OnReadFromStk :=
//
//  //////////////// Char type /////////////
//  //Tipo caracter
//  typChar := CreateSysType('char',t_uinteger,1);   //de 1 byte. Se crea como uinteger para leer/escribir su valor como número
//  typChar.OnLoadToWR   := @byte_LoadToRT;  //Es lo mismo
//  typChar.OnDefRegister:= @byte_DefineRegisters;  //Es lo mismo
//  typChar.OnSaveToStk  := @byte_SaveToStk; //Es lo mismo
//
//  //////////////// Word type /////////////
//  //Tipo numérico de dos bytes
//  typWord := CreateSysType('word',t_uinteger,2);   //de 2 bytes
//  typWord.OnLoadToWR   := @word_LoadToRT;
//  typWord.OnDefRegister:= @word_DefineRegisters;
//  typWord.OnSaveToStk  := @word_SaveToStk;
//
//  typWord.CreateField('Low' , @word_Low , @word_Low);
//  typWord.CreateField('High', @word_High, @word_High);
//
end;
procedure TGenCod.AddParam(var pars: TxpParFuncArray; parName: string; const srcPos: TSrcPos;
                   typ0: TEleTypeDec; adicDec: TxpAdicDeclar);
//Create a new parameter to the function.
var
  n: Integer;
begin
  //Add record to the array
  n := high(pars)+1;
  setlength(pars, n+1);
  pars[n].name := parName;  //Name is not important
  pars[n].srcPos := srcPos;
  pars[n].typ  := typ0;  //Agrega referencia
  pars[n].adicVar.hasAdic := adicDec;
  pars[n].adicVar.hasInit := false;
end;
function TGenCod.CreateSystemFunction(name: string; retType: TEleTypeDec; const srcPos: TSrcPos;
                              const pars: TxpParFuncArray;
                              pCompile: TxpCodInline): TEleFun;
{Create a new system function in the current element of the Syntax Tree.
 Returns the reference to the function created.}
var
   fundec: TEleFunDec;
begin
  //Add declaration
  curLocation := locInterface;
  fundec := AddFunctionDEC(name, retType, srcPos, pars, false);
  //Implementation
  {Note that implementation is added always after declarartion. It's not the usual
  in common units, where all declarations are first}
  curLocation := locImplement;
  Result := AddFunctionIMP(name, retType, srcPos, fundec);
  //Here variables can be added
  {Create a body, to be uniform with normal function and for have a space where
  compile code and access to posible variables or other elements.}
  TreeElems.AddElementBodyAndOpen(SrcPos);  //Create body
  Result.codInline := pCompile;  //Set routine to geenrate code
  TreeElems.CloseElement;  //Close body
  TreeElems.CloseElement;  //Close function implementation
end;
function TGenCod.CreateBOMethod(
                      clsType: TEleTypeDec;   //Base type where the method bellow.
                      opr     : string;      //Opertaor associated to the method
                      name    : string;      //Name of the method
                      parType : TEleTypeDec;  //Parameter type
                      retType : TEleTypeDec;  //Type returned by the method.
                      pCompile: TxpCodInline): TEleFun;
{Create a new system function (associated to a binary operator) in the current element of
 the AST. If "opr" is null, just create a method without operator.
 Returns the reference to the function created.}
var
  pars: TxpParFuncArray;     //Array of parameters
begin
  setlength(pars, 0);        //Reset parameters
  AddParam(pars, 'b', srcPosNull, clsType, decNone);  //Base object
  AddParam(pars, 'n', srcPosNull, parType, decNone);  //Parameter
  //Add declaration
  curLocation := locInterface;   { TODO : Does it is neccesary to specify location for a method? }
  Result      := AddFunctionUNI(name, retType, srcPosNull, pars, false);
  //Here variables can be added
  {Create a body, to be uniform with normal function and for have a space where
  compile code and access to posible variables or other elements.}
  Result.codInline := pCompile; //Set routine to generate code
  Result.oper := UpCase(opr); //Set operator as UpperCase to speed search.
  if opr = '' then Result.operTyp := opkNone
  else Result.operTyp := opkBinary;
  TreeElems.CloseElement;    //Close function implementation
end;
function TGenCod.CreateUOMethod(
                      clsType: TEleTypeDec;   //Base type where the method bellow.
                      opr     : string;      //Opertaor associated to the method
                      name    : string;      //Name of the method
                      retType : TEleTypeDec;  //Type returned by the method.
                      pCompile: TxpCodInline;
                      operTyp: TOperatorType = opkUnaryPre): TEleFun;
{Create a new system function (associated to a unary operator) in the current element of
 the AST.
 Returns the reference to the function created.}
var
  pars: TxpParFuncArray;     //Array of parameters
begin
  setlength(pars, 0);        //Reset parameters
  AddParam(pars, 'b', srcPosNull, clsType, decNone);  //Base object
  //Add declaration
  curLocation := locInterface;   { TODO : Does it is neccesary to specify location for a method? }
  Result      := AddFunctionUNI(name, retType, srcPosNull, pars, false);
  //Here variables can be added
  {Create a body, to be uniform with normal function and for have a space where
  compile code and access to posible variables or other elements.}
  Result.codInline := pCompile; //Set routine to generate code
  Result.oper := UpCase(opr); //Set operator as UpperCase to speed searching.
  if opr = '' then Result.operTyp := opkNone
  else Result.operTyp := operTyp; //Must be pre or post
  TreeElems.CloseElement;    //Close function implementation
end;
procedure TGenCod.CreateSystemElements;
{Initialize the system elements. Must be executed just one time when compiling.}
var
  uni: TEleUnit;
  pars: TxpParFuncArray;  //Array of parameters
  f: TEleFun;
begin
  //////// Funciones del sistema ////////////
  //Implement calls to Code Generator
  callDefineArray  := @DefineArray;
  callDefinePointer:= @DefinePointer;
  callValidRAMaddr := @ValidRAMaddr;
  callStartProgram := @Cod_StartProgram;
  callEndProgram   := @Cod_EndProgram;
  //////////////////////// Create "System" Unit. //////////////////////
  {Must be done once in First Pass. Originally system functions were created in a special
  list and has a special treatment but it implied a lot of work for manage the memory,
  linking, use of variables, and optimization. Now we create a "system unit" like a real
  unit (more less) and we create the system function here, so we use the same code for
  linking, calling and optimization that we use in common functions. Moreover, we can
  create private functions.}
  uni := CreateUnit('-');  //System unit
  TreeElems.AddElementAndOpen(uni);  //Open Unit
  /////////////// System types ////////////////////
  typBool := CreateEleType('boolean', srcPosNull, 1, tctAtomic, t_boolean);
  typBool.OnLoadToWR := @byte_LoadToRT;
  typBool.location := locInterface;   //Location for type (Interface/Implementation/...)
  TreeElems.AddElementAndOpen(typBool);  //Open to create "elements" list.
  TreeElems.CloseElement;   //Close Type
  typByte := CreateEleType('byte', srcPosNull, 1, tctAtomic, t_uinteger);
  typByte.OnLoadToWR := @byte_LoadToRT;
  typByte.location := locInterface;
  TreeElems.AddElementAndOpen(typByte);  //Open to create "elements" list.
  TreeElems.CloseElement;
  typChar := CreateEleType('char', srcPosNull, 1, tctAtomic, t_string);
  typChar.OnLoadToWR := @byte_LoadToRT;
  typChar.location := locInterface;
  TreeElems.AddElementAndOpen(typChar);
  TreeElems.CloseElement;
  typWord := CreateEleType('word', srcPosNull, 2, tctAtomic, t_uinteger);
  typWord.OnLoadToWR := @word_LoadToRT;
  typWord.location := locInterface;
  TreeElems.AddElementAndOpen(typWord);
  TreeElems.CloseElement;
  /////////////// Boolean type ////////////////////
  //Methods-Operators
  TreeElems.OpenElement(typBool);
  f:=CreateBOMethod(typBool, ':=',  '_set', typBool, typNull, @ROB_bool_asig_bool);
  f:=CreateUOMethod(typBool, 'NOT', '_not', typBool, @ROU_not_bool, opkUnaryPre);
  f.fConResCon := true;
  f:=CreateBOMethod(typBool, 'AND', '_and', typBool, typBool, @ROB_bool_and_bool);
  f.fConmutat := true; f.fConResCon := true; f.fNulResNul := true;
  f:=CreateBOMethod(typBool, '=' ,  '_equ', typBool, typBool, @ROB_bool_equal_bool);
  f.fConmutat := true; f.fConResCon := true;
  f:=CreateBOMethod(typBool, '<>',  '_dif', typBool, typBool, @ROB_bool_difer_bool);
  f.fConmutat := true; f.fConResCon := true;
  TreeElems.CloseElement;   //Close Type
  /////////////// Byte type ////////////////////
  //Methods-Operators
  TreeElems.OpenElement(typByte);
//  opr:=typByte.CreateUnaryPreOperator('@', 6, 'addr', @ROU_address);
  f:=CreateBOMethod(typByte, ':=', '_set', typByte, typNull, @ROB_byte_asig_byte);
  f:=CreateBOMethod(typByte, '+=', '_aadd',typByte, typNull, @ROB_byte_aadd_byte);
  f:=CreateBOMethod(typByte, '-=', '_asub',typByte, typNull, @ROB_byte_asub_byte);
  f:=CreateBOMethod(typByte, '+' , '_add', typByte, typByte, @ROB_byte_add_byte);
  f.fConmutat := true; f.fConResCon := true;
  f:=CreateBOMethod(typByte, '+' , '_add', typWord, typWord, @ROB_byte_add_word);
  f.fConmutat := true; f.fConResCon := true;
  f:=CreateBOMethod(typByte, '-' , '_sub', typByte, typByte, @ROB_byte_sub_byte);
  f.fConResCon := true;
  f:=CreateBOMethod(typByte, '*' , '_mul', typByte, typWord, @ROB_byte_mul_byte);
  f.fConmutat := true; f.fConResCon := true; f.fNulResNul := true;
  f:=CreateBOMethod(typByte, 'AND','_and', typByte, typByte, @ROB_byte_and_byte);
  f.fConmutat := true; f.fConResCon := true; f.fNulResNul := true;
  f:=CreateBOMethod(typByte, 'OR' ,'_or' , typByte, typByte, @ROB_byte_or_byte);
  f.fConmutat := true; f.fConResCon := true;
  f:=CreateBOMethod(typByte, 'XOR','_xor', typByte, typByte, @ROB_byte_xor_byte);
  f.fConmutat := true; f.fConResCon := true;
  f:=CreateUOMethod(typByte, 'NOT','_not', typByte, @ROU_not_byte, opkUnaryPre);
  f.fConResCon := true;
  f:=CreateBOMethod(typByte, '=' , '_equ', typByte, typBool, @ROB_byte_equal_byte);
  f.fConmutat := true; f.fConResCon := true;
  f:=CreateBOMethod(typByte, '<>', '_dif', typByte, typBool, @ROB_byte_difer_byte);
  f.fConmutat := true; f.fConResCon := true;
  f:=CreateBOMethod(typByte, '>' , '_gre', typByte, typBool, @ROB_byte_great_byte);
  f.fConResCon := true;
  f:=CreateBOMethod(typByte, '<' , '_les', typByte, typBool, @ROB_byte_less_byte);
  f.fConResCon := true;
  f:=CreateBOMethod(typByte, '>=', '_gequ',typByte, typBool, @ROB_byte_gequ_byte);
  f.fConResCon := true;
  f:=CreateBOMethod(typByte, '<=', '_lequ',typByte, typBool, @ROB_byte_lequ_byte);
  f.fConResCon := true;
  f:=CreateBOMethod(typByte, '>>', '_shr', typByte, typByte, @ROB_byte_shr_byte);  { TODO : Definir bien la precedencia }
  f.fConResCon := true;
  f:=CreateBOMethod(typByte, '<<', '_shl', typByte, typByte, @ROB_byte_shl_byte);
  f.fConResCon := true;
  TreeElems.CloseElement;   //Close Type
  /////////////// Char type ////////////////////
  TreeElems.OpenElement(typChar);
  //opr:=typChar.CreateUnaryPreOperator('@', 6, 'addr', @ROU_address);
  f:=CreateBOMethod(typChar, ':=', '_set', typChar, typNull, @ROB_char_asig_char);
  //opr.CreateOperation(typString, @ROB_char_asig_string);
  f:=CreateBOMethod(typChar, '=' , '_equ', typChar, typBool, @ROB_char_equal_char);
  f.fConmutat := true; f.fConResCon := true;
  f:=CreateBOMethod(typChar, '<>', '_dif', typChar, typBool, @ROB_char_difer_char);
  f.fConmutat := true; f.fConResCon := true;
  TreeElems.CloseElement;   //Close Type

  /////////////// Word type ////////////////////
  TreeElems.OpenElement(typWord);
  //opr:=typWord.CreateUnaryPreOperator('@', 6, 'addr', @ROU_address);
  f:=CreateBOMethod(typWord, ':=' ,'_set' , typWord, typNull, @ROB_word_asig_word);
  f:=CreateBOMethod(typWord, ':=' ,'_set' , typByte, typNull, @ROB_word_asig_byte);
  f:=CreateBOMethod(typWord, '+=' ,'_aadd', typByte, typNull, @ROB_word_aadd_byte);
  f:=CreateBOMethod(typWord, '+=' ,'_aadd', typWord, typNull, @ROB_word_aadd_word);
  f:=CreateBOMethod(typWord, '-=' ,'_asub', typByte, typNull, @ROB_word_asub_byte);
  f:=CreateBOMethod(typWord, '+'  , '_add', typByte, typWord, @ROB_word_add_byte);
  f.fConmutat := true; f.fConResCon := true;
  f:=CreateBOMethod(typWord, '+'  , '_add', typWord, typWord, @ROB_word_add_word);
  f.fConmutat := true; f.fConResCon := true;
  f:=CreateBOMethod(typWord, '-'  , '_sub', typByte, typWord, @ROB_word_sub_byte);
  f.fConResCon := true;
  f:=CreateBOMethod(typWord, '-'  , '_sub', typWord, typWord, @ROB_word_sub_word);
  f.fConResCon := true;
  f:=CreateBOMethod(typWord, 'AND', '_and', typByte, typByte, @ROB_word_and_byte);
  f.fConmutat := true; f.fConResCon := true; f.fNulResNul := true;
  f:=CreateBOMethod(typWord, 'AND', '_and', typWord, typWord, @ROB_word_and_word);
  f.fConmutat := true; f.fConResCon := true; f.fNulResNul := true;
  f:=CreateUOMethod(typWord, 'NOT', '_not', typWord, @ROU_not_word, opkUnaryPre);
  f.fConResCon := true;
  f:=CreateBOMethod(typWord, '>>' , '_shr', typByte, typWord, @ROB_word_shr_byte); { TODO : Definir bien la precedencia }
  f.fConResCon := true;
  f:=CreateBOMethod(typWord, '<<' , '_shl', typByte, typWord, @ROB_word_shl_byte);
  f.fConResCon := true;

  f:=CreateBOMethod(typWord, '=' , '_equ' , typWord, typBool, @ROB_word_equal_word);
  f.fConmutat := true; f.fConResCon := true;
  f:=CreateBOMethod(typWord, '<>', '_dif' , typWord, typBool, @ROB_word_difer_word);
  f.fConmutat := true; f.fConResCon := true;
  f:=CreateBOMethod(typWord, '>=', '_gequ', typWord, typBool, @ROB_word_gequ_word);
  f.fConResCon := true;
  f:=CreateBOMethod(typWord, '<' , '_les' , typWord, typBool, @ROB_word_less_word);
  f.fConResCon := true;
  f:=CreateBOMethod(typWord, '>' , '_gre' , typWord, typBool, @ROB_word_great_word);
  f.fConResCon := true;
  f:=CreateBOMethod(typWord, '<=', '_lequ', typWord, typBool, @ROB_word_lequ_word);
  f.fConResCon := true;
  //Methods
  f:=CreateUOMethod(typWord, '', 'low' , typByte, @word_Low);
  f.fConResCon := true;
  f:=CreateUOMethod(typWord, '', 'high', typByte, @word_High);
  f.fConResCon := true;

  TreeElems.CloseElement;   //Close Type

  {Create variables for aditional Working register. Note that this variables are
  accesible (and usable) from the code, because the name assigned is a common variable.}
  //Create register H as variable
  H := AddVariableAndOpen('__H', typByte, srcPosNull);
  TreeElems.CloseElement;
  H.adicPar.hasAdic := decNone;
  H.adicPar.hasInit := false;
  H.location := locInterface;  //make visible
  //Create register E as variable
  E := AddVariableAndOpen('__E', typByte, srcPosNull);
  TreeElems.CloseElement;
  E.adicPar.hasAdic := decNone;
  E.adicPar.hasInit := false;
  E.location := locInterface;  //make visible
  //Create register U as variable
  U := AddVariableAndOpen('__U', typByte, srcPosNull);
  TreeElems.CloseElement;
  U.adicPar.hasAdic := decNone;
  U.adicPar.hasInit := false;
  U.location := locInterface;  //make visible
  //Create register IX as variable
  IX := AddVariableAndOpen('__IX', typWord, srcPosNull);
  TreeElems.CloseElement;
  IX.adicPar.hasAdic := decNone;
  IX.adicPar.hasInit := false;
  IX.location := locInterface;  //make visible

  //Create system function "delay_ms"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'ms', srcPosNull, typWord, decRegis);  //Add parameter
  CreateSystemFunction('delay_ms', typNull, srcPosNull, pars, @fun_delay_ms);

  //Create system function "inc"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);  //Parameter NULL, allows any type.
  CreateSystemFunction('exit', typNull, srcPosNull, pars, @fun_Exit);

  //Create system function "inc"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);  //Parameter NULL, allows any type.
  CreateSystemFunction('inc', typNull, srcPosNull, pars, @fun_Inc);

  //Create system function "dec"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);  //Parameter NULL, allows any type.
  CreateSystemFunction('dec', typNull, srcPosNull, pars, @fun_Dec);

  //Create system function "SetOrig"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typWord, decNone);
  CreateSystemFunction('SetOrig', typNull, srcPosNull, pars, @SetOrig);

  //Create system function "ord"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);
  CreateSystemFunction('ord', typByte, srcPosNull, pars, @fun_ord);

  //Create system function "chr"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);
  CreateSystemFunction('chr', typChar, srcPosNull, pars, @fun_chr);

  //Create system function "byte"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);  //Parameter NULL, allows any type.
  CreateSystemFunction('byte', typByte, srcPosNull, pars, @fun_Byte);

  //Create system function "word"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);  //Parameter NULL, allows any type.
  CreateSystemFunction('word', typWord, srcPosNull, pars, @fun_Word);

  //Create system function "word"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);  //Parameter NULL, allows any type.
  CreateSystemFunction('addr', typWord, srcPosNull, pars, @fun_Addr);

  //Multiply system function
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'A', srcPosNull, typByte, decNone);  //Add parameter
  AddParam(pars, 'B', srcPosNull, typByte, decNone);  //Add parameter
  f_byte_mul_byte_16 := CreateSystemFunction('byte_mul_byte_16', typWord, srcPosNull, pars, @byte_mul_byte_16);

  //Word shift left
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typByte, decRegisX);   //Parameter counter shift
  f_word_shift_l := CreateSystemFunction('word_shift_l', typWord, srcPosNull, pars, @word_shift_l);

  TreeElems.CloseElement; //Close Unit
end;
end.
//5186
