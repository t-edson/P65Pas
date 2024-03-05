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
* Todas las BOR reciben sus dos parámetros en las variables p1^ y p2^.
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
  Classes, SysUtils,
  P65c02utils, CPUCore, GenCodBas_PIC16,
  CompBase, CompGlobals, XpresElemP65, LexPas;
type
    { TGenCod }
    TGenCod = class(TGenCodBas)
    private
      snfBytMulByt16: TEleFun;
      snfWordShift_l: TEleFun;
      snfDelayMs:     TEleFun;
      snfBytDivByt8:  TEleFun;
      snfWrdDivWrd16: TEleFun;
      procedure AddLocVar(var pars: TxpParFuncArray; parName: string;
        const srcPos: TSrcPos; typ0: TEleTypeDec; adicDec: TxpAdicDeclar);
      procedure AddParam(var pars: TxpParFuncArray; parName: string;
        const srcPos: TSrcPos; typ0: TEleTypeDec; adicDec: TxpAdicDeclar);
      function AddSysNormalFunction(name: string; retType: TEleTypeDec;
        const srcPos: TSrcPos; var pars: TxpParFuncArray; codSys: TCodSysNormal
  ): TEleFun;
      procedure arrayHigh(fun: TEleExpress);
      procedure arrayLength(fun: TEleExpress);
      procedure arrayLow(fun: TEleExpress);
      procedure DefineObject(etyp: TEleTypeDec);
      function FillArray(parray: TEleExpress): boolean;
      procedure SIF_bool_or_bool(fun: TEleExpress);
      procedure SIF_byte_div_byte(fun: TEleExpress);
      procedure SIF_byte_mod_byte(fun: TEleExpress);
      procedure SIF_GetPointer(fun: TEleExpress);
      procedure SIF_SetPointer(fun: TEleExpress);
      procedure SIF_word_div_word(fun: TEleExpress);
      procedure SIF_word_mod_word(fun: TEleExpress);
      procedure SIF_word_mul_byte(fun: TEleExpress);
      procedure SIF_word_or_word(fun: TEleExpress);
      procedure SNF_byt_div_byt_8(funEleExp: TEleFunBase);
      procedure SNF_byt_mul_byt_16(fun: TEleFunBase);
      procedure Invert_A_to_A;
      procedure Copy_Z_to_A;
      procedure Invert_Z_to_A;
      procedure Copy_C_to_A;
      procedure Invert_C_to_A;
      function Invert(fun: TEleExpress): boolean;
      function CreateInUOMethod(clsType: TEleTypeDec; opr: string; name: string;
        retType: TEleTypeDec; pCompile: TCodSysInline; operTyp: TOperatorType =
  opkUnaryPre): TEleFun;
      function CreateInBOMethod(clsType: TEleTypeDec; opr: string; name: string;
        parType: TEleTypeDec; retType: TEleTypeDec; pCompile: TCodSysInline
        ): TEleFun;
      function CreateInTerMethod(clsType: TEleTypeDec; name: string; parType1,
        parType2: TEleTypeDec; retType: TEleTypeDec; pCompile: TCodSysInline
  ): TEleFun;
      function AddSysInlineFunction(name: string; retType: TEleTypeDec;
        const srcPos: TSrcPos; const pars: TxpParFuncArray;
  codSys: TCodSysInline): TEleFun;
      procedure DefineArray(etyp: TEleTypeDec);
      procedure DefinePointer(etyp: TEleTypeDec);
      procedure DefineShortPointer(etyp: TEleTypeDec);
      procedure SIF_Addr(fun: TEleExpress);
      procedure SIF_Byte(fun: TEleExpress);
      procedure SIF_arr_asig_arr(fun: TEleExpress);
      procedure SIF_obj_asig_obj(fun: TEleExpress);
      procedure LoadByteIndexWord(const idxvar: TEleVarDec; offset: word);
      procedure LoadWordIndexWord(const idxvar: TEleVarDec; offset: word);
      procedure LoadWordIndexWord2(const idxvar: TEleVarDec; offset: word);
      procedure SetByteIndexWord(const idxvar: TEleVarDec; offset: word;
        parB: TEleExpress);
      procedure SIF_SetItemIndexWord(fun: TEleExpress);
      procedure SetWordIndexWord(const idxvar: TEleVarDec; offset: word;
        parB: TEleExpress);
      procedure SNF_wrd_div_wrd_16(fun: TEleFunBase);
      procedure ValidRAMaddr(addr: integer);
      procedure SIF_GetItemIdxByte(fun: TEleExpress);
      procedure SIF_GetItemIdxWord(fun: TEleExpress);
      procedure SIF_SetItemIndexByte(fun: TEleExpress);
      procedure SIF_ArrayClear(fun: TEleExpress);
      procedure SIF_pointer_add_word(fun: TEleExpress);
      procedure SIF_pointer_sub_word(fun: TEleExpress);
      procedure SIF_address(fun: TEleExpress);

      procedure SIF_word_and_byte(fun: TEleExpress);
      procedure SNF_word_shift_l(fun: TEleFunBase);
    protected //Boolean operations
      procedure SIF_bool_asig_bool(fun: TEleExpress);
      procedure SIF_bool_and_bool(fun: TEleExpress);
      procedure SIF_bool_xor_bool(fun: TEleExpress);
      procedure SIF_bool_equal_bool(fun: TEleExpress);
      procedure SIF_not_bool(fun: TEleExpress);
    protected //Byte operations
      procedure SIF_byte_asig_byte(fun: TEleExpress);
      procedure SIF_byte_aadd_byte(fun: TEleExpress);
      procedure SIF_byte_asub_byte(fun: TEleExpress);
      procedure SIF_byte_sub_byte(fun: TEleExpress);
      procedure SIF_byte_add_byte(fun: TEleExpress);
      procedure SIF_byte_and_byte(fun: TEleExpress);
      procedure SIF_byte_or_byte(fun: TEleExpress);
      procedure SIF_byte_xor_byte(fun: TEleExpress);
      procedure SIF_byte_equal_byte(fun: TEleExpress);
      procedure SIF_byte_difer_byte(fun: TEleExpress);
      procedure SIF_byte_great_byte(fun: TEleExpress);
      procedure SIF_byte_less_byte(fun: TEleExpress);
      procedure SIF_byte_gequ_byte(fun: TEleExpress);
      procedure SIF_byte_lequ_byte(fun: TEleExpress);
      procedure SIF_byte_shr_byte(fun: TEleExpress);
      procedure SIF_byte_shl_byte(fun: TEleExpress);
      procedure SIF_byte_add_word(fun: TEleExpress);
      procedure SIF_byte_mul_byte(fun: TEleExpress);
      procedure SIF_not_byte(fun: TEleExpress);
    private   //Operaciones con Word
      procedure SIF_word_asig_word(fun: TEleExpress);
      procedure SIF_word_asig_byte(fun: TEleExpress);
      procedure SIF_word_equal_word(fun: TEleExpress);
      procedure SIF_word_equal_byte(fun: TEleExpress);
      procedure SIF_word_difer_word(fun: TEleExpress);
      procedure SIF_word_add_byte(fun: TEleExpress);
      procedure SIF_word_add_word(fun: TEleExpress);
      procedure SIF_word_sub_byte(fun: TEleExpress);
      procedure SIF_word_sub_word(fun: TEleExpress);
      procedure SIF_word_aadd_byte(fun: TEleExpress);
      procedure SIF_word_aadd_word(fun: TEleExpress);
      procedure SIF_word_and_word(fun: TEleExpress);
      procedure SIF_word_asub_byte(fun: TEleExpress);
      procedure SIF_word_asub_word(fun: TEleExpress);
      procedure SIF_word_gequ_word(fun: TEleExpress);
      procedure SIF_word_great_word(fun: TEleExpress);
      procedure SIF_word_lequ_word(fun: TEleExpress);
      procedure SIF_word_less_word(fun: TEleExpress);
      procedure SIF_word_shl_byte(fun: TEleExpress);
      procedure SIF_word_shr_byte(fun: TEleExpress);
      procedure SIF_not_word(fun: TEleExpress);
        // Operations for DWord
      procedure SIF_DWord(fun: TEleExpress);
      procedure SIF_dword_asig_dword(fun: TEleExpress);
      procedure SIF_dword_asig_byte(fun: TEleExpress);
      procedure SIF_dword_asig_word(fun: TEleExpress);
      procedure SIF_dword_add_dword(fun: TEleExpress);
      procedure SIF_dword_add_byte(fun: TEleExpress);
      procedure SIF_dword_add_word(fun: TEleExpress);
    private   //Operaciones con Char
      procedure SIF_char_asig_char(fun: TEleExpress);
      procedure SIF_char_asig_string(fun: TEleExpress);
      procedure SIF_char_equal_char(fun: TEleExpress);
      procedure SIF_char_difer_char(fun: TEleExpress);
    protected //Operaciones con punteros
      procedure SIF_pointer_add_byte(fun: TEleExpress);
      procedure SIF_pointer_sub_byte(fun: TEleExpress);
      procedure SIF_derefPointer(fun: TEleExpress; SetRes: boolean);
    private   //Funciones internas.
      procedure codif_1mseg;
      procedure SNF_delay_ms(fun: TEleFunBase);
      procedure expr_end(posExpres: TPosExpres);
      procedure expr_start;
      procedure SIF_delay_ms(fun: TEleExpress);
      procedure SIF_Inc(fun: TEleExpress);
      procedure SIF_Dec(fun: TEleExpress);
      procedure SIF_Ord(fun: TEleExpress);
      procedure SIF_Chr(fun: TEleExpress);
      procedure SIF_Word(fun: TEleExpress);
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
  MSG_CANNOT_COMPL, MSG_IDX_BYT_WORD, ER_INV_MEMADDR, ER_INV_MAD_DEV: string;

procedure SetLanguage;
begin
  GenCodBas_PIC16.SetLanguage;
  {$I _language\tra_GenCod.pas}
end;
function GetAssignTarget(fun: TEleExpress; out target: TEleExpress): boolean;
var
  setFunct: TEleExpress;
begin
  setFunct := TEleExpress(fun.Parent);
  if setFunct = nil then exit(false);
  if setFunct.opType <> otFunct then exit(false);
  if setFunct.rfun.getset <> gsSetInSimple then exit(false);
  target := TEleExpress(setFunct.elements[0]);  //Parameter C := A + B
  exit(true);
end;
procedure TGenCod.Invert_A_to_A;
{Invert all the bits of A register (as boolean expression) .
If A=$00 => A = $FF
If A=$FF => A = $00
}
begin
  lastASMcode := lacInvAtoA;  //Activates flag
  lastASMaddr := _PC;  //Get current address.
  _EORi($FF); //Invert bits
end;
procedure TGenCod.Copy_Z_to_A;
{Copy the logic value of Z flag to A register (as boolean expression) .
If Z=0 => A = $00
If Z=1 => A = $FF
}
begin
  //Result in Z. Move to A.
  lastASMcode := lacCopyZtoA;  //Activates flag
  lastASMaddr := _PC;  //Get current address.
  _BEQ(2);  //If Z=1: regA = 0
  _LDAi($FF);
  _EORi($FF);  // Invert A
end;
procedure TGenCod.Invert_Z_to_A;
{Copy the logic value of Z flag (inverted) to A register (as boolean expression) .
If Z=1 => A = $00
If Z=0 => A = $FF
}
begin
  //Result in Z. Move to A.
  lastASMcode := lacInvZtoA;  //Activates flag
  lastASMaddr := _PC;  //Get current address.
  _BEQ(2);    //If Z=1: regA = 0
  _LDAi($FF);
end;
procedure TGenCod.Copy_C_to_A;
{Copy the logic value of C flag to A register (as boolean expression).
If C=0 => A = $00
If C=1 => A = $FF
}
begin
  lastASMcode := lacCopyCtoA;  //Activates flag
  lastASMaddr := _PC;  //Get current address.
//  _PHP;
//  _PLA;
//  _ANDi($01);
//  _ASLa;  //Leaves in bit 1.
  _LDAi($FF); //Doesn't change bit C
  _BCS(2);  //If C=1
  _EORi($00);
end;
procedure TGenCod.Invert_C_to_A;
{Copy the logic value of C flag (inverted) to A register (as boolean expression).
If C=0 => A = $FF
If C=1 => A = $00
}
begin
  lastASMcode := lacInvCtoA;  //Activates flag
  lastASMaddr := _PC;  //Get current address.
  _LDAi($00); //Doesn't change bit C
  _BCS(2);  //If C=1
  _EORi($FF);
end;
function TGenCod.Invert(fun: TEleExpress): boolean;
{Convert a boolean operand in the negative form, changing its constant value (if it's a
constant operand) or modifying the generated code (if it's a register operand).
If cannot invert the operand, returns FALSE.
}
begin
  if fun.Sto = stConst then begin
    //In constants, we can change the value.
    fun.SetLiteralBoolConst(not fun.value.valBool);
  end else if fun.Sto = stRegister then begin
    if lastASMcode = lacCopyZtoA then begin
      pic.iRam := lastASMaddr;   //Delete last instructions
      Invert_Z_to_A;
    end else if lastASMcode = lacCopyCtoA then begin
      pic.iRam := lastASMaddr;   //Delete last instructions
      Invert_C_to_A;
    end else if lastASMcode = lacInvCtoA then begin
      pic.iRam := lastASMaddr;   //Delete last instructions
      Copy_C_to_A;
    end else if lastASMcode = lacInvAtoA then begin
      pic.iRam := lastASMaddr;   //Delete last instructions
      lastASMcode := lacNone;
    end else begin
      //We could add here other types or negations.
      exit(false);
    end;
  end else begin
    exit(false);
  end;
  exit(true);
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
procedure TGenCod.SIF_address(fun: TEleExpress);
{Return the address of any operand.}
var
  startAddr: integer;
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //*** Ver si es necesario Completar
    exit;
  end;
  //Code generation
  case par.Sto of
  stConst : begin
    if par.Typ.catType = tctArray then begin
      //We allow to get the address for constant arrays, storing first in RAM.
      if pic.disableCodegen then begin
        //Cannot generate code
        SetFunExpres(fun); //Still as a function
      end else begin
        CreateValueInCode(par.Typ, par.Value, startAddr);
        SetFunConst(fun);
        fun.value.ValInt := startAddr;
        fun.evaluated := true;
      end;
    end else begin
      genError('Cannot obtain address of constant.');
    end;
  end;
  stRamFix: begin
    //Es una variable normal
    //La dirección de una variable es constante
    if par.rVar.allocated then begin
      SetFunConst(fun);
      fun.value.valInt := par.rVar.addr;
      fun.evaluated := par.rVar.allocated;
    end else begin
      SetFunExpres(fun); //Still as a function
    end;
  end;
  stRegister: begin
    genError('Cannot obtain address of an expression.');
  end;
  else
    genError('Cannot obtain address of this operand.');
  end;
end;
{%REGION Byte operations}
procedure TGenCod.SIF_not_byte(fun: TEleExpress);
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    if par.Sto = stConst then SetFunConst_byte(fun, (not par.value.valInt) and $FF);
    exit;
  end;
  //Code generation
  case par.Sto of
  stConst : begin
    SetFunConst_byte(fun, (not par.value.valInt) and $FF);
  end;
  stRamFix: begin
    SetFunExpres(fun);
    _LDA(par.rvar.addr);
    _EORi($FF);
  end;
  else
    genError('Not implemented: "%s"', [fun.name]);
  end;
end;
procedure TGenCod.SIF_byte_asig_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
  parBsto: TStorage;
  offset: word;
  idxvar: TEleVarDec;
begin
  SetFunNull(fun);  //In Pascal an assigment doesn't return type.
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  //Simplify parB
  parBsto := parB.Sto;  //Save storage
  if parB.Sto = stRamVarOf then begin
     LoadToWR(parB);  //Could require IX
     if HayError then exit;
     parB.Sto := stRegister;
  end;
  //Validates parA.
  if parA.opType<>otVariab then begin //The only valid type.
    GenError('Only variables can be assigned.', parA.srcDec);
    exit;
  end;
  //Implements assignment
  if parA.Sto = stRamFix then begin
    //Assignment to a common variable (constant Address)
    case parB.Sto of
    stConst: begin
      if (ParB.val = 0) and (cpuMode = cpu65C02) then
        _STZ(parA.add)
      else begin
        _LDAi(parB.val);
        _STA(parA.add);
      end;
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
      _TAX_opt;
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
  end else begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_byte_and_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_byte(fun, parA.val and parB.val);
      { TODO : Completar con otros casos }
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin  //suma de dos constantes. Caso especial
    SetFunConst_byte(fun, parA.val and parB.val);  //puede generar error
  end;
  stConst_RamFix: begin
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
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
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
  stRamFix_Const: begin
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
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parB.add);
    _AND(parA.add);   //leave in A
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _AND(parA.add);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
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
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _AND(parB.add);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.SIF_byte_or_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_byte(fun, parA.val or parB.val);
      { TODO : Completar con otros casos }
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin  //suma de dos constantes. Caso especial
    SetFunConst_byte(fun, parA.val or parB.val);  //puede generar error
  end;
  stConst_RamFix: begin
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
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
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
  stRamFix_Const: begin
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
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parA.add);
    _ORA(parB.add);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _ORA(parA.add);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
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
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _ORA(parB.add);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.SIF_byte_xor_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_byte(fun, parA.val xor parB.val);
      { TODO : Completar con otros casos }
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin  //suma de dos constantes. Caso especial
    SetFunConst_byte(fun, parA.val xor parB.val);  //puede generar error
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.val);
    _EOR(parB.add)
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _EORi(parA.val);  //leave in A
  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _LDA(parA.add);   //leave in A
    _EORi(parB.val);
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parA.add);   //leave in A
    _EOR(parB.add);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _EOR(parA.add);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _EORi(parB.val);
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _EOR(parB.add);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.SIF_byte_equal_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_bool(fun, parA.val = parB.val);
    end;
    exit;
  end;
  //Code generation
  if parA.Sto = stConst then begin
    case parB.Sto of
    stConst: begin  //compara constantes. Caso especial
      SetFunConst_bool(fun, parA.val = parB.val);
    end;
    stRamFix: begin
      SetFunExpres(fun);   //Se pide Z para el resultado
      if parA.val = 0 then begin  //caso especial
        _LDA(parB.add);
      end else begin
        _LDA(parB.add);
        _CMPi(parA.val);
      end;
      Copy_Z_to_A;
    end;
    stRegister, stRegistA: begin  //la expresión p2 se evaluó y esta en A
      if not AcumStatInZ then _TAX;   //Update Z, if needed.
      if parA.val = 0 then begin  //caso especial
        //Nothing
      end else begin
        _CMPi(parA.val);
      end;
      Copy_Z_to_A;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto = stRamFix then begin
    case parB.Sto of
    stConst: begin
      SetFunExpres(fun);
      if parB.val = 0 then begin  //caso especial
        _LDA(parA.add);
      end else begin
        _LDA(parA.add);
        _CMPi(parB.val);
      end;
      Copy_Z_to_A;
    end;
    stRamFix:begin
      SetFunExpres(fun);
      _LDA(parB.add);
      _CMP(parA.add);
      Copy_Z_to_A;
    end;
    stRegister, stRegistA:begin   //parB evaluated in regA
      SetFunExpres(fun);
      _CMP(parA.add);
      Copy_Z_to_A;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if parA.Sto in [stRegister, stRegistA] then begin
    case parB.Sto of
    stConst: begin   //la expresión p1 se evaluó y esta en A
      if not AcumStatInZ then _TAX;   //Update Z, if needed.
      SetFunExpres(fun);
      if parB.val = 0 then begin  //caso especial
        //Nothing
      end else begin
        _CMPi(parB.val);
      end;
      Copy_Z_to_A;
    end;
    stRamFix:begin  //parA evaluated in regA
      SetFunExpres(fun);
      _CMP(parB.add);
      Copy_Z_to_A;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_byte_difer_byte(fun: TEleExpress);
begin
  SIF_byte_equal_byte(fun);  //usa el mismo código
  if not Invert(fun) then begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_byte_aadd_byte(fun: TEleExpress);
{Operación de asignación suma: +=}
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
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
procedure TGenCod.SIF_byte_asub_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
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
procedure TGenCod.SIF_byte_add_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
  stoo: TStoOperandsBSIF;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_byte(fun, parA.val + parB.val);
    end;
    exit;
  end;
  //Code generation
  stoo := stoOperation(parA, parB);
  case stoo of
  stConst_Const: begin
    SetFunConst_byte(fun, parA.val + parB.val);  //puede generar error
  end;
  stConst_RamFix, stRamFix_Const: begin
    if stoo = stRamFix_Const then Exchange(parA, parB);
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
      exit;
    end;
    SetFunExpres(fun);
    _CLC;
    _LDAi(parA.val);
    _ADC(parB.add);
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _CLC;
    _ADCi(parA.val);
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _CLC;
    _LDA(parA.add);
    _ADC(parB.add);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _CLC;
    _ADC(parA.add);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _CLC;
    _ADCi(parB.val);
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _CLC;
    _ADC(parB.add);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.SIF_byte_add_word(fun: TEleExpress);
begin
  fun.elements.Exchange(0,1);  //Convert to word_add_byte
  SIF_word_add_byte(fun);
  fun.elements.Exchange(0,1);
end;
procedure TGenCod.SIF_byte_sub_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_byte(fun, parA.val-parB.val);
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const:begin  //suma de dos constantes. Caso especial
    SetFunConst_byte(fun, parA.val-parB.val);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _SEC;
    _LDAi(parA.val);
    _SBC(parB.add);
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _STA(H.addr);
    _SEC;
    _LDAi(parA.val);
    _SBC(H.addr);
  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.add);
    _SBCi(parB.val);
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.add);
    _SBC(parB.add);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    _SEC;
    _SBC(parA.add);   //a - p1 -> a
    //Invierte
    _EORi($FF);
    _CLC;
    _ADCi(1);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _SEC;
    _SBCi(parB.val);
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _SEC;
    _SBC(parB.add);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.SNF_byt_mul_byt_16(fun: TEleFunBase);
//Routine to multiply 8 bits X 8 bits
//pasA * parB -> [H:A]  Usa registros: A,H,E,U
//Based on https://codebase64.org/doku.php?id=base:short_8bit_multiplication_16bit_product
var
  m0, m1: integer;
  fac1,  fac2: TEleVarDec;
begin
    fac1 := fun.pars[0].pvar;
    fac2 := fun.pars[1].pvar;
    PutLabel('__byt_mul_byt_16');
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
procedure TGenCod.SIF_byte_mul_byte(fun: TEleExpress);
var
  AddrUndef: boolean;
  fmul: TEleFun;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  fmul := snfBytMulByt16;
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_word(fun, parA.val * parB.val);
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    SetFunConst_word(fun, parA.val*parB.val);  //puede generar error
  end;
  stConst_RamFix: begin
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
//      AddCallerToFromCurr(snfWordShift_l);  //Declare use
      functCall(snfWordShift_l, AddrUndef);  //Use
      exit;
    end else if parA.val = 16 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      _LDA(parB.add);
      //Loop
      _LDXi(4);  //Counter
//      AddCallerToFromCurr(snfWordShift_l);  //Declare use
      functCall(snfWordShift_l, AddrUndef);  //Use
      exit;
    end else if parA.val = 32 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      _LDA(parB.add);
      //Loop
      _LDXi(5);  //Counter
//      AddCallerToFromCurr(snfWordShift_l);  //Declare use
      functCall(snfWordShift_l, AddrUndef);  //Use
      exit;
    end;
    //General case
    SetFunExpres(fun);
    _LDAi(parA.val);
    _STA(fmul.pars[0].pvar.addr);
    _LDA(parB.add);
    _STA(fmul.pars[1].pvar.addr);
//    AddCallerToFromCurr(fmul);  //Declare use
//    AddCallerToFromCurr(fmul.pars[0].pvar);  //Declare use
//    AddCallerToFromCurr(fmul.pars[1].pvar);  //Declare use
    functCall(fmul, AddrUndef);   //Code the "JSR"
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    //Es casi el mismo código de stConst_RamFix
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
//      AddCallerToFromCurr(snfWordShift_l);  //Declare use
      functCall(snfWordShift_l, AddrUndef);  //Use
      exit;
    end else if parA.val = 16 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      //_LDA(parB.add);
      //Loop
      _LDXi(4);  //Counter
//      AddCallerToFromCurr(snfWordShift_l);  //Declare use
      functCall(snfWordShift_l, AddrUndef);  //Use
      exit;
    end else if parA.val = 32 then begin
      //Caso especial
      SetFunExpres(fun);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      //_LDA(parB.add);
      //Loop
      _LDXi(5);  //Counter
//      AddCallerToFromCurr(snfWordShift_l);  //Declare use
      functCall(snfWordShift_l, AddrUndef);  //Use
      exit;
    end;
    //General case
    SetFunExpres(fun);
    //_LDAi(parA.val);
    _STA(fmul.pars[0].pvar.addr);
    _LDA(parA.val);
    _STA(fmul.pars[1].pvar.addr);
//    AddCallerToFromCurr(fmul);  //Declare use
//    AddCallerToFromCurr(fmul.pars[0].pvar);  //Declare use
//    AddCallerToFromCurr(fmul.pars[1].pvar);  //Declare use
    functCall(fmul, AddrUndef);   //Code the "JSR"
  end;
  stRamFix_Const: begin
    fun.elements.Exchange(0,1);
    SIF_byte_mul_byte(fun);
    fun.elements.Exchange(0,1);
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parA.add);
    _STA(fmul.pars[0].pvar.addr);
    _LDA(parB.add);
    _STA(fmul.pars[1].pvar.addr);
//    AddCallerToFromCurr(fmul);  //Declare use
//    AddCallerToFromCurr(fmul.pars[0].pvar);  //Declare use
//    AddCallerToFromCurr(fmul.pars[1].pvar);  //Declare use
    functCall(fmul, AddrUndef);   //Code the "JSR"
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    //_LDA(parA.add);
    _STA(fmul.pars[0].pvar.addr);
    _LDA(parA.add);
    _STA(fmul.pars[1].pvar.addr);
//    AddCallerToFromCurr(fmul);  //Declare use
//    AddCallerToFromCurr(fmul.pars[0].pvar);  //Declare use
//    AddCallerToFromCurr(fmul.pars[1].pvar);  //Declare use
    functCall(fmul, AddrUndef);   //Code the "JSR"
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    fun.elements.Exchange(0,1);
    SIF_byte_mul_byte(fun);  //, true);
    fun.elements.Exchange(0,1);
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    fun.elements.Exchange(0,1);
    SIF_byte_mul_byte(fun);  //, true);
    fun.elements.Exchange(0,1);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.SIF_byte_great_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_bool(fun, parA.value.valBool > parB.value.valBool);
    end else if (parA.Sto = stConst) and (parA.val = 0) then begin
      SetFunConst_bool(fun, false);
    end else if (parB.Sto = stConst) and (parB.val = 255) then begin
      SetFunConst_bool(fun, false);
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin  //compara constantes. Caso especial
    SetFunConst_bool(fun, parA.val > parB.val);
  end;
  stConst_RamFix: begin
    if parA.val = 0 then begin
      //0 es mayor que nada
      SetFunConst_bool(fun, false);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      SetFunExpres(fun);
      _LDA(parB.add);
      _CMPi(parA.val); //Result in C (inverted)
      Invert_C_to_A; //Copy C to A (still inverted)
    end;
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    if parA.val = 0 then begin
      //0 es mayor que nada
      SetFunConst_byte(fun, 0);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      //Se necesita asegurar que p1, es mayo que cero.
      SetFunExpres(fun);
      //p2, already in A
      _CMPi(parA.val); //Result in C (inverted)
      Invert_C_to_A; //Copy C to A (still inverted)
    end;
  end;
  stRamFix_Const: begin
    if parB.val = 255 then begin
      //Nada es mayor que 255
      SetFunConst_bool(fun, false);
      GenWarn('Expression will always be FALSE or TRUE.');
    end else begin
      SetFunExpres(fun);
      _LDAi(parB.val);
      _CMP(parA.add); //Result in C (inverted)
      Invert_C_to_A; //Copy C to A (still inverted)
    end;
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parB.add);
    _CMP(parA.add); //Result in C (inverted)
    Invert_C_to_A; //Copy C to A (still inverted)
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    //p2, already in A
    _CMP(parA.add); //Result in C (inverted)
    Invert_C_to_A; //Copy C to A (still inverted)
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    if parB.val = 255 then begin
      //Nada es mayor que 255
      SetFunConst_byte(fun, 0);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      SetFunExpres(fun);
      //p1, already in A
      _CMPi(parB.val+1); //p1 >= p2+1. We've verified parB.val<255
      Copy_C_to_A; //Copy C to A
    end;
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    _CLC;   //A trick to get p1>p2 in C, after _SBC
    _SBC(parB.add);
    Copy_C_to_A; //Copy C to A
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.SIF_byte_less_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //A < B es lo mismo que B > A
  fun.elements.Exchange(0,1);
  SIF_byte_great_byte(fun);
  fun.elements.Exchange(0,1);
end;
procedure TGenCod.SIF_byte_gequ_byte(fun: TEleExpress);
begin
  SIF_byte_less_byte(fun);
  if not Invert(fun) then begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_byte_lequ_byte(fun: TEleExpress);
begin
  SIF_byte_great_byte(fun);
  if not Invert(fun) then begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_byte_shr_byte(fun: TEleExpress);  //Desplaza a la derecha
var
  L2, L1: integer;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_byte(fun, parA.val >> parB.val);
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin  //compara constantes. Caso especial
    SetFunConst_byte(fun, parA.val >> parB.val);
  end;
  stConst_RamFix: begin
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
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);   //Se pide Z para el resultado
    _TAX_opt;
    _BEQ_post(L2);
    _LDAi(parA.val);
_LABEL_pre(L1);
    _LSRa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stRamFix_Const: begin
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
  stRamFix_RamFix:begin
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
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);   //Se pide Z para el resultado
    _TAX_opt;
    _BEQ_post(L2);
    _LDA(parA.add);
_LABEL_pre(L1);
    _LSRa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
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
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    _LDX(parB.add);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _LSRa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
procedure TGenCod.SIF_byte_shl_byte(fun: TEleExpress);   //Desplaza a la izquierda
var
  L1, L2: integer;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_byte(fun, parA.val << parB.val);
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin  //compara constantes. Caso especial
    SetFunConst_byte(fun, parA.val << parB.val);
  end;
  stConst_RamFix: begin
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
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);   //Se pide Z para el resultado
    _TAX_opt;
    _BEQ_post(L2);
    _LDAi(parA.val);
_LABEL_pre(L1);
    _ASLa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stRamFix_Const: begin
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
  stRamFix_RamFix:begin
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
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);   //Se pide Z para el resultado
    _TAX_opt;
    _BEQ_post(L2);
    _LDA(parA.add);
_LABEL_pre(L1);
    _ASLa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
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
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    _LDX(parB.add);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _ASLa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
  end;
end;
{%ENDREGION}
{%REGION Boolean operations}
procedure TGenCod.SIF_not_bool(fun: TEleExpress);
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    if par.Sto = stConst then SetFunConst_bool(fun, not par.value.valBool);
    exit;
  end;
  //Code generation
  case par.Sto of
  stConst : begin
    //NOT for a constant is defined easily
    SetFunConst_bool(fun, not par.value.valBool);
  end;
  stRamFix: begin
    SetFunExpres(fun);
    //We have to return logical value inverted in A
    _LDA(par.rVar.addr);
    _EORi($FF);
  end;
  stRegister, stRegistA: begin
    SetFunExpres(fun);
    //We have to return logical value inverted in A
    _EORi($FF);  //Operand already in regA
  end;
  else
    genError('Not implemented: "%s"', [fun.name]);
  end;
end;
procedure TGenCod.SIF_bool_asig_bool(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  SetFunNull(fun);  //In Pascal an assigment doesn't return type.
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
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
      _LDAi(parB.value.ValInt);  //We have 255 or 0
      _STA(parA.add);
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
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end else if parA.Sto in [stRegistA, stRegister] then begin
    //Assignment to register A
    case parB.Sto of
    stConst : begin
      _LDAi(parB.value.ValInt);  //We have 255 or 0
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
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end else if parA.Sto = stRegistX then begin
    //Assignment to register X
    case parB.Sto of
    stConst : begin
      _LDXi(parB.value.ValInt);
    end;
    stRamFix: begin
      _LDX(parB.add);
    end;
    stRegister, stRegistA: begin  //Already in A
      _TAX_opt;
    end;
    stRegistX: begin  //Already in X
    end;
    stRegistY: begin
      _TYA;  //Modify A
      _TAX;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end else if parA.Sto = stRegistY then begin
    //Assignment to register Y
    case parB.Sto of
    stConst : begin
      _LDYi(parB.value.ValInt);
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
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.SIF_bool_and_bool(fun: TEleExpress);
var
  sale0: integer;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_bool(fun, parA.value.valBool and parB.value.valBool);
    end else if (parA.Sto = stConst) and (parA.value.ValBool = false) then begin
      SetFunConst_bool(fun, false);
    end else if (parB.Sto = stConst) and (parB.value.ValBool = false) then begin
      SetFunConst_bool(fun, false);
    end;
    exit;
  end;
  //Code generation
  if parA.Sto = stConst then begin
     case parB.Sto of
     stConst: begin
       SetFunConst_bool(fun, parA.value.valBool and parB.value.valBool);
     end;
     stRamFix: begin
        if parA.value.valBool = false then begin  //Special case.
          SetFunConst_bool(fun, false);
        end else begin  //Special case.
          SetFunVariab(fun, parB.rVar);  //Can be problematic return "var". Formaly it should be an expression.
        end;
     end;
     stRegister, stRegistA: begin
       if parA.value.valBool = false then begin  //Special case.
         SetFunConst_bool(fun, false);
       end else begin  //Special case.
         SetFunExpres(fun);  //No needed do anything. Result already in A
       end;
     end;
     else
       GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
     end;
  end else if parA.Sto = stRamFix then begin
    case parB.Sto of
    stConst: begin
      if parB.value.valBool = false then begin  //Special case.
        SetFunConst_bool(fun, false);
        exit;
      end else begin  //Special case.
        SetFunVariab(fun, parA.rVar);  //Can be problematic return "var". Formaly it should be an expression.
        exit;
      end;
    end;
    stRamFix: begin
      SetFunExpres(fun);
      _LDA(parA.add);
      _AND(parB.add)
    end;
    stRegister, stRegistA: begin
      SetFunExpres(fun);
      //parB already in A
      _AND(parA.add)
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end else if parA.Sto in [stRegister, stRegistA] then begin
    case parB.Sto of
    stConst: begin
      if parB.value.valBool = false then begin  //Special case.
        SetFunConst_bool(fun, false);
      end else begin  //Special case.
        SetFunExpres(fun);  //No needed do anything. Result already in A
      end;
    end;
    stRamFix: begin
      SetFunExpres(fun);
      //parA already in A
      _AND(parB.add)
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end else begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_bool_or_bool(fun: TEleExpress);
var
  sale0: integer;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_bool(fun, parA.value.valBool or parB.value.valBool);
    end else if (parA.Sto = stConst) and (parA.value.ValBool = true) then begin
      SetFunConst_bool(fun, true);
    end else if (parB.Sto = stConst) and (parB.value.ValBool = true) then begin
      SetFunConst_bool(fun, true);
    end else begin
      exit;
    end;
  end;
  if parA.Sto = stConst then begin
     case parB.Sto of
     stConst: begin
       SetFunConst_bool(fun, parA.value.valBool or parB.value.valBool);
     end;
     stRamFix: begin
        if parA.value.valBool = true then begin  //Special case.
          SetFunConst_bool(fun, true);
        end else begin  //Special case.
          SetFunVariab(fun, parB.rVar);  //Can be problematic return "var". Formaly it should be an expression.
        end;
     end;
     stRegister, stRegistA: begin
       if parA.value.valBool = true then begin  //Special case.
         SetFunConst_bool(fun, true);
       end else begin  //Special case.
         SetFunExpres(fun);  //No needed do anything. Result already in A
       end;
     end;
     else
       GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
     end;
  end else if parA.Sto = stRamFix then begin
    case parB.Sto of
    stConst: begin
      if parB.value.valBool = true then begin  //Special case.
        SetFunConst_bool(fun, true);
        exit;
      end else begin  //Special case.
        SetFunVariab(fun, parA.rVar);  //Can be problematic return "var". Formaly it should be an expression.
        exit;
      end;
    end;
    stRamFix: begin
      SetFunExpres(fun);
      _LDA(parA.add);
      _ORA(parB.add)
    end;
    stRegister, stRegistA: begin
      SetFunExpres(fun);
      //parB already in A
      _ORA(parA.add)
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end else if parA.Sto in [stRegister, stRegistA] then begin
    case parB.Sto of
    stConst: begin
      if parB.value.valBool = true then begin  //Special case.
        SetFunConst_bool(fun, true);
      end else begin  //Special case.
        SetFunExpres(fun);  //No needed do anything. Result already in A
      end;
    end;
    stRamFix: begin
      SetFunExpres(fun);
      //parA already in A
      _ORA(parB.add);
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end else begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_byte_div_byte(fun: TEleExpress);
  var parA, parB: TEleExpress;
      AddrUndef: boolean;
      fdiv: TEleFun;

  procedure DivbyConst;
  begin
    case parB.val of
      1: ;
      2: begin
        _LSRa;
      end;
      3: begin
        _STA(H.addr);
        _LSRa;
        _ADCi(21);
        _LSRa;
        _ADC(H.addr);
        _RORa;
        _LSRa;
        _ADC(H.addr);
        _RORa;
        _LSRa;
        _ADC(H.addr);
        _RORa;
        _LSRa;
      end;
      4: begin
        _LSRa;
        _LSRa;
      end;
      7: begin
        _STA(H.addr);
        _LSRa;
        _LSRa;
        _LSRa;
        _ADC(H.addr);
        _RORa;
        _LSRa;
        _LSRa;
        _ADC(H.addr);
        _RORa;
        _LSRa;
        _LSRa;
      end;
      8: begin
        _LSRa;
        _LSRa;
        _LSRa;
      end;
      16: begin
        _LSRa;
        _LSRa;
        _LSRa;
        _LSRa;
      end;
      32: begin
        _LSRa;
        _LSRa;
        _LSRa;
        _LSRa;
        _LSRa;
      end;
      64: begin
        _LSRa;
        _LSRa;
        _LSRa;
        _LSRa;
        _LSRa;
        _LSRa;
      end;
      128: begin
        _ASLa;
        _LDAi(0);
        _ROLa
      end;
    else
        _LDXi(parB.val);
        functCall(fdiv, AddrUndef);
        _LDA(H.addr);   // Here we need only the div part
    end;
  end;

begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  fdiv := snfBytDivByt8;
  if compMod = cmConsEval then begin
    if (parA.Sto = stConst) and (parB.Sto = stConst) then
      SetFunConst_word(fun, parA.val div parB.val);
    exit;
  end;
    //Code generation
  case stoOperation(parA, parB) of
  stConst_Const:
    SetFunConst_word(fun, parA.val div parB.val);
  stRamFix_Const: begin
    SetFunExpres(fun);
    _LDA(parA.add);
    DivbyConst;
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.val);
    if parA.val > 0 then begin
        _LDX(parB.add);
        functCall(fdiv, AddrUndef);
        _LDA(H.addr);   // Here we need only the div part
    end;
  end;
  stRamFix_RamFix: begin
    SetFunExpres(fun);
        _LDA(parA.add);
        _LDX(parB.add);
        functCall(fdiv, AddrUndef);
        _LDA(H.addr);   // Here we need only the div part
  end;
  stRegist_RamFix: begin
    SetFunExpres(fun);
        //_LDA(parA.add);
        _LDX(parB.add);
        functCall(fdiv, AddrUndef);
        _LDA(H.addr);   // Here we need only the div part
  end;
  stRegist_Const: begin
    SetFunExpres(fun);
    DivbyConst;
  end;
  stConst_Regist: begin
    SetFunExpres(fun);
        _TAX;
        _LDAi(parA.val);
        functCall(fdiv, AddrUndef);
        _LDA(H.addr);   // Here we need only the div part
  end;
  stRamFix_Regist: begin
    SetFunExpres(fun);
        _TAX;
        _LDA(parA.add);
        functCall(fdiv, AddrUndef);
        _LDA(H.addr);   // Here we need only the div part
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_byte_mod_byte(fun: TEleExpress);
  var parA, parB: TEleExpress;
      AddrUndef: boolean;
      fmod: TEleFun;

  procedure ModByConst;
  begin
    case parB.val of
        1: _LDAi(0);
        2: _ANDi(%1);
        4: _ANDi(%11);
        8: _ANDi(%111);
       16: _ANDi(%1111);
       32: _ANDi(%11111);
       64: _ANDi(%111111);
      128: _ANDi(%1111111);
    else
        _LDXi(parB.val);
        functCall(fmod, AddrUndef);
    end;
  end;

begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  fmod := snfBytDivByt8; // the same function as div
  if compMod = cmConsEval then begin
    if (parA.Sto = stConst) and (parB.Sto = stConst) then
      SetFunConst_word(fun, parA.val mod parB.val);
    exit;
  end;
    //Code generation
  case stoOperation(parA, parB) of
  stConst_Const:
    SetFunConst_word(fun, parA.val mod parB.val);
  stRamFix_Const: begin
    SetFunExpres(fun);
    _LDA(parA.add);
    ModByConst;
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.val);
    if parA.val > 0 then begin
        _LDX(parB.add);
        functCall(fmod, AddrUndef);
    end;
  end;
  stRamFix_RamFix: begin
    SetFunExpres(fun);
        _LDA(parA.add);
        _LDX(parB.add);
        functCall(fmod, AddrUndef);
  end;
  stRegist_RamFix: begin
    SetFunExpres(fun);
        //_LDA(parA.add);
        _LDX(parB.add);
        functCall(fmod, AddrUndef);
  end;
  stRegist_Const: begin
    SetFunExpres(fun);
    ModByConst;
  end;
  stConst_Regist: begin
    SetFunExpres(fun);
        _TAX;
        _LDAi(parA.val);
        functCall(fmod, AddrUndef);
  end;
  stRamFix_Regist: begin
    SetFunExpres(fun);
        _TAX;
        _LDA(parA.add);
        functCall(fmod, AddrUndef);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_bool_equal_bool(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_bool(fun, parA.value.valBool = parB.value.valBool);
    end;
    exit;
  end;
  //Code generation
  if parA.Sto = stConst then begin
     case parB.Sto of
     stConst: begin
       SetFunConst_bool(fun, parA.value.valBool = parB.value.valBool);
     end;
     stRamFix: begin
       if parA.value.valBool = false then begin  //Special case.
         SetFunExpres(fun);
         _LDA(parB.add);  // (A = false) is not A
         Invert_A_to_A;
       end else begin  //Special case: parA = True
         SetFunExpres(fun);
         _LDA(parB.add);  //if parB=0 then regA = 0
       end;
     end;
     stRegister, stRegistA: begin
       if parA.value.valBool = false then begin  //Special case.
         if not AcumStatInZ then _TAX;   //Update Z, if needed.
         SetFunExpres(fun);
         Invert_A_to_A;
       end else begin  //Special case: parA = True
         if not AcumStatInZ then _TAX;   //Update Z, if needed.
         SetFunExpres(fun);  //The same
       end;
     end;
     else
       GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
     end;
  end else if parA.Sto = stRamFix then begin
    case parB.Sto of
    stConst: begin
      if parB.value.valBool = false then begin  //Special case.
        SetFunExpres(fun);
        _LDA(parA.add);  // (A = false) is not A
        Invert_A_to_A;
      end else begin  //Special case.
        SetFunExpres(fun);
        _LDA(parA.add);   // (A = true) is A
      end;
    end;
    stRamFix: begin
      SetFunExpres(fun);
      _LDA(parB.add);
      _EOR(parA.add);  //Compare OperA with OperB. Result in A, inverted.
      Invert_A_to_A;
    end;
    stRegister, stRegistA: begin
      { TODO : We should check "lastASMcode" in order to optimize. }
      SetFunExpres(fun);
      //parA in regA
      _EOR(parA.add);  //Compare OperA with OperB. Result in A, inverted.
      Invert_A_to_A;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end else if parA.Sto in [stRegister, stRegistA] then begin
    case parB.Sto of
    stConst: begin
      if parB.value.valBool = false then begin  //Special case.
        SetFunExpres(fun);
        if not AcumStatInZ then _TAX;   //Update Z, if needed.
        Invert_A_to_A;
      end else begin  //Special case.
        SetFunExpres(fun);
        if not AcumStatInZ then _TAX;   //Update Z, if needed.
      end;
    end;
    stRamFix: begin
        SetFunExpres(fun);
        //parA in regA
        _EOR(parB.add);  //Compare OperA with OperB
        Invert_A_to_A;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end else begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_bool_xor_bool(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      SetFunConst_bool(fun, parA.value.valBool xor parB.value.valBool);
    end;
    exit;
  end;
  //Code generation
  if parA.Sto = stConst then begin
     case parB.Sto of
     stConst: begin
       SetFunConst_bool(fun, parA.value.valBool xor parB.value.valBool);
     end;
     stRamFix: begin
       if parA.value.valBool = false then begin  //Special case.
         SetFunExpres(fun);
         _LDA(parB.add);
       end else begin  //Special case: parA = True
         SetFunExpres(fun);
         _LDA(parB.add);
         Invert_A_to_A;
       end;
     end;
     stRegister, stRegistA: begin
       if parA.value.valBool = false then begin  //Special case.
         if not AcumStatInZ then _TAX;   //Update Z, if needed.
         SetFunExpres(fun);
       end else begin  //Special case: parA = True
         if not AcumStatInZ then _TAX;   //Update Z, if needed.
         SetFunExpres(fun);  //The same
         Invert_A_to_A;
       end;
     end;
     else
       GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
     end;
  end else if parA.Sto = stRamFix then begin
    case parB.Sto of
    stConst: begin
      if parB.value.valBool = false then begin  //Special case.
        SetFunExpres(fun);
        _LDA(parA.add);
      end else begin  //Special case.
        SetFunExpres(fun);
        _LDA(parA.add);
        Invert_A_to_A;
      end;
    end;
    stRamFix: begin
      SetFunExpres(fun);
      _LDA(parB.add);
      _EOR(parA.add);
    end;
    stRegister, stRegistA: begin
      { TODO : We should check "lastASMcode" in order to optimize. }
      SetFunExpres(fun);
      //parA in regA
      _EOR(parA.add);  //Compare OperA with OperB. Result in A, inverted.
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end else if parA.Sto in [stRegister, stRegistA] then begin
    case parB.Sto of
    stConst: begin
      if parB.value.valBool = false then begin  //Special case.
        SetFunExpres(fun);
        if not AcumStatInZ then _TAX;   //Update Z, if needed.
      end else begin  //Special case.
        SetFunExpres(fun);
        if not AcumStatInZ then _TAX;   //Update Z, if needed.
        Invert_A_to_A;
      end;
    end;
    stRamFix: begin
        SetFunExpres(fun);
        //parA in regA
        _EOR(parB.add);  //Compare OperA with OperB
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end else begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
{%ENDREGION}
{%REGION Word operations}
procedure TGenCod.SIF_not_word(fun: TEleExpress);
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  requireA;
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    if par.Sto = stConst then SetFunConst_word(fun, (not par.value.valInt) and $FFFF);
    exit;
  end;
  //Code generation
  case par.Sto of
  stConst : begin
    SetFunConst_word(fun, (not par.value.valInt) and $FFFF);
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
//    SetUORResultExpres_byte;
//    //////
//  end;
  else
    genError('Not implemented: "%s"', [fun.name]);
  end;
end;
procedure TGenCod.SIF_word_asig_word(fun: TEleExpress);
var
  idxVar: TEleVarDec;
  parA, parB: TEleExpress;
begin
  SetFunNull(fun);
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  //Validates parA.
  if parA.opType<>otVariab then begin //The only valid type.
    GenError('Only variables can be assigned.');
    exit;
  end;
  //Implements assignment
  if parA.Sto = stRamFix then begin
    case parB.Sto of
    stConst : begin
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
      _LDA(parB.addL);
      _STA(parA.addL);
      _LDA(parB.addH);
      _STA(parA.addH);
    end;
    stRegister: begin   //se asume que se tiene en (H,A)
      _STA(parA.addL);
      _LDA(H.addr);
      _STA(parA.addH);
    end;
    else
      GenError(MSG_UNSUPPORTED, parB.srcDec); exit;
    end;
  end else if parA.Sto = stRegister then begin
    requireA;
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
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end else begin
    GenError('Cannot assign to this Operand.', parA.srcDec); exit;
  end;
end;
procedure TGenCod.SIF_word_asig_byte(fun: TEleExpress);
var
  idxVar: TEleVarDec;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;   //We don't calculate constant here.
  end;
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
      genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.SIF_word_equal_word(fun: TEleExpress);
var
  sale0: integer;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_bool(fun, parA.val = parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin  //compara constantes. Caso especial
    SetFunConst_bool(fun, parA.val = parB.val);
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.valL);
    _CMP(parB.addL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDAi(parA.valH);
    _CMP(parB.addH);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó p2 esta en A
    SetFunExpres(fun);
//    _LDAi(parA.valL);
    _CMPi(parA.valL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDAi(parA.valH);
    _CMP(H.addr);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _LDA(parA.addL);
    _CMPi(parB.valL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDA(parA.addH);
    _CMPi(parB.valH);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parA.addL);
    _CMP(parB.addL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDA(parA.addH);
    _CMP(parB.addH);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
    //_LDA(parA.addL);
    _CMP(parA.addL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDA(parA.addH);
    _CMP(H.addr);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    //_LDA(parA.addL);
    _CMPi(parB.valL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDAi(parB.valH);
    _CMP(H.addr);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
    //_LDA(parA.addL);
    _CMP(parB.addL);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDA(parB.addH);
    _CMP(H.addr);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_word_equal_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
  stoo: TStoOperandsBSIF;
  sale0: integer;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_bool(fun, parA.val = parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  if parA.Sto = stConst then begin
    if parA.valH <> 0 then begin  //Always different
      SetFunConst_bool(fun, false);
      exit;
    end;
    //Compare like bytes
    case parB.Sto of
    stConst: begin  //compara constantes. Caso especial
      SetFunConst_bool(fun, parA.val = parB.val);
    end;
    stRamFix: begin
      SetFunExpres(fun);   //Se pide Z para el resultado
      if parA.val = 0 then begin  //caso especial
        _LDA(parB.add);
      end else begin
        _LDA(parB.add);
        _CMPi(parA.val);
      end;
      Copy_Z_to_A;
    end;
    stRegister, stRegistA: begin  //la expresión p2 se evaluó y esta en A
      if not AcumStatInZ then _TAX;   //Update Z, if needed.
      if parA.val = 0 then begin  //caso especial
        //Nothing
      end else begin
        _CMPi(parA.val);
      end;
      Copy_Z_to_A;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end else if parA.Sto = stRamFix then begin
    _LDA(parA.addH);
    _BNE_post(sale0);  //Jimp if <>zero (Z=0)
    //Need to compare low byte
    case parB.Sto of
    stConst: begin
      SetFunExpres(fun);
      if parB.val = 0 then begin  //caso especial
        _LDA(parA.addL);
      end else begin
        _LDA(parA.addL);
        _CMPi(parB.val);
      end;
    end;
    stRamFix:begin
      SetFunExpres(fun);
      _LDA(parB.add);
      _CMP(parA.addL);
    end;
    stRegister, stRegistA:begin   //parB evaluated in regA
      SetFunExpres(fun);
      _CMP(parA.addL);
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
      exit;
    end;
_LABEL_post(sale0);
    Copy_Z_to_A;
  end else if parA.Sto in [stRegister, stRegistA] then begin
    _LDX(H.addr);  //Load High byte
    _BNE(sale0);  //Jimp if <>zero (Z=0)
    case parB.Sto of
    stConst: begin   //la expresión p1 se evaluó y esta en A
      if not AcumStatInZ then _TAX;   //Update Z, if needed.
      if parB.val = 0 then begin  //caso especial
        //Nothing
      end else begin
        _CMPi(parB.val);
      end;
    end;
    stRamFix:begin  //parA evaluated in regA
      SetFunExpres(fun);
      _CMP(parB.add);
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
_LABEL_post(sale0);
    Copy_Z_to_A;
  end else begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_word_difer_word(fun: TEleExpress);
begin
  SIF_word_equal_word(fun);
  if not Invert(fun) then begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_word_add_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
         SetFunConst_word(fun, parA.val + parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimize
    SetFunConst_word(fun, parA.val + parB.val);
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _CLC;
    _LDAi(parA.valL);
    _ADC(parB.addL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
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
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en (A)
    SetFunExpres(fun);
    _CLC;
    _ADCi(parA.valL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _CLC;
    _LDA(parA.addL);
    _ADCi(parB.valL);
    _TAX;  //Save
    _LDA(parA.addH);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _CLC;
    _LDA(parA.addL);
    _ADC(parB.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en (_H,A)
    SetFunExpres(fun);
    _CLC;
    _ADC(parA.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _CLC;
    _ADCi(parB.valL);
    _TAX;  //Save
    _LDA(H.addr);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _CLC;
    _ADC(parB.addL);
    _TAX;  //Save
    _LDA(H.addr);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_word_add_word(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_word(fun, parA.val + parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimize
    SetFunConst_word(fun, parA.val + parB.val);
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _CLC;
    _LDAi(parA.valL);
    _ADC(parB.addL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _ADC(parB.addH);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en (A)
    SetFunExpres(fun);
    _CLC;
    _ADCi(parA.valL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _ADC(H.addr);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stRamFix_Const: begin
    if parB.val = 0 then begin  //Special case
      SetFunVariab(fun, parA.rVar);
    end else if parB.valL = 0 then begin
      SetFunExpres(fun);
      _CLC;
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
    end;
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _CLC;
    _LDA(parA.addL);
    _ADC(parB.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _ADC(parB.addH);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stRamFix_Regist:begin  //La expresión B se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _CLC;
    _ADC(parA.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _ADC(H.addr);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stRegist_Const: begin  //La expresión A se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _CLC;
    _ADCi(parB.valL);
    _TAX;  //Save
    _LDA(H.addr);
    _ADCi(parB.valH);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stRegist_RamFix:begin  //La expresión A se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _CLC;
    _ADC(parB.addL);
    _TAX;  //Save
    _LDA(H.addr);
    _ADC(parB.addH);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_word_sub_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_word(fun, parA.val-parB.val);  //puede generar error
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const:begin  //suma de dos constantes. Caso especial
    SetFunConst_word(fun, parA.val-parB.val);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _SEC;
    _LDAi(parA.valL);
    _SBC(parB.addL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _SBCi(0);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
//  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
//    SetResultExpres(fun);
//      AddCallerTo(H);  //Declare using register
//    _STA(H);
//    _SEC;
//    _LDA(parA.val);
//    _SBC(H);
//  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.addL);
    _SBCi(parB.valL);
    _TAX;  //Save
    _LDA(parA.addH);
    _SBCi(0);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.addL);
    _SBC(parB.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _SBCi(0);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
//  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBC(parA.add);   //a - p1 -> a
//    //Invierte
//    _EORi($FF);
//    _CLC;
//    _ADCi(1);
//  end;
//  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBCi(parB.val);
//  end;
//  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBC(parB.add);
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_word_sub_word(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_word(fun, parA.val-parB.val);  //puede generar error
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const:begin  //suma de dos constantes. Caso especial
    SetFunConst_word(fun, parA.val-parB.val);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _SEC;
    _LDAi(parA.valL);
    _SBC(parB.addL);
    _TAX;  //Save
    _LDAi(parA.valH);
    _SBC(parB.addH);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
//  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
//    SetResultExpres(fun);
//      AddCallerTo(H);  //Declare using register
//    _STA(H);
//    _SEC;
//    _LDA(parA.val);
//    _SBC(H);
//  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.addL);
    _SBCi(parB.valL);
    _TAX;  //Save
    _LDA(parA.addH);
    _SBCi(parB.valH);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _SEC;
    _LDA(parA.addL);
    _SBC(parB.addL);
    _TAX;  //Save
    _LDA(parA.addH);
    _SBC(parB.addH);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
//  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBC(parA.add);   //a - p1 -> a
//    //Invierte
//    _EORi($FF);
//    _CLC;
//    _ADCi(1);
//  end;
//  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBCi(parB.val);
//  end;
//  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
//    SetResultExpres(fun);
//    _SEC;
//    _SBC(parB.add);
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_word_mul_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_word(fun, parA.val * parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    SetFunConst_word(fun, parA.val*parB.val);  //puede generar error
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_word_aadd_byte(fun: TEleExpress);
var
  L1, L2: integer;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
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
procedure TGenCod.SIF_word_aadd_word(fun: TEleExpress);
var
  L1, L2: integer;
  val2: DWord;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
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
procedure TGenCod.SIF_word_asub_byte(fun: TEleExpress);
var
  L1: integer;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
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
procedure TGenCod.SIF_word_asub_word(fun: TEleExpress);
  var
    parA, parB: TEleExpress;
    L1: integer;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
    //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  if parA.Sto <> stRamFix then begin
    GenError('Cannot assign to this Operand.');
    exit;
  end;

  SetFunNull(fun);
  case parB.Sto of
  stConst : begin
    if parB.val=0 then begin // do nothing
    end else if parB.val=1 then begin  // we can optimize by doing Dec
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
      _SBCi(parB.valH);
      _STA(parA.addH);
    end;
  end;
  stRamFix : begin
      _SEC;
      _LDA(parA.addL);
      _SBCi(parB.addL);
      _STA(parA.addL);
      _LDA(parA.addH);
      _SBCi(parB.addH);
      _STA(parA.addH);
  end;
  stRegister: begin  //ya está en A
      SetFunExpres(fun);
      _STA(E.addr);
      _SEC;
      _LDA(parA.addL);
      _SBC(E.addr);
      _STA(parA.addL);
      _LDA(parA.addH);
      _SBC(H.addr);
      _STA(parA.addH);
  end
  else
    GenError(MSG_UNSUPPORTED); exit;
  end;
end;
procedure TGenCod.SIF_word_gequ_word(fun: TEleExpress);
var
  L1B: integer;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_bool(fun, parA.val >= parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin  //compara constantes. Caso especial
    SetFunConst_bool(fun, parA.val >= parB.val);
  end;
  stConst_RamFix: begin
    if parA.val = 65535 then begin
      //Always true
      SetFunConst_bool(fun, true);
      GenWarn('Expression will always be FALSE or TRUE.');
    end else begin
      SetFunExpres(fun);
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
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
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
  stRamFix_Const: begin
    if parB.val = 0 then begin
      //Alyway true
      SetFunConst_bool(fun, true);
      GenWarn('Expression will always be FALSE or TRUE.');
    end else begin
      SetFunExpres(fun);
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
  stRamFix_RamFix:begin
    SetFunExpres(fun);
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
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en A
    SetFunExpres(fun);
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
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en A
    if parB.val = 0 then begin
      //Alyway true
      SetFunConst_bool(fun, true);
      GenWarn('Expression will always be FALSE or TRUE.');
    end else begin
      SetFunExpres(fun);
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
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en A
    SetFunExpres(fun);
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
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_word_less_word(fun: TEleExpress);
begin
  SIF_word_gequ_word(fun);
  if not Invert(fun) then begin
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_word_great_word(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  fun.elements.Exchange(0,1);
  SIF_word_less_word(fun);
  fun.elements.Exchange(0,1);
end;
procedure TGenCod.SIF_word_lequ_word(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  fun.elements.Exchange(0,1);
  SIF_word_gequ_word(fun);
  fun.elements.Exchange(0,1);
end;
procedure TGenCod.SIF_word_and_byte(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_byte(fun, parA.val and parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimiza
    SetFunConst_byte(fun, parA.val and parB.val);
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.valL);
    _AND(parB.addL);
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en (A)
    SetFunExpres(fun);
    _ANDi(parA.valL);      //Deja en A
  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _LDA(parA.addL);
    _ANDi(parB.valL);
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parA.addL);
    _AND(parB.addL);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en (_H,A)
    SetFunExpres(fun);
    _AND(parA.add);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _ANDi(parB.valL);
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    _AND(parB.addL);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_word_and_word(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_word(fun, parA.val and parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimiza
    SetFunConst_word(fun, parA.val and parB.val);
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.valH);
    _AND(parB.addH);
    _STA(H.addr);
    _LDAi(parA.valL);
    _AND(parB.addL);
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en (A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _ANDi(parA.valL);
    _PHA;  //Save LSB result
    _LDAi(parA.valH);
    _AND(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _LDA(parA.addH);
    _ANDi(parB.valH);
    _STA(H.addr);
    _LDA(parA.addL);
    _ANDi(parB.valL);
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parA.addH);
    _AND(parB.addH);
    _STA(H.addr);
    _LDA(parA.addL);
    _AND(parB.addL);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en (_H,A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _AND(parA.addL);
    _PHA;  //Save LSB result
    _LDA(parA.addH);
    _AND(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _ANDi(parB.valL);
    _PHA;  //Save LSB result
    _LDAi(parB.valH);
    _AND(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _AND(parB.addL);
    _PHA;  //Save LSB result
    _LDA(parB.addH);
    _AND(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_word_or_word(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_word(fun, parA.val or parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimiza
    SetFunConst_word(fun, parA.val or parB.val);
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.valH);
    _ORA(parB.addH);
    _STA(H.addr);
    _LDAi(parA.valL);
    _ORA(parB.addL);
  end;
  stConst_Regist: begin  //la expresión p2 se evaluó y esta en (A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _ORAi(parA.valL);
    _PHA;  //Save LSB result
    _LDAi(parA.valH);
    _ORA(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  stRamFix_Const: begin
    SetFunExpres(fun);
    _LDA(parA.addH);
    _ORAi(parB.valH);
    _STA(H.addr);
    _LDA(parA.addL);
    _ORAi(parB.valL);
  end;
  stRamFix_RamFix:begin
    SetFunExpres(fun);
    _LDA(parA.addH);
    _ORA(parB.addH);
    _STA(H.addr);
    _LDA(parA.addL);
    _ORA(parB.addL);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en (_H,A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _ORA(parA.addL);
    _PHA;  //Save LSB result
    _LDA(parA.addH);
    _ORA(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _ORAi(parB.valL);
    _PHA;  //Save LSB result
    _LDAi(parB.valH);
    _ORA(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    //_LDAi(parA.valL);
    _ORA(parB.addL);
    _PHA;  //Save LSB result
    _LDA(parB.addH);
    _ORA(H.addr);
    _STA(H.addr);
    _PLA;  //Restore LSB result in A
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SNF_byt_div_byt_8(funEleExp: TEleFunBase);
{ Returns Div and Mod (byte, byte)
  Source: http://6502org.wikidot.com/software-math-intdiv
  Input:  A - numerator;
          X - denominator
  Output: _H - quotient (div)
          A  - remainder (mod)
}
  var L1, L2: integer;
begin
  PutLabel('__byt_div_byt_8');
    _STA(H.addr);
    _STX(E.addr);
    _LDAi(0);
    _LDXi(8);
    _ASL(H.addr);
_LABEL_pre(L1);
    _ROLa;
    _CMP(E.addr);
    _BCC_post(L2);
    _SBC(E.addr);
_LABEL_post(L2);
    _ROL(H.addr);
    _DEX;
    _BNE_pre(L1);
    _RTS;
end;
procedure TGenCod.SNF_wrd_div_wrd_16(fun: TEleFunBase);
{ Returns Div and Mod (word, word)
  Source: https://codebase64.org/doku.php?id=base:16bit_division_16-bit_result
  Input:  Dividend  - numerator;
          Divisor   - denominator
  Output: Dividend  - quotient (div)
          Remainder - remainder (mod)
}
  var loop, skip: integer;
      Dividend, Divisor, Remainder: TEleVarDec;
begin
  Dividend  := fun.pars[0].pvar;
  Divisor   := fun.pars[1].pvar;
  Remainder := TEleVarDec(fun.elements[2]);
  PutLabel('__word_div_word');
    _LDAi(0);
    _STA(Remainder.addr);
    _STA(Remainder.addr + 1);
    _LDXi(16);
_LABEL_pre(loop);
    _ASL(Dividend.addr);
    _ROL(Dividend.addr + 1);
    _ROL(Remainder.addr);
    _ROL(Remainder.addr + 1);
    _LDA(Remainder.addr);
    _SEC;
    _SBC(Divisor.addr);
    _TAY;
    _LDA(Remainder.addr + 1);
    _SBC(Divisor.addr + 1);
    _BCC_post(skip);
    _STA(Remainder.addr + 1);
    _STY(Remainder.addr);
    _INC(Dividend.addr);
_LABEL_post(skip);
    _DEX;
    _BNE_pre(loop);
    _RTS;
end;
procedure TGenCod.SNF_word_shift_l(fun: TEleFunBase);
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
procedure TGenCod.SIF_word_shl_byte(fun: TEleExpress);
var
  i, L1, L2: Integer;
  AddrUndef: boolean;
  fInLine: boolean;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  fInLine := false;
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_byte(fun, parA.val << parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimiza
    SetFunConst_word(fun, parA.val << parB.val);
  end;
  stConst_RamFix: begin
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
    AddCallerToFromCurr(snfWordShift_l);  //Declare use
    functCall(snfWordShift_l, AddrUndef);  //Use
_LABEL_post(L2);
  end;
  stRamFix_Const: begin
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
        AddCallerToFromCurr(snfWordShift_l);  //Declare use
        functCall(snfWordShift_l, AddrUndef);  //Use
      end;
    end;
  end;
  stRamFix_RamFix:begin
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
      AddCallerToFromCurr(snfWordShift_l);  //Declare use
      functCall(snfWordShift_l, AddrUndef);  //Use
    end;
_LABEL_post(L2);
  end;
  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta A
    SetFunExpres(fun);
    _TAX_opt;  //Counter
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
      AddCallerToFromCurr(snfWordShift_l);  //Declare use
      functCall(snfWordShift_l, AddrUndef);  //Use
    end;
_LABEL_post(L2);
  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
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
        AddCallerToFromCurr(snfWordShift_l);  //Declare use
        functCall(snfWordShift_l, AddrUndef);  //Use
      end;
    end;
  end;
  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en (H,A)
    _LDXi(parB.add);
    if fInLine then begin
_LABEL_pre(L1);
      _ASLa;
      _ROL(H.addr);
      _DEX;
      _BNE_pre(L1);
    end else begin
      AddCallerToFromCurr(snfWordShift_l);  //Declare use
      functCall(snfWordShift_l, AddrUndef);  //Use
    end;
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_word_shr_byte(fun: TEleExpress);
var
  i: Integer;
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_byte(fun, parA.val >> parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const: begin
    //Optimiza
    SetFunConst_byte(fun, parA.val >> parB.val);
  end;
//  stConst_RamFix: begin
//    SetResultExpres(fun);
//    _LDAi(parA.val);
//    _AND(parB.addL);
//  end;
//  stConst_Regist: begin  //la expresión p2 se evaluó y esta en (A)
//    SetResultExpres(fun);
//    _ANDi(parA.valL);      //Deja en A
//  end;
  stRamFix_Const: begin
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
      genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end;
//  stRamFix_RamFix:begin
//  end;
//  stRamFix_Regist:begin   //la expresión p2 se evaluó y esta en (_H,A)
//  end;
  stRegist_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetFunExpres(fun);
    if parB.val < 4 then begin
      for i:=1 to parB.val do begin
        _LSRa;
        _ROR(H.addr);
      end;
    end else begin
      genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end;
//  stRegist_RamFix:begin  //la expresión p1 se evaluó y esta en (H,A)
//  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
{%ENDREGION}
procedure TGenCod.SIF_dword_asig_dword(fun: TEleExpress);
var
  idxVar: TEleVarDec;
  parA, parB: TEleExpress;
begin
  SetFunNull(fun);
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  //Validates parA.
  if parA.opType<>otVariab then begin //The only valid type.
    GenError('Only variables can be assigned.');
    exit;
  end;
  //Implements assignment
  if parA.Sto = stRamFix then begin
    case parB.Sto of
    stConst : begin
      if (parB.val = 0) and (cpuMode = cpu65C02) then begin
        _STZ(parA.add);
        _STZ(parA.add+1);
        _STZ(parA.add+2);
        _STZ(parA.add+3);
      end else if (parB.valWlo = parB.valWhi) and (parB.valL = parB.valH) then begin
        //all byte parts are equal
        _LDAi(parB.valL);
        _STA(parA.add);
        _STA(parA.add+1);
        _STA(parA.add+2);
        _STA(parA.add+3);
      end else if parB.valWlo = parB.valWhi then begin
        //word parts are equal
        _LDAi(parB.valL);
        _STA(parA.add);
        _STA(parA.add+2);
        _LDAi(parB.valH);
        _STA(parA.add+1);
        _STA(parA.add+3);
      end else begin
        //General case
        _LDAi(parB.valL);
        _STA(parA.add);
        _LDAi(parB.valH);
        _STA(parA.add+1);
        _LDAi(parB.valE);
        _STA(parA.add+2);
        _LDAi(parB.valU);
        _STA(parA.add+3);
      end;
    end;
    stRamFix: begin      //stRamFix-stRamFix
      if parA.add = parB.add then begin
        //Maybe parB is the result of a SIF that identified an assignment target.
      end else begin
        _LDA(parB.add);
        _STA(parA.add);
        _LDA(parB.add+1);
        _STA(parA.add+1);
        _LDA(parB.add+2);
        _STA(parA.add+2);
        _LDA(parB.add+3);
        _STA(parA.add+3);
      end;
    end;
    else
      GenError(MSG_UNSUPPORTED, parB.srcDec); exit;
    end;
  end else begin
    GenError('Cannot assign to this Operand.', parA.srcDec); exit;
  end;
end;
procedure TGenCod.SIF_dword_asig_byte(fun: TEleExpress);
  var parA, parB: TEleExpress;
begin
  SetFunNull(fun);
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B

  //Process special modes of the compiler.
  if compMod = cmConsEval then exit;  //We don't calculate constant here.
  //Validates parA.
  if (parA.opType<>otVariab) or (parA.Sto <> stRamFix) then begin //The only valid type.
    GenError('Only variables can be assigned.');
    exit;
  end;

  case parB.Sto of
  stConst:    //constant
    if (parB.val = 0) and (cpuMode = cpu65C02) then
      _STZ(parA.add)
    else begin
      _LDAi(parB.valL);
      _STA(parA.add);
     end;
  stRamFix:   //variable
    if parA.add = parB.add then begin
      //Maybe parB is the result of a SIF that identified an assignment target.
    end else begin
      _LDA(parB.add);
      _STA(parA.add);
    end;
  stRegister: //expression
      _STA(parA.add);
  end;
  if cpuMode = cpu65C02 then begin
    _STZ(parA.add+1);
    _STZ(parA.add+2);
    _STZ(parA.add+3);
  end else begin
    _LDAi(0);
    _STA(parA.add+1);
    _STA(parA.add+2);
    _STA(parA.add+3);
  end;
end;
procedure TGenCod.SIF_dword_asig_word(fun: TEleExpress);
  var parA, parB: TEleExpress;
begin
  SetFunNull(fun);
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B

  //Process special modes of the compiler.
  if compMod = cmConsEval then exit;  //We don't calculate constant here.
  //Validates parA.
  if (parA.opType<>otVariab) or (parA.Sto <> stRamFix) then begin //The only valid type.
    GenError('Only variables can be assigned.');
    exit;
  end;

  case parB.Sto of
  stConst:    //constant
    if (parB.val = 0) and (cpuMode = cpu65C02) then begin
      _STZ(parA.add);
      _STZ(parA.add+1);
    end else begin
      if (parB.valL = 0) and (cpuMode = cpu65C02) then
        _STZ(parA.add)
      else begin
        _LDAi(parB.valL);
        _STA(parA.add);
      end;
      _LDAi(parB.valH);
      _STA(parA.add+1);
     end;
  stRamFix:   //variable
    if parA.add = parB.add then begin
      //Maybe parB is the result of a SIF that identified an assignment target.
    end else begin
      _LDA(parB.add);
      _STA(parA.add);
      _LDA(parB.add+1);
      _STA(parA.add+1);
    end;
  stRegister: begin //expression
      _STA(parA.add);
      _LDA(H.addr);
      _STA(parA.add+1);
    end;
  end;
  if cpuMode = cpu65C02 then begin
    _STZ(parA.add+2);
    _STZ(parA.add+3);
  end else begin
    _LDAi(0);
    _STA(parA.add+2);
    _STA(parA.add+3);
  end;
end;
procedure TGenCod.SIF_dword_add_dword(fun: TEleExpress);
var
  parA, parB, target: TEleExpress;
  stoo: TStoOperandsBSIF;
  L1, L2, L3: integer;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_dword(fun, parA.val + parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  if not GetAssignTarget(fun, target) then begin
    genError('Internal error.', [BinOperationStr(fun)], fun.srcDec);
    exit;
  end;

  stoo := stoOperation(parA, parB);
  case stoo of
  stConst_Const: begin
    //Optimize
    SetFunConst_dword(fun, parA.val + parB.val);
  end;
  stConst_RamFix, stRamFix_Const: begin
    if stoo = stRamFix_Const then Exchange(parA, parB);
    SetFunVariab(fun, target.add);  //stRamFix

    if (parA.val = 0) and (parB.add = target.add) then exit
    else if (parA.val = 0) and (parB.add <> target.add) then begin
      _LDA(parB.add);
      _STA(target.add);
      _LDA(parB.add+1);
      _STA(target.add+1);
      _LDA(parB.add+2);
      _STA(target.add+2);
      _LDA(parB.add+3);
      _STA(target.add+3);
    end else if (parA.val = 1) and (parB.add = target.add) then begin
      _INC(target.add);
      _BNE_post(L1);
      _INC(target.add+1);
      _BNE_post(L2);
      _INC(target.add+2);
      _BNE_post(L3);
      _INC(target.add+3);
  _LABEL_post(L1);
  _LABEL_post(L2);
  _LABEL_post(L3);
    end else begin
      _CLC;
      _LDAi(parA.valL);
      _ADC(parB.add);
      _STA(target.add);

      _LDAi(parA.valH);
      _ADC(parB.add+1);
      _STA(target.add+1);

      _LDAi(parA.valE);
      _ADC(parB.add+2);
      _STA(target.add+2);

      _LDAi(parA.valU);
      _ADC(parB.add+3);
      _STA(target.add+3);
    end;
  end;
  stRamFix_RamFix: begin
    SetFunVariab(fun, target.add);  //stRamFix
    _CLC;
    _LDA(parA.add);
    _ADC(parB.add);
    _STA(target.add);

    _LDA(parA.add+1);
    _ADC(parB.add+1);
    _STA(target.add+1);

    _LDA(parA.add+2);
    _ADC(parB.add+2);
    _STA(target.add+2);

    _LDA(parA.add+3);
    _ADC(parB.add+3);
    _STA(target.add+3);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_dword_add_byte(fun: TEleExpress);
  var parA, parB, target: TEleExpress;
      stoo: TStoOperandsBSIF;
      L1, L2, L3: integer;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B

  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_dword(fun, parA.val + parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  if not GetAssignTarget(fun, target) then begin
    genError('Internal error.', [BinOperationStr(fun)], fun.srcDec);
    exit;
  end;

  stoo := stoOperation(parA, parB);
  case stoo of
  stConst_Const:
    SetFunConst_dword(fun, parA.val + parB.val);
  stRamFix_Const, stConst_RamFix: begin
    if stoo = stConst_RamFix then Exchange(parA, parB);
    SetFunVariab(fun, target.add);  //stRamFix

    if (parB.val = 0) and (parA.add = target.add) then
      exit
    else if (parB.val = 0) and (parA.add <> target.add) then begin
      _LDA(parA.add);
      _STA(target.add);
      _LDA(parA.add+1);
      _STA(target.add+1);
      _LDA(parA.add+2);
      _STA(target.add+2);
      _LDA(parA.add+3);
      _STA(target.add+3);
      exit;
    end else if (parB.val = 1) and (parA.add = target.add) then begin
      _INC(target.add);
      _BNE_post(L1);
      _INC(target.add+1);
      _BNE_post(L2);
      _INC(target.add+2);
      _BNE_post(L3);
      _INC(target.add+3);
  _LABEL_post(L1);
  _LABEL_post(L2);
  _LABEL_post(L3);
      exit;
    end else begin
      _CLC;
      _LDA(parA.add);
      _ADCi(parB.val);
      _STA(target.add);
    end;
  end;
  stRamFix_RamFix: begin
    SetFunVariab(fun, target.add);  //stRamFix
    _CLC;
    _LDA(parA.add);
    _ADC(parB.add);
    _STA(target.add);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    exit;
  end;

  _LDA(parA.add+1);
  _ADCi(0);
  _STA(target.add+1);

  _LDA(parA.add+2);
  _ADCi(0);
  _STA(target.add+2);

  _LDA(parA.add+3);
  _ADCi(0);
  _STA(target.add+3);
end;
procedure TGenCod.SIF_dword_add_word(fun: TEleExpress);
  var parA, parB, target: TEleExpress;
      stoo: TStoOperandsBSIF;
      L1, L2, L3: integer;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B

  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    //Cases when result is constant
    if (parA.Sto = stConst) and (parB.Sto = stConst) then begin
      if parA.evaluated and parB.evaluated then begin
        SetFunConst_dword(fun, parA.val + parB.val);
      end;
    end;
    exit;
  end;
  //Code generation
  if not GetAssignTarget(fun, target) then begin
    genError('Internal error.', [BinOperationStr(fun)], fun.srcDec);
    exit;
  end;

  stoo := stoOperation(parA, parB);
  case stoo of
  stConst_Const:
    SetFunConst_dword(fun, parA.val + parB.val);
  stRamFix_Const, stConst_RamFix: begin
    if stoo = stConst_RamFix then Exchange(parA, parB);
    SetFunVariab(fun, target.add);  //stRamFix
    // No optimizations for 0 and 1. Byte takes precedence so it will handle them
    _CLC;
    _LDA(parA.add);
    _ADCi(parB.valL);
    _STA(target.add);
    _LDA(parA.add+1);
    _ADCi(parB.valH);
    _STA(target.add+1);
  end;
  stRamFix_RamFix: begin
    SetFunVariab(fun, target.add);  //stRamFix
    _CLC;
    _LDA(parA.add);
    _ADC(parB.add);
    _STA(target.add);
    _LDA(parA.add+1);
    _ADC(parB.add+1);
    _STA(target.add+1);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    exit;
  end;

  _LDA(parA.add+2);
  _ADCi(0);
  _STA(target.add+2);

  _LDA(parA.add+3);
  _ADCi(0);
  _STA(target.add+3);
end;

{%REGION Char operations}
procedure TGenCod.SIF_char_asig_char(fun: TEleExpress);
begin
  SIF_byte_asig_byte(fun);
end;
procedure TGenCod.SIF_char_asig_string(fun: TEleExpress);
var
  parA, parB: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;
  end;
  //Code generation
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
  SIF_byte_asig_byte(fun);
end;
procedure TGenCod.SIF_char_equal_char(fun: TEleExpress);
begin
  SIF_byte_equal_byte(fun);  //es lo mismo
end;
procedure TGenCod.SIF_char_difer_char(fun: TEleExpress);
begin
  SIF_byte_difer_byte(fun); //es lo mismo
end;
{%ENDREGION}
//////////// Pointer operations
procedure TGenCod.SIF_pointer_add_byte(fun: TEleExpress);
{Implementa la suma de un puntero (a cualquier tipo) y un byte.}
var
  ptrType: TEleTypeDec;
  parA: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  ptrType := parA.Typ;   //Se hace aquí porque después puede cambiar parA.
  //La suma de un puntero y un byte, se procesa, como una suma de word y byte.
  SIF_word_add_byte(fun);
  //Devuelve word, pero debe devolver el tipo puntero.
  fun.Typ := ptrType;
//  case fun.Sto of
//  stConst: res.SetAsConst(ptrType);  //Cambia el tipo a la constante
//  //stRamFix: res.SetAsVariab(res.rVar);
//  {Si devuelve variable, solo hay dos posibilidades:
//   1. Que sea la variable puntero, por lo que no hay nada que hacer, porque ya tiene
//      el tipo puntero.
//   2. Que sea la variable Word (y que la otra era constante puntero 0 = nil). En este
//      caso devolverá el tipo Word, lo cual tiene cierto sentido.}
//  stRegister: res.SetAsExpres(ptrType);  //Cambia tipo a la expresión
//  end;
end;
procedure TGenCod.SIF_pointer_add_word(fun: TEleExpress);
{Implementa la suma de un puntero (a cualquier tipo) y un byte.}
var
  ptrType: TEleTypeDec;
  parA: TEleExpress;
begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  ptrType := parA.Typ;   //Se hace aquí porque después puede cambiar parA.
  //La suma de un puntero y un word, se procesa, como una suma de words.
  SIF_word_add_word(fun);
  //Returns word. MUst returns pointer type.
  fun.Typ := ptrType;
//  case fun.Sto of
//  stConst: res.SetAsConst(ptrType);  //Cambia el tipo a la constante
//  //stRamFix: res.SetAsVariab(res.rVar);
//  {Si devuelve variable, solo hay dos posibilidades:
//   1. Que sea la variable puntero, por lo que no hay nada que hacer, porque ya tiene
//      el tipo puntero.
//   2. Que sea la variable Word (y que la otra era constante puntero 0 = nil). En este
//      caso devolverá el tipo Word, lo cual tiene cierto sentido.}
//  stRegister: res.SetAsExpres(ptrType);  //Cambia tipo a la expresión
//  end;
end;

procedure TGenCod.SIF_pointer_sub_byte(fun: TEleExpress);
{Implementa la resta de un puntero (a cualquier tipo) y un byte.}
var
  ptrType: TEleTypeDec;
  parA: TEleExpress;
begin
  //Similar to SIF_pointer_add_byte
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  ptrType := parA.Typ;
  SIF_word_sub_byte(fun);
  //Returns word. MUst returns pointer type.
  fun.Typ := ptrType;
//  case fun.Sto of
//  stConsta  : res.SetAsConst(ptrType);
//  stRegister: res.SetAsExpres(ptrType);
//  end;
end;
procedure TGenCod.SIF_pointer_sub_word(fun: TEleExpress);
{Implementa la resta de un puntero (a cualquier tipo) y un byte.}
var
  ptrType: TEleTypeDec;
  parA: TEleExpress;
begin
  //Similar to SIF_pointer_add_word
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  ptrType := parA.Typ;
  SIF_word_sub_word(fun);
  //Returns word. MUst returns pointer type.
  fun.Typ := ptrType;
//  case fun.Sto of
//  stConsta  : res.SetAsConst(ptrType);
//  stRegister: res.SetAsExpres(ptrType);
//  end;
end;
procedure TGenCod.SIF_derefPointer(fun: TEleExpress; SetRes: boolean);
{Implementa el operador de desreferencia "^", para Opr que se supone debe ser
 categoria "tctPointer", es decir, puntero a algún tipo de dato.}
begin
//  case parA.Sto of
//  stConsta : begin
//    //Caso especial. Cuando se tenga algo como: TPunteroAByte($FF)^
//    //Se asume que devuelve una variable de tipo Byte.
//    tmpVar := CreateTmpVar('', typByte);
//    tmpVar.addr := parA.val;  //Fija dirección de constante
//    SetUORResultVariab(fun, tmpVar);
//  end;
//  stRamFix: begin
//    //Caso común: ptrVar^
//    itemType := parA.Typ.ptrType;  //Type of pointed var
//    //By default we generate code as Setter
//    idxVar := CreateTmpVar('', typWord);
//    idxVar.addr := parA.addr;      //Var pointer as word
//    SetUORResultVarRef(idxVar, itemType);
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
//    SetUORResultExpRef(parA.Typ);
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
procedure TGenCod.SNF_delay_ms(fun: TEleFunBase);
//Codifica rutina de retardo en milisegundos
var
  delay: Word;
  LABEL1, ZERO: integer;
begin
  PutLabel('__delay_ms');
  {Esta rutina recibe los milisegundos en los registros en (H,A) o en (A)
  En cualquier caso, siempre usa el registros H , el acumulador "A" y un reg. auxiliar.
  Se supone que para pasar los parámetros, ya se requirió H, así que no es necesario
  crearlo.}
//  _LDXi(0);     PutComm(' ;enter when parameters in (0,A)');
//  _STX(H);
//  fun.adrr2 := pic.iRam;  {Se hace justo antes de generar código por si se crea
//                          la variable _H}
  _TAY; //PutComm(';enter when parameters in (H,A)');
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
end;
///////////// System INLINE function
procedure TGenCod.SIF_delay_ms(fun: TEleExpress);
var
  par: TEleExpress;
  elefun: TEleFun;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  elefun := TEleFun(fun.rfun);  ////**** VErificar si es válido siempre.
  if par.Typ = typByte then begin
    //El parámetro byte, debe estar en A
    if fun.opType=otFunct then begin
      _LDAi(par.val);
      _JSR(elefun.adrr);
    end else begin
      GenError('Cannot get address of %s', [fun.name]);
    end;
  end else if par.Typ = typWord then begin
    //El parámetro word, debe estar en (H, A)
    if fun.opType=otFunct then begin
      _LDAi(par.valH);
      _STA(H.addr);
      _LDAi(par.valL);
      _JSR(elefun.adrr2);
    end else begin
      GenError('Cannot get address of %s', [fun.name]);
    end;
  end else begin
    GenError(MSG_INVAL_PARTYP, [par.Typ.name]);
    exit;
  end;
end;
procedure TGenCod.SIF_Inc(fun: TEleExpress);
var
  LABEL1, L2: integer;
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  //Validations
  case par.opType of
  otConst: begin GenError('Cannot increase a constant.');exit; end;
  otFunct: begin GenError('Cannot increase a function/procedure or expression result.'); exit; end;
  otVariab: ; //The only valid type.
  else  //Not expected to happen
    GenError('Unimplemented.'); exit;
  end;
  //Code generation
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
      if par.Typ.ptrType.size = 1 then begin
        _INC(par.rVar.addr);
        _BNE_post(LABEL1);  //label
        _INC(par.rVar.addr+1);
_LABEL_post(LABEL1);
      end else if par.Typ.ptrType.size <256 then begin
        _CLC;
        _LDA(par.addL);
        _ADCi(par.Typ.ptrType.size);
        _STA(par.addL);
        _BCC_post(L2);
        _INC(par.addH);
_LABEL_post(L2);
      end else begin
        genError('Not implemented "%s()" for operands "%s".',
                 [fun.name, par.StoAsStr], par.srcDec);
      end;
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
procedure TGenCod.SIF_Dec(fun: TEleExpress);
var
  lbl1: integer;
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  case par.opType of
  otConst: begin GenError('Cannot decrease a constant.');exit; end;
  otFunct: begin GenError('Cannot decrease a function/procedure or expression result.'); exit; end;
  otVariab: ; //The only valid type.
  else  //Not expected to happen
    GenError('Unimplemented.'); exit;
  end;
  //Code generation
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
      if par.Typ.ptrType.size = 1 then begin
        _LDA(par.rVar.addr);
        _BNE_post(lbl1);
        _DEC(par.rVar.addr+1);
  _LABEL_post(lbl1);
        _DEC(par.rVar.addr);
      end else if par.Typ.ptrType.size <256 then begin
        _SEC;
        _LDA(par.addL);
        _SBCi(par.Typ.ptrType.size);
        _STA(par.addL);
        _LDA(par.addH);
        _SBCi(0);
        _STA(par.addH);
      end else begin
        genError('Not implemented "%s()" for operands "%s".',
                 [fun.name, par.StoAsStr], par.srcDec);
      end;
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
procedure TGenCod.SIF_Ord(fun: TEleExpress);
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  case par.Sto of
  stConst : begin
    if par.Typ = typChar then begin
      SetFunConst(fun);
      fun.evaluated := par.evaluated;
      fun.value.valInt := par.value.ValInt;
    end else if par.Typ = typBool then begin
      SetFunConst(fun);
      fun.evaluated := par.evaluated;
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
procedure TGenCod.SIF_Chr(fun: TEleExpress);
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  case par.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if par.Typ = typByte then begin
      SetFunConst(fun);
      fun.evaluated := par.evaluated;
      fun.value.valInt := par.value.ValInt;
    end else if par.Typ = typWord then begin
      SetFunConst(fun);
      fun.evaluated := par.evaluated;
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
procedure TGenCod.SIF_Byte(fun: TEleExpress);
var
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  case par.Sto of
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
    if compMod = cmConsEval then exit;  //We don't generate constants in this case.
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
    if compMod = cmConsEval then exit;  //We don't generate constants in this case.
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
procedure TGenCod.SIF_Word(fun: TEleExpress);
var
  tmpVar: TEleVarDec;
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  case par.Sto of  //El parámetro debe estar en "res"
  stConst : begin
    if par.Typ = typByte then begin
      SetFunConst(fun);
      fun.evaluated := par.evaluated;
      fun.value.ValInt := par.value.ValInt;  //Copy value
    end else if par.Typ = typChar then begin
      SetFunConst(fun);
      fun.evaluated := par.evaluated;
      fun.value.ValInt := par.value.ValInt;  //Copy value
    end else if par.Typ = typWord then begin
      //Already Word
      SetFunConst(fun);
      fun.evaluated := par.evaluated;
      fun.value.ValInt := par.value.ValInt;  //Copy value
    end else begin
      GenError('Cannot convert this constant to word.'); exit;
    end;
  end;
  stRamFix: begin
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
procedure TGenCod.SIF_DWord(fun: TEleExpress);
var
  tmpVar: TEleVarDec;
  par: TEleExpress;
begin
  par := TEleExpress(fun.elements[0]);  //Only one parameter
  case par.Sto of  //El parámetro debe estar en "res"
  stConst : begin
    if par.Typ = typByte then begin
      SetFunConst(fun);
      fun.evaluated := par.evaluated;
      fun.value.ValInt := par.value.ValInt;  //Copy value
    end else if par.Typ = typChar then begin
      SetFunConst(fun);
      fun.evaluated := par.evaluated;
      fun.value.ValInt := par.value.ValInt;  //Copy value
    end else if par.Typ = typWord then begin
      //Already Word
      SetFunConst(fun);
      fun.evaluated := par.evaluated;
      fun.value.ValInt := par.value.ValInt;  //Copy value
    end else begin
      GenError('Cannot convert this constant to word.'); exit;
    end;
  end;
//  stRamFix: begin
//    if par.Typ.IsByteSize then begin
//      SetFunExpres(fun);  //No podemos devolver variable. Pero sí expresión
//      _LDAi(0);
//      _STA(H.addr);
//      _LDA(par.rVar.addr);
//    end else if par.Typ = typWord then begin
//      //ya es Word
//      SetFunVariab(fun, par.add);
//    end else if par.Typ.IsWordSize then begin
//      //Has 2 bytes long, like pointers
//      SetFunVariab(fun, par.add);
//      {We could generate stRegister, but we prefer generate a variable, for simplicity
//      and to have the possibility of assign: word(x) := ...}
//    end else begin
//      SetFunExpres(fun);   //A default operand type
//      GenError('Cannot convert this variable to word.'); exit;
//    end;
//  end;
//  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
end;
procedure TGenCod.SIF_Addr(fun: TEleExpress);
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
      genError('Cannot obtain address for variable "%s"',[par.StoAsStr]);
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
  //otFunct: begin
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
{%REGION Routines for arrays and pointers}
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
procedure TGenCod.SIF_arr_asig_arr(fun: TEleExpress);
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
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
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
        for i:=0 to nBytes-1 do begin
          _LDAi(buffer[i].value);
          _STA(parA.add+i);
        end;
      end else if nBytes< 256 then begin
        //Several ítems, we first write Op2 in RAM.
        CreateValueInCode(parB.Typ, parB.Value, startAddr);
        //Now we have Op2 created in RAM. Lets move.
        _LDXi(nBytes);
_LABEL_pre(j2);
        _LDAx((startAddr-1) and $FFFF);  //Fix address to fit the index loop
        _STAx((parA.rVar.addr-1) and $FFFF);  //Fix address to fit the index loop
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
        _LDAx((parB.rVar.addr-1) and $FFFF);  //Fix address to fit the index loop
        _STAx((parA.rVar.addr-1) and $FFFF);  //Fix address to fit the index loop
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
procedure TGenCod.SIF_obj_asig_obj(fun: TEleExpress);
var
  parA, parB: TEleExpress;
  nBytes: SmallInt;
  des, src: Word;
  j2: integer;
begin
  SetFunNull(fun);  //In Pascal an assigment doesn't return type.
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  if parA.Sto = stRamFix then begin
    nBytes := parA.rVar.typ.size;
    case parB.Sto of
//    stConst: begin
//    end;
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
        _LDAx((parB.rVar.addr-1) and $FFFF);  //Fix address to fit the index loop
        _STAx((parA.rVar.addr-1) and $FFFF);  //Fix address to fit the index loop
        _DEX;
        _BNE_pre(j2);
      end else begin
        GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
      end;
    end;
//    stRegister: begin   //se asume que está en A
//    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    end;
  end else begin
    GenError('Cannot assign to this Operand.', fun.srcDec);
    exit;
  end;
end;
procedure TGenCod.LoadByteIndexWord(const idxvar: TEleVarDec; offset: word);
{Load in A register, the value indexed by "idxvar" variable and added
by "offset". Parameter "idxvar" must by word-size.
NOTE: Doesn't use IX register. We prefer Self-modifying code.}
var
  ad1, ad2: Integer;
begin
  if idxvar.typ.size>2 then begin
    GenError('Not supported this index or pointer type.');
    exit;
  end;
  if (idxvar.addr<256) and (offset<256) then begin
    //Special case
    _LDYi(offset);    //Could be zero.
    pic.codAsm(i_LDA, aIndirecY, idxvar.addr);
  end else if offset=0 then begin
    //Self-modifying.
    if pic.iRam < 256-13 then begin  //Everything can be done on zero-page.
      _LDA(idxvar.addrL);
      _STA($FF); ad1:=pic.iRam-1;  //Save address.
      _LDA(idxvar.addrH);   //LDA absolute
      _STA($FF); ad2:=pic.iRam-1;  //Save address.
      _LDA($FFFF);     //Load byte
      //Complete the addresses.
      pic.ram[ad1].value := pic.iRam-2; //Should be < 256, otherwise check condition: if pic.iram<
      pic.ram[ad2].value := pic.iRam-1; //Should be < 256, otherwise check condition: if pic.iram<
    end else begin
      _LDA(idxvar.addrL);
      _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
      _LDA(idxvar.addrH);   //LDA absolute
      _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
      _LDA($FFFF);     //Load byte
      //Complete the addresses.
      pic.ram[ad1].value   := (pic.iRam-2) and $FF;
      pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
      pic.ram[ad2].value   := (pic.iRam-1) and $ff;
      pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    end;
  end else if offset<256 then begin   //Byte offset
    if pic.iRam < 256-15 then begin  //Everything can be done on zero-page.
      _LDA(idxvar.addrL);
      _STA($FF); ad1:=pic.iRam-1;  //Save address.
      _LDA(idxvar.addrH);   //LDA absolute
      _STA($FF); ad2:=pic.iRam-1;  //Save address.
      _LDYi(offset);
      pic.codAsm(i_LDA, aAbsolutY, $FFFF);  //Instruction will be overwritten
      //Complete the addresses.
      pic.ram[ad1].value := pic.iRam-2; //Should be < 256, otherwise check condition: if pic.iram<
      pic.ram[ad2].value := pic.iRam-1; //Should be < 256, otherwise check condition: if pic.iram<
    end else begin
      _LDA(idxvar.addrL);
      _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
      _LDA(idxvar.addrH);   //LDA absolute
      _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
      _LDYi(offset);
      pic.codAsm(i_LDA, aAbsolutY, $FFFF);  //Instruction will be overwritten
      //Complete the addresses.
      pic.ram[ad1].value   := (pic.iRam-2) and $FF;
      pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
      pic.ram[ad2].value   := (pic.iRam-1) and $ff;
      pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    end;
  end else begin
    //Self-modifying.
    if pic.iRam < 256-18 then begin  //Everything can be done on zero-page.
      _CLC;
      _LDA(idxvar.addrL);
      _ADCi(lo(offset));
      _STA($FF); ad1:=pic.iRam-1;  //Save address.
      _LDA(idxvar.addrH);   //LDA absolute
      _ADCi(hi(offset));
      _STA($FF); ad2:=pic.iRam-1;  //Save address.
      _LDA($FFFF);     //Load byte
      //Complete the addresses.
      pic.ram[ad1].value := pic.iRam-2; //Should be < 256, otherwise check condition: if pic.iram<
      pic.ram[ad2].value := pic.iRam-1; //Should be < 256, otherwise check condition: if pic.iram<
    end else begin
      _CLC;
      _LDA(idxvar.addrL);
      _ADCi(lo(offset));
      _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
      _LDA(idxvar.addrH);   //LDA absolute
      _ADCi(hi(offset));
      _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
      _LDA($FFFF);     //Load byte
      //Complete the addresses.
      pic.ram[ad1].value   := (pic.iRam-2) and $FF;
      pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
      pic.ram[ad2].value   := (pic.iRam-1) and $ff;
      pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    end;
  end;
end;
procedure TGenCod.LoadWordIndexWord(const idxvar: TEleVarDec; offset: word);
{Load in H,A register, the value indexed by "idxVar" variable multiplied by 2 and
added by "offset". Parameter "idxVar" must by word-size.
IMPORTANT: Require IX defined and stored at zero-page
**** Not tested ****. We prefer to use LoadWordIndexWord2() because it's tested and well
optimized although it doesn't use IX register.}
begin
  if idxvar.addr<256 then begin  //*** Good Luck. Index is in Zero-page
    if offset<255 then begin  //Less than 255 because it will be incremeneted
      //Copy in IX.addr+1
      _LDA(idxvar.addr+1);
      _STA(IX.addr+1);
      //Multiply by 2 and Update IX.addr
      _LDA(idxvar.addr);
      _ASLa;
      _STA(IX.addr);
      _ROL(IX.addr+1);
      //Load LSB
      _LDYi(offset);    //Could be zero.
      pic.codAsm(i_LDA, aIndirecY, IX.addr);
    end else begin
      //Load in WR
      _LDA(idxvar.addr);    //LSB
      _LDX(idxvar.addr+1);  //MSB
      _STX(H.addr);  //Could be optimized for offset=0 if using IX.addr+1 instead of H.addr.
      //Multiply by 2
      _ASLa;
      _ROL(H.addr);
      //Add offset and store in IX
      _CLC;
      _ADCi(lo(offset));
      _STA(IX.addr);
      _LDA(H.addr);
      _ADCi(hi(offset));
      _STA(IX.addr+1);
      //Load LSB
      _LDYi(0);
      pic.codAsm(i_LDA, aIndirecY, IX.addr);
    end;
  end else begin                 //*** Bad. Index is in other page.
    //Similar to case "idxvar.addr<256".
    //Load in WR
    _LDA(idxvar.addr);    //LSB
    _LDX(idxvar.addr+1);  //MSB
    _STX(H.addr);  //Could be optimized for offset=0 if using IX.addr+1 instead of H.addr.
    //Multiply by 2
    _ASLa;
    _ROL(H.addr);
    //Add offset and store in IX
    if offset=0 then begin
      _STA(IX.addr);
      _LDA(H.addr);
      _STA(IX.addr+1);
    end else begin
      _CLC;
      _ADCi(lo(offset));
      _STA(IX.addr);
      _LDA(H.addr);
      _ADCi(hi(offset));
      _STA(IX.addr+1);
    end;
    //Load LSB
    _LDYi(0);
    pic.codAsm(i_LDA, aIndirecY, IX.addr);
  end;
  //Load MSB
  _INY;  //To point next byte
  pic.codAsm(i_LDX, aIndirecY, IX.addr);
  _STY(H.addr);     //Returns in H register.
end;
procedure TGenCod.LoadWordIndexWord2(const idxvar: TEleVarDec; offset: word);
{Load in H,A register, the value indexed by "idxvar" variable multiplied by 2 and
added by "offset". Parameter "idxvar" must by word-size.
NOTE: Doesn't use IX register.}
var
  ad1, ad2, lab1: Integer;
begin
  if idxvar.typ.size>2 then begin
    GenError('Not supported this index or pointer type.');
    exit;
  end;
  if (idxvar.addr<256) and (offset<255) then begin  //We need <255
    //Special case
//    //Keep MSB
//    _LDX(idxvar.addr+1);
    //Multiply by 2
    _ASL(idxvar.addr);
    _ROL(idxvar.addr+1);
    //Load MSB
    _LDYi(offset+1);
    pic.codAsm(i_LDA, aIndirecY, idxvar.addr);
    _STA(H.addr);     //Returns in H register.
    //Load LSB
    _DEY;  //To point to LSB
    pic.codAsm(i_LDA, aIndirecY, idxvar.addr);
    //Restore "idxvar"
    _ROR(idxvar.addr+1);  //Restore. Flag C must be still valid before the _ROR.
    _ROR(idxvar.addr);    //Restore OK
//    _STX(idxvar.addr+1);  //Restore all bits.
  end else if offset=0 then begin
    //Load in WR
    _LDA(idxvar.addr);    //LSB
    _LDX(idxvar.addr+1);  //MSB
    _STX(H.addr);  //Could be optimized if writing directly in LDA $FFFF.
    //Multiply by 2
    _ASLa;
    _ROL(H.addr);
    //Load in A
    _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
    _LDA(H.addr);
    _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
    //Start a two-cicles loop to load in H,A
    _LDYi(1);  //Initial offset
_LABEL_pre(lab1);
    _STA(H.addr);  //A->H. Used at the second iteration.
    pic.codAsm(i_LDA, aAbsolutY, $FFFF);  //Instruction will be overwritten
    //Complete the addresses.
    pic.ram[ad1].value   := (pic.iRam-2) and $FF;
    pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
    pic.ram[ad2].value   := (pic.iRam-1) and $ff;
    pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    //Complete the loop
    _DEY;
    _BPL_pre(lab1);  //Stop loop when negative
  end else if offset<255 then begin   //Needs to be <255
    //Similar to case offset=0, but we load offset in Y register.
    //Load in WR
    _LDA(idxvar.addr);    //LSB
    _LDX(idxvar.addr+1);  //MSB
    _STX(H.addr);  //Could be optimized if writing directly in LDA $FFFF.
    //Multiply by 2
    _ASLa;
    _ROL(H.addr);
    //Load in A
    _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
    _LDA(H.addr);
    _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
    //Start a two-cicles loop to load in H,A
    _LDYi(offset+1);  //Initial offset
_LABEL_pre(lab1);
    _STA(H.addr);  //A->H. Used at the second iteration.
    pic.codAsm(i_LDA, aAbsolutY, $FFFF);  //Instruction will be overwritten
    //Complete the addresses.
    pic.ram[ad1].value   := (pic.iRam-2) and $FF;
    pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
    pic.ram[ad2].value   := (pic.iRam-1) and $ff;
    pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    //Complete the loop
    _DEY;
    _CPYi(offset-1);
    _BNE_pre(lab1);  //Stop loop when negative
  end else begin
    //Load in WR
    _LDA(idxvar.addr);    //LSB
    _LDX(idxvar.addr+1);  //MSB
    _STX(H.addr);
    //Multiply by 2 -> H,A
    _ASLa;
    _ROL(H.addr);
    //Add offset and Load in A
    if pic.iRam < 256-21 then begin  //Everything can be done on zero-page.
      _CLC;     //***** Not tested yet.
      _ADCi(lo(offset));
      _STA($FF); ad1:=pic.iRam-1;  //Save address.
      _LDA(H.addr);
      _ADCi(hi(offset));
      _STA($FF); ad2:=pic.iRam-1;  //Save address.
      //Start a two-cicles loop to load in H,A
      _LDYi(1);  //Initial offset
_LABEL_pre(lab1);
      _STA(H.addr);  //A->H. Used at the second iteration.
      pic.codAsm(i_LDA, aAbsolutY, $FFFF);  //Instruction will be overwritten
      //Complete the addresses.
      pic.ram[ad1].value := pic.iRam-2; //Should be < 256, otherwise check condition: if pic.iram<
      pic.ram[ad2].value := pic.iRam-1; //Should be < 256, otherwise check condition: if pic.iram<
      //Complete the loop
      _DEY;
      _BPL_pre(lab1);  //Stop loop when negative
    end else begin      //We need to point to other page
      _CLC;
      _ADCi(lo(offset));
      _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
      _LDA(H.addr);
      _ADCi(hi(offset));
      _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
      //Start a two-cicles loop to load in H,A
      _LDYi(1);  //Initial offset
_LABEL_pre(lab1);
      _STA(H.addr);  //A->H. Used at the second iteration.
      pic.codAsm(i_LDA, aAbsolutY, $FFFF);  //Instruction will be overwritten
      //Complete the addresses.
      pic.ram[ad1].value   := (pic.iRam-2) and $FF;
      pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
      pic.ram[ad2].value   := (pic.iRam-1) and $ff;
      pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
      //Complete the loop
      _DEY;
      _BPL_pre(lab1);  //Stop loop when negative
    end;
  end;
end;
procedure TGenCod.SIF_GetItemIdxByte(fun: TEleExpress);
{SIF for _getitem() method when index is Byte. }
var
  arrVar, idx, op1, op2: TEleExpress;
  itemType: TEleTypeDec;
  offset: Word;
begin
  arrVar := TEleExpress(fun.elements[0]);
  idx := TEleExpress(fun.elements[1]);
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    {Although this SIF can return a stRamFix when index is constant, cannot generate
    a constant.}
    exit;
  end;
  if arrVar.sto = stRamFix then begin
    //Applied to a variable array. The normal.
    itemType := arrVar.Typ.itmType; //Reference to the item type
    //Generate code according to the index storage.
    case idx.Sto of
    stConst: begin  //Constant index. Can return a stRavFix if allocated.
      if arrVar.allocated then begin
        SetFunVariab(fun, arrVar.add + idx.value.valInt * itemType.size);
      end else begin
        //Not yet allocated. We keep as expression to simplify later.
        SetFunExpres(fun);
      end;
    end;
    stRamFix: begin  //Index by variable
      //Calculate offset
      offset := arrVar.add;
      if itemType.IsByteSize then begin  //Must return a byte
        SetFunExpres(fun);
        _LDX(idx.add);
        _LDAx(offset);
      end else if itemType.IsWordSize then begin
        SetFunExpres(fun);
        _LDA(idx.add);  // Load index.
        _ASLa;          // A*2->A. Only work for A<128
        _TAX;           //Move to X
        _INX;           //To point to MSB
        _LDAx(offset);
        _STA(H.addr);
        _DEX;           //To point to LSB
        _LDAx(offset);
      end else begin
        GenError('Cannot get item from this array type: %s.', [arrVar.Typ.name]);
      end;
    end;
    else
//Additional forms could be evaluated here
//      if idx.IsConstantPlusVariable then begin
//        //Is <constant> + <variable> that the SplitExpressions() routines
//        //has allowed pass because knows we can optimize here.
//        op1 := TEleExpress(idx.elements[0]);   //Constant evaluated.
//        op2 := TEleExpress(idx.elements[1]);   //Variable
//        SetFunVariab_RamVarOf(fun, op2.rvar, op1.val, arrVar.rvar); //Index by variable and an offset
//      end else begin
        GenError('Not supported this index.', idx.srcDec);
        exit;
//      end;
    end;
  end else begin
    GenError('Cannot index array with storage %s.', [arrVar.StoAsStr]);
  end;
end;
procedure TGenCod.SIF_GetItemIdxWord(fun: TEleExpress);
{SIF for _getitem() method when index is Word. }
var
  arrVar, idx, op1, op2: TEleExpress;
  itemType: TEleTypeDec;
  offset: Word;
begin
  arrVar := TEleExpress(fun.elements[0]);
  idx := TEleExpress(fun.elements[1]);
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    {Although this SIF can return a stRamFix when index is constant, cannot generate
    a constant.}
    exit;
  end;
  if arrVar.sto = stRamFix then begin
    //Applied to a variable array. The normal.
    itemType := arrVar.Typ.itmType; //Reference to the item type
    //Generate code according to the index storage.
    case idx.Sto of
    stConst: begin  //Constant index. Can return a stRavFix if allocated.
      if arrVar.allocated then begin
        SetFunVariab(fun, arrVar.add + idx.value.valInt * itemType.size);
      end else begin
        //Not yet allocated. We keep as expression to simplify later.
        SetFunExpres(fun);
      end;
    end;
    stRamFix: begin  //Index by variable
      //Calculate offset
      offset := arrVar.add;
      if itemType.IsByteSize then begin  //Must return a byte
        SetFunExpres(fun);
        //Variable index is word-size byte.
        LoadByteIndexWord(idx.rvar, offset);
      end else if itemType.IsWordSize then begin
        SetFunExpres(fun);
        //Variable index is word-size byte.
        //LoadWordIndexWord(idx.rvar, offset);   //Require IX
        LoadWordIndexWord2(idx.rvar, offset)
      end else begin
        GenError('Cannot get item from this array type: %s.', [arrVar.Typ.name]);
      end;
    end;
    else
      GenError('Not supported this index.', idx.srcDec);
      exit;
    end;
  end else begin
    GenError('Cannot index array with storage %s.', [arrVar.StoAsStr]);
  end;
end;
procedure TGenCod.SetByteIndexWord(const idxvar: TEleVarDec; offset: word; parB: TEleExpress);
{Write a value, in the variable indexed by "idxvar" variable and added
by "offset". Parameter "idxvar" must by word-size.
NOTE: Doesn't use IX register. We prefer Self-modifying code.}
var
  ad1, ad2: Integer;
begin
  if idxvar.typ.size>2 then begin
    GenError('Not supported this index or pointer type.');
    exit;
  end;
  if (idxvar.addr<256) and (offset<256) then begin
    //Special case
    case parB.Sto of
    stConst : _LDAi(parB.val);
    stRamFix: _LDA(parB.add);
    stRegister: ; //Operand is already in A
    else
      GenError(MSG_UNSUPPORTED, parB.srcDec); exit;
    end;
    _LDYi(offset);    //Could be zero.
    pic.codAsm(i_STA, aIndirecY, idxvar.addr);
  end else if offset=0 then begin
    //Self-modifying.
    if pic.iRam < 256-13 then begin  //Everything can be done on zero-page.
      _LDX(idxvar.addrL);   //We use LDX to preserve A
      _STX($FF); ad1:=pic.iRam-1;  //Save address.
      _LDX(idxvar.addrH);   //LDA absolute
      _STX($FF); ad2:=pic.iRam-1;  //Save address.
      //Load operand
      case parB.Sto of
      stConst : _LDAi(parB.val);
      stRamFix: _LDA(parB.add);
      stRegister: ; //Operand is already in A
      else
        GenError(MSG_UNSUPPORTED, parB.srcDec); exit;
      end;
      _STA($FFFF);     //Store byte
      //Complete the addresses.
      pic.ram[ad1].value := pic.iRam-2; //Should be < 256, otherwise check condition: if pic.iram<
      pic.ram[ad2].value := pic.iRam-1; //Should be < 256, otherwise check condition: if pic.iram<
    end else begin
      _LDX(idxvar.addrL);   //We use LDX to preserve A
      _STX($FFFF); ad1:=pic.iRam-2;  //Save address.
      _LDX(idxvar.addrH);   //LDA absolute
      _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
      //Load operand
      case parB.Sto of
      stConst : _LDAi(parB.val);
      stRamFix: _LDA(parB.add);
      stRegister: ; //Operand is already in A
      else
        GenError(MSG_UNSUPPORTED, parB.srcDec); exit;
      end;
      _STA($FFFF);     //Store byte
      //Complete the addresses.
      pic.ram[ad1].value   := (pic.iRam-2) and $FF;
      pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
      pic.ram[ad2].value   := (pic.iRam-1) and $ff;
      pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    end;
  end else if offset<256 then begin   //Byte offset
    if pic.iRam < 256-15 then begin  //Everything can be done on zero-page.
      _LDX(idxvar.addrL);   //We use LDX to preserve A
      _STX($FF); ad1:=pic.iRam-1;  //Save address.
      _LDX(idxvar.addrH);   //LDA absolute
      _STX($FF); ad2:=pic.iRam-1;  //Save address.
      _LDYi(offset);
      //Load operand
      case parB.Sto of
      stConst : _LDAi(parB.val);
      stRamFix: _LDA(parB.add);
      stRegister: ; //Operand is already in A
      else
        GenError(MSG_UNSUPPORTED, parB.srcDec); exit;
      end;
      pic.codAsm(i_STA, aAbsolutY, $FFFF);  //Instruction will be overwritten
      //Complete the addresses.
      pic.ram[ad1].value := pic.iRam-2; //Should be < 256, otherwise check condition: if pic.iram<
      pic.ram[ad2].value := pic.iRam-1; //Should be < 256, otherwise check condition: if pic.iram<
    end else begin
      _LDX(idxvar.addrL);   //We use LDX to preserve A
      _STX($FFFF); ad1:=pic.iRam-2;  //Save address.
      _LDX(idxvar.addrH);   //LDA absolute
      _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
      _LDYi(offset);
      //Load operand
      case parB.Sto of
      stConst : _LDAi(parB.val);
      stRamFix: _LDA(parB.add);
      stRegister: ; //Operand is already in A
      else
        GenError(MSG_UNSUPPORTED, parB.srcDec); exit;
      end;
      pic.codAsm(i_STA, aAbsolutY, $FFFF);  //Instruction will be overwritten
      //Complete the addresses.
      pic.ram[ad1].value   := (pic.iRam-2) and $FF;
      pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
      pic.ram[ad2].value   := (pic.iRam-1) and $ff;
      pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    end;
  end else begin
    //Self-modifying.
    if pic.iRam < 256-18 then begin  //Everything can be done on zero-page.
      //Load operand in X
      case parB.Sto of
      stConst : _LDXi(parB.val);
      stRamFix: _LDX(parB.add);
      stRegister: _TAX_opt; //Operand is already in A
      else
        GenError(MSG_UNSUPPORTED, parB.srcDec); exit;
      end;
      _CLC;
      _LDA(idxvar.addrL);
      _ADCi(lo(offset));
      _STA($FF); ad1:=pic.iRam-1;  //Save address.
      _LDA(idxvar.addrH);   //LDA absolute
      _ADCi(hi(offset));
      _STA($FF); ad2:=pic.iRam-1;  //Save address.
      _STX($FFFF);     //Write byte
      //Complete the addresses.
      pic.ram[ad1].value := pic.iRam-2; //Should be < 256, otherwise check condition: if pic.iram<
      pic.ram[ad2].value := pic.iRam-1; //Should be < 256, otherwise check condition: if pic.iram<
    end else begin
      //Load operand in X
      case parB.Sto of
      stConst : _LDXi(parB.val);
      stRamFix: _LDX(parB.add);
      stRegister: _TAX_opt; //Operand is already in A
      else
        GenError(MSG_UNSUPPORTED, parB.srcDec); exit;
      end;
      _CLC;
      _LDA(idxvar.addrL);
      _ADCi(lo(offset));
      _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
      _LDA(idxvar.addrH);   //LDA absolute
      _ADCi(hi(offset));
      _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
      _STX($FFFF);     //Write byte
      //Complete the addresses.
      pic.ram[ad1].value   := (pic.iRam-2) and $FF;
      pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
      pic.ram[ad2].value   := (pic.iRam-1) and $ff;
      pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    end;
  end;
end;
procedure TGenCod.SetWordIndexWord(const idxvar: TEleVarDec; offset: word; parB: TEleExpress);
{Write a value, in te variable indexed by "idxVar" variable multiplied by 2 and
added by "offset". Parameter "idxVar" must by word-size.
NOTE: Doesn't use IX register.}
var
  ad1, ad2, lab1: Integer;
begin
  if idxvar.typ.size>2 then begin
    GenError('Not supported this index or pointer type.');
    exit;
  end;
  if (idxvar.addr<256) and (offset<255) then begin  //We need <255
    //Special case
    //Multiply by 2
    _ASL(idxvar.addr);
    _ROL(idxvar.addr+1);
    case parB.Sto of
    stConst : begin
      //Write LSB
      _LDYi(offset);
      _LDAi(parB.valL);
      pic.codAsm(i_STA, aIndirecY, idxvar.addr);
      //Write MSB
      _INY;  //To point to MSB
      _LDAi(parB.valH);
      pic.codAsm(i_STA, aIndirecY, idxvar.addr);
    end;
    stRamFix: begin
      //Write LSB
      _LDYi(offset);
      _LDA(parB.add);
      pic.codAsm(i_STA, aIndirecY, idxvar.addr);
      //Write MSB
      _INY;  //To point to MSB
      _LDA(parB.add+1);
      pic.codAsm(i_STA, aIndirecY, idxvar.addr);
    end;
    stRegister: begin //Operand is already in A
      //Write LSB
      _LDYi(offset);
      //_LDA(parB.add);
      pic.codAsm(i_STA, aIndirecY, idxvar.addr);
      //Write MSB
      _INY;  //To point to MSB
      _LDA(H.addr);
      pic.codAsm(i_STA, aIndirecY, idxvar.addr);
    end;
    else
      GenError(MSG_UNSUPPORTED, parB.srcDec); exit;
    end;
    //Restore "idxvar"
    _ROR(idxvar.addr+1);  //Restore. Flag C must be still valid before the _ROR.
    _ROR(idxvar.addr);    //Restore OK
  end else if offset=0 then begin
    if parB.Sto = stRegister then _TAX_opt;  //Save A
    //Load in WR
    _LDA(idxvar.addr);    //LSB
    _LDX(idxvar.addr+1);  //MSB
    _STX(H.addr);  //Could be optimized if writing directly in LDA $FFFF.
    //Multiply by 2
    _ASLa;
    _ROL(H.addr);
    //Load in A
    _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
    _LDA(H.addr);
    _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
    //Start a two-cicles loop to load in H,A
    _LDYi(1);  //Initial offset
    //Load MSB
    case parB.Sto of
    stConst   : _LDAi(parB.valH);
    stRamFix  : _LDA(parB.addH);
    stRegister: _LDA(H.addr);
    else
      GenError(MSG_UNSUPPORTED, parB.srcDec); exit;
    end;
_LABEL_pre(lab1);
    pic.codAsm(i_STA, aAbsolutY, $FFFF);  //Instruction will be overwritten
    //Complete the addresses.
    pic.ram[ad1].value   := (pic.iRam-2) and $FF;
    pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
    pic.ram[ad2].value   := (pic.iRam-1) and $ff;
    pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    //Load LSB, for the next iteration.
    case parB.Sto of
    stConst   : _LDAi(parB.valL);
    stRamFix  : _LDA(parB.addL);
    stRegister: _TXA;  //Restore A
    end;
    //Complete the loop
    _DEY;
    _BPL_pre(lab1);  //Stop loop when negative
  end else if offset<255 then begin   //Needs to be <255
    //Similar to case offset=0, but we load offset in Y register.
    if parB.Sto = stRegister then _TAX_opt;  //Save A
    //Load in WR
    _LDA(idxvar.addr);    //LSB
    _LDX(idxvar.addr+1);  //MSB
    _STX(H.addr);  //Could be optimized if writing directly in LDA $FFFF.
    //Multiply by 2
    _ASLa;
    _ROL(H.addr);
    //Load in A
    _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
    _LDA(H.addr);
    _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
    //Start a two-cicles loop to load in H,A
    _LDYi(offset+1);  //Initial offset
    //Load MSB
    case parB.Sto of
    stConst   : _LDAi(parB.valH);
    stRamFix  : _LDA(parB.addH);
    stRegister: _LDA(H.addr);
    else
      GenError(MSG_UNSUPPORTED, parB.srcDec); exit;
    end;
_LABEL_pre(lab1);
    pic.codAsm(i_STA, aAbsolutY, $FFFF);  //Instruction will be overwritten
    //Complete the addresses.
    pic.ram[ad1].value   := (pic.iRam-2) and $FF;
    pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
    pic.ram[ad2].value   := (pic.iRam-1) and $ff;
    pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
    //Load LSB, for the next iteration.
    case parB.Sto of
    stConst   : _LDAi(parB.valL);
    stRamFix  : _LDA(parB.addL);
    stRegister: _TXA;  //Restore A
    end;
    //Complete the loop
    _DEY;
    _CPYi(offset-1);
    _BNE_pre(lab1);  //Stop loop when negative
  end else begin
    if parB.Sto = stRegister then begin
      //We need to save the (H,A).
      _TAX_opt;      //Save A in X
      _LDA(H.addr);  //Save H in Stack
      _PHA;
    end;
    //Load in WR
    _LDA(idxvar.addr);    //LSB
    _LDY(idxvar.addr+1);  //MSB
    _STY(H.addr);
    //Multiply by 2 -> H,A
    _ASLa;
    _ROL(H.addr);
    //Add offset and Load in A
    if pic.iRam < 256-21 then begin  //Everything can be done on zero-page.
      _CLC;     //***** Not tested yet.
      _ADCi(lo(offset));
      _STA($FF); ad1:=pic.iRam-1;  //Save address.
      _LDA(H.addr);
      _ADCi(hi(offset));
      _STA($FF); ad2:=pic.iRam-1;  //Save address.
      //Start a two-cicles loop to load in H,A
      _LDYi(1);  //Initial offset
      //Load MSB
      case parB.Sto of
      stConst   : _LDAi(parB.valH);
      stRamFix  : _LDA(parB.addH);
      stRegister: _PLA;  //Restore H
      else
        GenError(MSG_UNSUPPORTED, parB.srcDec); exit;
      end;
_LABEL_pre(lab1);
      pic.codAsm(i_STA, aAbsolutY, $FFFF);  //Instruction will be overwritten
      //Complete the addresses.
      pic.ram[ad1].value := pic.iRam-2; //Should be < 256, otherwise check condition: if pic.iram<
      pic.ram[ad2].value := pic.iRam-1; //Should be < 256, otherwise check condition: if pic.iram<
      //Load LSB, for the next iteration.
      case parB.Sto of
      stConst   : _LDAi(parB.valL);
      stRamFix  : _LDA(parB.addL);
      stRegister: _TXA;  //Restore A
      end;
      //Complete the loop
      _DEY;
      _BPL_pre(lab1);  //Stop loop when negative
    end else begin      //We need to point to other page
      _CLC;
      _ADCi(lo(offset));
      _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
      _LDA(H.addr);
      _ADCi(hi(offset));
      _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
      //Load MSB
      case parB.Sto of
      stConst   : _LDAi(parB.valH);
      stRamFix  : _LDA(parB.addH);
      stRegister: _PLA;  //Restore H
      else
        GenError(MSG_UNSUPPORTED, parB.srcDec); exit;
      end;
      //Start a two-cicles loop to load in H,A
      _LDYi(1);  //Initial offset
_LABEL_pre(lab1);
      pic.codAsm(i_STA, aAbsolutY, $FFFF);  //Instruction will be overwritten
      //Complete the addresses.
      pic.ram[ad1].value   := (pic.iRam-2) and $FF;
      pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
      pic.ram[ad2].value   := (pic.iRam-1) and $ff;
      pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
      //Load LSB, for the next iteration.
      case parB.Sto of
      stConst   : _LDAi(parB.valL);
      stRamFix  : _LDA(parB.addL);
      stRegister: _TXA;  //Restore A
      end;
      //Complete the loop
      _DEY;
      _BPL_pre(lab1);  //Stop loop when negative
    end;
  end;
end;
procedure TGenCod.SIF_SetItemIndexByte(fun: TEleExpress);
{Write a value to an array item indexed by a BYTE.}
var
  arrVar, idx, parB: TEleExpress;
  itemType: TEleTypeDec;
  parA_add: DWord;
  offset: Word;
begin
  SetFunNull(fun);  //In Pascal an assigment doesn't return type.
  arrVar := TEleExpress(fun.elements[0]);
  idx := TEleExpress(fun.elements[1]);
  parB := TEleExpress(fun.elements[2]);  //Value to assign.
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  if arrVar.sto <> stRamFix then begin
    //Applied to a variable array.
    GenError('Cannot write to this array.');
    exit;
  end;
  if not arrVar.allocated then begin
    GenError('Array not allocated.');
    exit;
  end;
  itemType := arrVar.Typ.itmType;    //Must be the same as parB.typ.
  if itemType.size<>parB.Typ.size then begin
    {Type compatibility is done in Analysis but can be relaxed because of some BOR like
    word := byte.}
    //genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    genError('Incompatible types', parB.srcDec);
    exit;
  end;
  //Generate code according to the index storage.
  if          idx.Sto = stConst then begin  //Constant index
    //It's like assign to a simple variable
    if itemType.IsByteSize then begin
      parA_add := arrVar.add + idx.val;
      case parB.Sto of
      stConst: begin
        _LDAi(parB.val);
        _STA(parA_add);
      end;
      stRamFix: begin
        _LDA(parB.add);
        _STA(parA_add);
      end;
      stRegister, stRegistA: begin  //Already in A
        _STA(parA_add);
      end;
      stRegistX: begin
        _STX(parA_add);
      end;
      stRegistY: begin
        _STY(parA_add);
      end;
      else
        GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
      end;
    end else if itemType.IsWordSize then begin
      parA_add := arrVar.add + idx.val*2;
      case parB.Sto of
      stConst : begin
        if parB.valL = parB.valH then begin  //Lucky case
          _LDAi(parB.valL);
          _STA(parA_add);
          _STA(parA_add+1);
        end else begin  //General case
          //Caso general
          _LDAi(parB.valL);
          _STA(parA_add);
          _LDAi(parB.valH);
          _STA(parA_add+1);
        end;
      end;
      stRamFix: begin
        _LDA(parB.addL);
        _STA(parA_add);
        _LDA(parB.addH);
        _STA(parA_add+1);
      end;
      stRegister: begin   //se asume que se tiene en (H,A)
        _STA(parA_add);
        _LDA(H.addr);
        _STA(parA_add+1);
      end;
      else
        GenError(MSG_UNSUPPORTED, parB.srcDec); exit;
      end;
    end else begin
      GenError('Cannot set item to this array type: %s.', [arrVar.Typ.name]);
    end;
  end else if idx.Sto = stRamFix then begin  //Indexed by variable.
    if itemType.IsByteSize then begin
      offset := arrVar.add;
      case parB.Sto of
      stConst: begin
        _LDAi(parB.val);
        _LDX(idx.add);
        _STAx(offset);
      end;
      stRamFix: begin
        _LDA(parB.add);
        _LDX(idx.add);
        _STAx(offset);
      end;
      stRegister, stRegistA: begin  //Already in A
        _LDX(idx.add);
        _STAx(offset);
      end;
      stRegistX: begin
        _TXA;
        _LDX(idx.add);
        _STAx(offset);
      end;
      stRegistY: begin
        _TYA;
        _LDX(idx.add);
        _STAx(offset);
      end;
      else
        GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
      end;
    end else if itemType.IsWordSize then begin
      offset := arrVar.add;
      case parB.Sto of
      stConst : begin
        _LDA(idx.add);  // Load index.
        _ASLa;          // A*2->A. Only work for A<128
        _TAX;           //Move to X
        if parB.valL = parB.valH then begin  //Lucky case
          _LDAi(parB.valL);
          _STAx(offset);
          _STAx(offset+1);
        end else begin  //General case
          _LDAi(parB.valL);
          _STAx(offset);
          _LDAi(parB.valH);
          _STAx(offset+1);
        end;
      end;
      stRamFix: begin
        _LDA(idx.add);  // Load index.
        _ASLa;          // A*2->A. Only work for A<128
        _TAX;           //Move to X
        _LDA(parB.add);
        _STAx(offset);
        _LDA(parB.add+1);
        _STAx(offset+1);
      end;
      stRegister: begin   //se asume que se tiene en A
        _TAY;     //Save A
        _LDA(idx.add);  // Load index.
        _ASLa;          // A*2->A. Only work for A<128
        _TAX;           //Move to X
        if offset<255 then begin
          pic.codAsm(i_STY, aZeroPagX, offset);
          _LDA(H.addr);
          pic.codAsm(i_STY, aZeroPagX, offset+1);
        end else begin
          _TYA;           //Restore A
          _STAx(offset);
          _LDA(H.addr);
          _STAx(offset+1);
        end;
      end;
      else
        genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
        exit;
      end;
    end else begin
      GenError('Cannot set item to this array type: %s.', [arrVar.Typ.name]);
    end;
  end else begin
    GenError('Not supported this index.', idx.srcDec);
  end;
end;
procedure TGenCod.SIF_SetItemIndexWord(fun: TEleExpress);
{Write a value to an array item indexed by a WORD.}
var
  arrVar, idx, parB: TEleExpress;
  itemType: TEleTypeDec;
  parA_add: DWord;
  offset: Word;
begin
  SetFunNull(fun);  //In Pascal an assigment doesn't return type.
  arrVar := TEleExpress(fun.elements[0]);
  idx := TEleExpress(fun.elements[1]);
  parB := TEleExpress(fun.elements[2]);  //Value to assign.
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  if arrVar.sto <> stRamFix then begin
    //Applied to a variable array.
    GenError('Cannot write to this array.');
    exit;
  end;
  if not arrVar.allocated then begin
    GenError('Array not allocated.');
    exit;
  end;
  itemType := arrVar.Typ.itmType;  //Must be the same as parB.typ.
  if itemType.size<>parB.Typ.size then begin
    {Type compatibility is done in Analysis but can be relaxed because of some BOR like
    word := byte.}
    //genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    genError('Incompatible types', parB.srcDec);
    exit;
  end;
  //Generate code according to the index storage.
  if          idx.Sto = stConst then begin  //Constant index
    //It's like assign to a simple variable
    if itemType.IsByteSize then begin
      parA_add := arrVar.add + idx.val;
      case parB.Sto of
      stConst: begin
        _LDAi(parB.val);
        _STA(parA_add);
      end;
      stRamFix: begin
        _LDA(parB.add);
        _STA(parA_add);
      end;
      stRegister, stRegistA: begin  //Already in A
        _STA(parA_add);
      end;
      stRegistX: begin
        _STX(parA_add);
      end;
      stRegistY: begin
        _STY(parA_add);
      end;
      else
        GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
      end;
    end else if itemType.IsWordSize then begin
      parA_add := arrVar.add + idx.val*2;
      case parB.Sto of
      stConst : begin
        if parB.valL = parB.valH then begin  //Lucky case
          _LDAi(parB.valL);
          _STA(parA_add);
          _STA(parA_add+1);
        end else begin  //General case
          //Caso general
          _LDAi(parB.valL);
          _STA(parA_add);
          _LDAi(parB.valH);
          _STA(parA_add+1);
        end;
      end;
      stRamFix: begin
        _LDA(parB.addL);
        _STA(parA_add);
        _LDA(parB.addH);
        _STA(parA_add+1);
      end;
      stRegister: begin   //se asume que se tiene en (H,A)
        _STA(parA_add);
        _LDA(H.addr);
        _STA(parA_add+1);
      end;
      else
        GenError(MSG_UNSUPPORTED, parB.srcDec); exit;
      end;
    end else begin
      GenError('Cannot set item to this array type: %s.', [arrVar.Typ.name]);
    end;
  end else if idx.Sto = stRamFix then begin  //Indexed by variable.
    //Calculate offset
    offset := arrVar.add;
    if itemType.IsByteSize then begin
      SetByteIndexWord(idx.rvar, offset, parB);
    end else if itemType.IsWordSize then begin
      SetWordIndexWord(idx.rvar, offset, parB);
    end else begin
      GenError('Not supported assigning to array of: %s.', [itemType.name], arrVar.srcDec);
    end;
  end else begin
    GenError('Cannot use this index storage: %s.', [idx.StoAsStr], idx.srcDec);
  end;
end;
function TGenCod.FillArray(parray: TEleExpress): boolean;
{Generates code to fill an array with the value of the A register.
If error, returns FALSE}
var
  n, add_end, n2: Word;
  i, lab1: Integer;
begin
  n := parray.Typ.size;
  add_end := parray.add + n -1;
  if n = 0 then begin
     //Nothing to clear
  end else if n = 1 then begin   //Just one byte
    _STA(parray.add);
  end else if n = 2 then begin  //Es de 2 bytes
    _STA(parray.add);
    _STA(parray.add+1);
  end else if n = 3 then begin  //Es de 3 bytes
    _STA(parray.add);
    _STA(parray.add+1);
    _STA(parray.add+2);
  end else if n = 4 then begin  //Es de 4 bytes
    _STA(parray.add);
    _STA(parray.add+1);
    _STA(parray.add+2);
    _STA(parray.add+3);
  end else if n<256 then begin  //Tamaño pequeño
    _LDXi(n);
_LABEL_pre(lab1);
    _DEX;
    _STAx(parray.add, add_end>255);
    _BNE_pre(lab1);
  end else if n=256 then begin  //Tamaño pequeño
    _LDXi(0);
_LABEL_pre(lab1);
    _STAx(parray.add);
    _INX;
    _BNE_pre(lab1);
  end else if n<512 then begin  //Tamaño pequeño
    n2 := n div 2;
    _LDXi(n2);
_LABEL_pre(lab1);
    _DEX;
    _STAx(parray.add, true);
    _STAx(parray.add + n2, true);
    _BNE_pre(lab1);
    if n mod 2 <> 0 then begin
      _STA(parray.add + n -1);
    end;
  end else if n=512 then begin  //Tamaño pequeño
    _LDXi(0);
_LABEL_pre(lab1);
    _STAx(parray.add, true);
    _STAx(parray.add+256, true);
    _INX;
    _BNE_pre(lab1);
  end else if n<1024 then begin  //Tamaño pequeño
    n2 := n div 4;
    _LDXi(n2);
_LABEL_pre(lab1);
    _DEX;
    _STAx(parray.add, true);
    _STAx(parray.add + n2, true);
    _STAx(parray.add + 2*n2, true);
    _STAx(parray.add + 3*n2, true);
    _BNE_pre(lab1);
    for i:=0 to n mod 4 -1 do begin
      _STA(n2*4 + i);
    end;
  end else if n=1024 then begin
    _LDXi(0);
_LABEL_pre(lab1);
    _STAx(parray.add, true);
    _STAx(parray.add+256, true);
    _STAx(parray.add+512, true);
    _STAx(parray.add+768, true);
    _INX;
    _BNE_pre(lab1);
  end else begin  //Tamaño mayor
    exit(false);
  end;
  //Code geenrated
  exit(true);
end;
procedure TGenCod.SIF_ArrayClear(fun: TEleExpress);
{Used to clear all items of an array operand.}
var
  parray, pvalue: TEleExpress;
begin
  parray := TEleExpress(fun.elements[0]);
  SetFunNull(fun);
//  //Return the same operand
//  SetResultVariab(fun, parray.add);
//  fun.Typ := parray.Typ;
//  fun.Sto := parray.Sto;
  //Clear the array
  case parray.Sto of
  stRamFix: begin
    if fun.elements.Count = 2 then begin
      //There is value to fill.
      pvalue := TEleExpress(fun.elements[1]);  //Value to fill
      if          pvalue.Sto = stConst then begin
        _LDAi(pvalue.valL);
        if not FillArray(parray) then begin
          GenError('Cannot clear a big array', fun.srcDec);
        end;
      end else if pvalue.Sto = stRamFix then begin
        _LDA(pvalue.addL);
        if not FillArray(parray) then begin
          GenError('Cannot clear a big array', fun.srcDec);
        end;
      end else if pvalue.Sto in [stRegister, stRegistA] then begin
        if not FillArray(parray) then begin
          GenError('Cannot clear a big array', fun.srcDec);
        end;
      end else begin
        GenError('Clear error.');
      end;
    end else begin
      //Normal clear
      _LDAi(0);
      if not FillArray(parray) then begin
        GenError('Cannot clear a big array', fun.srcDec);
      end;
    end;
  end;
  stConst: begin
    GenError('Cannot clear a constant array');
  end
  else
    GenError('Cannot clear this array');
  end;
end;
procedure TGenCod.SIF_GetPointer(fun: TEleExpress);
{SIF for getting the value referenced by pointer: p^}
var
  ptrVar: TEleExpress;
  ad1, ad2, lab1: Integer;
begin
  ptrVar := TEleExpress(fun.elements[0]);
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    {Cannot generate a constant.}
    exit;
  end;
  if ptrVar.sto = stConst then begin
    //Applied to a constant pointer.
    SetFunVariab(fun, ptrVar.val);  //Generates a variable
  end else if ptrVar.sto = stRamFix then begin
    //Applied to a variable pointer. The normal.
    if ptrVar.allocated then begin
      SetFunExpres(fun);  //Devolvemos expresión
      if ptrVar.Typ.ptrType = typByte then begin          //^byte
        if ptrVar.add<256 then begin   //In zero page
          _LDXi(0);
          pic.codAsm(i_LDA, aIndirecX, ptrVar.add);
        end else begin
          _LDA(ptrVar.add);    //LSB
          _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
          _LDA(ptrVar.add+1);  //MSB
          _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
          _LDA($FFFF);  //Self modified code
          //Complete the addresses.
          pic.ram[ad1].value   := (pic.iRam-2) and $FF;
          pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
          pic.ram[ad2].value   := (pic.iRam-1) and $ff;
          pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
        end;
      end else if ptrVar.Typ.ptrType = typWord then begin  //^word
        if ptrVar.add<256 then begin   //In zero page
          _LDYi(1);
          pic.codAsm(i_LDA, aIndirecY, ptrVar.add);
          _STA(H.addr);
          _DEY;
          pic.codAsm(i_LDA, aIndirecY, ptrVar.add);
        end else begin
          //Load in WR
          _LDA(ptrVar.add);    //LSB
          _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
          _LDX(ptrVar.add+1);  //MSB
          _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
          //Start a two-cicles loop to load in H,A
          _LDYi(1);  //Initial offset
      _LABEL_pre(lab1);
          _STA(H.addr);  //A->H. Used at the second iteration.
          pic.codAsm(i_LDA, aAbsolutY, $FFFF);  //Instruction will be overwritten
          //Complete the addresses.
          pic.ram[ad1].value   := (pic.iRam-2) and $FF;
          pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
          pic.ram[ad2].value   := (pic.iRam-1) and $ff;
          pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
          //Complete the loop
          _DEY;
          _BPL_pre(lab1);  //Stop loop when negative
        end;
      end else begin  //*** Faltaría implementar los tipos complejos como arreglos
          {Aquí lo que podemos hacer es simplemente generar una variable "stRamVar"
          que sería lo más universal para cualquier tipo de datos. De hecho no se
          necesitaría ya stRamVarOf o stRamReg, porque la regla sería es que todos
          los puteros se "dereferencien" en stRegister (como se hacen con los tipos
          byte y word), pero que se dejen en "stRamVar" para los otros tipos.
          Ya las rutina correspondientes verían como interpretar el "stRamVar",
          como las asignaciones.}
          GenError('Cannot get ^%s.', [ptrVar.Typ.ptrType.name], fun.srcDec);
      end;
    end else begin
      {No allocated. }
      GenError('Variable %s not allocated.', [ptrVar.name], fun.srcDec);
    end;
  end else begin
    GenError('Cannot get variable pointed by storage %s.', [ptrVar.StoAsStr], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_SetPointer(fun: TEleExpress);
{Setter for asignent values to pointer: p^ := }
var
  ptrVar, parB: TEleExpress;
  ptrTypeTo: TEleTypeDec;
  ad1, ad2, lab: Integer;
begin
  SetFunNull(fun);  //In Pascal an assigment doesn't return type.
  ptrVar := TEleExpress(fun.elements[0]);
  parB := TEleExpress(fun.elements[1]);  //Value to assign.
  //Process special modes of the compiler.
  if compMod = cmConsEval then begin
    exit;  //We don't calculate constant here.
  end;
  if ptrVar.sto <> stRamFix then begin
    //Applied to a variable array.
    GenError('Cannot write to this pointer: %s.', [ptrVar.name]);
    exit;
  end;
  if not ptrVar.allocated then begin
    GenError('Variable not allocated: %s.', [ptrVar.name]);
    exit;
  end;
  ptrTypeTo := ptrVar.Typ.ptrType;    //Must be the same as parB.typ.
  if ptrTypeTo.size<>parB.Typ.size then begin
    {Type compatibility is done in Analysis but can be relaxed because of some BOR like
    word := byte.}
    //genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
    genError('Incompatible types', parB.srcDec);
    exit;
  end;
  //Generate code
  if ptrTypeTo.IsByteSize then begin     //^byte
    case parB.Sto of
    stConst: begin
      _LDAi(parB.val);
      if ptrVar.add<256 then begin       //In zero page.
        if cpuMode = cpu65C02 then begin
          _STAin(ptrVar.add);
        end else begin
          _LDYi(0);
          _STAiny(ptrVar.add);
        end;
      end else begin
        _LDX(ptrVar.add);    //LSB
        _STX($FFFF); ad1:=pic.iRam-2;  //Save address.
        _LDX(ptrVar.add+1);  //MSB
        _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
        _STA($FFFF);  //Self modified code
        //Complete the addresses.
        pic.ram[ad1].value   := (pic.iRam-2) and $FF;
        pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
        pic.ram[ad2].value   := (pic.iRam-1) and $ff;
        pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
      end;
    end;
    stRamFix: begin
      _LDA(parB.add);
      if ptrVar.add<256 then begin       //In zero page.
        if cpuMode = cpu65C02 then begin
          _STAin(ptrVar.add);
        end else begin
          _LDYi(0);
          _STAiny(ptrVar.add);
        end;
      end else begin
        _LDX(ptrVar.add);    //LSB
        _STX($FFFF); ad1:=pic.iRam-2;  //Save address.
        _LDX(ptrVar.add+1);  //MSB
        _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
        _STA($FFFF);  //Self modified code
        //Complete the addresses.
        pic.ram[ad1].value   := (pic.iRam-2) and $FF;
        pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
        pic.ram[ad2].value   := (pic.iRam-1) and $ff;
        pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
      end;
    end;
    stRegister, stRegistA: begin  //Already in A
      if ptrVar.add<256 then begin       //In zero page.
        if cpuMode = cpu65C02 then begin
          _STAin(ptrVar.add);
        end else begin
          _LDYi(0);
          _STAiny(ptrVar.add);
        end;
      end else begin
        _LDX(ptrVar.add);    //LSB
        _STX($FFFF); ad1:=pic.iRam-2;  //Save address.
        _LDX(ptrVar.add+1);  //MSB
        _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
        _STA($FFFF);  //Self modified code
        //Complete the addresses.
        pic.ram[ad1].value   := (pic.iRam-2) and $FF;
        pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
        pic.ram[ad2].value   := (pic.iRam-1) and $ff;
        pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
      end;
    end;
    stRegistX: begin
      _TXA;
      if ptrVar.add<256 then begin       //In zero page.
        if cpuMode = cpu65C02 then begin
          _STAin(ptrVar.add);
        end else begin
          _LDYi(0);
          _STAiny(ptrVar.add);
        end;
      end else begin
        _LDX(ptrVar.add);    //LSB
        _STX($FFFF); ad1:=pic.iRam-2;  //Save address.
        _LDX(ptrVar.add+1);  //MSB
        _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
        _STA($FFFF);  //Self modified code
        //Complete the addresses.
        pic.ram[ad1].value   := (pic.iRam-2) and $FF;
        pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
        pic.ram[ad2].value   := (pic.iRam-1) and $ff;
        pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
      end;
    end;
    stRegistY: begin
      _TYA;
      if ptrVar.add<256 then begin       //In zero page.
        if cpuMode = cpu65C02 then begin
          _STAin(ptrVar.add);
        end else begin
          _LDYi(0);
          _STAiny(ptrVar.add);
        end;
      end else begin
        _LDX(ptrVar.add);    //LSB
        _STX($FFFF); ad1:=pic.iRam-2;  //Save address.
        _LDX(ptrVar.add+1);  //MSB
        _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
        _STA($FFFF);  //Self modified code
        //Complete the addresses.
        pic.ram[ad1].value   := (pic.iRam-2) and $FF;
        pic.ram[ad1+1].value := (pic.iRam-2) >> 8;
        pic.ram[ad2].value   := (pic.iRam-1) and $ff;
        pic.ram[ad2+1].value := (pic.iRam-1) >> 8;
      end;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [BinOperationStr(fun)]);
    end;
  end else if ptrTypeTo.IsWordSize then begin
    case parB.Sto of
    stConst : begin                    // w^ := $1234;
      if ptrVar.add<256 then begin
        _LDYi(0);
        _LDAi(parB.valL);
        _STAiny(ptrVar.add);
        _INY;
        if parB.valH <> parB.valL then
          _LDAi(parB.valH);
        _STAiny(ptrVar.add);
      end else begin
        _LDA(ptrVar.addL);
        _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
        _LDA(ptrVar.addH);
        _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
        _LDXi(1);
        _LDAi(parB.valH);
  _LABEL_pre(lab);
        _STAx($FFFF); _SELFMODw(ad1, ad2);
        if parB.valH <> parB.valL then
          _LDAi(parB.valL);
        _DEX;
        _BPL_pre(lab);
      end;
    end;
    stRamFix: begin                    // w^ := word_var;
      if ptrVar.add<256 then begin
        _LDYi(0);
        _LDA(parB.addL);
        _STAiny(ptrVar.add);
        _INY;
        if parB.addH <> parB.addL then
          _LDA(parB.addH);
        _STAiny(ptrVar.add);
      end else begin
        _LDA(ptrVar.addL);
        _STA($FFFF); ad1:=pic.iRam-2;  //Save address.
        _LDA(ptrVar.addH);
        _STA($FFFF); ad2:=pic.iRam-2;  //Save address.
        _LDXi(1);
        _LDA(parB.addH);
  _LABEL_pre(lab);
        _STAx($FFFF); _SELFMODw(ad1, ad2);
        if parB.addH <> parB.addL then
          _LDA(parB.addL);
        _DEX;
        _BPL_pre(lab);
      end;
    end;
    stRegister: begin                  // w^ := expression; of type word (_H/A)
        _LDX(ptrVar.addL);
        _STX($FFFF); ad1:=pic.iRam-2;  //Save address.
        _LDX(ptrVar.addH);
        _STX($FFFF); ad2:=pic.iRam-2;  //Save address.
        _LDXi(1);
        _TAY;
        _LDA(H.addr);
  _LABEL_pre(lab);
        _STAx($FFFF); _SELFMODw(ad1, ad2);
        _TYA;
        _DEX;
        _BPL_pre(lab);
    end;
    else
      genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
      exit;
    end;
  end else begin
    GenError('Cannot set item to this pointer type: %s.', [ptrVar.Typ.name]);
  end;
end;
procedure TGenCod.SIF_word_div_word(fun: TEleExpress);
  var parA, parB: TEleExpress;
      AddrUndef: boolean;
      fdiv: TEleFun;
      Dividend, Divisor: TEleVarDec;

  procedure DivbyConst;
    procedure Div2(n: integer);
      var i: integer;
    begin
        _LDA(parA.addH);
        _LSRa;
        _STA(H.addr);
        _LDA(parA.addL);
        _RORa;
      for i := 1 to n do begin
        _LSR(H.addr);
        _RORa;
      end;
    end;
    procedure Div2H(n: integer);
      var i: integer;
    begin
      if cpuMode = cpu65C02 then
        _STZ(H.addr)
      else begin
        _LDAi(0);
        _STA(H.addr);
      end;
        _LDA(parA.addH);
      for i := 1 to n do
        _LSRa;
    end;

  begin
    case parB.val of
      1: begin
        _LDA(parA.addH);
        _STA(H.addr);
        _LDA(parA.addL);
      end;
          2: Div2(0);
          4: Div2(1);
          8: Div2(2);
         16: Div2(3);
         32: Div2(4);
         64: Div2(5);
        128: Div2(6);
        256: Div2H(0);
        512: Div2H(1);
       1024: Div2H(2);
       2048: Div2H(3);
       4096: Div2H(4);
       8192: Div2H(5);
      16384: Div2H(6);
      32768: begin
        Div2H(0);  // to zero H
        _ASLa;
        _LDAi(0);
        _ROLa;
      end
    else
        _LDA(parA.addH);
        _STA(Dividend.addrH);
        _LDA(parA.addL);
        _STA(Dividend.addrL);
        _LDAi(parB.valH);
        _STA(Divisor.addrH);
        _LDAi(parB.valL);
        _STA(Divisor.addrL);
        functCall(fdiv, AddrUndef);
        _LDA(Dividend.addrH);  // Dividend contain DIV
        _STA(H.addr);
        _LDA(Dividend.addrL);
    end;
  end;

begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  fdiv := snfWrdDivWrd16;
  Dividend := fdiv.pars[0].pvar;
  Divisor  := fdiv.pars[1].pvar;
  if compMod = cmConsEval then begin
    if (parA.Sto = stConst) and (parB.Sto = stConst) then
      SetFunConst_word(fun, parA.val div parB.val);
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const:
    SetFunConst_word(fun, parA.val div parB.val);
  stRamFix_Const: begin
    SetFunExpres(fun);
    DivbyConst;
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.valH);
    _STA(Dividend.addrH);
    _LDAi(parA.valL);
    _STA(Dividend.addrL);
    _LDA(parB.addH);
    _STA(Divisor.addrH);
    _LDA(parB.addL);
    _STA(Divisor.addrL);
    functCall(fdiv, AddrUndef);
    _LDA(Dividend.addrH);  // Dividend contain DIV
    _STA(H.addr);
    _LDA(Dividend.addrL);
  end;
  stRamFix_RamFix: begin
    SetFunExpres(fun);
    _LDA(parA.addH);
    _STA(Dividend.addrH);
    _LDA(parA.addL);
    _STA(Dividend.addrL);
    _LDA(parB.addH);
    _STA(Divisor.addrH);
    _LDA(parB.addL);
    _STA(Divisor.addrL);
    functCall(fdiv, AddrUndef);
    _LDA(Dividend.addrH);  // Dividend contain DIV
    _STA(H.addr);
    _LDA(Dividend.addrL);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.SIF_word_mod_word(fun: TEleExpress);
  var parA, parB: TEleExpress;
      AddrUndef: boolean;
      fdiv: TEleFun;
      Dividend, Divisor, Remainder: TEleVarDec;

  procedure ModbyConst;

    procedure Mod2(n: integer);
    begin
      if cpuMode = cpu65C02 then
        _STZ(H.addr)
      else begin
        _LDAi(0);
        _STA(H.addr);
      end;
        _LDA(parA.addL);
      if n <> 0 then
        _ANDi(n);
    end;

    procedure Mod2H(n: integer);
    begin
        _LDA(parA.addH);
        _ANDi(n);
        _STA(H.addr);
        _LDA(parA.addL);
    end;

  begin
    case parB.val of
      0: ;  // there is no no mod0
      1: begin
        _LDAi(0);
        _STA(H.addr);
      end;
          2: Mod2(1);
          4: Mod2(3);
          8: Mod2(7);
         16: Mod2(15);
         32: Mod2(31);
         64: Mod2(63);
        128: Mod2(127);
        256: Mod2(0);
        512: Mod2H(1);
       1024: Mod2H(3);
       2048: Mod2H(7);
       4096: Mod2H(15);
       8192: Mod2H(31);
      16384: Mod2H(63);
      32768: Mod2H(127);
    else
        _LDA(parA.addH);
        _STA(Dividend.addrH);
        _LDA(parA.addL);
        _STA(Dividend.addrL);
        _LDAi(parB.valH);
        _STA(Divisor.addrH);
        _LDAi(parB.valL);
        _STA(Divisor.addrL);
        functCall(fdiv, AddrUndef);
        _LDA(Remainder.addrH);  // Remainder contain MOD
        _STA(H.addr);
        _LDA(Remainder.addrL);
    end;
  end;

begin
  parA := TEleExpress(fun.elements[0]);  //Parameter A
  parB := TEleExpress(fun.elements[1]);  //Parameter B
  fdiv := snfWrdDivWrd16;
  Dividend := fdiv.pars[0].pvar;
  Divisor  := fdiv.pars[1].pvar;
  Remainder := TEleVarDec(fdiv.elements[2]);
  if compMod = cmConsEval then begin
    if (parA.Sto = stConst) and (parB.Sto = stConst) then
      SetFunConst_word(fun, parA.val mod parB.val);
    exit;
  end;
  //Code generation
  case stoOperation(parA, parB) of
  stConst_Const:
    SetFunConst_word(fun, parA.val div parB.val);
  stRamFix_Const: begin
    SetFunExpres(fun);
    ModbyConst;
  end;
  stConst_RamFix: begin
    SetFunExpres(fun);
    _LDAi(parA.valH);
    _STA(Dividend.addrH);
    _LDAi(parA.valL);
    _STA(Dividend.addrL);
    _LDA(parB.addH);
    _STA(Divisor.addrH);
    _LDA(parB.addL);
    _STA(Divisor.addrL);
    functCall(fdiv, AddrUndef);
    _LDA(Remainder.addrH);  // Dividend contain DIV
    _STA(H.addr);
    _LDA(Remainder.addrL);
  end;
  stRamFix_RamFix: begin
    SetFunExpres(fun);
    _LDA(parA.addH);
    _STA(Dividend.addrH);
    _LDA(parA.addL);
    _STA(Dividend.addrL);
    _LDA(parB.addH);
    _STA(Divisor.addrH);
    _LDA(parB.addL);
    _STA(Divisor.addrL);
    functCall(fdiv, AddrUndef);
    _LDA(Remainder.addrH);  // Dividend contain DIV
    _STA(H.addr);
    _LDA(Remainder.addrL);
  end;
  else
    genError(MSG_CANNOT_COMPL, [BinOperationStr(fun)], fun.srcDec);
  end;
end;
procedure TGenCod.DefineShortPointer(etyp: TEleTypeDec);
{Configura las operaciones que definen la aritmética de punteros.}
//var
//  opr: TxpOperator;
begin
  //Asignación desde Byte y Puntero
//  opr:=etyp.CreateBinaryOperator(':=',2,'_set');
//  opr.isSetter :=
//  opr.CreateOperation(typByte, @SIF_byte_asig_byte);
//  opr.CreateOperation(etyp   , @SIF_byte_asig_byte);
//  //Agrega a los bytes, la posibilidad de ser asignados por punteros
//  typByte.operAsign.CreateOperation(etyp, @SIF_byte_asig_byte);
//
//  opr:=etyp.CreateBinaryOperator('=',3,'equal');  //asignación
//  opr.CreateOperation(typByte, @SIF_byte_equal_byte);
//  opr:=etyp.CreateBinaryOperator('+',4,'add');  //suma
//  opr.CreateOperation(typByte, @SIF_pointer_add_byte);
//  opr:=etyp.CreateBinaryOperator('-',4,'add');  //resta
//  opr.CreateOperation(typByte, @SIF_pointer_sub_byte);
//
//  etyp.CreateUnaryPostOperator('^',6,'deref', @SIF_derefPointer);  //dereferencia
end;
procedure TGenCod.DefineArray(etyp: TEleTypeDec);
var
  consDec: TEleConsDec;
  expr: TEleExpress;
  f, f1, f2: TEleFun;
begin
  //Create assigement method
  f := CreateInBOMethod(etyp, ':=', '_set', etyp, typNull, @SIF_arr_asig_arr);
  f.getset := gsSetInSimple;
  //Create attribute "low" as constant.
  AddConstDeclarByte('low', 0);
  //Create methods
//  CreateUOMethod(etyp, '', 'length', typByte, @arrayLength);
  CreateInUOMethod(etyp, '', 'high'  , typByte, @arrayHigh);
  CreateInUOMethod(etyp, '', 'clear' , typNull, @SIF_ArrayClear);
//  CreateInBOMethod(etyp, '', 'fill' , typByte, typNull, @SIF_ArrayFill);
  //Getters and setters.
  {Note we define only two getters, one for byte-index and one for word-index. Formally
  we should create getters and setters for each type of the item.}
  f1 := CreateInBOMethod(etyp, '', '_getitem', typByte, etyp.itmType, @SIF_GetItemIdxByte);
  f1.getset := gsGetInItem;
  f2 := CreateInBOMethod(etyp, '', '_getitem', typWord, etyp.itmType, @SIF_GetItemIdxWord);
  f2.getset := gsGetInItem;
  //AddCallerToFrom(IX, f.bodyNode);  //Dependency
  f := CreateInTerMethod(etyp, '_setitem', typByte, etyp.itmType, typNull, @SIF_SetItemIndexByte);
  f.getset := gsSetInItem;
  f1.funset := f;         //Connect to getter
  f := CreateInTerMethod(etyp, '_setitem', typWord, etyp.itmType, typNull, @SIF_SetItemIndexWord);
  f.getset := gsSetInItem;
  f2.funset := f;         //Connect to getter
  //Operation for pointers
//  CreateUOMethod(etyp, '@', 'addr', typWord, @SIF_address);
end;
procedure TGenCod.DefinePointer(etyp: TEleTypeDec);
{Set operations that defines pointers aritmethic.}
var
  f, f1: TEleFun;
begin
  //Asignación desde word y Puntero
  f := CreateInBOMethod(etyp, ':=', '_set', typWord, typNull, @SIF_word_asig_word);
  f.getset := gsSetInSimple;
  f := CreateInBOMethod(etyp, ':=', '_set', etyp, typNull, @SIF_word_asig_word);
  f.getset := gsSetInSimple;
  //Getter and setter
  f1 := CreateInUOMethod(etyp, '', '_getptr', etyp.ptrType, @SIF_GetPointer);
  f1.getset := gsGetInPtr;
  f := CreateInBOMethod(etyp, '', '_setptr', etyp.ptrType, typNull, @SIF_SetPointer);
  f.getset := gsSetInPtr;
  f1.funset := f;

  CreateInBOMethod(etyp, '=',  '_equ',  typWord, typBool, @SIF_word_equal_word);
  CreateInBOMethod(etyp, '=',  '_equ',  etyp   , typBool, @SIF_word_equal_word);

  CreateInBOMethod(etyp, '+',  '_add',  typWord, etyp   , @SIF_pointer_add_word);
  CreateInBOMethod(etyp, '+',  '_add',  typByte, etyp   , @SIF_pointer_add_byte);

  CreateInBOMethod(etyp, '-',  '_sub',  typWord, etyp   , @SIF_pointer_sub_word);
  CreateInBOMethod(etyp, '-',  '_sub',  typByte, etyp   , @SIF_pointer_sub_byte);
  CreateInBOMethod(etyp, '>' , '_gre',  etyp   , typBool, @SIF_word_great_word);
  CreateInBOMethod(etyp, '<' , '_les',  etyp   , typBool, @SIF_word_less_word);
  CreateInBOMethod(etyp, '>=', '_gequ', etyp   , typBool, @SIF_word_gequ_word);
  CreateInBOMethod(etyp, '<=', '_lequ', etyp   , typBool, @SIF_word_lequ_word);

  f := CreateInBOMethod(etyp, '+=', '_aadd', typWord, etyp, @SIF_word_aadd_word);
  f.getset := gsSetOther;
  f := CreateInBOMethod(etyp, '+=', '_aadd', typByte, etyp, @SIF_word_aadd_byte);
  f.getset := gsSetOther;
//  etyp.CreateUnaryPostOperator('^',6, 'deref', @SIF_derefPointer);  //dereferencia
end;
procedure TGenCod.DefineObject(etyp: TEleTypeDec);
var
  consDec: TEleConsDec;
  expr: TEleExpress;
  f, f1, f2: TEleFun;
begin
  //Create assigement method
  f := CreateInBOMethod(etyp, ':=', '_set', etyp, typNull, @SIF_obj_asig_obj);
  f.getset := gsSetInSimple;
end;
{%ENDREGION}
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
begin
  //Define métodos a usar
  OnExprStart := @expr_start;
  OnExprEnd   := @expr_End;

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
  pars[n].isLocVar := false;
end;
procedure TGenCod.AddLocVar(var pars: TxpParFuncArray; parName: string; const srcPos: TSrcPos;
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
  pars[n].isLocVar := true;
end;
function TGenCod.AddSysInlineFunction(name: string; retType: TEleTypeDec; const srcPos: TSrcPos;
               const pars: TxpParFuncArray; codSys: TCodSysInline): TEleFun;
{Create a new system function in the current element of the Syntax Tree.
 Returns the reference to the function created.
   pars   -> Array of parameters for the function to be created.
   codSys -> SIF Routine or the the routine to generate de code.
}
var
   fundec: TEleFunDec;
   tmpLoc: TxpEleLocation;
begin
  tmpLoc := curLocation;     //Save current location. We are going to change it.
  //Add declaration
  curLocation := locInterface;
  fundec := AddFunctionDEC(name, retType, srcPos, pars, false);
  fundec.callType := ctSysInline; //INLINE function
  //Implementation
  {Note that implementation is added always after declarartion. It's not the usual
  in common units, where all declarations are first}
  curLocation := locImplement;
  Result := AddFunctionIMP(name, retType, srcPos, fundec, true);
  //Here variables can be added
  {Create a body, to be uniform with normal function and for have a space where
  compile code and access to posible variables or other elements.}
  TreeElems.AddBodyAndOpen(SrcPos);  //Create body
  Result.callType     := ctSysInline; //INLINE function
  Result.codSysInline := codSys;  //Set routine to generate code o SIF routine.
  TreeElems.CloseElement;  //Close body
  TreeElems.CloseElement;  //Close function implementation
  curLocation := tmpLoc;   //Restore current location
end;
function TGenCod.AddSysNormalFunction(name: string; retType: TEleTypeDec; const srcPos: TSrcPos;
               var pars: TxpParFuncArray; codSys: TCodSysNormal): TEleFun;
{Create a new system function in the current element of the Syntax Tree.
 Returns the reference to the function created.
   pars   -> Array of parameters for the function to be created.
   codSys -> SIF Routine or the the routine to generate de code.
}
var
  local_vars: TxpParFuncArray;   //Array for local variables

  procedure extract_local_vars;
  //Extract the local variables from "pars" to "local_vars"
  var
    i, n: Integer;
  begin
    n := 0;
    setlength(local_vars, n);
    i := 0;
    while i<= high(pars) do begin
      if pars[i].isLocVar then begin
        inc(n);
        setlength(local_vars, n);
        local_vars[n-1] := pars[i];
        delete(pars, i, 1);  //Delete element
      end else begin
        inc(i);
      end;
    end;
  end;
var
   fundec: TEleFunDec;
   tmpLoc: TxpEleLocation;
   locvar: TEleVarDec;
   i: Integer;
begin
  extract_local_vars();
  tmpLoc := curLocation;     //Save current location. We are going to change it.
  //Add declaration
  curLocation := locInterface;
  fundec := AddFunctionDEC(name, retType, srcPos, pars, false);
  fundec.callType := ctSysNormal;
  //Implementation
  {Note that implementation is added always after declarartion. It's not the usual
  in common units, where all declarations are first}
  curLocation := locImplement;
  Result := AddFunctionIMP(name, retType, srcPos, fundec, true);
  //Create local variables
  for i:=0 to high(local_vars) do begin
    locvar := TreeElems.AddVarDecAndOpen(srcPos, local_vars[i].name, local_vars[i].typ);
    locvar.storage := stRamFix;
    locvar.adicPar := local_vars[i].adicVar;
    TreeElems.CloseElement;  //Close variable
    local_vars[i].pvar := locvar;  //Keep reference
  end;
  {Create a body, to be uniform with normal function and for have a space where
  compile code and access to posible variables or other elements.}
  TreeElems.AddBodyAndOpen(SrcPos);  //Create body
  //Set function created
  Result.callType     := ctSysNormal;
  Result.codSysNormal := codSys;  //Set routine to generate code SIF.
  TreeElems.CloseElement;  //Close body
  TreeElems.CloseElement;  //Close function implementation
  curLocation := tmpLoc;   //Restore current location
  //Add callers to local variables created. Must be done after creating the body.
  for i:=0 to high(local_vars) do begin
    locvar := local_vars[i].pvar;
    AddCallerToFrom(locvar, Result.BodyNode);
  end;

end;
function TGenCod.CreateInUOMethod(
                      clsType: TEleTypeDec;   //Base type where the method bellow.
                      opr     : string;      //Opertaor associated to the method
                      name    : string;      //Name of the method
                      retType : TEleTypeDec;  //Type returned by the method.
                      pCompile: TCodSysInline;
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
  Result      := AddFunctionUNI(name, retType, srcPosNull, pars, false, true);
  //Here variables can be added
  {Create a body, to be uniform with normal function and for have a space where
  compile code and access to posible variables or other elements.}
  Result.callType := ctSysInline; //INLINE function
  Result.codSysInline := pCompile; //Set routine to generate code
  Result.oper := UpCase(opr); //Set operator as UpperCase to speed searching.
  if opr = '' then Result.operTyp := opkNone
  else Result.operTyp := operTyp; //Must be pre or post
  TreeElems.CloseElement;    //Close function implementation
end;
function TGenCod.CreateInBOMethod(
                      clsType: TEleTypeDec;   //Base type where the method bellow.
                      opr     : string;      //Opertaor associated to the method
                      name    : string;      //Name of the method
                      parType : TEleTypeDec;  //Parameter type
                      retType : TEleTypeDec;  //Type returned by the method.
                      pCompile: TCodSysInline): TEleFun;
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
  Result      := AddFunctionUNI(name, retType, srcPosNull, pars, false,
                      false);  //Don't include variables to don't ask for RAM.
  TreeElems.AddBodyAndOpen(srcPosNull);  //Create body
  //Here variables can be added
  {Create a body, to be uniform with normal function and for have a space where
  compile code and access to posible variables or other elements.}
  Result.callType := ctSysInline; //INLINE function
  Result.codSysInline := pCompile; //Set routine to generate code
  Result.oper := UpCase(opr); //Set operator as UpperCase to speed search.
  if opr = '' then Result.operTyp := opkNone
  else Result.operTyp := opkBinary;
  TreeElems.CloseElement;  //Close body
  TreeElems.CloseElement;  //Close function implementation
end;
function TGenCod.CreateInTerMethod(clsType: TEleTypeDec;
  name: string; parType1, parType2: TEleTypeDec; retType: TEleTypeDec;
  pCompile: TCodSysInline): TEleFun;
{Create a new system ternary INLINE function in the current element of
 the AST.
 Returns the reference to the function created.}
var
  pars: TxpParFuncArray;     //Array of parameters
begin
  setlength(pars, 0);        //Reset parameters
  AddParam(pars, 'b', srcPosNull, clsType, decNone);  //Base object
  AddParam(pars, 'i', srcPosNull, parType1, decNone);  //Parameter
  AddParam(pars, 'n', srcPosNull, parType2, decNone);  //Parameter
  //Add declaration
  Result      := AddFunctionUNI(name, retType, srcPosNull, pars, false,
                      false);  //Don't include variables to don't ask for RAM.
  TreeElems.AddBodyAndOpen(srcPosNull);  //Create body
  //Here variables can be added
  {Create a body, to be uniform with normal function and for have a space where
  compile code and access to posible variables or other elements.}
  Result.callType := ctSysInline; //INLINE function
  Result.codSysInline := pCompile; //Set routine to generate code
  Result.operTyp := opkNone;   //Could be a ternary operator
  TreeElems.CloseElement;  //Close body
  TreeElems.CloseElement;  //Close function implementation
end;
procedure TGenCod.CreateSystemElements;
{Initialize the system elements. Must be executed just one time when compiling.}
var
  uni: TEleUnit;
  pars: TxpParFuncArray;  //Array of parameters
  f, sifByteMulByte, sifDelayMs, sifWord, sifByteDivByte,
    sifByteModByte, sifWordDivWord, sifWordModWord: TEleFun;
begin
  //////// Funciones del sistema ////////////
  //Implement calls to Code Generator
  callDefineArray  := @DefineArray;
  callDefineObject := @DefineObject;
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
  uni := CreateEleUnit('System');  //System unit
  TreeElems.AddElementAndOpen(uni);  //Open Unit
  /////////////// System types ////////////////////
  typBool := CreateEleTypeDec('boolean', srcPosNull, 1, tctAtomic, t_boolean);
  typBool.OnLoadToWR := @byte_LoadToWR;
  typBool.location := locInterface;   //Location for type (Interface/Implementation/...)
  TreeElems.AddElementAndOpen(typBool);  //Open to create "elements" list.
  TreeElems.CloseElement;   //Close Type
  typByte := CreateEleTypeDec('byte', srcPosNull, 1, tctAtomic, t_uinteger);
  typByte.OnLoadToWR := @byte_LoadToWR;
  typByte.location := locInterface;
  TreeElems.AddElementAndOpen(typByte);  //Open to create "elements" list.
  TreeElems.CloseElement;
  typChar := CreateEleTypeDec('char', srcPosNull, 1, tctAtomic, t_string);
  typChar.OnLoadToWR := @byte_LoadToWR;
  typChar.location := locInterface;
  TreeElems.AddElementAndOpen(typChar);
  TreeElems.CloseElement;
  typWord := CreateEleTypeDec('word', srcPosNull, 2, tctAtomic, t_uinteger);
  typWord.OnLoadToWR := @word_LoadToWR;
  typWord.OnRequireWR := @word_RequireWR;
  typWord.location := locInterface;
  TreeElems.AddElementAndOpen(typWord);
  TreeElems.CloseElement;

  typDWord := CreateEleTypeDec('dword', srcPosNull, 4, tctAtomic, t_uinteger);
  //typDWord.OnLoadToWR := @word_LoadToWR;
  //typDWord.OnRequireWR := @word_RequireWR;
  typDWord.location := locInterface;
  TreeElems.AddElementAndOpen(typDWord);
  TreeElems.CloseElement;

  {Create variables for aditional Working register. Note that this variables are
  accesible (and usable) from the code, because the name assigned is a common variable.}
  //Create register H as variable
  H := AddVarDecAndOpen('__H', typByte, srcPosNull);
  TreeElems.CloseElement;  { TODO : ¿No sería mejor evitar abrir el elemento para no tener que cerrarlo? }
  H.adicPar.hasAdic := decNone;
  H.adicPar.hasInit := false;
  H.location := locInterface;  //make visible
  //Create register E as variable
  E := AddVarDecAndOpen('__E', typByte, srcPosNull);
  TreeElems.CloseElement;
  E.adicPar.hasAdic := decNone;
  E.adicPar.hasInit := false;
  E.location := locInterface;  //make visible
  //Create register U as variable
  U := AddVarDecAndOpen('__U', typByte, srcPosNull);
  TreeElems.CloseElement;
  U.adicPar.hasAdic := decNone;
  U.adicPar.hasInit := false;
  U.location := locInterface;  //make visible
  //Create register IX as variable
  IX := AddVarDecAndOpen('__IX', typWord, srcPosNull);
  TreeElems.CloseElement;
  IX.adicPar.hasAdic := decNone;
  IX.adicPar.hasInit := false;
  IX.location := locInterface;  //make visible

  curLocation := locInterface;   {Maybe not needed because element here are created directly.}
  /////////////// Boolean type ////////////////////
  //Methods-Operators
  TreeElems.OpenElement(typBool);
  f:=CreateInBOMethod(typBool, ':=',  '_set', typBool, typNull, @SIF_bool_asig_bool);
  f.getset := gsSetInSimple;
  f:=CreateInUOMethod(typBool  , 'NOT', '_not', typBool, @SIF_not_bool, opkUnaryPre);
  f:=CreateInBOMethod(typBool, 'AND', '_and', typBool, typBool, @SIF_bool_and_bool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typBool, 'OR' , '_or' , typBool, typBool, @SIF_bool_or_bool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typBool, 'XOR', '_xor', typBool, typBool, @SIF_bool_xor_bool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typBool, '=' ,  '_equ', typBool, typBool, @SIF_bool_equal_bool);
  f.fConmutat := true;
  f:=CreateInBOMethod(typBool, '<>',  '_dif', typBool, typBool, @SIF_bool_xor_bool);
  f.fConmutat := true;
  TreeElems.CloseElement;   //Close Type
  /////////////// Byte type ////////////////////
  //Methods-Operators
  TreeElems.OpenElement(typByte);
  //f:=CreateUOMethod(typByte, '@', 'addr', typWord, @SIF_address);
  f:=CreateInBOMethod(typByte, ':=', '_set', typByte, typNull, @SIF_byte_asig_byte);
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typByte, '+=', '_aadd',typByte, typNull, @SIF_byte_aadd_byte);
  f.getset := gsSetOther;
  f:=CreateInBOMethod(typByte, '-=', '_asub',typByte, typNull, @SIF_byte_asub_byte);
  f.getset := gsSetOther;
  f:=CreateInBOMethod(typByte, '+' , '_add', typByte, typByte, @SIF_byte_add_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, '+' , '_add', typWord, typWord, @SIF_byte_add_word);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, '-' , '_sub', typByte, typByte, @SIF_byte_sub_byte);
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  f:=CreateInBOMethod(typByte, '*' , '_mul', typByte, typWord, @SIF_byte_mul_byte);
  f.fConmutat := true;
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  sifByteMulByte := f;
  f:=CreateInBOMethod(typByte, 'DIV' , '_div', typByte, typByte, @SIF_byte_div_byte);
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  sifByteDivByte := f;
  f:=CreateInBOMethod(typByte, 'MOD' , '_mod', typByte, typByte, @SIF_byte_mod_byte);
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  sifByteModByte := f;

  f:=CreateInBOMethod(typByte, 'AND','_and', typByte, typByte, @SIF_byte_and_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, 'OR' ,'_or' , typByte, typByte, @SIF_byte_or_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, 'XOR','_xor', typByte, typByte, @SIF_byte_xor_byte);
  f.fConmutat := true;
  f:=CreateInUOMethod(typByte, 'NOT','_not', typByte, @SIF_not_byte, opkUnaryPre);
  f:=CreateInBOMethod(typByte, '=' , '_equ', typByte, typBool, @SIF_byte_equal_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, '<>', '_dif', typByte, typBool, @SIF_byte_difer_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typByte, '>' , '_gre', typByte, typBool, @SIF_byte_great_byte);
  f:=CreateInBOMethod(typByte, '<' , '_les', typByte, typBool, @SIF_byte_less_byte);
  f:=CreateInBOMethod(typByte, '>=', '_gequ',typByte, typBool, @SIF_byte_gequ_byte);
  f:=CreateInBOMethod(typByte, '<=', '_lequ',typByte, typBool, @SIF_byte_lequ_byte);
  f:=CreateInBOMethod(typByte, '>>', '_shr', typByte, typByte, @SIF_byte_shr_byte);  { TODO : Definir bien la precedencia }
  f:=CreateInBOMethod(typByte, '<<', '_shl', typByte, typByte, @SIF_byte_shl_byte);
  TreeElems.CloseElement;   //Close Type
  /////////////// Char type ////////////////////
  TreeElems.OpenElement(typChar);
  f:=CreateInBOMethod(typChar, ':=', '_set', typChar, typNull, @SIF_char_asig_char);
  f.getset := gsSetInSimple;
  //opr.CreateOperation(typString, @SIF_char_asig_string);
  f:=CreateInBOMethod(typChar, '=' , '_equ', typChar, typBool, @SIF_char_equal_char);
  f.fConmutat := true;
  f:=CreateInBOMethod(typChar, '<>', '_dif', typChar, typBool, @SIF_char_difer_char);
  f.fConmutat := true;
  TreeElems.CloseElement;   //Close Type

  /////////////// Word type ////////////////////
  TreeElems.OpenElement(typWord);
  f:=CreateInBOMethod(typWord, ':=' ,'_set' , typWord, typNull, @SIF_word_asig_word);
  f.getset := gsSetInSimple;
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  f:=CreateInBOMethod(typWord, ':=' ,'_set' , typByte, typNull, @SIF_word_asig_byte);
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typWord, '+=' ,'_aadd', typByte, typNull, @SIF_word_aadd_byte);
  f.getset := gsSetOther;
  f:=CreateInBOMethod(typWord, '+=' ,'_aadd', typWord, typNull, @SIF_word_aadd_word);
  f.getset := gsSetOther;
  f:=CreateInBOMethod(typWord, '-=' ,'_asub', typByte, typNull, @SIF_word_asub_byte);
  f.getset := gsSetOther;
  f:=CreateInBOMethod(typWord, '-=' ,'_asub', typWord, typNull, @SIF_word_asub_word);
  AddCallerToFrom(E, f.bodyNode);  // Require _E
  f.getset := gsSetOther;
  f:=CreateInBOMethod(typWord, '+'  , '_add', typByte, typWord, @SIF_word_add_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, '+'  , '_add', typWord, typWord, @SIF_word_add_word);
  f.fConmutat := true;
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  f:=CreateInBOMethod(typWord, '-'  , '_sub', typByte, typWord, @SIF_word_sub_byte);
  f:=CreateInBOMethod(typWord, '-'  , '_sub', typWord, typWord, @SIF_word_sub_word);
  f:=CreateInBOMethod(typWord, '*' , '_mul', typByte, typWord, @SIF_word_mul_byte);
  f.fConmutat := true;

  f:=CreateInBOMethod(typWord, 'DIV' , '_div', typWord, typWord, @SIF_word_div_word);
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  sifWordDivWord := f;
  f:=CreateInBOMethod(typWord, 'MOD' , '_mod', typWord, typWord, @SIF_word_mod_word);
  AddCallerToFrom(H, f.bodyNode);  //Dependency
  sifWordModWord := f;

  f:=CreateInBOMethod(typWord, 'AND', '_and', typByte, typByte, @SIF_word_and_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, 'AND', '_and', typWord, typWord, @SIF_word_and_word);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, 'OR' , '_or' , typWord, typWord, @SIF_word_or_word);
  f.fConmutat := true;
  f:=CreateInUOMethod(typWord, 'NOT', '_not', typWord, @SIF_not_word, opkUnaryPre);
  f:=CreateInBOMethod(typWord, '>>' , '_shr', typByte, typWord, @SIF_word_shr_byte); { TODO : Definir bien la precedencia }
  f:=CreateInBOMethod(typWord, '<<' , '_shl', typByte, typWord, @SIF_word_shl_byte);

  f:=CreateInBOMethod(typWord, '=' , '_equ' , typWord, typBool, @SIF_word_equal_word);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, '=' , '_equ' , typByte, typBool, @SIF_word_equal_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, '<>', '_dif' , typWord, typBool, @SIF_word_difer_word);
  f.fConmutat := true;
  f:=CreateInBOMethod(typWord, '>=', '_gequ', typWord, typBool, @SIF_word_gequ_word);
  AddCallerToFrom(E, f.bodyNode);  //Dependency
  f:=CreateInBOMethod(typWord, '<' , '_les' , typWord, typBool, @SIF_word_less_word);
  f:=CreateInBOMethod(typWord, '>' , '_gre' , typWord, typBool, @SIF_word_great_word);
  f:=CreateInBOMethod(typWord, '<=', '_lequ', typWord, typBool, @SIF_word_lequ_word);
  //Methods
  f:=CreateInUOMethod(typWord, '', 'low' , typByte, @word_Low);
  f:=CreateInUOMethod(typWord, '', 'high', typByte, @word_High);

  TreeElems.CloseElement;   //Close Type

  /////////////// DWord type ////////////////////
  TreeElems.OpenElement(typDWord);
  f:=CreateInBOMethod(typDWord, ':=' ,'_set' , typDWord, typNull, @SIF_dword_asig_dword);
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typDWord, ':=' ,'_set' , typByte, typNull, @SIF_dword_asig_byte);
  f.getset := gsSetInSimple;
  f:=CreateInBOMethod(typDWord, ':=' ,'_set' , typWord, typNull, @SIF_dword_asig_word);
  f.getset := gsSetInSimple;
  AddCallerToFrom(H, f.bodyNode);  //Dependency
//  f:=CreateInBOMethod(typDWord, '+=' ,'_aadd', typByte, typNull, @SIF_word_aadd_byte);
//  f.getset := gsSetOther;
//  f:=CreateInBOMethod(typDWord, '+=' ,'_aadd', typDWord, typNull, @SIF_word_aadd_word);
//  f.getset := gsSetOther;
//  f:=CreateInBOMethod(typDWord, '-=' ,'_asub', typByte, typNull, @SIF_word_asub_byte);
//  f.getset := gsSetOther;
//  f:=CreateInBOMethod(typDWord, '-=' ,'_asub', typDWord, typNull, @SIF_word_asub_word);
//  AddCallerToFrom(E, f.bodyNode);  // Require _E
//  f.getset := gsSetOther;
//  f:=CreateInBOMethod(typDWord, '+'  , '_add', typByte, typDWord, @SIF_word_add_byte);
//  f.fConmutat := true;
  f:=CreateInBOMethod(typDWord, '+'  , '_add', typDWord, typDWord, @SIF_dword_add_dword);
  f.fConmutat := true;
  f:=CreateInBOMethod(typDWord, '+'  , '_add', typByte, typDWord, @SIF_dword_add_byte);
  f.fConmutat := true;
  f:=CreateInBOMethod(typDWord, '+'  , '_add', typWord, typDWord, @SIF_dword_add_word);
  f.fConmutat := true;
//  AddCallerToFrom(H, f.bodyNode);  //Dependency
//  f:=CreateInBOMethod(typDWord, '-'  , '_sub', typByte, typDWord, @SIF_word_sub_byte);
//  f:=CreateInBOMethod(typDWord, '-'  , '_sub', typDWord, typDWord, @SIF_word_sub_word);
//  f:=CreateInBOMethod(typDWord, '*' , '_mul', typByte, typDWord, @SIF_word_mul_byte);
//  f.fConmutat := true;
//
//  f:=CreateInBOMethod(typDWord, 'DIV' , '_div', typDWord, typDWord, @SIF_word_div_word);
//  AddCallerToFrom(H, f.bodyNode);  //Dependency
//  sifWordDivWord := f;
//  f:=CreateInBOMethod(typDWord, 'MOD' , '_mod', typDWord, typDWord, @SIF_word_mod_word);
//  AddCallerToFrom(H, f.bodyNode);  //Dependency
//  sifWordModWord := f;
//
//  f:=CreateInBOMethod(typDWord, 'AND', '_and', typByte, typByte, @SIF_word_and_byte);
//  f.fConmutat := true;
//  f:=CreateInBOMethod(typDWord, 'AND', '_and', typDWord, typDWord, @SIF_word_and_word);
//  f.fConmutat := true;
//  f:=CreateInBOMethod(typDWord, 'OR' , '_or' , typDWord, typDWord, @SIF_word_or_word);
//  f.fConmutat := true;
//  f:=CreateInUOMethod(typDWord, 'NOT', '_not', typDWord, @SIF_not_word, opkUnaryPre);
//  f:=CreateInBOMethod(typDWord, '>>' , '_shr', typByte, typDWord, @SIF_word_shr_byte); { TODO : Definir bien la precedencia }
//  f:=CreateInBOMethod(typDWord, '<<' , '_shl', typByte, typDWord, @SIF_word_shl_byte);
//
//  f:=CreateInBOMethod(typDWord, '=' , '_equ' , typDWord, typBool, @SIF_word_equal_word);
//  f.fConmutat := true;
//  f:=CreateInBOMethod(typDWord, '=' , '_equ' , typByte, typBool, @SIF_word_equal_byte);
//  f.fConmutat := true;
//  f:=CreateInBOMethod(typDWord, '<>', '_dif' , typDWord, typBool, @SIF_word_difer_word);
//  f.fConmutat := true;
//  f:=CreateInBOMethod(typDWord, '>=', '_gequ', typDWord, typBool, @SIF_word_gequ_word);
//  AddCallerToFrom(E, f.bodyNode);  //Dependency
//  f:=CreateInBOMethod(typDWord, '<' , '_les' , typDWord, typBool, @SIF_word_less_word);
//  f:=CreateInBOMethod(typDWord, '>' , '_gre' , typDWord, typBool, @SIF_word_great_word);
//  f:=CreateInBOMethod(typDWord, '<=', '_lequ', typDWord, typBool, @SIF_word_lequ_word);
//  //Methods
//  f:=CreateInUOMethod(typDWord, '', 'low' , typByte, @word_Low);
//  f:=CreateInUOMethod(typDWord, '', 'high', typByte, @word_High);

  TreeElems.CloseElement;   //Close Type

  ///////////////// System INLINE functions (SIF) ///////////////
  //Create system function "delay_ms". Too complex as SIF. We better implement as SNF.
//  setlength(pars, 0);  //Reset parameters
//  AddParam(pars, 'ms', srcPosNull, typWord, decRegis);  //Add parameter
//  sifDelayMs :=
//  AddSysInlineFunction('delay_ms', typNull, srcPosNull, pars, @SIF_delay_ms);

  //Create system function "inc"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);  //Parameter NULL, allows any type.
  sifFunInc :=
  AddSysInlineFunction('inc', typNull, srcPosNull, pars, @SIF_Inc);

  //Create system function "dec"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);  //Parameter NULL, allows any type.
  AddSysInlineFunction('dec', typNull, srcPosNull, pars, @SIF_Dec);

  //Create system function "ord"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);
  AddSysInlineFunction('ord', typByte, srcPosNull, pars, @SIF_Ord);

  //Create system function "chr"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);
  AddSysInlineFunction('chr', typChar, srcPosNull, pars, @SIF_Chr);

  //Create system function "byte"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);  //Parameter NULL, allows any type.
  AddSysInlineFunction('byte', typByte, srcPosNull, pars, @SIF_Byte);

  //Create system function "word"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);  //Parameter NULL, allows any type.
  sifWord :=
  AddSysInlineFunction('word', typWord, srcPosNull, pars, @SIF_Word);
  AddCallerToFrom(H, sifWord.BodyNode);  //Require H

  //Create system function "word"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typNull, decNone);  //Parameter NULL, allows any type.
  //sifWord :=
  AddSysInlineFunction('dword', typDWord, srcPosNull, pars, @SIF_DWord);
  //AddCallerToFrom(H, sifWord.BodyNode);  //Require H

  ///////////////// System Normal functions (SNF) ///////////////
  //Multiply system function
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'A', srcPosNull, typByte, decNone);  //Add parameter
  AddParam(pars, 'B', srcPosNull, typByte, decNone);  //Add parameter
  snfBytMulByt16 :=
  AddSysNormalFunction('byt_mul_byt_16', typWord, srcPosNull, pars, @SNF_byt_mul_byt_16);
  //Division system function
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'A', srcPosNull, typByte, decRegisA);  //Add parameter
  AddParam(pars, 'B', srcPosNull, typByte, decRegisX);  //Add parameter
  snfBytDivByt8 :=
  AddSysNormalFunction('byt_div_byt_8', typByte, srcPosNull, pars, @SNF_byt_div_byt_8);
  AddCallerToFrom(E, snfBytDivByt8.BodyNode);
  //Division system function
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'A', srcPosNull, typWord, decNone);  //Add parameter
  AddParam(pars, 'B', srcPosNull, typWord, decNone);  //Add parameter
  AddLocVar(pars, 'tmp', srcPosNull, typWord, decNone);  //Add local variable
  snfWrdDivWrd16 :=
  AddSysNormalFunction('wrd_div_wrd_16', typWord, srcPosNull, pars, @SNF_wrd_div_wrd_16);
  AddCallerToFrom(E, snfWrdDivWrd16.BodyNode);
  //Word shift left
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typByte, decRegisX);   //Parameter counter shift
  snfWordShift_l :=
  AddSysNormalFunction('word_shift_l', typWord, srcPosNull, pars, @SNF_word_shift_l);
  //Delay system function
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPosNull, typWord, decRegis);
  snfDelayMs :=
  AddSysNormalFunction('delay_ms', typWord, srcPosNull, pars, @SNF_delay_ms);
  //AddCallerToFrom(snfDelayMs, sifDelayMs.bodyNode);  //Dependency
  AddCallerToFrom(H, snfDelayMs.BodyNode);  //Require H

  //Add dependencies of TByte._mul.
  AddCallerToFrom(snfBytMulByt16, sifByteMulByte.bodyNode);
  AddCallerToFrom(snfWordShift_l, sifByteMulByte.bodyNode);

  AddCallerToFrom(snfBytDivByt8, sifByteDivByte.BodyNode);
  AddCallerToFrom(snfBytDivByt8, sifByteModByte.BodyNode);

  AddCallerToFrom(snfWrdDivWrd16, sifWordDivWord.BodyNode);
  AddCallerToFrom(snfWrdDivWrd16, sifWordModWord.BodyNode);

  //Close Unit
  TreeElems.CloseElement;
end;
end.
//5186
