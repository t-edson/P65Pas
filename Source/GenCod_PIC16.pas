{
Implementación de un compilador sencillo de Pascal para microcontroladores PIC de
rango medio.
Esta implementación no permitirá recursividad, por las limitaciones de recursos de los
dispositivos más pequeños, y por la dificultad adicional en la conmutación de bancos
para los dispositivos más grandes.
El compilador está orientado a uso de registros (solo hay uno) y memoria RAM, pero se
implementa una especie de estructura de pila para la evaluación de expresiones
aritméticas con cierta complejidad y para el paso de parámetros a las funciones.
Solo se manejan datos de tipo bit, boolean, byte y word, y operaciones sencillas.
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
  SynFacilBasic, XpresTypesPIC, XpresElementsPIC, P6502utils, GenCodBas_PIC16,
  CompBase, Globales, CompOperands, MisUtils, XpresBas;
type
    { TGenCod }
    TGenCod = class(TGenCodBas)
    private
//      f_byteXbyte_byte: TxpEleFun;  //índice para función
      f_byte_mul_byte_16: TxpEleFun;
      f_word_shift_l: TxpEleFun;
      procedure arrayHigh(const OpPtr: pointer);
      procedure arrayLength(const OpPtr: pointer);
      procedure arrayLow(const OpPtr: pointer);
      procedure byte_mul_byte_16(fun: TxpEleFun);
      procedure Copy_Z_to_A;
      procedure Copy_C_to_A;
      procedure fun_Addr(fun: TxpEleFunBase; out AddrUndef: boolean);
      procedure fun_Byte(fun: TxpEleFunBase; out AddrUndef: boolean);
      procedure DefineArray(etyp: TxpEleType);
      procedure ROB_arr_asig_arr(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_add_word(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_aadd_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_asub_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_shl_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_shr_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ValidRAMaddr(addr: integer);
      function GetIdxParArray(out WithBrack: boolean; out par: TOperand
        ): boolean;
      procedure GenCodArrayGetItem(const OpPtr: pointer);
      procedure GenCodArraySetItem(const OpPtr: pointer);
      procedure GenCodArrayClear(const OpPtr: pointer);
      procedure ROB_bool_and_bool(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_bool_difer_bool(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_bool_equal_bool(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_mul_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_pointer_add_word(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_pointer_sub_word(Opt: TxpOperation; SetRes: boolean);
//      procedure ROB_string_add_char(Opt: TxpOperation; SetRes: boolean);
//      procedure ROB_string_add_string(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_add_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_add_word(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_sub_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_sub_word(Opt: TxpOperation; SetRes: boolean);
      procedure ROU_not_bool(Opr: TxpOperator; SetRes: boolean);
      procedure ROU_not_byte(Opr: TxpOperator; SetRes: boolean);
      procedure ROU_address(Opr: TxpOperator; SetRes: boolean);

      procedure ROB_word_and_byte(Opt: TxpOperation; SetRes: boolean);
      procedure word_shift_l(fun: TxpEleFun);
    protected //Boolean oeprations
      procedure ROB_bool_asig_bool(Opt: TxpOperation; SetRes: boolean);

    protected //Operaciones con byte
      procedure ROB_byte_asig_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_aadd_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_asub_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_sub_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_add_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_and_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_or_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_xor_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_equal_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_difer_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_great_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_less_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_gequ_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_lequ_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_shr_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_shl_byte(Opt: TxpOperation; SetRes: boolean);
    private  //Operaciones con Word
      procedure ROB_word_asig_word(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_asig_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_equal_word(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_difer_word(Opt: TxpOperation; SetRes: boolean);
    private  //Operaciones con Char
      procedure ROB_char_asig_char(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_char_asig_string(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_char_equal_char(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_char_difer_char(Opt: TxpOperation; SetRes: boolean);
    protected //Operaciones con punteros
      procedure ROB_pointer_add_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_pointer_sub_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROU_derefPointer(Opr: TxpOperator; SetRes: boolean);
    private  //Funciones internas.
      procedure codif_1mseg;
      procedure codif_delay_ms(fun: TxpEleFun);
      procedure expr_end(posExpres: TPosExpres);
      procedure expr_start;
      procedure fun_delay_ms(fun: TxpEleFunBase; out AddrUndef: boolean);
      procedure fun_Exit(fun: TxpEleFunBase; out AddrUndef: boolean);
      procedure fun_Inc (fun: TxpEleFunBase; out AddrUndef: boolean);
      procedure fun_Dec (fun: TxpEleFunBase; out AddrUndef: boolean);
      procedure SetOrig (fun: TxpEleFunBase; out AddrUndef: boolean);
      procedure fun_Ord (fun: TxpEleFunBase; out AddrUndef: boolean);
      procedure fun_Chr (fun: TxpEleFunBase; out AddrUndef: boolean);
      procedure fun_Word(fun: TxpEleFunBase; out AddrUndef: boolean);
    protected
      procedure Cod_StartProgram;
      procedure Cod_EndProgram;
      procedure CreateSystemElements;
    public
      procedure StartSyntax;
      procedure DefCompiler;
      procedure DefinePointer(etyp: TxpEleType);
    end;

  procedure SetLanguage;
implementation
var
  MSG_NOT_IMPLEM, MSG_INVAL_PARTYP, MSG_UNSUPPORTED : string;
  MSG_CANNOT_COMPL, ER_INV_MEMADDR, ER_INV_MAD_DEV: string;

procedure SetLanguage;
begin
  GenCodBas_PIC16.SetLanguage;
  {$I ..\language\tra_GenCod.pas}
end;
procedure TGenCod.Copy_Z_to_A;
begin
  //Result in Z. Move to A.
  BooleanFromZ := _PC;  //Activates flag, and get current address.
  _PHP;
  _PLA;
  _ANDi($02);
end;
procedure TGenCod.Copy_C_to_A;
begin
  //Result in C. Move to A.
  BooleanFromC := _PC;  //Activates flag, and get current address.
  _PHP;
  _PLA;
  _ANDi($01);
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
//  if H<>nil then
//    H.used := false;      //Su ciclo de vida es de instrucción
  RTstate := nil;         //Inicia con los RT libres.
  //Limpia tabla de variables temporales
  varFields.Clear;
  //Guarda información de ubicación, en la ubicación actual
  pic.addPosInformation(cIn.curCon.row, cIn.curCon.col, cIn.curCon.idCtx);
end;
procedure TGenCod.expr_end(posExpres: TPosExpres);
//Se ejecuta al final de una expresión, si es que no ha habido error.
begin
  if exprLevel = 1 then begin  //el último nivel
//    Code('  ;fin expres');
  end;
  //Muestra informa
end;
procedure TGenCod.ROU_not_byte(Opr: TxpOperator; SetRes: boolean);
begin
  case p1^.Sto of
  stConst : begin
    SetROUResultConst_byte((not p1^.valInt) and $FF);
  end;
  stVariab: begin
    SetROUResultExpres_byte;
    _EOR(byte1);
  end;
//  stExpres: begin
//    SetROUResultExpres_byte;
//    //////
//  end;
  else
    genError('Not implemented: "%s"', [Opr.OperationString]);
  end;
end;
procedure TGenCod.ROU_address(Opr: TxpOperator; SetRes: boolean);
{Return the address of any operand.}
var
  startAddr: integer;
begin
  case p1^.Sto of
  stConst : begin
    if p1^.Typ.catType = tctArray then begin
      //We allow to get the address for constant arrays, storing first in RAM.
      CreateValueInCode(p1^.Typ, p1^.Value, startAddr);
      SetResultConst(typWord);
      res.valInt := startAddr;
    end else begin
      genError('Cannot obtain address of constant.');
    end;
  end;
  stVariab: begin
    //Es una variable normal
    //La dirección de una variable es constante
    SetResultConst(typWord);
    res.valInt := p1^.rVar.addr;
  end;
  stExpres: begin
    genError('Cannot obtain address of an expression.');
  end;
  else
    genError('Cannot obtain address of this operand.');
  end;
end;
procedure TGenCod.ROU_not_bool(Opr: TxpOperator; SetRes: boolean);
begin
  case p1^.Sto of
  stConst : begin
    //NOT for a constant is defined easily
    SetROUResultConst_bool(not p1^.valBool);
  end;
  stVariab: begin
    SetROUResultExpres_bool(logNormal);
    //We have to return logical value inverted in A
    _LDA(p1^.rVar.addr);   //Result of "NOT var" in Z
    Copy_Z_to_A;
  end;
//  stExpres: begin
//    SetROUResultExpres_byte;
//    //////
//  end;
  else
    genError('Not implemented: "%s"', [Opr.OperationString]);
  end;
end;
////////////Byte operations
procedure TGenCod.ROB_byte_asig_byte(Opt: TxpOperation; SetRes: boolean);
var
  idxVar: TxpEleVar;
  offaddr: Word;
  addrNextOp1, addrNextOp2: Integer;
begin
  SetResultNull;  //In Pascal an assigment doesn't return type.
  //Implements assigment
  if p1^.Sto = stVariab then begin
    //Assignment to a variable
    case p2^.Sto of
    stConst : begin
      _LDAi(value2);
      _STA(byte1);
    end;
    stVariab: begin
      _LDA(byte2);
      _STA(byte1);
    end;
    stExpres, stExpresA: begin  //Already in A
      _STA(byte1);
    end;
    stExpresX: begin
      _STX(byte1);
    end;
    stExpresY: begin
      _STY(byte1);
    end;
    else
      GenError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
    end;
  end else if p1^.Sto = stExpresA then begin
    //Assignment to register A
    case p2^.Sto of
    stConst : begin
      _LDAi(value2);
    end;
    stVariab: begin
      _LDA(byte2);
    end;
    stExpres, stExpresA: begin  //Already in A
    end;
    stExpresX: begin
      _TXA;
    end;
    stExpresY: begin
      _TYA;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
    end;
  end else if p1^.Sto = stExpresX then begin
    //Assignment to register X
    case p2^.Sto of
    stConst : begin
      _LDXi(value2);
    end;
    stVariab: begin
      _LDX(byte2);
    end;
    stExpres, stExpresA: begin  //Already in A
      _TAX;
    end;
    stExpresX: begin  //Already in X
    end;
    stExpresY: begin
      _TYA;  //Modify A
      _TAX;
    end;
    else
      GenError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
    end;
  end else if p1^.Sto = stExpresY then begin
    //Assignment to register Y
    case p2^.Sto of
    stConst : begin
      _LDYi(value2);
    end;
    stVariab: begin
      _LDY(byte2);
    end;
    stExpres, stExpresA: begin  //Already in A
      _TAY;
    end;
    stExpresX: begin
      _TXA;  //Modify A
      _TAY;
    end;
    stExpresY: begin //Already in X
    end;
    else
      GenError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
    end;
  end else if p1^.Sto in [stVarRef, stExpRef] then begin
    //Asignación a una variable referenciada por variable (o IX)
    case p2^.Sto of
    stConst, stVariab, stExpres: begin
      if p1^.Sto =stVarRef then
        idxVar := p1^.rVar
      else
        idxVar := IX;
      if idxVar.typ.IsByteSize then begin
        //Index is byte-size. It's easy.
        case p2^.Sto of //Operand2 to assign
        stConst:  _LDAi(value2);
        stVariab: _LDA(byte2);
        stExpres: ;  //Already in A
        end;
        _LDX(idxVar.addr);
        pic.codAsm(i_STA, aZeroPagX, 0);
      end else if idxvar.typ.IsWordSize then begin
        //Index is word-size
        //If it's in zero-page will be wonderful
        if (idxvar.addr<256) then begin
          _LDYi(0);
          case p2^.Sto of //Operand2 to assign
          stConst:  _LDAi(value2);
          stVariab: _LDA(byte2);
          stExpres: ;  //Already in A
          end;
          pic.codAsm(i_STA, aIndirecY, idxvar.addr); //Store forward
        end else begin
          //No problem. We create self-modifying code
          //Add to the index, the offset
          if p2^.Sto = stExpres then _PLA;  //Save A
          _CLC;
          _LDA(idxVar.addr);  //Load index
addrNextOp1 := pic.iRam + 1;  //Address next instruction
          pic.codAsm(i_STA, aAbsolute, 0); //Store forward
          _LDA(idxVar.addr+1);  //Load MSB
addrNextOp2 := pic.iRam + 1;  //Address next instruction
          pic.codAsm(i_STA, aAbsolute, 0); //Store forward
          case p2^.Sto of //Operand2 to assign
          stConst:  _LDAi(value2);
          stVariab: _LDA(byte2);
          stExpres: _PLA;  //Restore A
          end;
          //Modifiying instruction
          pic.codAsm(i_STA, aAbsolute, 0); //Store forward
          //Complete address
          pic.ram[addrNextOp1].value := (pic.iRam - 2) and $FF;
          pic.ram[addrNextOp1+1].value := (pic.iRam - 2)>>8 and $FF;
          pic.ram[addrNextOp2].value := (pic.iRam - 1) and $FF;
          pic.ram[addrNextOp2+1].value := (pic.iRam - 1)>>8 and $FF;
        end;
      end else begin
        //Index could be dword or other type
        GenError(MSG_UNSUPPORTED); exit;
      end;
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else if p1^.Sto = stVarConRef then begin
    //Asignación a una variable referenciada por variable + desplaz.
    case p2^.Sto of
    stConst, stVariab, stExpres: begin
      idxVar := p1^.rVar;
      if idxVar.typ.IsByteSize then begin
        //Index is byte-size. It's easy.
        offaddr := p1^.valInt;  //offset addres
        case p2^.Sto of //Operand2 to assign
        stConst:  _LDAi(value2);
        stVariab: _LDA(byte2);
        stExpres: ;  //Already in A
        end;
        _LDX(idxVar.addr);
        if offaddr<256 then begin
          pic.codAsm(i_STA, aZeroPagX, offaddr);
        end else begin
          pic.codAsm(i_STA, aAbsolutX, offaddr);
        end;
      end else if idxvar.typ.IsWordSize then begin
        //Index is word-size
        offaddr := p1^.valInt;  //offset addres
        //If it's in zero-page will be wonderful
        if (idxvar.addr<256) and (offaddr<256) then begin
          _LDYi(offaddr);
          case p2^.Sto of //Operand2 to assign
          stConst:  _LDAi(value2);
          stVariab: _LDA(byte2);
          stExpres: ;  //Already in A
          end;
          pic.codAsm(i_STA, aIndirecY, idxvar.addr); //Store forward
        end else begin
          //No problem. We create self-modifying code
          //Add to the index, the offset
          if p2^.Sto = stExpres then _PLA;  //Save A
          _CLC;
          _LDA(idxVar.addr);  //Load index
          _ADCi(lo(offaddr));
addrNextOp1 := pic.iRam + 1;  //Address next instruction
          pic.codAsm(i_STA, aAbsolute, 0); //Store forward
          _LDA(idxVar.addr+1);  //Load MSB
          _ADCi(hi(offaddr));
addrNextOp2 := pic.iRam + 1;  //Address next instruction
          pic.codAsm(i_STA, aAbsolute, 0); //Store forward
          case p2^.Sto of //Operand2 to assign
          stConst:  _LDAi(value2);
          stVariab: _LDA(byte2);
          stExpres: _PLA;  //Restore A
          end;
          pic.codAsm(i_STA, aAbsolute, 0); //Store forward
          //Complete address
          pic.ram[addrNextOp1].value := (pic.iRam - 2) and $FF;
          pic.ram[addrNextOp1+1].value := (pic.iRam - 2)>>8 and $FF;
          pic.ram[addrNextOp2].value := (pic.iRam - 1) and $FF;
          pic.ram[addrNextOp2+1].value := (pic.iRam - 1)>>8 and $FF;
        end;
      end else begin
        //Index could be dword or other type
        GenError(MSG_UNSUPPORTED); exit;
      end;
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else begin
//    GenError('Cannot assign to this Operand.'); exit;
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_and_byte(Opt: TxpOperation; SetRes: boolean);
begin
  case stoOperation of
  stConst_Const: begin  //suma de dos constantes. Caso especial
    SetROBResultConst_byte(value1 and value2);  //puede generar error
  end;
  stConst_Variab: begin
    if value1 = 0 then begin  //Caso especial
      SetROBResultConst_byte(0);  //puede generar error
      exit;
    end else if value1 = 255 then begin  //Caso especial
      SetROBResultVariab(p2^.rVar);  //puede generar error
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    _LDA(byte2);
    _ANDi(value1);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    if value1 = 0 then begin  //Caso especial
      SetROBResultConst_byte(0);  //puede generar error
      exit;
    end else if value1 = 255 then begin  //Caso especial
      SetROBResultExpres_byte(Opt);  //No es necesario hacer nada. Ya está en A
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    _ANDi(value1);
  end;
  stVariab_Const: begin
    if value2 = 0 then begin  //Caso especial
      SetROBResultConst_byte(0);  //puede generar error
      exit;
    end else if value1 = 255 then begin  //Caso especial
      SetROBResultVariab(p1^.rVar);  //puede generar error
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    _LDAi(value2);
    _AND(byte1);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    _LDA(byte2);
    _AND(byte1);   //leave in A
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _AND(byte1);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    if value2 = 0 then begin  //Caso especial
      SetROBResultConst_byte(0);  //puede generar error
      exit;
    end else if value1 = 255 then begin  //Caso especial
      SetROBResultExpres_byte(Opt);  //No es necesario hacer nada. Ya está en A
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    _ANDi(value2)
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _AND(byte2);
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //p1 está en la pila y p2 en el acumulador
//    rVar := GetVarByteFromStk;
    _TAX;  //Save A
    _PLA;
    _STA(H.addr);  //Use H a temp variable.
    _TXA;  //Restore A
    _AND(H.addr);
  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_or_byte(Opt: TxpOperation; SetRes: boolean);
begin
  case stoOperation of
  stConst_Const: begin  //suma de dos constantes. Caso especial
    SetROBResultConst_byte(value1 or value2);  //puede generar error
  end;
  stConst_Variab: begin
    if value1 = 0 then begin  //Caso especial
      SetROBResultVariab(p2^.rVar);
      exit;
    end else if value1 = 255 then begin  //Caso especial
      SetROBResultConst_byte(255);
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    _LDAi(value1);
    _ORA(byte2);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    if value1 = 0 then begin  //Caso especial
      SetROBResultExpres_byte(Opt);  //No es necesario hacer nada. Ya está en A
      exit;
    end else if value1 = 255 then begin  //Caso especial
      SetROBResultConst_byte(255);
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    _ORA(value1);
  end;
  stVariab_Const: begin
    if value2 = 0 then begin  //Caso especial
      SetROBResultVariab(p1^.rVar);
      exit;
    end else if value1 = 255 then begin  //Caso especial
      SetROBResultConst_byte(255);
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    _LDAi(value2);
    _ORA(byte1);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    _LDA(byte1);
    _ORA(byte2);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _ORA(byte1);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    if value2 = 0 then begin  //Caso especial
      SetROBResultExpres_byte(Opt);  //No es necesario hacer nada. Ya está en A
      exit;
    end else if value2 = 255 then begin  //Caso especial
      SetROBResultConst_byte(255);
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    _ORA(value2);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _ORA(byte2);
  end;
//  stExpres_Expres:begin
//    SetROBResultExpres_byte(Opt);
//    //p1 está en la pila y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _ORA(rVar.adrByte0);
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_xor_byte(Opt: TxpOperation; SetRes: boolean);
begin
  case stoOperation of
  stConst_Const: begin  //suma de dos constantes. Caso especial
    SetROBResultConst_byte(value1 xor value2);  //puede generar error
  end;
  stConst_Variab: begin
    SetROBResultExpres_byte(Opt);
    _LDAi(value1);
    _EOR(byte2)
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _EORi(value1);  //leave in A
  end;
  stVariab_Const: begin
    SetROBResultExpres_byte(Opt);
    _LDA(byte1);   //leave in A
    _EORi(value2);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    _LDA(byte1);   //leave in A
    _EOR(byte2);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _EOR(byte1);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _EORi(value2);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _EOR(byte2);
  end;
//  stExpres_Expres:begin
//    SetROBResultExpres_byte(Opt);
//    //p1 está en la pila y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _EOR(rVar.adrByte0);
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_equal_byte(Opt: TxpOperation; SetRes: boolean);
begin
  case stoOperation of
  stConst_Const: begin  //compara constantes. Caso especial
    SetROBResultConst_bool(value1 = value2);
  end;
  stConst_Variab: begin
    SetROBResultExpres_bool(Opt, logNormal);   //Se pide Z para el resultado
    if value1 = 0 then begin  //caso especial
      _LDA(byte2);
    end else begin
      _LDA(byte2);
      _CMPi(value1);
    end;
    Copy_Z_to_A;
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    if not AcumStatInZ then _TAX;   //Update Z, if needed.
    if value1 = 0 then begin  //caso especial
      //Nothing
    end else begin
      _CMPi(value1);
    end;
    Copy_Z_to_A;
  end;
  stVariab_Const: begin
    SetROBResultExpres_bool(Opt, logNormal);   //Se pide Z para el resultado
    if value2 = 0 then begin  //caso especial
      _LDA(byte1);
    end else begin
      _LDA(byte1);
      _CMPi(value2);
    end;
    Copy_Z_to_A;
  end;
  stVariab_Variab:begin
    SetROBResultExpres_bool(Opt, logNormal);   //Se pide Z para el resultado
    _LDA(byte2);
    _CMP(byte1);
    Copy_Z_to_A;
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    _CMP(byte1);
    Copy_Z_to_A;
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    if not AcumStatInZ then _TAX;   //Update Z, if needed.
    if value2 = 0 then begin  //caso especial
      //Nothing
    end else begin
      _CMPi(value2);
    end;
    Copy_Z_to_A;
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    _CMP(byte2);
    Copy_Z_to_A;
  end;
//  stExpres_Expres:begin
//    SetROBResultExpres_bool(Opt, logNormal);   //Se pide Z para el resultado
//    //la expresión p1 debe estar salvada y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _EOR(rVar.adrByte0);  //Si son iguales Z=1.
//    FreeStkRegisterByte;   //libera pila porque se usará el dato ahí contenido
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_difer_byte(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_byte_equal_byte(Opt, SetRes);  //usa el mismo código
  res.Invert;  //Invierte la lógica
end;
procedure TGenCod.ROB_byte_aadd_byte(Opt: TxpOperation; SetRes: boolean);
{Operación de asignación suma: +=}
begin
  //Special assigment
  if p1^.Sto = stVariab then begin
    SetResultNull;  //Fomalmente,  una aisgnación no devuelve valores en Pascal
    //Asignación a una variable
    case p2^.Sto of
    stConst : begin
      if value2=0 then begin
        //Caso especial. No hace nada
      end else if value2=1 then begin
        //Caso especial.
        _INC(byte1);
      end else if value2=2 then begin
        //Caso especial.
        _INC(byte1);
        _INC(byte1);
      end else begin
        _CLC;
        _LDA(byte1);
        _ADCi(value2);
        _STA(byte1);
      end;
    end;
    stVariab: begin
      _LDA(byte1);
      _CLC;
      _ADC(byte2);
      _STA(byte1);
    end;
    stExpres: begin  //ya está en A
      _CLC;
      _ADC(byte1);
      _STA(byte1);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else if p1^.Sto = stExpRef then begin
//    {Este es un caso especial de asignación a un puntero a byte dereferenciado, pero
//    cuando el valor del puntero es una expresión. Algo así como (ptr + 1)^}
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case p2^.Sto of
//    stConst : begin
//      //Asignación normal
//      if value2=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        kMOVWF(FSR);  //direcciona
//        _ADDWF(0, toF);
//      end;
//    end;
//    stVariab: begin
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(byte2, toW);
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
  end else if p1^.Sto = stVarRef then begin
//    //Asignación a una variable
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case p2^.Sto of
//    stConst : begin
//      //Asignación normal
//      if value2=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        //Caso especial de asignación a puntero dereferenciado: variable^
//        kMOVF(byte1, toW);
//        kMOVWF(FSR);  //direcciona
//        _ADDWF(0, toF);
//      end;
//    end;
//    stVariab: begin
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      kMOVF(byte1, toW);
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(byte2, toW);
//      _ADDWF(0, toF);
//    end;
//    stExpres: begin  //ya está en A
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      aux := GetAuxRegisterByte;
//      kMOVWF(aux);   //Salva A (p2)
//      //Apunta con p1
//      kMOVF(byte1, toW);
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
procedure TGenCod.ROB_byte_asub_byte(Opt: TxpOperation; SetRes: boolean);
begin
  //Caso especial de asignación
  if p1^.Sto = stVariab then begin
    SetResultNull;  //Fomalmente,  una aisgnación no devuelve valores en Pascal
    //Asignación a una variable
    case p2^.Sto of
    stConst : begin
      if value2=0 then begin
        //Caso especial. No hace nada
      end else if value2=1 then begin
        //Caso especial.
        _DEC(byte1);
      end else if value2=2 then begin
        //Caso especial.
        _DEC(byte1);
        _DEC(byte1);
      end else begin
        _SEC;
        _LDA(byte1);
        _SBCi(value2);
        _STA(byte1);
      end;
    end;
    stVariab: begin
      _SEC;
      _LDA(byte1);
      _SBC(byte2);
      _STA(byte1);
    end;
    stExpres: begin  //ya está en A
      _SEC;
      _SBC(byte1);   //a - p1 -> a
      //Invierte
      _EORi($ff);
      _CLC;
      _ADCi(1);
      //Devuelve
      _STA(byte1);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else if p1^.Sto = stExpRef then begin
//    {Este es un caso especial de asignación a un puntero a byte dereferenciado, pero
//    cuando el valor del puntero es una expresión. Algo así como (ptr + 1)^}
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case p2^.Sto of
//    stConst : begin
//      //Asignación normal
//      if value2=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        kMOVWF(FSR);  //direcciona
//        _SUBWF(0, toF);
//      end;
//    end;
//    stVariab: begin
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(byte2, toW);
//      _SUBWF(0, toF);
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
//      _SUBWF(0, toF);
//      aux.used := false;
//      exit;
//    end;
//    else
//      GenError(MSG_UNSUPPORTED); exit;
//    end;
  end else if p1^.Sto = stVarRef then begin
//    //Asignación a una variable
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case p2^.Sto of
//    stConst : begin
//      //Asignación normal
//      if value2=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        //Caso especial de asignación a puntero dereferenciado: variable^
//        kMOVF(byte1, toW);
//        kMOVWF(FSR);  //direcciona
//        _SUBWF(0, toF);
//      end;
//    end;
//    stVariab: begin
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      kMOVF(byte1, toW);
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(byte2, toW);
//      _SUBWF(0, toF);
//    end;
//    stExpres: begin  //ya está en A
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      aux := GetAuxRegisterByte;
//      kMOVWF(aux);   //Salva A (p2)
//      //Apunta con p1
//      kMOVF(byte1, toW);
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
procedure TGenCod.ROB_byte_add_byte(Opt: TxpOperation; SetRes: boolean);
begin
  case stoOperation of
  stConst_Const: begin
    SetROBResultConst_byte(value1+value2);  //puede generar error
  end;
  stConst_Variab: begin
    if value1 = 0 then begin
      //Caso especial
      SetROBResultVariab(p2^.rVar);  //devuelve la misma variable
      exit;
    end else if value1 = 1 then begin
      //Caso especial
      SetROBResultExpres_byte(Opt);
      _LDX(byte2);
      _INX;
      _TXA;
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    _CLC;
    _LDAi(value1);
    _ADC(byte2);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _CLC;
    _ADCi(value1);
  end;
  stVariab_Const: begin
    ExchangeP1_P2;
    ROB_byte_add_byte(Opt, true);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    _CLC;
    _LDA(byte1);
    _ADC(byte2);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _CLC;
    _ADC(byte1);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _CLC;
    _ADCi(value2);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _CLC;
    _ADC(byte2);
  end;
//  stExpres_Expres:begin
//    SetROBResultExpres_byte(Opt);
//    //La expresión p1 debe estar salvada y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _CLC;
//    _ADC(rVar.adrByte0);  //opera directamente al dato que había en la pila. Deja en A
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_add_word(Opt: TxpOperation; SetRes: boolean);
begin
  //A < B es lo mismo que B > A
  case stoOperation of
  stExpres_Expres:begin
//    {Este es el único caso que no se puede invertir, por la posición de los operandos en
//     la pila.}
//    //la expresión p1 debe estar salvada y p2 en el acumulador
//    p1^.SetAsVariab(GetVarByteFromStk);  //Convierte a variable
//    //Luego el caso es similar a stVariab_Expres
//    ROB_byte_less_byte(Opt, SetRes);
//    FreeStkRegisterByte;   //libera pila porque ya se usó el dato ahí contenido
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
  else
    //Para los otros casos, funciona
    ExchangeP1_P2;
    ROB_word_add_byte(Opt, SetRes);
  end;

//  case stoOperation of
//  stConst_Const: begin
//    //Optimize
//    SetROBResultConst_word(value1 + value2);
//  end;
//  stConst_Variab: begin
//    SetROBResultExpres_word(Opt);
//    _CLC;
//    _LDAi(value1L);
//    _ADC(byte2L);
//    _TAX;  //Save
//    _LDAi(0);
//    _ADC(byte2H);
//    _STA(H.addr);
//    _TXA;  //Restore A
//  end;
////  stConst_Expres: begin  //la expresión p2 se evaluó y esta en (A)
////    SetROBResultExpres_byte(Opt);
////    _ANDi(value1L);      //Deja en A
////  end;
//  stVariab_Const: begin
//    SetROBResultExpres_word(Opt);
//    _CLC;
//    _LDA(byte1L);
//    _ADCi(value2L);
//    _TAX;  //Save
//    _LDAi(0);
//    _ADCi(value2H);
//    _STA(H.addr);
//    _TXA;  //Restore A
//  end;
//  stVariab_Variab:begin
//    SetROBResultExpres_word(Opt);
//    _CLC;
//    _LDA(byte1L);
//    _ADC(byte2L);
//    _TAX;  //Save
//    _LDA(0);
//    _ADC(byte2H);
//    _STA(H.addr);
//    _TXA;  //Restore A
//  end;
//  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en (_H,A)
//    SetROBResultExpres_word(Opt);
//    _CLC;
//    //_LDA(byte2L);
//    _ADC(byte1L);
//    _TAX;  //Save
//    _LDA(0);
//    _ADC(H.addr);
//    _STA(H.addr);
//    _TXA;  //Restore A
//  end;
////  stExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
////    SetROBResultExpres_byte(Opt);
////    _ANDi(value2L);
////  end;
////  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,A)
////    SetROBResultExpres_byte(Opt);
////    _AND(byte2L);
////  end;
////  stExpres_Expres:begin
////    SetROBResultExpres_byte(Opt);
////    //p1 está salvado en pila y p2 en (A)
////    p1^.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
////    //Luego el caso es similar a stVariab_Expres
////    _AND(byte1);
////    FreeStkRegisterWord;   //libera pila
////  end;
//  else
//    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
//  end;
end;
procedure TGenCod.ROB_byte_sub_byte(Opt: TxpOperation; SetRes: boolean);
begin
  case stoOperation of
  stConst_Const:begin  //suma de dos constantes. Caso especial
    SetROBResultConst_byte(value1-value2);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  stConst_Variab: begin
    SetROBResultExpres_byte(Opt);
    _SEC;
    _LDAi(value1);
    _SBC(byte2);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    typWord.DefineRegister;   //Asegura que exista H
    _STA(H.addr);
    _SEC;
    _LDAi(value1);
    _SBC(H.addr);
  end;
  stVariab_Const: begin
    SetROBResultExpres_byte(Opt);
    _SEC;
    _LDA(byte1);
    _SBCi(value2);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    _SEC;
    _LDA(byte1);
    _SBC(byte2);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _SEC;
    _SBC(byte1);   //a - p1 -> a
    //Invierte
    _EORi($FF);
    _CLC;
    _ADCi(1);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _SEC;
    _SBCi(value2);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _SEC;
    _SBC(byte2);
  end;
//  stExpres_Expres:begin
//    SetROBResultExpres_byte(Opt);
//    //la expresión p1 debe estar salvada y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _SEC;
//    _SBC(rVar.adrByte0);
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.byte_mul_byte_16(fun: TxpEleFun);
//Routine to multiply 8 bits X 8 bits
//pasA * parB -> [H:A]  Usa registros: A,H,E,U
//Based on https://codebase64.org/doku.php?id=base:short_8bit_multiplication_16bit_product
var
  m0, m1: integer;
  fac1,  fac2: TxpEleVar;
begin
    fac1 := fun.pars[0].pvar;
    fac2 := fun.pars[1].pvar;
    typWord.DefineRegister;   //Ensure H register exists.
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
//    _ldx(fac1.addr);
    //Returns in H,A
    _STA(H.addr);
    _LDA(fac1.addr);
//    _TXA;
    _RTS();
end;
procedure TGenCod.ROB_byte_mul_byte(Opt: TxpOperation; SetRes: boolean);
var
  AddrUndef: boolean;
  fmul: TxpEleFun;
begin
  fmul := f_byte_mul_byte_16;
  case stoOperation of
  stConst_Const: begin
    SetROBResultConst_word(value1*value2);  //puede generar error
  end;
  stConst_Variab: begin
    if value1 = 0 then begin
      //Caso especial
      SetROBResultConst_word(0);  //devuelve la misma variable
      exit;
    end else if value1 = 1 then begin
      //Caso especial
      SetROBResultVariab(p2^.rVar);  //devuelve la misma variable
      exit;
    end else if value1 = 2 then begin
      //Caso especial
      SetROBResultExpres_word(Opt);
      _LDYi(0);
      _STY(H.addr);
      _LDA(byte2);
      _ASLa;
      _ROL(H.addr);
      exit;
    end else if value1 = 4 then begin
      //Caso especial
      SetROBResultExpres_word(Opt);
      _LDYi(0);
      _STY(H.addr);
      _LDA(byte2);
      _ASLa;
      _ROL(H.addr);
      _ASLa;
      _ROL(H.addr);
      exit;
    end else if value1 = 8 then begin
      //Caso especial
      SetROBResultExpres_word(Opt);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      _LDA(byte2);
      //Loop
      _LDXi(3);  //Counter
      AddCallerTo(f_word_shift_l);  //Declare use
      f_word_shift_l.procCall(f_word_shift_l, AddrUndef);  //Use
      exit;
    end else if value1 = 16 then begin
      //Caso especial
      SetROBResultExpres_word(Opt);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      _LDA(byte2);
      //Loop
      _LDXi(4);  //Counter
      AddCallerTo(f_word_shift_l);  //Declare use
      f_word_shift_l.procCall(f_word_shift_l, AddrUndef);  //Use
      exit;
    end else if value1 = 32 then begin
      //Caso especial
      SetROBResultExpres_word(Opt);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      _LDA(byte2);
      //Loop
      _LDXi(5);  //Counter
      AddCallerTo(f_word_shift_l);  //Declare use
      f_word_shift_l.procCall(f_word_shift_l, AddrUndef);  //Use
      exit;
    end;
    //General case
    SetROBResultExpres_word(Opt);
    _LDAi(value1);
    _STA(fmul.pars[0].pvar.addr);
    _LDA(byte2);
    _STA(fmul.pars[1].pvar.addr);
    AddCallerTo(fmul);  //Declare use
    AddCallerTo(fmul.pars[0].pvar);  //Declare use
    AddCallerTo(fmul.pars[1].pvar);  //Declare use
    fmul.procCall(fmul, AddrUndef);   //Code the "JSR"
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    //Es casi el mismo código de stConst_Variab
    if value1 = 0 then begin
      //Caso especial
      SetROBResultConst_word(0);  //devuelve la misma variable
      exit;
    end else if value1 = 1 then begin
      //Caso especial
      SetROBResultExpres_word(Opt);  //devuelve la misma variable
      exit;
    end else if value1 = 2 then begin
      //Caso especial
      SetROBResultExpres_word(Opt);
      _LDYi(0);
      _STY(H.addr);
      //_LDA(byte2);
      _ASLa;
      _ROL(H.addr);
      exit;
    end else if value1 = 4 then begin
      //Caso especial
      SetROBResultExpres_word(Opt);
      _LDYi(0);
      _STY(H.addr);
      //_LDA(byte2);
      _ASLa;
      _ROL(H.addr);
      _ASLa;
      _ROL(H.addr);
      exit;
    end else if value1 = 8 then begin
      //Caso especial
      SetROBResultExpres_word(Opt);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      //_LDA(byte2);
      //Loop
      _LDXi(3);  //Counter
      AddCallerTo(f_word_shift_l);  //Declare use
      f_word_shift_l.procCall(f_word_shift_l, AddrUndef);  //Use
      exit;
    end else if value1 = 16 then begin
      //Caso especial
      SetROBResultExpres_word(Opt);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      //_LDA(byte2);
      //Loop
      _LDXi(4);  //Counter
      AddCallerTo(f_word_shift_l);  //Declare use
      f_word_shift_l.procCall(f_word_shift_l, AddrUndef);  //Use
      exit;
    end else if value1 = 32 then begin
      //Caso especial
      SetROBResultExpres_word(Opt);
      _LDYi(0);
      _STY(H.addr);  //Load high byte
      //_LDA(byte2);
      //Loop
      _LDXi(5);  //Counter
      AddCallerTo(f_word_shift_l);  //Declare use
      f_word_shift_l.procCall(f_word_shift_l, AddrUndef);  //Use
      exit;
    end;
    //General case
    SetROBResultExpres_word(Opt);
    //_LDAi(value1);
    _STA(fmul.pars[0].pvar.addr);
    _LDA(value1);
    _STA(fmul.pars[1].pvar.addr);
    AddCallerTo(fmul);  //Declare use
    AddCallerTo(fmul.pars[0].pvar);  //Declare use
    AddCallerTo(fmul.pars[1].pvar);  //Declare use
    fmul.procCall(fmul, AddrUndef);   //Code the "JSR"
  end;
  stVariab_Const: begin
    ExchangeP1_P2;
    ROB_byte_mul_byte(Opt, true);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_word(Opt);
    _LDA(byte1);
    _STA(fmul.pars[0].pvar.addr);
    _LDA(byte2);
    _STA(fmul.pars[1].pvar.addr);
    AddCallerTo(fmul);  //Declare use
    AddCallerTo(fmul.pars[0].pvar);  //Declare use
    AddCallerTo(fmul.pars[1].pvar);  //Declare use
    fmul.procCall(fmul, AddrUndef);   //Code the "JSR"
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_word(Opt);
    //_LDA(byte1);
    _STA(fmul.pars[0].pvar.addr);
    _LDA(byte1);
    _STA(fmul.pars[1].pvar.addr);
    AddCallerTo(fmul);  //Declare use
    AddCallerTo(fmul.pars[0].pvar);  //Declare use
    AddCallerTo(fmul.pars[1].pvar);  //Declare use
    fmul.procCall(fmul, AddrUndef);   //Code the "JSR"
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    ExchangeP1_P2;
    ROB_byte_mul_byte(Opt, true);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    ExchangeP1_P2;
    ROB_byte_mul_byte(Opt, true);
  end;
//  stExpres_Expres:begin
//    SetROBResultExpres_byte(Opt);
//    //La expresión p1 debe estar salvada y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _CLC;
//    _ADC(rVar.adrByte0);  //opera directamente al dato que había en la pila. Deja en A
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_great_byte(Opt: TxpOperation; SetRes: boolean);
begin
  case stoOperation of
  stConst_Const: begin  //compara constantes. Caso especial
    SetROBResultConst_byte(ord(value1 > value2));
  end;
  stConst_Variab: begin
    if value1 = 0 then begin
      //0 es mayor que nada
      SetROBResultConst_byte(0);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      SetROBResultExpres_bool(Opt, logInverted);
      _LDA(byte2);
      _CMPi(value1); //Result in C (inverted)
      Copy_C_to_A; //Copy C to A (still inverted)
    end;
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    if value1 = 0 then begin
      //0 es mayor que nada
      SetROBResultConst_byte(0);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      //Se necesita asegurar que p1, es mayo que cero.
      SetROBResultExpres_bool(Opt, logInverted);
      //p2, already in A
      _CMPi(value1); //Result in C (inverted)
      Copy_C_to_A; //Copy C to A (still inverted)
    end;
  end;
  stVariab_Const: begin
    if value2 = 255 then begin
      //Nada es mayor que 255
      SetROBResultConst_bool(false);
      GenWarn('Expression will always be FALSE or TRUE.');
    end else begin
      SetROBResultExpres_bool(Opt, logInverted);
      _LDAi(value2);
      _CMP(byte1); //Result in C (inverted)
      Copy_C_to_A; //Copy C to A (still inverted)
    end;
  end;
  stVariab_Variab:begin
    SetROBResultExpres_bool(Opt, logInverted);
    _LDA(byte2);
    _CMP(byte1); //Result in C (inverted)
    Copy_C_to_A; //Copy C to A (still inverted)
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_bool(Opt, logInverted);
    //p2, already in A
    _CMP(byte1); //Result in C (inverted)
    Copy_C_to_A; //Copy C to A (still inverted)
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    if value2 = 255 then begin
      //Nada es mayor que 255
      SetROBResultConst_byte(0);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      SetROBResultExpres_bool(Opt, logNormal);
      //p1, already in A
      _CMPi(value2+1); //p1 >= p2+1. We've verified value2<255
      Copy_C_to_A; //Copy C to A
    end;
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_bool(Opt, logNormal);
    _CLC;   //A trick to get p1>p2 in C, after _SBC
    _SBC(byte2);
    Copy_C_to_A; //Copy C to A
  end;
  //stExpres_Expres:begin
  //  //la expresión p1 debe estar salvada y p2 en el acumulador
  //  p1^.SetAsVariab(GetVarByteFromStk, 0);  //Convierte a variable
  //  //Luego el caso es similar a stVariab_Expres
  //  ROB_byte_great_byte(Opt, true);
  //  FreeStkRegisterByte;   //libera pila porque ya se usó el dato ahí contenido
  //end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_less_byte(Opt: TxpOperation; SetRes: boolean);
begin
  //A < B es lo mismo que B > A
  case stoOperation of
  stExpres_Expres:begin
//    {Este es el único caso que no se puede invertir, por la posición de los operandos en
//     la pila.}
//    //la expresión p1 debe estar salvada y p2 en el acumulador
//    p1^.SetAsVariab(GetVarByteFromStk);  //Convierte a variable
//    //Luego el caso es similar a stVariab_Expres
//    ROB_byte_less_byte(Opt, SetRes);
//    FreeStkRegisterByte;   //libera pila porque ya se usó el dato ahí contenido
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
  else
    //Para los otros casos, funciona
    ExchangeP1_P2;
    ROB_byte_great_byte(Opt, SetRes);
  end;
end;
procedure TGenCod.ROB_byte_gequ_byte(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_byte_less_byte(Opt, SetRes);
  res.Invert;
end;
procedure TGenCod.ROB_byte_lequ_byte(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_byte_great_byte(Opt, SetRes);
  res.Invert;
end;
procedure TGenCod.ROB_byte_shr_byte(Opt: TxpOperation; SetRes: boolean);  //Desplaza a la derecha
var
  L2, L1: integer;
begin
  case stoOperation of
  stConst_Const: begin  //compara constantes. Caso especial
    SetROBResultConst_byte(value1 >> value2);
  end;
  stConst_Variab: begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    _LDAi(value1);
    _LDX(byte2);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _LSRa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    _TAX;
    _BEQ_post(L2);
    _LDAi(value1);
_LABEL_pre(L1);
    _LSRa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stVariab_Const: begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    //Verifica casos simples
    if value2 = 0 then begin
      _LDA(byte1);  //solo devuelve lo mismo en A
    end else if value2 = 1 then begin
      _LDA(byte1);
      _LSRa;
    end else if value2 = 2 then begin
      _LDA(byte1);
      _LSRa;
      _LSRa;
    end else if value2 = 3 then begin
      _LDA(byte1);
      _LSRa;
      _LSRa;
      _LSRa;
    end else if value2 = 4 then begin
      _LDA(byte1);
      _LSRa;
      _LSRa;
      _LSRa;
      _LSRa;
    end else begin
      //Caso general
      _LDA(byte1);
      _LDXi(value2);
_LABEL_pre(L1);
      _LSRa;
      _DEX;
      _BNE_pre(L1);  //loop1
    end;
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    _LDA(byte1);
    _LDX(byte2);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _LSRa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    _TAX;
    _BEQ_post(L2);
    _LDA(byte1);
_LABEL_pre(L1);
    _LSRa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    //Verifica casos simples
    if value2 = 0 then begin
      //solo devuelve lo mismo en A
    end else if value2 = 1 then begin
      _LSRa;
    end else if value2 = 2 then begin
      _LSRa;
      _LSRa;
    end else if value2 = 3 then begin
      _LSRa;
      _LSRa;
      _LSRa;
    end else if value2 = 4 then begin
      _LSRa;
      _LSRa;
      _LSRa;
      _LSRa;
    end else begin
      _LDXi(value2);
_LABEL_pre(L1);
      _LSRa;
      _DEX;
      _BNE_pre(L1);  //loop1
    end;
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    _LDX(byte2);
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
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_shl_byte(Opt: TxpOperation; SetRes: boolean);   //Desplaza a la izquierda
var
  L1, L2: integer;
begin
  case stoOperation of
  stConst_Const: begin  //compara constantes. Caso especial
    SetROBResultConst_byte(value1 << value2);
  end;
  stConst_Variab: begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    _LDAi(value1);
    _LDX(byte2);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _ASLa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    _TAX;
    _BEQ_post(L2);
    _LDAi(value1);
_LABEL_pre(L1);
    _ASLa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stVariab_Const: begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    //Verifica casos simples
    if value2 = 0 then begin
      _LDA(byte1);  //solo devuelve lo mismo en A
    end else if value2 = 1 then begin
      _LDA(byte1);
      _ASLa;
    end else if value2 = 2 then begin
      _LDA(byte1);
      _ASLa;
      _ASLa;
    end else if value2 = 3 then begin
      _LDA(byte1);
      _ASLa;
      _ASLa;
      _ASLa;
    end else if value2 = 4 then begin
      _LDA(byte1);
      _ASLa;
      _ASLa;
      _ASLa;
      _ASLa;
    end else begin
      //Caso general
      _LDA(byte1);
      _LDXi(value2);
_LABEL_pre(L1);
      _ASLa;
      _DEX;
      _BNE_pre(L1);  //loop1
    end;
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    _LDA(byte1);
    _LDX(byte2);
    _BEQ_post(L2);
_LABEL_pre(L1);
    _ASLa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    _TAX;
    _BEQ_post(L2);
    _LDA(byte1);
_LABEL_pre(L1);
    _ASLa;
    _DEX;
    _BNE_pre(L1);  //loop1
_LABEL_post(L2);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    //Verifica casos simples
    if value2 = 0 then begin
      //solo devuelve lo mismo en A
    end else if value2 = 1 then begin
      _ASLa;
    end else if value2 = 2 then begin
      _ASLa;
      _ASLa;
    end else if value2 = 3 then begin
      _ASLa;
      _ASLa;
      _ASLa;
    end else if value2 = 4 then begin
      _ASLa;
      _ASLa;
      _ASLa;
      _ASLa;
    end else begin
      _LDXi(value2);
_LABEL_pre(L1);
      _ASLa;
      _DEX;
      _BNE_pre(L1);  //loop1
    end;
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    _LDX(byte2);
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
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
////////////operaciones con Boolean
procedure TGenCod.ROB_bool_asig_bool(Opt: TxpOperation; SetRes: boolean);
begin
  //Realiza la asignación
  if p1^.Sto = stVariab then begin
    SetResultNull;  //Fomalmente, una asignación no devuelve valores en Pascal
    //Asignación a una variable
    case p2^.Sto of
    stConst : begin
      if p2^.valBool then begin
        _LDAi(1);
        _STA(byte1);
      end else begin
        _LDAi(0);
        _STA(byte1);
      end;
    end;
    stVariab: begin
      _LDA(byte2);
      _STA(byte1);
    end;
    stExpres: begin  //ya está en A
      _STA(byte1);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.ROB_bool_and_bool(Opt: TxpOperation; SetRes: boolean);
var
  sale0: integer;
begin
  case stoOperation of
  stConst_Const: begin  //Special case. Comapares constants.
    SetROBResultConst_bool(p1^.valBool and p2^.valBool);
  end;
  stConst_Variab: begin
    if p1^.valBool = false then begin  //Special case.
      SetROBResultConst_bool(false);
    end else begin  //Special case.
      SetROBResultVariab(p2^.rVar);  //Can be problematic return "var". Formaly it should be an expression.
    end;
  end;
  stConst_Expres: begin  //Expression p2 evaluated in A
    if p1^.valBool = false then begin  //Special case.
      SetROBResultConst_bool(false);
      exit;
    end else if p1^.valBool = true then begin  //Special case.
      SetROBResultExpres_bool(Opt);  //No needed do anything. Result already in A
      exit;
    end;
  end;
  stVariab_Const: begin
    if p2^.valBool = false then begin  //Special case.
      SetROBResultConst_bool(false);
      exit;
    end else if p2^.valBool = true then begin  //Special case.
      SetROBResultVariab(p1^.rVar);  //Can be problematic return "var". Formaly it should be an expression.
      exit;
    end;
  end;
  stVariab_Variab: begin
    SetROBResultExpres_bool(Opt, logInverted);
    _LDX(byte1);
    _BEQ_post(sale0);  //p1=0
    _LDX(byte2);
_LABEL_post(sale0);
    //Here: Z = 0 if p1<>0 and p2<>0
    Copy_Z_to_A;  //Logic inverted
  end;
  stVariab_Expres:begin   //Expresion p2 evaluated in A
    //Algorith like in "stVariab_Variab"
    SetROBResultExpres_bool(Opt, logInverted);
    //Obviously, the last RO was what generates the expression, so we can check "AcumStatInZ".
    if not AcumStatInZ then _TAX;   //Update Z, if needed.
    //Here, we have Z updated with the value of A
    _BEQ_post(sale0);  //p1=0
    _LDX(byte1);
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stExpres_Const: begin   //Expresion p1 evaluated in A
    if p2^.valBool = false then begin  //Special case.
      SetROBResultConst_bool(false);
      exit;
    end else if p2^.valBool = true then begin  //Special case.
      SetROBResultExpres_bool(Opt);  //No needed do anything. Result already in A
      exit;
    end;
  end;
  stExpres_Variab:begin  //Expresion p1 evaluated in A
    //Algorith like in "stVariab_Variab"
    SetROBResultExpres_bool(Opt, logInverted);
    //Obviously, the last RO was what generates the expression, so we can check "AcumStatInZ".
    if not AcumStatInZ then _TAX;   //Update Z, if needed.
    //Here, we have Z updated with the value of A
    _BEQ_post(sale0);  //p1=0
    _LDX(byte2);
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
//  stExpres_Expres:begin
//    SetROBResultExpres_byte(Opt);
//    //p1 está en la pila y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _AND(rVar.adrByte0);
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_bool_equal_bool(Opt: TxpOperation; SetRes: boolean);
begin
  case stoOperation of
  stConst_Const: begin  //Special case. Comapares constants.
    SetROBResultConst_bool(p1^.valBool = p2^.valBool);
  end;
  stConst_Variab: begin
    if p1^.valBool = false then begin  //Special case.
      SetROBResultExpres_bool(Opt);
      _LDA(byte2);  //if equals Z=1
      Copy_Z_to_A;
    end else begin  //Special case.
      SetROBResultExpres_bool(Opt, logInverted);
      _LDA(byte2);  //if equals Z=0
      Copy_Z_to_A;
    end;
  end;
  stConst_Expres: begin  //Expression p2 evaluated in A
    if p1^.valBool = false then begin  //Special case.
      SetROBResultExpres_bool(Opt);
      if not AcumStatInZ then _TAX;   //Update Z, if needed.
      //if equals Z=1
      Copy_Z_to_A;
    end else begin  //Special case.
      SetROBResultExpres_bool(Opt, logInverted);
      if not AcumStatInZ then _TAX;   //Update Z, if needed.
      //if equals Z=0
      Copy_Z_to_A;
    end;
  end;
  stVariab_Const: begin
    if p2^.valBool = false then begin  //Special case.
      SetROBResultExpres_bool(Opt);
      _LDA(byte1);  //if equals Z=1
      Copy_Z_to_A;
    end else begin  //Special case.
      SetROBResultExpres_bool(Opt, logInverted);
      _LDA(byte1);  //if equals Z=0
      Copy_Z_to_A;
    end;
  end;
  stVariab_Variab: begin
    if IsTheSameVar(p1^.rVar, p2^.rVar) then begin
      SetROBResultConst_bool(true);
      exit;
    end;
    SetROBResultExpres_bool(Opt);
    typWord.DefineRegister;  //To use _H
    _LDA(byte1);
    _PHP;     //Save SR
    _PLA;
    _STA(H.addr);  //In H
    _LDA(byte2);
    _PHP;     //Save SR
    _PLA;     //In A
    _EOR(H.addr);  //0 if equals
    _ANDi($02);//Mask position of bit Z. if equals -> Z = 1
    Copy_Z_to_A;  //Logic inverted
    {//Alternative version (not tested)
    SetROBResultExpres_bool(Opt);
    _LDA(byte1);  //if FALSE, Z=1
    _BEQ(false0); //Jump if FALSE
    //It's TRUE
    _LDA(byte2);  //if FALSE (different), Z=1
    //Invert Z
    _PHP;
    _PLA;
    _ANDi($02);  //if Z was 1, set Z to 0, if Z was 0, set Z to 1
    _JMP_post(exit0);  //exit with result in Z
_LABEL_post(false0):
    _LDA(byte2);  //if equal, Z=1
_LABEL_post(exit0):
    Copy_Z_to_A;
  end; }
  end;
  stVariab_Expres:begin   //Expresion p2 evaluated in A
    SetROBResultExpres_bool(Opt);
    typWord.DefineRegister;  //To use _H
    if not AcumStatInZ then _TAX;   //Update Z, if needed.
    _PHP;     //Save SR
    _PLA;
    _STA(H.addr);  //In H
    _LDA(byte1);
    _PHP;     //Save SR
    _PLA;     //In A
    _EOR(H.addr);  //0 if equals
    _ANDi($02);//Mask position of bit Z. if equals -> Z = 1
    Copy_Z_to_A;  //Logic inverted
  end;
  stExpres_Const: begin   //Expresion p1 evaluated in A
    if p2^.valBool = false then begin  //Special case.
      SetROBResultExpres_bool(Opt);
      if not AcumStatInZ then _TAX;   //Update Z, if needed.
      //if equals Z=1
      Copy_Z_to_A;
    end else begin  //Special case.
      SetROBResultExpres_bool(Opt, logInverted);
      if not AcumStatInZ then _TAX;   //Update Z, if needed.
      //if equals Z=0
      Copy_Z_to_A;
    end;
  end;
  stExpres_Variab:begin  //Expresion p1 evaluated in A
    SetROBResultExpres_bool(Opt);
    typWord.DefineRegister;  //To use _H
    if not AcumStatInZ then _TAX;   //Update Z, if needed.
    _PHP;     //Save SR
    _PLA;
    _STA(H.addr);  //In H
    _LDA(byte2);
    _PHP;     //Save SR
    _PLA;     //In A
    _EOR(H.addr);  //0 if equals
    _ANDi($02);//Mask position of bit Z. if equals -> Z = 1
    Copy_Z_to_A;  //Logic inverted
  end;
//  stExpres_Expres:begin
//    SetROBResultExpres_byte(Opt);
//    //p1 está en la pila y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _AND(rVar.adrByte0);
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_bool_difer_bool(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_bool_equal_bool(Opt, SetRes);
  res.Invert;
end;
////////////operaciones con Word
procedure TGenCod.ROB_word_asig_word(Opt: TxpOperation; SetRes: boolean);
begin
  //Realiza la asignación
  if p1^.Sto = stVariab then begin
    case p2^.Sto of
    stConst : begin
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
      if value2L = value2H then begin
        //Caso particular
        _LDAi(value2L);
        _STA(byte1L);
        _STA(byte1H);
      end else begin
        //Caso general
        _LDAi(value2L);
        _STA(byte1L);
        _LDAi(value2H);
        _STA(byte1H);
      end;
    end;
    stVariab: begin
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
      _LDA(byte2L);
      _STA(byte1L);
      _LDA(byte2H);
      _STA(byte1H);
    end;
    stExpres: begin   //se asume que se tiene en (H,A)
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
      _STA(byte1L);
      _LDA(H.addr);
      _STA(byte1H);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else if p1^.Sto = stVarRef then begin
//    //Asignación a una variable
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case p2^.Sto of
//    stConst : begin
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      kMOVF(byte1, toW);
//      kMOVWF(FSR);  //direcciona byte bajo
//      //Asignación normal
//      if value2L=0 then begin
//        //caso especial
//        _CLRF(0);
//      end else begin
//        _MOVWF(0);
//      end;
//      _INCF(FSR.offs, toF);  //direcciona byte alto
//      if value2H=0 then begin
//        //caso especial
//        _CLRF(0);
//      end else begin
//        _MOVWF(0);
//      end;
//    end;
//    stVariab: begin
//      //Caso especial de asignación a puntero dereferenciado: variable^
//      kMOVF(byte1, toW);
//      kMOVWF(FSR);  //direcciona byte bajo
//      //Asignación normal
//      kMOVF(byte2L, toW);
//      _MOVWF(0);
//      _INCF(FSR.offs, toF);  //direcciona byte alto
//      kMOVF(byte2H, toW);
//      _MOVWF(0);
//    end;
//    stExpres: begin  //ya está en H,A
//      //Caso especial de asignación a puntero dereferenciado: variable^
//      aux := GetAuxRegisterByte;
//      _MOVWF(aux.offs);   //Salva A (p2.L)
//      //Apunta con p1
//      kMOVF(byte1, toW);
//      _MOVWF(FSR.offs);  //direcciona a byte bajo
//      //Asignación normal
//      _MOVF(aux.offs, toW);   //recupero p2.L
//      _MOVWF(0);          //escribe
//      _MOVF(H.offs, toW);   //recupero p2.H
//      _INCF(FSR.offs, toF);   //apunta a byte alto
//      _MOVWF(0);          //escribe
//      aux.used := false;
//    end;
//    else
//      GenError(MSG_UNSUPPORTED); exit;
//    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.ROB_word_asig_byte(Opt: TxpOperation; SetRes: boolean);
var
  idxVar: TxpEleVar;
begin
  if p1^.Sto = stVariab then begin
    case p2^.Sto of
    stConst : begin
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
      if value2L = 0 then begin
        _LDAi(0);  //Load once
        _STA(byte1L);
        _STA(byte1H);
      end else begin
        _LDAi(value2L);
        _STA(byte1L);
        _LDAi(0);
        _STA(byte1H);
      end;
    end;
    stVariab: begin
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
      _LDA(byte2L);
      _STA(byte1L);
      _LDAi(0);
      _STA(byte1H);
    end;
    stExpres: begin   //se asume que está en A
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
      _STA(byte1L);
      _LDAi(0);
      _STA(byte1H);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else if p1^.Sto in [stVarRef, stExpRef] then begin
    //Asignación a una variable referenciada por variable (o IX)
    case p2^.Sto of
    stConst, stVariab, stExpres: begin
      if p1^.Sto =stVarRef then
        idxVar := p1^.rVar
      else
        idxVar := IX;
      if idxVar.typ.IsByteSize then begin
        //Index is byte-size. It's easy.
        case p2^.Sto of //Operand2 to assign
        stConst:  _LDAi(value2);
        stVariab: _LDA(byte2);
        stExpres: ;  //Already in A
        end;
        _LDX(idxVar.addr);
        pic.codAsm(i_STA, aZeroPagX, 0);
        pic.codAsm(i_STA, aZeroPagX, 1);
      end else if idxvar.typ.IsWordSize then begin
        //Index is word-size
        //If it's in zero-page will be wonderful
        if (idxvar.addr<256) then begin
          _LDYi(0);
          case p2^.Sto of //Operand2 to assign
          stConst : _LDAi(value2);
          stVariab: _LDA(byte2);
          stExpres: ;  //Already in A
          end;
          pic.codAsm(i_STA, aIndirecY, idxvar.addr); //Store forward
          _LDAi(0);
          _INY;
          pic.codAsm(i_STA, aIndirecY, idxvar.addr); //Store forward
//        end else begin
//          {We can create self-modifying code o some weird code to implement this, but
//          it's complex code. It's better using IX or the Var-index in Zero page}
        end else begin
          //Index could be dword or other type
          GenError(MSG_UNSUPPORTED); exit;
        end;
      end else begin
        //Index could be dword or other type
        GenError(MSG_UNSUPPORTED); exit;
      end;
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.ROB_word_equal_word(Opt: TxpOperation; SetRes: boolean);
var
  sale0: integer;
begin
  case stoOperation of
  stConst_Const: begin  //compara constantes. Caso especial
    SetROBResultConst_byte(ORD(value1 = value2));
  end;
  stConst_Variab: begin
    SetROBResultExpres_bool(Opt);
    _LDAi(value1L);
    _CMP(byte2L);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDAi(value1H);
    _CMP(byte2H);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
//  stConst_Expres: begin  //la expresión p2 se evaluó p2 esta en A
//    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
//    tmp := GetAuxRegisterByte;
//    if HayError then exit;
//    _MOVWF(tmp.offs);   //salva byte bajo de Expresión
//    //Compara byte alto
//    _SUBWF(H.offs, toW); //p2-p1
//    _BTFSS(Z.offs, Z.bit);
//    _JMP_post(sale);  //no son iguales
//    //Son iguales, comparar el byte bajo
//    _SUBWF(tmp.offs,toW);	//p2-p1
//_LABEL_post(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
//    tmp.used := false;
//  end;
  stVariab_Const: begin
    SetROBResultExpres_bool(Opt);
    _LDA(byte1L);
    _CMPi(value2L);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDA(byte1H);
    _CMPi(value2H);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
  stVariab_Variab:begin
    SetROBResultExpres_bool(Opt);
    _LDA(byte1L);
    _CMP(byte2L);
    _BNE_post(sale0);  //different, exit with Z=0.
    _LDA(byte1H);
    _CMP(byte2H);  //different, ends with Z=0.
_LABEL_post(sale0);
    Copy_Z_to_A;  //Logic inverted
  end;
//  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
//    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
//    tmp := GetAuxRegisterByte;
//    _MOVWF(tmp.offs);   //salva byte bajo de Expresión
//    //Compara byte alto
//    kMOVF(byte1H, toW);
//    _SUBWF(H.offs, toW); //p2-p1
//    _BTFSS(Z.offs, Z.bit);
//    _JMP_post(sale);  //no son iguales
//    //Son iguales, comparar el byte bajo
//    kMOVF(byte1L, toW);
//    _SUBWF(tmp.offs,toW);	//p2-p1
//    tmp.used := false;
//_LABEL_post(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
//  end;
//  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
//    ExchangeP1_P2;  //Convierte a stConst_Expres;
//    ROB_word_equal_word(Opt, SetRes);
//  end;
//  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
//    ExchangeP1_P2;  //Convierte a stVariab_Expres;
//    ROB_word_equal_word(Opt, SetRes);
//  end;
//  stExpres_Expres:begin
//    //La expresión p1, debe estar salvada y p2 en (H,A)
//    p1^.SetAsVariab(GetVarWordFromStk);
//    //Luego el caso es similar a variable-expresión
//    ROB_word_equal_word(Opt, SetRes);
//    FreeStkRegisterWord;
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_word_difer_word(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_word_equal_word(Opt, SetRes);
  res.Invert;
end;
procedure TGenCod.ROB_word_add_byte(Opt: TxpOperation; SetRes: boolean);
begin
  case stoOperation of
  stConst_Const: begin
    //Optimize
    SetROBResultConst_word(value1 + value2);
  end;
  stConst_Variab: begin
    SetROBResultExpres_word(Opt);
    _CLC;
    _LDAi(value1L);
    _ADC(byte2L);
    _TAX;  //Save
    _LDAi(value1H);
    _ADCi(0);
    _STA(H.addr);
    _TXA; //We could activate a Flag to indicate we have obteined MSB from X to optimize
    //Form 2: (Very similar)
//    _LDA(byte2H);  //byte2->H
//    _STA(H.addr);
//    _CLC;
//    _LDAi(value1L);
//    _ADC(byte2L);
//    _BCC_post(L2);
//    _INC(H.addr);
//_LABEL_post(L2);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en (A)
    SetROBResultExpres_word(Opt);
    _CLC;
    _ADCi(value1L);
    _TAX;  //Save
    _LDAi(value1H);
    _ADCi(0);
    _STA(H.addr);
    _TXA; //We could activate a Flag to indicate we have obteined MSB from X to optimize
  end;
  stVariab_Const: begin
    SetROBResultExpres_word(Opt);
    _CLC;
    _LDA(byte1L);
    _ADCi(value2L);
    _TAX;  //Save
    _LDA(byte1H);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
  end;
  stVariab_Variab:begin
    SetROBResultExpres_word(Opt);
    _CLC;
    _LDA(byte1L);
    _ADC(byte2L);
    _TAX;  //Save
    _LDA(byte1H);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en (_H,A)
    SetROBResultExpres_word(Opt);
    _CLC;
    _ADC(byte1L);
    _TAX;  //Save
    _LDA(byte1H);
    _ADCi(0);
    _STA(H.addr);
    _TXA; //We could activate a Flag to indicate we have obteined MSB from X to optimize
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetROBResultExpres_word(Opt);
    _CLC;
    _ADCi(value2L);
    _TAX;  //Save
    _LDA(H.addr);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,A)
    SetROBResultExpres_word(Opt);
    _CLC;
    _ADC(byte2L);
    _TAX;  //Save
    _LDA(H.addr);
    _ADCi(0);
    _STA(H.addr);
    _TXA;
  end;
//  stExpres_Expres:begin
//    SetROBResultExpres_byte(Opt);
//    //p1 está salvado en pila y p2 en (A)
//    p1^.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
//    //Luego el caso es similar a stVariab_Expres
//    _AND(byte1);
//    FreeStkRegisterWord;   //libera pila
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_word_add_word(Opt: TxpOperation; SetRes: boolean);
begin
  case stoOperation of
  stConst_Const: begin
    //Optimize
    SetROBResultConst_word(value1 + value2);
  end;
  stConst_Variab: begin
    SetROBResultExpres_word(Opt);
    _CLC;
    _LDAi(value1L);
    _ADC(byte2L);
    _TAX;  //Save
    _LDAi(value1H);
    _ADC(byte2H);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en (A)
    SetROBResultExpres_word(Opt);
    _CLC;
    _ADCi(value1L);
    _TAX;  //Save
    _LDAi(value1H);
    _ADC(H.addr);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stVariab_Const: begin
    SetROBResultExpres_word(Opt);
    _CLC;
    _LDA(byte1L);
    _ADCi(value2L);
    _TAX;  //Save
    _LDA(byte1H);
    _ADCi(value2H);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stVariab_Variab:begin
    SetROBResultExpres_word(Opt);
    _CLC;
    _LDA(byte1L);
    _ADC(byte2L);
    _TAX;  //Save
    _LDA(byte1H);
    _ADC(byte2H);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en (_H,A)
    SetROBResultExpres_word(Opt);
    _CLC;
    _ADC(byte1L);
    _TAX;  //Save
    _LDA(byte1H);
    _ADC(H.addr);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetROBResultExpres_word(Opt);
    _CLC;
    _ADCi(value2L);
    _TAX;  //Save
    _LDA(H.addr);
    _ADCi(value2H);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,A)
    SetROBResultExpres_word(Opt);
    _CLC;
    _ADC(byte2L);
    _TAX;  //Save
    _LDA(H.addr);
    _ADC(byte2H);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
//  stExpres_Expres:begin
//    SetROBResultExpres_byte(Opt);
//    //p1 está salvado en pila y p2 en (A)
//    p1^.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
//    //Luego el caso es similar a stVariab_Expres
//    _AND(byte1);
//    FreeStkRegisterWord;   //libera pila
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_word_sub_byte(Opt: TxpOperation; SetRes: boolean);
begin
  case stoOperation of
  stConst_Const:begin  //suma de dos constantes. Caso especial
    SetROBResultConst_word(value1-value2);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  stConst_Variab: begin
    SetROBResultExpres_word(Opt);
    _SEC;
    _LDAi(value1L);
    _SBC(byte2L);
    _TAX;  //Save
    _LDAi(value1H);
    _SBCi(0);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
//  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
//    SetROBResultExpres_byte(Opt);
//    typWord.DefineRegister;   //Asegura que exista H
//    _STA(H);
//    _SEC;
//    _LDA(value1);
//    _SBC(H);
//  end;
  stVariab_Const: begin
    SetROBResultExpres_word(Opt);
    _SEC;
    _LDA(byte1L);
    _SBCi(value2L);
    _TAX;  //Save
    _LDA(byte1H);
    _SBCi(0);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stVariab_Variab:begin
    SetROBResultExpres_word(Opt);
    _SEC;
    _LDA(byte1L);
    _SBC(byte2L);
    _TAX;  //Save
    _LDA(byte1H);
    _SBCi(0);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
//  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
//    SetROBResultExpres_byte(Opt);
//    _SEC;
//    _SBC(byte1);   //a - p1 -> a
//    //Invierte
//    _EORi($FF);
//    _CLC;
//    _ADCi(1);
//  end;
//  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
//    SetROBResultExpres_byte(Opt);
//    _SEC;
//    _SBCi(value2);
//  end;
//  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
//    SetROBResultExpres_byte(Opt);
//    _SEC;
//    _SBC(byte2);
//  end;
//  stExpres_Expres:begin
//    SetROBResultExpres_byte(Opt);
//    //la expresión p1 debe estar salvada y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _SEC;
//    _SBC(rVar.adrByte0);
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_word_sub_word(Opt: TxpOperation; SetRes: boolean);
begin
  case stoOperation of
  stConst_Const:begin  //suma de dos constantes. Caso especial
    SetROBResultConst_word(value1-value2);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  stConst_Variab: begin
    SetROBResultExpres_word(Opt);
    _SEC;
    _LDAi(value1L);
    _SBC(byte2L);
    _TAX;  //Save
    _LDAi(value1H);
    _SBC(byte2H);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
//  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
//    SetROBResultExpres_byte(Opt);
//    typWord.DefineRegister;   //Asegura que exista H
//    _STA(H);
//    _SEC;
//    _LDA(value1);
//    _SBC(H);
//  end;
  stVariab_Const: begin
    SetROBResultExpres_word(Opt);
    _SEC;
    _LDA(byte1L);
    _SBCi(value2L);
    _TAX;  //Save
    _LDA(byte1H);
    _SBCi(value2H);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
  stVariab_Variab:begin
    SetROBResultExpres_word(Opt);
    _SEC;
    _LDA(byte1L);
    _SBC(byte2L);
    _TAX;  //Save
    _LDA(byte1H);
    _SBC(byte2H);
    _STA(H.addr);
    _TXA;  //Restore A
  end;
//  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
//    SetROBResultExpres_byte(Opt);
//    _SEC;
//    _SBC(byte1);   //a - p1 -> a
//    //Invierte
//    _EORi($FF);
//    _CLC;
//    _ADCi(1);
//  end;
//  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
//    SetROBResultExpres_byte(Opt);
//    _SEC;
//    _SBCi(value2);
//  end;
//  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
//    SetROBResultExpres_byte(Opt);
//    _SEC;
//    _SBC(byte2);
//  end;
//  stExpres_Expres:begin
//    SetROBResultExpres_byte(Opt);
//    //la expresión p1 debe estar salvada y p2 en el acumulador
//    rVar := GetVarByteFromStk;
//    _SEC;
//    _SBC(rVar.adrByte0);
//    FreeStkRegisterByte;   //libera pila porque ya se uso
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_word_aadd_byte(Opt: TxpOperation; SetRes: boolean);
var
  L1, L2: integer;
begin
  //Special assigment
  if p1^.Sto = stVariab then begin
    SetResultNull;  //Fomalmente,  una aisgnación no devuelve valores en Pascal
    //Asignación a una variable
    case p2^.Sto of
    stConst : begin
      if value2=0 then begin
        //Caso especial. No hace nada
      end else if value2=1 then begin
        //Caso especial.
        _INC(byte1L);
        _BNE_post(L1);
        _INC(byte1H);
_LABEL_post(L1);
      end else begin
        _CLC;
        _LDA(byte1L);
        _ADCi(value2);
        _STA(byte1L);
        _BCC_post(L2);
        _INC(byte1H);
_LABEL_post(L2);
      end;
    end;
    stVariab: begin
      _CLC;
      _LDA(byte1L);
      _ADC(byte2);
      _STA(byte1L);
      _BCC_post(L2);
      _INC(byte1H);
_LABEL_post(L2);
    end;
    stExpres: begin  //ya está en A
      _CLC;
      _ADC(byte1L);
      _STA(byte1L);
      _BCC_post(L2);
      _INC(byte1H);
_LABEL_post(L2);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
//  end else if p1^.Sto = stExpRef then begin
//    {Este es un caso especial de asignación a un puntero a byte dereferenciado, pero
//    cuando el valor del puntero es una expresión. Algo así como (ptr + 1)^}
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case p2^.Sto of
//    stConst : begin
//      //Asignación normal
//      if value2=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        kMOVWF(FSR);  //direcciona
//        _ADDWF(0, toF);
//      end;
//    end;
//    stVariab: begin
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(byte2, toW);
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
//  end else if p1^.Sto = stVarRef then begin
//    //Asignación a una variable
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case p2^.Sto of
//    stConst : begin
//      //Asignación normal
//      if value2=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        //Caso especial de asignación a puntero dereferenciado: variable^
//        kMOVF(byte1, toW);
//        kMOVWF(FSR);  //direcciona
//        _ADDWF(0, toF);
//      end;
//    end;
//    stVariab: begin
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      kMOVF(byte1, toW);
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(byte2, toW);
//      _ADDWF(0, toF);
//    end;
//    stExpres: begin  //ya está en A
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      aux := GetAuxRegisterByte;
//      kMOVWF(aux);   //Salva A (p2)
//      //Apunta con p1
//      kMOVF(byte1, toW);
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
procedure TGenCod.ROB_word_asub_byte(Opt: TxpOperation; SetRes: boolean);
var
  L1: integer;
begin
  //Caso especial de asignación
  if p1^.Sto = stVariab then begin
    SetResultNull;  //Fomalmente,  una aisgnación no devuelve valores en Pascal
    //Asignación a una variable
    case p2^.Sto of
    stConst : begin
      if value2=0 then begin
        //Caso especial. No hace nada
      end else if value2=1 then begin
        //Caso especial.
        _LDA(byte1L);
        _BNE_post(L1);
        _DEC(byte1H);
_LABEL_post(L1);
        _DEC(byte1L);
      end else begin
        _SEC;
        _LDA(byte1L);
        _SBCi(value2L);
        _STA(byte1L);
        _LDA(byte1H);
        _SBCi(0);
        _STA(byte1H);
      end;
    end;
    stVariab: begin
      _SEC;
      _LDA(byte1);
      _SBC(byte2);
      _STA(byte1);
    end;
    stExpres: begin  //ya está en A
      _SEC;
      _SBC(byte1);   //a - p1 -> a
      //Invierte
      _EORi($ff);
      _CLC;
      _ADCi(1);
      //Devuelve
      _STA(byte1);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
//  end else if p1^.Sto = stExpRef then begin
//    {Este es un caso especial de asignación a un puntero a byte dereferenciado, pero
//    cuando el valor del puntero es una expresión. Algo así como (ptr + 1)^}
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case p2^.Sto of
//    stConst : begin
//      //Asignación normal
//      if value2=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        kMOVWF(FSR);  //direcciona
//        _SUBWF(0, toF);
//      end;
//    end;
//    stVariab: begin
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(byte2, toW);
//      _SUBWF(0, toF);
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
//      _SUBWF(0, toF);
//      aux.used := false;
//      exit;
//    end;
//    else
//      GenError(MSG_UNSUPPORTED); exit;
//    end;
//  end else if p1^.Sto = stVarRef then begin
//    //Asignación a una variable
//    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
//    case p2^.Sto of
//    stConst : begin
//      //Asignación normal
//      if value2=0 then begin
//        //Caso especial. No hace nada
//      end else begin
//        //Caso especial de asignación a puntero dereferenciado: variable^
//        kMOVF(byte1, toW);
//        kMOVWF(FSR);  //direcciona
//        _SUBWF(0, toF);
//      end;
//    end;
//    stVariab: begin
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      kMOVF(byte1, toW);
//      kMOVWF(FSR);  //direcciona
//      //Asignación normal
//      kMOVF(byte2, toW);
//      _SUBWF(0, toF);
//    end;
//    stExpres: begin  //ya está en A
//      //Caso especial de asignación a puntero derefrrenciado: variable^
//      aux := GetAuxRegisterByte;
//      kMOVWF(aux);   //Salva A (p2)
//      //Apunta con p1
//      kMOVF(byte1, toW);
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
procedure TGenCod.ROB_word_and_byte(Opt: TxpOperation; SetRes: boolean);
begin
  case stoOperation of
  stConst_Const: begin
    //Optimiza
    SetROBResultConst_byte(value1 and value2);
  end;
  stConst_Variab: begin
    SetROBResultExpres_byte(Opt);
    _LDAi(value1);
    _AND(byte2L);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en (A)
    SetROBResultExpres_byte(Opt);
    _ANDi(value1L);      //Deja en A
  end;
  stVariab_Const: begin
    SetROBResultExpres_byte(Opt);
    _LDA(byte1L);
    _ANDi(value2L);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    _LDA(byte1L);
    _AND(byte2L);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en (_H,A)
    SetROBResultExpres_byte(Opt);
    _AND(byte1);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetROBResultExpres_byte(Opt);
    _ANDi(value2L);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,A)
    SetROBResultExpres_byte(Opt);
    _AND(byte2L);
  end;
//  stExpres_Expres:begin
//    SetROBResultExpres_byte(Opt);
//    //p1 está salvado en pila y p2 en (A)
//    p1^.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
//    //Luego el caso es similar a stVariab_Expres
//    _AND(byte1);
//    FreeStkRegisterWord;   //libera pila
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.word_shift_l(fun: TxpEleFun);
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
procedure TGenCod.ROB_word_shl_byte(Opt: TxpOperation; SetRes: boolean);
var
  i, L1, L2: Integer;
  AddrUndef: boolean;
  fInLine: boolean;
begin
  fInLine := false;
  case stoOperation of
  stConst_Const: begin
    //Optimiza
    SetROBResultConst_byte(value1 << value2);
  end;
  stConst_Variab: begin
    SetROBResultExpres_word(Opt);
    _LDA(value1H);
    _STA(H.addr);  //Load high byte
    _LDA(value1L);
    //Loop
    _LDX(byte2);
    _BEQ_post(L2);  //Protección to zero
//_LABEL_pre(L1);
//      _ASLa;
//      _ROL(H.addr);
//      _DEX;
//    _BNE_pre(L1);
    AddCallerTo(f_word_shift_l);  //Declare use
    f_word_shift_l.procCall(f_word_shift_l, AddrUndef);  //Use
_LABEL_post(L2);
  end;
  stVariab_Const: begin
    SetROBResultExpres_word(Opt);
    if value2 < 4 then begin
      _LDA(byte1H);
      _STA(H.addr);  //Load high byte
      _LDA(byte1L);
      for i:=1 to value2 do begin
        _ASLa;
        _ROL(H.addr);
      end;
    end else begin
      _LDA(byte1H);
      _STA(H.addr);  //Load high byte
      _LDA(byte1L);
      //Loop
      _LDXi(value2);
      if fInLine then begin
_LABEL_pre(L1);
        _ASLa;
        _ROL(H.addr);
        _DEX;
        _BNE_pre(L1);
      end else begin
        AddCallerTo(f_word_shift_l);  //Declare use
        f_word_shift_l.procCall(f_word_shift_l, AddrUndef);  //Use
      end;
    end;
  end;
  stVariab_Variab:begin
    SetROBResultExpres_word(Opt);
    _LDA(byte1H);
    _STA(H.addr);  //Load high byte
    _LDA(byte1L);
    //Loop
    _LDX(byte2);
    _BEQ_post(L2);  //Protección to zero
    if fInLine then begin
_LABEL_pre(L1);
      _ASLa;
      _ROL(H.addr);
      _DEX;
      _BNE_pre(L1);
    end else begin
      AddCallerTo(f_word_shift_l);  //Declare use
      f_word_shift_l.procCall(f_word_shift_l, AddrUndef);  //Use
    end;
_LABEL_post(L2);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta A
    SetROBResultExpres_word(Opt);
    _TAX;  //Counter
    _BEQ_post(L2);  //Protección to zero

    _LDA(byte1H);
    _STA(H.addr);  //Load high byte
    _LDA(byte1L);
    if fInLine then begin
_LABEL_pre(L1);
      _ASLa;
      _ROL(H.addr);
      _DEX;
      _BNE_pre(L1);
    end else begin
      AddCallerTo(f_word_shift_l);  //Declare use
      f_word_shift_l.procCall(f_word_shift_l, AddrUndef);  //Use
    end;
_LABEL_post(L2);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetROBResultExpres_word(Opt);
    if value2 < 4 then begin
      for i:=1 to value2 do begin
        _ASLa;
        _ROL(H.addr);
      end;
    end else begin
      _LDXi(value2);
      if fInLine then begin
  _LABEL_pre(L1);
        _ASLa;
        _ROL(H.addr);
        _DEX;
        _BNE_pre(L1);
      end else begin
        AddCallerTo(f_word_shift_l);  //Declare use
        f_word_shift_l.procCall(f_word_shift_l, AddrUndef);  //Use
      end;
    end;
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,A)
    _LDXi(byte2);
    if fInLine then begin
_LABEL_pre(L1);
      _ASLa;
      _ROL(H.addr);
      _DEX;
      _BNE_pre(L1);
    end else begin
      AddCallerTo(f_word_shift_l);  //Declare use
      f_word_shift_l.procCall(f_word_shift_l, AddrUndef);  //Use
    end;
  end;
//  stExpres_Expres:begin
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_word_shr_byte(Opt: TxpOperation; SetRes: boolean);
var
  i: Integer;
begin
  case stoOperation of
  stConst_Const: begin
    //Optimiza
    SetROBResultConst_byte(value1 >> value2);
  end;
//  stConst_Variab: begin
//    SetROBResultExpres_byte(Opt);
//    _LDAi(value1);
//    _AND(byte2L);
//  end;
//  stConst_Expres: begin  //la expresión p2 se evaluó y esta en (A)
//    SetROBResultExpres_byte(Opt);
//    _ANDi(value1L);      //Deja en A
//  end;
  stVariab_Const: begin
    SetROBResultExpres_word(Opt);
    if value2 < 4 then begin
      _LDA(byte1H);
      _STA(H.addr);  //Load high byte
      _LDA(byte1L);
      for i:=1 to value2 do begin
        _LSRa;
        _ROR(H.addr);
      end;
    end else begin
      genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
    end;
  end;
//  stVariab_Variab:begin
//  end;
//  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en (_H,A)
//  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,A)
    SetROBResultExpres_word(Opt);
    if value2 < 4 then begin
      for i:=1 to value2 do begin
        _LSRa;
        _ROR(H.addr);
      end;
    end else begin
      genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
    end;
  end;
//  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,A)
//  end;
//  stExpres_Expres:begin
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
//////////// Operaciones con Char
procedure TGenCod.ROB_char_asig_char(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_byte_asig_byte(Opt, SetRes);
end;
procedure TGenCod.ROB_char_asig_string(Opt: TxpOperation; SetRes: boolean);
begin
  //Solo se permite asignar constamtes cadenas de 1 caracter
  if p2^.Sto <> stConst then begin
    GenError('Cannot assign to this Operand.'); exit;
    exit;
  end;
  if length(p2^.ValStr) <> 1 then begin
    GenError('String must be 1 char size.'); exit;
    exit;
  end;
  p2^.ValInt := ord(p2^.ValStr[1]);  //transform
  ROB_byte_asig_byte(Opt, SetRes);
end;
procedure TGenCod.ROB_char_equal_char(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_byte_equal_byte(Opt, SetRes);  //es lo mismo
end;
procedure TGenCod.ROB_char_difer_char(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_byte_difer_byte(Opt, SetRes); //es lo mismo
end;
//////////// Operaciones con String
//procedure TGenCod.ROB_string_add_string(Opt: TxpOperation; SetRes: boolean);
//{Implementation of string is incomplete. Just created this oepration to facilitate
//initialization of ARRAY OF chars with string}
//begin
//  case stoOperation of
//  stConst_Const: begin  //Special case. Comapares constants.
//    SetResultConst(typString);
//    res.valStr := p1^.valStr + p2^.valStr;
//  end;
//  else
//    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
//  end;
//end;
//procedure TGenCod.ROB_string_add_char(Opt: TxpOperation; SetRes: boolean);
//{Implementation of string is incomplete. Just created this oepration to facilitate
//initialization of ARRAY OF chars with string}
//begin
//  case stoOperation of
//  stConst_Const: begin  //Special case. Comapares constants.
//    SetResultConst(typString);
//    res.valStr := p1^.valStr + chr(p2^.valInt);
//  end;
//  else
//    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
//  end;
//end;
//////////// Pointer operations
procedure TGenCod.ROB_pointer_add_byte(Opt: TxpOperation; SetRes: boolean);
{Implementa la suma de un puntero (a cualquier tipo) y un byte.}
var
  ptrType: TxpEleType;
begin
  {Guarda la referencia al tipo puntero, porque:
  * Se supone que este tipo lo define el usuario y no se tiene predefinido.
  * Se podrían definir varios tipos de puntero) así que no se tiene una
  referencia estática
  * Conviene manejar esto de forma dinámica para dar flexibilidad al lenguaje}
  ptrType := p1^.Typ;   //Se ahce aquí porque después puede cambiar p1^.
  //La suma de un puntero y un byte, se procesa, como una suma de bytes
  ROB_word_add_byte(Opt, SetRes);
  //Devuelve byte, oero debe devolver el tipo puntero
  case res.Sto of
  stConst: res.SetAsConst(ptrType);  //Cambia el tipo a la constante
  //stVariab: res.SetAsVariab(res.rVar);
  {Si devuelve variable, solo hay dos posibilidades:
   1. Que sea la variable puntero, por lo que no hay nada que hacer, porque ya tiene
      el tipo puntero.
   2. Que sea la variable byte (y que la otra era constante puntero 0 = nil). En este
      caso devolverá el tipo Byte, lo cual tiene cierto sentido.}
  stExpres: res.SetAsExpres(ptrType);  //Cambia tipo a la expresión
  end;
end;
procedure TGenCod.ROB_pointer_add_word(Opt: TxpOperation; SetRes: boolean);
{Implementa la suma de un puntero (a cualquier tipo) y un byte.}
var
  ptrType: TxpEleType;
begin
  {Guarda la referencia al tipo puntero, porque:
  * Se supone que este tipo lo define el usuario y no se tiene predefinido.
  * Se podrían definir varios tipos de puntero) así que no se tiene una
  referencia estática
  * Conviene manejar esto de forma dinámica para dar flexibilidad al lenguaje}
  ptrType := p1^.Typ;   //Se hace aquí porque después puede cambiar p1^.
  //La suma de un puntero y un byte, se procesa, como una suma de bytes
  ROB_word_add_word(Opt, SetRes);
  //Devuelve byte, oero debe devolver el tipo puntero
  case res.Sto of
  stConst: res.SetAsConst(ptrType);  //Cambia el tipo a la constante
  //stVariab: res.SetAsVariab(res.rVar);
  {Si devuelve variable, solo hay dos posibilidades:
   1. Que sea la variable puntero, por lo que no hay nada que hacer, porque ya tiene
      el tipo puntero.
   2. Que sea la variable byte (y que la otra era constante puntero 0 = nil). En este
      caso devolverá el tipo Byte, lo cual tiene cierto sentido.}
  stExpres: res.SetAsExpres(ptrType);  //Cambia tipo a la expresión
  end;
end;
procedure TGenCod.ROB_pointer_sub_byte(Opt: TxpOperation; SetRes: boolean);
{Implementa la resta de un puntero (a cualquier tipo) y un byte.}
var
  ptrType: TxpEleType;
begin
  //La explicación es la misma que para la rutina ROB_pointer_add_byte
  ptrType := p1^.Typ;
  ROB_word_sub_byte(Opt, SetRes);
  case res.Sto of
  stConst: res.SetAsConst(ptrType);
  stExpres: res.SetAsExpres(ptrType);
  end;
end;
procedure TGenCod.ROB_pointer_sub_word(Opt: TxpOperation; SetRes: boolean);
{Implementa la resta de un puntero (a cualquier tipo) y un byte.}
var
  ptrType: TxpEleType;
begin
  //La explicación es la misma que para la rutina ROB_pointer_add_byte
  ptrType := p1^.Typ;
  ROB_word_sub_word(Opt, SetRes);
  case res.Sto of
  stConst: res.SetAsConst(ptrType);
  stExpres: res.SetAsExpres(ptrType);
  end;
end;
procedure TGenCod.ROU_derefPointer(Opr: TxpOperator; SetRes: boolean);
{Implementa el operador de desreferencia "^", para Opr que se supone debe ser
 categoria "tctPointer", es decir, puntero a algún tipo de dato.}
var
  tmpVar: TxpEleVar;
begin
  case p1^.Sto of
  stConst : begin
    //Caso especial. Cuando se tenga algo como: TPunteroAByte($FF)^
    //Se asume que devuelve una variable de tipo Byte.
    tmpVar := CreateTmpVar('', typByte);
    tmpVar.addr := value1;  //Fija dirección de constante
    SetROUResultVariab(tmpVar);
  end;
  stVariab: begin
    //Caso común: ptrWord^
    //La desreferencia de una variable "tctPointer" es un stVarRef.
    SetROUResultVarRef(p1^.rVar, p1^.rVar.typ.ptrType);
  end;
  stExpres: begin
    //La expresión Esta en RT, pero es una dirección, no un valor
    SetROUResultExpRef(p1^.Typ);
  end;
  else
    genError('Not implemented: "%s"', [Opr.OperationString]);
  end;
end;
///////////// Funciones del sistema
//**** Tal vez resulte útil, po rmodularidad, incluir también a las estructuras IF, REPEAT, WHILE, como funciones del sistema
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
procedure TGenCod.codif_delay_ms(fun: TxpEleFun);
//Codifica rutina de retardo en milisegundos
var
  delay: Word;
  LABEL1, ZERO: integer;
begin
  StartCodeSub(fun);  //inicia codificación
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
  EndCodeSub;  //termina codificación
  //aux.used := false;  //libera registro
end;
procedure TGenCod.fun_delay_ms(fun: TxpEleFunBase; out AddrUndef: boolean);
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //captura parámetro
  if HayError then exit;   //aborta
  //Se terminó de evaluar un parámetro
  LoadToRT(res);   //Carga en registro de trabajo
  if HayError then exit;
  if res.Typ = typByte then begin
    //El parámetro byte, debe estar en A
    if fun.idClass = eltFunc then begin
      _JSR(TxpEleFun(fun).adrr);
    end else begin
      GenError('Cannot get address of %s' + fun.name);
      //_JSR($1234);
    end;
  end else if res.Typ = typWord then begin
    //El parámetro word, debe estar en (H, A)
    if fun.idClass = eltFunc then begin
      _JSR(TxpEleFun(fun).adrr2);
    end else begin
      GenError('Cannot get address of %s' + fun.name);
      //_JSR($1234);
    end;
  end else begin
    GenError(MSG_INVAL_PARTYP, [res.Typ.name]);
    exit;
  end;
  //Verifica fin de parámetros
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Exit(fun: TxpEleFunBase; out AddrUndef: boolean);
{Se debe dejar en los registros de trabajo, el valor del parámetro indicado.}
var
  curFunTyp: TxpEleType;
  parentNod: TxpEleCodeCont;
  curFun: TxpEleFun;
  posExit: TSrcPos;
//  adrReturn: word;
begin
  //TreeElems.curNode, debe ser de tipo "Body".
  parentNod := TreeElems.CurCodeContainer;  //Se supone que nunca debería fallar
  posExit := cIn.ReadSrcPos;  //Guarda para el AddExitCall()
  if parentNod.idClass = eltMain then begin
    //Es el cuerpo del programa principal
    _RTS;   //Así se termina un programa en PicPas
  end else if parentNod.idClass = eltFunc then begin
    //Es el caso común, un exit() en procedimientos.
    //"parentNod" debe ser de tipo TxpEleFun
    curFun := TxpEleFun(parentNod);
    if curFun.IsInterrupt then begin
      GenError('Cannot use exit() in an INTERRUPT.');
      exit;
    end;
    //Codifica el retorno
    curFunTyp := curFun.typ;
    if curFunTyp = typNull then begin
      //No retorna valores. Es solo procedimiento
      _RTS;
      //No hay nada, más que hacer
    end else begin
      //Se espera el valor devuelto
      if not CaptureTok('(') then exit;
      res := GetExpression(0);  //captura parámetro
      if HayError then exit;   //aborta
      //Verifica fin de parámetros
      if not CaptureTok(')') then exit;
      //El resultado de la expresión está en "res".
      if curFunTyp <> res.Typ then begin
        GenError('Expected a "%s" expression.', [curFunTyp.name]);
        exit;
      end;
      LoadToRT(res);  //Carga expresión en RT y genera i_RETURN o i_RETLW
      _RTS;
    end;
  end else begin
    //Faltaría implementar en cuerpo de TxpEleUni
    GenError('Syntax error.');
  end;
  //Lleva el registro de las llamadas a exit()
  if FirstPass then begin
    parentNod.AddExitCall(posExit, parentNod.CurrBlockID);
  end;
  res.SetAsNull;  //No es función
end;
procedure TGenCod.fun_Inc(fun: TxpEleFunBase; out AddrUndef: boolean);
var
  LABEL1: integer;
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    GenError('Cannot increase a constant.'); exit;
  end;
  stVariab: begin
    if (res.Typ = typByte) or (res.Typ = typChar) then begin
      _INC(res.rVar.addr);
    end else if res.Typ = typWord then begin
      _INC(res.rVar.addr);
      _BNE_post(LABEL1);  //label
      _INC(res.rVar.addr+1);
_LABEL_post(LABEL1);
    end else if res.Typ.catType = tctPointer then begin
      //Es puntero corto
      _INC(res.rVar.addr);
    end else begin
      GenError(MSG_INVAL_PARTYP, [res.Typ.name]);
      exit;
    end;
  end;
//  stVarRef: begin
//    if (res.Typ = typByte) or (res.Typ = typChar) then begin
//      _INCF(res.offs, toF);
//    end else if res.Typ = typWord then begin
//      _INCF(res.addrL, toF);
//      _BTFSC(STATUS, _Z);
//      _INCF(res.addrH, toF);
//    end else if res.Typ = typDWord then begin
//      _INCF(res.addrL, toF);
//      _BTFSC(STATUS, _Z);
//      _INCF(res.addrH, toF);
//      _BTFSC(STATUS, _Z);
//      _INCF(res.addrE, toF);
//      _BTFSC(STATUS, _Z);
//      _INCF(res.addrU, toF);
//    end else if res.Typ.catType = tctPointer then begin
//      //Es puntero corto
//      _INCF(res.offs, toF);
//    end else begin
//      GenError(MSG_INVAL_PARTYP, [res.Typ.name]);
//      exit;
//    end;
//  end;
  stExpres: begin  //se asume que ya está en (_H,A)
    GenError('Cannot increase an expression.'); exit;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
  res.SetAsNull;  //No es función
  //Verifica fin de parámetros
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Dec(fun: TxpEleFunBase; out AddrUndef: boolean);
var
  lbl1: integer;
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    GenError('Cannot decrease a constant.'); exit;
  end;
  stVariab: begin
    if (res.Typ = typByte) or (res.Typ = typChar) then begin
      _DEC(res.rVar.addr);
    end else if res.Typ = typWord then begin
      _LDA(res.rVar.addr);
      _BNE_post(lbl1);
      _DEC(res.rVar.addr+1);
_LABEL_post(lbl1);
      _DEC(res.rVar.addr);
    end else if res.Typ.catType = tctPointer then begin
      //Es puntero corto
      _DEC(res.rVar.addr);
    end else begin
      GenError(MSG_INVAL_PARTYP, [res.Typ.name]);
      exit;
    end;
  end;
  stExpres: begin  //se asume que ya está en (_H,A)
    GenError('Cannot decrease an expression.'); exit;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
  res.SetAsNull;  //No es función
  //Verifica fin de parámetros
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.SetOrig(fun: TxpEleFunBase; out AddrUndef: boolean);
{Define el origen para colocar el código binario.}
// NO ES MUY ÚTIL PORQUE EL CAMBIO DE ORIGEN DEBE HACERSE ANTES DEL CÓDIGO
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    pic.iRam := res.valInt;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
  res.SetAsNull;  //No es función
  //Verifica fin de parámetros
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Ord(fun: TxpEleFunBase; out AddrUndef: boolean);
var
  tmpVar: TxpEleVar;
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if res.Typ = typChar then begin
      SetResultConst(typByte);  //Solo cambia el tipo, no el valor
      //No se usa SetROBResultConst_byte, porque no estamos en ROP
    end else begin
      GenError('Cannot convert to ordinal.'); exit;
    end;
  end;
  stVariab: begin
    if res.Typ = typChar then begin
      //Sigue siendo variable
      tmpVar := CreateTmpVar('', typByte);   //crea variable temporal Byte
      tmpVar.addr := res.rVar.addr; //apunta al mismo byte
      SetResultVariab(tmpVar);  //Actualiza "res"
    end else begin
      GenError('Cannot convert to ordinal.'); exit;
    end;
  end;
  stExpres: begin  //se asume que ya está en (A)
    if res.Typ = typChar then begin
      //Es la misma expresión, solo que ahora es Byte.
      res.SetAsExpres(typByte); //No se puede usar SetROBResultExpres_byte, porque no hay p1 y p2
    end else begin
      GenError('Cannot convert to ordinal.'); exit;
    end;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Chr(fun: TxpEleFunBase; out AddrUndef: boolean);
var
  tmpVar: TxpEleVar;
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if res.Typ = typByte then begin
      SetResultConst(typChar);  //Solo cambia el tipo, no el valor
      //No se usa SetROBResultConst_char, porque no estamos en ROP
    end else begin
      GenError('Cannot convert to char.'); exit;
    end;
  end;
  stVariab: begin
    if res.Typ = typByte then begin
      //Sigue siendo variable
      tmpVar := CreateTmpVar('', typChar);   //crea variable temporal
      tmpVar.addr := res.rVar.addr; //apunta al mismo byte
      SetResultVariab(tmpVar);
    end else begin
      GenError('Cannot convert to char.'); exit;
    end;
  end;
  stExpres: begin  //se asume que ya está en (A)
    if res.Typ = typByte then begin
      //Es la misma expresión, solo que ahora es Char.
      res.SetAsExpres(typChar); //No se puede usar SetROBResultExpres_char, porque no hay p1 y p2;
    end else begin
      GenError('Cannot convert to char.'); exit;
    end;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Byte(fun: TxpEleFunBase; out AddrUndef: boolean);
var
  tmpVar: TxpEleVar;
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if res.Typ = typByte then begin
      //ya es Byte
    end else if res.Typ = typChar then begin
      res.SetAsConst(typByte);  //Solo cambia el tipo
    end else if res.Typ = typWord then begin
      res.SetAsConst(typByte);  //Cambia el tipo
      res.valInt := res.valInt and $FF;
    end else begin
      GenError('Cannot convert to byte.'); exit;
    end;
  end;
  stVariab: begin
    if res.Typ = typChar then begin
      //Crea varaible que apunte al byte bajo
      tmpVar := CreateTmpVar('', typByte);   //crea variable temporal Byte
      tmpVar.addr := res.rVar.addr;  //apunta al mismo byte
      SetResultVariab(tmpVar);
    end else if res.Typ = typByte then begin
      //Es lo mismo
    end else if res.Typ = typWord then begin
      //Crea varaible que apunte al byte bajo
      tmpVar := CreateTmpVar('', typByte);   //crea variable temporal Byte
      tmpVar.addr := res.rVar.addr;  //apunta al mismo byte
      SetResultVariab(tmpVar);
    end else begin
      GenError('Cannot convert to byte.'); exit;
    end;
  end;
  stExpres: begin  //se asume que ya está en (A)
    if res.Typ = typByte then begin
      //Ya está en A
      //Ya es Byte
    end else if res.Typ = typChar then begin
      //Ya está en A
      res.SetAsExpres(typByte);  //Solo cambia el tipo
    end else if res.Typ = typWord then begin
      //Ya está en A el byte bajo
      res.SetAsExpres(typByte);  //Cambia el tipo
    end else begin
      GenError('Cannot convert to byte.'); exit;
    end;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Word(fun: TxpEleFunBase; out AddrUndef: boolean);
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if res.Typ = typByte then begin
      res.SetAsConst(typWord);  //solo cambia el tipo
    end else if res.Typ = typChar then begin
      res.SetAsConst(typWord);  //solo cambia el tipo
    end else if res.Typ = typWord then begin
      //ya es Word
    end else begin
      GenError('Cannot convert this constant to word.'); exit;
    end;
  end;
  stVariab: begin
    typWord.DefineRegister;
    if res.Typ = typByte then begin
      SetResultExpres(typWord);  //No podemos devolver variable. Pero sí expresión
      _LDAi(0);
      _STA(H.addr);
      _LDA(res.rVar.addr);
    end else if res.Typ = typChar then begin
      SetResultExpres(typWord);  //No podemos devolver variable. Pero sí expresión
      _LDAi(0);
      _STA(H.addr);
      _LDA(res.rVar.addr);
    end else if res.Typ = typWord then begin
      //ya es Word
    end else begin
      GenError('Cannot convert this variable to word.'); exit;
    end;
  end;
  stExpres: begin  //se asume que ya está en (A)
    typWord.DefineRegister;
    if res.Typ = typByte then begin
      res.SetAsExpres(typWord);
      //Ya está en A el byte bajo
      _LDXi(0);
      _STX(H.addr);
    end else if res.Typ = typChar then begin
      res.SetAsExpres(typWord);
      //Ya está en A el byte bajo
      _LDXi(0);
      _STX(H.addr);
    end else if res.Typ = typWord then begin
//      Ya es word
    end else begin
      GenError('Cannot convert expression to word.'); exit;
    end;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Addr(fun: TxpEleFunBase; out AddrUndef: boolean);
{Resturn de addres of a datatype.}
var
  xtyp: TxpEleType;
  typName: String;
  xvar: TxpEleVar;
  srcPos: TSrcPos;
begin
  srcPos := cIn.ReadSrcPos;
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    genError('Cannot obtain address of constant.');
    exit;
  end;
  stVariab: begin
    //Es una variable simple. Debe devolver un puntero o (dirección)
    xvar := res.rVar;
    //Crea un tipo puntero a la variable.
    if not TreeElems.ExistsPointerType(xvar.typ, xtyp) then begin
      {Genera nombre del tipo. Este nombre es compatible con el que se genera en
      TCompiler_PIC16.CompileVarDeclar().}
      typName := GenPointerTypeName(xvar.typ.name);
      //There is not a similar type. We create a new type.
      xtyp := CreateEleTypePtr(typName, srcPos, xvar.typ);
      //Add to the syntax tree
      xtyp.location := curLocation;   //Ubicación del tipo (Interface/Implementation/...)
      if TreeElems.curNode.idClass = eltBody then begin
        //This should be the normal position where we espect to have a call to Addr()
        //We prefer to declare the type in the parent (procedure or main)
        TreeElems.AddElementParent(xtyp, true);  //Add at the beginning
      end else begin
        //Normally, addr() shouldn't appears out of "body" element.
        TreeElems.AddElement(xtyp);
      end;
    end;
    //Define resultado
    SetResultConst(xtyp);  //Una variable tiene dirección fija
    res.valInt := xvar.addr;
  end;
  stExpres: begin  //se asume que ya está en (A)
    genError('Cannot obtain address of an expression.');
    exit;
  end;
  else
    genError('Cannot obtain address of this operand.');
  end;
  if not CaptureTok(')') then exit;
end;
//Routines to implement arrays and pointers
procedure TGenCod.arrayLow(const OpPtr: pointer);
//Devuelve el índice mínimo de un arreglo
var
  Op: ^TOperand;
begin
  cIn.Next;  //Toma identificador de campo
  Op := OpPtr;
  case Op^.Sto of
  stConst, stVariab: begin
    //Se devuelve una constante, byte
    res.SetAsConst(typByte);
    res.valInt := 0;  //por ahora siempre inicia en 0
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TGenCod.arrayHigh(const OpPtr: pointer);
//Devuelve el índice máximo de un arreglo
var
  Op: ^TOperand;
begin
  cIn.Next;  //Toma identificador de campo
  Op := OpPtr;
  case Op^.Sto of
  stConst, stVariab: begin
    if Op^.nItems-1>255 then res.SetAsConst(typWord)
    else res.SetAsConst(typByte);
    res.valInt := Op^.nItems-1;  //Lee directamente
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TGenCod.arrayLength(const OpPtr: pointer);
//Devuelve la cantidad de elementos de un arreglo
var
  Op: ^TOperand;
begin
  cIn.Next;  //Toma identificador de campo
  Op := OpPtr;
  case Op^.Sto of
  stConst, stVariab: begin
    if Op^.nItems>255 then res.SetAsConst(typWord)
    else res.SetAsConst(typByte);
    res.valInt := Op^.nItems;  //Lee directamente
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TGenCod.ROB_arr_asig_arr(Opt: TxpOperation; SetRes: boolean);
{Array asignation}
var
  nItems, itSize, i: Integer;
  nBytes, des: Integer;
  itType: TxpeleType;
  src: Word;
  tmpvar: TxpEleVar;
  values: array of TConsValue;
  opr1: TxpOperator;
  startAddr, j2: integer;
begin
  SetResultNull;  //In Pascal an assigment doesn't return type.
  if p1^.nItems <> p2^.nItems then begin
    GenError('Array sizes doesn''t match.');
    exit;
  end;
  if p1^.Sto = stVariab then begin
    nItems := p1^.nItems;
    nBytes := p1^.rVar.typ.size;
    itType := p1^.rVar.typ.itmType;
    itSize := itType.size;
    case p2^.Sto of
    stConst : begin
      if nBytes < 5 then begin
        //Just a little bytes
        tmpvar := CreateTmpVar('', itType);
        tmpVar.addr := p1^.rVar.addr;
        values := p2^.Value.items;
        for i:=1 to nItems do begin
          p1^.SetAsVariab(tmpvar);
          p2^.SetAsConst(itType, values[i-1]);
          opr1 := p1^.Typ.FindBinaryOperator(':=');  //Busca operador de comparación
          if opr1 = nullOper then begin
            GenError('Internal: No operator := defined for %s.', [p1^.Typ.name]);
            exit;
          end;
          Oper(p1^, opr1, p2^);   //Compile operations
          //Prepare next assigment
          tmpVar.addr += itSize;
        end;
      end else if nBytes< 256 then begin
        //Several ítems, we first write Op2 in RAM.
        CreateValueInCode(p2^.Typ, p2^.Value, startAddr);
        //Now we have Op2 created in RAM. Lets move.
        _LDXi(nBytes);
_LABEL_pre(j2);
        pic.codAsm(i_LDA, aAbsolutX, (startAddr-1) and $FFFF);  //Fix address to fit the index loop
        pic.codAsm(i_STA, aAbsolutX, (p1^.rVar.addr-1) and $FFFF);  //Fix address to fit the index loop
        _DEX;
        _BNE_pre(j2);
      end else begin
        GenError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
      end;
    end;
    stVariab: begin
      if nBytes < 5 then begin
        des:=p1^.rVar.addr;
        for src:=p2^.rVar.addr to p2^.rVar.addr+nBytes-1 do begin
          _LDA(src);
          _STA(des);
          inc(des);
        end;
      end else if nBytes< 256 then begin
        //Several ítems, we will use a loop to copy.
        //Now we have the variable created in RAM. Lets move
        _LDXi(nBytes);
_LABEL_pre(j2);
        pic.codAsm(i_LDA, aAbsolutX, (p2^.rVar.addr-1) and $FFFF);  //Fix address to fit the index loop
        pic.codAsm(i_STA, aAbsolutX, (p1^.rVar.addr-1) and $FFFF);  //Fix address to fit the index loop
        _DEX;
        _BNE_pre(j2);
      end else begin
        GenError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
      end;
    end;
//    stExpres: begin   //se asume que está en A
//      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
//      _STA(byte1L);
//      _LDA(0);
//      _STA(byte1H);
//    end;
    else
      GenError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
function TGenCod.GetIdxParArray(out WithBrack: boolean; out par: TOperand): boolean;
{Extrae el primer parámetro (que corresponde al índice) de las funciones getitem() o
setitem(). También reconoce las formas con corchetes [], y en ese caso pone "WithBrackets"
en TRUE. Si encuentra error, devuelve false.}
begin
  if cIn.tok = '[' then begin
    //Es la sintaxis a[i];
    WithBrack := true;
    cIn.Next;  //Toma "["
  end else begin
    //Es la sintaxis a.item(i);
    WithBrack := false;
    cIn.Next;  //Toma identificador de campo
    //Captura parámetro
    if not CaptureTok('(') then exit(false);
  end;
  par := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
//  if HayError then exit(false);
  if HayError then exit(false);
  exit(true);
end;
procedure TGenCod.GenCodArrayGetItem(const OpPtr: pointer);
{Función que devuelve el valor indexado del arreglo. Devuelve el resultado como un
operando en "res".}
var
  Op: ^TOperand;
  arrVar, tmpVar: TxpEleVar;
  idx: TOperand;
  WithBrack: Boolean;
  itemType: TxpEleType;
begin
  if not GetIdxParArray(WithBrack, idx) then exit;
  //Procesa
  Op := OpPtr;
  if Op^.Sto = stVariab then begin
    //Applied to a variable array. The normal.
    arrVar := Op^.rVar;        //Reference to variable
    itemType := arrVar.typ.itmType; //Reference to the item type
    //Generate code according to the index storage.
    case idx.Sto of
    stConst: begin  //ïndice constante
      tmpVar := CreateTmpVar('', itemType);  //VAriable del mismo tipo
      tmpVar.addr := arrVar.addr + idx.valInt * itemType.size;
      SetResultVariab(tmpVar);
    end;
    stVariab: begin  //Indexado por variable
        SetResultExpres(itemType, true);  //Devuelve el mismo tipo
        if idx.Typ.IsByteSize then begin
          //Index is byte-size variable
          if itemType.IsByteSize then begin
            //And item is byte size, so it's easy to index in 6502
            _LDX(idx.rVar.addr);
            if arrVar.addr<256 then begin
              pic.codAsm(i_LDA, aZeroPagX, arrVar.addr);
            end else begin
              pic.codAsm(i_LDA, aAbsolutX, arrVar.addr);
            end;
          end else if itemType.IsWordSize then begin
            //Item is word size. We need to multiply index by 2 and load indexed.
            //WARNING this is "Auto-modifiying" code.
            _LDA(idx.rVar.addr);  //Load index
            pic.codAsm(i_STA, aAbsolute, _PC + 15); //Store forward
            _LDAi(0);  //Load virtual MSB index
            pic.codAsm(i_STA, aAbsolute, _PC + 11); //Store forward
            pic.codAsm(i_ASL, aAbsolute, _PC + 7);  //Shift operand of LDA
            PIC.codAsm(i_ROL, aAbsolute, _PC + 5);  //Shift operand of LDA
            pic.codAsm(i_LDA, aAbsolute, 0); //Store forward (DANGER)
            //Could be optimized getting a Zero page index
          end else begin
            //Here we need to multiply the index by the item size.
            GenError('Too complex item.');
          end;
        end else if idx.Typ.IsWordSize then begin
          //Index is word-size variable
          if itemType.IsByteSize then begin
            //Item is byte size. We need to index with a word.
            if idx.rVar.addr<256 then begin
              //Index in zero page
              _LDYi(0);
              pic.codAsm(i_LDA, aIndirecY, idx.rVar.addr);
            end else begin
              //Index is out of zero page
              //WARNING this is "Auto-modifiying" code.
              _CLC;
              _LDA(idx.rVar.addr);  //Load index
              _ADCi(arrVar.addr and $FF);
              pic.codAsm(i_STA, aAbsolute, _PC + 12); //Store forward
              _LDA(idx.rVar.addr+1);  //Load MSB index
              _ADCi( arrVar.addr >> 8 );
              pic.codAsm(i_STA, aAbsolute, _PC + 5); //Store forward
              //Modifiying instruction
              pic.codAsm(i_LDA, aAbsolute, 0);
            end;
          end else if itemType.IsWordSize then begin
            //Item is word size. We need to multiply index by 2 and load indexed.
            //WARNING this is "Auto-modifiying" code.
            _LDA(idx.rVar.addr);  //Load index
            pic.codAsm(i_STA, aAbsolute, _PC + 16); //Store forward
            _LDA(idx.rVar.addr+1);  //Load virtual MSB index
            pic.codAsm(i_STA, aAbsolute, _PC + 11); //Store forward
            pic.codAsm(i_ASL, aAbsolute, _PC + 7);  //Shift operand of LDA
            PIC.codAsm(i_ROL, aAbsolute, _pc + 5);  //Shift operand of LDA
            pic.codAsm(i_LDA, aAbsolute, 0); //Store forward
          end else begin
            //Here we need to multiply the index by the item size.
            GenError('Too complex item.');
          end;
        end else begin
          GenError('Not supported this index.');
        end;
    end;
//    stExpres: begin
//      SetResultExpres(itemType, true);  //Devuelve el mismo tipo
//      if idx.Typ.IsByteSize then begin
//        //Index is byte-size variable
//        if itemType.IsByteSize then begin
//          //And item is byte size, so it's easy to index in 6502
//          _LDX(idx.rVar.addr);
//          if arrVar.addr<256 then begin
//            pic.codAsm(i_LDA, aZeroPagX, arrVar.addr);
//          end else begin
//            pic.codAsm(i_LDA, aAbsolutX, arrVar.addr);
//          end;
//        end else if itemType.IsWordSize then begin
//          //Item is word size. We need to multiply index by 2 and load indexed.
//          //WARNING this is "Auto-modifiying" code.
//          _LDA(idx.rVar.addr);  //Load index
//          pic.codAsm(i_STA, aAbsolute, _PC + 15); //Store forward
//          _LDAi(0);  //Load virtual MSB index
//          pic.codAsm(i_STA, aAbsolute, _PC + 11); //Store forward
//          pic.codAsm(i_ASL, aAbsolute, _PC + 7);  //Shift operand of LDA
//          PIC.codAsm(i_ROL, aAbsolute, _PC + 5);  //Shift operand of LDA
//          pic.codAsm(i_LDA, aAbsolute, 0); //Store forward (DANGER)
//          //Could be optimized getting a Zero page index
//        end else begin
//          //Here we need to multiply the index by the item size.
//          GenError('Too complex item.');
//        end;
//      end else if idx.Typ.IsWordSize then begin
//        //Index is word-size variable
//        if itemType.IsByteSize then begin
//          //Item is byte size. We need to index with a word.
//          if idx.rVar.addr<256 then begin
//            //Index in zero page
//            _LDYi(0);
//            pic.codAsm(i_LDA, aIndirecY, idx.rVar.addr);
//          end else begin
//            //Index is out of zero page
//            //WARNING this is "Auto-modifiying" code.
//            _CLC;
//            _LDA(idx.rVar.addr);  //Load index
//            _ADCi(arrVar.addr and $FF);
//            pic.codAsm(i_STA, aAbsolute, _PC + 12); //Store forward
//            _LDA(idx.rVar.addr+1);  //Load MSB index
//            _ADCi( arrVar.addr >> 8 );
//            pic.codAsm(i_STA, aAbsolute, _PC + 5); //Store forward
//            //Modifiying instruction
//            pic.codAsm(i_LDA, aAbsolute, 0);
//          end;
//        end else if itemType.IsWordSize then begin
//          //Item is word size. We need to multiply index by 2 and load indexed.
//          //WARNING this is "Auto-modifiying" code.
//          _LDA(idx.rVar.addr);  //Load index
//          pic.codAsm(i_STA, aAbsolute, _PC + 16); //Store forward
//          _LDA(idx.rVar.addr+1);  //Load virtual MSB index
//          pic.codAsm(i_STA, aAbsolute, _PC + 11); //Store forward
//          pic.codAsm(i_ASL, aAbsolute, _PC + 7);  //Shift operand of LDA
//          PIC.codAsm(i_ROL, aAbsolute, _pc + 5);  //Shift operand of LDA
//          pic.codAsm(i_LDA, aAbsolute, 0); //Store forward
//        end else begin
//          //Here we need to multiply the index by the item size.
//          GenError('Too complex item.');
//        end;
//      end else begin
//        GenError('Not supported this index.');
//      end;
//    end;
    else
      GenError('Not supported this index.');
    end;
  end else begin
    GenError('Cannot index a constant array.');
  end;
  if WithBrack then begin
    if not CaptureTok(']') then exit;
  end else begin
    if not CaptureTok(')') then exit;
  end;
end;
procedure TGenCod.GenCodArraySetItem(const OpPtr: pointer);
{Función que devuelve el valor indexado del arreglo para escritura. Devuelve el resultado
como un operando en "res".}
var
  Op: ^TOperand;
  arrVar, tmpVar: TxpEleVar;
  idx: TOperand;
  WithBrack: Boolean;
  itemType: TxpEleType;
begin
  if not GetIdxParArray(WithBrack, idx) then exit;
  //Procesa
  Op := OpPtr;
  if Op^.Sto = stVariab then begin
    //Applied to a variable array. The normal.
    arrVar := Op^.rVar;        //Reference to variable
    itemType := arrVar.typ.itmType; //Reference to the item type
    //Generate code according to the index storage.
    case idx.Sto of
    stConst: begin  //Constant index
        tmpVar := CreateTmpVar('', itemType);  //VAriable del mismo tipo
        tmpVar.addr := arrVar.addr + idx.valInt * itemType.size;
        SetResultVariab(tmpVar);
      end;
    stVariab: begin  //Indexed by variable
      if idx.Typ.IsByteSize then begin
        //Index is byte-size variable
        if itemType.IsByteSize then begin
          //This is important because we know the variable can direct the current position
          SetResultVarConRef(idx.rVar, arrVar.addr, itemType);  //Devuelve el mismo tipo
        end else if itemType.IsWordSize then begin
          SetResultExpRef(itemType, false);  //We don't need to check RT here in setter.
          AddCallerTo(IX);  //We declare using IX
          //Item is word size. We need to multiply index by 2 and load indexed.
          _LDXi(0);
          _STX(IX.addr+1);      //High(IX) <- 0
          _LDA(idx.rVar.addr);  //Load LSB index
          _ASLa;
          _STA(IX.addr);
          _ROL(IX.addr+1);
        end else begin
          //We need to calculate the final address accoridng to the item size
          GenError('Not supported this item size to assigment.');
        end;
      end else if idx.Typ.IsWordSize then begin
        //Index is words-size variable
        if itemType.IsByteSize then begin
          //This is important because we know the variable can direct the current position
          SetResultVarConRef(idx.rVar, arrVar.addr, itemType);  //Devuelve el mismo tipo
//        end else if itemType.IsWordSize then begin
//          SetResultExpRef(itemType, false);  //We don't need to check RT here in setter.
//          AddCallerTo(IX);  //We declare using IX
//          //Item is word size. We need to multiply index by 2 and load indexed.
//          //WARNING this is "Self-modifiying" code.
//          _LDA(idx.rVar.addr);  //Load index
//          _
        end else begin
          //We need to calculate the final address accoridng to the item size
          GenError('Not supported this item size to assigment.');
        end;
      end else begin
        GenError('Not supported this index.');
      end;
    end;
    stExpres: begin  //Indexed by expression
      //stExpres must return the address in IX register
      if idx.Typ.IsByteSize then begin
        //Index is byte-size expresión (Loaded in A).
        if itemType.IsByteSize then begin
          SetResultExpRef(itemType, False);
          AddCallerTo(IX);  //We declare using IX
          //We add A to base address of array -> IX
          _CLC;
          _ADCi(arrVar.addr and $FF);
          _STA(IX.addr);  //Save
          _LDAi(arrVar.addr >> 8);
          _ADCi(0);
          _STA(IX.addr+1);
        end else begin
          GenError('Not supported this item size.');
        end;
      end else if idx.Typ.IsWordSize then begin
        //Index is word-size expresión (Loaded in H,A).
        if itemType.IsByteSize then begin
          SetResultExpRef(itemType, False);
          AddCallerTo(IX);  //We declare using IX
          //We add H,A to base address of array -> IX
          _CLC;
          _ADCi(arrVar.addr and $FF);
          _STA(IX.addr);  //Save
          _LDAi(arrVar.addr >> 8);
          _ADC(H.addr);
          _STA(IX.addr+1);
        end else begin
          GenError('Not supported item size.');
        end;
      end else begin
        GenError('Not supported this index.');
      end;
    end
    else
      GenError('Not supported this index.');
    end;
  end else begin
    GenError('Cannot index a constant array.');
  end;
  if WithBrack then begin
    if not CaptureTok(']') then exit;
  end else begin
    if not CaptureTok(')') then exit;
  end;
end;
procedure TGenCod.GenCodArrayClear(const OpPtr: pointer);
{Used to clear all items of an array operand.}
var
  Op: ^TOperand;
  xvar: TxpEleVar;
  n, LABEL1: Integer;
begin
  cIn.Next;  //Toma identificador de campo
  //Limpia el arreglo
  Op := OpPtr;
  case Op^.Sto of
  stVariab: begin
    xvar := Op^.rVar;  //It's supossed to be ARRAY
    res.SetAs(Op^);     //Return the same operand
    n := xvar.typ.nItems;
    if n = 0 then exit;  //Nothing to clear
    if n > 0 then begin
      if n = 1 then begin   //Just one byte
        _LDAi(0);
        _STA(xvar.addr);
      end else if n = 2 then begin  //Es de 2 bytes
        _LDAi(0);
        _STA(xvar.addr);
        _STA(xvar.addr+1);
      end else if n = 3 then begin  //Es de 3 bytes
        _LDAi(0);
        _STA(xvar.addr);
        _STA(xvar.addr+1);
        _STA(xvar.addr+2);
      end else if n = 4 then begin  //Es de 4 bytes
        _LDAi(0);
        _STA(xvar.addr);
        _STA(xvar.addr+1);
        _STA(xvar.addr+2);
        _STA(xvar.addr+3);
      end else if n <=256 then begin  //Tamaño pequeño
        _LDAi(0);
        _LDXi(n and $FF);
LABEL1 := _PC;
        if xVar.addr <256 then pic.codAsm(i_STA, aZeroPagX, xVar.addr)  //STA has not aZeroPagY
        else pic.codAsm(i_STA, aAbsolutX, xVar.addr-1);
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
//procedure TGenCod.DefineShortPointer(etyp: TxpEleType);
//{Configura las operaciones que definen la aritmética de punteros.}
//var
//  opr: TxpOperator;
//begin
//  //Asignación desde Byte y Puntero
//  opr:=etyp.CreateBinaryOperator(':=',2,'asig');
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
//end;
procedure TGenCod.DefinePointer(etyp: TxpEleType);
{Set operations that defines pointers aritmethic.}
var
  opr: TxpOperator;
begin
  //Asignación desde Byte y Puntero
  opr:=etyp.CreateBinaryOperator(':=',2,'asig');
  opr.CreateOperation(typWord, @ROB_word_asig_word);
  opr.CreateOperation(etyp   , @ROB_word_asig_word);
  //Agrega a los word, la posibilidad de ser asignados por punteros
  typWord.operAsign.CreateOperation(etyp, @ROB_word_asig_word);

  opr:=etyp.CreateBinaryOperator('=',3,'equal');  //asignación
  opr.CreateOperation(typWord, @ROB_word_equal_word);
  opr:=etyp.CreateBinaryOperator('+',4,'add');  //suma
  opr.CreateOperation(typWord, @ROB_pointer_add_word);
  opr.CreateOperation(typByte, @ROB_pointer_add_byte);
  opr:=etyp.CreateBinaryOperator('-',4,'add');  //resta
  opr.CreateOperation(typWord, @ROB_pointer_sub_word);
  opr.CreateOperation(typByte, @ROB_pointer_sub_byte);

  etyp.CreateUnaryPreOperator('@', 6, 'addr', @ROU_address); //defined in all types
  etyp.CreateUnaryPostOperator('^',6, 'deref', @ROU_derefPointer);  //dereferencia
end;
procedure TGenCod.DefineArray(etyp: TxpEleType);
var
  opr: TxpOperator;
begin
  etyp.CreateField('length', @arrayLength, nil);
  etyp.CreateField('high'  , @arrayHigh, nil);
  etyp.CreateField('low'   , @arrayLow, nil);
  etyp.CreateField('item'  , @GenCodArrayGetItem, @GenCodArraySetItem);
  etyp.CreateField('clear' , @GenCodArrayClear, @GenCodArrayClear);
  etyp.CreateUnaryPreOperator('@', 6, 'addr', @ROU_address); //defined in all types
  opr := etyp.CreateBinaryOperator(':=', 2, 'asig');
  opr.CreateOperation(etyp, @ROB_arr_asig_arr);
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
procedure TGenCod.StartSyntax;
//Se ejecuta solo una vez al inicio
begin
  ///////////define la sintaxis del compilador
  //Tipos de tokens personalizados
  tnExpDelim := xLex.NewTokType('ExpDelim');//delimitador de expresión ";"
  tnBlkDelim := xLex.NewTokType('BlkDelim'); //delimitador de bloque
  tnStruct   := xLex.NewTokType('Struct');   //personalizado
  tnDirective:= xLex.NewTokType('Directive'); //personalizado
  tnAsm      := xLex.NewTokType('Asm');      //personalizado
  tnChar     := xLex.NewTokType('Char');     //personalizado
  tnOthers   := xLex.NewTokType('Others');   //personalizado
  //Configura atributos
  tkKeyword.Style := [fsBold];     //en negrita
  xLex.Attrib[tnBlkDelim].Foreground:=clGreen;
  xLex.Attrib[tnBlkDelim].Style := [fsBold];    //en negrita
  xLex.Attrib[tnStruct].Foreground:=clGreen;
  xLex.Attrib[tnStruct].Style := [fsBold];      //en negrita
  //inicia la configuración
  xLex.ClearMethodTables;          //limpia tabla de métodos
  xLex.ClearSpecials;              //para empezar a definir tokens
  //crea tokens por contenido
  xLex.DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');
  xLex.DefTokContent('[0-9]', '[0-9.]*', tnNumber);
  xLex.DefTokContent('[$]','[0-9A-Fa-f]*', tnNumber);
  xLex.DefTokContent('[%]','[01]*', tnNumber);
  //define palabras claves
  xLex.AddIdentSpecList('THEN var type absolute interrupt', tnKeyword);
  xLex.AddIdentSpecList('program public private method const', tnKeyword);
  xLex.AddIdentSpecList('class create destroy sub do begin', tnKeyword);
  xLex.AddIdentSpecList('END ELSE ELSIF UNTIL', tnBlkDelim);
  xLex.AddIdentSpecList('true false', tnBoolean);
  xLex.AddIdentSpecList('if while repeat for', tnStruct);
  xLex.AddIdentSpecList('and or xor not div mod in', tnOperator);
  xLex.AddIdentSpecList('umulword', tnOperator);
  //tipos predefinidos
  xLex.AddIdentSpecList('bit boolean byte word char dword', tnType);
  //funciones del sistema
  xLex.AddIdentSpecList('exit Inc Dec Ord Chr', tnSysFunct);
  xLex.AddIdentSpecList('SetOrig addr', tnSysFunct);
  //símbolos especiales
  xLex.AddSymbSpec('+',  tnOperator);
  xLex.AddSymbSpec('+=', tnOperator);
  xLex.AddSymbSpec('-=', tnOperator);
  xLex.AddSymbSpec('-',  tnOperator);
  xLex.AddSymbSpec('*',  tnOperator);
  xLex.AddSymbSpec('/',  tnOperator);
  xLex.AddSymbSpec('\',  tnOperator);
//  xLex.AddSymbSpec('%',  tnOperator);
  xLex.AddSymbSpec('**', tnOperator);
  xLex.AddSymbSpec('=',  tnOperator);
  xLex.AddSymbSpec('>',  tnOperator);
  xLex.AddSymbSpec('<',  tnOperator);
  xLex.AddSymbSpec('>=', tnOperator);
  xLex.AddSymbSpec('<=', tnOperator);
  xLex.AddSymbSpec('<>', tnOperator);
  xLex.AddSymbSpec('<=>',tnOperator);
  xLex.AddSymbSpec(':=', tnOperator);
  xLex.AddSymbSpec('>>', tnOperator);
  xLex.AddSymbSpec('<<', tnOperator);
  xLex.AddSymbSpec('^', tnOperator);
  xLex.AddSymbSpec('@', tnOperator);
  xLex.AddSymbSpec('.', tnOperator);
  xLex.AddSymbSpec(';', tnExpDelim);
  xLex.AddSymbSpec('(',  tnOthers);
  xLex.AddSymbSpec(')',  tnOthers);
  xLex.AddSymbSpec(':',  tnOthers);
  xLex.AddSymbSpec(',',  tnOthers);
  xLex.AddSymbSpec('[',  tnOthers);
  xLex.AddSymbSpec(']',  tnOthers);
  //crea tokens delimitados
  xLex.DefTokDelim('''','''', tnString);
  xLex.DefTokContent('[#]','[0-9]*', tnChar);
//  xLex.DefTokDelim('"','"', tnString);

  xLex.DefTokDelim('//','', xLex.tnComment);
  xLex.DefTokDelim('{','}', xLex.tnComment, tdMulLin);
  xLex.DefTokDelim('(\*','\*)', xLex.tnComment, tdMulLin);
  xLex.DefTokDelim('{$','}', tnDirective, tdUniLin);
  xLex.DefTokDelim('Asm','End', tnAsm, tdMulLin);
  //define bloques de sintaxis
//  xLex.AddBlock('{','}');
  xLex.Rebuild;   //es necesario para terminar la definición
end;
procedure TGenCod.DefCompiler;
{}
var
  opr: TxpOperator;
begin
  //Define métodos a usar
  OnExprStart := @expr_start;
  OnExprEnd := @expr_End;

  ///////////Crea tipos
  ClearSystemTypes;
  //////////////// Boolean type /////////////
  typBool := CreateSysType('boolean',t_uinteger,1);   //de 1 byte
  typBool.OnLoadToRT   := @byte_LoadToRT;
  typBool.OnDefRegister:= @byte_DefineRegisters;
  typBool.OnSaveToStk  := @byte_SaveToStk;
  //typBool.OnReadFromStk :=
  //////////////// Byte type /////////////
  typByte := CreateSysType('byte',t_uinteger,1);   //de 1 byte
  typByte.OnLoadToRT   := @byte_LoadToRT;
  typByte.OnDefRegister:= @byte_DefineRegisters;
  typByte.OnSaveToStk  := @byte_SaveToStk;
  //typByte.OnReadFromStk :=

  //////////////// Char type /////////////
  //Tipo caracter
  typChar := CreateSysType('char',t_uinteger,1);   //de 1 byte. Se crea como uinteger para leer/escribir su valor como número
  typChar.OnLoadToRT   := @byte_LoadToRT;  //Es lo mismo
  typChar.OnDefRegister:= @byte_DefineRegisters;  //Es lo mismo
  typChar.OnSaveToStk  := @byte_SaveToStk; //Es lo mismo

  //////////////// Word type /////////////
  //Tipo numérico de dos bytes
  typWord := CreateSysType('word',t_uinteger,2);   //de 2 bytes
  typWord.OnLoadToRT   := @word_LoadToRT;
  typWord.OnDefRegister:= @word_DefineRegisters;
  typWord.OnSaveToStk  := @word_SaveToStk;
//  typWord.OnClearItems := @word_ClearItems;

  typWord.CreateField('Low', @word_Low, @word_Low);
  typWord.CreateField('High', @word_High, @word_High);

  //////////////// String type /////////////
  {Se crea el tipo String, solo para permitir inicializar arreglos de
  caracteres. Por ahora no se implementan otras funcionalidades.}
//  typString := CreateSysType('string', t_string, 0);  //tamaño variable
  { TODO : String debería definirse mejor como un tipo común, no del sistema }

  {Operators must be created according to the Operator precedence in Pascal:
  6)    ~, not, sign "-"    (high precedence)
  5)    *, /, div, mod, and, shl, shr, &
  4)    |, !, +, -, or, xor
  3)    =, <>, <, <=, >, >=, in
  2)    :=, +=, -=, *=, /=  (low precedence)
  }
  //////////////////////////////////////////
  //////// Boolean operations ////////////
  //////////////////////////////////////////
  opr:=typBool.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typBool, @ROB_bool_asig_bool);
  opr:=typBool.CreateUnaryPreOperator('NOT', 6, 'not', @ROU_not_bool);
  opr:=typBool.CreateBinaryOperator('AND',5,'and');
  opr.CreateOperation(typBool,@ROB_bool_and_bool);
  opr:=typBool.CreateBinaryOperator('=',3,'equal');
  opr.CreateOperation(typBool,@ROB_bool_equal_bool);
  opr:=typBool.CreateBinaryOperator('<>',3,'difer');
  opr.CreateOperation(typBool,@ROB_bool_difer_bool);

  //////////////////////////////////////////
  //////// Operaciones con Byte ////////////
  //////////////////////////////////////////
  {Los operadores deben crearse con su precedencia correcta}
  opr:=typByte.CreateUnaryPreOperator('@', 6, 'addr', @ROU_address);
  opr:=typByte.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typByte,@ROB_byte_asig_byte);
  opr:=typByte.CreateBinaryOperator('+=',2,'aadd');  //asignación-suma
  opr.CreateOperation(typByte,@ROB_byte_aadd_byte);
  opr:=typByte.CreateBinaryOperator('-=',2,'asub');  //asignación-resta
  opr.CreateOperation(typByte,@ROB_byte_asub_byte);

  opr:=typByte.CreateBinaryOperator('+',4,'add');  //add
  opr.CreateOperation(typByte,@ROB_byte_add_byte);
  opr.CreateOperation(typWord,@ROB_byte_add_word);
  opr:=typByte.CreateBinaryOperator('-',4,'subs');  //sub
  opr.CreateOperation(typByte,@ROB_byte_sub_byte);
  opr:=typByte.CreateBinaryOperator('*',5,'subs');  //sub
  opr.CreateOperation(typByte,@ROB_byte_mul_byte);

  opr:=typByte.CreateBinaryOperator('AND',5,'and');
  opr.CreateOperation(typByte,@ROB_byte_and_byte);
  opr:=typByte.CreateBinaryOperator('OR',4,'or');
  opr.CreateOperation(typByte,@ROB_byte_or_byte);
  opr:=typByte.CreateBinaryOperator('XOR',4,'xor');
  opr.CreateOperation(typByte,@ROB_byte_xor_byte);

  opr:=typByte.CreateUnaryPreOperator('NOT', 6, 'not', @ROU_not_byte);

  opr:=typByte.CreateBinaryOperator('=',3,'equal');
  opr.CreateOperation(typByte,@ROB_byte_equal_byte);
  opr:=typByte.CreateBinaryOperator('<>',3,'difer');
  opr.CreateOperation(typByte,@ROB_byte_difer_byte);

  opr:=typByte.CreateBinaryOperator('>',3,'great');
  opr.CreateOperation(typByte,@ROB_byte_great_byte);
  opr:=typByte.CreateBinaryOperator('<',3,'less');
  opr.CreateOperation(typByte,@ROB_byte_less_byte);

  opr:=typByte.CreateBinaryOperator('>=',3,'gequ');
  opr.CreateOperation(typByte,@ROB_byte_gequ_byte);
  opr:=typByte.CreateBinaryOperator('<=',3,'lequ');
  opr.CreateOperation(typByte,@ROB_byte_lequ_byte);

  opr:=typByte.CreateBinaryOperator('>>',5,'shr');  { TODO : Definir bien la precedencia }
  opr.CreateOperation(typByte,@ROB_byte_shr_byte);
  opr:=typByte.CreateBinaryOperator('<<',5,'shl');
  opr.CreateOperation(typByte,@ROB_byte_shl_byte);
  //////////////////////////////////////////
  //////// Operaciones con Char ////////////
  {Los operadores deben crearse con su precedencia correcta}
  opr:=typChar.CreateUnaryPreOperator('@', 6, 'addr', @ROU_address);
  opr:=typChar.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typChar,@ROB_char_asig_char);
//  opr.CreateOperation(typString, @ROB_char_asig_string);

  opr:=typChar.CreateBinaryOperator('=',3,'equal');  //asignación
  opr.CreateOperation(typChar,@ROB_char_equal_char);
  opr:=typChar.CreateBinaryOperator('<>',3,'difer');  //asignación
  opr.CreateOperation(typChar,@ROB_char_difer_char);

  //////////////////////////////////////////
  //////// Operaciones con Word ////////////
  {Los operadores deben crearse con su precedencia correcta}

  opr:=typWord.CreateUnaryPreOperator('@', 6, 'addr', @ROU_address);
  opr:=typWord.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typWord,@ROB_word_asig_word);
  opr.CreateOperation(typByte,@ROB_word_asig_byte);

  opr:=typWord.CreateBinaryOperator('=',3,'equal');  //igualdad
  opr.CreateOperation(typWord,@ROB_word_equal_word);
  opr:=typWord.CreateBinaryOperator('<>',3,'difer');
  opr.CreateOperation(typWord,@ROB_word_difer_word);

  opr:=typWord.CreateBinaryOperator('AND', 5, 'and');  //AND
  opr.CreateOperation(typByte, @ROB_word_and_byte);
  opr:=typWord.CreateBinaryOperator('+',4,'add');  //add
  opr.CreateOperation(typByte, @ROB_word_add_byte);
  opr.CreateOperation(typWord, @ROB_word_add_word);
  opr:=typWord.CreateBinaryOperator('-',4,'sub');  //sub
  opr.CreateOperation(typByte, @ROB_word_sub_byte);
  opr.CreateOperation(typWord, @ROB_word_sub_word);
  opr:=typWord.CreateBinaryOperator('+=',2,'aadd');  //asignación-suma
  opr.CreateOperation(typByte,@ROB_word_aadd_byte);
  opr:=typWord.CreateBinaryOperator('-=',2,'asub');  //asignación-resta
  opr.CreateOperation(typByte,@ROB_word_asub_byte);

  opr:=typWord.CreateBinaryOperator('>>',5,'shr');  { TODO : Definir bien la precedencia }
  opr.CreateOperation(typByte,@ROB_word_shr_byte);
  opr:=typWord.CreateBinaryOperator('<<',5,'shl');
  opr.CreateOperation(typByte,@ROB_word_shl_byte);


  //////// Operaciones con String ////////////
//  opr:=typString.CreateUnaryPreOperator('@', 6, 'addr', @ROU_address);
//  opr:=typString.CreateBinaryOperator('+',4,'add');  //add
//  opr.CreateOperation(typString,@ROB_string_add_string);
//  opr.CreateOperation(typChar,@ROB_string_add_char);

end;
procedure TGenCod.CreateSystemElements;
{Initialize the system elements. Must be executed just one time when compiling.}
  procedure AddParam(var pars: TxpParFuncArray; parName: string; const srcPos: TSrcPos;
                     typ0: TxpEleType; adicDec: TxpAdicDeclar);
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
  function CreateSystemFunction(name: string; retType: TxpEleType; const srcPos: TSrcPos;
                                const pars: TxpParFuncArray;
                                compile: TProcExecFunction): TxpEleFun;
  {Create a new system fucntion in the current element of the Syntax Tree.
   Returns the reference to the function created.}
  var
     fundec: TxpEleFunDec;
     bod: TxpEleBody;
  begin
    //Add declaration
    curLocation := locInterface;
    fundec := AddFunctionDEC(name, retType, srcPos, pars, false);
    //Implementation
    {Note that implementation is added always after declarartion. It's not the usual
    in common units, where all declarations are first}
    curLocation := locImplement;
    Result := AddFunctionIMP(name, retType, srcPos, fundec);
    //Here varaibles can be added
    {Create a body, to be uniform with normal function and for have a space where
    compile code and access to posible variables or other elements.}
    bod := CreateBody;
    bod.srcDec := srcPos;
    TreeElems.AddElement(bod);  //Add Body (not need to open)
    Result.compile := compile;  //Set routine to geenrate code
    TreeElems.CloseElement;  //Close function implementation
  end;

var
  f: TxpEleFun;
  uni: TxpEleUnit;
  pars: TxpParFuncArray;  //Array of parameters
  srcPos: TSrcPos;
begin
  listFunSys.Clear;
  //////// Funciones del sistema ////////////
  {Notar que las funciones del sistema no crean espacios de nombres.}
//  f := CreateSysFunction('delay_ms', nil, @fun_delay_ms);
//  f.adrr:=$0;
//  f.compile := @codif_delay_ms;  //rutina de compilación
  //Funciones INLINE
  f := CreateSysFunction('exit'     , nil, @fun_Exit);
  f := CreateSysFunction('Inc'      , nil, @fun_Inc);
  f := CreateSysFunction('Dec'      , nil, @fun_Dec);
  f := CreateSysFunction('SetOrig'  , nil, @SetOrig);
  f := CreateSysFunction('Ord'      , @FunctParam, @fun_Ord);
  f := CreateSysFunction('Chr'      , @FunctParam, @fun_Chr);
  f := CreateSysFunction('Byte'     , @FunctParam, @fun_Byte);
  f := CreateSysFunction('Word'     , @FunctParam, @fun_Word);
  f := CreateSysFunction('addr'     , @FunctParam, @fun_Addr);
  //Implement calls to Code Generator
  callDefineArray  := @DefineArray;
  callDefinePointer:= @DefinePointer;
  callValidRAMaddr := @ValidRAMaddr;
  callStartProgram := @Cod_StartProgram;
  callEndProgram   := @Cod_EndProgram;
  //Create "System" Unit. Must be done once in First Pass
  {Originally system functions were created in a special list and has a special treatment
  but it implied a lot of work for manage the memory, linking, use of variables, and
  optimization. Now we create a "system unit" like a real unit (more less) and we create
  the system fucntion here, so we use the same code for linking, calling and optimization
  that we use in common fucntions. Moreover, we can create private functions.}
  uni := CreateUnit('-');
  TreeElems.AddElementAndOpen(uni);  //Open Unit
  //Create a fictional position
  srcPos.fil := '';
  srcPos.row := 1;
  srcPos.col := 1;
  {Create variables for aditional Working register. Note that this variables are
  accesible (and usable) from the code, because the name assigned is a common variable.}
  //Create register H as variable
  H := AddVariable('__H', typByte, srcPos);
  H.adicPar.hasAdic := decNone;
  H.adicPar.hasInit := false;
  H.location := locInterface;  //make visible
  //Create register E as variable
  E := AddVariable('__E', typByte, srcPos);
  E.adicPar.hasAdic := decNone;
  E.adicPar.hasInit := false;
  E.location := locInterface;  //make visible
  //Create register U as variable
  U := AddVariable('__U', typByte, srcPos);
  U.adicPar.hasAdic := decNone;
  U.adicPar.hasInit := false;
  U.location := locInterface;  //make visible
  //Create register IX as variable
  IX := AddVariable('__IX', typWord, srcPos);
  IX.adicPar.hasAdic := decNone;
  IX.adicPar.hasInit := false;
  IX.location := locInterface;  //make visible

  //Create system function "delay_ms"
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'ms', srcPos, typWord, decRegis);  //Add parameter
  f := CreateSystemFunction('delay_ms', typNull, srcPos,  pars, @codif_delay_ms);
  AddCallerTo(H, f.BodyNode); {Adds this dependency because, like this is a system function,
                           won`t be compiled in the First pass, that is when references are created.
                           As we need to use W. We declare a call.}

  //Multiply system function
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'A', srcPos, typByte, decNone);  //Add parameter
  AddParam(pars, 'B', srcPos, typByte, decNone);  //Add parameter
  f_byte_mul_byte_16 := CreateSystemFunction('byte_mul_byte_16', typWord, srcPos, pars, @byte_mul_byte_16);
  AddCallerTo(H, f_byte_mul_byte_16.BodyNode);  //We'll neede to return result.

  //Word shift left
  setlength(pars, 0);  //Reset parameters
  AddParam(pars, 'n', srcPos, typByte, decRegisX);   //Parameter counter shift
  f_word_shift_l := CreateSystemFunction('word_shift_l', typWord, srcPos, pars, @word_shift_l);
  AddCallerTo(H, f_word_shift_l.BodyNode);  //We'll neede to return result.

  TreeElems.CloseElement; //Close Unit
end;
end.

