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
  Parser, Globales, MisUtils, XpresBas;
type
    { TGenCod }
    TGenCod = class(TGenCodBas)
    protected
      procedure callParam(fun: TxpEleFun);
      procedure callFunct(fun: TxpEleFun);
    private
//      f_byteXbyte_byte: TxpEleFun;  //índice para función
      f_byte_mul_byte_16: TxpEleFun;  //índice para función
      f_word_mul_word_16: TxpEleFun;  //índice para función
      procedure mul_byte_16(fun: TxpEleFun);
      procedure Copy_Z_to_A;
      procedure Copy_C_to_A;
      procedure fun_Byte(fun: TxpEleFun);
      procedure ROU_addr_word(Opr: TxpOperator; SetRes: boolean);
      procedure ROU_not_byte(Opr: TxpOperator; SetRes: boolean);
      procedure ROU_addr_byte(Opr: TxpOperator; SetRes: boolean);

      procedure ROB_word_and_byte(Opt: TxpOperation; SetRes: boolean);
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
      procedure fun_delay_ms(fun: TxpEleFun);
      procedure fun_Exit(fun: TxpEleFun);
      procedure fun_Inc(fun: TxpEleFun);
      procedure fun_Dec(fun: TxpEleFun);
      procedure SetOrig(fun: TxpEleFun);
      procedure fun_Ord(fun: TxpEleFun);
      procedure fun_Chr(fun: TxpEleFun);
      procedure fun_Word(fun: TxpEleFun);
    protected
      procedure StartCodeSub(fun: TxpEleFun);
      procedure EndCodeSub;
      procedure Cod_StartProgram;
      procedure Cod_EndProgram;
      procedure CreateSystemElements;
    public
      procedure StartSyntax;
      procedure DefCompiler;
      procedure DefPointerArithmetic(etyp: TxpEleType);
    end;

  procedure SetLanguage;
implementation
var
  MSG_NOT_IMPLEM: string;
  MSG_INVAL_PARTYP: string;
  MSG_UNSUPPORTED : string;
  MSG_CANNOT_COMPL: string;

procedure SetLanguage;
begin
  GenCodBas_PIC16.SetLanguage;
  {$I ..\language\tra_GenCod.pas}
end;
procedure TGenCod.StartCodeSub(fun: TxpEleFun);
{debe ser llamado para iniciar la codificación de una subrutina}
begin
//  iFlashTmp :=  pic.iFlash; //guarda puntero
//  pic.iFlash := curBloSub;  //empieza a codificar aquí
end;
procedure TGenCod.EndCodeSub;
{debe ser llamado al terminar la codificaión de una subrutina}
begin
//  curBloSub := pic.iFlash;  //indica siguiente posición libre
//  pic.iFlash := iFlashTmp;  //retorna puntero
end;
procedure TGenCod.callParam(fun: TxpEleFun);
{Rutina genérica, que se usa antes de leer los parámetros de una función.}
begin
  {Haya o no, parámetros se debe proceder como en cualquier expresión, asumiendo que
  vamos a devolver una expresión.}
  SetResultExpres(fun.typ);  //actualiza "RTstate"
end;
procedure TGenCod.callFunct(fun: TxpEleFun);
{Rutina genérica para llamar a una función definida por el usuario.}
begin
  //Por ahora, no se implementa paginación, pero despuñes habría que considerarlo.
  _JSR(fun.adrr);  //codifica el salto
end;
procedure TGenCod.Copy_Z_to_A;
begin
  //El resultado está en Z y lo mueve a A
  _LDA(0);
  _BNE(_PC+2);
  _LDA(1);
  BooleanFromZ := true;  //Activa bandera para que se pueda optimizar
end;
procedure TGenCod.Copy_C_to_A;
begin
  //El resultado está en C y lo mueve a A
  _LDA(0);
  _BCC(_PC+2);
  _LDA(1);
  BooleanFromC := true;  //Activa bandera para que se pueda optimizar
end;
////////////rutinas obligatorias
procedure TGenCod.Cod_StartProgram;
//Codifica la parte inicial del programa
begin
  //Code('.CODE');   //inicia la sección de código
end;
procedure TGenCod.Cod_EndProgram;
//Codifica la parte inicial del programa
begin
  //Code('END');   //inicia la sección de código
end;
procedure TGenCod.expr_start;
//Se ejecuta siempre al iniciar el procesamiento de una expresión.
begin
  //Inicia banderas de estado para empezar a calcular una expresión
  A.used := false;        //Su ciclo de vida es de instrucción
  Z.used := false;        //Su ciclo de vida es de instrucción
  if H<>nil then
    H.used := false;      //Su ciclo de vida es de instrucción
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
    {Actualmente no existen constantes de tipo "Bit", pero si existieran, sería así}
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
procedure TGenCod.ROU_addr_byte(Opr: TxpOperator; SetRes: boolean);
{Devuelve la dirección de una variable.}
begin
  case p1^.Sto of
  stConst : begin
    genError('Cannot obtain address of constant.');
  end;
  stVariab: begin
    //Es una variable normal
    //La dirección de una variable es constante
    SetResultConst(typByte);
    //No se usa p1^.offs, porque solo retorna 7 bits;
    res.valInt := p1^.rVar.addr and $ff;
  end;
  stExpres: begin  //ya está en STATUS.Z
    genError('Cannot obtain address of an expression.');
  end;
  else
    genError('Cannot obtain address of this operand.');
  end;
end;
////////////operaciones con Byte
procedure TGenCod.ROB_byte_and_byte(Opt: TxpOperation; SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;

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
    _AND(value1);
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
    _AND(value1);
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
    _LDA(value2);
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
    _AND(value2)
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _AND(byte2);
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //p1 está en la pila y p2 en el acumulador
    rVar := GetVarByteFromStk;
    _AND(rVar.adrByte0);
    FreeStkRegisterByte;   //libera pila porque ya se uso
  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_or_byte(Opt: TxpOperation; SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;

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
    _LDA(value1);
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
    _LDA(value2);
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
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //p1 está en la pila y p2 en el acumulador
    rVar := GetVarByteFromStk;
    _ORA(rVar.adrByte0);
    FreeStkRegisterByte;   //libera pila porque ya se uso
  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_xor_byte(Opt: TxpOperation; SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;

  case stoOperation of
  stConst_Const: begin  //suma de dos constantes. Caso especial
    SetROBResultConst_byte(value1 xor value2);  //puede generar error
  end;
  stConst_Variab: begin
    SetROBResultExpres_byte(Opt);
    _LDA(value1);
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
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //p1 está en la pila y p2 en el acumulador
    rVar := GetVarByteFromStk;
    _EOR(rVar.adrByte0);
    FreeStkRegisterByte;   //libera pila porque ya se uso
  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_equal_byte(Opt: TxpOperation; SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stConst_Const: begin  //compara constantes. Caso especial
    SetROBResultConst_bool(value1 = value2);
  end;
  stConst_Variab: begin
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    if value1 = 0 then begin  //caso especial
      _EORi(0);  //si iguales _Z=1
    end else if value1 = 1 then begin  //caso especial
      _DEC(byte2);  //si el resultado es cero _Z=1
    end else if value1 = 255 then begin  //caso especial
      _INC(byte2);  //si el resultado es cero _Z=1
    end else begin
      _LDA(value1);
      _EOR(byte2);
    end;
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    _EORi(value1);  //Si son iguales Z=1.
  end;
  stVariab_Const: begin
    ExchangeP1_P2;  //Convierte a stConst_Variab
    ROB_byte_equal_byte(Opt, SetRes);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    _LDA(byte1);
    _EOR(byte2);  //si iguales _Z=1
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    _EOR(byte1);  //si iguales _Z=1
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    _EORi(value2);  //Si son iguales Z=1.
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    _EOR(byte2);  //Si son iguales Z=1.
  end;
  stExpres_Expres:begin
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    //la expresión p1 debe estar salvada y p2 en el acumulador
    rVar := GetVarByteFromStk;
    _EOR(rVar.adrByte0);  //Si son iguales Z=1.
    FreeStkRegisterByte;   //libera pila porque se usará el dato ahí contenido
  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_difer_byte(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_byte_equal_byte(Opt, SetRes);  //usa el mismo código
  res.Invert;  //Invierte la lógica
end;
procedure TGenCod.ROB_byte_asig_byte(Opt: TxpOperation; SetRes: boolean);
var
  aux: TPicRegister;
  rVar: TxpEleVar;
begin
  //Simplifcamos el caso en que p2, sea de tipo p2^
  if not ChangePointerToExpres(p2^) then exit;
  //Realiza la asignación
  if p1^.Sto = stVariab then begin
    SetResultNull;  //Fomalmente,  una aisgnación no devuelve valores en Pascal
    //Asignación a una variable
    case p2^.Sto of
    stConst : begin
      _LDA(value2);
      _STA(byte1);
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
  end else if p1^.Sto = stVarRefExp then begin
    //{Este es un caso especial de asignación a un puntero a byte dereferenciado, pero
    //cuando el valor del puntero es una expresión. Algo así como (ptr + 1)^}
    //SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
    //case p2^.Sto of
    //stConst : begin
    //  kMOVWF(FSR);  //direcciona
    //  //Asignación normal
    //  if value2=0 then begin
    //    //caso especial
    //    kCLRF(INDF);
    //  end else begin
    //    kMOVWF(INDF);
    //  end;
    //end;
    //stVariab: begin
    //  kMOVWF(FSR);  //direcciona
    //  //Asignación normal
    //  kMOVF(byte2, toW);
    //  kMOVWF(INDF);
    //end;
    //stExpres: begin
    //  //La dirección está en la pila y la expresión en A
    //  aux := GetAuxRegisterByte;
    //  kMOVWF(aux);   //Salva A (p2)
    //  //Apunta con p1
    //  rVar := GetVarByteFromStk;
    //  kMOVF(rVar.adrByte0, toW);  //Opera directamente al dato que había en la pila. Deja en A
    //  kMOVWF(FSR);  //direcciona
    //  //Asignación normal
    //  kMOVF(aux, toW);
    //  kMOVWF(INDF);
    //  aux.used := false;
    //  exit;
    //end;
    //else
    //  GenError(MSG_UNSUPPORTED); exit;
    //end;
  end else if p1^.Sto = stVarRefVar then begin
    ////Asignación a una variable
    //SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
    //case p2^.Sto of
    //stConst : begin
    //  //Caso especial de asignación a puntero desreferenciado: variable^
    //  kMOVF(byte1, toW);
    //  kMOVWF(FSR);  //direcciona
    //  //Asignación normal
    //  if value2=0 then begin
    //    //caso especial
    //    kCLRF(INDF);
    //  end else begin
    //    kMOVWF(INDF);
    //  end;
    //end;
    //stVariab: begin
    //  //Caso especial de asignación a puntero derefrrenciado: variable^
    //  kMOVF(byte1, toW);
    //  kMOVWF(FSR);  //direcciona
    //  //Asignación normal
    //  kMOVF(byte2, toW);
    //  kMOVWF(INDF);
    //end;
    //stExpres: begin  //ya está en A
    //  //Caso especial de asignación a puntero derefrrenciado: variable^
    //  aux := GetAuxRegisterByte;
    //  kMOVWF(aux);   //Salva A (p2)
    //  //Apunta con p1
    //  kMOVF(byte1, toW);
    //  kMOVWF(FSR);  //direcciona
    //  //Asignación normal
    //  kMOVF(aux, toW);
    //  kMOVWF(INDF);
    //  aux.used := false;
    //end;
    //else
    //  GenError(MSG_UNSUPPORTED); exit;
    //end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.ROB_byte_aadd_byte(Opt: TxpOperation; SetRes: boolean);
{Operación de asignación suma: +=}
var
  aux: TPicRegister;
  rVar: TxpEleVar;
begin
  //Simplifcamos el caso en que p2, sea de tipo p2^
  if not ChangePointerToExpres(p2^) then exit;
  //Caso especial de asignación
  if p1^.Sto = stVariab then begin
    SetResultNull;  //Fomalmente,  una aisgnación no devuelve valores en Pascal
    //Asignación a una variable
    case p2^.Sto of
    stConst : begin
      if value2=0 then begin
        //Caso especial. No hace nada
      end else begin
        _CLC;
        _LDA(byte1);
        _ADC(value2);
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
  end else if p1^.Sto = stVarRefExp then begin
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
  end else if p1^.Sto = stVarRefVar then begin
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
var
  aux: TPicRegister;
  rVar: TxpEleVar;
begin
  //Simplifcamos el caso en que p2, sea de tipo p2^
  if not ChangePointerToExpres(p2^) then exit;
  //Caso especial de asignación
  if p1^.Sto = stVariab then begin
    SetResultNull;  //Fomalmente,  una aisgnación no devuelve valores en Pascal
    //Asignación a una variable
    case p2^.Sto of
    stConst : begin
      if value2=0 then begin
        //Caso especial. No hace nada
      end else begin
        _SEC;
        _LDA(byte1);
        _SBC(value2);
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
      _ADC(1);
      //Devuelve
      _STA(byte1);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else if p1^.Sto = stVarRefExp then begin
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
  end else if p1^.Sto = stVarRefVar then begin
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
var
  rVar: TxpEleVar;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
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
      _INC(byte2);
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    _CLC;
    _LDA(value1);
    _ADC(byte2);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _CLC;
    _ADC(value1);
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
    _ADC(value2);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _CLC;
    _ADC(byte2);
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //La expresión p1 debe estar salvada y p2 en el acumulador
    rVar := GetVarByteFromStk;
    _CLC;
    _ADC(rVar.adrByte0);  //opera directamente al dato que había en la pila. Deja en A
    FreeStkRegisterByte;   //libera pila porque ya se uso
  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_sub_byte(Opt: TxpOperation; SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stConst_Const:begin  //suma de dos constantes. Caso especial
    SetROBResultConst_byte(value1-value2);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  stConst_Variab: begin
    SetROBResultExpres_byte(Opt);
    _SEC;
    _LDA(value1);
    _SBC(byte2);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    typWord.DefineRegister;   //Asegura que exista H
    _STA(H);
    _SEC;
    _LDA(value1);
    _SBC(H);
  end;
  stVariab_Const: begin
    SetROBResultExpres_byte(Opt);
    _SEC;
    _LDA(byte1);
    _SBC(value2);
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
    _ADC(1);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _SEC;
    _SBC(value2);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);
    _SEC;
    _SBC(byte2);
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //la expresión p1 debe estar salvada y p2 en el acumulador
    rVar := GetVarByteFromStk;
    _SEC;
    _SBC(rVar.adrByte0);
    FreeStkRegisterByte;   //libera pila porque ya se uso
  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.mul_byte_16(fun: TxpEleFun);
//E * A -> [H:A]  Usa registros: A,H,E,U
//Basado en código de Andrew Warren http://www.piclist.com
var
  LOOP: Word;
begin
//    typDWord.DefineRegister;   //Asegura que exista A,H,E,U
//    _CLRF (H.offs);
//    _CLRF (U.offs);
//    _BSF  (U.offs,3);  //8->U
//    _RRF  (E.offs,toF);
//LOOP:=_PC;
//    _BTFSC (_STATUS,0);
//    _ADDWF (H.offs,toF);
//    _RRF   (H.offs,toF);
//    _RRF   (E.offs,toF);
//    _DECFSZ(U.offs, toF);
//    _JMP  (LOOP);
//    //Realmente el algortimo es: E*A -> [H:E], pero lo convertimos a: E*A -> [H:A]
//    _MOVF(E.offs, toW);
//    _RTS;
end;
procedure TGenCod.ROB_byte_great_byte(Opt: TxpOperation; SetRes: boolean);
var
  tmp: TPicRegister;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
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
      SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
      _SEC;
      _LDA(value1);
      _SBC(byte2);
      Copy_C_to_A; //Pasa C a Z (invirtiendo)
    end;
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
    if value1 = 0 then begin
      //0 es mayor que nada
      SetROBResultConst_byte(0);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      //Optimiza rutina, usando: A>B  equiv. NOT (B<=A-1)
      //Se necesita asegurar que p1, es mayo que cero.
      SetROBResultExpres_byte(Opt);  //invierte la lógica
      typWord.DefineRegister;   //Asegura que exista H
      //p2, ya está en A
      _STA(H);
      _SEC;
      _LDA(value1);
      _SBC(H);
      Copy_C_to_A; //Pasa C a Z (invirtiendo)
    end;
  end;
  stVariab_Const: begin
    if value2 = 255 then begin
      //Nada es mayor que 255
      SetROBResultConst_byte(0);
      GenWarn('Expression will always be FALSE or TRUE.');
    end else begin
      SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
      _SEC;
      _LDA(byte1);
      _SBC(value2);
      Copy_C_to_A; //Pasa C a Z (invirtiendo)
    end;
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    _SEC;
    _LDA(byte1);
    _SBC(byte2);
    Copy_C_to_A; //Pasa C a Z (invirtiendo)
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    tmp := GetAuxRegisterByte;  //Se pide registro auxiliar
    _STA(tmp);
    _SEC;
    _LDA(byte1);
    _SBC(tmp);
    Copy_C_to_A; //Pasa C a Z (invirtiendo)
    tmp.used := false;  //libera
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en A
    if value2 = 255 then begin
      //Nada es mayor que 255
      SetROBResultConst_byte(0);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
      _SEC;
      _SBC(value2);
      Copy_C_to_A; //Pasa C a Z (invirtiendo)
    end;
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    _SEC;
    _SBC(byte2);
    Copy_C_to_A; //Pasa C a Z (invirtiendo)
  end;
  stExpres_Expres:begin
    //la expresión p1 debe estar salvada y p2 en el acumulador
    p1^.SetAsVariab(GetVarByteFromStk);  //Convierte a variable
    //Luego el caso es similar a stVariab_Expres
    ROB_byte_great_byte(Opt, true);
    FreeStkRegisterByte;   //libera pila porque ya se usó el dato ahí contenido
  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_less_byte(Opt: TxpOperation; SetRes: boolean);
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  //A < B es lo mismo que B > A
  case stoOperation of
  stExpres_Expres:begin
    {Este es el único caso que no se puede invertir, por la posición de los operandos en
     la pila.}
    //la expresión p1 debe estar salvada y p2 en el acumulador
    p1^.SetAsVariab(GetVarByteFromStk);  //Convierte a variable
    //Luego el caso es similar a stVariab_Expres
    ROB_byte_less_byte(Opt, SetRes);
    FreeStkRegisterByte;   //libera pila porque ya se usó el dato ahí contenido
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
  aux: TPicRegister;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stConst_Const: begin  //compara constantes. Caso especial
    SetROBResultConst_byte(value1 >> value2);
  end;
//  stConst_Variab: begin
//  end;
//  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
//  end;
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
      _LDX(value2);
//loop1:
      _LSRa;
      _DEX;
      _BNE(-4);  //loop1
    end;
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    _LDA(byte1);
    _LDX(byte2);
//loop1:
    _LSRa;
    _DEX;
    _BNE(-4);  //loop1
  end;
//  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
//  end;
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
      _LDX(value2);
  //loop1:
      _LSRa;
      _DEX;
      _BNE(-4);  //loop1
    end;
  end;
//  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
//  end;
//  stExpres_Expres:begin
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_shl_byte(Opt: TxpOperation; SetRes: boolean);   //Desplaza a la izquierda
var
  aux: TPicRegister;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stConst_Const: begin  //compara constantes. Caso especial
    SetROBResultConst_byte(value1 << value2);
  end;
//  stConst_Variab: begin
//  end;
//  stConst_Expres: begin  //la expresión p2 se evaluó y esta en A
//  end;
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
      _LDX(value2);
//loop1:
      _ASLa;
      _DEX;
      _BNE(-4);  //loop1
    end;
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    _LDA(byte1);
    _LDX(byte2);
//loop1:
    _ASLa;
    _DEX;
    _BNE(-4);  //loop1
  end;
//  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
//  end;
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
      _LDX(value2);
  //loop1:
      _ASLa;
      _DEX;
      _BNE(-4);  //loop1
    end;
  end;
//  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en A
//  end;
//  stExpres_Expres:begin
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
////////////operaciones con Boolean
procedure TGenCod.ROB_bool_asig_bool(Opt: TxpOperation; SetRes: boolean);
var
  aux: TPicRegister;
  rVar: TxpEleVar;
begin
  //Simplifcamos el caso en que p2, sea de tipo p2^
  if not ChangePointerToExpres(p2^) then exit;
  //Realiza la asignación
  if p1^.Sto = stVariab then begin
    SetResultNull;  //Fomalmente, una asignación no devuelve valores en Pascal
    //Asignación a una variable
    case p2^.Sto of
    stConst : begin
      if p2^.valBool then begin
        _LDA(1);
        _STA(byte1);
      end else begin
        _LDA(1);
        _STA(byte1);
      end;
    end;
    stVariab: begin
      _LDA(byte2);
      _STA(byte1);
    end;
//    stExpres: begin  //ya está en Z
//      _LDA(0);
//
//      _STA(byte1);
//    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
////////////operaciones con Word
procedure TGenCod.ROB_word_asig_word(Opt: TxpOperation; SetRes: boolean);
var
  aux: TPicRegister;
begin
  //Simplifcamos el caso en que p2, sea de tipo p2^
  if not ChangePointerToExpres(p2^) then exit;
  //Realiza la asignación
  if p1^.Sto = stVariab then begin
    case p2^.Sto of
    stConst : begin
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
      _LDA(value2L);
      _STA(byte1L);
      _LDA(value2H);
      _STA(byte1H);
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
      _LDA(H);
      _STA(byte1H);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else if p1^.Sto = stVarRefVar then begin
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
begin
  if p1^.Sto = stVariab then begin
    case p2^.Sto of
    stConst : begin
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
      _LDA(value2L);
      _STA(byte1L);
      _LDA(0);
      _STA(byte1H);
    end;
    stVariab: begin
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
      _LDA(byte2L);
      _STA(byte1L);
      _LDA(0);
      _STA(byte1H);
    end;
    stExpres: begin   //se asume que está en A
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
      _STA(byte1L);
      _LDA(0);
      _STA(byte1H);
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
  tmp: TPicRegister;
  sale: integer;
begin
//  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
//    GenError('Too complex pointer expression.'); exit;
//  end;
//  if not ChangePointerToExpres(p1^) then exit;
//  if not ChangePointerToExpres(p2^) then exit;
//  case stoOperation of
//  stConst_Const: begin  //compara constantes. Caso especial
//    SetROBResultConst_byte(ORD(value1 = value2));
//  end;
//  stConst_Variab: begin
//    SetROBResultExpres_bool(Opt, true);  //El res. estará en A invertido
//    ////////// Compara byte alto
//    if value1H = 0 then begin  //caso especial
//      _LDA(value2H);
//      _EOR(byte2);
//
//      kMOVF(byte2H, toW); //p2-p1
//      _BTFSS(Z.offs, Z.bit);
//      _JMP_lbl(sale);  //no son iguales
//    end else if value1H = 1 then begin  //caso especial
//      kDECF(byte2H, toW); //p2-p1
//      _BTFSS(Z.offs, Z.bit);
//      {De no ser porque se tiene que devolver siempre, el valor de Z,
//      las 2 instrucciones anteriores, se podrían reemplazar con un i_DECFSZ,
//      pero i_DECFSZ, no actualiza Z}
//      _JMP_lbl(sale);  //no son iguales
//    end else if value1H = 255 then begin  //caso especial
//      kINCF(byte2H, toW); //p2-p1
//      _BTFSS(Z.offs, Z.bit);
//      {De no ser porque se tiene que devolver siempre, el valor de Z,
//      las 2 instrucciones anteriores, se podrían reemplazar con un i_DECFSZ,
//      pero i_DECFSZ, no actualiza Z}
//      _JMP_lbl(sale);  //no son iguales
//    end else begin  //caso general
//      _LDA(value1H);
//      _EOR(byte2H); //p2-p1
//      _BNE_lbl(sale);  //no son iguales, sale con Z=0, A<>0
//    end;
//    //////////  Son iguales, comparar el byte bajo
//    if value1L = 0 then begin  //caso especial
//      kMOVF(byte2L,toW);	//p2-p1
//  _LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
//    end else if value1L = 1 then begin  //caso especial
//      kDECF(byte2L,toW);	//p2-p1
//  _LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
//    end else if value1L = 255 then begin  //caso especial
//      kINCF(byte2L,toW);	//p2-p1
//  _LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
//    end else begin
//      _LDA(value1L);
//      _EOR(byte2L);	//p2-p1
//      //Si p1=p2 -> Z=1. Si no son iguales -> Z=0, A<>0.
//  _LABEL(sale);
//    end;
//  end;
//  stConst_Expres: begin  //la expresión p2 se evaluó p2 esta en A
//    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
//    tmp := GetAuxRegisterByte;
//    if HayError then exit;
//    _MOVWF(tmp.offs);   //salva byte bajo de Expresión
//    //Compara byte alto
//    _SUBWF(H.offs, toW); //p2-p1
//    _BTFSS(Z.offs, Z.bit);
//    _JMP_lbl(sale);  //no son iguales
//    //Son iguales, comparar el byte bajo
//    _SUBWF(tmp.offs,toW);	//p2-p1
//_LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
//    tmp.used := false;
//  end;
//  stVariab_Const: begin
//    ExchangeP1_P2;  //Convierte a stConst_Variab
//    ROB_word_equal_word(Opt, SetRes);
//  end;
//  stVariab_Variab:begin
//    SetROBResultExpres_bool(Opt, true);
//    //Compara byte alto
//    _LDA(byte1H);
//    _EOR(byte2H); //p2-p1
//    _BNE_lbl(sale);  //no son iguales, sale con Z=0, A<>0
//    //Son iguales, comparar el byte bajo
//    _LDA(byte1L);
//    _EOR(byte2L);	//p2-p1
//    //Si p1=p2 -> Z=1. Si no son iguales -> Z=0, A<>0.
//_LABEL(sale);
//  end;
//  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en A
//    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
//    tmp := GetAuxRegisterByte;
//    _MOVWF(tmp.offs);   //salva byte bajo de Expresión
//    //Compara byte alto
//    kMOVF(byte1H, toW);
//    _SUBWF(H.offs, toW); //p2-p1
//    _BTFSS(Z.offs, Z.bit);
//    _JMP_lbl(sale);  //no son iguales
//    //Son iguales, comparar el byte bajo
//    kMOVF(byte1L, toW);
//    _SUBWF(tmp.offs,toW);	//p2-p1
//    tmp.used := false;
//_LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
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
//  else
//    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
//  end;
end;
procedure TGenCod.ROB_word_difer_word(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_word_equal_word(Opt, SetRes);
  res.Invert;
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
    _LDA(value1);
    _AND(byte2L);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en (A)
    SetROBResultExpres_byte(Opt);
    _AND(value1L);      //Deja en A
  end;
  stVariab_Const: begin
    SetROBResultExpres_byte(Opt);
    _LDA(byte1L);
    _AND(value2L);
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
    _AND(value2L);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,A)
    SetROBResultExpres_byte(Opt);
    _AND(byte2L);
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //p1 está salvado en pila y p2 en (A)
    p1^.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
    //Luego el caso es similar a stVariab_Expres
    _AND(byte1);
    FreeStkRegisterWord;   //libera pila
  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROU_addr_word(Opr: TxpOperator; SetRes: boolean);
{Devuelve la dirección de una variable.}
begin
  case p1^.Sto of
  stConst : begin
    genError('Cannot obtain address of constant.');
  end;
  stVariab: begin
    //Es una variable normal
    //La dirección de una variable es constante
    SetResultConst(typByte);
    //No se usa p1^.offs, porque solo retorna 7 bits;
    res.valInt := p1^.rVar.addr and $ff;
  end;
  stExpres: begin  //ya está en STATUS.Z
    genError('Cannot obtain address of an expression.');
  end;
  else
    genError('Cannot obtain address of this operand.');
  end;
end;
//////////// Operaciones con Char
procedure TGenCod.ROB_char_asig_char(Opt: TxpOperation; SetRes: boolean);
begin
  if p1^.Sto <> stVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Sto of
  stConst : begin
    SetROBResultExpres_char(Opt);  //Realmente, el resultado no es importante
    _LDA(value2);
    _STA(byte1);
  end;
  stVariab: begin
    SetROBResultExpres_char(Opt);  //Realmente, el resultado no es importante
    _LDA(byte2);
    _STA(byte1);
  end;
  stExpres: begin  //ya está en A
    SetROBResultExpres_char(Opt);  //Realmente, el resultado no es importante
    _STA(byte1);
  end;
  else
    GenError(MSG_UNSUPPORTED); exit;
  end;
end;
procedure TGenCod.ROB_char_equal_char(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_byte_equal_byte(Opt, SetRes);  //es lo mismo
end;
procedure TGenCod.ROB_char_difer_char(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_byte_difer_byte(Opt, SetRes); //es lo mismo
end;
//////////// Operaciones con punteros
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
  ROB_byte_add_byte(Opt, SetRes);
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
  ROB_byte_sub_byte(Opt, SetRes);
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
    tmpVar.addr0 := value1;  //Fija dirección de constante
    SetROUResultVariab(tmpVar);
  end;
  stVariab: begin
    //Caso común: ptrWord^
    //La desreferencia de una variable "tctPointer" es un stVarRefVar.
    SetROUResultVarRef(p1^.rVar);
  end;
  stExpres: begin
    //La expresión Esta en RT, pero es una dirección, no un valor
    SetROUResultExpRef(nil, p1^.Typ);
  end;
  else
    genError('Not implemented: "%s"', [Opr.OperationString]);
  end;
end;
///////////// Funciones del sistema
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
    _LDX(nCyc1m div 5);  //2 cycles
  //delay:
    _DEX;       //2 cycles (1 byte)
    _BNE(-3);   //3 cycles in loop, 2 cycles at end (2 bytes)
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
  typWord.DefineRegister;   //Se asegura de que se exista y lo marca como "usado".
  //aux := GetAuxRegisterByte;  //Pide un registro libre
  if HayError then exit;
  fun.adrr := pic.iRam;  {Se hace justo antes de generar código por si se crea
                          la variable _H}
  {Esta rutina recibe los milisegundos en los registros en (H,A) o en (A)
  En cualquier caso, siempre usa el registros H , el acumulador "A" y un reg. auxiliar.
  Se supone que para pasar los parámetros, ya se requirió H, así que no es necesario
  crearlo.}
  _LDX(0);     PutComm(' ;enter when parameters in (0,A)');
  _STX(H);
  fun.adrr2 := pic.iRam;  {Se hace justo antes de generar código por si se crea
                          la variable _H}
  _TAY; PutComm(';enter when parameters in (H,A)');
  //Se tiene el número en H,Y
delay:= _PC;
  _TYA;
  _BNE_lbl(LABEL1);  //label
  _LDA(H);
  _BEQ_lbl(ZERO); //NUM = $0000 (not decremented in that case)
  _DEC(H);
_LABEL(LABEL1);
  _DEY;
  codif_1mseg;   //codifica retardo 1 mseg
  if HayError then exit;
  _JMP(delay);
_LABEL(ZERO);
  _RTS();
  EndCodeSub;  //termina codificación
  //aux.used := false;  //libera registro
end;
procedure TGenCod.fun_delay_ms(fun: TxpEleFun);
begin
  if not CaptureTok('(') then exit;
  GetExpressionE(0, pexPARSY);  //captura parámetro
  if HayError then exit;   //aborta
  //Se terminó de evaluar un parámetro
  LoadToRT(res);   //Carga en registro de trabajo
  if HayError then exit;
  if res.Typ = typByte then begin
    //El parámetro byte, debe estar en A
    _JSR(fun.adrr);
  end else if res.Typ = typWord then begin
    //El parámetro word, debe estar en (H, A)
    _JSR(fun.adrr2);
  end else begin
    GenError(MSG_INVAL_PARTYP, [res.Typ.name]);
    exit;
  end;
  //Verifica fin de parámetros
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Exit(fun: TxpEleFun);
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
      GetExpressionE(0, pexPARSY);  //captura parámetro
      if HayError then exit;   //aborta
      //Verifica fin de parámetros
      if not CaptureTok(')') then exit;
      //El resultado de la expresión está en "res".
      if curFunTyp <> res.Typ then begin
        GenError('Expected a "%s" expression.', [curFunTyp.name]);
        exit;
      end;
      LoadToRT(res);  //Carga expresión en RT y genera i_RETURN o i_RETLW
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
procedure TGenCod.fun_Inc(fun: TxpEleFun);
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
      _INC(res.rVar.adrByte0);
    end else if res.Typ = typWord then begin
      _INC(res.rVar.adrByte0);
      _BNE_lbl(LABEL1);  //label
      _INC(res.rVar.adrByte1);
_LABEL(LABEL1);
    end else if res.Typ.catType = tctPointer then begin
      //Es puntero corto
      _INC(res.rVar.adrByte0);
    end else begin
      GenError(MSG_INVAL_PARTYP, [res.Typ.name]);
      exit;
    end;
  end;
//  stVarRefVar: begin
//    if (res.Typ = typByte) or (res.Typ = typChar) then begin
//      _INCF(res.offs, toF);
//    end else if res.Typ = typWord then begin
//      _INCF(res.Loffs, toF);
//      _BTFSC(STATUS, _Z);
//      _INCF(res.Hoffs, toF);
//    end else if res.Typ = typDWord then begin
//      _INCF(res.Loffs, toF);
//      _BTFSC(STATUS, _Z);
//      _INCF(res.Hoffs, toF);
//      _BTFSC(STATUS, _Z);
//      _INCF(res.Eoffs, toF);
//      _BTFSC(STATUS, _Z);
//      _INCF(res.Uoffs, toF);
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
procedure TGenCod.fun_Dec(fun: TxpEleFun);
var
  lbl1: ShortInt;
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
      _DEC(res.rVar.adrByte0);
    end else if res.Typ = typWord then begin
      _LDA(res.rVar.adrByte0);
      _BNE(lbl1);
      _DEC(res.rVar.adrByte1);
_LABEL(lbl1);
      _DEC(res.rVar.adrByte0);
    end else if res.Typ.catType = tctPointer then begin
      //Es puntero corto
      _DEC(res.rVar.adrByte0);
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
procedure TGenCod.SetOrig(fun: TxpEleFun);
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
procedure TGenCod.fun_Ord(fun: TxpEleFun);
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
      tmpVar.addr0 := res.rVar.addr0; //apunta al mismo byte
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
procedure TGenCod.fun_Chr(fun: TxpEleFun);
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
      tmpVar.addr0 := res.rVar.addr0; //apunta al mismo byte
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
procedure TGenCod.fun_Byte(fun: TxpEleFun);
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
      tmpVar.addr0 := res.rVar.addr0;  //apunta al mismo byte
      SetResultVariab(tmpVar);
    end else if res.Typ = typByte then begin
      //Es lo mismo
    end else if res.Typ = typWord then begin
      //Crea varaible que apunte al byte bajo
      tmpVar := CreateTmpVar('', typByte);   //crea variable temporal Byte
      tmpVar.addr0 := res.rVar.addr0;  //apunta al mismo byte
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
procedure TGenCod.fun_Word(fun: TxpEleFun);
var
  tmpVar: TxpEleVar;
begin
//  if not CaptureTok('(') then exit;
//  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
//  if HayError then exit;   //aborta
//  case res.Sto of  //el parámetro debe estar en "res"
//  stConst : begin
//    if res.Typ = typByte then begin
//      res.SetAsConst(typWord);  //solo cambia el tipo
//    end else if res.Typ = typChar then begin
//      res.SetAsConst(typWord);  //solo cambia el tipo
//    end else if res.Typ = typWord then begin
//      //ya es Word
//    end else if res.Typ = typDWord then begin
//      res.SetAsConst(typWord);
//      res.valInt := res.valInt and $FFFF;
//    end else begin
//      GenError('Cannot convert this constant to word.'); exit;
//    end;
//  end;
//  stVariab: begin
//    typWord.DefineRegister;
//    if res.Typ = typByte then begin
//      SetResultExpres(typWord);  //No podemos devolver variable. Pero sí expresión
//      _CLRF(H.offs);
//      _MOVF(res.offs, toW);
//    end else if res.Typ = typChar then begin
//      SetResultExpres(typWord);  //No podemos devolver variable. Pero sí expresión
//      _CLRF(H.offs);
//      _MOVF(res.offs, toW);
//    end else if res.Typ = typWord then begin
//      //ya es Word
//    end else if res.Typ = typDWord then begin
//      //Crea varaible que apunte al word bajo
//      tmpVar := CreateTmpVar('', typWord);   //crea variable temporal Word
//      tmpVar.addr0 := res.rVar.addr0; //apunta al byte L
//      tmpVar.addr1 := res.rVar.addr1; //apunta al byte H
//      SetResultVariab(tmpVar);
//    end else begin
//      GenError('Cannot convert this variable to word.'); exit;
//    end;
//  end;
//  stExpres: begin  //se asume que ya está en (A)
//    typWord.DefineRegister;
//    if res.Typ = typByte then begin
//      res.SetAsExpres(typWord);
//      //Ya está en A el byte bajo
//      _CLRF(H.offs);
//    end else if res.Typ = typChar then begin
//      res.SetAsExpres(typWord);
//      //Ya está en A el byte bajo
//      _CLRF(H.offs);
//    end else if res.Typ = typWord then begin
////      Ya es word
//    end else if res.Typ = typDWord then begin
//      res.SetAsExpres(typWord);
//      //Ya está en H,A el word bajo
//    end else begin
//      GenError('Cannot convert expression to word.'); exit;
//    end;
//  end;
//  else
//    genError('Not implemented "%s" for this operand.', [fun.name]);
//  end;
//  if not CaptureTok(')') then exit;
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
  xLex.AddIdentSpecList('exit delay_ms Inc Dec Ord Chr', tnSysFunct);
  xLex.AddIdentSpecList('SetOrig', tnSysFunct);
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
var
  opr: TxpOperator;
begin
  //Define métodos a usar
  OnExprStart := @expr_start;
  OnExprEnd := @expr_End;

  {Los operadores deben crearse con su precedencia correcta
  Precedencia de operadores en Pascal:
  6)    ~, not, signo "-"   (mayor precedencia)
  5)    *, /, div, mod, and, shl, shr, &
  4)    |, !, +, -, or, xor
  3)    =, <>, <, <=, >, >=, in
  2)    :=                  (menor precedencia)
  }
  //////////////////////////////////////////
  //////// Boolean operations ////////////
  //////////////////////////////////////////
  opr:=typBool.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typBool, @ROB_bool_asig_bool);

  //////////////////////////////////////////
  //////// Operaciones con Byte ////////////
  //////////////////////////////////////////
  {Los operadores deben crearse con su precedencia correcta}
  opr:=typByte.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typByte,@ROB_byte_asig_byte);
  opr:=typByte.CreateBinaryOperator('+=',2,'aadd');  //asignación-suma
  opr.CreateOperation(typByte,@ROB_byte_aadd_byte);
  opr:=typByte.CreateBinaryOperator('-=',2,'asub');  //asignación-resta
  opr.CreateOperation(typByte,@ROB_byte_asub_byte);

  opr:=typByte.CreateBinaryOperator('+',4,'add');  //suma
  opr.CreateOperation(typByte,@ROB_byte_add_byte);
  opr:=typByte.CreateBinaryOperator('-',4,'subs');  //suma
  opr.CreateOperation(typByte,@ROB_byte_sub_byte);

  opr:=typByte.CreateBinaryOperator('AND',5,'and');  //suma
  opr.CreateOperation(typByte,@ROB_byte_and_byte);
  opr:=typByte.CreateBinaryOperator('OR',4,'or');  //suma
  opr.CreateOperation(typByte,@ROB_byte_or_byte);
  opr:=typByte.CreateBinaryOperator('XOR',4,'xor');  //suma
  opr.CreateOperation(typByte,@ROB_byte_xor_byte);

  opr:=typByte.CreateUnaryPreOperator('NOT', 6, 'not', @ROU_not_byte);
  opr:=typByte.CreateUnaryPreOperator('@', 6, 'addr', @ROU_addr_byte);

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
  opr:=typChar.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typChar,@ROB_char_asig_char);
  opr:=typChar.CreateBinaryOperator('=',3,'equal');  //asignación
  opr.CreateOperation(typChar,@ROB_char_equal_char);
  opr:=typChar.CreateBinaryOperator('<>',3,'difer');  //asignación
  opr.CreateOperation(typChar,@ROB_char_difer_char);

  //////////////////////////////////////////
  //////// Operaciones con Word ////////////
  {Los operadores deben crearse con su precedencia correcta}

  opr:=typWord.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typWord,@ROB_word_asig_word);
  opr.CreateOperation(typByte,@ROB_word_asig_byte);

  opr:=typWord.CreateBinaryOperator('=',3,'equal');  //igualdad
  opr.CreateOperation(typWord,@ROB_word_equal_word);
  opr:=typWord.CreateBinaryOperator('<>',3,'difer');
  opr.CreateOperation(typWord,@ROB_word_difer_word);

  opr:=typWord.CreateBinaryOperator('AND', 5, 'and');  //AND
  opr.CreateOperation(typByte, @ROB_word_and_byte);

  opr:=typWord.CreateUnaryPreOperator('@', 6, 'addr', @ROU_addr_word);

end;
procedure TGenCod.DefPointerArithmetic(etyp: TxpEleType);
{Configura ls operaciones que definen la aritmética de punteros.}
var
  opr: TxpOperator;
begin
  //Asignación desde Byte y Puntero
  opr:=etyp.CreateBinaryOperator(':=',2,'asig');
  opr.CreateOperation(typByte, @ROB_byte_asig_byte);
  opr.CreateOperation(etyp   , @ROB_byte_asig_byte);
  //Agrega a los bytes, la posibilidad de ser asignados por punteros
  typByte.operAsign.CreateOperation(etyp, @ROB_byte_asig_byte);

  opr:=etyp.CreateBinaryOperator('=',3,'equal');  //asignación
  opr.CreateOperation(typByte, @ROB_byte_equal_byte);
  opr:=etyp.CreateBinaryOperator('+',4,'add');  //suma
  opr.CreateOperation(typByte, @ROB_pointer_add_byte);
  opr:=etyp.CreateBinaryOperator('-',4,'add');  //resta
  opr.CreateOperation(typByte, @ROB_pointer_sub_byte);
end;
procedure TGenCod.CreateSystemElements;
{Inicia los elementos del sistema. Se ejecuta cada vez que se compila.}
var
  f: TxpEleFun;  //índice para funciones
begin
  //////// Funciones del sistema ////////////
  {Notar que las funciones del sistema no crean espacios de nombres.}
  f := CreateSysFunction('delay_ms', nil, @fun_delay_ms);
  f.adrr:=$0;
  f.compile := @codif_delay_ms;  //rutina de compilación
  //Funciones INLINE
  f := CreateSysFunction('exit'     , nil, @fun_Exit);
  f := CreateSysFunction('Inc'      , nil, @fun_Inc);
  f := CreateSysFunction('Dec'      , nil, @fun_Dec);
  f := CreateSysFunction('SetOrig'  , nil, @SetOrig);
  f := CreateSysFunction('Ord'      , @callParam, @fun_Ord);
  f := CreateSysFunction('Chr'      , @callParam, @fun_Chr);
  f := CreateSysFunction('Byte'     , @callParam, @fun_Byte);
  f := CreateSysFunction('Word'     , @callParam, @fun_Word);
  //Funciones de sistema para operaciones aritméticas/lógicas complejas
  //Multiplicación byte por byte a word
  f_byte_mul_byte_16 := CreateSysFunction('byte_mul_byte_16', nil, nil);
  f_byte_mul_byte_16.adrr:=$0;
  f_byte_mul_byte_16.compile := @mul_byte_16;
  //Multiplicación word por word a word
  f_word_mul_word_16 := CreateSysFunction('word_mul_word_16', nil, nil);
  f_word_mul_word_16.adrr:=$0;
end;
end.

