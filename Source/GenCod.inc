////////////operaciones con Bit
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


