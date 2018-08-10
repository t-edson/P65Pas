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
    kCOMF(byte1, toW);
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
    kMOVF(byte2, toW);
    kANDLW(value1);  //leave in W
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    if value1 = 0 then begin  //Caso especial
      SetROBResultConst_byte(0);  //puede generar error
      exit;
    end else if value1 = 255 then begin  //Caso especial
      SetROBResultExpres_byte(Opt);  //No es necesario hacer nada. Ya está en W
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    kANDLW(value1);  //leave in W
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
    kMOVLW(value2);
    kANDWF(byte1, toW);   //leave in W
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    kMOVF(byte2, toW);
    kANDWF(byte1, toW);   //leave in W
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    kANDWF(byte1, toW);   //leave in W
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    if value2 = 0 then begin  //Caso especial
      SetROBResultConst_byte(0);  //puede generar error
      exit;
    end else if value1 = 255 then begin  //Caso especial
      SetROBResultExpres_byte(Opt);  //No es necesario hacer nada. Ya está en W
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    kANDLW(value2);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    kANDWF(byte2, toW);
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //p1 está en la pila y p2 en el acumulador
    rVar := GetVarByteFromStk;
    kANDWF(rVar.adrByte0, toW);
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
    kMOVF(byte2, toW);
    kIORLW(value1);  //leave in W
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    if value1 = 0 then begin  //Caso especial
      SetROBResultExpres_byte(Opt);  //No es necesario hacer nada. Ya está en W
      exit;
    end else if value1 = 255 then begin  //Caso especial
      SetROBResultConst_byte(255);
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    kIORLW(value1);  //leave in W
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
    kMOVLW(value2);
    kIORWF(byte1, toW);   //leave in W
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    kMOVF(byte2, toW);
    kIORWF(byte1, toW);   //leave in W
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    kIORWF(byte1, toW);   //leave in W
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    if value2 = 0 then begin  //Caso especial
      SetROBResultExpres_byte(Opt);  //No es necesario hacer nada. Ya está en W
      exit;
    end else if value2 = 255 then begin  //Caso especial
      SetROBResultConst_byte(255);
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    kIORLW(value2);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    kIORWF(byte2, toW);
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //p1 está en la pila y p2 en el acumulador
    rVar := GetVarByteFromStk;
    kIORWF(rVar.adrByte0, toW);
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
    kMOVF(byte2, toW);
    kXORLW(value1);  //leave in W
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    kXORLW(value1);  //leave in W
  end;
  stVariab_Const: begin
    SetROBResultExpres_byte(Opt);
    kMOVLW(value2);
    kXORWF(byte1, toW);   //leave in W
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    kMOVF(byte2, toW);
    kXORWF(byte1, toW);   //leave in W
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    kXORWF(byte1, toW);   //leave in W
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    kXORLW(value2);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    kXORWF(byte2, toW);
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //p1 está en la pila y p2 en el acumulador
    rVar := GetVarByteFromStk;
    kXORWF(rVar.adrByte0, toW);
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
    SetROBResultConst_byte(ord(value1 = value2));
  end;
  stConst_Variab: begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    if value1 = 0 then begin  //caso especial
      _MOVF(p2^.offs, toF);  //si iguales _Z=1
    end else if value1 = 1 then begin  //caso especial
      _DECF(p2^.offs, toW);  //si el resultado es cero _Z=1
    end else if value1 = 255 then begin  //caso especial
      _INCF(p2^.offs, toW);  //si el resultado es cero _Z=1
    end else begin
      _LDAi(value1);
      _SUBWF(p2^.offs, toW);  //si iguales _Z=1
    end;
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    kXORLW(value1);  //Si son iguales Z=1. Se usa XORLW, porque SUBLW no existe en la gama baja
  end;
  stVariab_Const: begin
    ExchangeP1_P2;  //Convierte a stConst_Variab
    ROB_byte_equal_byte(Opt, SetRes);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    kMOVF(byte1, toW);
    kSUBWF(byte2, toW);  //si iguales _Z=1
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    kSUBWF(byte1, toW);  //si iguales _Z=1
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    kXORLW(value2);  //Si son iguales Z=1. Se usa XORLW, porque SUBLW no existe en la gama baja
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    //ReserveW; if HayError then exit;
    _SUBWF(p2^.offs, toW);  //si iguales _Z=1
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    //la expresión p1 debe estar salvada y p2 en el acumulador
    rVar := GetVarByteFromStk;
    kSUBWF(rVar.adrByte0, toW);  //compara directamente a lo que había en pila.
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
      if value2L = 0 then begin  //optimiza
        kCLRF(byte1L);
      end else begin
        kMOVLW(value2L);
        kMOVWF(byte1L);
      end;
      if value2H = 0 then begin  //optimiza
        kCLRF(byte1H);
      end else begin
        kMOVLW(value2H);
        kMOVWF(byte1H);
      end;
    end;
    stVariab: begin
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
      kMOVF(byte2L, toW);
      kMOVWF(byte1L);
      kMOVF(byte2H, toW);
      kMOVWF(byte1H);
    end;
    stExpres: begin   //se asume que se tiene en (H,w)
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
      kMOVWF(byte1L);
      _MOVF(H.offs, toW);
      kMOVWF(byte1H);
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
  end else if p1^.Sto = stVarRefVar then begin
    //Asignación a una variable
    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
    case p2^.Sto of
    stConst : begin
      //Caso especial de asignación a puntero derefrrenciado: variable^
      kMOVF(byte1, toW);
      kMOVWF(FSR);  //direcciona byte bajo
      //Asignación normal
      if value2L=0 then begin
        //caso especial
        _CLRF(0);
      end else begin
        kMOVLW(value2L);
        _MOVWF(0);
      end;
      _INCF(FSR.offs, toF);  //direcciona byte alto
      if value2H=0 then begin
        //caso especial
        _CLRF(0);
      end else begin
        kMOVLW(value2H);
        _MOVWF(0);
      end;
    end;
    stVariab: begin
      //Caso especial de asignación a puntero dereferenciado: variable^
      kMOVF(byte1, toW);
      kMOVWF(FSR);  //direcciona byte bajo
      //Asignación normal
      kMOVF(byte2L, toW);
      _MOVWF(0);
      _INCF(FSR.offs, toF);  //direcciona byte alto
      kMOVF(byte2H, toW);
      _MOVWF(0);
    end;
    stExpres: begin  //ya está en H,w
      //Caso especial de asignación a puntero dereferenciado: variable^
      aux := GetAuxRegisterByte;
      _MOVWF(aux.offs);   //Salva W (p2.L)
      //Apunta con p1
      kMOVF(byte1, toW);
      _MOVWF(FSR.offs);  //direcciona a byte bajo
      //Asignación normal
      _MOVF(aux.offs, toW);   //recupero p2.L
      _MOVWF(0);          //escribe
      _MOVF(H.offs, toW);   //recupero p2.H
      _INCF(FSR.offs, toF);   //apunta a byte alto
      _MOVWF(0);          //escribe
      aux.used := false;
    end;
    else
      GenError(MSG_UNSUPPORTED); exit;
    end;
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
      if value2 = 0 then begin
        //caso especial
        kCLRF(byte1L);
        kCLRF(byte1H);
      end else begin;
        kCLRF(byte1H);
        kMOVLW(value2);
        kMOVWF(byte1L);
      end;
    end;
    stVariab: begin
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
      kCLRF(byte1H);
      kMOVF(byte2L, toW);
      kMOVWF(byte1L);
    end;
    stExpres: begin   //se asume que está en w
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
      kCLRF(byte1H);
      kMOVWF(byte1);
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
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stConst_Const: begin  //compara constantes. Caso especial
    SetROBResultConst_byte(ORD(value1 = value2));
  end;
  stConst_Variab: begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    ////////// Compara byte alto
    if value1H = 0 then begin  //caso especial
      kMOVF(byte2H, toW); //p2-p1
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale);  //no son iguales
    end else if value1H = 1 then begin  //caso especial
      kDECF(byte2H, toW); //p2-p1
      _BTFSS(Z.offs, Z.bit);
      {De no ser porque se tiene que devolver siempre, el valor de Z,
      las 2 instrucciones anteriores, se podrían reemplazar con un i_DECFSZ,
      pero i_DECFSZ, no actualiza Z}
      _GOTO_PEND(sale);  //no son iguales
    end else if value1H = 255 then begin  //caso especial
      kINCF(byte2H, toW); //p2-p1
      _BTFSS(Z.offs, Z.bit);
      {De no ser porque se tiene que devolver siempre, el valor de Z,
      las 2 instrucciones anteriores, se podrían reemplazar con un i_DECFSZ,
      pero i_DECFSZ, no actualiza Z}
      _GOTO_PEND(sale);  //no son iguales
    end else begin  //caso general
      kMOVLW(value1H);
      kSUBWF(byte2H, toW); //p2-p1
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale);  //no son iguales
    end;
    //////////  Son iguales, comparar el byte bajo
    if value1L = 0 then begin  //caso especial
      kMOVF(byte2L,toW);	//p2-p1
  _LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
    end else if value1L = 1 then begin  //caso especial
      kDECF(byte2L,toW);	//p2-p1
  _LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
    end else if value1L = 255 then begin  //caso especial
      kINCF(byte2L,toW);	//p2-p1
  _LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
    end else begin
      kMOVLW(value1L);
      kSUBWF(byte2L,toW);	//p2-p1
  _LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
    end;
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó p2 esta en W
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    tmp := GetAuxRegisterByte;
    if HayError then exit;
    _MOVWF(tmp.offs);   //salva byte bajo de Expresión
    //Compara byte alto
    kMOVLW(value1H);
    _SUBWF(H.offs, toW); //p2-p1
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale);  //no son iguales
    //Son iguales, comparar el byte bajo
    kMOVLW(value1L);
    _SUBWF(tmp.offs,toW);	//p2-p1
_LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
    tmp.used := false;
  end;
  stVariab_Const: begin
    ExchangeP1_P2;  //Convierte a stConst_Variab
    ROB_word_equal_word(Opt, SetRes);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    //Compara byte alto
    kMOVF(byte1H, toW);
    kSUBWF(byte2H, toW); //p2-p1
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale);  //no son iguales
    //Son iguales, comparar el byte bajo
    kMOVF(byte1L, toW);
    kSUBWF(byte2L,toW);	//p2-p1
_LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
    tmp := GetAuxRegisterByte;
    _MOVWF(tmp.offs);   //salva byte bajo de Expresión
    //Compara byte alto
    kMOVF(byte1H, toW);
    _SUBWF(H.offs, toW); //p2-p1
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale);  //no son iguales
    //Son iguales, comparar el byte bajo
    kMOVF(byte1L, toW);
    _SUBWF(tmp.offs,toW);	//p2-p1
    tmp.used := false;
_LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    ExchangeP1_P2;  //Convierte a stConst_Expres;
    ROB_word_equal_word(Opt, SetRes);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    ExchangeP1_P2;  //Convierte a stVariab_Expres;
    ROB_word_equal_word(Opt, SetRes);
  end;
  stExpres_Expres:begin
    //La expresión p1, debe estar salvada y p2 en (H,W)
    p1^.SetAsVariab(GetVarWordFromStk);
    //Luego el caso es similar a variable-expresión
    ROB_word_equal_word(Opt, SetRes);
    FreeStkRegisterWord;
  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_word_difer_word(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_word_equal_word(Opt, SetRes);
  res.Invert;
end;

procedure TGenCod.ROB_word_mul_byte(Opt: TxpOperation; SetRes: boolean);
begin
   if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
     GenError('Too complex pointer expression.'); exit;
   end;
   if not ChangePointerToExpres(p1^) then exit;
   if not ChangePointerToExpres(p2^) then exit;
   case stoOperation of
   stConst_Const:begin  //producto de dos constantes. Caso especial
     if value1*value2 < $100 then begin
       SetROBResultConst_byte((value1*value2) and $FF);  //puede generar error
     end else if value1*value2 < $10000 then begin
       SetROBResultConst_word((value1*value2) and $FFFF);  //puede generar error
     end else begin
       SetROBResultConst_dword((value1*value2) and $FFFFFFFF);  //puede generar error
     end;
     exit;  //sale aquí, porque es un caso particular
   end;
 //  stConst_Variab: begin
 //    if value1=0 then begin  //caso especial
 //      SetROBResultConst_byte(0);
 //      exit;
 //    end else if value1=1 then begin  //caso especial
 //      SetROBResultVariab(p2^.rVar);
 //      exit;
 //    end else if value1=2 then begin
 //      SetROBResultExpres_word(Opt);
 //      _CLRF(H.offs);
 //      _BCF(STATUS, _C);
 //      _RLF(p2^.offs, toW);
 //      _RLF(H.offs, toF);
 //      exit;
 //    end;
 //    SetROBResultExpres_word(Opt);
 //    _MOVF(p2^.offs, toW);
 //    _MOVWF(E.offs);
 //    kMOVLW(value1);
 //    _CALL(f_byte_mul_byte_16.adrr);
 //    AddCallerTo(f_byte_mul_byte_16);
 //  end;
 //  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
 //    SetROBResultExpres_word(opt);
 //    _MOVWF(E.offs);
 //    kMOVLW(value1);
 //    _CALL(f_byte_mul_byte_16.adrr);
 //    AddCallerTo(f_byte_mul_byte_16);
 //  end;
 //  stVariab_Const: begin
 //    SetROBResultExpres_word(opt);
 //    _MOVF(p1^.offs, toW);
 //    _MOVWF(E.offs);
 //    kMOVLW(value2);
 //    _CALL(f_byte_mul_byte_16.adrr);
 //    AddCallerTo(f_byte_mul_byte_16);
 //  end;
 //  stVariab_Variab:begin
 //    SetROBResultExpres_word(Opt);
 //    _MOVF(p1^.offs, toW);
 //    _MOVWF(E.offs);
 //    _MOVF(p2^.offs, toW);
 //    _CALL(f_byte_mul_byte_16.adrr);
 //    AddCallerTo(f_byte_mul_byte_16);
 //  end;
 //  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
 //    SetROBResultExpres_word(Opt);
 //    _MOVWF(E.offs);  //p2 -> E
 //    _MOVF(p1^.offs, toW); //p1 -> W
 //    _CALL(f_byte_mul_byte_16.adrr);
 //    AddCallerTo(f_byte_mul_byte_16);
 //  end;
 //  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
 //    SetROBResultExpres_word(Opt);
 //    _MOVWF(E.offs);  //p1 -> E
 //    kMOVLW(value2); //p2 -> W
 //    _CALL(f_byte_mul_byte_16.adrr);
 //    AddCallerTo(f_byte_mul_byte_16);
 //  end;
 //  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
 //    SetROBResultExpres_word(Opt);
 //    _MOVWF(E.offs);  //p1 -> E
 //    _MOVF(p2^.offs, toW); //p2 -> W
 //    _CALL(f_byte_mul_byte_16.adrr);
 //    AddCallerTo(f_byte_mul_byte_16);
 //  end;
 //  stExpres_Expres:begin
 //    SetROBResultExpres_word(Opt);
 //    //la expresión p1 debe estar salvada y p2 en el acumulador
 //    rVar := GetVarByteFromStk;
 //    _MOVWF(E.offs);  //p2 -> E
 //    _MOVF(rVar.adrByte0.offs, toW); //p1 -> W
 //    _CALL(f_byte_mul_byte_16.adrr);
 //    FreeStkRegisterByte;   //libera pila porque se usará el dato ahí contenido
 //    {Se podría ahorrar el paso de mover la variable de la pila a W (y luego a una
 //    variable) temporal, si se tuviera una rutina de multiplicación que compilara a
 //    partir de la direccion de una variable (en este caso de la pila, que se puede
 //    modificar), pero es un caso puntual, y podría no reutilizar el código apropiadamente.}
 //    AddCallerTo(f_byte_mul_byte_16);
 //  end;
   else
     genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
   end;
end;
procedure TGenCod.ROB_word_mul_word(Opt: TxpOperation; SetRes: boolean);
begin
   if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
     GenError('Too complex pointer expression.'); exit;
   end;
   if not ChangePointerToExpres(p1^) then exit;
   if not ChangePointerToExpres(p2^) then exit;
   case stoOperation of
   stConst_Const:begin  //producto de dos constantes. Caso especial
     if value1*value2 < $100 then begin
       SetROBResultConst_byte((value1*value2) and $FF);  //puede generar error
     end else if value1*value2 < $10000 then begin
       SetROBResultConst_word((value1*value2) and $FFFF);  //puede generar error
     end else begin
       SetROBResultConst_dword((value1*value2) and $FFFFFFFF);  //puede generar error
     end;
     exit;  //sale aquí, porque es un caso particular
   end;
 //  stConst_Variab: begin
 //    if value1=0 then begin  //caso especial
 //      SetROBResultConst_byte(0);
 //      exit;
 //    end else if value1=1 then begin  //caso especial
 //      SetROBResultVariab(p2^.rVar);
 //      exit;
 //    end else if value1=2 then begin
 //      SetROBResultExpres_word(Opt);
 //      _CLRF(H.offs);
 //      _BCF(STATUS, _C);
 //      _RLF(p2^.offs, toW);
 //      _RLF(H.offs, toF);
 //      exit;
 //    end;
 //    SetROBResultExpres_word(Opt);
 //    _MOVF(p2^.offs, toW);
 //    _MOVWF(E.offs);
 //    kMOVLW(value1);
 //    _CALL(f_byte_mul_byte_16.adrr);
 //    AddCallerTo(f_byte_mul_byte_16);
 //  end;
 //  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
 //    SetROBResultExpres_word(opt);
 //    _MOVWF(E.offs);
 //    kMOVLW(value1);
 //    _CALL(f_byte_mul_byte_16.adrr);
 //    AddCallerTo(f_byte_mul_byte_16);
 //  end;
 //  stVariab_Const: begin
 //    SetROBResultExpres_word(opt);
 //    _MOVF(p1^.offs, toW);
 //    _MOVWF(E.offs);
 //    kMOVLW(value2);
 //    _CALL(f_byte_mul_byte_16.adrr);
 //    AddCallerTo(f_byte_mul_byte_16);
 //  end;
 //  stVariab_Variab:begin
 //    SetROBResultExpres_word(Opt);
 //    _MOVF(p1^.offs, toW);
 //    _MOVWF(E.offs);
 //    _MOVF(p2^.offs, toW);
 //    _CALL(f_byte_mul_byte_16.adrr);
 //    AddCallerTo(f_byte_mul_byte_16);
 //  end;
 //  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
 //    SetROBResultExpres_word(Opt);
 //    _MOVWF(E.offs);  //p2 -> E
 //    _MOVF(p1^.offs, toW); //p1 -> W
 //    _CALL(f_byte_mul_byte_16.adrr);
 //    AddCallerTo(f_byte_mul_byte_16);
 //  end;
 //  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
 //    SetROBResultExpres_word(Opt);
 //    _MOVWF(E.offs);  //p1 -> E
 //    kMOVLW(value2); //p2 -> W
 //    _CALL(f_byte_mul_byte_16.adrr);
 //    AddCallerTo(f_byte_mul_byte_16);
 //  end;
 //  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
 //    SetROBResultExpres_word(Opt);
 //    _MOVWF(E.offs);  //p1 -> E
 //    _MOVF(p2^.offs, toW); //p2 -> W
 //    _CALL(f_byte_mul_byte_16.adrr);
 //    AddCallerTo(f_byte_mul_byte_16);
 //  end;
 //  stExpres_Expres:begin
 //    SetROBResultExpres_word(Opt);
 //    //la expresión p1 debe estar salvada y p2 en el acumulador
 //    rVar := GetVarByteFromStk;
 //    _MOVWF(E.offs);  //p2 -> E
 //    _MOVF(rVar.adrByte0.offs, toW); //p1 -> W
 //    _CALL(f_byte_mul_byte_16.adrr);
 //    FreeStkRegisterByte;   //libera pila porque se usará el dato ahí contenido
 //    {Se podría ahorrar el paso de mover la variable de la pila a W (y luego a una
 //    variable) temporal, si se tuviera una rutina de multiplicación que compilara a
 //    partir de la direccion de una variable (en este caso de la pila, que se puede
 //    modificar), pero es un caso puntual, y podría no reutilizar el código apropiadamente.}
 //    AddCallerTo(f_byte_mul_byte_16);
 //  end;
   else
     genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
   end;
end;
procedure TGenCod.ROB_word_umulword_word(Opt: TxpOperation; SetRes: boolean);
begin
  case stoOperation of
  stConst_Const:begin  //producto de dos constantes. Caso especial
    SetROBResultConst_word((value1*value2) and $FFFF);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
//  stConst_Variab: begin
//    SetROBResultExpres_word(Opt);
//    _MOVF(p2^.offs, toW);
//    _MOVWF(H.offs);
//    kMOVLW(value1);
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
//    _MOVWF(H.offs);
//    kMOVLW(value1);
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  stVariab_Const: begin
//    SetROBResultExpres_byte(Opt);
//    _MOVF(p1^.offs, toW);
//    _MOVWF(H.offs);
//    kMOVLW(value2);
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  stVariab_Variab:begin
//    SetROBResultExpres_byte(Opt);
//    _MOVF(p1^.offs, toW);
//    _MOVWF(H.offs);
//    _MOVF(p2^.offs, toW);
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
//    _MOVWF(H.offs);  //p2 -> H
//    _MOVF(p1^.offs, toW); //p1 -> W
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
//    _MOVWF(H.offs);  //p1 -> H
//    kMOVLW(value2); //p2 -> W
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
//    _MOVWF(H.offs);  //p1 -> H
//    _MOVF(p2^.offs, toW); //p2 -> W
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  stExpres_Expres:begin
//    SetROBResultExpres_byte(Opt);
//    //la expresión p1 debe estar salvada y p2 en el acumulador
//    FreeStkRegisterByte(r);   //libera pila porque se usará el dato ahí contenido
//    _MOVWF(H.offs);  //p2 -> H
//    _MOVF(r.offs, toW); //p1 -> W
//    _CALL(f_byteXbyte_byte.adrr);
//    {Se podría ahorrar el paso de mover la variable de la pila a W (y luego a una
//    variable) temporal, si se tuviera una rutina de multiplicación que compilara a
//    partir de la direccion de una variable (en este caso de la pila, que se puede
//    modificar), pero es un caso puntual, y podría no reutilizar el código apropiadamente.}
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
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
    kMOVLW(value1L);      //Carga menos peso del dato 1
    kANDWF(byte2L,toW);  //deja en W
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en (W)
    SetROBResultExpres_byte(Opt);
    _andlw(value1L);      //Deja en W
  end;
  stVariab_Const: begin
    SetROBResultExpres_byte(Opt);
    kMOVF(byte1L, toW);
    _ANDLW(value2L);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    kMOVF(byte1L, toW);
    kANDWF(byte2L, toW);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en (_H,W)
    SetROBResultExpres_byte(Opt);
    kANDWF(byte1L, toW);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,W)
    SetROBResultExpres_byte(Opt);
    _ANDLW(value2L);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,W)
    SetROBResultExpres_byte(Opt);
    kANDWF(byte2L, toW);
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //p1 está salvado en pila y p2 en (W)
    p1^.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
    //Luego el caso es similar a stVariab_Expres
    kANDWF(byte1L, toW);
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


