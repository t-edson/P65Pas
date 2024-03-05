//Parser unit
ER_IDEN_EXPECT := trans('Identifier expected.'       , 'Identificador esperado.'       ,'',
                        'Bezeichner erwartet.'       , 'Очікується ідентифікатор.','Ожидается идентификатор.', 'Identifiant attendu.');
ER_NOT_IMPLEM_ := trans('Not implemented: "%s"'       , 'No implementado: "%s"'         ,'',
                        'Nicht implementiert: "%s"'   , 'Не впроваджено: "%s"','Не реализовано: "%s"', 'Non implémenté : "%s"');
ER_DUPLIC_IDEN := trans('Duplicated identifier: "%s"' , 'Identificador duplicado: "%s"' ,'',
                        'Doppelter Platzhalter: "%s"' , 'Дубльований ідентифікатор: "%s"','Дублированный идентификатор: "%s"', 'Identifiant à double : "%s"');
ER_UNDEF_TYPE_ := trans('Undefined type "%s"'         , 'Tipo "%s" no definido.'        ,'',
                        'Undefinierter Typ "%s"'      , 'Невизначений тип "%s"','Неопределённый тип "%s"', 'Type indéfini "%s"');
ER_SEMIC_EXPEC  := trans('";" expected.'              , 'Se esperaba ";"', '',
                         '', '', '', '');
ER_STR_EXPECTED := trans('"%s" expected.'             , 'Se esperaba "%s"'              , '',
                         '', '', '', '');
ER_TYP_PARM_ER_ := trans('Type parameters error on %s', 'Error en tipo de parámetros de %s', '',
                         '', '', '', '');
ER_UNKNOWN_IDE_ := trans('Unknown identifier: %s'     , 'Identificador desconocido: %s' , '',
                         '', '', '', '');
ER_IN_EXPRESSI  := trans('Error in expression. ")" expected.', 'Error en expresión. Se esperaba ")"', '',
                         '', '', '', '');
ER_OPERAN_EXPEC := trans('Operand expected.'          , 'Se esperaba operando.'         , '',
                         '', '', '', '');
ER_ILLEG_OPERA_ := trans('Illegal Operation: %s'      , 'Operación no válida: %s'       , '',
                         '', '', '', '');
ER_UND_OPER_TY_ := trans('Undefined operator: %s for type: %s', 'No está definido el operador: %s para tipo: %s', '',
                         '', '', '', '');
ER_CAN_AP_OPER_ := trans('Cannot apply the operator "%s" to type "%s"', 'No se puede aplicar el operador "%s" al tipo "%s"', '',
                         '', '', '', '');
ER_IN_CHARACTER := trans('Error in character.'        , 'Error en caracter.'            , '',
                         '', '', '', '');
ER_INV_COD_CHAR := trans('Invalid code for char.'     , 'Código inválido para caracter' , '',
                         '', '', '', '');
ER_IDE_TYP_EXP := trans('Identifier of type expected.', 'Se esperaba identificador de tipo.','',
                        'Typenbezeichner erwartet.'   , 'Очікується ідентифікатор типу.','Ожидается идентификатор типа.', 'Identifiant de type attendu.');
ER_RA_HAV_USED := trans('Register A has been used.'   , 'Ya se ha usado el rgistro A.','',
                        ''  , '','', '');
ER_RX_HAV_USED := trans('Register X has been used.'   , 'Ya se ha usado el rgistro X.','',
                        ''  , '','', '');
ER_RY_HAV_USED := trans('Register Y has been used.'   , 'Ya se ha usado el rgistro Y.','',
                        ''  , '','', '');
ER_CON_EXP_EXP := trans('Constant expression expected.', 'Se esperaba una expresión constante','',
                        'Konstanter Ausdruck erwartet.', '', '', 'Expression constante attendue.');
ER_INV_ARR_SIZ := trans('Invalid array size.', 'Tamaño de arreglo inválido', '',
                        ''                   , 'Помилка в розмірі масиву.','Ошибка размера массива.', 'Taille de tableau invalide.');
ER_ARR_SIZ_BIG := trans('Array size to big.' , 'Tamaño de arreglo muy grande', '',
                        ''                   ,'Розмір масиву завеликий.','Размер массива слишком велик.', 'Tableau trop grand.');
ER_INV_MEMADDR := trans('Invalid memory address.'     , 'Dirección de memoria inválida.','',
                        'Ungültige Speicheradresse.'  , 'Недійсна адреса памʼяті.','Недопустимый адрес памяти.', 'Adresse mémoire invalide.');
ER_EXP_VAR_IDE := trans('Identifier of variable expected.', 'Se esperaba identificador de variable.','',
                        'Variablenbezeichner erwartet.'   , 'Очікується фдентифікатор змінної.','Ожидается идентификатор переменной.', 'Identifiant de variable attendu.');
ER_NUM_ADD_EXP := trans('Numeric address expected.'   , 'Se esperaba dirección numérica.','',
                        'Numerische Adresse erwartet.', 'Очікується числова адреса.'    ,'Ожидается числовой адрес.', 'Adresse numérique attendue.');
ER_EQU_EXPECTD := trans('"=" expected.'               , 'Se esperaba "="'               ,'',
                        '"=" erwartet.'               , '"=" очікується.'               ,'"=" ожидается.' , '"=" attendue.');
ER_IDE_CON_EXP := trans('Identifier of constant expected.', 'Se esperaba identificador de constante','',
                        'Konstantenbezeichner erwartet.'  , 'Очікується ідентифікатор константи.','Ожидается идентификатор константы.', 'Identifiant de constante attendu');
ER_EQU_COM_EXP := trans('"=" or "," expected.'        , 'Se esperaba "=" o ","'         ,'',
                        '"=" oder "," erwartet.'      , '"=" або "," очікується.','"=" или "," ожидается.', '"=" ou "," attendus.');
ER_SEM_COM_EXP := trans('":" or "," expected.'        , 'Se esperaba ":" o ",".'        ,'',
                        '":"oder"," erwartet.'        , '":" або "," очікується.','":" или "," ожидается.', '":" ou "," attendus.');

