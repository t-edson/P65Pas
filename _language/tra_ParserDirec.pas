
//Messages when compiling Directives
ER_ERROR_DIREC := trans('Error in directive.'  , 'Error en directiva'      ,'',
                        'Fehler bei Direktive.','Помилка директиви.','Ошибка в директиве.', 'Directive erronnée.');
ER_UNKNO_DEVIC := trans('Unknown device: %s'   , 'Dispositivo desconocido: %s','',
                        'Unbekanntes Gerät: %s','Невизначений прилад: %s','Неизвестное устройство: %s', 'Modèle inconnu : %s');
ER_MODE_UNKNOWN:= trans('Mode unknown: %s'     , 'Modo inválido'           , '',
                        'Unbekannter Modus: %s','Невизначений режим: %s','Неизвестный режим: %s', 'Mode inconnu : %s');
ER_UNKNO_DIREC := trans('Unknown directive: %s', 'Directiva desconocida: %s','',
                        'Unbekannte Direktive: %s','Невизначена директива: %s','Неизвестная директива: %s', 'Directive inconnue : %s');
ER_ERROR_FREQ  := trans('Error in frequency.'  , 'Error en frecuencia.'    , '',
                        'Fehler bei der Frequenz.','Помилка в частоті.','Ошибка в частоте.', 'Erreur de fréquence.');
ER_TOOHIGHFRE  := trans('Frequency too high for this device.', 'Frecuencia muy alta para este microproc.', '',
                        ''                                   , 'Частота зависока для цього приладу.','Слишком высокая частота для этого устройства.', 'Fréquence trop élevée pour ce modèle.');
ER_IDENT_EXPEC := trans('Identifier expected.' , 'Identificador esperado.' , '',
                        'Bezeichner erwartet.' ,'Очікується ідентифікатор.','Ожидается идентификатор.', 'Identifiant attendu.');
ER_EXPEC_EQUAL := trans('Expected "="'         , 'Se esperaba "="'         , '',
                        '"=" erwartet.'        , 'Очікується "="','Ожидается "="', 'Attendu: "="');
ER_SYNTAX_ERRO := trans('Syntax error.'        , 'Error de sintaxis.'      , '',
                        'Syntaxfehler.'        , 'Помилка синтаксиса.','Ошибка синтаксиса.', 'Erreur de syntaxe.');
ER_SYNTAX_ERR_ := trans('Syntax error: "%s"'   , 'Error de sintaxis: "%s"' , '',
                        'Syntax-Fehler: "%s"'  , 'Помилка синтаксиса: "%s"','Ошибка синтаксиса: "%s"', 'Erreur de syntaxe : "%s".');
ER_EXPECTED_BR := trans('Expected: "}".'       , 'Se esperaba "}".'        , '',
                        '"}" erwartet.'        , 'Очікується: "}".','Ожидается: "}".', 'Attendu: "}"');
ER_ENDIF_NOFOU := trans('"$ENDIF" not found.'  , 'No se encontró "$ENDIF".', '',
                        ''                     , '"$ENDIF" не знайдено.','"$ENDIF" не найден.', '$ENDIF non trouvé.');
ER_UNEXP_ENDIF := trans('Unexpected "$ENDIF".' , 'No se esperaba "$ENDIF".', '',
                        ''                     , 'Непередбачений "$ENDIF".','Непредвиденный "$ENDIF".', '"$ENDIF" non attendu.');
ER_UNEXP_ELSE  := trans('Unexpected "$ELSE".'  , 'No se esperaba "$ELSE".' , '',
                        ''                     , 'Непередбачений "$ELSE".','Непредвиденный "$ELSE".', '"$ELSE" non attendu.');
ER_FILE_NO_FND_:= trans('File no found: %s'      , 'Archivo no encontrado: %s' , '',
                        ''                       , 'Файл не знайдено: %s','Файл не найден: %s', 'Fichier non trouvé : %s');
ER_ERIN_NUMBER_:= trans('Error in number: %s'    , 'Error en número: %s' , '',
                        ''                       , 'Помилка в номері: %s','Ошибка в числе: %s', 'Nombre erroné : %s');
ER_UNKNW_IDENT_:= trans('Unknown Identifier: %s' , 'Identificador desconocido: %s' , '',
                        ''                       , 'Невідомий ідентифікатор: %s','Неизвестный идентификатор: %s', 'Identifiant inconnu : %s');
ER_DIVIDE_ZERO := trans('Cannot divide by zero'  , 'No se puede dividir por cero' , '',
                        ''                       , 'Не можу поділити на нуль','Не могу поделить на ноль', 'Division par zéro impossible.');
ER_EVA_ZER_ZER := trans('Cannot evaluate 0^0'    , 'No se puede evaluar 0^0', '',
                        ''                       , 'Не можу оцінити 0^0', 'Не могу оценить 0^0', 'Impossibilité d''évaluer 0^0');
ER_OPE_NOT_IMP_ := trans('Operator not implemented: %s', 'Operador no implementado: %s', '',
                        ''                       , 'Оператор не реалізовано: %s','Оператор не реализован: %s', 'Opérateur non implémenté : %s');
ER_EXPECT_CAR_ := trans('Expected "%s"'          , 'Se esperaba "%s"'         , '',
                        '"%s" erwartet.'         , 'Очікується "%s"','Ожидается "%s"', 'Attendu "%s"');


