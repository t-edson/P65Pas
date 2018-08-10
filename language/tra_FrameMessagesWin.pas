MSG_INICOMP := trans('Starting Compilation...', 'Iniciando compilación...', '',
                     'Compilieren ausführen...','Починаю компіляцію...','Начинаю компиляцию...', 'Lancement de la compilation....');
MSG_WARN    := trans('Warning'                , 'Advertencia'             , 'Avertissement',
                     'Warnung','Попередження','Предупреждение', '');
MSG_WARNS   := trans('Warnings'               , 'Advertencias'            , '',
                     'Warnungen','Попередження','Предупреждения', 'Avertissements');
MSG_ERROR   := trans('Error'                  , 'Error'                   , '',
                     'Fehler','Помилка','Ошибка', 'Erreur');
MSG_ERRORS  := trans('Errors'                 , 'Errores'                 , '',
                     'Fehler','Помилки','Ошибки', 'Erreurs');
MSG_COMPIL  := trans('Compiled in: '          , 'Compilado en: '          , '',
                     'Compiliert in: ','Скомпільовано за: ','Скомпилировано за: ', 'Compilé en:');

lblInform.Caption := Trans('Information'     , 'Información'      , ''      ,
                           'Information'       , 'Інформація'  ,'Информация', 'Information');
lblWarns.Caption  := Trans('Warnings'        , 'Advertencias'     , ''      ,
                           'Warnungen'         , 'Попередження','Предупреждения', 'Avertissements');
lblErrors.Caption := Trans('Errors'          , 'Errores'          , ''      ,
                           'Fehler'            , 'Помилки'     ,'Ошибки', 'Erreurs');
PanGrilla.Caption := Trans('<< No messages >>','<< Sin mensajes >>',''      ,
                           '<<Keine Meldungen>>','<< Немає повідомлень >>','<< Нет сообщений >>', '<< Aucun message >>');
mnClearAll.Caption:= Trans('Clear &All'      , 'Limpiar &Todo'    , ''      ,
                           ''                 , 'Зтерти все'  ,'', 'Effacer &Tout');
mnCopyRow.Caption := Trans('&Copy Row'       , '&Copiar fila'     , ''      ,
                           ''                 , '','', '&Copier ligne');

