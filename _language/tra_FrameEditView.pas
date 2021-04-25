MSG_NOFILES  := trans('No files'    , 'No hay archivos', 'Mana khipukuna kanchu',
                      'Keine Dateien','Немає файлів', 'Нет файлов', 'Aucun fichier');
MSG_PASFILES := trans('Pascal Files', 'Archivos Pascal','Pascal khipu',
                      'Pascal Dateien','Файли Pascal','Файлы Pascal', 'Fichiers Pascal');
MSG_ALLFILES := trans('All files'   , 'Todos los archivos','Llapan khipukuna',
                      'Alle Dateien','Всі файли','Все файлы', 'Tous les fichiers');
MSG_MODIFSAV := trans('File %s has been modified. Save?',
                      'El archivo %s ha sido modificado. Guardar cambios?',
                      'Kay %s sutiyuq khipuqa huknirayakun. Waqaychankichu huknirayasqanta?',
                      'Die Datei %s wurde verändert. Speichern ?',
		      'Файл %s було змінено. Зберегти?',
		      'Файл %s изменён. Сохранить?', 'Le fichier %s a été modifié. Enregistrer?');
MSG_NOSYNFIL := trans('Syntax file not found: %s'   , 'Archivo de sintaxis no encontrado: %s','Kay kipup sintaxis nisqantaqa mana tarikunchu %s',
                      'Syntax-Datei nicht gefunden: %s','Файл синтаксиса не знайдено: %s','Файл синтаксиса не найден: %s', 'Fichier de syntaxe non trouvé : %s');
lblBackground.Caption := Trans('<< No files >>', '<< Sin archivos >>', 'Mana khipukunayuq',
                               '<< Keine Dateien >>','<< Немає файлів >>','<< Нет файлов >>', '<< Aucun fichier >>');
mnNewTab.Caption      := Trans('New'           , 'Nuevo'             , 'Musuq'            ,
                               'Neu','Новий','Новый', 'Nouveau');
mnCloseTab.Caption    := Trans('Close'         , 'Cerrar'            , 'Wisqay'           ,
                               'Schließen','Закрити','Закрыть', 'Fermer');
mnCloseAll.Caption    := Trans('Close All'     , 'Cerrar todos'      , 'Llapanta wisqay'  ,
                               'Alle Schliessen','Закрити все','Закрыть всё', 'Fermer Tout');
mnCloseOthers.Caption := Trans('Close &others' , 'Cerrar &otros'     , ''                 ,
                               '','Закрити інші','Закрыть все другие', 'Fermer &Autres');
if mnRecents<>nil then
  mnRecents.Caption   := Trans('&Recents' ,'&Recientes','','&Letzte',
                               'Нещодавні','Недавние', '&Récents');

