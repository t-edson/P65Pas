//Main menu
 mnFile.Caption  := Trans('&File'    , '&Archivo'       , '&Khipu'        ,
                          '&Datei'   ,'Файл'		,'Файл',
                          '&Fichier');
 mnEdit.Caption  := Trans('&Edit'    , '&Edición'       , '&Allichay'     ,
                          '&Bearbeiten'	,'Зміни'	,'Редактировать',
                          '&Editer');
 mnFind.Caption  := Trans('&Search'  , '&Buscar'        , '&Maskhay'      ,
                          '&Suchen'  , 'Пошук',         'Поиск',
                          '&Chercher');
 mnView.Caption  := Trans('&View'    , '&Ver'           , '&Qhaway'       ,
                          '&Ansicht' , 'Вигляд'         ,'Вид',
                          '&Affichage');
 mnTools.Caption := Trans('&Tools'   , '&Herramientas'  , '&Llamk''anakuna' ,
                          '&Werkzeuge','Інструменти','Инструменты',
                          '&Outils');

//File Actions
 acArcNewFile.Caption := Trans('New &File'      , 'Nuevo &Archivo'   , 'Musuq &Khipu'        ,
                               '&Neu','Новий файл','Новый проект',
                               'Nouveau &Fichier');
 acArcNewFile.Hint    := Trans('New File'       , 'Nuevo Archivo'    , 'Musuq Khipu'         ,
                               'Neue Datei','Новий файл','Новый файл',
                               'Nouveau Fichier');
 acArcNewProj.Caption := Trans('New &Project'   , 'Nuevo &Proyecto'  , 'Musuq &Proyecto'     ,
                               'Neues &Projekt','Новий проект','Новый файл',
                               'Nouveau &Projet');
 acArcNewProj.Hint    := Trans('New &Project'   , 'Nuevo Proyecto'   , 'Musuq Proyecto'      ,
                               'Neues &Projekt','Новий проект','Новый проект',
                               'Nouveau &Projet');
 acArcOpen.Caption    := Trans('&Open...'       , '&Abrir...'        , 'K&ichay'             ,
                               '&Öffnen...','Відкрити...','Открыть...',
                               '&Ouvrir...');
 acArcOpen.Hint       := Trans('Open file'      , 'Abrir archivo'    , 'Khiputa kichay'      ,
                               'Datei Öffnen','Відкрити файл','Открыть файл',
                               'Ouvrir un fichier');
 acArcSave.Caption    := Trans('&Save'          , '&Guardar'         , '&Waqaychay'          ,
                               '&Speichern','Зберегти','Сохранить',
                               '&Enregistrer');
 acArcSave.Hint       := Trans('Save file'      , 'Guardar archivo'  , 'Khiputa waqaychay'   ,
                               'Datei speichern','Зберегти файл','Сохранить файл',
                               'Enregistrer fichier');
 acArcSaveAs.Caption  := Trans('Sa&ve As...'    , 'G&uardar Como...' , 'Kay hinata &waqaychay',
                               'Speichern &unter ...','Зберегти як...','Сохранить как...',
                               'Enregistrer &sous...');
 acArcSaveAs.Hint     := Trans('Save file as...','Guardar como...'  , 'Kay hinata waqaychay',
                               'Datei mit unter neuem Namen speichern ...','Зберегти файл як...','Сохранить файл как...',
                               'Enregistrer fichier sous...');
 acArcCloseFile.Caption:=Trans('&Close File'    , '&Cerrar archivo'  , 'Khiputa wi&sqay'     ,
                               'Datei s&chließen','Закрити файл','Закрыть файл',
                               '&Fermer Fichier');
 acArcCloseProj.Caption:=Trans('Close Project'  , 'Cerrar Proyecto'  , 'Proyectota wisqay'   ,
                               'Projekt schließen','Закрити проект','Закрыть проект',
                               'Fermer Projet');
 mnSamples.Caption    := Trans('Samples'        , 'Ejemplos'         , 'Qhawarinakuna'       ,
                               'Beispiele','Приклади','Примеры',
                               'Exemples');
 acArcQuit.Caption    := Trans('&Quit'          , '&Salir'           , 'Ll&uqsiy'            ,
                               '&Beenden','Вийти','Выход',
                               '&Quitter');
 acArcQuit.Hint       := Trans('Close the program','Cerrar el programa','Programata wi&sqay',
                               'Programm beenden','Закрити програму','Закрыть программу',
                               'Quitter l''application');

//Edit Actions
 acEdUndo.Caption     := Trans('&Undo'       , '&Deshacer'        , '&Paskay',
                               '&Zurück','Відміна','Отмена',
                               '&Annuler');
 acEdUndo.Hint        := Trans('Undo'        , 'Deshacer'         , 'Paskay',
                               'Änderung zurücknehmen','Відміна','Отмена',
                               'Annuler');
 acEdRedo.Caption     := Trans('&Redo'       , '&Rehacer'         , '&Ruwapay',
                               '&Wiederholen','Повторити','Повторить',
                               '&Refaire');
 acEdRedo.Hint        := Trans('Redo'        , 'Reahacer'         , 'Ruwapay',
                               'Änderung wiederholen','Повторити','Повторить',
                               'Refaire');
 acEdCut.Caption      := Trans('C&ut'        , 'Cor&tar'          , 'Ku&chuy',
                               'A&usschneiden','Вирізати','Вырезать',
                               'Co&uper');
 acEdCut.Hint         := Trans('Cut'         , 'Cortar'           , 'Kuchuy',
                               'Ausschneiden','Вирізати','Вырезать',
                               'Couper');
 acEdCopy.Caption     := Trans('&Copy'       , '&Copiar'          , 'Kiki&nchay',
                               '&Kopieren','Копіювати','Копировать',
                               'Copier');
 acEdCopy.Hint        := Trans('Copy'        , 'Copiar'           , 'Kikinchay',
                               'Kopieren','Копіювати','Копировать',
                               'Copier');
 acEdPaste.Caption    := Trans('&Paste'      , '&Pegar'           , 'k''ask&ay',
                               '&Einfügen','Вставити','Вставить',
                               'Co&ller');
 acEdPaste.Hint       := Trans('Paste'       , 'Pegar'            , 'K''askay',
                               'Einfügen','Вставити','Вставить',
                               'Coller');
 acEdSelecAll.Caption := Trans('Select &All'    , 'Seleccionar &Todo'  , 'Llapan&ta Akllay',
                               'Alles &Auswählen','Вибрати все','Выбрать всё',
                               'Tout &Sélectionner');
 acEdSelecAll.Hint    := Trans('Select all'  , 'Seleccionar todo' , 'Llapanta Akllay',
                               'Alles auswählen','Вибрати все','Выбрать всё',
                               'Tout sélectionner');

//Search Actions
 acSearFind.Caption    := Trans('Find...'      , 'Buscar...'          , 'Maskhay',
                                'Suchen...'    , 'Знайти...'          , 'Найти...',
                                'Chercher...');
 acSearFind.Hint       := Trans('Find text'    , 'Buscar texto'       , 'Qillqata maskhay',
                                'Text suchen'  , 'Знайти текст'       , 'Найти текст',
                                'Chercher texte');
 acSearFindNxt.Caption := Trans('Find &Next'   , 'Buscar &Siguiente'  , '&Hamuqta Maskhay',
                                'Weitersuche&n','Знайти наступний','Найти следующий',
                                'Chercher &Suivant');
 acSearFindNxt.Hint    := Trans('Find Next'    , 'Buscar Siguiente'   , 'Hamuqta Maskhay',
                                'Nächste Stelle suchen','Знайти наступний','Найти следующий',
                                'Chercher Suivant');
 acSearFindPrv.Caption := Trans('Find &Previous','Buscar &Anterior'   , '',
                                '','Знайти попередній','Найти предыдущий',
                                'Chercher &Précédent');
 acSearFindPrv.Hint    := Trans('Find &Previous','Buscar &Anterior'   , '',
                                '','Знайти попередній','Найти предыдущий',
                                'Chercher Précédent');
 acSearReplac.Caption  := Trans('&Replace...'    , '&Reemplazar...'     , '&Yankiy',
                                '&Ersetzen...','Замінити...','Замена...',
                                '&Remplacer...');
 acSearReplac.Hint     := Trans('Replace text'   , 'Reemplazar texto'   , 'Qillqata yankiy',
                                'Text ersetzen','Замінити текст','Заменить текст',
                                'Remplacer texte');

 //View actions
 acViewMsgPan.Caption := Trans('&Messages Panel'         , 'Panel de &Mensajes'           , '&Willanakuna qhawachiq',
                                '&Nachrichten Panel','Панель повідомлень','Панель сообщений',
                                'Panneau &Messages');
 acViewMsgPan.Hint    := Trans('Show/hide Messages Panel', 'Mostrar/Ocultar el Panel de Mensajes', 'Willanakuna qhawachiqta Rikuchiy/Pakachiy',
                                'Nachrichten Panel zeigen oder verbergen','Показати/Сховати панель повідомлень','Показать/Спрятать панель сообщений',
                                'Montrer/Cacher le panneau des Messages');
 acViewStatbar.Caption:= Trans('&Status Bar'             , 'Barra de &Estado'             , '&Imayna kasqanta Qhawachiq',
                                '&Statuszeile'           , '', '',
                                'Barre de &statut');
 acViewStatbar.Hint   := Trans('Show/hide Status Bar'    , 'Mostrar/Ocultar la barra de estado', 'Imayna Kasqanta Rikuchiy/Pakachiy',
                               'Statuszeile zeigen oder verbergen','','',
                               'Montrer/Cacher la barre de statut');
 acViewToolbar.Caption:= Trans('&Tool Bar'               , 'Barra de &Herramientas'       , '&Llamk''anakuna Qhawachiq',
                               '&Werkzeugleiste','Панель інструментів','Панель инструментов',
                               'Barre d''&Outils');
 acViewToolbar.Hint   := Trans('Show/hide Tool Bar'      , 'Mostrar/Ocultar la barra de herramientas', 'Llamk''anakuna Qhawachiqta Rikuchiy/Pakachiy',
                               'Werkzeugleiste zeigen oder verbergen','Показати/Сховати панель інструментів','Показать/Спрятать панель инструментов',
                               'Montrer/Cacher la barre d''Outils');
 acViewSynTree.Caption:= Trans('&Code explorer'          , '&Explorador de código.'       , '&Chimpukunata t''aqwiq',
                               '&Quelltext-Explorer','Оглядач кода','Обозреватель кода',
                               '&Explorateur de Code');
 acViewAsmPan.Caption := Trans('&Assembler Panel'        , '&Panel de ensamblador.'       , '',
                               '','Панель асемблера','Панель ассемблера',
                               'Panneau &Assembleur');


//Tool actions
 MenuItem51.Caption    := Trans('&Select Compiler'        , '&Elegir Compilador'            , '',
                                '','','',
                                '');
 acToolConfig.Caption  := Trans('&Settings'               , '&Configuración'                , 'Kamachina',
                                '&Einstellungen','Налагодження','Настройки',
                                '&Paramètres');
 acToolConfig.Hint     := Trans('Settings dialog'         , 'Ver configuración'            , 'Kamachinata qhaway',
                                'Einstellungs-Dialog','Діалог налагоджень','Диалог настроек',
                                'Paramètres');

//Messages
 MSG_MODIFIED      := Trans('(*)Modified'      , '(*)Modificado'                  , '',
                         '','(*)Змінено','(*)Изменено',
                         '(*)Modifié');
 MSG_SAVED      := Trans('Saved'            , 'Guardado'                       , '',
                         '','Збережено','Сохранено',
                         'Enregistré');
 MSG_NOFILES    := Trans('No files.'        , 'Sin archivos'                   , '',
                         '','Немає файлів.','Нет файлов.',
                         'Aucun fichier');
 MSG_NOFOUND_   := Trans('No found "%s"'    , 'No se encuentra: "%s"'          , '',
                         '','Не знайдено "%s"','Не найдено "%s"',
                         '"%s" non trouvé');
 MSG_N_REPLAC   := Trans('%d words replaced', 'Se reemplazaron %d ocurrencias.', '',
                         '','%d слів замінено','%d слов заменено',
                         '%d mots remplacés');
 MSG_REPTHIS    := Trans('Replace this?'    , '¿Reemplazar esta ocurrencia?'   , '',
                         '','Замінити це?','Заменить это?',
                         'Remplacer ceci?');
 MSG_SYNFIL_NOF := Trans('Syntax file not found: %s' , 'Archivo de sintaxis no encontrado: %s'   , '',
                         '','','',
                         'Fichier de syntaxe non trouvé : %s');
 MSG_FILSAVCOMP  := Trans('File must be saved before compiling.', 'Archivo debe ser guardado antes de compilar', '',
                          '', '', '',
                          'Le fichier doit être sauvegardé pour compiler.');
 MSG_PROJECT     := Trans('Project: ', '', '',
                          '', '', '',
                          '');

