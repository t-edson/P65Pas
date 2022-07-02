var
  TIT_CFG_ENVIRON, TIT_CFG_MESPAN, TIT_CFG_CODEXP,
  TIT_CFG_EDITOR, TIT_CFG_SYNTAX,
  TIT_CFG_ASSEMB,  TIT_CFG_COMPIL, TIT_CFG_EXTOOL: String;
  LABEL_THEM_NONE, TIT_CFG_EDICOL: String;

procedure TConfig.SetLanguage;
begin
  fraCfgSynEdit.SetLanguage;
  fraCfgExtTool.SetLanguage;
  fraCfgSyntax.SetLanguage;

Caption              := Trans('Settings'               , 'Configuración'            , '',
                              'Einstellungen','Налаштування','Настройки', 'Paramètres');
BitAceptar.Caption   := Trans('&OK'                    , 'Aceptar'                  , '',
                              '&Ok','','', '&Ok');
BitAplicar.Caption   := Trans('&Apply'                 , 'Aplicar'                  , '',
                              '&Übernehmen','Застосувати','Применить', '&Appliquer');
BitCancel.Caption    := Trans('&Cancel'                , 'Cancelar'                  , '',
                              '&Abbrechen','Відміна','Отмена', 'A&nnuler');

////////////////////////////////////////////////////////////////////////////
//////////////////////////  Environment Settings //////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_ENVIRON     := Trans('Environment', 'Entorno', '',
                             'Umgebung','Оточення','Окружение', 'Environnement');

Label2.Caption      := Trans('Language'               , 'Lenguaje'                 , '',
                              'Sprache','Мова','Язык', 'Langue');
RadioGroup1.Caption := Trans('Toolbar'                 , 'Barra de herramientas'    , 'Barre d''outils',
                              'Werkzeugleiste','Панель інструментів','Панель инструментов', '');
RadioGroup1.Items[0]:= Trans('Small Icons'             , 'Íconos pequeños'          , '',
                              'Kleine Bilder','Маленькі піктограми','Маленькие иконки', 'Petites Icônes');
RadioGroup1.Items[1]:= Trans('Big Icons'               , 'Íconos grandes'           , '',
                              'Große Bilder','Великі піктограми','Большие иконки', 'Grandes Icônes');
label1.Caption      := Trans('Units Path:'             , 'Ruta de unidades'         , '',
                              'Unitpfad:','','', 'Répertoire des unités:');

label3.Caption      := Trans('&Set Theme'        , '&Fijar Tema', '',
                               '','Обрати тему','Выбрать тему', '&Charger un thème');
LABEL_THEM_NONE     := Trans('None', 'Ninguno', '',
                             '','Нічого','Ничего', 'Aucun');
label4.Caption      := Trans('&Create Theme'        , '&Crear Tema', '',
                               '','Створити тему','Создать тему', '&Créer un thème');
butSaveCurThem.Caption := Trans('&Save current config.', 'Guardar config. actual', '',
                             '','Зберегти налаштування','Сохранить настройки', '&Enregister thème actuel');;

chkLoadLast.Caption := Trans('Load last file edited'     , 'Cargar último archivo editado', '',
                             'Letzte editierte Datei laden','Завантажити останній файл','Загрузить последний файл', 'Charger le dernier' + #13#10 + 'fichier édité');

lblPanelCol.Caption := Trans('Panels Color:'             , 'Color de los paneles:', '',
                               'Paneelenfarbe:','Колір панелей:','Цвет панелей:', 'Couleur des panneaux:');
lblSplitCol.Caption := Trans('Splitters color:'          , 'Color de los separadores:', '',
                               'Trenner-Farbe:','Колір розподілувача:','Цвет разделителя:', 'Couleur des séparateurs:');

////////////////////////////////////////////////////////////////////////////
//////////////////////////  Code Explorer //////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_CODEXP    := Trans('Code Explorer', 'Explorador de Código', '',
                           '','Оглядач коду','Инспектор кода', 'Explorateur de Code');
lblCodExplCol1.Caption:= Trans('Back color:' , 'Color de Fondo:', '',
                               'Hintergrundfarbe:','Колір фону:','Цвет фона:', 'Couleur de fond');
lblCodExplCol2.Caption:= Trans('Text Color:' , 'Color de Texto:', '',
                               'Textfarbe:','Колір тексту:','Цвет текста:', 'Couleur du texte');
grpFilType.Caption    := Trans('File types shown:' , 'Tipos de archivos mostrados:', '',
                               '','','', 'Types de fichiers affichés:');
////////////////////////////////////////////////////////////////////////////
//////////////////////////  Message Panel //////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_MESPAN    := Trans('Message Panel', 'Panel de Mensajes', '',
                           '','Панель повідомлень','Панель сообщений', 'Panneau de Messages');
lblMessPan1.Caption   := Trans('Back color'   , 'Color de Fondo', '',
                               'Hintergrundfarbe','Колір фону','Цвет фона', 'Couleur de fond');
lblMessPan2.Caption   := Trans('Text color:'  , 'Color de Texto', '',
                               'Textfarbe:','Колір тексту:','Цвет текста:', 'Couleur du texte:');
lblMessPan3.Caption   := Trans('Error color:' , 'Color de Error', '',
                               'Fehlerfarbe:','Колір помилки:','Цвет ошибки:', 'Couleur des erreurs:');
lblMessPan4.Caption   := Trans('Selection color:', 'Color de Selección', '',
                               'Auswahlfarbe:','Колір обраного:','Цвет выделения:', 'Couleur de sélection:');

////////////////////////////////////////////////////////////////////////////
//////////////////////////  Editor settings ///////////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_EDITOR    := Trans('Editor'                 , 'Editor'                   , '',
                              'Editor','Редактор','Редактор', 'Editeur');

Label6.Caption       := trans('&Font:'                 , '&Letra:'                     ,'',
                              'Schri&ftart:','Шрифт:','Шрифт:', 'Police');
Label7.Caption       := trans('&Size:'                 , '&Tamaño:'                    ,'',
                              '&Größe:','Розмір:','Размер:', 'Taille');
chkViewVScroll.Caption:= trans('&Vertical Scrollbar'    , 'Barra de desplaz &Vert.'     ,'',
                              '& Vertikale Bildlaufleiste','Вертикальній скролбар','Вертикальный скролбар', 'Barre de défilement' + #13#10 + 'verticale');
chkViewHScroll.Caption:= trans('&Horizontal Scrollbar'  , 'Barra de desplaz &Horiz.'    ,'',
                              '&Horizontale Bildlaufleiste','Горизонтальний скролбар','Горизонтальный скролбар', 'Barre de défilement' + #13#10 + 'horizontale');

grpTabEdiState.Caption :=Trans('Tab Editor State'  , 'Estado de pestañas del editor', '',
                              'Registerkarte Editor Zustand','','', 'Affichage des onglets');;
grpTabEdiState.Items[0]:=Trans('&Show always'      , 'Mostrar &Siempre'         , '',
                              '','Показувати завжди','Показывать всегда', '&Toujours afficher');
grpTabEdiState.Items[1]:=Trans('Hide for &One file', '&Ocultar si hay un archivo', '',
                              'Ausblenden für &eine Datei','Сховати для одного файлу','Скрыть для одного файла', '&Si plusieurs fichiers');
grpTabEdiState.Items[2]:=Trans('&Hide always'      , 'Ocultar &Siempre'          , '',
                              '&Immer ausblenden','Ховати завжди','Прятать всегда', '&Toujours cacher');

////////////////////////////////////////////////////////////////////////////
//////////////////////////// Editor Colors Settings ////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_EDICOL      := Trans('Colors'                 , 'Colores'                   , '',
                             '','Кольори','Цвета', 'Couleurs');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// Editor-Syntax Settings ////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_SYNTAX      := Trans('Syntax'                 , 'Sintaxis'                 , '',
                             'Syntax','Синтакс','Синтакс', 'Syntaxe');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// Assembler settings ////////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_ASSEMB       := Trans('Assembler'              , 'Ensamblador'              , '',
                                 'Assembler','Асемблер','Ассемблер', 'Assembleur');
chkIncDecVar.Caption := Trans('Include &Variables declaration', 'Incluir Declaración de variables', '',
                              'Variablendeklaration einfügen','Включити декларування змінних','Включить объявление переменных', 'Inclure les déclarations de &variables');
RadioGroup2.Caption  := Trans('Style'                  , 'Estilo'                   , '',
                              'Stil','Стиль','Стиль', 'Style');
chkExcUnused.Caption  := Trans('Exclude unused'         , 'Excluir no usadas'        , '',
                             'Unbenutzte ausschlieÃŸen','Виключити невикористовуване','Исключить неиспользуемое', 'Exclure les portions inutilisées');
chkIncAddress.Caption := Trans('Include &Memory Address','Incluir &Dirección de memoria','',
                               'Speicheradressen einbinden','Включити Memory Address','Включить Memory Address', 'Inclure les adresses' + #13#10 + '&mémoire');
chkIncComment.Caption := Trans('Include &Comments'      , 'Incluir &Comentarios'     , '',
                               'Kommentare hinzufügen','Включити коментарі','Включить комментарии', 'Inclure les' + #13#10 + '&commentaires');
chkIncComment2.Caption:= Trans('Include &Detailed comments', 'Incluir Comentarios &detallados' , '',
                               '&Detaillierte Kommentare hinzufügen','Включити детальні коментарі','Включить детальные комментарии', 'Inclure les commentaires' + #13#10 + '&détaillés');
chkIncVarName.Caption := Trans('Include &Variable Names','Incluir Nombre de &Variables','',
                               '&Variablennamen einbinden','Включити імена змінних','Включить имена переменных', 'Inclure les &noms de' + #13#10 + 'variables');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// Output Settings ///////////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_COMPIL         := Trans('Compiler'               , 'Compilador'               , '',
                                'Compiler','Компілятор','Компилятор', 'Compilateur');
chkShowErrMsg.Caption  := Trans('&Show Error Messages'   , '&Mostrar mensajes de error', '',
                                '&Zeige Fehlermeldungen','Показувати сповіщення про помилки','Показывать сообщения об ошибках', '&Afficher les messages d''erreur');
grpOptimLev.Caption    := Trans('Optimization Level:'    , 'Nivel de optimización:'   , '',
                                'Optimierungsstufe:','Рівень оптимізації:','Уровень оптимизации:', 'Niveau d''optimisation:');
grpOptimLev.Items[0]   := Trans('Fool'                   , 'Tonto'                    , '',
                                'Dumm','Дурень','Дурак', 'Basique');
grpOptimLev.Items[1]   := Trans('Smart'                  , 'Inteligente'              , '',
                                'Schlau','Розумний','Умный', 'Avancé');
chkOptBnkAftIF.Caption := Trans('After IF structure'    , 'Después de instrucciones IF.', '',
                               'Nach IF-Struktur','Після структури IF','После структуры IF', 'Après le bloc IF');
chkOptBnkBefPro.Caption:= Trans('Before Procedures'     , 'Antes de procedimientos.', '',
                               'Vor Prozeduren','Перед процедурами','Перед процедурами', 'Avant les procédures');
chkOptBnkAftPro.Caption:= Trans('After Procedures'      , 'Después de procedimientos.', '',
                                'Nach den Prozeduren','Після адреси','После адреса', 'Après les procédures');
chkReuProcVar.Caption  := Trans('Reuse Procedures Variables', 'Reutilizar variables de proced.', '',
                                '','Повторно використовувати змінні процедур','Повторно использовать переменные процедур', 'Ré-utiliser les variables de procédure');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// External Tool ////////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_EXTOOL    := Trans('External Tool'           , 'Herramienta Externa'      , '',
                           '','Завнішній інструмент','Внешний инструмент', 'Outils Externes');
FillTree;
end;

