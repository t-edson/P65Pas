MiConfig 0.4
============

## Descripción

MiConfig es una librería de Lazarus, que puede ser usada para crear fácilmente formularios de configuración.

Con esta librería se simplifica considerablemente, la creación de ventanas de configuración, porque la librería incluye métodos predefinidos que facilitan la manipulación de variables (propiedades) de la aplicación, de modo que editarlos en un diálogo y guardar los cambios a disco, se hacen de forma casi transparente.

Se pueden usar archivos INI o XML.

Con la librería "MiConfig", se pueden crear formularios de configuración sencillos como este:

```
unit FormConfig;
{$mode objfpc}{$H+}
interface
uses ..., MiConfigINI;  

type
  TConfig = class(TForm)
    BitCancel: TBitBtn;
    BitAceptar: TBitBtn;
    Edit1: TEdit;
    procedure BitAceptarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    //vars to manage
    MyText : string;
    procedure Initiate;
  end;

var
  Config: TConfig;

implementation
{$R *.lfm}
procedure TConfig.Initiate;
begin
  //asociate vars to controls
  iniFile.Asoc_Str('MyText', @MyText, Edit1, '');
  iniFile.FileToProperties;
end;

procedure TConfig.FormShow(Sender: TObject);
begin
  iniFile.PropertiesToWindow;
end;

procedure TConfig.BitAceptarClick(Sender: TObject);
begin
  iniFile.WindowToProperties;
  self.Close;
end;

end.
```

Con este código se podrá editar el valor de la variable "texto" con el control "Edit1", y guardar los cambios a disco o leerlos desde allí.

## Modo de uso

La librería puede trabajar en 3 formas:

1. Asociando propiedades a controles y a un archivo:
2. Asociando solamente propiedades a un archivo:
3. Asociando solamente propiedades a controles:

### Asociando propiedades a controles y a un archivo:

Esta es la forma más común, cuando se trabaja con formularios o diálogos de configuración, ya que es deseable poder modificar ciertas propiedades de la aplicación y mantener estos cambios en disco.

El siguiente diagrama muestra el flujo de información, y los métodos que permiten realizar ese flujo:

```
 +-----------+                  +-------------+                    +------------+
 |           | FileToProperties |             | PropertiesToWindow |            |
 |           | ---------------> |             | -----------------> |            |
 |   Disco   |                  | Variables   |                    | Controles  |
 |  (File)   | PropertiesToFile |(Properties) | WindowToProperties | (Window)   |
 |           | <--------------- |             | <----------------- |            |
 +-----------+                  +-------------+                    +------------+
```

De acuerdo al formato de archivo a manejar, se deberá usar la unidad TMiConfigINI o TMiConfigXML. 

Para empezar a trabajar, se debe crear una instancia del objeto TMiConfigINI (o TMiConfigXML, si se quiere trabajar con XML). También se puede usar el objeto "cfgFile" que se crea por defecto con la unidad.

Luego se deben crear asociaciones de las variables a guardar, y los controles que permitirán modificar estas variables. Para ello, existen un conjunto de métodos que permiten realizar estas asociaciones:

    Asoc_Int();
    Asoc_Dbl()
    Asoc_Str()
    Asoc_Bol()
    Asoc_Enum()

Estos métodos están sobrecargados, para permitir la asociación con diversos controles. Así por ejemplo, es posible asociar un entero a un control TEdit, pero también se puede asociar a un TSpinEdit.

Después de crear las asociaciones, solo resta llamar a los métodos:

* FileToProperties 
* PropertiesToWindow 
* PropertiesToFile 
* WindowToProperties 

Para realizar el movimiento de datos. Así por ejemplo, lo normal es leer todas las propiedades al iniciar la aplicación, entonces se debe llamar a FileToProperties() en el evento OnCreate o en el evento OnShow (recomendado).

También es común que al terminar la aplicación se llame a PropertiesToFile() para mantener el valor de las variables asociadas.
 
Todo este manejo de las propiedades, se puede hacer en el formulario principal, pero lo recomendable es crear un formulario especial o diálogo, de configuración, de modo que incluya los botones ACPETAR y CANCELAR. En este caso, solo cuando se acepten los cambios se debe llamar a WindowToProperties().

Para ver el código de una implementación de este tipo, se recomienda leer los proyectos de ejemplo que vienen en la librería.
 
### Asociando solamente propiedades a un archivo:
 
Esta forma de trabajo, se puede usar cuando no es necesario editar las propiedades en controles, porque usualmente tienen otros medio para modificarse, como podría ser el ancho o el alto de la ventana principal.

```
 +-----------+                  +-------------+ 
 |           | FileToProperties |             | 
 |           | ---------------> |             | 
 |   Disco   |                  | Variables   | 
 |  (File)   | PropertiesToFile |(Properties) | 
 |           | <--------------- |             | 
 +-----------+                  +-------------+ 
```

### Asociando solamente propiedades a controles:

```
                                +-------------+                    +------------+
                                |             | PropertiesToWindow |            |
                                |             | -----------------> |            |
                                | Variables   |                    | Controles  |
                                |(Properties) | WindowToProperties | (Window)   |
                                |             | <----------------- |            |
                                +-------------+                    +------------+
```

								

Para mayor información, revisar los códigos de ejemplo, de la página web.

## Detectando errores

Comunmente, los errores pueden producirse cuando se colocan valores erróneos en los controles asociados a variables, o cuando se accede a disco. Esto es, cuando se ejecuta alguno de estos métodos:

* FileToProperties 
* PropertiesToWindow 
* PropertiesToFile 
* WindowToProperties 

El objeto TMiConfigINI tiene un campo de cadena, llamado "MsjErr", cuyo objetivo es almacenar el error producido en la última operación.

Así, es común usar el siguiente código en el evento OnClick, del botón ACEPTAR de las ventanas de configuración:

```
procedure TConfig.BitAceptarClick(Sender: TObject);
begin
  iniFile.WindowToProperties;
  if iniFile.MsjErr<>'' then begin
    MsgErr(iniFile.MsjErr);
    exit;
  end;
  self.Close;
end;
```
## Dependencias

Esta librería requiere de la librería MisUtils: https://github.com/t-edson/MisUtils

