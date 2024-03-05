UtilsGrilla 0.8
===============

Librería con utilidades para el manejo y configuración de grillas de tipo TStringGrid 

Descripción
===========

La librería consiste en dos unidades y un Frame:

* BasicGrilla.pas -> Rutinas básicas de manejo de la grilla.
* UtilsGrilla.pas -> Definición del objeto TUtilGrilla.
* FrameFiltCampo.pas -> Frame para filtrado de filas.

No todas las unidades tienen que incluirse, Por lo general solo será necesario usar "UtilsGrilla".

## TUtilGrilla ##

La clase principal de la librería es TUtilGrilla. Este objeto es un envoltorio que permite administrar fácilmente una grilla de tipo TStringGrid.

Las facilidades que ofrece TUtilGrilla son:

- Mejora el desplazamiento de teclado, permitiendo usar <Ctrl>+teclas direccionales, para desplazarse hasta los extremos de la grilla.
- Permite la creación sencilla de encabezados, Usando  métodos como AgrEncabTxt() y AgrEncabNum().
- Permite opciones de alineamiento, del texto dentro de la celda.
- Permite ocultar fácilmente ciertas columnas.
- Permite activar opciones comunes (como el dimensionado del ancho de las columnas), de forma sencilla, sin necesidad de usar conjuntos.
- Permite crear estructuras de encabezados diversas para una misma grilla.
- Permite asociar una columna de una grilla a un índice, para facilitar la carga de campos desde un archivo de texto o desde base de datos.
- Brinda un soporte para posteriormente incluir, filtros por filas, en la grilla.
- Incluye funciones de conversión de tipos, de modo que, por ejemplo, permiten leer o escribir valores de tipo "booleano", directamente desde o hacia la grilla.

Para trabajar con una grilla se tiene dos formas:

1. Asociándola a una grilla desde el inicio:

```
  UtilGrilla := TUtilGrilla.Create(StringGrid1);
  UtilGrilla.IniEncab;
  UtilGrilla.AgrEncab('CAMPO1' , 40);  //Con 40 pixeles de ancho
  UtilGrilla.AgrEncab('CAMPO2' , 60);  //Con 60 pixeles de ancho
  UtilGrilla.AgrEncab('CAMPO3' , 35, -1).alineam := taRightJustify); //Justificado a la derecha
  UtilGrilla.FinEncab;
  ...
```

2. Sin asociarla a una UtilGrilla:

```
  UtilGrilla := TUtilGrilla.Create;
  UtilGrilla.IniEncab;
  UtilGrilla.AgrEncab('CAMPO1' , 40);  //Con 40 pixeles de ancho
  UtilGrilla.AgrEncab('CAMPO2' , 60);  //Con 60 pixeles de ancho
  UtilGrilla.AgrEncab('CAMPO3' , 35, -1).alineam := taRightJustify; //Justificado a la derecha
  UtilGrilla.FinEncab;
  ...
```

En esta segunda forma, se debe asociar posteriormente a la UtilGrilla, usando el método:
   UtilGrilla.AsignarGrilla(MiGrilla);

, haciendo que la grilla tome los encabezados que se definieron en "UtilGrilla". De esta forma se pueden tener diversos objetos TUtilGrilla, para usarse en un solo objeto
TStringGrid.

Existen diversas opciones que se pueden cambiar directamente en TUtilGrilla, sin necesidad de configurar al TStringGrid, directamente. 

Algunas de estas opciones son:

  MenuCampos: boolean        //Activa o desactiva el menú contextual
  OpDimensColumnas: boolean  //activa el dimensionamiento de columnas
  OpAutoNumeracion: boolean  //activa el autodimensionado en la columna 0
  OpResaltarEncabez: boolean //Resalta el encabezado, cuando se pasa el mouse
  OpEncabezPulsable: boolean //Permite pulsar sobre los encabezados como botones
  OpOrdenarConClick: boolean //Ordenación de filas pulsando en los encabezados
  OpResaltFilaSelec: boolean //Resaltar fila seleccionada

La propiedad MenuCampos, permite mostrar un menú PopUp, cuando se pulsa el botón derecho sobre la fila de los encabezados de la grilla. Dicho menú permite mostrar u ocultar las columnas de la grilla.

Para el manejo de filtros, TUtilGrilla, incluye los siguientes métodos:

  LimpiarFiltros;  //Elimina todos los filtros internos
  AgregarFiltro(); //Agrega un filtro a TUtilGrilla
  Filtrar;	       //Filtra las filas, usando los filtros ingresados.

Los filtros se ingresan como referencias a funciones que deben devolver TRUE, si la fila pasa el filtro y FALSE, en caso contrario. 

Todos los filtros agregados, se evalúan en cortocicuito, usando el operador AND.


## TUtilGrillaFil ##

![SynFacilCompletion](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2017/02/Sin-título-1.png "Título de la imagen")

Esta clase es similar a TUtilGrilla, pero se maneja a la grilla, por filas, permitiendo cambiar el color de fondo, del texto o los atributos de las filas, de forma independiente.

Incluye las mismas facilidades de TUtilGrilla, pero adicionalmente:
- Configura la selección por filas, aunque mantiene identificada a la celda seleccionada.
- Permite cambiar atributos de las filas (color de fondo, color de texto y atributos de texto)
- Permite activar y desactivar  la selección múltiple de filas.
- Permite crear columnas que muestren íconos en lugar de texto.

Su uso es similar al de TUtilGrilla:

```
  UtilGrilla:= TUtilGrillaFil.Create(StringGrid1);
  UtilGrilla.IniEncab;
  UtilGrilla.AgrEncabTxt('CAMPO0', 40);  //Con 40 pixeles de ancho
  UtilGrilla.AgrEncabTxt('CAMPO1', 60);  //Con 60 pixeles de ancho
  UtilGrilla.AgrEncabNum('CAMPO2', 60); //Campo numérico, justificado a la derecha
  UtilGrilla.AgrEncabIco('ÍCONO' , 60).alineam:=taCenter; //Ícono centrado
  UtilGrilla.FinEncab;
  UtilGrilla.ImageList := ImageList1;  //Íconos a usar
```

 Para mayor información, se recomienda ver el proyecto ejemplo.


## FrameFiltCampo ##

![SynFacilCompletion](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2017/02/Sin-título.png "Título de la imagen")

Como un complemento para el manejo de grillas, se incluye el frame FrameFiltCampo.pas, que se comporta como un componente para realizar búsquedas filtrando las filas que no coincidan con el criterio de búsqueda. El algortimo de búsqueda está optimizado para manejar varios miles de filas sin retraso notorio.

Para usar el frame, como cuadro de búsqueda, solo hay que colocarlo en el formulario, donde se encuentra la grilla, como se muestra en la figura. Al colocar el frame, se debe redimensionarlo pensando en que debe contener un cuadro de búsqueda y un combo para elegir el campo de búsqueda. Por defecto el frame viene con un tamaño muy superior al que suele usarse.

### Configuración simple ###

Para que un TfraFiltCampo, trabaje como filtro, se puede configurar de dos formas. La forma simple consiste en asociarla directamente a un objeto TUtilGrillaFil:

```
  fraFiltCampo1.Inic(UtilGrilla, 4);   //asocia a un TUtilGrilla
```
  
Y no se necesita nada más. Esta definición asocia el frame a la grilla, configura todos los campos de la grilla, como parte del filtro, y elige el campo 4 (segundo parámetro), como campo por defecto para el filtro. Además configura el evento OnCambiaFiltro, para que se filtre la grilla en cada pulsación de tecla.

Luego de esta configuración, solo bastaría con escribir sobre el editor del frame, para que automáticamente se empiecen a filtrar las filas en la grilla. El filtrado consiste en ocultar las filas que no cumplen con el criterio de búsqueda, se mantienen con un altura predefinida. 

### Configuración detallada ###

La forma más detallada, la que permite más libertad, sería:

```
  fraFiltCampo1.Inic(StringGrid1);   //asocia a grilla
  fraFiltCampo1.LeerCamposDeGrilla(UtilGrilla.cols, 1);  //configura menú de campos
  UtilGrilla.AgregarFiltro(@fraFiltCampo1.Filtro);  //agrega el filtro
```

Adicionalmente, para determinar cuando cambia el filtro (sea porque se ha modificado el texto de búsqueda o se cambia el campo de trabajo), se debe interceptar el método "OnCambiaFiltro". 

```
  fraFiltCampo1.OnCambiaFiltro:=@fraFiltCampo_CambiaFiltro;
```

Luego, lo más común sería que este método, llame al método Filtrar() de UtilGrilla.

Hay que notar que es posible agregar varios FrameFiltCampo, a un TUtilGrilla, y que funcionen a modo de filtro en cascada.

### Tipos de búsqueda con TfraFiltCampo ###

El tipo de búsueda por defecto, con TfraFiltCampo, consiste en que se buscarán en todas las filas, en el campo seleccionado, para ver si el contenido de texto mostrado (sea numérico, booleano, etc), incluye al texto escrito como palabra de búsqueda en el frame TfraFiltCampo (específicamente en el control TEdit, del frame).

Es decir, que la búsqueda es equivalente al operador LIKE del lenguaje SQL, cuando trabaja con campos de texto.

Sin embargo, cuando la búsqueda se hace en campos numéricos, es posible usar los operadores de comparación: =, <, >, <=, >= o <>. 

Así por ejemplo, si se escribe en el frame: ">5", se hará la comparación, en el campo indicado, comparando la cantidad a modo de número, mostrando las filas que etngan ese campo como un número mayor a 5.


### TfraFiltCampo con StringGrid ###

Si bien TfraFiltCampo, se ha creado para trabajar con un objeto TUtilGrilla, también es posible usarlo, con un TStringGrid común:

```
  fraFiltCampo1.Inic(StringGrid1);
  fraFiltCampo1.AgregarColumnaFiltro('Por columna A', 1);
  fraFiltCampo1.AgregarColumnaFiltro('Por columna B', 2);
  fraFiltCampo1.OnCambiaFiltro:=@fraFiltCampo1CambiaFiltro;
```

En este caso habría que implementar en fraFiltCampo1CambiaFiltro(), el código que realice el filtrado sobre la grilla.

### Objeto TListaCompletado ###

Esta clase no tiene que ver con grillas, pero se incluye como una utilidad adicional.

TListaCompletado permite implementar la funcionalidad de lista desplegable cuando se llena un control TEdit. 

En su forma de trabajo normal, se requiere de una grilla en donde deben estar los valores que se van a usar para llenar la lista de completado.

Para usarlo se debe primero crear el objeto: 

```
  completProv := TListaCompletado.Create;
```

Luego se debe asociarlo a un TEdit, indicando una grilla, y una columna, de donde se debe extraer los valores para la lista de completado del TEdit.

```
  completProv.Inic(txtEdit, grilla_datos, col_de_grilla_datos);
```
  
Opcionalmente se puede definir los eventos OnSelect y OnEditChange:

```
  completProv.OnSelect := @Proveed_Seleccionado;
  completProv.OnEditChange := @Proveed_EditChange;
```

La utilidad principal del evento OnSelect, es fijar el enfoque al siguiente control después de haber seleccionado un valor para el TEEdit.

Finalmente  se debe destruir el objeto:

```
  completProv.Destroy;
```

Los valores a mostrar en la lista se obtienen usando la columna indciada de la grilla, y filtrando por el valor contenido en el TEdit.

Si no se quiere usar una grilla como fuente de datos, se puede usar el evento OnLlenarLista, para implementar una rutina personalizada de llenado. Un ejemplo de rutina de llenado sería:

```
procedure TfrmIngTareo.Edit1_LlenarLista;
var
  lista: TListBox;
begin
  lista := completEdit1.listBox;  //Accede a la lista de completado.
  lista.Clear;

  if CumpleFiltro('aaa' , Edit1.Text) then
    lista.AddItem('aaa', nil);
  if CumpleFiltro('bbb' , Edit1.Text) then
    lista.AddItem('bbb', nil);

  if lista.Count>0 then
    lista.ItemIndex:=0;  //selecciona el primer elemento
end;
```

La función CumpleFiltro(), es una utilidad definida en BasicGrilla, que permite determinar si un texto cumple incluye otro texto.
