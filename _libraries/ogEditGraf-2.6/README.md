ogEditGraf 2.6
==============

Librería en Lazarus, para la creación de editores simples de objetos gráficos.

![SynFacilCompletion](http://blog.pucp.edu.pe/blog/tito/wp-content/uploads/sites/610/2018/04/Sin-título-12.png "Título de la imagen")

Esta librería permite implementar fácilmente un editor de objetos gráficos en dos dimensiones. Los objetos gráficos se crean a partir de una clase base, que incluye las funciones básicas para poder ser manipulados por un editor, con opciones de seleccionar, mover, y redimensionar los objetos. 

Se compone de las unidades:

* ogMotGraf2d.pas -> Es el motor gráfico, en donde se encuentran las rutinas de dibujo. Usa métodos comunes del lienzo (Canvas), pero puede ser cambiado para usar alguna otra librería gráfica.
* ogDefObjGraf.pas -> Es donde se define la clase TObjGraf, que es la clase que se usa para crear a todos los objetos gráficos de nuestra aplicación. También se definen algunos objetos accesorios.
* ogEditionMot.pas -> Es el motor de edición de objetos gráficos. Esta diseñado para trabajar con los objetos TObjGraf o descendientes. Incluye las rutinas para seleccionar, mover y redimensionar objetos con el ratón.
* ogControls.pas -> Unidad que define objetos gráficos que funcionan al estilo de los controles de una GUI típica. También define a la clase TObjGrafCtrls, descendiente de TObjGraf que puede incluir controles.

Para implementar un sencillo editor de objetos gráficos, se puede incluir el siguiente código en el formulario principal:

```
unit Unit1;
{$mode objfpc}{$H+}
interface
uses
  Classes, Forms, Controls, Graphics, ExtCtrls, ogEditionMot, ogDefObjGraf;

type
  //define el tipo de objeto a dibujar
  TMiObjeto = class(TObjGraf)
    procedure Draw; override;
  end;

  TForm1 = class(TForm)
    PaintBox1: TPaintBox;   //donde se dibujará
    procedure FormCreate(Sender: TObject);
  private
    motEdi: TEditionMot;  //motor de edición
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

procedure TMiObjeto.Draw();
begin
  v2d.SetPen(psSolid, 1, clBlack);
  v2d.RectangR(x, y, x+width, y+height);
  inherited;
end;

procedure TForm1.FormCreate(Sender: TObject);
var og: TMiObjeto;
begin
  //Crea motor de edición
  motEdi := TEditionMot.Create(PaintBox1);
  //Agrega objeto
  og := TMiObjeto.Create(motEdi.v2d);
  motEdi.AddGraphObject(og);
end;

end.
```

Este ejemplo mostrará un objeto rectangular en pantalla, con posibilidad de desplazarlo y dimensionarlo.

Las rutinas de dibujo, se dirigen siempre, a un control PaintBox, que debe ser indicado al momento de crear el motor de edición.

Este sencillo ejemplo solo requiere incluir un control TPaintBox en el formulario principal. Sin embargo, para modularizar mejor la aplicación, se sugiere usar una unidad especial para definir los objetos gráficos de nuestra aplicación, y un frame para incluir el PaintBox y las rutinas de trabajo del motor de edición.

### Arquitectura de la librería

Las unidades que componen  la librería siguen una organización particular, que determina también la arquitectura de la aplicación.

Esta librería se basa en los siguientes principios:

* Toda aplicación debe tener un Motor Gráfico.
* Toda aplicación debe tener un Motor de Edición.
* Los objetos a mostrar deben ser descendientes de la clase TObjGraf.


Un proyecto sencillo, requiere solo incluir a la unidad ogEditionMot y a la unidad ogDefObjGraf:

```
        +-------------------------+
        |        Programa         |
        +-------------------------+
             |                |
             |                |
     +------------------+     |
     |   ogEditionMot   |     |       <-------- MOTOR DE EDICIÓN
     +------------------+     |  
       |             |        |
       |       +------------------+
       |       |   ogDefObjGraf   |   <------- Definición de TObjGraf
       |       +------------------+
       |            |
   +-------------------+
   |    ogMotGraf2D    |              <-------- MOTOR GRÁFICO
   +-------------------+
```

La unidad ogMotGraf2D es la que contiene los métodos gráficos que dibujan en pantalla. Es por eso que es accedidad por ogDefObjGraf y ogEditGraf. El programa principal no suele acceder a ogMotGraf2D, porque para dibujar puede acceder al motor gráfico mediante la clase TObjGraf que está definida en ogDefObjGraf.

Un ejemplo de esta forma de trabajo, se encuentra en el proyecto ejemplo "Sample1 - Object Editor" en donde se ve que se ha definido una sola clase de objeto gráfico llamada TMyGraphObject, en la unidad del formulario principal.

En la práctica, sin embargo, se tendrán diversas clases de objetos gráficos, por lo que resulta conveniente agruparlas en una unidad a la que podríamos llamar "ObjGraficos". 

También se suele poner al motor de edición en una Frame en lugar de la propia aplicación. De modo que el programa tendría la siguiente forma:

```
        +-------------------------+
        |        Programa         |
        +-------------------------+
                      |              
        +-------------------------+
        |         Frame           |
        +-------------------------+
             |               |
             |       +-------------+
             |       | ObjGraficos |  <------ Definición de mis objetos gráficos
             |       +-------------+      
             |               |            
     +------------------+    |            
     |   ogEditionMot   |    |        <------ MOTOR DE EDICIÓN
     +------------------+    |            
       |             |       |            
       |       +------------------+       
       |       |   ogDefObjGraf   |   <------ Definición de TObjGraf
       |       +------------------+       
       |            |                     
   +-------------------+                  
   |    ogMotGraf2D    |              <------ MOTOR GRÁFICO
   +-------------------+
```
   
Esta es la forma recomendada por modularidad y tiene además la ventaja de poder crear diversas vistas, creando simplemente diversas instancias del Frame.

Un ejemplo de este diseño, se puede observar en el proyecto ejemplo "Sample2 - Object Editor Frame".

Para más información, revisar los ejemplos.
