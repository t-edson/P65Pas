t-Xpres 1.3.2
=============

Marco de trabajo (framework) para implementar compiladores o intérpretes para  lenguajes imperativos y tipados. 

Esta desarrollado con Free Pascal y  Lazarus.

Xpres no es solo una librería, es también una infraestructura que define normas y procedimientos en la creación de intérpretes o compiladores, usando Lazarus. Como ejemplo, se incluyen implementaciones simples de un intérprete y un compilador.

El framework, incluye las siguientes dependencias:

* Paquete SynEdit. Que viene incluido en Lazarus. Se requiere para poder usar la librería SynfacilSyn. No significa que debe usarse el componente SynEdit, sino que  el resaltador SynfacilSyn, usa clases definidas en este paquete.
* Librería SynFacilSyn https://github.com/t-edson/SynFacilSyn.  Necesaria porque se usará al resaltador de sintaxis SynfacilSyn, como analizador léxico. Xpres no implementa un analizador léxico propio.
* Librería MisUtils https://github.com/t-edson/MisUtils. Usada para permitir la traducción de los mensajes de error.

Xpres está diseñado para trabajar con el paquete SynEdit de Lazarus. Esto implica que para implementar un intérprete/compilador, se debe usar el paquete SynEdit, siempre. La arquitectura planteada no implica el uso de SynEdit como editor, pero si se requiere de algunas clases definidas en el paquete SynEdit, para el analizador léxico.

Además, al ser el analizador léxico, también un resaltador de sintaxis, se puede resaltar el código fuente con las mismas rutinas del analizador léxico, sin necesidad de implementar algún otro resaltador. De esta forma se garantiza una correspondencia al 100% entre los tokens del analizador léxico, y el coloreado que se puede lograr en pantalla.

Hay que notar que de SynFacilSyn, solo se está usando su capacidad de lexer, más no de manejo de bloques de sintaxis. De momento los bloques de sintaxis en Xpres, los maneja internamente el analizador sintáctico.

La librería Xpres, incluye a los siguientes archivos:

* "XpresBas.pas". Unidad con rutinas básicas del framework. Incluye el métodos para el manejo del texto fuente y el procesamiento de errores. Por lo general no debería modificarse. Funciona como una capa que se coloca sobre el analizador léxico o "lexer".
* "XpresTypes.pas". Unidad con las definiciones referidas a los tipos-operadores-operaciones. Es también una unidad básica del framework.
* "XpresParser.pas". Unidad con rutinas principales del framework. Incluyen el analizador sintáctivo o "parser". Incluye el analizador de expresiones y de las estructuras del lenguaje. No debería cambiar si el lenguaje sigue la línea del lenguaje Xpres.
* "XpresElements.pas". Unidad que define los elementos de la sintaxis, como constantes, variables, procedimientos y funciones.

Para la implementación de un Intérprete o Generador de código. Se debe crear una unidad que incluya a "XpresParser.pas" y "XpresTypes.pas" y ahí definir a  una clase (p. ej. TCompiler o TInterpreter) que descienda de la clase TCompilerBase.

Los generadores de código pueden desarrollarse para generar código intermedio, como el bytecode de Java, o cualquier otro. Este proyecto no incluye ninguna máquina virtual.

En los ejemplos se incluye un caso minimalista con intérprete y un ejemplo de compilador elemental para el intel 8086 en 16 bits.

El ejemplo de compilador incluye también una IDE sencilla. Solo permite manejar variables, expresiones numéricas y de cadena.

Xpres, puede ser usado también cuando se requiere implementar un lexer, un parser, un árbol de sintaxis, o un evaluador de expresiones.

Para más información sobrfe Xpres, revisar la documentación técnica.
