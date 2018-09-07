//  
//  (C) AguHDz 01-AGO-2017
//  Ultima Actualizacion: 20-SEP-2017
//  
//  Librería : UARTSoft.pas
//  
//  Compilador PicPas v.0.7.7 (https://github.com/t-edson/PicPas)
//  
//  LIBRERIA DE COMUNICACION SERIE RS232 (UART) MEDIANTE SOFTWARE
//  =============================================================
//  Envío y recepción de caracteres ASCII a través del puerto serie RS232.
//  
//  Esta librería (UARTSoft) crea una unidad UART (Universal Asynchronous
//  Receiver-Transmitter) mediante software.
//  
//  Cualquier pin de los puertos I/O del microcontrolador es valido para
//  configurarse como línea de Transmisión (TX) o Recepción (RX) de
//  datos.
//  
//  Configurar los pines en el programa mediante las directivas:
//  
//  {$SET UART_PIN_RX = 'PORTB.7'}   // PUERTO DE RECEPCION RDX 
//  {$SET UART_PIN_TX = 'PORTB.6'}   // PUERTO DE TRANSMISION TDX
//  
//  Se utiliza el protocolo RS-232 en su configuracion más comúnn: 8 bits de
//  datos, 1 bit de Stop, sin paridad ni control de flujo (solo 2 hilos de
//  comunicación). La velocidad, en bits por segundo (bps o baudios), se
//  configura en el programa mediante la directiva:
//  
//  {$SET BAUDIOS = 9600}     // VELOCIDAD DE COMUNICACION SERIE
//  
//  Cualquier velocidad es válida, siempre que la combinación velocidad de
//  trabajo del microcontrolador (fijaca con {$FREQUENCY XXMhz}) y los baudios
//  no den lugar a un número mayor de 255 (un byte) en los cálculos de ciclos 
//  de espera que realiza la librería, en cuyo caso, lo advierte con un mensaje
//  de error durante el tipo de compilación para se disminuya la velocidad de
//  reloj del microcontrolador o se eleve la velocidad de la comunicación.
//  
//  En caso de microcontroladores PIC que dispongar de UART integrada en su
//  hardware, esta librería se puede utilizar para crear un segundo puerto de
//  comunicaciones serie, para uso de la aplicación o para su uso durante la
//  depuración del programa.
//
//  La librería completa solo ocupa 64 bytes.
//  

unit UARTSoftLib;

interface

{$SET DEBUG_LIBRARY_UARTSOFT = 'ON'}   // Activa modo pruebas (DEBUG) de librería.
//{$SET DEBUG_LIBRARY_UARTSOFT = 'OFF'}    // Para usar la librería, SIEMPRE en OFF.

{$IF DEBUG_LIBRARY_UARTSOFT = 'ON'}
  {$PROCESSOR PIC16F84A}
  {$FREQUENCY 12MHz}
  {$SET BAUDIOS = 9600}              // VELOCIDAD DE COMUNICACION SERIE
  {$SET UART_PIN_RX = 'PORTB_RB7'}   // PUERTO DE RECEPCION RDX 
  {$SET UART_PIN_TX = 'PORTB_RB6'}   // PUERTO DE TRANSMISION TDX
{$ENDIF}

{$SET TIME_BITDELAY = 1/BAUDIOS}
{$SET TIME_MEDIOBITDELAY = TIME_BITDELAY / 2}
{$SET TIME_CICLO_MAQUINA = 4/PIC_FREQUEN}
{$SET CICLOS_DELAY = TIME_MEDIOBITDELAY/TIME_CICLO_MAQUINA}
// 2 ciclos de llamada a función delay.
// 2 ciclos de cargar de valores inciales de cuenta.
// 3*(CICLOS_DELAY_LOOP-1))+2 ciclos del bucle contador.
// 2 ciclos de retorno de función.
// Por lo tanto: CICLOS_DELAY = 2 + 2 + 3*(CICLOS_DELAY_LOOP-1))+2 + 2 = 8+3*(CICLOS_DELAY_LOOP-1)
//	  movlw	     CICLOS_DELAY_1
//	  movwf	     d1
//  Delay_0:               
//	  decfsz     d1, f
//	  goto       Delay_0
{$SET CICLOS_DELAY_LOOP = ROUND((((CICLOS_DELAY) - 8 ) / 3) + 1)}

// ******* MENSAJES DE ERROR PARA CONTROLAR LIMITES DE USO **************
{$IF CICLOS_DELAY_LOOP > 255}
  {$ERROR "Desbordamiento de contador de tiempo en función BITDELAY.\nPara solucionarlo, debes seleccionar una frecuenca de trabajo del microcontrolador menor {$FRECUENCY} o una velociada de comunicacion (BAUDIOS) mayor"}
{$ENDIF}
{$IF CICLOS_DELAY_LOOP < 4}
  {$ERROR "Contador de tiempo en función BITDELAY muy pequeño.\nPara solucionarlo, debes seleccionar una frecuenca de trabajo del microcontrolador mayor {$FRECUENCY} o una velociada de comunicacion (BAUDIOS) menor"}
{$ENDIF}
// **********************************************************************

{$IF PIC_MODEL='DEFAULT'}
  {$ERROR 'Debe seleccionar un modelo de Microcontralador al inicio de su programa mediante la directiva $PROCESSOR.}  
{$ENDIF}

{$IF DEBUG_LIBRARY_UARTSOFT = 'ON'}
  {$MSGBOX "TIME_MEDIOBITDELAY = " + TIME_MEDIOBITDELAY + "\nCICLOS_DELAY = " + CICLOS_DELAY + "\nCICLOS_DELAY_LOOP = " + CICLOS_DELAY_LOOP}
  uses {$PIC_MODEL};  // Parece que a partir de la versión 0.7.7 ya no es necesario volver a incluir la librería, e incluso produce un error. ¿?
{$ENDIF}
const
  DataBitCount = 8;        // 8 bits de datos, sin paridad ni control de flujo.
  Char_LF      = Chr(10);  // LF/NL	(Line Feed/New Line) - Salto de Linea.
  Char_CR      = Chr(13);  // CR (Carriage Return) - Retorno de Carro.
  HIGH_LEVEL   = 1;        // Nivel alto (uno logico)
  LOW_LEVEL    = 0;        // Nivel bajo (cero logico)

var
  UART_RX : bit absolute {$UART_PIN_RX};
  UART_TX : bit absolute {$UART_PIN_TX};

procedure MedioBitDelay;
procedure BitDelay;
procedure UARTSoft_Init;
procedure UARTSoft_SendChar(register dato : char);
procedure UARTSoft_GetChar : char;

implementation
  
// -----------------------------------------------------------------
// Procedure MEDIOBITDELAY
// Si baudrate   = 9600 bits per second (BPS)
// Entoces Delay = 0.000052084 seconds (1/Baudrate/2).
// -----------------------------------------------------------------
procedure MedioBitDelay;
const
   CICLOS_DELAY_1 = {$CICLOS_DELAY_LOOP};
var
  d1 : byte;
begin
{$SET CONTADOR_CICLOS_DELAY = 8 + (CICLOS_DELAY_LOOP-1)*3}
  ASM   
    movlw      CICLOS_DELAY_1
    movwf      d1
  Delay_0:               
    decfsz     d1, f
    goto       Delay_0
  END
{$IF DEBUG_LIBRARY_UARTSOFT = 'ON'}
  {$MSGBOX "CONTADOR_CICLOS_DELAY = " + CONTADOR_CICLOS_DELAY}
{$ENDIF}
// Si es necesario, ajusta añadiendo 2 ciclos.
{$IF (CICLOS_DELAY - CONTADOR_CICLOS_DELAY) >= 2}
  ASM           
    goto $+1
  END
  {$SET CONTADOR_CICLOS_DELAY = CONTADOR_CICLOS_DELAY + 2}
  //{$MSGBOX "CONTADOR_CICLOS_DELAY + 2 = " + CONTADOR_CICLOS_DELAY}
{$ENDIF}
// Si fuera necesario, ajusta añadiendo otros 2 ciclos.
{$IF (CICLOS_DELAY - CONTADOR_CICLOS_DELAY) >= 2}
  ASM           
    goto $+1    ; Por si se diera la combinación.
  END
  {$SET CONTADOR_CICLOS_DELAY = CONTADOR_CICLOS_DELAY + 2}
  //{$MSGBOX "CONTADOR_CICLOS_DELAY + 2 = " + CONTADOR_CICLOS_DELAY}
{$ENDIF}
// Si es necesario, ajusta añadiendo 1 ciclo. (con redondeo de diferencia para minimizar error)
{$IF (CICLOS_DELAY - CONTADOR_CICLOS_DELAY) > (1/2)}
  ASM
    nop
  END
  {$SET CONTADOR_CICLOS_DELAY = CONTADOR_CICLOS_DELAY + 1}
  //{$MSGBOX "CONTADOR_CICLOS_DELAY + 1 = " + CONTADOR_CICLOS_DELAY}
{$ENDIF}
{$IF DEBUG_LIBRARY_UARTSOFT = 'ON'}
  {$MSGBOX "CICLOS_DELAY = " + CICLOS_DELAY + "\nCONTADOR_CICLOS_DELAY = " + CONTADOR_CICLOS_DELAY} 
{$ENDIF}
end;

// -----------------------------------------------------------------
// Procedure BITDELAY
// Delay = 1/Baudrate
// -----------------------------------------------------------------
procedure BitDelay;
const
   // Resta 6 ciclos para compensar el tiempo de la llamada a MedioBitDelay y su retorno.
   CICLOS_DELAY_1 = {$CICLOS_DELAY_LOOP} - 2;
   // Se puede poner tambien CICLOS_DELAY_1 = {$CICLOS_DELAY_LOOP} * 2 pero es más facil que
   // el resultado sea un númeor mayor de 255 y por lo tanto no se pueda utilizar esa
   // combinación de velocidad de micro y baudios con esta librería.
var
  d1 : byte;
begin
{$SET CONTADOR_CICLOS_DELAY = 8 + (CICLOS_DELAY_LOOP-1)*3}
  ASM 
    call       MedioBitDelay ; 4 ciclos +
    goto       $+1           ; 2 ciclos = 6 ciclos (que son los eliminados del loop delay)
    movlw      CICLOS_DELAY_1
    movwf      d1
  Delay_0:               
    decfsz     d1, f
    goto       Delay_0
  END
// Si es necesario, ajusta añadiendo 2 ciclos.
{$IF (CICLOS_DELAY - CONTADOR_CICLOS_DELAY) >= 2}
  ASM           
    goto $+1
  END
{$ENDIF}
// Si fuera necesario, ajusta añadiendo otros 2 ciclos.
{$IF (CICLOS_DELAY - CONTADOR_CICLOS_DELAY) >= 2}
  ASM           
    goto $+1    ; Por si se diera la combinación.
  END
{$ENDIF}
// Si es necesario, ajusta añadiendo 1 ciclo. (con redondeo de diferencia para minimizar error)
{$IF (CICLOS_DELAY - CONTADOR_CICLOS_DELAY) > (1/2)}
  ASM
    nop
  END
{$ENDIF}
end;

{
// Primera opción asumiendo el pequeño error de tiempo por llamadas y retornos de funciones.
// Funciona en varias combinaciones de velocidades de micro y baudios, pero es menos preciso.
procedure BitDelay;
begin
  MedioBitDelay;  // Delay = 1/Baudrate/2
  MedioBitDelay;  // Delay = 1/Baudrate/2
end;
}

// -----------------------------------------------------------------
// Procedure UARTSOFT_INIT
// Inicializa los pines de comunicacion serie.
// -----------------------------------------------------------------
procedure UARTSoft_Init;
begin
  SetAsOutput(UART_TX);    // Salida.
  SetAsInput(UART_RX);     // Entrada.
  UART_TX := HIGH_LEVEL;   // Pone a 1 la linea TX.
end;

// -----------------------------------------------------------------
// Procedure UARTSOFT_SENDCHAR
// Envia un caracter por el puerto serie (UART).
// -----------------------------------------------------------------
procedure UARTSoft_SendChar(register dato : char);
var
  contador, dataValue : byte;
begin
  dataValue := Ord(dato);            // Conversión de carácter de entrada a variable tipo byte.
  contador  := 0;                    // Inicializa contador de bits de datos.
  UART_TX   := LOW_LEVEL;            // Comienza la transmision.
  BitDelay;                          // Tiempo en nivel lógico bajo de la línea de transmisión (TX).
  
  repeat                             // Envía los 8 bits de datos.
    UART_TX   := dataValue.0;        // La línea de transmisión toma el estado del bit de datos correspondiente.
    BitDelay;                        // Espera con estado de bit de datos en la línea de transmisión (TX).
    dataValue := dataValue>>1;       // Desplaza a la derecha el byte de datos para el siguiente vuelta enviar el bit.
    Inc(contador);                   // Incrementa contador de bits de datos.
  until (contador = DataBitCount);   // Acaba cuando se han transmitido los 8 bits de datos.
  
  UART_TX  := HIGH_LEVEL;            // Envía el bit de Stop.
  BitDelay;                          // Espera con estado de bits de Stop en linea de transmisión (TX).
end;

// -----------------------------------------------------------------
// Procedure UARTSOFT_GETCHAR
// Espera y lee un caracter enviado por el puerto serie (UART).
// -----------------------------------------------------------------
procedure UARTSoft_GetChar : char;
var
  contador, dataValue : byte;
begin
  contador  := 0;                    // Inicializa contador de bits de datos.
  dataValue := 0;                    // Inicializa a cero la variable que va a contener el byte recibido.
  repeat until(UART_RX = LOW_LEVEL); // Espera hasta detección de inicio la transmisión.
  BitDelay;                          // Espera el tiempo del bit de inicio de transmisión.
  MedioBitDelay;                     // Espera 1/2 tiempo de transmisión para hacer la lectura en un punto central del pulso.

  repeat                             // Recibe los 8 bits de datos.
    dataValue   := dataValue>>1;     // Desplaza a la derecha el dato parcialmente recibido antes de añadir un nuevo bit.
    dataValue.7 := UART_RX;          // Añade bit de datos recibido.
    BitDelay;                        // Tiempo de espera antes de detectar estado del siguiente bit de datos.
    Inc(contador);                   // Incrementa contador de bits de datos.
  until (contador = DataBitCount);   // Acaba cuando se han recibido los 8 bits de datos.
  
  // Comprueba correcta recepción mediante bit de Stop.
  // Aquí se podría añadir en su caso la deteccion de los bits de paridad.
  if (UART_RX = HIGH_LEVEL) then     // Bit de Stop debe ser un uno lógico.
    MedioBitDelay;                   // Espera final para completar el tiempo de la trama de bits completa.
    exit(Chr(DataValue));            // Devuelve el dato leído.
  else                               // Ha ocurrido algun error !
    MedioBitDelay;                   // Espera final para completar el tiempo de la trama de bits completa.
    exit(Chr(0));                    // Si detecta error devuelve el valor cero.
  end;
end; 

end.
