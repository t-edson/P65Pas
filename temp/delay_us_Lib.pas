//  
//  (C) AguHDz 30-SEP-2017
//  Ultima Actualizacion: 01-OCT-2017
//  
//  Librería : delay_us_Lib.pas
//  
//  Compilador PicPas v.0.7.8 (https://github.com/t-edson/PicPas)
//  
//  LIBRERIA PAUSAR PROGRAMA DURANTE MICROSEGUNDOS
//  ==============================================
//  PicPas incluye la función de sistema delay_ms() para pausar el 
//  programa durante microsegundos. Sin embargo, hay ocasiones
//  en se requieren pausar de microsegundos, especialmente cuando
//  se programan protocolos de comunicación con hardware exterior.
//  Esta librería automatiza la programación de pausas menores de
//  un milisegundo, para cualquier velocidad de reloj, utizando funciones
//  macro que insertarán en el programa el código de espera más 
//  eficiente en función de la velocidad de reloj del sistema y de
//  la pausa en microsegundos que se requiera.
//  

unit delay_us_Lib;

interface

//{$SET DEBUG_LIBRARY_UARTSOFT = 'ON'}   // Activa modo pruebas (DEBUG) de librería.
{$SET DEBUG_LIBRARY_UARTSOFT = 'OFF'}    // Para usar la librería, SIEMPRE en OFF.

{$IF DEBUG_LIBRARY_UARTSOFT = 'ON'}
  {$PROCESSOR PIC16F84A}
  {$FREQUENCY 8 MHz}
{$ENDIF}

{$SET TIME_US_CICLO_MAQUINA = 4/PIC_FREQUEN*1000000}
{$SET CICLOS_DELAY_1_US = 1/TIME_US_CICLO_MAQUINA}

{$SET CICLOS_001_US = ROUND(1/TIME_US_CICLO_MAQUINA)}
{$SET CICLOS_010_US = ROUND(10/TIME_US_CICLO_MAQUINA)}
{$SET CICLOS_010F_US = CICLOS_010_US-4}                 // Se decuentas los 4 ciclos de llamada a función.
{$SET CICLOS_100_US = ROUND(100/TIME_US_CICLO_MAQUINA)}
{$SET CICLOS_100F_US = CICLOS_100_US-4}                 // Se decuentas los 4 ciclos de llamada a función.

{$SET NUEVA_LINEA = "\n\r"}
{$SET ASM = NUEVA_LINEA + "ASM" + NUEVA_LINEA}
{$SET END = "END "}
{$SET DELAY_001_CICLOS = ASM + "NOP" + NUEVA_LINEA + END}
{$SET DELAY_002_CICLOS = ASM + "GOTO $+1" + NUEVA_LINEA + END}

{$SET DELAY_LOOP_CICLOS = ""}
{$SET DELAY_LOOP_CICLOS = DELAY_LOOP_CICLOS + "  movwf      d1"      + NUEVA_LINEA}
{$SET DELAY_LOOP_CICLOS = DELAY_LOOP_CICLOS + "Delay_0:"             + NUEVA_LINEA}
{$SET DELAY_LOOP_CICLOS = DELAY_LOOP_CICLOS + "  decfsz     d1, f"   + NUEVA_LINEA}
{$SET DELAY_LOOP_CICLOS = DELAY_LOOP_CICLOS + "  goto       Delay_0" + NUEVA_LINEA}
{$SET DELAY_LOOP_CICLOS = DELAY_LOOP_CICLOS + END}

//*************** DEFINICION DELAY_1_US ******************************//
{$SET CONTADOR_CICLOS_DELAY_001_US = CICLOS_001_US}
{$IF CICLOS_001_US < 1}
  {$SET DELAY_001_US = DELAY_001_CICLOS}   // INSERTA LA PAUSA MINIMA = 1 CICLO.
{$ELSE}
  {$SET DELAY_001_US = ""}
{$ENDIF}
{$IF CONTADOR_CICLOS_DELAY_001_US > 1}
  {$SET DELAY_001_US = DELAY_001_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_001_US = CONTADOR_CICLOS_DELAY_001_US - 2}
{$ENDIF}
{$IF CONTADOR_CICLOS_DELAY_001_US > 1}
  {$SET DELAY_001_US = DELAY_001_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_001_US = CONTADOR_CICLOS_DELAY_001_US - 2}
{$ENDIF}
{$IF CONTADOR_CICLOS_DELAY_001_US > 0}
  {$SET DELAY_001_US = DELAY_001_US + DELAY_001_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_001_US = CONTADOR_CICLOS_DELAY_001_US - 1}
{$ENDIF}
{$SET DELAY_1_US = DELAY_001_US}
//*********************************************************************//

//*************** DEFINICION DELAY_010F_US ******************************//
// 10 us - los 4 ciclos de llamada a función
{$SET DELAY_010F_US = ""}

{$SET CONTADOR_CICLOS_DELAY_010F_US = 0}
{$IF CICLOS_010F_US > 10}
  {$SET CICLOS_DELAY_US_LOOP_010F_US = 1 + TRUNC((((CICLOS_010F_US) - 8 ) / 3) + 1)}
  {$SET DELAY_010F_US = ASM}
  {$SET DELAY_010F_US = DELAY_010F_US + "movlw " + CICLOS_DELAY_US_LOOP_010F_US + NUEVA_LINEA}
  {$SET DELAY_010F_US = DELAY_010F_US + DELAY_LOOP_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_010F_US = CONTADOR_CICLOS_DELAY_010F_US + (4+3*(CICLOS_DELAY_US_LOOP_010F_US-1))}
{$ENDIF}

{$IF (CICLOS_010F_US - CONTADOR_CICLOS_DELAY_010F_US) >= 2}
  {$SET DELAY_010F_US = DELAY_010F_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_010F_US = CONTADOR_CICLOS_DELAY_010F_US + 2}
{$ENDIF}
{$IF (CICLOS_010F_US - CONTADOR_CICLOS_DELAY_010F_US) >= 2}
  {$SET DELAY_010F_US = DELAY_010F_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_010F_US = CONTADOR_CICLOS_DELAY_010F_US + 2}
{$ENDIF}
{$IF (CICLOS_010F_US - CONTADOR_CICLOS_DELAY_010F_US) >= 2}
  {$SET DELAY_010F_US = DELAY_010F_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_010F_US = CONTADOR_CICLOS_DELAY_010F_US + 2}
{$ENDIF}
{$IF (CICLOS_010F_US - CONTADOR_CICLOS_DELAY_010F_US) >= 2}
  {$SET DELAY_010F_US = DELAY_010F_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_010F_US = CONTADOR_CICLOS_DELAY_010F_US + 2}
{$ENDIF}
{$IF (CICLOS_010F_US - CONTADOR_CICLOS_DELAY_010F_US) >= 2}
  {$SET DELAY_010F_US = DELAY_010F_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_010F_US = CONTADOR_CICLOS_DELAY_010F_US + 2}
{$ENDIF}
{$IF (CICLOS_010F_US - CONTADOR_CICLOS_DELAY_010F_US) > (1/2)}
  {$SET DELAY_010F_US = DELAY_010F_US + DELAY_001_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_010F_US = CONTADOR_CICLOS_DELAY_010F_US + 1}
{$ENDIF}
//*********************************************************************//

//*************** DEFINICION DELAY_010_US ******************************//
{$SET DELAY_010_US = ""}

{$SET CONTADOR_CICLOS_DELAY_010_US = 0}
{$IF CICLOS_010_US > 10}
  {$SET CICLOS_DELAY_US_LOOP_010_US = 1 + TRUNC((((CICLOS_010_US) - 8 ) / 3) + 1)}
  {$SET DELAY_010_US = ASM}
  {$SET DELAY_010_US = DELAY_010_US + "movlw " + CICLOS_DELAY_US_LOOP_010_US + NUEVA_LINEA}
  {$SET DELAY_010_US = DELAY_010_US + DELAY_LOOP_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_010_US = CONTADOR_CICLOS_DELAY_010_US + (4+3*(CICLOS_DELAY_US_LOOP_010_US-1))}
{$ENDIF}

{$IF (CICLOS_010_US - CONTADOR_CICLOS_DELAY_010_US) >= 2}
  {$SET DELAY_010_US = DELAY_010_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_010_US = CONTADOR_CICLOS_DELAY_010_US + 2}
{$ENDIF}
{$IF (CICLOS_010_US - CONTADOR_CICLOS_DELAY_010_US) >= 2}
  {$SET DELAY_010_US = DELAY_010_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_010_US = CONTADOR_CICLOS_DELAY_010_US + 2}
{$ENDIF}
{$IF (CICLOS_010_US - CONTADOR_CICLOS_DELAY_010_US) >= 2}
  {$SET DELAY_010_US = DELAY_010_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_010_US = CONTADOR_CICLOS_DELAY_010_US + 2}
{$ENDIF}
{$IF (CICLOS_010_US - CONTADOR_CICLOS_DELAY_010_US) >= 2}
  {$SET DELAY_010_US = DELAY_010_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_010_US = CONTADOR_CICLOS_DELAY_010_US + 2}
{$ENDIF}
{$IF (CICLOS_010_US - CONTADOR_CICLOS_DELAY_010_US) >= 2}
  {$SET DELAY_010_US = DELAY_010_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_010_US = CONTADOR_CICLOS_DELAY_010_US + 2}
{$ENDIF}
{$IF (CICLOS_010_US - CONTADOR_CICLOS_DELAY_010_US) > (1/2)}
  {$SET DELAY_010_US = DELAY_010_US + DELAY_001_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_010_US = CONTADOR_CICLOS_DELAY_010_US + 1}
{$ENDIF}
{$SET DELAY_10_US = DELAY_010_US}
//*********************************************************************//

//*************** DEFINICION DELAY_100F_US ******************************//
{$SET DELAY_100F_US = ""}

{$SET CONTADOR_CICLOS_DELAY_100F_US = 0}
{$IF CICLOS_100F_US > 10}
  {$SET CICLOS_DELAY_US_LOOP_100F_US = 1 + TRUNC((((CICLOS_100F_US) - 8 ) / 3) + 1)}
  {$SET DELAY_100F_US = ASM}
  {$SET DELAY_100F_US = DELAY_100F_US + "movlw " + CICLOS_DELAY_US_LOOP_100F_US + NUEVA_LINEA}
  {$SET DELAY_100F_US = DELAY_100F_US + DELAY_LOOP_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_100F_US = CONTADOR_CICLOS_DELAY_100F_US + (4+3*(CICLOS_DELAY_US_LOOP_100F_US-1))}
{$ENDIF}

{$IF (CICLOS_100F_US - CONTADOR_CICLOS_DELAY_100F_US) >= 2}
  {$SET DELAY_100F_US = DELAY_100F_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_100F_US = CONTADOR_CICLOS_DELAY_100F_US + 2}
{$ENDIF}
{$IF (CICLOS_100F_US - CONTADOR_CICLOS_DELAY_100F_US) >= 2}
  {$SET DELAY_100F_US = DELAY_100F_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_100F_US = CONTADOR_CICLOS_DELAY_100F_US + 2}
{$ENDIF}
{$IF (CICLOS_100F_US - CONTADOR_CICLOS_DELAY_100F_US) >= 2}
  {$SET DELAY_100F_US = DELAY_100F_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_100F_US = CONTADOR_CICLOS_DELAY_100F_US + 2}
{$ENDIF}
{$IF (CICLOS_100F_US - CONTADOR_CICLOS_DELAY_100F_US) >= 2}
  {$SET DELAY_100F_US = DELAY_100F_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_100F_US = CONTADOR_CICLOS_DELAY_100F_US + 2}
{$ENDIF}
{$IF (CICLOS_100F_US - CONTADOR_CICLOS_DELAY_100F_US) >= 2}
  {$SET DELAY_100F_US = DELAY_100F_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_100F_US = CONTADOR_CICLOS_DELAY_100F_US + 2}
{$ENDIF}
{$IF (CICLOS_100F_US - CONTADOR_CICLOS_DELAY_100F_US) > (1/2)}
  {$SET DELAY_100F_US = DELAY_100F_US + DELAY_001_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_100F_US = CONTADOR_CICLOS_DELAY_100F_US + 1}
{$ENDIF}
//*********************************************************************//

//*************** DEFINICION DELAY_100_US ******************************//
{$SET DELAY_100_US = ""}

{$SET CONTADOR_CICLOS_DELAY_100_US = 0}
{$IF CICLOS_100_US > 10}
  {$SET CICLOS_DELAY_US_LOOP_100_US = 1 + TRUNC((((CICLOS_100_US) - 8 ) / 3) + 1)}
  {$SET DELAY_100_US = ASM}
  {$SET DELAY_100_US = DELAY_100_US + "movlw " + CICLOS_DELAY_US_LOOP_100_US + NUEVA_LINEA}
  {$SET DELAY_100_US = DELAY_100_US + DELAY_LOOP_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_100_US = CONTADOR_CICLOS_DELAY_100_US + (4+3*(CICLOS_DELAY_US_LOOP_100_US-1))}
{$ENDIF}

{$IF (CICLOS_100_US - CONTADOR_CICLOS_DELAY_100_US) >= 2}
  {$SET DELAY_100_US = DELAY_100_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_100_US = CONTADOR_CICLOS_DELAY_100_US + 2}
{$ENDIF}
{$IF (CICLOS_100_US - CONTADOR_CICLOS_DELAY_100_US) >= 2}
  {$SET DELAY_100_US = DELAY_100_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_100_US = CONTADOR_CICLOS_DELAY_100_US + 2}
{$ENDIF}
{$IF (CICLOS_100_US - CONTADOR_CICLOS_DELAY_100_US) >= 2}
  {$SET DELAY_100_US = DELAY_100_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_100_US = CONTADOR_CICLOS_DELAY_100_US + 2}
{$ENDIF}
{$IF (CICLOS_100_US - CONTADOR_CICLOS_DELAY_100_US) >= 2}
  {$SET DELAY_100_US = DELAY_100_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_100_US = CONTADOR_CICLOS_DELAY_100_US + 2}
{$ENDIF}
{$IF (CICLOS_100_US - CONTADOR_CICLOS_DELAY_100_US) >= 2}
  {$SET DELAY_100_US = DELAY_100_US + DELAY_002_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_100_US = CONTADOR_CICLOS_DELAY_100_US + 2}
{$ENDIF}
{$IF (CICLOS_100_US - CONTADOR_CICLOS_DELAY_100_US) > (1/2)}
  {$SET DELAY_100_US = DELAY_100_US + DELAY_001_CICLOS}
  {$SET CONTADOR_CICLOS_DELAY_100_US = CONTADOR_CICLOS_DELAY_100_US + 1}
{$ENDIF}
//*********************************************************************//


implementation

var
  d1 : byte;

// ******************************************************************************
// Las siguientes funciones espera el tiempo exacto, siendo equivalentes a las
// MACROS: DELAY_010_US y DELAY_100_US
// ******************************************************************************
procedure delay_10us;
begin
  {$DELAY_010F_US}
end;

procedure delay_100us;
begin
  {$DELAY_100F_US}
end;

// ******************************************************************************
// Las siguientes funciones no sin exactas ya que no tienen en cuenta la
// llamada a la función y las instrucciones de bucle.
// Se utilizará cuando el tiempo pueda ser aproximado o no tiene transcendencia
// que sea unos microsegundos mayor la pausa. Usar estas funciones ahorra memoria
// de programa.
// Si se requiera total precición, utilizar de manera repetitica las MACROS
// DELAY_001_US, DELAY_010_US y DELAY_100_US, hasta sumar el tiempo requerido con
// exactitud.
// ******************************************************************************
procedure delay_x10us(d2 : byte);
begin
  ASM
  Delay_1:
    call delay_10us
    DECFSZ  d2, f
    GOTO    Delay_1
  END
end;

procedure delay_x100us(d2 : byte);
begin
  ASM
  Delay_1:
    call delay_100us
    DECFSZ  d2, f
    GOTO    Delay_1
  END  
end;
end.
