{Description of the program.}
program nombre;
var x,y: byte;
procedure proc1;
begin
  x := 1;
end; 
begin
//Prueba de funciones
  {$IF abs(-1) <> 1}
  {$MSGERR 'Error'}
  {$ENDIF}
  {$IF sgn(-1000) <> -1}
  {$MSGERR 'Error'}
  {$ENDIF}
  {$IF abs(sin(3.14159)) > 0.0001 }  {$MSGERR 'Error'}  {$ENDIF}
  {$IF cos(0)<>1} {$MSGERR 'Error'}  {$ENDIF}
  {$IF tan(0)<>0} {$MSGERR 'Error'}  {$ENDIF}
  {$IF abs(log(2.7182)-1) > 0.0001 } {$MSGERR 'Error'}  {$ENDIF}
  {$IF round(3.5) <> 4 } {$MSGERR 'Error'}  {$ENDIF}
  {$IF round(2.1) <> 2 } {$MSGERR 'Error'}  {$ENDIF}
  {$IF round(2.9) <> 3 } {$MSGERR 'Error'}  {$ENDIF}
  {$IF length('Hola') <> 4 } {$MSGERR 'Error'}  {$ENDIF}
  {$IF length('tú') <> 3 } {$MSGERR 'Error'}  {$ENDIF}
  {$IF Upcase('Hola') <> 'HOLA' } {$MSGERR 'Error'}  {$ENDIF}

//Define características del hardware
{$SET PIC_MODEL='MIPIC'}
{$SET PIC_MAXFREQ = 1000000}
{$SET PIC_NPINS = 18}
{$SET PIC_NUMBANKS=2}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 1024}
//Inicia la memoria, para empezar a definir
{$SET_STATE_RAM '000-1FF:NIM'}
//Define estado de la memoria RAM
{$SET_STATE_RAM '000-00B:SFR, 00C-04F:GPR'}
{$SET_STATE_RAM '080-08B:SFR, 08C-0CF:GPR'}

end. 
