SET ruta=".\Editor_Muestra"
DEL "%ruta%\*.dbg"
DEL "%ruta%\*.exe"
RD  "%ruta%\lib" /s /Q

SET ruta=".\Simple Editor"
DEL %ruta%\*.dbg
DEL %ruta%\*.exe
RD  %ruta%\lib /s /Q

pause