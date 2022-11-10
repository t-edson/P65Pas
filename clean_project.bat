del .\*.dbg
rd .\Source\lib /s /Q
rd .\Source\backup /s /Q

del .\comp_p65pas\testcode65\*.hex /s
del .\comp_p65pas\temp\*.hex /s
del .\comp_p65pas\temp\*.prg /s
del .\comp_p65pas\samples\*.hex /s
del .\comp_p65pas\samples\*.prg /s
del .\comp_p65pas\testcode65\Simul_1\*.prg /s
del .\comp_p65pas\testcode65\Simul_2\*.prg /s
rd  .\comp_p65pas\src\lib /s /Q
rd  .\comp_p65pas\src\backup /s /Q

pause