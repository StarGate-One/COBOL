@echo off
@setlocal
set pgm=%1
set pgm=.\%pgm%

if exist %pgm%.asm (
   del /f %pgm%.asm
)

if exist %pgm%.exe (
   del /f %pgm%.exe
)

if exist %pgm%.lst (
   del /f %pgm%.lst
)

if exist %pgm%.obj (
   del /f %pgm%.obj
)

if exist %pgm%.txt (
   del /f %pgm%.txt
)

if exist *.c (
   del /f *.c
)

if exist *.h (
   del /f *.h
)

if exist %pgm%.cob (
   cobc -C %pgm%.cob
   cobc -S %pgm%.cob
   if exist %pgm%.s (
      move %pgm%.s %pgm%.asm
   )
   rem cobc --Xref --fixed -x -save-temps -t %pgm%.lst -o %pgm%.obj %pgm%.cob
   cobc -ftsymbols -ffilename-mapping --Xref --fixed -x -t %pgm%.txt -P -ext cpy -I ..\cpy %pgm%.cob
) else (
  echo %pgm%.cob does not exist
)

:end
@endlocal