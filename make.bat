@echo off

:: processingフォルダ内の lib\core.jar にパスを通してください
set PCORE=C:\bin\processing\lib\core.jar

if not exist bin mkdir bin
call scalac -cp %PCORE% -d bin *.scala
call scala -cp %PCORE%;bin neet.game.puzzle.souko.Main


