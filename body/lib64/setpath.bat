@echo off

SET TEMP=%~dp0..\emacs\tmp
SET TMP=%~dp0..\emacs\tmp

SET PATH=%~dp0;%PATH%

REM though it's a 32-bits Tcl/Tk
SET PATH=%~dp0..\tcl\bin;%PATH%

REM SET PATH FOR MSYS/MINGW though it's 32-bits 
SET PATH=%~dp0\..\mingw\bin;%~dp0\..\mingw\msys\1.0\bin;%PATH%