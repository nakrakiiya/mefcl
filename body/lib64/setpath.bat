@echo off

SET TEMP=%~dp0..\emacs\tmp
SET TMP=%~dp0..\emacs\tmp

SET PATH=%~dp0;%PATH%

REM though it's a 32-bits Tcl/Tk
SET PATH=%~dp0..\tcl\bin;%PATH%