@echo off

SET TEMP=%~dp0..\emacs\tmp
SET TMP=%~dp0..\emacs\tmp

SET PATH=%~dp0;%PATH%
SET PATH=%~dp0\gtk\bin;%~dp0..\tcl\bin;"%~dp0..\jvm\bin";%PATH%

REM SET PATH FOR MSYS/MINGW
SET PATH=%~dp0\..\mingw\bin;%~dp0\..\mingw\msys\1.0\bin;%PATH%