@echo off

SET TEMP=%~dp0..\emacs\tmp
SET TMP=%~dp0..\emacs\tmp

SET PATH=%~dp0;%PATH%
SET PATH=%~dp0\gtk\bin;%~dp0..\tcl\bin;"%~dp0..\jvm\bin";%PATH%