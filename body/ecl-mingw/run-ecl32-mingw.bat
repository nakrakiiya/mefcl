@ECHO OFF

REM Set Path for lib32
call %~dp0\..\lib32\setpath.bat

SET PATH=%~dp0\..\mingw\bin;%~dp0;%PATH%
SET ECLSRCDIR=%~dp0\build\src\

CD /d %~dp0
ecl -eval "(require :cmp)"