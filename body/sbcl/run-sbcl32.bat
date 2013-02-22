@ECHO OFF

REM Set Path for lib32
call %~dp0\..\lib32\setpath.bat

SET PATH=%~dp0;%PATH%

CD /d %~dp0
sbcl.exe --load startup.lisp