@ECHO OFF

REM Set Path for lib64
call %~dp0\..\lib64\setpath.bat

SET PATH=%~dp0;%PATH%

CD /d %~dp0
sbcl-with-contrib-1.0.55.1-x86-64.exe --load startup.lisp