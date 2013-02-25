echo off

REM Set Path for lib32
call %~dp0\..\lib32\setpath.bat

cd /d %~dp0
set HOME=%~dp0\home

SET CURRENT_COMMON_LISP_TYPE=CLISP
start bin\runemacs.exe --debug-init