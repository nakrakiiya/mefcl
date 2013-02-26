@echo off

REM Set Path for lib64
call %~dp0\..\lib64\setpath.bat

cd /d %~dp0
set HOME=%~dp0\home

SET CURRENT_COMMON_LISP_TYPE=CCL64
start bin\runemacs.exe -mm --debug-init