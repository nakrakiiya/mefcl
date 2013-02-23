@echo off

REM Set Path for lib32
call %~dp0\..\lib32\setpath.bat

SET PATH=%PATH%;%~dp0;%~dp0\dist
"%~dp0..\jvm\bin\javaw" -Dfile.encoding=UTF8 -Duser.home="%~dp0..\emacs\home" -cp "%~dp0\dist\abcl.jar;%~dp0\dist\abcl-contrib.jar;%~dp0\dist\jna.jar;%~dp0\dist\platform.jar;%CLASSPATH%"  org.armedbear.lisp.Main --load "%~dp0\abcl-mefcl-init.lisp" %1 %2 %3 %4 %5 %6 %7 %8 %9 
