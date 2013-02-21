@ECHO OFF

cd /d %~dp0

echo Updating... Please wait...

cd ccl
..\svn-1.6.5\bin\svn update

pause