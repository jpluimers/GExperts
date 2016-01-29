@echo off
echo moving %1 so the DLL can be compiled even though it is currently loaded in the IDE
move %1 %1~
exit /b 0