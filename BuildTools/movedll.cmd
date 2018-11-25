@echo off
if not exist %1 goto skipit
echo moving %1 to %1~ so the DLL can be compiled even though it is currently loaded in the IDE
move %1 %1~
:skipit
exit /b 0