@echo off
del GXIcons.rc
@for %%I in (*.bmp) do Echo %%~nI BITMAP "%%I">> GXIcons.rc
BRCC32 -r -32 GXIcons.rc
ren GXIcons.RES GXIcons.res
