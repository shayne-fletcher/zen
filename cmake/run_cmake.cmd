@echo off

del .\CMakeCache.txt 

c:/project/github/build/zen/bin/fortune.exe -f c:/project/github/build/zen/bin/fortunes

set "ZEN_RUNTIME_OUTPUT_DIRECTORY=c:/project/github/build/zen/bin"
set "ZEN_LIBRARY_OUTPUT_DIRECTORY=c:/project/github/build/zen/bin"
set "ZEN_ARCHIVE_OUTPUT_DIRECTORY=c:/project/github/build/zen/bin"

call "C:\program files\microsoft visual studio 10.0\vc\bin\vcvars32.bat"

cmake -G "NMake Makefiles" "c:/project/github/zen" -DCMAKE_BUILD_TYPE=Debug

