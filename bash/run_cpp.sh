CMAKE_PATH="`cygpath -p 'C:\Program Files (x86)\CMake\bin\'`"
GS_PATH="`cygpath -p 'C:\Program Files (x86)\gs\gs9.18\bin'`"
PATH="$GS_PATH:$CMAKE_PATH:$PATH"; export PATH
cmake.exe --build . --target install --config Release --clean-first
