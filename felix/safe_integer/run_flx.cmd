set "PFPATH=C:\Program Files"

set PATH=C:\Python31;"%PFPATH%\Cmake 2.8\bin";"C:\Program Files\flexdll";C:\Tcl\bin;C:\ocamlms\bin;C:\WINDOWS\system32;C:\WINDOWS;C:\WINDOWS\System32\Wbem;C:\unxutils\bin;c:\unxutils\usr\local\wbin

call "%PFPATH%\Microsoft Visual Studio 10.0\VC\bin\vcvars32.bat"

::"c:\project\github\felix\build\release\bin\flx.exe" --test="c:\project\github\felix\build\release" --static optional.flx

"c:\project\github\felix\build\release\bin\flx.exe" --test="c:\project\github\felix\build\release" --static optional.flx

::del /S /Q .\CMakeCache.txt
::cmake -G "NMake Makefiles" c:\project\zen\optional\


