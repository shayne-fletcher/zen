=================================================
 How to create a MinGW compatible Python library
=================================================

A quick note on how to build a link compatible Python library for use
with mingw/gcc.

  - Create a list of symbols which the Python DLL exports
    ::

      pexports python26.dll > python26.def

  - Create an import library for gcc
    ::

      dlltool --dllname python26.dll --def python26.def --output-lib libpython26.a
