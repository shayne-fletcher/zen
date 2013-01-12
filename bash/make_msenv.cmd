:: make_msenv.cmd
::
:: Setup a Cygwin environment for using the msvc toolchain.
::
:: You will need the following software components to perform the recompilation:
:: - Windows NT, 2000, XP, Vista, or 7 (32 or 64 bits).
:: - Items [1], [2] and [3] from the list of recommended software above.
::   Make sure to install the 32-bit version of TCL/TK, even if you are
::   compiling on a 64-bit Windows.
:: - The Cygwin port of GNU tools, available from http://www.cygwin.com/
::   Install at least the following packages: diffutils, make, ncurses.

::@echo off

echo Preparing a bash script for inclusion in your .bashrc.

:: First, you need to set up your cygwin environment for using the MS
:: tools.  The following assumes that you have installed [1], [2], and [3]
:: in their default directories.  If this is not the case, you will need
:: to adjust the paths accordingly.

set PFPATH=C:\Program Files

:: If you are compiling on the 64-bit version of Windows 7, enter the
::  following instead:
::    set PFPATH=C:\Program Files (x86)

cd "%PFPATH%\Microsoft Visual Studio 10.0\VC\bin"
call vcvars32.bat

echo VCPATH="`cygpath -p '%Path%'`" >C:\cygwin\tmp\msenv
echo LIB="%LIB%;C:\Tcl\lib" >>C:\cygwin\tmp\msenv
echo LIBPATH="%LIBPATH%" >>C:\cygwin\tmp\msenv
echo INCLUDE="%INCLUDE%;C:\Tcl\include;C:\Program Files\flexdll" >>C:\cygwin\tmp\msenv

echo FLPATH="`cygpath '%PFPATH%\flexdll'`" >>C:\cygwin\tmp\msenv
echo MY_PYTHON_BIN_PATH="`cygpath 'C:\Python31'`" >> C:\cygwin\tmp\msenv
echo MY_OCAML_BIN_PATH="`cygpath 'C:\ocamlms\bin'`" >> C:\cygwin\tmp\msenv

echo PATH="${VCPATH}:${FLPATH}:${MY_PYTHON_BIN_PATH}:${MY_OCAML_BIN_PATH}:${PATH}" >>C:\cygwin\tmp\msenv

echo export PATH LIB LIBPATH INCLUDE >>C:\cygwin\tmp\msenv
echo export OCAMLBUILD_FIND=/usr/bin/find >>C:\cygwin\tmp\msenv

echo Read the notes on the bottom of make_msenv.cmd for the next steps.

:: Open a Cygwin shell and enter the following commands:
:: tr -d '\r' </tmp/msenv >.msenv32
:: . $HOME/.msenv32' >>.bashrc
:: 
:: Now, close the Command Prompt and the shell and you're set up for
:: using the MS tools under Cygwin.
