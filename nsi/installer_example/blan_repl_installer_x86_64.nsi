; Usage: makensis [option | script.nsi] - [...]

Name "BLAN"; Installer name
OutFile "setup_blan_1_0_0_repl_win-x86_64.exe" ; Name of the setup file

InstallDir $PROGRAMFILES64\blan ; Installation directory
RequestExecutionLevel admin ; Request application privileges for Windows Vista

Page instfiles

Section
  SetOutPath $INSTDIR
  File /a "c:\project\ocaml-bb-blan.git\build\WinNT\bin\blan_repl.exe"
  File /a "c:\project\ocaml-bb-blan.git\build\WinNT\bin\blan_repl_win.exe"
  File /a /r "c:\project\ocaml-bb-blan.git\src\blan\blanstdlib"
SectionEnd
