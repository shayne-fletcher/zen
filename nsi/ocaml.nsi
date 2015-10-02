; Usage: makensis /V4 ocaml.nsi
;
; Example client usage:
; fletch@church /cygdrive/c/Temp
; $ ./setup_ocaml_4_02_03_msvc-14-x86_64.exe /S

Name "ocaml"; Installer name

AllowRootDirInstall true

OutFile "setup_ocaml_4_02_03_msvc-14-x86_64.exe" ; Name of the setup file

Function .onInit

    StrCpy $INSTDIR "$WINDIR" 2 ; Compute C:\ from C:\Windows

FunctionEnd

    InstallDir $INSTDIR ; Installation directory

Section

  SetOutPath "$INSTDIR\ocaml" ; C:\ocaml

  File /r "c:\ocaml\*"

SectionEnd

