#Include %A_ScriptDir%/../
#include MCL.ahk

C =
( %

#include <stdint.h>
#include <mcl.h>

MCL_IMPORT(int, User32, MessageBoxA, (uint32_t, char*, char*, uint32_t));

int __main() {
	MessageBoxA(0, "Hello world from C!", "Wow", 0);
	return 0;
}

)

Code := MCL.AHKFromC(C, MCL.Options.DoNotFormat) ; Compile and stringify the code, but don't format it as an AHK string literal (since we're going to load it again momentarily)

pCode := MCL.FromString(Code)

DllCall(pCode)