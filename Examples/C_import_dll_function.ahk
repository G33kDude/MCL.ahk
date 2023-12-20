#Requires AutoHotkey v2.0

#Include ..\MCL.ahk

lib := MCL.FromC("
(
#include <mcl.h>
#include <stdint.h>

MCL_IMPORT(int, User32, MessageBoxA, (uint32_t, char*, char*, uint32_t));

MCL_EXPORT(Call, Int)
int Call() {
	MessageBoxA(0, "Hello world from C!", "Wow", 0);
	return 0;
}
)")

lib()