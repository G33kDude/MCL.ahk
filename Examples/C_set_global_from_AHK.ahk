#Requires AutoHotkey v2.0

#Include ..\MCL.ahk

lib := MCL.FromC("
(
#include <mcl.h>

MCL_EXPORT_GLOBAL(savedValue, Int)
int savedValue = 10;

MCL_EXPORT(Call, Int, newValue, Int)
int Call(int newValue) {
	int result = savedValue;
	savedValue = newValue;
	return result;
}
)")

lib.savedValue := 20
MsgBox lib(30)
MsgBox lib(40)
MsgBox lib.savedValue