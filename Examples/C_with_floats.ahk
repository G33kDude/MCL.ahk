#Requires AutoHotkey v2.0

#Include ..\MCL.ahk

lib := MCL.FromC("
(
#include <mcl.h>

MCL_EXPORT(Call, Double, in, Double)
double Call(double in) {
	return in * 2.5;
}
)")

MsgBox lib(11.7)