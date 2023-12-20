#Requires AutoHotkey v2.0

#Include ../MCL.ahk

lib := MCL.FromC("
(
#include <mcl.h>

MCL_EXPORT(Add, Int, left, Int, right, Int)
int Add(int left, int right) {
    return left + right;
}

MCL_EXPORT(Multiply, Int, left, Int, right, Int)
int Multiply(int left, int right) {
    return left * right;
}

int unused() {
    return 20;
}
)")

added := lib.Add(300, -20)
MsgBox added

multiplied := lib.Multiply(Added, 2)
MsgBox multiplied