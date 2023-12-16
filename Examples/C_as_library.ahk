#Include %A_ScriptDir%/../
#include MCL.ahk

C := "
(
#include <MCL.h>

MCL_EXPORT_INLINE(int, Add, (int Left, int Right)) {
    return Left + Right;
}

MCL_EXPORT_INLINE(int, Multiply, (int Left, int Right)) {
    return Left * Right;
}

int unused() {
    return 20;
}
)"

Code := MCL.FromC(C)

Added := Code.Add("Int", 300, "Int", -20, "Int")
MsgBox Added

Multiplied := Code.Multiply("Int", Added, "Int", 2, "Int")
MsgBox Multiplied
