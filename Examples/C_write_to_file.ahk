#Requires AutoHotkey v2.0

#Include ..\MCL.ahk

lib := MCL.FromC("
(
#include <mcl.h>
#include <stdio.h>

MCL_EXPORT(Call, Int, value)
void Call(int value) {
    FILE* f = fopen("test.txt", "w");

    fputs("Hello world!\n", f);
    fprintf(f, "The number is: %i\n", value);

    fclose(f);
}
)")

lib(2931)

MsgBox FileRead("test.txt")