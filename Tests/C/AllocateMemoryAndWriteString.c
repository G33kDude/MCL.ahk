#include <stdlib.h>

char* _stdcall __MCL_f_Call$Ptr() {
    char* Memory = malloc(27);

    char NextCharacter = 'A';

    for (int Index = 0; Index < 26; Index++) {
        Memory[Index] = NextCharacter++;
    }

    return Memory;
}