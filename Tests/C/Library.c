#include "ahk.h"

int Find(char* String, char Target) {
    for (int Index = 0; String[Index] != 0; Index++) {
        if (String[Index] == Target) {
            return Index;
        }
    }

    return -1;
}

MCODE_EXPORT(Find)

MCODE_EXPORT_INLINE(unsigned long int, Hash, (char* String)) {
    unsigned long int Hash = 5381;
    
    for (int Index = 0; String[Index] != 0; Index++) {
        char Next = String[Index];
        Hash = ((Hash << 5) + Hash) + Next; 
    }

    return Hash;
}