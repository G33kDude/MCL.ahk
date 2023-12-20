#include <mcl.h>

MCL_EXPORT(Find, AStr, string, Char, target, CDecl_Int)
int Find(char* string, char target) {
    for (int index = 0; string[index] != 0; index++)
        if (string[index] == target)
            return index;
    return -1;
}

MCL_EXPORT_INLINE(unsigned int, Hash, (char* string), AStr, string, CDecl_UInt) {
    unsigned int hash = 5381;
    
    for (int index = 0; string[index] != 0; index++) {
        char next = string[index];
        hash = ((hash << 5) + hash) + next; 
    }

    return hash;
}