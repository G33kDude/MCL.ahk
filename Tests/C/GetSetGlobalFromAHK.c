#include <mcl.h>

int password = 1000;

MCL_EXPORT_GLOBAL(password, Int);

MCL_EXPORT_INLINE(char*, Check, (int guess), Int, guess, CDecl_AStr) {
    if (guess == password)
        return "You got the password right!";
    return "Oops, that's wrong";
}