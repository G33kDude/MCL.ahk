#include <mcl.h>

int Password = 1000;

MCL_EXPORT_GLOBAL(Password);

MCL_EXPORT_INLINE(char*, Check, (int Guess)) {
    if (Guess == Password) {
        return "You got the password right!";
    }
    else {
        return "Oops, that's wrong";
    }
}