#include <mcl.h>

MCL_EXPORT(Call, Int, a, Int, b, Int, c, Int, d, Int, e, Int)
int _stdcall Call(int a, int b, int c, int d, int e) {
    return a + b + c + d + e;
}