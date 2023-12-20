#include <mcl.h>

MCL_EXPORT(Call, Double, a, Double, b, Double)
double _stdcall Call(double a, double b) {
    return (a + b) * 2.2;
}