#Requires AutoHotkey v2.0

#Include ..\MCL.ahk

lib := MCL.FromC("
(
#include <mcl.h>
#include <stdlib.h>

typedef struct {
	int X;
	int Y;
} Point;

MCL_EXPORT(Call, Int, a, Int, b, Ptr)
Point* Call(int a, int b) {
	Point* P = malloc(sizeof(Point));
	
	P->X = a;
	P->Y = b;
	
	return P;
}
)")

pPoint := lib(20, 30)

MsgBox NumGet(pPoint, 0, "Int") ", " NumGet(pPoint, 4, "Int")