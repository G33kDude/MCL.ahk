#Requires AutoHotkey v2.0

#Include ..\MCL.ahk

lib := MCL.FromCPP("
(
#include <mcl.h>
#include <stdlib.h>

class Point {
public:
	Point(int NX, int NY) {
		X = NX;
		Y = NY;
	}

private:
	int X;
	int Y;
};

MCL_EXPORT(Call, Int, x, Int, y, Ptr)
Point* Call(int X, int Y) {
	return new Point(X, Y);
}
)")

pPoint := lib(20, 30)

MsgBox NumGet(pPoint, 0, "Int") ", " NumGet(pPoint, 4, "Int")