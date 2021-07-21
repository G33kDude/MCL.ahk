#Include %A_ScriptDir%/../
#include MCL.ahk

CPP = 
( %

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

Point* __main(int X, int Y) {
	return new Point(X, Y);
}

)

pCode := MCL.FromCPP(CPP)

pPoint := DllCall(pCode, "Int", 20, "Int", 30, "Ptr")

MsgBox, % NumGet(pPoint+0, 0, "Int") ", " NumGet(pPoint+0, 4, "Int")