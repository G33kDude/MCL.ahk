#Include %A_ScriptDir%/../
#include MCLib.ahk

C =
( %

#include "ahk.h"

typedef struct {
	int X;
	int Y;
} Point;

Point* __main(int a, int b) {
	Point* P = malloc(sizeof(Point));
	
	P->X = a;
	P->Y = b;
	
	return P;
}

)

pCode := MCLib.FromC(C)

pPoint := DllCall(pCode, "Int", 20, "Int", 30, "Ptr")

MsgBox, % NumGet(pPoint+0, 0, "Int") ", " NumGet(pPoint+0, 4, "Int")