#Include %A_ScriptDir%/../
#include MCLib.ahk

CPP = 
( %

#include <stdint.h>

typedef uint64_t size_t;

void* (*HeapAlloc)(uint64_t, uint32_t, size_t);
void (*HeapFree)(uint64_t, uint32_t, void*);

uint64_t hProcessHeap;

void* operator new(size_t size) {
	return HeapAlloc(hProcessHeap, 0x8, size);
}
void operator delete(void* Memory) {
	HeapFree(hProcessHeap, 0, Memory);
}

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

pCode := MCLib.FromCPP(CPP)

pPoint := DllCall(pCode, "Int", 20, "Int", 30, "Ptr")

MsgBox, % NumGet(pPoint+0, 0, "Int") ", " NumGet(pPoint+0, 4, "Int")