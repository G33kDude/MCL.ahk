#Include %A_ScriptDir%/../
#include MCLib.ahk

C =
( %

#include <stdint.h>

typedef uint64_t size_t;

void* (*HeapAlloc)(uint64_t, uint32_t, size_t);
void (*HeapFree)(uint64_t, uint32_t, void*);

uint64_t hProcessHeap;

void* malloc(size_t Size) {
	return HeapAlloc(hProcessHeap, 0x8, Size);
}
void free(void* Memory) {
	HeapFree(hProcessHeap, 0, Memory);
}


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