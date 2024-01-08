#ifndef STDLIB_HEADER
#define STDLIB_HEADER

#include <mcl.h>
#include <stdint.h>
#include <stddef.h>

MCL_IMPORT(uint32_t, Kernel32, GetProcessHeap, ());
MCL_IMPORT(void*, Kernel32, HeapAlloc, (uint32_t, uint32_t, size_t));
MCL_IMPORT(void*, Kernel32, HeapReAlloc, (uint32_t, uint32_t, void*, size_t));
MCL_IMPORT(void, Kernel32, HeapFree, (uint32_t, uint32_t, void*));

static void* malloc(size_t Size) {
	return HeapAlloc(GetProcessHeap(), 0x8, Size);
}
static void* calloc(size_t ElementSize, size_t ElementCount) {
	return malloc(ElementCount * ElementSize);
}
static void* realloc(void* Memory, size_t NewSize) {
	if (Memory == NULL) {
		return malloc(NewSize);
	}

	return HeapReAlloc(GetProcessHeap(), 0x8, Memory, NewSize);
}

static void free(void* Memory) {
	HeapFree(GetProcessHeap(), 0, Memory);
}


MCL_IMPORT(void*, msvcrt, memcpy, (void*, const void*, size_t));
MCL_IMPORT(void*, msvcrt, memset, (void*, int, size_t));
MCL_IMPORT(int, msvcrt, memcmp, (const void*, const void*, size_t));
MCL_IMPORT(void*, msvcrt, memmove, (void*, const void*, size_t));

#ifdef __cplusplus

// Apparently these can't be `static`. But considering its C++ we're talking about, I think defining `main()` won't be too bad.

void* operator new(size_t size) {
	return malloc(size);
}
void* operator new[](size_t size) {
	return malloc(size);
}
void operator delete(void* Memory) {
	free(Memory);
}
void operator delete[](void* Memory) {
	free(Memory);
}
#endif // __cplusplus

#endif // STDLIB_HEADER