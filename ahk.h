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

#define MCODE_EXPORT(Name) int __mcode_e_ ## Name () __attribute__((alias(#Name)));
#define MCODE_EXPORT_INLINE(ReturnType, Name, Parameters) int __mcode_e_ ## Name () __attribute__((alias(#Name))); \
ReturnType Name Parameters

#ifdef MCODE_LIBRARY
void __main() {};
#endif

#ifdef __cplusplus

void* operator new(size_t size) {
	return HeapAlloc(hProcessHeap, 0x8, size);
}
void* operator new[](size_t size) {
	return HeapAlloc(hProcessHeap, 0x8, size);
}
void operator delete(void* Memory) {
	HeapFree(hProcessHeap, 0, Memory);
}
void operator delete[](void* Memory) {
	HeapFree(hProcessHeap, 0, Memory);
}

#endif