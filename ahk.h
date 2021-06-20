#include <stdint.h>

#define MCODE_EXPORT(Name) \
int __mcode_e_ ## Name () __attribute__((alias(#Name)));

#define MCODE_EXPORT_INLINE(ReturnType, Name, Parameters) \
int __mcode_e_ ## Name () __attribute__((alias(#Name))); \
ReturnType Name Parameters

#define MCODE_IMPORT(ReturnType, DllName, Name, ParameterTypes) \
ReturnType (* Name)ParameterTypes; \
extern void* __attribute__((alias(#Name))) __mcode_i_ ## DllName ## _ ## Name;

typedef uint64_t size_t;

MCODE_IMPORT(uint64_t, Kernel32, GetProcessHeap, ());
MCODE_IMPORT(void*, Kernel32, HeapAlloc, (uint64_t, uint32_t, size_t));
MCODE_IMPORT(void, Kernel32, HeapFree, (uint64_t, uint32_t, void*));

void* malloc(size_t Size) {
	return HeapAlloc(GetProcessHeap(), 0x8, Size);
}
void free(void* Memory) {
	HeapFree(GetProcessHeap(), 0, Memory);
}

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