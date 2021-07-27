#ifndef STDDEF_HEADER
#define STDDEF_HEADER

#include <stdint.h>

#if MCL_BITNESS == 64
typedef uint64_t size_t;
typedef int64_t ssize_t;

typedef int64_t ptrdiff_t;
typedef uint64_t off_t;
#else
typedef uint32_t size_t;
typedef int32_t ssize_t;

typedef int32_t ptrdiff_t;
typedef uint32_t off_t;
#endif

#ifndef __cplusplus
typedef uint16_t wchar_t;
#endif

#define NULL (void*)0

#define offsetof(Type, Member) (off_t)(&((Type*)0)-> ## Member)

#endif // STDDEF_HEADER