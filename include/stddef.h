#ifndef STDDEF_HEADER
#define STDDEF_HEADER

#include <stdint.h>

#if MCL_BITNESS == 64
typedef uint64_t size_t;
typedef int64_t ssize_t;

typedef int64_t ptrdiff_t;
typedef uint64_t off_t;

typedef int64_t int_t;
typedef uint64_t uint_t;

typedef int64_t __int64;
#else
typedef uint32_t size_t;
typedef int32_t ssize_t;

typedef int32_t ptrdiff_t;
typedef uint32_t off_t;

typedef int32_t int_t;
typedef uint32_t uint_t;

typedef int64_t __int64;
#endif

#ifndef __cplusplus
typedef uint16_t wchar_t;
#endif

#define NULL (void*)0

#define offsetof(Type, Member) (off_t)(&((Type*)0)-> ## Member)

#endif // STDDEF_HEADER