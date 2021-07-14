#ifndef MCLIB_INCLUDED
#define MCLIB_INCLUDED

#include <stdint.h>

#define MCLIB_EXPORT(Name) \
int __MCLIB_e_ ## Name () __attribute__((alias(#Name)));

#define MCLIB_EXPORT_INLINE(ReturnType, Name, Parameters) \
int __MCLIB_e_ ## Name () __attribute__((alias(#Name))); \
ReturnType Name Parameters

#define MCLIB_QUOTE(X) #X

#define MCLIB_IMPORT(ReturnType, DllName, Name, ParameterTypes) \
ReturnType (* __MCLIB_i_ ## DllName ## _ ## Name)ParameterTypes = (ReturnType(*)ParameterTypes)0; \
static ReturnType __attribute__((alias(MCLIB_QUOTE(__MCLIB_i_ ## DllName ## _ ## Name)))) (*Name) ParameterTypes;

#ifdef MCLIB_LIBRARY
void __main() {};
#endif

typedef uint64_t size_t;
typedef int64_t ssize_t;

typedef int64_t ptrdiff_t;
typedef uint64_t off_t;

#define NULL (void*)0

#ifndef __cplusplus
typedef uint8_t bool;
#define true (bool)1
#define false (bool)0
#endif

#ifdef MCLIB_FORWARD_STDLIB
#define MCLIB_MEMORY
#define MCLIB_FORWARD_STDLIB_FILE
#define MCLIB_FORWARD_STDLIB_STRING
#define MCLIB_FORWARD_STDLIB_TIME
#define MCLIB_FORWARD_STDLIB_ERROR
#endif

#ifdef MCLIB_MEMORY
MCLIB_IMPORT(uint64_t, Kernel32, GetProcessHeap, ());
MCLIB_IMPORT(void*, Kernel32, HeapAlloc, (uint64_t, uint32_t, size_t));
MCLIB_IMPORT(void*, Kernel32, HeapReAlloc, (uint64_t, uint32_t, void*, size_t));
MCLIB_IMPORT(void, Kernel32, HeapFree, (uint64_t, uint32_t, void*));

void* malloc(size_t Size) {
	return HeapAlloc(GetProcessHeap(), 0x8, Size);
}
void* calloc(size_t ElementSize, size_t ElementCount) {
	return malloc(ElementCount * ElementSize);
}
void* realloc(void* Memory, size_t NewSize) {
	if (Memory == NULL) {
		return malloc(NewSize);
	}

	return HeapReAlloc(GetProcessHeap(), 0x8, Memory, NewSize);
}

void free(void* Memory) {
	HeapFree(GetProcessHeap(), 0, Memory);
}

void* memcpy(void* To, const void* From, size_t Count) {
	asm("cld\n\t" \
		"rep movsb" \
		:: "D"(To), "S"(From), "c"(Count)
	);

	return To;
}

void* memset(void* To, int Value, size_t Count) {
	asm("cld\n\t" \
		"rep stosb" \
		:: "D"(To), "c"(Count), "a"(Value)
	);
	
	return To;
}

int memcmp(const void* Left, const void* Right, size_t Count) {
	asm("cld\n\t" \
		"repe cmpsb\n\t" \
        "mov 0, %%eax\n\t" \
        "cmovl -1, %%eax\n\t" \
        "cmovg 1, %%eax\n\t" \
        "ret"
		:: "D"(Left), "S"(Right), "c"(Count)
	);

    return 0;
}

#ifdef __cplusplus
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
#endif // MCLIB_MEMORY

#ifdef MCLIB_FORWARD_STDLIB_FILE
typedef struct {
	char Dummy;
} FILE;

MCLIB_IMPORT(FILE*, msvcrt, fopen, (const char*, const char*));
MCLIB_IMPORT(long int, msvcrt, ftell, (FILE*));
MCLIB_IMPORT(int, msvcrt, fseek, (FILE*, long int, int));
MCLIB_IMPORT(size_t, msvcrt, fwrite, (const void*, size_t, size_t, FILE*));
MCLIB_IMPORT(int, msvcrt, fputs, (char*, FILE*));
MCLIB_IMPORT(size_t, msvcrt, fread, (void*, size_t, size_t, FILE*));
MCLIB_IMPORT(int, msvcrt, fprintf, (FILE*, char*, ...));
MCLIB_IMPORT(int, msvcrt, fclose, (FILE*));

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

#define stdout 0
#define stderr 0
#define stdin 0

struct stat {
	off_t	st_size;
};

#define fileno(f) ((uint64_t)f)

int fstat(uint64_t FileHandle, struct stat* Output) {
	FILE* f = (FILE*)FileHandle;

	int OldPosition = ftell(f);
	fseek(f, 0, SEEK_END);
	int Result = ftell(f);
	fseek(f, OldPosition, SEEK_SET);

	Output->st_size = Result;

	return 0;
}

#endif // MCLIB_FORWARD_STDLIB_FILE

#ifdef MCLIB_FORWARD_STDLIB_STRING
MCLIB_IMPORT(int, msvcrt, isspace, (int));
MCLIB_IMPORT(int, msvcrt, tolower, (int));
MCLIB_IMPORT(int, msvcrt, toupper, (int));

MCLIB_IMPORT(int, msvcrt, strlen, (const char*));
MCLIB_IMPORT(int, msvcrt, strcmp, (const char*, const char*));
MCLIB_IMPORT(int, msvcrt, strncmp, (const char*, const char*, size_t));
MCLIB_IMPORT(char, msvcrt, strcpy, (char*, const char*));
MCLIB_IMPORT(char*, msvcrt, strchr, (const char*, int));
MCLIB_IMPORT(char*, msvcrt, strrchr, (const char*, int));
MCLIB_IMPORT(char*, msvcrt, strncpy, (char*, const char*, size_t));

MCLIB_IMPORT(char*, msvcrt, strerror, (int));

MCLIB_IMPORT(int, msvcrt, sscanf, (char*, char*, ...));
MCLIB_IMPORT(int, msvcrt, sprintf, (char*, const char*, ...));
MCLIB_IMPORT(int, msvcrt, vsprintf, (char*, const char*, va_list));
MCLIB_IMPORT(int, msvcrt, vsnprintf, (char*, size_t, const char*, va_list));

int strcasecmp(const char* Left, const char* Right) {
	int ca, cb;

	do {
		ca = *(unsigned char *)Left;
		cb = *(unsigned char *)Right;
		ca = tolower(toupper(ca));
		cb = tolower(toupper(cb));
		Left++;
		Right++;
	} while (ca == cb && ca != '\0');

	return ca - cb;
}
#endif // MCLIB_FORWARD_STDLIB_STRING

#ifdef MCLIB_FORWARD_STDLIB_TIME

typedef uint64_t time_t;

MCLIB_IMPORT(time_t, msvcrt, time, (time_t*));

struct tm {
   int tm_sec;         /* seconds,  range 0 to 59          */
   int tm_min;         /* minutes, range 0 to 59           */
   int tm_hour;        /* hours, range 0 to 23             */
   int tm_mday;        /* day of the month, range 1 to 31  */
   int tm_mon;         /* month, range 0 to 11             */
   int tm_year;        /* The number of years since 1900   */
   int tm_wday;        /* day of the week, range 0 to 6    */
   int tm_yday;        /* day in the year, range 0 to 365  */
   int tm_isdst;       /* daylight saving time             */	
};

MCLIB_IMPORT(int, msvcrt, _localtime32_s, (struct tm*, const time_t*));

struct tm* localtime_r(const time_t* Time, struct tm* Output) {
	_localtime32_s(Output, Time);

	return Output;
}

struct tm* localtime(const time_t* Time) {
	struct tm* Result = malloc(sizeof(struct tm));

	return localtime_r(Time, Result);
}

MCLIB_IMPORT(size_t, msvcrt, strftime, (char*, size_t, const char*, const struct tm*));
#endif // MCLIB_FORWARD_STDLIB_TIME

#ifdef MCLIB_FORWARD_STDLIB_ERROR
#define EPERM           1
#define ENOENT          2
#define EBADF           9
#define EAGAIN          11
#define ENOMEM          12
#define EEXIST          17
#define EBUSY           16
#define EINVAL          22
#define ENOSPC          28
#define ERANGE          34

// Copied from Linux source

#define	EDEADLK		35	/* Resource deadlock would occur */
#define	ENAMETOOLONG	36	/* File name too long */
#define	ENOLCK		37	/* No record locks available */
#define	ENOSYS		38	/* Invalid system call number */
#define	ENOTEMPTY	39	/* Directory not empty */
#define	ELOOP		40	/* Too many symbolic links encountered */
#define	EWOULDBLOCK	EAGAIN	/* Operation would block */
#define	ENOMSG		42	/* No message of desired type */
#define	EIDRM		43	/* Identifier removed */
#define	ECHRNG		44	/* Channel number out of range */
#define	EL2NSYNC	45	/* Level 2 not synchronized */
#define	EL3HLT		46	/* Level 3 halted */
#define	EL3RST		47	/* Level 3 reset */
#define	ELNRNG		48	/* Link number out of range */
#define	EUNATCH		49	/* Protocol driver not attached */
#define	ENOCSI		50	/* No CSI structure available */
#define	EL2HLT		51	/* Level 2 halted */
#define	EBADE		52	/* Invalid exchange */
#define	EBADR		53	/* Invalid request descriptor */
#define	EXFULL		54	/* Exchange full */
#define	ENOANO		55	/* No anode */
#define	EBADRQC		56	/* Invalid request code */
#define	EBADSLT		57	/* Invalid slot */
#define	EDEADLOCK	EDEADLK
#define	EBFONT		59	/* Bad font file format */
#define	ENOSTR		60	/* Device not a stream */
#define	ENODATA		61	/* No data available */
#define	ETIME		62	/* Timer expired */
#define	ENOSR		63	/* Out of streams resources */
#define	ENONET		64	/* Machine is not on the network */
#define	ENOPKG		65	/* Package not installed */
#define	EREMOTE		66	/* Object is remote */
#define	ENOLINK		67	/* Link has been severed */
#define	EADV		68	/* Advertise error */
#define	ESRMNT		69	/* Srmount error */
#define	ECOMM		70	/* Communication error on send */
#define	EPROTO		71	/* Protocol error */
#define	EMULTIHOP	72	/* Multihop attempted */
#define	EDOTDOT		73	/* RFS specific error */
#define	EBADMSG		74	/* Not a data message */
#define	EOVERFLOW	75	/* Value too large for defined data type */
#define	ENOTUNIQ	76	/* Name not unique on network */
#define	EBADFD		77	/* File descriptor in bad state */
#define	EREMCHG		78	/* Remote address changed */
#define	ELIBACC		79	/* Can not access a needed shared library */
#define	ELIBBAD		80	/* Accessing a corrupted shared library */
#define	ELIBSCN		81	/* .lib section in a.out corrupted */
#define	ELIBMAX		82	/* Attempting to link in too many shared libraries */
#define	ELIBEXEC	83	/* Cannot exec a shared library directly */
#define	EILSEQ		84	/* Illegal byte sequence */
#define	ERESTART	85	/* Interrupted system call should be restarted */
#define	ESTRPIPE	86	/* Streams pipe error */
#define	EUSERS		87	/* Too many users */
#define	ENOTSOCK	88	/* Socket operation on non-socket */
#define	EDESTADDRREQ	89	/* Destination address required */
#define	EMSGSIZE	90	/* Message too long */
#define	EPROTOTYPE	91	/* Protocol wrong type for socket */
#define	ENOPROTOOPT	92	/* Protocol not available */
#define	EPROTONOSUPPORT	93	/* Protocol not supported */
#define	ESOCKTNOSUPPORT	94	/* Socket type not supported */
#define	EOPNOTSUPP	95	/* Operation not supported on transport endpoint */
#define	EPFNOSUPPORT	96	/* Protocol family not supported */
#define	EAFNOSUPPORT	97	/* Address family not supported by protocol */
#define	EADDRINUSE	98	/* Address already in use */
#define	EADDRNOTAVAIL	99	/* Cannot assign requested address */
#define	ENETDOWN	100	/* Network is down */
#define	ENETUNREACH	101	/* Network is unreachable */
#define	ENETRESET	102	/* Network dropped connection because of reset */
#define	ECONNABORTED	103	/* Software caused connection abort */
#define	ECONNRESET	104	/* Connection reset by peer */
#define	ENOBUFS		105	/* No buffer space available */
#define	EISCONN		106	/* Transport endpoint is already connected */
#define	ENOTCONN	107	/* Transport endpoint is not connected */
#define	ESHUTDOWN	108	/* Cannot send after transport endpoint shutdown */
#define	ETOOMANYREFS	109	/* Too many references: cannot splice */
#define	ETIMEDOUT	110	/* Connection timed out */
#define	ECONNREFUSED	111	/* Connection refused */
#define	EHOSTDOWN	112	/* Host is down */
#define	EHOSTUNREACH	113	/* No route to host */
#define	EALREADY	114	/* Operation already in progress */
#define	EINPROGRESS	115	/* Operation now in progress */
#define	ESTALE		116	/* Stale file handle */
#define	EUCLEAN		117	/* Structure needs cleaning */
#define	ENOTNAM		118	/* Not a XENIX named type file */
#define	ENAVAIL		119	/* No XENIX semaphores available */
#define	EISNAM		120	/* Is a named type file */
#define	EREMOTEIO	121	/* Remote I/O error */
#define	EDQUOT		122	/* Quota exceeded */
#define	ENOMEDIUM	123	/* No medium found */
#define	EMEDIUMTYPE	124	/* Wrong medium type */
#define	ECANCELED	125	/* Operation Canceled */
#define	ENOKEY		126	/* Required key not available */
#define	EKEYEXPIRED	127	/* Key has expired */
#define	EKEYREVOKED	128	/* Key has been revoked */
#define	EKEYREJECTED	129	/* Key was rejected by service */

int errno = 0;

#endif // MCLIB_FORWARD_STDLIB_ERROR

#endif // MCLIB_INCLUDED