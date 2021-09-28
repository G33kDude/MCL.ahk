#ifndef WINDOWS_HEADER
#define WINDOWS_HEADER

#include <stdint.h>

#define APIENTRY WINAPI

typedef unsigned short WORD;

typedef WORD ATOM;

typedef int BOOL;

typedef unsigned char BYTE;

typedef BYTE BOOLEAN;

#define CALLBACK __stdcall

typedef char CCHAR;

typedef char CHAR;

typedef unsigned long DWORD;

typedef DWORD COLORREF;

#define CONST const

typedef uint64_t DWORDLONG;

#if defined(_WIN64)
typedef uint64_t ULONG_PTR;
#else
typedef unsigned long ULONG_PTR;
#endif

typedef ULONG_PTR DWORD_PTR;

typedef unsigned int DWORD32;

typedef uint64_t DWORD64;

typedef float FLOAT;

typedef void *PVOID;

typedef PVOID HANDLE;

typedef HANDLE HACCEL;

#ifdef _WIN64
typedef int HALF_PTR;
#else
typedef short HALF_PTR;
#endif

typedef HANDLE HBITMAP;

typedef HANDLE HBRUSH;

typedef HANDLE HCOLORSPACE;

typedef HANDLE HCONV;

typedef HANDLE HCONVLIST;

typedef HANDLE HICON;

typedef HICON HCURSOR;

typedef HANDLE HDC;

typedef HANDLE HDDEDATA;

typedef HANDLE HDESK;

typedef HANDLE HDROP;

typedef HANDLE HENHMETAFILE;

typedef int HFILE;

typedef HANDLE HFONT;

typedef HANDLE HGDIOBJ;

typedef HANDLE HGLOBAL;

typedef HANDLE HHOOK;

typedef HANDLE HICON;

typedef HANDLE HINSTANCE;

typedef HANDLE HKEY;

typedef HANDLE HKL;

typedef HANDLE HLOCAL;

typedef HANDLE HMENU;

typedef HANDLE HMETAFILE;

typedef HINSTANCE HMODULE;

// if(WINVER >= 0x0500)
typedef HANDLE HMONITOR;

typedef HANDLE HPALETTE;

typedef HANDLE HPEN;

typedef long LONG;

typedef LONG HRESULT;

typedef HANDLE HRGN;

typedef HANDLE HRSRC;

typedef HANDLE HSZ;

typedef HANDLE WINSTA;

typedef HANDLE HWND;

typedef int INT;

#if defined(_WIN64)
typedef int64_t INT_PTR;
#else
typedef int INT_PTR;
#endif

typedef signed char INT8;

typedef signed short INT16;

typedef signed int INT32;

typedef int64_t INT64;

typedef WORD LANGID;

typedef DWORD LCID;

typedef DWORD LCTYPE;

typedef DWORD LGRPID;

typedef long LONG;

#if !defined(_M_IX86)
typedef int64_t LONGLONG;
#else
typedef double LONGLONG;
#endif

#if defined(_WIN64)
typedef int64_t LONG_PTR;
#else
typedef long LONG_PTR;
#endif

typedef signed int LONG32;

typedef int64_t LONG64;

typedef LONG_PTR LPARAM;

typedef BOOL *LPBOOL;

typedef BYTE *LPBYTE;

typedef DWORD *LPCOLORREF;

typedef CONST CHAR *LPCSTR;

typedef short WCHAR;

typedef CONST WCHAR *LPCWSTR;

#ifdef UNICODE
typedef LPCWSTR LPCTSTR;
#else
typedef LPCSTR LPCTSTR;
#endif

typedef CONST void *LPCVOID;

typedef CONST WCHAR *LPCWSTR;

typedef DWORD *LPDWORD;

typedef HANDLE *LPHANDLE;

typedef int *LPINT;

typedef long *LPLONG;

typedef CHAR *LPSTR;

typedef WCHAR *LPWSTR;

#ifdef UNICODE
typedef LPWSTR LPTSTR;
#else
typedef LPSTR LPTSTR;
#endif

typedef void *LPVOID;

typedef WORD *LPWORD;

typedef LONG_PTR LRESULT;

typedef BOOL *PBOOL;

typedef BOOLEAN *PBOOLEAN;

typedef BYTE *PBYTE;

typedef CHAR *PCHAR;

typedef CONST CHAR *PCSTR;

#ifdef UNICODE
typedef LPCWSTR PCTSTR;
#else
typedef LPCSTR PCTSTR;
#endif

typedef CONST WCHAR *PCWSTR;

typedef DWORD *PDWORD;

typedef DWORDLONG *PDWORDLONG;

typedef DWORD_PTR *PDWORD_PTR;

typedef DWORD32 *PDWORD32;

typedef DWORD64 *PDWORD64;

typedef FLOAT *PFLOAT;

#ifdef _WIN64
typedef HALF_PTR *PHALF_PTR;
#else
typedef HALF_PTR *PHALF_PTR;
#endif

typedef HANDLE *PHANDLE;

typedef HKEY *PHKEY;

typedef int *PINT;

typedef INT_PTR *PINT_PTR;

typedef INT8 *PINT8;

typedef INT16 *PINT16;

typedef INT32 *PINT32;

typedef INT64 *PINT64;

typedef PDWORD PLCID;

typedef LONG *PLONG;

typedef LONGLONG *PLONGLONG;

typedef LONG_PTR *PLONG_PTR;

typedef LONG32 *PLONG32;

typedef LONG64 *PLONG64;

// #if defined(_WIN64)
// #define POINTER_32 __ptr32
// #else
// #define POINTER_32
// #endif

// #if (_MSC_VER >= 1300)
// #define POINTER_64 __ptr64
// #else
// #define POINTER_64
// #endif

#define POINTER_SIGNED __sptr

#define POINTER_UNSIGNED __uptr

typedef short SHORT;

typedef SHORT *PSHORT;

typedef ULONG_PTR SIZE_T;

typedef SIZE_T *PSIZE_T;

typedef LONG_PTR SSIZE_T;

typedef SSIZE_T *PSSIZE_T;

typedef CHAR *PSTR;

#ifdef UNICODE
typedef WCHAR TBYTE;
#else
typedef unsigned char TBYTE;
#endif

typedef TBYTE *PTBYTE;

#ifdef UNICODE
typedef WCHAR TCHAR;
#else
typedef char TCHAR;
#endif

typedef TCHAR *PTCHAR;

#ifdef UNICODE
typedef LPWSTR PTSTR;
#else
typedef LPSTR PTSTR;
#endif

typedef unsigned char UCHAR;

typedef UCHAR *PUCHAR;

#ifdef _WIN64
typedef unsigned int UHALF_PTR;
#else
typedef unsigned short UHALF_PTR;
#endif

#ifdef _WIN64
typedef UHALF_PTR *PUHALF_PTR;
#else
typedef UHALF_PTR *PUHALF_PTR;
#endif

typedef unsigned int UINT;

typedef UINT *PUINT;

#if defined(_WIN64)
typedef uint64_t UINT_PTR;
#else
typedef unsigned int UINT_PTR;
#endif

typedef unsigned char UINT8;

typedef unsigned short UINT16;

typedef unsigned int UINT32;

typedef uint64_t UINT64;

typedef UINT_PTR *PUINT_PTR;

typedef UINT8 *PUINT8;

typedef UINT16 *PUINT16;

typedef UINT32 *PUINT32;

typedef UINT64 *PUINT64;

typedef unsigned long ULONG;

#if !defined(_M_IX86)
typedef uint64_t ULONGLONG;
#else
typedef double ULONGLONG;
#endif

typedef unsigned int ULONG32;

typedef uint64_t ULONG64;

typedef ULONG *PULONG;

typedef ULONGLONG *PULONGLONG;

typedef ULONG_PTR *PULONG_PTR;

typedef ULONG32 *PULONG32;

typedef ULONG64 *PULONG64;

typedef unsigned short USHORT;

typedef USHORT *PUSHORT;

typedef WCHAR *PWCHAR;

typedef WORD *PWORD;

typedef WCHAR *PWSTR;

typedef uint64_t QWORD;

typedef HANDLE SC_HANDLE;

typedef LPVOID SC_LOCK;

typedef HANDLE SERVICE_STATUS_HANDLE;

typedef struct _UNICODE_STRING
{
    USHORT Length;
    USHORT MaximumLength;
    PWSTR Buffer;
} UNICODE_STRING;
typedef UNICODE_STRING *PUNICODE_STRING;
typedef const UNICODE_STRING *PCUNICODE_STRING;

typedef LONGLONG USN;

#define VOID void

#define WINAPI __stdcall

typedef UINT_PTR WPARAM;

#endif // WINDOWS_HEADER