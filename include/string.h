#ifndef STRING_HEADER
#define STRING_HEADER

#include <mcl.h>

MCL_IMPORT(int, msvcrt, isspace, (int));
MCL_IMPORT(int, msvcrt, tolower, (int));
MCL_IMPORT(int, msvcrt, toupper, (int));

MCL_IMPORT(int, msvcrt, strlen, (const char*));
MCL_IMPORT(int, msvcrt, strcmp, (const char*, const char*));
MCL_IMPORT(int, msvcrt, strncmp, (const char*, const char*, size_t));
MCL_IMPORT(char, msvcrt, strcpy, (char*, const char*));
MCL_IMPORT(char*, msvcrt, strchr, (const char*, int));
MCL_IMPORT(char*, msvcrt, strrchr, (const char*, int));
MCL_IMPORT(char*, msvcrt, strncpy, (char*, const char*, size_t));

MCL_IMPORT(char*, msvcrt, strerror, (int));

MCL_IMPORT(int, msvcrt, sscanf, (char*, char*, ...));
MCL_IMPORT(int, msvcrt, sprintf, (char*, const char*, ...));
MCL_IMPORT(int, msvcrt, vsprintf, (char*, const char*, ...));
MCL_IMPORT(int, msvcrt, vsnprintf, (char*, size_t, const char*, ...));

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

#endif // STRING_HEADER