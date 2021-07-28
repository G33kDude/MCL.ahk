#ifndef MCL_HEADER
#define MCL_HEADER

#if MCL_BITNESS == 32
#define MCL_CALLING_CONVENTION __attribute__((__stdcall__))
#else
#define MCL_CALLING_CONVENTION
#endif

#define MCL_EXPORT(Name) \
int __MCL_e_ ## Name () __attribute__((alias(#Name)));

#define MCL_EXPORT_INLINE(ReturnType, Name, Parameters) \
int __MCL_e_ ## Name () __attribute__((alias(#Name))); \
ReturnType Name Parameters

#define MCL_EXPORT_GLOBAL(Name) \
extern char __MCL_e_ ## Name __attribute__((alias(#Name)));

#define MCL_QUOTE(X) #X

#define MCL_IMPORT(ReturnType, DllName, Name, ParameterTypes) \
ReturnType (* MCL_CALLING_CONVENTION (__MCL_i_ ## DllName ## $ ## Name))ParameterTypes = (ReturnType (MCL_CALLING_CONVENTION *)ParameterTypes)0; \
static ReturnType __attribute__((alias(MCL_QUOTE(__MCL_i_ ## DllName ## $ ## Name)))) (* MCL_CALLING_CONVENTION Name) ParameterTypes

#define main __main

#ifdef MCL_LIBRARY
void __main() {};
#endif

#endif // MCL_HEADER