#ifndef MCL_HEADER
#define MCL_HEADER

#define MCL_EXPORT(Name) \
int __MCL_e_ ## Name () __attribute__((alias(#Name)));

#define MCL_EXPORT_INLINE(ReturnType, Name, Parameters) \
int __MCL_e_ ## Name () __attribute__((alias(#Name))); \
ReturnType Name Parameters

#define MCL_QUOTE(X) #X

#define MCL_IMPORT(ReturnType, DllName, Name, ParameterTypes) \
ReturnType (* __MCL_i_ ## DllName ## _ ## Name)ParameterTypes = (ReturnType(*)ParameterTypes)0; \
static ReturnType __attribute__((alias(MCL_QUOTE(__MCL_i_ ## DllName ## _ ## Name)))) (*Name) ParameterTypes;

#define main __main

#ifdef MCL_LIBRARY
void __main() {};
#endif

#endif // MCL_HEADER