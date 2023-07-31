#ifndef MCL_HEADER
#define MCL_HEADER

#if MCL_BITNESS == 32
#define MCL_CALLING_CONVENTION __attribute__((__stdcall__))
#else
#define MCL_CALLING_CONVENTION
#endif

#define MCL_JOIN1(x, ...) MCL_JOIN2(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN2(x, ...) MCL_JOIN3(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN3(x, ...) MCL_JOIN4(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN4(x, ...) MCL_JOIN5(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN5(x, ...) MCL_JOIN6(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN6(x, ...) MCL_JOIN7(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN7(x, ...) MCL_JOIN8(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN8(x, ...) MCL_JOIN9(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN9(x, ...) MCL_JOIN10(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN10(x, ...) MCL_JOIN11(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN11(x, ...) MCL_JOIN12(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN12(x, ...) MCL_JOIN13(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN13(x, ...) MCL_JOIN14(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN14(x, ...) MCL_JOIN15(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN15(x, ...) MCL_JOIN16(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN16(x, ...) MCL_JOIN17(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN17(x, ...) MCL_JOIN18(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN18(x, ...) MCL_JOIN19(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN19(x, ...) x##__VA_OPT__($ERROR)

#define MCL_JOIN(x, ...) MCL_JOIN1(x##__VA_OPT__($##__VA_ARGS__))
#define MCL_JOIN_LAST(x, ...) MCL_JOIN19(x, __VA_ARGS__)

#define MCL_CONCAT_LATE(x, y) x ## y
#define MCL_CONCAT(x, y) MCL_CONCAT_LATE(x, y)

#define MCL_EXPORT(name, ...) \
int MCL_CONCAT(__MCL_f_, MCL_CONCAT(MCL_CONCAT(name, __VA_OPT__($)), MCL_JOIN(__VA_ARGS__))) () __attribute__((alias(#name)));

#define MCL_DEFER(x, ...) x(__VA_ARGS__)

#define MCL_EXPORT_GLOBAL_BODY(Name, SymbolName) \
extern char __MCL_g_ ## SymbolName __attribute__((alias(#Name)));

#define MCL_EXPORT_GLOBAL(Name, ...) \
MCL_DEFER(MCL_EXPORT_GLOBAL_BODY, Name, MCL_CONCAT(Name, __VA_OPT__(MCL_CONCAT($, MCL_JOIN_LAST(__VA_ARGS__)))))

#define MCL_EXPORT_INLINE(ReturnType, Name, Parameters, ...) \
MCL_EXPORT(Name, __VA_ARGS__) \
ReturnType Name Parameters

#define MCL_QUOTE(X) #X

#define MCL_IMPORT(ReturnType, DllName, Name, ParameterTypes) \
ReturnType (* MCL_CALLING_CONVENTION (__MCL_i_ ## DllName ## $ ## Name))ParameterTypes = (ReturnType (MCL_CALLING_CONVENTION *)ParameterTypes)0; \
static ReturnType __attribute__((alias(MCL_QUOTE(__MCL_i_ ## DllName ## $ ## Name)))) (* MCL_CALLING_CONVENTION Name) ParameterTypes

#define main __main

#ifdef MCL_LIBRARY
void __main() {};
#endif

#endif // MCL_HEADER
