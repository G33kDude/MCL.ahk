# MCL
A new kind of machine code library for AutoHotkey.

**THIS IS ALPHA SOFTWARE**

Currently only supports AutoHotkey V1 U32/U64.

Requires a gcc-based Windows C/C++ compiler (for example, mingw-w64). MSVC is not supported.

If you run into any problems, please report them as GitHub issues.

## How to use it

### From C/C++

The header `mcl.h` provides macros for "talking" to AHK from your code, mainly, this means "exporting" a function to AHK, or having AHK import a function from a `.dll` file before loading your code.

However, this header is not needed. By default, the `__main` function will be called if there are no "exported" functions, or if `mcl.h` is not used at all.

These macros are as follows:

* `MCL_EXPORT(Name)` <br> will "export" the function `Name` to AHK, allowing the code to function as more of a library. See `Examples/C_as_library.ahk` for an example of using compiled code as a library. <hr>
* `MCL_EXPORT_INLINE(ReturnType, Name, Parameters)` <br> will export *and* define `Name` at the same time, in the same place. <hr>
* `MCL_EXPORT_GLOBAL(Name)` <br> will export `Name` to AHK like it is a function. However, the "address" of the function is truly just the address of the global variable `Name`, and cannot be called like a function. See `Examples/C_set_global_from_AHK.ahk` for an example of getting/setting a C global variable from AHK. <hr>
* `MCL_IMPORT(ReturnType, DllName, Name, ParameterTypes)` <br> will import the function `Name` from the `.dll` file `DllName`, and declare `Name` to be a function pointer to said function, with the given `ReturnType`/`ParameterTypes`. See `Examples/C_import_dll_function.ahk` for an example of this macro.

Additionally, MCL provides a limited set of "fake" standard headers, which all use `mcl.h` to import/define implementations of common standard library functions.

The following headers are provided, but none contain the full set of functions mandated by the C standard. 

* `stddef.h`
* `stdio.h` (note: does not have a `printf` implementation, but most file based functions should work)
* `stdlib.h`
* `string.h` (note: only implements very common operations)
* `time.h`

If calling a standard library function gives you an error, then that function isn't implemented. Feel free to open an issue to request having the function added.

In C++, `mcl.h` uses `malloc`/`free` in order to implement the global `new`/`delete` operators, allowing you to use them in your C++. For an example of using `new`/`delete`, see `Examples/C++_with_new_delete.ahk`.

### From AHK

In AHK, MCL provides all functionality through the `MCL` class. 

Any method which is described as "returning compiled code" can return two different values:

* A pointer to the `__main` function if it is defined. 
* Or if multiple functions are exported to AHK through the `MCL_EXPORT`/`MCL_EXPORT_INLINE` macros (as described above), then an object which maps `{FunctionName: Address}`. 

See `Examples/C_return_struct_to_ahk.ahk` for an example of code with only a `__main` function, and `Examples/C_as_library.ahk` for an example of code which exports multiple functions.

Now, for the API:

* `MCL.FromC(Code, Options := MCL.Options.OutputAHKBit)` 
* `MCL.FromCPP(Code, Options := MCL.Options.OutputAHKBit)` <br> Compiles `Code` as C/C++ (throwing an exception for any compile errors) and returns the compiled code. <hr>
* `MCL.FromString(Code)` <br> Loads `Code`, which is pre-compiled code packed into a string (returned by one of the following methods) and returns the loaded compiled code.
* `MCL.AHKFromC(Code, Options := MCL.Options.OutputAHKBit)`
* `MCL.AHKFromCPP(Code, Options := MCL.Options.OutputAHKBit)` <br> Like `MCL.FromC()`/`MCL.FromCPP()`, but packs the compiled code into a string formatted as AHK source code, which `MCL.FromString(Code)` can load. <hr>
* `MCL.StandaloneAHKFromC(Code, Name, Options := MCL.Options.OutputAHKBit)` 
* `MCL.StandaloneAHKFromCPP(Code, Name, Options := MCL.Options.OutputAHKBit)` <br> Like `MCL.AHKFromC(Code)`, but returns code which does not require MCL at all, and can be used without including `MCL.ahk`. This code will be formatted as a single function, which returns the compiled code in the same format as all other functions.

For any method which takes an `Options` parameter, the following options can be provided to control the bitness/format of the generated code:

* `MCL.Options.OutputAHKBit` <br> Generates code which will run in `A_PtrSize * 8` AHK. So, on AHK U32, this flag tells MCL to generate 32 bit code. On AHK U64, this flag tells MCL to generate 64 bit code.
* `MCL.Options.Output32Bit` <br> Generates 32 bit code, ignoring the bitness of the AHK executable.
* `MCL.Options.Output64Bit` <br> Generates 64 bit code, ignoring the bitness of the AHK executable.
* `MCL.Options.OutputBothBit` <br> Only valid for methods which generate AHK code, such as `MCL.AHKFromC` or `MCL.StandaloneAHKFromCPP`. Generates code which is both 32 and 64 bit, and can run under either. This means compiling the code twice, once for each bitness. This also effectively doubles the size of the code.

However, if MCL is running under 32 bit AHK, the options `MCL.Options.Output64Bit` and `MCL.Options.OutputBothBit` will not work. This is because processing 64 bit code/data requires using 64 bit integers, which are not supported by 32 bit AHK.

Additionally, the following flag exists for the `MCL.AHKFromC` and `MCL.AHKFromCPP` methods:

* `MCL.Options.DoNotFormat` <br> Do not format the resulting code as an AHK string literal, instead simply return it as a string. This allows for chaining, like `MCL.FromString(MCL.AHKFromC(Code, MCL.Options.DoNotFormat))`.

And that's it.

## Differences

Compared to tools like [MCode4GCC](https://github.com/joedf/MCode4GCC) or [mcode-generator](https://github.com/zxc010613/mcode-generator), MCL has a much shorter list of limitations.

With MCL, you can define multiple functions, use global variables, and use function pointers (ex: `return &some_function_defined_in_c;` will return a valid pointer).

Generally, any language feature supported by standard C should work. If you find something that doesn't, please report it as an issue.
