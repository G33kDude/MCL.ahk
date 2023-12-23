#Requires AutoHotkey v2.0
#Include %A_LineFile%\..\Linker.ahk

/**
 * MCL
 */
class MCL {
    /**
     * LZ Compression
     */
    class LZ {
        /**
         * Compresses the given data buffer.
         * 
         * @param {Buffer} data - The data to compress
         * 
         * @return {Buffer} - A new buffer containing the compressed data
         */
        static Compress(data) {
            if (r := DllCall("ntdll\RtlGetCompressionWorkSpaceSize",
                "UShort", 0x102,         ; USHORT CompressionFormatAndEngine
                "UInt*", &cbwsSize := 0, ; PULONG CompressBufferWorkSpaceSize
                "UInt*", &cfwsSize := 0, ; PULONG CompressFragmentWorkSpaceSize
                "UInt")) ; NTSTATUS
                throw Error("Erorr calling RtlGetCompressionWorkSpaceSize", , Format("0x{:08x}", r))

            cbws := Buffer(cbwsSize)

            ; Make sure the buffer is big enough to hold the compresse data
            cData := Buffer(data.Size * 2)

            if (r := DllCall("ntdll\RtlCompressBuffer",
                "UShort", 0x102,          ; USHORT CompressionFormatAndEngine
                "Ptr", data,              ; PUCHAR UncompressedBuffer
                "UInt", data.Size,        ; ULONG  UncompressedBufferSize
                "Ptr", cData,             ; PUCHAR CompressedBuffer
                "UInt", cData.Size,       ; ULONG  CompressedBufferSize
                "UInt", cfwsSize,         ; ULONG  UncompressedChunkSize
                "UInt*", &finalSize := 0, ; PULONG FinalCompressedSize
                "Ptr", cbws,              ; PVOID  WorkSpace
                "UInt")) ; NTSTATUS
                throw Error("Error calling RtlCompressBuffer", , Format("0x{:08x}", r))

            cData.Size := finalSize
            return cData
        }

        /**
         * Decompresses the given data buffer.
         * 
         * The buffer will be resized to match the actual length of the
         * decompressed data.
         * 
         * @param {Buffer} cData - The compressed data buffer
         * @param {Buffer} data - A buffer large enough to hold the decompressed data
         */
        static Decompress(cData, data) {
            if (r := DllCall("ntdll\RtlDecompressBuffer",
                "UShort", 0x102,    ; USHORT CompressionFormat
                "Ptr", data,    ; PUCHAR UncompressedBuffer
                "UInt", data.Ptr,    ; ULONG  UncompressedBufferSize
                "Ptr", cData,    ; PUCHAR CompressedBuffer
                "UInt", cData.Ptr,    ; ULONG  CompressedBufferSize,
                "UInt*", &cbFinal := 0,    ; PULONG FinalUncompressedSize
                "UInt")) ; NTSTATUS
                throw Error("Error calling RtlDecompressBuffer", , Format("0x{:08x}", r))
            data.Size := cbFinal
        }
    }

    class Base64 {
        /**
         * Converts a data buffer to a base64 string.
         * 
         * @param {Buffer} data - The data buffer
         * 
         * @return {String} - The encoded data
         */
        static Encode(data) {
            cbts(data, pBase64, &size) => DllCall("Crypt32\CryptBinaryToString",
                "Ptr", data,        ; const BYTE *pbBinary
                "UInt", data.Size,  ; DWORD      cbBinary
                "UInt", 0x40000001, ; DWORD      dwFlags = CRYPT_STRING_BASE64 | CRYPT_STRING_NOCRLF
                "Ptr", pBase64,     ; LPWSTR     pszString
                "UInt*", &size,     ; DWORD      *pcchString
                "UInt") ; BOOL

            if !cbts(data, 0, &size := 0)
                throw Error("Failed to calculate b64 size")
            base64 := Buffer(size * 2)
            if !cbts(data, base64, &size)
                throw Error("Failed to convert to b64")
            return StrGet(base64)
        }

        /**
         * Converts a base64 string to a data buffer.
         * 
         * @param {String} base64 - The base64 string
         * 
         * @return {Buffer} - The data buffer
         */
        static Decode(base64) {
            cstb(base64, data, &size) => DllCall("Crypt32\CryptStringToBinary",
                "Str", base64,  ; [in]      LPCWSTR pszString,
                "UInt", 0,      ; [in]      DWORD   cchString,
                "UInt", 1,      ; [in]      DWORD   dwFlags,
                "Ptr", data,    ; [in]      BYTE    *pbBinary,
                "UInt*", &size, ; [in, out] DWORD   *pcbBinary,
                "Ptr", 0,       ; [out]     DWORD   *pdwSkip,
                "Ptr", 0,       ; [out]     DWORD   *pdwFlags
                "UInt") ; BOOL

            if !cstb(base64, 0, &size := 0)
                throw Error("Failed to parse b64 to binary")
            decoded := Buffer(size)
            if !cstb(base64, decoded, &size)
                throw Error("Failed to convert b64 to binary")

            return decoded
        }
    }

    /**
     * Generate a unique name for a temporary file in a given directory.
     * 
     * @param {String} baseDir - The base directory.
     * @param {String} prefix - A prefix for the file name.
     * @param {String} suffix - A suffix for the file name.
     * 
     * @return {String} - The unique name, relative to `baseDir
     */
    static GetTempPath(baseDir := A_Temp, prefix := "", suffix := ".tmp") {
        Loop {
            DllCall("QueryPerformanceCounter", "Int64*", &counter := 0)
            fileName := baseDir "\" prefix . counter . suffix
        } until !FileExist(fileName)
        return prefix . counter . suffix
    }

    /** @type {String} - A platform-specific prefix to apply to the compiler file name */
    static CompilerPrefix := ""

    /** @type {String} - A platform-specific suffix to apply to the compiler file name */
    static CompilerSuffix := ".exe"

    /**
     * Checks if the compilers can be found with the given prefix and suffix.
     * 
     * True results are wrapped in an array since `Prefix` could be an empty
     *  string but still be a correct compiler path. So we can't just
     *   `return Prefix` since `Prefix` itself can be false.
     * 
     * @param {String} prefix - The candidate prefix
     * @param {String} suffix - The candidate suffix
     * 
     * @return False or a single of the prefix required to find the compiler
     */
    static CompilersExist(prefix, suffix) {
        if (FileExist(prefix "gcc" suffix) && FileExist(prefix "g++" suffix))
            return [prefix]

        for k, folder in StrSplit(EnvGet("PATH"), ";") {
            newPrefix := folder "\" prefix

            if (FileExist(newPrefix "gcc" suffix) && FileExist(newPrefix "g++" suffix))
                return [newPrefix]
        }

        return false
    }

    /**
     * Attempt to automatically set CompilerPrefix based on PATH and known
     * cross compiler naming schemes.
     */
    static TryFindCompilers() {
        ; The default (or user provided values) are correct, no changes needed
        if this.CompilersExist(this.CompilerPrefix, this.CompilerSuffix)
            return

        ; Compiler executables might be named like they would be on Linux
        if (prefix := this.CompilersExist(this.CompilerPrefix "x86_64-w64-mingw32-", this.CompilerSuffix)) {
            this.CompilerPrefix := Prefix[1]
            return
        }

        ; Oops, I don't know any other naming schemes. Ah well, this is good
        ; enough to autodected mingw-w64 installed through cygwin.

        throw Error(
            "MCL couldn't find gcc and g++, please manually specify the path "
            "both can be found at via 'MCL.CompilerPrefix'"
        )
    }

    /**
     * Compile the given code with the given compiler
     * 
     * @param {String} compiler - The compiler to use (e.g. `"gcc"`, `"g++"`)
     * @param {String} code - The source code to be compiled
     * @param {String} [extraOptions] - Any extra options to pass to the compiler
     *     on the command line
     * @param {Integer} [bitness] - The bitness to target (e.g. 32, 64)
     * 
     * @return {Array} A bunch of stuff
     */
    static Compile(compiler, code, extraOptions := "", bitness := A_PtrSize * 8) {

        StdoutToVar(sCmd, sDir:="", sEnc:="CP0") {
            ; Create 2 buffer-like objects to wrap the handles to take advantage of the __Delete meta-function.
            oHndStdoutRd := { Ptr: 0, __Delete: delete(this) => DllCall("CloseHandle", "Ptr", this) }
            oHndStdoutWr := { Base: oHndStdoutRd }
            
            If !DllCall( "CreatePipe"
                    , "PtrP" , oHndStdoutRd
                    , "PtrP" , oHndStdoutWr
                    , "Ptr"  , 0
                    , "UInt" , 0 )
                Throw OSError(,, "Error creating pipe.")
            If !DllCall( "SetHandleInformation"
                    , "Ptr"  , oHndStdoutWr
                    , "UInt" , 1
                    , "UInt" , 1 )
                Throw OSError(,, "Error setting handle information.")

            PI := Buffer(A_PtrSize == 4 ? 16 : 24,  0)
            SI := Buffer(A_PtrSize == 4 ? 68 : 104, 0)
            NumPut( "UInt", SI.Size,          SI,  0 )
            NumPut( "UInt", 0x100,            SI, A_PtrSize == 4 ? 44 : 60 )
            NumPut( "Ptr",  oHndStdoutWr.Ptr, SI, A_PtrSize == 4 ? 60 : 88 )
            NumPut( "Ptr",  oHndStdoutWr.Ptr, SI, A_PtrSize == 4 ? 64 : 96 )

            If !DllCall( "CreateProcess"
                    , "Ptr"  , 0
                    , "Str"  , sCmd
                    , "Ptr"  , 0
                    , "Ptr"  , 0
                    , "Int"  , True
                    , "UInt" , 0x08000000
                    , "Ptr"  , 0
                    , "Ptr"  , sDir ? StrPtr(sDir) : 0
                    , "Ptr"  , SI
                    , "Ptr"  , PI )
                Throw OSError(,, "Error creating process.")

            ; The write pipe must be closed before reading the stdout so we release the object.
            ; The reading pipe will be released automatically on function return.
            oHndStdOutWr := ""

            ; Before reading, we check if the pipe has been written to, so we avoid freezings.
            nAvail := 0, nLen := 0
            While DllCall( "PeekNamedPipe"
                        , "Ptr"   , oHndStdoutRd
                        , "Ptr"   , 0
                        , "UInt"  , 0
                        , "Ptr"   , 0
                        , "UIntP" , &nAvail
                        , "Ptr"   , 0 ) != 0
            {
                ; If the pipe buffer is empty, sleep and continue checking.
                If !nAvail && Sleep(100)
                    Continue
                cBuf := Buffer(nAvail+1)
                DllCall( "ReadFile"
                    , "Ptr"  , oHndStdoutRd
                    , "Ptr"  , cBuf
                    , "UInt" , nAvail
                    , "PtrP" , &nLen
                    , "Ptr"  , 0 )
                sOutput .= StrGet(cBuf, nLen, sEnc)
            }
            
            ; Get the exit code, close all process handles and return the output object.
            DllCall( "GetExitCodeProcess"
                , "Ptr"   , NumGet(PI, 0, "Ptr")
                , "UIntP" , &nExitCode:=0 )
            DllCall( "CloseHandle", "Ptr", NumGet(PI, 0, "Ptr") )
            DllCall( "CloseHandle", "Ptr", NumGet(PI, A_PtrSize, "Ptr") )
            Return { Output: sOutput, ExitCode: nExitCode } 
        }

        this.TryFindCompilers()

        includeFolder := this.GetTempPath(A_WorkingDir, "mcl-include-", "")
        inputFile := this.GetTempPath(A_WorkingDir, "mcl-input-", ".c")
        outputFile := this.GetTempPath(A_WorkingDir, "mcl-output-", ".o")

        try {
            FileOpen(inputFile, "w").Write(code)
            while !FileExist(inputFile)
                Sleep 100

            DirCopy A_LineFile "/../include", includeFolder

            out := StdoutToVar(
                this.CompilerPrefix compiler this.CompilerSuffix " "
                "-m" bitness " "
                inputFile " "
                "-o " outputFile " "
                "-I " includeFolder " "
                "-D MCL_BITNESS=" bitness extraOptions " "
                "-ffreestanding "
                "-nostdlib "
                "-Wno-attribute-alias "
                "-fno-leading-underscore "
                "--function-sections "
                "--data-sections "
                "-c "
                "-Os "
            )

            if out.ExitCode
               throw Error(StrReplace(out.Output, " ", " "), , "Compiler Error")

            if !(f := FileOpen(OutputFile, "r"))
                throw Error("Failed to load output file")

            if !(pe := Buffer(f.Length))
                throw Error("Failed to reserve MCL PE memory")

            f.RawRead(pe, f.Length)
            f.Close()
        } finally {
            FileDelete inputFile
            DirDelete includeFolder, 1

            try {
                ; Depending on what when wrong, the output file might not exist yet.
                ; And if we're already in a `try {}` block, then this line will throw when trying to delete the file.

                FileDelete outputFile
            }
        }

        ; TODO
        Linker := PEObjectLinker(pe.Ptr, pe.Size)
        Linker.Read()

        TextSection := Linker.SectionsByName[".text"]

        NonExportedFunctions := []
        SingleFunction := false
        ExportCount := 0

        static COFF_SYMBOL_TYPE_FUNCTION := 0x20
        static COFF_SYMBOL_STORAGE_CLASS_STATIC := 0x3

        for SymbolName, Symbol in Linker.SymbolsByName {
            if (SymbolName = "__main" || RegExMatch(SymbolName, "^__MCL_[fg]_(\w+)")) {
                Linker.MergeSections(TextSection, Symbol.Section)
                ExportCount++
            } else if (Symbol.Type = COFF_SYMBOL_TYPE_FUNCTION && Symbol.StorageClass != COFF_SYMBOL_STORAGE_CLASS_STATIC) {
                NonExportedFunctions.Push(Symbol)
            }
        }
		
		OnUndefinedSymbolReference(Relocation) {
			Name := Relocation.Symbol.Name
			
			;MsgBox('Undefined ' Name)
			
			for SymbolName, Symbol in Linker.SymbolsByName {
				if (Name != SymbolName && InStr(SymbolName, "w_" Name) && InStr(SymbolName, ".text")) {
					;MsgBox('Mapped ' Name ' => ' SymbolName)
					Relocation.Symbol := Symbol
					return
				}
			}
			
			throw Error("Reference to undefined symbol " Name)
		}
		
		Linker.ResolveUndefinedSymbolReferences(OnUndefinedSymbolReference)

        if (ExportCount = 0 && NonExportedFunctions.Length = 1) {
            ; Special case for compatibility with old mcode, if there's only one function defined (and none exported)
            ;  then we'll help out and turn that function into `__main` (like old mcode expects) and return
            ;   a pointer directly to it in `.Load` which should make things easier to port

            SingleFunction := NonExportedFunctions[1]

            Linker.MergeSections(TextSection, SingleFunction.Section)
        }

        Linker.MakeSectionStandalone(TextSection)
        Linker.DoStaticRelocations(TextSection)

        Imports := Map()
        Exports := Map()

        if IsObject(SingleFunction)
            Exports[SingleFunction.Name] := SingleFunction.Value

        for SymbolName, Symbol in TextSection.SymbolsByName {
            if (SymbolName = "__main") {
                Exports[SymbolName] := Symbol.Value
                continue
            }

            if RegExMatch(SymbolName, "^__MCL_([ifg])_([\w\$]+?)(?:\$([\w\$]+))?$", &Match) {
                if (Match[1] = "i") {
                    RegExMatch(SymbolName, "^__MCL_([ifg])_([\w\$]+)$", &Match)
                    Imports[Match[2]] := Symbol.Value
                    continue
                }

                if (Match[3] ~= "\$ERROR$")
                    throw Error("Too many parameters given to export of " Match[2])

                Exports[Match[2]] := { value: Symbol.Value, type: Match[1], types: Match[3] }
            }
        }

        if (Exports.Count = 0)
            throw Error("Code does not define '__main', or export any functions")

        static IMAGE_REL_I386_DIR32 := 0x6
        static IMAGE_REL_AMD64_ADDR64 := 0x1

        Relocations := Map()
        RelocationDataType := Linker.Is32Bit ? "Int" : "Int64"

        for k, Relocation in TextSection.Relocations {
            if (!IsSet(Relocation))
                continue

            if (Relocation.Symbol.Section.Name != ".text")
                continue

            if (Relocation.Type != IMAGE_REL_I386_DIR32 && Relocation.Type != IMAGE_REL_AMD64_ADDR64)
                continue

            Offset := TextSection.Data.Read(Relocation.Address, RelocationDataType)
            Address := Relocation.Symbol.Value + Offset
            TextSection.Data.Write(Address, Relocation.Address, RelocationDataType)

            Relocations[k] := Relocation.Address
        }

        CodeSize := TextSection.Data.Length()

        if !(code := Buffer(CodeSize))
            throw Error("Failed to reserve MCL PE memory")

        TextSection.Data.Coalesce(code.Ptr)

        return [code, Imports, Exports, Relocations, Bitness]
    }

    static Load(code, Imports, Exports, Relocations, Bitness) {
        for ImportName, ImportOffset in Imports {
            Import := StrSplit(ImportName, "$")
            DllName := Import[1]
            FunctionName := Import[2]

            ; Clear A_LastError. Something has left it set here before, and GetModuleHandle doesn't clear it.
            DllCall("SetLastError", "Int", 0)

            hDll := DllCall("GetModuleHandle", "Str", DllName, "Ptr")
            if A_LastError
                throw Error("Could not load dll " DllName ", LastError " Format("{:0x}", A_LastError))

            DllCall("SetLastError", "Int", 0)
            pFunction := DllCall("GetProcAddress", "Ptr", hDll, "AStr", FunctionName, "Ptr")
            if A_LastError
                throw Error("Could not find function " FunctionName " from " DllName ".dll, LastError " Format("{:0x}", A_LastError))

            NumPut("Ptr", pFunction, code.Ptr, ImportOffset)
        }

        for k, Offset in Relocations {
            Old := NumGet(code.Ptr, Offset, "Ptr")
            NumPut("Ptr", Old + code.Ptr, code.Ptr, Offset)
        }

        if !DllCall("VirtualProtect", "Ptr", code.Ptr, "Ptr", code.Size, "UInt", 0x40, "UInt*", &OldProtect := 0, "UInt")
            throw Error("Failed to mark MCL memory as executable")

        for name, data in Exports
            data.value += code.Ptr

        library := {
            code: code,
            exports: Exports,
            Call: (this, params*) => this.__main(params*)
        }

        for name, data in Exports {
            if (data.type = "f") { ; function
                params := []
                for k, v in StrSplit(data.types, "$")
                    (A_Index & 1) ? params.Push(StrReplace(v, '_', ' ')) : params.Push(unset)
                library.%name% := ((this, p*) => DllCall(p*)).Bind(unset, data.value, params*)
            }
            if (data.type = "g") { ; global
                library.DefineProp(name, {
                    get: ((this, p*) => NumGet(p*)).Bind(unset, data.value, data.types),
                    set: ((this, p*) => NumPut(p*)).Bind(unset, data.types, unset, data.value)
                })
            }
        }

        return library
    }

    ; static Pack(FormatAsStringLiteral, code, Imports, Exports, Relocations, Bitness) {

    ;     compressed := MCL.LZ.Compress(code)
    ;     Base64 := MCL.Base64.Encode(compressed)

    ;     SymbolsString := ""

    ;     for ImportName, ImportOffset in Imports
    ;         SymbolsString .= ImportName ":" ImportOffset ","

    ;     SymbolsString := SubStr(SymbolsString, 1, -1) ";"

    ;     for ExportName, ExportOffset in Exports
    ;         SymbolsString .= ExportName ":" ExportOffset ","

    ;     SymbolsString := SubStr(SymbolsString, 1, -1)

    ;     RelocationsString := ""

    ;     for k, RelocationOffset in Relocations
    ;         RelocationsString .= RelocationOffset ","

    ;     RelocationsString := SubStr(RelocationsString, 1, -1)

    ;     ; And finally, format our output

    ;     if (FormatAsStringLiteral) {
    ;         ; Format the output as an AHK string literal, which can be dropped directly into a source file
    ;         Out := ""

    ;         while StrLen(Base64) {
    ;             Out .= '`n. "' SubStr(Base64, 1, 120 - 8) '"'
    ;             Base64 := SubStr(Base64, (120 - 8) + 1)
    ;         }

    ;         return Bitness ";" SymbolsString ";" RelocationsString ";" CodeSize ";" "" Out
    ;     } else {
    ;         ; Don't format the output, return the string that would result from AHK parsing the string literal returned when `FormatAsStringLiteral` is true

    ;         return Bitness ";" SymbolsString ";" RelocationsString ";" CodeSize ";" Base64
    ;     }
    ; }

    static StandalonePack(Name, code, Imports, Exports, Relocations, Bitness) {
        RunTemplate(input, context := {}, ahkPath := A_AhkPath) {
            Quote(text) {
                for pair in [["``", "``" "``"], ["`r", "``r"], ["`n", "``n"], ["`t", "``t"], ["'", "``'"]]
                    text := StrReplace(text, pair*)
                return "'" text "'"
            }
            code := "", pos := 1
            while foundPos := RegExMatch(input, "s)<\?\s*(.*?)\s*\?>", &match, pos) {
                code .= (
                    (foundPos <= pos ? "" : "ctx.result .= " Quote(SubStr(input, pos, foundPos - pos)) "`n")
                    (match.1 ~= "^=" ? "ctx.result .= (" SubStr(match.1, 2) ")`n" : match.1 "`n")
                ), pos := foundPos + match.Len
            }
            if pos <= StrLen(input)
                code .= "ctx.result .= " Quote(SubStr(input, pos)) "`n"
            IID_IDispatch := Buffer(16)
            NumPut("Int64", 0x20400, "Int64", 0x46000000000000c0, IID_IDispatch)
            lResult := DllCall("oleacc\LresultFromObject", "Ptr", IID_IDispatch, "Ptr", 0, "Ptr", ObjPtr(context), "Ptr")
            try {
                exec := ComObject("WScript.Shell").Exec('"' ahkPath '" *')
                exec.StdIn.Write(Format(
                    'ctx := ((lResult) => (`n'
                    '    IID_IDispatch := Buffer(16),`n'
                    '       NumPut("Int64", 0x20400, "Int64", 0x46000000000000c0, IID_IDispatch),`n'
                    '       DllCall("oleacc\ObjectFromLresult", "Ptr", lResult, "Ptr", IID_IDispatch, "Ptr", 0, "Ptr*", &pObj := 0),`n'
                    '       ComValue(9, pObj)`n'
                    '))({})`n'
                    '{}',
                    lResult,
                    code))
                exec.StdIn.Close()
                while exec.Status = 0
                    Sleep(10)
            } catch Any as e {
                DllCall("oleacc\ObjectFromLresult", "Ptr", lResult, "Ptr", IID_IDispatch, "Ptr", 0, "Ptr*", ComValue(9, 0))
                throw e
            }
            return context.result
        }

        compressed := MCL.LZ.Compress(code)
        base64 := MCL.Base64.Encode(compressed)

        template := FileRead(A_LineFile "/../StandaloneTemplate_AsFunc.atp")
        template := RunTemplate(template, {
            name: Name,
            base64: base64,
            codeSize: code.size,
            bitness: Bitness,
            compressedSize: compressed.Size,
            imports: imports,
            relocations: relocations,
            exports: exports,
            result: ""
        })

        template := RegexReplace(template, "\r?\n\s*\r?\n", "`n")

        return template
    }

    class Options {
        static OutputAHKBit => 0
        static Output32Bit => 1
        static Output64Bit => 2
        static OutputBothBit => 3
        static OutputMask => 3

        static FormatAsStringLiteral => 0
        static DoNotFormat => 4
    }

    static AHKFromLanguage(Compiler, Code, Options, CompilerOptions := "") {
        Literal := !(Options & MCL.Options.DoNotFormat)

        Result := (Literal ? '"' : '') "V0.3|"

        if (Options & MCL.Options.OutputMask = MCL.Options.OutputAHKBit) {
            Result .= MCL.Pack(Literal, MCL.Compile(Compiler, Code, CompilerOptions)*)
            return Result
        }

        if (Options & MCL.Options.Output32Bit)
            Result .= MCL.Pack(Literal, MCL.Compile(Compiler, Code, CompilerOptions, 32)*)

        if (Options & MCL.Options.Output64Bit) {
            if (Options & MCL.Options.Output32Bit)
                Result .= (Literal ? '`n."' : '') "|"

            Result .= MCL.Pack(Literal, MCL.Compile(Compiler, Code, CompilerOptions, 64)*)
        }

        return Result
    }

    static StandaloneAHKFromLanguage(Compiler, Code, Options, CompilerOptions := "", Name := "") {
        if (Options & MCL.Options.OutputMask = MCL.Options.OutputAHKBit)
            return MCL.StandalonePack(Name, MCL.Compile(Compiler, Code, CompilerOptions)*)

        Result := ""

        if (Options & MCL.Options.Output32Bit) {
            Result .= MCL.StandalonePack(Name "32Bit", MCL.Compile(Compiler, Code, CompilerOptions, 32)*) "`n"
        }

        if (Options & MCL.Options.Output64Bit) {
            Result .= MCL.StandalonePack(Name "64Bit", MCL.Compile(Compiler, Code, CompilerOptions, 64)*) "`n"

            if (Options & MCL.Options.Output32Bit) {
                Result .= Name "() {`n`treturn A_PtrSize = 4 ? this." Name "32Bit() : this." Name "64Bit()`n}`n"
            }
        }

        return Result
    }

    static FromC(Code, Options := 0) {
        Bitness := A_PtrSize * 8

        if (Options & MCL.Options.Output32Bit) {
            Bitness := 32
        } else if (Options & MCL.Options.Output64Bit) {
            Bitness := 64
        }

        return MCL.Load(MCL.Compile("gcc", Code, , Bitness)*)
    }
    static AHKFromC(Code, Options := 0) {
        return MCL.AHKFromLanguage("gcc", Code, Options)
    }
    static StandaloneAHKFromC(Code, Options := 0, Name := "MyC") {
        return MCL.StandaloneAHKFromLanguage("gcc", Code, Options, , Name)
    }

    static FromCPP(Code, Options := 0) {
        Bitness := A_PtrSize * 8

        if (Options & MCL.Options.Output32Bit) {
            Bitness := 32
        } else if (Options & MCL.Options.Output64Bit) {
            Bitness := 64
        }

        return MCL.Load(MCL.Compile("g++", 'extern "C" {`n' Code "`n}", " -fno-exceptions -fno-rtti", Bitness)*)
    }
    static AHKFromCPP(Code, Options := 0) {
        return MCL.AHKFromLanguage("g++", 'extern "C" {`n' Code "`n}", Options, " -fno-exceptions -fno-rtti")
    }
    static StandaloneAHKFromCPP(Code, Options := 0, Name := "MyCPP") {
        return MCL.StandaloneAHKFromLanguage("g++", 'extern "C" {`n' Code "`n}", Options, " -fno-exceptions -fno-rtti", Name)
    }

    static FromString(Code) {
        Parts := StrSplit(Code, "|")
        Version := Parts.RemoveAt(1)

        if (Version != "V0.3")
            throw Error("Unknown/corrupt MCL packed code format")

        for k, Flavor in Parts {
            Flavor := StrSplit(Flavor, ";")

            if (Flavor.Length != 6)
                throw Error("Unknown/corrupt MCL packed code format")

            Bitness := Flavor[1] + 0
            ImportsString := Flavor[2]
            ExportsString := Flavor[3]
            RelocationsString := Flavor[4]
            CodeSize := Flavor[5] + 0
            CodeBase64 := Flavor[6]

            if (Bitness != (A_PtrSize * 8))
                continue

            Imports := Map()

            for k, ImportEntry in StrSplit(ImportsString, ",") {
                ImportEntry := StrSplit(ImportEntry, ":")

                Imports[ImportEntry[1]] := ImportEntry[2] + 0
            }

            Exports := Map()

            for k, ExportEntry in StrSplit(ExportsString, ",") {
                ExportEntry := StrSplit(ExportEntry, ":")

                Exports[ExportEntry[1]] := ExportEntry[2] + 0
            }

            Relocations := Map()

            for k, Relocation in StrSplit(RelocationsString, ",")
                Relocations[k] := Relocation + 0

            if !(pBinary := DllCall("GlobalAlloc", "UInt", 0, "Ptr", CodeSize, "Ptr"))
                throw Error("Failed to reserve MCL memory")

            Data := ""
            DecodedSize := MCL.Base64.Decode(CodeBase64, &Data)
            MCL.LZ.Decompress(Data.Ptr, DecodedSize, pBinary, CodeSize)

            return MCL.Load(pBinary, CodeSize, Imports, Exports, Relocations, Bitness)
        }

        throw Error("Program does not have a " (A_PtrSize * 8) " bit version")
    }
}
