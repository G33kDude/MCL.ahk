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
		static Compress(pData, cbData, pCData, cbCData) {
			if (r := DllCall("ntdll\RtlGetCompressionWorkSpaceSize"
				, "UShort", 0x102	; USHORT CompressionFormatAndEngine
				, "UInt*", &compressBufferWorkSpaceSize := 0	; PULONG CompressBufferWorkSpaceSize
				, "UInt*", &compressFragmentWorkSpaceSize := 0	; PULONG CompressFragmentWorkSpaceSize
				, "UInt"))	; NTSTATUS
				throw Error("Erorr calling RtlGetCompressionWorkSpaceSize", , Format("0x{:08x}", r))

			compressBufferWorkSpace := Buffer(compressBufferWorkSpaceSize)

			if (r := DllCall("ntdll\RtlCompressBuffer"
				, "UShort", 0x102	; USHORT CompressionFormatAndEngine,
				, "Ptr", pData	; PUCHAR UncompressedBuffer,
				, "UInt", cbData	; ULONG  UncompressedBufferSize,
				, "Ptr", pCData	; PUCHAR CompressedBuffer,
				, "UInt", cbCData	; ULONG  CompressedBufferSize,
				, "UInt", compressFragmentWorkSpaceSize	; ULONG  UncompressedChunkSize,
				, "UInt*", &finalCompressedSize := 0	; PULONG FinalCompressedSize,
				, "Ptr", compressBufferWorkSpace	; PVOID  WorkSpace
				, "UInt"))	; NTSTATUS
				throw Error("Error calling RtlCompressBuffer", , Format("0x{:08x}", r))

			return finalCompressedSize
		}

		static Decompress(pCData, cbCData, pData, cbData) {
			cbFinal := 0

			if (r := DllCall("ntdll\RtlDecompressBuffer"
				, "UShort", 0x102	; USHORT CompressionFormat
				, "Ptr", pData	; PUCHAR UncompressedBuffer
				, "UInt", cbData	; ULONG  UncompressedBufferSize
				, "Ptr", pCData	; PUCHAR CompressedBuffer
				, "UInt", cbCData	; ULONG  CompressedBufferSize,
				, "UInt*", cbFinal	; PULONG FinalUncompressedSize
				, "UInt"))	; NTSTATUS
				throw Error("Error calling RtlDecompressBuffer", , Format("0x{:08x}", r))

			return cbFinal
		}
	}

	class Base64 {
		static Encode(pData, size) {
			b64size := 0

			if !DllCall("Crypt32\CryptBinaryToString"
				, "Ptr", pData	; const BYTE *pbBinary
				, "UInt", size	; DWORD      cbBinary
				, "UInt", 0x40000001	; DWORD      dwFlags = CRYPT_STRING_BASE64 | CRYPT_STRING_NOCRLF
				, "Ptr", 0	; LPWSTR     pszString
				, "UInt*", &b64size	; DWORD      *pcchString
				, "UInt")	; BOOL
				throw Error("Failed to calculate b64 size")

				base64 := ""
				VarSetStrCapacity(&base64, b64size*2)

			if !DllCall("Crypt32\CryptBinaryToString"
				, "Ptr", pData	; const BYTE *pbBinary
				, "UInt", size	; DWORD      cbBinary
				, "UInt", 0x40000001	; DWORD      dwFlags = CRYPT_STRING_BASE64 | CRYPT_STRING_NOCRLF
				, "Str", base64	; LPWSTR     pszString
				, "UInt*", b64size	; DWORD      *pcchString
				, "UInt")	; BOOL
				throw Error("Failed to convert to b64")

			return base64
		}

		static Decode(Base64, &DecodeBuffer) {
			DecodedSize := 0
			if !DllCall("Crypt32\CryptStringToBinary", "Str", Base64, "UInt", 0, "UInt", 1
				, "UPtr", 0, "UInt*", &DecodedSize, "Ptr", 0, "Ptr", 0, "UInt")
				throw Error("Failed to parse b64 to binary")

			DecodeBuffer := Buffer(DecodedSize, 0)

			if !DllCall("Crypt32\CryptStringToBinary", "Str", Base64, "UInt", 0, "UInt", 1
				, "Ptr", DecodeBuffer, "UInt*", DecodedSize, "Ptr", 0, "Ptr", 0, "UInt")
				throw Error("Failed to convert b64 to binary")

			return DecodedSize
		}
	}

	static GetTempPath(baseDir := "", prefix := "", ext := ".tmp") {
		if (baseDir == "" || !InStr(FileExist(baseDir), "D"))
			baseDir := A_Temp
		Loop {
			DllCall("QueryPerformanceCounter", "Int64*", &counter := 0)
			fileName := baseDir "\" prefix . counter . ext
		} until !FileExist(fileName)
		return prefix counter ext
	}

	static CompilerPrefix := ""
	static CompilerSuffix := ".exe"

	static CompilersExist(Prefix, Suffix) {
		; True results are wrapped in an array since `Prefix` could be an empty
		;  string but still be a correct compiler path. So we can't just
		;   `return Prefix` since `Prefix` itself can be false.

		if (FileExist(Prefix "gcc" Suffix) && FileExist(Prefix "g++" Suffix)) {
			return [Prefix]
		}

		SystemPath := EnvGet("PATH")

		for k, Folder in StrSplit(SystemPath, ";") {
			NewPrefix := Folder "\" Prefix

			if (FileExist(NewPrefix "gcc" Suffix) && FileExist(NewPrefix "g++" Suffix)) {
				return [NewPrefix]
			}
		}

		return false
	}

	static TryFindCompilers() {
		; Attempt to automatically set MCL.CompilerPrefix based on PATH and known cross compiler naming schemes

		if (MCL.CompilersExist(MCL.CompilerPrefix, MCL.CompilerSuffix)) {
			; The default (or user provided values) are correct, no changes needed
			return
		}

		if (Prefix := MCL.CompilersExist(MCL.CompilerPrefix "x86_64-w64-mingw32-", MCL.CompilerSuffix)) {
			; Compiler executables might be named like they would be on Linux

			MCL.CompilerPrefix := Prefix[1]
			Return
		}

		; Oops, I don't know any other naming schemes. Ah well, this is good enough to autodected mingw-w64 installed through cygwin.

		Throw Error("MCL couldn't find gcc and g++, please manually specify the path both can be found at via 'MCL.CompilerPrefix'")
	}

	static Compile(Compiler, Code, ExtraOptions := "", Bitness := unset) {
		MCL.TryFindCompilers()

		IncludeFolder := MCL.GetTempPath(A_WorkingDir, "mcl-include-", "")
		InputFile := MCL.GetTempPath(A_WorkingDir, "mcl-input-", ".c")
		OutputFile := MCL.GetTempPath(A_WorkingDir, "mcl-output-", ".o")

		if !IsSet(Bitness)
			Bitness := A_PtrSize * 8

		try {
			FileOpen(InputFile, "w").Write(code)

			while (!FileExist(InputFile)) {
				Sleep 100
			}

			DirCopy A_LineFile "/../include", IncludeFolder

			shell := ComObject("WScript.Shell")
			exec := shell.Exec(
				MCL.CompilerPrefix Compiler MCL.CompilerSuffix " "
				"-m" Bitness " "
				InputFile " "
				"-o " OutputFile " "
				"-I " IncludeFolder " "
				"-D MCL_BITNESS=" Bitness ExtraOptions " "
				"-ffreestanding "
				"-nostdlib "
				"-Wno-attribute-alias "
				"-fno-leading-underscore "
				"--function-sections "
				"--data-sections "
				"-c"
			)
			exec.StdIn.Close()

			if !exec.StdErr.AtEndOfStream
				throw Error(StrReplace(exec.StdErr.ReadAll(), " ", " "), , "Compiler Error")

			while !FileExist(OutputFile)
				Sleep 100

			if !(F := FileOpen(OutputFile, "r"))
				throw Error("Failed to load output file")

			Size := F.Length

			if !(pPE := DllCall("GlobalAlloc", "UInt", 0, "Ptr", Size, "Ptr"))
				throw Error("Failed to reserve MCL PE memory")

			F.RawRead(pPE + 0, Size)
			F.Close()
		} finally {
			FileDelete InputFile
			DirDelete IncludeFolder, 1

			try {
				; Depending on what when wrong, the output file might not exist yet.
				; And if we're already in a `try {}` block, then this line will throw when trying to delete the file.

				FileDelete OutputFile
			}
		}

		; TODO
		Linker := PEObjectLinker(pPE, Size)
		Linker.Read()

		TextSection := Linker.SectionsByName[".text"]

		NonExportedFunctions := []
		SingleFunction := false
		ExportCount := 0

		static COFF_SYMBOL_TYPE_FUNCTION := 0x20
		static COFF_SYMBOL_STORAGE_CLASS_STATIC := 0x3

		for SymbolName, Symbol in Linker.SymbolsByName {
			if (SymbolName = "__main" || RegExMatch(SymbolName, "^__MCL_e_(\w+)")) {
				Linker.MergeSections(TextSection, Symbol.Section)
				ExportCount++
			} else if (Symbol.Type = COFF_SYMBOL_TYPE_FUNCTION && Symbol.StorageClass != COFF_SYMBOL_STORAGE_CLASS_STATIC) {
				NonExportedFunctions.Push(Symbol)
			}
		}

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

			if RegExMatch(SymbolName, "^__MCL_(e|i)_([\w\$]+)", &Match) {
				if (Match[1] = "i")
					Imports[Match[2]] := Symbol.Value
				else
					Exports[Match[2]] := Symbol.Value
			}
		}

		if (Exports.Count = 0)
			throw Error("Code does not define '__main', or export any functions")

		static IMAGE_REL_I386_DIR32 := 0x6
		static IMAGE_REL_AMD64_ADDR64 := 0x1

		Relocations := []
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

			Relocations.Push(Relocation.Address)
		}

		CodeSize := TextSection.Data.Length()

		if !(pCode := DllCall("GlobalAlloc", "UInt", 0, "Ptr", CodeSize, "Ptr"))
			throw Error("Failed to reserve MCL PE memory")

		TextSection.Data.Coalesce(pCode)

		return [pCode, CodeSize, Imports, Exports, Relocations, Bitness]
	}

	static Load(pCode, CodeSize, Imports, Exports, Relocations, Bitness) {
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

			NumPut("Ptr", pFunction, pCode, ImportOffset)
		}

		for ExportName, ExportOffset in Exports
			Exports[ExportName] += pCode

		for k, Offset in Relocations {
			Old := NumGet(pCode, Offset, "Ptr")
			NumPut("Ptr", Old + pCode, pCode, Offset)
		}

		if !DllCall("VirtualProtect", "Ptr", pCode, "Ptr", CodeSize, "UInt", 0x40, "UInt*", &OldProtect := 0, "UInt")
			throw Error("Failed to mark MCL memory as executable")

		if (Exports.Count = 1) {
			; yeah, it is either this or `._NewEnum()` garbage.
			for k, v in Exports
				return v
		}

		return Exports
	}

	static Pack(FormatAsStringLiteral, pCode, CodeSize, Imports, Exports, Relocations, Bitness) {
		CompressionBufferSize := CodeSize * 2
		CompressionBuffer := Buffer(CompressionBufferSize, 0)
		pCompressionBuffer := CompressionBuffer.Ptr

		CompressedSize := MCL.LZ.Compress(pCode, CodeSize, pCompressionBuffer, CompressionBufferSize)
		Base64 := MCL.Base64.Encode(pCompressionBuffer, CompressedSize)

		SymbolsString := ""

		for ImportName, ImportOffset in Imports
			SymbolsString .= ImportName ":" ImportOffset ","

		SymbolsString := SubStr(SymbolsString, 1, -1) ";"

		for ExportName, ExportOffset in Exports
			SymbolsString .= ExportName ":" ExportOffset ","

		SymbolsString := SubStr(SymbolsString, 1, -1)

		RelocationsString := ""

		for k, RelocationOffset in Relocations
			RelocationsString .= RelocationOffset ","

		RelocationsString := SubStr(RelocationsString, 1, -1)

		; And finally, format our output

		if (FormatAsStringLiteral) {
			; Format the output as an AHK string literal, which can be dropped directly into a source file
			Out := ""

			while StrLen(Base64) {
				Out .= '`n. "' SubStr(Base64, 1, 120 - 8) '"'
				Base64 := SubStr(Base64, (120 - 8) + 1)
			}

			return Bitness ";" SymbolsString ";" RelocationsString ";" CodeSize ";" "" Out
		} else {
			; Don't format the output, return the string that would result from AHK parsing the string literal returned when `FormatAsStringLiteral` is true

			return Bitness ";" SymbolsString ";" RelocationsString ";" CodeSize ";" Base64
		}
	}

	static DoTemplateBlock(Template, BlockName, Values, AsArray := false) {
		; If Values is not empty, this function populates $BlockName with Values stringified, and
		;  preserves anything inside $HasBlockName...$HasBlockName, removing anything between
		;   $HasNoBlockName...$HasNoBlockName

		; If values is empty, the opposite is done, removing $HasBlockName...$HasBlockName and
		;  keeping $HasNoBlockName...$HasNoBlockName

		if (!Values.Count) {
			Template := StrReplace(Template, "$HasNo" BlockName)
			Template := RegExReplace(Template, "s)\$Has" BlockName ".*?\$Has" BlockName)
			return Template
		}

		Template := RegExReplace(Template, "s)\$HasNo" BlockName ".*?\$HasNo" BlockName)
		Template := StrReplace(Template, "$Has" BlockName)

		ValuesString := AsArray ? "[" : "{"

		for k, v in Values
			ValuesString .= (AsArray ? v : '"' k '": ' v) ", "

		ValuesString := SubStr(ValuesString, 1, -2) (AsArray ? "]" : "}")

		Template := StrReplace(Template, "$" BlockName, ValuesString)

		return Template
	}

	static StandalonePack(Name, pCode, CodeSize, Imports, Exports, Relocations, Bitness) {
		; First, compress the actual code bytes
		CompressionBuffer := Buffer(CodeSize * 2, 0)
		CompressionBufferSize := CompressionBuffer.Size
		pCompressionBuffer := CompressionBuffer.Ptr

		CompressedSize := MCL.LZ.Compress(pCode, CodeSize, pCompressionBuffer, CompressionBufferSize)
		Base64 := MCL.Base64.Encode(pCompressionBuffer, CompressedSize)

		CodeBase64 := '""'
		while StrLen(Base64) {
			CodeBase64 .= '`n. "' SubStr(Base64, 1, 120 - 8) '"'
			Base64 := SubStr(Base64, (120 - 8) + 1)
		}

		if (Exports.Count() = 1) {
			; As with `.Load`, either this or `._NewEnum()` fiddling.
			for k, v in Exports
				MainOffset := v

			Exports := Map()
		}

		Template := FileRead(A_LineFile "/../StandaloneTemplate.ahk")

		Template := StrReplace(Template, "$Name", Name)
		Template := StrReplace(Template, "$CodeBase64", CodeBase64)
		Template := StrReplace(Template, "$CodeSize", CodeSize)
		Template := StrReplace(Template, "$Bitness", Bitness)
		Template := StrReplace(Template, "$CompressedSize", CompressedSize)

		Template := MCL.DoTemplateBlock(Template, "Imports", Imports)
		Template := MCL.DoTemplateBlock(Template, "Relocations", Relocations, true)
		Template := MCL.DoTemplateBlock(Template, "Exports", Exports)

		Template := StrReplace(Template, "$MainOffset", MainOffset)

		Template := RegexReplace(Template, "\n\s*\n")

		return Template
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
			Result .= MCL.Pack(Literal, MCL.Compile(Compiler, Code, CompilerOptions) *)
			return Result
		}

		if (Options & MCL.Options.Output32Bit)
			Result .= MCL.Pack(Literal, MCL.Compile(Compiler, Code, CompilerOptions, 32) *)

		if (Options & MCL.Options.Output64Bit) {
			if (Options & MCL.Options.Output32Bit)
				Result .= (Literal ? '`n."' : '') "|"

			Result .= MCL.Pack(Literal, MCL.Compile(Compiler, Code, CompilerOptions, 64) *)
		}

		return Result
	}

	static StandaloneAHKFromLanguage(Compiler, Code, Options, CompilerOptions := "", Name := "") {
		if (Options & MCL.Options.OutputMask = MCL.Options.OutputAHKBit)
			return MCL.StandalonePack(Name, MCL.Compile(Compiler, Code, CompilerOptions) *)

		Result := ""

		if (Options & MCL.Options.Output32Bit) {
			Result .= MCL.StandalonePack(Name "32Bit", MCL.Compile(Compiler, Code, CompilerOptions, 32) *) "`n"
		}

		if (Options & MCL.Options.Output64Bit) {
			Result .= MCL.StandalonePack(Name "64Bit", MCL.Compile(Compiler, Code, CompilerOptions, 64) *) "`n"

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

		return MCL.Load(MCL.Compile("gcc", Code, , Bitness) *)
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

		return MCL.Load(MCL.Compile("g++", 'extern "C" {`n' Code "`n}", " -fno-exceptions -fno-rtti", Bitness) *)
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

			Relocations := []

			for k, Relocation in StrSplit(RelocationsString, ",")
				Relocations.Push(Relocation + 0)

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
