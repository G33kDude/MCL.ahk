#Include %A_LineFile%\..\Linker.ahk

class MCL {
	class LZ {
		Compress(pData, cbData, pCData, cbCData) {
			if (r := DllCall("ntdll\RtlGetCompressionWorkSpaceSize"
				, "UShort", 0x102                        ; USHORT CompressionFormatAndEngine
				, "UInt*", compressBufferWorkSpaceSize   ; PULONG CompressBufferWorkSpaceSize
				, "UInt*", compressFragmentWorkSpaceSize ; PULONG CompressFragmentWorkSpaceSize
				, "UInt")) ; NTSTATUS
				throw Exception("Erorr calling RtlGetCompressionWorkSpaceSize",, Format("0x{:08x}", r))
			
			VarSetCapacity(compressBufferWorkSpace, compressBufferWorkSpaceSize)
			
			if (r := DllCall("ntdll\RtlCompressBuffer"
				, "UShort", 0x102                       ; USHORT CompressionFormatAndEngine,
				, "Ptr", pData                          ; PUCHAR UncompressedBuffer,
				, "UInt", cbData                        ; ULONG  UncompressedBufferSize,
				, "Ptr", pCData                         ; PUCHAR CompressedBuffer,
				, "UInt", cbCData                       ; ULONG  CompressedBufferSize,
				, "UInt", compressFragmentWorkSpaceSize ; ULONG  UncompressedChunkSize,
				, "UInt*", finalCompressedSize          ; PULONG FinalCompressedSize,
				, "Ptr", &compressBufferWorkSpace       ; PVOID  WorkSpace
				, "UInt")) ; NTSTATUS
				throw Exception("Error calling RtlCompressBuffer",, Format("0x{:08x}", r))
			
			return finalCompressedSize
		}

		Decompress(pCData, cbCData, pData, cbData) {
			if (r := DllCall("ntdll\RtlDecompressBuffer"
				, "UShort",  0x102 ; USHORT CompressionFormat
				, "Ptr", pData     ; PUCHAR UncompressedBuffer
				, "UInt", cbData   ; ULONG  UncompressedBufferSize
				, "Ptr", pCData    ; PUCHAR CompressedBuffer
				, "UInt", cbCData  ; ULONG  CompressedBufferSize,
				, "UInt*", cbFinal ; PULONG FinalUncompressedSize
				, "UInt")) ; NTSTATUS
				throw Exception("Error calling RtlDecompressBuffer",, Format("0x{:08x}", r))
			
			return cbFinal
		}
	}

	class Base64 {
		Encode(pData, Size) {
			if !DllCall("Crypt32\CryptBinaryToString"
				, "Ptr", pData       ; const BYTE *pbBinary
				, "UInt", Size     ; DWORD      cbBinary
				, "UInt", 0x40000001 ; DWORD      dwFlags = CRYPT_STRING_BASE64 | CRYPT_STRING_NOCRLF
				, "Ptr", 0           ; LPWSTR     pszString
				, "UInt*", Base64Length    ; DWORD      *pcchString
				, "UInt") ; BOOL
				throw Exception("Failed to calculate b64 size")
			
			VarSetCapacity(Base64, Base64Length * (1 + A_IsUnicode), 0)
			
			if !DllCall("Crypt32\CryptBinaryToString"
				, "Ptr", pData       ; const BYTE *pbBinary
				, "UInt", Size     ; DWORD      cbBinary
				, "UInt", 0x40000001 ; DWORD      dwFlags = CRYPT_STRING_BASE64 | CRYPT_STRING_NOCRLF
				, "Str", Base64         ; LPWSTR     pszString
				, "UInt*", Base64Length    ; DWORD      *pcchString
				, "UInt") ; BOOL
				throw Exception("Failed to convert to b64")

			return Base64
		}

		Decode(Base64, ByRef DecodeBuffer) {
			if !DllCall("Crypt32\CryptStringToBinary", "Str", Base64, "UInt", 0, "UInt", 1
				, "UPtr", 0, "UInt*", DecodedSize, "Ptr", 0, "Ptr", 0, "UInt")
				throw Exception("Failed to parse b64 to binary")
			
			DecodedSize := VarSetCapacity(DecodeBuffer, DecodedSize, 0)
			pDecodeBuffer := &DecodeBuffer

			if !DllCall("Crypt32\CryptStringToBinary", "Str", Base64, "UInt", 0, "UInt", 1
				, "Ptr", pDecodeBuffer, "UInt*", DecodedSize, "Ptr", 0, "Ptr", 0, "UInt")
				throw Exception("Failed to convert b64 to binary")

			return DecodedSize
		}
	}

	GetTempPath(baseDir := "", prefix := "", ext := ".tmp") {
		if (baseDir == "" || !InStr(FileExist(baseDir), "D"))
			baseDir := A_Temp
		Loop
		{
			DllCall("QueryPerformanceCounter", "Int64*", counter)
			fileName := baseDir "\" prefix . counter . ext
		}
		until !FileExist(fileName)
		return prefix counter ext
	}

	static CompilerPrefix := ""
	static CompilerSuffix := ".exe"

	Compile(Compiler, Code, ExtraOptions := "", Bitness := 0) {
		IncludeFolder := this.GetTempPath(A_WorkingDir, "mcl-include-", "")
		InputFile     := this.GetTempPath(A_WorkingDir, "mcl-input-", ".c")
		OutputFile    := this.GetTempPath(A_WorkingDir, "mcl-output-", ".o")

		if (Bitness = 0) {
			Bitness := A_PtrSize * 8
		}

		try {
			FileOpen(InputFile, "w").Write(code)
			
			while (!FileExist(InputFile)) {
				Sleep, 100
			}

			FileCopyDir, %A_LineFile%/../include, % IncludeFolder
			
			shell := ComObjCreate("WScript.Shell")
			exec := shell.Exec(this.CompilerPrefix Compiler this.CompilerSuffix " -m" Bitness " " InputFile " -o " OutputFile " -I " IncludeFolder " -D MCL_BITNESS=" Bitness ExtraOptions " -ffreestanding -nostdlib -Wno-attribute-alias -fno-leading-underscore --function-sections --data-sections -c")
			exec.StdIn.Close()
			
			if !exec.StdErr.AtEndOfStream
				Throw Exception(StrReplace(exec.StdErr.ReadAll(), " ", " "),, "Compiler Error")
			
			while (!FileExist(OutputFile)) {
				Sleep, 100
			}

			if !(F := FileOpen(OutputFile, "r"))
				Throw Exception("Failed to load output file")
			
			Size := F.Length
			
			if !(pPE := DllCall("GlobalAlloc", "UInt", 0, "Ptr", Size, "Ptr"))
				Throw Exception("Failed to reserve MCL PE memory")
			
			F.RawRead(pPE + 0, Size)
			F.Close()
		}
		finally {
			FileDelete, % InputFile
			FileDelete, % OutputFile

			FileRemoveDir, % IncludeFolder, 1
		}
		
		Linker := new PEObjectLinker(pPE, Size)
		Linker.Read()

		TextSection := Linker.SectionsByName[".text"]

		for SymbolName, Symbol in Linker.SymbolsByName {
			if (SymbolName = "__main" || RegExMatch(SymbolName, "O)^__MCL_e_(\w+)")) {
				Linker.MergeSections(TextSection, Symbol.Section)
			}
		}

		Linker.MakeSectionStandalone(TextSection)
		Linker.DoStaticRelocations(TextSection)

		Imports := {}
		Exports := {}

		for SymbolName, Symbol in TextSection.SymbolsByName {
			if (SymbolName = "__main" || RegExMatch(SymbolName, "O)^__MCL_(e|i)_([\w\$]+)", Match)) {
				if (IsObject(Match)) {
					SymbolName := Match[2]
				}
				
				if (Match[1] = "i") {
					Imports[SymbolName] := Symbol.Value
				}
				else {
					Exports[SymbolName] := Symbol.Value
				}
			}
		}

		static IMAGE_REL_I386_DIR32   := 0x6
		static IMAGE_REL_AMD64_ADDR64 := 0x1

		Relocations := []
		RelocationDataType := Linker.Is32Bit ? "Int" : "Int64"

		for k, Relocation in TextSection.Relocations {
			if (Relocation.Symbol.Section.Name != ".text") {
				continue
			}

			if (Relocation.Type = IMAGE_REL_I386_DIR32 || Relocation.Type = IMAGE_REL_AMD64_ADDR64) {
				Offset := TextSection.Data.Read(Relocation.Address, RelocationDataType)
				Address := Relocation.Symbol.Value + Offset
				TextSection.Data.Write(Address, Relocation.Address, RelocationDataType)

				Relocations.Push(Relocation.Address)
			}
		}

		CodeSize := TextSection.Data.Length()

		if !(pCode := DllCall("GlobalAlloc", "UInt", 0, "Ptr", CodeSize, "Ptr"))
			Throw Exception("Failed to reserve MCL PE memory")
		
		TextSection.Data.Coalesce(pCode)

		;F := FileOpen("out.bin", "w")
		;F.RawWrite(pCode + 0, CodeSize)
		;F.Close()

		return [pCode, CodeSize, Imports, Exports, Relocations, Bitness]
	}
	
	Load(pCode, CodeSize, Imports, Exports, Relocations, Bitness) {
		for ImportName, ImportOffset in Imports {
			Import := StrSplit(ImportName, "$")
			DllName := Import[1]
			FunctionName := Import[2]

			DllCall("SetLastError", "Int", 0) ; Clear A_LastError. Something has left it set here before, and GetModuleHandle doesn't clear it.
			hDll := DllCall("GetModuleHandle", "Str", DllName, "Ptr")

			if (ErrorLevel || A_LastError) {
				Throw Exception("Could not load dll " DllName ", ErrorLevel " ErrorLevel ", LastError " Format("{:0x}", A_LastError))
			}

			DllCall("SetLastError", "Int", 0)
			pFunction := DllCall("GetProcAddress", "Ptr", hDll, "AStr", FunctionName, "Ptr")

			if (ErrorLevel || A_LastError) {
				Throw Exception("Could not find function " FunctionName " from " DllName ".dll, ErrorLevel " ErrorLevel ", LastError " Format("{:0x}", A_LastError))
			}

			NumPut(pFunction, pCode + 0, ImportOffset, "Ptr")
		}

		for ExportName, ExportOffset in Exports {
			Exports[ExportName] += pCode
		}

		for k, Offset in Relocations {
			Old := NumGet(pCode + 0, Offset, "Ptr")
			NumPut(Old + pCode, pCode + 0, Offset, "Ptr")
		}
		
		if !DllCall("VirtualProtect", "Ptr", pCode, "Ptr", CodeSize, "UInt", 0x40, "UInt*", OldProtect, "UInt")
			Throw Exception("Failed to mark MCL memory as executable")

		if (Exports.Count() = 1 && Exports.HasKey("__main")) {
			return Exports["__main"]
		}
		else {
			return Exports
		}
	}

	Pack(FormatAsStringLiteral, pCode, CodeSize, Imports, Exports, Relocations, Bitness) {
		CompressionBufferSize := VarSetCapacity(CompressionBuffer, CodeSize * 2, 0)
		pCompressionBuffer := &CompressionBuffer

		CompressedSize := this.LZ.Compress(pCode, CodeSize, pCompressionBuffer, CompressionBufferSize)
		Base64 := this.Base64.Encode(pCompressionBuffer, CompressedSize)

		SymbolsString := ""

		for ImportName, ImportOffset in Imports {
			SymbolsString .= ImportName ":" ImportOffset ","
		}

		SymbolsString := SubStr(SymbolsString, 1, -1) ";"

		for ExportName, ExportOffset in Exports {
			SymbolsString .= ExportName ":" ExportOffset ","
		}

		SymbolsString := SubStr(SymbolsString, 1, -1)

		RelocationsString := ""

		for k, RelocationOffset in Relocations {
			RelocationsString .= RelocationOffset ","
		}

		RelocationsString := SubStr(RelocationsString, 1, -1)

		; And finally, format our output
		
		if (FormatAsStringLiteral) {
			; Format the output as an AHK string literal, which can be dropped directly into a source file

			while StrLen(Base64) {
				Out .= "`n. """ SubStr(Base64, 1, 120-8) """"
				Base64 := SubStr(Base64, (120-8)+1)
			}

			return Bitness ";" SymbolsString ";" RelocationsString ";" CodeSize ";""" Out
		}
		else {
			; Don't format the output, return the string that would result from AHK parsing the string literal returned when `FormatAsStringLiteral` is true

			return Bitness ";" SymbolsString ";" RelocationsString ";" CodeSize ";" Base64
		}
	}

	DoTemplateBlock(Template, BlockName, Values, AsArray := false) {
		; If Values is not empty, this function populates $BlockName with Values stringified, and
		;  preserves anything inside $HasBlockName...$HasBlockName, removing anything between
		;   $HasNoBlockName...$HasNoBlockName

		; If values is empty, the opposite is done, removing $HasBlockName...$HasBlockName and
		;  keeping $HasNoBlockName...$HasNoBlockName

		if (Values.Count()) {
			Template := RegExReplace(Template, "s)\$HasNo" BlockName ".*?\$HasNo" BlockName)
			Template := StrReplace(Template, "$Has" BlockName)

			ValuesString := AsArray ? "[" : "{"

			for k, v in Values {
				ValuesString .= (AsArray ? v : """" k """: " v) ", "
			}

			ValuesString := SubStr(ValuesString, 1, -2) (AsArray ? "]" : "}")

			Template := StrReplace(Template, "$" BlockName, ValuesString)
		}
		else {
			Template := StrReplace(Template, "$HasNo" BlockName)
			Template := RegExReplace(Template, "s)\$Has" BlockName ".*?\$Has" BlockName)
		}

		return Template
	}

	StandalonePack(Name, pCode, CodeSize, Imports, Exports, Relocations, Bitness) {
		; First, compress the actual code bytes
		CompressionBufferSize := VarSetCapacity(CompressionBuffer, CodeSize * 2, 0)
		pCompressionBuffer := &CompressionBuffer

		CompressedSize := this.LZ.Compress(pCode, CodeSize, pCompressionBuffer, CompressionBufferSize)
		Base64 := this.Base64.Encode(pCompressionBuffer, CompressedSize)

		while StrLen(Base64) {
			Out .= "`n. """ SubStr(Base64, 1, 120-8) """"
			Base64 := SubStr(Base64, (120-8)+1)
		}
		
		if (Exports.Count() = 1 && Exports.HasKey("__main")) {
			MainOffset := Exports["__main"]

			Exports := {}
		}

		FileRead, Template, %A_LineFile%/../StandaloneTemplate.ahk

		Template := StrReplace(Template, "$Name", Name)
		Template := StrReplace(Template, "$CodeBase64", """""" Out)
		Template := StrReplace(Template, "$CodeSize", CodeSize)
		Template := StrReplace(Template, "$Bitness", Bitness)

		Template := this.DoTemplateBlock(Template, "Imports", Imports)
		Template := this.DoTemplateBlock(Template, "Relocations", Relocations, true)
		Template := this.DoTemplateBlock(Template, "Exports", Exports)

		Template := StrReplace(Template, "$MainOffset", MainOffset)

		Template := RegexReplace(Template, "\n\s*\n")

		return Template
	}

	class Options {
		static OutputAHKBit  := 0
		static Output32Bit   := 1
		static Output64Bit   := 2
		static OutputBothBit := 3
		static OutputMask    := 3

		static FormatAsStringLiteral := 0
		static DoNotFormat           := 4
	}

	AHKFromLanguage(Compiler, Code, Options, CompilerOptions := "") {
		Literal := Options & this.Options.FormatAsStringLiteral

		Result := (Literal ? """" : "") "V0.3|"

		if (Options & this.Options.OutputMask = this.Options.OutputAHKBit) {
			Result .= this.Pack(Literal, this.Compile(Compiler, Code, CompilerOptions)*)
		}
		else {
			if (Options & this.Options.Output32Bit) {
				Result .= this.Pack(Literal, this.Compile(Compiler, Code, CompilerOptions, 32)*)
			}

			if (Options & this.Options.Output64Bit) {
				if (Options & this.Options.Output32Bit) {
					Result .= (Literal ? "`n.""" : "") "|"
				}

				Result .= this.Pack(Literal, this.Compile(Compiler, Code, CompilerOptions, 64)*)
			}
		}

		return Result
	}

	StandaloneAHKFromLanguage(Compiler, Code, Options, CompilerOptions := "", Name := "") {
		if (Options & this.Options.OutputMask = this.Options.OutputAHKBit) {
			return this.StandalonePack(Name, this.Compile(Compiler, Code, CompilerOptions)*)
		}
		else {
			Result := ""

			if (Options & this.Options.Output32Bit) {
				Result .= this.StandalonePack(Name "32Bit", this.Compile(Compiler, Code, CompilerOptions, 32)*)
			}

			if (Options & this.Options.Output64Bit) {
				Result .= this.StandalonePack(Name "64Bit", this.Compile(Compiler, Code, CompilerOptions, 64)*)

				if (Options & this.Options.Output32Bit) {
					Result .= "`n" Name "() {`n`treturn A_PtrSize = 4 ? " Name "32Bit() : " Name "64Bit()`n}`n"
				}
			}

			return Result
		}
	}

	FromC(Code, Options := 0) {
		Bitness := A_PtrSize * 8

		if (Options & this.Options.Output32Bit) {
			Bitness := 32
		}
		else if (Options & this.Options.Output32Bit) {
			Bitness := 64
		}

		return this.Load(this.Compile("gcc", Code,, Bitness)*)
	}
	AHKFromC(Code, Options := 0) {
		return this.AHKFromLanguage("gcc", Code, Options)
	}
	StandaloneAHKFromC(Code, Options := 0, Name := "MyC") {
		return this.StandaloneAHKFromLanguage("gcc", Code, Options,, Name)
	}
	
	FromCPP(Code, Options := 0) {
		Bitness := A_PtrSize * 8

		if (Options & this.Options.Output32Bit) {
			Bitness := 32
		}
		else if (Options & this.Options.Output32Bit) {
			Bitness := 64
		}

		return this.Load(this.Compile("g++", "extern ""C"" {`n" Code "`n}", " -fno-exceptions -fno-rtti", Bitness)*)
	}
	AHKFromCPP(Code, Options := 0) {
		return this.AHKFromLanguage("g++", "extern ""C"" {`n" Code "`n}", Options, " -fno-exceptions -fno-rtti")
	}
	StandaloneAHKFromCPP(Code, Options := 0, Name := "MyCPP") {
		return this.StandaloneAHKFromLanguage("g++", "extern ""C"" {`n" Code "`n}", Options, " -fno-exceptions -fno-rtti", Name)
	}

	FromString(Code) {
		Parts := StrSplit(Code, "|")
		Version := Parts.RemoveAt(1)

		if (Version != "V0.3") {
			Throw Exception("Unknown/corrupt MCL packed code format")
		}

		for k, Flavor in Parts {
			Flavor := StrSplit(Flavor, ";")

			if (Flavor.Count() != 6) {
				Throw Exception("Unknown/corrupt MCL packed code format")
			}

			Bitness           := Flavor[1] + 0
			ImportsString     := Flavor[2]
			ExportsString     := Flavor[3]
			RelocationsString := Flavor[4]
			CodeSize          := Flavor[5] + 0
			CodeBase64        := Flavor[6]

			if (Bitness != (A_PtrSize * 8)) {
				continue
			}

			Imports := {}

			for k, ImportEntry in StrSplit(ImportsString, ",") {
				ImportEntry := StrSplit(ImportEntry, ":")

				Imports[ImportEntry[1]] := ImportEntry[2] + 0
			}

			Exports := {}

			for k, ExportEntry in StrSplit(ExportsString, ",") {
				ExportEntry := StrSplit(ExportEntry, ":")

				Exports[ExportEntry[1]] := ExportEntry[2] + 0
			}

			Relocations := []

			for k, Relocation in StrSplit(RelocationsString, ",") {
				Relocations.Push(Relocation + 0)
			}
			
			if !(pBinary := DllCall("GlobalAlloc", "UInt", 0, "Ptr", CodeSize, "Ptr"))
				throw Exception("Failed to reserve MCL memory")

			DecodedSize := this.Base64.Decode(CodeBase64, Data)
			this.LZ.Decompress(&Data, DecodedSize, pBinary, CodeSize)
			
			return this.Load(pBinary, CodeSize, Imports, Exports, Relocations, Bitness)
		}

		Throw Exception("Program does not have a " (A_PtrSize * 8) " bit version")
	}
}
