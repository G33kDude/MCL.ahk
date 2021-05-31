#Include SymbolReader.ahk

class MClib {
	class LZ {
		Compress(pData, cbData, pCData, cbCData)
		{
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

		Decompress(pCData, cbCData, pData, cbData)
		{
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

	NormalizeSymbols(Symbols) {
		KeepSymbolNames := {"__main": 1, "hProcessHeap": 1, "HeapAlloc": 1, "HeapFree": 1}

		Result := {}

		for SymbolName, Symbol in Symbols {
			if (KeepSymbolNames[SymbolName] || RegExMatch(SymbolName, "O)__mcode_e_(\w+)")) {
				Result[SymbolName] := Symbol.Value
			}
		}

		return Result
	}

	Compile(Compiler, Code, ExtraOptions := "") {
		OldWorkingDir := A_WorkingDir
		SetWorkingDir, %A_LineFile%/../

		FileOpen("linker_script", "w").Write("OUTPUT_FORMAT(pe-x86-64)SECTIONS{.text :{*(.text*)*(.rodata*)*(.rdata*)*(.data*)*(.bss*)}}")
		
		FileOpen("test.c", "w").Write(code)
		
		while (!FileExist("test.c")) {
			Sleep, 100
		}
		
		shell := ComObjCreate("WScript.Shell")
		exec := shell.Exec("x86_64-w64-mingw32-" Compiler ".exe -m64" ExtraOptions " -ffreestanding -nostdlib -Wno-attribute-alias -T linker_script test.c")
		exec.StdIn.Close()
		
		if !exec.StdErr.AtEndOfStream
			Throw Exception(exec.StdErr.ReadAll(),, "Compiler Error")
		
		while (!FileExist("a.exe")) {
			Sleep, 100
		}
		
		FileDelete, test.c
		FileDelete, linker_script
		
		if !(F := FileOpen("a.exe", "r"))
			Throw Exception("Failed to load output file")
		
		Size := F.Length
		
		if !(pPE := DllCall("GlobalAlloc", "UInt", 0, "Ptr", Size, "Ptr"))
			Throw Exception("Failed to reserve MCLib PE memory")
		
		F.RawRead(pPE + 0, Size)
		F.Close()
		
		;FileDelete, a.exe
		
		Reader := new PESymbolReader(pPE, Size)
		Output := Reader.Read()
		
		pCode := pPE + Output.SectionsByName[".text"].FileOffset

		SetWorkingDir, % OldWorkingDir

		return [pCode, Output.SectionsByName[".text"].FileSize, this.NormalizeSymbols(Output.AbsoluteSymbols)]
	}
	
	Load(pCode, CodeSize, Symbols) {
		static hKernel32    := DllCall("GetModuleHandle", "Str", "kernel32", "Ptr")
		static hProcessHeap := DllCall("GetProcessHeap", "Ptr")
		static HeapAlloc    := DllCall("GetProcAddress", "Ptr", hKernel32, "AStr", "HeapAlloc", "Ptr")
		static HeapFree     := DllCall("GetProcAddress", "Ptr", hKernel32, "AStr", "HeapFree", "Ptr")
		
		OffsetOfhProcessHeap := Symbols["hProcessHeap"]
		OffsetOfHeapAlloc    := Symbols["HeapAlloc"]
		OffsetOfHeapFree     := Symbols["HeapFree"]
		OffsetOfMain         := Symbols["__main"]
		
		if (OffsetOfhProcessHeap && OffsetOfHeapAlloc && OffsetOfHeapFree) {
			NumPut(hProcessHeap, pCode + 0, OffsetOfhProcessHeap, "Ptr")
			NumPut(HeapAlloc   , pCode + 0, OffsetOfHeapAlloc   , "Ptr")
			NumPut(HeapFree    , pCode + 0, OffsetOfHeapFree    , "Ptr")
		}
		
		if !DllCall("VirtualProtect", "Ptr", pCode, "Ptr", CodeSize, "UInt", 0x40, "UInt*", OldProtect, "UInt")
			Throw Exception("Failed to mark MCLib memory as executable")
		
		Exports := {}

		for SymbolName, SymbolOffset in Symbols {
			if (RegExMatch(SymbolName, "O)__mcode_e_(\w+)", Match)) {
				Exports[Match[1]] := pCode + SymbolOffset
			}
		}

		if (Exports.Count()) {
			return Exports
		}
		else {
			return pCode + OffsetOfMain
		}
	}
	
	FromC(Code) {
		return this.Load(this.Compile("gcc", Code)*)
	}
	
	FromCPP(Code) {
		return this.Load(this.Compile("g++", "extern ""C"" {`n" Code "`n}", " -fno-exceptions -fno-rtti")*)
	}
	
	Pack(FormatAsStringLiteral, pCode, CodeSize, Symbols) {

		; First, compress the actual code bytes
		CompressionBufferSize := VarSetCapacity(CompressionBuffer, CodeSize * 2, 0)
		pCompressionBuffer := &CompressionBuffer

		CompressedSize := MClib.LZ.Compress(pCode, CodeSize, pCompressionBuffer, CompressionBufferSize)
		
		; Then, convert the compressed code to Base64
		if !DllCall("Crypt32\CryptBinaryToString"
			, "Ptr", pCompressionBuffer       ; const BYTE *pbBinary
			, "UInt", CompressedSize     ; DWORD      cbBinary
			, "UInt", 0x40000001 ; DWORD      dwFlags = CRYPT_STRING_BASE64 | CRYPT_STRING_NOCRLF
			, "Ptr", 0           ; LPWSTR     pszString
			, "UInt*", Base64Length    ; DWORD      *pcchString
			, "UInt") ; BOOL
			throw Exception("Failed to calculate b64 size")
		
		VarSetCapacity(Base64, Base64Length * (1 + A_IsUnicode), 0)
		
		if !DllCall("Crypt32\CryptBinaryToString"
			, "Ptr", pCompressionBuffer       ; const BYTE *pbBinary
			, "UInt", CompressedSize     ; DWORD      cbBinary
			, "UInt", 0x40000001 ; DWORD      dwFlags = CRYPT_STRING_BASE64 | CRYPT_STRING_NOCRLF
			, "Str", Base64         ; LPWSTR     pszString
			, "UInt*", Base64Length    ; DWORD      *pcchString
			, "UInt") ; BOOL
			throw Exception("Failed to convert to b64")

		; And finally, format our output
		
		if (FormatAsStringLiteral) {
			; Format the output as an AHK string literal, which can be dropped directly into a source file

			while StrLen(Base64) {
				Out .= "`n. """ SubStr(Base64, 1, 120-8) """"
				Base64 := SubStr(Base64, (120-8)+1)
			}

			Header := """V0;"

			for SymbolName, SymbolOffset in Symbols {
				Header .= SymbolName "-" SymbolOffset ","
			}

			Header .= "__size-" CodeSize

			return Header ";""" Out
		}
		else {
			; Don't format the output, return the string that would result from AHK parsing the string literal returned when `FormatAsStringLiteral` is true

			Header := "V0;"

			for SymbolName, SymbolOffset in Symbols {
				Header .= SymbolName "-" SymbolOffset ","
			}

			Header .= "__size-" CodeSize

			return Header ";" Base64
		}
	}
	
	AHKFromC(Code, FormatAsStringLiteral := true) {
		return this.Pack(FormatAsStringLiteral, this.Compile("gcc", Code)*)
	}
	
	AHKFromCPP(Code, FormatAsStringLiteral := true) {
		return this.Pack(FormatAsStringLiteral, this.Compile("g++", "extern ""C"" {`n" Code "`n}", " -fno-exceptions -fno-rtti")*)
	}

	FromString(Code) {
		Parts := StrSplit(Code, ";")

		if (Parts[1] != "V0") {
			Throw "Unknown MClib packed code format"
		}

		Symbols := {}

		for k, SymbolEntry in StrSplit(Parts[2], ",") {
			SymbolEntry := StrSplit(SymbolEntry, "-")

			Symbols[SymbolEntry[1]] := SymbolEntry[2]
		}

		CodeBase64 := Parts[3]

		DecompressedSize := Symbols.__Size

		if !DllCall("Crypt32\CryptStringToBinary", "Str", CodeBase64, "UInt", 0, "UInt", 1
			, "UPtr", 0, "UInt*", CompressedSize, "Ptr", 0, "Ptr", 0, "UInt")
			throw Exception("Failed to parse MCLib b64 to binary")
		
		CompressedSize := VarSetCapacity(DecompressionBuffer, CompressedSize, 0)
		pDecompressionBuffer := &DecompressionBuffer

		if !DllCall("Crypt32\CryptStringToBinary", "Str", CodeBase64, "UInt", 0, "UInt", 1
			, "Ptr", pDecompressionBuffer, "UInt*", CompressedSize, "Ptr", 0, "Ptr", 0, "UInt")
			throw Exception("Failed to convert MCLib b64 to binary")
		
		if !(pBinary := DllCall("GlobalAlloc", "UInt", 0, "Ptr", DecompressedSize, "Ptr"))
			throw Exception("Failed to reserve MCLib memory")

		MClib.LZ.Decompress(pDecompressionBuffer, CompressedSize, pBinary, DecompressedSize)
		
		return MClib.Load(pBinary, DecompressedSize, Symbols)
	}
}