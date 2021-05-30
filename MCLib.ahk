#Include SymbolReader.ahk

SetWorkingDir, %A_ScriptDir%

class MClib {
	Generic(Compiler, Code, ExtraOptions := "") {
		FileOpen("linker_script", "w").Write("OUTPUT_FORMAT(pe-x86-64)SECTIONS{.text :{*(.text*)*(.rodata*)*(.rdata*)*(.data*)*(.bss*)}}")
		
		FileOpen("test.c", "w").Write(code)
		
		while (!FileExist("test.c")) {
			Sleep, 100
		}
		
		shell := ComObjCreate("WScript.Shell")
		exec := shell.Exec("x86_64-w64-mingw32-" Compiler ".exe -m64" ExtraOptions " -ffreestanding -nostdlib -T linker_script test.c")
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
		
		FileDelete, a.exe
		
		Reader := new PESymbolReader(pPE, Size)
		Output := Reader.Read()
		
		pCode := pPE + Output.SectionsByName[".text"].FileOffset

		return [pCode, Output]
	}
	
	Fixup(pCode, Output) {
		static hKernel32    := DllCall("GetModuleHandle", "Str", "kernel32", "Ptr")
		static hProcessHeap := DllCall("GetProcessHeap", "Ptr")
		static HeapAlloc    := DllCall("GetProcAddress", "Ptr", hKernel32, "AStr", "HeapAlloc", "Ptr")
		static HeapFree     := DllCall("GetProcAddress", "Ptr", hKernel32, "AStr", "HeapFree", "Ptr")
		
		OffsetOfhProcessHeap := Output.AbsoluteSymbols["hProcessHeap"].Value
		OffsetOfHeapAlloc    := Output.AbsoluteSymbols["HeapAlloc"].Value
		OffsetOfHeapFree     := Output.AbsoluteSymbols["HeapFree"].Value
		OffsetOfMain         := Output.AbsoluteSymbols["__main"].Value
		
		if (OffsetOfhProcessHeap && OffsetOfHeapAlloc && OffsetOfHeapFree) {
			NumPut(hProcessHeap, pCode + 0, OffsetOfhProcessHeap, "Ptr")
			NumPut(HeapAlloc   , pCode + 0, OffsetOfHeapAlloc   , "Ptr")
			NumPut(HeapFree    , pCode + 0, OffsetOfHeapFree    , "Ptr")
		}
		
		TextSize := Output.SectionsByName[".text"].FileSize
		
		if !DllCall("VirtualProtect", "Ptr", pCode, "Ptr", TextSize, "UInt", 0x40, "UInt*", OldProtect, "UInt")
			Throw Exception("Failed to mark MCLib memory as executable")
		
		return pCode + OffsetOfMain
	}
	
	FromC(Code) {
		return this.Fixup(this.Generic("gcc", Code)*)
	}
	
	FromCPP(Code) {
		return this.Fixup(this.Generic("g++", "extern ""C"" {`n" Code "`n}", " -fno-exceptions -fno-rtti")*)
	}
	
	Pack(pCode, Output) {
		if (Output.AbsoluteSymbols["__main"].Value != 0) {
			Throw "Main is not first function in compiled code, code cannot be packed to AHK mcode"
		}
		
		TextSize := Output.SectionsByName[".text"].FileSize
		
		; Calculate the target size of the base64 string buffer
		if !DllCall("Crypt32\CryptBinaryToString"
			, "Ptr", pCode       ; const BYTE *pbBinary
			, "UInt", TextSize     ; DWORD      cbBinary
			, "UInt", 0x40000001 ; DWORD      dwFlags = CRYPT_STRING_BASE64 | CRYPT_STRING_NOCRLF
			, "Ptr", 0           ; LPWSTR     pszString
			, "UInt*", Base64Length    ; DWORD      *pcchString
			, "UInt") ; BOOL
			throw Exception("Failed to calculate b64 size")
		
		VarSetCapacity(Base64, Base64Length * (1 + A_IsUnicode), 0)
		
		; Convert to base64
		if !DllCall("Crypt32\CryptBinaryToString"
			, "Ptr", pCode       ; const BYTE *pbBinary
			, "UInt", TextSize     ; DWORD      cbBinary
			, "UInt", 0x40000001 ; DWORD      dwFlags = CRYPT_STRING_BASE64 | CRYPT_STRING_NOCRLF
			, "Str", Base64         ; LPWSTR     pszString
			, "UInt*", Base64Length    ; DWORD      *pcchString
			, "UInt") ; BOOL
			throw Exception("Failed to convert to b64")
		
		while StrLen(Base64) {
			Out .= "`n. """ SubStr(Base64, 1, 120-8) """"
			Base64 := SubStr(Base64, (120-8)+1)
		}
		
		return Out
	}
	
	AHKFromC(Code) {
		return this.Pack(this.Generic("gcc", Code)*)
	}
	
	AHKFromCPP(Code) {
		return this.Pack(this.Generic("g++", "extern ""C"" {`n" Code "`n}", " -fno-exceptions -fno-rtti")*)
	}
}
