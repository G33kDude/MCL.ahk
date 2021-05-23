
MCLib_FromC(code)
{
	;EnvSet, PATH, C:\Program Files\mingw-w64\x86_64-8.1.0-win32-seh-rt_v6-rev0\mingw64\bin
	EnvSet, PATH, C:\TDM-GCC-64\bin
	SetWorkingDir, %A_Desktop%\MCLib
	FileDelete, test.*
	
	fmt := A_PtrSize == 4 ? "pe-i386" : "pe-x86-64"
	FileOpen("linker", "w").Write("OUTPUT_FORMAT(" fmt ")SECTIONS{.text :{*(.ahk_entry)*(.text*)*(.rodata*)*(.rdata*)}}")
	FileOpen("test.c", "w").Write("#define ahk_entry __attribute__((section("".ahk_entry"")))`n" code)
	
	shell := ComObjCreate("WScript.Shell")
	exec := shell.Exec("gcc.exe -m" A_PtrSize*8 " -g0 -O3 -c test.c")
	exec.StdIn.Close()
	
	if !exec.StdErr.AtEndOfStream
		throw Exception(exec.StdErr.ReadAll(),, "Compiler Error")
	
	m := A_PtrSize == 4 ? "-m i386pe" : ""
	if shell.Run("ld.exe " m " -T linker test.o -o test.bin", 0, 1)
		throw Exception("Failed to link",, "Linker Error")
	if shell.Run("objcopy.exe -O binary -j .text test.bin test.bin", 0, 1)
		throw Exception("Failed to extract",, "ObjCopy Error")
	
	if !(f := FileOpen("test.bin", "r"))
		throw Exception("Failed to load bin")
	
	;while !f.AtEOF
	;	hex .= Format("{:02x}", f.ReadUChar())
	;msgbox, %hex%
	
	cbBinary := f.length
	if !(pBinary := DllCall("GlobalAlloc", "UInt", 0, "Ptr", cbBinary, "Ptr"))
		throw Exception("Failed to reserve MCLib memory")
	
	if !DllCall("VirtualProtect"
		, "Ptr", pBinary  ; LPVOID lpAddress
		, "Ptr", cbBinary ; SIZE_T dwSize
		, "UInt", 0x40    ; DWORD  flNewProtect
		, "UInt*", op     ; PDWORD lpflOldProtect
		, "UInt") ; BOOL
		throw Exception("Failed to mark MCLib memory as executable")
	
	f.RawRead(pBinary+0, cbBinary)
	
	return pBinary
}

MCLib_AhkFromC(code)
{
	EnvSet, PATH, C:\TDM-GCC-64\bin
	SetWorkingDir, %A_Desktop%\MCLib
	FileDelete, test.*
	
	; Write the code to a file
	FileOpen("test.c", "w").Write("#define ahk_entry __attribute__((section("".ahk_entry"")))`n" code)
	
	; Execute GCC to compile the c to an unlinked object file
	shell := ComObjCreate("WScript.Shell")
	exec := shell.Exec("gcc.exe -m" A_PtrSize*8 " -g0 -O3 -c test.c")
	exec.StdIn.Close()
	
	if !exec.StdErr.AtEndOfStream
		throw Exception(exec.StdErr.ReadAll(),, "Compiler Error")
	
	; Write the appropriate linker script
	fmt := A_PtrSize == 4 ? "pe-i386" : "pe-x86-64"
	FileOpen("linker", "w").Write("OUTPUT_FORMAT(" fmt ")SECTIONS{.text :{*(.ahk_entry)*(.text*)*(.rodata*)*(.rdata*)}}")
	
	; Run the linker using the linker script
	m := A_PtrSize == 4 ? "-m i386pe" : ""
	if shell.Run("ld.exe " m " -T linker test.o -o test.bin", 0, 1)
		throw Exception("Failed to link",, "Linker Error")
	
	; Extract the .text section from the linked binary
	if shell.Run("objcopy.exe -O binary -j .text test.bin test.bin", 0, 1)
		throw Exception("Failed to extract",, "ObjCopy Error")
	
	; Load the linked binary into buffer `buf`
	if !(f := FileOpen("test.bin", "r"))
		throw Exception("Failed to load bin")
	cbBuf := f.length
	f.RawRead(buf, cbBuf)
	
	; Compress the buffer with LZ compression
	cbCBuf := VarSetCapacity(CBuf, cbBuf*2, 0)
	cbCBuf := LZ_Compress(&buf, cbBuf, &CBuf, cbCBuf)
	
	; Calculate the target size of the base64 string buffer
	if !DllCall("Crypt32\CryptBinaryToString"
		, "Ptr", &CBuf       ; const BYTE *pbBinary
		, "UInt", cbCBuf     ; DWORD      cbBinary
		, "UInt", 0x40000001 ; DWORD      dwFlags = CRYPT_STRING_BASE64 | CRYPT_STRING_NOCRLF
		, "Ptr", 0           ; LPWSTR     pszString
		, "UInt*", cchStr    ; DWORD      *pcchString
		, "UInt") ; BOOL
		throw Exception("Failed to calculate b64 size")
	
	; Allocate that much memory into the buffer `str`
	VarSetCapacity(str, cchStr*(1+A_IsUnicode), 0)
	
	; Convert to base64
	if !DllCall("Crypt32\CryptBinaryToString"
		, "Ptr", &CBuf       ; const BYTE *pbBinary
		, "UInt", cbCBuf     ; DWORD      cbBinary
		, "UInt", 0x40000001 ; DWORD      dwFlags = CRYPT_STRING_BASE64 | CRYPT_STRING_NOCRLF
		, "Str", str         ; LPWSTR     pszString
		, "UInt*", cchStr    ; DWORD      *pcchString
		, "UInt") ; BOOL
		throw Exception("Failed to calculate b64 size")
	
	; Format output
	; out := "MCLib(" cbBuf ", """""
	while StrLen(str)
		out .= "`n. """ SubStr(str, 1, 120-8) """", str := SubStr(str, (120-8)+1)
	; out .= ")"

	return out "`n" cbBuf
}

MCLib(cbBinary, ByRef b64)
{
	if (A_PtrSize != 8 || !A_IsUnicode)
		throw Exception("AHK U64 only")
	if !DllCall("Crypt32\CryptStringToBinary", "Str", b64, "UInt", 0, "UInt", 1
		, "UPtr", 0, "UInt*", cbCBuf, "Ptr", 0, "Ptr", 0, "UInt")
		throw Exception("Failed to parse MCLib b64 to binary")
	cbCbuf := VarSetCapacity(CBuf, cbCBuf, 0)
	if !DllCall("Crypt32\CryptStringToBinary", "Str", b64, "UInt", 0, "UInt", 1
		, "Ptr", &CBuf, "UInt*", cbCBuf, "Ptr", 0, "Ptr", 0, "UInt")
		throw Exception("Failed to convert MCLib b64 to binary")
	if !(pBinary := DllCall("GlobalAlloc", "UInt", 0, "Ptr", cbBinary, "Ptr"))
		throw Exception("Failed to reserve MCLib memory")
	if !DllCall("VirtualProtect", "Ptr", pBinary, "Ptr", cbBinary, "UInt", 0x40
		, "UInt*", op, "UInt")
		throw Exception("Failed to mark MCLib memory as executable")
	if (r := DllCall("ntdll\RtlDecompressBuffer", "UShort",  0x102, "Ptr"
		, pBinary, "UInt", cbBinary, "Ptr", &CBuf, "UInt", cbCBuf, "UInt*"
		, cbFinal, "UInt"))
		throw Exception("Error decompressing MCLib",, Format("0x{:08x}", r))
	return pBinary
}
