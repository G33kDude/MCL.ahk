class $Name {
	static code := Buffer($CodeSize), exports := $Exports, codeB64 := $CodeBase64
	static __New() {
		if ($Bitness != A_PtrSize * 8)
			throw Error("$Name does not support " (A_PtrSize * 8) " bit AHK, please run using $Bitness bit AHK")

		; MCL standalone loader https://github.com/G33kDude/MCLib.ahk
		; Copyright (c) 2023 G33kDude, CloakerSmoker (CC-BY-4.0)
		; https://creativecommons.org/licenses/by/4.0/

		if !DllCall("Crypt32\CryptStringToBinary", "Str", this.codeB64, "UInt", 0, "UInt", 1, "Ptr", buf := Buffer($CompressedSize), "UInt*", $CompressedSize, "Ptr", 0, "Ptr", 0, "UInt")
			throw Error("Failed to convert MCL b64 to binary")

		if (r := DllCall("ntdll\RtlDecompressBuffer", "UShort", 0x102, "Ptr", this.code, "UInt", $CodeSize, "Ptr", buf, "UInt", $CompressedSize, "UInt*", &DecompressedSize := 0, "UInt"))
			throw Error("Error calling RtlDecompressBuffer",, Format("0x{:08x}", r))
	
$HasImports
		for import, offset in $Imports {

			if !(hDll := DllCall("GetModuleHandle", "Str", import[1], "Ptr"))
				throw Error("Could not load dll " import[1] ": " OsError().Message)
			if !(pFunction := DllCall("GetProcAddress", "Ptr", hDll, "AStr", import[2], "Ptr"))
				throw Error("Could not find function " import[2] " from " import[1] ".dll: " OsError().Message)
			
			NumPut("Ptr", pFunction, this.code, offset)
		}
$HasImports

$HasRelocations
		for k, offset in $Relocations
			NumPut("Ptr", NumGet(this.code, offset, "Ptr") + this.code.Ptr, this.code, offset)
$HasRelocations

		if !DllCall("VirtualProtect", "Ptr", this.code, "Ptr", $CodeSize, "UInt", 0x40, "UInt*", &old := 0, "UInt")
			throw Error("Failed to mark MCL memory as executable")
	}
	static Call(params*) => this.__main(params*)
$Methods
}
