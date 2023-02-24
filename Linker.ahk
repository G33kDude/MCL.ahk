class LinkerBase {
	__New(pFileData, Length) {
		this.pData := pFileData
		this.Size := Length
	}

	/**
	 * Read a UTF-8 string from the given offset
	 * @param {Integer} Offset The offset to read from
	 * @param {Length}  Length The maximum numbers of characters to read
	 * @return {String} The read string
	 */
	ReadString(Offset, Length := unset) {
		if IsSet(Length)
			return StrGet(this.pData + Offset, Length, "UTF-8")
		else
			return StrGet(this.pData + Offset, "UTF-8")
	}

	__Call(MethodName, Params) {
		if (RegexMatch(MethodName, "Read(\w+)", &Read) && Read[1] != "String") {
			return NumGet(this.pData, Params[1], Read[1])
		}
	}
}

MoveMemory(pTo, pFrom, Size) {
	DllCall("RtlMoveMemory", "Ptr", pTo, "Ptr", pFrom, "UInt", Size)
}

class PEObjectLinker extends LinkerBase {
	class Data {
		; Just a list of "fragments" of data, aka an easy way to join multiple smaller buffers into one larger one without
		;  loads of copies (at least until all data needs to be written to a single buffer, aka coalesced)

		__New(pFirst, Size) {
			this.Fragments := [{ pData: pFirst, Size: Size }]
		}
		Add(pData, Size := "") {
			; Adding another Data object, just add all of its fragments
			frags := IsObject(pData) ? pData.Fragments : [{ pData: pData, Size: Size }]
			this.Fragments.Push(frags*)
		}
		Length() {
			Result := 0
			for k, v in this.Fragments
				Result += v.Size
			return Result
		}
		/**
		 * Merge all fragments into a single buffer
		 */
		Coalesce(pDestination) {
			for k, v in this.Fragments {
				MoveMemory(pDestination, v.pData, v.Size)
				pDestination += v.Size
			}
		}
		Read(Offset, Type) {
			for k, v in this.Fragments {
				if (Offset < v.Size)
					return NumGet(v.pData, Offset, Type)
				Offset -= v.Size
			}
			throw Error("Attempt to read from offset past end of section data")
		}
		Write(Value, Offset, Type) {
			for k, v in this.Fragments {
				if (Offset < v.Size)
					return NumPut(Type, Value, v.pData, Offset)
				Offset -= v.Size
			}
			throw Error("Attempt to write to offset past end of section data")
		}
	}

	/** Handle to the heap for this process */
	hProcessHeap := DllCall("GetProcessHeap", "Ptr")

	Allocate(Size) {
		; For sections initialized to 0, memory has to be allocated for them, since none is allocated inside of the file itself
		return DllCall("HeapAlloc", "Ptr", this.hProcessHeap, "UInt", 0x8, "UInt", Size, "Ptr")
	}
	Free(pMemory) {
		DllCall("HeapFree", "Ptr", this.hProcessHeap, "UInt", 0, "Ptr", pMemory)
	}

	ReadSection(HeadersBase, HeaderIndex) {
		static SIZEOF_SECTION_HEADER := 40

		HeaderOffset := HeadersBase + (HeaderIndex * SIZEOF_SECTION_HEADER)

		Result := {}

		Result.Name := this.ReadString(HeaderOffset, 8)
		Result.VirtualSize := this.ReadUInt(HeaderOffset + 8)
		Result.VirtualAddress := this.ReadUInt(HeaderOffset + 12)
		Result.FileSize := this.ReadUInt(HeaderOffset + 16)
		Result.FileOffset := this.ReadUInt(HeaderOffset + 20)
		Result.RelocationsOffset := this.ReadUInt(HeaderOffset + 24)
		Result.RelocationCount := this.ReadUInt(HeaderOffset + 32)
		Result.Characteristics := this.ReadUInt(HeaderOffset + 36)

		if (SubStr(Result.Name, 1, 1) = "/") {
			NameOffset := SubStr(Result.Name, 2) + 0

			Result.Name := this.ReadString(this.StringTableOffset + NameOffset)
		}

		InitialData := this.pData + Result.FileOffset

		if (Result.FileOffset = 0) {
			; As mentioned, a section initialized to 0, which has no space allocated for it in the file. So we
			;  need to allocate some space for it.

			InitialData := this.Allocate(Result.FileSize)
		}

		Result.Data := PEObjectLinker.Data(InitialData, Result.FileSize)

		Result.Symbol := this.SymbolsByName[Result.Name]

		Result.Relocations := []

		NextRelocationOffset := Result.RelocationsOffset

		loop Result.RelocationCount {
			; All relocations are processed later, but it is easiest to read them early and have them ready
			;  whenever we need.
			RelocationAddress := this.ReadUInt(NextRelocationOffset)
			SymbolIndex := this.ReadUInt(NextRelocationOffset + 4)
			RelocationType := this.ReadUShort(NextRelocationOffset + 8)
			NextRelocationOffset += 10
			RelocationSymbol := this.Symbols[SymbolIndex + 1]
			Result.Relocations.Push({
				Address: RelocationAddress,
				Type: RelocationType,
				Symbol: RelocationSymbol
			})
		}

		; Technically, symbols are global. But each one exists inside of a single section, so we can filter the
		; global list of symbols into the sections living inside of each section to make things a bit easier.

		Result.Symbols := []
		Result.SymbolsByName := Map()

		for Name, Symbol in this.SymbolsByName {
			if (Symbol.SectionIndex - 1 != HeaderIndex)
				continue
			Symbol.Section := Result

			Result.Symbols.Push(Symbol)
			Result.SymbolsByName[Name] := Symbol	; Also, we store a reference to the section containing the Symbol
			; inside the symbol itself, so we can easily figure out *where* a symbol is supposed to be.
		}

		return Result
	}

	ReadSymbolHeader(HeaderOffset) {
		Result := {}

		if (this.ReadUInt(HeaderOffset) != 0) {
			Result.Name := this.ReadString(HeaderOffset, 8)
		} else {
			Result.Name := this.ReadString(this.StringTableOffset + this.ReadUInt(HeaderOffset + 4))
		}

		Result.Value := this.ReadUInt(HeaderOffset + 8)
		Result.SectionIndex := this.ReadUShort(HeaderOffset + 12)
		Result.Type := this.ReadUShort(HeaderOffset + 14)
		Result.StorageClass := this.ReadUChar(HeaderOffset + 16)
		Result.AuxSymbolCount := this.ReadUChar(HeaderOffset + 17)

		return Result
	}

	MergeSections(Target, Sources*) {
		; Given a target section, and multiple source sections, join them all into a single section.
		; This is done by appending the "source" section's data to the "target" section's data, and
		;  then adding the offset of the "source" inside of the "target" to every symbol/relocation
		;   inside of the "source".
		; Then, the "source" symbols can just be added to the "target" symbol lists, and the "source"
		;  relocations can just be added to the "target" relocation list.

		for k, Section in Sources {
			if (Section.Name = Target.Name)
				continue

			OffsetInTarget := Target.Data.Length()
			Target.Data.Add(Section.Data)	; Merge section data

			for k, Symbol in Section.Symbols {
				Symbol.Value += OffsetInTarget
				Symbol.Section := Target

				Target.Symbols.Push(Symbol)
				Target.SymbolsByName[Symbol.Name] := Symbol
			}

			for k, Relocation in Section.Relocations {
				Relocation.Address += OffsetInTarget

				Target.Relocations.Push(Relocation)
			}

			Target.RelocationCount += Section.RelocationCount

			this.SectionsByName.Delete(Section.Name)	; Remove the merged section from the file's list of sections,
			; since it effectively no longer exists
		}
	}

	Read() {
		; Load all fields from the PE object file loaded into this instance

		static SIZEOF_COFF_HEADER := 20
		static SIZEOF_SECTION_HEADER := 40
		static SIZEOF_SYMBOL := 18

		Magic := this.ReadUShort(0)
		this.Is32Bit := Magic = 0x14c
		this.Is64Bit := Magic = 0x8664

		if (this.Is32Bit + this.Is64Bit != 1) {
			throw Error("Not a valid 32/64 bit PE object file")
		}

		SymbolTableOffset := this.ReadUInt(8)
		SymbolCount := this.ReadUInt(12)

		this.StringTableOffset := SymbolTableOffset + (SymbolCount * SIZEOF_SYMBOL)

		this.Symbols := []
		this.SymbolsByName := Map()

		SymbolIndex := 0

		while (SymbolIndex < SymbolCount) {
			this.Symbols.Push(NextSymbol := this.ReadSymbolHeader(SymbolTableOffset + (SymbolIndex * SIZEOF_SYMBOL)))

			this.SymbolsByName[NextSymbol.Name] := NextSymbol

			loop NextSymbol.AuxSymbolCount {
				; Aux symbols only serve to pad out the symbol table so the indexes inside of relocations are correct.
				this.Symbols.Push({})
			}

			SymbolIndex += 1 + NextSymbol.AuxSymbolCount
		}

		SectionHeaderCount := this.ReadUShort(2)
		SizeOfOptionalHeader := this.ReadUShort(16)

		SectionHeaderTableOffset := SIZEOF_COFF_HEADER + SizeOfOptionalHeader

		this.Sections := []
		this.SectionsByName := Map()

		loop SectionHeaderCount {
NextSection := this.ReadSection(SectionHeaderTableOffset, (A_Index - 1))
			this.Sections.Push(NextSection)
			this.SectionsByName[NextSection.Name] := NextSection
		}
	}

	DoStaticRelocations(Section) {
		; Resolve any relocations which are independent of the image base/load address.
		; On 64 bit, this is anything RIP-relative to `Section`, or DISP8/DISP32 operands.
		; On 32 bit, this is only DISP8/DISP32 operands, since RIP-relative doesn't exist.

		static IMAGE_REL_I386_REL32 := 0x14
		static IMAGE_REL_AMD64_REL32 := 0x4

		; Note: The relocation list is cloned since we'll be modifying it to remove any relocations resolved
		;  entirely within this function.

		for k, Relocation in Section.Relocations.Clone() {
			if (Relocation.Symbol.Section.Name != Section.Name) {
				; If the data is relocated against a symbol in any other section, we can't resolve it, since
				;  we don't actually know the offset between the two sections.

				continue
			}

			Is32BitRel32 := this.Is32Bit && Relocation.Type = IMAGE_REL_I386_REL32
			Is64BitRel32 := this.Is64Bit && Relocation.Type = IMAGE_REL_AMD64_REL32

			if (Is32BitRel32 || Is64BitRel32) {
				; Rel32, we need to find the distance between `RelocationAddress` and `SymbolAddress + *RelocationAddres`
				;  and write it back to `RelocationAddress`.

				Source := Relocation.Address

				Offset := Section.Data.Read(Source, "Int")
				Target := Relocation.Symbol.Value + Offset

				Difference := Target - (Source + 4)	; An extra 4 is added to get the value of RIP (or EIP) while the instruction
				; containing the RIP-relative address is executing.

				Section.Data.Write(Difference, Source, "Int")

				Section.Relocations.Delete(k)	; This relocation is handled, nobody else needs to worry about it.
			}
		}
	}

	/**
	 * @param {Map} Result - Map to populate with dependencies
	 */
	CollectSectionDependencies(Result, Target) {
		; Given a section `Target`, set `Result[RequiredSectionName] := RequiredSection` for each
		; section which `Target` requires loaded into memory in order to run correctly.
		; And of course, this includes any sections which `RequiredSection` itself requires, and so on.

		if (Result.Has(Target.Name))
			return

		Result[Target.Name] := Target

		for k, Relocation in Target.Relocations
			this.CollectSectionDependencies(Result, Relocation.Symbol.Section)
	}

	MakeSectionStandalone(Target) {
		; Do the needed data-juggling in order to make the section `Target` contain everything it needs
		; in order to run correctly.
		; This means finding every section it depends on, and then merging them all into one big happy
		; section that can be laoded as a binary blob.

		Dependencies := Map()
		this.CollectSectionDependencies(Dependencies, Target)

		for Name, Section in Dependencies
			this.MergeSections(Target, Section)
	}
}
