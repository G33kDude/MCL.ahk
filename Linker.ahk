class LinkerBase {
	__New(pFileData, Length) {
		this.pData := pFileData
		this.Size := Length
	}
	
	ReadString(Offset, Length := -1) {
		if (Length > 0) {
			return StrGet(this.pData + Offset, Length, "UTF-8")
		}
		else {
			return StrGet(this.pData + Offset, "UTF-8")
		}
	}
	
	__Call(MethodName, Params*) {
		if (RegexMatch(MethodName, "O)Read(\w+)", Read) && Read[1] != "String") {
			return NumGet(this.pData + 0, Params[1], Read[1])
		}
	}
}

MoveMemory(pTo, pFrom, Size) {
    DllCall("RtlMoveMemory", "Ptr", pTo, "Ptr", pFrom, "UInt", Size)
}

class PEObjectLinker extends LinkerBase {
    class Data {
        __New(pFirst, Size) {
            this.Fragments := [{"pData": pFirst, "Size": Size}]
        }
        Add(pData, Size := "") {
            if (IsObject(pData)) {
                this.Fragments.Push(pData.Fragments*)
            }
            else {
                this.Fragments.Push({"pData": pData, "Size": Size})
            }
        }
        Length() {
            Result := 0

            for k, v in this.Fragments {
                Result += v.Size
            }

            return Result
        }
        Coalesce(pDestination) {
            for k, v in this.Fragments {
                MoveMemory(pDestination, v.pData, v.Size)
                pDestination += v.Size
            }
        }
        Read(Offset, Type) {
            for k, v in this.Fragments {
                if (Offset < v.Size) {
                    return NumGet(v.pData, Offset, Type)
                }

                Offset -= v.Size
            }

            Throw Exception("Attempt to read from offset past end of section data")
        }
        Write(Value, Offset, Type) {
            for k, v in this.Fragments {
                if (Offset < v.Size) {
                    return NumPut(Value, v.pData, Offset, Type)
                }

                Offset -= v.Size
            }

            Throw Exception("Attempt to write to offset past end of section data")
        }
    }

    static hProcessHeap := DllCall("GetProcessHeap", "Ptr")

    Allocate(Size) {
        return DllCall("HeapAlloc", "Ptr", this.hProcessHeap, "UInt", 0x8, "UInt", Size, "Ptr")
    }
    Free(pMemory) {
        DllCall("HeapFree", "Ptr", this.hProcessHeap, "UInt", 0, "Ptr", pMemory)
    }

	ReadSection(HeadersBase, HeaderIndex) {
        static SIZEOF_SECTION_HEADER := 40

        HeaderOffset := HeadersBase + (HeaderIndex * SIZEOF_SECTION_HEADER)

		Result := {}
		
		Result.Name              := this.ReadString(HeaderOffset, 8)
		Result.VirtualSize       :=   this.ReadUInt(HeaderOffset + 8)
		Result.VirtualAddress    :=   this.ReadUInt(HeaderOffset + 12)
		Result.FileSize          :=   this.ReadUInt(HeaderOffset + 16)
		Result.FileOffset        :=   this.ReadUInt(HeaderOffset + 20)
		Result.RelocationsOffset :=   this.ReadUInt(HeaderOffset + 24)
		Result.RelocationCount   :=   this.ReadUInt(HeaderOffset + 32)
		Result.Characteristics   :=   this.ReadUInt(HeaderOffset + 36)

        InitialData := this.pData + Result.FileOffset

        if (Result.FileOffset = 0) {
            InitialData := this.Allocate(Result.FileSize)
        }

        Result.Data := new this.Data(InitialData, Result.FileSize)

        Result.Symbol := this.SymbolsByName[Result.Name]

        Result.Relocations := []

        NextRelocationOffset := Result.RelocationsOffset

        loop, % Result.RelocationCount {
            RelocationAddress :=   this.ReadUInt(NextRelocationOffset)
            SymbolIndex       :=   this.ReadUInt(NextRelocationOffset + 4)
            RelocationType    := this.ReadUShort(NextRelocationOffset + 8)
            
            NextRelocationOffset += 10

            RelocationSymbol := this.Symbols[SymbolIndex + 1]

            Result.Relocations.Push({"Address": RelocationAddress, "Type": RelocationType, "Symbol": RelocationSymbol})
        }

        Result.Symbols := []
        Result.SymbolsByName := {}

        for Name, Symbol in this.SymbolsByName {
            if (Symbol.SectionIndex - 1 = HeaderIndex) {
                Symbol.Section := Result

                Result.Symbols.Push(Symbol)
                Result.SymbolsByName[Name] := Symbol
            }
        }
		
		return Result
	}

	ReadSymbolHeader(SymbolNamesOffset, HeaderOffset) {
		Result := {}
		
		if (this.ReadUInt(HeaderOffset) != 0) {
			Result.Name := this.ReadString(HeaderOffset, 8)
		}
		else {
			Result.Name := this.ReadString(SymbolNamesOffset + this.ReadUInt(HeaderOffset + 4))
		}
		
		Result.Value          :=   this.ReadUInt(HeaderOffset + 8)
		Result.SectionIndex   := this.ReadUShort(HeaderOffset + 12)
		Result.Type           := this.ReadUShort(HeaderOffset + 14)
		Result.StorageClass   :=  this.ReadUChar(HeaderOffset + 16)
		Result.AuxSymbolCount :=  this.ReadUChar(HeaderOffset + 17)
		
		return Result
	}

    MergeSections(Target, Sources*) {
        for k, Section in Sources {
            if (Section.Name = Target.Name) {
                continue
            }

            OffsetInTarget := Target.Data.Length()
            Target.Data.Add(Section.Data) ; Merge section data

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
            
            this.SectionsByName.Delete(Name)
        }
    }
	
	Read() {
		static SIZEOF_COFF_HEADER := 20
        static SIZEOF_SECTION_HEADER := 40
		static SIZEOF_SYMBOL := 18
		
		if (this.ReadUShort(0) != 0x8664) {
			throw Exception("Not a valid 64 bit PE object file")
		}

		SymbolTableOffset := this.ReadUInt(8)
		SymbolCount := this.ReadUInt(12)
		SymbolNamesOffset := SymbolTableOffset + (SymbolCount * SIZEOF_SYMBOL)
		
		this.Symbols := []
		this.SymbolsByName := {}
		
		SymbolIndex := 0
		
		while (SymbolIndex < SymbolCount) {
			this.Symbols.Push(NextSymbol := this.ReadSymbolHeader(SymbolNamesOffset, SymbolTableOffset + (SymbolIndex * SIZEOF_SYMBOL)))
			
			this.SymbolsByName[NextSymbol.Name] := NextSymbol

            loop, % NextSymbol.AuxSymbolCount {
                this.Symbols.Push({})
            }
			
			SymbolIndex += 1 + NextSymbol.AuxSymbolCount
		}
		
		SectionHeaderCount := this.ReadUShort(2)
		SizeOfOptionalHeader := this.ReadUShort(16)
		
		SectionHeaderTableOffset := SIZEOF_COFF_HEADER + SizeOfOptionalHeader
		
		this.Sections := []
		this.SectionsByName := {}
		
		loop, % SectionHeaderCount {
			this.Sections.Push(NextSection := this.ReadSection(SectionHeaderTableOffset, (A_Index - 1)))
			
			this.SectionsByName[NextSection.Name] := NextSection
		}
	}

    DoStaticRelocations(Section) {
        static IMAGE_REL_AMD64_REL32 := 0x4
        
        for k, Relocation in Section.Relocations.Clone() {
            if (Relocation.Symbol.Section.Name != Section.Name) {
                continue
            }

            if (Relocation.Type = IMAGE_REL_AMD64_REL32) {
                Source := Relocation.Address

                Offset := Section.Data.Read(Source, "Int")
                Target := Relocation.Symbol.Value + Offset

                Difference := Target - (Source + 4)

                Section.Data.Write(Difference, Source, "Int")

                Section.Relocations.Delete(k)
            }
        }
    }

    /*
    RelocateSection(TargetName, ToAddress) {
        Target := this.SectionsByName[TargetName]
        Target.VirtualAddress := ToAddress

        Size := Target.Data.Length()
        pData := this.Allocate(Size)

        Target.Data.Read(pData)

        Target.Data := new this.Data(pData, Size)

        for k, Relocation in Target.Relocations {
            if (Relocation)

        }
    }
    */

    CollectSectionDependencies(Result, Target) {
        if (Result.HasKey(Target.Name)) {
            return
        }

        Result[Target.Name] := Target

        for k, Relocation in Target.Relocations {
            this.CollectSectionDependencies(Result, Relocation.Symbol.Section)
        }
    }

    MakeSectionStandalone(Target) {
        Dependencies := {}
        this.CollectSectionDependencies(Dependencies, Target)

        for Name, Section in Dependencies {
            this.MergeSections(Target, Section)
        }
    }
}

/* if !(F := FileOpen("C:\Users\Connor\Desktop\MCLib.ahk\Tests\C\out.o", "r"))
    Throw Exception("Failed to load output file")

Size := F.Length

if !(pPE := DllCall("GlobalAlloc", "UInt", 0, "Ptr", Size, "Ptr"))
    Throw Exception("Failed to reserve MCM PE memory")

F.RawRead(pPE + 0, Size)
F.Close()
p := new PEObjectLinker(pPE, Size)
p.Read()

s := p.SectionsByName[".text"]
p.MakeSectionStandalone(s)
p.DoStaticRelocations(s)

size := s.Data.Length()
VarSetCapacity(Buffer, Size, 0)
s.Data.Coalesce(&Buffer)

F := FileOpen("out.bin", "w")
F.RawWrite(&Buffer, Size)
F.Close() */