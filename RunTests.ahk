#Include %A_ScriptDir%

#Include Yunit\Yunit.ahk
#Include Yunit\Window.ahk
#Include Yunit\Stdout.ahk

#Include MCL.ahk

Tester := Yunit.Use(YunitStdout, YunitWindow)

Read(File) {
    return FileOpen(File, "r").Read()
}

class MCLTests {
    class C {
        Begin() {
            SetWorkingDir, %A_ScriptDir%/Tests/C
        }

        ReturnSingleValue() {
            pCode := MCL.FromC(Read("ReturnSingleValue.c"))

            Result := DllCall(pCode, "CDecl Int")
            Yunit.Assert(Result == 2390)
        }

        ManualFunctionImport() {
            pCode := MCL.FromC(Read("ManualFunctionImport.c"))

            Result := DllCall(pCode, "WStr", "2390", "CDecl Int")
            Yunit.Assert(Result == 2390)
        }

        PassLotsOfParameters() {
            pCode := MCL.FromC(Read("PassLotsOfParameters.c"))

            Result := DllCall(pCode, "Int", 2000, "Int", 150, "Int", 150, "Int", 60, "Int", 30, "CDecl Int")
            Yunit.Assert(Result == 2390)

            Result := DllCall(pCode, "Int", 1000, "Int", 1000, "Int", 1000, "Int", 1000, "Int", 1000, "CDecl Int")
            Yunit.Assert(Result == 5000)
        }

        BasicFloatMath() {
            pCode := MCL.FromC(Read("BasicFloatMath.c"))

            Result := DllCall(pCode, "Double", 3.1, "Double", 2.8, "CDecl Double")
            Yunit.Assert(Abs(Result - 12.98) < 0.0001)
        }

        ReturnSingleValueWithHeader() {
            pCode := MCL.FromC(Read("ReturnSingleValueWithHeader.c"))

            Result := DllCall(pCode, "CDecl Int")
            Yunit.Assert(Result == 2390)
        }

        AllocateMemoryAndWriteString() {
            pCode := MCL.FromC(Read("AllocateMemoryAndWriteString.c"))

            pResult := DllCall(pCode, "CDecl Ptr")

            Yunit.Assert(StrGet(pResult, "UTF-8") == "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "C code generated incorrect string '" StrGet(pResult, "UTF-8") "'")
        }

        Library() {
            Library := MCL.FromC(Read("Library.c"))

            Yunit.Assert(IsObject(Library))

            TestString := "ABC|DEF"

            PipePosition := DllCall(Library.Find, "AStr", TestString, "Int", Chr("|"), "CDecl Int")
            Yuint.Assert(PipePosition == 3)

            ReferenceHash := DllCall(Library.Hash, "AStr", "DEF", "CDecl Int")
            ActualHash := DllCall(Library.Hash, "AStr", SubStr(TestString, PipePosition), "CDecl Int")
            
            Yuint.Assert(ReferenceHash == ActualHash)
        }

        Relocations() {
            pCode := MCL.FromC(Read("Relocations.c"))

            for k, String in ["Hello", "Goodbye", "dog", "cat"] {
                pEntry := DllCall(pCode, "Int", k - 1, "CDecl Ptr")

                pString := NumGet(pEntry + 0, 0, "Ptr")

                Yunit.Assert(StrGet(pString, "UTF-8") = String)
            }
        }

        GetSetGlobal() {
            Code := MCL.FromC(Read("GetSetGlobalFromAHK.c"))

            pResultString := DllCall(Code.Check, "Int", 20, "CDecl Ptr")
            Yuint.Assert(StrGet(pResultString, "UTF-8") = "Oops, that's wrong")

            ThePassword := NumGet(Code.Password, 0, "Int")
            pResultString := DllCall(Code.Check, "Int", ThePassword, "CDecl Ptr")
            Yuint.Assert(StrGet(pResultString, "UTF-8") = "You got the password right!")

            NumPut(0, Code.Password, 0, "Int")
            pResultString := DllCall(Code.Check, "Int", ThePassword, "CDecl Ptr")
            Yuint.Assert(StrGet(pResultString, "UTF-8") = "Oops, that's wrong")
        }

        ImplicitExport() {
            pCode := MCL.FromC(Read("ImplicitExport.c"))

            Result := DllCall(pCode, "Int", 2, "Int", 9, "Int")
            Yuint.Assert(Result = 0x400)

            Result := DllCall(pCode, "Int", 0x10, "Int", 13, "Int")
            Yuint.Assert(Result = 0x20000)
        }
    }

    class CPP {
        Begin() {
            SetWorkingDir, %A_ScriptDir%/Tests/CPP
        }

        New() {
            pCode := MCL.FromCPP(Read("New.cpp"))

            pPoint := DllCall(pCode, "Int", 20, "Int", 30, "CDecl Ptr")

            Yunit.Assert(NumGet(pPoint+0, 0, "Int") = 20)
            Yunit.Assert(NumGet(pPoint+0, 4, "Int") = 30)
        }

        Spooky() {
            pCode := MCL.FromCPP(Read("Spooky.cpp"))

            Success := DllCall(pCode, "CDecl Int")

            Yunit.Assert(Success = -1)
        }
    }

    class Packed {

    }
}

Tester.Test(MCLTests)