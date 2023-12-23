#Requires AutoHotkey v2.0

#Include Yunit\Yunit.ahk
#Include Yunit\Window.ahk

#Include MCL.ahk

Tester := Yunit.Use(YunitWindow)

class MCLTests {
    class C {
        Begin() {
            SetWorkingDir A_ScriptDir "\Tests\C"
        }

        ReturnSingleValue() {
            lib := MCL.FromC(FileRead("ReturnSingleValue.c"))

            result := lib()
            Yunit.Assert(result == 2390)
        }

        ManualFunctionImport() {
            lib := MCL.FromC(FileRead("ManualFunctionImport.c"))

            result := lib("2390")
            Yunit.Assert(result == 2390)
        }

        PassLotsOfParameters() {
            lib := MCL.FromC(FileRead("PassLotsOfParameters.c"))

            result := lib(2000, 150, 150, 60, 30)
            Yunit.Assert(result == 2390)

            result := lib(1000, 1000, 1000, 1000, 1000)
            Yunit.Assert(result == 5000)
        }

        BasicFloatMath() {
            lib := MCL.FromC(FileRead("BasicFloatMath.c"))

            Result := lib(3.1, 2.8)
            Yunit.Assert(Abs(Result - 12.98) < 0.0001)
        }

        ReturnSingleValueWithHeader() {
            lib := MCL.FromC(FileRead("ReturnSingleValueWithHeader.c"))

            result := lib()
            Yunit.Assert(result == 2390)
        }

        AllocateMemoryAndWriteString() {
            lib := MCL.FromC(FileRead("AllocateMemoryAndWriteString.c"))

            pResult := lib()

            Yunit.Assert(
                StrGet(pResult, "UTF-8") == "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                "C code generated incorrect string '" StrGet(pResult, "UTF-8") "'"
            )
        }

        Library() {
            lib := MCL.FromC(FileRead("Library.c"))

            Yunit.Assert(IsObject(lib))

            testString := "ABC|DEF"

            pipePosition := lib.Find(testString, Ord("|"))
            Yunit.Assert(PipePosition == 3)

            referenceHash := lib.Hash("DEF")
            actualHash := lib.Hash(SubStr(testString, 1 + pipePosition + 1))

            Yunit.Assert(referenceHash == actualHash)
        }

        Relocations() {
            lib := MCL.FromC(FileRead("Relocations.c"))

            for string in ["Hello", "Goodbye", "dog", "cat"] {
                pEntry := lib(A_Index - 1)

                pString := NumGet(pEntry, 0, "Ptr")

                Yunit.Assert(StrGet(pString, "UTF-8") == string)
            }
        }

        GetSetGlobal() {
            lib := MCL.FromC(FileRead("GetSetGlobalFromAHK.c"))

            result := lib.Check(20)
            Yunit.Assert(result == "Oops, that's wrong")

            result := lib.Check(lib.Password)
            Yunit.Assert(result == "You got the password right!")

            oldPassword := lib.Password
            lib.Password := 0
            result := lib.Check(oldPassword)
            Yunit.Assert(result == "Oops, that's wrong")
        }

        ImplicitExport() {
            lib := MCL.FromC(FileRead("ImplicitExport.c"))

            result := DllCall(lib, "Int", 2, "Int", 9, "CDecl Int")
            Yunit.Assert(result == 0x400)

            result := DllCall(lib, "Int", 0x10, "Int", 13, "CDecl Int")
            Yunit.Assert(result == 0x20000)
        }

        ImplicitAllocateMemoryAndWriteString() {
            ; The big difference between this test and the last one is that this
            ; one also pulls in the stdlib headers, which define a bunch of
            ; (static) functions of their own. If static functions aren't
            ; handled correctly then the main function can't be guessed and this
            ; test will fail. The proper behavior is that static functions
            ; aren't considered when trying to guess which function is the real
            ; main function.

            lib := MCL.FromC(FileRead("ImplicitAllocateMemoryAndWriteString.c"))

            result := DllCall(lib, "CDecl AStr")

            Yunit.Assert(
                result == "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                "C code generated incorrect string '" result "'"
            )
        }

        AHKFromIsFormattedCorrectly() {
            CodeString := MCL.AHKFromC("int __main() {return 20;}", MCL.Options.DoNotFormat)
            lib := MCL.FromString(CodeString)

            Result := lib()
            Yunit.Assert(Result == 20)
        }
    }

    class CPP {
        Begin() {
            SetWorkingDir A_ScriptDir "\Tests\CPP"
        }

        New() {
            lib := MCL.FromCPP(FileRead("New.cpp"))

            pPoint := lib(20, 30)

            Yunit.Assert(NumGet(pPoint, 0, "Int") == 20)
            Yunit.Assert(NumGet(pPoint, 4, "Int") == 30)
        }

        Spooky() {
            lib := MCL.FromCPP(FileRead("Spooky.cpp"))

            success := lib()

            Yunit.Assert(success == -1)
        }
    }

    class Packed {

    }
}

Tester.Test(MCLTests)