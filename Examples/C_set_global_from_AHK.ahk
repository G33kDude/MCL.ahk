#Include %A_ScriptDir%/../
#include MCL.ahk

C := "
(

#include <mcl.h>

int SavedValue = 10;
MCL_EXPORT_GLOBAL(SavedValue);

int __main(int NewValue) {
	int Result = SavedValue;
	
	SavedValue = NewValue;
	
	return Result;
}

)"

Code := MCL.FromC(C)

NumPut("Int", 20, Code['SavedValue'])
MsgBox DllCall(Code['__main'], "Int", 30, "CDecl Int")
MsgBox DllCall(Code['__main'], "Int", 40, "CDecl Int")
MsgBox NumGet(Code['SavedValue'], 0, "Int")