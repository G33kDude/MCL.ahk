#Include %A_ScriptDir%/../
#include MCL.ahk

C := "
(

double __main(double In) {
	return In * 2.5;
}

)"

pCode := MCL.FromC(C)

MsgBox DllCall(pCode, "Double", 11.7, "Double")