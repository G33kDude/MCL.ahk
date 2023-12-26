
/**
 * Executes a template of the AutoHotkey Template Preprocessor format.
 * 
 * https://www.autohotkey.com/boards/viewtopic.php?f=83&t=115728
 * 
 * @param {String} input The template to be executed
 * @param {Object} context Parameters to pass to the template by COM
 * @param {String} [ahkPath] Path to the AHK executable
 * @returns {String} The result of the template execution
 */
RunTemplate(input, context := {}, ahkPath := A_AhkPath) {
    Quote(text) {
        for pair in [["``", "``" "``"], ["`r", "``r"], ["`n", "``n"], ["`t", "``t"], ["'", "``'"]]
            text := StrReplace(text, pair*)
        return "'" text "'"
    }
    code := "", pos := 1
    while foundPos := RegExMatch(input, "s)<\?\s*(.*?)\s*\?>", &match, pos) {
        code .= (
            (foundPos <= pos ? "" : "ctx.result .= " Quote(SubStr(input, pos, foundPos - pos)) "`n")
            (match.1 ~= "^=" ? "ctx.result .= (" SubStr(match.1, 2) ")`n" : match.1 "`n")
        ), pos := foundPos + match.Len
    }
    if pos <= StrLen(input)
        code .= "ctx.result .= " Quote(SubStr(input, pos)) "`n"
    IID_IDispatch := Buffer(16)
    NumPut("Int64", 0x20400, "Int64", 0x46000000000000c0, IID_IDispatch)
    lResult := DllCall("oleacc\LresultFromObject", "Ptr", IID_IDispatch, "Ptr", 0, "Ptr", ObjPtr(context), "Ptr")
    try {
        exec := ComObject("WScript.Shell").Exec('"' ahkPath '" *')
        exec.StdIn.Write(Format(
            'ctx := ((lResult) => (`n'
            '    IID_IDispatch := Buffer(16),`n'
            '       NumPut("Int64", 0x20400, "Int64", 0x46000000000000c0, IID_IDispatch),`n'
            '       DllCall("oleacc\ObjectFromLresult", "Ptr", lResult, "Ptr", IID_IDispatch, "Ptr", 0, "Ptr*", &pObj := 0),`n'
            '       ComValue(9, pObj)`n'
            '))({})`n'
            '{}',
            lResult,
            code))
        exec.StdIn.Close()
        while exec.Status = 0
            Sleep(10)
    } catch Any as e {
        DllCall("oleacc\ObjectFromLresult", "Ptr", lResult, "Ptr", IID_IDispatch, "Ptr", 0, "Ptr*", ComValue(9, 0))
        throw e
    }
    return context.result
}