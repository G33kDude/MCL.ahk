char (*__MCL_i_Shlwapi$StrToIntExW) (short*, int, int*) = (char(*)(short*, int, int*))0;

int _stdcall __MCL_f_Call$WStr$string$Int(short* string) {
    int result;

    __MCL_i_Shlwapi$StrToIntExW(string, 0, &result);

    return result;
}