char (*__MCL_i_Shlwapi$StrToIntExW) (short*, int, int*) = (char(*)(short*, int, int*))0;

int __main(short* String) {
    int Result;
    
    __MCL_i_Shlwapi$StrToIntExW(String, 0, &Result);

    return Result;
}