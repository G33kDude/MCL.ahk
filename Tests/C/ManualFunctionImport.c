char (*__mcode_i_Shlwapi_StrToIntExW) (short*, int, int*);

int __main(short* String) {
    int Result;
    
    __mcode_i_Shlwapi_StrToIntExW(String, 0, &Result);

    return Result;
}