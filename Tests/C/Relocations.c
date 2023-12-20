typedef struct {
	char* Key;
	int Value;
} MyMap;

// This structure will be initalized by the compiler, with relocations emitted to fixup the address of each string.
// Without properly relocating the values in the structure, they will all contain bogus pointers.

MyMap Map[] = {
	{"Hello", 20},
	{"Goodbye", 90},
	{"dog", 200},
	{"cat", 900}
};

MyMap* __MCL_f_Call$Int$index$CDecl_Ptr(int index) {
	return &Map[index];
}