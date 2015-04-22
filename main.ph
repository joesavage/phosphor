fn putchar(int32 ch) -> int32;

fn main() -> int32 {
	int32 i = 7;
	int32 result = 5;
	int32 ^resultptr = &result;
	while i > 0 {
		*resultptr = *resultptr * 2;
		i = i - 1;
	}

	// I don't think this is undefined behaviour, but I need to check with LLVM.
	uint8 ^charptr = (uint8 ^)resultptr;

	// 640 [little endian 32-bit] = 10000000 00000010 00000000 00000000 => 2
	//                                       ^_______________^
	//                                               |
	//                                           [16 bits]
	return *((int16 ^)(charptr + 1));
}
