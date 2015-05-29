fn putchar(int32 ch) -> int32;
fn puts(uint8^ ch) -> int32;

fn main() -> int32 {
	let message = "Hej! Hur mår du?";
	puts(message);

	int32 i = 7;
	int32 result = 5;
	int32 ^resultptr = &result;
	while i > 0 {
		*resultptr = *resultptr * 2;
		i = i - 1;
	}

	uint8 ^charptr = (uint8 ^)resultptr;
	uint8 ^^charptrptr = &charptr;

	// 640 [little endian 32-bit] = 10000000 00000010 00000000 00000000 => 2
	//                                       ^_______________^
	//                                               |
	//                                           [16 bits]
	// => 2 + 'e' (ASCII: 101) = 103
	return *((int16 ^)(*charptrptr + 1)) + *((uint8 ^)message + 1);
}
