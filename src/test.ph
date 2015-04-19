fn putchar(int32 ch) -> int32;

fn main() -> int32 {
	int32 i = 0;
	int32 result = 5;
	int32 ^resultptr = &result;
	while i != 5 {
		*resultptr = *resultptr * 2;
		i = i + 1;
	}

	return result;
}
