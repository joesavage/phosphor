fn putchar(int32 ch) -> int32;

fn main() -> int32 {
	int32 i = 5;
	int32 result = 5;
	int32 ^resultptr = &result;
	while i > 0 {
		*resultptr = *resultptr * 2;
		i = i - 1;
	}

	return *(resultptr + 1);
}
