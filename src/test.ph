fn putchar(int32 ch) -> int32;

fn main() -> int32 {
	int16 a = 2;

	int16 truncate = putchar(a);
	int32 ret = truncate;

	if (ret != 2) {
		return 0;
	}

	return ret;
}
