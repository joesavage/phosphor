fn putchar(int32 ch) -> int32;

fn main() -> int32 {
	int32 a;
	int64 b;
	int16 c;

	a = 10;
	b = 5;
	c = 2;

	int16 truncate;
	truncate = putchar(c);
	int32 ret;
	ret = truncate;

	if (ret != 2) {
		return 0;
	}

	return ret;
}
