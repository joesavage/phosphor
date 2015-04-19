fn putchar(int32 ch) -> int32;

fn main() -> int32 {
	int32 a = +(5 + 2);
	int32 ^b = &a;
	int32 ^^c = &b;

	return *(*c);
}
