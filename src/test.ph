fn putchar(int32 ch) -> int32;

fn main() -> int32 {
	int32 result;
	result = 0;

	while (result != 6) {
		putchar(result + 65);
		result = result + 1;
	}
	putchar(10);

	return 0;
}
