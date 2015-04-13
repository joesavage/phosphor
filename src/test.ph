fn putchar(int32 ch) -> int32;

fn main() -> int32 {
	let a = 53;
	while a != 49 {
		putchar(a);
		a = a - 1;
	}

	return 0;
}
