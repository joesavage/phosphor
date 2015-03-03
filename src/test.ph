fn putchar(int32 ch) -> int32;

fn fib(int32 n) -> int32 {
	if (n == 0) {
		return 0;
	} else if (n == 1) {
		return 1;
	}

	return fib(n - 1) + fib(n - 2);
}

fn main() -> int32 {
	return fib(8);
}
