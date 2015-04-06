fn putchar(uint32 ch) -> int32;

fn main() -> uint32 {
	/*
		NOTE: Try changing the type of 'a' to a 'uint8' - it requires ridiculous
		casts to get the code to work. Need to re-think int literal types (sign,
		size, etc.) and casting rules to make this less crazy.
	*/
	let a = 53;
	while a != 49 {
		putchar(a);
		a = a - 1;
	}

	return 0;
}
