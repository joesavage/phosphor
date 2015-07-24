fn putchar(int32 ch) -> int32;
fn puts(uint8^ ch) -> int32;

fn main() -> int32 {
	uint8[5] message;
	message[0] = 72;
	message[1] = 101;
	message[2] = 121;
	message[3] = 33;
	message[4] = 0;

	int32 i = 1;
	while i <= 3 {
		puts((uint8^)&message);
		i = i + 1;
	}

	return 0;
}
