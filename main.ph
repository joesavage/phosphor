fn putchar(int32 ch) -> int32;
fn puts(uint8^ ch) -> int32;

fn main() -> int32 {
	uint8[0b101] message;
	message[0] = 72;
	message[1] = 0x65;
	message[2] = 0o171;
	message[3] = 0b100001;
	message[4] = 0;

	int32 i = 5;
	while i != 0 {
		puts((uint8^)&message);
		i = i - 1;
	};

	return 0;
};
