putchar :: (ch: int32) -> int32;
puts :: (ch: uint8^) -> int32;

main :: () -> int32 {
	message: uint8[0b101];
	message[0] = 72;
	message[1] = 0x65;
	message[2] = 0o171;
	message[3] = 0b100001;
	message[4] = 0;

	i: int32 = 0;
	while i < 5 {
		puts((uint8^)&message);
		i = i + 1;
	}

	return 0;
}
