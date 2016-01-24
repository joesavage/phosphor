putchar :: (ch: i32) -> i32
puts :: (ch: u8^) -> i32

main :: () -> i32 {
	message: u8[0b101];
	message[0] = 72;
	message[1] = 0x65;
	message[2] = 0o171;
	message[3] = 0b100001;
	message[4] = 0;

	constants_work_now :: 0x5A;
	putchar(constants_work_now);
	putchar(10);

	i: i32 = 0;
	for i < 5 {
		puts(&message);
		i = i + 1;
	}

	return 0;
}
