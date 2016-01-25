putchar :: (ch: u8) -> i32
puts :: (ch: u8^) -> i32

salutations :: () -> void
some_constant :: 50;
some_mutable_value: u8 = 39;

main :: () -> i32 {
	salutations()
	some_mutable_value = some_mutable_value + 1;
	putchar(some_constant + some_mutable_value);
	putchar(10);

	return 0
}

salutations :: () -> void {
	message: u8[0b101]
	message[0] = 72
	message[1] = 0x65
	message[2] = 0o171
	message[3] = 0b100001
	message[4] = 0

	for i: auto = 0, i < 5, i = i + 1 {
		puts(&message)
	}
	
	return
}
