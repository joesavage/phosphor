putchar :: (ch: u8) -> i32
puts :: (ch: u8^) -> i32

salutations :: () -> void
array_initialization_works_now :: [255, 2, -9, 4]
some_constant :: 0x5A

main :: () -> i32 {
	salutations()

	putchar(some_constant);
	putchar(10);

	return 0
}

salutations :: () -> void {
	message: auto = [72, 0x65, 0o171, 0b100001, 0]
	for i: auto = 0, i < 5, i = i + 1 {
		puts(&message)
	}

	return
}
