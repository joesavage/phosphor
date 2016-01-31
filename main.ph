putchar :: (ch: u8) -> i32
puts :: (ch: u8^) -> i32

salutations :: () -> void

main :: () -> i32 {
	salutations()

	some_words :: [ "This is a test.", "Specifically, of string arrays." ]
	for i: auto = 0, i < 2, i = i + 1 {
		puts(some_words[i])
	}

	return 0
}

salutations :: () -> void {
	message: auto = [72, 0x65, 0o171, 0b100001, 0]
	puts(&message)

	return
}
