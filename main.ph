fn putchar(int32 ch) -> int32;
fn puts(uint8^ ch) -> int32;

fn main() -> int32 {
	let message = "Hej! Hur mår du?\n";

	putchar((*message)[10]);
	putchar((*message)[11]);
	putchar((*message)[17]);
	
	int32 i = 0; // Handle number parsing for: 0b, 0x, 0o

	// TODO: Support '>=', '<=', and logical operators
	while i >= 0 && i < 18 {
		putchar((*message)[i]);
		i = i + 1;
	}

	return 0;
}
