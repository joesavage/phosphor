fn putchar(int32 ch) -> int32;
fn puts(uint8^ ch) -> int32;

fn main() -> int32 {
	let original_message = "Hej! Hur mår du?\n";
	uint8[5]^ message = original_message;

	putchar((*message)[10]);
	putchar((*message)[11]);
	putchar((*message)[17]);
	
	int32 i = 0;
	while i < 18 {
		putchar((*message)[i]);
		i = i + 1;
	}

	return 0;
}
