fn putchar(int32 ch) -> int32;
fn puts(uint8^ ch) -> int32;

fn main() -> int32 {
	let original_message = "Hej! Hur mår du?\n";
	uint8[5]^ message = original_message;

	int32 i = 1;
	while i <= 10 {
		putchar((*message)[10]);
		putchar((*message)[11]);
		i += 1;
	}
	putchar((*message)[17]);
	
	i = 0;
	while i < 18 {
		putchar((*message)[i]);
		i = i + 1;
	}

	return 0;
}
