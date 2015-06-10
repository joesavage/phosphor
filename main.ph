fn putchar(int32 ch) -> int32;
fn puts(uint8^ ch) -> int32;

fn main() -> int32 {
	// TODO: Make escape sequences work.
	let message = "Hej! Hur mår du?\n";
	putchar((*message)[10]);
	putchar((*message)[11]);
	putchar(10);
	puts((uint8^)*message);

	return 0;
}
