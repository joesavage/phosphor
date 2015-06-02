fn putchar(int32 ch) -> int32;
fn puts(uint8^ ch) -> int32;

fn main() -> int32 {
	let message = "Hej! Hur mår du?\n"; // TODO: Make escape sequences work
	putchar((*message)[3]);
	putchar(10);
	puts((uint8^)*message);
	puts(message); // TODO: Fix implicit casting rules so this doesn't compile.

	return 0;
}
