fn putchar(int32 ch) -> int32;
fn puts(uint8^ str) -> int32;

fn main() -> int32 {
	let a = "Hej! Hur mår du?";
	// puts(a); // TODO: We need to get the address of the first element? This might be more about syntax/logic rather than LLVM conversion magic
	return 0;
}
