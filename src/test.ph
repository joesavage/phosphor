// This is a single-line comment
/* 
	This is a multi-line comment -
	it can span multiple lines.
*/
fn puts(int ch) -> float;

fn main() -> void {
	int a;
	a = 5;
	{
		int a;
		a = 2;
		return a + 12 * 2;

		// print(5);

		int b;
		b = 30;
		return b / 10;
	}

	return a;
}
