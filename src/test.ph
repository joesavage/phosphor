// This is a single-line comment
/* 
	This is a multi-line comment -
	it can span multiple lines.
*/
fn putchar(int32 ch) -> int32;

fn main() -> void {
	int32 a;
	a = 5;

	{
		int32 a;
		a = 2;
		return a + 12 * 2;

		while (a) {
			return a;
		}

		int32 b;
		b = putchar(35); // 35: ASCII '#'
		return b + 5;
	}

	return a;
}
