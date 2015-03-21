fn putchar(int32 ch) -> int32;

fn main() -> int32 {
	int64 a = 2; // Implicit sext from 32-bit signed (def. literal type for now)
	int16 b = (int16)a; // Explicit cast to truncate int64 to int16
	int32 c = b; // Implicit sext from int16 to int32

	return c + 5;
}
