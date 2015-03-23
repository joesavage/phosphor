fn putchar(int32 ch) -> int32;

fn main() -> int32 {
	uint16 a = 5;
	int32 b = a;
	float32 c = b;
	float64 d = c;
	uint16 e = (uint8)d;

	return e;
}
