fn putchar(int32 ch) -> int32;

fn print_hello() -> void {
	putchar(72);  // H
	putchar(101); // e
	putchar(108); // l
	putchar(108); // l
	putchar(111); // o
	putchar(44);  // ,
	putchar(32);  //  
	putchar(87);  // W
	putchar(111); // o
	putchar(114); // r
	putchar(108); // l
	putchar(100); // d
	putchar(33);  // !
	putchar(10);  // \n
	return;
}

fn main() -> int32 {
	print_hello();

	return 0;
}
