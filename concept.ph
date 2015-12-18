/*
 * Phosphor is a compiled statically-typed programming language.
 * This concept file outlines the direction the language is heading in.
 * 
 * NOTES:
 *   - Semicolons are mostly optional.
 *   - Operator overloading exists, I'm just not sure about the syntax yet.
 *   - 'struct' and 'union' packing keywords should be available.
 *   - Natural size types (like C's) might be useful, but I'm unsure.
 *     - Are default 'i32' etc. minimum-size or exact-size? Exact-size can be useful in structs, but minimum-size seems more useful for the general case.
 *   - Named parameters and named struct member initialization (and assignment?) would be nice.
 *   - Some good mechanisms for creating & handling parallelism would be nice, but I don't want to think about them quite yet.
 *   - Some kind of variable immutability might be a good idea, but I have mixed feelings about how to implement this.
 *   - An easy way to switch from AoS to SoA would be great, but I'm unsure of the specifics. I don't want to think about it right now.
 *     - The compiler could implement this for static arrays (arrays of structs are secretly structs of arrays under the hood).
 *       - e.g. arr[5].item -> arr.item[5]
 *     - With powerful compile-time introspective branching and '.' overload ability (MOP-style), STL types could implement this without worry. Unsure if this is a good solution though.
 */

#import "filename.ph"

// The 'main' function is the entry point, like in C. Note that functions can be
// defined after they are used without worry.
main :: (argc: i32, argv: &u8[]) -> i32 {
	with std; // Brings the namespace 'std' into the local scope.

	// UTF-8 strings! Also, string interpolation!
	some_string: auto = "Hello, World! (你好世界)\n";
	println("\"#{some_string}\", said the man.");

	// Variables can explicitly be declared as uninitialized using '---'.
	circle: Circle = ---;
	circle.pos.x = circle.pos.y = 42;
	circle.radius = 5;
	for i: auto = 0, i < 100, ++i {
		print_area(circle);
		println("#{cast<f32>(i)}");
	}

	// '_' is the blank identifier (for discarding values or marking sections as
	// unused), like in Go.
	for _, i < 100, ++i { print_area(circle); }

	// 'while' and 'for' loops are unified, like in Go.
	for condition { println("!"); }

	// This results in an error - conditions cannot merely be numbers, and must
	// instead be of a suitable type (e.g. bools).
	for 1 { println("!"); }

	// You can 'bake' specialised versions of a function - such as with particular
	// constants, or particular types (e.g. return type - no need to specialise
	// the parameter types explicitly mostly as they're inferred from the params).
	// Throughout the language, triangular brackets indicate constraints.
	add_returning_u64 :: add<T: u64>(i32, i32);
	add_variation :: add<A: i32>(i32, i32);
	add_one :: add(1, i32);

	// You can bake specialised structs too!
	PointPair :: Pair<T: Point>;
	point_pair: PointPair;

	// Structs have a default '==' operator function overload, which can be
	// overridden by a more specifically typed version.
	println(point_pair == point_pair);

	// Implicit contexts can get invisibly passed around to functions, with data
	// such as which allocator to use, debug info, etc. Obviously, this isn't
	// going to interact properly with precompiled C code.
	context: Context;
	context.allocator = some_allocator;
	context.allocator_data = some_data;
	context.logger = some_logger;
	{
		push_context context;
		defer release(some_allocator);
		ptr: &i32 = alloc(sizeof(ptr));
		*ptr = 10;
	}

	{ ... } // Anonymous Block
	[capture] { ... } // Captured Anonymous Block
	(i: i32) -> f32 [capture] { ... } // Anonymous Function
	f: auto = (i: i32) -> f32 [capture] { ... } // Named (Local/Global) Function
	g :: (i: i32) -> f32 [capture] { ... } // Const Named (Local/Global) Function

	// This results in an error, as local functions can't be overloaded in the
	// same way that global ones can.
	g :: (i: f32) -> f32 [capture] { ... }

	// This results in an error, as 'g' takes a parameter of type 'i32', and
	// floats don't coerce to integer types in this language.
	g(5.0);

	// Function calls with structs that use 'with' (as a sort of inheritance) work
	// as you might expect, and provide an alternative to 'methods' via functions
	// and overloading. Note that if multiple 'paths' through 'with' can result in
	// multiple possibly applicable functions, this conflict results in an error.
	println("#{add_points(circle.location, circle.location)}");
	println("#{add_points(circle, circle)}");

	// 'with' can bring members through pointers into scope too (auto deref)!
	ptr: &i32 = &some_struct;
	{
		with ptr;
		println("#{struct_member_one_accessed_through_the_ptr}");
		println("#{struct_member_one_accessed_through_the_ptr}");
	}

	// Compile-time execution. Note that the '#' symbol denotes compile-time
	// operations throughout the language.
	#run {
		println(some_func(5, some_func(1, 1)));
	}

	// Nim-style metaprogramming through templates and macros.
	#template some_template :: (some_param: expr) {
		// On #run, this exact code gets produced (with 'some_param' replaced).
		some_param :: () { println(astToStr(some_param)) }
	}
	#macro some_macro :: (some_param: i32) {
		// This hygenic syntactic macro returns an AST structure.
		// <insert macro code here>
	}
	#run some_template
	#run some_macro

	// Inline assembly is useful, and so is supported.
	asm {
		xor eax, eax
	}

	// 'match' statements are exhaustive, like in Rust.
	item: i32;
	match item {
		case 1 => println("A"),
		case 2 => println("B"),
		case _ => println("C")
	}

	// 'match' statements return a value, and so can be used in assignments.
	result: f32 = match item {
		case 1 => 42.0,
		case 2 => 43.0,
		case _ => 00.0
	};

	// Tagged unions can only have their values changed by assignment to something
	// completely new (e.g. in construction), or within a 'match'/'if'/conditional
	// (when it's definitely safe to modify them). If you want to call a function
	// inside one of the 'match' cases that can access everything, the parameter
	// must be of the TaggedUnion.SpecificType type, and must be called within the
	// 'case'. Note that the construction syntax here is very much still to be
	// determined and is not final (named member construction would be better).
	object: auto = TaggedUnion.SecondType(some_circle);

	// This is allowed in this case as the specialised type is known. It would not
	// be allowed if it was passed into another function accepting a TaggedUnion
	// (non-specialised) parameter.
	object.circle.x = 5;

	// 'match' matches (exhaustively) on tagged union types automatically.
	match object {
		case FirstType => {
			// After the match, a more local version with a specialised type shadows
			// any wider copies, so this is always OK (even if 'object' is only of the
			// TaggedUnion type).
			object.number = 5;
		}
		case with SecondType => { // 'with' works nicely here.
			circle.x = 5;
		}
	}


	// Generic functions (polymorphic procedures) are called with dynamic dispatch
	// for protocol types (or, indeed, static dispatch when it can be inferred).
	// As it's through functions, not methods, this supports multiple dispatch.
	something: HasArea = circle;
	println(area(something));

	// You can match on the specific types of interface types too!
	match something {
		case _ => println("!")
	}

	return 0;
}

std :: namespace {
	PI :: 3.14159;

	// Protocols are like interfaces and traits - I'm not sure if they should be
	// able to provide default functions yet, so I don't know which. As methods
	// don't exist in the language, protocols provide a contract for what function
	// overloads should be defined for a particular type.
	HasArea :: protocol {
		with SomeOtherProtocol; // 'with' works in protocols too!

		// The 'Self' type is the specific type implementing this protocol.
		area :: (self: &Self) -> f64;
	}

	// '$' denotes ownership and creation of a generic type. Constraints to this
	// type may be specified in triangular brackets (in this case, a protocol
	// constraint has been applied). Functions with generic type parameters go
	// through monomorphisation, and are statically dispatched where possible.
	print_area :: (shape: $T<HasArea>) {
		// Function calls can be forced as 'inline' from the call site.
		println("This shape has area #{inline area(shape)}");
	}

	// Structs can make use of generic types too.
	Pair :: struct { car: $T, cdr: T }

	// Struct members can have default initialization.
	Point :: struct {
		x: f64 = 0,
		y: f64 = 0
	}

	Circle :: struct {
		// 'with' can be used as a sort of inheritance for structs. Naming conflicts
		// with data members result in errors, as no overriding is performed.
		with location: Point;
		name: String,
		radius: f32
	}

	// As the language doesn't have methods, implementing a protocol for a given
	// type is simply specifying a bunch of function overloads (all of which must
	// be implemented for the type to abide to the protocol).
	impl HasArea for Circle {
		area :: (with self: &Circle) -> f64 {
			return PI * (radius * radius);
		}
	}

	impl HasArea for Pair<T: Circle> {
		area :: (with self: &Pair<T: Circle>) -> f64 {
			return area(car) + area(cdr);
		}
	}

	add :: (a: i32, b: i32) -> $T {
		return cast<$A>(a + b);
	}

	add_points :: inline (a: Point, b: Point) {
		return a + b;
	}

	// 'internal' functions aren't visible outside of their namespaces - or when
	// used in filescope, aren't visible in the compiled file (like C's 'static').
	some_helper_function :: internal (i32 a) -> i32 {
		return a + 1;
	}

	// 'b' is an autobaked function parameter (must be a constant or compile-time
	// expression), so the compiler will generate specialised versions of the
	// function for different values of 'b'.
	some_func :: (a: i32, b: #i32) {
		return a + b;
	}

	// If you want a constructor, just make a specialised function for your type.
	create :: () -> $T;
	create :: () -> $T<Circle> {
		result: Circle;
		result.name = "Hello":
		result.radius = 5;
		result.x = 0;
		result.y = 0;
		return result;
	}

	SomeEnum :: bitmask enum {
		First,
		Second,
		Third
	}

	SomeUnion :: union {
		integer: i32,
		floating_point: f32
	}

	TaggedUnion :: enum {
		FirstType => number: i32,
		SecondType => circle: Circle
	}

	Optional :: enum {
		None => _,
		Some => data: i32
	}
}

// You can add things to namespaces after they've been created too.
std :: namespace {
	TAU :: PI * 2;
}
