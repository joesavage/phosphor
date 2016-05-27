/*
 * Phosphor is a compiled statically-typed programming language.
 * This concept file outlines the direction the language is heading in.
 * 
 * Additional notes and extra tidbits (in no particular order):
 *   - All syntax (and, indeed, semantics) in this file is hugely non-final!
 *   - Eventually, I want the compiler to have really great warnings and errors (see: Elm's amazing errors)
 *     - I would also like to make it really easy to offer similar behaviour to clang's sanitizers
 *   - Built-in types like 'Int' may abide to certain protocols (similar to Ord, Num, etc. in Haskell)
 *   - Non-type struct specialisation/variations may be supported, e.g. a struct which takes an array length parameter
 *     - We should also have some way to query 'struct parameters' in general
 *       - e.g. add_to_hashmap :: (table: $T<HashMap>, key: T.key_type, value: T.value_type)
 *     - This could be in the form of general compile-time struct parameters (making structs a bit like functions/macros)
 *       - It feels more natural to use $ and <> to me though
 *       - Perhaps this can work its way into the constraint syntax somehow. e.g. Vector :: struct { arr: [N] Float32 } where N : Int64 // Vector<N: 10> vec
 *   - "First-class" types should be supported in some form as a purely compile-time construct (no runtime madness, no synthesis, just passing around types)
 *     - e.g. compile-time macros that create objects of a provided type for you and do something with them, or to automate specific cast patterns
 *   - Some convenient way of creating enums with specific value patterns (e.g. bitmasks) should be supported, e.g. via macros or similar to Go's 'iota'
 *   - A mechanism to switch between AoS to SoA will be supported (very common, yet a huge pain to deal with in C++)
 *     - The compiler could implement this for static arrays (AoS are secretly SoA under the hood), e.g. arr[5].item -> arr.item[5]
 *     - If there's built-in support for dynamic arrays too (with custom allocator support for flexibility), this could be handled in the dynamic case also.
 *       - This seems like the 'right' solution, and would make it really easy to do the right thing.
 *   - Operator overloading is a feature, I'm just not sure about the syntax yet
 *     - It might be nice to be able to define operators with non-strict semantics too (just like && and || are)
 *     - It might also be nice to be able to define statement operators
 *       - This would restore uniformity between built-in constructs like 'until' and STL constructs like 'repeat'
 *   - Multiple return values are supported
 *     - Particularly, I'm really fond of the pattern of returning a 'cleanup' lamda/closure which can be defer'd to free allocated memory
 *     - Named return values might be convenient (perhaps functions can return implicitly defined structs or something)
 *   - Named struct member initialization (and potentially assignment) will be present, but I'm unsure of the syntax
 *     - Similarly, named parameters may be useful.
 *   - Further introspection features will be supported
 *   - The language will have some good constructs for concurrency and parallism at some point too (inspired by Go, Rust, OpenCL, CUDA?)
 *     - In a similar light, it would be nice to have a better abstraction for SIMD than basic intrinsics
 *   - Is protocol inheritance (practically: 'with' flattening) something we want in the language? Potentially, I'm not sure right now
 *   - Strict aliasing is annoying, but useful for optimisations. I want to try and do something about that.
 *     - In general, we want to make the optimisers life easier
 *       - This is why, for example, iterators are supported to allow optimising numeric loops w/o exploiting signed overflow
 *         - e.g. "for(int i = A; i <= B; i += 4)" may be infinite if integer overflow is defined, which is clearly problematic
 *       - In general, maybe the language constructs should be more constrained for this same purpose (we want to make things easy for the optimiser)
 *   - Function boundaries to pre-compiled code are going to be a bit of a pain, tooling infrastructure changes may be required for things to work smoothly
 *     - The fact that optimisation is so terrible over function boundaries in C is really bad, so we should try to fix that.
 *     - Part of this is that we want contexts (which may include allocator information, etc.) to be essentially 'free' in as many cases as possible
 *       - Particularly in the case where users aren't using contexts but they're still getting passed around!
 *     - The side-effects of functions in pre-compiled code should be clear too, so the calling program doesn't have to assume all data has changed
 *       - Alternatively, we take the compilation performance hit in compiling the appropriate STL functions with a source program (inlining benefits, etc.)
 *   - Nested structs and struct-local constants should probably be supported, interacting properly with polymorphic structs (in creating a parent, give any type dependencies)
 *   - Using optionals rather than a NULL value seems a sensible decision.
 *   - Literals for both UTF-8 strings and ASCII byte strings should probably be present (e.g. b"string" or something)
 *   - Natural size types (similar to C's) might be useful, but I'm unsure
 *     - Similarly, I don't know if minimum-size or exact-size are better for default numeric types
 *     - Exact-size can be useful in structs, but minimum-size seems more useful for the general case.
 *   - Basic 'struct' and 'union' packing keywords will be supported
 *   - For bit-level manipulation, perhaps a bitwise addressing construct could be useful (and cleaner than masking, unions, etc. in C)
 *   - Useful datatypes (inc. hashmap) should be baked into the language or STL for easy, efficient usage. I'm not sure which datatypes belong where though.
 */

@import "filename.ph"

// The 'main' function is the entry point, like in C.
// Note that functions can be defined after they are used without worry.
main :: (argc: Int32, argv: &UInt8[]) -> Int32 {
	with std // Brings the namespace 'std' into the local scope.

	// Type inference, UTF-8 strings, string interpolation.
	some_string: Auto = "Hello, World! (你好世界)\n"
	println("\"@{some_string}\", said the man.")

	// Variables can explicitly be declared as uninitialized using '???'.
	circle: Circle = ???
	circle.pos.x = circle.pos.y = 42
	circle.radius = 5
	for i in 0..99 {
		print_area(circle)
		println("@{cast<Float32>(i)}")
	}

	// Iterators are supported for structures that implement the 'Enumerable'
	// trait providing some number of function overloads to visit items in a
	// collection.
	// Note: general purpose efficient iterators are hard, so we'll probably allow
	// some specialisation in these (see: nothings.org/computer/iterate.html).
	for circle in someEnumerableObject {
		print_area(circle)
	}

	if condition {
		println("These work as you might expect.")
	} else {
		println("Nothing particularly interesting to see here.")
	}

	while condition {
		println("!")
	}

	// This results in an error. Conditions cannot merely be numbers, and must
	// instead be of a suitable type (e.g. Bool).
	while 1 {
		println("!")
	}

	// Additional expressive branching and looping constructs are supported.
	unless condition {
		println("This is like an 'if' statement, but negates the condition")
	} else {
		println("It too works as you might expect.")
	}

	until condition {
		println("!")
	}

	// You can 'bake' specialised versions of a function, such as with particular
	// constants, or particular types (e.g. return type -- no need to specialise
	// the parameter types explicitly mostly as they're inferred from the params).
	// Throughout the language, triangular brackets indicate constraints.
	one_add_variation :: add<T: UInt64>(Int32, Int32)
	another_add_variation :: add<A: Int32>(Int32, Int32)
	add_one :: add(1, Int32) // This behaves kind of like partial application (though must use values known at compile-time)

	// You can bake specialised structs too!
	PointPair :: Pair<T: Point>
	point_pair: PointPair

	// Structs have a default '==' operator function overload, which can be
	// overridden by a more specifically typed version.
	println(point_pair == point_pair)

	// Implicit contexts can get invisibly passed around to functions, with data
	// such as which allocator to use, debug info, etc. Obviously, this isn't
	// going to interact properly with precompiled C code.
	context: Context
	context.allocator = some_allocator
	context.allocator_data = some_data
	context.logger = some_logger
	{
		push_context context
		defer release(some_allocator) // Defer until end of scope (note: multiple defer statements apply 'backwards')
		ptr: &Int32 = alloc(sizeof(ptr))
		*ptr = 10
	}

	{ ... } // Anonymous Block
	[capture] { ... } // Captured Anonymous Block
	(i: Int32) -> Float32 [capture] { ... } // Anonymous Closure
	f: Auto = (i: Int32) -> Float32 [capture] { ... } // Named (Local/Global) Closure
	g :: (i: Int32) -> Float32 [capture] { ... } // Const Named (Local/Global) Closure

	// This results in an error, as local closures can't be overloaded in the
	// same way that global ones can.
	g :: (i: Float32) -> Float32 [capture] { ... }

	// This results in an error, as 'g' takes a parameter of type 'Int32', and
	// floats don't coerce to integer types in this language.
	g(5.0)

	// Function calls with structs that use 'with' (as a sort of inheritance) work
	// as you might expect, and provide an alternative to 'methods' via functions
	// and overloading. Note that if multiple 'paths' through 'with' can result in
	// multiple possibly applicable functions, this conflict results in an error.
	println("@{add_points(circle.location, circle.location)}")
	println("@{add_points(circle, circle)}")

	// 'with' can bring members through pointers into scope too (auto deref)!
	ptr: &Int32 = &some_struct
	{
		with ptr
		println("@{struct_member_one_accessed_through_the_ptr}")
		println("@{struct_member_one_accessed_through_the_ptr}")
	}

	// Compile-time execution. Note that the '@' symbol denotes compile-time
	// operations throughout the language.
	@run {
		println(some_func(5, some_func(1, 1)))
	}

	// Nim-style metaprogramming through templates and macros.
	@template some_template :: (some_param: expr) -> void {
		// On @run, this exact code gets produced (with 'some_param' replaced)
		some_param :: () { println(ast_to_str(some_param)) }
	}
	@macro some_macro :: (some_param: Int32) -> void {
		// This hygenic syntactic macro returns an AST structure.
		// <insert macro code here>
	}
	@run some_template
	@run some_macro

	// Inline assembly is useful, and so is supported.
	asm {
		xor eax, eax
	}

	// 'match' statements are exhaustive, like in Rust.
	item: Int32
	match item {
		case 1 => println("A"),
		case 2 => println("B"),
		case _ => println("C") // Like in many languages, '_' is an anonymous variable / blank identifier.
	}

	// 'match' statements return a value, and so can be used in assignments.
	result: Float32 = match item {
		case 1 => 42.0,
		case 2 => 43.0,
		case _ => 00.0
	}

	// Tagged unions can only have their values changed by assignment to something
	// completely new (e.g. in construction), or within a 'match'/'if'/conditional
	// (when it's definitely safe to modify them). If you want to call a function
	// inside one of the 'match' cases that can access everything, the parameter
	// must be of the TaggedUnion.SpecificType type, and must be called within the
	// 'case'. Note that the construction syntax here is very much still to be
	// determined and is not final (named member construction would be better).
	object: Auto = TaggedUnion.SecondType(some_circle)

	// This is allowed in this case as the specialised type is known. It would not
	// be allowed if it was passed into another function accepting a TaggedUnion
	// (non-specialised) parameter.
	object.circle.x = 5

	// 'match' matches (exhaustively) on tagged union types automatically.
	match object {
		case FirstType => {
			// After the match, we know the more specialised type, so this is always
			// OK (even if 'object' was only declared with the TaggedUnion type).
			object.number = 5
		}
		case with SecondType => { // 'with' works nicely here.
			circle.x = 5
		}
	}


	// Procedures with parametric polymorphism are called with dynamic dispatch
	// for protocol types (or, indeed, static dispatch when it can be inferred).
	// As this is using the typical function call syntax, multiple dispatch works
	// as you would expect.
	something: HasArea = circle
	println(area(something))

	// You can match on the specific types of interface types, and the more
	// specific types of base objects (e.g. through 'with' "inheritance"), using
	// RTTI.
	match something.type {
		case _ => println("!")
	}

	return 0
}

std :: namespace {
	PI :: 3.14159

	// Protocols are like interfaces and traits - I'm not sure if they should be
	// able to provide default functions yet, so I don't know which. As methods
	// don't exist in the language, protocols provide a contract for what function
	// overloads should be defined for a particular type.
	HasArea :: protocol {
		with SomeOtherProtocol // 'with' works in protocols too!

		// The 'Self' type is the specific type implementing this protocol.
		area :: (self: &Self) -> Float64
	}

	// '$' denotes ownership and creation of a generic type. Constraints to this
	// type may be specified in triangular brackets (in this case, a protocol
	// constraint has been applied). Functions with generic type parameters go
	// through monomorphisation, and are statically dispatched where possible.
	// Polymorphic type constraints can also require that the parameter is a
	// version of some struct (e.g. is a Pair<T>).
	print_area :: (shape: $T<HasArea>) {
		// Function calls can be forced as 'inline' from the call site.
		println("This shape has area @{inline area(shape)}")
	}

	// Structs can make use of generic types too.
	Pair :: struct { car: $T, cdr: T }

	// Struct members can have default initialization.
	Point :: struct {
		x: Float64 = 0,
		y: Float64 = 0
	}

	Circle :: struct {
		// 'with' can be used as a sort of inheritance for structs. Naming conflicts
		// with data members result in errors, as no overriding is performed.
		with location: Point,
		name: String,
		radius: Float32
	}

	// As the language doesn't have methods, implementing a protocol for a given
	// type is simply specifying a bunch of function overloads (all of which must
	// be implemented for the type to abide to the protocol).
	// This whole thing is a little bit like the 'generic function' view of the
	// world for methods in, for example, Common Lisp.
	impl HasArea for Circle {
		area :: (with self: &Circle) -> Float64 {
			return PI * (radius * radius)
		}
	}

	// This works as you would expect with polymorphic types. Specialised versions
	// of functions can override more general ones too ("ad-hoc polymorphism").
	impl HasArea for Pair<T: Circle> {
		area :: (with self: &Pair<T: Circle>) -> Float64 {
			return area(car) + area(cdr)
		}
	}

	add :: (a: Int32, b: Int32) -> $T {
		return cast<$A>(a + b)
	}

	add_points :: inline (a: Point, b: Point) {
		return a + b
	}

	// 'internal' functions aren't visible outside of their namespaces - or when
	// used in filescope, aren't visible in the compiled file (like C's 'static').
	some_helper_function :: internal (Int32 a) -> Int32 {
		return a + 1
	}

	// 'b' is an autobaked function parameter, so the compiler will generate
	// specialised versions of the function for different values of 'b', or fall
	// back to a generic version if the value is not known at compile-time.
	// Stricter autobake functionality can use '@@', allowing only for constant or
	// compile-time expressions as parameters.
	some_func :: (a: Int32, b: @Int32) {
		return a + b
	}

	// If you want a constructor, just make a specialised function for your type.
	create :: () -> $T
	create :: () -> $T<Circle> {
		result: Circle
		result.name = "Hello"
		result.radius = 5
		result.x = 0
		result.y = 0
		return result
	}

	SomeUnion :: union {
		integer: Int32,
		floating_point: Float32
	}

	TaggedUnion :: enum {
		FirstType => number: Int32,
		SecondType => circle: Circle
	}

	Optional :: enum {
		None => _,
		Some => data: $T
	}
}

// You can add things to namespaces after they've been created too.
std :: namespace {
	TAU :: PI * 2

	// As we can autobake the function parameters here, we separate traversal logic using higher-order functions for free
	 do :: (function: @F, list: $T[]) -> void  where F : T -> void
	map :: (function: @F, list: $T[N]) -> T[N] where F : T -> T
}
