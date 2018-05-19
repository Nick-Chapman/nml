
structure Test = 
struct
	structure Foo = struct datatype foo = ref end : sig type foo val ref : foo end
	open Foo
	val ref = 42
	fun result() = ref
end;
EXPECT (fn()=> Test.result() = 42)

