
structure Test = 
struct

structure MakeValStatus : sig type blah val true : blah end =
struct datatype blah = true
end
open MakeValStatus; (* true has status V *)

datatype redDefBool = datatype bool (* true has status C *)

fun f (true,999) = 2 | f (false,_) = 1
fun res() = f (false,999)

val X = EXPECT(fn() => res() = 1)

end
