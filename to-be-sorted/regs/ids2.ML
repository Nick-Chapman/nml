
structure Test = 
struct

datatype colour = red | blue (* red has status C *)

structure MakeValStatus : sig val red : colour end =
struct datatype myCol = datatype colour
end
open MakeValStatus; (* red has status V *)

fun f (red,999) = 2 | f (blue,_) = 1
fun res() = f (blue,999)

end; EXPECT(fn()=> Test.res() = 2)
