
structure Test = 
struct

datatype colour = red | blue (* red has status C *)

structure MakeValStatus : sig type myCol val red : myCol end =
struct datatype myCol = red
end
open MakeValStatus; (* red has status V *)

datatype colReDef = datatype colour  (* red has status C again *)

fun f (red,999) = 2 | f (blue,_) = 1
fun res() = f (blue,999)


end; EXPECT(fn()=> Test.res() = 1)
