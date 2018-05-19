
structure Test = 
struct

datatype colour = red | blue (* red has status C *)

fun f (red,999) = 2 | f (blue,_) = 1
fun res() = f (blue,999)


end; EXPECT(fn()=> Test.res() = 1)
