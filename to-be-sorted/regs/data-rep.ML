
structure Test = 
struct

structure Inner =
struct
datatype t1 = c1 | c2
end

datatype t2 = datatype Inner.t1

fun f (c1) = 1
fun res() = f c1

val X = EXPECT (fn()=> res() = 1)

end
