
(*fun implode cs = let fun loop acc [] = acc | loop acc (c::cs) = loop (acc ^ Char.toString c) cs in loop "" cs end*)

fun myProd [] = 1 | myProd (x::xs) = x * myProd xs
(*fun fact5() = print ("fact(5)=" ^ Prel.stringOfInt (myProd [2,3,4,5]) ^ "\n")*)
(*val fact5 = let val five = 5 in fn () => print ("fact(5)=" ^ Prel.stringOfInt (myProd [2,3,4,five]) ^ "\n") end*)
fun fact n = myProd (Prel.upto (2,n))

fun fact_input [arg] = 
	print ("fact("^arg^")->"^ (case (Prel.readInt arg) of Some n => Prel.stringOfInt (fact n) ^ "\n"))
			  

fun fact_input_top args =
	(fact_input args) handle _ => print ("exception at top level")

