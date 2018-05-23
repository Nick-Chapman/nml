fun subfrom x y = y-x
fun thrice f x = f (f (f x))
val thrice_top = fn _ => print ("thrice_top -> "^Prel.stringOfInt (thrice thrice (subfrom 1) 100) ^ "\n")
