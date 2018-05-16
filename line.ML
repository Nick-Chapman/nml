fun increment x = x+1
fun square y = y * y
fun compose (f,g) = fn x => f (g x)
fun foo b = compose (increment,square) b
