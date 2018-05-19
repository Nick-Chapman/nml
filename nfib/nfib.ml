
fun nfib 0 = 1 
  | nfib 1 = 1 
  | nfib n = 1 + nfib (n-1) + nfib (n-2)

fun nfib_top [arg] = 
	print ("nfib("^arg^")->"^ (case (Prel.readInt arg) of Some n => Prel.stringOfInt (nfib n)) ^ "\n")
