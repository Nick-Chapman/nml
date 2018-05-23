fun U_even (even,odd) n = if n=0 then true else if n>5 then even (n-2) else odd (n-1)
fun U_odd (even,odd) n = if n=0 then false else if n>5 then odd (n-2) else even (n-1)
fun t_even n = U_even (t_even,t_odd) n
and t_odd n = U_odd (t_even,t_odd) n
fun even_input [arg] = 
	print ("even("^arg^")->"^ (case (Prel.readInt arg) of Some n => (if (t_even n) then "true" else "false") ^ "\n"))
