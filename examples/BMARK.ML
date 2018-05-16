signature BMARK = 
sig 
	type generation
	val show : (string -> unit) -> generation -> unit
	val gun : generation 
	val nthgen : generation -> int -> generation 
end;
