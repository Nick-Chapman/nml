
EXPECT
(fn () =>
	let
		val e1 = ref () = ref ()
		val e2 = let val R = ref () in R = R end
		val e3 = ref (fn x => x+1) = ref (fn x => x+1)
		val e4 = let val R = ref (fn x => x+1) in R = R end
	in
		(e1,e2,e3,e4) = (false,true,false,true)
	end)
