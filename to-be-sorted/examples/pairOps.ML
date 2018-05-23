
val mkPairOps1 = fn () =>
	let val id   = (fn (a,b) => (a,b)) : 'a * 'b -> 'a * 'b
		val swap = (fn (a,b) => (b,a)) : 'a * 'b -> 'b * 'a
	in {id=id,swap=swap}
	end


val 'a mkPairOps2 = fn () =>
	let val id   = (fn (a,b) => (a,b)) : 'a * 'b -> 'a * 'b
		val swap = (fn (a,b) => (b,a)) : 'a * 'b -> 'b * 'a
	in {id=id,swap=swap}
	end

val ('a,'b) mkPairOps3 = fn () =>
	let val id   = (fn (a,b) => (a,b)) : 'a * 'b -> 'a * 'b
		val swap = (fn (a,b) => (b,a)) : 'a * 'b -> 'b * 'a
	in {id=id,swap=swap}
	end
