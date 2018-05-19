
val T = fn (b,e) => if b then (5,6) else (raise (e 1), raise (e 2))

val res = fn()=>
let exception E of int
	val r = ref [9]
	fun e n = (r := n :: !r ; E n)
	val y = T (false,e) handle E n => (n,42)
in (y,!r)
end; EXPECT(fn()=> res() = ((1,42),[1,9]))
