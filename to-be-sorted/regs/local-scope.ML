
EXPECT(fn()=>
let
	val A = 1
	local val A = 2
	in val B = 10 * A
	end
	val C = A + B
in
	C = 21
end)