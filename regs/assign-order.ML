
val res = fn () =>
let val r = ref 1
in (!r, r:=2, !r) 
end; EXPECT(fn () => (res() = (1,(),2)))
