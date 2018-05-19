let
	fun ! (ref v) = v
	fun pow(x,n) = 
		let val i = ref n
			val acc = ref 1;
		in while (!i>0) do (acc := x * !acc; i:= !i-1) ; !acc
		end
in EXPECT(fn()=> pow(3,3) = 27 andalso pow(2,10) = 1024)
end
