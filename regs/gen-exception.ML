
let fun gen() =
		let exception MyE
		in (MyE, fn MyE => true | _ => false)
		end
	val (e1,f1) = gen()
	val (e2,f2) = gen()
in EXPECT (fn()=> (f1 e1, f1 e2, f2 e1, f2 e2) = (true,false,false,true))
end
