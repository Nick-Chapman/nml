
let fun gen() =
		let exception MyE of int
		in (MyE, fn MyE x => Some x | _ => None)
		end
	val (e1,f1) = gen()
	val (e2,f2) = gen()
in EXPECT (fn()=> (f1 (e1 101), f1 (e2 102), f2 (e1 103), f2 (e2 104)) = (Some 101,None,None,Some 104))
end
