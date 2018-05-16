
(* Using with patten matching against combination of static/dynamic exceptions (E/G) *)

local exception S of int
in val s = S
   fun gen() = let exception D of int
				   exception C = D (* either NJ is buggy, or I dont understand what this should mean! *)
			   in (D, fn S 1 => 11 | D 2 => 22 | C 3 => 33 | _ => 0)
			   end
end
val (a,f) = gen()
val (b,g) = gen()
val X = map f [s 1, s 2, s 3, a 1, a 2, a 3, b 1, b 2, b 3]
val Y = map g [s 1, s 2, s 3, a 1, a 2, a 3, b 1, b 2, b 3]

