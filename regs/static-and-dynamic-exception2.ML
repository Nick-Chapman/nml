
(* Using with patten matching against combination of static/dynamic exceptions (E/G) *)

local exception S of int
in val s = S
   fun gen() = let exception C of int
				   exception D of int (* this version has two diff generated exceptions *)
			   in (C, D, fn C 1 => 11 | S 2 => 22 | D 3 => 33 | _ => 0) (* and we test in order: dyn/stat/dyn *)
			   end
end
val (a,b,f) = gen()
val X = map f [s 1, s 2, s 3, a 1, a 2, a 3, b 1, b 2, b 3]

