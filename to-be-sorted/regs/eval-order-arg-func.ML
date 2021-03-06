
(* compare with: eval-order-func-arg.ML *)
(* this version tests for NON-standard, but more sensible eval-order *)
let
  fun ! (ref x) = x
  local fun revOnto acc [] = acc
		  | revOnto acc (x::xs) = revOnto (x::acc) xs
  in fun rev xs = revOnto [] xs
  end
  val X = ref [] : int list ref
  fun push x = (X := (x::(!X)); x)
in
EXPECT(fn()=>(
	push ((push 100; fn x => (push 200; fn y => (push 300; x+y))) (push 1) (push 2));
	(rev (!X) = [2,1,100,200,300,3])
))
end
