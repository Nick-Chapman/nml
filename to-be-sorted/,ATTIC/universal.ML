
(*----------------------------------------------------------------------
INDEX: universal type 
----------------------------------------------------------------------*)

(* When conversion from Lang -> Embed
we also loose datatype dec & consequently the constructiors
If we want to map back from Embed->Lang, for purposes of writing to file & loading
into NJ, to check transformations are good,
(also , so can mao CPS->Embed->Lang to check cps transform is ok)
then we need to create somehing which can simulate the constructors (now just cid strings)
to be used in pat/exp. to do this we need to embed in a universal type. And preserve 
the type info in each constructor to allow mapping in/out of the universal type based
on the type of each constructor - this will create a type-slack version of the original program
ie, all datatypes become the same type, but it should still evaluate the same
*)

datatype u = Universal exn
exception INT of int
exception STRING of string
exception PAIR of u * u
exception FUNC of u -> u


datatype 'a conv = CONV of ('a -> u) * (u -> 'a) (* (from,to) - w.r.t 'a type, so from-A / to-A *)


val Int : ('a conv) = CONV (fn x => Universal (INT x), 
							fn Universal (INT x) => x)

val Pair : ('a conv * 'b conv -> ('a,'b) conv) =
	fn (CONV (fromA,toA), CONV (fromB,toB)) =>
	   CONV (fn (a,b) => Universal (PAIR (fromA a, fromB b)),
			 fn Universal (PAIR (ua,ub)) => (toA ua, toB ub))

val Func : ('a conv * 'b conv -> ('a -> 'b) conv) =
	fn (CONV (fromA,toA), CONV (fromB,toB)) =>
	   CONV (fn F => Universal (FUNC (fn ua => fromB (F (toA ua)))),
			 fn Universal (FUNC Fu) => (fn a => toB (Fu (fromA a))))


datatype constructed = TAG of string * u

(*
exp: con : ty
-->
fn x => TAG ("con", fromTy ty)

pat: CONTEXT[ con:ty (pat) ] => exp
-->
CONTEXT[ TAG ("con",u) ] => case (toTy u) of pat => exp
*)

fun 			 

		TY'Var of tyvar
	  | TY'Record of tyrow
	  | TY'Con of ty list * tycon long
	  | TY'Tuple of ty list
	  | TY'Func of ty * ty

