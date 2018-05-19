
val preludeDir = "prelude/"
fun prefixPrelude s = preludeDir^s
val pervasives = map prefixPrelude ["pervasives.ML"]; (*["option.ML","either.ML"];*)
;map use pervasives;
val nml_sources = map (fn s => "ML/"^s)
	[
	 "CCODE.ML",
	 "MACHINE.ML",
	 "COMPILE3.ML",
	 "CPS.ML",
	 "EMBED.ML",
	 "EVAL3.ML",
(* pre-compiler *)
	 "ATOM.ML",
	 "VALUE.ML",
	 "BASIS.ML",
	 "BUILTIN.ML",
	 "LANG.ML",
	 "TOK.ML",
	 "POS.ML",
	 "LEX.ML",
	 "PARSER.ML",
	 "PRETTY.ML",
	 "RUN.ML",
	 "PROGRAM.ML",
	 "INTERPRETER.ML"
	 ] @ map prefixPrelude [
	 "PAR1.ML",
	 "PAR2.ML",
	 "PAR3.ML",
	 "PREL.ML",
	 "SORT.ML",
	 "ASSOC.ML",
	 "QLAYOUT.ML",
	 "MISCLAY.ML",
	 "IMP_HASH.ML"
	 ]
	
(* load the code into NJ... *)
;map use nml_sources;


(*----------------------------------------------------------------------*)
(* Small inlining example... *)
val prefixNML = "NML: ";
use "bind.ML";

print_lang := true; print_embed := true;
print_cps := true; print_code := true;
Run.Nuse "line.ML";
display_counts := true; trace_tailcalls := true;
Run.Nexec "foo 7";
display_counts := false; trace_tailcalls := false;



(*----------------------------------------------------------------------*)
(* LastPiecePuzzle... *)

;use "/home/nic/project/LastPiecePuzzle.ML"
;lpp1[]
(*;lpp[]*)


val prefixNML = "NML: ";
use "bind.ML";
Run.Nuse "predefined/nml_NonPrim.ML";
Run.Nexec "open NonPrim";
map Run.Nuse pervasives;
Run.Nuse (prefixPrelude "PREL.ML");
Run.Nexec "structure Prel = PREL();";
Run.Nuse "/home/nic/project/LastPiecePuzzle.ML";


display_counts := true; (*trace_tailcalls := true;*)
Run.Nexec "describeH \"1\" [1,0,0,1]";
display_counts := false; (*trace_tailcalls := false;*)


display_counts := true; trace_tailcalls := true;
Run.Nexec "describeH \"1\" [1,0,0,1]";
display_counts := false; trace_tailcalls := false;


Run.Nexec "thePieces()";
Run.Nexec "lpp1 []";


(*Run.Nexec "lpp []"*)
(*Run.Nexport ",C/lpp.C" "lpp"*)

Run.Nexport ",C/lpp.C" "lpp1";


print_lang := true; print_embed := true;
print_cps := true; print_code := true;
(*run_compiler := false; (* test interpreter... *)*)
(*bind_all_prim_app := true; right_to_left_tuples := true;*)
(*delay_select := false;*)
(*new_pat_comp := false;*)
(*opt_last_constr := false;*)


(*----------------------------------------------------------------------*)
(* nfib...... *)
val prefixNML = "NML: ";
use "bind.ML";
Run.Nuse "predefined/nml_NonPrim.ML";
Run.Nexec "open NonPrim";
map Run.Nuse pervasives;
Run.Nuse (prefixPrelude "PREL.ML");
Run.Nexec "structure Prel = PREL();";
Run.Nuse "examples/nfib.ML";
Run.Nexport ",C/nfib.C" "nfib_top";



(*----------------------------------------------------------------------*)
(* Type Check (with NJ) predefined... *)
use "predefined/nml_NonPrim.ML"
use "predefined/nml_NonPrimSig.ML"
structure foo = NonPrim : NonPrimSig


(*----------------------------------------------------------------------*)
(* type checking... *)
val prefixNML = "NML: ";
use "bind.ML";

use "ML/tc.ML";

val () = (THE_basis := basis'empty)
val () = tc "datatype 'a list = nil | op :: of 'a * 'a list"
val () = tc "datatype 'a ref = ref of 'a"
val () = tc "datatype bool = true | false"
val () = extendTheBasis (basis'env (env'tyenv tyenv'initial))
val () = tc' "PrimSig.ML"
val () = extendTheBasis (basis'env (look'sigid (!THE_basis) (SIGID (ID "PrimSig"))))

;tc' "predefined/nml_NonPrim.ML"
;tc' "/home/nic/project/prelude/pervasives.ML"
;tc "open NonPrim"
;tc' "examples/prel-body.ML"


;tc' "examples/predefined-body.ML"
;tc' "examples/prel-body.ML"





;tc' "predefined/nml_NonPrimSig.ML"
;tc' "examples/fact.ML" (* notYet: tc: long-var *)
;tc' "examples/pairOps.ML"


;tc "structure S = struct val foo = 42 end"
;tc "structure MyStucture = struct val foo = 42 val mm = map end"

;tc "S.foo" (* not yet *)

;tc "exception HaHa"
;tc "exception Whoops of string"

;tc "raise 1"
;tc "raise aa"
;tc "raise HaHa"
;tc "raise HaHa 1"
;tc "raise Whoops"
;tc "raise Whoops 1"
;tc "raise Whoops \"hey\""



;tc "val x = 5"
;tc "val y = (x,5)" (* work - context is sequenced through calls to tc *)


;tc "[]"
;tc "[1]"
;tc "[1,2]"
;tc "[op ::]"
;tc "[fn x => x, fn y => y]"
;tc "[fn x => x, 1]" (* err *)
;tc "[fn (a,b) => (a,b)]"
;tc "[fn (a,b) => (b,a)]"
;tc "[fn (a,b) => (a,b), fn (a,b) => (b,a)]"

;tc "[fn (1,a) => a]"
;tc "[fn (a,1) => a]"
;tc "[fn (1,a) => a, fn (a,1) => a]"

;tc "fn (a,b,c) => [a,b,c]"
;tc "fn (a,b,c) => [a,b,1,c]"

(* todo: pat list *)
;tc "fn [a,b,c] => (a,b,c)"
;tc "fn [a,b,1,c] => (a,b,c)"


;tc "fn x => 1::x"
;tc "fn x => x::2" (* err *)

; tc "op ::"
; tc "fn (a::b) => (a,b)"

;tc "{foo=1,foo=2}" (* should be an error *)

;tc "val myProd = fn [] => 1 | (x::xs) => x * myProd xs"
;tc "val rec myProd = fn [] => 1 | (x::xs) => x * myProd xs"
;tc "fun myProd [] = 1 | myProd (x::xs) = x * myProd xs"

;tc "fun myProd _ = 1 | myProd (x::xs) = x * myProd xs"

;tc "fun myProd (x::xs) = x * myProd xs"

;tc "datatype 'a myl = nil | cons of 'a * 'a myl ; val mul = fn (a,b) => a:int ; fun myProd nil = 1 | myProd (cons (x,xs)) = mul(x,myProd xs)"
;tc "datatype 'a myl = nil | cons of 'a * 'a myl ; val mul = fn (a,b) => a:int ; val rec myProd = fn nil => 1 | cons (x,xs) => mul(x,myProd xs)"




(*ok*)
;tc "fn x => case x of y => y"

;tc "{}"
;tc "fn {} => 1" (* parse error -oops!*)

;tc "{a=1}"
;tc "fn {a} => a"

;tc "{3=1}"
;tc "fn {3=a} => a"

;tc "fun f x = f x" (* fval - works*)



(* valrec... *)

;tc "val rec f = fn x => f x"
;tc "val rec f = fn x => f (f x)"
;tc "val rec f = fn x => f 5"
;tc "val rec f = fn x => f (f 5)"

;tc "val rec f = fn (x,y) => if y then x else f (x,y)"
;tc "val rec f = fn (x,y) => if y then x else f 5" (*err*)
;tc "val rec f = fn (x,y) => if y then x else f x" (*err*)
;tc "val rec f = fn (x,y) => if y then x else f y" (*err*)

;tc "val inc = fn x:int => x; val rec f = fn x => f (inc x)"
;tc "val inc = fn x:int => x; val rec f = fn x => inc (f x)"


tc "fn x => x"
tc "fn x => let val y = x in y end"

tc "fn f => fn x => f x"
tc "fn f => let val res = fn x => f x in res end"

tc "fn f => f 1"
tc "fn f => let val res = f 1 in res end" 

;tc "fn (p,b,inc) => fn x => if (p x) then b else (inc x) "

;tc "fn (p,b,inc) => let val f = fn x => if (p x) then b else (inc x) in f end"
;tc "fn (p,b,inc) => let val f = fn x => if (p x) then b else f (inc x) in f end" (*err*)

;tc "fn (p,b,inc) => let val rec f = fn x => if (p x) then b else f (inc x) in f end"
;tc "fn (p,b,inc) => let val rec f = fn x => if (p x) then b else inc (f x) in f end"
;tc "fn (p,b,inc) => let val rec f = fn x => if (p x) then b else inc (f (inc x)) in f end"

;tc "fn (a,b) => (a,b)"
;tc "fn (a,b) => 1"




;tc "val f = fn A => 1"
;tc "val A = 1; val f = fn A => 1"
;tc "datatype t = A ; val f = fn A => 1"

;tc "datatype t = A of int  ; val f = fn A x => x"
;tc "datatype t = A of int * int  ; val f = fn A (x,y) => x"
;tc "datatype 'a t = A of 'a * 'a t  ; val f = fn A (x,A (y,_)) => (x,y)"

;tc "datatype even = Zero | Esucc of odd and odd = Osucc of even" (* not yet *)
;tc "datatype 'a tree = Base of 'a | Branch of 'a wood and 'a wood = Nil | Cons of 'a tree * 'a wood" (* NY *)

;tc "datatype nat = Zero | Succ of nat"
;tc "datatype intlist = IntListNil | IntListCons of int * intlist"

;tc "datatype 'a option = None | Some of 'a"
;tc "datatype ('a,'b) either = This of 'a | That of 'b"
;tc "datatype ('a,'b) eitherOrBoth = This of 'a | That of 'b | Both of 'a * 'b"

;tc "datatype 'a andInt = andInt of int * 'a"
;tc "datatype 'a mylist = MyNil | MyCons of 'a * 'a mylist"
;tc "datatype 'a list = nil | op :: of 'a * 'a list"


;tc "datatype t = C of t" (* no base *)
;tc "datatype 'a t = C of 'a t"
;tc "datatype 'a t = B | C of 'a t"
;tc "datatype 'a t = B of 'a | C of 'a t"
;tc "datatype 'a t = B of 'a | C of int t" (* non uniform *)
;tc "datatype 'a t = B of 'a | C of ('a * 'a) t" (* non uniform *)
;tc "datatype 'a t = B of 'a | C of int t * 'a t * ('a * 'a) t" (* non uniform *)

;tc "datatype ('a,'b) t = A of 'a | B of 'b | C of ('a,'b) t"
;tc "datatype ('a,'b) t = A of 'a | B of 'b | C of ('a,'a) t" (* no uniform - aa *)
;tc "datatype ('a,'b) t = A of 'a | B of 'b | C of ('b,'b) t" (* no uniform - bb *)
;tc "datatype ('a,'b) t = A of 'a | B of 'b | C of ('b,'a) t" (* no uniform - ba (tvars swapped) *)

;tc "datatype ('a,'b) t = C of ('b,'a) t" (* no uniform - ba *)



(* more non-uniform... *)
tc "datatype 'a nonuniform = MyNil | NyCons of 'a * ('a * 'a) nonuniform"
tc "datatype 'a nonuniform = MyNil | NyCons of 'a * int nonuniform * 'a nonuniform"
tc "datatype ('a,'b) hmm = MyNil | NyCons of 'a * (int,bool) hmm" 
tc "datatype ('a,'b) hmm = MyNil | NyCons of 'a * (int,'a) hmm" 
tc "datatype ('a,'b) hmm = MyNil | NyCons of 'a * ('a,int) hmm" 

(* tc err... *)
tc "datatype intlist = IntListNil | IntListCons of int * bool intlist" (*err*)
tc "datatype 'a mylist = MyNil | NyCons of 'a * mylist" (*err*)
tc "datatype 'a mylist = MyNil | NyCons of 'a * ('a,'a) mylist" (*err*)
tc "datatype bad = Bad of int * 'a" (* err *)


tc "datatype maybeInt = Nope | Yup of int ; val (a,b) = (Nope,Yup 2)"

tc "datatype maybeInt = Nope | Yup of int ; datatype lala = lala of int * maybeInt"


;tc "fn f => fn g => fn x : 'x => f (g x)"

tc "val id:'x->'x = fn a => a"
tc "val (id1,foo) = (fn a => a,22):((int->int)*int)"

;tc "val five = 5"
;tc "val five = 5 val ten = (five,five)"
;tc "val (a,b) = (1,a)" (* no *)


;tc "val (a,b) = (1,2) val c = 3 val d = 4"
;tc "val (a,b) = (1,2) val (c,d) = (3,4)"

;tc "val x = (1,fn x=> x,3,(4,5))"

;tc "val (a,b) = (1,2) val (c,d) = (a+ ,4)"


;tc "val dub = fn a => fn f => f a a val d1 = dub dub val d2 = dub d1"


;tc "val (a,b) = (1,2) and c = 3 val d = 4"

;tc "val id = fn x => x" (* dec *)

;tc "val id = fn x => x" (* dec *)

;tc "let val id = fn x => x in id end"

;tc "let val id = fn x => x in id end"


;tc "fn f => fn g => fn x => f (g x)"

;tc "fn f => fn g: 'x -> 'z => fn x : 'x => f (g x) "



;tc "fn f => fn g => fn x => f (g x)" (* compose example *)

;tc "fn () => let val id1 = fn a => a val id2 = fn b => b val id3 = fn c => c in (id1,id2,id3, fn d => d) end"
;tc "fn () => let val id1 = fn a => a val id2 = fn b:'x => b val id3 = fn c:'x => c in (id1,id2,id3, fn d => d) end"
;tc "fn () => let val id1 = fn a => a val id2 = fn b:'x => b val 'x id3 = fn c:'x => c in (id1,id2,id3, fn d => d) end"

;tc "fn () => let val id1 = fn a => a val id2 = fn b:'x => b val id3 = fn c:'x => c in (id1,id2,id3, fn d:'x => d) end"
;tc "fn () => let val id1 = fn a => a val id2 = fn b:'x => b val 'x id3 = fn c:'x => c in (id1,id2,id3, fn d:'x => d) end"


;tc "fn () => let val x = let val id = fn z => z in id id end in x end"
;tc "fn () => let val x = let val id : 'a -> 'a = fn z => z in id id end in x end"
;tc "fn () => let val x = (let val id : 'a -> 'a = fn z => z in id id end, fn z => z) in x end"
;tc "fn () => let val x = (fn () => let val id : 'a -> 'a = fn z => z in id id end, fn z => z : 'a) in x end" (* no tc *)
;tc "fn () => let val x = (fn () => let val id : 'a -> 'a = fn z => z in id id end, fn z => z : 'b) in x end"
;tc "fn () => let val x = (fn () => let val 'a id : 'a -> 'a = fn z => z in id id end, fn z => z : 'a) in x end"


;tc "let val (id1,id2) = (let val id = fn x => x in id end, let val id = fn y => y in id end) in id1 id2 end"
;tc "let val ('one,'two) (id1,id2) = (let val id = fn x => x in id end, let val id = fn y => y in id end) in id1 id2 end"

;tc "fn a => let val id = fn x => x in (id a, id (a,a)) end"
;tc "fn a => let val id = fn x:int => x in (id a, id 5) end" 
;tc "fn a => let val id = fn x:bool => x in (id a, id 5) end" (*tc err*)

;tc "fn a => let val id = fn x:'a => x in (id a, id 5) end"
;tc "fn a => let val id = fn x:'a => x in (id a, id (a,a)) end" 


(* ty - works *)
; tc "(fn x => x): 'a -> 'a"
; tc "(fn x:'a list => x)"
; tc "fn x => x : 'x"
; tc "fn x : 'x => x"
; tc "(fn (x,y) => (x,y))"
; tc "(fn (x,y) => (x,y)): ('a*'a) -> ('a*'a)"
; tc "(fn x => x): {} -> {}"
; tc "1:int"
; tc "(fn x => x):int -> int"
; tc "fn x:int => x"
; tc "fn x => x : int"
; tc "(fn x : int list => x)"
; tc "(fn x : (int * bool) list => x)"
; tc "(fn x : unit list => x)"
; tc "(fn x => x): unit -> unit"

(* works - expect typ error *)
; tc "(fn x => x): 'x -> 'y"
; tc "fn x:'x => x:'y"
; tc "fn x:int int => x"
; tc "1:bool" 
; tc "1:'a"
; tc "(fn x => x): 'a -> 'b"
; tc "(fn x => x): 'a -> int"
; tc "(fn x => x): int -> 'a"
; tc "(fn (x,y) => (x,y)): 'a -> 'a"
; tc "(fn (x,y) => (x,y)): ('a*'a) -> ('b*'b)"


(* not yet *)
;tc "fn () => let fun dub a f = f a a in dub end" (* fb *)
;tc "1 + 2" (* abort: look'vid + *)


(* works... *)

;tc "fn 1 => 2 | 3 => 4"
;tc "fn (1,a,b) => a | (2,a,b) => b" 
;tc "fn (1,a) => (2,a) | (3,b) => (4,b)" 
;tc "fn (1,a) => (2,a) | (3,b) => (b,4)" 

;tc "fn 42 => 99"
;tc "fn (42,a,_) => (a,99)"
;tc "fn _ => 99"
;tc "fn (_,_) => 99"
;tc "fn {a,b} => 99"
;tc "42"
;tc "fn () => 1"

;tc "fn () => let val dub = fn a => fn f => f a a in dub end"
;tc "fn () => let val dub = fn a => fn f => f a a in dub dub end"
;tc "fn () => let val dub = fn a => fn f => f a a in dub (dub dub) end"

;tc "()"
;tc "((),())"
;tc "((),fn x => x)"
;tc "fn () => fn x => x"
;tc "fn (x,()) => x"
;tc "fn ((),()) => ()"

;tc "fn a => let val b = a in b end"
;tc "fn a => let val F = fn f => f a in (F,F) end"

;tc "fn a => let val id = fn x => x in id a end"
;tc "fn a => let val id = fn x => x in id (a,a) end"
;tc "let val id = fn x => x in (id,id) end"
;tc "fn a => let val id = fn x => x in (id a, id (a,a)) end"

;tc "fn (A,a,b,c,D,d,e,f) => (if A a then b else c, if D d then e else f)"
;tc "fn (a,b,c,d,e,f) => (a,b,c,d,e,f, if a then b else c, if d then e else f)"
;tc "fn (a,b,c,d) => (if a then a else b, if c then c else d)"

;tc "fn (a,b) => (b,a)"
;tc "fn {a,b} => (a,b)"
;tc "fn (a,b) => {2=a, 4=b}"
;tc "fn (a,b) => {1=a, 2=b}"
;tc "fn (a,b) => {1=a, 3=b}"
;tc "fn (a,b) => {2=a, 1=b}"
;tc "fn (a,b) => {3=a, 1=b}"

;tc "fn a => fn f => fn g => if a then (f,g) else (g,f)"
;tc "fn {a,b} => (b,a)"
;tc "fn a => fn f => fn g => if a then (f,g) else (f,g)"
;tc "fn a => if a then (a,a) else (a,a)"
;tc "fn a => fn b => if a then (a,a) else b"
;tc "fn f => fn g => {f=f,g=g}"
;tc "fn a => fn f => fn g => a (f (g,g)) (f (g,g))"
;tc "fn a => fn f => fn g => a {f=f,g=g}"
;tc "fn f => fn g => fn x=> (f g, g x)"
;tc "fn a => fn b => fn c => fn d => a (b c) (b c)"
;tc "fn x => x"

(* tc error expected... *)
;tc "fn a => if a then (a,a) else (a,a,a)"
;tc "fn x => x x"
;tc "fn a => fn b => if b then (a,a) else b"
;tc "fn a => (fn id => (id a, id (a,a))) (fn x => x)"
;tc "fn 1 => 2 | 3 => (4,5)"
;tc "fn (1,a) => a | (2,f) => f()"


print_lang := true; print_embed := true;
print_cps := true; print_code := true;
Run.Nexec "(fn f => f (f 5)) (fn x => x + 1)";


(*----------------------------------------------------------------------*)
(* NJ behav for examples of expliit type vars... *)


fn () =>
let val id1 = fn a => a
	val id2 = fn b => b
	val id3 = fn c => c
in (id1,id2,id3, fn d => d)
end

fn () =>
let val id1 = fn a => a
	val id2 = fn b : 'x => b
	val ('x,'y) id3 = fn c : 'x => c
in (id1,id2,id3, fn d : 'x => d)
end


(fn () =>
let val id1 = fn a => a
	val id2 = fn b => b
	val id3 = fn c => c
in (id1,id2,id3, fn d => d)
end) : unit -> ('a -> 'a) * ('b -> 'b) * ('c -> 'c) * ('d -> 'd)

(fn () =>
let val id1 = fn a => a
	val id2 = fn b : 'x => b
	val 'x id3 = fn c : 'x => c
in (id1,id2,id3, fn d : 'x => d)
end)
 : unit -> ('a -> 'a) * ('x -> 'x) * ('c -> 'c) * ('x -> 'x)
(* : unit -> ('a -> 'a) * ('y -> 'y) * ('c -> 'c) * ('y -> 'y) (* no *)*)


(fn () =>
let val id = fn a : 'b => a
in id
end
) : unit -> 'c -> 'c



fun completely_disallowed a =
	let datatype d = D of 'a
		fun deD (D x) = x
	in deD (D a)
	end

fun completely_disallowed a =
	let type d = 'a
	in a : d
	end

fun ok a =
	let exception E of 'a
	in (raise (E a)) handle E b => b
	end
	  
val nope = 
	let exception E of 'a
	in raise (E 5) handle E b => b
	end

val nope = 
	let exception E of 'a
	in fn a => (raise (E a)) handle E b => b
	end

fun ok () =
	let exception E of 'a
	in fn a => (raise (E a)) handle E b => b
	end



fn () => let val x = let val id = fn z => z in id id end in x end
fn () => let val x = let val id : 'a -> 'a = fn z => z in id id end in x end
fn () => let val x = (let val id : 'a -> 'a = fn z => z in id id end, fn z => z) in x end
fn () => let val x = (fn () => let val id : 'a -> 'a = fn z => z in id id end, fn z => z : 'a) in x end (* no tc *)
fn () => let val x = (fn () => let val id : 'a -> 'a = fn z => z in id id end, fn z => z : 'b) in x end
fn () => let val x = (fn () => let val 'a id : 'a -> 'a = fn z => z in id id end, fn z => z : 'a) in x end

fn () => let val x = (let val id : 'a -> 'a = fn z => z in id id end, fn z => z : 'a) in x end (* no tc *)
fn () => let val x = (let val id : 'a -> 'a = fn z => z in id id end, fn z => z : 'b) in x end (* no tc expansive!*)
fn () => let val x = (let val five = 5 in five end, fn z => z : 'b) in x end (* no tc - expansive! *)
fn () => let val x = (5, fn z => z : 'b) in x end (* ok *)


(*----------------------------------------------------------------------
INDEX: unresolved flex  - NJ experiment
----------------------------------------------------------------------*)

#one
fn {one,...} => ()
fn {one,two,...} => ()

#one : {one:int} -> int
#one : {one:int} -> 'a (* no *)
#one : {one:'a} -> 'a
#one : {one:'a,two:'b} -> 'a


#one {one=1,two=2}
(fn {one,two,...} => ()) {one=1,two=2}
(fn {one,two,...} => ()) {one=1,two=2,three=3}

(fn {one,two,...} => ()) {one=1,three=3} (* expect tc error *)

(*----------------------------------------------------------------------*)
(* 4x4 forcing... *)

val prefixNML = "NML: ";
use "bind.ML";
(* Run.Nuse "ff.ML" *)
Run.Nuse "predefined/nml_NonPrim.ML";
Run.Nexec "open NonPrim;";
Run.Nexec "structure Word32 = Word";
map Run.Nuse [
"/home/nic/project/prelude/pervasives.ML",
"/home/nic/project/prelude/PREL.ML",
"/home/nic/project/prelude/SORT.ML",
"/home/nic/project/prelude/QLAYOUT.ML",
"/home/nic/project/prelude/MISCLAY.ML",
"/home/nic/project/prelude/PAR3.ML",
"/home/nic/project/prelude/IMP_HASH.ML",
"ff-setup.ML",
"/home/nic/project/stack4x4/book.ML",
"/home/nic/project/stack4x4/force.ML"]

Run.Nexport ",C/ff.C" "ff_export";

Run.Nexec "ff \"D1 d2 E1\" \"pfw\"";


(*trace_tailcalls := false;*)
Run.Nexec "ff_export [\"d1d2\",\"w\"]";
Run.Nexec "ff \"d1d2 e1a8 f1h8\" \"w\"";
Run.Nexec "ff \"d1 c1 h5 h6 d8 c8 a5 a6 b6 c7 b5 c6 c5 d5 a4 b4\" \"fw\"";


(*----------------------------------------------------------------------*)
(* 4x4 forcing... *)
use "ff.ML";

ff "D1 d2 E1" "pfw";

ff "D1 d2 A4 h4 A1 h1" "fw";
ff "d1d2" "w";
ff "d1d2 e1a8 f1h8" "w";
ff "d1 c1 h5 h6 d8 c8 a5 a6 b6 c7 b5 c6 c5 d5 a4 b4" "fw";
ff "d1 c1 h5 h6 d8 c8 a5 a6 b6 c7 b5 c6 c5 d5" "ffw";
ff "d1e1 a4b4 d2d3 a1a2" "ffw";


(*----------------------------------------------------------------------*)
val prefixNML = "NML: ";
use "bind.ML";
(*run_compiler := false; (* test interpreter... *)*)
(*right_to_left_args := false; optimize_multi_app := false; (* correct ML func/arg eval-order semantics *)*)
(*lift_lets := true;*)
(*new_pat_comp := false;*)
use "test.ML";
exportTests();
runTests tests;



(*----------------------------------------------------------------------*)
(* Export various examples as C code... *)
val prefixNML = "NML: ";
use "bind.ML";
(*use "examples/xstrip.ML";*)
(*xstrip_top ["zero(one(two)three(four(five)six seven eight) nine"];*)
Run.Nuse "examples/xstrip.ML";
Run.Nexport ",C/xstrip.C" "xstrip_top";
Run.Nuse "predefined/nml_NonPrim.ML";
Run.Nexec "open NonPrim";
map Run.Nuse pervasives;
Run.Nuse (prefixPrelude "PREL.ML");
Run.Nexec "structure Prel = PREL();";
(*use "examples/nfib.ML"; nfib_top ["30"];*)
Run.Nuse "examples/nfib.ML";
Run.Nexport ",C/nfib.C" "nfib_top";

Run.Nuse "examples/even_odd.ML";
Run.Nuse "examples/fact.ML";
Run.Nuse "examples/thrice.ML";
Run.Nexport ",C/even.C" "even_input";
Run.Nexport ",C/fact.C" "fact_input";
Run.Nexport ",C/thrice.C" "thrice_top";
Run.Nexec "fun id x = x; fun id2 x y = x y";
Run.Nexport ",C/idchain.C" "fn _ => id2 id id id id 42";
Run.Nexec "fun addfive a b c d e = a+b+c+d+e";
Run.Nexport ",C/papchain.C" "fn _=> id (id (id (id (id addfive 1) 2) 3) 4) 5";
map Run.Nuse nml_sources;
Run.Nexec "val prefixNML = \"NML-inner: \";"; 
Run.Nuse "bind.ML";
Run.Nexport ",C/tok.C" "fn[file]=> Run.echo_tl {IN=file}";
Run.Nexport ",C/tpp.C" "fn[file]=> Run.tpp file";
(*nqueens*)
Run.Nuse "/home/nic/project/misc-ml/nqueens.ML";
Run.Nexec "local open Misclay in val Pqueens = output_layout o layVert o map (fn xs =>laySqBracket (layCommaSep (map layInt xs))) end";
Run.Nexport ",C/nqueens.C" "fn [arg] => Pqueens (nqueens (case (Prel.readInt arg) of Some i => i))";
(*life*)
Run.Nuse "examples/BMARK.ML";
Run.Nuse "/home/nic/sml-resources/BENCHMARKS/SML97/programs/life/life.sml";
Run.Nuse "examples/life_top.ML";
Run.Nexport ",C/life.C" "life_top";
(*nux*)
Run.Nexport ",C/nux.C" "Run.nux";
(*Run.Nexport ",C/tup.C" "let val P=(3,5) in fn _ => let val (a,b) = P in (a*42+b) end end";*)



(*----------------------------------------------------------------------*)
(* odds and sods... *)

print_lang := true; print_embed := true; 
print_cps := true; print_code := true;
Run.Nuse "regs/gen-exception1.ML"

Run.Nuse "regs/raise-order.ML"



(*----------------------------------------------------------------------*)
(* Self-Comp-Sum example... *)
val prefixNML = "NML: ";
use "bind.ML";
(*print_cps := true; print_code := true;*)
(*run_compiler := false; (* test interpreter... *)*)
(*bind_all_prim_app := true; right_to_left_tuples := true;*)
(*delay_select := false;*)
(*new_pat_comp := false;*)
(*opt_last_constr := false;*)
Run.Nuse "predefined/nml_NonPrim.ML";
Run.Nexec "open NonPrim";
map Run.Nuse pervasives;
map Run.Nuse nml_sources;
Run.Nexec "val prefixNML = \"NML-inner: \";"; 
Run.Nuse "bind.ML";

(* fun bind example - eval at levels 0,1,2 *)
let fun mySum [] = 0 | mySum (x::xs) = x + mySum xs in mySum [1,2,3] end;
Run.Nexec "let fun mySum [] = 0 | mySum (x::xs) = x + mySum xs in mySum [1,2,3] end";
display_counts := true;
Run.Nexec "Run.Nexec \"let fun mySum [] = 0 | mySum (x::xs) = x + mySum xs in mySum [1,2,3] end\"";
display_counts := false;


(*----------------------------------------------------------------------*)
(* Try NML-inner-squared *)

use "bind.ML";
Run.Nuse "predefined/nml_NonPrim.ML";
Run.Nexec "open NonPrim";
map Run.Nuse pervasives;
map Run.Nuse nml_sources;
Run.Nexec "val prefixNML = \"NML-inner: \";"; 
Run.Nuse "bind.ML";
fun Run2_Nuse s = Run.Nexec ("Run.Nuse \"" ^ s ^ "\"");
fun Run2_Nexec s = Run.Nexec ("Run.Nexec \"" ^ s ^ "\"");

(*
Run.Nuse "examples/fact.ML"
Run.Nexec "fact 9"
*)
Run2_Nuse "predefined/nml_NonPrim.ML";
Run2_Nexec "open NonPrim";
map Run2_Nuse pervasives;
(*
Run2_Nuse (prefixPrelude "PREL.ML");
Run2_Nexec "structure Prel = PREL();";
Run2_Nuse "examples/fact.ML";
Run2_Nexec "fact 9";
*)

map Run2_Nuse nml_sources; (* takes ages, aprox 15/20 mins or maybe more - this is an old comment! *)


Run.Nexec "Run.Nexec \"val prefixNML = \\\"NML-inner-squared: \\\";\"";
Run.Nexec "Run.Nuse \"bind.ML\""; (* another 9 mins *)
(* casues - Abort: error: VALUE: <=[arg1]:int'value !! *)


Run.Nexec "Run.Nexec \"Run.Nexec \\\"let fun mySum [] = 0 | mySum (x::xs) = x + mySum xs in mySum [1,2,3] end\\\"\""; (* 5ish mins *)

Run.Nexec "Run.Nexec \"Run.Nexec \\\"7\\\"\""; (* only a minute! *)





(*----------------------------------------------------------------------*)
(* Investigate problem with overlaoding (problem in C, not seen in ML) *)


(* use nux to export nux... *)
val old_nx_args = 
 ([
		  "predefined/nml_NonPrim.ML",
		  "-x","open NonPrim",
		  prefixPrelude "pervasives.ML"] 
		 @ nml_sources 
		 @ ["-x", "val prefixNML = \"NML-exported: \";",
			"bind.ML",
			"-x", "Run.nux",
			"--export", ",C/nux.C"
			]);

map (fn s => print (s^"\n")) works_nux_args;

val nux_args = [
"predefined/nml_NonPrim.ML",
"-x",
"open NonPrim",
"/home/nic/project/prelude/pervasives.ML",
"/home/nic/project/prelude/ASSOC.ML",
"/home/nic/project/prelude/IMP_HASH.ML",
"/home/nic/project/prelude/MISCLAY.ML",
"/home/nic/project/prelude/PAR1.ML",
"/home/nic/project/prelude/PAR2.ML",
"/home/nic/project/prelude/PAR3.ML",
"/home/nic/project/prelude/PREL.ML",
"/home/nic/project/prelude/QLAYOUT.ML",
"/home/nic/project/prelude/SORT.ML",
"ML/ATOM.ML",
"ML/BASIS.ML",
"ML/BUILTIN.ML",
"ML/CCODE.ML",
"ML/COMPILE3.ML",
"ML/CPS.ML",
"ML/EMBED.ML",
"ML/EVAL3.ML",
"ML/INTERPRETER.ML",
"ML/LANG.ML",
"ML/LEX.ML",
"ML/MACHINE.ML",
"ML/PARSER.ML",
"ML/POS.ML",
"ML/PRETTY.ML",
"ML/PROGRAM.ML",
"ML/RUN.ML",
"ML/TOK.ML",
"ML/VALUE.ML",
"-x",
"val prefixNML = \"NML-squared: \";",
"bind.ML",
"-x",
"Run.nux",
"--export",
"nux2.C"
]

map (fn s => print (s^"\n")) nux_args;

use "bind.ML";
Run.nux nux_args;

(*----------------------------------------------------------------------*)
use "bind.ML";

Run.Nexec "1 <= 1"
Run.Nexec "\"cat\" <= \"dog\""

Run.Nexec "fun printBool b = print (if b then \"true\\n\" else \"false\\n\")"
Run.Nexport ",C/leq.C" "fn [x,y] => printBool(x <= y)"


(*
Run.Nexec "val cat = Atom.ID \"cat\""
Run.Nexec "val dog = Atom.ID \"dog\""
Run.Nexec "Embed.leq'id (cat,dog)"
Run.nux ["-x", "\"cat\" <= \"dog\""]
Run.nux ["-x", "fn [] => \"cat\" <= \"dog\"", "--export",",C/cat_dog.C"]
Run.Nexec "Run.nux [\"-x\", \"\\\"cat\\\" <= \\\"dog\\\"\"]"
*)
(*Run.Nexport "nux.C" "Run.nux";*)


(*----------------------------------------------------------------------*)
(* nqueens example... *)

use "bind.ML";
Run.Nuse "predefined/nml_NonPrim.ML";
Run.Nexec "open NonPrim";
map (Run.Nuse o prefixPrelude) ["pervasives.ML", "PREL.ML", "QLAYOUT.ML", "MISCLAY.ML"];
Run.Nexec "structure Prel = PREL();";
Run.Nexec "structure QLayout = QLAYOUT(structure Prel = Prel)";
Run.Nexec "structure Misclay = MISCLAY(structure Prel = Prel structure QLayout = QLayout)";

Run.Nuse "/home/nic/project/misc-ml/nqueens.ML";
Run.Nexec "local open Misclay in val Pqueens = output_layout o layVert o map (fn xs =>laySqBracket (layCommaSep (map layInt xs))) end";
(*Run.Nexec "P (nqueens 5)";*)
Run.Nexport ",C/nqueens.C" "fn [arg] => Pqueens (nqueens (case (Prel.readInt arg) of Some i => i))"


(*----------------------------------------------------------------------*)
(* life example... *)

use "examples/BMARK.ML";
use "/home/nic/sml-resources/BENCHMARKS/SML97/programs/life/life.sml";
use "examples/life_top.ML"
val X = life_top ["3"]

use "bind.ML";
Run.Nuse ("predefined/nml_NonPrim.ML");
Run.Nexec ("open NonPrim");
map (Run.Nuse o prefixPrelude) ["pervasives.ML", "PREL.ML"];
Run.Nexec "structure Prel = PREL();";
Run.Nuse "examples/BMARK.ML";
Run.Nuse "/home/nic/sml-resources/BENCHMARKS/SML97/programs/life/life.sml";
Run.Nuse "examples/life_top.ML";

Run.Nexec "life_top [\"3\"]";

Run.Nexport ",C/new-life.C" "life_top";


(*----------------------------------------------------------------------*)
(* nux... *)

(* use nux to export fact...*)
use "bind.ML";
Run.nux [
		 "predefined/nml_NonPrim.ML",
		 "-x","open NonPrim",
		 prefixPrelude "pervasives.ML",
		 prefixPrelude "PREL.ML",
		 "-x","structure Prel = PREL();",
		 "examples/fact.ML",
		 "-x","fact_input",
		 "--export", ",C/fact.C"
		 ];

(* use nux to export nux... *)
use "bind.ML";
Run.nux ([
		  "predefined/nml_NonPrim.ML",
		  "-x","open NonPrim",
		  prefixPrelude "pervasives.ML"] 
		 @ nml_sources 
		 @ ["-x", "val prefixNML = \"NML-exported: \";",
			"bind.ML",
			"-x", "Run.nux",
			"--export", ",C/nux.C"
			])


(* export nux... *)
use "bind.ML";
Run.Nuse "nml_NonPrim.ML";
Run.Nexec "open NonPrim";
map Run.Nuse pervasives;
map Run.Nuse nml_sources;
Run.Nexec "val prefixNML = \"NML-inner: \";"; 
Run.Nuse "bind.ML";
Run.Nexport "runtime/nux.C" "Run.nux";


(*----------------------------------------------------------------------*)



use "bind.ML";
Run.tpp' "MACHINE.ML"
use ",g/MACHINE.ML";
Run.Nuse ",g/MACHINE.ML";







Run.tpp "runtime/sample.ML"
Run.Nexec "Run.tpp \"runtime/sample.ML\"" (* broken! - Abort: INSTREAM*)


Run.Nexec "val a = ref (fn () => 1,2); val T = (fn () => let val (p,q) = !a in p()+q end, 99); val () = (a:= T)"
Run.Nexport' "T"



Run.echo_tl {IN="runtime/sample.ML"}
Run.tpp "runtime/sample.ML"

Run.Nexec "decompile Compile.generateCode";



Run.Nexec "decompile mySum";
Run.Nexec "exportC mySum"

Run.Nexec "decompile Prel.stringOfInt";
Run.Nexec "exportC Prel.stringOfInt";

Run.Nexec "decompile go";



Run.Nuse "nml_NonPrim.ML";
Run.Nexec "decompile NonPrim.map";
Run.Nexec "exportC NonPrim.map";




Run.Nexec "exportC Run.Nexec";



use "bind.ML";
(*print_cps := true; print_code := true;*)
Run.Nexec "fun fact n = if n=0 then 1 else n * fact (n-1)";
Run.Nexec "decompile fact";
Run.Nexec "exportC fact";


use "bind.ML";
Run.Nexec "val v = [41,42]";
Run.Nexec "val v2 = (v,43,v)";
Run.Nexec "exportC v2";



use "bind.ML";
Run.Nexec "fun f x = x + x";
Run.Nexec "exportC f"

use "bind.ML";
Run.Nexec "val f = let val b = (5,6) in fn a => (a,b) end";
Run.Nexec "exportC f"


use "bind.ML";
(*print_cps := true; print_code := true;*)
Run.Nexec "fun fact fact n = if n=0 then 1 else n * fact (n-1)";
Run.Nexec "exportC fact";


use "bind.ML";
Run.Nuse "nml_NonPrim.ML";
Run.Nexec "open NonPrim";
Run.Nexec "exportC map";

Run.Nexec "exportC concat"
Run.Nexec "exportC (map,concat)"



Run.Nexec "val a = 1 + 2";
Run.Nexec "fun f b = a + b";
Run.Nexec "exportC f";


Run.Nexec "val f = let val b = 5 in fn a => a + b end";
Run.Nexec "exportC f"

Run.Nexec "fun plus x y = x + y"
Run.Nexec "exportC plus"


Run.Nexec "decompile go";





(*----------------------------------------------------------------------*)

(* run lib2x in NML... *)

val new_prelude_sources = map (fn s => preludeDir^s)
	[
	 "PAR1.ML", (* orig PVF *)
	 "PAR2.ML", (* continuation version *)
	 "PAR3.ML", (* continuation version - saturated apps *)
	 "CHAR_LEX.ML", 
	 "PREL.ML",
	 "ASSOC.ML",
	 "QLAYOUT.ML",
	 "MISCLAY.ML",
	 "IMP_HASH.ML"
	 ]
val lib2x_dir = "/home/nic/work/lib2x/"
val lib2x_sources = 
	 map (fn s => lib2x_dir ^ "ML/AbstractPrelude/"^s)
	[
	 "INT.ML",
	 "LIST.ML",
	 "STRING.ML",
	 "Type.ML",
	 "Combinator.ML",
	 "AbstractPreludeSig.ML"
	 ] @
	map (fn s => lib2x_dir ^ "ML/" ^ s)
	[
	 "GENTYPES.ML", 
	 "STREAMER.ML", 
	 "VERILOG.ML", 
	 "VHDL.ML",
	 "PROLOG.ML",
	 "LIB2X.ML"
	 ]

val prefixNML = "NML: ";
use "bind.ML";
Run.Nuse "nml_NonPrim.ML";
Run.Nexec "open NonPrim";
map Run.Nuse pervasives;

map Run.Nuse new_prelude_sources;
map Run.Nuse lib2x_sources;

Run.Nuse (preludeDir ^ "bind.ML");
Run.Nuse (lib2x_dir ^ "ML/AbstractPrelude/bind.ML");

L1.lib2verilog (lib2x_dir ^ "libs/onespin/crc/lib/onespin.lib")




(*----------------------------------------------------------------------*)


use "bind.ML"; tc();
trace_lex := true;
Run.Nuse "regs/test-while.ML"




val prefixNML = "NML: ";
use "bind.ML";
print_lang := true; print_embed := true; 
print_cps := true; print_code := true;
new_pat_comp := false;
new_pat_comp := true;


Run.Nexec "fn [] => 99 | a::b => a"
Run.Nexec "datatype myInt = Lift of int";
Run.Nexec "fn Lift 5 => 55"
Run.Nexec "datatype t = A | B";
Run.Nexec "fn (A,A) => 100 | (B,A) => 300 | (A,B) => 200 | (B,B) => 400"
Run.Nexec "fn [1,a] => a+100 | [2,b] => b+200"
Run.Nexec "fn (a,b,c) => if a then b else c"


Run.Nexec "decompile Embed.embed'longvid"



val prefixNML = "NML: ";
use "bind.ML"; tc();
print_lang := true; print_embed := true;
print_cps := true; print_code := true;
(*new_pat_comp := false;*)






Run.Nexec "exception S of int";
Run.Nexec "fun gen() = let exception C of int exception D of int in fn C 1 => 11 | S 2 => 22 | D 3 => 33 | _ => 0 end";
Run.Nexec "fun gen() = let exception C of int exception D of int in fn C 1 => 11 | S 2 => 22 | D 3 => 33 | C 4 => 44 | _ => 0 end";



display_counts := true;
Run.Nuse "regs/static-and-dynamic-exception2.ML";
display_counts := false;


Run.Nuse "regs/static-and-dynamic-exception.ML";


use "regs/static-and-dynamic-exception.ML";




val prefixNML = "NML: ";
use "bind.ML"; tc();
print_lang := true; print_embed := true; 
print_cps := true; print_code := true;
(*new_pat_comp := false;*)
Run.Nuse "regs/raise-order.ML";



Run.Nexec "decompile map"
Run.Nexec "decompile Prel.zipWith"

Run.Nexec "fn ref(1,1) => 11 | ref(1,2) => 12 | ref(2,1) => 21 | ref(2,2) => 22"

Run.Nexec "fn (1,1) => 11 | (1,2) => 12 | (2,1) => 21 | (2,2) => 22"






Run.Nexec "fn (1,1,_,_,_,_) => 100 | (_,_,1,1,_,_) => 200 | (_,_,_,_,1,1) => 300"


Run.Nexec "datatype e = E of int | F of int ; fn (E x,1) => x+1 | (E x,2) => x+2 | (F x,1) => x+3"
Run.Nexec "exception E of int ; exception F of int; fn (E x,1) => x+1 | (E x,2) => x+2 | (F x,1) => x+3"

Run.Nexec "datatype e = E of int | F of int ; fn (E 1) => 1 | (E 2) => 2 | (F 1) => 3"
Run.Nexec "exception E of int ; exception F of int; fn (E 1) => 1 | (E 2) => 2 | (F 1) => 3"

Run.Nexec "datatype e = E of int ; fn E (1,1) => 11 | E (1,2) => 12"
Run.Nexec "exception E of int ; fn E (1,1) => 11 | E (1,2) => 12"










Run.Nexec "fn op :: _ => 1"
Run.Nexec "fn op :: (_,_) => 1"

Run.Nexec "fn [1] => 100 | [2] => 200"


Run.Nexec "fn [1,a] => a+100 | [2,b] => b+200"

Run.Nexec "fn [a,1] => a+100 | [b,2] => b+200"




Run.Nexec "fn (_,A,1,_) => 100"
Run.Nexec "fn (1,_,a,4) => 100+a | (_,2,b,4) => 200+b"
Run.Nexec "fn (_,b,_) => b"


Run.Nexec "fn op :: p => p"
Run.Nexec "fn op :: (_,b) => b"
Run.Nexec "fn op :: (a,b) => a+b"






Run.Nexec "let val [x] = [] in x end"







Run.Nexec "fn (1,1) => 100"
Run.Nexec "fn op :: _ => 1 | op :: a => a"
Run.Nexec "fn [_] => 1 | [a] => a"
Run.Nexec "fn nil => 1 | op :: a => a"
Run.Nexec "fn op :: a => a"



Run.Nexec "fn _::[] => 1 | a::[] => a"
Run.Nexec "fn _::_ => 1 | a::_ => a"
Run.Nexec "fn (1,1) => 1 | (2,a) => a"






(* problem of RHS duplication *)
Run.Nexec "fn x => case x of (1,1) => 100 | (a,b) => a+b"
Run.Nexec "fn (x,y) => case x of (1,1) => 100 | (a,b) => (case y of (2,2) => 200 | (c,d) => a+b+c+d)"


new_pat_comp := false;
new_pat_comp := true;




(*new_pat_comp := false;*)
Run.Nexec "datatype t = A | B";

Run.Nexec "fn (A,A) => 100 | (A,B) => 200"
Run.Nexec "fn (A,A) => 100 | (B,A) => 300"
Run.Nexec "fn (A,A) => 100 | (A,B) => 200 | (B,A) => 300"
Run.Nexec "fn (A,A) => 100 | (B,A) => 300 | (A,B) => 200"
Run.Nexec "fn (A,A) => 100 | (A,B) => 200 | (B,A) => 300 | (B,B) => 400"
Run.Nexec "fn (A,A) => 100 | (B,A) => 300 | (A,B) => 200 | (B,B) => 400"



Run.Nexec "fn (f,a,b,c,d,e) => case (((a,b),(c,d)),e) of ((p,_),1) => p | ((_,q),2) => q | ((p,_),3) => p | x => f x"

Run.Nexec "fn (f,a,b,c) => case ((a,b),c) of (p,1) => f p | ((_,a),2) => a"










val prefixNML = "NML: ";
use "bind.ML"; tc();
print_lang := true; print_embed := true; 
print_cps := true; print_code := true;
(*new_pat_comp := false;*)
Run.Nuse "regs/eval-order-func-arg.ML";




(* examples to aid migratation of match/fatbar -> if/fail/goto... *)
val prefixNML = "NML: ";
use "bind.ML";
print_lang := true; print_embed := true;
print_cps := true; print_code := true;
Run.Nexec "let exception E fun id x = x val e = E val E = id e in 1 end"




new_pat_comp := false;
Run.Nexec "fn (a,b) => case (a,b) of (c,d) => c+d"

new_pat_comp := true;
Run.Nexec "fn (a,b) => case (a,b) of (c,d) => c+d"


new_pat_comp := false;

new_pat_comp := true;
Run.Nexec "fn (A,A) => 100 | (A,B) => 200"


new_pat_comp := false;
Run.Nexec "fn (a,b,c,d,e,f) => (if a then b else c) + (if d then e else f)"

new_pat_comp := true;
Run.Nexec "fn (a,b,c,d,e,f) => (if a then b else c) + (if d then e else f)"


Run.Nuse "nml_NonPrim.ML";
Run.Nexec "open NonPrim";
Run.Nuse "../prelude/Prel.ML";







(* provoke - 0 calls to goto abort... -fixed*)
Run.Nexec "fn (a,b,c) => case a of _ => b | _ => c"



Run.Nexec "fn (f,a,b,c) => f (case a of 1 => b | _ => c)"


Run.Nexec "fn (a,b,c,d) => (if a then b else c) + d"
Run.Nexec "fn (a,b,c) => case a of 1 => b | _ => c"
Run.Nexec "fn (a,b,c) => (if a then b else c)"
Run.Nexec "fn (a,b,c,d,e) => case a of 1 => b | 2 => c | 3 => d | _ => e"
Run.Nexec "fn (f,a,b,c) => f (if a then b else c)"
Run.Nexec "fn (f,a,b,c) => (if a then b else c) + f"
Run.Nexec "fn (xs,ys) => (case xs of [] => 123 | x::_ => x) + (case ys of [] => 456 | y::_ => y)"

Run.Nexec "fn (a,b,c,d,f) => let val abc = if a then b+c else raise (c+b) in f abc d end"
Run.Nexec "fn (a,b,c,d) => (if a then b else raise c) + d"





(* exhibit bug: Untaken handle fails to reinstate H-ref... fixed now*)
val prefixNML = "NML: ";
use "bind.ML";
Run.Nexec "let exception E in ((1 handle e => 2) + (raise E)) handle e => 4 end"

(* exhibit bug: Taken handle fails to reinstate D-ref... fixed now*)
val prefixNML = "NML: ";
use "bind.ML";
(*print_lang := true; print_embed := true;*)
(*print_cps := true; print_code := true;*)
Run.Nexec "let exception E in ((1+(raise E) handle e => 2) + (raise E)) handle e => 3 end"




val prefixNML = "NML: ";
use "bind.ML"; tc();
print_lang := true; print_embed := true; 
print_cps := true; print_code := true;
X "fn (_,b,_) => b"

X "fn (_,(a::_),_) => a"
X "fn (_,a as op :: _,_) => a"


val prefixNML = "NML: ";
use "bind.ML"; tc();
print_lang := true; print_embed := true; 
print_cps := true; print_code := true;

lift_lets := false;
Run.Nuse "regs/local-scope.ML";

lift_lets := true;
Run.Nuse "regs/local-scope.ML";


val prefixNML = "NML: ";
use "bind.ML"; tc();
print_lang := true; print_embed := true; 
print_cps := true; print_code := true;

Run.Nuse "regs/local-scope.ML";




lift_lets := false;
Run.Nexec "fn (f,a,b) => f a (f (f b))";

lift_lets := true;
Run.Nexec "fn (f,a,b) => f a (f (f b))";



Run.Nexec "fn (f,a,b) => f a (f (f b))"; (* no user lets, cps-nest-lets, push-push  *)
Run.Nexec "fn (f,a,b) => let val A = f b in f a (f A) end"; (* user let, cps-seq-lets, push-in-push *)
Run.Nexec "fn (f,a,b) => f a (let val A = f b in f A end)"; (* nested user-let, cps-nest-lets, push-push *)
Run.Nexec "fn (f,a,b) => let val A = f b in f a (f A) end"; (* effect of lifting user let, "A" would capture "a" *)









Run.Nexec "let fun mySum [] = 0 | mySum (x::xs) = x + mySum xs in mySum [1,2,3] end";






(* quadratic closure when list building ... *)
val prefixNML = "NML: ";
use "bind.ML";
print_lang := true; print_cps := true;
bind_all_prim_app := true;
right_to_left_tuples := true;
Run.Nexec "fn f => [f 1, f 2, f 3, f 4, f 5, f 6, f 7 ]"




(* delayed binding scheme does not work completely... *)
val prefixNML = "NML: ";
use "bind.ML";
print_code := true;
delay_select := true;
delay_var_bind := true;
X "fn (a,(b,c),d) => d" (* does selection of (b,c) despite not being required" *)




val prefixNML = "NML: ";
use "bind.ML";
print_lang := true; print_embed := true; 
print_cps := true; print_code := true;
delay_var_bind := false;
(*delay_select := false;*)
X "fn (a,b,c) => if a then b else c"

X "fn T as (a,_,_) => if a then (case T of (_,b,_) => b) else (case T of (_,_,c) => c)"


val prefixNML = "NML: ";
use "bind.ML";
print_cps := true; print_code := true;
delay_var_bind := false;

X "fn (_,(_,a),_) => a"
X "fn (_,(_,_),_) => 1"
X "fn (_,(_,(_,_),_),a,(_,b,_),_) => a+b" (* dont really want bindings for sub pats, wih no vars - but cps trans could solve this*)


val prefixNML = "NML: ";
use "bind.ML";
print_cps := true; print_code := true;
(*delay_var_bind := false;*)
X "fn (a,(_,_),(_,b),c) => if a then b else c" (* (_,b) = not delayed properly by delay_var_bind *)

X "fn (a,(b,c)) => if a then b else c"



(*----------------------------------------------------------------------
INDEX: old/fixed stuff
----------------------------------------------------------------------*)




val prefixNML = "NML: ";
use "bind.ML";
print_cps := true; print_code := true;
X "fn (r,v,f) => (r:=v, f 1)"
X "fn (r,v,f) => (r+v, f 1)"
X "fn (r,v,f) => explode v"
X "fn (r,v,f) => r:=v"
X "fn (a,b) => a+b"
X "fn (a,b) => (a,b)" (* needs to be optimized. destructs, then reconstructs tuple *)
X "fn (f,a,b) => f a b"
X "fn (r,v,f) => (r:=v, f 1)"
X "fn (r,g) => (r := 1 + g 1, r := 2 + g 2)" (* impure prim bug *)
X "let val r = ref 1 in (!r, r:=2, !r) end"
U "regs/assign-order.ML"


(* raise bug  -fixed *)
val prefixNML = "NML: ";
use "bind.ML";
print_cps := true; print_code := true;
Run.Nexec "fn e => (raise (e 1), raise (e 2))" 
Run.Nexec "fn (b,e,v) => if b then raise e else v";

let exception E of int fun f x = raise (E x) in (raise (E 1), f 2) handle E n => (n,n) end;
Run.Nexec "let exception E of int fun f x = raise (E x) in (raise (E 1), f 2) handle E n => (n,n) end";





Run.Nexec "decompile Parser.reduce_pa"

val prefixNML = "NML: ";
use "bind.ML"; tc(); (* test context *)
Run.Nuse "regs/raise-order.ML"



X "let exception E in (E,E) end"
X "let exception E exception F = E in (E,F) end"
X "let exception E val F = E in (E,F) end"
X "let val E = (1,2) val F = E in (E,F) end"


print_lang := true; print_embed := true;
print_cps := true; print_code := true;
Run.Nuse "regs/local-scope.ML";


Run.Nexec "fn (a,_) => (let val b = 1 in a+b end,let val b = 2 in a+b end)";
Run.Nexec "fn (a,_) => let val b = 2 in (let val b = 1 in a+b end, a+b) end"; (* name capture issue *)
Run.Nexec "fn (a,_) => a + a"
Run.Nexec "fn (a,_) => (a,a)"
Run.Nexec "fn (a,_) => (a+1,a+2)";
Run.Nexec "fn (a,f,g) => f a + g a"
Run.Nexec "fn (a,b) => (if b then a+1 else a+2) + a"


Run.Nexec "fun inc (r as ref x) = (r := 1 + x)"
Run.Nexec "fun foo x y = x := y"
Run.Nexec "fun foo p = op := p"
Run.Nexec "fn p => (fn f => f p) (op :=)"


Run.Nexec "let datatype 'a t = One of 'a in fn One x => 1 end"
Run.Nexec "let datatype ('a,'b) t = One of 'a | Two of 'b in fn (One x, Two ) => 1 | _ => 2 end"



(*delay_var_bind := false;*)
Run.Nexec "fn a => case a of op :: _ => 1"
Run.Nexec "fn a => case a of op :: (a,b) => a"

Run.Nexec "fn a => case a of (b,(c,d),e,(f,g)) => (c,e)"

Run.Nexec "fn a => case a of [b,c,d] => ()"
Run.Nexec "fn a => case a of x::xs => x"


Run.Nexec "fn a => let val (b,(c,d),e) = a in (c,e) end"


delay_select := false;
Run.Nexec "fn a => case a of (b,(c,d),e) => (c,e)"

delay_select := true;
Run.Nexec "fn a => case a of (b,(c,d),e) => (c,e)"


Run.Nexec "fn (F,i,a,c) => let val b = (a,a) in if i then F b b else F c c end";

Run.Nexec "fn (F,i,j,a,c) => let val b = (a,a) in F (if i then b else c) (if j then b else c) end";


Run.Nexec "fn (a,b) => let val x = (a,1) in if b then (x,x) else raise a end"


Run.Nexec "fn p => raise (let val (a,_) = p in a end)"



Run.Nexec "fn (f,x) => f x"

Run.Nexec "fn (f,x) => f (case x of (a,b) => a + b)"


Run.Nexec "fn x => let val b::c = x in (b,c) end"
Run.Nexec "fn x => let val (b,c) = x in (b+c) end"
Run.Nexec "fn x => let val a = (x,x+1) in (a,1) end"








Run.Nexec "fn x => case (let val p = x*2 in (p,p+1) end) of (a,b) => b-a"





Run.Nexec "fn p => 1 + (let val (a,_) = p in a end)"
Run.Nexec "fn (f,p) => f (let val (a,_) = p in a end)"
Run.Nexec "fn (f,p) => f (let val (a,b) = p in (b-a) end)"

Run.Nexec "fn (f,p,q) => case (let val (p,_) = f in (p,q) end) of (a,b) => b-a"



Run.Nexec "fn p => 1 + (let val (i,t,e) = p in if i then t else e end)"





Run.Nexec "fn f => let val {foo,bar} = f 100 in {bar=bar,foo=foo} end"

Run.Nexec "fn f => f 100"
Run.Nexec "fn f => let val res = f 100 in res end"






Run.Nexec "fn (f,g) => g (let val u = f 100 in u end)"

Run.Nexec "fn (f,g) => g (let val u = f 100 in u end,200)"


Run.Nexec "let exception Foo val f = (fn x => (1 + (if x<10 then raise Foo (x+10) else x)) handle Foo y => y) in (f 11, f 9) end";

Run.Nexec "exception Foo";
Run.Nexec "decompile (fn x => (1 + (if x<10 then raise Foo (x+10) else x)) handle Foo y => y)";

Run.Nexec "decompile (fn x => raise Foo x)"


Run.Nexec "(let exception Foo val p = (1,Foo 2) in fn x => x + (raise (let val (_,e) = p in e end)) end) 1";

Run.Nexec "fn p => raise (let val (a,_) = p in a end)"
Run.Nexec "fn p => raise (let val (c,t,e) = p in if c then t else e end)";


Run.Nexec "decompile Embed.unembed'dec";

val prefixNML = "NML: ";
use "bind.ML";
Run.Nexec "decompile (fn (t,y) => let val (f,x) = t in f x y end)"; (* needs atom-bind version of let in cps *)





val prefixNML = "NML: ";
use "bind.ML";
print_cps := true;
print_code := true;
Run.Nexec "fun add x y = x + y";

Run.Nuse "nml_NonPrim.ML";






print_lang := true;
print_embed := true;
print_cps := true;
print_code := true;


use "nml_NonPrim.ML";
NonPrim.@ ([1,2,3],[4,5,6]);
NonPrim.concat ["one","two","three"];


val prefixNML = "NML: ";
use "bind.ML";
Run.Nuse "nml_NonPrim.ML";
Run.Nexec "decompile NonPrim.concat";
display_counts := true; trace_tailcalls := true;
Run.Nexec "NonPrim.concat [\"one\",\"two\",\"three\"]";
display_counts := false; trace_tailcalls := false

val prefixNML = "NML: ";
use "bind.ML";
Run.Nuse "nml_NonPrim.ML";
Run.Nexec "decompile NonPrim.@";
display_counts := true; trace_tailcalls := true;
Run.Nexec "NonPrim.@ ([1,2,3],[4,5,6])";
display_counts := false; trace_tailcalls := false

val prefixNML = "NML: ";
use "bind.ML";
Run.Nuse "nml_NonPrim.ML";
Run.Nuse "nml_NonPrim.ML";
Run.Nexec "decompile NonPrim.map";
display_counts := true; trace_tailcalls := true;
Run.Nexec "NonPrim.map (fn x => x * x) [1,2,3,4,5]";
display_counts := false; trace_tailcalls := false 


display_counts := true; trace_tailcalls := true;
Run.Nexec "Run.Nexec \"let fun mySum [] = 0 | mySum (x::xs) = x + mySum xs in mySum [1,2,3] end\"";
display_counts := false; trace_tailcalls := false




val prefixNML = "NML: ";
use "bind.ML";
run_compiler := false;
use "test.ML";
runTests tests;


val prefixNML = "NML: ";
use "bind.ML";
(*flipPrimOpt();*)
Run.Nexec "fun foo (a,b) (c,d) = ((op + a) * b) - (c mod d)";
Run.Nexec "decompile foo";
display_counts := true;
Run.Nexec "foo ((1,2),3) (4,5)";
display_counts := false; 



Run.Nexec "decompile (fn f => f 100 + 200)"
Run.Nexec "decompile (fn (f,g) => g (f 100,200))"
Run.Nexec "decompile (fn (f,g) => g (let val u = f 100 in u end,200))" (* needs let-let opt - current disabled *)



Run.Nexec "fun comp f g x = f (g x)";
Run.Nexec "fun thrice f = comp f (comp f f)";
Run.Nexec "fun revsub x y = y - x";
Run.Nexec "fun go N = thrice thrice (revsub 1) N";

Run.Nexec "decompile comp; decompile thrice; decompile revsub; decompile go";

display_counts := true;
Run.Nexec "go 100";
display_counts := false;




Run.Nexec "fun foo a b c d e = a + b + c + d + e"
Run.Nexec "val bar = foo 1 2"
Run.Nexec "val qaz = bar 3"
Run.Nexec "val qaz = foo 1 2 3"



run_compiler := true;
Run.Nexec "fun twice f x = f (f x)";
Run.Nexec "val plus2 = twice (fn x => x + 1)";

Run.Nexec "decompile twice"
Run.Nexec "decompile plus2"





run_compiler := false;
Run.Nexec "exception Zero; fun squareR x = if x = 0 then raise Zero else x * x";
run_compiler := true;
Run.Nexec "fun square x = squareR x handle Zero => 999";

run_compiler := false; Run.Nexec "val x = (square 0, square 7)";
run_compiler := true; Run.Nexec "val y = (square 0, square 7)";




Run.Nexec "val rec odd = fn a => not (even a) and even = fn b => b=0 orelse (odd (b-1))";
Run.Nexec "map (fn x => (odd x, even x)) [0,1,2,3,4,5]";





Run.Nexec "fn a => let val b = a + 1 val c = a + b in c + 3 end"
Run.Nexec "val (a,b) = (3+1,4) and (c,d) = (5,6+2)"

Run.Nexec "decompile (fn f => let exception a exception b = a exception a in f (a,b) end)"
Run.Nexec "decompile (fn f => let exception a in (a, fn a => true | _ => false) end)"

Run.Nuse "regs/gen-exception.ML";

Run.Nuse "nml_NonPrim.ML";

Run.Nexec "decompile (fn (f,a) => case (a,f) of (b,c) => c b)";
Run.Nexec "decompile (fn (f,a) => let val (b,c) = (a,f) in c b end)";

Run.Nexec "decompile (fn a => case (a,a) of (b,c) => b*c)";
Run.Nexec "decompile (fn a => let val (b,c) = (a,a) in b*c end)";





Run.Nexec "decompile (fn (f,x) => let val a = x val b = a in f b end)"
Run.Nexec "decompile (fn (f,x) => let exception a exception b = a in f b end)"




Run.Nexec "foo";
Run.Nexec "foo.bar";
Run.Nexec "Parser.bar";
Run.Nexec "Parser.foo.bar";
Run.Nexec "fn foo.bar => 1";
Run.Nexec "fn Parser.bar => 1";
Run.Nexec "fn Parser.prog => 1";
Run.Nexec "decompile Lex.isSymid"
Run.Nexec "decompile Compile.generateCode"
Run.Nexec "decompile Parser.prog"



Run.Nuse "regs/gen-exception.ML";





(fn x => let local val x = 7 in val y = x + x end in (x,y) end) 42;
Run.Nexec "(fn x => let local val x = 7 in val y = x + x end in (x,y) end) 42";

Run.Nexec "let val bad = fn () => 1 val A = bad() val B = bad() in 1 end"

Run.Nexec "let val bad = fn x => x+1 val A = bad (bad 5) in 1 end"

(fn x => let local val x = 7 in val y = (x,x) end in (x,y) end) 42;
Run.Nexec "(fn x => let local val x = 7 in val y = (x,x) end in (x,y) end) 42";

Run.Nexec "let val oknow = 1 val A = oknow in 1 end"

Run.Nexec "()"
Run.Nexec "(1)"
Run.Nexec "(1,2)"
Run.Nexec "{a=1}"

Run.Nexec "datatype lala = LALA";
Run.Nexec "val foo = LALA";

Run.Nexec "structure S = struct val moo = 42 end";
Run.Nexec "open S;"
Run.Nexec "val bar = moo + 1";

Run.Nuse "nml_NonPrim.ML";
Run.Nexec "open NonPrim";

Run.Nexec "fun map f [] = [] | map f (x::xs) = f x :: map f xs"

Run.Nexec "[1]"
Run.Nexec "[1,2,3]"
Run.Nexec "map (fn x => (x,x)) [1,2,3]"

Run.Nexec "1"
Run.Nexec "val p = (1,2)"
Run.Nexec "val (a,b) = (1,2)"
Run.Nexec "val a = 1 and b = 2"
Run.Nexec "1+2"

Run.Nexec "let val thrice = fn f => fn x => f (f (f x)) in thrice thrice (fn x => x-1) 100 end"

Run.Nexec "fn (f,g,h,a,b) => let val fa = f a val gb = g b in g (fa,gb) end"

Run.Nexec "val foo = 1 + 2"
Run.Nexec "fn (f,g,h,a,b) => g (f a, g b)"
Run.Nexec "fn (f,g,h,a,b) => let val p = (f a, g b) in g p end"
Run.Nexec "fn (a,b,c) => if a then b else c"
Run.Nexec "fn (a,b,c,d,e,f) => (if a then b else c, if d then e else f)" (* shows cont duplication *)
Run.Nexec "fn x => let val foo = 1 + 2 in foo+x end"
Run.Nexec "1 + 2"
Run.Nexec "let val a = 3 in a + 1 end"
Run.Nexec "val res = let val a = 3 in a + 1 end"


Run.Nuse "nml_NonPrim.ML";



Run.Nexec "fn [f,g,a,b,c] => f a (g b (f c))"

Run.Nexec "fn (f,fs) => f"
Run.Nexec "fn op::(f,fs) => fs"

Run.Nexec "fn op::p => p"
Run.Nexec "fn (f::fs) => f"

Run.Nexec "fn [f] => f"


Run.Nexec "fn {f} => f"



Run.Nexec "val twice = fn f => fn x => f ( f x)"

Run.Nexec "fun twice f x = f ( f x)"




(* val rec *)
let val rec mySum = fn [] => 0 | (x::xs) => x + mySum xs in mySum [1,2,3] end;
Run.Nexec "let val rec mySum = fn [] => 0 | (x::xs) => x + mySum xs in mySum [1,2,3] end";




(* make this a regs test... NJ doesn't support it *)
Run.Nexec "val rec (odd,even) = (fn a => not (even a),fn b => b=0 orelse (odd (b-1)))";
Run.Nexec "map (fn x => (odd x, even x)) [0,1,2,3,4,5]";



Run.Nexec "val res = let val a = 3 in a + 1 end";
Run.Nexec "val res = (fn x => x)"
Run.Nexec "val res = fn b => let val a = b+3 in a + 1 end";
Run.Nexec "fun res b = if b then 1 else 2"


Run.Nexec "1+2";
Run.Nexec "Run.Nexec \"1+2\"";


(* Try NML-inner-squared *)
Run.Nexec "Run.Nuse \"nml_NonPrim.ML\"";
Run.Nexec "Run.Nexec \"open NonPrim\"";

fun Run2_Nuse s = Run.Nexec ("Run.Nuse \"" ^ s ^ "\"");
	
map Run2_Nuse pervasives;
map Run2_Nuse nml_sources; (* takes ages, aprox 15/20 mins or maybe more *)

Run.Nexec "Run.Nexec \"val prefixNML = \\\"NML-inner-squared: \\\";\"";
Run.Nexec "Run.Nuse \"bind.ML\""; (* another 9 mins *)

Run.Nexec "Run.Nexec \"Run.Nexec \\\"let fun mySum [] = 0 | mySum (x::xs) = x + mySum xs in mySum [1,2,3] end\\\"\""; (* 5ish mins *)

Run.Nexec "Run.Nexec \"Run.Nexec \\\"7\\\"\""; (* only a minute! *)


(*
Run.Nexec "val a = 5";
Run.Nexec "val b = a";
Run.Nexec "val c = let val b = a in b end";
*)

Run.Nuse ("nml_NonPrim.ML");
Run.Nexec ("open NonPrim");


(*
Run.Nexec "val a = ref 5";
Run.Nexec "!a";
Run.Nexec "[1,2]@[3,4]";
*)
map Run.Nuse pervasives;
map Run.Nuse nml_sources;
Run.Nexec "val prefixNML = \"NML-inner: \";"; 
Run.Nuse "bind.ML";




(fn f => f 3)(fn x => x + 1);
Run.Nexec "(fn f => f 3)(fn x => x + 1)";
Run.Nexec "Run.Nexec \"(fn f => f 3)(fn x => x + 1)\"";


local val x = 1 in val y = (fn x => x) 2 + x end; (* y=3 is correct *)
Run.Nexec "local val x = 1 in val y = (fn x => x) 2 + x end" (* y=2 is bug *)





let val x = 1 in (fn x => x) 2 + x end;
Run.Nexec "let val x = 1 in (fn x => x) 2 + x end"



signature BMARK = 
sig 
	type generation
	val show : (string -> unit) -> generation -> unit
	val gun : generation 
	val nthgen : generation -> int -> generation 
end;

use "/home/nic/sml-resources/BENCHMARKS/SML97/programs/life/life.sml";
let open Main in show print gun end;
let open Main in show print (nthgen gun 1) end;;

use "BUILTIN.ML";
use "bind.ML";

Run.Nuse ("nml_NonPrim.ML");
Run.Nexec ("open NonPrim");
Run.Nexec "signature BMARK = sig type generation; val show : (string -> unit) -> generation -> unit; val gun : generation val nthgen : generation -> int -> generation end";
Run.Nuse "/home/nic/sml-resources/BENCHMARKS/SML97/programs/life/life.sml";
Run.Nexec "let open Main in show print gun end";
Run.Nexec "let open Main in show print (nthgen gun 1) end";



signature BMARK = sig val testit : TextIO.outstream -> unit end;
use "/home/nic/sml-resources/BENCHMARKS/SML97/programs/life/life.sml"
Main.testit TextIO.stdOut;

Run.Nuse ("nml_NonPrim.ML");
Run.Nexec ("open NonPrim");
Run.Nexec "signature BMARK = sig val testit : TextIO.outstream -> unit end;";
Run.Nuse "/home/nic/sml-resources/BENCHMARKS/SML97/programs/life/life.sml";
Run.Nexec "Main.testit TextIO.stdOut;";*





let exception MyE of int in ((raise MyE 1) + 2) handle MyE x => x end;
Run.Nexec "let exception MyE of int in ((raise MyE 1) + 2) handle MyE x => x end";
Run.Nexec "Run.Nexec \"let exception MyE of int in ((raise MyE 1) + 2) handle MyE x => x end\"";


Run.Nuse ("nml_NonPrim.ML");
Run.Nexec ("open NonPrim");
Run.Nexec "map (fn x => x * x) [1,2,3,4,5]";

Run.Nexec "Run.Nuse (\"nml_NonPrim.ML\"); Run.Nexec (\"open NonPrim\"); Run.Nexec \"map (fn x => x * x) [1,2,3,4,5]\";";


Run.Nexec "Run.Nexec \"map (fn x => x * x) [1,2,3,4,5]\";";



Run.Nuse ("nml_NonPrim.ML");
Run.Nexec "open NonPrim;";
Run.Nexec "val r = ref 11 ; (!r, (r := 22; !r))";
Run.Nexec "[1,2,3] @ [4,5,6]";
Run.Nexec "map (fn x => x * x) [1,2,3,4,5]";
Run.Nexec "rev [1,2,3,4,5]";
Run.Nexec "(1 <> 1, 2 <> 3)";
Run.Nexec "length [11,12,13]"


Run.Nexec ("TextIO.stdOut");
Run.Nexec "exception foo of string;";

Run.Nuse "test.ML";
Run.Nuse "regs/append-example.ML"

Run.Nuse (preludeDir^"Prel.ML");
Run.Nexec "Prel.link [[1,2,3],[4,5,6],[],[7],[8,9]]";

Run.Nuse "../misc-ml/thrice.ML";
Run.Nexec "let open XXX in echo(stringOfInt(1+result)) end";

use "../misc-ml/try-assign-op.ML"		  
Run.Nuse "../misc-ml/try-assign-op.ML"	  

Run.Nuse "../misc-ml/nqueens.ML";
Run.Nexec "nqueens 3";
Run.Nexec "nqueens 4";
Run.Nexec "nqueens 5";


(* parse/pp/use tests... *)		  
Run.tpp' "VALUE.ML";
use ",g/VALUE.ML";
		  
Run.tpp' "BUILTIN.ML";
use ",g/BUILTIN.ML";

Run.tpp' "EVAL.ML";
use ",g/EVAL.ML";

Run.tpp' "../misc-ml/nqueens.ML";
use ",g/nqueens.ML";
nqueens 4;
nqueens 5;

Run.tpp' "top.ML"; (* this file *)
Run.tpp' "PFV.ML";
Run.tpp'' "PFV.ML";
Run.tpp' "../misc-ml/thrice.ML";
Run.tpp' "../misc-ml/nqueens.ML"; (* has symbolic idents *)
Run.tpp' "../prelude/Prel.ML";
Run.tpp' "../prelude/ASSOC.ML";
Run.tpp' "TOK.ML";
Run.tpp' "LEX.ML";
Run.tpp' "LANG.ML";
Run.tpp' "PARSER.ML";

Run.tpp "../xxx.ML";

Run.echo_tl {IN=preludeDir^"option.ML"};
Run.tpp (preludeDir^"option.ML");
