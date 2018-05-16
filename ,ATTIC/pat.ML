(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  37.    con
  49.    pat
  65.    rule
  74.    (static) descriptions / SMA / staticMatch
  94.    staticMatch
  117.   access / decision
  132.   decision
  150.   aenv - access env
  161.   refactor matchCompiler
  233.   test...
  247.   lam example from paprt
  290.   examples
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

val preludeDir = "/home/nic/project/prelude/"
fun prefixPrelude s = preludeDir^s
val pervasives = map prefixPrelude ["option.ML","either.ML"]
;map use pervasives
;map (use o prefixPrelude) ["Prel.ML", "QLAYOUT.ML", "MISCLAY.ML"];
structure QLayout = QLAYOUT(structure Prel = Prel)
structure Misclay = MISCLAY(structure Prel = Prel
							structure QLayout = QLayout)
open Prel Misclay
infix // @@ ++ `` //``


fun typeError s = abort ("typeError/"^s)

fun echo'lay lay x =
	(prefixed_output_layout "MC: " (squash_to 100 (lay x));
	 echo "")

fun nth _ [] = abort "nth/[]"
  | nth n (x::xs) = if (n=0) then x else nth (n-1) xs (* interesting for opt: xs only used in one arm! *)


(*----------------------------------------------------------------------
INDEX: con
----------------------------------------------------------------------*)
(*
datatype con = CON of {name: string, span: int option, isExcon:bool}
fun isExcon'con (CON{isExcon,...}) = isExcon
fun span'con (CON{span,...}) = span
fun name'con (CON{name,...}) = name
fun makeCons names = map (fn X => CON {isExcon=false,name=X,span=Some(length names)}) names
*)

datatype con = CON of int * string list * {isExcon:bool}
fun isExcon'con (CON (_,_,{isExcon})) = isExcon
fun span'con (CON(_,names,_)) = Some (length names)
fun name'con (CON(i,names,_)) = nth i names
fun makeCons names = map (fn i => CON (i,names,{isExcon=false})) (upto (0,length names - 1))

fun sibs'con (CON(_,names,_)) = makeCons names

fun lay'con con = Word (name'con con)


(*----------------------------------------------------------------------
INDEX: pat
----------------------------------------------------------------------*)

type id = string
datatype pat = PAT'Var of id | PAT'Con of con * pat | PAT'Tup of pat list

val atomic'pat = fn PAT'Con _ => false | _ => true

fun layBracIfNonAtomic A L x = if A x then L x else layBracket (L x)

val rec lay'pat =
	fn PAT'Var x => Word x
	 | PAT'Tup pats => layTuple (map lay'pat pats)
	 | PAT'Con (con,pat) => lay'con	con ++ layBracIfNonAtomic atomic'pat lay'pat pat

(*----------------------------------------------------------------------
INDEX: rule
----------------------------------------------------------------------*)

datatype 'a rule = RULE of pat * 'a
fun lay'rule lay (RULE (pat,rhs)) = lay'pat pat ++ Word "=>" ++ lay rhs

fun lay'rules lay rules = layVert (map (lay'rule lay) rules)

(*----------------------------------------------------------------------
INDEX: (static) descriptions / SMA / staticMatch
----------------------------------------------------------------------*)

datatype des 
  = DES'NoInfo				(* DES'NoInfo ===? DES'NotCon [] *)
  | DES'Con     of con * des
  | DES'NotCon  of con list
  | DES'Tup     of des list

fun addNeg'des des con =
	case des of
		DES'NoInfo => DES'NotCon [con]
	  | DES'NotCon cons => if (exists (curry op= con) cons) then abort "addNeg/repeated nocon" 
						   else DES'NotCon (con::cons)
	  | DES'Tup _ => typeError "addNeg/tup"
	  | DES'Con con1 => if abort "addNeg/con - is possible for excon - have pos&neg description"
						then DES'Con con1 
						else DES'NotCon [con]

(*----------------------------------------------------------------------
INDEX: staticMatch
----------------------------------------------------------------------*)

datatype staticMatchAnswer 
  = SMA'Yes of des
  | SMA'No
  | SMA'Maybe

fun staticMatch pcon des =
	case des of
		DES'NoInfo => SMA'Maybe
	  | DES'Con (dcon,des) => if (pcon=dcon) then (SMA'Yes des) else if (isExcon'con dcon) then SMA'Maybe else SMA'No
	  | DES'NotCon cons => if (exists (curry op= pcon) cons) then SMA'No
						   else (case (span'con pcon) of
									 None => SMA'Maybe (* infinite span *)
								   | Some n =>
									 let val m = length cons + 1
										 val () = if (m>n) then abort "span/NoCon" else ()
									 in if (n = m) then (SMA'Yes DES'NoInfo) else SMA'Maybe
									 end)
	  | DES'Tup _ => typeError "des/tup"

(*----------------------------------------------------------------------
INDEX: access / decision
----------------------------------------------------------------------*)

datatype access 
  = ACCESS'Obj
  | ACCESS'Tup  of int * access
  | ACCESS'Con  of con * access (* con just for checking/layout *)

fun lay'access objLayout access =
	case access
	 of ACCESS'Obj => objLayout
	  | ACCESS'Con (con,access) => lay'access (objLayout @@ Sep "." @@ lay'con con) access
	  | ACCESS'Tup (n,access) => lay'access (objLayout @@ Sep "." @@ layInt n) access

(*----------------------------------------------------------------------
INDEX: aenv - access env
----------------------------------------------------------------------*)
datatype aenv = AENV of (id * access) list
val aenv'empty = AENV []
fun aenv'cons p (AENV ps) = AENV (p::ps)
fun lay'aenv objLayout (AENV ps) =
	layBrace (layCommaSep (map (fn (x,access) => Word x @@ Sep "=" @@ lay'access objLayout access) ps))

	 
(*----------------------------------------------------------------------
INDEX: decision
----------------------------------------------------------------------*)

datatype 'a decision 
  = DEC'Fail 
  | DEC'Succ of 'a 
  | DEC'If of access * con * 'a decision * 'a decision
  | DEC'Assert of access * con * 'a decision
  | DEC'Switch of access * (con * 'a decision) list * 'a decision option

fun lay'decision lay objLayout decision =
	case decision
	 of DEC'Fail => Word "Fail"
	  | DEC'Succ x => lay x
	  | DEC'If (access,con,dThen,dElse) => 
		(Word "if" ++ layBracket (lay'access objLayout access ++ Word "~" ++ lay'con con))
		// Word "then" ++ lay'decision lay objLayout dThen
		// Word "else" ++ lay'decision lay objLayout dElse
	  | DEC'Assert (access,con,decision) => 
		(Word "assert" ++ layBracket (lay'access objLayout access ++ Word "~" ++ lay'con con))
		// lay'decision lay objLayout decision
	  | DEC'Switch (access,arms,defopt) =>
		let val (suffix,deflay) = case defopt 
								   of None => ("*",Blank)
									| Some def => ("",Word "def:" ++ lay'decision lay objLayout def)
		in
			(Word ("switch"^suffix) ++ layBracket (lay'access objLayout access))
			// layVert (map (fn (con,arm) => (lay'con con ++ Right ":") ++ lay'decision lay objLayout arm) arms)
			// deflay
		end


(* full-switch-completion *)
fun dec'switch (a,arms,None) = abort "dec'switch/def=None"
  | dec'switch (a,arms,Some def) = 
	let val c1 = case arms of [] => abort "dec'switch/arms=[]" | (c1,_)::_ => c1
		val missing_sibs = filter (fn sib => not (exists (fn (con,_) => con=sib) arms)) (sibs'con c1)
	in case missing_sibs
		of [] => abort "dec'switch/missing_sibs=[]"
		 | [sib] => DEC'Switch (a,arms@[(sib,def)],None)
		 | _ => DEC'Switch (a,arms,Some def)
	end


(* post-switchification *)
fun dec'if (a,c,t,e) =
	let val (arms,def) = case e 
						  of DEC'Switch (a2,arms,def) => if (a=a2) then (arms,def) else ([],Some e)
						   (*| DEC'Assert (a2,c2,t2) => if (a=a2) then ([(c2,t2)],None) else ([],Some e)*)
						   | _ => ([],Some e)
	in dec'switch (a,(c,t)::arms,def)
	end



(*----------------------------------------------------------------------
INDEX: refactor matchCompiler
----------------------------------------------------------------------*)

fun deTuple pats access des =
	case des 
     of DES'NoInfo      => map (fn (pat,i) => (pat, ACCESS'Tup (i,access), DES'NoInfo)) (enumerateFrom 0 pats)
      | DES'Tup dess    => map (fn ((pat,des),i) => (pat, ACCESS'Tup (i,access), des)) (enumerateFrom 0 (zipPair pats dess))
      | DES'Con _       => typeError "deTuple/con"
      | DES'NotCon _    => typeError "deTuple/not-con"


val full_switch = ref true (* compile directly to full-switch...*)

type 'a S       = (des * aenv -> 'a decision)
type 'a F       = (des -> 'a decision)
fun Umatch {match,succeed} : ('a S * 'a F * pat * access * des * aenv -> 'a decision) =
    fn (S,F,pat,access,des,aenv) =>
    case pat 
	 of PAT'Var x => S (des,aenv'cons (x,access) aenv)
	  | PAT'Tup pats => succeed (fn (dess,aenv) => S (DES'Tup dess,aenv),
								 fn dess => F (DES'Tup dess),
								 deTuple pats access des,
								 aenv)
	  | PAT'Con(con,pat) => let fun S' subdes = match (fn (des,aenv) => S (DES'Con (con,des),aenv),
													   fn des => F (DES'Con (con,des)),
													   pat,
													   ACCESS'Con (con,access),
													   subdes,
													   aenv)
								fun ARM sib = (sib, if (sib=con) 
													then S' DES'NoInfo 
													else F (DES'Con (sib,DES'NoInfo)))
							 in
								 case (staticMatch con des)
								  of SMA'Yes subdes => (*DEC'Assert (access,con,S' subdes)*) S' subdes
								   | SMA'No => F des
								   | SMA'Maybe => if (!full_switch)
												  then DEC'Switch (access, map ARM (sibs'con con), None)
												  else dec'if (access,con, S' DES'NoInfo, F (addNeg'des des con))
							 end

type 'a SS       = (des list * aenv -> 'a decision)
type 'a FF       = (des list -> 'a decision)
fun Usucceed {match,succeed} : ('a SS * 'a FF * (pat * access * des) list * aenv -> 'a decision) =
	fn (S,F,pads,aenv) =>
	case pads
	 of [] => S ([],aenv)
	  | ((pat,access,des)::pads) => match (fn (des,aenv) => succeed (fn (dess,aenv) => S (des::dess,aenv),
																	 (fn dess => F (des :: dess)),
																	 pads,aenv),
										   (fn des => F (des:: map (fn (_,_,des)=>des) pads)),
										   pat,access,des,aenv)


(* fix *)
fun match x = Umatch {match=match,succeed=succeed} x
and succeed x = Usucceed {match=match,succeed=succeed} x
val matchCompiler : (unit -> 'a rule list -> (aenv * 'a) decision) = 
	fn () => 
	   let val topAccess = ACCESS'Obj
		   val topAenv = aenv'empty
		   fun fail des [] = DEC'Fail
			 | fail des (RULE(pat,rhs)::rules) = match (fn (des,aenv) => DEC'Succ (aenv,rhs),
														(fn des => fail des rules),
														pat,topAccess,des,topAenv)
	   in fail DES'NoInfo
	   end


(*----------------------------------------------------------------------
INDEX: seperate S/F cont manipulationa
----------------------------------------------------------------------*)

(* succ/fail continuation monad, passing internal info *)
datatype ('a,'b,'c) sf = SF of ('a * 'b -> 'c) * ('b -> 'c) * 'b -> 'c
fun deSF (SF x) = x
fun resultSF a = SF (fn (S,F,b) => S(a,b))
fun bindSF (sf,M) = SF (fn (S,F,b) => deSF sf (fn (a,b) => deSF (M a) (S,F,b), F, b))
val failSF = SF (fn (S,F,b) => F b)
fun altSF (m1,m2) = SF (fn (S,F,b) => deSF m1 (S, fn b => deSF m2 (S,F,b), b))
fun sequenceSF (m1,m2) = bindSF (m1, fn x1 => bindSF (m2, fn x2 => resultSF (x1,x2)))
fun mapSF (sf,A) = bindSF (sf, resultSF o A)
infix ## >> >>> **
val op ## = altSF
val op >> = bindSF
val op >>> = mapSF
val op ** = sequenceSF
fun execD sf def b = deSF sf (fn (a,_) => a,fn _ => def(),b)
(*fun execE sf b = deSF sf (This,That,b)
fun execE sf = execD ((sf ** getPartial >>> This) ## (getPartial >>> That)) (fn _ => abort "execE/def")*)

val getPartial = SF (fn (S,F,b) => S(b,b))
(*
fun setPartial b = SF (fn (S,F,_) => S((),b))
fun mapPartial B1 B2 sf = SF (fn (S,F,b) => deSF sf (fn (a,b) => S (a,B1 b), fn b => F (B2 b), b))
fun setPartial b sf = SF (fn (S,F,_) => deSF sf (S,F,b))
*)
fun convPartial (there,back) sf = SF (fn (S,F,b) => deSF sf (fn (a,b) => S (a, back b), F o back, there b))



(*----------------------------------------------------------------------
INDEX: recode match compiler using SF monad - clearer?
----------------------------------------------------------------------*)

fun deTuple2 pats access des =
	case des 
     of DES'NoInfo      => (map (fn (pat,i) => (pat, ACCESS'Tup (i,access))) (enumerateFrom 0 pats), copy (length pats) DES'NoInfo)
      | DES'Tup dess    => (map (fn (pat,i) => (pat, ACCESS'Tup (i,access))) (enumerateFrom 0 pats),dess)
      | DES'Con _       => typeError "deTuple/con"
      | DES'NotCon _    => typeError "deTuple/not-con"


fun withinTuple pats access sf = 
	getPartial >> (fn des => let val (pas,dess) = deTuple2 pats access des
							 in convPartial (fn _ => dess, DES'Tup) (sf pas)
							 end)


fun headPartial sf =
	getPartial >> (fn [] => abort "headPartial"
					| des::dess =>
					  convPartial (fn _ => des, fn des => des :: dess) sf)

fun tailPartial sf =
	getPartial >> (fn [] => abort "headPartial"
					| des::dess =>
					  convPartial (fn _ => dess, fn dess => des :: dess) sf)


	
fun Umatch {match,succeed} : (pat * access * aenv -> (aenv,des,'a decision) sf) =
    fn (pat,access,aenv) =>
    case pat 
	 of PAT'Var x => resultSF (aenv'cons (x,access) aenv)
	  | PAT'Tup pats => withinTuple pats access (fn pas => succeed (pas,aenv))


fun Umatch {match,succeed} : (pat * access * aenv -> (aenv,des,'a decision) sf) =
    fn (pat,access,aenv) =>
    case pat 
	 of 
		PAT'Con(con,pat) => let fun S' subdes = match (fn (des,aenv) => S (DES'Con (con,des),aenv),
													   fn des => F (DES'Con (con,des)),
													   pat,
													   ACCESS'Con (con,access),
													   subdes,
													   aenv)
								fun ARM sib = (sib, if (sib=con) 
													then S' DES'NoInfo 
													else F (DES'Con (sib,DES'NoInfo)))
							 in
								 case (staticMatch con des)
								  of SMA'Yes subdes => (*DEC'Assert (access,con,S' subdes)*) S' subdes
								   | SMA'No => F des
								   | SMA'Maybe => if (!full_switch)
												  then DEC'Switch (access, map ARM (sibs'con con), None)
												  else dec'if (access,con, S' DES'NoInfo, F (addNeg'des des con))
							 end



fun Usucceed {match,succeed} : ((pat * access) list * aenv -> (aenv, des list, 'a decision) sf) =
	fn (pas,aenv) =>
	case pas
	 of [] => resultSF aenv
	  | ((pat,access)::pads) => 
		headPartial (match (pat,access,aenv)) >> (fn aenv => tailPartial (succeed (pads,aenv)))



(* fix *)
fun match x = Umatch {match=match,succeed=succeed} x
and succeed x = Usucceed {match=match,succeed=succeed} x

val matchCompiler : (unit -> 'a rule list -> (aenv * 'a) decision) = 
	fn () => 
	   let val topAccess = ACCESS'Obj
		   val topAenv = aenv'empty
		   fun fail des [] = DEC'Fail
			 | fail des (RULE(pat,rhs)::rules) = match (fn (des,aenv) => DEC'Succ (aenv,rhs),
														(fn des => fail des rules),
														pat,topAccess,des,topAenv)
	   in fail DES'NoInfo
	   end



(*----------------------------------------------------------------------
INDEX: test...
----------------------------------------------------------------------*)
fun test layEnvAndRhs rules =
	let val decision = matchCompiler () rules
		val lay = layInt
		fun L rules  = (lay'rules layInt rules)
					   // Word "---->"
					   // lay'decision layEnvAndRhs (Word "R") decision
	in echo'lay L rules
	end
val test1 = test (fn (aenv,n) => layPair (lay'aenv (Word "R") aenv,layInt n))
val test2 = test (fn (_,n) => layInt n)


(*----------------------------------------------------------------------
INDEX: examples
----------------------------------------------------------------------*)
fun makeNullConApp con = PAT'Con (con,PAT'Tup [])
fun makeNullConPats xs = map makeNullConApp (makeCons xs)
val (A,B) = case (makeNullConPats ["A","B"]) of [c1,c2] => (c1,c2) | _ => abort "A/B" (*span=2*)
val (X,Y,Z) = case (makeNullConPats ["X","Y","Z"]) of [c1,c2,c3] => (c1,c2,c3) | _ => abort "X/Y/Z" (*span=3*)
val (P,Q,R,S) = case (makeNullConPats ["P","Q","R","S"]) of [c1,c2,c3,c4] => (c1,c2,c3,c4) | _ => abort "P/Q/R/S" (*span=3*)
val u = PAT'Var "_"


full_switch := true


test2
[
 RULE (PAT'Tup [X, X, X], 100),
 RULE (PAT'Tup [Y, Y, Y], 200),
 RULE (PAT'Tup [Z, Z, Z], 300)
]

test2
[
 RULE (PAT'Tup [X, X, u], 100),
 RULE (PAT'Tup [u, X, X], 200),
 RULE (PAT'Tup [X, u, X], 300)
]

test2
[
 RULE (PAT'Tup [X, u], 100),
 RULE (PAT'Tup [u, X], 200),
 RULE (PAT'Tup [Y, u], 300),
 RULE (PAT'Tup [u, Y], 400)
]

test2
[
 RULE (PAT'Tup [X, u], 100),
 RULE (PAT'Tup [u, X], 200),
 RULE (PAT'Tup [Y, u], 300),
 RULE (PAT'Tup [u, Y], 400),
 RULE (PAT'Tup [Z, Z], 500)
]


test2
[
 RULE (PAT'Tup [A, A], 100),
 RULE (PAT'Tup [A, B], 200),
 RULE (PAT'Tup [B, A], 300),
 RULE (PAT'Tup [B, B], 400)
]

test2
[
 RULE (PAT'Tup [X, X], 100),
 RULE (PAT'Tup [X, Y], 200),
 RULE (PAT'Tup [Y, X], 300),
 RULE (PAT'Tup [Y, Y], 400)
]

test2
[
 RULE (PAT'Tup [P, P], 100),
 RULE (PAT'Tup [P, Q], 200),
 RULE (PAT'Tup [Q, P], 300),
 RULE (PAT'Tup [Q, Q], 400)
]

test2
[
 RULE (PAT'Tup [A, u], 100),
 RULE (PAT'Tup [u, A], 200),
 RULE (PAT'Tup [B, u], 300),
 RULE (PAT'Tup [u, B], 400)
]


test2
[
 RULE (PAT'Tup [A, A, A], 100),
 RULE (PAT'Tup [B, B, B], 200),
 RULE (PAT'Tup [A, A, B], 300),
 RULE (PAT'Tup [A, B, B], 400)
]

test2
[
 RULE (PAT'Tup [u, A], 100),
 RULE (PAT'Tup [B, A], 200)
]

test2
[
 RULE (PAT'Tup [A, u, A], 100),
 RULE (PAT'Tup [u, B, A], 200)
]

(*----------------------------------------------------------------------
INDEX: lam example from paprt
----------------------------------------------------------------------*)
val (cVar,cLam,cApp,cLet) = case (makeCons ["Var","Lam","App","Let"]) of [c1,c2,c3,c4] => (c1,c2,c3,c4) | _ => abort "lam"
fun Var p = PAT'Con (cVar, PAT'Tup [p])
fun Lam (p,q) = PAT'Con (cLam, PAT'Tup [p,q])
fun App (p,q) = PAT'Con (cApp, PAT'Tup [p,q])
fun Let (p,q,r) = PAT'Con (cLet, PAT'Tup [p,q,r])
val x = PAT'Var "x"
val y = PAT'Var "y"
val z = PAT'Var "z"
val v = PAT'Var "v"
val w = PAT'Var "w"

test2
(map RULE [
(Var x, 111),
(Lam (x, Var y), 222),
(Lam (x, Lam (y,z)), 333),
(Lam (x, App (y,z)), 444),
(App (Lam (x, y), z), 555),
(App (App (x, y), z), 666),
(Let (x, Let (y, z, v), w), 777),
(Lam (x, Let (y, z, v)), 888),
(Let (x, y, App(z, v)), 999),
(App (App (Lam (x, Lam (y, z)), v), w), 1010) (* redundant *)
])


(* move last rule to top - rhs 666 is duplicated *)
test2
(map RULE [
(App (App (Lam (x, Lam (y, z)), v), w), 1010),
(Var x, 111),
(Lam (x, Var y), 222),
(Lam (x, Lam (y,z)), 333),
(Lam (x, App (y,z)), 444),
(App (Lam (x, y), z), 555),
(App (App (x, y), z), 666),
(Let (x, Let (y, z, v), w), 777),
(Lam (x, Let (y, z, v)), 888),
(Let (x, y, App(z, v)), 999)
])


(*----------------------------------------------------------------------
INDEX: exponential explode
----------------------------------------------------------------------*)

test2
[
 RULE (PAT'Tup [A,A,x,x,x,x,x,x], 1000)
,RULE (PAT'Tup [x,x,A,A,x,x,x,x], 1001)
,RULE (PAT'Tup [x,x,x,x,A,A,x,x], 1002)
,RULE (PAT'Tup [x,x,x,x,x,x,A,A], 1003)
(*,RULE (PAT'Tup [A,B,A,B,A,B,A,B], 999)*)
]

test2
[
 RULE (PAT'Tup [A,x,x,x,x,x,x], 1000),
 RULE (PAT'Tup [x,x,A,x,x,x,x], 1001),
 RULE (PAT'Tup [x,x,x,x,A,x,x], 1002),
 RULE (PAT'Tup [x,x,x,x,x,x,A], 1003)
]



test2
[
 RULE (PAT'Tup [A, A], 100),
 RULE (x, 200) (* duplicated *)
]


test2
[
 RULE (PAT'Tup [A, A, A], 100),
 RULE (PAT'Tup [B, B, B], 200),
 RULE (PAT'Tup [A, A, B], 300),
 RULE (PAT'Tup [A, B, B], 400)
]
