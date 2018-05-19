(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  52.    v1 - almost as given in paper
  83.    v2 - nest function match/succeed in fail - avoid passing rhs/rules
  114.   v3 - collect env of var bindings
  148.   descriptions / SMA / staticMatch
  185.   (no good) context - like paper, except con/tup are confused, so...
  215.   context - with tuples
  255.   v4 - instrument with static matching (keep env)
  317.   v5 - restructure to avoid work lists (no static matching)
  350.   Add back static-matcing. Now des(scriptions) are build on the way up (if we fail)
  374.   v6 - no worklists / with static matching
  427.   v7 - restructure to use succ/fail continuations, instead of matchres
  470.   stage computation (partial eval) - pat/des - static, obj - dynamic
  476.   access / decision
  514.   v8 - stage computation - pat/des - static, obj - dynamic
  559.   layout
  619.   have a play...
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


datatype con = CON of {name: string, span: int option, isExcon:bool}

fun isExcon'con (CON{isExcon,...}) = isExcon
fun span'con (CON{span,...}) = span


datatype obj = OBJ'Con of con * obj | OBJ'Tup of obj list | OBJ'Other

type id = string

datatype pat = PAT'Var of id | PAT'Con of con * pat | PAT'Tup of pat list

datatype 'a rule = RULE of pat * 'a

(*----------------------------------------------------------------------
INDEX: v1 - almost as given in paper
----------------------------------------------------------------------*)

type work = (pat list * obj list) list

val matchTop1 : (obj -> 'a rule list -> 'a option) =
	fn topObj => 
	   let val topWork : work = []
		   fun fail rules =
			   case rules of
				   [] => None
				 | RULE(pat,rhs)::rules => match pat topObj topWork rhs rules
		   and match pat obj (work:work) rhs rules =
			   case (pat,obj) of
				   (PAT'Var x,_) => succeed work rhs rules
				 | (PAT'Tup pats, OBJ'Tup objs) => succeed ((pats,objs)::work) rhs rules
				 | (PAT'Con(pcon,pat),OBJ'Con(ocon,obj)) => if (pcon=ocon) then match pat obj work rhs rules
															else fail rules
				 | (PAT'Tup _, _) => typeError "match/tup"
				 | (PAT'Con _, _) => typeError "match/con"
		   and succeed work rhs rules =
			   case work 
				of [] => Some rhs
				 | ([],[])::work => succeed work rhs rules
				 | (pat::pats,obj::objs)::work => match pat obj ((pats,objs)::work) rhs rules
				 | _ => typeError "succeed/arity"
	   in
		   fail
	   end

(*----------------------------------------------------------------------
INDEX: v2 - nest function match/succeed in fail - avoid passing rhs/rules
----------------------------------------------------------------------*)

val matchTop2 : (obj -> 'a rule list -> 'a option) = 
	fn topObj => 
	   let fun fail [] = None
			 | fail (RULE(topPat,rhs)::rules) =
			   let 
				   fun match pat obj (work:work) =
					   case (pat,obj) of
						   (PAT'Var x,_) => succeed work
						 | (PAT'Tup pats, OBJ'Tup objs) => succeed ((pats,objs)::work)
						 | (PAT'Con(pcon,pat),OBJ'Con(ocon,obj)) => if (pcon=ocon) then match pat obj work
																	else fail rules
						 | (PAT'Tup _, _) => typeError "match/tup"
						 | (PAT'Con _, _) => typeError "match/con"
				   and succeed work =
					   case work 
						of [] => Some rhs
						 | ([],[])::work => succeed work 
						 | (pat::pats,obj::objs)::work => match pat obj ((pats,objs)::work)
						 | _ => typeError "succeed/arity"
				   val topWork : work = []
			   in
				   match topPat topObj topWork
			   end
	   in
		   fail
	   end

(*----------------------------------------------------------------------
INDEX: v3 - collect env of var bindings
----------------------------------------------------------------------*)

type env = (id * obj) list

val matchTop3 : (obj -> 'a rule list -> (env * 'a) option) = 
	fn topObj => 
	   let fun fail [] = None
			 | fail (RULE(topPat,rhs)::rules) =
			   let 
				   fun match pat obj (work:work) (env:env) =
					   case (pat,obj) of
						   (PAT'Var x,_) => succeed work ((x,obj)::env)
						 | (PAT'Tup pats, OBJ'Tup objs) => succeed ((pats,objs)::work) env
						 | (PAT'Con(pcon,pat),OBJ'Con(ocon,obj)) => if (pcon=ocon) then match pat obj work env
																	else fail rules
						 | (PAT'Tup _, _) => typeError "match/tup"
						 | (PAT'Con _, _) => typeError "match/con"
				   and succeed work env =
					   case work 
						of [] => Some (env,rhs)
						 | ([],[])::work => succeed work env
						 | (pat::pats,obj::objs)::work => match pat obj ((pats,objs)::work) env
						 | _ => typeError "succeed/arity"
				   val topWork : work = []
				   val topEnv : env = []
			   in
				   match topPat topObj topWork topEnv
			   end
	   in
		   fail
	   end

(*----------------------------------------------------------------------
INDEX: descriptions / SMA / staticMatch
----------------------------------------------------------------------*)

(* static description *) 
datatype des = DES'NoInfo | DES'Con of con * des | DES'NotCon of con list | DES'Tup of des list

(* DES'NoInfo === DES'NotCon [] *)

fun addNeg'des des con =
	case des of
		DES'NoInfo => DES'NotCon [con]
	  | DES'NotCon cons => if (exists (curry op= con) cons) then abort "addNeg/repeated nocon" 
						   else DES'NotCon (con::cons)
	  | DES'Tup _ => typeError "addNeg/tup"
	  | DES'Con con1 => if abort "addNeg/con - is possible for excon - have pos&neg description"
						then DES'Con con1 
						else DES'NotCon [con]
										
																								   

datatype staticMatchAnswer = SMA'Yes of des | SMA'No | SMA'Maybe

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
INDEX: (no good) context - like paper, except con/tup are confused, so...
----------------------------------------------------------------------*)

structure Old = struct
datatype context = CONTEXT of (con * des list) list
val topContext = CONTEXT []
fun pushContext pcon (CONTEXT rest) = CONTEXT ((pcon,[])::rest)
fun pushTupleContext (CONTEXT rest) = CONTEXT rest (* ??? *)
fun augment'context des context =
	case context
	 of CONTEXT [] => CONTEXT []
	  | CONTEXT ((con,dess)::rest) => CONTEXT ((con,des::dess)::rest)
fun norm'context context =
	case context
	 of CONTEXT [] => abort "norm/[]"
	  | CONTEXT ((con,[des])::rest) => augment'context (DES'Con (con,des)) (CONTEXT rest)
	  | CONTEXT ((_,dess)::rest) => augment'context (DES'Tup (rev dess)) (CONTEXT rest) 
fun build'des context des work =
	case (context,work) of
		(CONTEXT [],[]) => des
	  | (CONTEXT ((con,[])::rest), work) => let val des = DES'Con (con, des)
											in build'des (CONTEXT rest) des work
											end
	  | (CONTEXT ((_,args)::rest), (_,_,dargs)::work) => let val des = DES'Tup (rev args @ des :: dargs)
														 in build'des (CONTEXT rest) des work
														 end
	  | _ => abort "build'des"
end

(*----------------------------------------------------------------------
INDEX: context - with tuples
----------------------------------------------------------------------*)

datatype context 
  = CONTEXT'Top 
  | CONTEXT'ConPre		of con * context
  | CONTEXT'ConPost		of con * des * context
  | CONTEXT'Tup			of des list * context

val topContext = CONTEXT'Top
fun pushTupleContext context  = CONTEXT'Tup ([],context)
fun pushContext pcon context = CONTEXT'ConPre (pcon,context)

fun augment'context des context =
	case context
	 of CONTEXT'Top							=> CONTEXT'Top
	  | CONTEXT'ConPre (con,context)		=> CONTEXT'ConPost (con,des,context)
	  | CONTEXT'ConPost _					=> abort "augment/con-post"
	  | CONTEXT'Tup (dess,context)			=> CONTEXT'Tup (des::dess,context)

fun norm'context context =
	case context
	 of CONTEXT'Top							=> abort "norm/top"
	  | CONTEXT'ConPre _					=> abort "norm/con-pre"
	  | CONTEXT'ConPost (con,des,context)	=> augment'context (DES'Con (con,des)) context
	  | CONTEXT'Tup (dess,context)			=> augment'context (DES'Tup (rev dess)) context

fun build'des context des work =
	case (context,work) of
		(CONTEXT'Top,[]) => des
	  | (CONTEXT'ConPre (con,context),work) => let val des = DES'Con (con, des)
											   in build'des context des work
											   end
	  | (CONTEXT'ConPost _,_)				=> abort "build/post"
	  | (CONTEXT'Tup (dess,context),(_,_,dargs)::work) => let val des = DES'Tup (rev dess @ des :: dargs)
														  in build'des context des work
														  end
	  | _ => abort "build'des"

(*----------------------------------------------------------------------
INDEX: v4 - instrument with static matching (keep env)
----------------------------------------------------------------------*)

type workx = (pat list * obj list * des list) list

val matchTop4 : (obj -> 'a rule list -> (env * 'a) option) =
	fn topObj => 
	   let fun fail topDes [] = None
			 | fail topDes (RULE(topPat,rhs)::rules) =
			   let 
				   fun match pat obj des context (work:workx) (env:env) =
					   case (pat,obj) of
						   (PAT'Var x,_) => let val context = augment'context des context
												val env = (x,obj) :: env
											in succeed context work env
											end
						 | (PAT'Tup pats, OBJ'Tup objs) => let val dess = case des
																		   of DES'Tup dess => dess
																			| _ => map (fn _ => DES'NoInfo) pats
															   val work = (pats,objs,dess)::work
															   val context = pushTupleContext context
														   in succeed context work env
														   end
						 | (PAT'Con(pcon,pat),OBJ'Con(ocon,obj)) => 
						   let
							   fun succeed' des = let val context = pushContext pcon context
												  in match pat obj des context work env
												  end
							   fun fail' des = let val desTop = build'des context des work
											   in fail desTop rules
											   end
						   in 
							   case (staticMatch pcon des) 
								of SMA'Yes des => succeed' des
								 | SMA'No  => fail' des
								 | SMA'Maybe => if (pcon=ocon) 
												then succeed' DES'NoInfo
												else fail' (addNeg'des des pcon)
						   end
						 | (PAT'Tup _, _) => typeError "match/tup"
						 | (PAT'Con _, _) => typeError "match/con"
				   and succeed context work env =
					   case work 
						of [] => Some (env,rhs)
						 | ([],[],[])::work => let val context = norm'context context 
											   in succeed context work env
											   end
						 | (pat::pats,obj::objs,des::dess)::work => let val context = pushTupleContext context
																		val work = (pats,objs,dess)::work
																	in match pat obj des context work env
																	end
						 | _ => typeError "succeed"
				   val topWork : workx = []
				   val topEnv : env = []
			   in
				   match topPat topObj topDes topContext topWork topEnv
			   end
	   in
		   fail DES'NoInfo
	   end

(*----------------------------------------------------------------------
INDEX: v5 - restructure to avoid work lists (no static matching)
----------------------------------------------------------------------*)

local
	fun match pat obj (env:env) : env option =
		case (pat,obj) 
		 of (PAT'Var x,_) => Some ((x,obj)::env)
		  | (PAT'Tup pats, OBJ'Tup objs) => succeed pats objs env
		  | (PAT'Con(pcon,pat),OBJ'Con(ocon,obj)) => if (pcon=ocon) then match pat obj env
													 else None
		  | (PAT'Tup _, _) => typeError "match/tup"
		  | (PAT'Con _, _) => typeError "match/con"
	and succeed pats objs env : env option=
		case (pats,objs) 
		 of ([],[]) => Some env
		  | (pat::pats,obj::objs) => (case (match pat obj env) 
									   of None => None 
										| Some env => succeed pats objs env)
		  | _ => typeError "succeed/arity"
in
val matchTop5 : (obj -> 'a rule list -> (env * 'a) option) = 
	fn topObj => 
	   let val topEnv : env = []
		   fun fail [] = None
			 | fail (RULE(pat,rhs)::rules) = (case (match pat topObj topEnv) 
											   of None => fail rules
												| Some env => Some (env,rhs))
	   in
		   fail
	   end
end

(*----------------------------------------------------------------------
INDEX: Add back static-matcing. Now des(scriptions) are build on the way up (if we fail)
- can do without contexts
----------------------------------------------------------------------*)

(* convert: des -> pat. des is slightly richer because of the DES'NotCon *)

fun des'pat pat =
	case pat
	 of PAT'Var _ => DES'NoInfo
	  | PAT'Tup pats => DES'Tup (map des'pat pats)
	  | PAT'Con (con,pat) => DES'Con (con, des'pat pat)

(* descriptions may have smaller arity *)
val delist'dess = 
	fn [] => (DES'NoInfo,[]) 
	 | (des::dess) => (des,dess)

val deTup'des =
	fn DES'Tup dess => dess
	 | DES'NoInfo => []
	 | DES'Con _ => typeError "deTup/con"
	 | DES'NotCon _ => typeError "deTup/not-con"

(*----------------------------------------------------------------------
INDEX: v6 - no worklists / with static matching
----------------------------------------------------------------------*)

datatype 'a matchRes = MR'Succ of env | MR'Fail of 'a

fun mapFail f =
	fn MR'Succ env => MR'Succ env
	 | MR'Fail x => MR'Fail (f x)

local
	fun match pat obj des (env:env) : des matchRes =
		case (pat,obj) 
		 of (PAT'Var x,_) => MR'Succ ((x,obj)::env)
		  | (PAT'Tup pats, OBJ'Tup objs) => mapFail DES'Tup (succeed pats objs (deTup'des des) env)
		  | (PAT'Con(pcon,pat),OBJ'Con(ocon,obj)) => (case (staticMatch pcon des) 
													   of SMA'Yes des => mapFail (curry DES'Con pcon) (match pat obj des env)
														| SMA'No  => MR'Fail des 
														| SMA'Maybe => if (pcon=ocon)
																	   then mapFail (curry DES'Con pcon) (match pat obj DES'NoInfo env)
																	   else MR'Fail (addNeg'des des pcon))
		  | (PAT'Tup _, _) => typeError "match/tup"
		  | (PAT'Con _, _) => typeError "match/con"
	and succeed pats objs dess (env:env) : des list matchRes =
		case (pats,objs) 
		 of ([],[]) => MR'Succ env
		  | (pat::pats,obj::objs) => let val (des,dess) = delist'dess dess
									 in case (match pat obj des env) 
										 of MR'Fail des => MR'Fail (des::dess)
										  | MR'Succ env => mapFail (fn dess => des'pat pat::dess) (succeed pats objs dess env)
									 end
		  | _ => typeError "succeed/arity"
in
val matchTop6 : (obj -> 'a rule list -> (env * 'a) option) = 
	fn topObj => 
	   let val topEnv : env = []
		   fun fail des [] = None
			 | fail des (RULE(pat,rhs)::rules) = (case (match pat topObj des topEnv) 
												   of MR'Fail des => fail des rules
													| MR'Succ env => Some (env,rhs))
	   in fail DES'NoInfo
	   end
end


(* 
inline staticMatch? -  avoids SMA - try.. code expands / not simpler!! 
is everything correct?
- do we get maximal descriptions at all times? or do we loose something my not passing desciption on success?
- des'pat is a but yuck - seems unesc to have this 2nd pas
- do we always build the correct env?
*)

(*----------------------------------------------------------------------
INDEX: v7 - restructure to use succ/fail continuations, instead of matchres
----------------------------------------------------------------------*)

local
	fun match S F pat obj des (env:env) = 
		case (pat,obj) 
		 of (PAT'Var x,_) => S ((x,obj)::env)
		  | (PAT'Tup pats, OBJ'Tup objs) => succeed S (F o DES'Tup) pats objs (deTup'des des) env
		  | (PAT'Con(pcon,pat),OBJ'Con(ocon,obj)) => (case (staticMatch pcon des) 
													   of SMA'Yes des => match S (F o curry DES'Con pcon) pat obj des env
														| SMA'No  => F des
														| SMA'Maybe => if (pcon=ocon)
																	   then match S (F o curry DES'Con pcon) pat obj DES'NoInfo env
																	   else F (addNeg'des des pcon))
		  | (PAT'Tup _, _) => typeError "match/tup"
		  | (PAT'Con _, _) => typeError "match/con"
	and succeed S F pats objs dess (env:env) = 
		case (pats,objs) 
		 of ([],[]) => S env
		  | (pat::pats,obj::objs) => let val (des,dess) = delist'dess dess
									 in match 
										(fn env => succeed
												   S
												   (fn dess => F (des'pat pat :: dess))
												   pats objs dess env) 
										(fn des => F (des::dess)) 
										pat obj des env
									 end
		  | _ => typeError "succeed/arity"
in
val matchTop7 : (obj -> 'a rule list -> (env * 'a) option) = 
	fn topObj => 
	   let val topEnv : env = []
		   fun fail des [] = None
			 | fail des (RULE(pat,rhs)::rules) = match
												 (fn env => Some (env,rhs))
												 (fn des => fail des rules)
												 pat topObj des topEnv
	   in fail DES'NoInfo
	   end
end

(*----------------------------------------------------------------------
INDEX: stage computation (partial eval) - pat/des - static, obj - dynamic
- result is a decision tree to select rhs/ build env
- make use of obj-path selection, in deciding which ob to test & in building the env
----------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: access / decision
----------------------------------------------------------------------*)

datatype access 
  = ACCESS'Obj
  | ACCESS'Tup  of int * access
  | ACCESS'Con  of con * access (* con just for checking *)

fun tupAccesss n access = map (fn i => ACCESS'Tup (i,access)) (upto (0,n-1))

datatype 'a decision 
  = DEC'Fail 
  | DEC'Succ of 'a 
  | DEC'If of access * con * 'a decision * 'a decision

fun nth _ [] = abort "nth/[]"
  | nth n (x::xs) = if (n=0) then x else nth (n-1) xs (* interesting for opt: xs only used in one arm! *)

fun accObj access obj =
	case (access,obj) 
	 of (ACCESS'Obj,_) => obj
	  | (ACCESS'Tup (n,access), OBJ'Tup objs) => accObj access (nth n objs)
	  | (ACCESS'Con (pcon,access), OBJ'Con (ocon,obj)) => let val () = if pcon<>ocon then abort "accObj/con" else ()
														  in accObj access obj
														  end
	  | _ => abort "accObj"																	 

fun execDecision decision obj =
	case decision
	 of DEC'Fail => None
	  | DEC'Succ x => Some x
	  | DEC'If (access,pcon,dThen,dElse) => (case (accObj access obj)
											  of OBJ'Con (ocon,_) => if (pcon=ocon) 
																	 then execDecision dThen obj
																	 else execDecision dElse obj
											   | _ => typeError "execDecision/con")

type aenv = (id * access) list

(*----------------------------------------------------------------------
INDEX: layout
----------------------------------------------------------------------*)

fun lay'con (CON {name,...}) = Word name

val atomic'pat =
	fn PAT'Con _ => false
	 | _ => true

fun layBracIfNonAtomic A L x = if A x then L x else layBracket (L x)

val rec lay'pat =
	fn PAT'Var x => Word x
	 | PAT'Tup pats => layTuple (map lay'pat pats)
	 | PAT'Con (con,pat) => lay'con	con ++ layBracIfNonAtomic atomic'pat lay'pat pat

fun lay'rule lay (RULE (pat,rhs)) = lay'pat pat ++ Word "=>" ++ lay rhs

fun lay'rules lay rules = layVert (map (lay'rule lay) rules)

fun lay'access objLayout access =
	case access
	 of ACCESS'Obj => objLayout
	  | ACCESS'Con (con,access) => lay'access (objLayout @@ Sep "." @@ lay'con con) access
	  | ACCESS'Tup (n,access) => lay'access (objLayout @@ Sep "." @@ layInt n) access

fun lay'decision lay objLayout decision =
	case decision
	 of DEC'Fail => Word "Fail"
	  | DEC'Succ x => lay x
	  | DEC'If (access,con,dThen,dElse) => 
		(Word "if-match" ++ layBracket (lay'access objLayout access ++ Word "~" ++ lay'con con))
		// Word "then" ++ lay'decision lay objLayout dThen
		// Word "else" ++ lay'decision lay objLayout dElse


fun lay'aenv objLayout (aenv:aenv) =
	layBrace (layCommaSep (map (fn (x,access) => Word x @@ Sep "=" @@ lay'access objLayout access) aenv))

fun lay'pair (L1,L2) (x1,x2) = layPair (L1 x1, L2 x2) 


fun echo'lay lay x =
	(prefixed_output_layout "MC: " (squash_to 100 (lay x));
	 echo "")


(*----------------------------------------------------------------------
INDEX: v8 - stage computation - pat/des - static, obj - dynamic
----------------------------------------------------------------------*)

local
	fun match S F pat access des (aenv:aenv) = 
		case pat 
		 of PAT'Var x => S ((x,access)::aenv)
		  | PAT'Tup pats => succeed S (F o DES'Tup) pats (tupAccesss (length pats) access) (deTup'des des) aenv
		  | PAT'Con(pcon,pat) => (case (staticMatch pcon des) 
								   of SMA'Yes des => match S (F o curry DES'Con pcon) pat (ACCESS'Con (pcon,access)) des aenv
									| SMA'No  => F des
									| SMA'Maybe => DEC'If (access,pcon,
														   match S (F o curry DES'Con pcon) pat (ACCESS'Con (pcon,access)) DES'NoInfo aenv,
														   F (addNeg'des des pcon)))
	and succeed S F pats accesss dess (aenv:aenv) = 
		case (pats,accesss)
		 of ([],[]) => S aenv
		  | (pat::pats,access::accesss) => let val (des,dess) = delist'dess dess
										   in match 
											  (fn aenv => succeed
														  S
														  (fn dess => F ((*des'pat pat*) des :: dess))
														  pats accesss dess aenv) 
											  (fn des => F (des::dess)) 
											  pat access des aenv
										   end
		  | _ => typeError "succeed/arity"
in
val matchTop8 : (unit -> 'a rule list -> (aenv * 'a) decision) = 
	fn () => 
	   let val topAccess = ACCESS'Obj
		   val topAenv : aenv = []
		   fun fail des [] = DEC'Fail
			 | fail des (RULE(pat,rhs)::rules) = match
												 (fn aenv => DEC'Succ (aenv,rhs))
												 (fn des => fail des rules)
												 pat topAccess des topAenv
	   in fail DES'NoInfo
	   end
end


(*----------------------------------------------------------------------
INDEX: v9 - think need to pass des to S continuation & extend where nesc.
----------------------------------------------------------------------*)

local
	fun match S F pat access des (aenv:aenv) = 
		case pat 
		 of PAT'Var x => S (des,(x,access)::aenv)
		  | PAT'Tup pats => succeed 
							(fn (dess,aenv) => S (DES'Tup dess,aenv))
							(F o DES'Tup) pats (tupAccesss (length pats) access) (deTup'des des) aenv
		  | PAT'Con(pcon,pat) => (case (staticMatch pcon des) 
								   of SMA'Yes des => match 
													 (fn (des,aenv) => S (DES'Con (pcon,des),aenv))
													 (F o curry DES'Con pcon) pat (ACCESS'Con (pcon,access)) des aenv
									| SMA'No  => F des
									| SMA'Maybe => DEC'If (access,pcon,
														   match 
														   (fn (des,aenv) => S (DES'Con (pcon,des),aenv))
														   (F o curry DES'Con pcon) pat (ACCESS'Con (pcon,access)) DES'NoInfo aenv,
														   F (addNeg'des des pcon)))
	and succeed S F pats accesss dess (aenv:aenv) = 
		case (pats,accesss)
		 of ([],[]) => S ([],aenv)
		  | (pat::pats,access::accesss) => let val (des,dess) = delist'dess dess
										   in match 
											  (fn (des,aenv) => succeed
																(fn (dess,aenv) => S (des::dess,aenv))
																(fn dess => F (des :: dess))
																pats accesss dess aenv) 
											  (fn des => F (des::dess)) 
											  pat access des aenv
										   end
		  | _ => typeError "succeed/arity"
in
val matchTop9 : (unit -> 'a rule list -> (aenv * 'a) decision) = 
	fn () => 
	   let val topAccess = ACCESS'Obj
		   val topAenv : aenv = []
		   fun fail des [] = DEC'Fail
			 | fail des (RULE(pat,rhs)::rules) = match
												 (fn (des,aenv) => DEC'Succ (aenv,rhs))
												 (fn des => fail des rules)
												 pat topAccess des topAenv
	   in fail DES'NoInfo
	   end
end


(*----------------------------------------------------------------------
INDEX: test...
----------------------------------------------------------------------*)

val matchTop = matchTop9

fun layRunMatchCompiler lay objLayout rules =
	let val decision = matchTop () rules
	in (lay'rules lay rules)
	   // Word "---->"
	   // lay'decision (lay'pair (lay'aenv objLayout, lay)) objLayout decision
	end

fun test rules =
	let val lay = layInt
		val objLayout = Word "R"
		val L = layRunMatchCompiler lay objLayout
	in echo'lay L rules
	end

(*----------------------------------------------------------------------
INDEX: have a play...
----------------------------------------------------------------------*)

val u = PAT'Var "_"
val A = PAT'Con (CON {isExcon=false,name="A",span=Some 2}, PAT'Tup [])
val B = PAT'Con (CON {isExcon=false,name="B",span=Some 2}, PAT'Tup [])


test 
[
 RULE (PAT'Tup [A, A, A], 100),
 RULE (PAT'Tup [B, B, B], 200),
 RULE (PAT'Tup [A, A, B], 300),
 RULE (PAT'Tup [A, B, B], 400)
]


test 
[
 RULE (PAT'Tup [A, u], 100),
 RULE (PAT'Tup [u, A], 200),
 RULE (PAT'Tup [B, B], 300) (* bug here - fixed *)
]

test 
[
 RULE (PAT'Tup [A, A], 100),
 RULE (PAT'Tup [u, A], 200),
 RULE (PAT'Tup [A, u], 300), (* ... now get bug here - this A is re-tested -fixed !!*)
 RULE (PAT'Tup [B, B], 400)
]


test 
[
 RULE (PAT'Tup [u, A], 200),
 RULE (PAT'Tup [A, u], 100),
 RULE (PAT'Tup [B, B], 300) (* ok if reverse rules 1 & 2. *)
]

(* example to show des info is lost, think problem is... -fixed *)
test 
[
 RULE (PAT'Tup [A, u], 100), (* when ths fails, we no elem-1 is not-A (hence non-B) *)
 RULE (PAT'Tup [u, A], 200), (* but we loose that info here - problem is des'pat??*)
 RULE (PAT'Tup [B, u], 300)  (* and so wew tests elem-1 again *)
]


test 
[
 RULE (PAT'Tup [A, A], 100),
 RULE (PAT'Tup [A, u], 300),
 RULE (PAT'Tup [u, A], 200),
 RULE (PAT'Tup [B, B], 400)
]


val x = PAT'Var "x"
val xs = PAT'Var "xs"
val y = PAT'Var "y"
val ys = PAT'Var "ys"
val u = PAT'Var "_"
val N = PAT'Con (CON {isExcon=false,name="nil",span=Some 2}, PAT'Tup [])
fun C (x,xs) = PAT'Con (CON {isExcon=false,name="cons",span=Some 2}, PAT'Tup [x,xs])

test 
[
 RULE (PAT'Tup [C (x,xs), C (y,ys)], 100),
 RULE (PAT'Tup [N, N], 200)
]

test 
[
 RULE (PAT'Tup [C (x,xs), C (y,ys)], 100),
 RULE (PAT'Tup [C (u,u), u], 200),
 RULE (PAT'Tup [N, N], 300)
]

test 
[
 RULE (PAT'Tup [C (x,xs), C (y,ys)], 100),
 RULE (PAT'Tup [u, C (u,u)], 200),
 RULE (PAT'Tup [N, N], 300)
]

test 
[
 RULE (PAT'Tup [C (x,xs), C (y,ys)], 100),
 RULE (PAT'Tup [u, C (u,u)], 200),
 RULE (PAT'Tup [C (u,u), u], 300),
 RULE (PAT'Tup [N, N], 400)
]

test 
[
 RULE (PAT'Tup [C (x,xs), C (y,ys)], 100),
 RULE (PAT'Tup [C (u,u), u], 300),
 RULE (PAT'Tup [u, C (u,u)], 200),
 RULE (PAT'Tup [N, N], 400)
]



test 
[
 RULE (PAT'Tup [N, N], 2)
]



test 
[RULE (N,1), 
 RULE (C (x,xs), 2)
 ]

test 
[
 RULE (C (x,xs), 2),
 RULE (N,1)
]
