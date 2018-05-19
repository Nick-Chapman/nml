(*------------------------------------------------------------------------------
  CONTENTS-START-LINE: HERE=2 SEP=1
   27.    top
   52.    bind
   84.    open
   98.    source language generation - vids
   115.   source language generation - list(pat/exp), seq, if/andalso/orelse
   140.   source language generation - exp - while
   163.   source language generation - dec - fvalbind
   182.   fvalbind
   198.   DeEnv
   266.   collect vars bound by pat
   289.   Compile Environment
   321.   env lookup
   346.   var/con status
   395.   pat
   428.   comp'exp
   483.   typbind / datbinda
   511.   pes - (pat*exp) list 
   525.   valbind
   552.   exbind
   632.   fix compilation P/E/D
  CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: top
----------------------------------------------------------------------*)

val preludeDir = "/home/nic/project/prelude/"
fun prefixPrelude s = preludeDir^s
val pervasives = map prefixPrelude ["option.ML","either.ML"];
map use pervasives;
val some_sources = 
	map prefixPrelude [
	 "Prel.ML",
	 "Sort.ML",
	 "ASSOC.ML",
	 "QLAYOUT.ML",
	 "MISCLAY.ML"
	 ] @ 
	[
	 "ATOM.ML",	 
	 (*"VALUE.ML",*)
	 "BASIS.ML",
	 "POS.ML",
	 "LANG.ML"
	 ];
map use some_sources;

(*----------------------------------------------------------------------
INDEX: bind
----------------------------------------------------------------------*)

structure Assoc = ASSOC(structure Prel = Prel)
structure QLayout = QLAYOUT(structure Prel = Prel)
structure Misclay = MISCLAY(structure Prel = Prel
							structure QLayout = QLayout)
structure Atom = ATOM(structure Prel = Prel)

structure Value =
struct
type vid = Atom.vid
type layout = Misclay.layout
abstype value = V and exname = X with end
fun lay'value (_:value) = Misclay.Word "<value>"
val value'con0			: (vid -> value) = fn _ => Prel.abort "value'con"
val value'scon			: (Atom.scon -> value) = fn _ => Prel.abort "value'scon"
val excon0'value        : (string -> value -> exname) = fn _ => Prel.abort "excon0'value"
val ref_or_con0'value   : (string -> value -> ({Ref:unit},{Con0:vid}) either) = fn _ => Prel.abort "ref_or_con0'value"
end

structure Basis = BASIS(structure Prel = Prel
						structure Assoc = Assoc
						structure Misclay = Misclay
						structure Atom = Atom
						structure Value = Value)
structure Pos = POS(structure Prel = Prel)
structure Lang = LANG(structure Prel = Prel
					  structure Pos = Pos
					  structure Atom = Atom)

(*----------------------------------------------------------------------
INDEX: open
----------------------------------------------------------------------*)

  open Prel
  open Assoc
  open Atom
  open Lang
  open Value

  fun error s = abort ("error: de-env: "^s)

  fun dummyPos who = Pos.makePos{path=who,row=0,col=0}

(*----------------------------------------------------------------------
INDEX: source language generation - vids
----------------------------------------------------------------------*)
						  
  local val U = ref 1
  in fun genVid prefix = let val n = !U
						 in U := 1+n; VID (ID (prefix^"-G#"^stringOfInt n))
						 end
  end
	  
  fun genVids prefix n =
	  if (n<0) then error "genVids:n<0"
	  else let fun loop acc n = if n=0 then acc
								else loop (genVid (prefix^"-"^stringOfInt n) ::acc) (n-1)
		   in loop [] n
		   end

(*----------------------------------------------------------------------
INDEX: source language generation - list(pat/exp), seq, if/andalso/orelse
----------------------------------------------------------------------*)

  fun long's s =  LONG([],VID(ID s))

  fun pat'con0 s = PAT'Var (long's s)
  fun pat'consList p ps = PAT'Cons (long's "::",PAT'Tuple [p,ps])
  val pat'list = foldR pat'consList (pat'con0 "nil")

  fun exp's s = EXP'Var (long's s, dummyPos s)
  fun exp'consList e es = EXP'App (exp's "::",EXP'Tuple [e,es])
  val exp'list = foldR exp'consList (exp's "nil")

  fun exp'seq2 (e1,e2) =  EXP'Case (e1,MATCH (dummyPos "exp/seq",[MRULE(PAT'Wild,e2)]))
  fun exp'seq [] = error "exp'seq:[]"
	| exp'seq (e::es) = fold (curry exp'seq2) e es (* fold-left better for CPS transform? *)

  fun exp'if (e1,e2,e3) = 
	  EXP'Case (e1,MATCH(dummyPos "exp/if",[MRULE (pat'con0 "true",e2),
											MRULE (pat'con0 "false",e3)]))

  fun exp'andalso (e1,e2) = exp'if (e1,e2,exp's "false")
  fun exp'orelse (e1,e2) = exp'if (e1,exp's "true",e2)

(*----------------------------------------------------------------------
INDEX: source language generation - exp - while
----------------------------------------------------------------------*)

  fun expDerived'while (e1,e2) =
	  let val pos = dummyPos "while"
		  val x = genVid "while"
		  val X = LONG([],x)
	  in (*let val rec X = fn () => if e1 then (e2; X()) else () in X() end*)
		  EXP'Let 
		  (DEC'Val 
		   (VALBIND'Rec 
			(VALBIND'Seq (PAT'Var X,
						  EXP'Fn (MATCH (pos,
										 [MRULE (PAT'Unit,
												 EXP'If (e1,
														 EXP'Seq [e2, EXP'App (EXP'Var (X,pos),EXP'Unit)], 
														 EXP'Unit)
												 )])),
						  None))),
		   EXP'App (EXP'Var (X,pos), EXP'Unit))
	  end

(*----------------------------------------------------------------------
INDEX: source language generation - dec - fvalbind
----------------------------------------------------------------------*)

  fun exp'fnVid pos x body = EXP'Fn (MATCH (pos,[MRULE (PAT'Var (LONG ([],x)),body)]))

  fun exp'fnVids pos [] body = body
	| exp'fnVids pos (x::xs) body = exp'fnVid pos x (exp'fnVids pos xs body)

  fun mrule'clause (pats,tyo_IGNORED,exp) = MRULE (PAT'Tuple pats,exp)
									
  fun exp'clauses prefix pos [] = error "exp'clauses:[]"
	| exp'clauses prefix pos (clauses as (pats,_,_)::_) =
	  let val vids = genVids prefix (length pats)
	  in exp'fnVids pos vids
		 (EXP'Case (EXP'Tuple (map (exp'vid pos) vids),
					MATCH (pos,map mrule'clause clauses)))
	  end

(*----------------------------------------------------------------------
INDEX: fvalbind
----------------------------------------------------------------------*)

  fun valbind'pes [] = abort "valbind'pes[]"
	| valbind'pes [(p,e)] = VALBIND'Seq (p,e,None)
	| valbind'pes ((p,e)::pes) = VALBIND'Seq (p,e,Some(valbind'pes pes))


  fun valbind'fvalbind (FVALBIND fs) =
	  VALBIND'Rec 
	  (valbind'pes
	   (map (fn (pos,vid,clauses) => let val exp = exp'clauses (sof'vid vid) pos clauses
									 in (PAT'Var (LONG ([],vid)), exp)
									 end) fs))

(*----------------------------------------------------------------------
INDEX: DeEnv
----------------------------------------------------------------------*)

structure DeEnv =
struct

  datatype scon = datatype Atom.scon
  datatype vid = datatype Atom.vid

  type exname = Value.exname
  type value = Value.value

  datatype pat 
	= PAT'Wild
    | PAT'V         of vid
    | PAT'Layered   of vid * pat
    | PAT'Ref       of pat
    | PAT'C         of vid * pat option
    | PAT'E         of exname * pat option
    | PAT'G         of vid * pat option (* runtime generated exception constructor *)
    | PAT'Scon      of scon
    | PAT'Tuple     of pat list
(*sugar...
    | PAT'List      of pat list
    | PAT'Record    of (lab * pat) list
    | PAT'Unit
*)

  datatype exp 
	= EXP'Value     of value (* scon, or created by de-env *)
    | EXP'Var       of vid
    | EXP'Tuple     of exp list
    | EXP'Let       of dec * exp
	| EXP'App       of exp * exp
	| EXP'Case      of exp * match
    | EXP'Handle    of exp * match
    | EXP'Raise     of exp
	| EXP'Fn        of func
(*sugar..
	| EXP'If        of exp * exp * exp
	| EXP'Andalso   of exp * exp
    | EXP'Orelse    of exp * exp
    | EXP'Seq       of exp list
	| EXP'Vector    of exp list
    | EXP'List      of exp list
    | EXP'Record    of (lab * exp) list
    | EXP'Select    of lab
    | EXP'Unit
*)

  and func = FUNC	of match

  and match
	= MATCH'Alt     of match * match 
	| MATCH'Rule    of pat * exp

  and dec
	= DEC'Empty
    | DEC'Seq       of dec * dec
    | DEC'Local     of dec * dec
    | DEC'Val       of pat * exp
    | DEC'Valrec    of (vid * func) list
    | DEC'ExGen		of vid
    | DEC'ExCopy	of vid * vid

end

(*----------------------------------------------------------------------
INDEX: collect vars bound by pat
----------------------------------------------------------------------*)

  local
	  open DeEnv
	  fun P acc pat =
		  case pat of
			  PAT'Wild              => acc
			| PAT'V vid             => vid :: acc
			| PAT'Layered (vid,pat) => P (vid :: acc ) pat
			| PAT'Ref pat           => P acc pat
			| PAT'C (_,None)        => acc
			| PAT'C (_,Some pat)    => P acc pat
			| PAT'E (_,None)        => acc
			| PAT'E (_,Some pat)    => P acc pat
			| PAT'G (_,None)        => acc
			| PAT'G (_,Some pat)    => P acc pat
			| PAT'Scon _            => acc
			| PAT'Tuple pats        => fold P acc pats
  in val boundVars'pat = P []
  end

(*----------------------------------------------------------------------
INDEX: Compile Environment
----------------------------------------------------------------------*)

  datatype valenv = VALENV of (vid, value option * Basis.idstatus) assoc (* None, means value not available at CT *)

  val valenv'empty = VALENV assoc'empty
  fun valenv'plus (VALENV a1) (VALENV a2) = VALENV (assoc'plus a1 a2)
  val valenv'plusList = foldR valenv'plus valenv'empty
  fun valenv'single kv = VALENV (assoc'single kv)
  fun comp'valenv x = VALENV (map'assoc (fn (k,(v,ids)) => (k,(Some v,ids))) (Basis.assoc'valenv x))
  fun isUnbound'vid (VALENV assoc) vid = case (lookup'assoc assoc vid) of None => true | Some _ => false
  fun look'vid who (VALENV a) k = look'assoc who sof'vid a k

  fun valenv'runtimeVars xs = 
	  valenv'plusList (map (fn x => valenv'single (x,(None,Basis.IDSTATUS'V))) xs) 

  datatype env = ENV of Basis.strenv * Basis.tyenv * valenv

  val env'empty = ENV (Basis.strenv'empty,Basis.tyenv'empty,valenv'empty)
  fun env'tyenv te = ENV(Basis.strenv'empty,te,valenv'empty)
  fun env'valenv ve = ENV(Basis.strenv'empty,Basis.tyenv'empty,ve)
  fun env'plus (ENV (se1,te1,ve1)) (ENV (se2,te2,ve2)) =
	  let val se = Basis.strenv'plus se1 se2
		  val te = Basis.tyenv'plus te1 te2
		  val ve = valenv'plus ve1 ve2
	  in ENV (se,te,ve)
	  end
  val env'plusList = foldR env'plus env'empty
  fun comp'env env =
	  ENV (Basis.strenv'env env,
		   Basis.tyenv'env env,
		   comp'valenv (Basis.valenv'env env))

(*----------------------------------------------------------------------
INDEX: env lookup
----------------------------------------------------------------------*)

  fun longOrShort'long (LONG ([],x)) = This x
	| longOrShort'long (LONG (s1::ss,x)) = That (s1,LONG(ss,x))


  fun look'longstrid (ENV (strenv,_,_)) longx =
	  comp'env (case longOrShort'long longx
				 of This x => Basis.look'strid "" strenv x
				  | That (strid,longx) => Basis.look'longstrid (Basis.look'strid "" strenv strid) longx)
	  
  fun look'longtycon (ENV (strenv,tyenv,_)) longx =
	  case longOrShort'long longx
	   of This x => Basis.look'tycon "" tyenv x
		| That (strid,longx) => Basis.look'longtycon (Basis.look'strid "" strenv strid) longx

  fun look'longvid (ENV (strenv,_,valenv)) longx =
	  case longOrShort'long longx
	   of This x => This (x, if (isUnbound'vid valenv x) 
							 then None
							 else Some (look'vid "" valenv x))
		| That (strid,longx) => That (Basis.look'longvid "" (Basis.look'strid "" strenv strid) longx)

(*----------------------------------------------------------------------
INDEX: var/con status
----------------------------------------------------------------------*)

  fun comp'var env longx =
	  case (look'longvid env longx) 
	   of This (vid,None) => DeEnv.PAT'V vid (*unbound/short*)
		| This (vid,Some(vopt,ids)) => (*short*)
		  (case ids
			of Basis.IDSTATUS'V => DeEnv.PAT'V vid
			 | Basis.IDSTATUS'E => (case vopt of 
										None => DeEnv.PAT'G (vid,None)
									  | Some v => DeEnv.PAT'E (excon0'value "patvar/short/E" v,None))
			 | Basis.IDSTATUS'C => (case vopt of
										None => error "patvar/short/C/RT"
									  | Some v => (case (ref_or_con0'value "patvar/short/C" v)
													of This {Ref=()} => error "patvar/short/ref"
													 | That {Con0=vid} => DeEnv.PAT'C (vid,None))))
		| That (v,ids) => (*long*)
		  (case ids
			of Basis.IDSTATUS'V => error "patvar/long/V"
			 | Basis.IDSTATUS'E => DeEnv.PAT'E (excon0'value "patvar/long/E" v,None)
			 | Basis.IDSTATUS'C => (case (ref_or_con0'value "patvar/long/C" v)
									 of This {Ref=()} => error "patvar/long/ref"
									  | That {Con0=vid} => DeEnv.PAT'C (vid,None)))


  fun comp'cons env longx pat =
	  case (look'longvid env longx) 
	   of This (vid,None) => error "patcon/unbound"
		| This (vid,Some(vopt,ids)) => (*short*)
		  (case ids
			of Basis.IDSTATUS'V => error "patcon/short/V"
			 | Basis.IDSTATUS'E => (case vopt of 
										None => DeEnv.PAT'G (vid,Some pat)
									  | Some v => DeEnv.PAT'E (excon0'value "patcon/short/E" v,Some pat))
			 | Basis.IDSTATUS'C => (case vopt of
										None => error "patcon/short/C/RT"
									  | Some v => (case (ref_or_con0'value "patcon/short/C" v)
													of This {Ref=()} => DeEnv.PAT'Ref pat
													 | That {Con0=vid} => DeEnv.PAT'C (vid,None))))
		| That (v,ids) => (*long*)
		  (case ids
			of Basis.IDSTATUS'V => error "patcon/long/V"
			 | Basis.IDSTATUS'E => DeEnv.PAT'E (excon0'value "patcon/long/E" v,None)
			 | Basis.IDSTATUS'C => (case (ref_or_con0'value "patcon/long/C" v)
									 of This {Ref=()} => DeEnv.PAT'Ref pat
									  | That {Con0=vid} => DeEnv.PAT'C (vid,None)))

(*----------------------------------------------------------------------
INDEX: pat
----------------------------------------------------------------------*)

(*
  fun comp'patrow P (PATROW (lps,{wildcard})) =
	  Code.PATROW (map (fn (lab,pat) => (lab,P pat)) lps,{wildcard=wildcard})
*)

  fun U_comp'pat env P pat =
	  case pat of
          PAT'Wild                  => DeEnv.PAT'Wild
        | PAT'Scon scon             => DeEnv.PAT'Scon scon
        | PAT'Var x                 => comp'var env x
        | PAT'Record pr             => (*DeEnv.PAT'Record (comp'patrow P pr)*) error "record"
        | PAT'Unit                  => DeEnv.PAT'Tuple []
        | PAT'Tuple pats            => DeEnv.PAT'Tuple (map P pats)
        | PAT'List pats             => P (pat'list pats)
        | PAT'Cons (x,pat)          => comp'cons env x (P pat)
        | PAT'Typed (pat,ty)        => P pat
        | PAT'Layered (vid,tyo,pat)	=> DeEnv.PAT'Layered (vid,P pat)

  local fun P env pat = U_comp'pat env (P env) pat
  in val ONE_comp'pat = P
  end
	  

  fun comp'pat env pat =
	  let val pat' = ONE_comp'pat env pat
		  val valenv = valenv'runtimeVars (boundVars'pat pat')
	  in (pat',valenv)
	  end

(*----------------------------------------------------------------------
INDEX: comp'exp
----------------------------------------------------------------------*)

  fun comp'longvid env longvid =
	  case (look'longvid env longvid)
	   (* strange (inefficient) match order jsut to streatch nml compiler!... *)
       of This(x,None)              => error ("comp'longvid/unbound/"^sof'vid x)
        | This(x,Some(None,_))      => DeEnv.EXP'Var x (* short/RT *)
        | That(v,_)                 => DeEnv.EXP'Value v (* long/CT *)
        | This(vid,Some(Some v ,_)) => DeEnv.EXP'Value v (* short/CT - embed into DeEnv.exp type *)


  fun comp'mrule E env (MRULE (pat,exp)) = let val (pat',ve) = comp'pat env pat
											   val env = env'plus env (env'valenv ve)
										   in DeEnv.MATCH'Rule (pat',E env exp)
										   end

  fun comp'match E env (MATCH (pos,[])) = error "comp'match:[]"
	| comp'match E env (MATCH (pos,x::xs)) = 
	  fold (curry DeEnv.MATCH'Alt) (comp'mrule E env x) (map (comp'mrule E env) xs)

(*
  fun comp'exprow Eshad exprow =
	  case exprow of
		  EXPROW xs => DeEnv.EXPROW (map (fn (lab,exp) => (lab,Eshad exp)) xs)
*)


  fun U_comp'exp E D env exp =
	  case exp of
          EXP'Scon scon             => DeEnv.EXP'Value (value'scon scon)
        | EXP'Var (x,pos)  			=> comp'longvid env x
        | EXP'Record exprow         => (*DeEnv.EXP'Record (comp'exprow (E env) exprow)*) error "exp/record"
        | EXP'Select lab            => (*DeEnv.EXP'Select lab*) error "exp/sel"
        | EXP'Unit                  => DeEnv.EXP'Tuple []
        | EXP'Tuple exps            => DeEnv.EXP'Tuple (map (E env) exps)
        | EXP'List exps             => E env (exp'list exps)
        | EXP'Vector exps           => (*DeEnv.EXP'Vector (map (E env) exps)*) abort "exp'vector"
        | EXP'Seq exps              => E env (exp'seq exps)
        | EXP'Let (dec,exp)         => let val (dec',env1) = D env dec
										   val exp' = E (env'plus env env1) exp
									   in DeEnv.EXP'Let (dec',exp')
									   end
        | EXP'App (e1,e2)           => DeEnv.EXP'App (E env e1,E env e2)
        | EXP'Typed (exp,ty)        => E env exp
        | EXP'Andalso (e1,e2)       => E env (exp'andalso (e1,e2))
        | EXP'Orelse (e1,e2)        => E env (exp'orelse (e1,e2))
        | EXP'Handle (exp,match)    => DeEnv.EXP'Handle (E env exp,comp'match E env match)
        | EXP'Raise exp             => DeEnv.EXP'Raise (E env exp)
        | EXP'If (e1,e2,e3)         => E env (exp'if (e1,e2,e3))
        | EXP'While (e1,e2)         => E env (expDerived'while (e1,e2))
        | EXP'Case (exp,match)      => DeEnv.EXP'Case (E env exp,comp'match E env match)
        | EXP'Fn match              => DeEnv.EXP'Fn (DeEnv.FUNC (comp'match E env match))

(*----------------------------------------------------------------------
INDEX: typbind / datbinda
----------------------------------------------------------------------*)

  fun comp'typbind (TYPBIND xs) =
	  let open Basis
	  in tyenv'plusList (map (fn (_,tycon,_) => tyenv'single (tycon,valenv'empty)) xs)
	  end

  fun comp'datbind (DATBIND xs) =
	  let open Basis
		  fun loop ve te [] = (ve,te)
			| loop ve te ((IGNORED_tyvars,tycon,clauses)::xs) =
			  let val ve' = 
					  valenv'plusList
						(map (fn (vid,IGNORED_tyo) =>
								 let val v = value'con0 vid
								 in valenv'single (vid,(v,Basis.IDSTATUS'C))
								 end) clauses)
				  val te' = tyenv'single (tycon,ve')
			  in loop (valenv'plus ve ve') (tyenv'plus te te') xs
			  end
	  in loop valenv'empty tyenv'empty xs
	  end

  fun comp'optWithType tbo =
	  case tbo of Some tb => comp'typbind tb | None => Basis.tyenv'empty

(*----------------------------------------------------------------------
INDEX: pes - (pat*exp) list 
----------------------------------------------------------------------*)

  fun pes'valbind vb =
	  case vb of		
		  VALBIND'Rec vb => pes'valbind vb (* 2nd inner rec ignored *)
		| VALBIND'Seq (pat,exp,None) => [(pat,exp)]
		| VALBIND'Seq (pat,exp,Some vb) => (pat,exp) :: pes'valbind vb


  fun xm'pe (PAT'Var (LONG([],vid)),EXP'Fn match) = (vid,match) 
	| xm'pe _ = abort "xm'pe"

(*----------------------------------------------------------------------
INDEX: valbind
----------------------------------------------------------------------*)

  fun comp'valbind E env vb =
	  case vb of		
		  VALBIND'Seq (pat,exp,None) => let val exp = E env exp
											val (pat,valenv) = comp'pat env pat
											val dec = DeEnv.DEC'Val (pat,exp)
										in (dec,valenv)
										end
		| VALBIND'Seq (pat,exp,Some vb1) => let val exp = E env exp
												val (pat,valenv1) = comp'pat env pat
												val dec1 = DeEnv.DEC'Val (pat,exp)
												val (dec2,valenv2) = comp'valbind E env vb1
												val valenv = valenv'plus valenv1 valenv2
												val dec = DeEnv.DEC'Seq (dec1,dec2)
										   in (dec,valenv)
										   end
		| VALBIND'Rec vb1 =>
		  let val xfs = map xm'pe (pes'valbind vb1)
			  val valenv = valenv'runtimeVars (map (fn (x,_) => x) xfs)
			  val env = env'plus env (env'valenv valenv)
			  val dec = DeEnv.DEC'Valrec (map (fn (x,match) => (x,DeEnv.FUNC (comp'match E env match))) xfs)
		  in (dec,valenv)
		  end

(*----------------------------------------------------------------------
INDEX: exbind
----------------------------------------------------------------------*)

  fun comp'exbind1 env b =
	  case b of
		  EXBIND1'New (vid,tyo) => let val valenv = valenv'single (vid,(None,Basis.IDSTATUS'E))
								   in (DeEnv.DEC'ExGen vid ,valenv)
								   end
		| EXBIND1'Copy (vid,longvid) =>
		  case (case (look'longvid env longvid)
				 of This(x,None)                  => error ("excopyunbound/"^sof'vid x)
				  | This(x,Some(None,ids))        => (ids, This x)
				  | This(vid,Some(Some v,ids))    => (ids, That v)
				  | That(v,ids)                   => (ids, That v))
		   of (Basis.IDSTATUS'V, _) => error "excopy/V"
			| (Basis.IDSTATUS'C, _) => error "excopy/C"
			| (Basis.IDSTATUS'E, That v) => (DeEnv.DEC'Empty,          valenv'single (vid,(Some v, Basis.IDSTATUS'E)))
			| (Basis.IDSTATUS'E, This x) => (DeEnv.DEC'ExCopy (vid,x), valenv'single (vid,(None, Basis.IDSTATUS'E)))


  fun comp'exbind env (EXBIND bs) = 
	  fold (fn (dec1,ve1) => 
			   fn b => let val (dec2,ve2) = comp'exbind1 env b
					   in (DeEnv.DEC'Seq (dec1,dec2), valenv'plus ve1 ve2)
					   end) (DeEnv.DEC'Empty,valenv'empty) bs


(*----------------------------------------------------------------------
INDEX: dec
----------------------------------------------------------------------*)

  fun U_comp'dec E D env dec = 
	  case dec of
		  DEC'Val vb => let val (dec,ve) = comp'valbind E env vb
						in (dec, env'valenv ve)
						end
		| DEC'Fval fb => let val (dec,ve) = comp'valbind E env (valbind'fvalbind fb)
						 in (dec, env'valenv ve)
						 end
		| DEC'Type typbind => (DeEnv.DEC'Empty, env'tyenv (comp'typbind typbind))
		| DEC'Datatype (db,tbo) =>
		  let val (ve,te) = comp'datbind db
			  val env1 = env'plus (env'valenv (comp'valenv ve)) (env'tyenv te)
			  val env2 = env'tyenv (comp'optWithType tbo)
		  in (DeEnv.DEC'Empty, env'plus env1 env2)
		  end
		| DEC'DataReplicate (tycon,tyconlong) =>
		  let val ve = look'longtycon env tyconlong
			  val env1 = env'valenv (comp'valenv ve)
			  val env2 = env'tyenv (Basis.tyenv'single (tycon,ve))
		  in (DeEnv.DEC'Empty, env'plus env1 env2)
		  end
		| DEC'Abstype (db,tbo,dec) =>
		  let val (ve,te_IGNORED) = comp'datbind db
			  val env1 = env'valenv (comp'valenv ve)
			  val env2 = env'tyenv (comp'optWithType tbo)
			  val (dec',env3) = D (env'plusList [env,env1,env2]) dec
		  in (dec', env'plus env2 env3)
		  end
		| DEC'Exception exbind => 
		  let val (dec,ve) = comp'exbind env exbind
		  in (dec, env'valenv ve)
		  end
		| DEC'Local (dec1,dec2) =>
		  let val (dec1',env1) = D env dec1
			  val (dec2',env2) = D (env'plus env env1) dec2
		  in (DeEnv.DEC'Local (dec1',dec2'), env2)
		  end
		| DEC'Open xs => (DeEnv.DEC'Empty, env'plusList (map (look'longstrid env) xs))
		| DEC'Empty => (DeEnv.DEC'Empty,env'empty)
		| DEC'Seq (dec1,dec2) =>
		  let val (dec1',env1) = D env dec1
			  val (dec2',env2) = D (env'plus env env1) dec2
		  in (DeEnv.DEC'Seq (dec1',dec2'),env'plus env1 env2)
		  end

(*----------------------------------------------------------------------
INDEX: fix compilation P/E/D
----------------------------------------------------------------------*)

  local
	  fun E env exp = U_comp'exp E D env exp
	  and D env dec = U_comp'dec E D env dec
  in
  fun comp'dec env dec = D (comp'env env) dec
  end
