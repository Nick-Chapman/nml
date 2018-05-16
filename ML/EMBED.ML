(*------------------------------------------------------------------------------
  CONTENTS-START-LINE: HERE=2 SEP=1
   35.    refs
   39.    export
   119.   imports
   323.   functor
   361.   con - type for datatype constructor, which know their siblings
   374.   uid - uniquified idents
   412.   source language generation - vids
   429.   source language generation - list(pat/exp), seq, if/andalso/orelse
   454.   source language generation - exp - while
   477.   source language generation - dec - fvalbind
   507.   source language generation - fvalbind
   523.   records -> tuples
   543.   Embed - types
   612.   kill 1-element tuple - exp/pat - as exmbed is built
   616.   collect vars bound by pat
   647.   Compile Environment
   694.   reify'env
   714.   env lookup
   746.   var/con status
   802.   pat - embed & build up vid->uid mapping in same step, thus avoding dup cals to makeUid
   836.   exp
   893.   typbind / datbind
   921.   valbind
   966.   exbind
   996.   Transform local decs
   1007.  dec
   1056.  fix embed schemes E/D
   1072.  unembed (for pp)
  CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: refs
----------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

(* Embed compile-time values / identifier status info into pat/exp/dec types
- all env lookups done at compile time 
- restrict val rec to: ident = fn ...
- combine match/mrule types - disallow zero mrules
- expand various non-bare langauge elements : fvalbind, while, if-then-else, andalso, orelse, lists
*)

signature EmbedSig =
sig

  type exname
  type value
  type prim

  type vid
  type scon
  type con

  type uid
  val newuid		: unit -> uid
  val eq'uid		: uid -> uid -> bool
  val sof'uid		: uid -> string

  datatype pat 
	= PAT'Wild
    | PAT'V         of uid
    | PAT'Layered   of uid * pat
    | PAT'Ref       of pat
    | PAT'C         of con * pat option
    | PAT'E         of exname * pat option
    | PAT'G         of uid * pat option
    | PAT'Scon      of scon
    | PAT'Tuple     of pat list

  datatype exp 
	= EXP'Value     of value * {info : (vid,scon) either} (* vid? *)
    | EXP'Var       of uid
	| EXP'NewEx		of vid (* still a vid - would a string do? *)
    | EXP'Tuple     of exp list
	| EXP'PrimApp	of prim * exp
	| EXP'Fn        of func
    | EXP'Raise     of exp
	| EXP'App       of exp * exp list
    | EXP'Handle    of exp * match
    | EXP'Let       of dec * exp
	| EXP'Case      of exp * match

  and func = FUNC'Def	of uid list * exp

  and match
	= MATCH'Alt     of match * match 
	| MATCH'Rule    of pat * exp

  and dec
	= DEC'Empty
    | DEC'Seq       of dec * dec
    | DEC'Val       of pat * exp
    | DEC'Valrec    of (uid * func) list


  val pat'tuple		: pat list -> pat  (* avoids singletons *)

  type env

  type Basis_env
  type Lang_dec

  (*type Lang_exp
  val embed'exp		: Basis_env -> Lang_exp -> exp*)

  val embed'dec     : Basis_env -> Lang_dec -> dec * env * {bound: uid list}
  val unembed'dec   : dec -> Lang_dec (* for pp *)

  val reify'env		: env -> (uid * value) list -> Basis_env

end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ~/project/prelude/PREL.ML *)
signature PrelSig_Import =
sig
	val abort			: string -> 'a
	val fold			: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
	val foldR			: ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
	val stringOfInt		: int -> string
	val forall			: ('a -> bool) -> 'a list -> bool
	val upto			: int * int -> int list
	val curry			: ('a * 'b -> 'c) -> 'a -> 'b -> 'c
end;
(*IMPORTDONE:*)

(*IMPORTSIG: SortSig ~/project/prelude/SORT.ML *)
signature SortSig_Import =
sig
    val sort : ('a * 'a -> bool) -> 'a list -> 'a list
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AssocSig ~/project/prelude/ASSOC.ML *)
signature AssocSig_Import =
sig
	type ('a,'b) assoc
	val assoc'empty		: ('a,'b) assoc
	val assoc'single	: 'a * 'b -> ('a,'b) assoc
	val assoc'list		: ('a * 'b) list -> ('a,'b) assoc
	val assoc'plus		: ('a,'b) assoc -> ('a,'b) assoc -> ('a,'b) assoc
	val map'assoc		: ('a * 'b -> 'c * 'd) -> ('a,'b) assoc -> ('c,'d) assoc
	val list'assoc		: ('a,'b) assoc -> ('a * 'b) list
	val find'assoc	    : ('a,'b) assoc -> ('a -> bool) -> 'b option
end;
(*IMPORTDONE:*)

(*IMPORTSIG: PosSig POS.ML *)
signature PosSig_Import =
sig
	type pos
	val makePos : {path: string, row: int, col: int} -> pos
	val sofPos : pos -> string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AtomSig ATOM.ML *)
signature AtomSig_Import =
sig
	datatype id = ID of string
	datatype vid = VID of id
    type tyvar;
    type tycon;
	datatype strid = STRID of id
	datatype 'a long = LONG of strid list * 'a
	datatype lab = LAB'Id of id | LAB'Num of int
    type scon;
	val sof'vid : vid -> string
	val sof'strid : strid -> string
	val sof'lab : lab -> string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: LangSig LANG.ML *)
signature LangSig_Import =
sig
	type vid
	type tyvar
	type tycon
	type strid
	type 'a long
	type lab
	type scon
	type pos
    type ty;
	datatype patrow = PATROW of (lab * pat) list * {wildcard: pos option}
	and pat =
		PAT'Wild
	  | PAT'Scon of scon
	  | PAT'Var of vid long
	  | PAT'Record of patrow
	  | PAT'Unit
	  | PAT'Tuple of pat list
	  | PAT'List of pat list
	  | PAT'Cons of vid long * pat
	  | PAT'Typed of pat * ty
	  | PAT'Layered of vid * ty option * pat
	datatype typbind =
		TYPBIND of (tyvar list * tycon * ty) list
	datatype datbind =
		DATBIND of (tyvar list * tycon * (vid * ty option) list) list
	datatype exbind1 =
		EXBIND1'New of vid * ty option
	  | EXBIND1'Copy of vid * vid long
	datatype exbind = EXBIND of exbind1 list
	datatype valbind =
		VALBIND'Seq of pat * exp * valbind option
	  | VALBIND'Rec of valbind
	and fvalbind = FVALBIND of (pos * vid * (pat list * ty option * exp) list) list
	and dec =
		DEC'Val of tyvar list * valbind
	  | DEC'Fval of fvalbind
	  | DEC'Type of typbind 
	  | DEC'Datatype of datbind * typbind option
	  | DEC'DataReplicate of tycon * tycon long
	  | DEC'Abstype of datbind * typbind option * dec
	  | DEC'Exception of exbind
	  | DEC'Local of dec * dec
	  | DEC'Open of strid long list
	  | DEC'Empty
	  | DEC'Seq of dec * dec
	and mrule = MRULE of pat * exp
	and match = MATCH of pos * mrule list
	and exprow = EXPROW of (lab * exp) list
	and exp =
		EXP'Scon of scon
	  | EXP'Var of vid long * pos
	  | EXP'Record of exprow
	  | EXP'Select of pos * lab
	  | EXP'Unit
	  | EXP'Tuple of exp list
	  | EXP'List of exp list
	  | EXP'Vector of exp list
	  | EXP'Seq of exp list
	  | EXP'Let of dec * exp
	  | EXP'App of exp * exp
	  | EXP'Typed of exp * ty
	  | EXP'Andalso of exp * exp
	  | EXP'Orelse of exp * exp
	  | EXP'Handle of exp * match
	  | EXP'Raise of exp
	  | EXP'If of exp * exp * exp
	  | EXP'While of exp * exp
	  | EXP'Case of exp * match
	  | EXP'Fn of match
	val exp'vid : pos -> vid -> exp
end;
(*IMPORTDONE:*)

(*IMPORTSIG: ValueSig VALUE.ML *)
signature ValueSig_Import =
sig
	type vid
	type lab
	type scon
	type exname
	type value
	type con
	
	val makeCons		: (vid * int) list -> con list
    val vid'con         : con -> vid
    type prim;
    val name'prim		: prim -> string
	val prim'value		: value -> prim option
	val value'scon		: scon -> value
	val value'con0		: con -> value
	val vid'exname		: exname -> vid
	val excon0'value	: string -> value -> exname
	val ref_or_con0'value : string -> value -> ({Ref:unit},{Con0:con}) either
											   
end;
(*IMPORTDONE:*)

(*IMPORTSIG: BasisSig BASIS.ML *)
signature BasisSig_Import =
sig
	type 'a long
	type vid
	type tycon
	type strid
	type value
	datatype idstatus = IDSTATUS'C | IDSTATUS'E | IDSTATUS'V
	type strenv 
	type tyenv 
	type valenv 
	type env
		 
	val strenv'empty	: strenv
	val tyenv'empty		: tyenv
	val valenv'empty	: valenv
	val env'empty		: env
	val env'se_te_ve	: strenv * tyenv * valenv -> env
	val env'tyenv		: tyenv -> env
	val env'valenv		: valenv -> env
	val strenv'plus		: strenv -> strenv -> strenv
	val tyenv'plus		: tyenv -> tyenv -> tyenv
	val valenv'plus		: valenv -> valenv -> valenv
	val env'plus		: env -> env -> env
	val tyenv'plusList	: tyenv list -> tyenv
	val valenv'plusList : valenv list -> valenv
	val env'plusList	: env list -> env
	val strenv'env		: env -> strenv
	val tyenv'env		: env -> tyenv
	val valenv'env		: env -> valenv
								 
	val tyenv'single	: tycon * valenv -> tyenv
	val valenv'single	: vid * (value * idstatus) -> valenv
	val look'longstrid	: string -> env -> strid long -> env
	val look'longtycon	: string -> env -> tycon long -> valenv
	val look'longvid	: string -> env -> vid long -> value * idstatus
	type ('a,'b) assoc
	val assoc'valenv	: valenv -> (vid, value * idstatus) assoc
	val valenv'assoc	: (vid, value * idstatus) assoc -> valenv
	
	val look'strid		: string -> strenv -> strid -> env
	val look'tycon		: string -> tyenv -> tycon -> valenv
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor EMBED
			(structure Prel : PrelSig_Import

			 val optimize_prim_app : bool ref
			 val optimize_multi_app : bool ref
			 val optimize_multi_fn : bool ref
							  
			 structure Sort : SortSig_Import
			 structure Assoc : AssocSig_Import
			 structure Pos : PosSig_Import
			 structure Atom : AtomSig_Import
			 structure Lang : LangSig_Import
			 structure Value : ValueSig_Import
			 structure Basis : BasisSig_Import

			 sharing type Assoc.assoc = Basis.assoc
			 sharing type Pos.pos = Lang.pos
			 sharing type Atom.lab = Lang.lab = Value.lab
			 sharing type Atom.scon = Lang.scon = Value.scon
			 sharing type Atom.long = Lang.long = Basis.long 
			 sharing type Atom.vid = Lang.vid = Value.vid = Basis.vid
			 sharing type Atom.strid = Lang.strid = Basis.strid
			 sharing type Atom.tycon = Lang.tycon = Basis.tycon
			 sharing type Value.value = Basis.value
							   ) : EmbedSig =
struct

  open Prel Sort Assoc Atom Lang Value

  fun error s = abort ("error: embed: "^s)
  fun notYet s = abort ("notYet: embed: "^s)

  fun dummyPos who = Pos.makePos{path=who,row=0,col=0}

(*----------------------------------------------------------------------
INDEX: uid - uniquified idents
----------------------------------------------------------------------*)

(*
  abstype uid = CUID of vid ref
with
  type uid = uid
  fun makeUid vid = CUID (ref vid)
  fun deUid (CUID vid) = !vid
  (*fun eq'uid (CUID x1) (CUID x2) = eq'vid (!x1) (!x2)*)
  fun eq'uid (CUID x1) (CUID x2) = x1 = x2
  fun sof'uid (CUID x) = sof'vid (!x)
end
*)


  abstype uid = CUID of vid * int
  with local val U = ref 1 in
	   fun makeUid vid = let val n = !U
						 in U := 1+n; CUID (vid, n)
						 end
	   fun deUid (CUID (vid,_)) = vid
	   fun eq'uid (CUID (_,n1)) (CUID (_,n2)) = (n1=n2)
	   fun sof'uid (CUID (vid,n)) = sof'vid vid (*^ "-" ^ stringOfInt n*)
	   end
  end


  (*fun newuid() = makeUid (VID (ID "U"))*)

  local val U = ref 1
  in fun newVid () = let val n = !U
					 in U := 1+n; VID (ID ("U-"^stringOfInt n))
					 end
  end
  fun newuid() = makeUid (newVid())

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


(*
  fun Function'effects (name,arg,return,f) effects path =
	  let val value = value'builtin (builtin'fn (path^name, effects, arity arg, fn x => from return (f (back arg name x))))
	  in env'valenv (valenv'single (VID (ID name), (value,IDSTATUS'V)))
	  end
  fun FunctionV q = Function'effects q EFF'VarAlloc
					 FunctionV ("fromList",LIST(V),VECTOR_V		, Vector.fromList)
  fun exp'vector es = EXP'App (exp'vectorFromList, exp'list es)
*)

  (* bit of a hack - lookup Vector.fromList - but might be rebound! *)
  val exp'vectorFromList  = EXP'Var (LONG([STRID(ID"Vector")],VID(ID"fromList")), dummyPos "exp'vectorFromList")
  fun exp'vector es = EXP'App (exp'vectorFromList, exp'list es)

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
		   ([],VALBIND'Rec 
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


  fun exp'fnPat pos pat body = EXP'Fn (MATCH (pos,[MRULE (pat,body)]))
  fun exp'fnPats pos [] body = body
	| exp'fnPats pos (x::xs) body = exp'fnPat pos x (exp'fnPats pos xs body)
									
  fun exp'clauses prefix pos [] = error "exp'clauses:[]"
(*de-optimization!!
	(* special case for 1 clause *)
	| exp'clauses prefix pos [(pats,_,exp)] = exp'fnPats pos pats exp
*)
	(* general cases for 2 or more clauses*)
	| exp'clauses prefix pos (clauses as (pats,_,_)::_) =
	  let (*val vids = genVids prefix (length pats)*)
		  val vids = map (fn n => VID (ID (prefix^"-"^stringOfInt n))) (upto (1,length pats))
	  in exp'fnVids pos vids
		 (EXP'Case (EXP'Tuple (map (exp'vid pos) vids),
					MATCH (pos,map mrule'clause clauses)))
	  end

(*----------------------------------------------------------------------
INDEX: source language generation - fvalbind
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
INDEX: records -> tuples
----------------------------------------------------------------------*)

  fun leq'id (ID x1, ID x2) = (x1 <= x2)

  val leq'lab =
	  fn (LAB'Num x1, LAB'Num x2) => (x1<=x2)
	   | (LAB'Num _, LAB'Id _) => true
	   | (LAB'Id _, LAB'Num _) => false
	   | (LAB'Id x1, LAB'Id x2) => leq'id (x1,x2)


  fun leq'keyed leq ((k1,v1),(k2,v2)) = leq (k1,k2)

  fun tuple'patrow (PATROW (xs,{wildcard=Some pos})) = abort ("tuple'patrow/wildcard: "^Pos.sofPos pos)
	| tuple'patrow (PATROW (xs,{wildcard=None})) = PAT'Tuple (map (fn (lab,pat) => pat) (Sort.sort (leq'keyed leq'lab) xs))

  fun tuple'exprow (EXPROW xs) =  EXP'Tuple (map (fn (lab,exp) => exp) (Sort.sort (leq'keyed leq'lab) xs))

(*----------------------------------------------------------------------
INDEX: Embed - types
----------------------------------------------------------------------*)

structure Embed =
struct

  datatype pat 
	= PAT'Wild
    | PAT'V         of uid
    | PAT'Layered   of uid * pat
    | PAT'Ref       of pat
    | PAT'C         of con * pat option
    | PAT'E         of exname * pat option
    | PAT'G         of uid * pat option (* runtime generated exception constructor *)
    | PAT'Scon      of scon
    | PAT'Tuple     of pat list

  datatype exp 
	= EXP'Value     of value * {info : (vid,scon) either} (* scon, or created by de-env *)
    | EXP'Var       of uid
    | EXP'Tuple     of exp list
    | EXP'Let       of dec * exp
	| EXP'App       of exp * exp list
	| EXP'Case      of exp * match
    | EXP'Handle    of exp * match
    | EXP'Raise     of exp
	| EXP'Fn        of func
	| EXP'NewEx		of vid
	| EXP'PrimApp	of prim * exp

  and func = FUNC'Def	of uid list * exp

  and match
	= MATCH'Alt     of match * match 
	| MATCH'Rule    of pat * exp

  and dec
	= DEC'Empty
    | DEC'Seq       of dec * dec
    | DEC'Val       of pat * exp
    | DEC'Valrec    of (uid * func) list

  val exp'tuple = fn [e] => e | es => EXP'Tuple es


  val isWild'pat = fn PAT'Wild => true | _ => false

  val pat'tuple = fn [e] => e | es => if (forall isWild'pat es) then PAT'Wild else
									  PAT'Tuple es

  fun exp'app (func,arg) =		
	  case (case (!optimize_prim_app,func) 
			 of (true,EXP'Value (v,_)) => prim'value v 
			  | _ => None) 
	   of Some prim => (EXP'PrimApp (prim,arg))
		| None => case (!optimize_multi_app, func) of 
					  (true,EXP'App(func,args)) => EXP'App (func,args@[arg])
					| _ => EXP'App (func,[arg])
							  
  local val X = makeUid (VID (ID "fn-arg")) (* not a legal ML ident - so wont capture at user free vars *)
  in fun func'def match = 
		 case (!optimize_multi_fn, match) of
			 (true, MATCH'Rule (PAT'V x,EXP'Fn (FUNC'Def (xs,exp)))) => FUNC'Def (x::xs,exp)
		   | (_,MATCH'Rule (PAT'V x,exp)) => FUNC'Def ([x],exp)
		   | _ => FUNC'Def ([X],EXP'Case (EXP'Var X, match))
  end
end

(*----------------------------------------------------------------------
INDEX: kill 1-element tuple - exp/pat - as exmbed is built
----------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: collect vars bound by pat
----------------------------------------------------------------------*)

  local
	  open Embed
	  fun P acc pat =
		  case pat of
              PAT'Wild              => acc
            | PAT'V x               => x :: acc
            | PAT'Layered (x,pat)   => P (x :: acc) pat
            | PAT'Ref pat           => P acc pat
            | PAT'C (_,None)        => acc
            | PAT'C (_,Some pat)    => P acc pat
            | PAT'E (_,None)        => acc
            | PAT'E (_,Some pat)    => P acc pat
            | PAT'G (_,None)        => acc
            | PAT'G (_,Some pat)    => P acc pat
            | PAT'Scon _            => acc
            | PAT'Tuple pats        => fold P acc pats
      fun D acc dec =
		  case dec of
              DEC'Empty             => acc
            | DEC'Seq (dec1,dec2)   => D (D acc dec1) dec2
            | DEC'Val (pat,_)       => P acc pat
            | DEC'Valrec binds		=> fold (fn acc => fn (x,_) => x :: acc) acc binds
  in
  val boundVars'pat = P []
  val boundVars'dec = D []
  end

(*----------------------------------------------------------------------
INDEX: Compile Environment
----------------------------------------------------------------------*)

  datatype valenv = VALENV of (vid, (value,uid) either * Basis.idstatus) assoc (* None, means value not available at CT *)

  val valenv'empty = VALENV assoc'empty
  fun valenv'plus (VALENV a1) (VALENV a2) = VALENV (assoc'plus a1 a2)
  val valenv'plusList = foldR valenv'plus valenv'empty
  fun valenv'single kv = VALENV (assoc'single kv)
  fun embed'valenv x = VALENV (map'assoc (fn (k,(v,ids)) => (k,(This v,ids))) (Basis.assoc'valenv x))
  (*fun isUnbound'vid (VALENV assoc) vid = case (lookup'assoc assoc vid) of None => true | Some _ => false*)
  (*fun look'vid who (VALENV a) k = look'assoc who sof'vid a k*)

  fun valenv'xus xus = 
	  valenv'plusList (map (fn (x,u) => valenv'single (x,(That u,Basis.IDSTATUS'V))) xus)

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
  fun embed'env env =
	  ENV (Basis.strenv'env env,
		   Basis.tyenv'env env,
		   embed'valenv (Basis.valenv'env env))


  (* version of isUnbound'vid which excercises exception raise/handle... *)
  exception NotFound
  fun X_lookup'assoc A k =
	  let fun look [] = raise NotFound
			| look ((k',v)::xs) = if (k=k') then v else look xs
	  in look (list'assoc A)
	  end

  (*fun isUnbound'vid (VALENV assoc) vid = (X_lookup'assoc assoc vid; false) handle NotFound => true*)

  fun X_look'vid (VALENV assoc) k = X_lookup'assoc assoc k

(*----------------------------------------------------------------------
INDEX: reify'env
----------------------------------------------------------------------*)

  fun valenv'CT_RT (VALENV a) rtAssoc =
	  let fun LOOK uid = case (find'assoc rtAssoc (eq'uid uid)) of None => abort ("valenv'CT_RT/"^sof'uid uid)
																 | Some v => v
	  in
		  Basis.valenv'assoc
		  (map'assoc (fn (vid,(This v,ids)) => (vid, (v,ids))
					   | (vid,(That uid,ids)) => (vid, (LOOK uid, ids)
													   )) a)
	  end


  fun reify'env (ENV(se,te,ve)) rtBinds =
	  let val ve = valenv'CT_RT ve (assoc'list rtBinds)
	  in Basis.env'se_te_ve (se,te,ve)
	  end

(*----------------------------------------------------------------------
INDEX: env lookup
----------------------------------------------------------------------*)

  fun longOrShort'long (LONG ([],x)) = This x
	| longOrShort'long (LONG (s1::ss,x)) = That (s1,LONG(ss,x))

  fun look'longstrid (ENV (strenv,_,_)) longx =
	  case longOrShort'long longx
	   of This x => Basis.look'strid "" strenv x
		| That (strid,longx) => Basis.look'longstrid (sof'strid strid^".") (Basis.look'strid "" strenv strid) longx
	  
  fun look'longtycon (ENV (strenv,tyenv,_)) longx =
	  case longOrShort'long longx
	   of This x => Basis.look'tycon "" tyenv x
		| That (strid,longx) => Basis.look'longtycon (sof'strid strid^".") (Basis.look'strid "" strenv strid) longx

(*
  fun look'longvid (ENV (strenv,_,valenv)) longx =
	  case longOrShort'long longx
	   of This x => This (x, if (isUnbound'vid valenv x) 
							 then None
							 else Some (look'vid "" valenv x))
		| That (strid,longx) => That (Basis.look'longvid (sof'strid strid^".") (Basis.look'strid "" strenv strid) longx)
*)

(* version which exercises exception handling... *)
  fun look'longvid (ENV (strenv,_,valenv)) longx =
	  case longOrShort'long longx
	   of This x => (This (x, Some (X_look'vid valenv x)) handle NotFound => This (x,None))
		| That (strid,longx) => That (Basis.look'longvid (sof'strid strid^".") (Basis.look'strid "" strenv strid) longx)

(*----------------------------------------------------------------------
INDEX: var/con status
----------------------------------------------------------------------*)

  fun embed'vid vid acc = let val uid = makeUid vid
						  in ((vid,uid)::acc, Embed.PAT'V uid)
						  end

  fun embed'var env longx acc =
	  case (look'longvid env longx) 
	   of This (vid,None) => embed'vid vid acc (*unbound/short*)
		| This (vid,Some(voru,ids)) => (*short*)
		  (case ids
			of Basis.IDSTATUS'V => embed'vid vid acc
			 | Basis.IDSTATUS'E => (acc, 
									case voru of 
										That uid => Embed.PAT'G (uid,None)
									  | This v => Embed.PAT'E (excon0'value "patvar/short/E" v,None))
			 | Basis.IDSTATUS'C => (acc,
									case voru of
										That _ => error "patvar/short/C/RT"
									  | This v => (case (ref_or_con0'value "patvar/short/C" v)
													of This {Ref=()} => error "patvar/short/ref"
													 | That {Con0=con} => Embed.PAT'C (con,None))))
		| That (v,ids) => (*long*)
		  (acc,
		   case ids
			of Basis.IDSTATUS'V => error "patvar/long/V"
			 | Basis.IDSTATUS'E => Embed.PAT'E (excon0'value "patvar/long/E" v,None)
			 | Basis.IDSTATUS'C => (case (ref_or_con0'value "patvar/long/C" v)
									 of This {Ref=()} => error "patvar/long/ref"
									  | That {Con0=con} => Embed.PAT'C (con,None)))


  fun embed'cons env longx pat =
	  case (look'longvid env longx) 
	   of This (vid,None) => error ("patcon/unbound:"^sof'vid vid)
		| This (vid,Some(voru,ids)) => (*short*)
		  (case ids
			of Basis.IDSTATUS'V => error "patcon/short/V"
			 | Basis.IDSTATUS'E => (case voru of 
										That uid => Embed.PAT'G (uid,Some pat)
									  | This v => Embed.PAT'E (excon0'value "patcon/short/E" v,Some pat))
			 | Basis.IDSTATUS'C => (case voru of
										That _ => error "patcon/short/C/RT"
									  | This v => (case (ref_or_con0'value "patcon/short/C" v)
													of This {Ref=()} => Embed.PAT'Ref pat
													 | That {Con0=con} => Embed.PAT'C (con,Some pat))))
		| That (v,ids) => (*long*)
		  (case ids
			of Basis.IDSTATUS'V => error "patcon/long/V"
			 | Basis.IDSTATUS'E => Embed.PAT'E (excon0'value "patcon/long/E" v,Some pat)
			 | Basis.IDSTATUS'C => (case (ref_or_con0'value "patcon/long/C" v)
									 of This {Ref=()} => Embed.PAT'Ref pat
									  | That {Con0=con} => Embed.PAT'C (con,Some pat)))

(*----------------------------------------------------------------------
INDEX: pat - embed & build up vid->uid mapping in same step, thus avoding dup cals to makeUid
----------------------------------------------------------------------*)

  fun U_embed'pat env P acc pat =
	  case pat of
          PAT'Wild                  => (acc,Embed.PAT'Wild)
        | PAT'Scon scon             => (acc,Embed.PAT'Scon scon)
        | PAT'Var x                 => embed'var env x acc
        | PAT'Record pr             => P acc (tuple'patrow pr)
        | PAT'Unit                  => (acc, Embed.pat'tuple [])
        | PAT'Tuple pats            => (case (fold (fn (acc,qs) => fn pat => let val (acc,q) = P acc pat
																			 in (acc,q::qs)
																			 end) (acc,[]) pats) 
										 of (acc,qs) => (acc,Embed.pat'tuple (rev qs)))
        | PAT'List pats             => P acc (pat'list pats)
        | PAT'Cons (x,pat)          => let val (acc,q) = P acc pat in (acc, embed'cons env x q) end
        | PAT'Typed (pat,ty)        => P acc pat
        | PAT'Layered (vid,tyo,pat)	=> let val uid = makeUid vid
										   val acc = (vid,uid)::acc
										   val (acc,q) = P acc pat
									   in (acc, Embed.PAT'Layered (uid,q))
									   end

  local fun P env pat = U_embed'pat env (P env) pat
  in val TWO_embed'pat = P
  end
	
  fun embed'pat env pat =
	  let val (acc,pat') = TWO_embed'pat env [] pat
		  val valenv = valenv'xus acc
	  in (pat',valenv)
	  end

(*----------------------------------------------------------------------
INDEX: exp
----------------------------------------------------------------------*)

  fun infoVid (LONG (_,vid)) = {info = This vid}

  fun embed'longvid env longvid =
	  case (look'longvid env longvid)
	   (* strange (inefficient) match order just to streach nml pat compiler!... when I finally code it!*)
       of This(x,None)              => error ("embed'longvid/unbound/"^sof'vid x)
        | This(x,Some(That uid,_))  => Embed.EXP'Var uid (* short/RT - use uniquified identifier *)
        | That(v,_)                 => Embed.EXP'Value (v,infoVid longvid) (* long/CT - embed value *)
        | This(vid,Some(This v ,_)) => Embed.EXP'Value (v,infoVid longvid) (* short/CT - embed value *)


  fun embed'mrule E env (MRULE (pat,exp)) = let val (pat',ve) = embed'pat env pat
											   val env = env'plus env (env'valenv ve)
										   in Embed.MATCH'Rule (pat',E env exp)
										   end

  fun embed'match E env (MATCH (pos,[])) = error "embed'match:[]"
	| embed'match E env (MATCH (pos,x::xs)) = 
	  fold (curry Embed.MATCH'Alt) (embed'mrule E env x) (map (embed'mrule E env) xs)

(*
  fun embed'exprow Eshad exprow =
	  case exprow of
		  EXPROW xs => Embed.EXPROW (map (fn (lab,exp) => (lab,Eshad exp)) xs)
*)


  fun U_embed'exp E D env exp =
	  case exp of
          EXP'Scon scon             => Embed.EXP'Value (value'scon scon, {info = That scon})
        | EXP'Var (x,pos)  			=> embed'longvid env x
        | EXP'Record exprow         => E env (tuple'exprow exprow)
        | EXP'Select (pos,lab)      => (*Embed.EXP'Select lab*) notYet ("exp/sel/" ^ sof'lab lab ^ Pos.sofPos pos)
        | EXP'Unit                  => Embed.exp'tuple []
        | EXP'Tuple exps            => Embed.exp'tuple (map (E env) exps)
        | EXP'List exps             => E env (exp'list exps)
        (*| EXP'Vector exps           => (*Embed.EXP'Vector (map (E env) exps)*) notYet "exp'vector"*)
        | EXP'Vector exps           => E env (exp'vector exps)
        | EXP'Seq exps              => E env (exp'seq exps)
        | EXP'Let (dec,exp)         => let val (dec',env1) = D env dec
										   val exp' = E (env'plus env env1) exp
									   in Embed.EXP'Let (dec',exp')
									   end
        | EXP'App (e1,e2)           => Embed.exp'app (E env e1,E env e2)
        | EXP'Typed (exp,ty)        => E env exp
        | EXP'Andalso (e1,e2)       => E env (exp'andalso (e1,e2))
        | EXP'Orelse (e1,e2)        => E env (exp'orelse (e1,e2))
        | EXP'Handle (exp,match)    => Embed.EXP'Handle (E env exp,embed'match E env match)
        | EXP'Raise exp             => Embed.EXP'Raise (E env exp)
        | EXP'If (e1,e2,e3)         => E env (exp'if (e1,e2,e3))
        | EXP'While (e1,e2)         => E env (expDerived'while (e1,e2))
        | EXP'Case (exp,match)      => Embed.EXP'Case (E env exp,embed'match E env match)
        | EXP'Fn match              => Embed.EXP'Fn (Embed.func'def (embed'match E env match))

(*----------------------------------------------------------------------
INDEX: typbind / datbind
----------------------------------------------------------------------*)

  fun embed'typbind (TYPBIND xs) =
	  let open Basis
	  in tyenv'plusList (map (fn (_,tycon,_) => tyenv'single (tycon,valenv'empty)) xs)
	  end

  val arity'tyo = fn None => 0 | Some IGNORED_ty => 1 (* may like to count the number of tuple elements *)

  fun embed'datbind (DATBIND xs) =
	  let open Basis
		  fun loop ve te [] = (ve,te)
			| loop ve te ((IGNORED_tyvars,tycon,clauses)::xs) =
			  let 
				  val vids_and_aritys = map (fn (vid,tyo) => (vid,arity'tyo tyo)) clauses
				  val cons = makeCons vids_and_aritys (* so each con knows it's siblings *)
				  val ve' = 
					  valenv'plusList
						(map (fn con =>
								 let val v = value'con0 con
								 in valenv'single (vid'con con,(v,Basis.IDSTATUS'C))
								 end) cons)
				  val te' = tyenv'single (tycon,ve')
			  in loop (valenv'plus ve ve') (tyenv'plus te te') xs
			  end
	  in loop valenv'empty tyenv'empty xs
	  end

  fun embed'optWithType tbo =
	  case tbo of Some tb => embed'typbind tb | None => Basis.tyenv'empty

(*----------------------------------------------------------------------
INDEX: valbind
----------------------------------------------------------------------*)

  fun vid'pat pat =
	  case pat of
		  PAT'Typed (pat,_)  => vid'pat pat
		| PAT'Var (LONG([],vid)) => vid
		| PAT'Var (LONG(_::_,vid)) => abort "vid'pat/long"
		| _ => abort "vid'pat"

  val match'exp =
	  fn EXP'Fn match => match
	   | _ => abort "match'exo"

  fun xfs'valbind vb =
	  case vb of		
		  VALBIND'Rec vb => xfs'valbind vb (* 2nd inner rec ignored *)
		| VALBIND'Seq (pat,exp,None) => [(vid'pat pat,match'exp exp)]
		| VALBIND'Seq (pat,exp,Some vb) => (vid'pat pat,match'exp exp) :: xfs'valbind vb


  fun embed'valbind E env vb =
	  case vb of		
		  VALBIND'Seq (pat,exp,None) => let val exp = E env exp
											val (pat,valenv) = embed'pat env pat
											val dec = Embed.DEC'Val (pat,exp)
										in (dec,valenv)
										end
		| VALBIND'Seq (pat,exp,Some vb1) => let val exp = E env exp
												val (pat,valenv1) = embed'pat env pat
												val dec1 = Embed.DEC'Val (pat,exp)
												val (dec2,valenv2) = embed'valbind E env vb1
												val valenv = valenv'plus valenv1 valenv2
												val dec = Embed.DEC'Seq (dec1,dec2)
										   in (dec,valenv)
										   end
		| VALBIND'Rec vb1 =>
		  let val xufs = map (fn (x,f) => (x,makeUid x,f)) (xfs'valbind vb1)
			  val valenv = valenv'xus (map (fn (x,u,_) => (x,u)) xufs)
			  val env = env'plus env (env'valenv valenv)
			  val dec = Embed.DEC'Valrec (map (fn (_,u,match) => (u,Embed.func'def (embed'match E env match))) xufs)
		  in (dec,valenv)
		  end

(*----------------------------------------------------------------------
INDEX: exbind
----------------------------------------------------------------------*)

  fun embed'exbind1 env b =
	  case b of
		  EXBIND1'New (vid,tyo) => let val uid = makeUid vid
									   val valenv = valenv'single (vid,(That uid,Basis.IDSTATUS'E))
									   val exp = Embed.EXP'NewEx vid
									   val dec = Embed.DEC'Val (Embed.PAT'V uid, exp)
								   in (dec,valenv)
								   end
		| EXBIND1'Copy (vid,longY) =>
		  case (case (look'longvid env longY)
				 of This(y,None)                  => error ("excopy/unbound/"^sof'vid y)
				  | This(_,Some(voru,ids))        => (ids, voru)
				  | That(v,ids)                   => (ids, This v))
		   of (Basis.IDSTATUS'V, _) => error "excopy/V"
			| (Basis.IDSTATUS'C, _) => error "excopy/C"
			| (Basis.IDSTATUS'E, voru) => let val valenv = valenv'single (vid,(voru, Basis.IDSTATUS'E))
										  in (Embed.DEC'Empty, valenv)
										  end


  fun embed'exbind env (EXBIND bs) = 
	  fold (fn (dec1,ve1) => 
			   fn b => let val (dec2,ve2) = embed'exbind1 env b
					   in (Embed.DEC'Seq (dec1,dec2), valenv'plus ve1 ve2)
					   end) (Embed.DEC'Empty,valenv'empty) bs

(*----------------------------------------------------------------------
INDEX: Transform local decs
----------------------------------------------------------------------*)

  fun Embed_Transform_DEC'Local (D1,D2) =
	  (* local D1 in D2 end --> val B = let D1 in let D2 in B end end -- (B is bound-vars of D2) *)
	  let val B = boundVars'dec D2
	  in Embed.DEC'Val (Embed.pat'tuple (map Embed.PAT'V B),
						Embed.EXP'Let (D1, Embed.EXP'Let (D2, Embed.exp'tuple (map Embed.EXP'Var B))))
	  end

(*----------------------------------------------------------------------
INDEX: dec
----------------------------------------------------------------------*)

  fun U_embed'dec E D env dec = 
	  case dec of
		  DEC'Val (tvs,vb) => let val (dec,ve) = embed'valbind E env vb
							  in (dec, env'valenv ve)
							  end
		| DEC'Fval fb => let val (dec,ve) = embed'valbind E env (valbind'fvalbind fb)
						 in (dec, env'valenv ve)
						 end
		| DEC'Type typbind => (Embed.DEC'Empty, env'tyenv (embed'typbind typbind))
		| DEC'Datatype (db,tbo) =>
		  let val (ve,te) = embed'datbind db
			  val env1 = env'plus (env'valenv (embed'valenv ve)) (env'tyenv te)
			  val env2 = env'tyenv (embed'optWithType tbo)
		  in (Embed.DEC'Empty, env'plus env1 env2)
		  end
		| DEC'DataReplicate (tycon,tyconlong) =>
		  let val ve = look'longtycon env tyconlong
			  val env1 = env'valenv (embed'valenv ve)
			  val env2 = env'tyenv (Basis.tyenv'single (tycon,ve))
		  in (Embed.DEC'Empty, env'plus env1 env2)
		  end
		| DEC'Abstype (db,tbo,dec) =>
		  let val (ve,te_IGNORED) = embed'datbind db
			  val env1 = env'valenv (embed'valenv ve)
			  val env2 = env'tyenv (embed'optWithType tbo)
			  val (dec',env3) = D (env'plusList [env,env1,env2]) dec
		  in (dec', env'plus env2 env3)
		  end
		| DEC'Exception exbind => 
		  let val (dec,ve) = embed'exbind env exbind
		  in (dec, env'valenv ve)
		  end
		| DEC'Local (dec1,dec2) =>
		  let val (dec1',env1) = D env dec1
			  val (dec2',env2) = D (env'plus env env1) dec2
		  in (Embed_Transform_DEC'Local (dec1',dec2'), env2)
		  end
		| DEC'Open xs => (Embed.DEC'Empty, env'plusList (map (embed'env o look'longstrid env) xs))
		| DEC'Empty => (Embed.DEC'Empty,env'empty)
		| DEC'Seq (dec1,dec2) =>
		  let val (dec1',env1) = D env dec1
			  val (dec2',env2) = D (env'plus env env1) dec2
		  in (Embed.DEC'Seq (dec1',dec2'),env'plus env1 env2)
		  end

(*----------------------------------------------------------------------
INDEX: fix embed schemes E/D
----------------------------------------------------------------------*)

  local fun E env exp = U_embed'exp E D env exp
		and D env dec = U_embed'dec E D env dec
  in fun embed'dec env dec = let val (dec,env) = D (embed'env env) dec
								 val xs = boundVars'dec dec
							 in (dec,env,{bound=xs})
							 end
	 fun embed'exp env exp = E (embed'env env) exp
  end

  open Embed
  type Basis_env = Basis.env
  type Lang_dec = Lang.dec
  (*type Lang_exp = Lang.exp*)

(*----------------------------------------------------------------------
INDEX: unembed (for pp)
----------------------------------------------------------------------*)

  fun Lang_valbind'pes [] = abort "Lang_valbind'pes[]"
	| Lang_valbind'pes [(p,e)] = Lang.VALBIND'Seq (p,e,None)
	| Lang_valbind'pes ((p,e)::pes) = Lang.VALBIND'Seq (p,e,Some(Lang_valbind'pes pes))

  fun Lang_mrules match = Lang.MATCH (dummyPos "Lang_match",match)

  fun long x = LONG([],x)
  fun Lang_Var x = Lang.EXP'Var (long x,dummyPos "Lang_var")

  type unembed_schemes = {P	: Embed.pat -> Lang.pat,
						  E	: Embed.exp -> Lang.exp,
						  M : Embed.match -> Lang.mrule list,
						  D : Embed.dec -> Lang.dec}

  fun unembed_F (P,E) (FUNC'Def (xs,exp)) = 
	  fold (fn exp => fn x => Lang.EXP'Fn (Lang_mrules [Lang.MRULE (P (PAT'V x),exp)])) (E exp) xs

  fun unembed_E ({E,M,D,P}:unembed_schemes) = 
      fn EXP'Value (_,{info})		=> (case info of This x => Lang_Var x | That scon => Lang.EXP'Scon scon)
       | EXP'Var x                  => Lang_Var (deUid x)
       | EXP'Tuple exps             => Lang.EXP'Tuple (map E exps)
       | EXP'Let (dec,exp)          => Lang.EXP'Let (D dec, E exp)
       | EXP'Case (exp,match)       => Lang.EXP'Case (E exp, Lang_mrules (M match))
       | EXP'Raise exp              => Lang.EXP'Raise (E exp)
       | EXP'Fn func				=> unembed_F (P,E) func
       | EXP'App (func,args)        => fold (curry Lang.EXP'App) (E func) (map E args)
       | EXP'Handle (exp,match)     => Lang.EXP'Handle (E exp,Lang_mrules (M match))
       | EXP'NewEx x				=> Lang.EXP'App (Lang_Var (VID (ID "exception")), Lang_Var x)
       | EXP'PrimApp (prim,arg) => Lang.EXP'App (Lang_Var (VID (ID ("prim:"^name'prim prim))), E arg)

  fun unembed_M ({P,E,M,D}:unembed_schemes) = 
      fn MATCH'Alt (m1,m2)			=> M m1 @  M m2
       | MATCH'Rule (pat,exp)       => [Lang.MRULE (P pat, E exp)]

  fun unembed_D ({P,E,D,M}:unembed_schemes) =
      fn DEC'Empty                  => Lang.DEC'Empty
       | DEC'Seq (dec1,dec2)        => Lang.DEC'Seq (D dec1, D dec2)
       | DEC'Val (pat,exp)          => Lang.DEC'Val ([],Lang.VALBIND'Seq (P pat, E exp, None))
       | DEC'Valrec binds           => Lang.DEC'Val 
									   ([],Lang.VALBIND'Rec
										(Lang_valbind'pes 
										 (map (fn (x,func) => (Lang.PAT'Var (long (deUid x)), unembed_F (P,E) func)) binds)))

  fun unembed_P {P} =
	  fn PAT'Wild              => Lang.PAT'Wild
	   | PAT'V x               => Lang.PAT'Var (long (deUid x))
	   | PAT'Layered (x,pat)   => Lang.PAT'Layered (deUid x,None,P pat)
	   | PAT'Ref pat           => Lang.PAT'Cons (long (VID (ID "ref")), P pat)
	   | PAT'C (x,None)        => Lang.PAT'Var (long (vid'con x))
	   | PAT'C (x,Some pat)    => Lang.PAT'Cons (long (vid'con x), P pat)
	   | PAT'E (ex,None)       => Lang.PAT'Var (long (vid'exname ex))
	   | PAT'E (ex,Some pat)   => Lang.PAT'Cons (long (vid'exname ex), P pat)
	   | PAT'G (x,None)        => Lang.PAT'Var (long (deUid x))
	   | PAT'G (x,Some pat)    => Lang.PAT'Cons (long (deUid x), P pat)
	   | PAT'Scon scon         => Lang.PAT'Scon scon
	   | PAT'Tuple pats        => Lang.PAT'Tuple (map P pats)

  local
      fun P  x = unembed_P {P=P} x
	  fun makeSchemes () = {P=P,E=E,M=M,D=D}
      and E  x = unembed_E    (makeSchemes()) x
      and M  x = unembed_M    (makeSchemes()) x
      and D  x = unembed_D    (makeSchemes()) x
  in val unembed_schemes = makeSchemes()
  end

  fun unembed'dec x =
	  let val {D,P,E,M} = unembed_schemes
	  in D x
	  end


end (* functor *)
