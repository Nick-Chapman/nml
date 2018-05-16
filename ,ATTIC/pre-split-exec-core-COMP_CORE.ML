(*------------------------------------------------------------------------------

 CONTENTS-START-LINE: HERE=3 SEP=1
  37.    export
  49.    imports
  298.   functor
  341.   valstat - represent just possible states of (value*idstatus)
  375.   Compile Environment
  435.   merge CT/RT envs...
  458.   patStatus
  508.   Code - runtime versions of: pat,exp,dec etc
  575.   compc'* - compile-time
  581.   shad
  590.   shad'pat
  619.   comp'pat
  645.   NEW comp'pat
  665.   comp'exp
  716.   source language generation -- support fvalbind clauses
  748.   source language generation - while exp
  752.   NEW comp'dec
  903.   fix compilation P/E/D
  914.   exec'* - runtime
  921.   runtime counts
  936.   pat
  1057.  exval'match
  1068.  mrule,match
  1089.  makeEvalClosure
  1102.  varLookup
  1111.  exp
  1186.  dec
  1235.  fix -- pat/exp/dec
  1249.  eval - sequence comp/exec
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature CompCoreSig =
sig
	type dec
	type env
	type exval
	val eval'dec : env -> dec -> (env,exval) either
end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ../prelude/Prel.ML *)
signature PrelSig_Import =
sig
	val echo			: string -> unit
	val abort			: string -> 'a
	val fold			: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
	val foldR			: ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
	val stringOfInt		: int -> string
	val exists			: ('a -> bool) -> 'a list -> bool
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AssocSig ../prelude/ASSOC.ML *)
signature AssocSig_Import =
    sig
	type ('a,'b) assoc
        val assoc'empty     : ('a,'b) assoc
        val assoc'single    : 'a * 'b -> ('a,'b) assoc
        val assoc'plus      : ('a,'b) assoc -> ('a,'b) assoc -> ('a,'b) assoc
        val map'assoc       : ('a * 'b -> 'c * 'd) -> ('a,'b) assoc -> ('c,'d) assoc
        val lookup'assoc    : (''a,'b) assoc -> ''a -> 'b option
        val look'assoc      : string -> (''a -> string) -> (''a,'b) assoc -> ''a -> 'b
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

(*IMPORTSIG: MisclaySig ../prelude/MISCLAY.ML *)
signature MisclaySig_Import =
sig
  type layout
  val show_layout               : layout -> string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AtomSig ATOM.ML *)
signature AtomSig_Import =
sig
	datatype id = ID of string
	datatype vid = VID of id
    type tyvar;
    type tycon;
    type strid;
	datatype 'a long = LONG of strid list * 'a
	datatype lab = LAB'Id of id | LAB'Num of int
    type scon;
	val sof'vid : vid -> string
	val sof'strid : strid -> string
	val eq'vid : vid -> vid -> bool
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
	datatype patrow = PATROW of (lab * pat) list * {wildcard:bool}
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
		DEC'Val of valbind
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
	  | EXP'Select of lab
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
	type record
	type closure
	type exval
		 
	val apply'value		: value -> value -> value
								
	val value'scon		: scon -> value
	val value'ref		: value
	val value'con0		: vid -> value
	val value'exval		: exval -> value
	val value'closure	: closure -> value
	val value'record	: record -> value
	val value'vector	: value vector -> value
	val gen'exname		: vid -> exname
	val closure'fn		: (value -> value) -> closure
	val record'empty	: record
	val record'valrow	: (lab * value) list -> record
	val record'values	: value list -> record
	val look'record		: string -> record -> lab -> value
	val exval'con0		: exname -> exval
	val exval'Match1	: value -> exval
	val exval'Bind		: exval
	val value'string	: string -> value
	val value'true		: value
	val value'false		: value
	val value'list		: value list -> value
	
	val exval'value		: string -> value -> exval
	val scon'value		: string -> value -> scon
	val excon0'value	: string -> value -> exname
	val ref_or_con0'value : string -> value -> ({Ref:unit},{Con0:vid}) either
											   
	val con'value		: string -> value -> (vid * value option)
	val excon'value		: string -> value -> (exname * value option)
	val derefAddr'value : string -> value -> value
	val bool'value		: string -> value -> bool
	val record'value	: string -> value -> record
	val unit'value		: string -> value -> unit
	val listCons'value	: string -> value -> (value * value) option
	val eq'scon			: scon -> scon -> bool
	val eq'exname		: exname -> exname -> bool
	type layout
	val lay'value		: value -> layout
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
	val env'strenv		: strenv -> env
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
	val isUnbound'vid	: env -> vid -> bool
	val look'longstrid	: env -> strid long -> env
	val look'longtycon	: env -> tycon long -> valenv
	val look'longvid	: string -> env -> vid long -> value * idstatus
	type ('a,'b) assoc
	val assoc'valenv	: valenv -> (vid, value * idstatus) assoc
	val valenv'assoc	: (vid, value * idstatus) assoc -> valenv
	
	val look'strid		: string -> strenv -> strid -> env
	val look'tycon		: string -> tyenv -> tycon -> valenv
	val look'vid		: string -> valenv -> vid -> value * idstatus
	type layout
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor COMP_CORE
			(val prefixNML : string
			 structure Prel : PrelSig_Import
			 structure Assoc : AssocSig_Import
			 structure Pos : PosSig_Import
			 structure Misclay : MisclaySig_Import
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
			 sharing type Misclay.layout = Value.layout
							   ) : CompCoreSig =
struct


  open Prel Assoc Pos Misclay Atom Lang Value (*Basis*)
	   
  fun error s = abort ("error: comp_core: "^s)
  fun notYet s = abort ("notYet: comp_core: "^s)

  fun mapLR E xs = (* map guaranteed to traverse L->R *)
	  let fun loop acc [] = rev acc (* this version is also tail-recursive *)
			| loop acc (x::xs) = loop (E x :: acc) xs
	  in loop [] xs
	  end

  fun NMLecho s = echo (prefixNML^s)
									   
  exception Pack of exval

(*----------------------------------------------------------------------
INDEX: valstat - represent just possible states of (value*idstatus)
----------------------------------------------------------------------*)

  datatype vstat =
		   VSTAT'V of value
		 | VSTAT'E of exname
		 | VSTAT'C of vid
		 | VSTAT'Ref

   fun vstat'value_idstatus (v,ids) =
	   case (ids) of
		   Basis.IDSTATUS'V => VSTAT'V v
		 | Basis.IDSTATUS'E => VSTAT'E (excon0'value "vstat(E)" v)
		 | Basis.IDSTATUS'C => let val either = ref_or_con0'value "vstat(C)" v
							   in case either of
									  This {Ref=()} => VSTAT'Ref
									| That {Con0=vid} => VSTAT'C vid
							   end

   fun value_ids'vstat vstat = 
	   case vstat of
           VSTAT'V v        => (v,Basis.IDSTATUS'V)
         | VSTAT'E exname   => (value'exval (exval'con0 exname), Basis.IDSTATUS'E)
         | VSTAT'C vid      => (value'con0 vid, Basis.IDSTATUS'C)
         | VSTAT'Ref        => (value'ref, Basis.IDSTATUS'C)


   fun value'vstat vstat = #1 (value_ids'vstat vstat)

(*----------------------------------------------------------------------
INDEX: Compile Environment
----------------------------------------------------------------------*)


local open Basis
in

  datatype val_status = VS'CT of vstat | VS'RT of bool

  structure CompEnv =
  struct
	datatype valenv = VALENV of (vid,val_status) assoc
	datatype env = ENV of strenv * tyenv * valenv
	val valenv'empty = VALENV assoc'empty
	val env'empty = ENV (strenv'empty,tyenv'empty,valenv'empty)
	fun valenv'plus (VALENV a1) (VALENV a2) = VALENV (assoc'plus a1 a2)
	val valenv'plusList = foldR valenv'plus valenv'empty
	fun valenv'single kv = VALENV (assoc'single kv)
	fun isUnbound'vid (ENV (_,_,VALENV assoc)) vid =
		case (lookup'assoc assoc vid) of None => true | Some _ => false
	fun env'strenv se = ENV(se,tyenv'empty,valenv'empty)
	fun env'tyenv te = ENV(strenv'empty,te,valenv'empty)
	fun env'valenv ve = ENV(strenv'empty,tyenv'empty,ve)
	fun valenv'plus (VALENV a1) (VALENV a2) = VALENV (assoc'plus a1 a2)
	fun env'plus (ENV (se1,te1,ve1)) (ENV (se2,te2,ve2)) =
		let val se = strenv'plus se1 se2
			val te = tyenv'plus te1 te2
			val ve = valenv'plus ve1 ve2
		in ENV (se,te,ve)
		end
	val env'plusList = foldR env'plus env'empty
	fun strenv'env (ENV (se,_,_)) = se
	fun tyenv'env (ENV (_,te,_)) = te
	fun valenv'env (ENV (_,_,ve)) = ve
	fun look'vid who (ve as VALENV a) k =
		look'assoc ("CompEnv.look'vid("^who^")") sof'vid a k
	fun comp'valenv x =
		VALENV (map'assoc (fn (k,(v,ids)) => (k,VS'CT (vstat'value_idstatus (v,ids)))) (Basis.assoc'valenv x))
	fun comp'env env =
		ENV (Basis.strenv'env env,
			 Basis.tyenv'env env,
			 comp'valenv (Basis.valenv'env env))
	fun look'long dotpath look component env (LONG (strids,x)) =
		case strids of
			[] => look dotpath (component env) x
		  |  strid::strids =>
			 let val env = comp'env (look'strid dotpath (strenv'env env) strid)
				 val dotpath = dotpath ^ sof'strid strid ^ "."
			 in look'long dotpath look component env (LONG (strids,x))
			 end
	val look'longstrid = look'long "" look'strid strenv'env
	val look'longtycon = look'long "" look'tycon tyenv'env
	fun look'longvid who = look'long (who^":") look'vid valenv'env
  end

end

  open CompEnv

(*----------------------------------------------------------------------
INDEX: merge CT/RT envs...
----------------------------------------------------------------------*)

  fun valenv'CT_RT (VALENV a) RT_valenv =
	  Basis.valenv'assoc
	  (map'assoc (fn (vid,VS'CT vstat) => (vid, value_ids'vstat vstat)
				   | (vid,VS'RT b) => let val (v,ids) = Basis.look'vid "valenv'CT_RT" RT_valenv vid
										  val () = (case (vstat'value_idstatus (v,ids),b) of
														(VSTAT'V _,false) => ()
													  | (VSTAT'E _,true) => ()
													  | _ => error "valenv'CT_RT - status mismatch")
									  in (vid,(v,ids))
									  end) a)


  fun env'CT_RT (ENV(se,te,ve)) RT_valenv =
	  let val ve = valenv'CT_RT ve RT_valenv
	  in Basis.env'se_te_ve (se,te,ve)
	  end

(*----------------------------------------------------------------------
INDEX: patStatus
----------------------------------------------------------------------*)

  datatype eref = EREF'CT of exname | EREF'RT of vid

  datatype patVarStatus =
		   PVS'BindableV of vid
		 | PVS'C of vid
		 | PVS'E of eref

  fun patStatus'longvid env x =
	  case (case (x) of
				LONG([],vid) => if (isUnbound'vid env vid) 
								then This vid (*short: unbound*)
								else That (Some vid, look'longvid "patvar:look1" env x) (*short: bound*)
			  | _ => That (None, look'longvid "patvar:look2" env x)) of (*long: must be bound*)
		  This vid => PVS'BindableV vid
		| That (shortOpt,VS'RT false) =>
		  (case shortOpt of Some vid => PVS'BindableV vid | None => error "patvar:longV/RT - impossible")
		| That (shortOpt,VS'RT true) =>
		  (case shortOpt of Some vid => PVS'E (EREF'RT vid) | None => error "patvar:longE/RT - impossible")
		| That (shortOpt,VS'CT vstat) =>
		  (case (vstat) of
               VSTAT'V _        => (case shortOpt of Some vid => PVS'BindableV vid | None => error "patvar:longV")
             | VSTAT'E exname   => PVS'E (EREF'CT exname)
             | VSTAT'C vid      => PVS'C vid
             | VSTAT'Ref        => error "patvar:ref")

  datatype patConStatus =
		   PCS'Ref
		 | PCS'C of vid
		 | PCS'E of eref


  val shortOpt = fn LONG([],vid) => Some vid | _ => None

  fun patconsStatus'longvid env x =
	  case (look'longvid "patcons:look" env x) of
		  VS'RT false => error "patcons:V/RT"
		| VS'RT true => 
		  (case (shortOpt x) of Some vid => PCS'E (EREF'RT vid)
							  | None => error "patvar:longE/RT - impossible")
		| VS'CT vstat =>
		  (case (vstat) of
               VSTAT'V _        => error "patcons:V"
             | VSTAT'E exname   => PCS'E (EREF'CT exname)
             | VSTAT'C vid      => PCS'C vid
             | VSTAT'Ref        => PCS'Ref)

(*----------------------------------------------------------------------
INDEX: Code - runtime versions of: pat,exp,dec etc
----------------------------------------------------------------------*)

  structure Code = 
  struct
	datatype patrow = PATROW of (lab * pat) list * {wildcard:bool}
	and pat =
		PAT'Wild
	  | PAT'Scon of scon
	  | PAT'Var of (*vid long*) patVarStatus
	  | PAT'Record of patrow
	  | PAT'Unit
	  | PAT'Tuple of pat list
	  | PAT'List of pat list
	  | PAT'Cons of (*vid long*) patConStatus * pat
	  | PAT'Typed of pat * ty
	  | PAT'Layered of vid * ty option * pat
	datatype exbind1 =
		EXBIND1'New of vid * ty option
	  | EXBIND1'Copy (* do nothing at runtime *)
	datatype exbind = EXBIND of exbind1 list
	datatype valbind =
		VALBIND'Seq of pat * exp * valbind option
	  | VALBIND'Rec of valbind
	and fvalbind = FVALBIND of (pos * vid * exp) list (* clauses expanded to expression *)
	and dec =
		DEC'Val of valbind
	  | DEC'Fval of fvalbind
	  | DEC'Exception of exbind
	  | DEC'Local of dec * dec
	  | DEC'Empty
	  | DEC'Seq of dec * dec
(*
	  | DEC'Type of typbind 
	  | DEC'Datatype of datbind * typbind option
	  | DEC'DataReplicate of tycon * tycon long
	  | DEC'Abstype of datbind * typbind option * dec
	  | DEC'Open of strid long list
*)
	and mrule = MRULE of pat * exp
	and match = MATCH of pos * mrule list
	and exprow = EXPROW of (lab * exp) list
	and exp =
		EXP'Scon of scon
	  | EXP'Var of vid * pos (* compiled - so not long! *)
	  | EXP'Record of exprow
	  | EXP'Select of lab
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
	  (*| EXP'While of exp * exp*) (* compiled to derived form *)
	  | EXP'Case of exp * match
	  | EXP'Fn of match
      | EXP'Value of value (* created by compile-time env-lookup *)
  end


(*----------------------------------------------------------------------
INDEX: source language generation - support fvalbind clauses
----------------------------------------------------------------------*)

  fun exp'fnVid pos x body = EXP'Fn (MATCH (pos,[MRULE (PAT'Var (LONG ([],x)),body)]))

  fun exp'fnVids pos [] body = body
	| exp'fnVids pos (x::xs) body = exp'fnVid pos x (exp'fnVids pos xs body)
						  
  local val U = ref 1
  in fun genVid () = let val n = !U
					 in U := 1+n; VID (ID ("G#"^stringOfInt n))
					 end
  end

  fun genVids n =
	  if (n<0) then abort "genVids:n<0"
	  else let fun loop acc n = if n=0 then acc
								else loop (genVid ()::acc) (n-1)
		   in loop [] n
		   end
  fun mrule'clause (pats,tyo_IGNORED,exp) = MRULE (PAT'Tuple pats,exp)
									
  fun exp'clauses pos [] = abort "exp'clauses:[]"
	| exp'clauses pos (clauses as (pats,_,_)::_) =
	  let val vids = genVids (length pats)
	  in exp'fnVids pos vids
		 (EXP'Case (EXP'Tuple (map (exp'vid pos) vids),
					MATCH (pos,map mrule'clause clauses)))
	  end

(*----------------------------------------------------------------------
INDEX: source language generation - support while exp
----------------------------------------------------------------------*)

local open Code
in
  fun expDerived'while (e1,e2) =
	  let val dummyPos = makePos{path="expDerived'while",row=0,col=0}
		  val X = genVid()
	  in (*let val rec X = fn () => if e1 then (e2; X()) else () in X() end*)
		  EXP'Let 
		  (DEC'Val 
		   (VALBIND'Rec 
			(VALBIND'Seq (PAT'Var (PVS'BindableV X),
						  EXP'Fn (MATCH (dummyPos,[MRULE (PAT'Unit,
														  EXP'If (e1,
																  EXP'Seq [e2, EXP'App (EXP'Var (X,dummyPos),EXP'Unit)], 
																  EXP'Unit)
														  )])),
						  None))),
		   EXP'App (EXP'Var (X,dummyPos), EXP'Unit))
	  end
end

(*----------------------------------------------------------------------
INDEX: compc'* - compile-time
----------------------------------------------------------------------*)

local

(*----------------------------------------------------------------------
INDEX: shad
----------------------------------------------------------------------*)

  datatype shad = SHAD of vid list
  val shad'empty = SHAD []
  fun inShad (SHAD xs) x = exists (eq'vid x)  xs
  fun consShad (SHAD xs) x = SHAD (x::xs)

(*----------------------------------------------------------------------
INDEX: shad'pat
----------------------------------------------------------------------*)

  fun shad'patrow P shad (Code.PATROW (lps,{wildcard=IGNORED})) =
	  fold (fn shad => fn (_,pat) => P shad pat) shad lps

local open Code
in
  fun U_shad'pat P shad pat =
	  case pat of
		  PAT'Wild => shad
		| PAT'Scon _ => shad
		| PAT'Var (PVS'BindableV x) => consShad shad x
		| PAT'Var _ => shad
		| PAT'Record pr => shad'patrow P shad pr
		| PAT'Unit => shad
		| PAT'Tuple pats => fold P shad pats
		| PAT'List pats => fold P shad pats
		| PAT'Cons (x,pat) => P shad pat
		| PAT'Typed (pat,ty_IGNORED) => P shad pat
		| PAT'Layered (vid,tyo_IGNORED,pat) => P (consShad shad vid) pat
end

  fun shad'pat shad pat = U_shad'pat shad'pat shad pat


in

(*----------------------------------------------------------------------
INDEX: comp'pat
----------------------------------------------------------------------*)

  fun comp'patrow P (PATROW (lps,{wildcard})) =
	  Code.PATROW (map (fn (lab,pat) => (lab,P pat)) lps,{wildcard=wildcard})

  fun U_comp'pat env P pat =
	  case pat of
          PAT'Wild                  => Code.PAT'Wild
        | PAT'Scon scon             => Code.PAT'Scon scon
        | PAT'Var x                 => Code.PAT'Var (patStatus'longvid env x)
        | PAT'Record pr             => Code.PAT'Record (comp'patrow P pr)
        | PAT'Unit                  => Code.PAT'Unit
        | PAT'Tuple pats            => Code.PAT'Tuple (map P pats)
        | PAT'List pats             => Code.PAT'List (map P pats)
        | PAT'Cons (x,pat)          => Code.PAT'Cons (patconsStatus'longvid env x,P pat)
        | PAT'Typed (pat,ty)        => Code.PAT'Typed (P pat,ty)
        | PAT'Layered (vid,tyo,pat) => Code.PAT'Layered (vid,tyo,P pat)

  local
	  fun P env pat = U_comp'pat env (P env) pat
  in
  val comp'pat = P
  end

(*----------------------------------------------------------------------
INDEX: NEW comp'pat
----------------------------------------------------------------------*)


  fun valenv'pat pat' = 
	let
		val shad = shad'pat shad'empty pat'
		val SHAD vids = shad
		val valenv = valenv'plusList (map (fn x => valenv'single  (x,VS'RT false)) vids)
	in valenv
	end

  fun TWO_comp'pat env pat =
	  let val pat' = comp'pat env pat
	  in (pat',valenv'pat pat')
	  end

end (* local *)

(*----------------------------------------------------------------------
INDEX: comp'exp
----------------------------------------------------------------------*)

  fun comp'longvid env (longvid,pos) =
	  case longvid of
		  LONG([],vid) => (case (look'longvid "comp'longvid-1" env longvid) of
							   VS'CT vstat => Code.EXP'Value (value'vstat vstat)
							 | VS'RT _ => Code.EXP'Var (vid,pos))
		| _ => (case (look'longvid "comp'longvid-2" env longvid) of
					VS'CT vstat => Code.EXP'Value (value'vstat vstat)
				  | VS'RT _ => error "comp'longvid:long/RT")

  fun comp'mrule E env (MRULE (pat,exp)) = let val pat' = comp'pat env pat
											   val ve = valenv'pat pat'
											   val env = env'plus env (env'valenv ve)
										   in Code.MRULE (pat',E env exp)
										   end

  fun comp'match E env (MATCH (pos,xs)) = Code.MATCH (pos,map (comp'mrule E env) xs)

  fun comp'exprow Eshad exprow =
	  case exprow of
		  EXPROW xs => Code.EXPROW (map (fn (lab,exp) => (lab,Eshad exp)) xs)

  fun U_comp'exp E D env exp =
	  case exp of
          EXP'Scon scon             => Code.EXP'Scon scon 
        | EXP'Var x      			=> comp'longvid env x
        | EXP'Record exprow         => Code.EXP'Record (comp'exprow (E env) exprow)
        | EXP'Select lab            => Code.EXP'Select lab
        | EXP'Unit                  => Code.EXP'Unit
        | EXP'Tuple exps            => Code.EXP'Tuple (map (E env) exps)
        | EXP'List exps             => Code.EXP'List (map (E env) exps)
        | EXP'Vector exps           => Code.EXP'Vector (map (E env) exps)
        | EXP'Seq exps              => Code.EXP'Seq (map (E env) exps)
        | EXP'Let (dec,exp)         => let val (dec',env1) = D env dec
										   val exp' = E (env'plus env env1) exp
									   in Code.EXP'Let (dec',exp')
									   end
        | EXP'App (e1,e2)           => Code.EXP'App (E env e1,E env e2)
        | EXP'Typed (exp,ty)        => Code.EXP'Typed (E env exp,ty)
        | EXP'Andalso (e1,e2)       => Code.EXP'Andalso (E env e1,E env e2)
        | EXP'Orelse (e1,e2)        => Code.EXP'Orelse (E env e1,E env e2)
        | EXP'Handle (exp,match)    => Code.EXP'Handle (E env exp,comp'match E env match)
        | EXP'Raise exp             => Code.EXP'Raise (E env exp)
        | EXP'If (e1,e2,e3)         => Code.EXP'If (E env e1,E env e2,E env e3)
        | EXP'While (e1,e2)         => expDerived'while (E env e1,E env e2)
        | EXP'Case (exp,match)      => Code.EXP'Case (E env exp,comp'match E env match)
        | EXP'Fn match              => Code.EXP'Fn (comp'match E env match)


(*----------------------------------------------------------------------
INDEX: NEW comp'dec
----------------------------------------------------------------------*)

  fun valenv'valbind vb =
	  case vb of		
		  VALBIND'Rec vb1 => valenv'valbind vb1
		| VALBIND'Seq (pat,exp,None) => valenv'pat (comp'pat env'empty pat)
		| VALBIND'Seq (pat,exp,Some vb) => valenv'plus (valenv'pat (comp'pat env'empty pat)) (valenv'valbind vb)

  fun REC_valenv'valbind vb =
	  case vb of		
		  VALBIND'Rec vb1 => valenv'valbind vb1
		| VALBIND'Seq (pat,exp,None) => valenv'empty
		| VALBIND'Seq (pat,exp,Some vb) => REC_valenv'valbind vb

  fun TWO_comp'valbind env E vb =
	  case vb of		
		  VALBIND'Rec vb1 => let val (vb1',valenv) = TWO_comp'valbind env E vb1
							 in (Code.VALBIND'Rec vb1', valenv)
							 end
		| VALBIND'Seq (pat,exp,vbo) =>
			(case vbo of 
				 None  => let val (pat',valenv) = TWO_comp'pat env pat
							  val vb' = Code.VALBIND'Seq (pat',E exp,None)
						  in (vb',valenv)
						  end
			   | Some vb1 =>
				 let val (pat',valenv1) = TWO_comp'pat env pat
					 val (vb1',valenv2) = TWO_comp'valbind env E vb1
					 val vb' = Code.VALBIND'Seq (pat',E exp,Some vb1')
					 val valenv= valenv'plus valenv1 valenv2
				 in (vb',valenv)
				 end)


  fun REC_valenv'fvalbind (FVALBIND fs) =
	  valenv'plusList (map (fn (_,vid,_) => valenv'single (vid,VS'RT false)) fs)

  fun comp'fvalbind E (FVALBIND fs) =
	  let val pairs = map (fn (pos,vid,clauses) =>
							  let val exp = exp'clauses pos clauses
								  val valenv = valenv'single (vid,VS'RT false)
								  val trip' = (pos,vid,E exp)
							  in (trip',valenv)
							  end) fs
	  in (Code.FVALBIND (map #1 pairs), valenv'plusList (map #2 pairs))
	  end

  fun comp'exbind1 env b =
	  case b of
		  EXBIND1'New (vid,tyo) => let val valenv = valenv'single (vid,VS'RT true)
								   in (Code.EXBIND1'New (vid,tyo) ,valenv)
								   end
		| EXBIND1'Copy (vid,longvid) =>
		  (case (look'longvid "comp'exbind1" env longvid) of
			   VS'RT false => error "exbind(copy)/VS'RT:expected status E, got V"
			 | VS'RT true => let val valenv = valenv'single (vid,VS'RT true)
							 in (Code.EXBIND1'Copy,valenv)
							 end
			 | VS'CT vstat =>
			   (case (vstat) of
					VSTAT'V _ => error "exbind(copy):expected status E, got V"
				  | VSTAT'C _ => error "exbind(copy):expected status E, got C"
				  | VSTAT'Ref => error "exbind(copy):expected status E, got Ref"
				  | VSTAT'E exname => let val valenv = valenv'single (vid,VS'CT vstat)
									  in (Code.EXBIND1'Copy,valenv)
									  end))

  fun comp'exbind env (EXBIND bs) =
	  let val pairs = map (comp'exbind1 env) bs
	  in (Code.EXBIND (map #1 pairs), valenv'plusList (map #2 pairs))
	  end



local open Basis
in
  fun comp'typbind (TYPBIND xs) =
	  tyenv'plusList (map (fn (_,tycon,_) =>
									tyenv'single (tycon,valenv'empty)
									) xs)
  fun comp'datbind (DATBIND xs) =
	  let fun loop ve te [] = (ve,te)
			| loop ve te ((IGNORED_tyvars,tycon,clauses)::xs) =
			  let val ve' = 
					  valenv'plusList
						(map (fn (vid,IGNORED_tyo) =>
								 let val v = value'con0 vid
								 in valenv'single (vid,(v,IDSTATUS'C))
								 end) clauses)
				  val te' = tyenv'single (tycon,ve')
			  in loop (valenv'plus ve ve') (tyenv'plus te te') xs
			  end
	  in loop valenv'empty tyenv'empty xs
	  end
  fun comp'optWithType tbo =
	  case tbo of Some tb => comp'typbind tb | None => tyenv'empty
end


  fun U_comp'dec E D env dec = 
	  case dec of
		  DEC'Val vb => let val veRec = REC_valenv'valbind vb
							val env = env'plus env (env'valenv veRec)
							val (vb',ve) = TWO_comp'valbind env (E env) vb
						in (Code.DEC'Val vb',env'valenv ve)
						end
		| DEC'Fval fb => let val veRec = REC_valenv'fvalbind fb
							 val env = env'plus env (env'valenv veRec)
							 val (fb',ve) = comp'fvalbind (E env) fb
						 in (Code.DEC'Fval fb',env'valenv ve)
						 end
		| DEC'Type typbind => (Code.DEC'Empty, env'tyenv (comp'typbind typbind))
		| DEC'Datatype (db,tbo) =>
		  let val (ve,te) = comp'datbind db
			  val env1 = env'plus (env'valenv (comp'valenv ve)) (env'tyenv te)
			  val env2 = env'tyenv (comp'optWithType tbo)
		  in (Code.DEC'Empty, env'plus env1 env2)
		  end
		| DEC'DataReplicate (tycon,tyconlong) =>
		  let val ve = look'longtycon env tyconlong
			  val env1 = env'valenv (comp'valenv ve)
			  val env2 = env'tyenv (Basis.tyenv'single (tycon,ve))
		  in (Code.DEC'Empty, env'plus env1 env2)
		  end
		| DEC'Abstype (db,tbo,dec) =>
		  let val (ve,te_IGNORED) = comp'datbind db
			  val env1 = env'valenv (comp'valenv ve)
			  val env2 = env'tyenv (comp'optWithType tbo)
			  val (dec',env3) = D (env'plusList [env,env1,env2]) dec
		  in (dec', env'plus env2 env3)
		  end
		| DEC'Exception exbind => 
		  let val (exbind',ve) = comp'exbind env exbind
		  in (Code.DEC'Exception exbind', env'valenv ve)
		  end
		| DEC'Local (dec1,dec2) =>
		  let val (dec1',env1) = D env dec1
			  val (dec2',env2) = D (env'plus env env1) dec2
		  in (Code.DEC'Local (dec1',dec2'), env2)
		  end
		| DEC'Open xs => (Code.DEC'Empty, env'plusList (map (comp'env o look'longstrid env) xs))
		| DEC'Empty => (Code.DEC'Empty,env'empty)
		| DEC'Seq (dec1,dec2) =>
		  let val (dec1',env1) = D env dec1
			  val (dec2',env2) = D (env'plus env env1) dec2
		  in (Code.DEC'Seq (dec1',dec2'),env'plus env1 env2)
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

(*----------------------------------------------------------------------
INDEX: exec'* - runtime
----------------------------------------------------------------------*)

  open Basis
  open Code


  fun mapOption f = fn None => None | Some x => Some (f x)

(*----------------------------------------------------------------------
INDEX: Rec.valenv
----------------------------------------------------------------------*)

  structure Rec =
  struct

    datatype valenv = VALENV of (vid,recval * idstatus) assoc
	and recval = RECVAL of valenv -> value

	val valenv'empty = VALENV assoc'empty
	fun valenv'single kv = VALENV (assoc'single kv)
	fun valenv'plus (VALENV a1) (VALENV a2) = VALENV (assoc'plus a1 a2)
	val valenv'plusList = foldR valenv'plus valenv'empty

	fun value'recval (RECVAL F) = F valenv'empty
	fun recval'value v = RECVAL (fn _ => v)
	val recval'fn = RECVAL

	fun valenv'valenv ve =
		VALENV (map'assoc (fn (k,(rv,ids)) => (k,(recval'value rv,ids))) (assoc'valenv ve))

  end

  fun valenv'Rec (ve as Rec.VALENV a) =
	  valenv'assoc (map'assoc (fn (vid,(Rec.RECVAL F,ids)) => (vid,(F ve,ids))) a)

(*----------------------------------------------------------------------
INDEX: runtime counts
----------------------------------------------------------------------*)

  val Napps = ref 0;
  val apply'value = fn v1 => fn v2 =>
	  (Napps := 1 + !Napps; apply'value v1 v2)

  val Nlooks = ref 0;
  val look'vid = fn who => fn valenv => fn x =>
	  (Nlooks := 1 + !Nlooks; 
	   let val () = NMLecho ("look'vid: "^sof'vid x^" - "^who)
	   in Basis.look'vid who valenv x
	   end)

(*----------------------------------------------------------------------
INDEX: pat
----------------------------------------------------------------------*)

  fun exname'eref valenv eref =
	  case eref of
		  EREF'CT exname => exname
		| EREF'RT vid => let val p = look'vid ("exname'eref") valenv vid
						 in case (vstat'value_idstatus p) of
								VSTAT'E exname => exname
							  | _ => error "exname'eref: expected statsus E"
						 end

  fun exec'patvar valenv x v =
	  case (x) of
		  PVS'BindableV vid => Some (valenv'single (vid,(v,IDSTATUS'V)))
		| PVS'C vid =>
		  let val (vid',vOpt) = con'value "patvar(C)" v
		  in case vOpt of
				 Some _ => None
			   | None => if (eq'vid vid vid')
						 then (Some valenv'empty) else None
		  end
		| PVS'E eref =>
		  let val exname = exname'eref valenv eref
			  val (exname',vOpt) = excon'value "parvar(E)" v
		  in case vOpt of
				 Some _ => None
			   | None => if (eq'exname exname exname')
						 then (Some valenv'empty) else None
		  end

  fun exec'patcons valenv P v x pat =
	  case (x) of
		  PCS'Ref => let val v' = derefAddr'value "patcons(ref)" v
						 in P v' pat
						 end
		| PCS'C vid => let val (vid',vOpt) = con'value "patcons(C)" v
						   in case vOpt of
								  Some v' => if (eq'vid vid vid')
											 then (P v' pat) else None
								| None => None
						   end
		| PCS'E eref => let val exname = exname'eref valenv eref
							val (exname',vOpt) = excon'value "parvar(E)" v
						in case vOpt of
							   Some v' => if (eq'exname exname exname')
										  then (P v' pat) else None
							 | None => None
						end

  fun exec'patrow P record (Code.PATROW (lps,{wildcard=IGNORED})) =
	  let
		fun loop accVe [] = Some accVe
		  | loop accVe ((lab,pat)::lps) =
			let val v = look'record "patrow" record lab
			in case (P v pat) of
				 None => None
			   | Some ve => loop (valenv'plus accVe ve) lps
			end
	  in
		loop valenv'empty lps
	  end
		  
  fun exec'pattuple P record pats =
	  let fun loop accVe n [] = Some accVe
			| loop accVe n (pat::pats) =
			  let val v = look'record "pattuple" record (LAB'Num n)
			  in case (P v pat) of
					 None => None
				   | Some ve => loop (valenv'plus accVe ve) (n+1) pats
			  end
	  in
		loop valenv'empty 1 pats
	  end

  fun exec'patlist P v pats =
	  case (pats,listCons'value "patlist" v) of
		  ([],None) => Some valenv'empty
		| (pat::pats,Some(v1,v2)) =>
		  (case (P v1 pat) of
			   None => None
			 | Some ve1 => (case (exec'patlist P v2 pats) of
								None => None
							  | Some ve2 => Some (valenv'plus ve1 ve2)))
		| _ => None

  fun exec'layered P v (vid,pat) =
	  case (P v pat) of
		  None => None
		| Some ve => Some (valenv'plus (valenv'single (vid,(v,IDSTATUS'V))) ve)

			   
  fun U_exec'pat P valenv v pat =
	  case pat of
		  PAT'Wild => Some valenv'empty
		| PAT'Scon scon => let val scon' = scon'value "exec'pat" v
						   in if (eq'scon scon scon') then Some valenv'empty
							  else None
						   end
		| PAT'Var x => exec'patvar valenv x v
		| PAT'Record pr => let val record = record'value "exec'pat" v
						   in exec'patrow P record pr
						   end
		| PAT'Unit => let val U as () = unit'value "exec'pat" v
					  in Some valenv'empty
					  end
		| PAT'Tuple pats => let val record = record'value "exec'pat" v
							in exec'pattuple P record pats
							end
		| PAT'List pats => exec'patlist P v pats
		| PAT'Cons (x,pat) => exec'patcons valenv P v x pat
		| PAT'Typed (pat,ty_IGNORED) => P v pat
		| PAT'Layered (vid,tyo_IGNORED,pat) => exec'layered P v (vid,pat)


  local fun P valenv v pat = U_exec'pat (P valenv) valenv v pat
  in val exec'pat = P
  end

	
  fun exec'patvarRecursive valenv x recval =
	  case (x) of
		  PVS'BindableV vid => Some (Rec.valenv'single (vid,(recval,IDSTATUS'V)))
		| _ => mapOption Rec.valenv'valenv (exec'patvar valenv x (Rec.value'recval recval))


  fun exec'layeredRecursive P recval (vid,pat) =
	  case (P recval pat) of
		  None => None
		| Some ve => Some (Rec.valenv'plus (Rec.valenv'single (vid,(recval,IDSTATUS'V))) ve)


  fun exec'patRecursive valenv recval pat =
	  case pat of
		  PAT'Var x => exec'patvarRecursive valenv x recval
		| PAT'Layered (vid,tyo_IGNORED,pat) => exec'layeredRecursive (exec'patRecursive valenv) recval (vid,pat)
		| _ => mapOption Rec.valenv'valenv (exec'pat valenv (Rec.value'recval recval) pat)


(*----------------------------------------------------------------------
INDEX: exval'match
----------------------------------------------------------------------*)

  fun sofValue v = show_layout (lay'value v)

  fun pos'match (Code.MATCH (pos,_)) = pos 

  fun exval'match match value = 
	  exval'Match1 (value'string (sofValue value ^ " @ " ^ sofPos (pos'match match)))

(*----------------------------------------------------------------------
INDEX: mrule,match
----------------------------------------------------------------------*)

  fun exec'mrule E valenv value (MRULE (pat,exp)) =
	  case (exec'pat valenv value pat) of
		  None => None
		| Some ve =>
			  let val valenv = valenv'plus valenv ve
			  in Some (E valenv exp)
			  end

  fun exec'match E valenv value (MATCH (pos,xs)) =
	  let fun loop [] = None
			| loop (x::xs) =
			  case (exec'mrule E valenv value x) of
				  Some res => Some res
				| None => loop xs
	  in loop xs
	  end

(*----------------------------------------------------------------------
INDEX: makeEvalClosure
----------------------------------------------------------------------*)

  fun makeEvalClosure E (match,valenv) =
	  closure'fn (fn vArg => case (exec'match E valenv vArg match) of
								 Some res => res
							   | None => raise (Pack (exval'match match vArg)))

  fun makeEvalClosureRecursive E (match,valenv,ve) =
	  closure'fn
		  (fn vArg =>
			  let val valenv = valenv'plus valenv (valenv'Rec ve)
			  in case (exec'match E valenv vArg match) of
					 Some res => res
				   | None => raise (Pack (exval'match match vArg))
			  end)

(*----------------------------------------------------------------------
INDEX: varLookup
----------------------------------------------------------------------*)

  fun exec'varLookup valenv (x,pos) =
	  let val (v,is) = look'vid ("exec'varLookup-"^sofPos pos) valenv x
	  in v
	  end

(*----------------------------------------------------------------------
INDEX: exp
----------------------------------------------------------------------*)

  fun exec'exprow Evalenv exprow =
	  case exprow of
		  EXPROW xs => mapLR (fn (lab,exp) => (lab,Evalenv exp)) xs

  fun exec'seq Evalenv [] = abort "exec'seq"
	| exec'seq Evalenv (exp::exps) =
	  let fun loop v [] = v
			| loop _ (exp::exps) = loop (Evalenv exp) exps
	  in loop (Evalenv exp) exps
	  end


  fun value'lab lab =
	  value'closure (closure'fn (fn vArg => let val record = record'value "value'lab" vArg
											in look'record "value'lab" record lab
											end))
	  

  fun U_exec'exp E D valenv exp =
	  case exp of
		  EXP'Scon scon => value'scon scon
		| EXP'Var (x,pos) => exec'varLookup valenv (x,pos)
		| EXP'Record exprow => value'record (record'valrow (exec'exprow (E valenv) exprow))
		| EXP'Select lab => value'lab lab
		| EXP'Unit => value'record record'empty
		| EXP'Tuple exps => value'record (record'values (mapLR (E valenv) exps))
		| EXP'List exps => value'list (mapLR (E valenv) exps)
		| EXP'Vector exps => value'vector (Vector.fromList (mapLR (E valenv) exps))
		| EXP'Seq exps => exec'seq (E valenv) exps
		| EXP'Let (dec,exp) =>
		  let val valenv' = D valenv dec
		  in E (valenv'plus valenv valenv') exp
		  end
		| EXP'App (e1,e2) => apply'value (E valenv e1) (E valenv e2)
		| EXP'Typed (exp,ty) => E valenv exp
		| EXP'Andalso (e1,e2) => if (bool'value "andalso" (E valenv e1))
								 then E valenv e2
								 else value'false
		| EXP'Orelse (e1,e2) => if (bool'value "orele" (E valenv e1))
								then value'true
								else E valenv e2
		| EXP'Handle (exp,match) =>
		  ((E valenv exp)
		   handle Pack exval =>
				  (case (exec'match E valenv (value'exval exval) match) of
					   None => raise (Pack exval)
					 | Some res => res))
		| EXP'Raise exp =>
		  let val exval = exval'value "exec'exp(Raise)" (E valenv exp)
		  in (*NMLecho "***raising";*) raise (Pack exval)
		  end
		| EXP'If (e1,e2,e3) => if (bool'value "if" (E valenv e1))
							   then E valenv e2
							   else E valenv e3
		| EXP'Case (exp,match) => let val vCase = E valenv exp
								  in case (exec'match E valenv vCase match) of
									   Some res => res
									 | None => raise (Pack (exval'match match vCase))
								  end
		| EXP'Fn match => value'closure (makeEvalClosure E (match,valenv))
		| EXP'Value v => v


  fun U_exec'expRecursive E D valenv exp =
	  case exp of
		  Code.EXP'Fn match => Rec.recval'fn (fn ve => value'closure (makeEvalClosureRecursive E (match,valenv,ve)))
		| _ => Rec.recval'value (U_exec'exp E D valenv exp)


(*----------------------------------------------------------------------
INDEX: dec
----------------------------------------------------------------------*)

  fun exec'valbind_inRec Er valenv vb =
	  case vb of		
		  VALBIND'Rec vb => exec'valbind_inRec Er valenv vb (* 2nd rec ignored *)
		| VALBIND'Seq (pat,exp,vbo) =>
		  case (exec'patRecursive valenv (Er valenv exp) pat) of
			  None => raise (Pack exval'Bind)
			| Some ve =>
			  (case vbo of
				   None => ve
				 | Some vb => Rec.valenv'plus ve (exec'valbind_inRec Er valenv vb))

  fun exec'valbind E Er valenv vb =
	  case vb of		
		  VALBIND'Rec vb => valenv'Rec (exec'valbind_inRec Er valenv vb)
		| VALBIND'Seq (pat,exp,vbo) =>
		  case (exec'pat valenv (E valenv exp) pat) of
			  None => raise (Pack exval'Bind)
			| Some ve =>
			  (case vbo of
				   None => ve
				 | Some vb => valenv'plus ve (exec'valbind E Er valenv vb))

  fun exec'fvalbind Evalenv (FVALBIND fs) =
	  valenv'Rec (Rec.valenv'plusList
				  (map (fn (pos,vid,exp) =>
						   let val recval = Evalenv exp
						   in Rec.valenv'single (vid,(recval,IDSTATUS'V))
						   end) fs))

  fun exec'exbind1 b =
	  case b of
		  EXBIND1'New (vid,IGNORED_tyo) =>
		  let val en = gen'exname vid
			  val v = value'exval (exval'con0 en)
		  in valenv'single (vid,(v,IDSTATUS'E))
		  end
		| EXBIND1'Copy => Basis.valenv'empty

  fun exec'exbind (EXBIND bs) =
	  valenv'plusList (mapLR exec'exbind1 bs)


  fun U_exec'dec E Er D valenv dec = 
	  case dec of
		  DEC'Val vb => exec'valbind E Er valenv vb
		| DEC'Fval fb => exec'fvalbind (Er valenv) fb
		| DEC'Exception exbind => exec'exbind exbind
		| DEC'Local (dec1,dec2) => let val valenv1 = D valenv dec1
								   in D (valenv'plus valenv valenv1) dec2
								   end
		| DEC'Empty => valenv'empty
		| DEC'Seq (dec1,dec2) =>
		  let val valenv1 = D valenv dec1
			  val valenv2 = D (valenv'plus valenv valenv1) dec2
		  in valenv'plus valenv1 valenv2
		  end

(*----------------------------------------------------------------------
INDEX: fix -- pat/exp/dec
----------------------------------------------------------------------*)

  local
	  fun E  x = U_exec'exp			 E D x
	  and Er x = U_exec'expRecursive E D x
	  and D  x = U_exec'dec          E Er D x
  in
  fun exec'dec dec = 						  
	  This (D valenv'empty dec)
	  handle (Pack exval) => That exval
  end

(*----------------------------------------------------------------------
INDEX: eval - sequence comp/exec
----------------------------------------------------------------------*)

  fun eval'dec env dec =
	  let val (dec',CT_env) = comp'dec env dec
	  in case (exec'dec dec') of
			 This RT_valenv => This (env'CT_RT CT_env RT_valenv)
		   | That x => That x
	  end

  val eval'dec = 
	  fn env => fn dec => 
				   let 
					   val () = Napps := 0
					   val () = Nlooks := 0
					   val res = eval'dec env dec
					   val () = NMLecho ("***NappsC = " ^ stringOfInt (!Napps))
					   val () = NMLecho ("***NlooksC = " ^ stringOfInt (!Nlooks))
				   in res 
				   end

  type dec = Lang.dec (* because Code was opened *)					   

end
