(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  25.    export
  37.    imports
  317.   functor
  364.   valstat - represent just possible states of (value*idstatus)
  394.   Compile Environment
  454.   merge CT/RT envs...
  472.   patStatus
  509.   source language generation - support fvalbind clauses
  540.   source language generation - support while exp
  564.   compc'* - compile-time
  570.   shad
  579.   shad'pat
  607.   comp'pat
  633.   NEW comp'pat
  653.   comp'exp
  704.   NEW comp'dec
  855.   fix compilation P/E/D
  866.   eval - sequence comp/exec
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature EvalSig = 
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
	val link			: 'a list list -> 'a list
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
end;
(*IMPORTDONE:*)

(*IMPORTSIG: MisclaySig ../prelude/MISCLAY.ML *)
signature MisclaySig_Import =
sig
  type layout
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
    type lab;
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
	type exval
		 
	val value'ref		: value
	val value'con0		: vid -> value
	val value'exval		: exval -> value
	val exval'con0		: exname -> exval
	val excon0'value	: string -> value -> exname
	val ref_or_con0'value : string -> value -> ({Ref:unit},{Con0:vid}) either
											   
	type layout
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

(*IMPORTSIG: MachineSig MACHINE_1.ML *)
signature MachineSig_Import =
sig
	type vid
	type lab
	type scon
	type pos
	type value
	type exname
	datatype eref = EREF'CT of exname | EREF'RT of vid
	datatype patVarStatus =
		   PVS'BindableV of vid
		 | PVS'C of vid
		 | PVS'E of eref
	datatype patConStatus =
		   PCS'Ref
		 | PCS'C of vid
		 | PCS'E of eref
	structure Code : sig
	  datatype patrow = PATROW of (lab * pat) list * {wildcard:bool}
	  and pat =
		  PAT'Wild
		| PAT'Scon of scon
		| PAT'Var of patVarStatus
		| PAT'Record of patrow
		| PAT'Unit
		| PAT'Tuple of pat list
		| PAT'List of pat list
		| PAT'Cons of patConStatus * pat
		| PAT'Layered of vid * pat
	  datatype valbind =
		  VALBIND'Seq of pat * exp * valbind option
		| VALBIND'Rec of valbind
	  and fvalbind = FVALBIND of (pos * vid * exp) list 
	  and dec =
		  DEC'Val of valbind
		| DEC'Fval of fvalbind
		| DEC'Exception of vid list 
		| DEC'Local of dec * dec
		| DEC'Empty
		| DEC'Seq of dec * dec
	  and mrule = MRULE of pat * exp
	  and match = MATCH of pos * mrule list
	  and exprow = EXPROW of (lab * exp) list
	  and exp =
		  EXP'Scon of scon
		| EXP'Var of vid * pos 
		| EXP'Record of exprow
		| EXP'Select of lab
		| EXP'Unit
		| EXP'Tuple of exp list
		| EXP'List of exp list
		| EXP'Vector of exp list
		| EXP'Seq of exp list
		| EXP'Let of dec * exp
		| EXP'App of exp * exp
		| EXP'Andalso of exp * exp
		| EXP'Orelse of exp * exp
		| EXP'Handle of exp * match
		| EXP'Raise of exp
		| EXP'If of exp * exp * exp
		| EXP'Case of exp * match
		| EXP'Fn of match
		| EXP'Value of value 
	end
	type exval
	type ('a,'b) assoc
	val exec'dec : Code.dec -> ((vid,value) assoc,exval) either
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor COMPILER_1
			(val prefixNML : string
			 structure Prel : PrelSig_Import
			 structure Assoc : AssocSig_Import
			 structure Pos : PosSig_Import
			 structure Misclay : MisclaySig_Import
			 structure Atom : AtomSig_Import
			 structure Lang : LangSig_Import
			 structure Value : ValueSig_Import
			 structure Basis : BasisSig_Import
			 structure Machine : MachineSig_Import

			 sharing type Assoc.assoc = Basis.assoc = Machine.assoc
			 sharing type Pos.pos = Lang.pos = Machine.pos
			 sharing type Atom.lab = Lang.lab = Value.lab = Machine.lab
			 sharing type Atom.scon = Lang.scon = Value.scon = Machine.scon
			 sharing type Atom.long = Lang.long = Basis.long 
			 sharing type Atom.vid = Lang.vid = Value.vid = Basis.vid = Machine.vid
			 sharing type Atom.strid = Lang.strid = Basis.strid
			 sharing type Atom.tycon = Lang.tycon = Basis.tycon
			 sharing type Value.value = Basis.value = Machine.value
			 sharing type Value.exname = Machine.exname
			 sharing type Value.exval = Machine.exval
			 sharing type Misclay.layout = Value.layout
							   ) : EvalSig =
struct


  open Prel Assoc Pos Misclay Atom Lang Value Machine (*Basis*)
	   
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

  fun valenv'CT_RT (VALENV a) RT_assoc =
	  Basis.valenv'assoc
	  (map'assoc (fn (vid,VS'CT vstat) => (vid, value_ids'vstat vstat)
				   | (vid,VS'RT b) => let val v = look'assoc "valenv'CT_RT" sof'vid RT_assoc vid
										  val ids = if b then Basis.IDSTATUS'E else Basis.IDSTATUS'V
									  in (vid,(v,ids))
									  end) a)


  fun env'CT_RT (ENV(se,te,ve)) RT_assoc =
	  let val ve = valenv'CT_RT ve RT_assoc
	  in Basis.env'se_te_ve (se,te,ve)
	  end

(*----------------------------------------------------------------------
INDEX: patStatus
----------------------------------------------------------------------*)

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
		| PAT'Layered (vid,pat) => P (consShad shad vid) pat
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
        | PAT'Typed (pat,ty)        => P pat
        | PAT'Layered (vid,tyo,pat)	=> Code.PAT'Layered (vid,P pat)

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
        | EXP'Typed (exp,ty)        => E env exp
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
								   in ([vid] ,valenv)
								   end
		| EXBIND1'Copy (vid,longvid) =>
		  (case (look'longvid "comp'exbind1" env longvid) of
			   VS'RT false => error "exbind(copy)/VS'RT:expected status E, got V"
			 | VS'RT true => let val valenv = valenv'single (vid,VS'RT true)
							 in ([],valenv)
							 end
			 | VS'CT vstat =>
			   (case (vstat) of
					VSTAT'V _ => error "exbind(copy):expected status E, got V"
				  | VSTAT'C _ => error "exbind(copy):expected status E, got C"
				  | VSTAT'Ref => error "exbind(copy):expected status E, got Ref"
				  | VSTAT'E exname => let val valenv = valenv'single (vid,VS'CT vstat)
									  in ([],valenv)
									  end))

  fun comp'exbind env (EXBIND bs) =
	  let val pairs = map (comp'exbind1 env) bs
	  in (link (map #1 pairs), valenv'plusList (map #2 pairs))
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
		  let val (vids,ve) = comp'exbind env exbind
		  in (Code.DEC'Exception vids, env'valenv ve)
		  end
		| DEC'Local (dec1,dec2) =>
		  let val (dec1',env1) = D env dec1
			  val (dec2',env2) = D (env'plus env env1) dec2
		  in (Code.DEC'Local (dec1',dec2'), env2)
		  (*in (Code.DEC'Seq (dec1',dec2'), env2) (*Bug: local->seq*)*)
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
INDEX: eval - sequence comp/exec
----------------------------------------------------------------------*)

  fun eval'dec env dec =
	  let val (dec',CT_env) = comp'dec env dec
	  in case (exec'dec dec') of
			 This RT_assoc => This (env'CT_RT CT_env RT_assoc)
		   | That x => That x
	  end

  type dec = Lang.dec (* because Code was opened *)					   
  type env = Basis.env (* because CompEnv was opened *)					   

end
