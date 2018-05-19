(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  28.    export
  40.    imports
  315.   functor
  373.   valstat - represent just possible states of (value*idstatus)
  403.   Compile Environment
  463.   merge CT/RT envs...
  484.   patStatus
  522.   source language generation - support fvalbind clauses
  553.   source language generation - support while exp
  578.   pat
  629.   exp
  739.   typbind / datbind
  767.   exbind
  799.   pes - (pat*exp) list 
  817.   valbind
  853.   fvalbind
  868.   dec
  915.   fix compilation P/E/D
  926.   eval - sequence comp/exec
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(* Compiler 2 - stack slot numbers *)

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
	val fold			: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
	val foldR			: ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
	val stringOfInt		: int -> string
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

(*IMPORTSIG: MachineSig MACHINE_2.ML *)
signature MachineSig_Import =
sig
	type vid
	type lab
	type scon
	type value
	type exname
	datatype depth = DEPTH of int
	datatype slot = SLOT of int
	datatype eref = EREF'CT of exname | EREF'RT of depth * slot * vid
	datatype patVarStatus =
		   PVS'BindableV of depth * vid
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
		| PAT'Layered of depth * vid * pat
	  datatype valbind =
		  VALBIND'Seq of pat * exp * valbind option
		| VALBIND'Rec of valbind
	  and dec =
		  DEC'Val of valbind
		| DEC'Exception of (depth * vid) list
		| DEC'Empty
		| DEC'Seq of dec * dec
	  and mrule = MRULE of pat * exp
	  and match = MATCH of mrule list
	  and exprow = EXPROW of (lab * exp) list
	  and exp =
		  EXP'Scon of scon
		| EXP'Var of depth * slot * vid
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

functor COMPILER_2
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
			 sharing type Pos.pos = Lang.pos
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


  fun incDepth (DEPTH d) = DEPTH (d+1)


  fun mkSLOT n = if n<1 then abort "mkSLOT:n<1" else SLOT n

  fun mkSlot (DEPTH d) (DEPTH depth) = mkSLOT (d-depth)

  fun map2nd f (a,b) = (a, f b)
  fun map2'3 f (a,b,c) = (a, f b, c)

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

  datatype val_status = VS'CT of vstat | VS'RTV of depth | VS'RTE of depth

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
				   | (vid,VS'RTV _) => let val v = look'assoc "valenv'CT_RT" sof'vid RT_assoc vid
									   in (vid,(v,Basis.IDSTATUS'V))
									   end
				   | (vid,VS'RTE _) => let val v = look'assoc "valenv'CT_RT" sof'vid RT_assoc vid
									   in (vid,(v,Basis.IDSTATUS'E))
									   end
										   ) a)


  fun env'CT_RT (ENV(se,te,ve)) RT_assoc =
	  let val ve = valenv'CT_RT ve RT_assoc
	  in Basis.env'se_te_ve (se,te,ve)
	  end

(*----------------------------------------------------------------------
INDEX: patStatus
----------------------------------------------------------------------*)


  fun patStatus'longvid d env x =
	  case (case (x) of
				LONG([],vid) => if (isUnbound'vid env vid) 
								then This vid (*short: unbound*)
								else That (Some vid, look'longvid "patvar:look1" env x) (*short: bound*)
			  | _ => That (None, look'longvid "patvar:look2" env x)) of (*long: must be bound*)
		  This vid => (incDepth d, PVS'BindableV (d,vid))
		| That (shortOpt,VS'RTV _) =>
		  (case shortOpt of Some vid => (incDepth d,PVS'BindableV (d,vid)) | None => error "patvar:longV/RT - impossible")
		| That (shortOpt,VS'RTE e_depth) =>
		  (case shortOpt of Some vid => (d,PVS'E (EREF'RT (d,mkSlot d e_depth,vid))) | None => error "patvar:longE/RT - impossible")
		| That (shortOpt,VS'CT vstat) =>
		  (case (vstat) of
               VSTAT'V _        => (case shortOpt of Some vid => (incDepth d,PVS'BindableV (d,vid)) | None => error "patvar:longV")
             | VSTAT'E exname   => (d,PVS'E (EREF'CT exname))
             | VSTAT'C vid      => (d,PVS'C vid)
             | VSTAT'Ref        => error "patvar:ref")

  val shortOpt = fn LONG([],vid) => Some vid | _ => None

  fun patconsStatus'longvid d env x =
	  case (look'longvid "patcons:look" env x) of
		  VS'RTV _ => error "patcons:V/RT"
		| VS'RTE e_depth => 
		  (case (shortOpt x) of Some vid => PCS'E (EREF'RT (d, mkSlot d e_depth,vid))
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
  in fun genVid prefix = let val n = !U
						 in U := 1+n; VID (ID (prefix^"-G#"^stringOfInt n))
						 end
  end
	  
  fun genVids prefix n =
	  if (n<0) then abort "genVids:n<0"
	  else let fun loop acc n = if n=0 then acc
								else loop (genVid (prefix^"-"^stringOfInt n) ::acc) (n-1)
		   in loop [] n
		   end
  fun mrule'clause (pats,tyo_IGNORED,exp) = MRULE (PAT'Tuple pats,exp)
									
  fun exp'clauses prefix pos [] = abort "exp'clauses:[]"
	| exp'clauses prefix pos (clauses as (pats,_,_)::_) =
	  let val vids = genVids prefix (length pats)
	  in exp'fnVids pos vids
		 (EXP'Case (EXP'Tuple (map (exp'vid pos) vids),
					MATCH (pos,map mrule'clause clauses)))
	  end

(*----------------------------------------------------------------------
INDEX: source language generation - support while exp
----------------------------------------------------------------------*)

  fun expDerived'while (e1,e2) =
	  let val pos = makePos{path="expDerived'while",row=0,col=0}
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
INDEX: pat
----------------------------------------------------------------------*)

  fun comp'pats d P pats =
	  case (fold (fn (d,acc,ve) =>
					 fn pat => let val (d,pat',valenv) = P d pat
							   in (d,pat'::acc,valenv'plus ve valenv)
							   end) (d,[],valenv'empty) pats)
	   of (d,acc,ve) =>
		  (d,rev acc,ve)

  fun comp'patrow d P (PATROW (lps,{wildcard})) =
	  case (fold (fn (d,acc,ve) =>
					 fn (lab,pat) => let val (d,pat',valenv) = P d pat
									 in (d,(lab,pat')::acc,valenv'plus ve valenv)
									 end) (d,[],valenv'empty) lps)
	   of (d,acc,ve) =>
		  (d,Code.PATROW (rev acc, {wildcard=wildcard}),ve)

  fun U_comp'pat env d P pat =
	  case pat of
          PAT'Wild                  => (d,Code.PAT'Wild,valenv'empty)
        | PAT'Scon scon             => (d,Code.PAT'Scon scon,valenv'empty)
        | PAT'Var x                 => (case (patStatus'longvid d env x) of
											(_,pvs as PVS'BindableV (_,x)) => let val ve = valenv'single (x,VS'RTV d)
																			  in (incDepth d, Code.PAT'Var pvs, ve)
																			  end
										  | (_,pvs) => (d, Code.PAT'Var pvs, valenv'empty))
        | PAT'Record pr             => map2'3 Code.PAT'Record (comp'patrow d P pr)
        | PAT'Unit                  => (d,Code.PAT'Unit,valenv'empty)
        | PAT'Tuple pats            => map2'3 Code.PAT'Tuple (comp'pats d P pats)
        | PAT'List pats             => map2'3 Code.PAT'List (comp'pats d P pats)
        | PAT'Cons (x,pat)          => let val (d,pat',valenv) = P d pat
									   in (d, Code.PAT'Cons (patconsStatus'longvid d env x, pat'),valenv)
									   end
        | PAT'Typed (pat,ty)        => P d pat
        | PAT'Layered (vid,tyo,pat)	=> let val TOP_d = d
										   val valenv1 = valenv'single (vid,VS'RTV d)
										   val (d,pat',valenv2) = P (incDepth d) pat
										   val valenv = valenv'plus valenv1 valenv2
									   in (d,Code.PAT'Layered (TOP_d,vid,pat'),valenv)
									   end


  local
	  fun P env d pat = U_comp'pat env d (P env) pat
  in
  val comp'pat = P
  end

(*----------------------------------------------------------------------
INDEX: exp
----------------------------------------------------------------------*)

  fun comp'longvid env d (longvid,pos) =
	  case longvid of
		  LONG([],vid) => (case (look'longvid "comp'longvid-1" env longvid) of
							   VS'CT vstat => Code.EXP'Value (value'vstat vstat)
							 | VS'RTV v_depth => Code.EXP'Var (d,mkSlot d v_depth,vid)
							 | VS'RTE e_depth => Code.EXP'Var (d,mkSlot d e_depth,vid))
		| _ => (case (look'longvid "comp'longvid-2" env longvid) of
					VS'CT vstat => Code.EXP'Value (value'vstat vstat)
				  | VS'RTV _ => error "comp'longvid:long/RT"
				  | VS'RTE _ => error "comp'longvid:long/RT")

  fun comp'mrule E env d (MRULE (pat,exp)) = let val (d,pat',ve) = comp'pat env d pat
												 val env = env'plus env (env'valenv ve)
												 val (d,exp) = E env d exp
											 in (d, Code.MRULE (pat',exp))
										   end

  fun comp'match E env (ORIG_d as d) (MATCH (pos,xs)) = 
	  case (fold (fn (d,acc) => 
					 fn mrule => 
						let val (d,mrule) = comp'mrule E env ORIG_d mrule (* stack reverted between maches *)
						in (d,mrule::acc)
						end) (d,[]) xs)
	   of (d,acc) => (d,Code.MATCH (rev acc)) (* actual result depth is ignored by all callers *)


  fun comp'exprow Eenv d exprow =
	  case exprow of
		  EXPROW xs => 
		  case (fold (fn (d,acc) =>
						 fn (lab,exp) => let val (d,exp') = Eenv d exp
										 in (d,(lab,exp')::acc)
										 end) (d,[]) xs)
		   of (d,acc) =>
			  (d,Code.EXPROW (rev acc))

  fun comp'exps Eenv d exps =
	  case (fold (fn (d,acc) =>
					 fn exp => let val (d,exp') = Eenv d exp
							   in (d,exp'::acc)
							   end) (d,[]) exps) 
	   of (d,acc) => (d,rev acc)



  fun U_comp'exp E D env d exp =
	  case exp of
          EXP'Scon scon             => (d, Code.EXP'Scon scon)
        | EXP'Var x      			=> (d, comp'longvid env d x)
        | EXP'Record exprow         => map2nd Code.EXP'Record (comp'exprow (E env) d exprow)
        | EXP'Select lab            => (d,Code.EXP'Select lab)
        | EXP'Unit                  => (d,Code.EXP'Unit)
        | EXP'Tuple exps            => map2nd Code.EXP'Tuple (comp'exps (E env) d exps)
        | EXP'List exps             => map2nd Code.EXP'List (comp'exps (E env) d exps)
        | EXP'Vector exps           => map2nd Code.EXP'Vector (comp'exps (E env) d exps)
        | EXP'Seq exps              => map2nd Code.EXP'Seq (comp'exps (E env) d exps)
        | EXP'Let (dec,exp)         => let val ORIG_d = d
										   val (d, dec',env1) = D d env dec
										   val env = env'plus env env1
										   val (d,exp') = E env d exp
									   in (ORIG_d, Code.EXP'Let (dec',exp')) (* hack to match machne! *)
									   end
        | EXP'App (e1,e2)           => let val (d,e1) = E env d e1
										   val (d,e2) = E env d e2
									   in (d, Code.EXP'App (e1,e2))
									   end
        | EXP'Typed (exp,ty)        => E env d exp
        | EXP'Andalso (e1,e2)       => let val (d,e1) = E env d e1
										   val (d,e2) = E env d e2
									   in (d, Code.EXP'Andalso (e1,e2))
									   end
        | EXP'Orelse (e1,e2)        => let val (d,e1) = E env d e1
										   val (d,e2) = E env d e2
									   in (d, Code.EXP'Orelse (e1,e2))
									   end
        | EXP'Handle (exp,match)    => let val ORIG_d = d
										   val (d,exp) = E env d exp
										   val (d,match) = comp'match E env d match
									   in (ORIG_d, Code.EXP'Handle (exp,match))
									   end
        | EXP'Raise exp             => map2nd Code.EXP'Raise (E env d exp)
        | EXP'If (e1,e2,e3)         => let val (d,e1) = E env d e1
										   val (d,e2) = E env d e2
										   val (d,e3) = E env d e3
									   in (d, Code.EXP'If (e1,e2,e3))
									   end
        | EXP'While (e1,e2)         => E env d (expDerived'while (e1,e2))
        | EXP'Case (exp,match)      => let val ORIG_d = d
										   val (d,exp) = E env d exp
										   val (d,match) = comp'match E env d match
									   in (ORIG_d, Code.EXP'Case (exp,match))
									   end
        | EXP'Fn match              => (d, Code.EXP'Fn (let val (d_ignored,match) = comp'match E env d match
														in match
														end))


  val Q = U_comp'exp
  fun U_comp'exp E D env d exp = let val (d',code) = Q E D env d exp
									 val () = if d=d' then () else abort "comp-exp-inreases-depth"
								 in (d',code)
								 end

(*----------------------------------------------------------------------
INDEX: typbind / datbind
----------------------------------------------------------------------*)

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

(*----------------------------------------------------------------------
INDEX: exbind
----------------------------------------------------------------------*)

  fun comp'exbind1 d env b =
	  case b of
		  EXBIND1'New (vid,tyo) => let val valenv = valenv'single (vid,VS'RTE d)
								   in (incDepth d,[(d,vid)], valenv)
								   end
		| EXBIND1'Copy (vid,longvid) =>
		  (case (look'longvid "comp'exbind1" env longvid) of
			   VS'RTV _ => error "exbind(copy)/VS'RT:expected status E, got V"
			 | VS'RTE depth => let val valenv = valenv'single (vid,VS'RTE depth)
							   in (d,[],valenv)
							   end
			 | VS'CT vstat =>
			   (case (vstat) of
					VSTAT'V _ => error "exbind(copy):expected status E, got V"
				  | VSTAT'C _ => error "exbind(copy):expected status E, got C"
				  | VSTAT'Ref => error "exbind(copy):expected status E, got Ref"
				  | VSTAT'E exname => let val valenv = valenv'single (vid,VS'CT vstat)
									  in (d,[],valenv)
									  end))

  fun comp'exbind d env (EXBIND bs) =
	  case (fold (fn (d,gens,ve) => 
					 fn b => let val (d,gens1,valenv) = comp'exbind1 d env b
							 in (d,gens1 @ gens,valenv'plus ve valenv)
							 end) (d,[],valenv'empty) bs)
	   of (d,gens,ve) =>
		  (d,rev gens,ve)

(*----------------------------------------------------------------------
INDEX: pes - (pat*exp) list 
----------------------------------------------------------------------*)

  fun pes'valbind vb =
	  case vb of		
		  VALBIND'Rec vb => pes'valbind vb (* 2nd inner rec ignored *)
		| VALBIND'Seq (pat,exp,None) => [(pat,exp)]
		| VALBIND'Seq (pat,exp,Some vb) => (pat,exp) :: pes'valbind vb

  fun valbind'pes [] = abort "valbind'pes[]"
	| valbind'pes [(p,e)] = VALBIND'Seq (p,e,None)
	| valbind'pes ((p,e)::pes) = VALBIND'Seq (p,e,Some(valbind'pes pes))

  fun code_valbind'pes [] = abort "code_valbind'pes[]"
	| code_valbind'pes [(p,e)] = Code.VALBIND'Seq (p,e,None)
	| code_valbind'pes ((p,e)::pes) = Code.VALBIND'Seq (p,e,Some(code_valbind'pes pes))

(*----------------------------------------------------------------------
INDEX: valbind
----------------------------------------------------------------------*)

  fun comp'valbind E env d vb =
	  case vb of		
		  VALBIND'Seq (pat,exp,None) => let val (d,exp) = E env d exp
											val (d,pat',valenv) = comp'pat env d pat
											val vb' = Code.VALBIND'Seq (pat',exp,None)
										in (d, vb',valenv)
										end
		| VALBIND'Seq (pat,exp,Some vb1) => let val (d,exp) = E env d exp
												val (d,pat',valenv1) = comp'pat env d pat
												val (d,vb1',valenv2) = comp'valbind E env d vb1
												val valenv = valenv'plus valenv1 valenv2
												val vb' = Code.VALBIND'Seq (pat',exp,Some vb1')
										   in (d, vb',valenv)
										   end
		| VALBIND'Rec vb1 =>
		  case (fold (fn (d,stage1s,ve) =>
						 fn (pat,exp) =>
							let val (d,pat',valenv) = comp'pat env d pat
								val stage1 = (pat',exp)
							in (d,stage1::stage1s,valenv'plus ve valenv)
							end) (d,[],valenv'empty) (pes'valbind vb1))
		   of (d,stage1s,ve) =>
			  let val env = env'plus env (env'valenv ve)
				  val vb' = 
					  Code.VALBIND'Rec 
					  (code_valbind'pes (map (fn (pat',exp) =>
												 let val (d1_ignored,exp') = E env d exp
												 in (pat',exp')
												 end) (rev stage1s)))
			  in (d,vb',ve)
			  end

(*----------------------------------------------------------------------
INDEX: fvalbind
----------------------------------------------------------------------*)

  fun valbind'fvalbind (FVALBIND fs) =
	  VALBIND'Rec 
	  (valbind'pes
	   (map (fn (pos,vid,clauses) => let val exp = exp'clauses (sof'vid vid) pos clauses
									 in (PAT'Var (LONG ([],vid)), exp)
									 end) fs))
																				
  fun comp'fvalbind E env d fb = let val (d,vb,ve) = comp'valbind E env d (valbind'fvalbind fb)
								 in (d, Code.DEC'Val vb, env'valenv ve)
								 end

(*----------------------------------------------------------------------
INDEX: dec
----------------------------------------------------------------------*)

  fun U_comp'dec E D d env dec = 
	  case dec of
		  DEC'Val vb => let val (d,vb,ve) = comp'valbind E env d vb
						in (d, Code.DEC'Val vb, env'valenv ve)
						end
		| DEC'Fval fb => comp'fvalbind E env d fb
		| DEC'Type typbind => (d,Code.DEC'Empty, env'tyenv (comp'typbind typbind))
		| DEC'Datatype (db,tbo) =>
		  let val (ve,te) = comp'datbind db
			  val env1 = env'plus (env'valenv (comp'valenv ve)) (env'tyenv te)
			  val env2 = env'tyenv (comp'optWithType tbo)
		  in (d, Code.DEC'Empty, env'plus env1 env2)
		  end
		| DEC'DataReplicate (tycon,tyconlong) =>
		  let val ve = look'longtycon env tyconlong
			  val env1 = env'valenv (comp'valenv ve)
			  val env2 = env'tyenv (Basis.tyenv'single (tycon,ve))
		  in (d,Code.DEC'Empty, env'plus env1 env2)
		  end
		| DEC'Abstype (db,tbo,dec) =>
		  let val (ve,te_IGNORED) = comp'datbind db
			  val env1 = env'valenv (comp'valenv ve)
			  val env2 = env'tyenv (comp'optWithType tbo)
			  val (d, dec',env3) = D d (env'plusList [env,env1,env2]) dec
		  in (d, dec', env'plus env2 env3)
		  end
		| DEC'Exception exbind => 
		  let val (d,xs,ve) = comp'exbind d env exbind
		  in (d, Code.DEC'Exception xs, env'valenv ve)
		  end
		| DEC'Local (dec1,dec2) =>
		  let val (d,dec1',env1) = D d env dec1
			  val (d,dec2',env2) = D d (env'plus env env1) dec2
		  in (d, Code.DEC'Seq (dec1',dec2'), env2) (* local->seq - works for stack slot based machine *)
		  end
		| DEC'Open xs => (d, Code.DEC'Empty, env'plusList (map (comp'env o look'longstrid env) xs))
		| DEC'Empty => (d, Code.DEC'Empty,env'empty)
		| DEC'Seq (dec1,dec2) =>
		  let val (d,dec1',env1) = D d env dec1
			  val (d,dec2',env2) = D d (env'plus env env1) dec2
		  in (d, Code.DEC'Seq (dec1',dec2'),env'plus env1 env2)
		  end

(*----------------------------------------------------------------------
INDEX: fix compilation P/E/D
----------------------------------------------------------------------*)

  local
	  fun E env d exp = U_comp'exp E D env d exp
	  and D d env dec = U_comp'dec E D d env dec
  in
  val comp'dec = D
  end

(*----------------------------------------------------------------------
INDEX: eval - sequence comp/exec
----------------------------------------------------------------------*)

  fun eval'dec env dec =
	  let val (final_depth,dec',CT_env) = comp'dec (DEPTH 0) (comp'env env) dec
	  in case (exec'dec dec') of
			 This RT_assoc => This (env'CT_RT CT_env RT_assoc)
		   | That x => That x
	  end

  type dec = Lang.dec (* because Code was opened *)					   
  type env = Basis.env (* because CompEnv was opened *)					   

end
