(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  17.    export
  31.    imports
  50.    functor
  89.    pat
  251.   mrule,match
  272.   makeEvalClosure
  285.   exp
  351.   source language generation -- support fvalbind clauses
  382.   dec
  484.   fix -- pat/exp/dec
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature EvalCoreSig =
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
	val stringOfInt		: int -> string
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
    type sigid;
    type funid;
	datatype 'a long = LONG of strid list * 'a
	datatype lab = LAB'Id of id | LAB'Num of int
    type scon;
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
	type sigid
	type funid
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
	  | EXP'Var of vid long
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
	val exp'vid : vid -> exp
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
	val exval'Match		: exval
	val exval'Bind		: exval
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
end;
(*IMPORTDONE:*)

(*IMPORTSIG: BasisSig BASIS.ML *)
signature BasisSig_Import =
sig
	type 'a long
	type vid
	type tycon
	type strid
	type sigid
	type funid
	type value
	datatype idstatus = IDSTATUS'C | IDSTATUS'E | IDSTATUS'V
	type recval
	type tyenv 
	type valenv 
	type env
		 
	val tyenv'empty		: tyenv
	val valenv'empty	: valenv
	val env'empty		: env
	val env'tyenv		: tyenv -> env
	val env'valenv		: valenv -> env
	val tyenv'plus		: tyenv -> tyenv -> tyenv
	val valenv'plus		: valenv -> valenv -> valenv
	val env'plus		: env -> env -> env
	val tyenv'plusList	: tyenv list -> tyenv
	val valenv'plusList : valenv list -> valenv
	val env'plusList	: env list -> env
	val tyenv'single	: tycon * valenv -> tyenv
	val valenv'single	: vid * (recval * idstatus) -> valenv
	val recval'value	: value -> recval
	val recval'fn		: (valenv -> value) -> recval
	val value'recval	: recval -> value
	val valenv'Rec		: valenv -> valenv
												
	val isUnbound'vid	: env -> vid -> bool
	val look'longstrid	: env -> strid long -> env
	val look'longtycon	: env -> tycon long -> valenv
	val look'longvid	: env -> vid long -> value * idstatus
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor EVAL_CORE
			(val prefixNML : string
			 structure Prel : PrelSig_Import
			 structure Atom : AtomSig_Import
			 structure Lang : LangSig_Import
			 structure Value : ValueSig_Import
			 structure Basis : BasisSig_Import
			 sharing type Atom.lab = Lang.lab = Value.lab
			 sharing type Atom.scon = Lang.scon = Value.scon
			 sharing type Atom.long = Lang.long = Basis.long
			 sharing type Atom.vid = Lang.vid = Value.vid = Basis.vid
			 sharing type Atom.strid = Lang.strid = Basis.strid
			 sharing type Atom.tycon = Lang.tycon = Basis.tycon
			 sharing type Atom.funid = Lang.funid = Basis.funid
			 sharing type Atom.sigid = Lang.sigid = Basis.sigid
			 sharing type Value.value = Basis.value
							   ) : EvalCoreSig =
struct


  open Prel Atom Lang Value Basis
	   
  fun error s = abort ("error: eval_core: "^s)
  fun notYet s = abort ("notYet: eval_core: "^s)

  fun mapLR E xs = (* map guaranteed to traverse L->R *)
	  let fun loop acc [] = rev acc (* this version is also tail-recursive *)
			| loop acc (x::xs) = loop (E x :: acc) xs
	  in loop [] xs
	  end

  fun NMLecho s = echo (prefixNML^s)
									   
  exception Pack of exval

  val Napps = ref 0;
  val apply'value = fn v1 => fn v2 =>
	  (Napps := 1 + !Napps; apply'value v1 v2)

  val Nlooks = ref 0;
  val look'longvid = fn env => fn vid =>
	  (Nlooks := 1 + !Nlooks; look'longvid env vid)

(*----------------------------------------------------------------------
INDEX: pat
----------------------------------------------------------------------*)
							  
  fun maybeGetBindableVid'longvid env (LONG (_::_,vid)) = None
	| maybeGetBindableVid'longvid env (longvid as LONG ([],vid)) =
	  if (isUnbound'vid env vid) then (Some vid)
	  else let val (_,ids) = look'longvid env longvid
		   in case ids of
				  IDSTATUS'V => Some vid
				| IDSTATUS'C => None
				| IDSTATUS'E => None
		   end


  datatype status =
		   STATUS'V
		 | STATUS'Ref
		 | STATUS'C of vid
		 | STATUS'E of exname

  fun lookStatus'longvid env x = 
	  let val (x_value,ids) = look'longvid env x
	  in case ids of
			 IDSTATUS'V => STATUS'V
		   | IDSTATUS'C =>
			 let val either = ref_or_con0'value "patcons(C)" x_value
			 in case either of
					This {Ref=()} => STATUS'Ref
				  | That {Con0=vid} => STATUS'C vid
			 end
		   | IDSTATUS'E =>
			 let val exname = excon0'value "parvar(E)" x_value
			 in STATUS'E exname
			 end
	  end

	  
  fun valenv'singleValue (k,(v,ids)) = valenv'single (k,(recval'value v,ids))
			   
  fun eval'vidBinding vid recval = valenv'single (vid,(recval,IDSTATUS'V))
		  
  fun eval'patvar env recval x =
	  case (maybeGetBindableVid'longvid env x) of
		  Some vid => Some (eval'vidBinding vid recval)
		| None =>
		  case (lookStatus'longvid env x) of
			  STATUS'V => error "patvar(V)"
			| STATUS'Ref => error "patvar(Ref)"
			| STATUS'C vid =>
			  let val (vid',vOpt) = con'value "patvar(C)" (value'recval recval)
			  in case vOpt of
					 Some _ => None
				   | None => if (eq'vid vid vid')
							 then (Some valenv'empty) else None
			  end
			| STATUS'E exname =>
			  let val (exname',vOpt) = excon'value "parvar(E)" (value'recval recval)
			  in case vOpt of
					 Some _ => None
				   | None => if (eq'exname exname exname')
							 then (Some valenv'empty) else None
			  end

  fun eval'patcons P env v x pat =
	  case (lookStatus'longvid env x) of
		  STATUS'V => error "patcons(V)"
		| STATUS'Ref =>
		  let val v' = derefAddr'value "patcons(ref)" v
		  in P env v' pat
		  end
		| STATUS'C vid =>
		  let val (vid',vOpt) = con'value "patcons(C)" v
		  in case vOpt of
				 Some v' => if (eq'vid vid vid')
							then (P env v' pat) else None
			   | None => None
		  end
		| STATUS'E exname =>
		  let val (exname',vOpt) = excon'value "parvar(E)" v
		  in case vOpt of
				 Some v' => if (eq'exname exname exname')
							then (P env v' pat) else None
			   | None => None
		  end

  fun (*eval'patrow P env record (PATROW (lps,{wildcard=true})) = notYet "patrow:wildcard"
	| *)eval'patrow P env record (PATROW (lps,{wildcard=IGNORED})) =
	  let
		fun loop accVe [] = Some accVe
		  | loop accVe ((lab,pat)::lps) =
			let val v = look'record "patrow" record lab
			in case (P env v pat) of
				 None => None
			   | Some ve => loop (valenv'plus accVe ve) lps
			end
	  in
		loop valenv'empty lps
	  end
		  
  fun eval'pattuple P env record pats =
	  let fun loop accVe n [] = Some accVe
			| loop accVe n (pat::pats) =
			  let val v = look'record "pattuple" record (LAB'Num n)
			  in case (P env v pat) of
					 None => None
				   | Some ve => loop (valenv'plus accVe ve) (n+1) pats
			  end
	  in
		loop valenv'empty 1 pats
	  end

  fun eval'patlist P env v pats =
	  case (pats,listCons'value "patlist" v) of
		  ([],None) => Some valenv'empty
		| (pat::pats,Some(v1,v2)) =>
		  (case (P env v1 pat) of
			   None => None
			 | Some ve1 => (case (eval'patlist P env v2 pats) of
								None => None
							  | Some ve2 => Some (valenv'plus ve1 ve2)))
		| _ => None
			   
  fun inner_eval'pat_U P env v pat =
	  case pat of
		  PAT'Wild => Some valenv'empty
		| PAT'Scon scon =>
		  let val scon' = scon'value "eval'pat" v
		  in if (eq'scon scon scon') then Some valenv'empty
			 else None
		  end
		| PAT'Var x => abort "inner_eval'pat_U--Var"
		| PAT'Record pr =>
		  let val record = record'value "eval'pat" v
		  in eval'patrow P env record pr
		  end
		| PAT'Unit =>
		  let val U as () = unit'value "eval'pat" v
		  in Some valenv'empty
		  end
		| PAT'Tuple pats =>
		  let val record = record'value "eval'pat" v
		  in eval'pattuple P env record pats
		  end
		| PAT'List pats => eval'patlist P env v pats
		| PAT'Cons (x,pat) => eval'patcons P env v x pat
		| PAT'Typed (pat,ty_IGNORED) => P env v pat
		| PAT'Layered (vid,tyo_IGNORED,pat) => abort "inner_eval'pat_U--Layered"
											   
  fun eval'pat_U P env recval pat =
	  case pat of
		  PAT'Var x => eval'patvar env recval  x 
		| PAT'Layered (vid,tyo_IGNORED,pat) =>
		  (case (P env recval pat) of
			   None => None
			 | Some ve => Some (valenv'plus (eval'vidBinding vid recval) ve))
		| _ =>
		  let fun P' env v pat = P env (recval'value v) pat
		  in inner_eval'pat_U P' env (value'recval recval) pat
		  end

(*----------------------------------------------------------------------
INDEX: mrule,match
----------------------------------------------------------------------*)

  fun eval'mrule P E env value (MRULE (pat,exp)) =
	  case (P env value pat) of
		  None => None
		| Some ve =>
			  let val env = env'plus env (env'valenv ve)
			  in Some (E env exp)
			  end

  fun eval'match P E env value (MATCH (pos,xs)) =
	  let fun loop [] = None
			| loop (x::xs) =
			  case (eval'mrule P E env value x) of
				  Some res => Some res
				| None => loop xs
	  in loop xs
	  end

(*----------------------------------------------------------------------
INDEX: makeEvalClosure
----------------------------------------------------------------------*)

  fun makeEvalClosure P E (match,env,ve) =
	  closure'fn
		  (fn vArg =>
			  let val env = env'plus env (env'valenv (valenv'Rec ve))
			  in case (eval'match P E env vArg match) of
					 Some res => res
				   | None => raise (Pack exval'Match)
			  end)

(*----------------------------------------------------------------------
INDEX: exp
----------------------------------------------------------------------*)
	
  fun eval'exprow Eenv exprow =
	  case exprow of
		  EXPROW xs => mapLR (fn (lab,exp) => (lab,Eenv exp)) xs

  fun eval'seq Eenv [] = abort "eval'seq"
	| eval'seq Eenv (exp::exps) =
	  let fun loop v [] = v
			| loop _ (exp::exps) = loop (Eenv exp) exps
	  in loop (Eenv exp) exps
	  end
	  
  fun inner_eval'exp_U P E D env exp =
	  case exp of
		  EXP'Scon scon => value'scon scon
		| EXP'Var x =>
		  let val (v,is) = look'longvid env x
		  in v
		  end
		| EXP'Record exprow => value'record (record'valrow (eval'exprow (E env) exprow))
		| EXP'Select lab => notYet "exp:select"
		| EXP'Unit => value'record record'empty
		| EXP'Tuple exps => value'record (record'values (mapLR (E env) exps))
		| EXP'List exps => value'list (mapLR (E env) exps)
		| EXP'Vector exps => value'vector (Vector.fromList (mapLR (E env) exps))
		| EXP'Seq exps => eval'seq (E env) exps
		| EXP'Let (dec,exp) =>
		  let val env' = D env dec
		  in E (env'plus env env') exp
		  end
		| EXP'App (e1,e2) => apply'value (*P E*) (E env e1) (E env e2)
		| EXP'Typed (exp,ty) => E env exp
		| EXP'Andalso (e1,e2) => if (bool'value "andalso" (E env e1))
								 then E env e2
								 else value'false
		| EXP'Orelse (e1,e2) => if (bool'value "orele" (E env e1))
								then value'true
								else E env e2
		| EXP'Handle (exp,match) =>
		  ((E env exp)
		   handle Pack exval =>
				  (case (eval'match P E env (value'exval exval) match) of
					   None => raise (Pack exval)
					 | Some res => res))
		| EXP'Raise exp =>
		  let val exval = exval'value "eval'exp" (E env exp)
		  in (NMLecho "***raising"; raise (Pack exval))
		  end
		| EXP'If (e1,e2,e3) => if (bool'value "if" (E env e1))
							   then E env e2
							   else E env e3
		| EXP'While (e1,e2) => notYet "while"
		| EXP'Case (exp,match) => (case (eval'match P E env (E env exp) match) of
									   Some res => res
									 | None => raise (Pack exval'Match))
		| EXP'Fn match =>
		  abort "inner_eval'exp_U--Fn"
		  
  fun eval'exp_U P E D env exp =
	  case exp of
		  EXP'Fn match => recval'fn (fn ve => value'closure (makeEvalClosure P E (match,env,ve)))
		| _ => recval'value (inner_eval'exp_U P E D env exp)

(*----------------------------------------------------------------------
INDEX: source language generation -- support fvalbind clauses
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
		 (EXP'Case (EXP'Tuple (map exp'vid vids),
					MATCH (pos, map mrule'clause clauses)))
	  end

(*----------------------------------------------------------------------
INDEX: dec
----------------------------------------------------------------------*)
			   
  fun eval'valbind P E env vb =
	  case vb of		
		  VALBIND'Rec vb => valenv'Rec (eval'valbind P E env vb)
		| VALBIND'Seq (pat,exp,vbo) =>
		  case (P env (E env exp) pat) of
			  None => raise (Pack exval'Bind)
			| Some ve =>
			  (case vbo of
				   None => ve
				 | Some vb => valenv'plus ve (eval'valbind P E env vb))

  fun eval'fvalbind Eenv (FVALBIND fs) =
	  valenv'Rec (valenv'plusList
					(map (fn (pos,vid,clauses) =>
							 eval'vidBinding vid (Eenv (exp'clauses pos clauses))
							 ) fs))

  fun eval'exbind1 env b =
	  case b of
		  EXBIND1'New (vid,IGNORED_tyo) =>
		  let val en = gen'exname vid
			  val v = value'exval (exval'con0 en)
		  in valenv'singleValue (vid,(v,IDSTATUS'E))
		  end
		| EXBIND1'Copy (vid,longvid) =>
		  let val (v,ids) = look'longvid env longvid
		  in case ids of
				 IDSTATUS'V => error "exbind(copy):expected status E, got V"
			   | IDSTATUS'C => error "exbind(copy):expected status E, got C"
			   | IDSTATUS'E =>
				 let val exname = excon0'value "eval'exbind1(E)" v
				 in valenv'singleValue (vid,(v,IDSTATUS'E))
				 end
		  end

  fun eval'exbind env (EXBIND bs) =
	  valenv'plusList (mapLR (eval'exbind1 env) bs)

  fun eval'typbind (TYPBIND xs) =
	  tyenv'plusList (map (fn (_,tycon,_) =>
							  tyenv'single (tycon,valenv'empty)
							  ) xs)

  fun eval'datbind (DATBIND xs) =
	  let fun loop ve te [] = (ve,te)
			| loop ve te ((IGNORED_tyvars,tycon,clauses)::xs) =
			  let val ve' = 
					  valenv'plusList
						(map (fn (vid,IGNORED_tyo) =>
								 let val v = value'con0 vid
								 in valenv'singleValue (vid,(v,IDSTATUS'C))
								 end) clauses)
				  val te' = tyenv'single (tycon,ve')
			  in loop (valenv'plus ve ve') (tyenv'plus te te') xs
			  end
	  in loop valenv'empty tyenv'empty xs
	  end

  fun eval'optWithType tbo =
	  case tbo of Some tb => eval'typbind tb | None => tyenv'empty

  fun eval'dec_U P E D env dec = 
	  case dec of
		  DEC'Val vb => env'valenv (eval'valbind P E env vb)
		| DEC'Fval fb => env'valenv (eval'fvalbind (E env) fb)
		| DEC'Type typbind => env'tyenv (eval'typbind typbind)
		| DEC'Datatype (db,tbo) =>
		  let val (ve,te) = eval'datbind db
			  val env1 = env'plus (env'valenv ve) (env'tyenv te)
			  val env2 = env'tyenv (eval'optWithType tbo)
		  in env'plus env1 env2
		  end
		| DEC'DataReplicate (tycon,tyconlong) =>
		  let val ve = look'longtycon env tyconlong
			  val env1 = env'valenv ve
			  val env2 = env'tyenv (tyenv'single (tycon,ve))
		  in env'plus env1 env2
		  end
		| DEC'Abstype (db,tbo,dec) =>
		  let val (ve,te_IGNORED) = eval'datbind db
			  val env1 = env'valenv ve
			  val env2 = env'tyenv (eval'optWithType tbo)
			  val env3 = D (env'plusList [env,env1,env2]) dec
		  in env'plus env2 env3
		  end
		| DEC'Exception exbind => env'valenv (eval'exbind env exbind)
		| DEC'Local (dec1,dec2) =>
		  let val env1 = D env dec1
		  in D (env'plus env env1) dec2
		  end
		| DEC'Open xs => env'plusList (map (look'longstrid env) xs)
		| DEC'Empty => env'empty
		| DEC'Seq (dec1,dec2) =>
		  let val env1 = D env dec1
			  val env2 = D (env'plus env env1) dec2
		  in env'plus env1 env2
		  end

(*----------------------------------------------------------------------
INDEX: fix -- pat/exp/dec
----------------------------------------------------------------------*)

  fun eval'patR x = eval'pat_U eval'patR x
  fun eval'pat env v = eval'patR env (recval'value v)

  fun eval'exp env exp = value'recval (eval'expR env exp)
  and eval'expR x = eval'exp_U eval'pat	 eval'exp  eval'dec x
  and eval'dec x  = eval'dec_U eval'patR eval'expR eval'dec x

  val mayRaise_eval'dec = eval'dec

  fun eval'dec env dec =
	  This (mayRaise_eval'dec env dec)
	  handle (Pack exval) => That exval

  val eval'dec = 
	  fn env => fn dec => 
				   let 
					   val () = Napps := 0
					   val () = Nlooks := 0
					   val res = eval'dec env dec
					   val () = NMLecho ("***Napps = " ^ stringOfInt (!Napps))
					   val () = NMLecho ("***Nlooks = " ^ stringOfInt (!Nlooks))
				   in res 
				   end


end
