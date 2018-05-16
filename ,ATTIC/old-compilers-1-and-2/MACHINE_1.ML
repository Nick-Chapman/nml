(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  24.    export
  104.   imports
  209.   functor
  245.   patStatus
  261.   Code - runtime versions of: pat,exp,dec etc
  314.   exec'* - runtime
  320.   runenv
  336.   runtime counts
  351.   pat
  468.   exval'match
  479.   mrule,match
  500.   makeEvalClosure
  509.   varLookup
  518.   exp
  584.   recfixenv - ref based recursive fixup...
  623.   dec..
  680.   fix -- pat/exp/dec
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature MachineSig =
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
	  and fvalbind = FVALBIND of (pos * vid * exp) list (* clauses expanded to expression *)
	  and dec =
		  DEC'Val of valbind
		| DEC'Fval of fvalbind
		| DEC'Exception of vid list (* generative *)
		| DEC'Local of dec * dec
		| DEC'Empty
		| DEC'Seq of dec * dec
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
		| EXP'Andalso of exp * exp
		| EXP'Orelse of exp * exp
		| EXP'Handle of exp * match
		| EXP'Raise of exp
		| EXP'If of exp * exp * exp
		| EXP'Case of exp * match
		| EXP'Fn of match
		| EXP'Value of value (* created by compile-time env-lookup *)
	end

	type exval
	type ('a,'b) assoc
	val exec'dec : Code.dec -> ((vid,value) assoc,exval) either

end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ../prelude/Prel.ML *)
signature PrelSig_Import =
sig
	val echo			: string -> unit
	val abort			: string -> 'a
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
        val list'assoc      : ('a,'b) assoc -> ('a * 'b) list
        val look'assoc      : string -> (''a -> string) -> (''a,'b) assoc -> ''a -> 'b
    end;
(*IMPORTDONE:*)

(*IMPORTSIG: PosSig POS.ML *)
signature PosSig_Import =
sig
	type pos
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
	datatype lab = LAB'Id of id | LAB'Num of int
    type scon;
	val sof'vid : vid -> string
	val eq'vid : vid -> vid -> bool
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

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor MACHINE_1
			(val prefixNML : string
			 structure Prel : PrelSig_Import
			 structure Assoc : AssocSig_Import
			 structure Pos : PosSig_Import
			 structure Misclay : MisclaySig_Import
			 structure Atom : AtomSig_Import
			 structure Value : ValueSig_Import
			 sharing type Atom.lab = Value.lab
			 sharing type Atom.vid  = Value.vid
			 sharing type Misclay.layout = Value.layout
							   ) : MachineSig =
struct

  open Prel Assoc Pos Misclay Atom Value

    val U = (ID,VID); (* force import as datatype -- avoid eq-type prob *)
	   
  fun error s = abort ("error: exec_core: "^s)

  fun mapLR E xs = (* map guaranteed to traverse L->R *)
	  let fun loop acc [] = rev acc (* this version is also tail-recursive *)
			| loop acc (x::xs) = loop (E x :: acc) xs
	  in loop [] xs
	  end

  fun NMLecho s = echo (prefixNML^s)

  fun mapOption f = fn None => None | Some x => Some (f x)
									   
  exception Pack of exval

(*----------------------------------------------------------------------
INDEX: patStatus
----------------------------------------------------------------------*)

  datatype eref = EREF'CT of exname | EREF'RT of vid

  datatype patVarStatus =
      PVS'BindableV of vid
	| PVS'C of vid
	| PVS'E of eref

  datatype patConStatus =
	  PCS'Ref
	| PCS'C of vid
	| PCS'E of eref

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
	  | PAT'Layered of vid * pat
	datatype valbind =
		VALBIND'Seq of pat * exp * valbind option
	  | VALBIND'Rec of valbind
	and fvalbind = FVALBIND of (pos * vid * exp) list (* clauses expanded to expression *)
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
	  | EXP'Andalso of exp * exp
	  | EXP'Orelse of exp * exp
	  | EXP'Handle of exp * match
	  | EXP'Raise of exp
	  | EXP'If of exp * exp * exp
	  | EXP'Case of exp * match
	  | EXP'Fn of match
      | EXP'Value of value (* created by compile-time env-lookup *)
  end

(*----------------------------------------------------------------------
INDEX: exec'* - runtime
----------------------------------------------------------------------*)

  open Code

(*----------------------------------------------------------------------
INDEX: runenv
----------------------------------------------------------------------*)

  datatype runenv = RUNENV of (vid,value) assoc
  val runenv'empty = RUNENV assoc'empty
  fun runenv'plus (RUNENV a1) (RUNENV a2) = RUNENV (assoc'plus a1 a2)
  fun look'runenv who (RUNENV a) k = look'assoc ("look'runenv("^who^")") sof'vid a k

  val runenv'assoc = RUNENV
  fun assoc'runenv (RUNENV a) = a

  val runenv'plusList = foldR runenv'plus runenv'empty

  fun runenv'single kv = RUNENV (assoc'single kv)

(*----------------------------------------------------------------------
INDEX: runtime counts
----------------------------------------------------------------------*)

  val Napps = ref 0;
  val apply'value = fn v1 => fn v2 =>
	  (Napps := 1 + !Napps; apply'value v1 v2)

  val Nlooks = ref 0;
  val look'runenv = fn who => fn runenv => fn x =>
	  (Nlooks := 1 + !Nlooks; 
	   let (*val () = NMLecho ("look'runenv: "^sof'vid x^" - "^who)*)
	   in look'runenv who runenv x
	   end)

(*----------------------------------------------------------------------
INDEX: pat
----------------------------------------------------------------------*)

  fun exname'eref runenv eref =
	  case eref of
		  EREF'CT exname => exname
		| EREF'RT vid => let val v = look'runenv ("exname'eref") runenv vid
						 in excon0'value "exname'eref" v
						 end

  fun exec'patvar runenv x v =
	  case (x) of
		  PVS'BindableV vid => Some (runenv'single (vid,v))
		| PVS'C vid =>
		  let val (vid',vOpt) = con'value "patvar(C)" v
		  in case vOpt of
				 Some _ => None
			   | None => if (eq'vid vid vid')
						 then (Some runenv'empty) else None
		  end
		| PVS'E eref =>
		  let val exname = exname'eref runenv eref
			  val (exname',vOpt) = excon'value "parvar(E)" v
		  in case vOpt of
				 Some _ => None
			   | None => if (eq'exname exname exname')
						 then (Some runenv'empty) else None
		  end

  fun exec'patcons runenv P v x pat =
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
		| PCS'E eref => let val exname = exname'eref runenv eref
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
			   | Some ve => loop (runenv'plus accVe ve) lps
			end
	  in
		loop runenv'empty lps
	  end
		  
  fun exec'pattuple P record pats =
	  let fun loop accVe n [] = Some accVe
			| loop accVe n (pat::pats) =
			  let val v = look'record "pattuple" record (LAB'Num n)
			  in case (P v pat) of
					 None => None
				   | Some ve => loop (runenv'plus accVe ve) (n+1) pats
			  end
	  in
		loop runenv'empty 1 pats
	  end

  fun exec'patlist P v pats =
	  case (pats,listCons'value "patlist" v) of
		  ([],None) => Some runenv'empty
		| (pat::pats,Some(v1,v2)) =>
		  (case (P v1 pat) of
			   None => None
			 | Some ve1 => (case (exec'patlist P v2 pats) of
								None => None
							  | Some ve2 => Some (runenv'plus ve1 ve2)))
		| _ => None

  fun exec'layered P v (vid,pat) =
	  case (P v pat) of
		  None => None
		| Some ve => Some (runenv'plus (runenv'single (vid,v)) ve)

			   
  fun U_exec'pat P runenv v pat =
	  case pat of
		  PAT'Wild => Some runenv'empty
		| PAT'Scon scon => let val scon' = scon'value "exec'pat" v
						   in if (eq'scon scon scon') then Some runenv'empty
							  else None
						   end
		| PAT'Var x => exec'patvar runenv x v
		| PAT'Record pr => let val record = record'value "exec'pat" v
						   in exec'patrow P record pr
						   end
		| PAT'Unit => let val U as () = unit'value "exec'pat" v
					  in Some runenv'empty
					  end
		| PAT'Tuple pats => let val record = record'value "exec'pat" v
							in exec'pattuple P record pats
							end
		| PAT'List pats => exec'patlist P v pats
		| PAT'Cons (x,pat) => exec'patcons runenv P v x pat
		| PAT'Layered (vid,pat) => exec'layered P v (vid,pat)


  local fun P runenv v pat = U_exec'pat (P runenv) runenv v pat
  in val exec'pat = P
  end

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

  fun exec'mrule E runenv value (MRULE (pat,exp)) =
	  case (exec'pat runenv value pat) of
		  None => None
		| Some ve =>
			  let val runenv = runenv'plus runenv ve
			  in Some (E runenv exp)
			  end

  fun exec'match E runenv value (MATCH (pos,xs)) =
	  let fun loop [] = None
			| loop (x::xs) =
			  case (exec'mrule E runenv value x) of
				  Some res => Some res
				| None => loop xs
	  in loop xs
	  end

(*----------------------------------------------------------------------
INDEX: makeEvalClosure
----------------------------------------------------------------------*)

  fun makeEvalClosure E (match,runenv) =
	  closure'fn (fn vArg => case (exec'match E runenv vArg match) of
								 Some res => res
							   | None => raise (Pack (exval'match match vArg)))

(*----------------------------------------------------------------------
INDEX: varLookup
----------------------------------------------------------------------*)

  fun exec'varLookup runenv (x,pos) =
	  let val v = look'runenv ("exec'varLookup-"^sofPos pos) runenv x
	  in v
	  end

(*----------------------------------------------------------------------
INDEX: exp
----------------------------------------------------------------------*)

  fun exec'exprow Erunenv exprow =
	  case exprow of
		  EXPROW xs => mapLR (fn (lab,exp) => (lab,Erunenv exp)) xs

  fun exec'seq Erunenv [] = abort "exec'seq"
	| exec'seq Erunenv (exp::exps) =
	  let fun loop v [] = v
			| loop _ (exp::exps) = loop (Erunenv exp) exps
	  in loop (Erunenv exp) exps
	  end


  fun value'lab lab =
	  value'closure (closure'fn (fn vArg => let val record = record'value "value'lab" vArg
											in look'record "value'lab" record lab
											end))
	  

  fun U_exec'exp E D runenv exp =
	  case exp of
		  EXP'Scon scon => value'scon scon
		| EXP'Var (x,pos) => exec'varLookup runenv (x,pos)
		| EXP'Record exprow => value'record (record'valrow (exec'exprow (E runenv) exprow))
		| EXP'Select lab => value'lab lab
		| EXP'Unit => value'record record'empty
		| EXP'Tuple exps => value'record (record'values (mapLR (E runenv) exps))
		| EXP'List exps => value'list (mapLR (E runenv) exps)
		| EXP'Vector exps => value'vector (Vector.fromList (mapLR (E runenv) exps))
		| EXP'Seq exps => exec'seq (E runenv) exps
		| EXP'Let (dec,exp) =>
		  let val runenv' = D runenv dec
		  in E (runenv'plus runenv runenv') exp
		  end
		(*| EXP'App (e1,e2) => apply'value (E runenv e1) (E runenv e2) (*func,arg*)*)
		| EXP'App (e1,e2) => let val argV = (E runenv e2) in apply'value (E runenv e1) argV end (*arg,func*)
		| EXP'Andalso (e1,e2) => if (bool'value "andalso" (E runenv e1))
								 then E runenv e2
								 else value'false
		| EXP'Orelse (e1,e2) => if (bool'value "orele" (E runenv e1))
								then value'true
								else E runenv e2
		| EXP'Handle (exp,match) =>
		  ((E runenv exp)
		   handle Pack exval =>
				  (case (exec'match E runenv (value'exval exval) match) of
					   None => raise (Pack exval)
					 | Some res => res))
		| EXP'Raise exp =>
		  let val exval = exval'value "exec'exp(Raise)" (E runenv exp)
		  in (*NMLecho "***raising";*) raise (Pack exval)
		  end
		| EXP'If (e1,e2,e3) => if (bool'value "if" (E runenv e1))
							   then E runenv e2
							   else E runenv e3
		| EXP'Case (exp,match) => let val vCase = E runenv exp
								  in case (exec'match E runenv vCase match) of
									   Some res => res
									 | None => raise (Pack (exval'match match vCase))
								  end
		| EXP'Fn match => value'closure (makeEvalClosure E (match,runenv))
		| EXP'Value v => v

(*----------------------------------------------------------------------
INDEX: recfixenv - ref based recursive fixup...
----------------------------------------------------------------------*)

  datatype recfixenv = RECFIXENV of (vid,(value -> unit) * value) assoc

  val recfixenv'empty = RECFIXENV assoc'empty
  fun recfixenv'plus (RECFIXENV a1) (RECFIXENV a2) = RECFIXENV (assoc'plus a1 a2)
  fun look'recfixenv who (RECFIXENV a) k = look'assoc ("look'recfixenv("^who^")") sof'vid a k
  val recfixenv'plusList = foldR recfixenv'plus recfixenv'empty
  fun recfixenv'single kv = RECFIXENV (assoc'single kv)
  fun runenv'recfixenv (RECFIXENV a) = RUNENV (map'assoc (fn (k,(_,v)) => (k,v)) a)


  fun tie'runenv recfixenv runenv =
	  let fun loop [] = ()
			| loop ((k,v)::xs) = let val (tie,_) = look'recfixenv "tie'runenv" recfixenv k
									 val () = tie v
								 in loop xs
								 end
	  in loop (list'assoc (assoc'runenv runenv))
	  end

  fun recfixenv'vid vid = 
	  let val F = ref None 
		  fun tie f = (F := Some f)
		  fun get() = case (!F) of Some f => f | None => abort ("recursive-hole:"^sof'vid vid)
		  val v = value'closure (closure'fn (fn vArg => apply'value (get()) vArg))
	  in recfixenv'single (vid, (tie,v))
	  end

  fun recfixenv'pat pat =
	  case pat of
		  PAT'Var x => (case (x) of
							PVS'BindableV vid => recfixenv'vid vid
						  | _ => recfixenv'empty)
		| PAT'Layered (vid,pat) => recfixenv'plus (recfixenv'vid vid) (recfixenv'pat pat)
		| _ => recfixenv'empty

(*----------------------------------------------------------------------
Index: dec..
----------------------------------------------------------------------*)

  fun exec'fvalbind E runenv (FVALBIND fs) =
	  let val recfixenv = recfixenv'plusList (map (fn (_,vid,_) => (recfixenv'vid vid)) fs)
		  val runenvDown = runenv'plus runenv (runenv'recfixenv recfixenv)
		  val runenvUp = runenv'plusList (map (fn (_,vid,exp) => runenv'single (vid, E runenvDown exp)) fs)
		  val () = tie'runenv recfixenv runenvUp
	  in
		  runenvUp
	  end

  fun recfixenv'valbind vb =
	  case vb of
		  VALBIND'Rec vb => recfixenv'valbind vb
		| VALBIND'Seq (pat,_,None) => recfixenv'pat pat
		| VALBIND'Seq (pat,_,Some vb) => recfixenv'plus (recfixenv'pat pat) (recfixenv'valbind vb)


  fun exec'valbind E runenv vb =
	  case vb of
		  VALBIND'Rec vb =>  let val recfixenv = recfixenv'valbind vb
								 val runenvDown = runenv'plus runenv (runenv'recfixenv recfixenv)
								 val runenvUp = exec'valbind E runenvDown vb
								 val () = tie'runenv recfixenv runenvUp
							 in runenvUp
							 end
		| VALBIND'Seq (pat,exp,vbo) =>
		  case (exec'pat runenv (E runenv exp) pat) of
			  None => raise (Pack exval'Bind)
			| Some ve =>
			  (case vbo of
				   None => ve
				 | Some vb => runenv'plus ve (exec'valbind E runenv vb))

  fun exec'genException vid =
	  let val en = gen'exname vid
		  val v = value'exval (exval'con0 en)
	  in runenv'single (vid,v)
	  end

  fun U_exec'dec E D runenv dec = 
	  case dec of
		  DEC'Val vb => exec'valbind E runenv vb
		| DEC'Fval fb => exec'fvalbind E runenv fb
		| DEC'Exception vids => runenv'plusList (mapLR exec'genException vids)
		| DEC'Local (dec1,dec2) => let val runenv1 = D runenv dec1
								   in D (runenv'plus runenv runenv1) dec2
								   end
		| DEC'Empty => runenv'empty
		| DEC'Seq (dec1,dec2) =>
		  let val runenv1 = D runenv dec1
			  val runenv2 = D (runenv'plus runenv runenv1) dec2
		  in runenv'plus runenv1 runenv2
		  end

(*----------------------------------------------------------------------
INDEX: fix -- pat/exp/dec
----------------------------------------------------------------------*)

  local
	  fun E  x = U_exec'exp E D x
	  and D  x = U_exec'dec E D x
  in
  fun exec'dec dec = 						  
	  This (assoc'runenv (D runenv'empty dec))
	  handle (Pack exval) => That exval
  end


  val exec'dec = fn dec => 
					let 
						val () = Napps := 0
						val () = Nlooks := 0
						val res = exec'dec dec
						val () = NMLecho ("***NappsC = " ^ stringOfInt (!Napps))
						val () = NMLecho ("***NlooksC = " ^ stringOfInt (!Nlooks))
					in res 
					end

end
