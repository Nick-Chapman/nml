(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  23.    export
  102.   imports
  185.   functor
  216.   depth, slot, patStatus
  235.   Code - runtime versions of: pat,exp,dec etc
  285.   exec'* - runtime
  291.   runstack
  328.   runtime counts
  343.   pat
  460.   mrule,match
  479.   makeEvalClosure
  493.   exp
  566.   dec..
  615.   fix -- pat/exp/dec
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(* Machine 2 - stack slot numbers *)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature MachineSig =
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
	val link			: 'a list list -> 'a list
	val linkMap			: ('a -> 'b list) -> 'a list -> 'b list
	val fold			: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
	val stringOfInt		: int -> string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AssocSig ../prelude/ASSOC.ML *)
signature AssocSig_Import =
    sig
	type ('a,'b) assoc
        val assoc'list      : ('a * 'b) list -> ('a,'b) assoc
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
	val exval'Match		: exval
	val exval'Bind		: exval
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
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor MACHINE_2
			(val prefixNML : string
			 structure Prel : PrelSig_Import
			 structure Assoc : AssocSig_Import
			 structure Atom : AtomSig_Import
			 structure Value : ValueSig_Import
			 sharing type Atom.lab = Value.lab
			 sharing type Atom.vid  = Value.vid
							   ) : MachineSig =
struct

  open Prel Assoc Atom Value

  val U = (ID,VID); (* force import as datatype -- avoid eq-type prob *)
	   
  fun error s = abort ("error: MACHINE_2: "^s)

  fun mapLR E xs = (* map guaranteed to traverse L->R *)
	  let fun loop acc [] = rev acc (* this version is also tail-recursive *)
			| loop acc (x::xs) = loop (E x :: acc) xs
	  in loop [] xs
	  end

  fun NMLecho s = echo (prefixNML^s)

  exception Pack of exval

(*----------------------------------------------------------------------
INDEX: depth, slot, patStatus
----------------------------------------------------------------------*)

  datatype depth = DEPTH of int (* expected depth of stack *)
  datatype slot = SLOT of int (* offset from pushable-end of stack *)

  datatype eref = EREF'CT of exname | EREF'RT of depth * slot * vid  (* depth/vid for debug/checking *)

  datatype patVarStatus =
      PVS'BindableV of depth * vid (* depth/vid for debug/checking *)
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
	  | PAT'Var of patVarStatus
	  | PAT'Record of patrow
	  | PAT'Unit
	  | PAT'Tuple of pat list
	  | PAT'List of pat list
	  | PAT'Cons of patConStatus * pat
	  | PAT'Layered of depth * vid * pat (* depth/vid for debug/checking *)
	datatype valbind =
		VALBIND'Seq of pat * exp * valbind option
	  | VALBIND'Rec of valbind
	and dec =
		DEC'Val of valbind
	  | DEC'Exception of (depth * vid) list  (* generative *)
	  | DEC'Empty
	  | DEC'Seq of dec * dec
	and mrule = MRULE of pat * exp
	and match = MATCH of mrule list
	and exprow = EXPROW of (lab * exp) list
	and exp =
		EXP'Scon of scon
	  | EXP'Var of depth * slot * vid (* depth/vid for debug/checking *)
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
INDEX: runstack
----------------------------------------------------------------------*)

  datatype runstack = RUNSTACK of (vid*value) list
  val runstack'empty = RUNSTACK []


  fun sof'runstack (RUNSTACK xs) =
	  "[" ^ implode (linkMap (fn (vid,_) => explode (sof'vid vid ^ ", ")) xs) ^"]"

  fun runstack'push who (DEPTH depth) (RUNSTACK xs) (x as (vid,_))= 
	  (*if (length xs <> depth) 
	  then abort ("runstack'push(length xs<>depth)"^who^"-"^stringOfInt(length xs)^"<>"^stringOfInt(depth)^"-"^sof'vid vid
				  ^",statck="^sof'runstack (RUNSTACK xs))
	  else*) RUNSTACK (x::xs)

  fun nth _ [] = abort "nth:[]"
	| nth 1 (x::_) = x
	| nth n (_::xs) = nth (n-1) xs
  val nth = fn n => fn xs => if (n<1) then abort "nth:n<1" else nth n xs

  fun look'runstack who (RUNSTACK xs) (DEPTH depth,SLOT n,vid) =
	  if (length xs <> depth)
	  then abort ("look'runstack(length xs<>depth)"^who^"-"^stringOfInt(length xs)^"<>"^stringOfInt(depth))
	  else
	  if (n<1) then abort ("look'runstack(n<1):"^who) else
	  if (n<1) then abort ("look'runstack(n<1):"^who) else
	  if (n>length xs) then abort ("look'runstack(n>depth):"^who^"-"^sof'vid vid) else
	  let val (vid',v) = nth n xs
	  in if (vid<>vid') then abort ("look'runstack(vid<>vid')"^who^"-"^sof'vid vid^"<>"^sof'vid vid')
		 else v
	  end


  fun assoc'runstack (RUNSTACK xs) = assoc'list xs

(*----------------------------------------------------------------------
INDEX: runtime counts
----------------------------------------------------------------------*)

  val Napps = ref 0;
  val apply'value = fn v1 => fn v2 =>
	  (Napps := 1 + !Napps; apply'value v1 v2)

  val Nlooks = ref 0;
  val look'runstack = fn who => fn runstack => fn x =>
	  (Nlooks := 1 + !Nlooks; 
	   let (*val () = NMLecho ("look'runstack: "^sof'vid x^" - "^who)*)
	   in look'runstack who runstack x
	   end)

(*----------------------------------------------------------------------
INDEX: pat
----------------------------------------------------------------------*)

  fun exname'eref runstack eref =
	  case eref of
		  EREF'CT exname => exname
		| EREF'RT x => let val v = look'runstack ("exname'eref") runstack x
					   in excon0'value "exname'eref" v
					   end

  fun exec'patvar runstack x v =
	  case (x) of
		  PVS'BindableV (depth,vid) => Some (runstack'push "exec'patvar" depth runstack (vid,v))
		| PVS'C vid =>
		  let val (vid',vOpt) = con'value "patvar(C)" v
		  in case vOpt of
				 Some _ => None
			   | None => if (eq'vid vid vid')
						 then (Some runstack) else None
		  end
		| PVS'E eref =>
		  let val exname = exname'eref runstack eref
			  val (exname',vOpt) = excon'value "parvar(E)" v
		  in case vOpt of
				 Some _ => None
			   | None => if (eq'exname exname exname')
						 then (Some runstack) else None
		  end

  fun exec'patcons runstack P v x pat =
	  case (x) of
		  PCS'Ref => let val v' = derefAddr'value "patcons(ref)" v
						 in P runstack v' pat
						 end
		| PCS'C vid => let val (vid',vOpt) = con'value "patcons(C)" v
						   in case vOpt of
								  Some v' => if (eq'vid vid vid')
											 then (P runstack v' pat) else None
								| None => None
						   end
		| PCS'E eref => let val exname = exname'eref runstack eref
							val (exname',vOpt) = excon'value "parvar(E)" v
						in case vOpt of
							   Some v' => if (eq'exname exname exname')
										  then (P runstack v' pat) else None
							 | None => None
						end

  fun exec'patrow runstack P record (Code.PATROW (lps,{wildcard=IGNORED})) =
	  let
		fun loop runstack [] = Some runstack
		  | loop runstack ((lab,pat)::lps) =
			let val v = look'record "patrow" record lab
			in case (P runstack v pat) of
				 None => None
			   | Some runstack => loop runstack lps
			end
	  in
		loop runstack lps
	  end
		  
  fun exec'pattuple runstack P record pats =
	  let fun loop runstack n [] = Some runstack
			| loop runstack n (pat::pats) =
			  let val v = look'record "pattuple" record (LAB'Num n)
			  in case (P runstack v pat) of
					 None => None
				   | Some runstack => loop runstack (n+1) pats
			  end
	  in
		loop runstack 1 pats
	  end

  fun exec'patlist runstack P v pats =
	  case (pats,listCons'value "patlist" v) of
		  ([],None) => Some runstack
		| (pat::pats,Some(v1,v2)) =>
		  (case (P runstack v1 pat) of
			   None => None
			 | Some runstack => (case (exec'patlist runstack P v2 pats) of
								None => None
							  | Some runstack => Some runstack))
		| _ => None


  fun BUG_exec'layered runstack P v (depth,vid,pat) = (* example of bug, order deos not math compiler *)
	  case (P runstack v pat) of
		  Some runstack => Some (runstack'push "BUG_exec'layered" depth runstack (vid,v))
		| None => None

  fun exec'layered runstack P v (depth,vid,pat) =
	  let val runstack = runstack'push "exec'layered" depth runstack (vid,v)
	  in P runstack v pat
	  end

			   
  fun U_exec'pat P runstack v pat =
	  case pat of
		  PAT'Wild => Some runstack
		| PAT'Scon scon => let val scon' = scon'value "exec'pat" v
						   in if (eq'scon scon scon') then Some runstack
							  else None
						   end
		| PAT'Var x => exec'patvar runstack x v
		| PAT'Record pr => let val record = record'value "exec'pat" v
						   in exec'patrow runstack P record pr
						   end
		| PAT'Unit => let val U as () = unit'value "exec'pat" v
					  in Some runstack
					  end
		| PAT'Tuple pats => let val record = record'value "exec'pat" v
							in exec'pattuple runstack P record pats
							end
		| PAT'List pats => exec'patlist runstack P v pats
		| PAT'Cons (x,pat) => exec'patcons runstack P v x pat
		| PAT'Layered (depth,vid,pat) => exec'layered runstack P v (depth,vid,pat)


  local fun P runstack v pat = U_exec'pat P runstack v pat
  in val exec'pat = P
  end

(*----------------------------------------------------------------------
INDEX: mrule,match
----------------------------------------------------------------------*)

  fun exec'mrule E runstack value (MRULE (pat,exp)) =
	  case (exec'pat runstack value pat) of
		  None => None
		| Some runstack => Some (E runstack exp)


  fun exec'match E runstack value (MATCH xs) =
	  let fun loop [] = None
			| loop (x::xs) =
			  case (exec'mrule E runstack value x) of (* same runstack used repeatedly - i.e. state is reverted *)
				  Some res => Some res
				| None => loop xs
	  in loop xs
	  end

(*----------------------------------------------------------------------
INDEX: makeEvalClosure
----------------------------------------------------------------------*)

  fun makeEvalClosure E (match,runstack) =
	  closure'fn (fn vArg => case (exec'match E runstack vArg match) of
								 Some res => res
							   | None => raise (Pack exval'Match))

  fun makeEvalClosureRecursive E (match,runstackRef) =
	  closure'fn (fn vArg => case (exec'match E (!runstackRef) vArg match) of
								 Some res => res
							   | None => raise (Pack exval'Match))

(*----------------------------------------------------------------------
INDEX: exp
----------------------------------------------------------------------*)

  fun exec'exprow Erunstack exprow =
	  case exprow of
		  EXPROW xs => mapLR (fn (lab,exp) => (lab,Erunstack exp)) xs

  fun exec'seq Erunstack [] = abort "exec'seq"
	| exec'seq Erunstack (exp::exps) =
	  let fun loop v [] = v
			| loop _ (exp::exps) = loop (Erunstack exp) exps
	  in loop (Erunstack exp) exps
	  end


  fun value'lab lab =
	  value'closure (closure'fn (fn vArg => let val record = record'value "value'lab" vArg
											in look'record "value'lab" record lab
											end))
	  

  fun U_exec'exp E D runstack exp =
	  case exp of
		  EXP'Scon scon => value'scon scon
		| EXP'Var x => look'runstack "exec'exp:EXP'Var" runstack x
		| EXP'Record exprow => value'record (record'valrow (exec'exprow (E runstack) exprow))
		| EXP'Select lab => value'lab lab
		| EXP'Unit => value'record record'empty
		| EXP'Tuple exps => value'record (record'values (mapLR (E runstack) exps))
		| EXP'List exps => value'list (mapLR (E runstack) exps)
		| EXP'Vector exps => value'vector (Vector.fromList (mapLR (E runstack) exps))
		| EXP'Seq exps => exec'seq (E runstack) exps
		| EXP'Let (dec,exp) =>
		  let val runstack = D runstack dec
		  in E runstack exp
		  end
		(*| EXP'App (e1,e2) => apply'value (E runstack e1) (E runstack e2) (*func,arg*)*)
		| EXP'App (e1,e2) => let val argV = (E runstack e2) in apply'value (E runstack e1) argV end (*arg,func*)
		| EXP'Andalso (e1,e2) => if (bool'value "andalso" (E runstack e1))
								 then E runstack e2
								 else value'false
		| EXP'Orelse (e1,e2) => if (bool'value "orele" (E runstack e1))
								then value'true
								else E runstack e2
		| EXP'Handle (exp,match) =>
		  ((E runstack exp)
		   handle Pack exval =>
				  (case (exec'match E runstack (value'exval exval) match) of
					   None => raise (Pack exval)
					 | Some res => res))
		| EXP'Raise exp =>
		  let val exval = exval'value "exec'exp(Raise)" (E runstack exp)
		  in (*NMLecho "***raising";*) raise (Pack exval)
		  end
		| EXP'If (e1,e2,e3) => if (bool'value "if" (E runstack e1))
							   then E runstack e2
							   else E runstack e3
		| EXP'Case (exp,match) => let val vCase = E runstack exp
								  in case (exec'match E runstack vCase match) of
									   Some res => res
									 | None => raise (Pack exval'Match)
								  end
		| EXP'Fn match => value'closure (makeEvalClosure E (match,runstack))
		| EXP'Value v => v


  fun exec'expRecursive E runstackRef exp =
	  case (exp) of
		  EXP'Fn match => value'closure (makeEvalClosureRecursive E (match,runstackRef))
		| EXP'Tuple exps => value'record (record'values (mapLR (exec'expRecursive E runstackRef) exps)) (* ML extension *)
		| _ => E (!runstackRef) exp

(*----------------------------------------------------------------------
Index: dec..
----------------------------------------------------------------------*)

  fun exec'valbind_inRec E runstack runstackRef vb =
	  case vb of
		  VALBIND'Rec vb => exec'valbind_inRec E runstack runstackRef vb (* 2nd rec ignored *)
		| VALBIND'Seq (pat,exp,vbo) =>
		  case (exec'pat runstack (exec'expRecursive E runstackRef exp) pat) of
			  None => raise (Pack exval'Bind)
			| Some runstack =>
			  (case vbo of
				   None => runstack
				 | Some vb => exec'valbind_inRec E runstack runstackRef vb)

  fun exec'valbind E runstack vb =
	  case vb of
		  VALBIND'Rec vb => let val runstackRef = ref runstack
								val runstack = exec'valbind_inRec E runstack runstackRef vb
								val () = (runstackRef := runstack)
							 in runstack
							 end
		| VALBIND'Seq (pat,exp,vbo) =>
		  case (exec'pat runstack (E runstack exp) pat) of
			  None => raise (Pack exval'Bind)
			| Some runstack =>
			  (case vbo of
				   None => runstack
				 | Some vb => exec'valbind E runstack vb)


  fun exec'genException runstack (depth,vid) =
	  let val en = gen'exname vid
		  val v = value'exval (exval'con0 en)
	  in runstack'push "exec'genException" depth runstack (vid,v)
	  end

  fun U_exec'dec E D runstack dec = 
	  case dec of
		  DEC'Val vb => exec'valbind E runstack vb

		| DEC'Exception xs => fold exec'genException runstack xs
		| DEC'Empty => runstack
		| DEC'Seq (dec1,dec2) =>
		  let val runstack = D runstack dec1
			  val runstack = D runstack dec2
		  in runstack
		  end

(*----------------------------------------------------------------------
INDEX: fix -- pat/exp/dec
----------------------------------------------------------------------*)

  local
	  fun E  x = U_exec'exp E D x
	  and D  x = U_exec'dec E D x
  in
  fun exec'dec dec = 						  
	  This (assoc'runstack (D runstack'empty dec))
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
