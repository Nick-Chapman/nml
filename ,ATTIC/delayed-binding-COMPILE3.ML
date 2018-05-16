(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  29.    refs
  35.    export
  46.    imports
  245.   functor
  287.   util
  314.   compileSequence (always L->R)
  325.   Delayed tuple building
  363.   env : vid -> r_delayed 
  388.   compileSequenceE (always L->R)
  399.   compileTupleSequenceE
  408.   compilation schemes : Cps(Fv) -> Code
  432.   pat - basic pattern compilation
  484.   pat compilation - avoid force when binding vars
  501.   Pd - avoid tuple building when matching againt tuple pattern
  521.   compile pat, extending env
  531.   pat Match/Bind
  559.   PrimApp
  581.   compile_saveFrame
  598.   B
  624.   C, Aret, CM, AM
  722.   Ad, A, At, AMt, Ft
  808.   fix-up schemes
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: refs
----------------------------------------------------------------------*)

val memo_bind_lets = ref false

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature CompileSig =
sig
  type cps
  type code
  val generateCode : cps -> code
end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ../prelude/Prel.ML *)
signature PrelSig_Import =
sig
	val abort			: string -> 'a
	val fold			: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
	val stringOfInt		: int -> string
	val forall			: ('a -> bool) -> 'a list -> bool
	val enumerateFrom	: int -> 'a list -> ('a * int) list
	val upto			: int * int -> int list
	val curry			: ('a * 'b -> 'c) -> 'a -> 'b -> 'c
	val zipPair			: 'a list -> 'b list -> ('a * 'b) list
	val unzip			: ('a * 'b) list -> 'a list * 'b list
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AssocSig ../prelude/ASSOC.ML *)
signature AssocSig_Import =
sig
	type ('a,'b) assoc
	val assoc'empty		: ('a,'b) assoc
	val assoc'single	: 'a * 'b -> ('a,'b) assoc
	val assoc'plus		: ('a,'b) assoc -> ('a,'b) assoc -> ('a,'b) assoc
	val find'assoc	    : ('a,'b) assoc -> ('a -> bool) -> 'b option
end;
(*IMPORTDONE:*)

(*IMPORTSIG: MisclaySig ../prelude/MISCLAY.ML *)
signature MisclaySig_Import =
sig
  type layout
  val ++                        : layout * layout -> layout
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AtomSig ATOM.ML *)
signature AtomSig_Import =
sig
    type vid;
    type scon;
end;
(*IMPORTDONE:*)

(*IMPORTSIG: ValueSig VALUE.ML *)
signature ValueSig_Import =
sig
	type vid
	type scon
	type exname
	type value
	type exval
	type layout
	type builtin
    val arity'builtin   : builtin -> int
    datatype prim
      = PRIM'Builtin    of builtin
      | PRIM'Con0       of vid
      | PRIM'Ref
	val value'exval		: exval -> value
	val exval'Match		: exval
	val exval'Bind		: exval
end;
(*IMPORTDONE:*)

(*IMPORTSIG: EmbedSig EMBED.ML *)
signature EmbedSig_Import =
sig
  type exname
  type value
  type prim
  type vid
  type scon
  type uid
  val eq'uid		: uid -> uid -> bool
  val sof'uid		: uid -> string
  datatype pat 
	= PAT'Wild
    | PAT'V         of uid
    | PAT'Layered   of uid * pat
    | PAT'Ref       of pat
    | PAT'C         of vid * pat option 
    | PAT'E         of exname * pat option
    | PAT'G         of uid * pat option
    | PAT'Scon      of scon
    | PAT'Tuple     of pat list
    type func;
    type match;
    type dec;
  type env
end;
(*IMPORTDONE:*)

(*IMPORTSIG: CpsSig CPS.ML *)
signature CpsSig_Import =
sig
  type value
  type prim
  type vid
  type uid
  type scon
  type pat
  datatype cps 
    = CPS'Return    of atom
    | CPS'Raise     of atom
	| CPS'Let       of bind * cps
	| CPS'Case      of atom * cmatch
	| CPS'App       of atom * atom list
    | CPS'Handle    of cps * {save:uid list} * cmatch
	| CPS'LetC      of {save:uid list} * pat * cps * cps
  and atom
    = ATOM'Value    of value
    | ATOM'Var      of uid
    | ATOM'NewEx	of vid
    | ATOM'Tuple    of atom list
	| ATOM'PrimApp	of prim * atom
    | ATOM'Fn       of afunc
	| ATOM'Let      of bind * atom
    | ATOM'Case     of atom * amatch
  and bind
	= BIND'Val		of pat * atom
	| BIND'ValRec	of (uid * afunc) list
  and afunc = AFUNC'Def	of {save:uid list} * uid list * cps
  and cmatch
	= CMATCH'Alt    of cmatch * cmatch 
	| CMATCH'Rule   of pat * cps
    | CMATCH'Atom	of amatch
  and amatch
	= AMATCH'Alt    of amatch * amatch 
	| AMATCH'Rule   of pat * atom
  type layout
end;
(*IMPORTDONE:*)

(*IMPORTSIG: MachineSig MACHINE.ML *)
signature MachineSig_Import =
sig
  type value
  type builtin
  type exname
  type vid
  type uid
  type scon
  type pat
  type atom
  type cps
  type cmatch
  type afunc
  datatype info
	= INFO'Uid      of uid
	| INFO'Pat      of pat
	| INFO'Atom     of atom
	| INFO'String	of string
  datatype q = SLOC of int * info
  datatype r
    = LOC'Stack             of q
    | LOC'Frame             of int * info
    | LOC'Arg				of int * info
    | LOC'Cret
    | LOC'Xret
	| LOC'Value				of value
  datatype code
	= CODE'Seq              of seq * code
    | CODE'FatBar           of code * code
    | CODE'Return           of r
    | CODE'TailCall			of r * r list
	| CODE'Raise			of r
  and seq =
	  SEQ'Empty
	| SEQ'Seq				of seq * seq
	| SEQ'FatBar			of seq * seq
	| SEQ'Raise				of r
	| SEQ'I					of instruction
  and instruction
    = INS'NewTuple          of q * {tuple_size:int}
    | INS'NewException      of q * vid
    | INS'NewFn             of q * {frame_size:int, num_args: int} * code * {name: string, info: afunc}
    | INS'NewRef            of q * r
    | INS'NewCon            of q * vid * r
    | INS'Copy              of q * r
    | INS'DeCon             of q * r
    | INS'DeExcon           of q * r
    | INS'DeRef             of q * r
    | INS'DeTuple			of q * r * int
    | INS'BuiltinApp        of q * builtin * r list
    | INS'PushContinuation  of {frame_size:int} * code * {info: cps * uid list}
    | INS'PushHandler       of {frame_size:int} * code * {info: cmatch * uid list}
	| INS'SetTupleElement	of r * int * r
    | INS'SetFrameElement   of r * int * r
    | INS'SetContFrameElem  of int * r
    | INS'SetXcontFrameElem of int * r
    | INS'MatchScon         of r * scon
    | INS'MatchC            of r * vid
    | INS'MatchE            of r * exname
    | INS'MatchG            of r * r
  type layout
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor COMPILE3
		(structure Prel : PrelSig_Import

		 val right_to_left_tuples : bool ref
		 val right_to_left_args : bool ref
		 val delay_var_bind : bool ref
		 val delay_select : bool ref

		 structure Assoc : AssocSig_Import
		 structure Misclay : MisclaySig_Import
		 structure Atom : AtomSig_Import
		 structure Value : ValueSig_Import
		 structure Embed : EmbedSig_Import
		 structure Cps : CpsSig_Import
		 structure Machine : MachineSig_Import
		 sharing type Atom.scon = Value.scon = Embed.scon = Cps.scon = Machine.scon
		 sharing type Atom.vid = Embed.vid = Cps.vid = Value.vid = Machine.vid
		 sharing type Embed.uid = Cps.uid = Machine.uid
		 sharing type Embed.pat = Cps.pat = Machine.pat
		 sharing type Value.value = Embed.value = Cps.value = Machine.value
		 sharing type Value.builtin = Machine.builtin
		 sharing type Value.prim = Embed.prim = Cps.prim
		 sharing type Value.exname = Embed.exname = Machine.exname
		 sharing type Cps.atom = Machine.atom
		 sharing type Cps.cps = Machine.cps
		 sharing type Cps.cmatch = Machine.cmatch
		 sharing type Cps.afunc = Machine.afunc
		 sharing type Cps.layout = Misclay.layout
		) : CompileSig =
struct

  open Prel Assoc Atom Value Embed Cps Machine Misclay

  (* force import as datatype -- avoid eq-type prob *)
  (*val U = (ID,VID)*)

  val value'Bind = value'exval (exval'Bind)
  val value'Match = value'exval (exval'Match)

(*----------------------------------------------------------------------
INDEX: util
----------------------------------------------------------------------*)

  local val uref = ref 0
  in fun newQ info = let val u = !uref in (uref:=1+u; SLOC (u,info)) end
  end

  infix $ $$;

  val op $ =
	  fn (seq,SEQ'Empty) => seq
	   | (SEQ'Empty,seq) => seq
	   | p => SEQ'Seq p

  fun op $$ (seq,code) =
	  case seq
	   of SEQ'Empty => code
		| _ => CODE'Seq (seq,code)

  val seq'concat = fold (curry op $) SEQ'Empty
  val seq'Is = seq'concat o map SEQ'I

  fun seq'tuple (q,rs) = if length rs < 2 then abort "seq'tuple" else
	  SEQ'I (INS'NewTuple (q, {tuple_size = length rs}))
	  $ seq'Is (map (fn (r,n) => INS'SetTupleElement (LOC'Stack q,n,r)) (enumerateFrom 0 rs))

(*----------------------------------------------------------------------
INDEX: compileSequence (always L->R)
----------------------------------------------------------------------*)

  fun compileSequence (X: ('a -> seq * 'b)) : ('a list -> seq * 'b list) =
	  fn atoms =>
		 case (fold (fn (seq1,rs) => fn atom => let val (seq2,r) = X atom
												in (seq1 $ seq2, r::rs)
												end) (SEQ'Empty,[]) atoms)
		  of (seq,rs) => (seq,rev rs)

(*----------------------------------------------------------------------
INDEX: Delayed tuple building
----------------------------------------------------------------------*)

  datatype r_delayed
	= DELAYED'R			of r
	| DELAYED'Tuple		of r_delayed list * {info:atom}
	| DELAYED'DeTuple	of r_delayed * int * {info:pat}
		   
  val rec forceDelayed : r_delayed -> (seq * r) =
	   fn DELAYED'R r => (SEQ'Empty,r)
		| DELAYED'Tuple (rds,
						 {info=atom}) => let val (seq,rs) = compileSequence forceDelayed rds
											 val q = newQ (INFO'Atom atom)
										 in (seq $ seq'tuple (q,rs), LOC'Stack q)
										 end
		| DELAYED'DeTuple (rd,n,
						   {info=pat}) => let val q = newQ (INFO'Pat pat)
											  val (seq,r) = forceDelayed rd
										  in (seq $ SEQ'I (INS'DeTuple (q,r,n)), LOC'Stack q)
										  end

  val forceDelayedN : (int * r_delayed) -> (seq * r list) =
	  fn (N,rd) =>
	  case rd 
	   of DELAYED'Tuple (rds,_) => if (length rds <> N) then abort "forceDelayedN" else
									 let val (seqs,rs) = unzip (map forceDelayed rds)
									 in (seq'concat seqs,rs)
									 end
		| _ =>
		  let val (seq,r) = forceDelayed rd
			  val (seqs,rs) = unzip (map (fn n => let val q = newQ (INFO'String ("BuiltinApp-Arg-"^stringOfInt n))
													  val seq = SEQ'I (INS'DeTuple (q,r,n))
												  in (seq,LOC'Stack q)
												  end) (upto (0,N-1)))
		  in (seq $ seq'concat seqs,rs)
		  end

(*----------------------------------------------------------------------
INDEX: env : vid -> r_delayed 
----------------------------------------------------------------------*)

  local open Assoc Atom
  in datatype env = ENV of (uid,r_delayed) assoc
	 val env'empty = ENV assoc'empty
	 fun env'plus (ENV a1, ENV a2) = ENV (assoc'plus a1 a2)
	 val env'plusList = fold (curry env'plus) env'empty
	 (*fun del_look'env who (ENV a) k = look'assoc ("look'env("^who^")") sof'uid a k*)
	 fun del_look'env who (ENV a) k = case (find'assoc a (eq'uid k)) of Some v => v
																	  | None => abort("del_look'env("^who^")"^sof'uid k)
	 fun del_env'single kv = ENV (assoc'single kv)
  end

  infix ++
  val op ++ = env'plus

  fun look'env who : (env * uid) -> (env * seq * r) =
	  fn (env,uid) => let val rd = del_look'env who env uid
						  val (seq,r) = forceDelayed rd
						  val env = env ++ del_env'single (uid,DELAYED'R r)
					  in (env,seq,r)
					  end

  fun env'single (x,r) = ENV (assoc'single (x,DELAYED'R r))

(*----------------------------------------------------------------------
INDEX: compileSequenceE (always L->R)
----------------------------------------------------------------------*)

  fun compileSequenceE (X: (env * 'a -> env * seq * 'b)) : (env * 'a list -> env * seq * 'b list) =
	  fn (env,atoms) =>
		 case (fold (fn (env,seq1,rs) => fn atom => let val (env,seq2,r) = X (env,atom)
													in (env,seq1 $ seq2, r::rs)
													end) (env,SEQ'Empty,[]) atoms)
		  of (env,seq,rs) => (env,seq,rev rs)

(*----------------------------------------------------------------------
INDEX: compileTupleSequenceE
----------------------------------------------------------------------*)

  fun compileTupleSequenceE X (env,atoms) =
	  if (!right_to_left_tuples)
	  then case (compileSequenceE X (env,rev atoms)) of (env,seq,rs) => (env,seq, rev rs)
	  else compileSequenceE X (env,atoms)

(*----------------------------------------------------------------------
INDEX: compilation schemes : Cps(Fv) -> Code
----------------------------------------------------------------------*)

  type C        = (env * cps                        -> code)
  type Aret     = (env * atom                       -> code)
  type CM       = (env * cmatch * r_delayed         -> code)
  type AM       = (env * amatch * r_delayed         -> code)

  type Ad       = (env * atom                       -> env * seq * r_delayed)
  type A        = (env * atom                       -> env * seq * r)
  type At       = (env * atom * q                   -> env * seq)

  type AMt      = (env * amatch * r_delayed * q     -> seq)
  type Ft       = (env * {name:string} * afunc * q  -> env * seq * seq)

  type P        = (env * pat * r					-> seq * env)		(* env has just new bindings *)
  type Pd       = (env * pat * r_delayed			-> seq * env)		(* env has just new bindings *)

  type Pdx      = (env * pat * r_delayed            -> env * seq)       (* env extended *)
  type B        = (env * bind                       -> env * seq)       (* env extended *)

  type schemes = {Pdx:Pdx,B:B,C:C,Aret:Aret,Ad:Ad,A:A,At:At,Ft:Ft,CM:CM,AM:AM,AMt:AMt}

(*----------------------------------------------------------------------
INDEX: pat - basic pattern compilation
----------------------------------------------------------------------*)

  fun U_compile'pat {P:P,Pd:Pd} : (env * pat * r -> seq * env) =
	  fn (env,pat,r) =>
		 case pat
		  of PAT'Wild				=> (SEQ'Empty, env'empty)
		   | PAT'V x                => (SEQ'Empty, env'single (x,r))
		   | PAT'Layered (x,pat)    => let val (seq,env) = P (env,pat,r)
									   in (seq, env'single (x,r) ++ env)
									   end
		   | PAT'Ref pat            => let val q = newQ (INFO'Pat pat)
										   val (seq,env) = P (env,pat,LOC'Stack q)
									   in (SEQ'I (INS'DeRef (q,r)) $ seq, env)
									   end
		   | PAT'C (x,None)         => (SEQ'I (INS'MatchC (r,x)), env'empty)
		   | PAT'C (x,Some pat)     => let val q = newQ (INFO'Pat pat)
										   val (seq,env) = P (env,pat,LOC'Stack q)
									   in (seq'Is [INS'MatchC (r,x), INS'DeCon (q,r)] $ seq, env)
									   end
		   | PAT'E (exname,None)    => (SEQ'I (INS'MatchE (r,exname)), env'empty)
		   | PAT'E (exname,Some pat)=> let val q = newQ (INFO'Pat pat)
										   val (seq,env) = P (env,pat,LOC'Stack q)
									   in (seq'Is [INS'MatchE (r,exname), INS'DeExcon (q,r)] $ seq, env)
									   end
           | PAT'G (x,None)         => let val (env_DONT_IGNORE,seqf,r2) = look'env "compile'pat" (env,x)
									   in (seqf $ SEQ'I (INS'MatchG (r,r2)), env'empty)
									   end
           | PAT'G (x,Some pat)     => let val (env_DONT_IGNORE,seqf,r2) = look'env "compile'pat" (env,x)
										   val q = newQ (INFO'Pat pat)
										   val (seq,env) = P (env,pat,LOC'Stack q)
									   in (seqf $ seq'Is [INS'MatchG (r,r2), INS'DeExcon (q,r)] $ seq, env)
									   end
           | PAT'Scon scon          => (SEQ'I (INS'MatchScon (r,scon)), env'empty)
           | PAT'Tuple pats => 
			 fold (fn (seqAcc,envAcc) => 
					  fn (pat,n) => 
						 let val (seq,env) = if (!delay_select)
											 then let val rd = DELAYED'DeTuple (DELAYED'R r,n,{info=pat})
												  in Pd (env,pat,rd)
												  end
											 else let val q = newQ (INFO'Pat pat)
													  val seq1 = SEQ'I (INS'DeTuple (q,r,n))
													  val (seq2,env) = P (env,pat,LOC'Stack q)
												  in (seq1 $ seq2, env)
												  end
						 in (seqAcc $ seq, envAcc ++ env)
						 end) (SEQ'Empty,env'empty) (enumerateFrom 0 pats)

  fun compile'pat {Pd:Pd} x = U_compile'pat {P=compile'pat {Pd=Pd},Pd=Pd} x

(*----------------------------------------------------------------------
INDEX: pat compilation - avoid force when binding vars
----------------------------------------------------------------------*)

  fun DelayVarBind_compile'pat {Pd:Pd} : (env * pat * r_delayed -> seq * env) =
	  fn (env,pat,rd) =>
		 case pat
		  of PAT'Wild				=> (SEQ'Empty, env'empty)
		   | PAT'V x                => (SEQ'Empty, del_env'single (x,rd))
		   | PAT'Layered (x,pat)    => let val (seq,env) = DelayVarBind_compile'pat {Pd=Pd} (env,pat,rd)
									   in (seq, del_env'single (x,rd) ++ env)
									   end
		   | _ => let val (seq1,r) = forceDelayed rd
					  val (seq2,env) = compile'pat {Pd=Pd} (env,pat,r)
				  in (seq1 $ seq2,env)
				  end

(*----------------------------------------------------------------------
INDEX: Pd - avoid tuple building when matching againt tuple pattern
----------------------------------------------------------------------*)

  val rec compile_Pd : (env * pat * r_delayed -> seq * env) = (* returned env just has new bindings *)
	  fn (env,pat,rd) =>
		 case (pat,rd)
		  of (PAT'Tuple pats,DELAYED'Tuple (rds,_)) =>  
			 let val () = if (length pats = length rds) then () else abort "Pd/tuple/size"
			 in case (unzip (map (fn (pat,rd) => compile_Pd (env,pat,rd)) (zipPair pats rds))) of
					(seqs,envs) => (seq'concat seqs, env'plusList envs)
			 end
		   | _ => 
			 if (!delay_var_bind) 
			 then DelayVarBind_compile'pat {Pd=compile_Pd} (env,pat,rd)
			 else 
				 case pat
				  of PAT'Wild => (SEQ'Empty, env'empty)
				   | _ => let val (seq1,r) = forceDelayed rd
							  val (seq2,env) = compile'pat {Pd=compile_Pd} (env,pat,r)
						  in (seq1 $ seq2,env)
						  end

(*----------------------------------------------------------------------
INDEX: compile pat, extending env
----------------------------------------------------------------------*)

  val compile'Pdx : (env * pat * r_delayed -> env * seq) = (* extends env *)
	  fn (env,pat,rd) =>
		 let val (seq,env1) = compile_Pd (env,pat,rd)
		 in (env ++ env1, seq)
		 end

(*----------------------------------------------------------------------
INDEX: pat Match/Bind
----------------------------------------------------------------------*)

  fun exhaustive'pat pat =
	  case pat
	   of PAT'Wild              => true
		| PAT'V x               => true
		| PAT'Layered (x,pat)   => exhaustive'pat pat
		| PAT'Ref pat           => (*true - bug, never witnessed!*) exhaustive'pat pat
		| PAT'Tuple pats        => forall exhaustive'pat pats
		| _						=> false

  fun simple_exhaustive'amatch amatch =
	  case amatch
	   of AMATCH'Rule (pat,_) => exhaustive'pat pat
		| _ => false

  fun simple_exhaustive'cmatch cmatch =
	  case cmatch
	   of CMATCH'Rule (pat,_) => exhaustive'pat pat
		| _ => false

  val seq'Match = SEQ'Raise (LOC'Value value'Match)
  val code'Match = CODE'Raise (LOC'Value value'Match)

  val seq'Bind = SEQ'Raise (LOC'Value value'Bind)

(*----------------------------------------------------------------------
INDEX: PrimApp
----------------------------------------------------------------------*)

  fun compile_primApp {A:A,Ad:Ad} : (env * prim * atom * q -> env * seq) =
	  fn (env,prim,atom,q) =>
		 case (prim)
		  of PRIM'Builtin bi   => if (arity'builtin bi = 1) 
								  then let val (env,seq,r) = A (env,atom)
									   in (env,seq $ SEQ'I (INS'BuiltinApp (q,bi,[r])))
									   end
								  else let val (env,seq1,rd) = Ad (env,atom)
										   val (seq2,rs) = forceDelayedN (arity'builtin bi, rd)
									   in (env,seq1 $ seq2 $ SEQ'I (INS'BuiltinApp (q,bi,rs)))
									   end
		   | PRIM'Con0 c		=> let val (env,seq,r) = A (env,atom)
								   in (env,seq $ SEQ'I (INS'NewCon (q,c,r)))
								   end
		   | PRIM'Ref			=> let val (env,seq,r) = A (env,atom)
								   in (env,seq $ SEQ'I (INS'NewRef (q,r)))
								   end

(*----------------------------------------------------------------------
INDEX: compile_saveFrame
----------------------------------------------------------------------*)

  fun compile_saveFrame (SetFrameInstr: int * r -> instruction) : (env * uid list -> env * seq * env) =
	  fn (env,save) =>
		 let val save_slots = enumerateFrom 0 save
			 val envNew = env'plusList (map (fn (x,n) => env'single (x,LOC'Frame (n,INFO'Uid x))) save_slots)
			 val (env,fill) =
				 fold (fn (env,seqAcc) => 
						  fn (x,n) => 
							 let val (env,seq,r) = look'env "compile'cdec" (env,x)
							 in (env,seqAcc $ seq $ SEQ'I (SetFrameInstr (n,r)))
							 end) (env,SEQ'Empty) save_slots
		 in (env,fill,envNew)
		 end

(*----------------------------------------------------------------------
INDEX: B
----------------------------------------------------------------------*)

  fun compile_LetRecBinds {Ft} : (env * (uid * afunc) list -> env * seq) =
	  fn (env,binds) =>
		 let val trips = map (fn (x,f) => (x,f,newQ (INFO'Uid x))) binds
			 val env = env ++ env'plusList (map (fn (x,_,q) => env'single (x,LOC'Stack q)) trips)
			 val (env,seqA,seqB) = 
				 fold (fn (env,seqA1,seqB1) => 
						  fn (x,f,q) => let val (env,seqA2,seqB2) = Ft (env,{name=sof'uid x},f,q)
										in (env,seqA1 $ seqA2, seqB1 $ seqB2)
										end) (env,SEQ'Empty,SEQ'Empty) trips
		 in (env, seqA $ seqB)
		 end

  fun compile'B ({A,Ad,B,C,Aret,CM,Pdx,At,Ft,AM,AMt}:schemes) : (env * bind -> env * seq) =
	  fn (env,bind) =>
		 case bind of
			 BIND'Val (pat,atom) => let val (env,seq1,rd) = Ad (env,atom) 
										val (env,seqPat) = Pdx (env,pat,rd)
										val seqPat = if (exhaustive'pat pat) then seqPat else SEQ'FatBar (seqPat, seq'Bind)
									in  (env, seq1 $ seqPat)
									end
		   | BIND'ValRec binds => compile_LetRecBinds {Ft=Ft} (env,binds)

(*----------------------------------------------------------------------
INDEX: C, Aret, CM, AM
----------------------------------------------------------------------*)

  fun compile'C ({A,Ad,B,C,Aret,CM,Pdx,At,Ft,AM,AMt}:schemes) : (env * cps -> code) =
	  fn (env,cps) => 
		 case cps
		  of CPS'Return atom				=> Aret (env,atom)
		   | CPS'Raise atom					=> let val (env,seq,r) = A (env,atom)
											   in seq $$ CODE'Raise r
											   end
		   | CPS'App (func,args)			=> let val (seq,r,rs) =
													   if (!right_to_left_args)
													   then let val (env,seq1,rs) = compileSequenceE A (env, rev args)
																val (env,seq2,r) = A (env,func)
															in (seq1 $ seq2, r, rev rs)
															end
													   else let val (env,seq2,r) = A (env,func)
																val (env,seq1,rs) = compileSequenceE A (env, args)
															in (seq1 $ seq2, r, rs)
															end
											   in seq $$ CODE'TailCall (r,rs)
											   end
		   | CPS'Let (bind,cps)				=> let val (env,seq1) = B (env,bind)
												   val code2 = C (env,cps)
											   in seq1 $$ code2
											   end
		   | CPS'LetC ({save},pat,cps1,cps2) =>
			 let
				 val (env,fill,envNew) = compile_saveFrame INS'SetContFrameElem (env,save) 
				 val code1 = C (env,cps1)
				 val (envNew,seqPat) = Pdx (envNew,pat,DELAYED'R LOC'Cret)
				 val seqPat = if (exhaustive'pat pat) then seqPat else SEQ'FatBar (seqPat, seq'Bind)
				 val code2 = C (envNew,cps2)
				 val push = SEQ'I (INS'PushContinuation ({frame_size=length save},
														 seqPat $$ code2,
														 {info=(cps1,save)}))
			 in push $ fill $$ code1
			 end
           | CPS'Handle (cps,{save},cmatch) =>
			 let
				 val (env,fill,envNew) = compile_saveFrame INS'SetXcontFrameElem (env,save) 
				 val code = C (env,cps)
				 val codeHandle = CM (envNew,cmatch,DELAYED'R (LOC'Xret))
				 val push = SEQ'I (INS'PushHandler ({frame_size = length save},
													CODE'FatBar (codeHandle, CODE'Raise LOC'Xret),
													{info=(cmatch,save)}))
			 in push $ fill $$ code
			 end
		   | CPS'Case (atom,cmatch)			=> let val (env,seq1,rd) = Ad (env,atom)
												   val code2 = CM (env,cmatch,rd)
												   val code2 = if (simple_exhaustive'cmatch cmatch) then code2
															   else CODE'FatBar (code2, code'Match)
											   in seq1 $$ code2
											   end

  fun compile'Aret ({Ad,A,At,Pdx,B,C,Aret,Ft,CM,AM,AMt}:schemes) : (env * atom -> code) =
	  fn (env,atom) =>
		 case atom 
          of ATOM'Case (atom,amatch)	=> let val (env,seq1,rd) = Ad (env,atom)
											   val code2 = AM (env,amatch,rd)
											   val code2 = if (simple_exhaustive'amatch amatch) then code2
														   else CODE'FatBar (code2, code'Match)
										   in seq1 $$ code2
										   end
		   | ATOM'Let (bind,atom)		=> let val (env,seq1) = B (env,bind)
											   val code2 = Aret (env,atom)
										   in seq1 $$ code2
										   end
		   | _ => let val (env,seq,r) = A (env,atom)
				  in seq $$ CODE'Return r
				  end
	  
  fun compile'CM ({Pdx,B,C,Aret,Ad,A,At,Ft,CM,AM,AMt}:schemes) : (env * cmatch * r_delayed -> code) =
	  fn (env,cmatch,rd) =>
		 case cmatch 
          of CMATCH'Alt (cm1,cm2)	=> let val code1 = CM (env,cm1,rd)
										   val code2 = CM (env,cm2,rd)
									   in CODE'FatBar(code1,code2)
									   end
		   | CMATCH'Rule (pat,cps)	=> let val (env,seq1) = Pdx (env,pat,rd)
										   val code2 = C (env,cps)
									   in seq1 $$ code2
									   end
		   | CMATCH'Atom am			=> AM (env,am,rd)

  fun compile'AM ({Pdx,B,C,Aret,Ad,A,At,Ft,CM,AM,AMt}:schemes) : (env * amatch * r_delayed  -> code) =
	  fn (env,amatch,rd) =>
		 case amatch 
          of AMATCH'Alt (am1,am2)	=> let val code1 = AM (env,am1,rd)
										   val code2 = AM (env,am2,rd)
									   in CODE'FatBar(code1,code2)
									   end
		   | AMATCH'Rule (pat,atom)	=> let val (env,seq1) = Pdx (env,pat,rd)
										   val code2 = Aret (env,atom)
									   in seq1 $$ code2
									   end

(*----------------------------------------------------------------------
INDEX: Ad, A, At, AMt, Ft
----------------------------------------------------------------------*)

  fun compile'Ad ({Ad,A,At,Pdx,B,C,Aret,Ft,CM,AM,AMt}:schemes) : (env * atom -> env * seq * r_delayed) =
	  fn (env,atom) =>
		 case atom of
			 ATOM'Tuple atoms		=> let val (env,seq,rs) = compileTupleSequenceE Ad (env,atoms)
									   in (env,seq,DELAYED'Tuple (rs,{info=atom}))
									   end
		   | ATOM'Let (bind,atom)	=> let val env0 = env
										   val (env,seq1) = B (env,bind)
										   val (env,seq2,rd) = Ad (env,atom)
										   val env = if (!memo_bind_lets) then env else env0
									   in (env,seq1 $ seq2, rd)
									   end
		   | _ => let val (env,seq,r) = A (env,atom)
				  in (env,seq,DELAYED'R r)
				  end
									  
  fun compile'A ({Ad,A,At,Pdx,B,C,Aret,Ft,CM,AM,AMt}:schemes) : (env * atom -> env * seq * r) =
	  fn (env,atom) =>
		 case atom 
          of ATOM'Value v           => (env, SEQ'Empty, LOC'Value v) 
           | ATOM'Var x             => look'env "compile_A" (env,x)
		   | ATOM'Let (bind,atom)	=> let val env0 = env
										   val (env,seq1) = B (env,bind)
										   val (env,seq2,r) = A (env,atom)
										   val env = if (!memo_bind_lets) then env else env0
									   in (env, seq1 $ seq2, r)
									   end
		   | _ => let val q = newQ (INFO'Atom atom)
					  val (env,seq) = At (env,atom,q)
				  in (env, seq, LOC'Stack q) 
				  end

  fun compile'At ({Ft,Ad,A,At,AMt,Pdx,B,C,Aret,CM,AM}:schemes) : (env * atom * q -> env * seq) =
	  fn (env,TOP as atom,q) =>
		 case atom 
          of ATOM'Value v               => (env, SEQ'I (INS'Copy (q,LOC'Value v)))
		   | ATOM'NewEx x				=> (env, SEQ'I (INS'NewException (q,x)))
           | ATOM'Var x                 => let val (env,seq,r) = look'env "compile_At" (env,x)
										   in (env, seq $ SEQ'I (INS'Copy (q,r)))
										   end
           | ATOM'Tuple atoms           => let val (env,seq,rs) = compileTupleSequenceE A (env,atoms)
										   in (env,seq $ seq'tuple (q,rs))
										   end
		   | ATOM'PrimApp (prim,atom)	=> compile_primApp {A=A,Ad=Ad} (env,prim,atom,q)
		   | ATOM'Fn func				=> let val (env,seqA,seqB) = Ft (env,{name="<anon>"},func,q)
										   in (env, seqA $ seqB)
										   end
		   | ATOM'Case (atom,amatch)	=> let val (env,seq1,rd) = Ad (env,atom)
											   val seq2 = AMt (env,amatch,rd,q)
											   val seq2 = if (simple_exhaustive'amatch amatch) then seq2
														   else SEQ'FatBar (seq2, seq'Match)
										   in (env, seq1 $ seq2)
										   end
		   | ATOM'Let (bind,atom)		=> let val env0 = env
											   val (env,seq1) = B (env,bind)
											   val (env,seq2) = At (env,atom,q)
											   val env = if (!memo_bind_lets) then env else env0
										   in (env, seq1 $ seq2)
										   end

  fun compile'AMt ({Pdx,B,C,Aret,Ad,A,At,Ft,CM,AM,AMt}:schemes) : (env * amatch * r_delayed * q -> seq) =
	  fn (env,amatch,rd,q) =>
		 case amatch 
          of AMATCH'Alt (amatch1,amatch2) => let val seq1 = AMt (env,amatch1,rd,q)
                                                 val seq2 = AMt (env,amatch2,rd,q)
											 in SEQ'FatBar(seq1,seq2)
											 end
		   | AMATCH'Rule (pat,atom)	=> let val (env,seq1) = Pdx (env,pat,rd)
										   val (env,seq2) = At (env,atom,q)
									   in seq1 $ seq2
									   end

  fun compile'Ft ({CM,Pdx,B,C,Aret,Ad,A,At,Ft,AM,AMt}:schemes) : (env * {name:string} * afunc * q -> env * seq * seq) =
	  fn (env,{name},afunc as AFUNC'Def({save},args,cps),q) => 
		 let fun SetFrameInstr (n,r) = INS'SetFrameElement (LOC'Stack q, n, r)
			 val (env,seqB,envFrame) = compile_saveFrame SetFrameInstr (env,save)
			 val envArgs = env'plusList (map (fn (x,n) => env'single (x,LOC'Arg (n, INFO'Uid x))) (enumerateFrom 0 args))
			 val envNew = envFrame ++ envArgs
			 val code = C (envNew,cps)
			 val sizes = {frame_size = length save, num_args = length args}
			 val info = {name=name, info=afunc}
			 val seqA = SEQ'I (INS'NewFn (q,sizes,code,info))
		 in (env,seqA,seqB)
		 end

(*----------------------------------------------------------------------
INDEX: fix-up schemes
----------------------------------------------------------------------*)

  local
      fun Pdx x = compile'Pdx x
	  fun makeSchemes () = {Pdx=Pdx,B=B,C=C,CM=CM,Ad=Ad,Aret=Aret,A=A,At=At,Ft=Ft,AM=AM,AMt=AMt}
      and Ad    x = compile'Ad      (makeSchemes()) x
      and Aret  x = compile'Aret    (makeSchemes()) x
      and A     x = compile'A       (makeSchemes()) x
      and At    x = compile'At      (makeSchemes()) x
      and Ft    x = compile'Ft      (makeSchemes()) x
      and B     x = compile'B       (makeSchemes()) x
      and C     x = compile'C       (makeSchemes()) x
      and CM    x = compile'CM      (makeSchemes()) x
      and AM    x = compile'AM      (makeSchemes()) x
      and AMt   x = compile'AMt     (makeSchemes()) x
  in val schemes = makeSchemes()
  end

  fun generateCode cps =
	  let val {Pdx,B,C,Ad,Aret,A,At,Ft,CM,AM,AMt}:schemes = schemes
	  in C (env'empty,cps)
	  end

end (* functor *)
