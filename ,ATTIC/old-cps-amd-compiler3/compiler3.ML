
open Value

val value'Bind = value'exval (exval'Bind)
val value'Match = value'exval (exval'Match)


(*----------------------------------------------------------------------
INDEX: CPS_FV
----------------------------------------------------------------------*)

structure CPS_FV =
struct

  datatype scon = datatype Atom.scon
  datatype vid = datatype Atom.vid
  datatype pat = datatype Embed.pat

  datatype fvs = FVS of vid list

  datatype cps 
    = CPS'Let       of cdec * fvs * cps
	| CPS'Case      of atom * cmatch
    | CPS'Handle    of cps * fvs * cmatch
	| CPS'App       of atom * atom
    | CPS'Atom      of atom

  and atom
    = ATOM'Value    of value
    | ATOM'Var      of vid
    | ATOM'Tuple    of atom list
    | ATOM'Let      of adec * atom
    | ATOM'Case     of atom * amatch
    | ATOM'Raise    of atom
    | ATOM'Fn       of func

  and func = FUNC	of fvs * cmatch

  and cmatch
	= CMATCH'Alt    of cmatch * cmatch 
	| CMATCH'Rule   of pat * cps

  and amatch
	= AMATCH'Alt    of amatch * amatch 
	| AMATCH'Rule   of pat * atom

  and cdec
	= CDEC'Empty
    | CDEC'Seq      of cdec * fvs * cdec
    | CDEC'Local    of cdec * fvs * cdec
    | CDEC'Val      of fvs * pat * cps (* pat may contain runtime generated excons *)
    | CDEC'Atom     of adec

  and adec
	= ADEC'Empty
    | ADEC'Seq      of adec * adec
    | ADEC'Local    of adec * adec
    | ADEC'Val      of pat * atom
    | ADEC'Valrec   of (vid * func) list
    | ADEC'ExGen	of vid
    | ADEC'ExCopy   of vid * vid

end

(*----------------------------------------------------------------------
INDEX: (Flat) Code
----------------------------------------------------------------------*)

structure Code =
struct

  datatype scon = datatype Atom.scon
  datatype vid = datatype Atom.vid
  datatype atom = datatype CPS_FV.atom

  datatype info = INFO'Atom of atom | INFO'Vid of vid | INFO'Pat of Embed.pat

  datatype stack_location = SLOC of int * info
  datatype frame_location = FLOC of int * info

  datatype location (* of values at runtime *)
	= LOC'Stack of stack_location
	| LOC'Frame of frame_location
	| LOC'FuncArg (* just one arg at the moment *)
	| LOC'Cret
	| LOC'Xret
	| LOC'Value of value (* value embedded in code *)
	| LOC'Null (* used when compiling raise-expressions - will/must never be inspected at runtime *)

  type q = stack_location (* q for lhs - "l" looks too much like a 1, and idents like l1 are hard to read *)
  type r = location (* r for rhs *)

  datatype code (* terminated code sequence *)
	= CODE'Seq              of seq * code
    | CODE'FatBar           of code * code
    | CODE'Return           of r
    | CODE'TailCall         of r * r (* one arg *)
	| CODE'Raise			of r

  and seq = (* UN-terminated code sequence *)
	  SEQ'Empty
	| SEQ'Seq				of seq * seq
	| SEQ'FatBar			of seq * seq
	| SEQ'Raise				of r
	| SEQ'I					of instruction

  and instruction
(* allocated new object & assign to a location *)
    = INS'NewTuple          of q * {tuple_size:int}
    | INS'NewFn             of q * {frame_size:int} * code
    | INS'NewException      of q * vid
(* copy/unpack existing object & assign to a location *)
    | INS'Copy              of q * r
    | INS'DeCon             of q * r
    | INS'DeExcon           of q * r
    | INS'DeRef             of q * r
    | INS'DeTuple			of q * r * int
(* allocate new control/exception continuation & push on relevant stack *)
    | INS'PushContinuation  of {frame_size:int} * code
    | INS'PushHandle	    of {frame_size:int} * code
(* pop unexecuted exception handler *)
    | INS'PopHandle
(* set tuple/frame element in existing object *)
	| INS'SetTupleElement	of r * int * r
    | INS'SetFrameElement   of r * int * r
    | INS'SetContFrameElem  of int * r
    | INS'SetXcontFrameElem of int * r

(* match existing object, if match: continue to next instruction, if not: FAIL to next FatBar *)
    | INS'MatchScon         of r * scon
    | INS'MatchC            of r * vid
    | INS'MatchE            of r * exname (*nesc? - special case of MatchG, when r isLOC'Value ? *)
    | INS'MatchG            of r * r

end



(*----------------------------------------------------------------------
INDEX: compile: CPS_FV -> Code
----------------------------------------------------------------------*)

open CPS_FV;
open Code;
open Prel;

(*----------------------------------------------------------------------
INDEX: env : vid -> r
----------------------------------------------------------------------*)

local open Assoc Atom
in datatype env = ENV of (vid,location) assoc
  val env'empty = ENV assoc'empty
  fun env'plus (ENV a1, ENV a2) = ENV (assoc'plus a1 a2)
  val env'plusList = fold (curry env'plus) env'empty
  fun look'env who (ENV a) k = look'assoc ("look'env("^who^")") sof'vid a k
  fun env'single kv = ENV (assoc'single kv)
end

  infix ++
  val op ++ = env'plus

(*----------------------------------------------------------------------
INDEX: util
----------------------------------------------------------------------*)

  local val uref = ref 0
  in fun newQ info = let val u = !uref in (uref:=1+u; SLOC (u,info)) end
  end

  infix $
  val op $ =
	  fn (seq,SEQ'Empty) => seq
	   | (SEQ'Empty,seq) => seq
	   | p => SEQ'Seq p

  val seq'concat = fold (curry op $) SEQ'Empty
  val seq'Is = seq'concat o map SEQ'I

  val seq'Bind = SEQ'Raise (LOC'Value value'Bind)
  val seq'Match = SEQ'Raise (LOC'Value value'Match)

  val code'Match = CODE'Raise (LOC'Value value'Match)

(*----------------------------------------------------------------------
INDEX: compilation schemes
----------------------------------------------------------------------*)

  type code_context = code -> code

  type schemes = {P     : (env * pat * r -> seq * env),
                  C     : (env * cps -> code),
                  CM    : (env * cmatch * r -> code),
                  CD    : (env * cdec * fvs -> env * code_context * env),
                  A     : (env * atom -> seq * r),
                  At    : (env * atom * q -> seq),
                  Ft    : (env * func * q -> seq * seq),
                  AMt   : (env * amatch * r * q -> seq),
                  AD    : (env * adec -> seq * env)
				  }

  fun compileSequence (X: ('a -> seq * 'b)) : ('a list -> seq * 'b list) =
	  fn atoms =>
		 case (fold (fn (seq1,rs) => fn atom => let val (seq2,r) = X atom
												in (seq1 $ seq2, r::rs)
												end) (SEQ'Empty,[]) atoms)
		  of (seq,rs) => (seq,rev rs)



  fun compile'cps ({A,C,CM,CD,...}:schemes) : (env * cps -> code) =
	  fn (env,cps) => 
		 case cps 
          of CPS'Let (cdec,fvs,cps)		=> let val (env,CC,env') = CD (env,cdec,fvs)
											   val code = C (env,cps)
										   in CC code
										   end
           | CPS'Case (atom,cmatch)		=> let val (seq1,r) = A (env,atom)
											   val code2 = CM (env,cmatch,r)
										   in CODE'Seq (seq1, CODE'FatBar (code2, code'Match))
										   end
           | CPS'App (atom1,atom2)      => let val (seq1,r1) = A (env,atom1)
											   val (seq2,r2) = A (env,atom2)
										   in CODE'Seq (seq1 $ seq2, CODE'TailCall (r1,r2))
										   end
           | CPS'Atom atom              => let val (seq,r) = A (env,atom)
										   in CODE'Seq (seq, CODE'Return r)
										   end
           | CPS'Handle (cps,FVS(xs),cmatch) => 
			 let
				 val code = C (env,cps)
				 val xns = enumerateFrom 0 xs
				 val env_NEW = env'plusList (map (fn (x,n) => env'single (x,LOC'Frame (FLOC (n,INFO'Vid x)))) xns)
				 val codeHandle = CM (env_NEW,cmatch,LOC'Xret)
				 val push = SEQ'I (INS'PushHandle ({frame_size = length xs},
												   CODE'FatBar (codeHandle, CODE'Raise LOC'Xret)))
				 val fill = seq'Is (map (fn (x,n) => let val r = look'env "compile'cps:Handle" env x
													 in INS'SetXcontFrameElem (n, r)
													 end) xns)
				 val pop = SEQ'I (INS'PushContinuation ({frame_size = 0},
														CODE'Seq (SEQ'I INS'PopHandle, CODE'Return LOC'Cret)))
			 in CODE'Seq (push $ fill $ pop, code)
			 end


  fun compile'func_target ({CM,...}:schemes) : (env * func * q -> seq * seq) =
	  fn (env,FUNC(FVS(xs),cmatch),q) => 
		 let val xns = enumerateFrom 0 xs
			 val env_NEW = env'plusList (map (fn (x,n) => env'single (x,LOC'Frame (FLOC (n,INFO'Vid x)))) xns)
			 val code = CM (env_NEW,cmatch, LOC'FuncArg)
			 val seqA = SEQ'I (INS'NewFn (q, {frame_size = length xs}, code))
			 val seqB = seq'Is (map (fn (x,n) => let val r = look'env "compile'func_target" env x
												 in INS'SetFrameElement (LOC'Stack q, n, r)
												 end) xns)
		 in (seqA,seqB)
		 end


  fun compile'cmatch ({P,C,CM,...}:schemes) : (env * cmatch * r -> code) =
	  fn (env,cmatch,r) =>
		 case cmatch 
          of CMATCH'Alt (cmatch1,cmatch2) => let val code1 = CM (env,cmatch1,r)
                                                 val code2 = CM (env,cmatch2,r)
											 in CODE'FatBar(code1,code2)
											 end
		   | CMATCH'Rule (pat,cps)	=> let val (seq1,env1) = P (env,pat,r)
										   val code2 = C (env ++ env1,cps)
									   in CODE'Seq (seq1, code2)
									   end


  fun compile'atom ({A,At,AD,...}:schemes) : (env * atom -> seq * r) =
	  fn (env,atom) =>
		 case atom 
          of ATOM'Value v           => (SEQ'Empty, LOC'Value v) 
           | ATOM'Var x             => (SEQ'Empty, look'env "compile'atom" env x)
           | ATOM'Let (adec,atom)   => let val (seq1,env1) = AD (env,adec)
										   val (seq2,r) = A (env ++ env1, atom)
									   in (seq1 $ seq2, r)
									   end
		   | ATOM'Raise atom		=> let val (seq,r) = A (env,atom)
									   in (SEQ'Raise r, LOC'Null)
									   end
		   | _						=> let val q = newQ (INFO'Atom atom)
										   val seq = At (env,atom,q)
									   in (seq, LOC'Stack q) 
									   end

  fun seq'tuple (q,rs) =
	  SEQ'I (INS'NewTuple (q, {tuple_size = length rs}))
	  $ seq'Is (map (fn (r,n) => INS'SetTupleElement (LOC'Stack q,n,r)) (enumerateFrom 0 rs))


  fun compile'atom_target ({Ft,A,At,AD,AMt,...}:schemes) : (env * atom * q -> seq) =
	  fn (env,atom,q) =>
		 case atom 
		  of ATOM'Value v			=> SEQ'I (INS'Copy (q,LOC'Value v))
		   | ATOM'Var x             => SEQ'I (INS'Copy (q,look'env "compile'atom_target" env x))
		   | ATOM'Tuple atoms       => let val (seq,rs) = compileSequence (curry A env) atoms 
									   in seq $ seq'tuple (q,rs)
									   end
		   | ATOM'Let (adec,atom)	=> let val (seq1,env1) = AD (env,adec)
										   val seq2 = At (env ++ env1,atom,q)
									   in seq1 $ seq2 
									   end
		   | ATOM'Case (atom,amatch)=> let val (seq1,r) = A (env,atom)
										   val seq2 = AMt (env,amatch,r,q)
									   in seq1 $ SEQ'FatBar (seq2,seq'Match)
									   end
		   | ATOM'Raise atom        => let val (seq,r) = A (env,atom)
									   in SEQ'Raise r
									   end
		   | ATOM'Fn func			=> let val (seqA,seqB) = Ft (env,func,q)
									   in seqA $ seqB
									   end


  fun compile'amatch_target ({P,At,AMt,...}:schemes) : (env * amatch * r * q -> seq) =
	  fn (env,amatch,r,q) =>
		 case amatch 
          of AMATCH'Alt (amatch1,amatch2) => let val seq1 = AMt (env,amatch1,r,q)
                                                 val seq2 = AMt (env,amatch2,r,q)
											 in SEQ'FatBar(seq1,seq2)
											 end
		   | AMATCH'Rule (pat,atom)	=> let val (seq1,env1) = P (env,pat,r)
										   val seq2 = At (env ++ env1,atom,q)
									   in seq1 $ seq2
									   end


  fun compile'adec ({P,Ft,A,AD,...}:schemes) : (env * adec -> seq * env) =
	  fn (env,adec) =>
		 case adec 
		  of ADEC'Empty					=> (SEQ'Empty,env)
           | ADEC'Seq (adec1,adec2)     => let val (seq1,env1) = AD (env,adec1)
											   val (seq2,env2) = AD (env,adec2)
										   in (seq1 $ seq2, env1 ++ env2)
										   end
           | ADEC'Local (adec1,adec2)   => let val (seq1,env1) = AD (env,adec1)
											   val (seq2,env2) = AD (env,adec2)
										   in (seq1 $ seq2, env2)
										   end
           | ADEC'Val (pat,atom)        => let val (seq1,r) = A (env,atom)
											   val (seq2,env') = P (env,pat,r)
										   in (seq1 $ SEQ'FatBar (seq2, seq'Bind) , env')
										   end
           | ADEC'ExGen x               => let val q = newQ (INFO'Vid x)
											   val r = LOC'Stack q
										   in (SEQ'I (INS'NewException (q,x)), env'single (x,r))
										   end
           | ADEC'ExCopy (x,y)          => let val r = look'env "compile'adec" env x
										   in (SEQ'Empty, env'single (x,r))
										   end
           | ADEC'Valrec binds => 
			 let val trips = map (fn (x,f) => (x,f,newQ (INFO'Vid x))) binds
				 val env' = env'plusList (map (fn (x,_,q) => env'single (x,LOC'Stack q)) trips)
				 val envR = env ++ env'
				 val seqPair = map (fn (_,f,q) => Ft (envR,f,q)) trips
				 val (seqA,seqB) = 
					 fold (fn (seqA1,seqB1) => 
							  fn (_,f,q) => let val (seqA2,seqB2) = Ft (envR,f,q)
											in (seqA1 $ seqA2, seqB1 $ seqB2)
											end) (SEQ'Empty,SEQ'Empty) trips
			 in (seqA $ seqB, env')
			 end


(*----------------------------------------------------------------------
INDEX: new - not checked in NJ yet
----------------------------------------------------------------------*)

  fun compile'pat ({P,...}:schemes) : (env * pat * r -> seq * env) =
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
           | PAT'G (x,None)         => let val r2 = look'env "compile'pat" env x
									   in (SEQ'I (INS'MatchG (r,r2)), env'empty)
									   end
           | PAT'G (x,Some pat)     => let val r2 = look'env "compile'pat" env x
										   val q = newQ (INFO'Pat pat)
										   val (seq,env) = P (env,pat,LOC'Stack q)
									   in (seq'Is [INS'MatchG (r,r2), INS'DeExcon (q,r)] $ seq, env)
									   end
           | PAT'Scon scon          => (SEQ'I (INS'MatchScon (r,scon)), env'empty)
           | PAT'Tuple pats => 
			 let val trips = map (fn (pat,n) => (pat,n,newQ (INFO'Pat pat))) (enumerateFrom 0 pats)
			 in fold (fn (seqAcc,envAcc) => 
						 fn (pat,n,q) => let val seq1 = SEQ'I (INS'DeTuple (q,r,n))
											 val (seq2,env') = P (env,pat,LOC'Stack q)
										 in (seqAcc $ seq1 $ seq2, envAcc ++ env')
										 end) (SEQ'Empty,env'empty) trips
			 end


  infix U
  fun op U (FVS xs,FVS ys) = FVS(xs@ys)

  fun compile'cdec ({P,C,CD,AD,...}:schemes) : (env * cdec * fvs -> env * code_context * env) =
	  fn (env,cdec,fvs1) =>
		 case cdec
          of CDEC'Empty                     => (env,fn code => code, env'empty)
           | CDEC'Seq (cdec1,fvs2,cdec2)    => let val (env,cc1,env1) = CD (env,cdec1,fvs1)
												   val (env,cc2,env2) = CD (env,cdec2,fvs1 U fvs2)
											   in (env, cc1 o cc2, env1 ++ env2)
											   end
           | CDEC'Local (cdec1,fvs2,cdec2)  => let val (env,cc1,env1) = CD (env,cdec1,fvs1)
												   val (env,cc2,env2) = CD (env,cdec2,fvs1 U fvs2)
											   in (env, cc1 o cc2, env2)
											   end
           | CDEC'Atom adec					=> let val (seq,env1) = AD (env,adec)
											   in (env, fn code => CODE'Seq (seq,code), env1)
											   end
           | CDEC'Val (FVS(xs),pat,cps) =>
			 let
				 val code1 = C (env,cps)
				 val xns = enumerateFrom 0 xs
				 val env_NEW = env'plusList (map (fn (x,n) => env'single (x,LOC'Frame (FLOC (n,INFO'Vid x)))) xns)
				 val (seq2,env') = P (env_NEW,pat,LOC'Cret)
				 fun cc code =
					 let val push = SEQ'I (INS'PushContinuation ({frame_size=length xs},
																 CODE'Seq (SEQ'FatBar (seq2, seq'Bind), 
																		   code)))
						 val fill = seq'Is (map (fn (x,n) => let val r = look'env "compile'cdec" env x
															 in INS'SetContFrameElem (n, r)
															 end) xns)
					 in CODE'Seq (push $ fill, code1)
					 end
			 in (env_NEW,cc,env')
			 end



(*----------------------------------------------------------------------
INDEX: fix-up schemes
----------------------------------------------------------------------*)

  local
	  fun makeSchemes () = {P=P,C=C,CM=CM,CD=CD,A=A,At=At,Ft=Ft,AMt=AMt,AD=AD}
	  and P   x = compile'pat           (makeSchemes()) x
	  and C   x = compile'cps           (makeSchemes()) x
	  and CM  x = compile'cmatch        (makeSchemes()) x
	  and CD  x = compile'cdec          (makeSchemes()) x
	  and A   x = compile'atom          (makeSchemes()) x
	  and At  x = compile'atom_target   (makeSchemes()) x
	  and Ft  x = compile'func_target   (makeSchemes()) x
	  and AMt x = compile'amatch_target (makeSchemes()) x
	  and AD  x = compile'adec          (makeSchemes()) x
  in val schemes = makeSchemes()
  end

(*----------------------------------------------------------------------
INDEX:  TODO - avoid tupling / untupling
----------------------------------------------------------------------*)
(*

(* this is merging two diff optimization ideas... hmm, maybe can seperate*)
  datatype location_structure
    = LOCS'Tuple    of location_structure list
    | LOCS'Loc      of r
    | LOCS'DeCon    of r
    | LOCS'DeExcon  of r
    | LOCS'DeTuple  of r * int

  type rs = location_structure
		   
(* New compilation schemes *)
			
	S : rs -> seq * r (* do the Decons / build the tuple implied by the rs location_structure *)

(* note: more important to avoid un-nesc tuple build (as this allocates) 
as opposed to avoiding un-nesc de-cons *)

	Ps: env * pat * rs -> seq * env (* compile pat to seq, with matching-value spread over rs *)
								  
	As: env * atom -> seq * rs (* compile atom to seq, (delaying tuple builds), so value of atom is spread over es *)

(* Is the following a good idea / sensible / possible ? - what exactly do we avoid?*)
- (* NO, I cant see what benefit these are - despite them being written on my paper notes !!! *)
(* perhaps it helps: let val pat = case ...
or case (case ...) of
*)

	type qs = stack_location_structure

    Pexpect : env * pat -> qs * seq * env (* compile pattern to seq, requested matching-value to be spead over qs *)

    Ats : env * atom * qs -> seq (* compile atom, speading value over targer stack_location_structure (qs) *)


*)
