(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  27.    refs
  33.    export
  114.   imports
  148.   functor
  202.   info
  212.   name'* - make a legal prefix string (maybe empty)
  239.   q/r -- LH (stack_location) / RHS (location)
  259.   code
  306.   execution counts
  457.   pp
  559.   export -> ccode
  706.   value manip
  753.   frame
  771.   stack - need to know max stack size to use arrays; use assoc for now
  780.   env
  804.   ccode - frame,closure
  821.   Pap / Closure
  948.   control
  1028.  Return / Enter / TailCall
  1082.  execute
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: refs
----------------------------------------------------------------------*)

val trace_tailcalls = ref false

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature MachineSig =
sig

  type value
  type builtin
  type exname
  type exval
  type vid
  type uid
  type con
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

  datatype im
    = IM'MatchScon          of r * scon
    | IM'MatchC             of r * con
    | IM'MatchE             of r * exname
    | IM'MatchG             of r * r

  datatype label = LABEL of int

  datatype code
	= CODE'Seq              of instruction * code
    | CODE'Return           of r
    | CODE'TailCall			of r * r list
	| CODE'Raise			of r
	| CODE'If				of im * code * code
	| CODE'Where			of code * label * code
	| CODE'Goto				of label

  and instruction
    = INS'NewTuple          of q * {tuple_size:int}
    | INS'NewException      of q * vid
    | INS'NewFn             of q * {frame_size:int, num_args: int} * code * {name: string, info: afunc}
    | INS'NewRef            of q * r
    | INS'NewCon            of q * con * r
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

  type layout
  val lay'code : code -> layout						  

  val eval'code	: code -> (value,exval) either

end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ~/project/prelude/PREL.ML *)
signature PrelSig_Import =
sig
	val echo			: string -> unit
	val abort			: string -> 'a
	val filter			: ('a -> bool) -> 'a list -> 'a list
	val stringOfInt		: int -> string
	val exists			: ('a -> bool) -> 'a list -> bool
	val enumerateFrom	: int -> 'a list -> ('a * int) list
	val spaces			: int -> string
	val max				: int -> int -> int
	val upto			: int * int -> int list
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AssocSig ~/project/prelude/ASSOC.ML *)
signature AssocSig_Import =
sig
	type ('a,'b) assoc
	val assoc'empty		: ('a,'b) assoc
	val extend'assoc	: ('a,'b) assoc -> 'a * 'b -> ('a,'b) assoc
	val look'assoc		: string -> (''a -> string) -> (''a,'b) assoc -> ''a -> 'b
end;
(*IMPORTDONE:*)

(*IMPORTSIG: PosSig POS.ML *)
signature PosSig_Import =
sig
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AtomSig ATOM.ML *)
signature AtomSig_Import =
sig
    type vid;
    type lab;
	datatype scon =
		SCON'Num of int
	  | SCON'Word of word
	  | SCON'Real of real 
	  | SCON'Char of char
	  | SCON'String of string
	val sof'vid : vid -> string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: EmbedSig EMBED.ML *)
signature EmbedSig_Import =
sig
  type exname
  type value
  type vid
  type scon
  type con
  type uid
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
    type exp;
    type func;
    type match;
    type dec;
  type env
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
	type closure
	type exval
	type layout
	type cid
	type cexp
	type cact
	type cdef
	type builtin
    val name'builtin    : builtin -> string
    val apply'builtin   : builtin -> value list -> value
	type con
	
	val eq'con			: con -> con -> bool
    val sof'con         : con -> string
    val n'con           : con -> int
                                    
	val value'null		: string -> value
    val value'exn       : string * exn 
						  * (unit -> layout) 
						  * (unit -> cid * ((value -> cexp) * (cdef -> unit) * (cact -> unit) -> cexp))
						  * (value -> (value,exval) either) -> value
	val exn'value		: value -> exn option
	val value'newTuple	: string -> int -> value
	val setTuple'value	: string -> value * int * value -> unit
	val value'getTuple	: string -> value * int -> value
	val apply'value		: value -> value -> (value,exval) either
	val funcName'value	: value -> string
								
	val value'con1		: con * value -> value
	val value'exval		: exval -> value
	val value'addr		: value ref -> value
	val gen'exname		: vid -> exname
	val vid'exname		: exname -> vid
	val n'exname		: exname -> int
	val exval'con0		: exname -> exval
	val exval'value		: string -> value -> exval
	val scon'value		: string -> value -> scon
						 
	val con'value		: string -> value -> (con * value option)
	val excon'value		: string -> value -> (exname * value option)
	val derefAddr'value : string -> value -> value
	val eq'scon			: scon -> scon -> bool
	val eq'exname		: exname -> exname -> bool
	val lay'value		: value -> layout
end;
(*IMPORTDONE:*)

(*IMPORTSIG: MisclaySig ~/project/prelude/MISCLAY.ML *)
signature MisclaySig_Import =
sig
  type layout
  val Word                      : string -> layout
  val Left                      : string -> layout
  val Right                     : string -> layout
  val Sep                       : string -> layout
  val Blank                     : layout
  val squash_to                 : int -> layout -> layout
  val prefixed_output_layout    : string -> layout -> unit
  val //                        : layout * layout -> layout
  val @@                        : layout * layout -> layout
  val ++                        : layout * layout -> layout
  val ``                        : layout * layout -> layout
  val //``                      : layout * layout -> layout
  val layInt                    : int -> layout
  val layBracket                : layout -> layout
  val layBrace                  : layout -> layout
  val layListSep                : layout -> layout list -> layout
  val layCommaSep               : layout list -> layout
  val layVert                   : layout list -> layout
end;
(*IMPORTDONE:*)

(*IMPORTSIG: PrettySig PRETTY.ML *)
signature PrettySig_Import =
	sig
		type layout
		type scon
		type dec
		val lay'scon : scon -> layout
	end;
(*IMPORTDONE:*)

(*IMPORTSIG: CpsSig CPS.ML *)
signature CpsSig_Import =
sig
  type value
  type vid
  type uid
  type scon
  type pat
    type cps;
    type atom;
    type bind;
    type afunc;
    type cmatch;
    type amatch;
  type layout
  val lay'afunc     : afunc -> layout
  val lay'cmatch	: cmatch -> layout
  val N_lay'pat     : int -> pat -> layout
  val N_lay'atom    : {depth: int} -> atom -> layout
  val N_lay'cps_tail: {depth:int} -> cps -> layout
end;
(*IMPORTDONE:*)

(*IMPORTSIG: CcodeSig CCODE.ML *)
signature CcodeSig_Import =
sig
	type cid
	type clab
	type ctype
	type ccall
	type cexp
	type cact
	type cstat
	type cdef
    val makeCid                     : string -> cid
    val clab's                      : string -> clab
    val ctype's                     : string -> ctype
    val ccall                       : cid * cexp list -> ccall
    val cdef'FuncDef                : ctype * cid * (ctype * cid) list * cstat -> cdef
    val cdef'VarDef                 : ctype * cid * cexp -> cdef
    val cexp'Id                     : cid -> cexp
	val cact'Assign					: cid * cexp -> cact
	val cact'Call					: ccall -> cact
    val cstat'DecListThen           : ctype * cid list * cstat -> cstat
    val cstat'ActThen               : cact * cstat -> cstat
    val cstat'Goto                  : clab -> cstat
    val cstat'If                    : cexp * cstat * cstat -> cstat
    val cstat'Where                 : cstat * clab * cstat -> cstat
    val newCid                      : string -> cid
    val ctype'Nword                 : ctype
    val ctype'Ncode                 : ctype
    val cexp'NewTup                 : int -> cexp
    val cexp'NewException           : string -> cexp
    
    val cexp'MakeSiFn               : {frame_size:int, num_args:int} -> cexp -> cexp
    val cexp'MakeFn                 : cexp -> cexp
    val cexp'NewPap                 : cexp -> {num_early:int, num_late:int} -> cexp
    val cexp'NewRef                 : cexp -> cexp
    val cexp'NewCon                 : int -> cexp -> cexp
    val cexp'Copy                   : cexp -> cexp
    val cexp'DeCon                  : cexp -> cexp
    val cexp'DeExcon                : cexp -> cexp
    val cexp'DeRef                  : cexp -> cexp
    val cexp'DeTuple                : cexp -> int -> cexp
    val cexp'Builtin                : string -> cexp list -> cexp
	val cexp'MakeSiCont				: int -> cid -> cexp
	val cexp'MakeSiHandle			: int -> cid -> cexp
	val ccall'PushContinuation		: cexp -> ccall
    val ccall'PushHandler           : cexp -> ccall
    val ccall'SetTupleElement       : cexp -> int -> cexp -> ccall
    val ccall'SetFrameElement       : cexp -> int -> cexp -> ccall
    val ccall'SetContFrameElem      : int -> cexp -> ccall
    val ccall'SetXcontFrameElem     : int -> cexp -> ccall
    val cexp'MatchNum               : cexp -> int -> cexp
    val cexp'MatchWord              : cexp -> word -> cexp
    val cexp'MatchChar              : cexp -> char -> cexp
    val cexp'MatchString            : cexp -> string -> cexp
    val cexp'MatchC                 : cexp -> int -> cexp
    val cexp'MatchE                 : cexp -> int -> cexp
    val cexp'MatchG                 : cexp -> cexp -> cexp
    val cstat'ReturnWith            : cexp -> cstat
    val cstat'Tail                  : cexp -> cexp list -> cstat
    val cstat'Raise                 : cexp -> cstat
    val cexp'Frame                  : int -> cexp
    val cexp'Arg                    : int -> cexp
    val cexp'Cret                   : cexp
    val cexp'Xret                   : cexp
    type layout
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor MACHINE
		(val prefixNML : string
		 structure Prel : PrelSig_Import
		 structure Assoc : AssocSig_Import
		 structure Pos : PosSig_Import
		 structure Atom : AtomSig_Import
		 structure Embed : EmbedSig_Import
		 structure Value : ValueSig_Import
		 structure Misclay : MisclaySig_Import
		 structure Pretty : PrettySig_Import
		 structure Cps : CpsSig_Import
		 structure Ccode : CcodeSig_Import

		 val wrapCol : int
		 val display_counts : bool ref

		 sharing type Atom.lab = Value.lab
		 sharing type Atom.vid = Value.vid = Cps.vid = Embed.vid
		 sharing type Cps.pat = Embed.pat
		 sharing type Atom.scon = Value.scon = Pretty.scon = Embed.scon
		 sharing type Misclay.layout = Value.layout = Cps.layout
		 sharing type Misclay.layout = Pretty.layout = Ccode.layout

		 sharing type Embed.con = Value.con
		 sharing type Embed.exname = Value.exname
		 sharing type Embed.value = Value.value

		 sharing type Ccode.cid = Value.cid
		 sharing type Ccode.cexp = Value.cexp
		 sharing type Ccode.cact = Value.cact
		 sharing type Ccode.cdef = Value.cdef

	   ) : MachineSig =
struct

  infix // @@ ++ `` //``

  open Cps Prel Assoc Pos Atom Value Misclay Pretty Embed Ccode

  fun list'array array = 
	  map (fn n => Array.sub (array,n)) (upto(0,Array.length array - 1))


  fun member eq ys x = exists (fn y => eq x y) ys

  fun removeDups _ [] = []
	| removeDups eq (x::xs) = x :: removeDups eq (if (member eq xs x)
												  then filter (fn y => not (eq x y)) xs 
												  else xs)

(*----------------------------------------------------------------------
INDEX: info
----------------------------------------------------------------------*)

  datatype info
	= INFO'Uid      of uid
	| INFO'Pat      of pat
	| INFO'Atom     of atom
	| INFO'String	of string

(*----------------------------------------------------------------------
INDEX: name'* - make a legal prefix string (maybe empty)
----------------------------------------------------------------------*) 

  val name'pat =
      fn PAT'V x                => sof'uid x ^ "_"
       | PAT'Layered (x,pat)    => sof'uid x ^ "_AS_"
       | _                      => "" (*"PAT_"*)

  val name'info =
      fn INFO'Uid x             => sof'uid x ^ "_"
       | INFO'Pat pat           => name'pat pat
       | INFO'Atom atom         => "" (*"ATOM_"*)
       | INFO'String _          => "" (*"ATOM_"*)


  fun lay'vid x = Word (sof'vid x)
  fun lay'uid x = Word (sof'uid x)
  fun lay'con x = Word (sof'con x)

  fun N_lay'info {depth=n} info =
	  case info
       of INFO'Uid x            => lay'uid x
        | INFO'Pat pat          => N_lay'pat n pat
        | INFO'Atom atom        => N_lay'atom {depth=n} atom
        | INFO'String s         => Word ("<"^s^">")

(*----------------------------------------------------------------------
Index: q/r -- LH (stack_location) / RHS (location)
----------------------------------------------------------------------*)

  (* q - lhs - "l" looks too much like a 1, and idents like l1 are hard to read *)	   
  datatype q = SLOC of int * info (* stack_location *)

  (* r - rhs - location of values at runtime *)
  datatype r 
    = LOC'Stack     of q
    | LOC'Frame     of int * info (* free vars *)
    | LOC'Arg       of int * info (* multi args *)
    | LOC'Cret
	| LOC'Xret
	| LOC'Value		of value (* value embedded in code *)

  fun info'q (SLOC (_,info)) = info

  fun eq'q (SLOC (n1,_)) (SLOC (n2,_)) = (n1=n2)

(*----------------------------------------------------------------------
INDEX: code
----------------------------------------------------------------------*)

(* match existing object, for use in CODE'If *)
  datatype im
    = IM'MatchScon          of r * scon
    | IM'MatchC             of r * con
    | IM'MatchE             of r * exname  (*nesc? - special case of MatchG, when r isLOC'Value ? *)
    | IM'MatchG             of r * r

  datatype label = LABEL of int

  datatype code (* terminated code sequence *)
	= CODE'Seq              of instruction * code
    | CODE'Return           of r
    | CODE'TailCall			of r * r list (* multi args *)
	| CODE'Raise			of r
	| CODE'If				of im * code * code
	| CODE'Where			of code * label * code
	| CODE'Goto				of label

  and instruction
(* allocated new object & assign to a location *)
    = INS'NewTuple          of q * {tuple_size:int}
    | INS'NewException      of q * vid
    | INS'NewFn             of q * {frame_size:int, num_args:int} * code * {name: string, info: afunc}
    | INS'NewRef            of q * r
    | INS'NewCon            of q * con * r
(* copy/unpack existing object & assign to a location *)
    | INS'Copy              of q * r
    | INS'DeCon             of q * r
    | INS'DeExcon           of q * r
    | INS'DeRef             of q * r
    | INS'DeTuple			of q * r * int
    | INS'BuiltinApp        of q * builtin * r list
(* allocate/push new control continuation & push on continuation stack *)
    | INS'PushContinuation  of {frame_size:int} * code * {info: cps * uid list}
(* allocate/push new exception handler + continuation to pop hander *)
    | INS'PushHandler	    of {frame_size:int} * code * {info: cmatch * uid list}

(* set tuple/frame element in existing object *)
	| INS'SetTupleElement	of r * int * r
    | INS'SetFrameElement   of r * int * r
    | INS'SetContFrameElem  of int * r
    | INS'SetXcontFrameElem of int * r

(*----------------------------------------------------------------------
INDEX: execution counts
----------------------------------------------------------------------*)

  datatype icounts 
	= Icounts of {NT: int ref, 
				  (*NF1: int ref, TAIL1 : int ref, *)
				  NF: int ref, NR: int ref,NC: int ref,
				  TAIL : int ref,
				  NX: int ref, CP: int ref, BI: int ref, 
				  DC: int ref, DE: int ref, DR: int ref, DT: int ref, 
				  PC: int ref, 
				  PCX: int ref, (* max cont stack depth *)
				  PH: int ref,
				  ST: int ref, SF: int ref, SC: int ref, SX: int ref, 
				  MS: int ref, MC: int ref, ME: int ref, MG: int ref, 
				  IF: int ref, CW: int ref, GO: int ref,
				  PAP: int ref, REAPP: int ref,
				  RET : int ref, RAISE : int ref, SKIP : int ref}

  fun new'icounts() =
	  Icounts {NT = ref 0, 
			   (*NF1 = ref 0, TAIL1 = ref 0, *)
			   NF = ref 0, NR = ref 0, NC = ref 0, 
			   TAIL = ref 0,
			   NX = ref 0, CP = ref 0, BI = ref 0, 
			   DC = ref 0, DE = ref 0, DR = ref 0, DT = ref 0, 
			   PC = ref 0, 
			   PCX = ref 0, 
			   PH = ref 0,
			   ST = ref 0, SF = ref 0, SC = ref 0, SX = ref 0, 
			   MS = ref 0, MC = ref 0, ME = ref 0, MG = ref 0, 
			   IF = ref 0, CW = ref 0, GO = ref 0,
			   PAP = ref 0, REAPP = ref 0,
			   RET = ref 0, RAISE = ref 0, SKIP = ref 0}


  infix +=
  fun ((r as ref x) += n) = (r:=n+x)

  fun inc (r as ref x) = (r:=x+1)
  fun dec (r as ref x) = (r:=x-1)

  fun count'im
	  (Icounts {NT,NX,NF,NR,NC,CP,BI,DC,DE,DR,DT,PC,PCX,PH,ST,SF,SC,SX,MS,MC,ME,MG,IF,CW,GO,RET,RAISE,SKIP,TAIL,PAP,REAPP}) =
      fn IM'MatchScon (r,scon)                      => inc MS
       | IM'MatchC (r,con)                          => inc MC
       | IM'MatchE (r,exname)                       => inc ME
       | IM'MatchG (r1,r2)                          => inc MG

  fun count'instruction 
	  (Icounts {NT,NX,NF,NR,NC,CP,BI,DC,DE,DR,DT,PC,PCX,PH,ST,SF,SC,SX,MS,MC,ME,MG,IF,CW,GO,RET,RAISE,SKIP,TAIL,PAP,REAPP}) =
      fn INS'NewTuple (q,{tuple_size})              => inc NT
       | INS'NewException (q,vid)                   => inc NX
       | INS'NewFn (q,{frame_size,num_args},code,info) => inc NF (* if num_args=1 then inc NF1 else ()*)
       | INS'NewRef (q,r)							=> inc NR
       | INS'NewCon (q,con,r)						=> inc NC
       | INS'Copy (q,r)                             => inc CP
       | INS'DeCon (q,r)                            => inc DC
       | INS'DeExcon(q,r)                           => inc DE
       | INS'DeRef (q,r)                            => inc DR
       | INS'DeTuple (q,r,n)                        => inc DT
       | INS'BuiltinApp (q,bi,rs)                   => inc BI
       | INS'PushContinuation ({frame_size},code,_) => inc PC
       | INS'PushHandler ({frame_size},code,_)      => inc PH
       | INS'SetTupleElement (r1,n,r2)              => inc ST
       | INS'SetFrameElement (r1,n,r2)              => inc SF
       | INS'SetContFrameElem (n,r2)                => inc SC
       | INS'SetXcontFrameElem (n,r2)               => inc SX


  fun max (a,b) = if a>b then a else b

  fun count'return depth (Icounts {NT,NX,NF,NR,NC,CP,BI,DC,DE,DR,DT,PC,PCX,PH,ST,SF,SC,SX,MS,MC,ME,MG,IF,CW,GO,RET,RAISE,SKIP,TAIL,PAP,REAPP}) = 
	  (inc RET ; PCX := max (!PCX,depth))


  fun count'raise skip
				  (Icounts {NT,NX,NF,NR,NC,CP,BI,DC,DE,DR,DT,PC,PCX,PH,ST,SF,SC,SX,MS,MC,ME,MG,IF,CW,GO,RET,RAISE,SKIP,TAIL,PAP,REAPP}) = 
	  (inc RAISE; SKIP += skip);

  fun count'pap (Icounts {NT,NX,NF,NR,NC,CP,BI,DC,DE,DR,DT,PC,PCX,PH,ST,SF,SC,SX,MS,MC,ME,MG,IF,CW,GO,RET,RAISE,SKIP,TAIL,PAP,REAPP}) = 
	  (inc PAP)
  fun count'reapp (Icounts {NT,NX,NF,NR,NC,CP,BI,DC,DE,DR,DT,PC,PCX,PH,ST,SF,SC,SX,MS,MC,ME,MG,IF,CW,GO,RET,RAISE,SKIP,TAIL,PAP,REAPP}) = 
	  (inc REAPP)

  fun count'tail (Icounts {NT,NX,NF,NR,NC,CP,BI,DC,DE,DR,DT,PC,PCX,PH,ST,SF,SC,SX,MS,MC,ME,MG,IF,CW,GO,RET,RAISE,SKIP,TAIL,PAP,REAPP}) = 
	  fn num_args => inc TAIL (*if (num_args = 1) then inc TAIL1 else ()*)


  fun count'code
	  (Icounts {NT,NX,NF,NR,NC,CP,BI,DC,DE,DR,DT,PC,PCX,PH,ST,SF,SC,SX,MS,MC,ME,MG,IF,CW,GO,RET,RAISE,SKIP,TAIL,PAP,REAPP}) =
       fn CODE'Seq (ins,code)           => ()
        | CODE'Return r                 => ()
        | CODE'TailCall (func,args)     => ()
        | CODE'Raise r                  => ()
        | CODE'If (im,code1,code2)      => inc IF
        | CODE'Where (code1,lab,code2)  => inc CW
        | CODE'Goto lab                 => inc GO


  fun lay'count tag (ref x) = Word tag @@ Sep "=" @@ layInt x

  val layCommaSep = layListSep (Right ",")

  fun lay'icounts (Icounts {NT,NX,NF,NR,NC,CP,BI,DC,DE,DR,DT,PC,PCX,PH,ST,SF,SC,SX,MS,MC,ME,MG,IF,CW,GO,RET,RAISE,SKIP,TAIL,PAP,REAPP}) =
	  Word "icounts: " @@ (layVert [
									Word "Control:" @@ 
										 layCommaSep [lay'count "Call" TAIL,
													  lay'count "Push" PC,
													  lay'count "Handle" PH,
													  lay'count "Reapp" REAPP,
													  lay'count "Return" RET,
													  lay'count "Raise" RAISE,
													  lay'count "Skip" SKIP],
									Word "FrameSet:" @@ 
										 layCommaSep [lay'count "Function" SF,
													  lay'count "Continuation" SC,
													  lay'count "Handler" SX],
									Word "Branch:" @@ 
										 layCommaSep [lay'count "If" IF,
													  lay'count "CodeWhere" CW,
													  lay'count "Goto" GO],
									Word "Match:" @@ 
										 layCommaSep [lay'count "Scon" MS,
													  lay'count "Con" MC,
													  lay'count "Excon" ME,
													  lay'count "GenEx" MG],
									Word "New:" @@ 
										 layCommaSep [lay'count "Closure" NF,
													  lay'count "Pap" PAP,
													  lay'count "Tuple" NT,
													  lay'count "TupleElem" ST,
													  lay'count "Con" NC,
													  lay'count "Ref" NR,
													  lay'count "Exception" NX
													  ],
									Word "Destruct" @@ 
										 layCommaSep [lay'count "TupleElem" DT,
													  lay'count "Con" DC,
													  lay'count "Excon" DE,
													  lay'count "Ref" DR],
									Word "Builtin:" @@ 
										 layCommaSep [lay'count "App" BI,
													  lay'count "Copy" CP],
									lay'count "Max-Control-Stack-Depth" PCX,
									if (1 + !PC + !PH + !REAPP - (!RET + !SKIP) <> 0)
									then Word "*** Invariant Failed: (1+Push+ReApp+PushHandler)-(Return+Skip) = 0"
									else Blank
									])

(*----------------------------------------------------------------------
INDEX: pp
----------------------------------------------------------------------*)

  fun lay'q (SLOC (n,info)) = Word (name'info info ^ "q" ^stringOfInt n)

  fun lay'r location =
	  case location 
       of LOC'Stack q           => lay'q q
        | LOC'Frame (n,info)    => Word (name'info info ^ "FRAME_" ^ stringOfInt n)
        | LOC'Arg (n,info)      => Word (name'info info ^ "ARG_"   ^ stringOfInt n)
        | LOC'Cret              => Word "CRET"
        | LOC'Xret              => Word "XRET"
        | LOC'Value v           => Left "value<" @@ lay'value v @@ Right ">"


  fun lay'exname exname = lay'vid (vid'exname exname)

  fun layComment lay = Word "//" @@ lay

  fun layBind' q lay = lay'q q @@ Word "=" @@ lay
  fun layBind q lay = layBind' q lay @@ layComment (N_lay'info {depth=4} (info'q q))

  fun layIndexed lay1 n = lay1 @@ Sep "[" @@ layInt n @@ Right "]"
  fun layIndexBind lay1 n lay2 = layIndexed lay1 n @@ Word "=" @@ lay2

  fun lay'op name lay = Word name @@ layBracket lay
  fun lay'match r lay = lay'r r @@ Word "==" @@ lay

  fun layNestedCode head body =
	  (head @@ Word "{") //`` body // Word "}"

  fun lay'save xs = layBrace (layCommaSep (map lay'uid xs))

  fun layContinuationInfo (cps,save) = 
	  layComment (lay'save save)
	  // layComment (Word "CRET=" @@ N_lay'cps_tail {depth=4} cps)

  fun layHandleInfo (cmatch,save) = 
	  layComment (lay'save save)
	  // layComment (Word "handler=" @@ lay'cmatch cmatch)

  fun layAngle x = Left "<" @@ x ++ Right ">"

  fun layArgs lay args = layAngle (layCommaSep (map lay args))

  fun lay'NewFn ({frame_size,num_args},{name,info=afunc},C) =
	  layNestedCode (Left ("NewFn:"^name))
	  ((layComment (Word ("#frame="^stringOfInt frame_size) @@ Word ("#args="^stringOfInt num_args)))
	   (* // layComment (lay'afunc afunc)*)
	   // C)

  fun lay'im im =
	  case im
       of IM'MatchScon (r,scon)                         => Word "MatchScon:" @@ lay'match r (lay'scon scon)
        | IM'MatchC (r,con)                             => Word "MatchC:" @@ lay'match r (lay'con con)
        | IM'MatchE (r,exname)                          => Word "MatchE:" @@ lay'match r (lay'exname exname)
        | IM'MatchG (r1,r2)                             => Word "MatchG:" @@ lay'match r1 (lay'r r2)

  fun layIfElse im lay1 lay2 = 
	  (layNestedCode (Word "if" @@ layBracket (lay'im im)) lay1)
	  // (layNestedCode (Word "else") lay2)

  fun sof'label (LABEL n) = "L_"^stringOfInt n
  fun lay'label lab = Word (sof'label lab)

  fun lay'instruction ins =
	  case ins
       of INS'NewTuple (q,{tuple_size})                 => layBind q (Left "NewTuple" @@ layInt tuple_size)
        | INS'NewException (q,vid)                      => layBind q (lay'op "NewException" (lay'vid vid))
        | INS'NewFn (q,sizes,code,info)					=> layBind' q (lay'NewFn (sizes,info,lay'code code))
		| INS'NewRef (q,r)								=> layBind q (lay'op "Ref" (lay'r r)) 
		| INS'NewCon (q,con,r)							=> layBind q (lay'op "Con" (lay'con con) @@ layBracket (lay'r r))
        | INS'Copy (q,r)                                => layBind q (lay'op "Copy" (lay'r r)) 
        | INS'DeCon (q,r)                               => layBind q (lay'op "DeCon" (lay'r r)) 
        | INS'DeExcon(q,r)                              => layBind q (lay'op "DeExCopy" (lay'r r)) 
        | INS'DeRef (q,r)                               => layBind q (lay'op "DeRef" (lay'r r)) 
        | INS'DeTuple (q,r,n)                           => layBind q (layIndexed (lay'r r) n)
        | INS'BuiltinApp (q,bi,rs)                      => layBind q (lay'op ("BuiltinApp:"^name'builtin bi) (layArgs lay'r rs))
        | INS'PushContinuation ({frame_size},code,{info}) => layNestedCode (Left "PushContinuation" @@ layBracket (layInt frame_size))
																		   ((layContinuationInfo info)
																			// (lay'code code))
        | INS'PushHandler ({frame_size},code,{info})	=> layNestedCode (Left "PushHandler" @@ layBracket (layInt frame_size))
																		 ((layHandleInfo info)
																		  //(lay'code code))
        | INS'SetTupleElement (r1,n,r2)                 => Word "SetTuple:" @@ layIndexBind (lay'r r1) n (lay'r r2)
        | INS'SetFrameElement (r1,n,r2)                 => Word "SetFrame:" @@ layIndexBind (lay'r r1) n (lay'r r2)
        | INS'SetContFrameElem (n,r2)                   => Word "SetContFrame:" @@ layIndexBind (Word "cont") n (lay'r r2)
        | INS'SetXcontFrameElem (n,r2)                  => Word "SetXcontFrame:" @@ layIndexBind (Word "xcont") n (lay'r r2)


  and lay'code code =
	  case code
       of CODE'Seq (ins,code)           => lay'instruction ins // lay'code code
        | CODE'Return r                 => Word "Return:" @@ lay'r r
        | CODE'TailCall (func,args)     => Word "Call:" @@ lay'r func @@ layArgs lay'r args
        | CODE'Raise r                  => Word "Raise:" @@ lay'r r
        | CODE'If (im,code1,code2)      => layIfElse im (lay'code code1) (lay'code code2)
        | CODE'Where (code1,lab,code2)  => layAngle ((lay'code code1)
													 // layNestedCode (Word "where" @@ lay'label lab @@ Word "=") (lay'code code2))
        | CODE'Goto lab                 => Word "goto" @@ lay'label lab

(*----------------------------------------------------------------------
INDEX: export -> ccode
----------------------------------------------------------------------*)

  fun cid'q (SLOC (n,info)) = makeCid (name'info info ^ "q" ^stringOfInt n)

  fun cexp'matchScon e scon =
      case scon of
          SCON'Num n                => cexp'MatchNum e n
        | SCON'Word w               => cexp'MatchWord e w
        | SCON'Real r               => abort "cexp'matchScon/real"
        | SCON'Char c               => cexp'MatchChar e c
        | SCON'String s             => cexp'MatchString e s

  fun cexp'im R im =
	  case im
       of IM'MatchScon (r,scon)         => cexp'matchScon (R r) scon
        | IM'MatchC (r,con)             => cexp'MatchC (R r) (n'con con)
        | IM'MatchE (r,exname)          => cexp'MatchE (R r) (n'exname exname)
        | IM'MatchG (r1,r2)             => cexp'MatchG (R r1) (R r2)

  fun clab'label label = clab's (sof'label label)

  (*fun cc'bind (q,cexp) next  = cstat'DecThen (ctype'Nword, cid'q q, cexp, next)*)
  fun cc'bind (q,cexp) next  = cstat'ActThen (cact'Assign (cid'q q, cexp), next)

  fun cc'call call next = cstat'ActThen (cact'Call call,next)


  fun nameExp EmitDef ctype name e =
	  let val def = cdef'VarDef (ctype,name,e)
		  val () = EmitDef def
	  in cexp'Id name
	  end

  fun ccall'PushCont EmitDef EmitCode frame_size code =
	  let val cid = EmitCode "cont" code
		  val si = cexp'MakeSiCont frame_size cid
	  in ccall'PushContinuation (nameExp EmitDef (ctype's "SiCont*") (newCid "si_cont") si) 
	  end

  fun ccall'PushHand EmitDef EmitCode frame_size code =
	  let val cid = EmitCode "cont" code
		  val si = cexp'MakeSiHandle frame_size cid
	  in ccall'PushHandler (nameExp EmitDef (ctype's "SiCont*") (newCid "si_cont") si)
	  end


  fun cexp'NewFn EmitDef sizes C = 
	  cexp'MakeFn (nameExp EmitDef (ctype's "SiClosure*") (newCid "si_closure") (cexp'MakeSiFn sizes C))


  fun cstat'instruction EmitDef EmitCode R ins =
      case ins
       of INS'NewTuple (q,{tuple_size})                 => cc'bind (q,cexp'NewTup tuple_size)
        | INS'NewException (q,vid)                      => cc'bind (q,cexp'NewException (sof'vid vid))
        | INS'NewFn (q,sizes,code,{info,name})			=> cc'bind (q,cexp'NewFn EmitDef sizes (cexp'Id (EmitCode name code)))
		| INS'NewRef (q,r)								=> cc'bind (q,cexp'NewRef (R r))
		| INS'NewCon (q,con,r)							=> cc'bind (q,cexp'NewCon (n'con con) (R r))
        | INS'Copy (q,r)                                => cc'bind (q,cexp'Copy (R r))
        | INS'DeCon (q,r)                               => cc'bind (q,cexp'DeCon (R r))
        | INS'DeExcon (q,r)                             => cc'bind (q,cexp'DeExcon (R r))
        | INS'DeRef (q,r)                               => cc'bind (q,cexp'DeRef (R r))
        | INS'DeTuple (q,r,n)                           => cc'bind (q,cexp'DeTuple (R r) n)
        | INS'BuiltinApp (q,bi,rs)                      => cc'bind (q,cexp'Builtin (name'builtin bi) (map R rs))
        | INS'PushContinuation ({frame_size},code,_)    => cc'call (ccall'PushCont EmitDef EmitCode frame_size code)
        | INS'PushHandler ({frame_size},code,_)         => cc'call (ccall'PushHand EmitDef EmitCode frame_size code)
        | INS'SetTupleElement (r1,n,r2)                 => cc'call (ccall'SetTupleElement (R r1) n (R r2))
        | INS'SetFrameElement (r1,n,r2)                 => cc'call (ccall'SetFrameElement (R r1) n (R r2))
        | INS'SetContFrameElem (n,r)                    => cc'call (ccall'SetContFrameElem n (R r))
        | INS'SetXcontFrameElem (n,r)                   => cc'call (ccall'SetXcontFrameElem n (R r))


  fun cstat'code EmitDef EmitCode R code =
	  case code
       of CODE'Seq (ins,code)           => cstat'instruction EmitDef EmitCode R ins (cstat'code EmitDef EmitCode R code)
        | CODE'Return r                 => cstat'ReturnWith (R r)
        | CODE'TailCall (func,args)     => cstat'Tail (R func) (map R args)
        | CODE'Raise r                  => cstat'Raise (R r)
        | CODE'If (im,code1,code2)      => cstat'If (cexp'im R im, 
													 cstat'code EmitDef EmitCode R code1, 
													 cstat'code EmitDef EmitCode R code2)
        | CODE'Where (code1,lab,code2)  => cstat'Where (cstat'code EmitDef EmitCode R code1, clab'label lab, 
														cstat'code EmitDef EmitCode R code2)
        | CODE'Goto lab                 => cstat'Goto (clab'label lab)


  fun cexp'r V location =
	  case location 
       of LOC'Stack q           => cexp'Id (cid'q q)
        | LOC'Frame (n,info)    => cexp'Frame n
        | LOC'Arg (n,info)      => cexp'Arg n
        | LOC'Cret              => cexp'Cret
        | LOC'Xret              => cexp'Xret
        | LOC'Value v           => V v


  fun qs'instruction acc ins =
      case ins
       of INS'NewTuple (q,{tuple_size})                 => q::acc
        | INS'NewException (q,vid)                      => q::acc
        | INS'NewFn (q,sizes,code,{info,name})			=> q::acc
		| INS'NewRef (q,r)								=> q::acc
		| INS'NewCon (q,con,r)							=> q::acc
        | INS'Copy (q,r)                                => q::acc
        | INS'DeCon (q,r)                               => q::acc
        | INS'DeExcon (q,r)                             => q::acc
        | INS'DeRef (q,r)                               => q::acc
        | INS'DeTuple (q,r,n)                           => q::acc
        | INS'BuiltinApp (q,bi,rs)                      => q::acc
        | INS'PushContinuation ({frame_size},code,_)    => acc
        | INS'PushHandler ({frame_size},code,_)         => acc
        | INS'SetTupleElement (r1,n,r2)                 => acc
        | INS'SetFrameElement (r1,n,r2)                 => acc
        | INS'SetContFrameElem (n,r)                    => acc
        | INS'SetXcontFrameElem (n,r)                   => acc


  fun qs'code acc code =
	  case code
       of CODE'Seq (ins,code)           => qs'instruction (qs'code acc code) ins
        | CODE'Return r                 => acc
        | CODE'TailCall (func,args)     => acc
        | CODE'Raise r                  => acc
        | CODE'If (im,code1,code2)      => qs'code (qs'code acc code2) code1
        | CODE'Where (code1,lab,code2)  => qs'code (qs'code acc code2) code1
        | CODE'Goto lab                 => acc


  fun cexp'code cexp'V EmitDef name code =
	  let val R = cexp'r cexp'V
		  fun EmitCode name code =
			  let val x = newCid name
				  val xs = removeDups eq'q (qs'code [] code)
				  val body = cstat'code EmitDef EmitCode R code
				  val body = case xs of [] => body | _::_ => cstat'DecListThen (ctype'Nword,map cid'q xs,body)
				  val def = cdef'FuncDef (ctype'Ncode,x,[],body)
				  val () = EmitDef def
			  in x
			  end
	  in cexp'Id (EmitCode name code)
	  end

(*----------------------------------------------------------------------
INDEX: value manip
----------------------------------------------------------------------*)

  val NewTuple : (int -> value) =
	  fn N => value'newTuple "NewTuple" N

  val NewException : (vid -> value) =
	  fn vid => value'exval (exval'con0 (gen'exname vid))

  val NewRef : (value -> value) =
	  fn v => value'addr (ref v)

  val NewCon : (con * value -> value) =
	  fn (x,v) => value'con1 (x,v)

  val SetTupleElement : (value * int * value -> unit) =
	  fn (tup,n,v) => setTuple'value "SetTupleElement" (tup,n,v)

  val DeTuple : (value * int -> value) =
	  fn (tup,n) => value'getTuple "DeTuple" (tup,n)

  val BuiltinApp : (builtin * value list -> value) =
	  fn (bi,vs) => apply'builtin bi vs

  val DeCon : (value -> value) =
	  fn v1 => case (con'value "DeCon" v1) of (_,None) => abort "Decon:None" | (_,Some v2) => v2

  val DeExcon : (value -> value) =
	  fn v1 => case (excon'value "DeCon" v1) of (_,None) => abort "Decon:None" | (_,Some v2) => v2

  val DeRef : (value -> value) =
	  fn v => derefAddr'value "DeRef" v

  val MatchScon : (scon * value -> bool) = 
	  fn (scon,v) => eq'scon scon (scon'value "MatchScon" v)

  val MatchC : (con * value -> bool) = 
	  fn (con,v) => eq'con con (let val (con,_) = con'value "MatchC" v in con end)

  val MatchE : (exname * value -> bool) = 
	  fn (exname,v) => eq'exname exname (let val (exname,_) = excon'value "MatchE" v in exname end)

  val MatchG : (value * value -> bool) = 
	  fn (v1,v2) => eq'exname (let val (exname,_) = excon'value "MatchG/1" v1 in exname end)
							  (let val (exname,_) = excon'value "MatchG/2" v2 in exname end)

(*----------------------------------------------------------------------
INDEX: frame
----------------------------------------------------------------------*)

  datatype frame = FRAME of value array
  fun NewFrame (N:int) = FRAME (Array.array(N,value'null "NewFrame"))
  fun SetFrame (FRAME A,n,v) = Array.update(A,n,v)
  fun GetFrame (FRAME A,n) = Array.sub(A,n)

  fun lay'frame (FRAME A) =
	  layVert (map (fn n => layInt n @@ Word "=" @@ lay'value (Array.sub(A,n))) (upto (0,Array.length(A)-1)))


  fun MkFrame args = let val frame = NewFrame (length args)
						 val _:unit list = map (fn (arg,n) => SetFrame (frame,n,arg)) (enumerateFrom 0 args)
					 in frame
					 end

(*----------------------------------------------------------------------
INDEX: stack - need to know max stack size to use arrays; use assoc for now
----------------------------------------------------------------------*)

  datatype stack = STACK of (int,value) assoc ref
  fun EmptyStack () = STACK (ref assoc'empty)
  fun GetStack (STACK A,n) = look'assoc "GetStack" stringOfInt (!A) n
  fun SetStack (STACK A,n,v) = (A := extend'assoc (!A) (n,v))

(*-------------------------------------------------------------
INDEX: env
----------------------------------------------------------------------*)

  datatype env = ENV of stack * frame * {args:frame}

  fun NewEnv (frame,{args}) = ENV (EmptyStack(),frame,{args = MkFrame args})

  val GetLoc : (env * r -> value) =
	  fn (ENV (stack,frame,{args}),r) =>
		 case r 
          of LOC'Stack (SLOC(n,_))  => GetStack (stack,n)
           | LOC'Frame (n,_)        => GetFrame (frame,n)
           | LOC'Arg (n,_)          => GetFrame (args,n)
           | LOC'Cret               => GetFrame (args,0)
           | LOC'Xret               => GetFrame (args,0)
           | LOC'Value v            => v

  val GetLocs : (env * r list -> value list) =
	  fn (env,args) => map (fn arg => GetLoc (env,arg)) args

  val SetStackLoc : (env * q * value -> unit)=
      fn (ENV(stack,_,_),SLOC (n,_),v) => SetStack(stack,n,v)

(*----------------------------------------------------------------------
INDEX: Pap / Closure
----------------------------------------------------------------------*)

  datatype closure_code_info = CCI of code * string * afunc * {frame_size:int, num_args:int}

  fun lay'cci (CCI (code,name,afunc,{frame_size,num_args})) =
	  layVert [Word "Name:" @@ Word name,
			   Word "#Args:" @@ layInt num_args,
			   Word "#Frame:" @@ layInt frame_size,
			   Word "Source:" @@ lay'afunc afunc,
			   Word "Code:" //`` lay'code code]

  datatype closure = Closure of closure_code_info * frame

  fun num_args'closure (Closure (CCI (code,name,afunc,{frame_size,num_args}),frame)) = num_args

  fun cacts'closure clo es =
	  let val acts = map (fn (e,n) => cact'Call (ccall'SetFrameElement (cexp'Id clo) n e)) (enumerateFrom 0 es)
	  in acts
	  end

  fun cexp'closure_bits V EmitDef (EmitAct:cact->unit) name cid sizes code (FRAME A) =
	  let val C = cexp'code V EmitDef name code
		  val es = map V (list'array A)
		  val acts = cacts'closure cid es
		  val (_:unit list) = map EmitAct acts
		  val exp = cexp'NewFn EmitDef sizes C
		  val def = cdef'VarDef (ctype'Nword,cid,exp)
		  val () = EmitDef def
	  in cexp'Id cid
	  end

  fun cexp'closure V EmitDef EmitAct cid (Closure (CCI (code,name,afunc,sizes),frame)) =
	  cexp'closure_bits V EmitDef EmitAct name cid sizes code frame

  fun cacts'pap pap es =
	  let val acts = map (fn (e,n) => cact'Call (ccall'SetFrameElement (cexp'Id pap) n e)) (enumerateFrom 0 es)
	  in acts
	  end

  fun cexp'pap V EmitDef EmitAct name cid closure early =
	  let
		  val es = map V early
		  val F = cexp'closure V EmitDef EmitAct (newCid ("paps_closure_"^name)) closure
		  val num_early = length early
		  val num_late = num_args'closure closure - num_early
		  val pap_sizes = {num_early = num_early, num_late = num_late}
		  (* *)
		  val acts = cacts'pap cid es
		  val (_:unit list) = map EmitAct acts
		  val exp = cexp'NewPap F pap_sizes
		  val def = cdef'VarDef (ctype'Nword,cid,exp)
		  val () = EmitDef def
	  in cexp'Id cid
	  end


  type res	= (value,exval) either
  type interpret = (closure * value list -> res)

  fun lay'closure (Closure (cci,frame)) =
	  layVert [Word "Frame:" @@ lay'frame frame, 
			   lay'cci cci]

  exception V_Closure of closure

  fun NewFn (Interpret,sizes as {frame_size,num_args},code,{name,info=afunc}) = 
	  let val frame = NewFrame(frame_size)
		  val cci = CCI (code,name,afunc,sizes)
		  val closure = Closure (cci,frame)
		  fun Decompile() = lay'closure closure
		  fun NewExportStage2 cid (V,EmitDef,EmitAct) = cexp'closure_bits V EmitDef EmitAct name cid sizes code frame
		  fun NewExportC () = let val cid = newCid ("closure_"^name)
							  in (cid,NewExportStage2 cid)
							  end
		  fun Apply v = Interpret (closure,[v]) (* for use by interpreter *)
	  in value'exn ("closure:"^name, V_Closure closure, Decompile, NewExportC, Apply)
	  end

  val SetFrameElement : (value * int * value -> unit) =
	  fn (func,n,v) => 
		 case (exn'value func) 
		  of Some (V_Closure (Closure (_,frame))) => SetFrame (frame,n,v)
		   | _ => abort "SetFrameElement"

  exception V_Pap of closure * value list

  fun MakePap (Interpret,closure,early) = 
	  let val Closure(CCI(_,name,_,{frame_size=_,num_args=N}),_) = closure
		  val tag = name ^ ":" ^ stringOfInt (length early) ^ "/" ^ stringOfInt N
		  fun Decompile() = layVert [Word "Pap:" @@ Word tag,
									 Word "Closure:" @@ lay'closure closure,
									 Word "Early:" @@ layArgs lay'value early]
		  fun NewExportStage2 cid (V,EmitDef,EmitAct) = 
			  cexp'pap V EmitDef EmitAct name cid closure early
		  fun NewExportC () = let val cid = newCid ("pap_"^name)
							  in (cid, NewExportStage2 cid)
							  end
		  fun Apply v = Interpret(closure,early@[v]) (* for use by interpreter *)
	  in value'exn ("pap:"^tag, V_Pap (closure,early), Decompile, NewExportC, Apply)
	  end

(*----------------------------------------------------------------------
INDEX: control
----------------------------------------------------------------------*)

  local fun loop xs 0 ys = (rev xs, ys)
		  | loop xs N [] = abort "splitArgs"
		  | loop xs N (y::ys) = loop (y::xs) (N-1) ys
  in fun splitArgs N ys = loop [] N ys
  end


	datatype continue = CONTINUE of code * frame | OVERAPP of value list

local

	datatype cont = CONT of continue | HANDLE of code * frame * int
	datatype control = CONTROL of {D: int ref, C: cont list ref, H: int ref}

in

  type control = control 

  type jump = (control * code * env -> res)

  fun newControl() = CONTROL {D = ref 1, C = ref [], H = ref 0}

  fun depth'control (CONTROL{D,C,H}) = !D

  val PushContinue : (control * continue -> unit) =
	  fn (CONTROL{D,C,H},continue) => (inc D; C := CONT continue :: !C)

  val PushHandler : (control * int * code -> unit) =
	  fn (CONTROL{D,C,H},N,code) => (C := HANDLE (code,NewFrame N,!H) :: !C; H := !D; inc D)

  val SetContFrameElement : (control * int * value -> unit) =
	  fn (CONTROL{D,C,H},n,v) =>
		 case (!C) of CONT(CONTINUE(_,frame))::_ => SetFrame (frame,n,v)
					| _ => abort "SetContFrameElement"
												   
  val SetXcontFrameElement : (control * int * value -> unit) =
	  fn (CONTROL{D,C,H},n,v) =>
		 case (!C) of HANDLE(_,frame,_)::_ => SetFrame (frame,n,v)
					| _ => abort "SetXcontFrameElement"


  fun PopCont (0,_)             = abort "PopCont/0"
    | PopCont (_,[])            = abort "PopCont/FINISH"
    | PopCont (1,HANDLE p::xs)  = (xs,p)
    | PopCont (_,HANDLE _::_)   = abort "PopCont/POPH/!1"
    | PopCont (n,CONT _::xs)    = PopCont (n-1,xs)


  val Raise : (icounts * jump * control * value -> res) =
	  fn (icounts,Jump,control as CONTROL{D,C,H},v) =>
		 if (!H=0) then That (exval'value "Raise" v) else
		 let val skip = !D - !H
			 val () = if (skip<0) then abort "Raise:skip<0" else ()
			 val () = count'raise skip icounts
			 val (conts,(code,frame,h)) = PopCont (skip,!C)
			 val d = !H (* new depth *)
		 in (H:=h; D:=d; C:=conts;
			 Jump (control, code, NewEnv (frame,{args=[v]})))
		 end
			
  val rec ReturnContinue : (icounts * control -> continue option) =
	  fn (icounts,control as CONTROL{D,C,H}) => (count'return (!D) icounts;
		 case (!C) of CONT(x)::conts => (dec D; C:=conts; Some x)
					| HANDLE(_,_,h)::conts => (H:=h; dec D; C:=conts; ReturnContinue (icounts,control))
					| [] => None)


end

		  
  val PushContinuation : (control * int * code -> unit) =
	  fn (control,N,code) => PushContinue (control,CONTINUE (code,NewFrame N))

  val PushReApp : (control * value list -> unit) =
	  fn (control,args) => PushContinue (control,OVERAPP args)

(*----------------------------------------------------------------------
INDEX: Return / Enter / TailCall
----------------------------------------------------------------------*)

  val rec Return : (icounts * jump * control * value -> res) =
	  fn (icounts,Jump,control,v) =>
		 case (ReturnContinue (icounts,control))
		  of None => This v
		   | Some x => case x of
						   CONTINUE (code,frame)  => Jump (control, code, NewEnv (frame,{args=[v]}))
						 | OVERAPP (args)         => TailCall (icounts,Jump,control,v,args)

  and Interpret : (icounts * jump) -> (closure * value list -> res) =
	  fn (icounts,Jump) =>
		 fn (closure,args) => Enter (icounts,Jump,newControl(),closure,args)

  and Enter : (icounts * jump * control * closure * value list -> res) =
	  fn (icounts,Jump,control,closure as Closure(CCI(code,name,_,{frame_size=_,num_args=N}),frame),args) =>
		 let val () = if !trace_tailcalls then echo (prefixNML ^ spaces (depth'control control)^name) else ()
			 val n_supplied = length args
		 in if (n_supplied < N) 
			then let val () = count'pap icounts
					 val pap = MakePap (Interpret (icounts,Jump),closure,args)
				 in Return (icounts,Jump,control,pap)
				 end
			else let val args = if (n_supplied = N) then args else
								let val (args1,args2) = splitArgs N args
									val () = count'reapp icounts
									val () = PushReApp (control,args2)
								in args1
								end
				 in Jump (control, code, NewEnv (frame,{args=args}))
				 end
		 end

  and TailCall : (icounts * jump * control * value * value list -> res) =
	  fn (icounts,Jump,control,func,args) => (count'tail icounts (length args);
		 case (exn'value func) 
		  of Some (V_Closure closure) => Enter (icounts,Jump,control,closure,args)
		   | Some (V_Pap (closure,early)) => Enter (icounts,Jump,control,closure,early @ args)
		   | _ => 
			 (*  intepreted closures, builtins, :=/ref/con0/excon0 application *)
			 let 
				 val () = if !trace_tailcalls then echo (prefixNML 
														 ^ spaces (depth'control control)
														 ^"**funcValue:"
														 ^funcName'value func) else ()
				 fun loop v [] = Return (icounts,Jump,control,v)
				   | loop f (x::xs) = (case (apply'value f x) of
										   This f => loop f xs
										 | That exval => Raise (icounts,Jump,control,value'exval exval))
			 in loop func args
			 end)

(*----------------------------------------------------------------------
INDEX: execute
----------------------------------------------------------------------*)

  val executeIm : (icounts * env * im -> bool) = 
	  fn (icounts,env,im) => (count'im icounts im; 
		 case im
          of IM'MatchScon (r,scon)                      => MatchScon (scon,            GetLoc (env,r))
           | IM'MatchC (r,con)                          => MatchC    (con,             GetLoc (env,r))
           | IM'MatchE (r,exname)                       => MatchE    (exname,          GetLoc (env,r))
           | IM'MatchG (r1,r2)                          => MatchG    (GetLoc (env,r1), GetLoc (env,r2)))

  val executeInstruction : (icounts * interpret * control * env * instruction -> unit) = 
	  fn (icounts,I,control,env,instruction) => (count'instruction icounts instruction;
		 case instruction
          of INS'NewTuple (q,{tuple_size})              => SetStackLoc (env,q,NewTuple tuple_size)
           | INS'NewException (q,vid)                   => SetStackLoc (env,q,NewException vid)
           | INS'NewFn (q,sizes,code,info)				=> SetStackLoc (env,q,NewFn (I,sizes,code,info))
		   | INS'NewRef (q,r)							=> SetStackLoc (env,q,NewRef (GetLoc (env,r)))
		   | INS'NewCon (q,con,r)						=> SetStackLoc (env,q,NewCon (con,GetLoc (env,r)))
           | INS'Copy (q,r)                             => SetStackLoc (env,q,GetLoc (env,r))
           | INS'DeCon (q,r)                            => SetStackLoc (env,q,DeCon (GetLoc (env,r)))
           | INS'DeExcon(q,r)                           => SetStackLoc (env,q,DeExcon (GetLoc (env,r)))
           | INS'DeRef (q,r)                            => SetStackLoc (env,q,DeRef (GetLoc (env,r)))
           | INS'DeTuple (q,r,n)                        => SetStackLoc (env,q,DeTuple (GetLoc (env,r),n))
           | INS'BuiltinApp (q,bi,rs)                   => SetStackLoc (env,q,BuiltinApp (bi,GetLocs(env,rs)))
           | INS'PushContinuation ({frame_size},code,_) => PushContinuation (control,frame_size,code)
           | INS'PushHandler ({frame_size},code,_)      => PushHandler (control,frame_size,code)
           | INS'SetTupleElement (r1,n,r2)              => SetTupleElement(GetLoc(env,r1),n,GetLoc(env,r2))
           | INS'SetFrameElement (r1,n,r2)              => SetFrameElement(GetLoc(env,r1),n,GetLoc(env,r2))
           | INS'SetContFrameElem (n,r2)                => SetContFrameElement(control,n,GetLoc(env,r2))
           | INS'SetXcontFrameElem (n,r2)               => SetXcontFrameElement(control,n,GetLoc(env,r2)))


  val executeCode : (icounts * jump * control * env * code  -> res) =
	  fn (icounts,J,control,env,code) =>
		 let
			 fun doCode (code,goto) = (count'code icounts code;
				 case code
				  of CODE'Seq (ins,code)        => (executeInstruction (icounts,Interpret(icounts,J),control,env,ins);
													doCode (code,goto))
				   | CODE'Return r              => Return (icounts,J,control,GetLoc(env,r))
				   | CODE'TailCall (func,args)  => TailCall (icounts,J,control,GetLoc(env,func),GetLocs(env,args))
				   | CODE'Raise r               => Raise (icounts,J,control,GetLoc(env,r))
				   | CODE'If (im,code1,code2)	=> if (executeIm (icounts,env,im))
												   then doCode (code1,goto)
												   else doCode (code2,goto)
				   | CODE'Where (code1,L,code2) => let fun goto'(lab) =
														   if (lab=L) then doCode (code2,goto)
														   else goto(lab)
												   in doCode (code1,goto')
												   end
				   | CODE'Goto lab				=> goto lab)
			 fun goto(lab) = abort ("goto/"^sof'label lab)
		 in
			 doCode (code,goto)
		 end
														
  fun echo'lay lay x =
	  (prefixed_output_layout prefixNML (squash_to wrapCol (lay x));
	   echo "")

  fun eval'code code =
	  let val icounts = new'icounts()
		  fun Jump (control,code,env) = executeCode (icounts,Jump,control,env,code)
		  val control = newControl()
		  val env = NewEnv (NewFrame 0,{args=[]})
		  val res = Jump (control,code,env)
		  val () = if !display_counts then (echo'lay lay'icounts icounts) else ()
	  in res
	  end

end (* functor *)
