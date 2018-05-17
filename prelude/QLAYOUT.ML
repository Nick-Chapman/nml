(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  18.	 export
  47.	 imports
  63.	 ChoicesSig
  76.	 functor
  85.	 nodeState, tokenTree (tt)
  98.	 emit
  130.	 choices -- greedy
  174.	 squash
  253.	 layout
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)
	  
(* Sat Apr	8 21:33:41 2000 : New Layout stuff... *)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature QLayoutSig = sig
  type layout
  val Word			: string -> layout
  val Left			: string -> layout
  val Right			: string -> layout
  val Sep			: string -> layout
  val Blank			: layout
  val Then			: int -> layout -> layout -> layout
  val Glue			: layout -> layout -> layout
  val Above			: layout -> layout -> layout
  val AboveTab		: int -> layout -> layout -> layout
  val squash_to		: int -> layout -> layout
  val show_layout	: layout -> string
  val output_layout : layout -> unit
  val stream_layout : TextIO.outstream -> layout -> unit
  val prefixed_output_layout : string -> layout -> unit
  val prefixed_stream_layout : string -> TextIO.outstream -> layout -> unit
end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig Prel.ML *)
signature PrelSig_Import =
	sig
		val abort			: string -> 'a
		val old_implode		: string list -> string
		val spaces			: int -> string
		val max				: int -> int -> int
	end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: ChoicesSig
----------------------------------------------------------------------*)

signature ChoicesSig =
	sig
		type choices
		val emptyChoices : choices
		val pushChoice : {saving:int} -> choices -> choices
		val popChoice : choices -> ({broken:bool} * choices)
		val makeBreaks : choices -> {goal:int} -> ({saved:int} * choices)
	end

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor QLAYOUT(structure Prel : PrelSig_Import) : QLayoutSig =
struct

  open Prel

(*----------------------------------------------------------------------
INDEX: nodeState, tokenTree (tt)
----------------------------------------------------------------------*)

  datatype nodeState =
	  NS'Glue of {jux:bool}
	| NS'MayBreak of {jux:bool, nest:int}
	| NS'Broken of {nest:int}

  datatype tt =
	  TT'Leaf of string
	| TT'Node of tt * nodeState	 * tt

(*----------------------------------------------------------------------
INDEX: emit
----------------------------------------------------------------------*)

  fun emitTT (EMIT:string->unit,NL:string) tt =
	  let
		  fun emitJux cc jux = if jux then cc else (EMIT (" "); cc+1)
		  fun emit cc tt =
			  case tt of
				  TT'Leaf s => (EMIT s; cc + size s)
				| TT'Node (left,nodeState,right) =>
					  let
						  val ccLeft = cc
						  val cc = emit cc left
						  val cc = case nodeState of
									   NS'Glue {jux} => emitJux cc jux
									 | NS'MayBreak {jux,nest} => emitJux cc jux
									 | NS'Broken {nest} =>
										   let val cc = ccLeft+nest
										   in (EMIT NL; EMIT (spaces cc); cc)
										   end
						  val cc = emit cc right
					  in cc
					  end
		  val cc = emit 0 tt
	  in ()
	  end;

(*----------------------------------------------------------------------
INDEX: choices -- greedy
----------------------------------------------------------------------*)

  structure Choices : ChoicesSig =
	  struct
		  datatype choices = CHOICES of int option list
		  val emptyChoices = CHOICES []
		  fun pushChoice {saving} (CHOICES xs) = CHOICES (Some saving::xs)
		  val isNone = fn Some _ => false | None => true
		  fun popChoice (CHOICES []) = abort "popChoice[]"
			| popChoice (CHOICES (x::xs)) = ({broken=isNone x},CHOICES xs)
		  local
			  fun max_iopts acc [] = acc
				| max_iopts acc (None::xs) = max_iopts acc xs
				| max_iopts acc (Some x::xs) = max_iopts (max x acc) xs
			  fun noneify _ [] = abort"noneify:[]"
				| noneify best (None::xs) = None :: noneify best xs
				| noneify best ((iopt as Some x)::xs) =
				  if x<>best then iopt :: noneify best xs
							 else None :: xs
		  in
			  fun makeOneBreak (CHOICES xs) {goal} =
				  let val best = max_iopts 0 xs
				  in if best=0 then ({saved=0},(CHOICES xs))
							   else
								   let val xs' = rev (noneify best (rev xs))
								   in ({saved=best},(CHOICES xs'))
								   end
				  end
		  end
		  fun makeBreaks choices {goal} =
			  let fun loop {accSaved} choices =
					  let val ({saved},choices) = makeOneBreak choices {goal=goal}
						  val accSaved = accSaved+saved
					  in if (saved=0) orelse (accSaved>=goal)
						 then ({saved=accSaved},choices)
						 else loop {accSaved=accSaved} choices
					  end
			  in loop {accSaved=0} choices
			  end
	  end

(*----------------------------------------------------------------------
INDEX: squash
----------------------------------------------------------------------*)

  open Choices

  fun withChoice {saving:int} choices (f:choices -> (choices * 'b)) =
	  let val choices = pushChoice {saving=saving} choices
		  val (choices,x) = f choices
		  val ({broken},choices) = popChoice choices
	  in ({broken=broken},choices,x)
	  end

  fun withoutChoice (choices:choices) f =
	  let val (choices:choices,x) = f choices
	  in ({broken=false},choices,x)
	  end

  fun squashTT max cc choices tt =
	  case tt of
		  TT'Leaf s =>
			  let
				  val w = size s
				  val cc = cc+w
				  val (cc,choices) =
					  if (cc<=max)
					  then (cc,choices)
					  else let val ({saved},choices) =
								   makeBreaks choices {goal=(cc-max)}
						   in (cc-saved,choices)
						   end
			  in (w,cc,choices,tt)
			  end
		| TT'Node (left,nodeState as NS'Glue{jux},right) =>
			  let
				  val (wLeft,cc,choices,left') = squashTT max cc choices left
				  val iSpace = if jux then 0 else 1
				  val cc = cc+iSpace
				  val (wRight,cc,choices,right') = squashTT max cc choices right
				  val w = wLeft+iSpace+wRight
				  val tt' = TT'Node(left',nodeState,right')
			  in (w,cc,choices,tt')
			  end
		| TT'Node (left,nodeState as NS'Broken{nest},right) =>
			  let
				  val (wLeft,cc,choices,left') = squashTT max cc choices left
				  val cc = (cc-wLeft)+nest
				  val (wRight,cc,choices,right') = squashTT max cc choices right
				  val w = nest+wRight
				  val tt' = TT'Node(left',nodeState,right')
			  in (w,cc,choices,tt')
			  end
		| TT'Node (left,nodeState as NS'MayBreak{jux,nest},right) =>
			  let
				  val (wLeft,cc,choices,left') = squashTT max cc choices left
				  val iSpace = if jux then 0 else 1
				  val cc = cc+iSpace
				  val saving = wLeft+iSpace-nest
				  val ({broken},choices,(wRight,cc,right')) =
					  (if (saving<=0) then withoutChoice
									  else withChoice {saving=saving})
					  choices
					  (fn choices =>
					   let val (wRight,cc,choices,right') = squashTT max cc choices right
					   in (choices,(wRight,cc,right'))
					   end)
				  val (w,nodeState') =
					  if broken
					  then (nest+wRight, NS'Broken {nest=nest})
					  else (wLeft+iSpace+wRight, nodeState)
				  val tt' = TT'Node(left',nodeState',right')
			  in (w,cc,choices,tt')
			  end

  fun top_squashTT max tt =
	  let val (_,_,_,tt') = squashTT max 0 emptyChoices tt
	  in tt'
	  end

(*----------------------------------------------------------------------
INDEX: layout
----------------------------------------------------------------------*)

  type jux = bool
  datatype layout = Layout of tt * jux * jux | Blank
  val deLayout = fn (Layout (x,_,_)) => x | Blank => TT'Leaf ""

  fun Word x	= Layout (TT'Leaf x, false, false)
  fun Left x	= Layout (TT'Leaf x, false, true )
  fun Right x	= Layout (TT'Leaf x, true , false)
  fun Sep x		= Layout (TT'Leaf x, true , true )

  fun block _ _ Blank Blank = Blank
	| block _ _ x Blank = x
	| block _ _ Blank x = x
	| block mkNode mkNodeState (Layout(tt1,juxL1,juxR1)) (Layout(tt2,juxL2,juxR2)) =
	  Layout (mkNode (tt1,
					  mkNodeState {jux= juxR1 orelse juxL2},
					  tt2),
			  juxL1,
			  juxR2)

  fun HangRight (L,ns,R) =
      case L of
		  TT'Leaf _ => TT'Node (L,ns,R)
		| TT'Node (LL,Lns,LR) => TT'Node (LL,Lns, HangRight (LR,ns,R))


  val Glue        = block HangRight (fn {jux} => NS'Glue {jux=jux})
  fun Then i      = block TT'Node (fn {jux} => NS'MayBreak {jux=jux,nest=i})
  val Above       = block TT'Node (fn {jux} => NS'Broken {nest=0})
  fun AboveTab v  = block TT'Node (fn {jux} => NS'Broken {nest=v})


  fun squash_to max layout =
	  case layout of
		  Blank => Blank
		| Layout (tt,juxL,juxR) => Layout (top_squashTT max tt, juxL, juxR)
	  
  fun show_layout layout =
	  case layout of
		  Blank => ""
		| Layout (tt,juxL,juxR) =>
		  let val xs : string list ref = ref []
			  fun EMIT x = (xs := x::(!xs))
		  in (emitTT (EMIT,"\n") tt; old_implode (rev (!xs)))
		  end
	  
  fun prefixed_stream_layout prefix os layout =
	  let val NL = "\n"^prefix
		  fun EMIT x = TextIO.output(os,x)
	  in (EMIT prefix;
		  case layout of
			  Blank => ()
			| Layout (tt,juxL,juxR) => emitTT (EMIT,NL) tt)
	  end

  fun prefixed_output_layout prefix = prefixed_stream_layout prefix TextIO.stdOut

  val stream_layout = prefixed_stream_layout ""
  val output_layout = stream_layout TextIO.stdOut;

	  
end (* functor *)
