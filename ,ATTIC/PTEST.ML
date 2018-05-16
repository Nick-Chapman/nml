(*------------------------------------------------------------------------------ CONTENTS-START-LINE: HERE=2 SEP=1
  10.    export
  21.    imports
  78.    functor
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature PtestSig =
    sig
	type pos
	type topdec
	val tp : string -> pos * {eof:bool} * topdec list option
	type infixMap
	val theIM : infixMap
    end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig Prel.ML *)
signature PrelSig_Import =
    sig
    end;
(*IMPORTDONE:*)

(*IMPORTSIG: LexSig LEX.ML *)
signature LexSig_Import =
    sig
	datatype pos = POS of {row: int, col: int}
	type lexState
	type tok
	type 'a llist
	val initLS : pos -> string llist -> lexState
	val stepLS : lexState -> (tok * lexState) option
	val posLS : lexState -> pos
	val lazyFile : {path:string} -> string llist
    end;
(*IMPORTDONE:*)

(*IMPORTSIG: PfvSig PFV.ML *)
signature PfvSig_Import =
sig
    type 'a par
    type tok
    type lexState
    val execP	    : 'a par -> lexState -> lexState * 'a option
end;
(*IMPORTDONE:*)

(*IMPORTSIG: LangSig LANG.ML *)
signature LangSig_Import =
sig
    datatype id = ID of string
    datatype vid = VID of id
end;
(*IMPORTDONE:*)

(*IMPORTSIG: ParserSig PARSER.ML *)
signature ParserSig_Import =
    sig
	datatype id = ID of string
	datatype vid = VID of id
	datatype leftOrRight = LR'Left | LR'Right
	datatype pa = PA of {assoc:leftOrRight, prec:int}
	type infixMap
	val makeIM : (vid * pa) list -> infixMap
	type topdec
	type 'a par
	val prog : infixMap -> topdec list par
    end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor PTEST(structure Prel : PrelSig_Import
	      structure Lex : LexSig_Import
	      structure Pfv : PfvSig_Import
	      structure Lang : LangSig_Import
	      structure Parser : ParserSig_Import
	      sharing type Lex.lexState = Pfv.lexState
	      sharing type Pfv.par = Parser.par
		  ) : PtestSig =
struct

    open Prel
    open Lex
    open Pfv
    open Lang
    open Parser
	
    val theIM = makeIM [
			(* initial basis... *)
			(VID (ID "::"), PA {prec=5, assoc=LR'Right}),
			(VID (ID "="),  PA {prec=4, assoc=LR'Left}),
			(VID (ID ":="), PA {prec=3, assoc=LR'Left}),
			(* other (adhoc) *)
			(VID (ID "^"), PA {prec=0, assoc=LR'Left}),
			(VID (ID "+"), PA {prec=0, assoc=LR'Left}),
			(VID (ID "-"), PA {prec=0, assoc=LR'Left})
			]

    fun tp IN =
	let val par = prog theIM
	    val ll_string = lazyFile {path=IN}
	    val ls = Lex.initLS (Lex.POS{row=1,col=0}) ll_string
	    val (ls',xOpt) = Pfv.execP par ls
	    val eof = case (stepLS ls') of None => true | Some _ => false
	val posOpt = Lex.posLS ls'
	in (posOpt,{eof=eof},xOpt)
	end;


end
