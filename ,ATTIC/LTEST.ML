(*------------------------------------------------------------------------------ CONTENTS-START-LINE: HERE=2 SEP=1
  10.    export
  21.    imports
  78.    functor
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature LtestSig =
    sig
	val tl : {IN:string, OUT:string} -> unit
    end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig Prel.ML *)
signature PrelSig_Import =
    sig
        val stringOfInt     : int -> string
    end;
(*IMPORTDONE:*)

(*IMPORTSIG: TokSig TOK.ML *)
signature TokSig_Import =
sig
    type tok;
    val sofTok : tok -> string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: LexSig LEX.ML *)
signature LexSig_Import =
    sig
	type 'a llist
	val iterateLL : ('a -> ('b * 'a) option) -> 'a -> 'b llist
	val appLL : ('a -> unit) -> 'a llist -> unit
	val lazyFile : {path:string} -> string llist
	datatype pos = POS of {row: int, col: int}
	type lexState
	type tok
	val initLS : pos -> string llist -> lexState
	val stepLS : lexState -> (tok * lexState) option
	val posLS : lexState -> pos
    end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor LTEST(structure Prel : PrelSig_Import
	      structure Tok : TokSig_Import
	      structure Lex : LexSig_Import
	      sharing type Tok.tok = Lex.tok
		  ) : LtestSig =
struct

    open Prel
    open Lex

    fun sofPos (Lex.POS{row,col}) =
	stringOfInt row ^ ":" ^ stringOfInt col

    fun printPosTok os (p,tok) =
	TextIO.output (os,"(*"^sofPos p^"--*)"^Tok.sofTok tok^"\n")

    fun stepLSP ls =
	case (Lex.stepLS ls) of
	    Some (tok,ls') => Some ((Lex.posLS ls', tok), ls')
	  | None => None

    fun tl {IN,OUT} =
	let val ll_string = lazyFile {path=IN}
	    val ls = Lex.initLS (Lex.POS{row=1,col=0}) ll_string
	    val ll_posToken = iterateLL stepLSP ls
	    val os = TextIO.openOut OUT
	in (appLL (printPosTok os) ll_posToken; TextIO.closeOut os)
	end;
	
end
