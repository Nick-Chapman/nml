(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  10.	 export
  25.	 imports
  113.	 functor
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature PosSig =
sig
	type pos
	val makePos : {path: string, row: int, col: int} -> pos
	val sofPos : pos -> string
	val stepPos : pos -> char -> pos
end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ../prelude/Prel.ML *)
signature PrelSig_Import =
sig
	val stringOfInt		: int -> string
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor POS(structure Prel : PrelSig_Import) : PosSig =
struct

  open Prel

  val TAB_WIDTH = 4
	  
  fun tabCol x = TAB_WIDTH * (1 + (x div TAB_WIDTH))
	  
  datatype pos = POS of {path: string, row: int, col: int}

  fun makePos {path,row,col} = POS {path=path,row=row,col=col}
						
  fun stepPos (POS{path,row,col}) c =
	  case c of
		  #"\n" => POS{path=path,row=row+1, col=0}
		| #"\t" => POS{path=path,row=row, col=tabCol(col)}
		| _ => POS{path=path,row=row,col=col+1}

  fun sofPos (POS{path,row,col}) =
	  path ^ ":" ^ stringOfInt row ^ "." ^ stringOfInt col

end
