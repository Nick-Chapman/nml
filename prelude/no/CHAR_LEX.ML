(*------------------------------------------------------------------------------
CONTENTS-START-LINE: HERE=2 SEP=1
 10.    export
 26.    imports
 37.    functor
CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*------------------------------------------------------------------------------
INDEX: export
------------------------------------------------------------------------------*)

signature CharLexSig =
sig
	type tok = char
	type lexState
	type pos
    val makeLS      : string -> lexState
    val stepLS      : lexState -> (tok * lexState) option
    val posLS       : lexState -> pos
    val sofTok      : tok -> string
    val stringOfPos : pos -> string     
end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig /home/nic/project/prelude/PREL.ML *)
signature PrelSig_Import =
sig
	val echo			: string -> unit
	val stringOfInt		: int -> string
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor CHAR_LEX (val quiet : bool ref
				  structure Prel : PrelSig_Import) : CharLexSig =
struct
	type tok = char
	fun sofTok c = implode [c]
	datatype pos = Pos of int * int
	(*fun stringOfPos (Pos(row,col)) = "line "^Prel.stringOfInt row^", column "^Prel.stringOfInt col*)
	fun stringOfPos (Pos(row,col)) = Prel.stringOfInt row^"."^Prel.stringOfInt col
	val pos0 = Pos (1,1)
	fun tabCol x = 1 + 8 * (1 + ((x-1) div 8))
	fun incPos (Pos(row,col)) = Pos(row,col+1)
	fun tabPos (Pos(row,col)) = Pos(row,tabCol col)
	fun newlinePos (Pos(row,col)) = Pos(row+1,1)
	datatype lexState = Text of {string : string,
								 size   : int,
								 index  : int,
								 charOpt: char option,
								 pos    : pos}
	fun makeLS string =
		let val size = size string
		in Text {string = string,
				 size   = size,
				 index  = 1,
				 charOpt= if size = 0 then None
						  else Some (String.sub(string,0)),
				 pos    = pos0}
		end
	fun posLS (Text{pos,...}) = pos
	fun stepLS (Text{string,size,index,charOpt,pos}) =
		case charOpt
		 of None => None
		  | Some char => Some (char, Text {string = string,
										   size   = size,
										   index  = index+1,
										   charOpt= if index >= size then None
													else Some (String.sub(string,index)),
										   pos    = (case char of
														 #"\n" => newlinePos pos
													   | #"\t" => tabPos pos
													   | _ => incPos pos)})

	val stepLS = 
		fn q => if !quiet then stepLS q else
		   let val res = stepLS q
			   val pos = posLS q
			   val () = Prel.echo (concat ["* CharLex.stepLS -> ",
										   case res of None => "<eof>" | Some (char,_) => implode [char],
										   " - ",
										   stringOfPos pos])
		   in res
		   end

end

