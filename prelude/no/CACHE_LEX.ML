

signature LexSig =
	sig
		type pos
		type lexState
		type tok
		val stepLS : lexState -> (tok * lexState) option
		val posLS : lexState -> pos
	end;


functor CACHE_LEX(structure Inner : LexSig) >: LexSig =
struct

  abstype cachingLexState =
	  CLS of Lex.lexState * {res : (tok * cachingLexState) option
								   } option ref
  with
	  fun initCLS ls = CLS(ls,ref None)
	  fun posCLS (CLS(ls,_)) = Lex.posLS ls
	  fun endCLS (CLS(ls,_)) = ls
	  fun stepCLS (CLS(ls,r)) =
		  case (!r) of
			  Some {res} => res
			| None =>
				  let val res = case (Lex.stepLS ls) of
									Some (tok,ls) => Some (tok, initCLS ls)
								  | None => None
				  in (r := Some {res = res};
					  res)
				  end
  end

