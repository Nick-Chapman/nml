(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  21.    export
  49.    import
  62.    functor
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(* Based on ~/per/Abstract/pelican/Prelude/BasicPar/PARSER4V.ML
 (RCS version 1.16)
 This version generalizes text/char --> lexState/tok
 27/3/00 -- functorized.
 *)


(* Mon Sep 25 13:50:16 2006
rename: PFV -> PAR1
*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature ParSig =
sig

	type 'a par

	val resultP		: 'a -> 'a par	 
	val failP		: 'a par
	val bindP		: 'a par * ('a -> 'b par) -> 'b par	  
	val altP		: 'a par * 'a par -> 'a par	  
	val fixP		: ('a par -> 'a par) -> 'a par	 
	val manyP		: 'a par -> 'a list par
	val noErrorP	: 'a par -> 'a par

	type pos
	val positionP	: pos par

	type tok
	val satisfyP	: string -> (tok -> 'a option) -> 'a par

	type lexState
	val execP		: 'a par -> lexState -> lexState * 'a option

end;

(*----------------------------------------------------------------------
INDEX: import
----------------------------------------------------------------------*)

signature LexSig_Import =
	sig
		type pos
		type lexState
		type tok
		val stepLS : string -> lexState -> (tok * lexState) option
		val posLS : lexState -> pos
	end;

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor PAR1(structure Lex : LexSig_Import) : ParSig =
struct

  type pos = Lex.pos
  type tok = Lex.tok
  type lexState = Lex.lexState

  structure Lex = 
  struct open Lex
		 fun initCaching x = x
		 fun endCaching x = x
  end

  structure LexCaching  =
  struct
	abstype lexState =
			CLS of Lex.lexState * {res : (tok * lexState) option
											  } option ref
	with
			fun initCaching ls = CLS(ls,ref None)
			fun endCaching (CLS(ls,_)) = ls
			fun posLS (CLS(ls,_)) = Lex.posLS ls
			fun stepLS who (CLS(ls,r)) =
				case (!r) of
					Some {res} => res
				  | None =>
					let val res = case (Lex.stepLS who ls) of
									  Some (tok,ls) => Some (tok, initCaching ls)
									| None => None
					in (r := Some {res = res};
						res)
					end
	end
  end

  structure Lex = LexCaching (* use caching *)


  datatype 'a result =
	  RES'Succ	  of Lex.lexState * 'a
	| RES'Epsilon of 'a
	| RES'Error	  of Lex.lexState
	| RES'Fail
	  
  datatype 'a sofar =
	  SOFAR'Epsilon of 'a
	| SOFAR'Fail
	| SOFAR'Succ of 'a
	| SOFAR'Error

  datatype 'a par = Parser of Lex.lexState * 'a sofar -> 'a result

  fun deParser (Parser f) = f

  fun fixP_v1 f =
	  let fun fix ls = deParser (f (Parser fix)) ls
	  in Parser fix
	  end

  exception FixP	
  fun fixP f =
	  let val fRef = ref (fn _ => raise FixP)
		  val p = Parser (fn x => (!fRef) x)
		  val p' = f p
		  val (U:unit) = (fRef := deParser p')
	  in p'
	  end

  fun restore ls sofar =
	  case sofar of
		  SOFAR'Epsilon x => RES'Epsilon x
		| SOFAR'Fail	  => RES'Fail
		| SOFAR'Succ x	  => RES'Succ (ls, x)
		| SOFAR'Error	  => RES'Error ls

  fun makeResult sofar ls x =
	  case sofar of
		  SOFAR'Epsilon _ => RES'Epsilon x
		| SOFAR'Fail	  => RES'Epsilon x
		| SOFAR'Succ _	  => RES'Succ (ls,x)
		| SOFAR'Error	  => RES'Succ (ls,x)

  fun satisfyP who f =
	  Parser (fn (ls,sofar) =>
			  case (Lex.stepLS who ls) of
				  None => restore ls sofar
				| Some (tok,ls') =>
					  (case (let val xopt = f tok
								 (*val mes = "satisfyP("^who^"):"^sofTok tok
								 val () = echo
									 (case xopt of
										 None => mes^"-->no"
									   | Some _ => mes^"-->Y")*)
							 in xopt
							 end
							 ) of
						   None => restore ls sofar
						 | Some x => RES'Succ (ls',x)))

  val failP =
	  Parser (fn (ls,sofar) => restore ls sofar)

  fun resultP x =
	  Parser (fn (ls,sofar) => makeResult sofar ls x)

  val positionP =
	  Parser (fn (ls,sofar) => makeResult sofar ls (Lex.posLS ls))

  fun addConsume sofar =
	  case sofar of
		  SOFAR'Epsilon x => SOFAR'Succ x
		| SOFAR'Fail	  => SOFAR'Error
		| SOFAR'Succ x	  => SOFAR'Succ x
		| SOFAR'Error	  => SOFAR'Error

  fun bindP (par,f) =
	  Parser (fn (ls,sofar) =>
			  case (deParser par (ls, SOFAR'Fail)) of
				  RES'Epsilon x		=> deParser (f x) (ls, sofar)
				| RES'Succ (ls,x)	=> deParser (f x) (ls, addConsume sofar)
				| RES'Fail			=> restore ls sofar
				| RES'Error ls		=> RES'Error ls)

  fun checkpoint sofar = (* remove consumption *)
	  case sofar of
		  SOFAR'Epsilon x => SOFAR'Epsilon x
		| SOFAR'Fail	  => SOFAR'Fail
		| SOFAR'Succ x	  => SOFAR'Epsilon x
		| SOFAR'Error	  => SOFAR'Fail

  fun addAlt sofar x =
	  case sofar of
		  SOFAR'Epsilon _ => SOFAR'Epsilon x
		| SOFAR'Fail	  => SOFAR'Epsilon x
		| SOFAR'Succ _	  => SOFAR'Succ x
		| SOFAR'Error	  => SOFAR'Succ x

  fun altP (par1,par2) =
	  Parser (fn (ls,sofar) =>
			  case (deParser par1 (ls, checkpoint sofar))  of
				  RES'Fail			  => deParser par2 (ls, sofar)
				| RES'Epsilon x		  => deParser par2 (ls, addAlt sofar x)
				| x as RES'Succ _	  => x
				| x as RES'Error _	  => x)
	  

  fun acc_manyP acc par ls =
	  (* Note: in a non-primitive defintion, the epsilon case would cause non-termination *)
	  case (deParser par (ls, SOFAR'Fail)) of
		  RES'Succ (ls,x)	=> acc_manyP (x :: acc) par ls
		| RES'Epsilon x		=> RES'Succ (ls, rev (x :: acc))
		| RES'Fail			=> RES'Succ (ls, rev acc)
		| RES'Error e		=> RES'Error e

  fun manyP par =
	  Parser (fn (ls,sofar) =>
			  case (deParser par (ls, SOFAR'Fail)) of
				  RES'Succ (ls,x)	=> acc_manyP [x] par ls
				| RES'Epsilon x		=> makeResult sofar ls [x]
				| RES'Fail			=> makeResult sofar ls []
				| RES'Error e		=> RES'Error e)

  fun noErrorP par =
	  Parser (fn x =>
			  case (deParser par x) of
				  RES'Error _		 => RES'Fail
				| x as RES'Fail		 => x
				| x as RES'Succ _	 => x
				| x as RES'Epsilon _ => x)

  fun execP par ls =
	  case (deParser par (ls, SOFAR'Fail)) of
		  RES'Succ (ls,x)	=> (ls, Some x)
		| RES'Epsilon x		=> (ls, Some x)
		| RES'Fail			=> (ls, None)
		| RES'Error ls		=> (ls, None)

  val execP = fn par => fn ls =>
	  let val (cls,x) = execP par (Lex.initCaching ls)
	  in (Lex.endCaching cls,x)
	  end
			  
end
