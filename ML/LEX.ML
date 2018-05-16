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

signature LexSig =
	sig
		type 'a llist
		val iterateLL : ('a -> ('b * 'a) option) -> 'a -> 'b llist
		val appLL : ('a -> unit) -> 'a llist -> unit
		val lazyFile : {path:string} -> string llist
		val lazyString : string -> string llist

		type pos
		type lexState
		type tok
		val initLS : pos -> string llist -> lexState
		val stepLS : string -> lexState -> (tok * lexState) option
		val posLS : lexState -> pos
	end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ../prelude/Prel.ML *)
signature PrelSig_Import =
sig
end;
(*IMPORTDONE:*)

(*IMPORTSIG: PosSig POS.ML *)
signature PosSig_Import =
sig
    type pos;
	val sofPos : pos -> string
	val stepPos : pos -> char -> pos
end;
(*IMPORTDONE:*)

(*IMPORTSIG: TokSig TOK.ML *)
signature TokSig_Import =
sig
	datatype tok =
		KEY_abstype
	  | KEY_and
	  | KEY_andalso
	  | KEY_as
	  | KEY_case
	  | KEY_datatype
	  | KEY_do
	  | KEY_else
	  | KEY_end
	  | KEY_eqtype
	  | KEY_exception
	  | KEY_fn
	  | KEY_fun
	  | KEY_functor
	  | KEY_handle
	  | KEY_if
	  | KEY_in
	  | KEY_include
	  | KEY_infix
	  | KEY_infixr
	  | KEY_let
	  | KEY_local
	  | KEY_nonfix
	  | KEY_of
	  | KEY_op
	  | KEY_open
	  | KEY_orelse
	  | KEY_raise
	  | KEY_rec
	  | KEY_sharing
	  | KEY_sig
	  | KEY_signature
	  | KEY_struct
	  | KEY_structure
	  | KEY_then
	  | KEY_type
	  | KEY_val
	  | KEY_where
	  | KEY_while
	  | KEY_with
	  | KEY_withtype
	  | SYM_COLON
	  | SYM_COLON_GT
	  | SYM_COMMA
	  | SYM_DOT
	  | SYM_DOTS
	  | SYM_EQ
	  | SYM_FUNC_ARROW
	  | SYM_HASH
	  | SYM_HASH_LS
	  | SYM_LB
	  | SYM_LP
	  | SYM_LS
	  | SYM_RB
	  | SYM_RP
	  | SYM_RS
	  | SYM_SEMI
	  | SYM_STAR
	  | SYM_TYPE_ARROW
	  | SYM_UNDERSCORE
	  | SYM_VBAR
	  | TOK'CharLit		of char
	  | TOK'Err			of string
	  | TOK'Ident		of string
	  | TOK'Number		of int
	  | TOK'StringLit	of string
	  | TOK'Symid		of string
	  | TOK'TyVar		of string
	  | TOK'Unknown		of char
	  | TOK'Word		of word
	val sofTok : tok -> string
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor LEX(structure Prel : PrelSig_Import
			val trace_lex : bool ref
			structure Pos : PosSig_Import
			structure Tok : TokSig_Import) : LexSig =
struct

  open Prel Pos Tok

  val TAB_WIDTH = 4
	  
  fun tabCol x = TAB_WIDTH * (1 + (x div TAB_WIDTH))
	  
  local
	  datatype 'a lazyVal = Thunk of unit -> 'a | Value of 'a
  in
	  abstype 'a lazy = Lazy of 'a lazyVal ref
	  with
		  fun lazy v = Lazy (ref (Value v))
		  fun delay thunk = Lazy (ref (Thunk thunk))
		  fun force (Lazy r) =
			  case (!r) of
				  Thunk thunk => let val v = thunk()
								 in r := Value v; v
								 end
				| Value v => v
	  end
  end


  datatype 'a llist = LNil | LCons of 'a * 'a llist lazy

  val rec iterateLL : ('a -> ('b * 'a) option) -> 'a -> 'b llist
	  = fn f => fn a =>
	  case f a of
		  None => LNil
		| Some (b,a) => LCons (b, delay (fn () => iterateLL f a))
		  
  val rec appLL : ('a -> unit) -> 'a llist -> unit
	  = fn f => fn ll =>
	  case ll of
		  LNil => ()
		| LCons (x,ll) => (f x; appLL f (force ll))

  val FILE_CHUNK = 1000 (* whats a good number for this? (Fri May  5 15:53:58 2000) *)
  fun lazyFile {path} =
	  let open TextIO
		  val instream = openIn path
		  fun get() =
			  let val s : string = inputN (instream,FILE_CHUNK)
			  in if s = "" then (closeIn instream; LNil)
						   else LCons (s, delay get)
			  end
	  in
		  get()
	  end

  fun lazyString string = LCons (string, delay (fn () => LNil))
  
  abstype lexState =  LS of pos * int * string * int * string llist lazy
  with
	  fun initLS pos ll = LS (pos,0,"",0,lazy ll)
	  fun posLS (LS(pos,_,_,_,_)) = pos
	  fun nextLS (LS(pos,n,s,z,lll)) =
		  if n >= z
		  then case (force lll) of
				   LNil => None
				 | LCons (s,lll) => nextLS (LS(pos,0,s,size s,lll))
		  else let val c = String.sub(s,n)
				   val pos' = stepPos pos c
				   val ls' = LS(pos',n+1,s,z,lll)
			   in Some (c,ls')
			   end
  end


  fun nextChar ls =
	  case (nextLS ls) of
		  None => None
		| Some (c,_) => Some c

  fun next2Chars ls =
	  case (nextLS ls) of
		  None => None
		| Some (c1,ls) =>
			  case (nextLS ls) of
				  None => None
				| Some (c2,_) =>
					  Some (c1,c2)

					  
  fun isAlpha c =
	  let (*open Char*) val op >= = Char.>= val op <= = Char.<=
	  in (c >= #"A" andalso c <= #"Z") orelse
		  (c >= #"a" andalso c <= #"z")
		  end

  fun isDigit c =
	  let (*open Char*) val op >= = Char.>= val op <= = Char.<=
	  in (c >= #"0" andalso c <= #"9")
	  end

  fun isWhite c =
	  let (*open Char*) val op >= = Char.>= val op <= = Char.<=
	  in c = #" " orelse c = #"\t" orelse c = #"\n"
		  orelse c = #"\^M"
	  end

  fun isIdent c = isAlpha c orelse isDigit c orelse c = #"'" orelse c = #"_"

  fun mkIdentOrKey s =
	  case s of
		"abstype"	=> KEY_abstype
	  | "and"		=> KEY_and
	  | "andalso"	=> KEY_andalso
	  | "as"		=> KEY_as
	  | "case"		=> KEY_case
	  | "do"		=> KEY_do
	  | "datatype"	=> KEY_datatype
	  | "else"		=> KEY_else
	  | "end"		=> KEY_end
	  | "exception" => KEY_exception
	  | "fn"		=> KEY_fn
	  | "fun"		=> KEY_fun
	  | "handle"	=> KEY_handle
	  | "if"		=> KEY_if
	  | "in"		=> KEY_in
	  | "infix"		=> KEY_infix
	  | "infixr"	=> KEY_infixr
	  | "let"		=> KEY_let
	  | "local"		=> KEY_local
	  | "nonfix"	=> KEY_nonfix
	  | "of"		=> KEY_of
	  | "op"		=> KEY_op
	  | "open"		=> KEY_open
	  | "orelse"	=> KEY_orelse
	  | "raise"		=> KEY_raise
	  | "rec"		=> KEY_rec
	  | "then"		=> KEY_then
	  | "type"		=> KEY_type
	  | "val"		=> KEY_val
	  | "with"		=> KEY_with
	  | "withtype"	=> KEY_withtype
	  | "while"		=> KEY_while
	  (* module level keywords *)
	  | "eqtype"	=> KEY_eqtype	 
	  | "functor"	=> KEY_functor	 
	  | "include"	=> KEY_include	 
	  | "sharing"	=> KEY_sharing	 
	  | "sig"		=> KEY_sig		 
	  | "signature" => KEY_signature 
	  | "struct"	=> KEY_struct	 
	  | "structure" => KEY_structure 
	  | "where"		=> KEY_where
	  (* identifier *)
	  | _			=> TOK'Ident s


		(* Wed Mar 21 20:34:59 2001 -- why not do case analysis in the following function?
																	   -- surely much faster!*)
  fun isSymid c =
			 c = #"!" orelse c = #"%" orelse c = #"&" orelse c = #"$"
	  orelse c = #"#" orelse c = #"+" orelse c = #"-" orelse c = #"/"
	  orelse c = #":" orelse c = #"<" orelse c = #"=" orelse c = #">"
	  orelse c = #"?" orelse c = #"@" orelse c = #"\\" orelse c = #"~"
	  orelse c = #"`" orelse c = #"^" orelse c = #"|" orelse c = #"*"
			
  fun mkIdentOrSym s =
	  case s of
		  ":"  => SYM_COLON
		| "|"  => SYM_VBAR
		| "="  => SYM_EQ
		| "#"  => SYM_HASH
		| "*"  => SYM_STAR
		| "=>" => SYM_FUNC_ARROW
		| "->" => SYM_TYPE_ARROW
		| ":>" => SYM_COLON_GT	
		| _	   => TOK'Symid s
		  
			
  abstype lexer = LEXER of lexState -> (tok * lexState) option
  with
	  fun execLexer (LEXER x) = x
	  val fixLexer : (lexer -> lexer) -> lexer
		  = fn f =>
		  let val rec lexer = (fn ls => execLexer (f (LEXER lexer)) ls) (* PROBLEM *)
		  (*let fun lexer ls = execLexer (f (LEXER lexer)) ls*) (* WORKS*)
		  in LEXER lexer
		  end
	  fun emit tok = LEXER (fn ls => Some (tok,ls))
	  val stop = LEXER (fn _ => None)
	  val getOpt : (char option -> lexer) -> lexer
		  = fn f =>
		  LEXER (fn ls =>
				 case (nextLS ls) of
					 None => execLexer (f None) ls
				   | Some (c,ls') => execLexer (f (Some c)) ls')
	  val peekOpt : (char option -> lexer) -> lexer
		  = fn f =>
		  LEXER (fn ls => execLexer (f (nextChar ls)) ls)
	  val peekOpt2 : ((char*char) option -> lexer) -> lexer
		  = fn f =>
		  LEXER (fn ls => execLexer (f (next2Chars ls)) ls)
  end

  fun delayLexer thunk = fixLexer (fn _ => thunk ())

  val drop : lexer -> lexer
	  = fn lexer => getOpt (fn None => stop | Some _ => lexer)

  val getIf : (char -> bool) -> (char option -> lexer) -> lexer
	  = fn pred => fn f =>
	  peekOpt (fn None => f None
			 | Some c => if (pred c) then drop (f (Some c))
									 else f None)

  val getIfEq : char -> (lexer * lexer) -> lexer
	  = fn char => fn (yes,no) =>
	  getIf (fn c => c=char) (fn None => no | Some _ => yes)


  val getIf2 : (char * char -> bool) -> ((char * char) option -> lexer) -> lexer
	  = fn pred => fn f =>
	  peekOpt2 (fn None => f None
			  | Some cc => if (pred cc) then drop (drop (f (Some cc)))
										else f None)

  val getIfEq2 : (char * char) -> (lexer * lexer) -> lexer
	  = fn charChar => fn (yes,no) =>
	  getIf2 (fn cc => cc=charChar) (fn None => no | Some _ => yes)


	  
  fun err s = emit (TOK'Err s)

  fun symidAcc (acc:char list) =
	  getIf isSymid (fn None => emit (mkIdentOrSym (implode (rev acc)))
					  | Some c => symidAcc (c::acc))
	  
  fun identAcc (acc:char list) =
	  getIf isIdent (fn None => emit (mkIdentOrKey (implode (rev acc)))
					  | Some c => identAcc (c::acc))
	  
  fun tyvarAcc (acc:char list) =
	  getIf isIdent (fn None => emit (TOK'TyVar (implode (rev acc)))
					  | Some c => tyvarAcc (c::acc))
	  
  val ord0 = ord #"0"						 
  fun numOfChar c = ord c - ord0

  fun numeralAcc (makeTok:int->tok) (acc:int) =
	  getIf isDigit (fn None => emit (makeTok acc)
					   | Some c => numeralAcc makeTok (10*acc+numOfChar c))

  val numberAcc = numeralAcc TOK'Number
  val wordAcc = numeralAcc (fn i => TOK'Word (Word.fromInt i))

  val ordAt = ord #"@"						 
  fun controlChar c = chr (ord c - ordAt)

  fun afterOpenQuote {contNone : lexer,
					  contQ	   : lexer,
					  contNQ   : char -> lexer} =
	  getOpt
	  (fn None => contNone
		| Some #"\"" => contQ
		| Some #"\\" =>
			  getOpt
			  (fn None => err "unterminated backslash in string literal"
				| Some #"t" => contNQ #"\t"
				| Some #"n" => contNQ #"\n"
				| Some #"\\" => contNQ #"\\"
				| Some #"\"" => contNQ #"\""
				| Some #"^" =>
					  getOpt
					  (fn None => err "illegal control escape in string literal"
						| Some c => contNQ (controlChar c))
				| Some c =>
					  if isDigit c
					  then
						  getIf2 (fn (c2,c3) => isDigit c2 andalso isDigit c3)
						  (fn None => err "illegal ascii escape in string literal"
							| Some (c2,c3) =>
								  let val n = 
										  100*numOfChar c +
										  10*numOfChar c2 +
										  numOfChar c3
								  in
									  if n < 256 then contNQ (chr n) else
									  err "illegal ascii escape in string literal"
								  end)
					  else err "illegal escape in string literal")
		| Some c => contNQ c)

	  
  fun inStringLit (acc:char list) =
	  afterOpenQuote {contNone = err "unterminated string literal",
					  contQ = delayLexer (fn () => emit (TOK'StringLit
														 (implode (rev acc)))),
					  contNQ = (fn c => inStringLit (c::acc))}


  fun closeCharLit (c:char) = 
	  getOpt
	  (fn None => err "unterminated char literal"
		| Some #"\"" => emit (TOK'CharLit c)
		| Some _ => err "char literal: size>1")

  val inCharLit =
	  afterOpenQuote {contNone = err "unterminated char literal",
					  contQ = err "char literal: size=0",
					  contNQ = closeCharLit}
	  
  fun inComment (cont:lexer) (level:int) =
	  if level=0 then cont else
	  let
		  val afterLP =
			  fixLexer (fn afterLP =>
						getOpt
						(fn None => err "unclosed comment"
						  | Some #"(" => afterLP
						  | Some #"*" => inComment cont (level+1)
						  | Some _ => inComment cont level))
		  val afterStar =
			  fixLexer (fn afterStar =>
						getOpt
						(fn None => err "unclosed comment"
						  | Some #"*" => afterStar
						  | Some #")" => inComment cont (level-1)
						  | Some _ => inComment cont level))
	  in
		  getOpt
		  (fn None => err "unclosed comment"
			| Some #"(" => afterLP
			| Some #"*" => afterStar
			| Some _ => inComment cont level)
	  end
	  
  val step : lexer
	  = fixLexer
	  (fn step =>
	   getOpt
	   (fn None => stop
		 | Some c =>
			   if isWhite c then step else
			   (* Must test for # before symid (which contains #) *)
			   if c = #"#"then peekOpt (fn None => emit SYM_HASH
										 | Some #"\"" => drop inCharLit
										 | Some #"[" => drop (emit SYM_HASH_LS)
										 | Some _ => symidAcc [c]) else
			   if isSymid c then symidAcc [c] else
			   if isAlpha c then identAcc [c] else
			   if c = #"'" then tyvarAcc [] else
			   if c = #"0" then
				   getIf2 (fn (c1,c2) => c1 = #"w" andalso isDigit c2
						   ) (fn None => numberAcc 0
						 | Some (_,c2) => wordAcc (numOfChar c2)) else
			   if isDigit c then numberAcc (numOfChar c) else
			   case c of
				   #"(" => getIfEq #"*" (inComment step 1, emit SYM_LP)
				 | #")" => emit SYM_RP
				 | #"[" => emit SYM_LS
				 | #"]" => emit SYM_RS
				 | #"{" => emit SYM_LB
				 | #"}" => emit SYM_RB
				 | #"," => emit SYM_COMMA
				 | #";" => emit SYM_SEMI
				 | #"_" => emit SYM_UNDERSCORE
				 | #"\""=> inStringLit []
				 | #"." => getIfEq2 (#".", #".") (emit SYM_DOTS, emit SYM_DOT)
				 | _	=> emit (TOK'Unknown c)))
			 
  fun echoFlush s =
	  (TextIO.output (TextIO.stdOut,s^"\n");
	   TextIO.flushOut TextIO.stdOut)

  fun stepLS ls =
	  let val U = echoFlush (sofPos (posLS ls))
		  val res = execLexer step ls
	  in res
	  end

  val stepLS = execLexer step

  val stepLS = fn who =>
	  fn q => if not (!trace_lex) then stepLS q else
			  let val res = stepLS q
				  val pos = posLS q
				  val () = print (concat ["* stepLS (",
										  who,
										  ")-> ",
										  case res of None => "<eof>" | Some (tok,_) => sofTok tok,
										  " - ",
										  sofPos pos,
										  "\n"])
			  in res
			  end
  
end
