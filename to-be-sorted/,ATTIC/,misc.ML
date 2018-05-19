
local val r = ref 0
in val natsU = iterateU (fn () => let val v = !r
				in r := 1+v; Some v
				end)
end

val nats = iterate (fn a => Some (a,a+1)) 0

takeLL 44 natsU

takeLL 44 nats
val a = "xTo fontify a block (the function or paragraph containing"
	
takeLL 3333 (lazyString a)


signature Lex1Sig =
	sig
	type 'a llist
	type lexState
	val initLS : string llist -> lexState
	val nextLS : lexState -> (char * lexState) option
	val endLS : lexState
	end

	  
		
  fun lexWhile p f b acc ls =
	  case (nextLS ls) of
	  None => (b acc,endLS)
	| Some (c,ls') =>
		  if p c then lexWhile p f b (f acc c) ls'
			 else (b acc,ls)
  fun lexIdent acc ls=
	  Some (lexWhile isIdent (fn acc=> fn c => c :: acc
							  ) (fn acc=> mkIdentOrKey (implode (rev acc))
								 ) acc ls)
  val ord0 = ord #"0"				 
  fun numOfChar c = ord c - ord0
  fun lexNumberLit acc ls makeLit =
	  lexWhile isNumber (fn acc => fn c => 10*acc+numOfChar c
			 ) makeLit acc ls
  fun lexNumber c ls =
	  if c = #"0"
	  then
	  case (nextLS ls) of
		  None => (TOK'Number 0,endLS)
		| Some (c,ls') =>
		  if c = #"w"
		  then
			  case (nextLS ls') of
			  None => (TOK'Number 0,ls')
			| Some (c,ls'') =>
				  if isNumber c
				  then lexNumberLit (numOfChar c) ls'' TOK'Word
				  else (TOK'Number 0,ls')
		  else
			  lexNumberLit 0 ls TOK'Number
	  else
	  lexNumberLit (numOfChar c) ls TOK'Number
  fun stepLS ls =
	  case (nextLS ls) of
	  None => None
	| Some (c,ls) =>
		  if isWhite c then stepLS ls else
		  if isAlpha c then lexIdent [c] ls else
		  if isNumber c then Some (lexNumber c ls) else
		  Some
		  (case c of
		   #":" => (case (nextLS ls) of
				None => (SYM_COLON,endLS)
				  | Some (c,ls') => if c = #":" then (SYM_CONS, ls')
								else (SYM_COLON, ls))
		 | #"<" => (case (nextLS ls) of
				None => (SYM_LT,endLS)
				  | Some (c,ls') => if c = #"<" then (SYM_LSHIFT, ls')
								else (SYM_LT, ls))
		 | #">" => (case (nextLS ls) of
				None => (SYM_GT,endLS)
				  | Some (c,ls') => if c = #">" then (SYM_RSHIFT, ls')
								else (SYM_GT, ls))
		 | #"#" => (case (nextLS ls) of
				None => (SYM_HASH,endLS)
				  | Some (c,ls') => if c = #"[" then (SYM_HASH_LS, ls')
								else (SYM_HASH, ls))
		 | _ => (tokOfChar c, ls))

		   
  val get : (char -> lexer) -> lexer
	  = fn f =>
	  LEXER (fn ls =>
		 case (nextLS ls) of
		 None => None
		   | Some (c,ls') => deLexer (f c) ls')
  val drop : lexer -> lexer
	  = fn lexer =>
	  LEXER (fn ls => case (nextLS ls) of
			  None => None
			| Some (_,ls') => deLexer lexer ls')


	  
		  | #":" => peekOpt (
				 fn None => emit SYM_COLON
				  | Some c2 =>
					if c2 = #":" then drop (emit SYM_CONS)
							 else emit SYM_COLON)
  val peekOpt : (char option -> lexer) -> lexer
	  = fn f =>
	  LEXER (fn ls => 
		 case (nextLS ls) of
		 None => execLexer (f None) ls
		   | Some (c,_) => execLexer (f (Some c)) ls)


  fun tokOfChar c =
	  case c of
	  #"(" => SYM_LP
	| #")" => SYM_RP
	| #"[" => SYM_LS
	| #"]" => SYM_RS
	| #"{" => SYM_LB
	| #"}" => SYM_RB
	| #"," => SYM_COMMA
	| #":" => SYM_COLON
	| #";" => SYM_SEMI
	| #"_" => SYM_UNDERSCORE
	| #"|" => SYM_VBAR
		| #"=" => SYM_EQ
	| #"#" => SYM_HASH
		| #"." => SYM_DOT
		| #"*" => SYM_STAR
		| #"+" => SYM_PLUS
		| #"-" => SYM_MINUS
		| #"<" => SYM_LT
		| #">" => SYM_GT
	| _ => TOK'Unknown c

			

(*
  fun mkIdentOrKey s =
	  let val z = size s
	  in case z of
		 2 =>
		 if s = "in" then TOK'Key KEY_in else
		 TOK'Ident s
	   | 3 =>
		 if s = "let" then TOK'Key KEY_let else
		 if s = "end" then TOK'Key KEY_end else
		 TOK'Ident s
	   | _ =>
		 TOK'Ident s
	  end
  *)
  val getIfEq' : char -> (bool -> lexer) -> lexer
	  = fn char => fn f =>
	  getIf (fn c => c=char) (fn None => f false | Some _ => f true)

  val peek : lexer -> (char -> lexer) -> lexer
	  = fn def => fn f =>
	  LEXER (fn ls => execLexer (case (nextChar ls) of
					 None => def
				   | Some c => f c) ls)

  val get : lexer -> (char ->lexer) -> lexer
	  = fn def => fn f => getOpt (fn None => def | Some c => f c)

  val drop : lexer -> lexer
	  = fn lexer => get stop (fn _ => lexer)

  fun octal c = 
	  let val ordc = ord c
	  val lo = ordc mod 8
	  val mid = (ordc div 8) mod 8
	  val high = (ordc div 64) mod 8
	  in stringOfInt high ^ stringOfInt mid ^ stringOfInt lo
	  end

				  contNQ (chr (64*numOfChar c +
					   8*numOfChar c2 +
					   numOfChar c3)))
  fun isDigit0123 c =
	  let open Char
	  in (c >= #"0" andalso c <= #"3")
	  end


(* parser... *)

datatype longid = LONGID of id list
val longid =
	id >>
	(fn x1 =>
	 manyP (key SYM_DOT -$$ id) >>>
	 (fn xs =>
	  LONGID (x1::xs)))

		  
  datatype cachingLexState = CLS of lexState * (tok * lexState) option option ref
	  
  fun initCLS ls = CLS (ls,ref None)
  fun endCLS (CLS(ls,_)) = ls

  fun stepCLS (CLS(ls,r)) =
	  case (!r) of
	  Some res => res
	| None => let val res = stepLS ls
		  in r := Some res; initCLS res
		  end

fun acc_typed_exp x =
	(key SYM_COLON -$$ ty >>
	 (fn t => acc_typed_exp (EXP'Typed (x,t))))
	## resultP x
val typed_exp =
	fn ED =>
	infexp ED >> acc_typed_exp
	
fun acc_andalso_exp ED x1 =
	(key KEY_andalso -$$ typed_exp ED >>
	 (fn x2 => acc_andalso_exp ED (EXP'Andalso (x1,x2))))
	## resultP x1
val andalso_exp =
	fn ED =>
	typed_exp ED >> acc_andalso_exp ED

fun acc_orelse_exp ED x1 =
	(key KEY_orelse -$$ andalso_exp ED >>
	 (fn x2 => acc_orelse_exp ED (EXP'Orelse (x1,x2))))
	## resultP x1
val orelse_exp =
	fn ED =>
	andalso_exp ED >> acc_orelse_exp ED



	fun acc_patrow acc P =
	(key SYM_DOTS >> (fn _ => resultP (PATROW (rev acc,{wildcard=true}))))
	## (labelled_pat P >>
	(fn lp =>
	 (key SYM_COMMA -$$ acc_patrow (lp::acc) P)
	 ## resultP (PATROW (rev acc,{wildcard=false}))))

fun acc_infexp ED x1 =
	((infop ** appexp ED) >>
	 (fn (f,x2) => let val x = EXP'App (exp'id f, exp'pair (x1,x2))
		   in acc_infexp ED x
		   end))
	## resultP x1
val infexp =
	fn ED =>
	appexp ED >> acc_infexp ED



val stringLit = satisfyP (fn TOK'StringLit s => Some s | _ => None)
val scon =
	(num >>> SCON'Num)
	## (stringLit >>> SCON'String)

	
open Tok
open Lex
fun lexFile P =
	let val ll_string = lazyFile P
	val ls = initLS ll_string
	val ll_token = iterate stepLS ls
	in ll_token
	end
fun writeTokLL {path} ll =
	let val os = TextIO.openOut path
	in (appLL (fn tok => TextIO.output (os,sofTok tok^"\n")) ll;
	TextIO.closeOut os)
	end
fun tt {IN,OUT} =
	writeTokLL {path=OUT} (lexFile {path=IN})

tt {IN="Lex.ML", OUT="g-LL.ML"};
use "g-LL.ML";

tt {IN="g-LL.ML", OUT="g-L<LL>.ML"};
use "g-L<LL>.ML";

(* open Lex... *)

tt {IN="Lex.ML", OUT="g-<LL>L.ML"};
tt {IN="g-LL.ML", OUT="g-<LL><LL>.ML"};



(* Buggy old version... fun tabCol x = 8 * (1 + (x div 8)); *)
(* [1] Columns start at 1, so tab stops are at columns 1,9,17 etc.
	   So need an outer (+1), so move the multiple of 8 to a tab-stop column.
   [2] When in a column which is amultiple of 8, the tab moves to the next column.
	   So need to reduce the x by 1 before the (div 8)
*)
(*	fun tabCol x = 1 + 8 * (1 + ((x-1) div 8)) *)

(* Thu Nov 11 00:51:58 1999 -- reverting to the orig version
	 -- columns do start at 0 (at least in emacs!) *)



fun tl {IN,OUT} =
	let val ll_string = lazyFile {path=IN}
	val ls = Lex.initLS ll_string
	val ll_token = iterate Lex.stepLS ls
	val os = TextIO.openOut OUT
	in (appLL (fn tok => TextIO.output (os,Tok.sofTok tok^"\n")) ll_token;
	TextIO.closeOut os)
	end


(* like lexer's get/peek functions... -- NO doesn't work, coz consumes *)
val tok = satisfyP (fn x => Some x)

fun acc_patrow acc P =
	(key SYM_DOTS >> (fn _ => resultP (PATROW (rev acc,{wildcard=true}))))
	## (labelled_pat P >>
	(fn lp =>
	 tok >>
	 (fn SYM_COMMA => acc_patrow (lp::acc) P
	   | _ => resultP (PATROW (rev acc,{wildcard=false})))))

val fix2P : ('a par * 'b par -> 'a par * 'b par) -> 'a par * 'b par
	= fn F =>
	let fun A b = fixP (fn a => #1 (F (a,b)))
	val b = fixP (fn b => #2 (F (A b,b)))
	val a = A b
	in (a,b)
	end

	(* grammar not factored properly here: "(E... *)
	
	## ((key SYM_LP -$$ sequence0 exp (key SYM_COMMA) $$- key SYM_RP) >>>
	(fn [] => EXP'Unit
	  | [x] => x
	  | xs => EXP'Tuple xs))
	## ((key SYM_LP -$$ sequence exp (key SYM_SEMI) $$- key SYM_RP) >>>
	EXP'Seq)
		  
  abstype lexState = LS'End | LS of pos * string * int * int * string llist lazy
  with
	  fun initLS pos ll =
	  case ll of
		  LNil => LS'End
		| LCons (s,lll) => LS (pos,s,0,size s,lll)
	  fun posLS ls =
	  case ls of
		  LS'End => None
		| LS (pos,s,n,z,lll) => Some pos
	  fun nextLS ls =
	  case ls of
		  LS'End => None
		| LS (pos,s,n,z,lll) =>
		  if n >= z then nextLS (initLS pos (force lll))
				else let val c = String.sub(s,n)
					 val pos' = stepPos pos c
					 val ls' = LS(pos',s,n+1,z,lll)
				 in Some (c,ls')
				 end
  end

fun sofPosOpt (Some pos) = "Some"^sofPos pos
  | sofPosOpt None = "None"


(*----------------------------------------------------------------------
INDEX: pre symbolic idents
----------------------------------------------------------------------*)
	
	(* some 1-char ops *)
	| SYM_PLUS
	| SYM_MINUS
	| SYM_LT
	| SYM_LEQ
	| SYM_GT
	| SYM_GEQ
	| SYM_AT
	| SYM_HAT
	| SYM_PLING
	(* 2-char ops *)
	| SYM_CONS
	| SYM_ASSIGN
	| SYM_LSHIFT
	| SYM_RSHIFT

			  (* some 1-char ops *)
		| SYM_PLUS		=> "+"
		| SYM_MINUS		=> "-"
		| SYM_LT		=> "<"
		| SYM_LEQ		=> "<="
		| SYM_GT		=> ">"
		| SYM_GEQ		=> ">="
		| SYM_AT		=> "@"
		| SYM_HAT		=> "^"
		| SYM_PLING		=> "!"
	(* 2-char ops *)
		| SYM_CONS		=> "::"
		| SYM_ASSIGN	=> ":="
		| SYM_LSHIFT	=> "<<"
		| SYM_RSHIFT	=> ">>"
	  
  val step : lexer
	  = fixLexer
	  (fn step =>
	   getOpt
	   (fn None => stop
	 | Some c =>
		   if isWhite c then step else
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
		 | #"|" => emit SYM_VBAR
		 | #"=" => getIfEq #">" (emit SYM_FUNC_ARROW, emit SYM_EQ)
		 | #"\""=> inStringLit []
		 | #"#" => 
			   peekOpt
			   (fn None => emit SYM_HASH
			 | Some #"[" => drop (emit SYM_HASH_LS)
			 | Some #"\"" => drop inCharLit
			 | Some _ => emit SYM_HASH)
		 | #"." => getIfEq2 (#".", #".") (emit SYM_DOTS, emit SYM_DOT)
		 | #"*" => emit SYM_STAR
		 | #"+" => emit SYM_PLUS
		 | #"-" => getIfEq #">" (emit SYM_TYPE_ARROW, emit SYM_MINUS)
		 | #"<" => 
			   peekOpt
			   (fn None => emit SYM_LT
			 | Some #"=" => drop (emit SYM_LEQ)
			 | Some #"<" => drop (emit SYM_LSHIFT)
			 | Some _ => emit SYM_LT)
		 | #">" => 
			   peekOpt
			   (fn None => emit SYM_GT
			 | Some #"=" => drop (emit SYM_GEQ)
			 | Some #">" => drop (emit SYM_RSHIFT)
			 | Some _ => emit SYM_GT)
		 | #":" => 
			   peekOpt
			   (fn None => emit SYM_COLON
			 | Some #":" => drop (emit SYM_CONS)
			 | Some #"=" => drop (emit SYM_ASSIGN)
			 | Some _ => emit SYM_COLON)
		 | #"@" => emit SYM_AT
		 | #"^" => emit SYM_HAT
		 | #"!" => emit SYM_PLING
		 | _	=> emit (TOK'Unknown c)))


	  (* temp -- special case for infix cons *)
val isInfCons = (fn SYM_CONS => true | _ => false)
val infcons =
	satisfyP (fn tok => if (isInfCons tok) then Some (ID (sofTok tok))
					   else None)
(* temp -- special case a few important infixes *)	  
val isInfOp =
	(* some 1-char ops *)
	fn SYM_PLUS		 => true
	 | SYM_MINUS	 => true
	 | SYM_LT		 => true
	 | SYM_LEQ		 => true
	 | SYM_GT		 => true
	 | SYM_GEQ		 => true
	 | SYM_AT		 => true
	 | SYM_HAT		 => true
	 (* 2-char ops *)
	 | SYM_CONS		 => true
	 | SYM_LSHIFT	 => true
	 | SYM_RSHIFT	 => true
	 | _ => false
val infop =
	satisfyP (fn tok => if (isInfOp tok) then Some (ID (sofTok tok))
					 else None)

	
fun acc_longid acc =
	id >>
	(fn x =>
	 (key SYM_DOT -$$ acc_longid (x::acc))
	 ##
	 (resultP (LONGID (rev acc, x)))) (* BAD -- rev called every iteration!! *)
val longid = acc_longid []	  
	
val num = satisfyP (fn TOK'Number n => Some n | _ => None)
	
val scon =
	(num >>> SCON'Num)
	## (satisfyP (fn TOK'Word n => Some (SCON'Word n) | _ => None))
	## (satisfyP (fn TOK'StringLit s => Some (SCON'String s) | _ => None))

val lab = (id >>> LAB'Id) ## (num >>> LAB'Num)

	val strid = (* not symbolic *)
	satisfyP
	(fn TOK'Ident s => Some (ID s)
	  | _			=> None)

(* no good *)
val longid = manyP (strid $$- key SYM_DOT) ** id >>> LONGID
	


	
fun acc_longid acc =
	id >> (fn x =>
	   (key SYM_DOT -$$ acc_longid (x::acc))
	   ## (resultP (acc,x)))
val longid = acc_longid [] >>> (fn (acc,x) => LONGID (rev acc, x))


	fun acc_longid acc =
	(strid >> (fn x =>
		   (key SYM_DOT -$$ acc_longid (STRID x::acc))
		   ## (resultP (acc,x))))
	## id >> (fn x => resultP (acc,x))
	
val longid = acc_longid [] >>> (fn (acc,x) => LONG (rev acc, x))


	


val ty =
	fixP
	(fn ty =>
	 let val tyrow =
		 sequence0 (lab $$- key SYM_COLON ** ty) (key SYM_COMMA) >>> TYROW
	 val atty =
		 tyvar >>> TY'Var
		 ## ((key SYM_LB -$$ tyrow $$- key SYM_RB) >>> TY'Record)
		 ## seq ty	** longtycon >>> TY'Con
		 ## key SYM_LP -$$ ty $$- key SYM_RP
	 val tupty = sequence atty (key SYM_STAR) >>> TY'Tuple
	 val functy = left tupty (key SYM_FUNC_ARROW -$$ tupty) TY'Func
	 in
	 functy
	 end)

val ty = ...
	(* oops, left recursive !! *)
	## seq ty  ** longtycon >>> TY'Con	  




(* temp -- special case for infix cons *)
val isInfCons = (fn	 _ => false)
val isInfCons = (fn	 TOK'Symid "::" => true | _ => false)
val infcons =
	satisfyP (fn tok => if (isInfCons tok) then Some (VID (ID (sofTok tok)))
					   else None)

	
fun acc_appexp im ED x1 =
	(atexp im ED >> (fn x2 => acc_appexp im ED (EXP'App (x1,x2))))
	## resultP x1
fun appexp im ED =
	atexp im ED >> acc_appexp im ED

(* temp -- special case a few important infixes *)	  
val isInfOp =
	 fn _ => false
val infop =
	satisfyP (fn tok => if (isInfOp tok) then Some (VID (ID (sofTok tok)))
					 else None)

(*----------------------------------------------------------------------
INDEX: Wed Nov 17 23:44:07 1999
----------------------------------------------------------------------*)
	
fun long f id =
	let
	fun acc_longid acc =
		(ident >> (fn x =>
			   (key SYM_DOT -$$ acc_longid (STRID x::acc))
			   ## (resultP (acc,f x))))
		## id >> (fn x => resultP (acc,x))
	in
	acc_longid [] >>> (fn (acc,x) => LONG (rev acc, x))
	end

val longtycon = long TYCON tycon
val longstrid = long STRID strid
fun longvid im = long VID (vid im)

			
fun long (matchX,matchX1) : 'a long par =
	let
	val strid = satisfyP stridMatch
	val X1 = satisfyP matchX1
	val X = satisfyP matchX
	val X1orStrid = satisfyP (getForBoth (matchX1,stridMatch))
	val XorStrid = satisfyP (getForBoth (matchX,stridMatch))
	fun acc_longid acc =
		(XorStrid >> (fn (x,s) =>
			  (key SYM_DOT -$$ acc_longid (s::acc))
			  ## (resultP (acc,x))))
		## X >> (fn x => resultP (acc,x))
		## strid >> (fn s =>
			 key SYM_DOT -$$ acc_longid (s::acc))
	in
	(X1orStrid >> (fn (x,s) =>
			   (key SYM_DOT -$$ acc_longid [s])
			   ## (resultP ([],x)))
	 ## X1 >> (fn x => resultP ([],x))
	 ## strid >> (fn s =>
			  key SYM_DOT -$$ acc_longid [s])
	 ) >>>
	(fn (acc,x) => LONG (rev acc, x))
	end


fun fval_clause im exp =
	many1 (atpat im)  >>
	(fn ps =>
	 opt (key SYM_COLON -$$ ty) >>
	 (fn topt =>
	  key SYM_EQ -$$ exp >>>
	  (fn e => (ps,topt,e))))

fun vid_requiredName im requiredName =
	vid im >>
	(fn name => if (name = requiredName) then resultP ()
					 else failP)

fun fvalbind im E =
	sequence (opt (key KEY_op) -$$ vid im >>
		  (fn name =>
		   fval_clause im E >>
		   (fn clause1 =>
		manyP (key SYM_VBAR -$$ vid_requiredName im name
			   -$$ fval_clause im E) >>>
		(fn clauses =>
		 (name,clause1::clauses))))
		  ) (key KEY_and) >>> FVALBIND

	

	
fun atpatP_followLPpat pat p1 =
	manyP (key SYM_COMMA -$$ pat) >>>
	(fn [] => p1
	  | ps => PAT'Tuple (p1::ps))

fun atpatP_followLP pat =
	key SYM_RP -$$ resultP PAT'Unit
	## pat >> atpatP_followLPpat pat

fun atpatP im pat =
	underscorePat
	## (scon >>> PAT'Scon)
	## (opt (key KEY_op) -$$ (longvid im) >>> PAT'Var)
	## key SYM_LB -$$ patrow pat $$- key SYM_RB >>> PAT'Record
	## key SYM_LP -$$ atpatP_followLP pat
	## ((key SYM_LS -$$ sequence0 pat (key SYM_COMMA) $$- key SYM_RS) >>>
	PAT'List)

fun atpat_followLP im = atpatP_followLP (pat im)
fun atpat_followLPpat im = atpatP_followLPpat (pat im)


(*----------------------------------------------------------------------
INDEX: pre update im with fixity decs
----------------------------------------------------------------------*)
	
fun dec1DE im dec E =
	(key KEY_val -$$ valbind im E >>> DEC'Val)
	## (key KEY_fun -$$ fvalbind im E >>> DEC'Fval)
	## (key KEY_type -$$ typbind >>> DEC'Type)
	## (key KEY_datatype -$$ datbind ** opt (key KEY_withtype -$$ typbind)
	>>> DEC'Datatype)
	(* ## datatype replication *)
	## (key KEY_abstype -$$ datbind ** opt (key KEY_withtype -$$ typbind)
	>> (fn (dat,wo) =>
		key KEY_with -$$ dec $$- key KEY_end
		>>> (fn d => DEC'Abstype (dat,wo,d))))
	## (key KEY_exception -$$ exbind im >>> DEC'Exception)
	## (key KEY_local -$$ dec $$-
	key KEY_in ** dec $$-
	key KEY_end
	>>> DEC'Local)
	## key KEY_open -$$ many1 longstrid >>> DEC'Open
	## (key KEY_infix -$$ precOpt -$$ many1 vidAnyFix -$$ resultP DEC'Empty)
	## (key KEY_infixr -$$ precOpt -$$ many1 vidAnyFix -$$ resultP DEC'Empty)
	## (key KEY_nonfix -$$ many1 vidAnyFix -$$ resultP DEC'Empty)


val semis = manyP (key SYM_SEMI)

fun decDE im D E =
	(manyP (semis -$$ dec1DE im D E) $$- semis) >>> makeDecSeq

fun decE im E = fixP (fn D => decDE im D E)
fun exp im = fixP (fn E => expED im (E,decE im E))
fun dec im = decE im (exp im)
fun dec1s im = manyP (dec1DE im (dec im) (exp im)) >>> makeDecSeq



	fun atpatP im pat =
	underscorePat
	## (scon >>> PAT'Scon)
	## (opt (key KEY_op) -$$ (longvid im) >>> PAT'Var)
	## ((key SYM_LB -$$ patrow pat $$- key SYM_RB) >>> PAT'Record)
	## ((key SYM_LP -$$ sequence0 pat (key SYM_COMMA) $$- key SYM_RP) >>>
	(fn [] => PAT'Unit
	  | [x] => x
	  | xs => PAT'Tuple xs))
	## ((key SYM_LS -$$ sequence0 pat (key SYM_COMMA) $$- key SYM_RS) >>>
	PAT'List)

	fun cons_pat im P =
	(longvid im >>
	 (fn x =>
	  (atpatP im P >>> (fn p => PAT'Cons (x,p)))
	  ## resultP (PAT'Var x)))
	## atpatP im P

	fun atexp im (exp,dec) =
	(scon >>> EXP'Scon)
	## (opt (key KEY_op) -$$ longvid im >>> EXP'Var)
	
val datbind =
	sequence (tyvarseq >>
		  (fn tvs =>
		   tycon >>
		   (fn tc =>
		key SYM_EQ -$$
		sequence (vidAnyFix ** opt (key KEY_of -$$ ty)) (key SYM_VBAR) >>>
		(fn vcons => (tvs,tc,vcons))))
		  ) (key KEY_and) >>> DATBIND

fun exbind1 im =
	vidAnyFix >>
	(fn x =>
	 (opt (key KEY_of -$$ ty) >>> (fn topt => EXBIND1'New (x,topt)))
	 ## (key SYM_EQ -$$ longvid im >>> (fn y => EXBIND1'Copy (x,y))))


		
fun clauseBody exp =	
	opt (key SYM_COLON -$$ ty) >>
	(fn topt =>
	 key SYM_EQ -$$ exp >>>
	 (fn e => (topt,e)))

fun clause im E =
	name_pats im >>
	(fn (name,ps) =>
	 clauseBody E >>>
	 (fn (topt,e) =>
	  (name,(ps,topt,e))))


	
fun vid_requiredName name im =
	vid im >>
	(fn name' => if (name = name') then resultP ()
				   else failP)
	
fun clause_requiredName name im exp =
	vid_requiredName name im -$$
	many1 (atpat im) >>
	(fn ps =>
	 opt (key SYM_COLON -$$ ty) >>
	 (fn topt =>
	  key SYM_EQ -$$ exp >>>
	  (fn e => (ps,topt,e))))

	val theIM = makeIM [
			(mkVid "-$$", PA {prec=3, assoc=LR'Left}),
			(mkVid "+", PA {prec=5, assoc=LR'Right}),
			(mkVid "::", PA {prec=5, assoc=LR'Right})
			]


	(* look ahead 1 token -- but doesn't cache correctly*)
  abstype cachingLexState = CLS of lexState * (tok * lexState) option
  with
	  fun initCLS ls = CLS (ls,stepLS ls)
	  fun endCLS (CLS(ls,_)) = ls
	  fun stepCLS (CLS(_,res)) =
	  case (res) of
		  Some (tok,ls) => Some (tok, initCLS ls)
		| None => None
  end

(*----------------------------------------------------------------------
INDEX: Fri Nov 26 00:14:30 1999
----------------------------------------------------------------------*)

val theIM = makeIM [(VID (ID "::"), PA {prec=5, assoc=LR'Right})]

(* read/write infix maps -- not quite right *)	  
fun dec1DE (im,imW) (decIMRW,E) =
	dec1E im E >>> (fn d => (im,d))
	## (key KEY_abstype -$$ datbind im ** opt (key KEY_withtype -$$ typbind)
	>> (fn (dat,wo) =>
		key KEY_with -$$ decIMRW (im,imW) $$- key KEY_end
		>>> (fn (im,d) => (im, DEC'Abstype (dat,wo,d)))))
	## (key KEY_local -$$ decIMRW (im,imW) >>
	(fn (im,d1) =>
	 key KEY_in -$$ decIMRW (im,imW) $$-
	 key KEY_end >>>
	 (fn (im,d2) => (im,DEC'Local(d1,d2)))))

fun decE im E = fixP (fn D => decDE_sep semis im D E)
fun exp im = fixP (fn E => expED im (E,decE im E))
fun dec im = decE im (exp im)
fun dec1s im = decDE_sep null im (dec im) (exp im)

	
fun expD im D = expED im (fn im => expD im D) D
fun decF im = decDE_sep semis im decF (expD im decF)
fun exp im = expD im decF
fun dec im = decDE_sep null im decF (expD im decF) >>> (fn (F,d) => (F im,d))

	
fun decE im E = decDE_sep
	semis im (fn im => decE im E) E
	
fun exp im = fixP (fn E =>
		   expED
		   im (E,decE im E))
fun decInner im = decE im (exp im)
fun dec im = decDE_sep null im (decInner im) (exp im)


(*----------------------------------------------------------------------
INDEX: Sun Nov 28 00:53:39 1999
----------------------------------------------------------------------*)
	
val atty =
	fn ty =>
	tyvar >>> TY'Var
	## ((key SYM_LB -$$ tyrow ty $$- key SYM_RB) >>> TY'Record)
	## longtycon >>> ty'con0
	## (key SYM_LP -$$ sequence ty (key SYM_COMMA) $$- key SYM_RP
	** longtycon >>> TY'Con)
	## key SYM_LP -$$ ty $$- key SYM_RP


fun dfm_noSemiPrefix im = dfmDE_sep null im (dfm im, dfm, exp im)
fun topdec im = dfm_noSemiPrefix im >>> getFM >>> STRDEC'Dec

## key SYM_EQ -$$ resultP (exp'vid (VID (ID "=")))


(*----------------------------------------------------------------------
INDEX: Mon Apr 10 21:11:21 2000
----------------------------------------------------------------------*)


		val choices = if noPush
				  then choices
				  else pushChoice {saving=saving} choices
		val (wRight,cc,choices,right') = squashTT max cc choices right
		val (choices,broken) =
			if noPush
			then (choices,false)
			else let val ({broken},choices) = popChoice choices
			 in (choices,broken)
			 end

	

(*----------------------------------------------------------------------
INDEX: choices -- dummy
----------------------------------------------------------------------*)

  structure Choices : ChoicesSig =
	  struct
	  abstype choices = CHOICES of (int*bool) list
	  with
		  val emptyChoices = CHOICES []
		  fun pushChoice {saving} (CHOICES xs) = CHOICES ((saving,false)::xs)
		  fun popChoice (CHOICES []) = abort "popChoice[]"
		| popChoice (CHOICES ((_,b)::xs)) = ({broken=b},CHOICES xs)
		  fun makeBreaks (CHOICES xs) {goal} = ({saved=0},(CHOICES xs))
	  end
	  end

  
(*----------------------------------------------------------------------
INDEX: choices -- outer break strategy
----------------------------------------------------------------------*)

  structure Choices : ChoicesSig =
  struct

	type saving = int;	  
	datatype choices = Breaks of int * saving list * int * saving list;

	(***
	 Breaks (used,outer,outerStillvalid,inner)
	 used == consumed items, yet to be popped
	***) 

	val emptyChoices = Breaks (0,[],0,[]);

	fun pushChoice {saving} (Breaks (used,outer,outerStillvalid,inner)) =
	Breaks (used,outer,outerStillvalid, saving::inner);

	fun revLenAcc (revAcc,lenAcc) [] = (revAcc,lenAcc)
	  | revLenAcc (revAcc,lenAcc) (x::xs) = revLenAcc (x::revAcc,1+lenAcc) xs

	fun revLen xs = revLenAcc ([],0) xs

	fun makeBreak (breaks as Breaks (used,outer,outerStillvalid,inner)) =
	if outerStillvalid > 0
	then
		case outer of
		   [] => abort "makeBreak:outer=[]" (* outerStillValid would have equalled 0 *)
		  | (x::outer) => (Some x, Breaks (used+1,outer,outerStillvalid-1,inner))
	else
		let
		(* shift inner-->outer, and count them *)
		val (outer,outerStillvalid) = revLen inner
		in
		case outer of
			(* there were no inners *)
			[] => (None, Breaks (used,[],0,[]))
		  | (x::outer) => (Some x, Breaks (used+1,outer,outerStillvalid-1,[]))
		end;

	fun makeBreaks choices {goal} =
	let
		fun loop saved choices =
		if (saved>=goal) then ({saved=saved},choices) else
		let val (savOpt,choices) = makeBreak choices
		in case savOpt of
			   None => ({saved=saved},choices)
			 | Some sav => loop (saved+sav) choices
		end
	in 
		loop 0 choices
	end
		
	fun popChoice (Breaks (used,outer,outerStillvalid,inner)) =
	case inner of
		(_::inner) => ({broken=false}, Breaks (used,outer,outerStillvalid,inner))
	  | [] => if outerStillvalid > 0
		  then ({broken=false}, Breaks (used,outer,outerStillvalid-1,[]))
		  else if used > 0
			   then ({broken=true}, Breaks (used-1,outer,0,[]))
			   else abort "popChoice[]"


  end;

  
use "../ML/QLayout.ML";
structure QLayout_ORIG = QLayout;


(*----------------------------------------------------------------------
INDEX: Wed Apr 12 22:36:56 2000
----------------------------------------------------------------------*)


use "PTEST.ML";
use "LTEST.ML";


structure Ltest = LTEST(structure Prel = Prel
			structure Tok = Tok
			structure Lex = Lex);
structure Ptest = PTEST(structure Prel = Prel
			structure Lex = Lex
			structure Pfv = Pfv
			structure Lang = Lang
			structure Parser = Parser);


(*----------------------------------------------------------------------
INDEX: Sun Apr 16 01:28:35 2000
----------------------------------------------------------------------*)

structure Parser_DummyInfixStuff =
	struct
	datatype id = datatype Lang.id
	datatype vid = datatype Lang.vid
	abstype infixMap = IM with end
	type topdec = Lang.topdec
	fun isInfix (_:infixMap) (_:vid) = false
	end

(*----------------------------------------------------------------------
INDEX: (old) long -- Tue Apr 18 00:40:41 2000
----------------------------------------------------------------------*)
	
fun matchBoth (match1,match2) x =
	case (match1 x, match2 x) of
	(Some y1, Some y2) => Some (y1,y2)
	  | _ => None

fun matchABfollow (matchA,matchB) Afollow Bfollow =
	let
	val A = satisfyP "matchA" matchA
	val B = satisfyP "matchB" matchB
	val AB = satisfyP "matchBoth" (matchBoth (matchA,matchB))
	in
	(AB >> (fn (a,b) => Afollow a ## Bfollow b))
	## A >> Afollow
	## B >> Bfollow
	end
		
fun long (matchX,matchX1) : 'a long par =
	let
	fun acc_longid acc =
		key SYM_DOT -$$
		matchABfollow (matchX,stridMatch)
		(fn x => resultP (acc,x))
		(fn strid => acc_longid (strid::acc))
	in
	matchABfollow (matchX1,stridMatch)
	(fn x => resultP (LONG ([],x)))
	(fn strid => acc_longid [strid] >>> (fn (acc,x) => LONG (rev acc, x)))
	end
	
val longtycon = long (tyconMatch,tyconMatch)
val longstrid = long (stridMatch,stridMatch)
val longvidAnyFix = long (vidMatch,vidMatch)
fun longvid im = long (vidMatch,nonFixVidMatch im)
fun longvid_noeq im = long (vidMatch,nonFixVidMatch_noeq im)

(*----------------------------------------------------------------------
INDEX: debug versions of parser functions
----------------------------------------------------------------------*)
	
fun isInfix im x =
	(let val (VID (ID s)) = x in echo ("isInfix: "^s) end;
		 case (getIM im x) of
		 Some _ => (echo "isInfix:true"; true)
		   | _ => (echo "isInfix:false"; false)
		 )
	

					  (case (let val xopt = f tok
				 val mes = "satisfyP("^who^"):"^sofTok tok
				 val () = echo
					 (case xopt of
					 None => mes^"-->no"
					   | Some _ => mes^"-->Y")
				 in xopt
				 end
				 ) of

datatype stree = STREE'String of string | STREE'Pair of stree * stree
	
	reduce_pa STREE'Pair (STREE'String "a")
	[
	 (PA{prec=4,assoc=LR'Right},STREE'String "b"),
	 (PA{prec=4,assoc=LR'Left},STREE'String "c")
	 ]


fun infexp im ED =
	left (appexp im ED) (vidInfix im ** appexp im ED)
	(fn (x1,(f,x2)) => EXP'App (exp'vid f, exp'pair (x1,x2)))



(*----------------------------------------------------------------------
INDEX: Wed May	3 11:06:03 2000
----------------------------------------------------------------------*)
	
  val tt =
	  sort (fn ((a1,_),(a2,_)) => a1<a2)
	   [
		(5,"5"),
		(6,"6"),
		(1,"1"),
		(1,"1(x)"),
		(6,"6(x)"),
		(3,"3"),
		(4,"4"),
		(2,"2")]

		  
  fun extend'env_ve (ENV (se,te,ve1)) ve2 =
	  let val ve = valenv'plus ve1 ve2
	  in ENV(se,te,ve)
	  end

  val isConstructor'idstatus = fn IDSTATUS'V => false | _ => true

	  
  fun eval'patvar env v x =
	  let val varOpt =
		  case x of
		LONG ([],vid) =>
		if (case (lookupVidOpt'env vid env) of
			  Some (_,idstatus) => not (isConstructor'idstatus idstatus)
			| None => true)
		then Some vid
		else None
		  | LONG (_::_,_) => None
	  in 
	case (varOpt) of
	  Some vid => Some (valenv'single (vid,(v,IDSTATUS'V)))
	| None =>
	  let val (v',ids) = lookupLongVid'env x env
	  in if (not (isConstructor'idstatus ids))
		 then abort "pat:longvid has value status"
		 else
		   if (eq'value v v') then Some valenv'empty
		   else None
	  end
	  end
	  
  fun eval'exprow E exprow =
	  RECORD (case exprow of
		  EXPROW xs =>
			  fold (fn assoc => fn (lab,exp) =>
				let val value = E exp
				val assoc = extend assoc (lab,value)
				in assoc
				end) (emptyAssoc sof'lab) xs)
	  
  fun eval'tuple E xs =
	  RECORD (#2 (fold (fn (n,assoc) => fn exp =>
			let val value = E exp
				val lab = LAB'Num n
			val assoc = extend assoc (lab,value)
			in (n+1,assoc)
			end) (1, emptyAssoc sof'lab) xs))
	  
  fun sof'vid (VID (ID s)) = s
	  
  fun sof'lab lab =
	  "#" ^ (case lab of
		 LAB'Id (ID string) => string
		   | LAB'Num n => stringOfInt n)

	  
  fun valenv'Rec (ve as VALENV assoc) =			
	  VALENV (fold (fn assoc =>
			   fn (vid,(v,idstatus)) =>
			  let val v' =
				  case v of
					  VALUE'Closure (CLOSURE (match,env,_)) =>
					  VALUE'Closure (CLOSURE (match,env,ve))
					| _ => v
				  val assoc = extend assoc (vid,(v,idstatus))
			  in assoc
			  end) emptyAssoc (rev (listOfAssoc assoc)))

		| EXP'Case (exp,match) =>
		  let val bareExp = EXP'App (EXP'Fn match, exp)
		  in E env bareExp
		  end


  datatype 'a valenv = VALENV of (vid,'a) assoc
  datatype 'a tyenv = TYENV of (tycon,'a valenv) assoc
  datatype 'a strenv = STRENV of (strid,'a env) assoc
  and 'a env = ENV of 'a strenv * 'a tyenv * 'a valenv

		val sofAssoc		: ('a -> string) -> ('b,'a) assoc -> string

				  fun sofIM (IM ass) = sofAssoc (fn x => ".") ass
		(*val () = echo ("**app'fmXXX:"^sofIM im^"-->"^sofIM res)*)
	fun app'fmXXX (FM (fmo,_)) im =
	let
		val res =
		case fmo of Some fm => fm im | None => im
	in
		res
	end

					   
  fun look'longstrid (env as ENV (se,te,ve)) (LONG (strids,strid)) =
	  case strids of
	  [] => look'strid se strid
	| strid::strids =>
		  let val env = look'strid se strid 
		  in look'longstrid env (LONG (strids,strid))
		  end

  fun look'longtycon (LONG (strids,tycon)) (env as ENV (se,te,ve)) =
	  case strids of
	  [] => look'tycon'tyenv tycon te
	| strid::strids =>
		  let val env = look'strid se strid 
		  in look'longtycon (LONG (strids,tycon)) env
		  end

  fun look'longvid (LONG (strids,vid)) (ENV (se,te,ve)) =
	  case strids of
	  [] => look'vid'valenv vid ve 
	| strid::strids =>
		  let val env = look'strid strid se
		  in look'longvid (LONG (strids,vid)) env
		  end

		  
signature NmlSig =
sig
	val useFile : string -> unit
	val useString : string -> unit
end
(** structure Nml : NmlSig = struct ... end *)
	
fun tev IN =
	let 
	val (pos,{eof},tdso) = Test.tp IN
	val tds = case tdso of Some x => x | _ => abort "tev:tds"
	val td = case tds of [td] => td | _	 => abort "tev:td"
	val strdec = case td of TOPDEC'Strdec x => x | _ => abort "tev:strdec"
	val dec = case strdec of STRDEC'Seq (STRDEC'Empty,STRDEC'Dec x) => x
				   | _ => abort "tev:dec"
	val env = eval'dec env'initial dec
	in
	env
	end
		
val env = tev "../thrice-mod.ML"
val ENV (_,_,ve) = env
val VALENV assoc = ve
val pairs = listOfAssoc assoc
val [(_,(res,_))] = pairs
(* expect res = 73 -- YES! *)			  

	  
  datatype basval =
	   BASVAL'Eq (* = *)
	 | BASVAL'Minus (* - *)
	 | BASVAL'Echo
	 | BASVAL'StringOfInt

 | VALUE'Basval of basval
			
  fun apply'basval basval vArg =
	  case basval of
	  BASVAL'Eq =>
	  (case vArg of
		   VALUE'Record record =>
		   let val v1 = look'record record (LAB'Num 1)
		   val v2 = look'record record (LAB'Num 2)
		   in
		   value'bool (eq'value v1 v2)
		   end
		 | _ => error "apply:(=)[to non record]")
	| BASVAL'Minus =>
	  (case vArg of
		   VALUE'Record record =>
		   let val v1 = look'record record (LAB'Num 1)
		   val v2 = look'record record (LAB'Num 2)
		   in
		   value'num (num'value "-:1" v1 - num'value "-:2" v2)
		   end
		 | _ => error "apply:(-)[to non record]")
	| BASVAL'Echo =>
	  (case vArg of
		   VALUE'Special (SCON'String s) => (echo s; value'unit)
		 | _ => error "apply: echo")
	| BASVAL'StringOfInt => value'string (stringOfInt (num'value "stringOfInt" vArg))


| VALUE'Basval basval			  => apply'basval basval vArg			

							(*
	   ("stringOfInt", fn v => value'string (stringOfInt (num'value "stringOfInt" v)))
	*)

(*----------------------------------------------------------------------
INDEX: assoc functions -- no longer required -- Mon May	 8 23:36:02 2000
----------------------------------------------------------------------*)

		val emptyAssoc'sofk : ('a -> string) -> ('a,'b) assoc
		val sofAssoc		: ('a -> string) -> ('b,'a) assoc -> string

	 fun emptyAssoc'sofk sofk = ASSOC ([],sofk)
	
	fun commaSep ss =	 
	case ss of
		[] => ""
	  | s::ss => old_implode (s :: map (fn s => "," ^ s) ss)
	fun sofAssoc sofv (ASSOC(xs,sofk)) =
		let fun sofPair (k,v) = sofk k ^ "->" ^ sofv v
		in "{"^commaSep (map sofPair xs)^"}"
		end

(*----------------------------------------------------------------------
INDEX: pre kill sofk
----------------------------------------------------------------------*)
		
	abstype ('a,'b) assoc = ASSOC of ('a * 'b) list * ('a -> string)
	with
	fun listOfAssoc (ASSOC (xs,_)) = xs
	val emptyAssoc = ASSOC ([],fn _ => "no-sofk")
	fun lookup (ASSOC(xs,sofk)) k =
		let fun look [] = None
		  | look ((k',v)::xs) = if (k=k') then Some v else look xs
		in look xs
		end
	fun extend (ASSOC(xs,sofk)) x = ASSOC (x::xs,sofk)
	fun delete (ASSOC(xs,sofk)) k =
		ASSOC (filter (fn (k',_) => k<>k') xs, sofk)
	end

(*----------------------------------------------------------------------
INDEX: pre -- rename some assoc functions
----------------------------------------------------------------------*)

signature AssocSig =
	sig
	type ('a,'b) assoc
		val listOfAssoc		: ('a,'b) assoc -> ('a * 'b) list
		val emptyAssoc		: ('a,'b) assoc
		val lookup			: (''a,'b) assoc -> ''a -> 'b option
		val extend			: ('a,'b) assoc -> 'a * 'b -> ('a,'b) assoc
		val delete			: (''a,'b) assoc -> ''a -> (''a,'b) assoc

		val assoc'plus		: ('a,'b) assoc -> ('a,'b) assoc -> ('a,'b) assoc
		val assoc'list		: ('a * 'b) list -> ('a,'b) assoc
		val assoc'single	: 'a * 'b -> ('a,'b) assoc
		val map'assoc		: ('a * 'b -> 'c * 'd) -> ('a,'b) assoc -> ('c,'d) assoc
		val look			: string -> (''a -> string) -> (''a,'b) assoc -> ''a -> 'b
	end;

		
(*----------------------------------------------------------------------
INDEX: pre -- move some non-prim-ops into the assoc abstype
----------------------------------------------------------------------*)

	fun assoc'plus a1 a2 = fold extend'assoc a1 (rev (list'assoc a2))
	fun assoc'list xs = fold extend'assoc assoc'empty xs
	fun assoc'single kv = extend'assoc assoc'empty kv
	fun map'assoc f a = assoc'list (map f (list'assoc a))
			
	fun look'assoc who sofk a k =
	case (lookup'assoc a k) of
		Some x => x
	  | None => abort (who^":"^sofk k)

(*----------------------------------------------------------------------
INDEX: Thu Sep 14 11:26:23 2000
----------------------------------------------------------------------*)

  type match
  type env
  type valenv

  and closure = CLOSURE of match * env * valenv

  VALUE'Closure (CLOSURE (match,env,valenv'empty))

  fun valenv'Rec (ve as VALENV assoc) =
	  VALENV (map'assoc (fn (vid,(v,ids)) =>
				(vid, ((case v of
					VALUE'Closure (CLOSURE (match,env,_)) =>
					VALUE'Closure (CLOSURE (match,env,ve))
					  | _ => v),
				   ids))
				) assoc)
	  
  fun apply'closure P E (CLOSURE (match,env,ve)) vArg =
	  let val env = env'plus env (env'valenv (valenv'Rec ve))
	  in case (eval'match P E env vArg match) of
		 Some res => res
	   | None => raise (Pack exval'Match)
	  end


  fun eval'vidBinding vid v = valenv'single (vid,(v,IDSTATUS'V))


(*----------------------------------------------------------------------
INDEX: Thu Nov 29 12:18:21 2001
----------------------------------------------------------------------*)

val rec f = fn x => if x=0 then 1 else f x + g x
and g = fn	y => f (y-1) * g (y-1)

val rec (f2,g2) = (* NJ -- syntax err *)
	(fn x => if x=0 then 1 else f2 x + g2 x,
	 fn	 y => f2 (y-1) * g2 (y-1))



(*----------------------------------------------------------------------
INDEX: Thu Nov 29 13:51:08 2001
----------------------------------------------------------------------*)

(* not accepted by NJML -- why not?*)

val rec tri1 as tri2 = fn x => if x=0 then 0 else x + tri1 (x-1) + tri2 (x-1)


	| SPEC'Val xs =>
	  inter'plusList (map (fn (vid,_) => 
				  inter'valinter (valinter'single (vid,IDSTATUS'V)))
				xs)
    | SPEC'Type xs =>
      inter'tyinter (tyinter'list (map (fn (_,tycon) => (tycon,valinter'empty)) xs))
    | SPEC'Eqtype xs =>
      inter'tyinter (tyinter'list (map (fn (_,tycon) => (tycon,valinter'empty)) xs))

    | SPEC'Structure xs =>
      inter'plusList
      (map (fn (strid,sigexp) => inter'strinter (strinter'single (strid,SIG sigexp))
           ) xs)



(*----------------------------------------------------------------------
INDEX: Fri Nov 30 12:28:23 2001
----------------------------------------------------------------------*)

  fun listCons'value v =
      let val (vid',vOpt) = con'value "patlist" v
      in
      if (eq'vid vid' vid'nil) then
          case vOpt of
          None => None
        | Some _ => error "patlist(nil): unexpected value"
      else if (eq'vid vid' vid'cons) then
          case vOpt of
          None => error "patlist(cons): expected value"
        | Some v =>
          let val record = record'value "patlist(cons)" v
              val v1 = look'record "patlist(cons):1" record (LAB'Num 1)
              val v2 = look'record "patlist(cons):2" record (LAB'Num 2)
          in Some(v1,v2)
          end
      else error "patlist: expected nil/cons"
      end
      
  fun eval'patlist P env v pats =
      case (pats,listCons'value v) of
      ([],None) => Some valenv'empty
    | (pat::pats,Some(v1,v2)) =>
      (case (P env v1 pat) of
           None => None
         | Some ve1 => (case (eval'patlist P env v2 pats) of
                None => None
                  | Some ve2 => Some (valenv'plus ve1 ve2)))
    | _ => None


  fun list'value who v =
      let val who = (who^":list'value")
      fun loop v =
          case v of
          VALUE'Con0 vid => if (eq'vid vid vid'nil) then []
                    else error (who^":bad con0")
        | VALUE'Con1 (vid,v) =>
          if (eq'vid vid vid'cons)
          then let val (v1,v2) = pair'value (who^":cons") v
               in v1 :: loop v2
               end
          else error (who^":bad con1")
        | _ => error (who^":expected nil/cons")
      in
      loop v
      end

  fun listCons'value who v =
      let val (vid',vOpt) = con'value (who^": expected con") v
      in
      if (eq'vid vid' vid'nil) then
          case vOpt of
          None => None
        | Some _ => error (who^"(nil): unexpected value")
      else if (eq'vid vid' vid'cons) then
          case vOpt of
          None => error (who^"(cons): expected value")
        | Some v =>
          let val record = record'value (who^"(cons)") v
              val v1 = look'record (who^"(cons):1") record (LAB'Num 1)
              val v2 = look'record (who^"(cons):2") record (LAB'Num 2)
          in Some(v1,v2)
          end
      else error (who^": expected nil/cons")
      end


(*----------------------------------------------------------------------
INDEX: Fri Nov 30 12:46:56 2001
----------------------------------------------------------------------*)


use "BUILTIN.ML";
structure Builtin = BUILTIN(structure Prel = Prel
                structure Lang = Lang
                structure Value = Value
                structure Basis = Basis)


Run.extendTheBasis Builtin.basis'initial

Run.Nuse ("nml_NonPrim.ML");
Run.Nexec ("open NonPrim");

Run.Nuse "BUILTIN.ML";

Run.Nexec(old_implode[
"structure Assoc = ASSOC(structure Prel = Prel)",
"structure QLayout = QLAYOUT(structure Prel = Prel)",
"structure Misclay = MISCLAY(structure Prel = Prel",
"                           structure QLayout = QLayout)",
"structure Lang = LANG(structure Prel = Prel)",
"structure Value = VALUE(structure Prel = Prel",
"                       structure Sort = Sort",
"                       structure Assoc = Assoc",
"                       structure Lang = Lang",
"                       structure Misclay = Misclay)",
"structure Basis = BASIS(structure Prel = Prel",
"                       structure Assoc = Assoc",
"                       structure Misclay = Misclay",
"                       structure Lang = Lang",
"                       structure Value = Value)",
"structure Eval = EVAL(structure Prel = Prel",
"                     structure Lang = Lang",
"                     structure Value = Value",
"                     structure Basis = Basis)",
"structure Builtin = BUILTIN(structure Prel = Prel",
"                           structure Lang = Lang",
"                           structure Value = Value",
"                           structure Basis = Basis)",
""]);


Run.Nexec(old_implode[
"structure Tok = TOK(structure Prel = Prel)",
"structure Lex = LEX(structure Prel = Prel",
"                   structure Tok = Tok)",
"structure Pfv = PFV(structure Lex = Lex",
"                   structure Tok = Tok)",
"structure Parser = PARSER(structure Prel = Prel",
"                         structure Assoc = Assoc",
"                         structure Tok = Tok",
"                         structure Pfv = Pfv",
"                         structure Lang = Lang)",
"structure Pretty = PRETTY(structure Prel = Prel",
"                         structure Misclay = Misclay",
"                         structure Tok = Tok",
"                         structure Lang = Lang",
"                         structure Parser = Parser) (* stupid *)",
"structure Run =",
"    RUN(val wrapCol = 80",
"       structure Prel = Prel",
"       structure QLayout = QLayout",
"       structure Tok = Tok",
"       structure Lex = Lex",
"       structure Pfv = Pfv",
"       structure Lang = Lang",
"       structure Parser = Parser",
"       structure Pretty = Pretty",
"       structure Value = Value",
"       structure Basis = Basis",
"       structure Builtin = Builtin",
"       structure Eval = Eval);",
""])




  fun func1 (ARG,RET) name f =
      let val who = name^":func1"
      in (name, fn v => from RET (f (back ARG who v)))
      end
             
      
  fun f_un (a'value,value'b) (who:string) unop =
      (who, fn (v:value) => value'b (unop (a'value who v)) : value)
      
  fun f_bin (a'value,b'value,value'c) who binop =
      let val who1 = who^"[arg1]"
      val who2 = who^"[arg2]"
      in (who, fn v => let val (v1,v2) = pair'value who v
               in value'c (binop (a'value who1 v1, b'value who2 v2)) : value
               end)
      end
            
  val f_un_W_W  = f_un (word'value, value'word)
  val f_un_W_I  = f_un (word'value, value'int)
  val f_un_I_W  = f_un (int'value,  value'word)
  val f_un_S_I  = f_un (string'value,   value'int)
  val f_un_I_C  = f_un (int'value,  value'char)
  val f_un_C_I  = f_un (char'value, value'int)
  val f_un_CL_S = f_un (charList'value, value'string)

  val f_bin_WxW_W = f_bin (word'value,word'value,value'word)
  val f_bin_IxI_I = f_bin (int'value,int'value,value'int)
  val f_bin_IxI_B = f_bin (int'value,int'value,value'bool)
  val f_bin_SxS_S = f_bin (string'value,string'value,value'string)
      

  fun valenv'clist vids =
      valenv'plusList
      (map valenv'singleValue
           (map (fn vid => (vid, (value'con0 vid, IDSTATUS'C))) vids))

  fun valenv'vlist xs = 
      valenv'plusList
      (map (fn (s,v) => valenv'singleValue (VID (ID s), (v, IDSTATUS'V))) xs)
       
  fun valenv'flist xs =
      valenv'vlist
      (map (fn (s,f) => (s, value'basval (s, f))) xs)
      
  val f_bin_XVxI_X = f_bin (vector'value,int'value,fn x => x)

  val Vector_env =
      env'valenv
      (valenv'flist
       [
        f_bin_XVxI_X  "sub"       Vector.sub
        ])

  val Word_env =
      env'valenv
      (valenv'flist
       [
        f_bin_WxW_W  "+"         Word.+,
        f_bin_WxW_W  "-"         Word.-,
        f_bin_WxW_W  "mod"       Word.mod,
        (*f_bin_WxW_W  ">>"        Word.>>,*)
        (*f_bin_WxW_W  "<<"        Word.<<,*)
        func1(PAIR(W,W),W) ">>" Word.>>,
        func1(PAIR(W,W),W) "<<" Word.<<,
        (*func1(PAIR(I,I),B) "<<" op >, (* BUG!! -- to test error message*) *) 
        f_bin_WxW_W  "orb"       Word.orb,
        f_bin_WxW_W  "andb"      Word.andb,
        f_un_W_W     "notb"      Word.notb,
        f_un_W_I     "toInt"     Word.toInt,
        f_un_I_W     "fromInt"   Word.fromInt
        ])

  val TextIO_env =
      env'valenv
      (valenv'plusList
       [valenv'vlist [
              ("stdOut", value'outstream TextIO.stdOut)
              ],
    valenv'flist [
              (*f_bin_XVxI_X  "sub"       TextIO.sub*)
              ]
    ])

  val Top_strenv =
      strenv'list [(STRID (ID "Word"), Word_env),
           (STRID (ID "Vector"), Vector_env),
           (STRID (ID "TextIO"), TextIO_env)
           ]

  val Top_tyenv = tyenv'empty

  val vid'true = VID (ID "true")
  val vid'false = VID (ID "false")

  val Top_valenv =
      valenv'plusList
      [valenv'singleValue (VID (ID "ref"), (value'ref, IDSTATUS'C)),
       valenv'vlist [(":=",value'assignOp)],
       valenv'clist [vid'nil,vid'nil,vid'cons,vid'true,vid'false],
       valenv'flist
       [
        (           "=", fn v => let val (v1,v2) = pair'value "=" v
                     in value'bool (eq'value v1 v2)
                     end),
        (           "print", fn v => (print prefixNML;
                          print (string'value "print" v);
                          value'unit)),
            f_bin_IxI_I     "-"         op -,
            f_bin_IxI_I     "+"         op +,
            f_bin_IxI_I     "*"         op *,
            f_bin_IxI_I     "div"       op div,
            f_bin_IxI_I     "mod"       op mod,
            f_bin_IxI_B     "<"         op <,
            f_bin_IxI_B     "<="        op <=,
            f_bin_IxI_B     ">"         op >,
            f_bin_IxI_B     ">="        op >=,
            f_bin_SxS_S     "^"         op ^,
            f_un_S_I        "size"      size,
            f_un_I_C        "chr"       chr,
            f_un_C_I        "ord"       ord,
            f_un_CL_S       "implode"   implode
            ]]

  val env'initial = env'se_te_ve (Top_strenv, Top_tyenv, Top_valenv)
  val basis'initial = basis'env env'initial


(*----------------------------------------------------------------------
INDEX: Mon Dec  3 11:59:48 2001a
----------------------------------------------------------------------*)

    ## (noErrorP (key KEY_datatype -$$ tycon $$- key SYM_EQ $$- key KEY_datatype) ** longtycon
    >>> DEC'DataReplicate)

    ## (key KEY_datatype -$$ datbind im ** opt (key KEY_withtype -$$ typbind)
    >>> DEC'Datatype)


(*----------------------------------------------------------------------
INDEX: fold -- Thu Dec  6 22:39:57 2001
----------------------------------------------------------------------*)

  fun fold'mrule P E acc (MRULE (pat,exp)) = E (P acc pat) exp

  fun fold'match P E acc (MATCH xs) = fold (fold'mrule P E) acc xs
    
  fun fold'exprow E acc exprow =
      case exprow of
      EXPROW xs => fold (fn acc => fn (lab,exp) => E acc exp) acc xs
      
  fun fold_exp P E D V acc exp =
      case exp of
      EXP'Scon scon => acc
    | EXP'Var x => V acc x
    | EXP'Record exprow => fold'exprow E acc exprow
    | EXP'Select lab => acc
    | EXP'Unit => acc
    | EXP'Tuple exps => fold E acc exps
    | EXP'List exps => fold E acc exps
    | EXP'Vector exps => fold E acc exps
    | EXP'Seq exps => fold E acc exps
    | EXP'Let (dec,exp) => E (D acc dec) exp
    | EXP'App (e1,e2) => E (E acc e1) e2
    | EXP'Typed (exp,ty) => E acc exp
    | EXP'Andalso (e1,e2) => E (E acc e1) e2
    | EXP'Orelse (e1,e2) => E (E acc e1) e2
    | EXP'Handle (exp,match) => fold'match P E (E acc exp) match
    | EXP'Raise exp => E acc exp
    | EXP'If (e1,e2,e3) => E (E (E acc e1) e2) e3
    | EXP'While (e1,e2) => E (E acc e1) e2
    | EXP'Case (exp,match) => fold'match P E (E acc exp) match
    | EXP'Fn match => fold'match P E acc match 


  fun free'exp_U P E D env exp =
      case exp of
      EXP'Fn match => recval'fn (fn ve => value'closure (makeFreeClosure P E (match,env,ve)))
    | _ => recval'value (inner_free'exp_U P E D env exp)

(*----------------------------------------------------------------------
INDEX: Tue Apr 25 12:26:05 2006
----------------------------------------------------------------------*)


          TOPDEC'Exp exp
          let val v = eval'exp (env'basis basis) exp
              val ve = eval'vidBinding vid'it (recval'value v)
              val env = env'valenv ve
              val basis = basis'env env
          in basis
          end


         val ExpectEnv = look'strid (strenv'env (env'basis basis)) (STRID (ID "Expect"))
         val (value,_) = look'vid (valenv'env ExpectEnv) (VID (ID "expect"))

    val look'strid      : strenv -> strid -> env
    val look'tycon      : tyenv -> tycon -> valenv
    val look'vid        : valenv -> vid -> value * idstatus
    val look'long       : (env -> 'a -> 'b) -> env -> 'a long -> 'b

    fun sofPos (Lex.POS{row,col}) =
        stringOfInt row ^ ":" ^ stringOfInt col


signature PVF_LexSig =
    sig
        type lexState
        type tok
        val stepLS : lexState -> (tok * lexState) option
    end

  fun sofPos (POS{row,col}) =
      stringOfInt row ^ ":" ^ stringOfInt col


(*----------------------------------------------------------------------
INDEX: Fri Apr 28 09:47:15 2006
----------------------------------------------------------------------*)


(*fun useP s = use (preludeDir^s);*)
fun useP s = use (prefixPrelude s)

(*
(* pervasive types *)    
useP "option.ML";
useP "either.ML";
*)
(*

useP "Prel.ML";
useP "Sort.ML";
useP "ASSOC.ML";
useP "QLAYOUT.ML";
useP "MISCLAY.ML";
use "ATOM.ML";
use "VALUE.ML";
use "BASIS.ML";
use "BUILTIN.ML";
use "LANG.ML";
use "TOK.ML";
use "POS.ML";
use "LEX.ML";
use "PFV.ML";
use "PARSER.ML";
use "PRETTY.ML";
use "RUN.ML";
use "EVAL.ML";
use "EVAL_CORE.ML";
use "COMP_CORE.ML";
*)

(*
Run.Nuse ("nml_NonPrim.ML");
Run.Nexec ("open NonPrim");
Run.Nuse (preludeDir^"option.ML");
Run.Nuse (preludeDir^"either.ML");
Run.Nuse (preludeDir^"Prel.ML");
Run.Nuse (preludeDir^"Sort.ML");
Run.Nuse (preludeDir^"ASSOC.ML");
Run.Nuse (preludeDir^"QLAYOUT.ML");
Run.Nuse (preludeDir^"MISCLAY.ML");
Run.Nuse "ATOM.ML";
Run.Nuse "VALUE.ML";
Run.Nuse "BASIS.ML";
Run.Nuse "BUILTIN.ML";
Run.Nuse "LANG.ML";
Run.Nuse "EVAL_CORE.ML";
Run.Nuse "COMP_CORE.ML";
Run.Nuse "EVAL.ML";
Run.Nuse "TOK.ML";
Run.Nuse "POS.ML";
Run.Nuse "LEX.ML";
Run.Nuse "PFV.ML";
Run.Nuse "PARSER.ML";
Run.Nuse "PRETTY.ML";
Run.Nuse "RUN.ML";
*)


  datatype status =
           STATUS'V
         | STATUS'Ref
         | STATUS'C of vid
         | STATUS'E of exname

  fun lookStatus'longvid env x = 
      let val (x_value,ids) = look'longvid env x
      in case ids of
             IDSTATUS'V => STATUS'V
           | IDSTATUS'C =>
             let val either = ref_or_con0'value "patcons(C)" x_value
             in case either of
                    This {Ref=()} => STATUS'Ref
                  | That {Con0=vid} => STATUS'C vid
             end
           | IDSTATUS'E =>
             let val exname = excon0'value "parvar(E)" x_value
             in STATUS'E exname
             end
      end

      
  fun eval'patvar env recval x =
      case (maybeGetBindableVid'longvid env x) of
          Some vid => Some (valenv'single (vid,(recval,IDSTATUS'V)))
        | None =>
          case (lookStatus'longvid env x) of
              STATUS'V => error "patvar(V)"
            | STATUS'Ref => error "patvar(Ref)"
            | STATUS'C vid =>
              let val (vid',vOpt) = con'value "patvar(C)" (value'recval recval)
              in case vOpt of
                     Some _ => None
                   | None => if (eq'vid vid vid')
                             then (Some valenv'empty) else None
              end
            | STATUS'E exname =>
              let val (exname',vOpt) = excon'value "parvar(E)" (value'recval recval)
              in case vOpt of
                     Some _ => None
                   | None => if (eq'exname exname exname')
                             then (Some valenv'empty) else None
              end

  fun eval'patcons P env v x pat =
      case (lookStatus'longvid env x) of
          STATUS'V => error "patcons(V)"
        | STATUS'Ref =>
          let val v' = derefAddr'value "patcons(ref)" v
          in P env v' pat
          end
        | STATUS'C vid =>
          let val (vid',vOpt) = con'value "patcons(C)" v
          in case vOpt of
                 Some v' => if (eq'vid vid vid')
                            then (P env v' pat) else None
               | None => None
          end
        | STATUS'E exname =>
          let val (exname',vOpt) = excon'value "parvar(E)" v
          in case vOpt of
                 Some v' => if (eq'exname exname exname')
                            then (P env v' pat) else None
               | None => None
          end


  fun patStatus'longvid env x = 
      let val bindOpt =
              case (x) of
                  LONG (_::_,vid) => None
                | LONG ([],vid) =>
                  if (isUnbound'vid env vid) then (Some vid)
                  else let val (_,ids) = look'longvid env x
                       in case ids of
                              IDSTATUS'V => Some vid
                            | IDSTATUS'C => None
                            | IDSTATUS'E => None
                       end
      in
          case (bindOpt) of
              Some vid => STATUS'BindableV vid
            | None =>
              let val (x_value,ids) = look'longvid env x
              in case ids of
                     IDSTATUS'V => error "patStatus'longvid(V)"
                   | IDSTATUS'C =>
                     let val either = ref_or_con0'value "patcons(C)" x_value
                     in case either of
                            This {Ref=()} => STATUS'Ref
                          | That {Con0=vid} => STATUS'C vid
                     end
                   | IDSTATUS'E =>
                     let val exname = excon0'value "parvar(E)" x_value
                     in STATUS'E exname
                     end
              end
      end



  fun eval'dec_U Px Ex D env dec = 
      let fun E env exp = let val cexp = comp'exp env shad'empty exp
                          in Ex env'empty cexp
                          end
          fun P env recval pat = let val cpat = comp'pat env pat
                                 in Px recval cpat
                                 end
      in


(*----------------------------------------------------------------------
INDEX: Tue May  2 16:02:45 2006
----------------------------------------------------------------------*)
                   
(*----------------------------------------------------------------------
INDEX: comp'dec
----------------------------------------------------------------------*)

structure OLD = struct
  fun comp'exbind1 b =
      case b of
          EXBIND1'New (vid,tyo) => Comp.EXBIND1'New (vid,tyo)
        | EXBIND1'Copy (vid,longvid) => Comp.EXBIND1'Copy (vid,longvid)

  fun comp'exbind (EXBIND bs) = Comp.EXBIND (map comp'exbind1 bs)
               
  fun comp'valbind P E vb =
      case vb of        
          VALBIND'Rec vb => Comp.VALBIND'Rec (comp'valbind P E vb)
        | VALBIND'Seq (pat,exp,vbo) => 
          Comp.VALBIND'Seq (P pat,E exp,
                            case vbo of None => None | Some vb => Some (comp'valbind P E vb))

  fun comp'fvalbind E (FVALBIND fs) =
      Comp.FVALBIND (map (fn (pos,vid,clauses) => (pos,vid,E (exp'clauses pos clauses))) fs)

  fun U_comp'dec P E D dec =
      case dec of
          DEC'Val vb                => Comp.DEC'Val (comp'valbind P E vb)
        | DEC'Fval fb               => Comp.DEC'Fval (comp'fvalbind E fb)
        | DEC'Type typbind          => Comp.DEC'Type typbind
        | DEC'Datatype (db,tbo)     => Comp.DEC'Datatype (db,tbo)
        | DEC'DataReplicate p       => Comp.DEC'DataReplicate p
        | DEC'Abstype (db,tbo,dec)  => Comp.DEC'Abstype (db,tbo,D dec)
        | DEC'Exception exbind      => Comp.DEC'Exception (comp'exbind exbind)
        | DEC'Local (dec1,dec2)     => Comp.DEC'Local (D dec1,D dec2)
        | DEC'Open xs               => Comp.DEC'Open xs
        | DEC'Empty                 => Comp.DEC'Empty
        | DEC'Seq (dec1,dec2)       => Comp.DEC'Seq (D dec1,D dec2)
end


  fun patStatus'longvid env x =
      case (case (x) of
                LONG([],vid) => if (isUnbound'vid env vid) 
                                then This vid (*short: unbound*)
                                else That (Some vid, look'longvid "patStatus'longvid:1" env x) (*short: bound*)
              | _ => That (None, look'longvid "patStatus'longvid:2" env x)) of (*long: must be bound*)
          This vid => STATUS'BindableV vid
        | That (Some vid,(_,IDSTATUS'V)) => STATUS'BindableV vid
        | That (None,(_,IDSTATUS'V)) => error "patStatus'longvid(LongV)"
        | That (_,(x_value,IDSTATUS'E)) => STATUS'E (excon0'value "parvar(E)" x_value)
        | That (_,(x_value,IDSTATUS'C)) => let val either = ref_or_con0'value "patcons(C)" x_value
                                           in case either of
                                                  This {Ref=()} => STATUS'Ref
                                                | That {Con0=vid} => STATUS'C vid
                                           end



  fun patconsStatus'longvid env x =
      case (case (x) of
                LONG([],vid) => if (isUnbound'vid env vid) 
                                then This vid (*short: unbound*)
                                else That (Some vid, look'longvid "patconsStatus'longvid:1" env x) (*short: bound*)
              | _ => That (None, look'longvid "patconsStatus'longvid:2" env x)) of (*long: must be bound*)
          This vid => error ("patconsStatus'longvid(V):1"^sof'vid vid)
        | That (Some vid,(_,IDSTATUS'V)) => error ("patconsStatus'longvid(V):2"^sof'vid vid)
        | That (None,(_,IDSTATUS'V)) => error "patconsStatus'longvid(LongV)"
        | That (_,(x_value,IDSTATUS'E)) => STATUS'E (excon0'value "parvar(E)" x_value)
        | That (_,(x_value,IDSTATUS'C)) => let val either = ref_or_con0'value "patcons(C)" x_value
                                           in case either of
                                                  This {Ref=()} => STATUS'Ref
                                                | That {Con0=vid} => STATUS'C vid
                                           end




local open CompEnv
in
  fun patStatus'longvid env x =
      case (case (x) of
                LONG([],vid) => if (isUnbound'vid env vid) 
                                then This vid (*short: unbound*)
                                else That (Some vid, look'longvid "patStatus'longvid:1" env x) (*short: bound*)
              | _ => That (None, look'longvid "patStatus'longvid:2" env x)) of (*long: must be bound*)
          This vid => PVS'BindableV vid
        | That (a,VS'RT) => notYet "patvar:VS'RT"
        | That (a,VS'CT (recval,ids)) =>
          (case (a,(value'recval recval,ids)) of
               (Some vid,(_,IDSTATUS'V)) => PVS'BindableV vid
             | (None,(_,IDSTATUS'V)) => error "patStatus'longvid(LongV)"
             | (_,(x_value,IDSTATUS'E)) => PVS'E (excon0'value "parvar(E)" x_value)
             | (_,(x_value,IDSTATUS'C)) => let val either = ref_or_con0'value "patcons(C)" x_value
                                           in case either of
                                                  This {Ref=()} => error "parStatus'longvid(ref)"
                                                | That {Con0=vid} => PVS'C vid
                                           end)
end

  datatype patConStatus =
           PCS'Ref
         | PCS'C of vid
         | PCS'E of exname

local open CompEnv
in
  fun patconsStatus'longvid env x =
      case (look'longvid "patconsStatus'longvid:2" env x) of
          VS'RT => notYet "patcons:VS'RT"
        | VS'CT (recval,ids) =>
          (case (value'recval recval,ids) of
               (_,IDSTATUS'V) => error "patconsStatus'longvid(LongV)"
             | (x_value,IDSTATUS'E) => PCS'E (excon0'value "parvar(E)" x_value)
             | (x_value,IDSTATUS'C) => let val either = ref_or_con0'value "patcons(C)" x_value
                                       in case either of
                                              This {Ref=()} => PCS'Ref
                                            | That {Con0=vid} => PCS'C vid
                                       end)
end


(*----------------------------------------------------------------------
INDEX: comp'exp - split shad/env - does not work
----------------------------------------------------------------------*)

  fun comp'longvid shad env (longvid,pos) =
      case longvid of
          LONG([],vid) => if ((*true orelse*)
                              inShad shad vid
                              orelse isUnbound'vid env vid) (* rec idents will be unbound *)
                          then Comp.EXP'Var (vid,pos) (* must find value at runtime *)
                          else let val (v,_) = look'longvid "comp'longvid:1" env longvid
                               in Comp.EXP'Value v
                               end
        | _ => let val (v,_) = look'longvid "comp'longvid:2" env longvid
               in Comp.EXP'Value v
               end


  fun comp'mrule P E shad (MRULE (pat,exp)) = let val shad = shad'pat shad pat
                                              in Comp.MRULE (P pat,E shad exp)
                                              end

  fun comp'match P E shad (MATCH (pos,xs)) = Comp.MATCH (pos,map (comp'mrule P E shad) xs)

  fun comp'exprow Eshad exprow =
      case exprow of
          EXPROW xs => Comp.EXPROW (map (fn (lab,exp) => (lab,Eshad exp)) xs)
      
  fun U_comp'exp env P E D shad exp =
      case exp of
          EXP'Scon scon             => Comp.EXP'Scon scon 
        | EXP'Var x                 => comp'longvid shad env x
        | EXP'Record exprow         => Comp.EXP'Record (comp'exprow (E shad) exprow)
        | EXP'Select lab            => Comp.EXP'Select lab
        | EXP'Unit                  => Comp.EXP'Unit
        | EXP'Tuple exps            => Comp.EXP'Tuple (map (E shad) exps)
        | EXP'List exps             => Comp.EXP'List (map (E shad) exps)
        | EXP'Vector exps           => Comp.EXP'Vector (map (E shad) exps)
        | EXP'Seq exps              => Comp.EXP'Seq (map (E shad) exps)
        | EXP'Let (dec,exp)         => let val shad = shad'dec shad dec
                                       in Comp.EXP'Let (D dec,E shad exp)
                                       end
        | EXP'App (e1,e2)           => Comp.EXP'App (E shad e1,E shad e2)
        | EXP'Typed (exp,ty)        => Comp.EXP'Typed (E shad exp,ty)
        | EXP'Andalso (e1,e2)       => Comp.EXP'Andalso (E shad e1,E shad e2)
        | EXP'Orelse (e1,e2)        => Comp.EXP'Orelse (E shad e1,E shad e2)
        | EXP'Handle (exp,match)    => Comp.EXP'Handle (E shad exp,comp'match P E shad match)
        | EXP'Raise exp             => Comp.EXP'Raise (E shad exp)
        | EXP'If (e1,e2,e3)         => Comp.EXP'If (E shad e1,E shad e2,E shad e3)
        | EXP'While (e1,e2)         => Comp.EXP'While (E shad e1,E shad e2)
        | EXP'Case (exp,match)      => Comp.EXP'Case (E shad exp,comp'match P E shad match)
        | EXP'Fn match              => Comp.EXP'Fn (comp'match P E shad match)


local (*open CompEnv*)
in
  fun comp'exbind1 env b =
      case b of
          EXBIND1'New (vid,IGNORED_tyo) =>
          let val en = gen'exname vid
              val v = value'exval (exval'con0 en)
          in valenv'single (vid,(recval'value v,IDSTATUS'E))
          end
        | EXBIND1'Copy (vid,longvid) =>
          let val (v,ids) = look'longvid "comp'exbind1" env longvid
          in case ids of
                 IDSTATUS'V => error "exbind(copy):expected status E, got V"
               | IDSTATUS'C => error "exbind(copy):expected status E, got C"
               | IDSTATUS'E =>
                 let val exname = excon0'value "comp'exbind1(E)" v
                 in valenv'single (vid,(recval'value v,IDSTATUS'E))
                 end
          end
end


(*----------------------------------------------------------------------
INDEX: shad'dec
----------------------------------------------------------------------*)

  fun shad'valbind shad vb =
      case vb of        
          VALBIND'Rec vb => shad'valbind shad vb
        | VALBIND'Seq (pat,exp,vbo) => shad'pat shad pat
  fun shad'fvalbind shad (FVALBIND fs) =
      fold (fn shad => fn (pos,vid,clauses) => shad) shad fs
  fun shad'dec shad dec =
      case dec of
          DEC'Val vb => shad'valbind shad vb
        | DEC'Fval fb => shad'fvalbind shad fb
(*
        | DEC'Type typbind => env'tyenv (eval'typbind typbind)
        | DEC'Datatype (db,tbo) =>
        | DEC'DataReplicate (tycon,tyconlong) =>
        | DEC'Abstype (db,tbo,dec) =>
        | DEC'Exception exbind => env'valenv (eval'exbind env exbind)
        | DEC'Local (dec1,dec2) =>
*)
(*
        | DEC'Open xs => env'plusList (map (look'longstrid env) xs)
*)
  (* TODO: support DEC'Open for shad'dec *)
        | DEC'Empty => shad
        | DEC'Seq (dec1,dec2) => shad'dec (shad'dec shad dec1) dec2
        | _ => shad (* catch all *)


               
  fun exec'valbind P Eenv vb =
      case vb of        
          VALBIND'Rec vb => Basis.valenv'Rec (exec'valbind P Eenv vb)
        | VALBIND'Seq (pat,exp,vbo) =>
          case (P (Eenv exp) pat) of
              None => raise (Pack exval'Bind)
            | Some ve =>
              (case vbo of
                   None => ve
                 | Some vb => valenv'plus ve (exec'valbind P Eenv vb))

  fun exec'fvalbind Eenv (FVALBIND fs) =
      Basis.valenv'Rec (Basis.valenv'plusList
                    (map (fn (pos,vid,exp) =>
                             let val recval = Eenv exp
                             in Basis.valenv'single (vid,(recval,IDSTATUS'V))
                             end) fs))

  fun exec'exbind1 env b =
      case b of
          EXBIND1'New (vid,IGNORED_tyo) =>
          let val en = gen'exname vid
              val v = value'exval (exval'con0 en)
          in valenv'single (vid,(recval'value v,IDSTATUS'E))
          end
        | EXBIND1'Copy (vid,longvid) =>
          let val (v,ids) = look'longvid "exec'exbind1" env longvid
          in case ids of
                 IDSTATUS'V => error "exbind(copy):expected status E, got V"
               | IDSTATUS'C => error "exbind(copy):expected status E, got C"
               | IDSTATUS'E =>
                 let val exname = excon0'value "exec'exbind1(E)" v
                 in valenv'single (vid,(recval'value v,IDSTATUS'E))
                 end
          end

  fun exec'exbind env (EXBIND bs) =
      valenv'plusList (mapLR (exec'exbind1 env) bs)

  fun exec'typbind (TYPBIND xs) =
      tyenv'plusList (map (fn (_,tycon,_) =>
                              tyenv'single (tycon,valenv'empty)
                              ) xs)

  fun exec'datbind (DATBIND xs) =
      let fun loop ve te [] = (ve,te)
            | loop ve te ((IGNORED_tyvars,tycon,clauses)::xs) =
              let val ve' = 
                      valenv'plusList
                        (map (fn (vid,IGNORED_tyo) =>
                                 let val v = value'con0 vid
                                 in valenv'single (vid,(recval'value v,IDSTATUS'C))
                                 end) clauses)
                  val te' = tyenv'single (tycon,ve')
              in loop (valenv'plus ve ve') (tyenv'plus te te') xs
              end
      in loop valenv'empty tyenv'empty xs
      end

  fun exec'optWithType tbo =
      case tbo of Some tb => exec'typbind tb | None => tyenv'empty

  fun exec'dec_U P E D env dec = 
      case dec of
          DEC'Val vb => env'valenv (exec'valbind P (E env) vb)
        | DEC'Fval fb => env'valenv (exec'fvalbind (E env) fb)
        | DEC'Type typbind => env'tyenv (exec'typbind typbind)
        | DEC'Datatype (db,tbo) =>
          let val (ve,te) = exec'datbind db
              val env1 = env'plus (env'valenv ve) (env'tyenv te)
              val env2 = env'tyenv (exec'optWithType tbo)
          in env'plus env1 env2
          end
        | DEC'DataReplicate (tycon,tyconlong) =>
          let val ve = look'longtycon env tyconlong
              val env1 = env'valenv ve
              val env2 = env'tyenv (tyenv'single (tycon,ve))
          in env'plus env1 env2
          end
        | DEC'Abstype (db,tbo,dec) =>
          let val (ve,te_IGNORED) = exec'datbind db
              val env1 = env'valenv ve
              val env2 = env'tyenv (exec'optWithType tbo)
              val env3 = D (env'plusList [env,env1,env2]) dec
          in env'plus env2 env3
          end
        | DEC'Exception exbind => env'valenv (exec'exbind env exbind)
        | DEC'Local (dec1,dec2) =>
          let val env1 = D env dec1
          in D (env'plus env env1) dec2
          end
        | DEC'Open xs => env'plusList (map (look'longstrid env) xs)
        | DEC'Empty => env'empty
        | DEC'Seq (dec1,dec2) =>
          let val env1 = D env dec1
              val env2 = D (env'plus env env1) dec2
          in env'plus env1 env2
          end

end (* open Comp *)

(*----------------------------------------------------------------------
INDEX: Wed May  3 13:43:51 2006
----------------------------------------------------------------------*)

  (* look'long does not need to be in BASIS *)
  fun BASIS_look'long dotpath look env (LONG (strids,x)) =
      case strids of
          [] => look env dotpath x
        | strid::strids =>
          let val env = look'strid (strenv'env env) dotpath strid 
              val dotpath = dotpath ^ sof'strid strid ^ "."
          in BASIS_look'long dotpath look env (LONG (strids,x))
          end


(*
  fun uncomp'valenv (CompEnv.VALENV a) =
      valenv'assoc (filterMap'assoc (fn (k,VS'CT v) => Some (k,v)
                                      | (k,VS'RT _) => None) a)

  fun valenv'Rec (ve as CompEnv.VALENV a) =
      CompEnv.VALENV (map'assoc (fn (vid,VS'CT(recval,ids)) => (vid, VS'CT (recval'Rec recval (uncomp'valenv ve), ids))
                                  | (vid,VS'RT b) => (vid,VS'RT b)) a)
*)


(*----------------------------------------------------------------------
INDEX: compiled dec - cheat
----------------------------------------------------------------------*)
(*
  datatype cdec = CDEC of dec * env
  fun comp'dec env dec = CDEC (dec,env)
  fun exec'cdec (CDEC (dec,env)) = eval'dec env dec
  fun eval'dec env dec = let val cdec = comp'dec env dec
                         in exec'cdec cdec
                         end
*)

(*
    type cdec (* compiled dec *)
    val comp'dec : env -> dec -> cdec
    val exec'cdec : cdec -> (env,exval) either
*)



  local
      val isSome = fn Some x => true | _ => false
      val getSome = fn Some x => x | _ => abort "getSome"
  in
      fun SLOW_filterMap F xs = map getSome (filter isSome (map F xs))
  end

  fun filterMap FP [] = []
    | filterMap FP (x::xs) = (case (FP x) of 
                                  None => filterMap FP xs
                                | Some y => y :: filterMap FP xs)


  fun filterMap'assoc F A = assoc'list (filterMap F (list'assoc A))


  (*val mayRaise_exec'dec = D*)
(*
  fun mayRaise_eval'dec env dec =
      let val env' = comp'env env
          val (dec',CT_env) = comp'dec env' dec
          val RT_valenv = mayRaise_exec'dec valenv'empty dec'
          val res_env = env'CT_RT CT_env RT_valenv
      in res_env
      end

  fun eval'dec env dec =
      This (mayRaise_eval'dec env dec)
      handle (Pack exval) => That exval
*)


  structure Rec =
  struct
    datatype valenv = VALENV of (vid,recval*idstatus) assoc
    and recval = RECVAL'Simple of value | RECVAL'Rec of valenv -> value
    val valenv'empty = VALENV assoc'empty
    fun valenv'single kv = VALENV (assoc'single kv)
    fun valenv'plus (VALENV a1) (VALENV a2) = VALENV (assoc'plus a1 a2)
    val valenv'plusList = foldR valenv'plus valenv'empty
    local
        fun recval'Rec recval ve =
            RECVAL'Simple (case recval of
                               RECVAL'Simple v => v
                             | RECVAL'Rec f => f ve)
    in
    fun valenv'Rec (ve as VALENV a) =
      VALENV (map'assoc (fn (vid,(recval,ids)) => (vid,(recval'Rec recval ve, ids))) a)
    end     
    fun value'recval recval =
        case recval of
            RECVAL'Simple v => v
          | RECVAL'Rec f => f valenv'empty
    val recval'value = RECVAL'Simple
    val recval'fn = RECVAL'Rec
    fun valenv'valenv ve =
        VALENV (map'assoc (fn (k,(rv,ids)) => (k,(recval'value rv,ids))) (assoc'valenv ve))
  end

                                           
(*
  fun eval'pat_U P env recval pat =
      case pat of
          PAT'Var x => eval'patvar env recval  x 
        | PAT'Layered (vid,tyo_IGNORED,pat) =>
          (case (P env recval pat) of
               None => None
             | Some ve => Some (Rec.valenv'plus (Rec.valenv'single (vid,(recval,IDSTATUS'V))) ve))
        | _ =>
          let fun P' env v pat = mapOption valenv'Rvalenv (P env (Rec.recval'value v) pat)
          in case (inner_eval'pat_U P' env (Rec.value'recval recval) pat) of
                 None => None
               | Some ve => Some (Rec.valenv'valenv ve)
          end

  local fun P x = eval'pat_U P x
  in val eval'pat = P
  end
*)

(*
  fun eval'patvarRecursive env recval x =
      case (patStatus'longvid env x) of
          STATUS'BindableV vid => Some (Rec.valenv'single (vid,(recval,IDSTATUS'V)))
        | STATUS'Ref => error "patvar(Ref)"
        | STATUS'C vid =>
          let val (vid',vOpt) = con'value "patvar(C)" (Rec.value'recval recval)
          in case vOpt of
                 Some _ => None
               | None => if (eq'vid vid vid')
                         then (Some Rec.valenv'empty) else None
          end
        | STATUS'E exname =>
          let val (exname',vOpt) = excon'value "parvar(E)" (Rec.value'recval recval)
          in case vOpt of
                 Some _ => None
               | None => if (eq'exname exname exname')
                         then (Some Rec.valenv'empty) else None
          end
*)

(*
  fun eval'exp_U Pr Er D env exp =
      let fun E exp env = Rec.value'recval (Er exp env)
          fun P env v pat = mapOption valenv'Rvalenv (Pr env (Rec.recval'value v) pat)
      in case exp of
             EXP'Fn match => Rec.recval'fn (fn ve => value'closure (makeEvalClosure P E (match,env,ve)))
           | _ => Rec.recval'value (inner_eval'exp_U P E D env exp)
      end
*)


  fun exec'exp_U Pr Er D valenv exp =
      let fun E valenv exp = Rec.value'recval (Er valenv exp)
          fun P valenv v = Pr valenv (Rec.recval'value v)
      in case exp of
             Code.EXP'Fn match => Rec.recval'fn (fn ve => value'closure (makeEvalClosureRecursive P E (match,valenv,ve)))
           | _ => Rec.recval'value (inner_exec'exp_U P E D valenv exp)
      end

(*----------------------------------------------------------------------
INDEX: Mon May  8 16:16:23 2006
----------------------------------------------------------------------*)

(*
structure Eval =
struct
local open Eval in type dec = dec type env = env type exval = exval end
val eval'dec = if runCompiler then Comp.eval'dec else Interpret.eval'dec
end                
*)

val runCompiler = true;
Run.Nexec "val runCompiler = true";
Run.Nexec "Run.Nexec \"val runCompiler = true\"";


    datatype exbind = EXBIND of exbind1 list
    datatype valbind =
        VALBIND'Seq of pat * exp * valbind option
      | VALBIND'Rec of valbind
    and fvalbind = FVALBIND of (pos * vid * exp) list (* clauses expanded to expression *)
    and dec =
        DEC'Val of valbind
      | DEC'Fval of fvalbind
      | DEC'Exception of exbind
      | DEC'Local of dec * dec
      | DEC'Empty
      | DEC'Seq of dec * dec

(*----------------------------------------------------------------------
INDEX: Tue May  9 15:42:06 2006
----------------------------------------------------------------------*)

  fun zip'assoc sofk a1 a2 =
      assoc'list (map (fn (k,v) => (k, (v,look'assoc "zip'assoc" sofk a2 k))) (list'assoc a1))


(*----------------------------------------------------------------------
INDEX: pre fef recsive fixup - recval etc
----------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: Rec.runenv
----------------------------------------------------------------------*)

  structure Rec =
  struct

    datatype runenv = RUNENV of (vid,recval) assoc
    and recval = RECVAL of runenv -> value

    val runenv'empty = RUNENV assoc'empty
    fun runenv'single kv = RUNENV (assoc'single kv)
    fun runenv'plus (RUNENV a1) (RUNENV a2) = RUNENV (assoc'plus a1 a2)
    val runenv'plusList = foldR runenv'plus runenv'empty

    fun value'recval (RECVAL F) = F runenv'empty
    fun recval'value v = RECVAL (fn _ => v)
    val recval'fn = RECVAL

    fun runenv'runenv ve =
        RUNENV (map'assoc (fn (k,rv) => (k,recval'value rv)) (assoc'runenv ve))

  end

  fun runenv'Rec (ve as Rec.RUNENV a) =
      runenv'assoc (map'assoc (fn (vid,Rec.RECVAL F) => (vid,F ve)) a)




  fun exec'patvarRecursive runenv x recval =
      case (x) of
          PVS'BindableV vid => Some (Rec.runenv'single (vid,recval))
        | _ => mapOption Rec.runenv'runenv (exec'patvar runenv x (Rec.value'recval recval))


  fun exec'layeredRecursive P recval (vid,pat) =
      case (P recval pat) of
          None => None
        | Some ve => Some (Rec.runenv'plus (Rec.runenv'single (vid,recval)) ve)


  fun exec'patRecursive runenv recval pat =
      case pat of
          PAT'Var x => exec'patvarRecursive runenv x recval
        | PAT'Layered (vid,pat) => exec'layeredRecursive (exec'patRecursive runenv) recval (vid,pat)
        | _ => mapOption Rec.runenv'runenv (exec'pat runenv (Rec.value'recval recval) pat)


  fun makeEvalClosureRecursive E (match,runenv,ve) =
      closure'fn
          (fn vArg =>
              let val runenv = runenv'plus runenv (runenv'Rec ve)
              in case (exec'match E runenv vArg match) of
                     Some res => res
                   | None => raise (Pack (exval'match match vArg))
              end)



  fun U_exec'expRecursive E D runenv exp =
      case exp of
          Code.EXP'Fn match => Rec.recval'fn (fn ve => value'closure (makeEvalClosureRecursive E (match,runenv,ve)))
        | _ => Rec.recval'value (U_exec'exp E D runenv exp)


  fun exec'valbind_inRec Er runenv vb =
      case vb of        
          VALBIND'Rec vb => exec'valbind_inRec Er runenv vb (* 2nd rec ignored *)
        | VALBIND'Seq (pat,exp,vbo) =>
          case (exec'patRecursive runenv (Er runenv exp) pat) of
              None => raise (Pack exval'Bind)
            | Some ve =>
              (case vbo of
                   None => ve
                 | Some vb => Rec.runenv'plus ve (exec'valbind_inRec Er runenv vb))

  fun exec'valbind E Er runenv vb =
      case vb of        
          VALBIND'Rec vb => runenv'Rec (exec'valbind_inRec Er runenv vb)
        | VALBIND'Seq (pat,exp,vbo) =>
          case (exec'pat runenv (E runenv exp) pat) of
              None => raise (Pack exval'Bind)
            | Some ve =>
              (case vbo of
                   None => ve
                 | Some vb => runenv'plus ve (exec'valbind E Er runenv vb))

  fun exec'fvalbind Er runenv (FVALBIND fs) =
      runenv'Rec (Rec.runenv'plusList
                  (map (fn (pos,vid,exp) =>
                           let val recval = Er runenv exp
                           in Rec.runenv'single (vid,recval)
                           end) fs))



  local
      fun E  x = U_exec'exp          E D x
      and Er x = U_exec'expRecursive E D x
      and D  x = U_exec'dec          E Er D x
  in
  fun exec'dec dec =                          
      This (assoc'runenv (D runenv'empty dec))
      handle (Pack exval) => That exval
  end


(*----------------------------------------------------------------------
INDEX: Wed May 10 15:16:22 2006
----------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: hole/ref based fixup...
----------------------------------------------------------------------*)

  fun makeHole'vid vid = 
      let val F = ref None 
          fun tie f = (F := Some f)
          fun get() = case (!F) of Some f => f | None => abort ("recursive-hole:"^sof'vid vid)
          val v = value'closure (closure'fn (fn vArg => apply'value (get()) vArg))
      in (vid,(tie,v))
      end

  fun runstack'pushHoles runstack holes =
      fold (fn runstack => fn (vid,(_,v)) => runstack'push runstack (vid,v)) runstack holes

  fun tie'holes holes runstack =
      let val n = length holes
          fun loop [] = ()
            | loop ((vid,(tie,_))::xs) = let val v = look'runstack "tie'runstack" runstack (SLOT n,vid)
                                             val () = tie v
                                         in loop xs
                                         end
      in loop holes
      end

(*----------------------------------------------------------------------
INDEX: recvids'* - should be at compile time
----------------------------------------------------------------------*)
               
  fun vids'pat acc pat = 
      case pat of
          PAT'Wild => acc
        | PAT'Scon scon => acc
        | PAT'Var (PVS'BindableV vid) => vid :: acc
        | PAT'Var _ => acc
        | PAT'Record (PATROW (lps,_)) => fold (fn acc => fn (_,pat) => vids'pat acc pat) acc lps
        | PAT'Unit => acc
        | PAT'Tuple pats => fold vids'pat acc pats
        | PAT'List pats => fold vids'pat acc pats
        | PAT'Cons (x,pat) => vids'pat acc pat
        | PAT'Layered (vid,pat) => vids'pat (vid :: acc) pat


  fun vids'valbind vb =
      case vb of
          VALBIND'Rec vb => vids'valbind vb
        | VALBIND'Seq (pat,_,None) => vids'pat [] pat
        | VALBIND'Seq (pat,_,Some vb) => vids'pat [] pat @ vids'valbind vb



  fun UNFIXED_exec'fvalbind E runstack (FVALBIND fs) =
      fold (fn runstack => 
               fn (vid,exp) => 
                  runstack'push runstack (vid, E runstack exp)
                  ) runstack fs


  fun vids'fvalbind (FVALBIND fs) = map (fn (vid,_) => vid) fs

  fun exec'fvalbind E runstack fvalbind =
      let val vids = vids'fvalbind fvalbind (* should be computed at compile time *)
          val holes = map makeHole'vid vids
          val runstack = runstack'pushHoles runstack holes
          val runstack = UNFIXED_exec'fvalbind E runstack fvalbind
          val () = tie'holes holes runstack
      in runstack
      end

  fun exec'valbind E runstack vb =
      case vb of
          VALBIND'Rec vb =>  let val vids = vids'valbind vb (* should be computed at compile time *)
                                 val holes = map makeHole'vid vids
                                 val runstack = runstack'pushHoles runstack holes
                                 val runstack = exec'valbind E runstack vb
                                 val () = tie'holes holes runstack
                             in runstack
                             end
        | VALBIND'Seq (pat,exp,vbo) =>
          case (exec'pat runstack (E runstack exp) pat) of
              None => raise (Pack exval'Bind)
            | Some runstack =>
              (case vbo of
                   None => runstack
                 | Some vb => exec'valbind E runstack vb)



  fun comp'exbind1 env b =
      case b of
          EXBIND1'New (vid,tyo) => let val valenv = valenv'single (vid,VS'RTE depthDummy)
                                   in ([(depthDummy,vid)] ,valenv)
                                   end
        | EXBIND1'Copy (vid,longvid) =>
          (case (look'longvid "comp'exbind1" env longvid) of
               VS'RTV _ => error "exbind(copy)/VS'RT:expected status E, got V"
             | VS'RTE depth => let val valenv = valenv'single (vid,VS'RTE depth)
                               in ([],valenv)
                               end
             | VS'CT vstat =>
               (case (vstat) of
                    VSTAT'V _ => error "exbind(copy):expected status E, got V"
                  | VSTAT'C _ => error "exbind(copy):expected status E, got C"
                  | VSTAT'Ref => error "exbind(copy):expected status E, got Ref"
                  | VSTAT'E exname => let val valenv = valenv'single (vid,VS'CT vstat)
                                      in ([],valenv)
                                      end))

  fun comp'exbind env (EXBIND bs) =
      let val pairs = map (comp'exbind1 env) bs
      in (link (map #1 pairs), valenv'plusList (map #2 pairs))
      end


  fun comp'fvalbind E (FVALBIND fs) =
      let val pairs = map (fn (pos,vid,clauses) =>
                              let val exp = exp'clauses pos clauses
                                  val valenv = valenv'single (vid,VS'RTV dummy)
                                  val res = (dummy,vid,E exp)
                              in (res,valenv)
                              end) fs
      in (Code.FVALBIND (map #1 pairs), valenv'plusList (map #2 pairs))
      end



  fun REC_valenv'fvalbind (FVALBIND fs) =
      valenv'plusList (map (fn (_,vid,_) => valenv'single (vid,VS'RTV dummy)) fs)


  fun U_comp'exp E D env exp =
      case exp of
          EXP'Scon scon             => Code.EXP'Scon scon 
        | EXP'Var x                 => comp'longvid env x
        | EXP'Record exprow         => Code.EXP'Record (comp'exprow (E env) exprow)
        | EXP'Select lab            => Code.EXP'Select lab
        | EXP'Unit                  => Code.EXP'Unit
        | EXP'Tuple exps            => Code.EXP'Tuple (map (E env) exps)
        | EXP'List exps             => Code.EXP'List (map (E env) exps)
        | EXP'Vector exps           => Code.EXP'Vector (map (E env) exps)
        | EXP'Seq exps              => Code.EXP'Seq (map (E env) exps)
        | EXP'Let (dec,exp)         => let val (d, dec',env1) = D dummy env dec
                                           val exp' = E (env'plus env env1) exp
                                       in Code.EXP'Let (dec',exp')
                                       end
        | EXP'App (e1,e2)           => Code.EXP'App (E env e1,E env e2)
        | EXP'Typed (exp,ty)        => E env exp
        | EXP'Andalso (e1,e2)       => Code.EXP'Andalso (E env e1,E env e2)
        | EXP'Orelse (e1,e2)        => Code.EXP'Orelse (E env e1,E env e2)
        | EXP'Handle (exp,match)    => Code.EXP'Handle (E env exp,comp'match E env match)
        | EXP'Raise exp             => Code.EXP'Raise (E env exp)
        | EXP'If (e1,e2,e3)         => Code.EXP'If (E env e1,E env e2,E env e3)
        | EXP'While (e1,e2)         => expDerived'while (E env e1,E env e2)
        | EXP'Case (exp,match)      => Code.EXP'Case (E env exp,comp'match E env match)
        | EXP'Fn match              => Code.EXP'Fn (comp'match E env match)



  fun REC_valenv'fvalbind d (FVALBIND fs) =
      fold (fn (d,ve) =>
               fn (_,vid,_) =>
                  let val valenv = valenv'single (vid,VS'RTV d)
                  in (incDepth d,valenv'plus valenv ve)
                  end) (d,valenv'empty) fs



  fun UNREC_comp'fvalbind Eenv d (FVALBIND fs) =
      case (fold (fn (d,acc,ve) =>
                     fn (pos,vid,clauses) =>
                        let val exp = exp'clauses pos clauses
                            val valenv = valenv'single (vid,VS'RTV d)
                            val (d,exp) = Eenv d exp
                            val res = (d,vid,exp)
                        in (incDepth d,res::acc,valenv'plus valenv ve)
                        end) (d,[],valenv'empty) fs) 
       of (d,acc,ve) =>
          (d,Code.FVALBIND acc,ve)

  fun comp'fvalbind E env d fb =
          let val (d1,veRec) = REC_valenv'fvalbind d fb
              val env = env'plus env (env'valenv veRec)
              val (d,fb',ve) = UNREC_comp'fvalbind (E env) d fb
              val () = if (d=d1) then () else abort "comp:DEC'Fval:d<>d1"
          in (d,fb',ve)
          end




(*----------------------------------------------------------------------
INDEX: OLD valbind
----------------------------------------------------------------------*)

  fun valenv'valbind vb =
      case vb of        
          VALBIND'Rec vb1 => valenv'valbind vb1
        | VALBIND'Seq (pat,exp,None) => valenv'pat (let val (d,pat') = comp'pat env'empty dummy pat in pat' end)
        | VALBIND'Seq (pat,exp,Some vb) => valenv'plus (valenv'pat (
                                                                    let val (d,pat') =
                                                                            comp'pat env'empty dummy pat
                                                                    in pat'
                                                                    end
                                                                    )) (valenv'valbind vb)

  fun REC_valenv'valbind d vb =
      case vb of        
          VALBIND'Rec vb1 => (d,valenv'valbind vb1)
        | VALBIND'Seq (pat,exp,None) => (d,valenv'empty)
        | VALBIND'Seq (pat,exp,Some vb) => REC_valenv'valbind d vb


  fun UNREC_comp'valbind E env d vb =
      case vb of        
          VALBIND'Rec vb1 => let val (d, vb1',valenv) = UNREC_comp'valbind E env d vb1
                             in (d, Code.VALBIND'Rec vb1', valenv)
                             end
        | VALBIND'Seq (pat,exp,vbo) =>
            (case vbo of 
                 None  => let val (d,pat',valenv) = TWO_comp'pat env d pat
                              val (d,exp) = E env d exp
                              val vb' = Code.VALBIND'Seq (pat',exp,None)
                          in (d, vb',valenv)
                          end
               | Some vb1 =>
                 let val (d,pat',valenv1) = TWO_comp'pat env d pat
                     val (d,vb1',valenv2) = UNREC_comp'valbind E env d vb1
                     val (d,exp) = E env d exp
                     val vb' = Code.VALBIND'Seq (pat',exp,Some vb1')
                     val valenv = valenv'plus valenv1 valenv2
                 in (d, vb',valenv)
                 end)


  fun comp'valbind E env d vb =
      let val (d1,veRec) = REC_valenv'valbind d vb
          val env = env'plus env (env'valenv veRec)
          val (d,vb',ve) = UNREC_comp'valbind E env d vb
          val () = if (d=d1) then () else abort "comp:DEC'Vald<>d1"
      in (d, vb',ve)
      end


(*----------------------------------------------------------------------
INDEX: compc'* - compile-time
----------------------------------------------------------------------*)
(*...
local

(*----------------------------------------------------------------------
INDEX: shad
----------------------------------------------------------------------*)

  datatype shad = SHAD of vid list
  val shad'empty = SHAD []
  fun inShad (SHAD xs) x = exists (eq'vid x)  xs
  fun consShad (SHAD xs) x = SHAD (x::xs)

(*----------------------------------------------------------------------
INDEX: shad'pat
----------------------------------------------------------------------*)

  fun shad'patrow P shad (Code.PATROW (lps,{wildcard=IGNORED})) =
      fold (fn shad => fn (_,pat) => P shad pat) shad lps

local open Code
in
  fun U_shad'pat P shad pat =
      case pat of
          PAT'Wild => shad
        | PAT'Scon _ => shad
        | PAT'Var (PVS'BindableV (_,x)) => consShad shad x
        | PAT'Var _ => shad
        | PAT'Record pr => shad'patrow P shad pr
        | PAT'Unit => shad
        | PAT'Tuple pats => fold P shad pats
        | PAT'List pats => fold P shad pats
        | PAT'Cons (x,pat) => P shad pat
        | PAT'Layered (depth,vid,pat) => P (consShad shad vid) pat
end

  fun shad'pat shad pat = U_shad'pat shad'pat shad pat

(*----------------------------------------------------------------------
INDEX: comp'pat
----------------------------------------------------------------------*)


  fun comp'pats d P pats =
      map2nd rev (fold (fn (d,acc) => fn pat => let val (d,pat') = P d pat
                                                 in (d,pat'::acc)
                                                 end) (d,[]) pats)

  fun comp'patrow d P (PATROW (lps,{wildcard})) =
      let val (d,acc) = 
              fold (fn (d,acc) => fn (lab,pat) => let val (d,pat') = P d pat
                                                  in (d,(lab,pat')::acc)
                                                  end) (d,[]) lps
      in
          (d, Code.PATROW (rev acc, {wildcard=wildcard}))
      end

  fun U_comp'pat env d P pat =
      case pat of
          PAT'Wild                  => (d,Code.PAT'Wild)
        | PAT'Scon scon             => (d,Code.PAT'Scon scon)
        | PAT'Var x                 => map2nd Code.PAT'Var (patStatus'longvid d env x)
        | PAT'Record pr             => map2nd Code.PAT'Record (comp'patrow d P pr)
        | PAT'Unit                  => (d,Code.PAT'Unit)
        | PAT'Tuple pats            => map2nd Code.PAT'Tuple (comp'pats d P pats)
        | PAT'List pats             => map2nd Code.PAT'List (comp'pats d P pats)
        | PAT'Cons (x,pat)          => let val (d,pat') = P d pat
                                       in (d, Code.PAT'Cons (patconsStatus'longvid d env x, pat'))
                                       end
        | PAT'Typed (pat,ty)        => P d pat
        | PAT'Layered (vid,tyo,pat) => let val (d,pat') = P (incDepth d) pat
                                       in (d,Code.PAT'Layered (d,vid,pat'))
                                       end

  local
      fun P env d pat = U_comp'pat env d (P env) pat
  in
  val ONE_comp'pat = P
  end

in

  fun comp'pat env d pat =
      let val (d,pat') = ONE_comp'pat env d pat
          val (SHAD vids) = shad'pat shad'empty pat'
          val valenv = valenv'plusList (map (fn x => valenv'single  (x,VS'RTV dummy)) vids)
      in (d, pat',valenv)
      end

end (* local *)

...*)

(*----------------------------------------------------------------------
INDEX: Thu May 11 14:28:57 2006
----------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: remove fbind from machine 2
----------------------------------------------------------------------*)
(*
      and fvalbind = FVALBIND of (depth * vid * exp) list (* clauses expanded to expression *)
*)
        (*| DEC'Fval of fvalbind*)

(*
  fun exec'fvalbind E runstack (FVALBIND fs) =
      let val runstackRef = ref runstack
          val runstack = 
              fold (fn runstack => 
                       fn (depth,vid,exp) => 
                          runstack'push "exec'fvalbind" depth runstack (vid, exec'expRecursive E runstackRef exp)
                          ) (!runstackRef) fs
          val () = (runstackRef := runstack)
      in runstack
      end
*)
        (*| DEC'Fval fb => exec'fvalbind E runstack fb*)


  (*val Code_DEC_FVALBIND = Code.DEC'Fval o Code.FVALBIND*)

(*----------------------------------------------------------------------
INDEX: no special code for fval in Compiler2
----------------------------------------------------------------------*)

(*
  fun Code_DEC_FVALBIND xs = 
      Code.DEC'Val 
      (Code.VALBIND'Rec 
       (code_valbind'pes
        (map (fn (d,vid,exp) => (Code.PAT'Var (PVS'BindableV (d,vid)),exp)) xs)))

  fun comp'fvalbind E env d (FVALBIND fs) =
      case (fold (fn (d,stage1s,ve) =>
                     fn f as (pos,vid,clauses) =>
                        let val exp = exp'clauses (sof'vid vid) pos clauses
                            val valenv = valenv'single (vid,VS'RTV d)
                            val stage1 = (d,vid,exp)
                        in (incDepth d,stage1::stage1s,valenv'plus ve valenv)
                        end) (d,[],valenv'empty) fs)
       of (d,stage1s,ve) =>
          let val env = env'plus env (env'valenv ve)
              val dec' = Code_DEC_FVALBIND (map (fn (d1,vid,exp) =>
                                                   let val (d2,exp') = E env d exp
                                                       val () = if (d=d2) then () else abort "stage2_comp'fvalbind"
                                                   in (d1,vid,exp')
                                                   end) (rev stage1s))
          in (d,dec',env'valenv ve)
          end
*)



local open Code
  val sillySlot = SLOT 42
  val dummy = DEPTH 99
in
  fun expDerived'while (e1,e2) =
      let val X = genVid "while"
      in (*let val rec X = fn () => if e1 then (e2; X()) else () in X() end*)
          EXP'Let 
          (DEC'Val 
           (VALBIND'Rec 
            (VALBIND'Seq (PAT'Var (PVS'BindableV (dummy,X)),
                          EXP'Fn (MATCH ([MRULE (PAT'Unit,
                                                 EXP'If (e1,
                                                         EXP'Seq [e2, EXP'App (EXP'Var (dummy,sillySlot,X),EXP'Unit)], 
                                                         EXP'Unit)
                                                 )])),
                          None))),
           EXP'App (EXP'Var (dummy,sillySlot,X), EXP'Unit))
      end
end

(*
        | EXP'While (e1,e2)         => let val (d,e1) = E env d e1
                                           val (d,e2) = E env d e2
                                       in (d,expDerived'while (e1,e2))
                                       end
*)

(*----------------------------------------------------------------------
INDEX: Tue May 23 09:42:31 2006
----------------------------------------------------------------------*)

  type schemes = {
                  C     : exp -> cps,
                  A     : exp -> (atom -> cps) -> cps,
                  As    : exp list -> (atom list -> cps) -> cps,
                  F     : func -> afunc,
                  CM    : match -> cmatch,
(*
                  AM    : match -> (amatch -> cps) -> cps,
*)
                  AM    : match -> (atom -> cps) -> cmatch, (* new *)
                  CD    : dec -> cdec
(* dont need either! ?
                  AD    : dec -> (adec -> cps) -> cps,
                  ADv2  : dec -> (adec -> cdec) -> cdec
*)

  fun compile_C ({C,CM,CD,A,As,F,...}:schemes) : (exp -> cps) =
      fn exp => 
         case exp
          of EXP'Let (dec,exp)          => cps'let (CD dec,C exp)
           | EXP'Case (exp,match)       => A exp (fn atom => cps'case (atom, CM match))
           | EXP'App (exp1,exp2)        => A exp1 (fn atom1 => A exp2 (fn atom2 => CPS'App (atom1,atom2)))
           | EXP'Handle (exp,match)     => CPS'Handle (C exp, CM match)
(* have atom cases here: *)
           | EXP'Value v             => CPS'Atom (ATOM'Value v)
           | EXP'Var x               => CPS'Atom (ATOM'Var x)
           | EXP'Fn func             => CPS'Atom (ATOM'Fn (F func))
           | EXP'Tuple exps          => As exps (fn atoms => CPS'Atom (ATOM'Tuple atoms))
           | EXP'Raise exp           => A exp (fn atom => CPS'Atom (ATOM'Raise atom))
(* no need for this now:
           | _ => 
             (* EXP'Value, EXP'Var, EXP'Fn, EXP'Tuple, EXP'Raise *)
             A exp (fn a => CPS'Atom a)
*)

  fun compile_A ({C,A,As,F,AM,...}:schemes) : (exp -> (atom -> cps) -> cps) =
      fn exp => 
         fn c =>
            case exp 
(* maybe cleaner to move all this simple atomic cases into the _C version
above - leaving only the default case, which is:
try compile with _C - if an atom, ok, just call continuation with the atom,
otherwise gen a new var, bind it, and call the continuation with that
+ pos 1 other case
that is the c-duplicting version for case
*)
             of (*EXP'Value v             => c (ATOM'Value v)
              | EXP'Var x               => c (ATOM'Var x)
              | EXP'Fn func             => c (ATOM'Fn (F func))
              | EXP'Tuple exps          => As exps (fn atoms => c (ATOM'Tuple atoms))
              | EXP'Raise exp           => A exp (fn atom => c (ATOM'Raise atom))
(*
              | EXP'Case (exp,match)    => A exp (fn atom => AM match (fn amatch => c (ATOM'Case (atom,amatch))))
*)
(* this version may duplicate continuation -c
just how explosive will this be?
to avoid, just let it drop into catchall default case below 
*)
              | *)EXP'Case (exp,match)    => A exp (fn atom => cps'case (atom, AM match c))
(*
 NO - does not do early push
              | EXP'Let (dec,exp)       => AD dec (fn adec => A exp (fn atom => c (ATOM'Let (adec,atom))))
*)
              | _ => 
                (* EXP'Handle, EXP'App *)
                case (C exp)
                 of CPS'Atom atom => c(atom)
                  | cps => let val x = newVid()
                           in CPS'Let (CDEC'Val (PAT'V x,cps),
                                       c(ATOM'Var x))
                           end
(*  
  fun compile'match_AM ({A,AM,...}:schemes) : (match -> (amatch -> cps) -> cps) =
      fn match =>
         fn c =>
            case match
(* This cant be right!
Was expecting to duplicate continuation here - for code blowup
BUt it's not here.
What's happenning?
Is this lifting function call out of a given match to the surrounding contect?
That would be wrong!
*)
             of MATCH'Alt (m1,m2)       => AM m1 (fn am1 => AM m2 (fn am2 => c (AMATCH'Alt (am1,am2))))
              | MATCH'Rule (pat,exp)    => A exp (fn atom => c (AMATCH'Rule (pat,atom)))
*)

(*  
  fun compile'decC ({C,CD,ADv2,...}:schemes) : (dec -> cdec) =
      fn dec =>
         case dec
          of DEC'Seq (dec1,dec2)        => cdec'seq (CD dec1, CD dec2) 
           | DEC'Local (dec1,dec2)      => cdec'local (CD dec1, CD dec2) 
           | DEC'Val (pat,exp)          => cdec'val (pat, C exp)
           | _ => 
             (* DEC'Valrec, DEC'ExGen, DEC'ExCopy *)
             ADv2 dec (fn adec => CDEC'Atom adec)
  fun compile'decA ({A,AD,F,...}:schemes) : (dec -> (adec -> cps) -> cps) =
      fn dec =>
         fn c =>
            case dec
             of DEC'Seq (dec1,dec2)     => AD dec1 (fn ad1 => AD dec2 (fn ad2 => c (ADEC'Seq (ad1,ad2))))
              | DEC'Local (dec1,dec2)   => AD dec1 (fn ad1 => AD dec2 (fn ad2 => c (ADEC'Local (ad1,ad2))))
              | DEC'Val (pat,exp)       => A exp (fn atom => c (ADEC'Val (pat,atom)))
              | DEC'Empty               => c (ADEC'Empty)
              | DEC'Valrec binds        => c (ADEC'Valrec (map (fn (x,f) => (x,F f)) binds))
              | DEC'ExGen x             => c (ADEC'ExGen x)
              | DEC'ExCopy (x,y)        => c (ADEC'ExCopy (x,y))
*)


  fun fv'unionList T xs =
      fv_map T (fold (curry (fv_union (fn (xs,x) => x::xs))) ({free=varset'empty},[]) (rev xs))



(*
structure Machine_1 = MACHINE_1(val prefixNML = prefixNML
                                structure Prel = Prel
                                structure Assoc = Assoc
                                structure Pos = Pos
                                structure Misclay = Misclay
                                structure Atom = Atom
                                structure Value = Value)
                      
structure Compiler_1 = COMPILER_1(val prefixNML = prefixNML
                                  structure Prel = Prel
                                  structure Assoc = Assoc
                                  structure Pos = Pos
                                  structure Misclay = Misclay
                                  structure Atom = Atom
                                  structure Lang = Lang
                                  structure Value = Value
                                  structure Basis = Basis
                                  structure Machine = Machine_1)
                       

structure Machine_2 = MACHINE_2(val prefixNML = prefixNML
                                structure Prel = Prel
                                structure Assoc = Assoc
                                structure Atom = Atom
                                structure Value = Value)

structure Machine = Machine_2; (* for inside functor dev *)
                      
structure Compiler_2 = COMPILER_2(val prefixNML = prefixNML
                                  structure Prel = Prel
                                  structure Assoc = Assoc
                                  structure Pos = Pos
                                  structure Misclay = Misclay
                                  structure Atom = Atom
                                  structure Lang = Lang
                                  structure Value = Value
                                  structure Basis = Basis
                                  structure Machine = Machine_2)

*)

(*----------------------------------------------------------------------
INDEX: mutual tuples embedded vua exn
----------------------------------------------------------------------*)

  exception V_MutTuple of value array

  fun NewTuple (N:int) = 
      value'exn (V_MutTuple (Array.array(N,value'null)))

  val SetTupleElement : (value * int * value -> unit) =
      fn (tup,n,v) =>
         case (exn'value tup) 
          of Some (V_MutTuple A) => Array.update(A,n,v)
           | _ => abort "SetTupleElement"

  val DeTuple : (value * int -> value) =
      fn (tup,n) =>
         case (exn'value tup) 
          of Some (V_MutTuple A) => Array.sub(A,n)
           | _ => abort "DeTuple"


(*----------------------------------------------------------------------
INDEX: old record stuff - primitive in VALUE
----------------------------------------------------------------------*)
(*
    type record
    val value'record    : record -> value
    val record'empty    : record
    val record'valrow   : (lab * value) list -> record
    val record'values   : value list -> record
    val size'record     : record -> int
    val look'record     : string -> record -> lab -> value
    val isEmpty'record  : record -> bool
    val record'value    : string -> value -> record
*)

    | VALUE'Record of record

  and record = RECORD of (lab,value) assoc


  val record'empty = RECORD assoc'empty
               
  fun record'valrow xs = RECORD (assoc'list xs)

  fun record'values vs =
      record'valrow
          (let fun loop acc n [] = acc
                 | loop acc n (v::vs) = loop ((LAB'Num n,v)::acc) (n+1) vs
           in loop [] 1 vs
           end)

  fun size'record (RECORD a) = length (list'assoc a)
  fun look'record who (RECORD a) = look'assoc (who^":look'record") sof'lab a
          
  fun isEmpty'record (RECORD assoc) =
      case (list'assoc assoc)
       of [] => true
        | _ => false

  val value'unit = VALUE'Record record'empty

  fun value'pair (v1,v2) =
      VALUE'Record (record'values [v1,v2])

  fun record'value who v =
      case v of
          VALUE'Record record => record
        | _ => error (who^":record'value")

  fun unit'value who v =
      let val who = who^":unit'value"
          val record = record'value who v
      in if (isEmpty'record record) then ()
         else error (who^":non-empty-record")
      end
               
  fun pair'value who v =
      let val who = who^":pair'value"
          val record = record'value who v
      in if (size'record record <> 2)
         then error (who^"[size<>2]")
         else
             let val v1 = look'record who record (LAB'Num 1)
                 val v2 = look'record who record (LAB'Num 2)
             in (v1,v2)
             end
      end

  (* copied from PRETTY.ML *)
  fun lay'id (ID x) = Word x

  fun lay'lab lab =
      case lab of
          LAB'Id id => lay'id id
        | LAB'Num n => layInt n

  fun lay'record V (RECORD assoc) =
      layBrace
      (layCommaSep (map (fn (lab,v) => lay'lab lab ++ Word "=" ++ V v
                         ) (rev (* hack to get tuple order correct *)
                            (list'assoc assoc))))



  fun sortPairList'record (RECORD assoc) =
      sort (fn ((lab1,_),(lab2,_)) => lt'lab lab1 lab2) (list'assoc assoc)
                               
  fun eq'record eq'value r1 r2 =
      let fun loop [] [] = true
            | loop [] _ = error "eq'record:arg2 has extra elems"
            | loop _ [] = error "eq'record:arg1 has extra elems"
            | loop ((lab1,v1)::lvs1) ((lab2,v2)::lvs2) =
              if (not (eq'lab lab1 lab2)) then error "eq'record:diff labs"
              else (eq'value v1 v2) andalso loop lvs1 lvs2
      in loop (sortPairList'record r1) (sortPairList'record r2)
      end



(*----------------------------------------------------------------------
INDEX: specific infix levels -- not yet
----------------------------------------------------------------------*)
(*    
fun isInfixN n im x =
    case (getIM im x) of Some (PA{prec=n',...}) => (n=n') | _ => false

fun infixNvidMatch n im tok =
    case (vidMatch tok) of
        None => None
      | Some x => if (isInfixN n im x) then Some x else None
*)

(*
  fun array'map f a = 
      Array.tabulate (Array.length(a), fn n => f (Array.sub(a,n)))

  fun ARRAY(c) =
      mkConv (fn xs => value'array (array'map (from c) xs),
              fn who => fn v => array'map (back c who) (array'value who v))
*)

  fun VECTOR(c) =
      mkConv (fn xs => value'vector (Vector.map (from c) xs),
              fn who => fn v => Vector.map (back c who) (vector'value who v))


(*----------------------------------------------------------------------
INDEX: Thu Jun  8 12:28:19 2006
----------------------------------------------------------------------*)

    | DEC'ExGen     of vid
    | DEC'ExCopy    of vid * vid


       | DEC'ExGen x                => Lang.DEC'Exception (EXBIND [EXBIND1'New (x,None)])
       | DEC'ExCopy (x,y)           => Lang.DEC'Exception (EXBIND [EXBIND1'Copy (x,long y)])


    | ADEC'ExGen    of vid
    | ADEC'ExCopy   of vid * vid

           | DEC'ExGen x                => CDEC'Atom (ADEC'ExGen x)
           | DEC'ExCopy (x,y)           => CDEC'Atom (ADEC'ExCopy (x,y))

       | ADEC'ExGen x               => ({bound=varset'single x,free=varset'empty},FV.ADEC'ExGen x)
       | ADEC'ExCopy (x,y)          => ({bound=varset'single x,free=varset'single y},FV.ADEC'ExCopy (x,y))

       | ADEC'ExGen x                   => Word "exception" ++ lay'vid x
       | ADEC'ExCopy (x,y)              => Word "exception" ++ lay'vid x ++ Word "=" ++ lay'vid y


           | ADEC'ExGen x               => let val q = newQ (INFO'Vid x)
                                               val r = LOC'Stack q
                                           in (SEQ'I (INS'NewException (q,x)), env'single (x,r))
                                           end
           | ADEC'ExCopy (x,y)          => let val r = look'env "compile'adec" env y
                                           in (SEQ'Empty, env'single (x,r))
                                           end


    | DEC'Local     of dec * dec
    | DEC'Local     of dec * dec
            | DEC'Local _           => abort "bound'dec/local"
       | DEC'Local (dec1,dec2)      => Lang.DEC'Local (D dec1, D dec2)
    | CDEC'Local    of cdec * cdec
    | ADEC'Local    of adec * adec
  val cdec'local = 
      fn (CDEC'Atom ad1, CDEC'Atom ad2) => CDEC'Atom (ADEC'Local (ad1,ad2))
       | (cd1,cd2) => CDEC'Local (cd1,cd2)

           | DEC'Local (dec1,dec2)      => cdec'local (CD dec1, CD dec2) 
       | CDEC'Local (cdec1,cdec2) =>
         let
             val ({bound=bound1},D1) = CD cdec1
             val ({bound=bound2},D2) = CD cdec2
         in
             ({bound = (*bound1 && *)bound2},
              fn freeBody =>
                 let val ({free=free2},d2) = D2 (*freeBody*) bound2
                     val ({free=free1},d1) = D1 free2
                     (*val dec = FV.CDEC'Local (d1,d2)*)
                     val save = freeBody -- bound2
                     val dec = FV.CDEC'Val ({save= list'varset save}, pat'varset bound2,
                                            FV.CPS'Let (d1, FV.CPS'Let (d2, FV.CPS'Atom (atom'varset bound2))))
                 in ({free = free1},dec)
                 end)
         end
       | ADEC'Local (adec1,adec2) =>
         let
             val ({bound=bound1,free=free1},d1) = AD adec1
             val ({bound=bound2,free=free2},d2) = AD adec2
             val dec = FV.ADEC'Val (pat'varset bound2,
                                    FV.ATOM'Let (d1, FV.ATOM'Let (d2, atom'varset bound2)))
         in
             ({bound = bound2,
               free = free1 && (free2 -- bound1)},
              dec)
         end

  val old_cpsTransform : Embed_dec -> {bound:vid list} * cdec 


  fun old_cpsTransform embed_dec =
      let val {CD=cps,(*...*)C=_,CM=_,A=_,As=_,F=_,AM=_} = cps_schemes
          val {CD=fv,(*...*)P=_,AD=_,C=_,A=_,F=_,CM=_,AM=_} = fv_schemes
          val ({bound},CDEC) = fv (cps embed_dec)
          val ({free=_},cdec) = CDEC bound
      in ({bound = list'varset bound}, cdec)
      end



  type vid
  val old_generateCode : {bound:vid list} -> cdec -> code


  fun old_generateCode {bound=xs} cdec =
      let val {(*A,CD,...*)Pd,C,Ad,A,At,Ft,CMd,AMd,AMt,CD,AD}:schemes = schemes
          val (env,cc) = CD (env'empty,cdec)
          val (seq1,rs) = compileSequence (curry A env) (map ATOM'Var xs) (* avoid compileSequence/A - use look'env direct *)
          val (seq2,r) = (case rs of
                              [] => (SEQ'Empty,LOC'Value value'unit)
                            | [r] => (SEQ'Empty, r)
                            | _ => let val q = newQ (INFO'Top cdec)
                                   in (seq'tuple (q,rs), LOC'Stack q)
                                   end)
          val code = code'Seq (seq1 $ seq2, CODE'Return r)
      in cc (code)
      end


                val ({bound=xs},cdec) = Cps.old_cpsTransform embed_dec
                val () = echoCpsDec cdec
                val code = Compile.old_generateCode {bound=xs} cdec


  (*fun lay'cps_dec cdec = lay'embed_dec (Cps.uncps'cdec cdec)*)
  fun lay'cps_dec cdec = Cps.lay'cdec cdec
  val echoCpsDec = echo'lay (layTag "CPS... " lay'cps_dec)


    | ATOM'Let      of adec * atom
    = CPS'Let       of cdec * cps

  and cdec
    = CDEC'Seq      of cdec * cdec
    | CDEC'Val      of {save:vid list} * pat * cps (* generated exceptions may be free in pat *)
    | CDEC'Atom     of adec

  and adec
    = ADEC'Empty
    | ADEC'Seq      of adec * adec
    | ADEC'Val      of pat * atom
    | ADEC'Valrec   of (vid * afunc) list


    = CPS'Let       of cdec * cps
    | ATOM'Let      of adec * atom

  and cdec
    = CDEC'Seq      of cdec * cdec
    | CDEC'Val      of pat * cps
    | CDEC'Atom     of adec

  and adec
    = ADEC'Empty
    | ADEC'Seq      of adec * adec
    | ADEC'Val      of pat * atom
    | ADEC'Valrec   of (vid * afunc) list


  val cps'let =
      fn (CDEC'Atom adec,CPS'Atom atom) => CPS'Atom (ATOM'Let (adec,atom))
       | (cdec,cps) => CPS'Let (cdec,cps)

  val cdec'seq = 
      fn (CDEC'Atom am1, CDEC'Atom am2) => CDEC'Atom (ADEC'Seq (am1,am2))
       | (cd1,cd2) => CDEC'Seq (cd1,cd2)

  val cdec'val = 
      fn (pat,CPS'Atom atom) => CDEC'Atom (ADEC'Val (pat, atom))
       | (pat,cps) => CDEC'Val (pat,cps)

(*
  fun old_cps_CD ({(*C,CD,F,...*)C,CM,CD,A,As,F,AM}:cps_schemes) : (dec -> cdec) =
      fn dec =>
         case dec
          of DEC'Seq (dec1,dec2)        => cdec'seq (CD dec1, CD dec2) 
           | DEC'Val (pat,exp)          => cdec'val (pat, C exp)
           | DEC'Empty                  => CDEC'Atom (ADEC'Empty)
           | DEC'Valrec binds           => CDEC'Atom (ADEC'Valrec (map (fn (x,f) => (x,F f)) binds))
*)


  and cdec
    = CDEC'Seq      of cdec * cdec
    | CDEC'Val      of {save:vid list} * pat * cps (* generated exceptions may be free in pat *)
    | CDEC'Atom     of adec

  and adec
    = ADEC'Empty
    | ADEC'Seq      of adec * adec
    | ADEC'Val      of pat * atom
    | ADEC'Valrec   of (vid * afunc) list


(*
  fun old_cpsTransform embed_dec =
      let val {CD=cps,(*...*)C=_,CM=_,A=_,As=_,F=_,AM=_} = cps_schemes
          val {CD=fv,(*...*)P=_,AD=_,C=_,A=_,F=_,CM=_,AM=_} = fv_schemes
          val ({bound},CDEC) = fv (cps embed_dec)
          val ({free=_},cdec) = CDEC bound
          val xs = list'varset bound
          val cps = CPS'Let (cdec,CPS'Atom (atom'tuple (map ATOM'Var xs)))
      in ({bound = xs}, cps)
      end
*)

(*
  fun pat'varset varset = case (list'varset varset) of [v] => PAT'V v | vs => PAT'Tuple (map PAT'V vs)
  fun atom'varset varset = case (list'varset varset) of [v] => FV.ATOM'Var v | vs => FV.ATOM'Tuple (map FV.ATOM'Var vs)
*)


  fun fv_letC F (({bound=boundDec},DEC),({free=freeBody},body)) =
      let val ({free=freeDec},dec) = DEC freeBody
          val save = freeBody -- boundDec
          val free = freeDec && save
      in ({free=free}, F (dec,body))
      end

  fun fv_seqC F (({bound=bound1},D1),({bound=bound2},D2)) =
      ({bound = bound1 && bound2},
       fn freeBody =>
          let val ({free=free2},d2) = D2 freeBody
              val ({free=free1},d1) = D1 free2
          in ({free = free1}, F (d1,d2))
          end)

  fun fv_valC F (({bound=boundPat,free=freePat},pat),({free=freeExp},exp)) =
      ({bound = boundPat},
       fn freeBody =>
          let val save = (freeBody -- boundPat) && freePat
              val free = freeExp && save
          in ({free=free}, F ({save = list'varset save},pat,exp))
          end)


  fun fv_letA F (({bound=boundDec,free=freeDec},dec),({free=freeBody},body)) =
      let val save = freeBody -- boundDec
          val free = freeDec && save
      in ({free=free}, F (dec,body))
      end

  fun fv_seqA F (({bound=bound1,free=free1},d1),({bound=bound2,free=free2},d2)) =
      ({bound = bound1 && bound2,
        free = free1 && (free2 -- bound1)},
       F (d1,d2))

  fun fv_valA F (({bound=boundPat,free=freePat},pat),({free=freeExp},exp)) =
      ({bound = boundPat,
        free = freePat && freeExp},
       F (pat,exp))

              fn CPS'Let (cdec,cps)         => fv_letC FV.CPS'Let (CD cdec, C cps)
       | ATOM'Let (adec,atom)       => fv_letA FV.ATOM'Let (AD adec, A atom)


  fun fv_CD ({(*P,C,CD,AD,...*)P,CD,AD,C,A,F,CM,AM}:fv_schemes) =
      fn CDEC'Seq (cdec1,cdec2)     => fv_seqC FV.CDEC'Seq (CD cdec1, CD cdec2)
       | CDEC'Val (pat,cps)         => fv_valC FV.CDEC'Val (P pat, C cps)
       | CDEC'Atom ad               => fv_atomDec (AD ad)


  fun fv_AD ({(*P,CM,A,AD,F,...*)P,CD,AD,C,A,F,CM,AM}:fv_schemes) =
      fn ADEC'Empty                 => ({bound=varset'empty,free=varset'empty}, FV.ADEC'Empty)
       | ADEC'Seq (adec1,adec2)     => fv_seqA FV.ADEC'Seq (AD adec1, AD adec2)
       | ADEC'Val (pat,atom)        => fv_valA FV.ADEC'Val (P pat, A atom)
       | ADEC'Valrec binds          => fv_valrec (map (fn (x,f) => (x, F f)) binds)

      fn CPS'Let (cdec,cps)             => (Word "let" @@ CD cdec) // (Word "in" @@ C cps) // Word "end"
       | ATOM'Let (adec,atom)           => (Word "let" ++ AD adec) // (Word "in" ++ A atom) // Word "end"


  fun lay_CD ({P,C,A,F,AS,CM,AM,CD,AD}:lay_schemes) =
      fn CDEC'Seq (cdec1,cdec2)         => CD cdec1 ++ CD cdec2
       | CDEC'Val ({save},pat,cps)      => lay'save save ++ Word "val" ++ P pat ++ Word "=" ++ C cps
       | CDEC'Atom adec                 => AD adec

  fun lay_AD ({P,C,A,F,AS,CM,AM,CD,AD}:lay_schemes) =
      fn ADEC'Empty                     => Blank
       | ADEC'Seq (adec1,adec2)         => AD adec1 ++ AD adec2
       | ADEC'Val (pat,atom)            => Word "val" ++ P pat ++ Word "=" ++ A atom
       | ADEC'Valrec []                 => Blank (*abort "<lay/valrec/[]>"*)
       | ADEC'Valrec ((x1,f1)::binds)   => Word "val rec" ++ (lay'vid x1 ++ Word "=" ++ F f1)
                                                // layVert (map (fn (x,f) => Word "and" ++ (lay'vid x ++ Word "=" ++ F f)) binds)


    | INFO'Top      of cdec
       | INFO'Top cdec          => "" (*"DEC_"*)
        | INFO'Top cdec         => N_lay'cdec {depth=n} cdec

  fun N_lay'cdec n x = 
      let val {P,C,A,F,AS,CM,AM,CD,AD} = N_lay_schemes n
      in CD x
      end

      fun CD0 _ = Word "<cdec>"
      fun AD0 _ = Word "<adec>"
      and CD  n x = lay_CD    (schemes(n-1)) x
      and AD  n x = lay_AD    (schemes(n-1)) x

                     CD    : cdec      -> {bound: varset              } * (varset -> {free:varset} * FV.cdec),
                     AD    : adec      -> {bound: varset, free: varset} * FV.adec,

  fun fv_atomDec ({bound=boundDec,free=freeDec},dec) =
      ({bound=boundDec}, 
       fn freeBody => let val free = freeDec && (freeBody -- boundDec)
                      in ({free=free}, FV.CDEC'Atom dec) 
                      end)
      
  fun fv_valrec binds =
      let val (bf,binds) = fv_binds binds
      in (bf, FV.ADEC'Valrec binds)
      end
      and CD  x = fv_CD (makeSchemes()) x
      and AD  x = fv_AD (makeSchemes()) x

                      CD    : cdec -> layout,
                      AD    : adec -> layout}
      and CD  x = lay_CD    (makeSchemes()) x
      and AD  x = lay_AD    (makeSchemes()) x

  fun lay'cdec x =
      let val {P,C,A,F,AS,CM,AM} = lay_schemes
      in CD x
      end


                  CD    : (env * cdec -> env * code_context), (* accumulated bindings *)
                  AD    : (env * adec -> seq * env) (* just new bindings *)
                  }

      and CD  x = CD_compile'cdec           (makeSchemes()) x
      and AD  x = AD_compile'adec           (makeSchemes()) x

  fun CD_compile'cdec ({(*Pd,C,...*)Pd,C,Ad,A,At,Ft,CMd,AMd,AMt}:schemes) : (env * cdec -> env * code_context) =
      fn (env,cdec) =>
         case cdec
          of CDEC'Seq (cdec1,cdec2)         => let val (env,cc1) = CD (env,cdec1)
                                                   val (env,cc2) = CD (env,cdec2)
                                               in (env, cc1 o cc2)
                                               end
           | CDEC'Atom adec                 => let val (seq,env1) = AD (env,adec)
                                               in (env ++ env1, fn code => code'Seq (seq,code))
                                               end
           | CDEC'Val ({save},pat,cps) =>
             let
                 val code1 = C (env,cps)
                 val xns = enumerateFrom 0 save
                 val fill = seq'Is (map (fn (x,n) => let val r = look'env "compile'cdec" env x
                                                     in INS'SetContFrameElem (n, r)
                                                     end) xns)
                 (* environment replaced here *)
                 val env = env'plusList (map (fn (x,n) => env'single (x,LOC'Frame (FLOC (n,INFO'Vid x)))) xns)
                 val (seqPat,env1) = Pd (env,pat,DELAYED'R LOC'Cret)
                 fun cc code =
                     let val push = SEQ'I (INS'PushContinuation ({frame_size=length save},
                                                                 code'Seq (seq'FatBarBind seqPat,code),
                                                                 {info=(This cps,save)}))
                     in code'Seq (push $ fill, code1)
                     end
             in (env ++ env1,cc)
             end


  fun AD_compile'adec ({(*P,Ft,A,AD,...*)Pd,C,Ad,A,At,Ft,CMd,AMd,AMt}:schemes) : (env * adec -> seq * env) =
      fn (env,adec) =>
         case adec 
          of ADEC'Empty                 => (SEQ'Empty,env)
           | ADEC'Seq (adec1,adec2)     => let val (seq1,env1) = AD (env,adec1)
                                               val (seq2,env2) = AD (env ++ env1,adec2)
                                           in (seq1 $ seq2, env1 ++ env2)
                                           end
           | ADEC'Val (pat,atom)        => let val (seq1,rdel) = Ad (env,atom) 
                                               val (seq2,env') = Pd (env,pat,rdel)
                                           in (seq1 $ seq'FatBarBind seq2 , env')
                                           end
           | ADEC'Valrec binds => 
             let val trips = map (fn (x,f) => (x,f,newQ (INFO'Vid x))) binds
                 val env' = env'plusList (map (fn (x,_,q) => env'single (x,LOC'Stack q)) trips)
                 val envR = env ++ env'
                 val (seqA,seqB) = 
                     fold (fn (seqA1,seqB1) => 
                              fn (x,f,q) => let val (seqA2,seqB2) = Ft (envR,{name=sof'vid x},f,q)
                                            in (seqA1 $ seqA2, seqB1 $ seqB2)
                                            end) (SEQ'Empty,SEQ'Empty) trips
             in (seqA $ seqB, env')
             end

           | ATOM'Let (adec,atom)   => let val (seq1,env1) = AD (env,adec)
                                           val (seq2,rdel) = Ad (env ++ env1, atom)
                                       in (seq1 $ seq2, rdel)
                                       end


           | ATOM'Let (adec,atom)   => let val (seq1,env1) = AD (env,adec)
                                           val seq2 = At (env ++ env1,atom,q)
                                       in seq1 $ seq2 
                                       end

          of CPS'Let (cdec,cps)         => let val (env,CC) = CD (env,cdec)
                                               val code = C (env,cps)
                                           in CC code
                                           end

(*----------------------------------------------------------------------
INDEX: Wed Jun 14 13:10:53 2006
----------------------------------------------------------------------*)


local
    datatype cont   = CONTINUE of cont * code * frame | OVERAPP of cont * value list | POPH of cont | FINISH
    datatype control = CONTROL of {D: int ref, C: cont ref, H: (int * cont * code * frame) list ref}
in

  type control =control 

  fun makeControl() = CONTROL {D = ref 0, C = ref FINISH, H = ref []}
          
  val PushContinuation : (control * int * code -> unit) =
      fn (CONTROL{D,C,H},N,code) => (inc D; C := CONTINUE (!C,code,NewFrame N))

  val PushReApp : (control * value list -> unit) =
      fn (CONTROL{D,C,H},args) => (inc D; C := OVERAPP (!C,args))

  val PushHandler : (control * int * code -> unit) =
      fn (CONTROL{D,C,H},N,code) => (H := (!D,!C,code,NewFrame N) :: !H; inc D; C := POPH (!C))


  val SetContFrameElement : (control * int * value -> unit) =
      fn (CONTROL{D,C,H},n,v) =>
         case (!C) of CONTINUE(_,_,frame) => SetFrame (frame,n,v)
                    | _ => abort "SetContFrameElement"
                                                   
  val SetXcontFrameElement : (control * int * value -> unit) =
      fn (CONTROL{D,C,H},n,v) =>
         case (!H) of [] => abort "SetXcontFrameElement"
                    | (_,_,_,frame)::_ => SetFrame (frame,n,v)

  val Raise : (icounts * jump * control * value -> res) =
      fn (icounts,Jump,CONTROL{D,C,H},v) =>
         case (!H) of [] => That (exval'value "Raise" v)
                    | (Hdepth,cont,code,frame)::handlers => (count'raise (!D-Hdepth) icounts;
                                                             H:=handlers; C:=cont; Jump (code, NewEnv (frame,{args=[v]})))
            

  val rec Return : (icounts * jump * control * value -> res) =
      fn (icounts,Jump,control as CONTROL{D,C,H},v) => (count'return icounts;
         case (!C) of CONTINUE (cont,code,frame) => (dec D; C:=cont; Jump (code, NewEnv (frame,{args=[v]})))
                    | OVERAPP (cont,args) => (dec D; C:=cont; TailCall (icounts,Jump,control,v,args))
                    | POPH cont => (dec D; C:=cont; 
                                    H := (case !H of [] => abort "PopHandler" | _::handlers => handlers);
                                    Return (icounts,Jump,control,v))
                    | FINISH => This v)

  and TailCall : (icounts * jump * control * value * value list -> res) =
      fn (icounts,Jump,control,func,args) => (count'tail icounts (length args);
         case (exn'value func) 
          of Some (name,V_Closure (code,frame,N)) => let val n_supplied = length args
                                                     in if (n_supplied < N) 
                                                        then let val () = count'pap icounts
                                                                 val pap = MakePap (name,func,args,N)
                                                             in Return (icounts,Jump,control,pap)
                                                             end
                                                        else let val args = if (n_supplied = N) then args else
                                                                            let val (args1,args2) = splitArgs N args
                                                                                val () = count'reapp icounts
                                                                                val () = PushReApp (control,args2)
                                                                            in args1
                                                                            end
                                                             in Jump (code, NewEnv (frame,{args=args}))
                                                             end
                                                     end
           | Some (name,V_Pap (func,early)) => TailCall (icounts,Jump,control,func,early @ args)
           | _ => let val v = fold apply'value func args (* interpreted functions, builtins, :=/ref/con0/excon0 application *)
                  in Return (icounts,Jump,control,v)
                  end)

end


local

    datatype cont = CONT of cont * continue | POPH of cont | FINISH

    datatype control = CONTROL of {D: int ref, C: cont ref, H: (int * cont * code * frame) list ref}

in

  type control = control 

  fun makeControl() = CONTROL {D = ref 0, C = ref FINISH, H = ref []}

  val PushContinue : (control * continue -> unit) =
      fn (CONTROL{D,C,H},continue) => (inc D; C := CONT (!C,continue))


  val PushHandler : (control * int * code -> unit) =
      fn (CONTROL{D,C,H},N,code) => (H := (!D,!C,code,NewFrame N) :: !H; inc D; C := POPH (!C))


  val SetContFrameElement : (control * int * value -> unit) =
      fn (CONTROL{D,C,H},n,v) =>
         case (!C) of CONT (cont,CONTINUE(_,frame)) => SetFrame (frame,n,v)
                    | _ => abort "SetContFrameElement"
                                                   
  val SetXcontFrameElement : (control * int * value -> unit) =
      fn (CONTROL{D,C,H},n,v) =>
         case (!H) of [] => abort "SetXcontFrameElement"
                    | (_,_,_,frame)::_ => SetFrame (frame,n,v)

  val Raise : (icounts * jump * control * value -> res) =
      fn (icounts,Jump,CONTROL{D,C,H},v) =>
         case (!H) of [] => That (exval'value "Raise" v)
                    | (Hdepth,cont,code,frame)::handlers => (count'raise (!D-Hdepth) icounts;
                                                             H:=handlers; C:=cont; Jump (code, NewEnv (frame,{args=[v]})))
            
  val PopHandler : (control -> unit) =
      fn CONTROL{D,C,H} => H := (case !H of [] => abort "PopHandler" | _::handlers => handlers)

  val rec ReturnContinue : (control -> continue option) =
      fn (control as CONTROL{D,C,H}) =>
         case (!C) of CONT(cont,x) => (dec D; C:=cont; Some x)
                    | POPH(cont) => (dec D; C:=cont; PopHandler control; ReturnContinue control)
                    | FINISH => None


end



  fun eval'code code =
      let val control = makeControl()
          val fail = (fn () => abort "evalCode:fail")
          val icounts = new'icounts()
          fun Jump (code,env) = executeCode (icounts,Jump,control,env,code,{fail=fail})
          val env = NewEnv (NewFrame 0,{args=[]})
          val res = Jump (code,env)
          val () = if !display_counts then (echo'lay lay'icounts icounts) else ()
      in res
      end


  fun eval'code code =
      let val icounts = new'icounts()
          fun Execute (code,env) =
              let val control = makeControl()
                  val fail = (fn () => abort "MACHINE/Apply/fail")
                  fun Jump (code,env) = executeCode (icounts,Jump,(*Execute,*)control,env,code,{fail=fail})
              in Jump (code,env)
              end
          val env = NewEnv (NewFrame 0,{args=[]})
          val res = Execute (code,env)
          val () = if !display_counts then (echo'lay lay'icounts icounts) else ()
      in res
      end


  fun makeEvalClosure E (match,env) =
      closure'fn (fn vArg => case (eval'match E env vArg match) of
                                 Some res => res
                               | None => raise (Pack exval'Match))


  fun makeEvalClosureRecursive E (match,env,ve) =
      let (*val env = env'plus env (env'valenv (valenv'Rec ve))*) (*no,no!*)
      in closure'fn
          (fn vArg =>
              let val env = env'plus env (env'valenv (valenv'Rec ve))
              in case (eval'match E env vArg match) of
                     Some res => res
                   | None => raise (Pack exval'Match)
              end)
      end


           | _ => let val v = fold apply'value func args (*  builtins, :=/ref/con0/excon0 application *)
                  in Return (icounts,Jump,TailCall,control,v)
                  end)




          of Some (name,V_Closure (closure as Closure (code,frame,N))) => let val n_supplied = length args
                                                     in if (n_supplied < N) 
                                                        then let val () = count'pap icounts
                                                                 val pap = MakePap (name,closure,args)
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


  fun MakePap (Jump,name,closure as Closure(code,frame,N),early) = 
      let val tag = name ^ ":" ^ stringOfInt (length early) ^ "/" ^ stringOfInt N
          fun Decompile() = layVert [Word "Name:" @@ Word tag,
                                     Word "Pap:" @@ layArgs lay'value early]
          fun Apply v = Jump(makeControl(),code,NewEnv (frame,{args=early@[v]})) (* for use by interpreter *)
      in value'exn ("pap:"^tag, V_Pap (closure,early), Decompile, Apply)
      end

  fun NewFn (Jump,{frame_size=F,num_args=N},code,{name,info=afunc}) = 
      let val frame = NewFrame F
          fun Decompile() = layVert [Word "Name:" @@ Word name,
                                     Word "Def:" @@ lay'afunc afunc,
                                     Word "Frame:" @@ lay'frame frame, 
                                     Word "Code:" //`` lay'code code]
          fun Apply v = Jump(makeControl(),code,NewEnv (frame,{args=[v]})) (* for use by interpreter *)
      in value'exn ("closure:"^name, V_Closure (Closure (code,frame,N)), Decompile, Apply)
      end


(*----------------------------------------------------------------------
INDEX: Thu Jun 15 10:37:10 2006
----------------------------------------------------------------------*)
  fun cps_A ({(*C,A,As,F,AM,...*)C,CM,CD,A,As,F,AM}:cps_schemes) : (exp -> (atom -> cps) -> cps) =
      fn exp => 
         fn c => (*... causes first blowup for: isSymid in LEX.ML
            case exp 
             of EXP'Case (exp,match)    => A exp (fn atom => cps'case (atom, AM match c)) (* may duplicate continuation *)
              | _ =>  ...*)
                case (C exp)
                 of CPS'Atom atom => c(atom)
                  | cps => let val x = newVid()
                           (*in CPS'Let (CDEC'Val (PAT'V x,cps), c(ATOM'Var x))*)
                           in CPS'Let (PAT'V x,cps, c(ATOM'Var x))
                           end


  fun cps_AM ({(*A,AM,...*)C,CM,CD,A,As,F,AM}:cps_schemes) : (match -> (atom -> cps) -> cmatch) =
      fn match =>
         fn c =>
            case match
             of MATCH'Alt (m1,m2)       => CMATCH'Alt (AM m1 c, AM m2 c) (* continuation duplicated here *)
              | MATCH'Rule (pat,exp)    => CMATCH'Rule (pat, A exp c)



  fun old_lay'icounts (Icounts {NT,NX,NF,NR,NC,CP,BI,DC,DE,DR,DT,PC,PCX,PH,ST,SF,SC,SX,MS,MC,ME,MG,FB,RET,RAISE,SKIP,TAIL,PAP,REAPP}) =
      Word "icounts: " @@ (layVert [Word "Tuples:" @@ 
                                         layCommaSep [lay'count "New" NT,
                                                      lay'count "Set" ST,
                                                      lay'count "Get" DT],
                                    Word "Functions:" @@ 
                                         layCommaSep [lay'count "New" NF (*@@ layBracket (lay'count "#1" NF1) *),
                                                      lay'count "SetFrame" SF,
                                                      lay'count "Pap" PAP],
                                    Word "BuiltinApp: " @@ layInt (!BI),
                                    Word "Calls:" @@ 
                                         layCommaSep [lay'count "Tail" TAIL (*@@ layBracket (lay'count "#1" TAIL1)*)
                                                                             ],
                                    Word "Continuations" @@ 
                                         layCommaSep [lay'count "Push" PC,
                                                      lay'count "ReApp" REAPP,
                                                      lay'count "SetFrame" SC,
                                                      lay'count "Return" RET,
                                                      lay'count "Max-Cont-Depth" PCX
                                                      ],
                                    Word "Match::" @@ 
                                         layCommaSep [lay'count "Scon" MS,
                                                      lay'count "Con" MC,
                                                      lay'count "Excon" ME,
                                                      lay'count "GenEx" MG],
                                    Word "Destruct:" @@
                                         layCommaSep [lay'count "Copy" CP,
                                                      lay'count "Con" DC,
                                                      lay'count "Excon" DE,
                                                      lay'count "Ref" DR],
                                    Word "Exceptions" @@ 
                                         layCommaSep [lay'count "NewEx" NX,
                                                      lay'count "PushHandle" PH,
                                                      lay'count "SetFrame" SX,
                                                      lay'count "Raise" RAISE,
                                                      lay'count "Skip" SKIP],
                                    Word "(1+Push+ReApp+PushHandler)-(Return+Skip) ="
                                         @@ layInt (1 + !PC + !PH + !REAPP - (!RET + !SKIP))
                                    ])


           | CPS'LetC ({save},pat,cps1,cps2) =>
             let val code1 = C (env,cps1)
                 val xns = enumerateFrom 0 save
                 val fill = seq'Is (map (fn (x,n) => let val r = look'env "compile'cdec" env x
                                                     in INS'SetContFrameElem (n, r)
                                                     end) xns)
                 (* environment replaced here *)
                 val env = env'plusList (map (fn (x,n) => env'single (x,LOC'Frame (FLOC (n,INFO'Vid x)))) xns)
                 val (seqPat,env1) = FatBarBind Pd (env,pat,DELAYED'R LOC'Cret)
                 fun cc code =
                     let val push = SEQ'I (INS'PushContinuation ({frame_size=length save},
                                                                 seqPat $$ code,
                                                                 {info=(This cps1,save)}))
                     in push $ fill $$ code1
                     end
                 val (env,CC) = (env ++ env1,cc)
                 val code = C (env,cps2)
             in CC code
             end

  type code_context = code -> code


    | INS'PushContinuation  of {frame_size:int} * code * {info: (cps,{POPHANDLE:unit}) either * vid list}

             ATOM'Tuple []          => abort "Ad/tuple/0"
           | ATOM'Tuple [_]         => abort "Ad/tuple/1"

           | ATOM'Tuple []              => abort "A/tuple/0" (*(SEQ'Empty, LOC'Value value'unit) (* special case *)*)
           | ATOM'Tuple [_]             => abort "A/tuple/1"

           | ATOM'Tuple []              => abort "At/tuple/0" (*SEQ'I (INS'Copy (q,LOC'Value value'unit)) (* special case *)*)
           | ATOM'Tuple [_]             => abort "At/tuple/1"

(*----------------------------------------------------------------------
INDEX: Wed Jun 21 12:05:57 2006
----------------------------------------------------------------------*)


    | CPS'OldLet    of pat * atom * cps
    | CPS'LetRec    of (vid * afunc) list * cps

    | ATOM'OldLet   of pat * atom * atom
    | ATOM'LetRec   of (vid * afunc) list * atom

  val cps'oldlet = CPS'OldLet
  val atom'oldlet = ATOM'OldLet

  val cps'let = 
      fn (pat,CPS'Atom atom1,CPS'Atom atom2) => CPS'Atom (atom'oldlet (pat,atom1,atom2))
       | (pat,CPS'Atom atom1,cps2) => cps'oldlet (pat,atom1,cps2)
       | (pat,cps1,cps2) => CPS'LetC (pat,cps1,cps2)

  val cps'letrec = 
      fn (binds,CPS'Atom atom) => CPS'Atom (ATOM'LetRec (binds,atom))
       | (binds,cps) => CPS'LetRec (binds,cps)

(*
           | EXP'OldLet (dec,exp)          => CD dec (A exp c) (* special, enough, or need AD? *)
*)
         (*| EXP'Case (exp,match)       => () --Need Special? *)
(*
           | EXP'App (func,args)        => ()
           | EXP'Handle (exp,match)     => ()
*)

       | CPS'OldLet (pat,atom1,cps2) =>
         let val ({free=freeP,bound=boundP},pat) = P pat
             val ({free=free1},atom1) = A atom1
             val ({free=free2},cps2) = C cps2
             val atom = FV.CPS'OldLet (pat,atom1,cps2)
             val free = free1 && freeP && (free2 -- boundP)
         in ({free = free},atom)
         end
       | CPS'LetRec (binds,cps1) => let val ({bound,free},binds) = fv_binds (map (fn (x,f) => (x, F f)) binds)
                                        val ({free=free1},cps1) = C cps1
                                        val free = free && (free1 -- bound)
                                        val cps = FV.CPS'LetRec (binds,cps1)
                                    in ({free = free},cps)
                                    end

       | ATOM'OldLet (pat,atom1,atom2) =>
         let val ({free=freeP,bound=boundP},pat) = P pat
             val ({free=free1},atom1) = A atom1
             val ({free=free2},atom2) = A atom2
             val atom = FV.ATOM'OldLet (pat,atom1,atom2)
             val free = free1 && freeP && (free2 -- boundP)
         in ({free = free},atom)
         end
       | ATOM'LetRec (binds,atom1) => let val ({bound,free},binds) = fv_binds (map (fn (x,f) => (x, F f)) binds)
                                          val ({free=free1},atom1) = A atom1
                                          val free = free && (free1 -- bound)
                                          val atom = FV.ATOM'LetRec (binds,atom1)
                                      in ({free = free},atom)
                                      end
(*
  fun pat'varset varset = pat'tuple (map PAT'V (list'varset varset))
  fun atom'varset varset = FV.atom'tuple (map FV.ATOM'Var (list'varset varset))
*)
(*
       | CPS'OldLet (pat,atom1,cps2)    => lay'let1 (Word "letA") (P pat) (A atom1) // (C cps2)
       | CPS'LetRec (binds,cps)         => lay'letrec F binds // C cps
*)
(*
       | ATOM'OldLet (pat,atom1,atom2)  => lay'let1 (Word "letA") (P pat) (A atom1) // (A atom2)
       | ATOM'LetRec (binds,atom)       => lay'letrec F binds // A atom
*)
(*
                | CPS'OldLet (_,_,cps)              => CT cps
                | CPS'LetRec (_,cps)             => CT cps
*)


           | CPS'OldLet (pat,atom1,cps2)    => let val (seq1,rd) = Ad (env,atom1) 
                                               val (seq',env') = FatBarBind Pd (env,pat,rd)
                                               val code2 = C (env ++ env',cps2)
                                           in seq1 $ seq' $$ code2
                                           end
           | CPS'LetRec (binds,cps)         => let val (env,seq) = compile_LetRecBinds {Ft=Ft} (env,binds)
                                                   val code = C (env,cps)
                                               in seq $$ code
                                               end


           | ATOM'LetRec (binds,atom)   => let val (env,seq) = compile_LetRecBinds {Ft=Ft} (env,binds)
                                               val code = Aret (env,atom)
                                           in seq $$ code
                                           end
           | ATOM'OldLet (pat,atom1,atom2)  => let val (seq1,rd) = Ad (env,atom1) 
                                               val (seq',env') = FatBarBind Pd (env,pat,rd)
                                               val code2 = Aret (env ++ env',atom2)
                                           in seq1 $ seq' $$ code2
                                           end

           | ATOM'LetRec (binds,atom)   => let val (env,seq1) = compile_LetRecBinds {Ft=Ft} (env,binds)
                                               val (seq2,rd) = Ad (env,atom)
                                           in (seq1 $ seq2, rd)
                                           end
           | ATOM'OldLet (pat,atom1,atom2)  => let val (seq1,rd1) = Ad (env,atom1) 
                                               val (seq',env') = FatBarBind Pd (env,pat,rd1)
                                               val (seq2,rd2) = Ad (env ++ env',atom2)
                                           in (seq1 $ seq' $ seq2, rd2)
                                           end

           | ATOM'LetRec (binds,atom)   => let val (env,seq1) = compile_LetRecBinds {Ft=Ft} (env,binds)
                                               val (seq2,r) = A (env,atom)
                                           in (seq1 $ seq2, r)
                                           end
           | ATOM'OldLet (pat,atom1,atom2)  => let val (seq1,rd) = Ad (env,atom1) 
                                               val (seq',env') = FatBarBind Pd (env,pat,rd)
                                               val (seq2,r) = A (env ++ env',atom2)
                                           in (seq1 $ seq' $ seq2, r)
                                           end

           | ATOM'LetRec (binds,atom)   => let val (env,seq1) = compile_LetRecBinds {Ft=Ft} (env,binds)
                                               val seq2 = At (env,atom,q)
                                           in seq1 $ seq2
                                           end
           | ATOM'OldLet (pat,atom1,atom2)  => let val (seq1,rd) = Ad (env,atom1) 
                                               val (seq',env') = FatBarBind Pd (env,pat,rd)
                                               val seq2 = At (env ++ env',atom2,q)
                                           in seq1 $ seq' $ seq2 
                                           end


             (case (prim)
              of PRIM'Builtin bi        => if (arity'builtin bi = 1) 
                                           then let val (seq,r) = A (env,atom)
                                                in seq $ SEQ'I (INS'BuiltinApp (q,bi,[r]))
                                                end
                                           else let val (seq1,rd) = Ad (env,atom)
                                                    val (seq2,rs) = forceDelayedN (arity'builtin bi) rd
                                                in seq1 $ seq2 $ SEQ'I (INS'BuiltinApp (q,bi,rs))
                                                end
               | PRIM'Con0 c            => let val (seq,r) = A (env,atom)
                                           in seq $ SEQ'I (INS'NewCon (q,c,r))
                                           end
               | PRIM'Ref               => let val (seq,r) = A (env,atom)
                                           in seq $ SEQ'I (INS'NewRef (q,r))
                                           end)

  fun FatBarBind (Pd:Pd) : (env * pat * r_delayed -> seq * env) =
      fn (env,pat,rd) => 
         if (exhaustive'pat pat) then Pd (env,pat,rd)
         else let val (seq,env) = Pd (env,pat,rd)
              in (SEQ'FatBar (seq, seq'Bind), env)
              end
                                        (*val (seq2,env') = FatBarBind Pd (env,pat,rd)*)
                 (*val (seqPat,env') = FatBarBind Pd (env,pat,rd)*)



(*
                 val xns = enumerateFrom 0 save
                 val fill = seq'Is (map (fn (x,n) => let val r = raw_look'env "compile'cdec" env x
                                                     in INS'SetContFrameElem (n, r)
                                                     end) xns)
                 val env = env'plusList (map (fn (x,n) => env'single (x,LOC'Frame (FLOC (n,INFO'Vid x)))) xns)
*)

(*
                 val xns = enumerateFrom 0 save
                 val fill = seq'Is (map (fn (x,n) => let val r = raw_look'env "compile'cps:Handle" env x
                                                     in INS'SetXcontFrameElem (n, r)
                                                     end) xns)
                 val env = env'plusList (map (fn (x,n) => env'single (x,LOC'Frame (FLOC (n,INFO'Vid x)))) xns)
*)

(*
             val save_slots = enumerateFrom 0 save
             val seqB = seq'Is (map (fn (x,n) => let val r = raw_look'env "compile'func_target" env x
                                                 in INS'SetFrameElement (LOC'Stack q, n, r)
                                                 end) save_slots)
             
             val env = env'plusList (map (fn (x,n) => env'single (x,LOC'Frame (FLOC (n,INFO'Vid x)))) save_slots
                                     @ map (fn (x,n) => env'single (x,LOC'Arg n)) (enumerateFrom 0 args))
*)


(*
  fun echo'lay prefix lay x =
      (prefixed_output_layout prefix (squash_to 90 (lay x));
       echo "")
*)
                                               (*val () = echo'lay "AMt - " (N_lay'atom {depth=99}) TOP*)

fun flipPrimOpt() = 
    case (if (!optimize_prim_app) then (false,"off") else (true,"on")) 
     of (p,s) => (Prel.echo ("**PrimOpt->"^s); optimize_prim_app:=p)



(*----------------------------------------------------------------------
INDEX: compile env : vid -> r
----------------------------------------------------------------------*)

(*
  local open Assoc Atom
  in datatype env = ENV of (vid,r) assoc
     val env'empty = ENV assoc'empty
     fun env'plus (ENV a1, ENV a2) = ENV (assoc'plus a1 a2)
     val env'plusList = fold (curry env'plus) env'empty
     fun raw_look'env who (ENV a) k = look'assoc ("look'env("^who^")") sof'vid a k
     fun env'single kv = ENV (assoc'single kv)
  end
      
  fun look'env who : (env * vid) -> (env * seq * r) =
      fn (env,vid) => let val r = raw_look'env who env vid
                      in (env,SEQ'Empty,r)
                      end
*)

(*----------------------------------------------------------------------
INDEX: Wed Jun 28 11:10:12 2006
----------------------------------------------------------------------*)

    | LOC'Null
    | LOC'Null                      (* used when compiling raise-expressions - will/must never be inspected at runtime *)
        | LOC'Null          => Word "loc_null"
           | LOC'Null               => abort "GetLoc/null"

           | ATOM'Raise atom            => let val (env,seq,r) = A (env,atom)
                                           in seq $$ CODE'Raise r
                                           end

           | ATOM'Raise atom        => let val (env,seq,r) = A (env,atom)
                                       in (env, seq $ SEQ'Raise r, LOC'Null)
                                       end
           | ATOM'Raise atom            => let val (env,seq,r) = A (env,atom)
                                           in (env, seq $ SEQ'Raise r)
                                           end

(*----------------------------------------------------------------------
INDEX: Wed Jun 28 14:40:47 2006
----------------------------------------------------------------------*)

(*
           | EXP'PrimApp (prim,exp)     => A exp (fn atom => CPS'Atom (ATOM'PrimApp (prim,atom)))
           | EXP'Tuple exps             => if (!right_to_left_tuples) 
                                           then As (rev exps) (fn atoms => CPS'Atom (atom'tuple (rev atoms)))
                                           else As exps (fn atoms => CPS'Atom (atom'tuple atoms))
           | EXP'Value (v,_)            => CPS'Atom (ATOM'Value v)
           | EXP'Var x                  => CPS'Atom (ATOM'Var x)
           | EXP'Fn func                => CPS'Atom (ATOM'Fn (F func))
           | EXP'NewEx x                => CPS'Atom (ATOM'NewEx x)
*)



(*----------------------------------------------------------------------
INDEX: Wed Jun 28 20:34:49 2006
----------------------------------------------------------------------*)

  fun cps_C ({C,C',CD,CD',CM,A,As,F}:cps_schemes) : (exp -> cps) =
      fn exp => 
         case exp
          of EXP'Raise exp              => A exp (fn atom => CPS'Raise atom)
           | EXP'App (func,args)        => if (!right_to_left_args)
                                           then As (rev args) (fn args => A func (fn func => CPS'App (func,rev args)))
                                           else A func (fn func => As args (fn args => CPS'App (func,args)))
           | EXP'Handle (exp,match)     => CPS'Handle (C exp, CM match)
           | EXP'Let (dec,exp)          => CD dec (C exp)
           | EXP'Case (exp,match)       => A exp (fn atom => cps'case (atom, CM match))
           | _                          => A exp CPS'Return

  fun cps_CD ({C,C',CD,CD',CM,A,As,F}:cps_schemes) : (dec -> cps -> cps) =
      fn dec => fn body =>
         case dec
          of DEC'Seq (dec1,dec2)        => CD dec1 (CD dec2 body)
           | DEC'Val (pat,exp)          => transform_cps'let (pat, C exp, body)
           | DEC'Empty                  => body
           | DEC'Valrec binds           => cps'letrec (map (fn (x,f) => (x,F f)) binds, body)


  fun cpsSequence X =
      fn xs =>
         fn c =>
            case xs of
                [] => c []
              | x::xs => X x (fn y => cpsSequence X xs (fn ys => c (y :: ys)))

  fun cpsSequence X [] c = c []
    | cpsSequence X (x::xs) c = X x (fn y => cpsSequence X xs (fn ys => c (y :: ys))) 

  fun cpsSequence X =
      let fun XS xs c = 
              case xs 
               of [] => c [] 
                | x::xs => X x (fn y => XS xs (fn ys => c (y::ys)))
      in XS
      end


  fun cps_As ({C,C',CD,CD',CM,A,As,F}:cps_schemes) : (exp list -> (atom list -> cps) -> cps) =
      fn exps =>
         fn c =>
            case exps of
                [] => c []
              | exp::exps => A exp (fn atom => As exps (fn atoms => c (atom :: atoms)))


                      As    : exp list -> (atom list -> cps) -> cps,



  fun cps_As ({C,C',CD,CD',CM,A,F}:cps_schemes) : (exp list -> (atom list -> cps) -> cps) =
      cpsSequence A

(*----------------------------------------------------------------------
INDEX: Mon Jul 24 17:59:57 2006
----------------------------------------------------------------------*)

(*
                  | This(_,Some(That y,ids))      => (ids, That y)
                  | This(_,Some(This v,ids))      => (ids, This v)
*)
(*
            | (Basis.IDSTATUS'E, This v) => let val valenv = valenv'single (vid,(This v, Basis.IDSTATUS'E))
                                            in (Embed.DEC'Empty, valenv)
                                            end
            | (Basis.IDSTATUS'E, That y) =>  let val uid = makeUid vid
                                                 val valenv = valenv'single (vid,(That y, Basis.IDSTATUS'E))
                                                 val dec = Embed.DEC'Empty
                                             in (dec,valenv)
                                             end
*)
 (*
            | (Basis.IDSTATUS'E, That y) =>  let val uid = makeUid vid
                                                 val valenv = valenv'single (vid,(That uid, Basis.IDSTATUS'E))
                                                 val dec = Embed.DEC'Val (Embed.PAT'V uid, Embed.EXP'Var y)
                                             in (dec,valenv)
                                             end
*)


(*----------------------------------------------------------------------
INDEX: Tue Aug  1 22:35:39 2006
----------------------------------------------------------------------*)

(*
  fun embed'patrow P (PATROW (lps,{wildcard})) =
      Code.PATROW (map (fn (lab,pat) => (lab,P pat)) lps,{wildcard=wildcard})
*)

(*----------------------------------------------------------------------
INDEX: pat
----------------------------------------------------------------------*)

  fun U_embed'pat env P pat =
      case pat of
          PAT'Wild                  => Embed.PAT'Wild
        | PAT'Scon scon             => Embed.PAT'Scon scon
        | PAT'Var x                 => let val (acc,res) = embed'var env x [] in res end
        | PAT'Record pr             => P (tuple'patrow pr)
        | PAT'Unit                  => Embed.pat'tuple []
        | PAT'Tuple pats            => Embed.pat'tuple (map P pats)
        | PAT'List pats             => P (pat'list pats)
        | PAT'Cons (x,pat)          => embed'cons env x (P pat)
        | PAT'Typed (pat,ty)        => P pat
        | PAT'Layered (vid,tyo,pat) => Embed.PAT'Layered (makeUid vid,P pat)

  local fun P env pat = U_embed'pat env (P env) pat
  in val ONE_embed'pat = P
  end
      

  fun valenv'runtimeVars xs = 
      valenv'plusList (map (fn x => valenv'single (x,(That (makeUid x),Basis.IDSTATUS'V))) xs) 

  fun OLD_embed'pat env pat =
      let val pat' = ONE_embed'pat env pat
          val valenv = valenv'runtimeVars (map deUid (boundVars'pat pat'))
      in (pat',valenv)
      end


        | VALBIND'Rec vb1 =>
          let val xfs = xfs'valbind vb1
              val valenv = valenv'runtimeVars (map (fn (x,_) => x) xfs)
              val env = env'plus env (env'valenv valenv)
              val dec = Embed.DEC'Valrec (map (fn (x,match) => (makeUid x,Embed.func'def (embed'match E env match))) xfs)
          in (dec,valenv)
          end


(*----------------------------------------------------------------------
INDEX: Mon Sep  4 11:49:37 2006
----------------------------------------------------------------------*)

              
(*
  val rec executeCode : (icounts * jump * control * env * code * {fail:step} -> res) =
      fn (icounts,J,control,env,code,{fail}) =>
         case code
          of CODE'Seq (seq,code)        => executeSeq (icounts,J,control,env,seq,
                                                       {next = fn()=> executeCode (icounts,J,control,env,code,{fail=fail}),
                                                        fail = fail})
           | CODE'FatBar (code1,code2)  => (count'fatbar icounts;
                                            executeCode (icounts,J,control,env,code1,
                                                         {fail = fn()=> executeCode (icounts,J,control,env,code2, {fail=fail})}))
           | CODE'Return r              => Return (icounts,J,control,GetLoc(env,r))
           | CODE'TailCall (func,args)  => TailCall (icounts,J,control,GetLoc(env,func),GetLocs(env,args))
           | CODE'Raise r               => Raise (icounts,J,control,GetLoc(env,r))
           | CODE'If (im,code1,code2)   => if (count'im icounts im; executeIm (env,im))
                                           then executeCode (icounts,J,control,env,code1,{fail=fail})
                                           else executeCode (icounts,J,control,env,code2,{fail=fail})
*)

    (*= CODE'Seq              of seq * code*)

(*
  and seq =
      SEQ'Empty
    | SEQ'Seq               of seq * seq
    | SEQ'FatBar            of seq * seq
    | SEQ'Raise             of r
    | SEQ'If                of im * seq * seq
    | SEQ'I                 of instruction
*)

(*
  and seq = (* UN-terminated code sequence *)
      SEQ'Empty
    | SEQ'Seq               of seq * seq
    | SEQ'FatBar            of seq * seq
    | SEQ'Raise             of r
    | SEQ'I                 of instruction
    | SEQ'If                of im * seq * seq
*)
(*
  val rec executeSeq : (icounts * jump * control * env * seq * nf * goto -> res) =
      fn (icounts,J,control,env,seq,nf as {next,fail},goto) =>
         case seq
          of SEQ'Empty                  => next()
           | SEQ'Seq (seq1,seq2)        => executeSeq (icounts,J,control,env,seq1, 
                                                       {next = fn()=> executeSeq (icounts,J,control,env,seq2,{next=next,fail=fail},
                                                                                  goto),
                                                        fail = fail},
                                                       goto)
           | SEQ'FatBar (seq1,seq2)     => (count'fatbar icounts;
                                            executeSeq (icounts,J,control,env,seq1, 
                                                        {next = next,
                                                         fail = fn()=> executeSeq (icounts,J,control,env,seq2,{next=next,fail=fail},
                                                                                   goto)},
                                                        goto))
           | SEQ'Raise r                => Raise (icounts,J,control,GetLoc (env,r))
           | SEQ'I instruction          => executeInstruction (icounts,Interpret(icounts,J),control,env,instruction,nf,goto)
           | SEQ'If (im,seq1,seq2)      => if (count'im icounts im; executeIm (env,im))
                                           then executeSeq (icounts,J,control,env,seq1,nf,goto)
                                           else executeSeq (icounts,J,control,env,seq2,nf,goto)
*)



                  (*of CODE'Seq (seq,code)        => executeSeq (icounts,J,control,env,seq,
                                                               {next = fn()=> doCode (code,{fail=fail},goto),
                                                                fail = fail},goto)*)

(*
  and lay'seq seq =
      case seq
       of SEQ'Empty                 => (*Blank*) Word "<EMPTY>"
        | SEQ'Seq (seq1,seq2)       => lay'seq seq1 // lay'seq seq2
        | SEQ'FatBar (seq1,seq2)    => layFatBar "<S-FAT-BAR>" (lay'seq seq1) (lay'seq seq2)
        | SEQ'Raise r               => Word "Raise:" @@ lay'r r
        | SEQ'I ins                 => lay'instruction ins (*@@ Right ";"*)
        | SEQ'If (im,seq1,seq2)     => layIfElse im (lay'seq seq1) (lay'seq seq2)
*)

       (*of CODE'Seq (seq,code)           => lay'seq seq // lay'code code*)


(*----------------------------------------------------------------------
INDEX: Use of primitive MACHINE seq
----------------------------------------------------------------------*)
(*
  val op $ =
      fn (seq,SEQ'Empty) => seq
       | (SEQ'Empty,seq) => seq
       | p => SEQ'Seq p

  fun op $$ (seq,code) =
      case seq
       of SEQ'Empty => code
        | _ => CODE'Seq (seq,code)

  val seq'Empty = SEQ'Empty
  val seq'I = SEQ'I
  val seq'If = SEQ'If
  val seq'Raise = SEQ'Raise
  val seq'FatBar = SEQ'FatBar
*)

  fun seq'I INS'Fail                = CC (fn _ => CODE'Fail)
    | seq'I (INS'Goto lab)          = CC (fn _ => CODE'Goto lab)
(*
    | seq'I instruction             = CC (fn code => CODE'Seq (SEQ'I instruction, code))
*)
    | seq'I instruction             = CC (fn code => CODE'Seq (instruction, code))


(*
    | INS'Fail
    | INS'Goto              of label
*)

(*
(* match existing object, if match: continue to next instruction, if not: FAIL to next FatBar *)
    | INS'Fail
    | INS'Goto              of label
*)

(*
       | INS'Fail                                   => inc FX
       | INS'Goto lab                               => inc GO
*)
(*
        | INS'Fail                                      => Word "<I-FAIL>"
        | INS'Goto lab                                  => Word "I-goto" @@ lay'label lab
*)
(*
           | INS'Fail                                   => fail()
           | INS'Goto lab                               => goto lab)
*)


  val seq'MatchScon = seq'Match o IM'MatchScon
  val seq'MatchC = seq'Match o IM'MatchC
  val seq'MatchE = seq'Match o IM'MatchE
  val seq'MatchG = seq'Match o IM'MatchG


(*----------------------------------------------------------------------
INDEX: Thu Sep  7 09:41:07 2006 - pre optimized goto
----------------------------------------------------------------------*)
                                      
(*
  fun seq'If (im, CC cc1, CC cc2)   = CC(*_nodup*) (fn code => CODE'If (im, cc1(code), cc2(code)))
  (*fun seq'Match im fail = seq'If (im,seq'Empty,seq'I fail)*)
  fun seq'Match im (fail:code) = seq'If (im,seq'Empty,fail)
*)

(*
  val code'Raise = CODE'Raise
  val code'TailCall = CODE'TailCall
  val code'Return = CODE'Return
  val code'Seq = CODE'Seq
  val code'If = CODE'If
  val code'Goto = CODE'Goto
  val code'Where = CODE'Where
*)

(*
  abstype g_code = G of code
  with val mkG = G
       fun deG (G x) = x
  end

  fun code'Raise x              = mkG (CODE'Raise x)
  fun code'TailCall x           = mkG (CODE'TailCall x)
  fun code'Return x             = mkG (CODE'Return x)
  fun code'Seq (instruction,g)  = mkG (CODE'Seq (instruction, deG g))
  fun code'If (im,g1,g2)        = mkG (CODE'If (im, deG g1, deG g2))
  fun code'Goto lab             = mkG (CODE'Goto lab)
  fun code'Where (g1,lab,g2)    = mkG (CODE'Where (deG g1, lab, deG g2))

  (*val canDup'code = (fn g => canDup'code (deG g))*)

  val closeCode : (g_code -> code) = deG
*)

if (canDup'code code2)
                            then f_code1 {fail=code2}
                            else 

(*if (canDup'code next) then cc(next)
                                             else*) 

(*----------------------------------------------------------------------
INDEX: opt_goto ref
----------------------------------------------------------------------*)
                             val opt_goto = opt_goto
(*opt_goto := false;*)
val opt_goto = ref true;
         val opt_goto : bool ref
  fun code'Where (g1,lab,g2)    = 
      case (deG g1,deG g2) 
       of ((xs1,F1),(xs2,F2)) =>
          case (restrict'labs xs1 lab) 
           of (xs1,N) =>
              mkG (xs1@xs2,
                   case (!opt_goto,N) of
                       (_,0)    => (echo "***code'Where:N=0"; 
                                    fn m => CODE'Where (F1 (extend'labmap m (lab,CODE'Goto lab)), lab, 
                                                        F2 m)) (* show the unreached code *)
                     | (true,1) => (fn m => F1 (extend'labmap m (lab, F2 m)))
                     | _        => (fn m => let val c2 = F2 m
                                            in if canDup'code c2
                                               then F1 (extend'labmap m (lab, c2))
                                               else CODE'Where (F1 (extend'labmap m (lab,CODE'Goto lab)), lab, c2)
                                            end))

(*----------------------------------------------------------------------
INDEX: Thu Sep  7 09:52:10 2006 - fatbar
----------------------------------------------------------------------*)
         (*val old_fatbar : bool ref*)


  (*fun seq'Where (seq1,lab,seq2) = makeSeq_nodup (fn next => code'Where (deSeq seq1(next), lab, deSeq seq2(next)))*)

  (*fun seq'Raise q               = makeSeq_0 (fn _=> code'Raise q) (* throw away continuation code *)*)
  (*fun seq'FatBar (seq1,seq2) = makeSeq_nodup (fn next => code'FatBar (deSeq seq1(next), deSeq seq2(next)))*)
(*
  val f_seq'FatBarUsingGoto : (f_seq * seq -> seq) =
      fn (f_seq1,seq2) => (*if (canDup'seq seq2)
                          then f_seq1 {fail=seq2}
                          else*) let val lab = newLabel()
                               in seq'Where (f_seq1 {fail=code'Goto lab},lab,seq2)
                               end
*)

(*
  fun exhaustive'pat pat =
      case pat
       of PAT'Wild              => true
        | PAT'V x               => true
        | PAT'Layered (x,pat)   => exhaustive'pat pat
        | PAT'Ref pat           => (*true - bug, never witnessed!*) exhaustive'pat pat
        | PAT'Tuple pats        => forall exhaustive'pat pats
        | _                     => false

  fun simple_exhaustive'amatch amatch =
      case amatch
       of AMATCH'Rule (pat,_) => exhaustive'pat pat
        | _ => false

  fun simple_exhaustive'cmatch cmatch =
      case cmatch
       of CMATCH'Rule (pat,_) => exhaustive'pat pat
        | _ => false
*)

(*
  val seq'RaiseMatch = seq'Raise (LOC'Value value'Match)
  val seq'RaiseBind = seq'Raise (LOC'Value value'Bind)
*)

                                            (*if (!old_fatbar)
                                            then let val (env,seqPat) = Pdx ({fail=code'Fail},env,pat,rd)
                                                     val seqPat = if (exhaustive'pat pat) then seqPat 
                                                                  else seq'FatBar (seqPat, seq'RaiseBind)
                                                 in (env,seqPat)
                                                 end
                                            else*) 

                     (*if (!old_fatbar)
                     then let val (envNew,seqPat) = Pdx ({fail=code'Fail},envNew,pat,DELAYED'R LOC'Cret)
                              val seqPat = if (exhaustive'pat pat) then seqPat else seq'FatBar (seqPat, seq'RaiseBind)
                          in (envNew,seqPat)
                          end
                     else*) 

                     (*if (!old_fatbar)
                     then let val codeHandle = CM ({fail=code'Fail},envNew,cmatch,DELAYED'R (LOC'Xret))
                              val codeHandle = code'FatBar (codeHandle, code'Raise LOC'Xret)
                          in codeHandle
                          end
                     else*) 

                                                       (*if (!old_fatbar)
                                                       then let val code2 = CM ({fail=code'Fail},env,cmatch,rd)
                                                                val code2 = if (simple_exhaustive'cmatch cmatch) then code2
                                                                            else code'FatBar (code2, code'RaiseMatch)
                                                            in code2
                                                            end
                                                       else*) 

                                                   (*if (!old_fatbar)
                                                   then let val code2 = AM ({fail=code'Fail},env,amatch,rd)
                                                            val code2 = if (simple_exhaustive'amatch amatch) then code2
                                                                        else code'FatBar (code2, code'RaiseMatch)
                                                        in code2
                                                        end
                                                   else*) 
(*if (!old_fatbar)
                                       then let val code1 = CM (fail,env,cm1,rd)
                                                val code2 = CM (fail,env,cm2,rd)
                                            in code'FatBar(code1,code2)
                                            end
                                       (*else let val lab = newLabel()
                                                val code1 = CM ({fail=code'Goto lab},env,cm1,rd)
                                                val code2 = CM (fail,env,cm2,rd)
                                            in code'Where(code1,lab,code2)
                                            end *)
                                       else*)
(*if (!old_fatbar)
                                       then let val code1 = AM (fail,env,am1,rd)
                                                val code2 = AM (fail,env,am2,rd)
                                            in code'FatBar(code1,code2)
                                            end
                                       (*else let val lab = newLabel()
                                                val code1 = AM ({fail=code'Goto lab},env,am1,rd)
                                                val code2 = AM (fail,env,am2,rd)
                                            in code'Where(code1,lab,code2)
                                            end*)
                                       else*) 

                                                   (*if (!old_fatbar)
                                                   then let val seq2 = AMt ({fail=code'Fail},env,amatch,rd,q)
                                                            val seq2 = if (simple_exhaustive'amatch amatch) then seq2
                                                                       else seq'FatBar (seq2, seq'RaiseMatch)
                                                        in seq2
                                                        end
                                                   else*) 
(*if (!old_fatbar)
                                             then let val seq1 = AMt (fail,env,amatch1,rd,q)
                                                      val seq2 = AMt (fail,env,amatch2,rd,q)
                                                  in seq'FatBar(seq1,seq2)
                                                  end
                                             (*else let val lab = newLabel()
                                                      val seq1 = AMt ({fail=code'Goto lab},env,amatch1,rd,q)
                                                      val seq2 = AMt (fail,env,amatch2,rd,q)
                                                  in seq'Where(seq1,lab,seq2)
                                                  end*)
                                             else*) 
(*val old_fatbar = ref false;*)
                             (*val old_fatbar = old_fatbar*)

(*----------------------------------------------------------------------
INDEX: delayed_var_bind - this is from ages ago
----------------------------------------------------------------------*)

(*val delay_var_bind = ref true;*)
                             (*val delay_var_bind = delay_var_bind*)

(*----------------------------------------------------------------------
INDEX: allow_dup_crr
----------------------------------------------------------------------*)
         val allow_dup_crr : bool ref

  val canDup'code =
      fn CODE'Goto _        => true 
       | CODE'TailCall _    => !allow_dup_crr
       | CODE'Raise _       => !allow_dup_crr
       | CODE'Return _      => !allow_dup_crr
       | _                  => false
val allow_dup_crr = ref true;
                             val allow_dup_crr = allow_dup_crr

(*----------------------------------------------------------------------
INDEX: MACHINE - fatbar
----------------------------------------------------------------------*)


(*
    | CODE'FatBar           of code * code
    | CODE'Fail
*)
(*
    | CODE'FatBar           of code * code
    | CODE'Fail
*)
                        
FB: int ref, FX: int ref, 
FB = ref 0, FX = ref 0,
                              lay'count "FatBar" FB,
                                                      lay'count "Fail" FX,
  fun lay'icounts (Icounts {NT,NX,NF,NR,NC,CP,BI,DC,DE,DR,DT,PC,PCX,PH,ST,SF,SC,SX,MS,MC,ME,MG,IF,FB,FX,CW,GO,RET,RAISE,SKIP,TAIL,PAP,REAPP}) =

(*
        | CODE'FatBar (code1,code2)     => inc FB
        | CODE'Fail                     => inc FX
*)

  fun layBraceFatBar lay = (Left "{" //`` lay) // Right "}"

  fun layFatBar s lay1 lay2 = layBraceFatBar lay1 // Word s // layBraceFatBar lay2

(*
        | CODE'FatBar (code1,code2)     => layFatBar "<FAT-BAR>" (lay'code code1) (lay'code code2)
        | CODE'Fail                     => Word "<FAIL>"
*)

  val executeCode : (icounts * jump * control * env * code * {fail:unit->res} -> res) =
      fn (icounts,J,control,env,code,{fail}) =>
         let
             fun doCode (code,{fail},goto) = (count'code icounts code;
                 case code
                  of CODE'Seq (ins,code)        => (executeInstruction (icounts,Interpret(icounts,J),control,env,ins);
                                                    doCode (code,{fail=fail},goto))
                   | CODE'Return r              => Return (icounts,J,control,GetLoc(env,r))
                   | CODE'TailCall (func,args)  => TailCall (icounts,J,control,GetLoc(env,func),GetLocs(env,args))
                   | CODE'Raise r               => Raise (icounts,J,control,GetLoc(env,r))
                   | CODE'If (im,code1,code2)   => if (executeIm (icounts,env,im))
                                                   then doCode (code1,{fail=fail},goto)
                                                   else doCode (code2,{fail=fail},goto)
(*
                   | CODE'FatBar (code1,code2)  => doCode (code1,{fail = fn() => doCode (code2, {fail=fail},goto)},goto)
                   | CODE'Fail                  => fail()
*)
                   | CODE'Where (code1,L,code2) => let fun goto'(lab) =
                                                           if (lab=L) then doCode (code2,{fail=fail},goto)
                                                           else goto(lab)
                                                   in doCode (code1,{fail=fail},goto')
                                                   end
                   | CODE'Goto lab              => goto lab)
             fun goto(lab) = abort ("goto/"^sof'label lab)
         in
             doCode (code,{fail=fail},goto)
         end
                                                        
  fun echo'lay lay x =
      (prefixed_output_layout prefixNML (squash_to wrapCol (lay x));
       echo "")

  fun eval'code code =
      let val fail = (fn () => abort "evalCode:fail")
          val icounts = new'icounts()
          fun Jump (control,code,env) = executeCode (icounts,Jump,control,env,code,{fail=fail})
          val control = newControl()
          val env = NewEnv (NewFrame 0,{args=[]})
          val res = Jump (control,code,env)
          val () = if !display_counts then (echo'lay lay'icounts icounts) else ()
      in res
      end

(*
                   | CODE'FatBar (code1,code2)  => doCode (code1,{fail = fn() => doCode (code2, {fail=fail},goto)},goto)
                   | CODE'Fail                  => fail()
*)
* {fail:unit->res}
,{fail}
,{fail=fail}
 val fail = (fn () => abort "evalCode:fail")

(*----------------------------------------------------------------------
INDEX: m_seq (mc) - defined in terms of b - pre defined in terms of m_code
----------------------------------------------------------------------*)

  abstype m_seq = T of ({fail: obi -> code}
                        * {next: code} 
                        * obi) -> code
  with val mkMS = T
       fun deMS (T x) = x
  end

  val ms'mrule : (b_code * (env -> seq) -> m_seq) =
      fn (b,F) =>
         mkMS (fn (fail,{next},obi) => deB b (fail,
                                              {succ = fn (_,env) => F env $$ next},
                                              obi, 
                                              env'empty))

  val ms'fatbar : (m_seq * m_seq -> m_seq) =
      fn (ms1,ms2) =>
         mkMS (fn (fail,next,obi) => deMS ms1 ({fail = fn obi => deMS ms2 (fail,next,obi)},
                                               next,obi))

  val closeMS : (m_seq * {fail: code} * r -> seq) =
      fn (ms,{fail},r) => 
         mkSeq (fn next => deMS ms ({fail = fn _ => fail}, {next=next}, mkObi r))


(*----------------------------------------------------------------------
INDEX: pat - basic pattern compilation
----------------------------------------------------------------------*)

local
  type P        = (env * pat * r                    -> seq * env)       (* env has just new bindings *)
  type Pd       = (env * pat * r_delayed            -> seq * env)       (* env has just new bindings *)
in
  fun U_compile'pat {fail:code} {P:P,Pd:Pd} : (env * pat * r -> seq * env) =
      fn (env,pat,r) =>
         case pat
          of PAT'Wild               => (seq'Empty, env'empty)
           | PAT'V x                => (seq'Empty, env'single (x,r))
           | PAT'Layered (x,pat)    => let val (seq,env) = P (env,pat,r)
                                       in (seq, env'single (x,r) ++ env)
                                       end
           | PAT'Ref pat            => let val q = newQ (INFO'Pat pat)
                                           val (seq,env) = P (env,pat,LOC'Stack q)
                                       in (seq'I (INS'DeRef (q,r)) $ seq, env)
                                       end
           | PAT'C (x,None)         => (seq'Match (IM'MatchC (r,x)) fail, env'empty)
           | PAT'C (x,Some pat)     => let val q = newQ (INFO'Pat pat)
                                           val (seq,env) = P (env,pat,LOC'Stack q)
                                       in (seq'Match (IM'MatchC (r,x)) fail $ seq'I (INS'DeCon (q,r)) $ seq, env)
                                       end
           | PAT'E (exname,None)    => (seq'Match (IM'MatchE (r,exname)) fail, env'empty)
           | PAT'E (exname,Some pat)=> let val q = newQ (INFO'Pat pat)
                                           val (seq,env) = P (env,pat,LOC'Stack q)
                                       in (seq'Match (IM'MatchE (r,exname)) fail $ seq'I (INS'DeExcon (q,r)) $ seq, env)
                                       end
           | PAT'G (x,None)         => let val r2 = look'env "compile'pat" env x
                                       in (seq'Match (IM'MatchG (r,r2)) fail, env'empty)
                                       end
           | PAT'G (x,Some pat)     => let val r2 = look'env "compile'pat" env x
                                           val q = newQ (INFO'Pat pat)
                                           val (seq,env) = P (env,pat,LOC'Stack q)
                                       in (seq'Match (IM'MatchG (r,r2)) fail $ seq'I (INS'DeExcon (q,r)) $ seq, env)
                                       end
           | PAT'Scon scon          => (seq'Match (IM'MatchScon (r,scon)) fail, env'empty)
           | PAT'Tuple pats => 
             fold (fn (seqAcc,envAcc) => 
                      fn (pat,n) => 
                         let val (seq,env) = if (!delay_select andalso (case pat of PAT'Wild => true | _ => false))
                                             then (*let val rd = DELAYED'DeTuple (DELAYED'R r,n,{info=pat})
                                                  in Pd (env,pat,rd)
                                                  end*) (seq'Empty, env'empty)
                                             else let val q = newQ (INFO'Pat pat)
                                                      val seq1 = seq'I (INS'DeTuple (q,r,n))
                                                      val (seq2,env) = P (env,pat,LOC'Stack q)
                                                  in (seq1 $ seq2, env)
                                                  end
                         in (seqAcc $ seq, envAcc ++ env)
                         end) (seq'Empty,env'empty) (enumerateFrom 0 pats)
  fun compile'pat fail {Pd:Pd} x = U_compile'pat fail {P=compile'pat fail {Pd=Pd},Pd=Pd} x
end


  fun compile_Pd fail : (env * pat * r_delayed -> seq * env) = (* returned env just has new bindings *)
      fn (env,pat,rd) =>
         case (pat,rd)
          of (PAT'Tuple pats,DELAYED'Tuple (rds,_)) =>  
             let val () = if (length pats = length rds) then () else abort "Pd/tuple/size"
             in case (unzip (map (fn (pat,rd) => compile_Pd fail (env,pat,rd)) (zipPair pats rds))) of
                    (seqs,envs) => (seq'concat seqs, env'plusList envs)
             end
           | _ => 
             case pat
              of PAT'Wild => (seq'Empty, env'empty)
               | _ => let val (seq1,r) = forceDelayed rd
                          val (seq2,env) = compile'pat fail {Pd=compile_Pd fail} (env,pat,r)
                      in (seq1 $ seq2,env)
                      end


(*----------------------------------------------------------------------
INDEX: Fri Sep  8 14:22:43 2006
----------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: b_code (b) -- succ continuation prevents env reify 
----------------------------------------------------------------------*)

  abstype b_code = B of ({fail: obi -> code}
                         * {succ: obi * env -> code} 
                         * obi * env) -> code
  with val mkB = B
       fun deB (B x) = x
  end

  val b'Empty : b_code =
      mkB (fn ({fail=_},{succ},obi,env) => succ (obi,env))

  infix &&

  val op && : (b_code * b_code -> b_code) =
      fn (b1,b2) =>
         mkB (fn (fail,succ,obi,env) =>
                 deB b1 (fail,
                         {succ = fn (obi,env) => deB b2 (fail,succ,obi,env)},
                         obi,env))

  val b'Bind : (uid -> b_code) =
      fn x =>
         mkB (fn ({fail=_},{succ},obi,env) =>
                 let val (seq,r) = r'obi obi
                 in seq $$ succ (obi, env'single (x,r) ++ env)
                 end)
      

  datatype sma = SMA'Yes | SMA'No | SMA'Maybe of seq * im

  val b'Match : {StaticMatch: obi -> sma, 
                 Xpos: obi -> obi,
                 Xneg: obi -> obi} -> b_code =
      fn {StaticMatch,Xpos,Xneg} =>
         mkB (fn ({fail},{succ},obi,env) =>
                 case (StaticMatch obi)
                  of SMA'Yes => succ (obi,env)
                   | SMA'No => fail (Xneg obi)
                   | SMA'Maybe (seq,im) => seq $$ code'If (im,
                                                           succ (Xpos obi, env),
                                                           fail (Xneg obi)))


  val b'Destruct : {Sub: obi -> seq * obi * {Replace: obi -> obi}} -> b_code -> b_code =
      fn {Sub} =>
         fn bsub =>
            mkB (fn ({fail},{succ},obi,env) =>
                    let val (seq,obi,{Replace}) = Sub(obi)
                    in seq $$ deB bsub ({fail = fn obi => fail (Replace obi)},
                                        {succ = fn (obi,env) => succ (Replace obi, env)},
                                        obi,env)
                    end)


  val mc'rule : (b_code * (env -> code) -> m_code) =
      fn (b,F) =>
         mkMC (fn (fail,obi) => deB b (fail,
                                       {succ = fn (_,env) => F env},
                                       obi, 
                                       env'empty))



(*----------------------------------------------------------------------
INDEX: f_code / f_seq
----------------------------------------------------------------------*)

(*
(*----------------------------------------------------------------------
INDEX: f_code
----------------------------------------------------------------------*)

  type f_code = {fail:code} -> code

(*
  val f_code'FatBarUsingGoto : (f_code * code -> code) =
      fn (f_code1,code2) => let val lab = newLabel()
                            in code'Where (f_code1 {fail=code'Goto lab},lab,code2)
                            end
*)

(*
  val f_code'FatBarUsingGoto : (f_code * code -> code) =
      fn (f_code1,code2) => 
         code'Share code2 (fn code2 => f_code1 {fail=code2})
*)

  val f_code'FatBarUsingGoto : (f_code * code -> code) =
      fn (f_code1,code2) => 
         SharingCode (fn code2 => f_code1 {fail=code2}) code2
*)

(*
  fun mkSeq_nodup cc = mkSeq (fn next => let val lab = newLabel()
                                                 val next' = code'Goto lab
                                             in code'Where (cc(next'),lab,next)
                                             end)
*)

  (*fun mkSeq_nodup cc = mkSeq (fn next => code'Share next (fn next => cc next))*)
  (*fun mkSeq_nodup cc = mkSeq (fn next => code'Share next cc)*)

(*----------------------------------------------------------------------
INDEX: f_seq
----------------------------------------------------------------------*)
(*                      
  type f_seq = {fail:code} -> seq

(*
  val f_seq'FatBarUsingGoto : (f_seq * seq -> seq) =
      fn (f_seq1,seq2) => 
         mkSeq_nodup (fn next =>
                           f_code'FatBarUsingGoto (fn fail => deSeq (f_seq1 fail) next,
                                                   deSeq seq2 next))
*)
(*
  val f_seq'FatBarUsingGoto : (f_seq * seq -> seq) =
      fn (f_seq1,seq2) => 
         mkSeq (fn next => 
                     code'Share next
                                (fn next =>
                                    f_code'FatBarUsingGoto (fn fail => deSeq (f_seq1 fail) next,
                                                            deSeq seq2 next)))
*)
  val f_seq'FatBarUsingGoto : (f_seq * seq -> seq) =
      fn (f_seq1,seq2) => 
         mkSeq (SharingCode 
                (fn next =>
                    f_code'FatBarUsingGoto (fn fail => deSeq (f_seq1 fail) next,
                                            deSeq seq2 next)))
*)

(*----------------------------------------------------------------------
INDEX: Delayed'DeTuple...
----------------------------------------------------------------------*)
    (*| DELAYED'DeTuple of r_delayed * int * {info:pat}*)
(*
        | DELAYED'DeTuple (rd,n,
                           {info=pat}) => let val q = newQ (INFO'Pat pat)
                                              val (seq,r) = forceDelayed rd
                                          in (seq $ seq'I (INS'DeTuple (q,r,n)), LOC'Stack q)
                                          end
*)
(*let val rd = DELAYED'DeTuple (DELAYED'R r,n,{info=pat})
                                                  in Pd (env,pat,rd)
                                                  end*) 

(*----------------------------------------------------------------------
INDEX: old cmatch/amtch compilation schemed
----------------------------------------------------------------------*)

(*
  type CM       = ({fail:code} * env * cmatch * r_delayed       -> code)
  type AM       = ({fail:code} * env * amatch * r_delayed       -> code)
  type AMt      = ({fail:code} * env * amatch * r_delayed * q   -> seq)
*)
  type schemes = {Pdx:Pdx,B:B,C:C,Aret:Aret,Ad:Ad,A:A,At:At,Ft:Ft(*,CM:CM,AM:AM,AMt:AMt*)}

(*if not (!new_pat_comp)
                                  then CM ({fail=code'Raise LOC'Xret},envNew,cmatch,DELAYED'R (LOC'Xret))
                                  else*)    
                                               (*val code2 = CM ({fail=code'RaiseMatch},env,cmatch,rd)*)
(*if not (!new_pat_comp)
                                                           then AM ({fail=code'RaiseMatch},env,amatch,rd)
                                                           else*) 

(*
  fun compile'CM ({Pdx,B,C,Aret,Ad,A,At,Ft,CM,AM,AMt}:schemes) : ({fail:code} * env * cmatch * r_delayed -> code) =
      fn (fail,env,cmatch,rd) =>
         case cmatch 
          of CMATCH'Alt (cm1,cm2)   =>  let val f_code1 = fn fail => CM (fail,env,cm1,rd)
                                            val code2 = CM (fail,env,cm2,rd)
                                        in f_code'FatBarUsingGoto(f_code1,code2)
                                        end
           | CMATCH'Rule (pat,cps)  => let val (env,seq1) = Pdx (fail,env,pat,rd)
                                           val code2 = C (env,cps)
                                       in seq1 $$ code2
                                       end
           | CMATCH'Atom am         => AM (fail,env,am,rd)

  fun compile'AM ({Pdx,B,C,Aret,Ad,A,At,Ft,CM,AM,AMt}:schemes) : ({fail:code} * env * amatch * r_delayed  -> code) =
      fn (fail,env,amatch,rd) =>
         case amatch 
          of AMATCH'Alt (am1,am2)   => let val f_code1 = fn fail => AM (fail,env,am1,rd)
                                           val code2 = AM (fail,env,am2,rd)
                                       in f_code'FatBarUsingGoto(f_code1,code2)
                                       end
           | AMATCH'Rule (pat,atom) => let val (env,seq1) = Pdx (fail,env,pat,rd)
                                           val code2 = Aret (env,atom)
                                       in seq1 $$ code2
                                       end
*)

(*if not (!new_pat_comp)
                                                          then AMt ({fail=code'RaiseMatch},env,amatch,rd,q)
                                                          else*) 
(*
  fun compile'AMt ({Pdx,B,C,Aret,Ad,A,At,Ft,CM,AM,AMt}:schemes) : ({fail:code} * env * amatch * r_delayed * q -> seq) =
      fn (fail,env,amatch,rd,q) =>
         case amatch 
          of AMATCH'Alt (amatch1,amatch2) => let val f_seq1 = fn fail => AMt (fail,env,amatch1,rd,q)
                                                 val seq2 = AMt (fail,env,amatch2,rd,q)
                                             in f_seq'FatBarUsingGoto(f_seq1,seq2)
                                             end
           | AMATCH'Rule (pat,atom) => let val (env,seq1) = Pdx (fail,env,pat,rd)
                                           val seq2 = At (env,atom,q)
                                       in seq1 $ seq2
                                       end
*)
(*
      and CM    x = compile'CM      (makeSchemes()) x
      and AM    x = compile'AM      (makeSchemes()) x
      and AMt   x = compile'AMt     (makeSchemes()) x
*)

(*----------------------------------------------------------------------
INDEX: finally remove old pat compilation code...
----------------------------------------------------------------------*)


(*----------------------------------------------------------------------
INDEX: OLD  - basic pattern compilation
----------------------------------------------------------------------*)

  fun OLD_U_compile'pat {fail:code} {P} : (env * pat * r -> seq * env) =
      fn (read_env,pat,r) =>
         case pat
          of PAT'Wild               => (seq'Empty, env'empty)
           | PAT'V x                => (seq'Empty, env'single (x,r))
           | PAT'Layered (x,pat)    => let val (seq,env) = P (read_env,pat,r)
                                       in (seq, env'single (x,r) ++ env)
                                       end
           | PAT'Ref pat            => let val q = newQ (INFO'Pat pat)
                                           val (seq,env) = P (read_env,pat,LOC'Stack q)
                                       in (seq'I (INS'DeRef (q,r)) $ seq, env)
                                       end
           | PAT'C (x,None)         => (seq'Match (IM'MatchC (r,x)) fail, env'empty)
           | PAT'C (x,Some pat)     => let val q = newQ (INFO'Pat pat)
                                           val (seq,env) = P (read_env,pat,LOC'Stack q)
                                       in (seq'Match (IM'MatchC (r,x)) fail $ seq'I (INS'DeCon (q,r)) $ seq, env)
                                       end
           | PAT'E (exname,None)    => (seq'Match (IM'MatchE (r,exname)) fail, env'empty)
           | PAT'E (exname,Some pat)=> let val q = newQ (INFO'Pat pat)
                                           val (seq,env) = P (read_env,pat,LOC'Stack q)
                                       in (seq'Match (IM'MatchE (r,exname)) fail $ seq'I (INS'DeExcon (q,r)) $ seq, env)
                                       end
           | PAT'G (x,None)         => let val r2 = look'env "compile'pat" read_env x
                                       in (seq'Match (IM'MatchG (r,r2)) fail, env'empty)
                                       end
           | PAT'G (x,Some pat)     => let val r2 = look'env "compile'pat" read_env x
                                           val q = newQ (INFO'Pat pat)
                                           val (seq,env) = P (read_env,pat,LOC'Stack q)
                                       in (seq'Match (IM'MatchG (r,r2)) fail $ seq'I (INS'DeExcon (q,r)) $ seq, env)
                                       end
           | PAT'Scon scon          => (seq'Match (IM'MatchScon (r,scon)) fail, env'empty)
           | PAT'Tuple pats => 
             fold (fn (seqAcc,envAcc) => 
                      fn (pat,n) => 
                         let val (seq,env) = if (!delay_select andalso (case pat of PAT'Wild => true | _ => false))
                                             then (seq'Empty, env'empty)
                                             else let val q = newQ (INFO'Pat pat)
                                                      val seq1 = seq'I (INS'DeTuple (q,r,n))
                                                      val (seq2,env) = P (read_env,pat,LOC'Stack q)
                                                  in (seq1 $ seq2, env)
                                                  end
                         in (seqAcc $ seq, envAcc ++ env)
                         end) (seq'Empty,env'empty) (enumerateFrom 0 pats)

  fun OLD_compile'pat fail x = OLD_U_compile'pat fail {P=OLD_compile'pat fail} x

(*----------------------------------------------------------------------
INDEX: Pd - avoid tuple building when matching againt tuple pattern
----------------------------------------------------------------------*)

  fun OLD_compile_Pd fail : (env * pat * r_delayed -> seq * env) = (* returned env just has new bindings *)
      fn (read_env,pat,rd) =>
         case (pat,rd)
          of (PAT'Tuple pats,DELAYED'Tuple (rds,_)) =>  
             let val () = if (length pats = length rds) then () else abort "Pd/tuple/size"
             in case (unzip (map (fn (pat,rd) => OLD_compile_Pd fail (read_env,pat,rd)) (zipPair pats rds))) of
                    (seqs,envs) => (seq'concat seqs, env'plusList envs)
             end
           | _ => 
             case pat
              of PAT'Wild => (seq'Empty, env'empty)
               | _ => let val (seq1,r) = forceDelayed rd
                          val (seq2,env) = OLD_compile'pat fail (read_env,pat,r)
                      in (seq1 $ seq2,env)
                      end

  val WRAPPED_OLD_compile'Pd : (env * pat -> b_code) =
      fn (read_env,pat) =>
         mkB (fn ({fail},obi,_) =>
                 let val (seq,env1) = OLD_compile_Pd {fail = fail obi} (read_env,pat, rd'obi obi)
                 in (obi,env1,seq)
                 end)

(*----------------------------------------------------------------------
INDEX: compile pat, extending env
----------------------------------------------------------------------*)


  val OLD_compile'Pdx : ({fail:code} * env * pat * r_delayed -> env * seq) = (* extends env *)
      fn (fail,env,pat,rd) =>
         let val (seq,env1) = OLD_compile_Pd fail (env,pat,rd)
         in (env ++ env1, seq)
         end


(*----------------------------------------------------------------------
INDEX: compile'Pd (old/new)
----------------------------------------------------------------------*)

  val compile'Pd : (env * pat -> b_code) =
      fn (env,pat) =>
         (*if not (!new_pat_comp)
         then WRAPPED_OLD_compile'Pd (env,pat)
         else*) NEW_compile'Pd (env,pat)


(*----------------------------------------------------------------------
INDEX: obi (object info)  -- version 1, just an r_delayed
----------------------------------------------------------------------*)

(*
  datatype obi = OBI of r_delayed

  fun mkObi rd = OBI rd

  fun r'obi (OBI rd) = let val (seq,r) = forceDelayed rd
                       in (seq, OBI (DELAYED'R r), r)
                       end
*)



  (*fun b'DeTuple n = b'DestructSimple (fn (q,r) => INS'DeTuple (q,r,n))*)

(*
  fun b'DeTuple n pat =
      b'Destruct 
      {Sub = fn obi as OBI rd =>
                case rd
                 of DELAYED'Tuple (rds,info) => (seq'Empty, 
                                                 mkObi (nth n rds), 
                                                 {Replace = fn OBI sub_rd => mkObi (DELAYED'Tuple (replace_nth n sub_rd rds,info))}
                                                 )
                  | DELAYED'R r => let val q = newQ (INFO'Pat pat)
                                   in (seq'I (INS'DeTuple (q,r,n)), mkObi (DELAYED'R (LOC'Stack q)),
                                       {Replace = fn _ => obi})
                                   end}
*)


  fun b'MatchSimple M =
      b'Match {StaticMatch = fn obi => let val (seq,obi,r) = r'obi obi
                                       in SMA'Maybe (seq, M r, obi)
                                       end,
               Xpos = Identity,
               Xneg = Identity}
                
  fun b'MatchC vid      = b'MatchSimple (fn r => IM'MatchC (r,vid))
  fun b'MatchE exname   = b'MatchSimple (fn r => IM'MatchE (r,exname))
  fun b'MatchG r2       = b'MatchSimple (fn r => IM'MatchG (r,r2))
  fun b'MatchScon scon  = b'MatchSimple (fn r => IM'MatchScon (r,scon))

(*----------------------------------------------------------------------
INDEX: Tue Sep 12 22:08:05 2006
----------------------------------------------------------------------*)


(*----------------------------------------------------------------------
INDEX: b_code (b)  - not optimized for unconditional fail
----------------------------------------------------------------------*)

  abstype b_code = B of {fail: obi -> code} * obi * env -> (obi * env * seq)
  with val mkB = B
       fun deB (B x) = x
  end

  val b'Empty : b_code =
      mkB (fn ({fail=_},obi,env) => (obi,env,seq'Empty))

  infix &&

  val op && : (b_code * b_code -> b_code) =
      fn (b1,b2) =>
         mkB (fn (fail,obi,env) => let val (obi,env,seq1) = deB b1 (fail,obi,env)
                                       val (obi,env,seq2) = deB b2 (fail,obi,env)
                                   in (obi, env, seq1 $ seq2)
                                   end)

  val b'Bind : (uid -> b_code) =
      fn x =>
         mkB (fn ({fail=_},obi,env) =>
                 let val (seq,obi,r) = r'obi obi
                 (*in (obi,env'single (x,r) ++ env, seq)*)
                 in (obi,env ++ env'single (x,r), seq)
                 end)
      

  val b'Match : {StaticMatch: obi -> sma, 
                 Xpos: obi -> obi,
                 Xneg: obi -> obi} -> b_code =
      fn {StaticMatch,Xpos,Xneg} =>
         mkB (fn ({fail},obi,env) =>
                 case (StaticMatch obi)
                  of SMA'Yes => (obi, env, seq'Empty)
                   | SMA'No => (obi, env, mkSeq (fn _ => fail ((*Xneg*) obi))) (* next ignored - should optimize below *)
                   | SMA'Maybe (seq,im,obi) => (Xpos obi, env,
                                                mkSeq (fn next => seq $$ code'If (im,
                                                                                  next,
                                                                                  fail (Xneg obi)))))


  val b'Destruct : {Sub: obi -> seq * obi * {Replace: obi -> obi}} -> b_code -> b_code =
      fn {Sub} =>
         fn bsub =>
            mkB (fn ({fail},obi,env) =>
                    let val (seq,obi,{Replace}) = Sub(obi)
                        val (obi,env,seq1) = deB bsub ({fail = fn obi => fail (Replace obi)}, obi,env)
                    in (Replace obi, env, seq $ seq1)
                    end)


  val closeB : (b_code * {fail: obi -> code} * obi -> (seq * env)) =
      fn (b,fail,obi) => 
         let val (_:obi,env,seq) = deB b (fail,obi,env'empty)
         in (seq,env)
         end

(*
  val closeB'rd : (b_code * {fail: code} * r_delayed -> (seq * env)) =
      fn (b,{fail},rd) => 
         let val (_:obi,env,seq) = deB b ({fail = fn _ => fail}, mkObi rd, env'empty)
         in (seq,env)
         end
*)


(*----------------------------------------------------------------------
INDEX: b_code (b) - optimized for unconditional-fail - never finsihed/tried
----------------------------------------------------------------------*)


  datatype bsf = BSF'S of obi * env * seq | BSF'F of code

  abstype b_code = B of {fail: obi -> code} * obi * env -> bsf
  with val mkB = B
       fun deB (B x) = x
  end

  val b'Empty : b_code =
      mkB (fn ({fail=_},obi,env) => BSF'S (obi,env,seq'Empty))

  infix &&

  val op && : (b_code * b_code -> b_code) =
      fn (b1,b2) =>
         mkB (fn (fail,obi,env) => 
                 case (deB b1 (fail,obi,env))
                  of BSF'F code              => BSF'F code
                   | BSF'S (obi,env,seq1)    => case (deB b2 (fail,obi,env))
                                                    of BSF'F code2             => BSF'F (seq1 $$ code2)
                                                     | BSF'S (obi,env,seq2)    => BSF'S (obi, env, seq1 $ seq2))



(*----------------------------------------------------------------------
INDEX: b_code (b) - back to cont passing!
----------------------------------------------------------------------*)

  abstype b_code = B of {fail: obi -> code} 
                        * {succ: obi * env -> env * seq}
                          * obi * env -> (env * seq)
  with val mkB = B
       fun deB (B x) = x
  end

  val b'Empty : b_code =
      mkB (fn ({fail=_},{succ},obi,env) => succ (obi,env))

  infix &&

  val op && : (b_code * b_code -> b_code) =
      fn (b1,b2) =>
         mkB (fn (fail,succ,obi,env) => 
                 deB b1 (fail,{succ = fn (obi,env) => deB b2 (fail,succ,obi,env)},
                         obi,env))

  val b'Bind : (uid -> b_code) =
      fn x =>
         mkB (fn ({fail=_},{succ},obi,env) => let val (seq1,obi,r) = r'obi obi
                                                  val (env,seq2) = succ (obi, env ++ env'single (x,r))
                                              in (env, seq1 $ seq2)
                                              end)
      

  val b'Match : {StaticMatch: obi -> sma, 
                 Xpos: obi -> obi,
                 Xneg: obi -> obi} -> b_code =
      fn {StaticMatch,Xpos,Xneg} =>
         mkB (fn ({fail},{succ},obi,env) =>
                 case (StaticMatch obi)
                  of SMA'Yes => succ (obi, env)
                   | SMA'No => (env, mkSeq (fn _ => fail obi))
                   | SMA'Maybe (seq1,im,obi) => 
                     let val (env,seq2) = succ (Xpos obi, env)
                         val failCode = fail (Xneg obi)
                     in (env, seq1 $ mkSeq (fn next => code'If (im,
                                                                seq2 $$ next,
                                                                failCode)))
                     end)
         

  val b'Destruct : {Sub: obi -> seq * obi * {Replace: obi -> obi}} -> b_code -> b_code =
      fn {Sub} =>
         fn bsub =>
            mkB (fn ({fail},{succ},obi,env) =>
                    let val (seq1,obi,{Replace}) = Sub(obi)
                        val (env,seq2) = deB bsub ({fail = fn obi => fail (Replace obi)},
                                                   {succ = fn (obi,env) => succ (Replace obi, env)},
                                                   obi,env)
                    in (env, seq1 $ seq2)
                    end)


  val closeB : (b_code * {fail: obi -> code} * obi -> (seq * env)) =
      fn (b,fail,obi) =>
         let val (env,seq) = deB b (fail,
                                    {succ = fn (_,env) => (env,seq'Empty)},
                                    obi, env'empty)
         in (seq,env)
         end

(*----------------------------------------------------------------------
INDEX: Wed Sep 13 00:26:13 2006
----------------------------------------------------------------------*)
  fun obi'MatchC vid : matcher =
      {StaticMatch = 
       fn OBI'C (_,con,_) => if (eq'vid vid con) then SMA'Yes else SMA'No
        | obi as OBI'NotC (r,cons) => if (exists (eq'vid vid) cons) then SMA'No 
                                      else SMA'Maybe (seq'Empty, IM'MatchC (r,vid), obi)
        (*| obi => let val (seq,obi,r) = r'obi obi
                 in SMA'Maybe (seq, IM'MatchC (r,vid), obi)
                 end*)
        | obi as OBI'Location r => SMA'Maybe (seq'Empty, IM'MatchC (r,vid), obi)
        | OBI'Tuple _ => abort "obi'MatchC/StaticMatch/Tuple"
        | OBI'DelTup _ => abort "obi'MatchC/StaticMatch/DelTup"
       ,
       Xpos = 
       fn OBI'Location r    => OBI'C (r,vid,None)
        | OBI'NotC (r,_)    => OBI'C (r,vid,None)
        (*| obi as OBI'C _    => obi*)
        | OBI'C _           => abort "obi'MatchC/Xpos/C"
        | OBI'Tuple _       => abort "obi'MatchC/Xpos/Tuple"
        | OBI'DelTup _      => abort "obi'MatchC/Xpos/DelTup",
       Xneg =
       fn OBI'Location r    => OBI'NotC (r,[vid])
        | OBI'NotC (r,xs)   => OBI'NotC (r,vid::xs)
        | OBI'C _           => abort "obi'MatchC/Xneg/C"
        | OBI'Tuple _       => abort "obi'MatchC/XnegTuple"
        | OBI'DelTup _      => abort "obi'MatchC/XnegDelTup"}


(*
  fun obi'DeCon pat =
      fn obi => let val q = newQ (INFO'Pat pat)
                    val (seq,obi,r) = r'obi obi
                in (seq $ seq'I (INS'DeCon (q,r)), mkObi (DELAYED'R (LOC'Stack q)),
                    {Replace = fn _:obi => obi})
                end
*)

       (*| obi as OBI'NotC _ => let val q = newQ (INFO'String "silly-obi'deCon/NotC")
                              in (seq'Empty,
                                  OBI'Location (LOC'Stack q),
                                  {Replace = fn _ => obi})
                              end*)


(*
  fun obi'MatchC vid : matcher =
      {StaticMatch = fn obi => let val (seq,obi,r) = r'obi obi
                               in SMA'Maybe (seq, IM'MatchC (r,vid), obi)
                               end,
       Xpos = Identity,
       Xneg = Identity}
*)

  (*val obi'DeCon = obi'DestructSimple INS'DeCon*)


  fun obi'DestructSimple INS'Destruct pat =
      fn obi => let val q = newQ (INFO'Pat pat)
                    val (seq,obi,r) = r'obi obi
                in (seq $ seq'I (INS'Destruct (q,r)), mkObi (DELAYED'R (LOC'Stack q)),
                    {Replace = fn _:obi => obi})
                end

  val obi'DeExcon = obi'DestructSimple INS'DeExcon
  val obi'DeRef = obi'DestructSimple INS'DeRef


  fun obi'MatchScon scon : matcher =
      {StaticMatch = fn obi => let val (seq,obi,r) = r'obi obi
                               in SMA'Maybe (seq, IM'MatchScon (r,scon), obi)
                               end,
       Xpos = fn obi => obi,
       Xneg = fn obi => obi}

  fun obi'MatchE exname : matcher =
      {StaticMatch = fn obi => let val (seq,obi,r) = r'obi obi
                               in SMA'Maybe (seq, IM'MatchE (r,exname), obi)
                               end,
       Xpos = fn obi => obi,
       Xneg = fn obi => obi}


  fun obi'DeExcon pat =
      fn obi => let val q = newQ (INFO'Pat pat)
                    val (seq,obi,r) = r'obi obi
                in (seq $ seq'I (INS'DeExcon (q,r)), mkObi (DELAYED'R (LOC'Stack q)),
                    {Replace = fn _:obi => obi})
                end


  fun obi'DeRef pat =
      fn obi => let val q = newQ (INFO'Pat pat)
                    val (seq,obi,r) = r'obi obi
                in (seq $ seq'I (INS'DeRef (q,r)), mkObi (DELAYED'R (LOC'Stack q)),
                    {Replace = fn _:obi => obi})
                end



(*----------------------------------------------------------------------
Index: b_code - cont passing, with Succ/Fail result type...
----------------------------------------------------------------------*)

  datatype sf = SF'Succ of seq * env | SF'Fail of code

  abstype b_code = B of {fail: obi -> code} 
                        * {succ: obi * env -> sf}
                          * obi * env -> sf
  with val mkB = B
       fun deB (B x) = x
  end

  val closeB : (b_code * {fail: obi -> code} * obi -> sf) =
      fn (b,fail,obi) =>
         deB b (fail,
                {succ = fn (_,env) => SF'Succ (seq'Empty,env)},
                obi, env'empty)

  val b'Empty : b_code =
      mkB (fn ({fail=_},{succ},obi,env) => succ (obi,env))

  val b'Fail : b_code =
      mkB (fn ({fail},{succ=_},obi,env) => SF'Fail (fail obi))

  val op && : (b_code * b_code -> b_code) =
      fn (b1,b2) =>
         mkB (fn (fail,succ,obi,env) => 
                 deB b1 (fail,{succ = fn (obi,env) => deB b2 (fail,succ,obi,env)},
                         obi,env))

  val b'Bind : (uid -> b_code) =
      fn x =>
         mkB (fn ({fail=_},{succ},obi,env) => let val (seq1,obi,r) = r'obi obi
                                              in case (succ (obi, env ++ env'single (x,r)))
                                                  of SF'Succ (seq2,env) => SF'Succ (seq1 $ seq2, env)
                                                   | res as SF'Fail _  => res
                                              end)

  val b'Match : {StaticMatch: obi -> sma, 
                 Xpos: obi -> obi,
                 Xneg: obi -> obi} -> b_code =
      fn {StaticMatch,Xpos,Xneg} =>
         mkB (fn ({fail},{succ},obi,env) =>
                 case (StaticMatch obi)
                  of SMA'Yes => succ (obi, env)
                   | SMA'No => SF'Fail (fail obi) (* SF'Succ (mkSeq (fn _ => fail obi), env) *)
                   | SMA'Maybe (seq1,im,obi) =>
                     let val failCode = fail (Xneg obi)
                     in case (succ (Xpos obi, env))
                         of SF'Succ (seq2,env) => SF'Succ (seq1 $ seq'Match (im,{fail=failCode}) $ seq2, env)
                          | res as SF'Fail _ => res
                     end)
         
  val b'Destruct : {Sub: obi -> seq * obi * {Replace: obi -> obi}} -> b_code -> b_code =
      fn {Sub} =>
         fn bsub =>
            mkB (fn ({fail},{succ},obi,env) =>
                    let val (seq1,obi,{Replace}) = Sub(obi)
                    in case (deB bsub ({fail = fn obi => fail (Replace obi)},
                                       {succ = fn (obi,env) => succ (Replace obi, env)},
                                       obi,env))
                        of SF'Succ (seq2,env) => SF'Succ (seq1 $ seq2, env)
                         | res as SF'Fail _  => res
                    end)

(*----------------------------------------------------------------------
INDEX: Wed Sep 13 13:13:34 2006
----------------------------------------------------------------------*)

(*
  val b'Fail : b_code =
      mkB (fn ({fail},{succ=_},obi,env) => SF'Fail (fail obi))
*)


  fun old_obi'MatchScon scon : old_matcher =
      {StaticMatch = 
       fn OBI'Scon (_,scon2)        => if (eq'scon scon scon2) then Old_SMA'Yes else Old_SMA'No
        | obi as OBI'NotScon (r,xs) => if (exists (eq'scon scon) xs) then Old_SMA'No 
                                       else Old_SMA'Maybe (IM'MatchScon (r,scon), obi)
        | obi as OBI'Location r     => Old_SMA'Maybe (IM'MatchScon (r,scon), obi)
        | _                         => abort "obi'MatchScon/StaticMatch"
       ,
       Xpos = 
       fn OBI'Location r            => OBI'Scon (r,scon)
        | OBI'NotScon (r,_)         => OBI'Scon (r,scon)
        | _                         => abort "obi'MatchScon/Xpos",
       Xneg = 
       fn OBI'Location r            => OBI'NotScon (r,[scon])
        | OBI'NotScon (r,xs)        => OBI'NotScon (r,scon::xs)
        | _                         => abort "obi'MatchScon/Xneg"}



  fun obi'MatchScon scon : matcher =
      let val {StaticMatch,Xpos,Xneg} = old_obi'MatchScon scon
      in fn obi => case (StaticMatch obi)
                    of Old_SMA'Yes => SMA'Yes
                     | Old_SMA'No => SMA'No
                     | Old_SMA'Maybe (im,obi) => SMA'Maybe (im,  Xpos obi,  Xneg obi)
      end

  fun old_obi'MatchC vid : old_matcher =
      {StaticMatch = 
       fn OBI'C (_,con,_)           => if (eq'vid vid con) then Old_SMA'Yes else Old_SMA'No
        | obi as OBI'NotC (r,cons)  => if (exists (eq'vid vid) cons) then Old_SMA'No 
                                       else Old_SMA'Maybe (IM'MatchC (r,vid), obi)
        | obi as OBI'Location r     => Old_SMA'Maybe (IM'MatchC (r,vid), obi)
        | _                         => abort "obi'MatchC/StaticMatch"
       ,
       Xpos = 
       fn OBI'Location r            => OBI'C (r,vid,None)
        | OBI'NotC (r,_)            => OBI'C (r,vid,None)
        | _                         => abort "obi'MatchC/Xpos",
       Xneg =
       fn OBI'Location r            => OBI'NotC (r,[vid])
        | OBI'NotC (r,xs)           => OBI'NotC (r,vid::xs)
        | _                         => abort "obi'MatchC/Xneg"}

  fun obi'MatchC vid : matcher =
      let val {StaticMatch,Xpos,Xneg} = old_obi'MatchC vid
      in fn obi => case (StaticMatch obi)
                    of Old_SMA'Yes => SMA'Yes
                     | Old_SMA'No => SMA'No
                     | Old_SMA'Maybe (im,obi) => SMA'Maybe (im,  Xpos obi,  Xneg obi)
      end


  fun old_obi'MatchE exname : old_matcher =
      {StaticMatch = 
       fn OBI'E (_,x,_)             => if (eq'exname exname x) then Old_SMA'Yes else Old_SMA'No
        | obi as OBI'NotE (r,xs)    => if (exists (eq'exname exname) xs) then Old_SMA'No 
                                       else Old_SMA'Maybe (IM'MatchE (r,exname), obi)
        | obi as OBI'Location r     => Old_SMA'Maybe (IM'MatchE (r,exname), obi)
        | _                         => abort "obi'MatchE/StaticMatch"
       ,
       Xpos = 
       fn OBI'Location r            => OBI'E (r,exname,None)
        | OBI'NotE (r,_)            => OBI'E (r,exname,None)
        | _                         => abort "obi'MatchE/Xpos",
       Xneg =
       fn OBI'Location r            => OBI'NotE (r,[exname])
        | OBI'NotE (r,xs)           => OBI'NotE (r,exname::xs)
        | _                         => abort "obi'MatchE/Xneg"}

  fun obi'MatchE exname : matcher =
      let val {StaticMatch,Xpos,Xneg} = old_obi'MatchE exname
      in fn obi => case (StaticMatch obi)
                    of Old_SMA'Yes => SMA'Yes
                     | Old_SMA'No => SMA'No
                     | Old_SMA'Maybe (im,obi) => SMA'Maybe (im,  Xpos obi,  Xneg obi)
      end


  fun old_obi'MatchG r2 : old_matcher =
      {StaticMatch = 
       fn OBI'E _  => Old_SMA'No 
        | obi as OBI'NotE  (r,_)    => Old_SMA'Maybe (IM'MatchG (r,r2), obi)
        | obi as OBI'Location r     => Old_SMA'Maybe (IM'MatchG (r,r2), obi)
        | _                         => abort "obi'MatchG/StaticMatch",
       Xpos = fn obi => obi,
       Xneg = fn obi => obi}

  fun obi'MatchG r2 : matcher =
      let val {StaticMatch,Xpos,Xneg} = old_obi'MatchG r2
      in fn obi => case (StaticMatch obi)
                    of Old_SMA'Yes => SMA'Yes
                     | Old_SMA'No => SMA'No
                     | Old_SMA'Maybe (im,obi) => SMA'Maybe (im,  Xpos obi,  Xneg obi)
      end

(*----------------------------------------------------------------------
INDEX: Wed Sep 13 14:57:06 2006
----------------------------------------------------------------------*)


(*----------------------------------------------------------------------
INDEX: code'Share - avoid explicit manipulatin of labels...
----------------------------------------------------------------------*)

(*
  val code'Share : (code -> (code -> code) -> code) =
      fn code2 => fn F => let val lab = newLabel()
                              val code1 = F (code'Goto lab)
                          in code'Where (code1,lab,code2)
                          end
*)

  (* reverse arg order... which is clearer?...*)
  val SharingCode : (code -> code) -> (code -> code) =
      fn F => fn code2 => let val lab = newLabel()
                              val code1 = F (code'Goto lab)
                          in code'Where (code1,lab,code2)
                          end
(*----------------------------------------------------------------------
Index: closeB'rd
----------------------------------------------------------------------*)

  (* for use in let binding...*)
(*
  val closeB'rd : (b_code * {fail: code} * r_delayed -> (seq * env)) =
      fn (b,{fail},rd) => closeB (b, {fail = fn _ => fail}, mkObi rd)
*)

  val closeB'rd : (b_code * {fail: code} * r_delayed -> (seq * env)) =
      fn (b,{fail},rd) => 
         case (closeB (b, {fail = fn _ => fail}, mkObi rd))
          of SF'Succ (seq,env) => (seq,env)
           | SF'Fail _ => abort "closeB'rd /Fail" (* hack - really should avoid comp of never-used body *)
  (* actuall - I dont think can happen, because we have no info on the object being bound
    even it it has just been built by constructors - and so we connot unconditionally fail *)

(*
  val mc'rule : (b_code * (env -> code) -> m_code) =
      fn (b,RHS) =>
         mkMC (fn (fail,obi) => 
                  let val (_:obi,env,seq) = deB b (fail,obi,env'empty)
                  in seq $$ RHS env
                  end)
*)

(*
  val mc'rule : (b_code * (env -> code) -> m_code) =
      fn (b,RHS) =>
         mkMC (fn (fail,obi) => 
                  let val (seq,env) = closeB (b,fail,obi)
                  in seq $$ RHS env
                  end)
*)


  type schemes = {(*Pdx:Pdx,*)B:B,C:C,Aret:Aret,Ad:Ad,A:A,At:At,Ft:Ft(*,CM:CM,AM:AM,AMt:AMt*)}

  fun compile'B {Pd:Pd} ({A,Ad,B,C,Aret,(*CM,*)(*Pdx,*)At,Ft(*,AM,AMt*)}:schemes) : (env * bind -> env * seq) =
      fn (env,bind) =>
         case bind of
             BIND'Val (pat,atom) => let val (seq1,rd) = Ad (env,atom)
                                        val (env,seqPat) = (*if not (!new_pat_comp)
                                                           then Pdx ({fail=code'RaiseBind},env,pat,rd)
                                                           else*) let val b = Pd (env,pat)
                                                                    val (seq,env1) = closeB'rd (b,{fail=code'RaiseBind},rd)
                                                                in (env ++ env1, seq)
                                                                end
                                    in  (env, seq1 $ seqPat)
                                    end
           | BIND'ValRec binds => compile_LetRecBinds {Ft=Ft} (env,binds)

  fun compile'C {Pd:Pd,CM:CM} ({A,Ad,B,C,Aret(*,CM*),(*Pdx,*)At,Ft(*,AM,AMt*)}:schemes) : (env * cps -> code) =          let
                 val (fill,envNew) = compile_saveFrame INS'SetContFrameElem (env,save) 
                 val code1 = C (env,cps1)
                 val (envNew,seqPat) = (*if not (!new_pat_comp)
                                       then Pdx ({fail=code'RaiseBind},envNew,pat,DELAYED'R LOC'Cret)
                                       else*) let val b = Pd (envNew,pat)
                                                val (seq,env1) = closeB'rd (b,{fail=code'RaiseBind},DELAYED'R LOC'Cret)
                                            in (envNew ++ env1, seq)
                                            end
  fun compile'Aret {AM:AM} ({Ad,A,At,(*Pdx,*)B,C,Aret,Ft(*,CM,AM,AMt*)}:schemes) : (env * atom -> code) =  
fun compile'Ad ({Ad,A,At,(*Pdx,*)B,C,Aret,Ft(*,CM,AM,AMt*)}:schemes) : (env * atom -> seq * r_delayed) =

  local
      fun Pd x = compile'Pd {Pd=Pd} x
      fun makeSchemes () = {(*Pdx=Pdx,*)B=B,C=C,(*CM=CM,*)Ad=Ad,Aret=Aret,A=A,At=At,Ft=Ft(*,AM=AM,AMt=AMt*)}
      and Ad    x = compile'Ad      (makeSchemes()) x
      and Aret  x = compile'Aret    {AM=AM} (makeSchemes()) x
      and A     x = compile'A       (makeSchemes()) x
      and At    x = compile'At      {AMt=AMt} (makeSchemes()) x
      and Ft    x = compile'Ft      (makeSchemes()) x
      and B     x = compile'B       {Pd=Pd} (makeSchemes()) x
      and C     x = compile'C       {Pd=Pd,CM=CM}(makeSchemes()) x

      and AMt  x = compile'AMt {Pd=Pd,AMt=AMt,At=At} x
      and AM   x = compile'AM {Pd=Pd,AM=AM,Aret=Aret} x
      and CM   x = compile'CM {Pd=Pd,AM=AM,CM=CM,C=C} x

  in val schemes = makeSchemes()
  end


(*----------------------------------------------------------------------
INDEX: Tue Nov 14 16:54:31 2006
----------------------------------------------------------------------*)

  fun cstat'closure sizes C es =
      let val clo = makeCid "Myclosure"
          val acts = cacts'closure clo es
          val return = cstat'Return (cexp'Id clo)
          val setElemsAndReturn = fold (fn stat => fn act => cstat'ActThen (act,stat)) return (rev acts)
          val body = cstat'ActThen (cact'Assign (clo, cexp'NewFn sizes C), setElemsAndReturn)
      in cstat'StaticThen (ctype'Nword, clo, cstat'If (cexp'Id clo,return,body))
      end

  fun cexp'closure_bits V EmitDef name cid sizes code (FRAME A) =
      let val C = NEW_cexp'code V EmitDef name code
          val es = map V (list'array A)
          val body = cstat'closure sizes C es
          val def = cdef'FuncDef (ctype'Nword,cid,[],body)
          val () = EmitDef(def)
      in cexp'FunCall (ccall (cid,[]))
      end

  fun cexp'closure_bits V EmitDef (EmitAct:cact->unit) name cid sizes code (FRAME A) =
      let val C = NEW_cexp'code V EmitDef name code
          val es = map V (list'array A)
          val clo = makeCid "Myclosure"
          val acts = cacts'closure clo es
          val return = cstat'Return (cexp'Id clo)
          val setElemsAndReturn = fold (fn stat => fn act => cstat'ActThen (act,stat)) return (rev acts)
          val body = cstat'ActThen (cact'Assign (clo, cexp'NewFn sizes C), setElemsAndReturn)
          val body = cstat'StaticThen (ctype'Nword, clo, cstat'If (cexp'Id clo,return,body))
          val def = cdef'FuncDef (ctype'Nword,cid,[],body)
          val () = EmitDef(def)
      in cexp'FunCall (ccall (cid,[]))
      end



  fun cstat'pap F sizes es =
      let val pap = makeCid "MyPap"
          val sets = map (fn (e,n) => ccall'SetFrameElement (cexp'Id pap) n e) (enumerateFrom 0 es)
          val return = cstat'Return (cexp'Id pap)
          val body = cstat'DefThen (ctype'Nword, pap, cexp'NewPap F sizes,
                                    fold (fn stat => fn set => cstat'ActThen (cact'Call set,stat)) return (rev sets))
      in body
      end

  fun cstat'pap F sizes es =
      let val pap = makeCid "MyPap"
          val acts = cacts'pap pap es
          val return = cstat'Return (cexp'Id pap)
          val body = cstat'DefThen (ctype'Nword, pap, cexp'NewPap F sizes,
                                    fold (fn stat => fn act => cstat'ActThen (act,stat)) return (rev acts))
      in body
      end

  fun cexp'pap V EmitDef EmitAct name cid closure early =
      let (*val C = NEW_cexp'code V EmitDef name code*)
          val es = map V early
          val func = cexp'closure V EmitDef EmitAct (newCid ("paps_closure_"^name)) closure
          val num_early = length early
          val num_late = num_args'closure closure - num_early
          val pap_sizes = {num_early = num_early, num_late = num_late}
          val body = cstat'pap func pap_sizes es
          val def = cdef'FuncDef (ctype'Nword,cid,[],body)
          val () = EmitDef(def)
      in cexp'FunCall (ccall (cid,[]))
      end

(*----------------------------------------------------------------------
INDEX: Wed Nov 22 15:17:56 2006
----------------------------------------------------------------------*)

  fun cexp'value ccV EmitDef v =
      let val (defs,e) = ccV v
          val (_:unit list ) = map EmitDef (rev defs)
      in e
      end
  fun cexp'code ccV EmitDef name code =
      let val R = cexp'r (cexp'value ccV EmitDef)
          fun Emit name code =
              let val x = newCid name
                  val body = cstat'code Emit R code
                  val def = cdef'FuncDef (ctype'Ncode,x,[],body)
                  val e = (*cexp'makeCode*) (cexp'Id x)
                  val () = EmitDef def
              in e
              end
      in Emit name code
      end
  fun cc'code ccV name code =
      let val emitted = ref []
          fun EmitDef def =  (emitted := def :: !emitted)
          val e = cexp'code ccV EmitDef name code
      in (!emitted,e)
      end


(*----------------------------------------------------------------------
INDEX: ccode - frame,closure
----------------------------------------------------------------------*)
(*
  fun cc'closure ccV name sizes code (FRAME A) =
      let val (defs,C) = cc'code ccV name code
          val (defs,es) =
              fold (fn (defs,es) => fn v => let val (defs1,e) = ccV v
                                            in (defs1 @ defs, e::es)
                                            end) (defs,[]) (rev (list'array A))
          val body = cstat'closure sizes C es
          val clo = newCid ("closure_"^name)
          val def = cdef'FuncDef (ctype'Nword,clo,[],body)
      in (def::defs, cexp'FunCall (ccall (clo,[])))
      end
*)
          (*fun OldExportC ccV = cc'closure ccV name sizes code frame*)


  fun OLD_cexp'pap V EmitDef EmitAct name cid closure early =
      let val es = map V early
          val F = cexp'closure V EmitDef EmitAct (newCid ("paps_closure_"^name)) closure
          val num_early = length early
          val num_late = num_args'closure closure - num_early
          val pap_sizes = {num_early = num_early, num_late = num_late}
          val pap = makeCid "MyPap"
          val acts = cacts'pap pap es
          val return = cstat'Return (cexp'Id pap)
          val body = cstat'DefThen (ctype'Nword, pap, cexp'NewPap F pap_sizes,
                                    fold (fn stat => fn act => cstat'ActThen (act,stat)) return (rev acts))
          val def = cdef'FuncDef (ctype'Nword,cid,[],body)
          val () = EmitDef(def)
      in cexp'FunCall (ccall (cid,[]))
      end


(*----------------------------------------------------------------------
INDEX: Tue May 29 13:49:12 2007
----------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: compute constraints (Embed.exp)
----------------------------------------------------------------------*)

local open Embed 
in
  datatype context = Context of uid -> tv
  fun app'context (Context f) c = f c
  val empty'context = Context (fn x => abort "empty'context")
  fun extend'context (Context f) (x,tv) = Context (fn y => if (eq'uid x y) then tv else f y)
  type E = (context -> exp -> tor * constraints)
  fun constraints_E (E:E) : E = fn context =>
      fn EXP'Value (_,{info})       => notYet "value" 
       | EXP'Var x                  => (TOR'Tv (app'context context x), empty'constraints)
       | EXP'Tuple exps             => notYet "tuple" 
       | EXP'Let (dec,exp)          => notYet "let" 
       | EXP'Case (exp,match)       => notYet "case" 
       | EXP'Raise exp              => notYet "raise" 
       | EXP'Fn (FUNC'Def(xs,exp))  => let val (context,vs) = fold (fn (context,vs) => fn x => 
                                                                                          let val v = genTv (sof'uid x)
                                                                                          in (extend'context context (x,v), v::vs)
                                                                                          end) (context,[]) xs
                                           val (tor,c) = E context exp
                                           val tor = fold (fn tor => fn v => TOR'Arrow (TOR'Tv v,tor)) tor vs
                                       in (tor,c)
                                       end
       | EXP'App (func,args)        => fold (fn (tor1,c1) => 
                                                fn arg =>
                                                   let val (tor2,c2) = E context arg
                                                       val tor = TOR'Tv (genTv "@")
                                                       val c = c1 @ c2 @ [(tor1, TOR'Arrow (tor2,tor))]
                                                   in (tor,c)
                                                   end) (E context func) args
       | EXP'Handle (exp,match)     => notYet "handle" 
       | EXP'NewEx x                => notYet "new-ex" 
       | EXP'PrimApp (prim,arg)     => notYet "prim-app" 
end
  fun constraints'exp context exp  = constraints_E constraints'exp context exp


  fun occurs v = existsTv'tor (fn v' =>v=v')
  val unify'constraints =
      let exception NoUnifier
          val rec unify =
              fn [] => empty'sub
               | (tor1,tor2)::constraints =>
                 let fun unifyTv tv tor = if (occurs tv tor) then raise NoUnifier else
                                           let val sub = unit'sub (tv,tor)
                                               val constraints = mapTor'constraint (apply'sub sub) constraints
                                               (*val () = see (Word "cons1=" @@ layConstraints constraints)*)
                                           in compose'sub (unify constraints, sub)
                                           end
                 in case (tor1,tor2) of
                        (TOR'Tv v1,tor as TOR'Tv v2) => if (v1=v2) then unify constraints else unifyTv v1 tor
                      | (TOR'Tv v,tor) => unifyTv v tor
                      | (tor, TOR'Tv v) => unifyTv v tor
                      | (TOR'Arrow (a1,b1),TOR'Arrow(a2,b2)) => unify ((a1,a2)::(b1,b2)::constraints)
                 end
      in fn x => Some (unify x) handle NoUnifier => None
      end

 (*val exp = Embed.embed'exp Basis.env'empty Lexp*)

(*
  val rec unify'constraints =
      fn [] => Some empty'sub
       | ((tor1,tor2)::constraints) =>
          exec'unifier
          (unifier'join (unifier'fn (fn sub => unify (apply'sub sub tor1,apply'sub sub tor2)),
                         unifier'fn (fn sub => unify'constraints (mapTor'constraint (apply'sub sub) constraints))))
*)



(*----------------------------------------------------------------------
INDEX: constraints - list of type pairs to be unified
----------------------------------------------------------------------*)
  type constraints = (tor*tor) list
  (*fun mapTor'constraint TOR = map (fn (tor1,tor2) => (TOR tor1,TOR tor2)) : constraints -> constraints*)
  val empty'constraints = []
(*
  val layConstraints =
      layVert o map (fn (t1,t2) => lay'tor t1 ++ Word "=" ++ lay'tor t2)
*)
  fun unify tt = exec'unifier (unifier'tt tt)
  fun unify'constraints cs =
      exec'unifier (unifier'joinList (map unifier'tt cs))



  fun U_tc'match (E:E) (context,match) =
      let val (tor,c) = U_constraints'match E (context,match)
          val u = unifier'tts c
      in case (exec'unifier u)
          of Err s => Err s
           | Okay sub => Okay (sub, apply'sub sub tor)
      end

(*----------------------------------------------------------------------
INDEX: Wed May 30 16:07:52 2007 -- pre - "uni" monad
----------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: unifier abstraction
----------------------------------------------------------------------*)
  datatype unifier = Unifier of (sub -> sub err)
  fun exec'unifier sub (Unifier f) = f sub
  val unifier'fn = Unifier

  fun unifier'join (Unifier f1, Unifier f2) = 
      Unifier (fn sub => case (f1 sub)
                          of Err s => Err s
                           | Okay sub1 => let val sub = compose'sub (sub1,sub)
                                          in case (f2 sub)
                                              of Err s => Err s
                                               | Okay sub2 => Okay (compose'sub (sub2,sub))
                                          end)

  val unifier'empty = unifier'fn (fn sub => Okay sub)
  val unifier'joinList = fold (curry unifier'join) unifier'empty

(*----------------------------------------------------------------------
INDEX: unifier'tt
----------------------------------------------------------------------*)
  fun occurs v = existsTv'tor (fn v' =>v=v')

  fun unifier'tt (tor1,tor2) =
      unifier'fn 
      (fn sub => 
          case (apply'sub sub tor1,apply'sub sub tor2)
           of (TOR'Tv tv1,TOR'Tv tv2)   => if (tv1=tv2) then Okay (empty'sub) else Okay (unit'sub (tv1,TOR'Tv tv2))
            | (TOR'Tv tv,tor)           => if (occurs tv tor) then Err "v/occ" else Okay (unit'sub (tv,tor))
            | (tor,TOR'Tv tv)           => if (occurs tv tor) then Err "occ/v" else Okay (unit'sub (tv,tor))
            | (TOR'Bool,TOR'Bool)       => Okay empty'sub
            | (TOR'Arrow (a1,b1),TOR'Arrow (a2,b2)) => exec'unifier empty'sub (unifier'join (unifier'tt (a1,a2), unifier'tt (b1,b2)))
            | (TOR'Row row1, TOR'Row row2) => 
              (case (zip'rows (row1,row2))
                of Err s => Err ("row:"^s)
                 | Okay zipped => exec'unifier empty'sub (unifier'tts (map (fn (_,tt) => tt) zipped)))
            (* unification failures *)
            | (TOR'Bool,TOR'Arrow _)    => Err "bool/arrow"
            | (TOR'Bool,TOR'Row _)      => Err "bool/row"
            | (TOR'Arrow _,TOR'Bool)    => Err "arrow/bool"
            | (TOR'Arrow _,TOR'Row _)   => Err "arrow/row"
            | (TOR'Row _,TOR'Bool)      => Err "row/bool"
            | (TOR'Row _,TOR'Arrow _)   => Err "row/arrow")
  and unifier'tts tts =
      unifier'joinList (map unifier'tt tts)

  val REAL_unifier'tts = unifier'tts

(*----------------------------------------------------------------------
INDEX: constraint generation schemes
----------------------------------------------------------------------*)
  type P = (context * pat -> tor * context)
  type E = (context * exp -> tor * (tor * tor) list)

(*----------------------------------------------------------------------
INDEX: patrow
----------------------------------------------------------------------*)
  fun U_tc'patrow (P:P) context (PATROW (xs,{wildcard=Some pos})) = abort ("U_tc'patrow/wildcard: "^Pos.sofPos pos)
    | U_tc'patrow (P:P) context (PATROW (xs,{wildcard=None})) =
      case (fold (fn (ys,context) => 
                     fn (lab,pat) => let val (tor,context) = P (context,pat)
                                         val ys = (lab,tor) :: ys
                                     in (ys,context)
                                     end) ([],context) (rev xs))
       of (ys,c) => (TOR'Row ys,c) 

(*----------------------------------------------------------------------
INDEX: pat
----------------------------------------------------------------------*)
  fun U_tc'pat (P:P) : P = fn (context,pat) =>
      case pat of
          PAT'Wild                  => notYet "pat/wild"
        | PAT'Scon scon             => notYet "pat/scon"
        | PAT'Var (LONG ([],x))     => let val v = genTv (sof'vid x)
                                           val context = extend'context context (x,v)
                                           val tor = TOR'Tv v
                                       in (tor,context)
                                       end
        | PAT'Var x                 => notYet "pat/var/long"
        | PAT'Record patrow         => U_tc'patrow P context patrow
        | PAT'Unit                  => notYet "pat/unit"
        | PAT'Tuple pats            => U_tc'patrow P context (patrow'tuple pats)
        | PAT'List pats             => notYet "pat/list"
        | PAT'Cons (x,pat)          => notYet "pat/cons"
        | PAT'Typed (pat,ty)        => notYet "pat/typed"
        | PAT'Layered (vid,tyo,pat) => notYet "pat/layered"

  local fun P x  = U_tc'pat P x
  in val tc'pat = P
  end

(*----------------------------------------------------------------------
INDEX: match
----------------------------------------------------------------------*)
  fun U_constraints'match (E:E) (context,match) =
      case match
       of MATCH (pos,[MRULE (pat,exp)]) => let val (tor1,context) = tc'pat (context,pat)
                                               val (tor2,c) = E (context,exp)
                                               val tor = TOR'Arrow (tor1,tor2)
                                           in (tor,c)
                                           end
        | _ => notYet "constraints'match"

(*----------------------------------------------------------------------
INDEX: exprow
----------------------------------------------------------------------*)
  fun U_constraints'exprow (E:E) context (EXPROW xs) = 
      case (fold (fn (ys,c1) => fn (lab,exp) => let val (tor,c2) = E (context,exp)
                                                    val c = c1 @ c2 
                                                    val ys = (lab,tor) :: ys
                                                in (ys,c)
                                                end) ([],[]) (rev xs))
       of (ys,c) => (TOR'Row ys,c) 

(*----------------------------------------------------------------------
INDEX: exp
----------------------------------------------------------------------*)
  fun U_constraints'exp (E:E) : E = fn (context,exp) =>
      case exp of
          EXP'Scon scon             => notYet "scon"
        | EXP'Var (LONG([],x),pos)  => let val tor = TOR'Tv (app'context context x)
                                           val c = []
                                       in (tor,c)
                                       end
        | EXP'Var (x,pos)           => notYet "long-var"
        | EXP'Record exprow         => U_constraints'exprow E context exprow
        | EXP'Select (pos,lab)      => notYet "select"
        | EXP'Unit                  => notYet "unit"
        | EXP'Tuple exps            => U_constraints'exprow E context (exprow'tuple exps)
        | EXP'List exps             => notYet "list"
        | EXP'Vector exps           => notYet "vector"
        | EXP'Seq exps              => notYet "seq"
        | EXP'Let (dec,exp)         => notYet "let"
        | EXP'App (func,arg)        => let val (tor1,c1) = E (context,func)
                                           val (tor2,c2) = E (context,arg)
                                           val tor = TOR'Tv (genTv "@")
                                           val c = c1 @ c2 @ [(tor1, TOR'Arrow (tor2,tor))]
                                       in (tor,c)
                                       end
        | EXP'Typed (exp,ty)        => notYet "typed"
        | EXP'Andalso (e1,e2)       => notYet "andalso"
        | EXP'Orelse (e1,e2)        => notYet "orelse"
        | EXP'Handle (exp,match)    => notYet "handle"
        | EXP'Raise exp             => notYet "raise"
        | EXP'If (e1,e2,e3)         => let val (tor1,c1) = E (context,e1)
                                           val (tor2,c2) = E (context,e2)
                                           val (tor3,c3) = E (context,e3)
                                           val c = c1 @ c2 @ c3 @ [(tor1,TOR'Bool),(tor2, tor3)]
                                       in (tor2,c)
                                       end
        | EXP'While (e1,e2)         => notYet "while"
        | EXP'Case (exp,match)      => notYet "case"
        | EXP'Fn match              => U_constraints'match E (context,match)

  local fun E x  = U_constraints'exp E x
  in val constraints'exp = E
  end

(*----------------------------------------------------------------------
INDEX: unify constraint in two parts
----------------------------------------------------------------------*)
  local fun splitListAcc acc [] = acc
          | splitListAcc (A,B) (x::xs) = splitListAcc (x::B,A) xs
  in fun splitListAnyhow xs = splitListAcc ([],[]) (rev xs)
  end

  val layConstraints =
      layVert o map (fn (t1,t2) => raw_lay'tor t1 ++ Word "=" ++ raw_lay'tor t2)

(*
  val unifier'tts = 
      fn tts =>
         let val (tts1,tts2) = splitListAnyhow tts
             val () = see (Word "tts=" @@ layConstraints tts)
             val () = see (Word "tts1=" @@ layConstraints tts1)
             val () = see (Word "tts2=" @@ layConstraints tts2)
         in unifier'fn 
            (fn sub =>
                case (exec'unifier sub (REAL_unifier'tts tts1))
                 of Err s => Err s
                  | Okay sub1 => 
                    (case (exec'unifier sub1 (REAL_unifier'tts tts2))
                      of Err s => Err s
                       | Okay sub2 => Okay (compose'sub (sub1,sub2))))
         end
*)

  val unifier'tts = 
      fn tts =>
         let val () = see (Word "cons=" @@ layConstraints tts)
         in REAL_unifier'tts tts
         end

(*----------------------------------------------------------------------
INDEX: tc'exp (compute constraints, then unify)
----------------------------------------------------------------------*)
  fun old_tc'exp context exp = 
      let val (tor1,c) = constraints'exp (context,exp)
          val u = unifier'tts c
      in case (exec'unifier empty'sub u)
          of Err s => Err s
           | Okay sub => Okay (apply'sub sub tor1)
      end

(*----------------------------------------------------------------------
INDEX: runTc
----------------------------------------------------------------------*)
  fun runTc exp =
      let val context = empty'context
      in case (old_tc'exp context exp) 
          of Err s => abort ("TcErr:"^s)
           | Okay tor => 
             let (*val () = see (Word "raw =" @@ raw_lay'tor tor)*)
                 val () = see (Word "(old)result =" @@ lay'tor tor)
             in ()
             end
      end

(*----------------------------------------------------------------------
INDEX: Wed May 30 16:08:48 2007
----------------------------------------------------------------------*)

(*
  fun new_tc'exp context exp =
      execUni ((tc'exp (context,exp) ** withSubUni resultUni)
               >>> (fn (tor,sub) => (apply'sub sub tor)))


  fun new_tc'exp context exp =
      execUni (tc'exp (context,exp) >> applySubUni)   
*)

  (*fun joinUni (u1,u2) = bindUni (u1, fn () => bindUni (u2, fn () => resultUni ()))*)

(*----------------------------------------------------------------------
INDEX: Thu May 31 15:40:24 2007
----------------------------------------------------------------------*)

(* old functional rep *)

  datatype valenv = Valenv of vid -> tscheme
  fun app'valenv (Valenv f) c = f c
  val valenv'empty = Valenv (fn x => abort "valenv'empty")
  fun extend'valenv (Valenv f) (x,tv) = Valenv (fn y => if (x = y) then tv else f y)


(* old via new *)
  fun app'valenv ve k = case (look'vid ve k) of (ts,_) => ts
  fun extend'valenv ve (k,v) = valenv'plus ve (valenv'single (k,(v,Notyet_idstatus)))

(*----------------------------------------------------------------------
INDEX: Thu May 31 15:53:40 2007 valenv (extend --> plus)
----------------------------------------------------------------------*)

  type P = (valenv * pat -> tor * valenv)

  fun tc'patrow P (valenv,PATROW (xs,{wildcard=Some pos})) = abort ("tc'patrow/wildcard: "^Pos.sofPos pos)
    | tc'patrow P (valenv,PATROW (xs,{wildcard=None})) =
      case (fold (fn (ys,valenv) => 
                     fn (lab,pat) => let val (tor,valenv) = P (valenv,pat)
                                         val ys = (lab,tor) :: ys
                                     in (ys,valenv)
                                     end) ([],valenv) (rev xs))
       of (ys,c) => (TOR'Row ys,c) 

(*----------------------------------------------------------------------
INDEX: pat
----------------------------------------------------------------------*)
  fun U_tc'pat {P} : P = fn (valenv,pat) =>
      case pat of
          PAT'Wild                  => notYet "pat/wild"
        | PAT'Scon scon             => notYet "pat/scon"
        | PAT'Var (LONG ([],x))     => let val v = genTv (sof'vid x)
                                           val tor = TOR'Tv v
                                           val tscheme = tscheme'tor tor
                                           val ve1 = valenv'single (x,(tscheme,Notyet_idstatus))
                                           val valenv = valenv'plus valenv ve1
                                       in (tor,valenv)
                                       end
        | PAT'Var x                 => notYet "pat/var/long"
        | PAT'Record patrow         => tc'patrow P (valenv,patrow)
        | PAT'Unit                  => notYet "pat/unit"
        | PAT'Tuple pats            => tc'patrow P (valenv,patrow'tuple pats)
        | PAT'List pats             => notYet "pat/list"
        | PAT'Cons (x,pat)          => notYet "pat/cons"
        | PAT'Typed (pat,ty)        => notYet "pat/typed"
        | PAT'Layered (vid,tyo,pat) => notYet "pat/layered"

  fun tc'match (P:P) (E:E) (valenv,match) =
      case match
       of MATCH (pos,[MRULE (pat,exp)]) => let val (tor1,valenv) = P (valenv,pat)
                                           in E (valenv,exp) >>> (fn tor2 => TOR'Arrow (tor1,tor2))
                                           end
        | _ => notYet "constraints'match"

(*----------------------------------------------------------------------
INDEX: valbind
----------------------------------------------------------------------*)
  fun tc'valbind (P:P) (E:E) (valenv,vb) =
      case vb of
          VALBIND'Seq (pat,exp,None)    => let val (tor1,valenv) = P (valenv,pat)
                                           in E (valenv,exp) >> (fn tor2 => unifyTT (tor1,tor2) -$$ resultUni valenv)
                                           end
        | VALBIND'Seq (pat,exp,Some vb) => notYet "valbind/Some"
        | VALBIND'Rec vb                => notYet "valbind/rec"

(*----------------------------------------------------------------------
INDEX: pre intro downwards context
----------------------------------------------------------------------*)

  type P = (pat -> tor * valenv)
  type E = (valenv * exp -> tor uni)
  type D = (valenv * dec -> valenv uni)

(*----------------------------------------------------------------------
INDEX: Mon Jun  4 13:00:34 2007
----------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: tv - type vars (for unification)
----------------------------------------------------------------------*)
  datatype tv = Tv of string
  fun deTv (Tv s) = s
  val sof'tv = deTv
  local val U = ref 1
  in fun genTv prefix = let val n = !U
                        in U := 1+n; Tv(prefix ^ stringOfInt n)
                        end
  end
  fun eq'tv (Tv s1) (Tv s2) = (s1=s2)

(*----------------------------------------------------------------------
INDEX: patrow
----------------------------------------------------------------------*)
  fun tc'patrow (P) (context,PATROW (xs,{wildcard=Some pos})) = abort ("tc'patrow/wildcard: "^Pos.sofPos pos)
    | tc'patrow (P) (context,PATROW (xs,{wildcard=None})) =
      case (fold (fn (ys,valenv1) => 
                     fn (lab,pat) => let val (tor,valenv2) = P (context,pat)
                                         val ys = (lab,tor) :: ys
                                     in (ys,valenv'plus valenv1 valenv2)
                                     end) ([],valenv'empty) (rev xs))
       of (ys,c) => (tor'row ys,c) 



(*----------------------------------------------------------------------
INDEX: pat
----------------------------------------------------------------------*)
  fun U_tc'pat {P:P} : P = fn (context,pat) =>
      case pat of
          PAT'Wild                  => let val tor = TOR'Tv (genTv ())
                                       in (tor,valenv'empty)
                                       end
        | PAT'Scon scon             => (tor'scon scon,valenv'empty)
        | PAT'Var (LONG ([],x))     => let val tor = TOR'Tv (genTv ((*sof'vid x*)))
                                           val valenv = valenv'single (x,(tscheme'tor tor,Notyet_idstatus))
                                       in (tor,valenv)
                                       end
        | PAT'Var x                 => notYet "pat/var/long"
        | PAT'Record patrow         => tc'patrow P (context,patrow)
        | PAT'Unit                  => (tor'unit,valenv'empty)
        | PAT'Tuple pats            => tc'patrow P (context,patrow'tuple pats)
        | PAT'List pats             => notYet "pat/list"
        | PAT'Cons (x,pat)          => notYet "pat/cons"
        | PAT'Typed (pat,ty)        => notYet "pat/typed"
        | PAT'Layered (vid,tyo,pat) => notYet "pat/layered"

        | PAT'Typed (pat,ty)        => P (context,exp) >> (fn res as (t1,valenv) =>
                                       case (T ty) of t2 =>
                                       unifyTT (t1,t2) -$$ resultUni res)


  fun tc'tyrow T tyrow =
      case tyrow of
          TYROW xs => tor'row (map (fn (lab,ty) => (lab, T ty)) xs)



(*----------------------------------------------------------------------
INDEX: subexps
----------------------------------------------------------------------*)

  val subexps'match =
        fn MATCH (pos,mrules) => map (fn MRULE (pat,exp) => exp) mrules

  val subexps'exp = fn exp =>
      case exp of
          EXP'Scon _                => []
        | EXP'Var (x,pos)           => []
        | EXP'Record (EXPROW xs)    => map (fn (lab,exp) => exp) xs
        | EXP'Select (pos,lab)      => []
        | EXP'Unit                  => []
        | EXP'Tuple exps            => exps
        | EXP'List exps             => exps
        | EXP'Vector exps           => exps
        | EXP'Seq exps              => exps
        | EXP'Let (dec,exp)         => [exp] (* dont enter dec *)
        | EXP'App (func,arg)        => [func,arg]
        | EXP'Typed (exp,ty)        => [exp]
        | EXP'Andalso (e1,e2)       => [e1,e2]
        | EXP'Orelse (e1,e2)        => [e1,e2]
        | EXP'Handle (exp,match)    => [exp] @ subexps'match match
        | EXP'Raise exp             => [exp]
        | EXP'If (e1,e2,e3)         => [e1,e2,e3]
        | EXP'While (e1,e2)         => [e1,e2]
        | EXP'Case (exp,match)      => [exp] @ subexps'match match
        | EXP'Fn match              => subexps'match match


(*----------------------------------------------------------------------
INDEX: fold'dec (too general for me!)
----------------------------------------------------------------------*)

  fun fold'dec {T,P,E,D} acc dec =
      case dec
       of DEC'Val (tyvars,vb)       => fold'valbind P E acc vb
        | DEC'Fval fb               => notYet "fold/fval"
        | DEC'Type typbind          => acc
        | DEC'Datatype (db,tbo)     => acc
        | DEC'DataReplicate (a,b)   => acc
        | DEC'Abstype (db,tbo,dec)  => D acc dec
        | DEC'Exception (EXBIND bs) => fold (fold'exbind1 T) acc bs
        | DEC'Local (dec1,dec2)     => D (D acc dec1) dec2
        | DEC'Open xs               => acc
        | DEC'Empty                 => acc
        | DEC'Seq (dec1,dec2)       => D (D acc dec1) dec2


  local 
      fun collectT acc ty =
          case ty
           of TY'Var x              => if exists (eq'tyvar x) acc then acc else x :: acc
            | TY'Record (TYROW xs)  => fold (fn acc => fn (lab,ty) => collectT acc ty) acc xs
            | TY'Con (tys,ltc)      => fold collectT acc tys
            | TY'Tuple tys          => fold collectT acc tys
            | TY'Func (ty1,ty2)     => collectT (collectT acc ty1) ty2
      fun collectE acc exp = 
          case exp
           of EXP'Typed (exp,ty)    => collectT (collectE acc exp) ty
            | _                     => fold collectE acc (subexps'exp exp)
  in fun collectExplitTvars exp = rev (collectE [] exp)
  end

(*----------------------------------------------------------------------
INDEX: Wed Jun  6 20:45:14 2007
----------------------------------------------------------------------*)


                                           (*val Z = Y -- tyvars
                                           val S = X -- Z
                                           val () = see (Word "X =" @@ (layCommaSep o map (Word o sof'tyvar)) X)
                                           val () = see (Word "Y =" @@ (layCommaSep o map (Word o sof'tyvar)) Y)
                                           val () = see (Word "Z =" @@ (layCommaSep o map (Word o sof'tyvar)) Z)*)

(*        val tyvars = []
          val () = see (Word "exp =" @@ lay'exp exp)
          val X = unguarded'exp exp
          val Y = list'tvscoped (tvscoped'context context)
          (*val Z = Y -- tyvars
          val S = X -- Z
          val () = see (Word "X =" @@ (layCommaSep o map (Word o sof'tyvar)) X)
          val () = see (Word "Y =" @@ (layCommaSep o map (Word o sof'tyvar)) Y)
          val () = see (Word "Z =" @@ (layCommaSep o map (Word o sof'tyvar)) Z)*)
          val S = X -- (Y -- tyvars)
          val () = see (Word "S =" @@ (layCommaSep o map (Word o sof'tyvar)) S)
          val context = context'plus context (context'tvscoped (tvscoped'list S)) *)
      (*in case (execUni (tc'exp (context,exp) >> refineTorUni))*)


(*----------------------------------------------------------------------
INDEX: Thu Jun  7 18:14:50 2007
----------------------------------------------------------------------*)

(*
  val getTopExp = 
      (fn [Lang.TOPDEC'Exp exp]=> exp | _ => abort "getTopExp")
      o filter (fn Lang.TOPDEC'Strdec Lang.STRDEC'Empty=> false | _ => true)
  fun old_tc'topdecs (context,tds)=
      let val exp= getTopExp tds
      in tc'expTop (context,exp)
      end
*)

  fun tc'expTop (context,exp) = 
      let
          val contextVars = tvarsOf'context context
          val () = case contextVars of [] => () | _ => abort "free-vars-in-context"
          val tyvars = []
          (*val () = see (Word "exp =" @@ lay'exp exp)*)
          val X = unguarded'exp exp
          val Y = list'tvscoped (tvscoped'context context)
          val S = X -- (Y -- tyvars)
          (*val () = see (Word "S =" @@ (layCommaSep o map (Word o sof'rigid)) S)*)
          val context = context'plus context (context'tvscoped (tvscoped'list S))
      in
          tc'exp (context,exp) >> (fn tor =>
          theSubUni >>> (fn sub => 
          (*refineTor sub tor*)
          tscheme'close S contextVars sub tor
          ))
      end



(*
  fun tc'topdecs (context,tds) =
      case tds (*(filter (fn TOPDEC'Strdec STRDEC'Empty => false | _ => true) tds)*)
       of []                => notYet "topdecs/[]"
        | [td]              => tc'topdec (context,td)
        | _                 => notYet "topdecs/2+"
*)

  fun lay'tscheme (Tscheme (tvs,tor)) =
      let (*val tvs' = tvs @ tvarsOf'tor tor*)
          val assoc = assoc'list (enumerateFrom 0 tvs)
          (*fun TV tv = Word (sof'TvInt (look'assoc "lay'tscheme" sof'tv assoc tv))*)
          fun TV tv = Word (case (lookup'assoc assoc tv) of Some n => sof'TvInt n | None => sof'tv tv)
      in (*Word "forall" @@ layBracket (layCommaSep (map TV tvs)) @@ Word "." @@ *)
          lay'tor_TV TV tor
      end

  fun tscheme'close scoped contextVars sub tor =
      let
          val tor = refineTor sub tor
          val a = assoc'list (map (fn rigid => (rigid, TOR'Tv (genTv ()))) scoped)
          val tor = optMapRigid'tor (fn rigid => find'assoc a (eq'tyvar rigid)) tor
          val tvs = tvarsOf'tor tor -- contextVars
          (*val tvs = case tvs of [] => [] | _::tvs => tvs (* HACK -loose one tvar *)*)
      in Tscheme (tvs,tor)
      end

  fun U_tc'dec {P,E,D} : D = fn (context,dec) =>
      case dec
       of DEC'Val (tyvars,vb)       => let (*val () = see (Word "vb =" @@ Pretty.lay'dec_noIM (DEC'Val (tyvars,vb)))*)
                                           val X = unguarded'vb vb
                                           val Y = list'tvscoped (tvscoped'context context)
                                           val S = X -- (Y -- tyvars)
                                           (*val () = see (Word "S =" @@ (layCommaSep o map (Word o sof'rigid)) S)*)
                                           val context = context'plus context (context'tvscoped (tvscoped'list S))
                                       in 
                                           tc'valbind P E (context,vb) >> (fn valenv => 
                                           theSubUni >>> (fn sub =>
                                           env'valenv (valenv'close sub S (tvarsOf'context context) valenv)))
                                       end


  fun showXvb (p as (tyvars,vb)) = 
      let val () = see (Pretty.lay'dec_noIM (DEC'Val p))
          val Xs = unguarded'vb vb
          val () = see (Word "unguardedVB =" @@ (layCommaSep o map (Word o sof'rigid)) Xs)
      in ()
      end

(*----------------------------------------------------------------------
INDEX: Tue Jun 12 15:01:12 2007
----------------------------------------------------------------------*)

  (*val raw_lay'tor = lay'tor_UT (Word o sof'ut)*)

  (*fun lay'tor tor =
      let val assoc = assoc'list (enumerateFrom 0 (utsOf'tor tor))
          fun UT ut = Word (sof'UtInt (look'assoc "lay'tor" sof'ut assoc ut))
      in lay'tor_UT UT tor
      end*)


  fun tc'datbind (T:T) (context,DATBIND xs) =
      case xs
       of [(tyvars,tycon,clauses)] =>
          let val tname = genTname tycon
              val pairs = map (fn tyvar => (tyvar,genUt())) tyvars
              val uts = map #2 pairs
              val tor = tor'cons (tname,map TOR'Ut uts)
              val a = assoc'list pairs
              fun TV tv = case (lookup'assoc a tv) of Some ut => Okay (TOR'Ut ut) | None => Err (sof'tyvar tv)
              val typefun =
                  typefun'fn (fn tors =>
                                 if (length tors <> length uts)
                                 then Err (sof'tycon tycon ^ "/arity")
                                 else let val sub = sub'listPairs (zipPair uts tors)
                                      in Okay (refineTor sub tor)
                                      end)
              fun tyenv'valenv valenv = tyenv'single (tycon, tystr'cons (typefun,valenv))
              val tyenv = tyenv'plus (tyenv'context context) (tyenv'valenv valenv'empty)
          in
              linkUni (map (fn (vid,tyopt) => 
                               (case tyopt
                                 of Some ty => T TV (tyenv,ty) >>> (fn tor' => tor'arrow (tor',tor))
                                  | None => resultUni tor) 
                               >>> (fn tor => valenv'single (vid,(tscheme'cons uts tor,Notyet_idstatus))))
                           clauses)
                      >>> valenv'plusList >>> (fn valenv => (tyenv'valenv valenv, valenv))
          end
        | _ => notYet "datbind/2+"        


  (*fun tscheme'closeTor tor = tscheme'close empty'sub [] [] tor*)

  fun makeTypefun tname N =
          typefun'fn (fn tors =>
                         let val uts = map genUt (copy N ())
                         in if (length tors <> length uts)
                            then Err (sof'tname tname ^ "/arity")
                            else let val sub = sub'listPairs (zipPair uts tors)
                                     val tor = tor'cons (tname,map TOR'Ut uts)
                                 in Okay (refineTor sub tor)
                                 end
                         end)


    | TOR'Bool
    | TOR'Int
    | TOR'List  of tor

  val tor'bool  = TOR'Bool
  val tor'int   = TOR'Int
  val tor'list  = TOR'List


       | TOR'Bool       => "bool"
       | TOR'Int        => "int"
       | TOR'List _     => "list"

                         | TOR'Bool             => TOR'Bool
                         | TOR'Int              => TOR'Int
                         | TOR'List tor         => TOR'List (TOR tor)

                         | TOR'Bool             => false
                         | TOR'Int              => false
                         | TOR'List tor         => TOR tor

               | TOR'Bool               => Word "bool"
               | TOR'Int                => Word "int"
               | TOR'List tor           => TOR true tor ++ Word "list"

              | TOR'Bool                => acc
              | TOR'Int                 => acc
              | TOR'List tor            => collect tor acc

            | (TOR'Bool,TOR'Bool)       => resultUni ()
            | (TOR'Int,TOR'Int)         => resultUni ()
            | (TOR'List t1, TOR'List t2)=> unifyTT (t1,t2)

(*----------------------------------------------------------------------
INDEX: Thu Jun 14 10:13:59 2007
----------------------------------------------------------------------*)

                                           (*val scoped = X -- (Y -- tyvars)*) (*NO*)
                                           val scoped = tyvars && (X -- Y) 
                                           val () = see (Word "vb =" @@ Pretty.lay'dec_noIM (DEC'Val (tyvars,vb)))
                                           val () = see (Word "tyvars =" @@ (layCommaSep o map (Word o sof'rigid)) tyvars)
                                           val () = see (Word "X =" @@ (layCommaSep o map (Word o sof'rigid)) X)
                                           val () = see (Word "Y =" @@ (layCommaSep o map (Word o sof'rigid)) Y)
                                           val () = see (Word "scoped =" @@ (layCommaSep o map (Word o sof'rigid)) scoped)


(*----------------------------------------------------------------------
INDEX: Fri Jun 15 14:29:32 2007
----------------------------------------------------------------------*)
  val tname'int     = genTname (TYCON (ID "int"))
  val tname'char    = genTname (TYCON (ID "char"))
  val tname'string  = genTname (TYCON (ID "string"))
  val tname'bool    = genTname (TYCON (ID "bool"))
  val tname'list    = genTname (TYCON (ID "list"))
  val tname'ref     = genTname (TYCON (ID "ref"))

  val tor'int       = TOR'Cons (tname'int,[])
  val tor'char      = TOR'Cons (tname'char,[])
  val tor'string    = TOR'Cons (tname'string,[])
  val tor'bool      = TOR'Cons (tname'bool,[])
  val tor'list      = fn t => TOR'Cons (tname'list,[t])
  val tor'ref       = fn t => TOR'Cons (tname'ref,[t])



(*----------------------------------------------------------------------
INDEX: Fri Jun 15 15:07:43 2007
----------------------------------------------------------------------*)

  val valenv'initial =
      valenv'plusList
      [
       let val a = genUt() val b = genUt()
       in valenv'single(VID (ID "::"),
                        (tscheme'cons [a,b] (tor'arrow (tor'row [(LAB'Num 1,tor'ut a),(LAB'Num 2,tor'list (tor'ut a))], 
                                                        tor'list (tor'ut a))),
                         IDS'C))
       end,
       valenv'single(VID (ID "*"),
                     (tscheme'cons [] (tor'arrow (tor'row [(LAB'Num 1,tor'int),(LAB'Num 2,tor'int)], 
                                                  tor'int)),
                      IDS'V)),
       valenv'single(VID (ID "+"),
                     (tscheme'cons [] (tor'arrow (tor'row [(LAB'Num 1,tor'int),(LAB'Num 2,tor'int)], 
                                                  tor'int)),
                      IDS'V)),
       let val a = genUt()
       in valenv'single(VID (ID "ref"),
                        (tscheme'cons [a] (tor'arrow (tor'ut a, tor'ref (tor'ut a))),
                         IDS'C))
       end,
       valenv'single(VID (ID "false"), (tscheme'cons [] tor'bool, IDS'C)),
       valenv'single(VID (ID "true"), (tscheme'cons [] tor'bool, IDS'C)),
       let val a = genUt() (* SHOULD BE EQVAR *)
       in valenv'single(VID (ID "="),
                        (tscheme'cons [a] (tor'arrow (tor'row [(LAB'Num 1,tor'ut a),(LAB'Num 2,tor'ut a)], 
                                                        tor'bool)),
                         IDS'C))
       end,
       valenv'single(VID (ID "implode"),
                     (tscheme'cons [] (tor'arrow (tor'list tor'char, tor'string)),
                      IDS'V)),
       valenv'single(VID (ID "explode"),
                     (tscheme'cons [] (tor'arrow (tor'string, tor'list tor'char)),
                      IDS'V))
       ]

(*
see (lay'valenv valenv'initial)
*)


  fun tc'patrow (P:P) (context,PATROW (xs,{wildcard=Some pos})) = notYet ("tc'patrow/wildcard: "^Pos.sofPos pos)
    | tc'patrow (P:P) (context,PATROW (xs,{wildcard=None})) =
      (fold (fn acc =>
                fn (lab,pat) => 
                   acc >> (fn (ys,valenv1) => 
                              P (context,pat) >>> (fn (tor,valenv2) =>
                                                      ((lab,tor)::ys, valenv'plus valenv1 valenv2))))
            (resultUni ([],valenv'empty)) xs
       ) >>> (fn (ys,c) => (tor'row (rev ys),c))



(*----------------------------------------------------------------------
INDEX: sig/spec (for loadSgi)
----------------------------------------------------------------------*)
local
  fun tscheme'ty (context,ty) =
      case (execUni (tc'ty (Okay o TOR'Rigid) (tyenv'context context, ty)
                           >>> tscheme'close empty'sub (tyvars'ty ty) []))
       of Err s => abort ("tscheme'ty - "^s)
        | Okay x => x
  fun U1_tc'sigexp SPEC sigexp =
      case sigexp
       of SIG'Spec spec         => SPEC spec
        | SIG'Id sigid          => notYet "sig/id" 
        | SIG'TypeRealisation _ => notYet "sig/TypeRealisation" 
  fun U1_tc'spec TY SPEC spec =
      case spec 
       of SPEC'Val xs           => env'valenv (valenv'plusList (map (fn (vid,ty) =>
                                                                        valenv'single (vid,(TY ty, IDS'V)))
                                                                    xs))
        | SPEC'Type xs          => notYet "spec/type"
        | SPEC'Eqtype xs        => notYet "spec/eqtype"
        | SPEC'Datatype xs      => notYet "spec/datatype"
        | SPEC'DataReplicate x  => notYet "spec/data-rep"
        | SPEC'Exception xs     => notYet "spec/exception"
        | SPEC'Structure xs     => notYet "spec/structure"
        | SPEC'Include sigexp   => notYet "spec/include" 
        | SPEC'Empty            => env'empty
        | SPEC'Seq (spec1,spec2)=> env'plus (SPEC spec1) (SPEC spec2)
        | SPEC'Sharing (spec,_) => notYet "spec/sharing" 
in
  local fun TY context x = tscheme'ty (context,x)
        fun SPEC context x = U1_tc'spec (TY context) (SPEC context) x
        fun SIG context x  = U1_tc'sigexp (SPEC context) x
  in val tc'sigexp = SIG
  end
end
(*----------------------------------------------------------------------
INDEX: loadSig
----------------------------------------------------------------------*)
  fun getFirstSig'sigdec x =
      case x 
       of SIGDEC ((sigid,sigexp)::_) => Some sigexp
        | _ => None
  fun getFirstSig'td td =
      case td 
       of TOPDEC'Exp exp    => None
        | TOPDEC'Strdec x   => None
        | TOPDEC'Sigdec x   => getFirstSig'sigdec x
        | TOPDEC'Fundec x   => None
  fun getFirstSig'tds tds =
      case tds
       of []                => None
        | td::tds           => case (getFirstSig'td td) of None => getFirstSig'tds tds | s => s
  fun getSig tds =
      case (getFirstSig'tds tds)
       of None => abort "Signature Not Found"
        | Some x => x
  fun loadSig {path} = parseThen getSig (fn () => Run.tpFile {path=path})


  (*fun seeTheBasis() = see (lay'basis (!THE_basis))*)

(*
       ("bool"  , fn []  => Okay tor'bool       | _ => Err "bool/arity"),
       ("list"  , fn [t] => Okay (tor'list t)   | _ => Err "list/arity"),
       ("ref"   , fn [t] => Okay (tor'ref t)    | _ => Err "ref/arity")
*)
