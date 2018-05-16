(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  22.	 export
  36.	 imports
  237.	 functor
  255.	 core -- combinations
  273.	 id, scon
  312.	 ty
  340.	 pat
  384.	 exp
  449.	 dec
  553.	 fix -- dec/pat/exp
  561.	 module level
  568.	 sig/spec
  619.	 strexp/strdec
  650.	 sigdec/fundec
  677.	 topdec
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature PrettySig =
	sig
		type layout
		type infixMap
		type scon
		type exp
		type dec
		type topdec
		type match
		val lay'scon : scon -> layout
		val lay'exp_noIM : exp -> layout
		val lay'dec_noIM : dec -> layout
		val lay'topdecs : infixMap -> topdec list -> layout
		val sof'match : match -> string
	end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ~/project/prelude/PREL.ML *)
signature PrelSig_Import =
sig
	val abort			: string -> 'a
	val sofChar			: char -> string
	val sofStringLit	: string -> string
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
  val show_layout               : layout -> string
  val //                        : layout * layout -> layout
  val @@                        : layout * layout -> layout
  val ++                        : layout * layout -> layout
  val ``                        : layout * layout -> layout
  val //``                      : layout * layout -> layout
  val layWord                   : word -> layout
  val layInt                    : int -> layout
  val layBracket                : layout -> layout
  val layBrace                  : layout -> layout
  val laySqBracket              : layout -> layout
  val layHashSqBracket          : layout -> layout
  val layListSep                : layout -> layout list -> layout
  val layCommaSep               : layout list -> layout
  val laySequence               : layout list -> layout
  val layVert                   : layout list -> layout
end;
(*IMPORTDONE:*)

(*IMPORTSIG: TokSig TOK.ML *)
signature TokSig_Import =
sig
	val sofChar : char -> string
	val sofStringLit : string -> string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AtomSig ATOM.ML *)
signature AtomSig_Import =
sig
	datatype id = ID of string
	datatype vid = VID of id
	datatype tyvar = TYVAR of id
	datatype tycon = TYCON of id
	datatype strid = STRID of id
	datatype sigid = SIGID of id
	datatype funid = FUNID of id
	datatype 'a long = LONG of strid list * 'a
	datatype lab = LAB'Id of id | LAB'Num of int
	datatype scon =
		SCON'Num of int
	  | SCON'Word of word
	  | SCON'Real of real 
	  | SCON'Char of char
	  | SCON'String of string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: LangSig LANG.ML *)
signature LangSig_Import =
sig
	type vid
	type tyvar
	type tycon
	type strid
	type sigid
	type funid
	type 'a long
	type lab
	type scon
	type pos
	datatype tyrow = TYROW of (lab * ty) list
	and ty =
		TY'Var of tyvar
	  | TY'Record of tyrow
	  | TY'Con of ty list * tycon long
	  | TY'Tuple of ty list
	  | TY'Func of ty * ty
	datatype patrow = PATROW of (lab * pat) list * {wildcard: pos option}
	and pat =
		PAT'Wild
	  | PAT'Scon of scon
	  | PAT'Var of vid long
	  | PAT'Record of patrow
	  | PAT'Unit
	  | PAT'Tuple of pat list
	  | PAT'List of pat list
	  | PAT'Cons of vid long * pat
	  | PAT'Typed of pat * ty
	  | PAT'Layered of vid * ty option * pat
	datatype typbind =
		TYPBIND of (tyvar list * tycon * ty) list
	datatype datbind =
		DATBIND of (tyvar list * tycon * (vid * ty option) list) list
	datatype exbind1 =
		EXBIND1'New of vid * ty option
	  | EXBIND1'Copy of vid * vid long
	datatype exbind = EXBIND of exbind1 list
	datatype valbind =
		VALBIND'Seq of pat * exp * valbind option
	  | VALBIND'Rec of valbind
	and fvalbind = FVALBIND of (pos * vid * (pat list * ty option * exp) list) list
	and dec =
		DEC'Val of tyvar list * valbind
	  | DEC'Fval of fvalbind
	  | DEC'Type of typbind 
	  | DEC'Datatype of datbind * typbind option
	  | DEC'DataReplicate of tycon * tycon long
	  | DEC'Abstype of datbind * typbind option * dec
	  | DEC'Exception of exbind
	  | DEC'Local of dec * dec
	  | DEC'Open of strid long list
	  | DEC'Empty
	  | DEC'Seq of dec * dec
	and mrule = MRULE of pat * exp
	and match = MATCH of pos * mrule list
	and exprow = EXPROW of (lab * exp) list
	and exp =
		EXP'Scon of scon
	  | EXP'Var of vid long * pos
	  | EXP'Record of exprow
	  | EXP'Select of pos * lab
	  | EXP'Unit
	  | EXP'Tuple of exp list
	  | EXP'List of exp list
	  | EXP'Vector of exp list
	  | EXP'Seq of exp list
	  | EXP'Let of dec * exp
	  | EXP'App of exp * exp
	  | EXP'Typed of exp * ty
	  | EXP'Andalso of exp * exp
	  | EXP'Orelse of exp * exp
	  | EXP'Handle of exp * match
	  | EXP'Raise of exp
	  | EXP'If of exp * exp * exp
	  | EXP'While of exp * exp
	  | EXP'Case of exp * match
	  | EXP'Fn of match
	datatype sigexp =
		SIG'Spec of spec
	  | SIG'Id of sigid
	  | SIG'TypeRealisation of sigexp * (tyvar list * tycon long * ty)
	and spec =
		SPEC'Val of (vid * ty) list
	  | SPEC'Type of (tyvar list * tycon * ty option) list
	  | SPEC'Eqtype of (tyvar list * tycon) list
	  | SPEC'Datatype of (tyvar list * tycon * (vid * ty option) list) list
	  | SPEC'DataReplicate of tycon * tycon long
	  | SPEC'Exception of (vid * ty option) list
	  | SPEC'Structure of (strid * sigexp) list
	  | SPEC'Include of sigexp
	  | SPEC'Empty
	  | SPEC'Seq of spec * spec
	  | SPEC'Sharing of spec * tycon long list
	datatype strexp =
		STREXP'Struct of strdec
	  | STREXP'Id of strid long
	  | STREXP'TransCon of strexp * sigexp
	  | STREXP'OpaqueCon of strexp * sigexp
	  | STREXP'FunctorApp of funid * strexp
	  | STREXP'Let of strdec * strexp
	and strdec =
		STRDEC'Dec of dec
	  | STRDEC'Structure of (strid * strexp) list
	  | STRDEC'Local of strdec * strdec
	  | STRDEC'Empty
	  | STRDEC'Seq of strdec * strdec
	datatype sigdec = SIGDEC of (sigid * sigexp) list
	datatype funimport =
		FUNIMPORT'Basic of strid * sigexp
	  | FUNIMPORT'Derived of spec
	datatype fundec = FUNDEC of (funid * funimport * strexp) list
	datatype topdec =
		TOPDEC'Exp of exp
	  | TOPDEC'Strdec of strdec
	  | TOPDEC'Sigdec of sigdec
	  | TOPDEC'Fundec of fundec
end;
(*IMPORTDONE:*)

(*IMPORTSIG: ParserSig PARSER.ML *)
signature ParserSig_Import =
sig
	datatype id = ID of string
	datatype vid = VID of id
    type pa;
	type infixMap
	val makeIM : (vid * pa) list -> infixMap
	val isInfix : infixMap -> vid -> bool
	type topdec
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor PRETTY(structure Prel : PrelSig_Import
			   structure Misclay : MisclaySig_Import
			   structure Tok : TokSig_Import
			   structure Atom : AtomSig_Import
			   structure Lang : LangSig_Import
			   structure Parser : ParserSig_Import
			   sharing type Atom.id = Parser.id

			   sharing type Atom.lab = Lang.lab
			   sharing type Atom.scon = Lang.scon
			   sharing type Atom.long = Lang.long
			   sharing type Atom.vid = Lang.vid = Parser.vid
			   sharing type Atom.tyvar = Lang.tyvar
			   sharing type Atom.tycon = Lang.tycon
			   sharing type Atom.strid = Lang.strid
			   sharing type Atom.funid = Lang.funid
			   sharing type Atom.sigid = Lang.sigid

			   sharing type Lang.topdec = Parser.topdec
			   ) : PrettySig =
struct

  infix // @@ ++ `` //``
  open Prel Misclay Tok Atom Lang Parser

(*----------------------------------------------------------------------
INDEX: core -- combinations
----------------------------------------------------------------------*)

  fun lay'andList tag X xs =
	  case xs of
		  [] => abort "lay'andList[]"
		| x::xs => layVert ((tag ++ X x) :: map (fn x => Word "and" ++ X x) xs)

  fun needAtomic isAtomic layAtomic x =
	  (if (isAtomic x) then (fn x => x) else layBracket)
	  (layAtomic x)

	  
  val laySemiSep = layListSep (Left ";")
	  
  fun notYet s = Left "<..." @@ Word s @@ Right "...>"

(*----------------------------------------------------------------------
INDEX: id, scon
----------------------------------------------------------------------*)

  fun lay'id (ID x) = Word (case x of "*" => "* " | _ => x)
  fun noOp_lay'vid (VID id) = lay'id id
					  
  fun lay'vid im vid =
	  (if (isInfix im vid) then Word "op" else Blank) ++ noOp_lay'vid vid

  fun lay'tyvar (TYVAR id) = Left "'" @@ lay'id id
  fun lay'tycon (TYCON id) = lay'id id
  fun lay'strid (STRID id) = lay'id id

  fun lay'lab lab =
	  case lab of
		  LAB'Id id => lay'id id
		| LAB'Num n => layInt n

  fun lay'long X Xshort long =
	  case long of
		  LONG([],x) => Xshort x
		| LONG(strids,x) =>
			  laySequence (map (fn strid => lay'strid strid ++ Sep "."
								) strids)
			  ++ X x

  fun lay'longvid im = lay'long noOp_lay'vid (lay'vid im)
  val lay'longtycon = lay'long lay'tycon lay'tycon
  val lay'longstrid = lay'long lay'strid lay'strid

  fun lay'scon scon =
	  case scon of
		  SCON'Num n => layInt n
		| SCON'Word w => layWord w
		| SCON'Real r => notYet "real"
		| SCON'Char c => Word (sofChar c)
		| SCON'String s => Word (sofStringLit s)

(*----------------------------------------------------------------------
INDEX: ty
----------------------------------------------------------------------*)

  fun lay'seq X xs =
	  case xs of
		  [] => Blank
		| [x] => X x
		| xs => layBracket (layCommaSep (map X xs))

  val lay'tyvarseq = lay'seq lay'tyvar

  fun lay'tyrow T tyrow =
	  case tyrow of
		  TYROW xs =>
			  layBrace(layCommaSep
					   (map (fn (lab,ty) =>
							 lay'lab lab ++ Word ":" ++ T ty
							 ) xs))

  fun lay'ty ty =
	  case ty of
		  TY'Var tv => lay'tyvar tv
		| TY'Record tyrow => lay'tyrow lay'ty tyrow
		| TY'Con (tys,ltc) => lay'seq lay'ty tys ++ lay'longtycon ltc
		| TY'Tuple tys => layBracket (layListSep (Word "*") (map lay'ty tys))
		| TY'Func (ty1,ty2) => layBracket (lay'ty ty1 ++ Word "->" ++ lay'ty ty2)

(*----------------------------------------------------------------------
INDEX: pat
----------------------------------------------------------------------*)

  fun lay'patrow P patrow =
	  case patrow of
		  PATROW (lps,{wildcard=w}) =>
			  layBrace(layCommaSep
					   (map (fn (lab,pat) =>
							 lay'lab lab ++ Word "=" ++ P pat
							 ) lps
						) ++
					   (case w of Some _ => Word ",..." | None => Blank))


  fun isAtomicPat pat =
	  case pat of
		  PAT'Wild => true
		| PAT'Scon _ => true
		| PAT'Var _ => true
		| PAT'Record _ => true
		| PAT'Unit => true
		| PAT'Tuple _ => true
		| PAT'List _ => true
		|  _ => false

  val atomicPat = needAtomic isAtomicPat

  fun lay'patP im P pat =
	  case pat of
		  PAT'Wild => Word "_"
		| PAT'Scon scon => lay'scon scon
		| PAT'Var x => lay'longvid im x
		| PAT'Record pr => lay'patrow P pr
		| PAT'Unit => Word "()"
		| PAT'Tuple pats => layBracket (layCommaSep (map P pats))
		| PAT'List pats => laySqBracket (layCommaSep (map P pats))
		| PAT'Cons (x,pat) => lay'longvid im x ++ atomicPat P pat
		| PAT'Typed (pat,ty) => P pat ++ Word ":" ++ lay'ty ty
		| PAT'Layered (vid,tyo,pat) =>
			  lay'vid im vid
			  ++ (case tyo of Some ty => Word ":" ++ lay'ty ty | None => Blank)
			  ++ Word "as" ++ P pat

(*----------------------------------------------------------------------
INDEX: exp
----------------------------------------------------------------------*)

  fun lay'mrule P E (MRULE (pat,exp)) = P pat ++ Word "=>" `` E exp

(* best for fn,handle... *)		 
  fun lay'match P E tag (MATCH (_,[])) = abort "MATCH[]"
	| lay'match P E tag (MATCH (_,x::xs)) =
	  tag @@ lay'mrule P E x
	  // layVert (map (fn x => Word "|" @@ lay'mrule P E x
					   ) xs)

  fun lay'match_forCase P E tag (MATCH (_,[])) = abort "MATCH[]"
	| lay'match_forCase P E tag (MATCH (_,x::xs)) =
	  tag //`` lay'mrule P E x
	  // layVert (map (fn x => Word "|" @@ lay'mrule P E x
					   ) xs)

  fun lay'exprow E (EXPROW les) =
	  layBrace
	  (layCommaSep (map (fn (lab,exp) => lay'lab lab ++ Word "=" ++ E exp
						 ) les))

  fun isAtomicExp exp =
	  case exp of
		  EXP'Scon _ => true
		| EXP'Var _ => true
		| EXP'Record _ => true
		| EXP'Select _ => true
		| EXP'Unit => true
		| EXP'Tuple _ => true
		| EXP'List _ => true
		| EXP'Vector _ => true
		| EXP'Seq _ => true
		| EXP'Let _ => true
		|  _ => false

  val atomicExp = needAtomic isAtomicExp

  fun lay'expDEP im D E P exp =
	  case exp of
		EXP'Scon scon => lay'scon scon
	  | EXP'Var (x,pos) => lay'longvid im x
	  | EXP'Record exprow => lay'exprow E exprow
	  | EXP'Select (_,lab) => Word "#" ++ lay'lab lab
	  | EXP'Unit => Word "()"
	  | EXP'Tuple exps => layBracket (layCommaSep (map E exps))
	  | EXP'List exps => laySqBracket (layCommaSep (map E exps))
	  | EXP'Vector exps => layHashSqBracket (layCommaSep (map E exps))
	  | EXP'Seq exps => layBracket (laySemiSep (map E exps))
	  | EXP'Let (d,e) => (Word "let" @@ D d) // (Word "in" @@ E e) // Word "end"
	  | EXP'App (e1,e2) => atomicExp E e1 ++ atomicExp E e2
	  | EXP'Typed (exp,ty) => atomicExp E exp ++ Word ":" ++ lay'ty ty
	  | EXP'Andalso (e1,e2) => atomicExp E e1 ++ Word "andalso" ++ atomicExp E e2
	  | EXP'Orelse (e1,e2) => atomicExp E e1 ++ Word "orelse" ++ atomicExp E e2
	  | EXP'Handle (exp,match) => E exp ++ lay'match P E (Word "handle") match
	  | EXP'Raise exp => Word "raise" ++ E exp
	  | EXP'If (e1,e2,e3) =>
			(Word "if" ++ E e1) ++ (Word "then" ++ E e2) ++ (Word "else" ++ E e3)
	  | EXP'While (e1,e2) => Word "while" ++ E e1 ++ Word "do" ++ E e2
	  | EXP'Case (exp,match) =>
			lay'match_forCase P (atomicExp E) (Word "case" ++ E exp ++ Word "of") match
	  | EXP'Fn match => lay'match P E (Word "fn") match

(*----------------------------------------------------------------------
INDEX: dec
----------------------------------------------------------------------*)
			   
  fun lay'valbind P E vb =
	  case vb of
		  VALBIND'Seq (p,e,vbo) =>
			  P p ++ Word "=" ++ E e ++ 
			  (case vbo of Some vb => Word "and" ++ lay'valbind P E vb | None => Blank)
		| VALBIND'Rec vb => Word "rec" ++ lay'valbind P E vb


  fun lay'fvalbind im P E (FVALBIND []) = abort "FVALBIND[]"
	| lay'fvalbind im P E (FVALBIND (f::fs)) =
	  let
		  fun lay'clause tag (pats,tyo,exp) =
			  tag
			  ++ laySequence (map (atomicPat P) pats)
			  ++ (case tyo of Some ty => Word ":" ++ lay'ty ty | None => Blank)
			  ++ Word "="
			  `` E exp
		  fun lay'one tag (pos,vid,[]) = abort "lay'one[]"
			| lay'one tag (pos,vid,c::cs) =
			  lay'clause (tag ++ lay'vid im vid) c
			  // layVert (map (fn c => lay'clause (Word "  |" ++
												   lay'vid im vid) c
							   ) cs)
	  in
		  lay'one (Word "fun") f
		  // layVert (map (fn f => lay'one (Word "and") f) fs)
	  end

  fun lay'exbind1 im b =
	  case b of
		  EXBIND1'New (x,tyo) =>
			  lay'vid im x ++ (case tyo of
								   Some ty => Word "of" ++ lay'ty ty
								 | None => Blank)
		| EXBIND1'Copy (x,y) =>
			  lay'vid im x ++ Word "=" ++ lay'longvid im y

  fun lay'exbind im x =
	  case x of
		  EXBIND [] => abort "EXBIND[]"
		| EXBIND (b::bs) =>
			  layVert ((Word "exception" ++ lay'exbind1 im b)
					   :: map (fn b => Word "and" ++ lay'exbind1 im b) bs)

  fun lay'typbind x =
	  case x of
		  TYPBIND xs =>
			  lay'andList (Word "type")
			  (fn (tvs,tc,ty) =>
			   lay'tyvarseq tvs ++ lay'tycon tc ++ Word "=" ++ lay'ty ty
			   ) xs

  fun lay'vbarList X xs =
	  case xs of
		  [] => abort "lay'vbarList[]"
		| x::xs => laySequence (X x :: map (fn x => Word "|" ++ X x) xs)

  fun lay'datbind im tag x =
	  case x of
		  DATBIND xs =>
			  lay'andList tag
			  (fn (tvs,tc,clauses) =>
			   lay'tyvarseq tvs ++ lay'tycon tc ++ Word "=" ++
			   lay'vbarList (fn (vid,tyo) =>
							 (lay'vid im vid ++
							  (case tyo of
								   None => Blank
								 | Some ty => Word "of" ++ lay'ty ty))
							 ) clauses
			   ) xs


  fun lay'optWithType tbo =
	  case tbo of None => Blank | Some tb => Word "with" ++ lay'typbind tb

  fun lay'dataReplicate (tc,tclong) =
	  Word "datatype" ++ lay'tycon tc
	  ++ Word "=" ++ (Word "datatype" ++ lay'longtycon tclong)
			  
  fun lay'decDEP im D E P dec =
	  case dec of
		  DEC'Val (tvs,vb) => Word "val" ++ lay'tyvarseq tvs ++ lay'valbind P E vb
		| DEC'Fval fb => lay'fvalbind im P E fb
		| DEC'Type typbind => lay'typbind typbind 
		| DEC'Datatype (db,tbo) =>
			  lay'datbind im (Word "datatype") db ++ lay'optWithType tbo
		| DEC'DataReplicate x => lay'dataReplicate x
		| DEC'Abstype (db,tbo,dec) =>
			  lay'datbind im (Word "abstype") db ++ lay'optWithType tbo
			  // (Word "with" `` D dec)
			  // Word "end"
		| DEC'Exception exbind => lay'exbind im exbind
		| DEC'Local (dec1,dec2) =>
			  (Word "local" `` D dec1)
			  // (Word "in" `` D dec2)
			  // Word "end"
		| DEC'Open xs => Word "open" ++ laySequence (map (lay'longstrid) xs)
		| DEC'Empty => Blank
		| DEC'Seq (d1,d2) => D d1 // D d2

(*----------------------------------------------------------------------
INDEX: fix -- dec/pat/exp
----------------------------------------------------------------------*)

  fun lay'dec im x = lay'decDEP im (lay'dec im) (lay'exp im) (lay'pat im) x
  and lay'pat im x = lay'patP im (lay'pat im) x
  and lay'exp im x = lay'expDEP im (lay'dec im) (lay'exp im) (lay'pat im) x


  fun sof'match match = 
	  let val im = makeIM []
		  val P = lay'pat im
		  val E = lay'exp im
		  val tag = Blank
	  in show_layout (lay'match P E tag match)
	  end

  val lay'dec_noIM = lay'dec (makeIM [])
  val lay'exp_noIM = lay'exp (makeIM [])


(*----------------------------------------------------------------------
INDEX: module level
----------------------------------------------------------------------*)

  fun lay'sigid (SIGID id) = lay'id id
  fun lay'funid (FUNID id) = lay'id id

(*----------------------------------------------------------------------
INDEX: sig/spec
----------------------------------------------------------------------*)

  fun lay'sigexp_U SP SIG sigexp =
	  case sigexp of
		  SIG'Spec spec => Word "sig" `` SP spec // Word "end"
		| SIG'Id sigid => lay'sigid sigid
		| SIG'TypeRealisation (sigexp,(tvs,ltc,ty)) =>
			  SIG sigexp ++
			  Word "where" ++
			  ((Word "type" ++ lay'tyvarseq tvs ++ lay'longtycon ltc)
			   ++ Word "=" ++ lay'ty ty)
	  
  fun lay'spec_U im SP SIG spec =
	  case spec of 
		  SPEC'Val xs =>
			  lay'andList (Word "val")
			  (fn (vid,ty) => noOp_lay'vid vid ++ Word ":" ++ lay'ty ty
			   ) xs
		| SPEC'Type xs =>
			  lay'andList (Word "type")
			  (fn (tvs,tc,tyo) => 
				  lay'tyvarseq tvs ++ lay'tycon tc ++
				  (case tyo of None => Blank | Some ty => Word "=" ++ lay'ty ty)
			   ) xs
		| SPEC'Eqtype xs => 
			  lay'andList (Word "eqtype")
			  (fn (tvs,tc) => lay'tyvarseq tvs ++ lay'tycon tc
			   ) xs
		| SPEC'Datatype x => (* x ought to already be wrapped with DATBIND *)
			  lay'datbind im (Word "datatype") (DATBIND x)
		| SPEC'DataReplicate x => lay'dataReplicate x
		| SPEC'Exception xs =>
			  lay'andList (Word "exception")
			  (fn (vid,tyo) =>
			   (lay'vid im vid ++
				(case tyo of None => Blank | Some ty => Word "of" ++ lay'ty ty))
			   ) xs
		| SPEC'Structure xs =>
			  lay'andList (Word "structure")
			  (fn (x,sigexp) => lay'strid x ++ Word ":" ++ SIG sigexp
			   ) xs
		| SPEC'Include sigexp => Word "include" ++ SIG sigexp
		| SPEC'Empty => Blank
		| SPEC'Seq (spec1,spec2) => SP spec1 // SP spec2
		| SPEC'Sharing (spec,xs) =>
			  SP spec ++ Word "sharing" ++ Word "type" ++
			  layListSep (Word "=") (map lay'longtycon xs)

  fun lay'sigexp im x = lay'sigexp_U (lay'spec im) (lay'sigexp im) x
  and lay'spec im x = lay'spec_U im (lay'spec im) (lay'sigexp im) x

(*----------------------------------------------------------------------
INDEX: strexp/strdec
----------------------------------------------------------------------*)

  fun lay'strexp_U SE SD SIG x =
	  case x of
		  STREXP'Struct strdec => Word "struct" // SD strdec // Word "end"
		| STREXP'Id stridlong => lay'longstrid stridlong
		| STREXP'TransCon (strexp,sigexp) => SE strexp ++ Word ":" ++ SIG sigexp
		| STREXP'OpaqueCon (strexp,sigexp) => SE strexp ++ Word ":>" ++ SIG sigexp
		| STREXP'FunctorApp (funid,strexp) => lay'funid funid ++ layBracket (SE strexp)
		| STREXP'Let (strdec,strexp) =>
			  (Word "let" @@ SD strdec) // (Word "in" @@ SE strexp) // Word "end"
	  
  fun lay'strdec_U D SE SD x =
	  case x of
		  STRDEC'Dec d => D d
		| STRDEC'Structure xs =>
			  lay'andList (Word "structure")
			  (fn (x,se) => lay'strid x ++ Word "=" ++ SE se
			   ) xs
		| STRDEC'Local (sd1,sd2) =>
			  (Word "local" `` SD sd1)
			  // (Word "in" `` SD sd2)
			  // Word "end"
		| STRDEC'Empty => Blank
		| STRDEC'Seq (sd1,sd2) => SD sd1 // SD sd2
	
  fun lay'strexp im x = lay'strexp_U (lay'strexp im) (lay'strdec im) (lay'sigexp im) x
  and lay'strdec im x = lay'strdec_U (lay'dec im) (lay'strexp im) (lay'strdec im) x

(*----------------------------------------------------------------------
INDEX: sigdec/fundec
----------------------------------------------------------------------*)

  fun lay'sigdec im x =
	  case x of
		  SIGDEC xs =>
			  lay'andList (Word "signature")
			  (fn (si,se) =>
			   lay'sigid si ++ Word "=" `` lay'sigexp im se
			   ) xs

  fun lay'funimport im x =
	  case x of
		  FUNIMPORT'Basic (strid,sigexp) =>
			  lay'strid strid ++ Word ":" ++ lay'sigexp im sigexp
		| FUNIMPORT'Derived spec => lay'spec im spec
		
  fun lay'fundec im x =
	  case x of
		  FUNDEC xs =>
			  lay'andList (Word "functor")
			  (fn (f,I,se) =>
			   lay'funid f ++ layBracket (lay'funimport im I) ++ Word "="
			   ++ lay'strexp im se
			   ) xs

(*----------------------------------------------------------------------
INDEX: topdec
----------------------------------------------------------------------*)
  
  fun lay'topdec im x =
	  case x of
		  TOPDEC'Exp e => Word "val it =" ++ lay'exp im e
		| TOPDEC'Strdec x => lay'strdec im x
		| TOPDEC'Sigdec x => lay'sigdec im x
		| TOPDEC'Fundec x => lay'fundec im x

  fun lay'topdecs im xs = layVert (map (lay'topdec im) xs)

end (* functor *)
