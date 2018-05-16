(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  12.	 export
  156.	 imports
  185.	 functor
  268.	 module language
  317.	 non-prim builders
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature LangSig =
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

	datatype patrow = PATROW of (lab * pat) list * {wildcard: (*bool*)pos option}
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

	val pat'vid : vid -> pat
	val exp'pair : exp * exp -> exp
	val exp'vid : pos -> vid -> exp
	val makeBinPat : pat * vid * pat -> pat
	val makeDecSeq : dec list -> dec
	val makeItDec : exp -> dec
	val pat'pair : pat * pat -> pat

end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ../prelude/Prel.ML *)
signature PrelSig_Import =
sig
	val fold			: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
	val stringOfInt		: int -> string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: PosSig POS.ML *)
signature PosSig_Import =
sig
	type pos
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AtomSig ATOM.ML *)
signature AtomSig_Import =
sig
	datatype id = ID of string
	datatype vid = VID of id
	type tyvar;
	type tycon;
	type strid;
	type sigid;
	type funid;
	type 'a long;
	datatype lab = LAB'Id of id | LAB'Num of int
	type scon;
	val long'x : 'a -> 'a long
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor LANG(structure Prel : PrelSig_Import
			 structure Pos : PosSig_Import
			 structure Atom : AtomSig_Import) : LangSig =
struct

	open Prel Pos Atom

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

(*----------------------------------------------------------------------
INDEX: module language
----------------------------------------------------------------------*)

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

(*----------------------------------------------------------------------
INDEX: non-prim builders
----------------------------------------------------------------------*)

	fun pat'vid vid = PAT'Var (long'x vid)
	fun pat'pair (e1,e2) = PAT'Record (PATROW ([(LAB'Num 1, e1), (LAB'Num 2, e2)],
											   {wildcard=None}))
	fun exp'vid pos vid = EXP'Var (long'x vid,pos)
	fun exp'pair (e1,e2) = EXP'Record (EXPROW [(LAB'Num 1, e1), (LAB'Num 2, e2)])
	fun makeDecSeq [] = DEC'Empty
	  | makeDecSeq (d1::ds) = fold (fn acc => fn x => DEC'Seq(acc,x)) d1 ds

	fun makeItDec exp = DEC'Val ([],VALBIND'Seq (pat'vid (VID (ID "it")),exp,None))

	fun makeBinPat (x1,f,x2) = PAT'Cons (long'x f, pat'pair (x1,x2))

end;
