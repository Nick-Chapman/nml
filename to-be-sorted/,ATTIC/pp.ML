
local
    
infix @@    
infix ++
infix --
infix ---
infix //
infix //--

open Lang
open Tok
open Parser
    
fun echo s = print (s^"\n");
exception Abort of string
fun abort s = (echo s; raise Abort ("Abort: "^s))

    
val laySemiSep = layListSep (Left ";")
val layVbarSep = layListSep (Word "|")


fun notYet s = Left "<..." @@ Word s @@ Right "...>"

fun lay'id (ID x) = Word x

fun lay'vid im (vid as VID id) =
    (if (isInfix im vid) then Word "op" else Blank) ++ lay'id id

fun lay'tyvar (TYVAR id) = lay'id id
fun lay'tycon (TYCON id) = lay'id id
fun lay'strid (STRID id) = lay'id id

fun lay'lab lab =
    case lab of
	LAB'Id id => lay'id id
      | LAB'Num n => layInt n

fun lay'long X long =
    case long of
	LONG(strids,x) =>
	    laySequence (map (fn strid => lay'strid strid ++ Word "."
			      ) strids)
	    ++ X x
	    
fun lay'longvid im = lay'long (lay'vid im)

    
fun lay'scon scon =
    case scon of
	SCON'Num n => layInt n
      | SCON'Word n => notYet "word"
      | SCON'Real r => notYet "real"
      | SCON'Char c => notYet "char"
      | SCON'String s => Word (sofStringLit s)



fun lay'tyrow T tyrow =
    case tyrow of
	TYROW xs =>
	    layBrace(layCommaSep
		     (map (fn (lab,ty) =>
			   lay'lab lab ++ Word "=" ++ T ty
			   ) xs))
    
fun lay'ty ty =
    case ty of
	TY'Var tv => lay'tyvar tv
      | TY'Record tyrow => lay'tyrow lay'ty tyrow
      | TY'Con (tys,x) => layCommaSep (map lay'ty tys) ++ lay'long lay'tycon x
      | TY'Tuple tys => layBracket (layListSep (Word "*") (map lay'ty tys))
      | TY'Func (ty1,ty2) => layBracket (lay'ty ty1 ++ Word "->" ++ lay'ty ty2)

    
fun lay'patrow P patrow =
    case patrow of
	PATROW (lps,{wildcard=w}) =>
	    layBrace(layCommaSep
		     (map (fn (lab,pat) =>
			   lay'lab lab ++ Word "=" ++ P pat
			   ) lps
		      ) ++
		     (if w then Word ",..." else Blank))


	    
fun lay'patP_noBrac im P pat =
    case pat of
	PAT'Wild => Word "_"
      | PAT'Scon scon => lay'scon scon
      | PAT'Var x => lay'longvid im x
      | PAT'Record pr => lay'patrow P pr
      | PAT'Unit => Word "()"
      | PAT'Tuple pats => layBracket (layCommaSep (map P pats))
      | PAT'List pats => laySqBracket (layCommaSep (map P pats))
      | PAT'Cons (x,pat) => lay'longvid im x ++ layBracket (P pat)
      | PAT'Typed (pat,ty) => P pat ++ Word ":" ++ lay'ty ty
      | PAT'Layered (vid,tyo,pat) =>
	    lay'vid im vid
	    ++ (case tyo of Some ty => Word ":" ++ lay'ty ty | None => Blank)
	    ++ Word "as" ++ P pat

fun isSimplePat pat =
    case pat of
	PAT'Wild => true
      | PAT'Scon _ => true
      | PAT'Var _ => true
      | PAT'Record _ => true
      | PAT'Unit => true
      | PAT'Tuple _ => true
      | PAT'List _ => true
      |  _ => false
(*fun isSimplePat pat = false*)
fun lay'patP im P x =
    (if (isSimplePat x) then (fn x => x) else layBracket)
    (lay'patP_noBrac im P x)

    
fun lay'mrule P E (MRULE (pat,exp)) = P pat ++ Word "=>" ++ E exp
fun lay'match P E (MATCH mrules) = layVbarSep (map (lay'mrule P E) mrules)

fun lay'exprow E (EXPROW les) =
    layBrace
    (layCommaSep (map (fn (lab,exp) => lay'lab lab ++ Word "=" ++ E exp
		       ) les))

fun lay'expDEP_noBrac im D E P exp =
    case exp of
      EXP'Scon scon => lay'scon scon
    | EXP'Var x => lay'longvid im x
    | EXP'Record exprow => lay'exprow E exprow
    | EXP'Select lab => Word "#" ++ lay'lab lab
    | EXP'Unit => Word "()"
    | EXP'Tuple exps => layBracket (layCommaSep (map E exps))
    | EXP'List exps => laySqBracket (layCommaSep (map E exps))
    | EXP'Vector exps => notYet "EXP'Vector"
    | EXP'Seq exps => layBracket (laySemiSep (map E exps))
    | EXP'Let (d,e) => Word "let" ++ D d ++ Word "in" ++ E e ++ Word "end"
    | EXP'App (e1,e2) => E e1 ++ E e2
    | EXP'Typed (exp,ty) => E exp ++ Word ":" ++ lay'ty ty
    | EXP'Andalso (e1,e2) => E e1 ++ Word "andalso" ++ E e2
    | EXP'Orelse (e1,e2) => E e1 ++ Word "orelse" ++ E e2
    | EXP'Handle (exp,match) => E exp ++ Word "handle" ++ lay'match P E match
    | EXP'Raise exp => Word "raise" ++ E exp
    | EXP'If (e1,e2,e3) =>
	  (Word "if" ++ E e1) ++ (Word "then" ++ E e2) ++ (Word "else" ++ E e3)
    | EXP'While (e1,e2) => Word "while" ++ E e1 ++ Word "do" ++ E e2
    | EXP'Case (exp,match) =>
	  Word "case" ++ E exp ++ Word "of" ++ lay'match P E match
    | EXP'Fn match => Word "fn"
	  ++ lay'match P E match

fun isSimpleExp exp =
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
(*fun isSimpleExp exp = false*)
fun lay'expDEP im D E P x =
    (if (isSimpleExp x) then (fn x => x) else layBracket)
    (lay'expDEP_noBrac im D E P x)

      
fun lay'valbind P E vb =
    case vb of
	VALBIND'Seq (p,e,vbo) =>
	    P p ++ Word "=" ++ E e ++
	    (case vbo of Some vb => lay'valbind P E vb | None => Blank)
      | VALBIND'Rec vb => Word "rec" ++ lay'valbind P E vb


fun lay'fvalbind im P E (FVALBIND []) = abort "FVALBIND[]"
  | lay'fvalbind im P E (FVALBIND (f::fs)) =
    (*
     (vid * (pat list * ty option * exp) list) list
     *)
    let
	fun lay'clause (pats,tyo,exp) =
	    laySequence (map P pats)
	    ++ (case tyo of Some ty => Word ":" ++ lay'ty ty | None => Blank)
	    ++ Word "=" ++ E exp
	fun lay'one (vid,[]) = abort "lay'one[]"
	  | lay'one (vid,c::cs) =
	    (lay'vid im vid ++ lay'clause c)
	    // layVert (map (fn c => Word "|" ++ lay'vid im vid ++ lay'clause c
			     ) cs)
    in
	lay'one f
	// layVert (map (fn f => Word "and" ++ lay'one f) fs)
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


fun lay'decDEP im D E P dec =
    case dec of
	DEC'Val vb => Word "val" ++ lay'valbind P E vb
      | DEC'Fval fb => Word "fun" ++ lay'fvalbind im P E fb
(*
  | DEC'Type of typbind 
  | DEC'Datatype of datbind * typbind option
  | DEC'DataReplicate of tycon * tycon long
  | DEC'Abstype of datbind * typbind option * dec
  | DEC'Exception of exbind
  | DEC'Local of dec * dec
  | DEC'Open of strid long list
*)
  | DEC'Type typbind => notYet "DEC'Type"
  | DEC'Datatype (datbind,tbo) => notYet "DEC'Datatype"
  | DEC'DataReplicate (tc,tclong) => notYet "DEC'DataReplicate"
  | DEC'Abstype (db,tbo,dec) => notYet "DEC'Abstype"
  | DEC'Exception exbind => lay'exbind im exbind
  | DEC'Local (dec1,dec2) =>
	Word "local" ++ D dec1 ++
	Word "in" ++ D dec2 ++
	Word "end"
  | DEC'Open xs => Word "open" ++ laySequence (map (lay'long lay'strid) xs)
  | DEC'Empty => Blank
  | DEC'Seq (d1,d2) => D d1 ++ D d2

fun lay'dec im x = lay'decDEP im (lay'dec im) (lay'exp im) (lay'pat im) x
and lay'pat im x = lay'patP im (lay'pat im) x
and lay'exp im x = lay'expDEP im (lay'dec im) (lay'exp im) (lay'pat im) x
    
fun lay'strdec im x =
    case x of
	STRDEC'Dec d => lay'dec im d
      | STRDEC'Structure xs => notYet "STRDEC'Structure"
      | STRDEC'Local (sd1,sd2) => notYet "STRDEC'Local"
      | STRDEC'Empty => Blank
      | STRDEC'Seq (sd1,sd2) => lay'strdec im sd1 // lay'strdec im sd2
fun lay'sigdec (x:sigdec) = notYet "sigdec"
fun lay'topdec im x =
    case x of
	TOPDEC'Exp e => Word "val it =" ++ lay'exp im e
      | TOPDEC'Strdec x => lay'strdec im x
      | TOPDEC'Sigdec x => lay'sigdec x
val BlankLine = Sep ""

in

fun lay'top im x =
    layVert [BlankLine,
	     squash_to 50 (lay'topdec im x),
	     Word ";",
	     BlankLine]

end (* local open *)
