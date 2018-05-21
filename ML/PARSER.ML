(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  30.    export
  48.    imports
  316.   functor
  343.   parse combinators
  384.   infix mapping
  407.   fixMod
  434.   keywords, scon, tyvar, lab
  463.   match
  511.   identifiers
  523.   (new) long -- backtracking for strid-dot
  539.   keyword: op
  555.   types
  612.   patterns
  736.   expressions
  910.   declarations
  1101.  fixup
  1112.  sigid,funid
  1126.  spec
  1176.  sigexp
  1201.  strexp, strdec
  1254.  fixup
  1263.  sigdec, topdec
  1297.  prog
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature ParserSig =
sig
    datatype id = ID of string
    datatype vid = VID of id
    datatype leftOrRight = LR'Left | LR'Right
    datatype pa = PA of {assoc:leftOrRight, prec:int}
    type infixMap
    val makeIM : (vid * pa) list -> infixMap
    val isInfix : infixMap -> vid -> bool
    type topdec
    type 'a par
    val prog : infixMap -> topdec list par
end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ~/project/prelude/PREL.ML *)
signature PrelSig_Import =
sig
    val echo            : string -> unit
    val abort           : string -> 'a
    val link            : 'a list list -> 'a list
    val fold            : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AssocSig ~/project/prelude/ASSOC.ML *)
signature AssocSig_Import =
sig
    type ('a,'b) assoc
    val assoc'list      : ('a * 'b) list -> ('a,'b) assoc
    val extend'assoc    : ('a,'b) assoc -> 'a * 'b -> ('a,'b) assoc
    val delete'assoc    : (''a,'b) assoc -> ''a -> (''a,'b) assoc
    val lookup'assoc    : (''a,'b) assoc -> ''a -> 'b option
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
      | TOK'CharLit     of char
      | TOK'Err         of string
      | TOK'Ident       of string
      | TOK'Number      of int
      | TOK'StringLit   of string
      | TOK'Symid       of string
      | TOK'TyVar       of string
      | TOK'Unknown     of char
      | TOK'Word        of word
    val sofTok : tok -> string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: ParSig ~/project/prelude/PAR1.ML *)
signature ParSig_Import =
sig
    type 'a par
    val resultP     : 'a -> 'a par
    val failP       : 'a par
    val bindP       : 'a par * ('a -> 'b par) -> 'b par
    val altP        : 'a par * 'a par -> 'a par
    val fixP        : ('a par -> 'a par) -> 'a par
    val manyP       : 'a par -> 'a list par
    val noErrorP    : 'a par -> 'a par
    type pos
    val positionP   : pos par
    type tok
    val satisfyP    : string -> (tok -> 'a option) -> 'a par
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
    val pat'vid : vid -> pat
    val exp'pair : exp * exp -> exp
    val exp'vid : pos -> vid -> exp
    val makeBinPat : pat * vid * pat -> pat
    val pat'pair : pat * pat -> pat
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor PARSER(val prefixNML : string
               structure Prel : PrelSig_Import
               structure Assoc : AssocSig_Import
               structure Tok : TokSig_Import
               structure Par : ParSig_Import
               structure Atom : AtomSig_Import
               structure Lang : LangSig_Import
               sharing type Tok.tok = Par.tok
               sharing type Par.pos = Lang.pos
               sharing type Atom.lab = Lang.lab
               sharing type Atom.scon = Lang.scon
               sharing type Atom.long = Lang.long
               sharing type Atom.vid = Lang.vid
               sharing type Atom.tyvar = Lang.tyvar
               sharing type Atom.tycon = Lang.tycon
               sharing type Atom.strid = Lang.strid
               sharing type Atom.funid = Lang.funid
               sharing type Atom.sigid = Lang.sigid

                   ) (*: ParserSig*) =
struct

  open Prel Assoc Tok Par Atom Lang

  fun NMLecho s = echo (prefixNML^s)

(*----------------------------------------------------------------------
INDEX: parse combinators
----------------------------------------------------------------------*)

infix 1 >> >>> -$$ $$- **
infix 0 ##

val op >> = bindP
fun p >>> f = p >> (fn x => resultP (f x))
fun (p1 -$$ p2) = p1 >> (fn _ => p2)
fun (p1 $$- p2) = p1 >> (fn x => p2 -$$ resultP x)
fun (p1 ** p2) = p1 >> (fn x1 => p2 >>> (fn x2 => (x1,x2)))
val op ## = altP

fun opt p = (p >>> Some) ## resultP None
fun many1 p = p >> (fn x => manyP p >>> (fn xs => x::xs))

fun sequence p sep =
    p >>
    (fn x =>
     manyP (sep -$$ p) >>>
     (fn xs =>
      x :: xs))

fun sequence0 p sep = sequence p sep ## resultP []

fun left p1 p2 f =
    let fun acc x1 = resultP x1 ## (p2 >> (fn x2 => acc (f (x1,x2))))
    in p1 >> acc
    end

fun foldr1 f [] = abort "foldr1[]"
  | foldr1 f [x] = x
  | foldr1 f (x::xs) = f (x, foldr1 f xs)

fun right p1 p2 f =
    p1 >>
    (fn x1 =>
     manyP p2 >>>
     (fn xs => foldr1 f (x1::xs)))

(*----------------------------------------------------------------------
INDEX: infix mapping
----------------------------------------------------------------------*)

datatype leftOrRight = LR'Left | LR'Right

datatype pa = PA of {prec: int, assoc: leftOrRight}

abstype infixMap = IM of (vid,pa) assoc
with
    fun makeIM xs = IM (assoc'list xs)
    fun getIM (IM ass) x = lookup'assoc ass x
    fun infixIM (IM ass) lr d xs =
        IM (fold (fn ass => fn x => extend'assoc ass (x,PA{prec=d,assoc=lr})
                  ) (fold delete'assoc ass xs) xs)
    fun nonfixIM (IM ass) xs = IM (fold delete'assoc ass xs)
end

fun isInfix im x =
    case (getIM im x) of
        Some _ => true
      | _ => false

(*----------------------------------------------------------------------
INDEX: fixMod
----------------------------------------------------------------------*)

abstype 'a fixMod = FM of ((infixMap -> infixMap) option * 'a)
with
    fun getFM (FM (_,x)) = x
    fun makeFM x = FM (None, x)
    fun fm'fixMod x fm = FM(Some fm,x)
    local
        fun composeOptFunc (None,None) = None
          | composeOptFunc (Some f,None) = Some f
          | composeOptFunc (None,Some f) = Some f
          | composeOptFunc (Some f, Some g) = Some (f o g)
    in
        fun fm'pair (FM(fmo1,x1),FM(fmo2,x2)) =
            FM (composeOptFunc (fmo2,fmo1), (x1,x2))
    end
    fun map'fm f (FM(fmo,dec)) = FM(fmo,f dec)
    fun app'fm (FM (fmo,_)) im =
        case fmo of Some fm => fm im | None => im
end

fun fm'seq seq p = map'fm seq (fm'pair p)
fun dfm'infix lr d xs = fm'fixMod DEC'Empty (fn im => infixIM im lr d xs)
fun dfm'nonfix xs = fm'fixMod DEC'Empty (fn im => nonfixIM im xs)

(*----------------------------------------------------------------------
INDEX: keywords, scon, tyvar, lab
----------------------------------------------------------------------*)

fun key tok =
    satisfyP ("key-"^sofTok tok)
    (fn tok' => if (tok=tok') then Some () else None)

val scon =
    satisfyP "scon"
    (fn TOK'Number n    => Some (SCON'Num n)
      | TOK'Word w      => Some (SCON'Word w)
      | TOK'CharLit c   => Some (SCON'Char c)
      | TOK'StringLit s => Some (SCON'String s)
      | _               => None)

val tyvar =
    satisfyP "tyvar"
    (fn TOK'TyVar s => Some (TYVAR (ID s)) | _ => None)

val lab =
    satisfyP "lab"
    (fn TOK'Ident s => Some (LAB'Id (ID s))
      | TOK'Symid s => Some (LAB'Id (ID s))
      | SYM_STAR    => Some (LAB'Id (ID "*"))
      | SYM_EQ      => Some (LAB'Id (ID "="))
      | TOK'Number n=> Some (LAB'Num n)
      | _           => None)

(*----------------------------------------------------------------------
INDEX: match
----------------------------------------------------------------------*)

val stridMatch = (* not symbolic *)
    fn TOK'Ident s => Some (STRID (ID s))
     | _           => None

val tyconMatch = (* not star *)
    (fn TOK'Ident s => Some (TYCON (ID s))
      | TOK'Symid s => Some (TYCON (ID s))
      | _           => None)

fun vidMatch tok =
    case tok of
        TOK'Ident s => Some (VID (ID s))
      | TOK'Symid s => Some (VID (ID s))
      | SYM_STAR    => Some (VID (ID "*"))
      | SYM_EQ      => Some (VID (ID "="))
      | _           => None

fun vidMatch_noeq tok =
    case tok of
        TOK'Ident s => Some (VID (ID s))
      | TOK'Symid s => Some (VID (ID s))
      | SYM_STAR    => Some (VID (ID "*"))
      | _           => None

fun nonFixVidMatch im tok=
    case (vidMatch tok) of
        None => None
      | Some x => if (isInfix im x) then None else Some x

fun nonFixVidMatch_noeq im tok=
    case (vidMatch_noeq tok) of
        None => None
      | Some x => if (isInfix im x) then None else Some x

fun infixVidMatch im tok =
    case (vidMatch tok) of
        None => None
      | Some x => if (isInfix im x) then Some x else None

fun infixVidMatch_noeq im tok =
    case (vidMatch_noeq tok) of
        None => None
      | Some x => if (isInfix im x) then Some x else None

(*----------------------------------------------------------------------
INDEX: identifiers
----------------------------------------------------------------------*)

val strid = satisfyP "strid" stridMatch
val tycon = satisfyP "tycon" tyconMatch
val vidAnyFix = satisfyP "vidAnyFix" vidMatch
fun vidInfix im = satisfyP "vidInfix" (infixVidMatch im)
fun vidInfix_noeq im = satisfyP "vidInfix_noeq" (infixVidMatch_noeq im)
fun vid im = satisfyP "vid" (nonFixVidMatch im)
fun vid_noeq im = satisfyP "vid_noeq" (nonFixVidMatch_noeq im)

(*----------------------------------------------------------------------
INDEX: (new) long -- backtracking for strid-dot
----------------------------------------------------------------------*)

val strid_dot = noErrorP (strid $$- key SYM_DOT)

fun long (p:'a par,p1: 'a par) : 'a long par =
    many1 strid_dot ** p >>> LONG
    ## (p1 >>> (fn x => LONG ([],x)))

val longtycon       = long (tycon,tycon)
val longstrid       = long (strid,strid)
val longvidAnyFix   = long (vidAnyFix,vidAnyFix)
fun longvid im      = long (vidAnyFix,vid im)
fun longvid_noeq im = long (vidAnyFix,vid_noeq im)

(*----------------------------------------------------------------------
INDEX: keyword: op
----------------------------------------------------------------------*)

fun op_vid im =
    key KEY_op -$$ vidAnyFix
    ## vid im

fun op_longvid im =
    key KEY_op -$$ longvidAnyFix
    ## longvid im

fun op_longvid_noeq im =
    key KEY_op -$$ longvidAnyFix
    ## longvid_noeq im

(*----------------------------------------------------------------------
INDEX: types
----------------------------------------------------------------------*)

fun seq p =
    p >>> (fn x => [x])
    ## key SYM_LP -$$ sequence p (key SYM_COMMA) $$- key SYM_RP
    ## resultP []

fun seq' p =
    p >>> (fn x => [x])
    ## noErrorP (key SYM_LP -$$ sequence p (key SYM_COMMA) $$- key SYM_RP)
    ## resultP []

val tyvarseq = seq tyvar
val tyvarseq' = seq' tyvar

(*
  tyrow = lab : ty < , tyrow >
  ty =
    tyvar
    { <tyrow> }
    tyseq longtycon
    ty * ... * ty
    ty -> ty
    (ty)
*)

val tyrow =
    fn ty =>
    sequence0 (lab $$- key SYM_COLON ** ty) (key SYM_COMMA) >>> TYROW

fun ty'con0 c = TY'Con ([],c)
fun ty'con1 (t,c) = TY'Con ([t],c)


val atty =
    fn ty =>
    tyvar >>> TY'Var
    ## ((key SYM_LB -$$ tyrow ty $$- key SYM_RB) >>> TY'Record)
    ## longtycon >>> ty'con0
    ## (key SYM_LP -$$ ty >>
        (fn t =>
         key SYM_RP -$$ (resultP t
                         ## longtycon >>> (fn c => ty'con1 (t,c)))
         ## (manyP (key SYM_COMMA -$$ ty) >>
             (fn ts =>
              key SYM_RP -$$ longtycon >>>
              (fn c => TY'Con (t::ts,c))))))


fun conty T =
    left (atty T) longtycon ty'con1

fun tupty T =
    sequence (conty T) (key SYM_STAR) >>> (fn [t] => t
                                            | ts => TY'Tuple ts)

fun functy T =
    right (tupty T) (key SYM_TYPE_ARROW -$$ tupty T) TY'Func

val ty = fixP functy

(*----------------------------------------------------------------------
INDEX: patterns
----------------------------------------------------------------------*)

(*
  patrow =
    ...
    lab = pat <, patrow>
    id <:ty> <as pat> <, patrow>
*)
fun after_label_as_variable pat (x:vid) =
    opt (key SYM_COLON -$$ ty) >>
    (fn tOpt =>
     opt (key KEY_as -$$ pat) >>>
     (fn pOpt =>
      case (pOpt) of
          Some p => PAT'Layered (x,tOpt,p)
        | None => let val p = pat'vid x
                  in case (tOpt) of
                         Some t => PAT'Typed (p,t)
                       | None => p
                  end))

val labelled_pat =
    fn pat =>
    lab >>
    (fn l =>
     ((key SYM_EQ -$$ pat)
      ## (case l of (* BAD -- case performed even when "=" is found *)
              LAB'Num _ => failP
            | LAB'Id x => after_label_as_variable pat (VID x)))
     >>> (fn p => (l,p)))

fun acc_patrow acc P =
    (positionP $$- key SYM_DOTS >> (fn pos => resultP (PATROW (rev acc,{wildcard=Some pos}))))
    ## (labelled_pat P >>
        (fn lp =>
         (key SYM_COMMA -$$ acc_patrow (lp::acc) P)
         ## resultP (PATROW (rev (lp::acc),{wildcard=None}))))

val patrow = acc_patrow []

val underscorePat =
    satisfyP "underscorePat" (fn SYM_UNDERSCORE => Some PAT'Wild | _ => None)

(*
  atpat =
    _
    scon
    <op> longid
    { <patrow> }
    ()
    (pat1,...,patN)
    [pat1,...,patN]
    (pat)
*)

fun atpatP im pat =
    underscorePat
    ## (scon >>> PAT'Scon)
    ## (op_longvid_noeq im >>> PAT'Var)
    ## ((key SYM_LB -$$ patrow pat $$- key SYM_RB) >>> PAT'Record)
    ## ((key SYM_LP -$$ sequence0 pat (key SYM_COMMA) $$- key SYM_RP) >>>
        (fn [] => PAT'Unit
          | [x] => x
          | xs => PAT'Tuple xs))
    ## ((key SYM_LS -$$ sequence0 pat (key SYM_COMMA) $$- key SYM_RS) >>>
        PAT'List)

(*
  pat =
    atpat
    <op> longid atpat
    pat id pat
    pat : ty
    <op> var <:ty> as pat
*)

fun constructed_pat longx im pat =
    atpatP im pat >>> (fn p => PAT'Cons (longx,p))
    ## resultP (PAT'Var longx)


fun layered_pat im pat =
    op_longvid_noeq im >>
    (fn longx =>
     constructed_pat longx im pat
     ## (case longx of
             LONG([],x) =>
                 opt (key SYM_COLON -$$ ty) >>
                 (fn tOpt =>
                  key KEY_as -$$ pat >>> (fn p => PAT'Layered (x,tOpt,p))
                  ## (case tOpt of
                          Some t => resultP (PAT'Typed (PAT'Var longx, t))
                        | None => failP))
           | _ => failP))
    ## atpatP im pat


(* left associating infixes... *)
fun IGNORED_left_infpat im P =
    left (layered_pat im P) (vidInfix_noeq im ** layered_pat im P)
    (fn (x1,(f,x2)) => makeBinPat(x1,f,x2))


(* right associating infixes...*)
fun right_infpat im P =
    layered_pat im P >>
    (fn x1 =>
     (vidInfix_noeq im >>
      (fn f =>
       right_infpat im P >>>
       (fn x2 =>
        makeBinPat(x1,f,x2))))
     ## resultP x1)

val infpat = right_infpat

fun typed_pat im P =
    left (infpat im P) (key SYM_COLON -$$ ty) PAT'Typed

fun pat im = fixP (typed_pat im)
fun atpat im = atpatP im (pat im)

(*----------------------------------------------------------------------
INDEX: expressions
----------------------------------------------------------------------*)

fun exprow exp =
    sequence0 (lab $$- key SYM_EQ ** exp) (key SYM_COMMA) >>> EXPROW

fun mrule im exp =
    pat im $$- key SYM_FUNC_ARROW ** exp >>> MRULE

fun match im exp =
    positionP >> (fn pos =>
    sequence (mrule im exp) (key SYM_VBAR) >>> (fn xs => MATCH (pos,xs)))

(*
  atexp =
    scon
    <op> longid
    { <exprow> }
    #lab
    ()
    (exp1,...,expN)
    [exp1,...,expN]
    #[exp1,...,expN] (vector literal extension)
    (exp1;...;expN)
    let dec in exp1;...;expN end
    (exp)
*)
fun atexp1 im exp =
    (scon >>> EXP'Scon)
    ## op_longvid im ** positionP >>> EXP'Var
    ## ((key SYM_LB -$$ exprow exp $$- key SYM_RB) >>> EXP'Record)
    ## ((key SYM_HASH -$$ positionP ** lab) >>> EXP'Select)
    ## (key SYM_LP -$$
        (resultP EXP'Unit
         ## (exp >>
             (fn x1 =>
              ((many1 (key SYM_SEMI -$$ exp) >>> (fn xs => EXP'Seq (x1::xs)))
               ## (manyP (key SYM_COMMA -$$ exp) >>>
                   (fn [] => x1
                     | xs => EXP'Tuple (x1::xs)))))))
        $$- key SYM_RP)
    ## ((key SYM_LS -$$ sequence0 exp (key SYM_COMMA) $$- key SYM_RS)
        >>> EXP'List)
    ## ((key SYM_HASH_LS -$$ sequence0 exp (key SYM_COMMA) $$- key SYM_RS)
        >>> EXP'Vector)

fun atexp im (exp,expIM,dfm) =
    atexp1 im exp
    ## (key KEY_let -$$ dfm >>
        (fn d =>
         key KEY_in -$$ (sequence (expIM (app'fm d im)) (key SYM_SEMI) >>>
                         (fn [x] => x | xs => EXP'Seq xs)
                         ) $$- key KEY_end >>>
         (fn e =>
          EXP'Let(getFM d,e))))


(*
  appexp =
    atexp
    appexp atexp
*)
fun appexp im ED =
    left (atexp im ED) (atexp im ED) EXP'App

(*
  infexp =
    appexp
    infexp id infexp
*)

fun pa_infixVidMatch im tok =
    case (vidMatch tok) of
        None => None
      | Some x => (case (getIM im x) of
                       Some pa => Some (pa,x)
                     | None => None)

fun pa_vidInfix im = satisfyP "pa_vidInfix" (pa_infixVidMatch im)

fun reduce_pa (f:'a*'b*'a->'a) (x1:'a) (xs:((pa*'b)*'a)list) : 'a option =
    case xs of
        [] => Some x1
      | [((_,op12),x2)] => Some (f (x1,op12,x2))
      | ((PA{prec=prec12,assoc=assoc12},op12),x2) ::
        (xs' as ((PA{prec=prec23,assoc=assoc23},_),x3) :: _) =>
            let
                val reductionDirectionOpt =
                    if (prec12 > prec23) then (Some LR'Left) else
                    if (prec12 < prec23) then (Some LR'Right) else
                    (*prec12=prec23*)
                    case (assoc12,assoc23) of
                        (LR'Left,LR'Left) => Some LR'Left
                      | (LR'Right,LR'Right) => Some LR'Right
                      | (LR'Left,LR'Right) => None
                      | (LR'Right,LR'Left) => None
            in
                case (reductionDirectionOpt) of
                    None => None
                  | Some LR'Left => reduce_pa f (f(x1,op12,x2)) xs'
                  | Some LR'Right =>
                        (case (reduce_pa f x2 xs') of
                             None => None
                           | Some reduced => Some (f (x1,op12,reduced)))
            end



fun infexp im ED =
    positionP >> (fn dummyPos =>
    appexp im ED >>
    (fn x1 =>
     manyP (pa_vidInfix im ** appexp im ED) >>>
     (fn xs =>
      reduce_pa
      (fn (a,b,c) => EXP'App (exp'vid dummyPos b, exp'pair (a,c))
       ) x1 xs
      )))
    (* fail if ambiguous opertor associativity *)
    >> (fn Some e => resultP e | None => failP)

(*
  exp =
    infexp
    exp : ty
    exp andalso exp
    exp orelse exp
    exp handle match
    raise exp
    if exp then exp else exp
    while exp do exp
    case exp of match
    fn match
*)

fun typed_exp im ED =
    left (infexp im ED) (key SYM_COLON -$$ ty) EXP'Typed

fun andalso_exp im ED =
    left (typed_exp im ED) (key KEY_andalso -$$ typed_exp im ED) EXP'Andalso

fun orelse_exp im ED =
    left (andalso_exp im ED) (key KEY_orelse -$$ andalso_exp im ED) EXP'Orelse

fun handle_exp im ED exp =
    left (orelse_exp im ED) (key KEY_handle -$$ match im exp) EXP'Handle

fun expED im (ED as (exp,_,_)) =
    handle_exp im ED exp
    ## (key KEY_raise -$$ exp >>> EXP'Raise)
    ##
    (key KEY_if -$$ exp >>
     (fn i =>
      (key KEY_then -$$ exp >>
       (fn t =>
        (key KEY_else -$$ exp >>>
         (fn e =>
          EXP'If(i,t,e)))))))
    ##
    (key KEY_while -$$ exp >>
     (fn w =>
      (key KEY_do -$$ exp >>>
       (fn d =>
        EXP'While(w,d)))))
    ##
    (key KEY_case -$$ exp >>
     (fn e =>
      (key KEY_of -$$ match im exp >>>
       (fn m =>
        EXP'Case(e,m)))))
    ##
    (key KEY_fn -$$ match im exp >>> EXP'Fn)

(*----------------------------------------------------------------------
INDEX: declarations
----------------------------------------------------------------------*)

fun valbind im exp =
    fixP (fn valbind =>
          (pat im >>
           (fn p =>
            key SYM_EQ -$$ exp >>
            (fn e =>
             opt (key KEY_and -$$ valbind) >>>
             (fn vopt => VALBIND'Seq (p,e,vopt)))))
          ##
          (key KEY_rec -$$ valbind >>> VALBIND'Rec))



fun name_pats im =
    key KEY_op -$$ vidAnyFix >>
    (fn name =>
     many1 (atpat im) >>>
     (fn ps =>
      (name,ps)))
    ##
    key SYM_LP -$$ atpat im >>
    (fn p1 =>
     vidInfix_noeq im >>
     (fn n2 =>
      atpat im >>
      (fn p3 =>
       key SYM_RP -$$ (manyP (atpat im) >>>
                       (fn ps =>
                        (n2,pat'pair(p1,p3)::ps))
                       ##
                       vidInfix_noeq im >>
                       (fn n4 =>
                        atpat im >>>
                        (fn p5 =>
                         (n4,[pat'pair(makeBinPat(p1,n2,p3),p5)])))))))
    ##
    vid im >>
    (fn n1 => (vidInfix_noeq im >>
               (fn n2 =>
                atpat im >>>
                (fn p3 =>
                 (n2,[pat'pair(pat'vid n1,p3)])))
               ##
               many1 (atpat im) >>>
               (fn ps =>
                (n1,ps))))
    ##
    atpat im >>
    (fn p1 =>
     vidInfix_noeq im >>
     (fn n2 =>
      atpat im >>>
      (fn p3 =>
       (n2,[pat'pair(p1,p3)]))))

fun name_pats_reqName name im =
    name_pats im >>
    (fn (name',ps) => if name=name' then resultP ps
                                    else failP)

fun clause_body (ps:pat list) exp =
    opt (key SYM_COLON -$$ ty) >>
    (fn topt =>
     key SYM_EQ -$$ exp >>>
     (fn e =>
      (ps,topt,e)))

fun clause im exp =
    name_pats im >>
    (fn (name,ps) =>
     clause_body ps exp >>>
     (fn cl =>
      (name,cl)))

fun clause_reqName name im exp =
    name_pats_reqName name im >>
    (fn ps => clause_body ps exp)

fun fvalbind im E =
    positionP >> (fn pos =>
    sequence (clause im E >>
              (fn (name,clause1) =>
               manyP (key SYM_VBAR -$$ clause_reqName name im E) >>>
               (fn clauses =>
                (pos,name,clause1::clauses)))
              ) (key KEY_and) >>> FVALBIND)

val typbind =
    sequence (tyvarseq >>
              (fn tvs =>
               tycon >>
               (fn tc =>
                key SYM_EQ -$$
                ty >>>
                (fn t => (tvs,tc,t))))
              ) (key KEY_and) >>> TYPBIND

fun dat_bindOrDesc id =
    sequence (tyvarseq >>
              (fn tvs =>
               tycon >>
               (fn tc =>
                key SYM_EQ -$$
                sequence (id ** opt (key KEY_of -$$ ty)) (key SYM_VBAR) >>>
                (fn vcons => (tvs,tc,vcons))))
              ) (key KEY_and)

fun datbind im = dat_bindOrDesc (op_vid im) >>> DATBIND

fun exbind1 im =
    op_vid im >>
    (fn x =>
     (opt (key KEY_of -$$ ty) >>> (fn topt => EXBIND1'New (x,topt)))
     ## (key SYM_EQ -$$ op_longvid im >>> (fn y => EXBIND1'Copy (x,y))))

fun exbind im =
    sequence (exbind1 im) (key KEY_and) >>> EXBIND


val digit =
    satisfyP "digit"
    (fn TOK'Number n => if (0 <= n andalso n <= 9)
                        then Some n else None
      | _ => None)

val precOpt =
    opt digit >>> (fn Some n => n | None => 0)


fun dec1E im E =
    (key KEY_val -$$ tyvarseq' ** valbind im E >>> DEC'Val)
    ## (key KEY_fun -$$ fvalbind im E >>> DEC'Fval)
    ## (key KEY_type -$$ typbind >>> DEC'Type)

    ## (key KEY_datatype -$$
            ((noErrorP (tycon $$- key SYM_EQ $$- key KEY_datatype) ** longtycon
                       >>> DEC'DataReplicate)
             ## (datbind im ** opt (key KEY_withtype -$$ typbind) >>> DEC'Datatype)))

    ## (key KEY_exception -$$ exbind im >>> DEC'Exception)
    ## key KEY_open -$$ many1 longstrid >>> DEC'Open

val fixityDec =
    ((key KEY_infix -$$ resultP LR'Left)
     ## (key KEY_infixr -$$ resultP LR'Right)
     ) >> (fn lr =>
           precOpt >>
           (fn d => many1 vidAnyFix >>>
            (fn xs => dfm'infix lr d xs)))
    ##
    key KEY_nonfix -$$ many1 vidAnyFix >>> dfm'nonfix

fun dfm1DE im (dfm,dfmIM,expIM) =
    dec1E im (expIM im) >>> makeFM
    ## fixityDec
    ## (key KEY_abstype -$$ datbind im ** opt (key KEY_withtype -$$ typbind)
        >> (fn (dat,wo) =>
            key KEY_with -$$ dfm $$- key KEY_end
            >>> map'fm (fn d => DEC'Abstype (dat,wo,d))))
    ## (key KEY_local -$$ dfm >>
        (fn d1 =>
         key KEY_in -$$ dfmIM (app'fm d1 im) $$-
         key KEY_end
         >>> map'fm (fn d2 => DEC'Local(getFM d1,d2))))

(*
fun acc_dfmDE acc (sep:'a par) im DE =
    resultP acc
    ##
    sep (*-$*) >> (* to avoid non-termination *)
    (fn _ =>
     dfm1DE im DE >>
     (fn d => acc_dfmDE (fm'seq DEC'Seq (acc,d)) sep (app'fm d im) DE))
*)

(* Mon Sep 25 17:53:51 2006 *)
fun acc_dfmDE acc (sep:'a par) im DE =
    sep >> (fn _ =>
    resultP acc
    ##
    dfm1DE im DE >>
    (fn d => acc_dfmDE (fm'seq DEC'Seq (acc,d)) sep (app'fm d im) DE))


fun dfmDE_sep sep im DE =
    acc_dfmDE (makeFM DEC'Empty) sep im DE

(*----------------------------------------------------------------------
INDEX: fixup
----------------------------------------------------------------------*)

val semis = manyP (key SYM_SEMI)
val null = resultP ()

fun dfm im = fixP (fn D => dfmDE_sep semis im (D,dfm,exp))
and exp im = fixP (fn E => expED im (E,exp,dfm im))
fun dfm1 im = dfm1DE im (dfm im, dfm, exp)

(*----------------------------------------------------------------------
INDEX: sigid,funid
----------------------------------------------------------------------*)

val sigid =
    satisfyP "sigid"
    (fn TOK'Ident s => Some (SIGID (ID s))
      | _           => None)

val funid =
    satisfyP "funid"
    (fn TOK'Ident s => Some (FUNID (ID s))
      | _           => None)

(*----------------------------------------------------------------------
INDEX: spec
----------------------------------------------------------------------*)

val warnOp = fn None => () | Some _ => (NMLecho "Warning: unnecessary `op'"; ())

val valdesc =
    sequence (opt (key KEY_op) >>> warnOp -$$ vidAnyFix $$- key SYM_COLON ** ty) (key KEY_and)

val typdesc =
    sequence ((tyvarseq ** tycon ** opt (key SYM_EQ -$$ ty)) >>> (fn ((a,b),c) => (a,b,c))
              ) (key KEY_and)

val eqtypdesc =
    sequence (tyvarseq ** tycon) (key KEY_and)

val datadesc = (* share code with core-language parser *)
    dat_bindOrDesc vidAnyFix

val exdesc =
    sequence (vidAnyFix ** opt (key KEY_of -$$ ty)) (key SYM_VBAR)

fun strdesc sigexp =
    sequence (strid $$- key SYM_COLON ** sigexp) (key KEY_and)

fun spec1 sigexp =
    key KEY_val -$$ valdesc >>> SPEC'Val
    ## key KEY_type -$$ typdesc >>> SPEC'Type
    ## key KEY_eqtype -$$ eqtypdesc >>> SPEC'Eqtype
    ## (key KEY_datatype -$$
        datadesc >>> SPEC'Datatype
        ## (tycon $$- key SYM_EQ $$- key KEY_datatype ** longtycon
            ) >>> SPEC'DataReplicate)
    ## key KEY_exception -$$ exdesc >>> SPEC'Exception
    ## key KEY_include -$$ sigexp >>> SPEC'Include
    ## key KEY_structure -$$ strdesc sigexp >>> SPEC'Structure

val sharing_type =
    key KEY_sharing -$$ key KEY_type -$$
    sequence longtycon (key SYM_EQ)

fun acc_specs S sp =
    semis -$$
    (resultP sp
     ## sharing_type >> (fn tcs => acc_specs S (SPEC'Sharing (sp,tcs)))
     ## spec1 S >> (fn sp' => acc_specs S (SPEC'Seq (sp,sp'))))

fun specS S =
    resultP SPEC'Empty >> acc_specs S

(*----------------------------------------------------------------------
INDEX: sigexp
----------------------------------------------------------------------*)

fun sigexp1 S =
    key KEY_sig -$$ specS S $$- key KEY_end >>> SIG'Spec
    ## sigid >>> SIG'Id

val where_type =
    key KEY_where -$$ key KEY_type -$$
    tyvarseq >>
    (fn tvs =>
     longtycon >>
     (fn tc =>
      key SYM_EQ -$$
      ty >>>
      (fn t => (tvs,tc,t))))

fun sigexpS S =
    left (sigexp1 S) where_type SIG'TypeRealisation

val sigexp = fixP sigexpS

val spec = specS sigexp

(*----------------------------------------------------------------------
INDEX: strexp, strdec
----------------------------------------------------------------------*)

val funid_LP = noErrorP (funid $$- key SYM_LP)

fun strexp1DE im (strexp,strexpIM,strdecfm) =
    funid_LP ** (strexp ## (strdecfm >>> getFM >>> STREXP'Struct)
                 ) $$- key SYM_RP >>> STREXP'FunctorApp
    ## longstrid >>> STREXP'Id
    ## (key KEY_struct -$$ strdecfm $$- key KEY_end
        >>> getFM >>> STREXP'Struct)
    ## (key KEY_let -$$ strdecfm >>
        (fn d =>
         key KEY_in -$$ strexpIM (app'fm d im) $$- key KEY_end
         >>> (fn e => STREXP'Let (getFM d,e))))

val signatureConstraint =
    (key SYM_COLON -$$ resultP STREXP'TransCon) ##
    (key SYM_COLON_GT -$$ resultP STREXP'OpaqueCon)

fun strexpDE im DE =
    left (strexp1DE im DE)
    (signatureConstraint ** sigexp) (fn (e,(F,s)) => F(e,s))

fun bindStrexpWithDerivedSignatureConstraintForm E =
    opt (signatureConstraint ** sigexp) ** (key SYM_EQ -$$ E)
        >>> (fn (None,e) => e
              | (Some(F,s),e) => F(e,s))

fun strbind E =
    sequence (strid ** (bindStrexpWithDerivedSignatureConstraintForm E)
              ) (key KEY_and)

fun strdecfm1DE im (strdecfm,strdecfmIM,E) =
    key KEY_structure -$$ strbind E >>> STRDEC'Structure >>> makeFM
    ## (key KEY_local -$$ strdecfm >>
        (fn d1 =>
         key KEY_in -$$ strdecfmIM (app'fm d1 im) $$-
         key KEY_end
         >>> map'fm (fn d2 => STRDEC'Local(getFM d1,d2))))
    ## dfm1 im >>> map'fm STRDEC'Dec

fun acc_strdecfmDE acc (sep:'a par) im DE =
    sep -$$
    (resultP acc
     ##
     strdecfm1DE im DE >>
     (fn d => acc_strdecfmDE (fm'seq STRDEC'Seq (acc,d)) sep (app'fm d im) DE))

fun strdecfmDE_sep sep im DE =
    acc_strdecfmDE (makeFM STRDEC'Empty) sep im DE

(*----------------------------------------------------------------------
INDEX: fixup
----------------------------------------------------------------------*)

fun strdecfmE E im = fixP (fn D => strdecfmDE_sep semis im (D,strdecfmE E,E))
fun strexp im = fixP (fn E => strexpDE im (E,strexp,strdecfmE E im))
fun strdecfm im = strdecfmE (strexp im) im
fun strdecfm_noSemiPrefix im = strdecfmDE_sep null im (strdecfm im,strdecfm,strexp im)

(*----------------------------------------------------------------------
INDEX: sigdec, topdec
----------------------------------------------------------------------*)

val sigdec =
    key KEY_signature -$$
    sequence (sigid $$- key SYM_EQ ** sigexp) (key KEY_and)
    >>> SIGDEC


val funImport =
    (strid >> (fn x =>
               key SYM_COLON -$$ sigexp >>> (fn X => FUNIMPORT'Basic (x,X))))
     ## spec >>> FUNIMPORT'Derived

fun fundec im =
    key KEY_functor -$$
    sequence (funid >> (fn F =>
                        key SYM_LP -$$
                        funImport >>
                        (fn I =>
                         key SYM_RP -$$
                         bindStrexpWithDerivedSignatureConstraintForm (strexp im) >>>
                         (fn S => (F,I,S))))
              ) (key KEY_and) >>> FUNDEC

fun topdec_item im =
    exp im >>> TOPDEC'Exp
    ## strdecfm_noSemiPrefix im >>> getFM >>> TOPDEC'Strdec
    ## sigdec >>> TOPDEC'Sigdec
    ## fundec im >>> TOPDEC'Fundec

fun topdec im = many1 (topdec_item im)

(*----------------------------------------------------------------------
INDEX: prog
----------------------------------------------------------------------*)

fun prog im = sequence (topdec im) (key SYM_SEMI) >>> link


end
