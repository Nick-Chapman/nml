

signature ParserSig = sig datatype id = ID of string
                          datatype vid = VID of id
                          datatype leftOrRight = LR'Left | LR'Right
                          datatype pa = PA of {assoc : leftOrRight,prec : int}
                          type infixMap
                          val makeIM : ((vid * pa) list -> infixMap)
                          val isInfix : (infixMap -> (vid -> bool))
                          type topdec
                          type 'a par
                          val prog : (infixMap -> topdec list par)
                      end
signature PrelSig_Import = sig val abort : (string -> 'a)
                               val link : ('a list list -> 'a list)
                               val fold : (('a -> ('b -> 'a)) ->
                                           ('a -> ('b list -> 'a)))
                           end
signature AssocSig_Import = sig type ('a,'b) assoc
                                val assoc'list : (('a * 'b) list ->
                                                  ('a,'b) assoc)
                                val extend'assoc : (('a,'b) assoc ->
                                                    (('a * 'b) -> ('a,'b) assoc)
                                                   )
                                val delete'assoc : ((''a,'b) assoc ->
                                                    (''a -> (''a,'b) assoc))
                                val lookup'assoc : ((''a,'b) assoc ->
                                                    (''a -> 'b option))
                            end
signature TokSig_Import = sig datatype tok = KEY_abstype | KEY_and | KEY_andalso
                                             | KEY_as | KEY_case | KEY_datatype
                                             | KEY_do | KEY_else | KEY_end
                                             | KEY_eqtype | KEY_exception
                                             | KEY_fn | KEY_fun | KEY_functor
                                             | KEY_handle | KEY_if | KEY_in
                                             | KEY_include | KEY_infix
                                             | KEY_infixr | KEY_let | KEY_local
                                             | KEY_nonfix | KEY_of | KEY_op
                                             | KEY_open | KEY_orelse | KEY_raise
                                             | KEY_rec | KEY_sharing | KEY_sig
                                             | KEY_signature | KEY_struct
                                             | KEY_structure | KEY_then
                                             | KEY_type | KEY_val | KEY_where
                                             | KEY_while | KEY_with
                                             | KEY_withtype | SYM_COLON
                                             | SYM_COLON_GT | SYM_COMMA
                                             | SYM_DOT | SYM_DOTS | SYM_EQ
                                             | SYM_FUNC_ARROW | SYM_HASH
                                             | SYM_HASH_LS | SYM_LB | SYM_LP
                                             | SYM_LS | SYM_RB | SYM_RP | SYM_RS
                                             | SYM_SEMI | SYM_STAR
                                             | SYM_TYPE_ARROW | SYM_UNDERSCORE
                                             | SYM_VBAR | TOK'CharLit of char
                                             | TOK'Err of string
                                             | TOK'Ident of string
                                             | TOK'Number of int
                                             | TOK'StringLit of string
                                             | TOK'Symid of string
                                             | TOK'TyVar of string
                                             | TOK'Unknown of char
                                             | TOK'Word of word
                              val sofTok : (tok -> string)
                          end
signature PfvSig_Import = sig type 'a par
                              val resultP : ('a -> 'a par)
                              val failP : 'a par
                              val bindP : (('a par * ('a -> 'b par)) -> 'b par)
                              val altP : (('a par * 'a par) -> 'a par)
                              val fixP : (('a par -> 'a par) -> 'a par)
                              val manyP : ('a par -> 'a list par)
                              val noErrorP : ('a par -> 'a par)
                              type tok
                              val satisfyP : (string -> ((tok -> 'a option) ->
                                                         'a par))
                          end
signature AtomSig_Import = sig datatype id = ID of string
                               datatype vid = VID of id
                               datatype tyvar = TYVAR of id
                               datatype tycon = TYCON of id
                               datatype strid = STRID of id
                               datatype sigid = SIGID of id
                               datatype funid = FUNID of id
                               datatype 'a long = LONG of (strid list * 'a)
                               datatype lab = LAB'Id of id | LAB'Num of int
                               datatype scon = SCON'Num of int
                                               | SCON'Word of word
                                               | SCON'Real of real
                                               | SCON'Char of char
                                               | SCON'String of string
                           end
signature LangSig_Import =
            sig type vid
                type tyvar
                type tycon
                type strid
                type sigid
                type funid
                type 'a long
                type lab
                type scon
                datatype tyrow = TYROW of (lab * ty) list
                and ty = TY'Var of tyvar | TY'Record of tyrow
                         | TY'Con of (ty list * tycon long)
                         | TY'Tuple of ty list
                         | TY'Func of (ty * ty)
                datatype patrow = PATROW of ((lab * pat) list *
                                             {wildcard : bool})
                and pat = PAT'Wild | PAT'Scon of scon
                          | PAT'Var of vid long
                          | PAT'Record of patrow | PAT'Unit
                          | PAT'Tuple of pat list
                          | PAT'List of pat list
                          | PAT'Cons of (vid long * pat)
                          | PAT'Typed of (pat * ty)
                          | PAT'Layered of (vid * ty option * pat
                                           )
                datatype typbind = TYPBIND of (tyvar list * tycon * ty) list
                datatype datbind = DATBIND of (tyvar list * tycon *
                                               (vid * ty option) list) list
                datatype exbind1 = EXBIND1'New of (vid * ty option)
                                   | EXBIND1'Copy of (vid * vid long)
                datatype exbind = EXBIND of exbind1 list
                datatype valbind = VALBIND'Seq of (pat * exp * valbind option)
                                   | VALBIND'Rec of valbind
                and fvalbind = FVALBIND of (vid * (pat list * ty option * exp)
                                                  list) list
                and dec = DEC'Val of valbind | DEC'Fval of fvalbind
                          | DEC'Type of typbind
                          | DEC'Datatype of (datbind * typbind option)
                          | DEC'DataReplicate of (tycon * tycon long)
                          | DEC'Abstype of (datbind * typbind option * dec)
                          | DEC'Exception of exbind | DEC'Local of (dec * dec)
                          | DEC'Open of strid long list | DEC'Empty
                          | DEC'Seq of (dec * dec)
                and mrule = MRULE of (pat * exp)
                and match = MATCH of mrule list
                and exprow = EXPROW of (lab * exp) list
                and exp = EXP'Scon of scon | EXP'Var of vid long
                          | EXP'Record of exprow | EXP'Select of lab | EXP'Unit
                          | EXP'Tuple of exp list | EXP'List of exp list
                          | EXP'Vector of exp list | EXP'Seq of exp list
                          | EXP'Let of (dec * exp) | EXP'App of (exp * exp)
                          | EXP'Typed of (exp * ty) | EXP'Andalso of (exp * exp)
                          | EXP'Orelse of (exp * exp)
                          | EXP'Handle of (exp * match) | EXP'Raise of exp
                          | EXP'If of (exp * exp * exp)
                          | EXP'While of (exp * exp) | EXP'Case of (exp * match)
                          | EXP'Fn of match
                datatype sigexp = SIG'Spec of spec | SIG'Id of sigid
                                  | SIG'TypeRealisation
                                    of (sigexp * (tyvar list * tycon long * ty))
                and spec = SPEC'Val of (vid * ty) list
                           | SPEC'Type of (tyvar list * tycon) list
                           | SPEC'Eqtype of (tyvar list * tycon) list
                           | SPEC'Datatype of (tyvar list * tycon *
                                               (vid * ty option) list) list
                           | SPEC'DataReplicate of (tycon * tycon long)
                           | SPEC'Exception of (vid * ty option) list
                           | SPEC'Structure of (strid * sigexp) list
                           | SPEC'Include of sigexp | SPEC'Empty
                           | SPEC'Seq of (spec * spec)
                           | SPEC'Sharing of (spec * tycon long list)
                datatype strexp = STREXP'Struct of strdec
                                  | STREXP'Id of strid long
                                  | STREXP'TransCon of (strexp * sigexp)
                                  | STREXP'OpaqueCon of (strexp * sigexp)
                                  | STREXP'FunctorApp of (funid * strexp)
                                  | STREXP'Let of (strdec * strexp)
                and strdec = STRDEC'Dec of dec
                             | STRDEC'Structure of (strid * strexp) list
                             | STRDEC'Local of (strdec * strdec) | STRDEC'Empty
                             | STRDEC'Seq of (strdec * strdec)
                datatype sigdec = SIGDEC of (sigid * sigexp) list
                datatype funimport = FUNIMPORT'Basic of (strid * sigexp)
                                     | FUNIMPORT'Derived of spec
                datatype fundec = FUNDEC of (funid * funimport * strexp) list
                datatype topdec = TOPDEC'Exp of exp | TOPDEC'Strdec of strdec
                                  | TOPDEC'Sigdec of sigdec
                                  | TOPDEC'Fundec of fundec
                val pat'vid : (vid -> pat)
                val exp'pair : ((exp * exp) -> exp)
                val exp'vid : (vid -> exp)
                val makeBinPat : ((pat * vid * pat) -> pat)
                val pat'pair : ((pat * pat) -> pat)
            end
functor
PARSER (structure Prel : PrelSig_Import
        structure Assoc : AssocSig_Import
        structure Tok : TokSig_Import
        structure Pfv : PfvSig_Import
        structure Atom : AtomSig_Import
        structure Lang : LangSig_Import sharing type Tok.tok = Pfv.tok
        sharing type Atom.lab = Lang.lab sharing type
        Atom.scon = Lang.scon sharing type Atom.long = Lang.long sharing
        type Atom.vid = Lang.vid sharing type Atom.tyvar = Lang.tyvar
        sharing type Atom.tycon = Lang.tycon sharing type
        Atom.strid = Lang.strid sharing type Atom.funid = Lang.funid
        sharing type Atom.sigid = Lang.sigid) =
struct
open Prel Assoc Tok Pfv Atom Lang
val op >> = bindP
fun op >>> {1 = p,2 = f} = op >> {1 = p,2 = fn x => resultP (f x)}
fun op -$$ {1 = p1,2 = p2} = op >> {1 = p1,2 = fn _ => p2}
fun op $$- {1 = p1,2 = p2} =
  op >> {1 = p1,2 = fn x => op -$$ {1 = p2,2 = resultP x}}
fun op ** {1 = p1,2 = p2} =
  op >> {1 = p1,2 = fn x1 => op >>> {1 = p2,2 = fn x2 => (x1,x2)}}
val op ## = altP
fun opt p = op ## {1 = op >>> {1 = p,2 = Some},2 = resultP None}
fun many1 p =
  op >> {1 = p,2 = fn x => op >>> {1 = manyP p,
                                   2 = fn xs => op :: {1 = x,2 = xs}}}
fun sequence p sep =
  op >> {1 = p,2 = fn x => op >>> {1 = manyP (op -$$ {1 = sep,2 = p}),
                                   2 = fn xs => op :: {1 = x,2 = xs}}}
fun sequence0 p sep = op ## {1 = (sequence p) sep,2 = resultP []}
fun left p1 p2 f =
  let fun acc x1 = op ## {1 = resultP x1,
                          2 = op >> {1 = p2,2 = fn x2 => acc (f (x1,x2))
                                    }}
  in op >> {1 = p1,2 = acc}
  end
fun foldr1 f [] = abort "foldr1[]"
  | foldr1 f [x] = x
  | foldr1 f (op :: {1 = x,2 = xs}) = f (x,(foldr1 f) xs)
fun right p1 p2 f =
  op >> {1 = p1,2 = fn x1 => op >>> {1 = manyP p2,
                                     2 = fn xs => (foldr1 f)
                                                  (op :: {1 = x1,2 = xs}
                                                  )}}
datatype leftOrRight = LR'Left | LR'Right
datatype pa = PA of {prec : int,assoc : leftOrRight}
abstype infixMap = IM of (vid,pa) assoc
with fun makeIM xs = IM (assoc'list xs)
     fun getIM (IM ass) x = (lookup'assoc ass) x
     fun infixIM (IM ass) lr d xs =
       IM (((fold (fn ass => fn x => (extend'assoc ass)
                                     (x,PA {prec = d,assoc = lr})))
            (((fold delete'assoc) ass) xs)) xs)
     fun nonfixIM (IM ass) xs = IM (((fold delete'assoc) ass) xs)
end
fun isInfix im x = case (getIM im) x of
                     Some _ => true
                   | _ => false
abstype 'a fixMod = FM of ((infixMap -> infixMap) option * 'a)
with fun getFM (FM (_,x)) = x
     fun makeFM x = FM (None,x)
     fun fm'fixMod x fm = FM (Some fm,x)
     local fun composeOptFunc (None,None) = None
             | composeOptFunc (Some f,None) = Some f
             | composeOptFunc (None,Some f) = Some f
             | composeOptFunc (Some f,Some g) =
             Some (op o {1 = f,2 = g})
     in fun fm'pair (FM (fmo1,x1),FM (fmo2,x2)) =
          FM (composeOptFunc (fmo2,fmo1),(x1,x2))
     end
     fun map'fm f (FM (fmo,dec)) = FM (fmo,f dec)
     fun app'fm (FM (fmo,_)) im = case fmo of
                                    Some fm => (fm im)
                                  | None => im
end
fun fm'seq seq p = (map'fm seq) (fm'pair p)
fun dfm'infix lr d xs =
  (fm'fixMod DEC'Empty) (fn im => (((infixIM im) lr) d) xs)
fun dfm'nonfix xs = (fm'fixMod DEC'Empty) (fn im => (nonfixIM im) xs)
fun key tok = (satisfyP (op ^ {1 = "key:",2 = sofTok tok}))
              (fn tok' => if op = {1 = tok,2 = tok'} then Some ()
                          else None)
val scon = (satisfyP "scon") (fn TOK'Number n => Some (SCON'Num n)
                              | TOK'Word w => Some (SCON'Word w)
                              | TOK'CharLit c => Some (SCON'Char c)
                              | TOK'StringLit s => Some (SCON'String s)
                              | _ => None)
val tyvar = (satisfyP "tyvar") (fn TOK'TyVar s => Some (TYVAR (ID s))
                                | _ => None)
val lab = (satisfyP "lab") (fn TOK'Ident s => Some (LAB'Id (ID s))
                            | TOK'Symid s => Some (LAB'Id (ID s))
                            | SYM_STAR => Some (LAB'Id (ID "*"))
                            | SYM_EQ => Some (LAB'Id (ID "="))
                            | TOK'Number n => Some (LAB'Num n)
                            | _ => None)
val stridMatch = fn TOK'Ident s => Some (STRID (ID s))
                 | _ => None
val tyconMatch = fn TOK'Ident s => Some (TYCON (ID s))
                 | TOK'Symid s => Some (TYCON (ID s))
                 | _ => None
fun vidMatch tok = case tok of
                     TOK'Ident s => (Some (VID (ID s)))
                   | TOK'Symid s => (Some (VID (ID s)))
                   | SYM_STAR => (Some (VID (ID "*")))
                   | SYM_EQ => (Some (VID (ID "=")))
                   | _ => None
fun vidMatch_noeq tok = case tok of
                          TOK'Ident s => (Some (VID (ID s)))
                        | TOK'Symid s => (Some (VID (ID s)))
                        | SYM_STAR => (Some (VID (ID "*")))
                        | _ => None
fun nonFixVidMatch im tok = case vidMatch tok of
                              None => None
                            | Some x => (if (isInfix im) x then None
                                         else Some x)
fun nonFixVidMatch_noeq im tok =
  case vidMatch_noeq tok of
    None => None
  | Some x => (if (isInfix im) x then None else Some x)
fun infixVidMatch im tok = case vidMatch tok of
                             None => None
                           | Some x => (if (isInfix im) x then Some x
                                        else None)
fun infixVidMatch_noeq im tok =
  case vidMatch_noeq tok of
    None => None
  | Some x => (if (isInfix im) x then Some x else None)
val strid = (satisfyP "strid") stridMatch
val tycon = (satisfyP "tycon") tyconMatch
val vidAnyFix = (satisfyP "vidAnyFix") vidMatch
fun vidInfix im = (satisfyP "vidInfix") (infixVidMatch im)
fun vidInfix_noeq im = (satisfyP "vidInfix_noeq")
                       (infixVidMatch_noeq im)
fun vid im = (satisfyP "vid") (nonFixVidMatch im)
fun vid_noeq im = (satisfyP "vid_noeq") (nonFixVidMatch_noeq im)
val strid_dot = noErrorP (op $$- {1 = strid,2 = key SYM_DOT})
fun long (p : 'a par,p1 : 'a par) : 'a long par =
  op ## {1 = op >>> {1 = op ** {1 = many1 strid_dot,2 = p},2 = LONG},
         2 = op >>> {1 = p1,2 = fn x => LONG ([],x)}}
val longtycon = long (tycon,tycon)
val longstrid = long (strid,strid)
val longvidAnyFix = long (vidAnyFix,vidAnyFix)
fun longvid im = long (vidAnyFix,vid im)
fun longvid_noeq im = long (vidAnyFix,vid_noeq im)
fun op_vid im = op ## {1 = op -$$ {1 = key KEY_op,2 = vidAnyFix},
                       2 = vid im}
fun op_longvid im =
  op ## {1 = op -$$ {1 = key KEY_op,2 = longvidAnyFix},2 = longvid im}
fun op_longvid_noeq im =
  op ## {1 = op -$$ {1 = key KEY_op,2 = longvidAnyFix},
         2 = longvid_noeq im}
fun seq p = op ## {1 = op >>> {1 = p,2 = fn x => [x]},
                   2 = op ## {1 = op $$- {1 = op -$$ {1 = key SYM_LP,
                                                      2 = (sequence p)
                                                          (key SYM_COMMA
                                                          )},
                                          2 = key SYM_RP},2 = resultP []
                             }}
val tyvarseq = seq tyvar
val tyrow =
    fn ty => op >>> {1 = (sequence0
                          (op ** {1 = op $$- {1 = lab,2 = key SYM_COLON},2 = ty}
                          )) (key SYM_COMMA),2 = TYROW}
fun ty'con0 c = TY'Con ([],c)
fun ty'con1 (t,c) = TY'Con ([t],c)
val atty =
    fn ty => op ##
             {1 = op ## {1 = op >>> {1 = tyvar,2 = TY'Var},
                         2 = op >>>
                             {1 = op $$- {1 = op -$$ {1 = key SYM_LB,
                                                      2 = tyrow ty},
                                          2 = key SYM_RB},2 = TY'Record}},
              2 = op ##
                  {1 = op >>> {1 = longtycon,2 = ty'con0},
                   2 = op >>
                       {1 = op -$$ {1 = key SYM_LP,2 = ty},
                        2 = fn t => op ##
                                    {1 = op -$$
                                         {1 = key SYM_RP,
                                          2 = op ## {1 = resultP t,
                                                     2 = op >>>
                                                         {1 = longtycon,
                                                          2 = fn c => ty'con1
                                                                      (t,c)}}},
                                     2 = op >>
                                         {1 = manyP (op -$$
                                                     {1 = key SYM_COMMA,
                                                      2 = ty}),
                                          2 = fn ts => op >>>
                                                       {1 = op -$$
                                                            {1 = key SYM_RP,
                                                             2 = longtycon},
                                                        2 = fn c => TY'Con
                                                                    (op ::
                                                                     {1 = t,
                                                                      2 = ts},c)
                                                       }}}}}}
fun conty T = ((left (atty T)) longtycon) ty'con1
fun tupty T = op >>> {1 = (sequence (conty T)) (key SYM_STAR),
                      2 = fn [t] => t
                          | ts => TY'Tuple ts}
fun functy T = ((right (tupty T)) (op -$$ {1 = key SYM_TYPE_ARROW,2 = tupty T}))
               TY'Func
val ty = fixP functy
fun after_label_as_variable pat (x : vid) =
  op >> {1 = opt (op -$$ {1 = key SYM_COLON,2 = ty}),
         2 = fn tOpt => op >>> {1 = opt (op -$$ {1 = key KEY_as,2 = pat}),
                                2 = fn pOpt => case pOpt of
                                                 Some p => (PAT'Layered
                                                            (x,tOpt,p))
                                               | None => let val p = pat'vid x
                                                         in case tOpt of
                                                              Some t =>
                                                                (PAT'Typed (p,t)
                                                                )
                                                            | None => p
                                                         end}}
val labelled_pat =
    fn pat => op >> {1 = lab,
                     2 = fn l => op >>>
                                 {1 = op ## {1 = op -$$ {1 = key SYM_EQ,
                                                         2 = pat},
                                             2 = case l of
                                                   LAB'Num _ => failP
                                                 | LAB'Id x =>
                                                     ((after_label_as_variable
                                                       pat) (VID x))},
                                  2 = fn p => (l,p)}}
fun acc_patrow acc P =
  op ## {1 = op >> {1 = key SYM_DOTS,
                    2 = fn _ => resultP (PATROW (rev acc,{wildcard = true}))},
         2 = op >> {1 = labelled_pat P,
                    2 = fn lp => op ## {1 = op -$$ {1 = key SYM_COMMA,
                                                    2 = (acc_patrow
                                                         (op :: {1 = lp,2 = acc}
                                                         )) P},
                                        2 = resultP
                                            (PATROW (rev (op :: {1 = lp,2 = acc}
                                                         ),{wildcard = false}))}
                   }}
val patrow = acc_patrow []
val underscorePat = (satisfyP "underscorePat")
                    (fn SYM_UNDERSCORE => Some PAT'Wild
                     | _ => None)
fun atpatP im pat =
  op ##
  {1 = op ## {1 = op ## {1 = op ## {1 = op ## {1 = underscorePat,
                                               2 = op >>> {1 = scon,
                                                           2 = PAT'Scon}},
                                    2 = op >>> {1 = op_longvid_noeq im,
                                                2 = PAT'Var}},
                         2 = op >>>
                             {1 = op $$- {1 = op -$$ {1 = key SYM_LB,
                                                      2 = patrow pat},
                                          2 = key SYM_RB},2 = PAT'Record}},
              2 = op >>> {1 = op $$- {1 = op -$$ {1 = key SYM_LP,
                                                  2 = (sequence0 pat)
                                                      (key SYM_COMMA)},
                                      2 = key SYM_RP},2 = fn [] => PAT'Unit
                                                          | [x] => x
                                                          | xs => PAT'Tuple xs}},
   2 = op >>> {1 = op $$- {1 = op -$$ {1 = key SYM_LS,2 = (sequence0 pat)
                                                          (key SYM_COMMA)},
                           2 = key SYM_RS},2 = PAT'List}}
fun constructed_pat longx im pat =
  op ## {1 = op >>> {1 = (atpatP im) pat,2 = fn p => PAT'Cons (longx,p)},
         2 = resultP (PAT'Var longx)}
fun layered_pat im pat =
  op ##
  {1 = op >>
       {1 = op_longvid_noeq im,
        2 = fn longx =>
                 op ##
                 {1 = ((constructed_pat longx) im) pat,
                  2 = case longx of
                        LONG ([],x) =>
                          (op >>
                           {1 = opt (op -$$
                                     {1 = key SYM_COLON,
                                      2 = ty}),
                            2 = fn tOpt => op ## {1 = op >>>
                                                      {1 = op -$$
                                                           {1 = key KEY_as,
                                                            2 = pat},
                                                       2 = fn p => PAT'Layered
                                                                   (x,tOpt,p)},
                                                  2 = case tOpt of
                                                        Some t =>
                                                          (resultP
                                                           (PAT'Typed
                                                            (PAT'Var longx,t)))
                                                      | None => failP}})
                      | _ => failP}},2 = (atpatP im) pat}
fun IGNORED_left_infpat im P =
  ((left ((layered_pat im) P))
   (op ** {1 = vidInfix_noeq im,2 = (layered_pat im) P}))
  (fn (x1,(f,x2)) => makeBinPat (x1,f,x2))
fun right_infpat im P =
  op >> {1 = (layered_pat im) P,
         2 = fn x1 => op ## {1 = op >> {1 = vidInfix_noeq im,
                                        2 = fn f => op >>>
                                                    {1 = (right_infpat im
                                                         ) P,
                                                     2 = fn x2 => makeBinPat
                                                                  (x1,f,x2)}},
                             2 = resultP x1}}
val infpat = right_infpat
fun typed_pat im P = ((left ((infpat im) P)) (op -$$ {1 = key SYM_COLON,2 = ty})
                     ) PAT'Typed
fun pat im = fixP (typed_pat im)
fun atpat im = (atpatP im) (pat im)
fun exprow exp =
  op >>> {1 = (sequence0 (op ** {1 = op $$- {1 = lab,2 = key SYM_EQ},2 = exp}))
              (key SYM_COMMA),2 = EXPROW}
fun mrule im exp =
  op >>> {1 = op ** {1 = op $$- {1 = pat im,2 = key SYM_FUNC_ARROW},2 = exp},
          2 = MRULE}
fun match im exp = op >>> {1 = (sequence ((mrule im) exp)) (key SYM_VBAR),
                           2 = MATCH}
fun atexp1 im exp =
  op ##
  {1 = op >>> {1 = scon,2 = EXP'Scon},
   2 =
   op ##
   {1 = op ##
        {1 = op ##
             {1 = op ## {1 = op ## {1 = op >>>
                                        {1 = op_longvid im,
                                         2 = EXP'Var},
                                    2 = op >>>
                                        {1 = op $$-
                                             {1 = op -$$
                                                  {1 = key SYM_LB,
                                                   2 = exprow exp},
                                              2 = key SYM_RB},
                                         2 = EXP'Record}},
                         2 = op >>> {1 = op -$$ {1 = key SYM_HASH,
                                                 2 = lab},
                                     2 = EXP'Select}},
              2 = op $$-
                  {1 = op -$$
                       {1 = key SYM_LP,
                        2 = op ##
                            {1 = resultP EXP'Unit,
                             2 = op >>
                                 {1 = exp,
                                  2 = fn x1 => op ##
                                               {1 = op >>>
                                                    {1 = many1
                                                         (op -$$
                                                          {1 = key SYM_SEMI,
                                                           2 = exp}),
                                                     2 = fn xs => EXP'Seq
                                                                  (op ::
                                                                   {1 = x1,
                                                                    2 = xs})
                                                    },
                                                2 = op >>>
                                                    {1 = manyP
                                                         (op -$$
                                                          {1 = key SYM_COMMA,
                                                           2 = exp}),
                                                     2 = fn [] => x1
                                                         | xs => EXP'Tuple
                                                                 (op :: {1 = x1,
                                                                         2 = xs}
                                                                 )}}}}},
                   2 = key SYM_RP}},
         2 = op >>>
             {1 = op $$- {1 = op -$$ {1 = key SYM_LS,2 = (sequence0 exp)
                                                         (key SYM_COMMA)},
                          2 = key SYM_RS},2 = EXP'List}},
    2 = op >>> {1 = op $$- {1 = op -$$ {1 = key SYM_HASH_LS,
                                        2 = (sequence0 exp) (key SYM_COMMA)},
                            2 = key SYM_RS},2 = EXP'Vector}}}
fun atexp im (exp,expIM,dfm) =
  op ## {1 = (atexp1 im) exp,
         2 = op >> {1 = op -$$ {1 = key KEY_let,2 = dfm},
                    2 = fn d => op >>>
                                {1 = op $$-
                                     {1 = op -$$
                                          {1 = key KEY_in,
                                           2 = op >>> {1 = (sequence
                                                            (expIM ((app'fm d)
                                                                    im)))
                                                           (key SYM_SEMI),
                                                       2 = fn [x] => x
                                                           | xs => EXP'Seq xs}},
                                      2 = key KEY_end},
                                 2 = fn e => EXP'Let (getFM d,e)}}}
fun appexp im ED = ((left ((atexp im) ED)) ((atexp im) ED)) EXP'App
fun pa_infixVidMatch im tok = case vidMatch tok of
                                None => None
                              | Some x => (case (getIM im) x of
                                             Some pa => (Some (pa,x))
                                           | None => None)
fun pa_vidInfix im = (satisfyP "pa_vidInfix") (pa_infixVidMatch im)
fun reduce_pa (f : (('a * 'b * 'a) -> 'a)) (x1 : 'a)
              (xs : ((pa * 'b) * 'a) list) : 'a option =
  case xs of
    [] => (Some x1)
  | [((_,op12),x2)] => (Some (f (x1,op12,x2)))
  | op :: {1 = ((PA {prec = prec12,assoc = assoc12},op12),x2),
           2 = xs' as op :: {1 = ((PA {prec = prec23,assoc = assoc23},_),x3),
                             2 = _}} =>
      let val reductionDirectionOpt =
              if op > {1 = prec12,2 = prec23}
              then Some LR'Left else if op < {1 = prec12,2 = prec23}
                                     then Some LR'Right
                                     else case (assoc12,assoc23) of
                                            (LR'Left,LR'Left) => (Some LR'Left)
                                          | (LR'Right,LR'Right) =>
                                              (Some LR'Right)
                                          | (LR'Left,LR'Right) => None
                                          | (LR'Right,LR'Left) => None
      in case reductionDirectionOpt of
           None => None
         | Some LR'Left => (((reduce_pa f) (f (x1,op12,x2))) xs')
         | Some LR'Right => (case ((reduce_pa f) x2) xs' of
                               None => None
                             | Some reduced => (Some (f (x1,op12,reduced))))
      end
fun infexp im ED =
  op >> {1 = op >> {1 = (appexp im) ED,
                    2 = fn x1 => op >>> {1 = manyP (op ** {1 = pa_vidInfix im,
                                                           2 = (appexp im) ED}),
                                         2 = fn xs => ((reduce_pa
                                                        (fn (a,b,c) =>
                                                              EXP'App
                                                              (exp'vid b,
                                                               exp'pair (a,c))))
                                                       x1) xs}},
         2 = fn Some e => resultP e
             | None => failP}
fun typed_exp im ED = ((left ((infexp im) ED)) (op -$$ {1 = key SYM_COLON,2 = ty
                                                       })) EXP'Typed
fun andalso_exp im ED = ((left ((typed_exp im) ED))
                         (op -$$ {1 = key KEY_andalso,2 = (typed_exp im) ED}))
                        EXP'Andalso
fun orelse_exp im ED = ((left ((andalso_exp im) ED))
                        (op -$$ {1 = key KEY_orelse,2 = (andalso_exp im) ED}))
                       EXP'Orelse
fun handle_exp im ED exp = ((left ((orelse_exp im) ED))
                            (op -$$ {1 = key KEY_handle,2 = (match im) exp}))
                           EXP'Handle
fun expED im (ED as (exp,_,_)) =
  op ##
  {1 = op ##
       {1 = op ##
            {1 = op ##
                 {1 = op ## {1 = ((handle_exp im) ED)
                                 exp,
                             2 = op >>>
                                 {1 = op -$$
                                      {1 = key KEY_raise,
                                       2 = exp},2 = EXP'Raise}
                            },
                  2 = op >>
                      {1 = op -$$ {1 = key KEY_if,
                                   2 = exp},
                       2 = fn i => op >> {1 = op -$$
                                              {1 = key KEY_then,
                                               2 = exp},
                                          2 = fn t => op >>>
                                                      {1 = op -$$
                                                           {1 = key KEY_else,
                                                            2 = exp},
                                                       2 = fn e => EXP'If
                                                                   (i,t,e)}}}},
             2 = op >> {1 = op -$$ {1 = key KEY_while,2 = exp},
                        2 = fn w => op >>> {1 = op -$$ {1 = key KEY_do,2 = exp},
                                            2 = fn d => EXP'While (w,d)}}},
        2 = op >> {1 = op -$$ {1 = key KEY_case,2 = exp},
                   2 = fn e => op >>> {1 = op -$$ {1 = key KEY_of,
                                                   2 = (match im) exp},
                                       2 = fn m => EXP'Case (e,m)}}},
   2 = op >>> {1 = op -$$ {1 = key KEY_fn,2 = (match im) exp},2 = EXP'Fn}}
fun valbind im exp =
  fixP (fn valbind =>
             op ##
             {1 = op >>
                  {1 = pat im,
                   2 = fn p => op >> {1 = op -$$ {1 = key SYM_EQ,
                                                  2 = exp},
                                      2 = fn e => op >>>
                                                  {1 = opt (op -$$
                                                            {1 = key KEY_and,
                                                             2 = valbind}),
                                                   2 = fn vopt => VALBIND'Seq
                                                                  (p,e,vopt)}}},
              2 = op >>> {1 = op -$$ {1 = key KEY_rec,2 = valbind},
                          2 = VALBIND'Rec}})
fun name_pats im =
  op ##
  {1 = op >> {1 = op -$$ {1 = key KEY_op,2 = vidAnyFix},
              2 = fn name => op >>> {1 = many1 (atpat im),
                                     2 = fn ps => (name,ps)}},
   2 =
   op ##
   {1 =
    op >>
    {1 = op -$$ {1 = key SYM_LP,2 = atpat im},
     2 =
     fn p1 =>
          op >>
          {1 = vidInfix_noeq im,
           2 =
           fn n2 =>
                op >>
                {1 = atpat im,
                 2 =
                 fn p3 =>
                      op -$$
                      {1 = key SYM_RP,
                       2 = op ##
                           {1 = op >>>
                                {1 = manyP
                                     (atpat im),
                                 2 = fn ps => (n2,op ::
                                                  {1 = pat'pair
                                                       (p1,p3),
                                                   2 = ps})},
                            2 = op >>
                                {1 = vidInfix_noeq im,
                                 2 = fn n4 => op >>>
                                              {1 = atpat im,
                                               2 = fn p5 => (n4,[pat'pair
                                                                 (makeBinPat
                                                                  (p1,n2,p3),p5)
                                                                ])}}}}}}},
    2 = op ##
        {1 = op >>
             {1 = vid im,
              2 = fn n1 => op ##
                           {1 = op >> {1 = vidInfix_noeq im,
                                       2 = fn n2 => op >>>
                                                    {1 = atpat im,
                                                     2 = fn p3 => (n2,[pat'pair
                                                                       (pat'vid
                                                                        n1,p3)])
                                                    }},
                            2 = op >>> {1 = many1 (atpat im),
                                        2 = fn ps => (n1,ps)}}},
         2 = op >> {1 = atpat im,
                    2 = fn p1 => op >> {1 = vidInfix_noeq im,
                                        2 = fn n2 => op >>>
                                                     {1 = atpat im,
                                                      2 = fn p3 => (n2,[pat'pair
                                                                        (p1,p3)]
                                                                   )}}}}}}
fun name_pats_reqName name im =
  op >> {1 = name_pats im,2 = fn (name',ps) => if op = {1 = name,2 = name'}
                                               then resultP ps else failP}
fun clause_body (ps : pat list) exp =
  op >> {1 = opt (op -$$ {1 = key SYM_COLON,2 = ty}),
         2 = fn topt => op >>> {1 = op -$$ {1 = key SYM_EQ,2 = exp},
                                2 = fn e => (ps,topt,e)}}
fun clause im exp =
  op >> {1 = name_pats im,2 = fn (name,ps) => op >>> {1 = (clause_body ps) exp,
                                                      2 = fn cl => (name,cl)}}
fun clause_reqName name im exp = op >> {1 = (name_pats_reqName name) im,
                                        2 = fn ps => (clause_body ps) exp}
fun fvalbind im E =
  op >>> {1 = (sequence
               (op >> {1 = (clause im) E,
                       2 = fn (name,clause1) =>
                                op >>> {1 = manyP (op -$$ {1 = key SYM_VBAR,
                                                           2 = ((clause_reqName
                                                                 name) im) E}),
                                        2 = fn clauses =>
                                                 (name,op :: {1 = clause1,
                                                              2 = clauses})}}))
              (key KEY_and),2 = FVALBIND}
val typbind =
    op >>>
    {1 = (sequence
          (op >> {1 = tyvarseq,
                  2 = fn tvs => op >> {1 = tycon,
                                       2 = fn tc => op >>>
                                                    {1 = op -$$ {1 = key SYM_EQ,
                                                                 2 = ty},
                                                     2 = fn t => (tvs,tc,t)}}}))
         (key KEY_and),2 = TYPBIND}
fun dat_bindOrDesc id =
  (sequence
   (op >>
    {1 = tyvarseq,
     2 = fn tvs => op >>
                   {1 = tycon,
                    2 = fn tc => op >>>
                                 {1 = op -$$
                                      {1 = key SYM_EQ,
                                       2 = (sequence
                                            (op ** {1 = id,
                                                    2 = opt (op -$$
                                                             {1 = key KEY_of,
                                                              2 = ty})}))
                                           (key SYM_VBAR)},
                                  2 = fn vcons => (tvs,tc,vcons)}}}))
  (key KEY_and)
fun datbind im = op >>> {1 = dat_bindOrDesc (op_vid im),2 = DATBIND}
fun exbind1 im =
  op >> {1 = op_vid im,
         2 = fn x => op ## {1 = op >>>
                                {1 = opt (op -$$ {1 = key KEY_of,2 = ty}),
                                 2 = fn topt => EXBIND1'New (x,topt)},
                            2 = op >>> {1 = op -$$ {1 = key SYM_EQ,
                                                    2 = op_longvid im},
                                        2 = fn y => EXBIND1'Copy (x,y)}}}
fun exbind im = op >>> {1 = (sequence (exbind1 im)) (key KEY_and),2 = EXBIND}
val digit = (satisfyP "digit")
            (fn TOK'Number n => if (op <= {1 = 0,2 = n})
                                   andalso (op <= {1 = n,2 = 9}) then Some n
                                else None
             | _ => None)
val precOpt = op >>> {1 = opt digit,2 = fn Some n => n
                                        | None => 0}
fun dec1E im E =
  op ##
  {1 = op ##
       {1 = op ##
            {1 = op ## {1 = op ## {1 = op >>>
                                       {1 = op -$$
                                            {1 = key KEY_val,
                                             2 = (valbind im)
                                                 E},
                                        2 = DEC'Val},
                                   2 = op >>>
                                       {1 = op -$$
                                            {1 = key KEY_fun,
                                             2 = (fvalbind im)
                                                 E},
                                        2 = DEC'Fval}},
                        2 = op >>>
                            {1 = op -$$ {1 = key KEY_type,
                                         2 = typbind},
                             2 = DEC'Type}},
             2 = op -$$
                 {1 = key KEY_datatype,
                  2 = op ## {1 = op >>>
                                 {1 = op ** {1 = noErrorP
                                                 (op $$-
                                                  {1 = op $$-
                                                       {1 = tycon,
                                                        2 = key SYM_EQ},
                                                   2 = key KEY_datatype}),
                                             2 = longtycon},
                                  2 = DEC'DataReplicate},
                             2 = op >>>
                                 {1 = op ** {1 = datbind im,
                                             2 = opt (op -$$
                                                      {1 = key KEY_withtype,
                                                       2 = typbind})},
                                  2 = DEC'Datatype}}}},
        2 = op >>> {1 = op -$$ {1 = key KEY_exception,2 = exbind im},
                    2 = DEC'Exception}},
   2 = op >>> {1 = op -$$ {1 = key KEY_open,2 = many1 longstrid},2 = DEC'Open}}
val fixityDec =
    op ## {1 = op >> {1 = op ## {1 = op -$$ {1 = key KEY_infix,
                                             2 = resultP LR'Left},
                                 2 = op -$$ {1 = key KEY_infixr,
                                             2 = resultP LR'Right}},
                      2 = fn lr => op >> {1 = precOpt,
                                          2 = fn d => op >>>
                                                      {1 = many1 vidAnyFix,
                                                       2 = fn xs => ((dfm'infix
                                                                      lr) d) xs}
                                         }},
           2 = op >>> {1 = op -$$ {1 = key KEY_nonfix,2 = many1 vidAnyFix},
                       2 = dfm'nonfix}}
fun dfm1DE im (dfm,dfmIM,expIM) =
  op ##
  {1 = op ## {1 = op ## {1 = op >>> {1 = (dec1E im) (expIM im),2 = makeFM},
                         2 = fixityDec},
              2 = op >> {1 = op ** {1 = op -$$ {1 = key KEY_abstype,
                                                2 = datbind im},
                                    2 = opt (op -$$ {1 = key KEY_withtype,
                                                     2 = typbind})},
                         2 = fn (dat,wo) =>
                                  op >>>
                                  {1 = op $$- {1 = op -$$ {1 = key KEY_with,
                                                           2 = dfm},
                                               2 = key KEY_end},
                                   2 = map'fm (fn d => DEC'Abstype (dat,wo,d))}}
             },
   2 = op >> {1 = op -$$ {1 = key KEY_local,2 = dfm},
              2 = fn d1 => op >>> {1 = op $$- {1 = op -$$ {1 = key KEY_in,
                                                           2 = dfmIM ((app'fm d1
                                                                      ) im)},
                                               2 = key KEY_end},
                                   2 = map'fm (fn d2 => DEC'Local (getFM d1,d2))
                                  }}}
fun acc_dfmDE acc (sep : 'a par) im DE =
  op ## {1 = resultP acc,
         2 = op >> {1 = sep,2 = fn _ => op >> {1 = (dfm1DE im) DE,
                                               2 = fn d => (((acc_dfmDE
                                                              ((fm'seq DEC'Seq)
                                                               (acc,d))) sep)
                                                            ((app'fm d) im)) DE}
                   }}
fun dfmDE_sep sep im DE = (((acc_dfmDE (makeFM DEC'Empty)) sep) im) DE
val semis = manyP (key SYM_SEMI)
val null = resultP ()
fun dfm im = fixP (fn D => ((dfmDE_sep semis) im) (D,dfm,exp))
and exp im = fixP (fn E => (expED im) (E,exp,dfm im))
fun dfm1 im = (dfm1DE im) (dfm im,dfm,exp)
val sigid = (satisfyP "sigid") (fn TOK'Ident s => Some (SIGID (ID s))
                                | _ => None)
val funid = (satisfyP "funid") (fn TOK'Ident s => Some (FUNID (ID s))
                                | _ => None)
val valdesc = (sequence (op ** {1 = op $$- {1 = vidAnyFix,2 = key SYM_COLON},
                                2 = ty})) (key KEY_and)
val typdesc = (sequence (op ** {1 = tyvarseq,2 = tycon})) (key KEY_and)
val datadesc = dat_bindOrDesc vidAnyFix
val exdesc = (sequence (op ** {1 = vidAnyFix,2 = opt (op -$$ {1 = key KEY_of,
                                                              2 = ty})}))
             (key SYM_VBAR)
fun strdesc sigexp = (sequence (op ** {1 = op $$- {1 = strid,2 = key SYM_COLON},
                                       2 = sigexp})) (key KEY_and)
fun spec1 sigexp =
  op ##
  {1 = op >>> {1 = op -$$ {1 = key KEY_val,2 = valdesc},
               2 = SPEC'Val},
   2 = op ##
       {1 = op >>> {1 = op -$$ {1 = key KEY_type,2 = typdesc},
                    2 = SPEC'Type},
        2 = op ##
            {1 = op ## {1 = op >>>
                            {1 = op -$$ {1 = key KEY_eqtype,
                                         2 = typdesc},
                             2 = SPEC'Eqtype},
                        2 = op ## {1 = op >>>
                                       {1 = op -$$
                                            {1 = key KEY_datatype,
                                             2 = datadesc},
                                        2 = SPEC'Datatype},
                                   2 = op >>>
                                       {1 = op ** {1 = op $$-
                                                       {1 = op $$-
                                                            {1 = tycon,
                                                             2 = key SYM_EQ},
                                                        2 = key KEY_datatype},
                                                   2 = longtycon},
                                        2 = SPEC'DataReplicate}}},
             2 = op ## {1 = op >>> {1 = op -$$ {1 = key KEY_exception,2 = exdesc
                                               },2 = SPEC'Exception},
                        2 = op ## {1 = op >>> {1 = op -$$ {1 = key KEY_include,
                                                           2 = sigexp},
                                               2 = SPEC'Include},
                                   2 = op >>>
                                       {1 = op -$$ {1 = key KEY_structure,
                                                    2 = strdesc sigexp},
                                        2 = SPEC'Structure}}}}}}
val sharing_type = op -$$ {1 = op -$$ {1 = key KEY_sharing,2 = key KEY_type},
                           2 = (sequence longtycon) (key SYM_EQ)}
fun acc_specs S sp =
  op -$$ {1 = semis,
          2 = op ## {1 = resultP sp,
                     2 = op ## {1 = op >> {1 = sharing_type,
                                           2 = fn tcs => (acc_specs S)
                                                         (SPEC'Sharing
                                                          (sp,tcs))},
                                2 = op >> {1 = spec1 S,
                                           2 = fn sp' => (acc_specs S)
                                                         (SPEC'Seq (sp,sp'))}}}}
fun specS S = op >> {1 = resultP SPEC'Empty,2 = acc_specs S}
fun sigexp1 S = op ## {1 = op >>> {1 = op $$- {1 = op -$$ {1 = key KEY_sig,
                                                           2 = specS S},
                                               2 = key KEY_end},2 = SIG'Spec},
                       2 = op >>> {1 = sigid,2 = SIG'Id}}
val where_type =
    op >> {1 = op -$$ {1 = op -$$ {1 = key KEY_where,
                                   2 = key KEY_type},2 = tyvarseq},
           2 = fn tvs => op >> {1 = longtycon,
                                2 = fn tc => op >>> {1 = op -$$ {1 = key SYM_EQ,
                                                                 2 = ty},
                                                     2 = fn t => (tvs,tc,t)}}}
fun sigexpS S = ((left (sigexp1 S)) where_type) SIG'TypeRealisation
val sigexp = fixP sigexpS
val spec = specS sigexp
val funid_LP = noErrorP (op $$- {1 = funid,2 = key SYM_LP})
fun strexp1DE im (strexp,strexpIM,strdecfm) =
  op ## {1 = op >>>
             {1 = op $$-
                  {1 = op ** {1 = funid_LP,
                              2 = op ## {1 = strexp,
                                         2 = op >>> {1 = op >>> {1 = strdecfm,
                                                                 2 = getFM},
                                                     2 = STREXP'Struct}}},
                   2 = key SYM_RP},2 = STREXP'FunctorApp},
         2 = op ## {1 = op ## {1 = op >>> {1 = longstrid,2 = STREXP'Id},
                               2 = op >>>
                                   {1 = op >>>
                                        {1 = op $$-
                                             {1 = op -$$ {1 = key KEY_struct,
                                                          2 = strdecfm},
                                              2 = key KEY_end},2 = getFM},
                                    2 = STREXP'Struct}},
                    2 = op >> {1 = op -$$ {1 = key KEY_let,2 = strdecfm},
                               2 = fn d => op >>>
                                           {1 = op $$-
                                                {1 = op -$$ {1 = key KEY_in,
                                                             2 = strexpIM
                                                                 ((app'fm d) im)
                                                            },2 = key KEY_end},
                                            2 = fn e => STREXP'Let (getFM d,e)}}
                   }}
val signatureConstraint =
    op ## {1 = op -$$ {1 = key SYM_COLON,2 = resultP STREXP'TransCon},
           2 = op -$$ {1 = key SYM_COLON_GT,2 = resultP STREXP'OpaqueCon}}
fun strexpDE im DE = ((left ((strexp1DE im) DE))
                      (op ** {1 = signatureConstraint,2 = sigexp}))
                     (fn (e,(F,s)) => F (e,s))
fun bindStrexpWithDerivedSignatureConstraintForm E =
  op >>> {1 = op ** {1 = opt (op ** {1 = signatureConstraint,2 = sigexp}),
                     2 = op -$$ {1 = key SYM_EQ,2 = E}},
          2 = fn (None,e) => e
              | (Some (F,s),e) => F (e,s)}
fun strbind E =
  (sequence (op ** {1 = strid,2 = bindStrexpWithDerivedSignatureConstraintForm E
                   })) (key KEY_and)
fun strdecfm1DE im (strdecfm,strdecfmIM,E) =
  op ## {1 = op >>> {1 = dfm1 im,2 = map'fm STRDEC'Dec},
         2 = op ## {1 = op >>> {1 = op >>> {1 = op -$$ {1 = key KEY_structure,
                                                        2 = strbind E},
                                            2 = STRDEC'Structure},2 = makeFM},
                    2 = op >> {1 = op -$$ {1 = key KEY_local,2 = strdecfm},
                               2 = fn d1 => op >>>
                                            {1 = op $$-
                                                 {1 = op -$$ {1 = key KEY_in,
                                                              2 = strdecfmIM
                                                                  ((app'fm d1)
                                                                   im)},
                                                  2 = key KEY_end},
                                             2 = map'fm (fn d2 => STRDEC'Local
                                                                  (getFM d1,d2))
                                            }}}}
fun acc_strdecfmDE acc (sep : 'a par) im DE =
  op -$$ {1 = sep,2 = op ## {1 = resultP acc,
                             2 = op >> {1 = (strdecfm1DE im) DE,
                                        2 = fn d => (((acc_strdecfmDE
                                                       ((fm'seq STRDEC'Seq)
                                                        (acc,d))) sep)
                                                     ((app'fm d) im)) DE}}}
fun strdecfmDE_sep sep im DE = (((acc_strdecfmDE (makeFM STRDEC'Empty)) sep) im)
                               DE
fun strdecfmE E im = fixP (fn D => ((strdecfmDE_sep semis) im) (D,strdecfmE E,E)
                          )
fun strexp im = fixP (fn E => (strexpDE im) (E,strexp,(strdecfmE E) im))
fun strdecfm im = (strdecfmE (strexp im)) im
fun strdecfm_noSemiPrefix im =
  ((strdecfmDE_sep null) im) (strdecfm im,strdecfm,strexp im)
val sigdec =
    op >>> {1 = op -$$ {1 = key KEY_signature,
                        2 = (sequence (op ** {1 = op $$- {1 = sigid,
                                                          2 = key SYM_EQ},
                                              2 = sigexp})) (key KEY_and)},
            2 = SIGDEC}
val funImport =
    op ## {1 = op >> {1 = strid,
                      2 = fn x => op >>> {1 = op -$$ {1 = key SYM_COLON,
                                                      2 = sigexp},
                                          2 = fn X => FUNIMPORT'Basic (x,X)}},
           2 = op >>> {1 = spec,2 = FUNIMPORT'Derived}}
fun fundec im =
  op >>>
  {1 =
   op -$$
   {1 = key KEY_functor,
    2 =
    (sequence
     (op >>
      {1 = funid,
       2 =
       fn F => op >>
               {1 = op -$$ {1 = key SYM_LP,
                            2 = funImport},
                2 =
                fn I => op >>>
                        {1 = op -$$
                             {1 = key SYM_RP,
                              2 = bindStrexpWithDerivedSignatureConstraintForm
                                  (strexp im)},2 = fn S => (F,I,S)}}}))
    (key KEY_and)},2 = FUNDEC}
fun topdec_item im =
  op ## {1 = op >>> {1 = exp im,2 = TOPDEC'Exp},
         2 = op ## {1 = op >>> {1 = op >>> {1 = strdecfm_noSemiPrefix im,
                                            2 = getFM},2 = TOPDEC'Strdec},
                    2 = op ## {1 = op >>> {1 = sigdec,2 = TOPDEC'Sigdec},
                               2 = op >>> {1 = fundec im,2 = TOPDEC'Fundec}}}}
fun topdec im = many1 (topdec_item im)
fun prog im = op >>> {1 = (sequence (topdec im)) (key SYM_SEMI),2 = link}
end : ParserSig
;

