

signature LexSig = sig type 'a llist
                       val iterateLL : (('a -> ('b * 'a) option) ->
                                        ('a -> 'b llist))
                       val appLL : (('a -> unit) -> ('a llist -> unit))
                       val lazyFile : ({path : string} -> string llist)
                       val lazyString : (string -> string llist)
                       datatype pos = POS of {row : int,col : int}
                       type lexState
                       type tok
                       val initLS : (pos -> (string llist -> lexState))
                       val stepLS : (lexState -> (tok * lexState) option)
                       val posLS : (lexState -> pos)
                   end
signature PrelSig_Import = sig val stringOfInt : (int -> string)
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
                          end
functor
LEX (structure Prel : PrelSig_Import
     structure Tok : TokSig_Import) =
struct
open Prel
open Tok
fun tabCol x = op *  {1 = 8,2 = op + {1 = 1,2 = op div {1 = x,2 = 8}}}
datatype pos = POS of {row : int,col : int}
fun stepPos (POS {row = row,col = col}) c =
  case c of
    \n => (POS {row = op + {1 = row,2 = 1},col = 0})
  | \t => (POS {row = row,col = tabCol col})
  | _ => (POS {row = row,col = op + {1 = col,2 = 1}})
local datatype 'a lazyVal = Thunk of (unit -> 'a) | Value of 'a
in abstype 'a lazy = Lazy of 'a lazyVal ref
   with fun lazy v = Lazy (ref (Value v))
        fun delay thunk = Lazy (ref (Thunk thunk))
        fun force (Lazy r) =
          case ! r of
            Thunk thunk => let val v = thunk ()
                           in (op := {1 = r,2 = Value v} ;v)
                           end
          | Value v => v
   end
end
datatype 'a llist = LNil | LCons of ('a * 'a llist lazy)
val rec iterateLL : (('a -> ('b * 'a) option) -> ('a -> 'b llist)) =
        fn f => fn a => case f a of
                          None => LNil
                        | Some (b,a) =>
                            (LCons (b,delay (fn () => (iterateLL f) a)))
val rec appLL : (('a -> unit) -> ('a llist -> unit)) =
        fn f => fn ll => case ll of
                           LNil => ()
                         | LCons (x,ll) => (f x ;(appLL f) (force ll))
val FILE_CHUNK = 1000
fun lazyFile {path = path} =
  let open TextIO
      val instream = openIn path
      fun get () = let val s : string = inputN (instream,FILE_CHUNK)
                   in if op = {1 = s,2 = ""}
                      then (closeIn instream ;LNil)
                      else LCons (s,delay get)
                   end
  in get ()
  end
fun lazyString string = LCons (string,delay (fn () => LNil))
abstype lexState = LS of (pos * int * string * int * string llist lazy)
with fun initLS pos ll = LS (pos,0,"",0,lazy ll)
     fun posLS (LS (pos,_,_,_,_)) = pos
     fun nextLS (LS (pos,n,s,z,lll)) =
       if op >= {1 = n,2 = z}
       then case force lll of
              LNil => None
            | LCons (s,lll) => (nextLS (LS (pos,0,s,size s,lll)))
       else let val c = String.sub (s,n)
                val pos' = (stepPos pos) c
                val ls' = LS (pos',op + {1 = n,2 = 1},s,z,lll)
            in Some (c,ls')
            end
end
fun nextChar ls = case nextLS ls of
                    None => None
                  | Some (c,_) => (Some c)
fun next2Chars ls = case nextLS ls of
                      None => None
                    | Some (c1,ls) => (case nextLS ls of
                                         None => None
                                       | Some (c2,_) => (Some (c1,c2)))
fun isAlpha c = let open Char
                in ((op >= {1 = c,2 = A}) andalso (op <= {1 = c,2 = Z}))
                   orelse ((op >= {1 = c,2 = a}) andalso
                           (op <= {1 = c,2 = z}))
                end
fun isDigit c = let open Char
                in (op >= {1 = c,2 = 0}) andalso (op <= {1 = c,2 = 9})
                end
fun isWhite c = let open Char
                in (((op = {1 = c,2 =  }) orelse (op = {1 = c,2 = \t}))
                    orelse (op = {1 = c,2 = \n})) orelse
                   (op = {1 = c,2 = \013})
                end
fun isIdent c = (((isAlpha c) orelse (isDigit c)) orelse
                 (op = {1 = c,2 = '})) orelse (op = {1 = c,2 = _})
fun mkIdentOrKey s = case s of
                       "abstype" => KEY_abstype
                     | "and" => KEY_and
                     | "andalso" => KEY_andalso
                     | "as" => KEY_as
                     | "case" => KEY_case
                     | "do" => KEY_do
                     | "datatype" => KEY_datatype
                     | "else" => KEY_else
                     | "end" => KEY_end
                     | "exception" => KEY_exception
                     | "fn" => KEY_fn
                     | "fun" => KEY_fun
                     | "handle" => KEY_handle
                     | "if" => KEY_if
                     | "in" => KEY_in
                     | "infix" => KEY_infix
                     | "infixr" => KEY_infixr
                     | "let" => KEY_let
                     | "local" => KEY_local
                     | "nonfix" => KEY_nonfix
                     | "of" => KEY_of
                     | "op" => KEY_op
                     | "open" => KEY_open
                     | "orelse" => KEY_orelse
                     | "raise" => KEY_raise
                     | "rec" => KEY_rec
                     | "then" => KEY_then
                     | "type" => KEY_type
                     | "val" => KEY_val
                     | "with" => KEY_with
                     | "withtype" => KEY_withtype
                     | "while" => KEY_while
                     | "eqtype" => KEY_eqtype
                     | "functor" => KEY_functor
                     | "include" => KEY_include
                     | "sharing" => KEY_sharing
                     | "sig" => KEY_sig
                     | "signature" => KEY_signature
                     | "struct" => KEY_struct
                     | "structure" => KEY_structure
                     | "where" => KEY_where
                     | _ => (TOK'Ident s)
fun isSymid c = (((((((((((((((((((op = {1 = c,2 = !}) orelse
                                  (op = {1 = c,2 = %})) orelse
                                 (op = {1 = c,2 = &})) orelse
                                (op = {1 = c,2 = $})) orelse
                               (op = {1 = c,2 = #})) orelse
                              (op = {1 = c,2 = +})) orelse
                             (op = {1 = c,2 = -})) orelse
                            (op = {1 = c,2 = /})) orelse
                           (op = {1 = c,2 = :})) orelse
                          (op = {1 = c,2 = <})) orelse
                         (op = {1 = c,2 = =})) orelse
                        (op = {1 = c,2 = >})) orelse
                       (op = {1 = c,2 = ?})) orelse (op = {1 = c,2 = @})
                     ) orelse (op = {1 = c,2 = \\})) orelse
                    (op = {1 = c,2 = ~})) orelse (op = {1 = c,2 = `}))
                  orelse (op = {1 = c,2 = ^})) orelse
                 (op = {1 = c,2 = |})) orelse (op = {1 = c,2 = *})
fun mkIdentOrSym s = case s of
                       ":" => SYM_COLON
                     | "|" => SYM_VBAR
                     | "=" => SYM_EQ
                     | "#" => SYM_HASH
                     | "*" => SYM_STAR
                     | "=>" => SYM_FUNC_ARROW
                     | "->" => SYM_TYPE_ARROW
                     | ":>" => SYM_COLON_GT
                     | _ => (TOK'Symid s)
abstype lexer = LEXER of (lexState -> (tok * lexState) option)
with fun execLexer (LEXER x) = x
     val fixLexer : ((lexer -> lexer) -> lexer) =
         fn f => let val rec lexer = fn ls => (execLexer (f (LEXER lexer
                                                            ))) ls
                 in LEXER lexer
                 end
     fun emit tok = LEXER (fn ls => Some (tok,ls))
     val stop = LEXER (fn _ => None)
     val getOpt : ((char option -> lexer) -> lexer) =
         fn f => LEXER (fn ls => case nextLS ls of
                                   None => ((execLexer (f None)) ls)
                                 | Some (c,ls') =>
                                     ((execLexer (f (Some c))) ls'))
     val peekOpt : ((char option -> lexer) -> lexer) =
         fn f => LEXER (fn ls => (execLexer (f (nextChar ls))) ls)
     val peekOpt2 : (((char * char) option -> lexer) -> lexer) =
         fn f => LEXER (fn ls => (execLexer (f (next2Chars ls))) ls)
end
fun delayLexer thunk = fixLexer (fn _ => thunk ())
val drop : (lexer -> lexer) = fn lexer => getOpt (fn None => stop
                                                  | Some _ => lexer)
val getIf : ((char -> bool) -> ((char option -> lexer) -> lexer)) =
    fn pred => fn f => peekOpt (fn None => f None
                                | Some c => if pred c
                                            then drop (f (Some c))
                                            else f None)
val getIfEq : (char -> ((lexer * lexer) -> lexer)) =
    fn char => fn (yes,no) => (getIf (fn c => op = {1 = c,2 = char}))
                              (fn None => no
                               | Some _ => yes)
val getIf2 : (((char * char) -> bool) -> (((char * char) option -> lexer
                                          ) -> lexer)) =
    fn pred => fn f => peekOpt2
                       (fn None => f None
                        | Some cc => if pred cc
                                     then drop (drop (f (Some cc)))
                                     else f None)
val getIfEq2 : ((char * char) -> ((lexer * lexer) -> lexer)) =
    fn charChar =>
         fn (yes,no) => (getIf2 (fn cc => op = {1 = cc,2 = charChar}))
                        (fn None => no
                         | Some _ => yes)
fun err s = emit (TOK'Err s)
fun symidAcc (acc : char list) =
  (getIf isSymid) (fn None => emit (mkIdentOrSym (implode (rev acc)))
                   | Some c => symidAcc (op :: {1 = c,2 = acc}))
fun identAcc (acc : char list) =
  (getIf isIdent) (fn None => emit (mkIdentOrKey (implode (rev acc)))
                   | Some c => identAcc (op :: {1 = c,2 = acc}))
fun tyvarAcc (acc : char list) =
  (getIf isIdent) (fn None => emit (TOK'TyVar (implode (rev acc)))
                   | Some c => tyvarAcc (op :: {1 = c,2 = acc}))
val ord0 = ord 0
fun numOfChar c = op - {1 = ord c,2 = ord0}
fun numeralAcc (makeTok : (int -> tok)) (acc : int) =
  (getIf isDigit) (fn None => emit (makeTok acc)
                   | Some c => (numeralAcc makeTok)
                               (op + {1 = op *  {1 = 10,2 = acc},
                                      2 = numOfChar c}))
val numberAcc = numeralAcc TOK'Number
val wordAcc = numeralAcc (fn i => TOK'Word (Word.fromInt i))
val ordAt = ord @
fun controlChar c = chr (op - {1 = ord c,2 = ordAt})
fun afterOpenQuote {contNone = contNone : lexer,contQ = contQ : lexer,
                    contNQ = contNQ : (char -> lexer)} =
  getOpt
  (fn None => contNone
   | Some \" => contQ
   | Some \\ =>
       getOpt
       (fn None => err "unterminated backslash in string literal"
        | Some t => contNQ \t
        | Some n => contNQ \n
        | Some \\ => contNQ \\
        | Some \" => contNQ \"
        | Some ^ =>
            getOpt (fn None => err "illegal control escape in string literal"
                    | Some c => contNQ (controlChar c))
        | Some c =>
            if isDigit c
            then (getIf2 (fn (c2,c3) => (isDigit c2) andalso
                                        (isDigit c3)))
                 (fn None => err "illegal ascii escape in string literal"
                  | Some (c2,c3) =>
                      let val n = op + {1 = op *  {1 = 100,2 = numOfChar c},
                                        2 = op + {1 = op *  {1 = 10,
                                                             2 = numOfChar c2},
                                                  2 = numOfChar c3}}
                      in if op < {1 = n,2 = 256} then contNQ (chr n)
                         else err "illegal ascii escape in string literal"
                      end) else err "illegal escape in string literal")
   | Some c => contNQ c)
fun inStringLit (acc : char list) =
  afterOpenQuote
  {contNone = err "unterminated string literal",
   contQ = delayLexer (fn () => emit (TOK'StringLit (implode (rev acc)))),
   contNQ = fn c => inStringLit (op :: {1 = c,2 = acc})}
fun closeCharLit (c : char) = getOpt (fn None => err "unterminated char literal"
                                      | Some \" => emit (TOK'CharLit c)
                                      | Some _ => err "char literal: size>1")
val inCharLit = afterOpenQuote {contNone = err "unterminated char literal",
                                contQ = err "char literal: size=0",
                                contNQ = closeCharLit}
fun inComment (cont : lexer) (level : int) =
  if op = {1 = level,2 = 0} then cont
  else let val afterLP =
               fixLexer (fn afterLP => getOpt (fn None => err "unclosed comment"
                                               | Some ( => afterLP
                                               | Some * => (inComment cont)
                                                           (op + {1 = level,
                                                                  2 = 1})
                                               | Some _ => (inComment cont)
                                                           level))
           val afterStar =
               fixLexer (fn afterStar =>
                              getOpt (fn None => err "unclosed comment"
                                      | Some * => afterStar
                                      | Some ) => (inComment cont)
                                                  (op - {1 = level,2 = 1})
                                      | Some _ => (inComment cont) level))
       in getOpt (fn None => err "unclosed comment"
                  | Some ( => afterLP
                  | Some * => afterStar
                  | Some _ => (inComment cont) level)
       end
val step : lexer =
    fixLexer
    (fn step =>
          getOpt
          (fn None => stop
           | Some c =>
               if isWhite c then step
               else if op = {1 = c,2 = #}
                    then peekOpt
                         (fn None => emit SYM_HASH
                          | Some \" =>
                              drop inCharLit
                          | Some [ => drop (emit SYM_HASH_LS)
                          | Some _ => symidAcc [c])
                    else if isSymid c then symidAcc [c]
                         else if isAlpha c then identAcc [c]
                              else if op = {1 = c,2 = '}
                                   then tyvarAcc []
                                   else if op = {1 = c,2 = 0}
                                        then (getIf2
                                              (fn (c1,c2) =>
                                                    (op = {1 = c1,2 = w})
                                                    andalso (isDigit c2)))
                                             (fn None => numberAcc 0
                                              | Some (_,c2) =>
                                                  wordAcc (numOfChar c2))
                                        else if isDigit c
                                             then numberAcc (numOfChar c)
                                             else case c of
                                                    ( => ((getIfEq *)
                                                          ((inComment step
                                                           ) 1,emit SYM_LP
                                                          ))
                                                  | ) => (emit SYM_RP)
                                                  | [ => (emit SYM_LS)
                                                  | ] => (emit SYM_RS)
                                                  | { => (emit SYM_LB)
                                                  | } => (emit SYM_RB)
                                                  | , => (emit SYM_COMMA)
                                                  | ; => (emit SYM_SEMI)
                                                  | _ => (emit SYM_UNDERSCORE)
                                                  | \" => (inStringLit [])
                                                  | . => ((getIfEq2 (.,.))
                                                          (emit SYM_DOTS,
                                                           emit SYM_DOT))
                                                  | _ => (emit (TOK'Unknown c)))
    )
fun sofPos (POS {row = row,col = col}) =
  op ^ {1 = op ^ {1 = stringOfInt row,2 = ":"},2 = stringOfInt col}
fun echo s = (TextIO.output (TextIO.stdOut,op ^ {1 = s,2 = "\n"}) ;
              TextIO.flushOut TextIO.stdOut)
fun stepLS ls = let val U = echo (sofPos (posLS ls))
                    val res = (execLexer step) ls
                in res
                end
val stepLS = execLexer step
end : LexSig
;

