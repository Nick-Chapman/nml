

signature PfvSig = sig type 'a par
                       val resultP : ('a -> 'a par)
                       val failP : 'a par
                       val bindP : (('a par * ('a -> 'b par)) -> 'b par)
                       val altP : (('a par * 'a par) -> 'a par)
                       val fixP : (('a par -> 'a par) -> 'a par)
                       val manyP : ('a par -> 'a list par)
                       val noErrorP : ('a par -> 'a par)
                       type tok
                       val satisfyP : (string -> ((tok -> 'a option) -> 'a par))
                       type lexState
                       val execP : ('a par -> (lexState -> (lexState * 'a option
                                                           )))
                   end
signature PVF_LexSig = sig type lexState
                           type tok
                           val stepLS : (lexState -> (tok * lexState) option)
                       end
signature TokSig_Import = sig type tok
                              val sofTok : (tok -> string)
                          end
functor PFV (structure Lex : PVF_LexSig
             structure Tok : TokSig_Import sharing type Lex.tok = Tok.tok) =
        struct
        open Lex
        open Tok
        abstype cachingLexState =
                CLS of (lexState * {res : (tok * cachingLexState) option} option
                                   ref)
        with fun initCLS ls = CLS (ls,ref None)
             fun endCLS (CLS (ls,_)) = ls
             fun stepCLS (CLS (ls,r)) =
               case ! r of
                 Some {res = res} => res
               | None => let val res = case stepLS ls of
                                         Some (tok,ls) => (Some (tok,initCLS ls)
                                                          )
                                       | None => None
                         in (op := {1 = r,2 = Some {res = res}} ;res)
                         end
        end
        datatype 'a result = RES'Succ of (cachingLexState * 'a)
                             | RES'Epsilon of 'a | RES'Error of cachingLexState
                             | RES'Fail
        datatype 'a sofar = SOFAR'Epsilon of 'a | SOFAR'Fail | SOFAR'Succ of 'a
                            | SOFAR'Error
        datatype 'a par = Parser of ((cachingLexState * 'a sofar) -> 'a result)
        fun deParser (Parser f) = f
        fun fixP_v1 f = let fun fix ls = (deParser (f (Parser fix))) ls
                        in Parser fix
                        end
        exception FixP
        fun fixP f = let val fRef = ref (fn _ => raise FixP)
                         val p = Parser (fn x => (! fRef) x)
                         val p' = f p
                         val U : unit = op := {1 = fRef,2 = deParser p'}
                     in p'
                     end
        fun restore ls sofar = case sofar of
                                 SOFAR'Epsilon x => (RES'Epsilon x)
                               | SOFAR'Fail => RES'Fail
                               | SOFAR'Succ x => (RES'Succ (ls,x))
                               | SOFAR'Error => (RES'Error ls)
        fun makeResult sofar ls x = case sofar of
                                      SOFAR'Epsilon _ => (RES'Epsilon x)
                                    | SOFAR'Fail => (RES'Epsilon x)
                                    | SOFAR'Succ _ => (RES'Succ (ls,x))
                                    | SOFAR'Error => (RES'Succ (ls,x))
        fun satisfyP who f =
          Parser (fn (ls,sofar) => case stepCLS ls of
                                     None => ((restore ls) sofar)
                                   | Some (tok,ls') =>
                                       (case let val xopt = f tok
                                             in xopt
                                             end of
                                          None => ((restore ls) sofar)
                                        | Some x => (RES'Succ (ls',x))))
        val failP = Parser (fn (ls,sofar) => (restore ls) sofar)
        fun resultP x = Parser (fn (ls,sofar) => ((makeResult sofar) ls) x)
        fun addConsume sofar = case sofar of
                                 SOFAR'Epsilon x => (SOFAR'Succ x)
                               | SOFAR'Fail => SOFAR'Error
                               | SOFAR'Succ x => (SOFAR'Succ x)
                               | SOFAR'Error => SOFAR'Error
        fun bindP (par,f) =
          Parser (fn (ls,sofar) => case (deParser par) (ls,SOFAR'Fail) of
                                     RES'Epsilon x => ((deParser (f x))
                                                       (ls,sofar))
                                   | RES'Succ (ls,x) =>
                                       ((deParser (f x)) (ls,addConsume sofar))
                                   | RES'Fail => ((restore ls) sofar)
                                   | RES'Error ls => (RES'Error ls))
        fun checkpoint sofar = case sofar of
                                 SOFAR'Epsilon x => (SOFAR'Epsilon x)
                               | SOFAR'Fail => SOFAR'Fail
                               | SOFAR'Succ x => (SOFAR'Epsilon x)
                               | SOFAR'Error => SOFAR'Fail
        fun addAlt sofar x = case sofar of
                               SOFAR'Epsilon _ => (SOFAR'Epsilon x)
                             | SOFAR'Fail => (SOFAR'Epsilon x)
                             | SOFAR'Succ _ => (SOFAR'Succ x)
                             | SOFAR'Error => (SOFAR'Succ x)
        fun altP (par1,par2) =
          Parser (fn (ls,sofar) => case (deParser par1) (ls,checkpoint sofar) of
                                     RES'Fail => ((deParser par2) (ls,sofar))
                                   | RES'Epsilon x => ((deParser par2)
                                                       (ls,(addAlt sofar) x))
                                   | x as RES'Succ _ => x
                                   | x as RES'Error _ => x)
        fun acc_manyP acc par ls =
          case (deParser par) (ls,SOFAR'Fail) of
            RES'Succ (ls,x) => (((acc_manyP (op :: {1 = x,2 = acc})) par) ls)
          | RES'Epsilon x => (RES'Succ (ls,rev (op :: {1 = x,2 = acc})))
          | RES'Fail => (RES'Succ (ls,rev acc))
          | RES'Error e => (RES'Error e)
        fun manyP par =
          Parser (fn (ls,sofar) => case (deParser par)
                                        (ls,SOFAR'Fail) of
                                     RES'Succ (ls,x) =>
                                       (((acc_manyP [x]) par) ls)
                                   | RES'Epsilon x =>
                                       (((makeResult sofar) ls)
                                        [x])
                                   | RES'Fail => (((makeResult sofar) ls) [])
                                   | RES'Error e => (RES'Error e))
        fun noErrorP par = Parser (fn x => case (deParser par) x of
                                             RES'Error _ => RES'Fail
                                           | x as RES'Fail => x
                                           | x as RES'Succ _ => x
                                           | x as RES'Epsilon _ => x)
        fun execP par ls = case (deParser par) (ls,SOFAR'Fail) of
                             RES'Succ (ls,x) => (ls,Some x)
                           | RES'Epsilon x => (ls,Some x)
                           | RES'Fail => (ls,None)
                           | RES'Error ls => (ls,None)
        val execP = fn par => fn ls => let val (cls,x) = (execP par)
                                                         (initCLS ls)
                                       in (endCLS cls,x)
                                       end
        end : PfvSig
;

