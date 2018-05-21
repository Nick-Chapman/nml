(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  12.    export
  40.    import
  53.    functor
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(* Continuation version of 4-value parser combinators *)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature ParSig =
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

    type lexState
    val execP       : 'a par -> lexState -> lexState * 'a option

end

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
end

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor PAR2(structure Lex : LexSig_Import) : ParSig =
struct

    type lexState = Lex.lexState (* q *)
    type tok = Lex.tok
    type pos = Lex.pos

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


    type cont = Lex.lexState -> unit
    datatype 'a par = Parser of {S  : 'a -> cont,
                                 ep : 'a -> cont,
                                 F  : cont,
                                 E  : cont} -> cont

    fun deParser (Parser x) = x

    fun appParser par (S,ep,F,E) = deParser par {S=S,ep=ep,F=F,E=E}

    exception ExecP
    fun execP par q =
        let val X = ref None
            fun Get() = case X of ref (Some x) => x | _ => raise ExecP
            fun Set res = (X:=Some res)
            val () =
                appParser par (fn x => fn q => Set (q,Some x),
                               fn x => fn q => Set (q,Some x),
                               fn q => Set (q,None),
                               fn q => Set (q,None)) q
        in Get()
        end

    val execP =
        fn par =>
           fn ls =>
              let val (cls,x) = execP par (Lex.initCaching ls)
              in (Lex.endCaching cls,x)
              end

    exception FixP
    fun fixP f =
        let val fRef = ref (fn _ => raise FixP)
            val p = Parser (fn x => (!fRef) x)
            val p' = f p
            val (U:unit) = (fRef := deParser p')
        in p'
        end

    fun resultP x   = Parser (fn {S,ep,F,E} => ep(x))
    val failP       = Parser (fn {S,ep,F,E} => F)
    val positionP   = Parser (fn {S,ep,F,E} => fn q => ep (Lex.posLS q) q)

    fun satisfyP who T =
        Parser (fn {S,ep,F,E} =>
                   fn q =>
                      case (Lex.stepLS who q) of
                          None => F q
                        | Some (tok,q') => case (T tok) of
                                               None => F q
                                             | Some x => S x q')


    fun bindP (par,M) =
        Parser (fn {S,ep,F,E} =>
                   appParser par (fn x => appParser (M x) (S,S,E,E),
                                  fn x => appParser (M x) (S,ep,F,E),
                                  F,
                                  E))

    fun altP (par1,par2) =
        Parser (fn {S,ep,F,E} =>
                   appParser par1 (S,
                                   fn x => appParser par2 (S,fn _ => ep(x),ep(x),E),
                                   appParser par2 (S,ep,F,E),
                                   E))

    fun noErrorP par =
        Parser (fn {S,ep,F,E} =>
                   fn q =>
                      appParser par (S,ep,F,fn _ => F q) q)


    fun acc_manyP acc par =
        Parser (fn {S,ep=_,F=_,E} =>
                   appParser par (fn x => appParser (acc_manyP (x::acc) par) (S,S,E,E),
                                  fn x => S (rev (x::acc)), (* would cause non-termination in non-primitive definition *)
                                  S (rev acc),
                                  E))

    fun manyP par =
        Parser (fn {S,ep,F=_,E} =>
                   appParser par (fn x => appParser (acc_manyP [x] par) (S,S,E,E),
                                  fn x => ep [x], (* would cause non-termination in non-primitive definition *)
                                  ep [],
                                  E))


end
