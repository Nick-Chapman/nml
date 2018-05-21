(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  14.    export
  42.    import
  55.    functor
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(* Continuation version of 4-value parser combinators *)

(* Like PAR2 - but fully saturate all applications with lexState "q" *)

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

functor PAR3(structure Lex : LexSig_Import) : ParSig =
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


    datatype 'a par = Parser of {S  : 'a * Lex.lexState -> unit,
                                 ep : 'a * Lex.lexState -> unit,
                                 F  : Lex.lexState -> unit,
                                 E  : Lex.lexState -> unit,
                                 q  : Lex.lexState                  } -> unit

    fun deParser (Parser x) = x

    fun appParser par (S,ep,F,E,q) = deParser par {S=S,ep=ep,F=F,E=E,q=q}

    exception ExecP
    fun execP par q =
        let val X = ref None
            fun Get() = case X of ref (Some x) => x | _ => raise ExecP
            fun Set res = (X:=Some res)
            val () =
                appParser par (fn (x,q) => Set (q,Some x),
                               fn (x,q) => Set (q,Some x),
                               fn q => Set (q,None),
                               fn q => Set (q,None),
                               q
                               )
        in Get()
        end

    val execP =
        fn par =>
           fn ls =>
              let val (cls,x) = execP par (Lex.initCaching ls)
              in (Lex.endCaching cls,x)
              end

(*
  fun fixP_v1 f =
      let fun fix ls = deParser (f (Parser fix)) ls
      in Parser fix
      end
*)

    exception FixP
    fun fixP f =
        let val fRef = ref (fn _ => raise FixP)
            val p = Parser (fn x => (!fRef) x)
            val p' = f p
            val (U:unit) = (fRef := deParser p')
        in p'
        end


    fun resultP x   = Parser (fn {S,ep,F,E,q} => ep(x,q))
    val failP       = Parser (fn {S,ep,F,E,q} => F q)
    val positionP   = Parser (fn {S,ep,F,E,q} => ep (Lex.posLS q,q))

    fun satisfyP who T =
        Parser (fn {S,ep,F,E,q} =>
                   case (Lex.stepLS who q) of
                       None => F q
                     | Some (tok,q') => case (T tok) of
                                            None => F q
                                          | Some x => S (x,q'))


    fun bindP (par,M) =
        Parser (fn {S,ep,F,E,q} =>
                   appParser par (fn (x,q) => appParser (M x) (S,S,E,E,q),
                                  fn (x,q) => appParser (M x) (S,ep,F,E,q),
                                  F,
                                  E,
                                  q))

    fun altP (par1,par2) =
        Parser (fn {S,ep,F,E,q} =>
                   appParser par1 (S,
                                   fn (x,q) => appParser par2 (S,
                                                               ep, (*fn (_,q) => ep(x,q),*)
                                                               fn q => ep(x,q),
                                                               E,
                                                               q),
                                   fn q => appParser par2 (S,ep,F,E,q),
                                   E,
                                   q))
    fun noErrorP par =
        Parser (fn {S,ep,F,E,q} =>
                   appParser par (S,
                                  ep,
                                  F,
                                  fn _ => F q,
                                  q))

(*
    fun manyP par =
        altP (resultP [],
              bindP (par, fn x =>
              bindP (manyP par, fn xs =>
              resultP (x::xs))))
*)

(*
    fun acc_manyP acc par =
        Parser (fn {S,ep=_,F=_,E,q} =>
                   appParser par (fn (x,q) => appParser (acc_manyP (x::acc) par) (S,S,E,E,q),
                                  fn (x,q) => S (rev (x::acc),q), (* would cause non-termination in non-primitive definition *)
                                  fn q => S (rev acc,q),
                                  E,
                                  q))
    fun manyP par =
        Parser (fn {S,ep,F=_,E,q} =>
                   appParser par (fn (x,q) => appParser (acc_manyP [x] par) (S,S,E,E,q),
                                  fn (x,q) => ep ([x],q), (* would cause non-termination in non-primitive definition *)
                                  fn q => ep ([],q),
                                  E,
                                  q))
*)


    fun acc_manyP acc par S E q =
        appParser par (fn (x,q) => acc_manyP (x::acc) par S E q,
                       fn (x,q) => S (rev (x::acc),q), (* would cause non-termination in non-primitive definition *)
                       fn q => S (rev acc,q),
                       E,
                       q)

    fun manyP par =
        Parser (fn {S,ep,F=_,E,q} =>
                   appParser par (fn (x,q) => acc_manyP [x] par S E q,
                                  fn (x,q) => ep ([x],q), (* would cause non-termination in non-primitive definition *)
                                  fn q => ep ([],q),
                                  E,
                                  q))



end
