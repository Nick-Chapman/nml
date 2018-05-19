
type value = Value.value

(*----------------------------------------------------------------------
INDEX: CPS
----------------------------------------------------------------------*)

structure CPS =
struct

  datatype scon = datatype Atom.scon
  datatype vid = datatype Atom.vid
  datatype pat = datatype Embed.pat

  datatype cps 
    = CPS'Let       of cdec * cps
	| CPS'Case      of atom * cmatch
    | CPS'Handle    of cps * cmatch
	| CPS'App       of atom * atom
    | CPS'Atom      of atom

  and atom
    = ATOM'Value    of value
    | ATOM'Var      of vid
    | ATOM'Tuple    of atom list
    | ATOM'Let      of adec * atom
    | ATOM'Case     of atom * amatch
    | ATOM'Raise    of atom
    | ATOM'Fn       of func

  and func = FUNC	of cmatch

  and cmatch
	= CMATCH'Alt    of cmatch * cmatch 
	| CMATCH'Rule   of pat * cps

  and amatch
	= AMATCH'Alt    of amatch * amatch 
	| AMATCH'Rule   of pat * atom

  and cdec
	= CDEC'Empty
    | CDEC'Seq      of cdec * cdec
    | CDEC'Local    of cdec * cdec
    | CDEC'Val      of pat * cps
    | CDEC'Atom     of adec

  and adec
	= ADEC'Empty
    | ADEC'Seq      of adec * adec
    | ADEC'Local    of adec * adec
    | ADEC'Val      of pat * atom
    | ADEC'Valrec   of (vid * func) list
    | ADEC'ExGen	of vid
    | ADEC'ExCopy   of vid * vid

end

(*----------------------------------------------------------------------
INDEX: Embed -> CPS
----------------------------------------------------------------------*)

open Embed
open CPS

  type schemes = {
                  C     : exp -> cps,
                  CM    : match -> cmatch,
                  CD    : dec -> cdec,
                  A     : exp -> (atom -> cps) -> cps,
                  AM    : match -> (amatch -> cps) -> cps,
                  AD    : dec -> (adec -> cps) -> cps,
                  F     : func -> atom
				  }

  fun notYet s = Prel.abort s

  fun compile'expA ({C,CM,A,AM,F,...}:schemes) : (exp -> (atom -> cps) -> cps) =
	  fn exp => 
		 fn c =>
			case exp 
             of EXP'Value v             => c (ATOM'Value v)
              | EXP'Var x               => c (ATOM'Var x)
              | EXP'Tuple exps          => notYet "tuple"
              | EXP'Let (dec,exp)       => notYet "let"
			  | EXP'App (exp1,exp2)		=> A exp1 (fn atom1 => A exp2 (fn atom2 => CPS'App (atom1,atom2)))
              | EXP'Case (exp,match)    => A exp (fn atom => AM match (fn amatch => c (ATOM'Case (atom, amatch))))
              | EXP'Handle (exp,match)  => CPS'Handle (C exp, CM match)
              | EXP'Raise exp           => A exp (fn atom => c (ATOM'Raise atom))

              | EXP'Fn func             => c (F func)

                   

  fun compile'expC ({A,C,CM,CD,...}:schemes) : (exp -> cps) =
	  fn exp => 
		 case exp
		  of EXP'Value v => CPS'Atom (ATOM'Value v)
		   | EXP'App (exp1,exp2) => A exp1 (fn atom1 => A exp2 (fn atom2 => CPS'App (atom1,atom2)))
		   | - => A (fn a => CPS'Atom a)
