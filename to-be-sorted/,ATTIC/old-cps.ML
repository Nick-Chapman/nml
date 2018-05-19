
type value = Value.value

fun notYet s = Prel.abort s


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
    | ATOM'Fn       of afunc

  and afunc = AFUNC	of cmatch

  and cmatch
	= CMATCH'Alt    of cmatch * cmatch 
	| CMATCH'Rule   of pat * cps
    | CMATCH'Atom	of amatch (* extra ! *)

  and amatch
	= AMATCH'Alt    of amatch * amatch 
	| AMATCH'Rule   of pat * atom

  and cdec
	= (*CDEC'Empty (* not nesc! *)
    | *)CDEC'Seq      of cdec * cdec
    | CDEC'Local    of cdec * cdec
    | CDEC'Val      of pat * cps
    | CDEC'Atom     of adec

  and adec
	= ADEC'Empty
    | ADEC'Seq      of adec * adec
    | ADEC'Local    of adec * adec
    | ADEC'Val      of pat * atom
    | ADEC'Valrec   of (vid * afunc) list
    | ADEC'ExGen	of vid
    | ADEC'ExCopy   of vid * vid

end

(*----------------------------------------------------------------------
INDEX: Embed -> CPS
----------------------------------------------------------------------*)

;
open Embed;
open CPS;
							   

  local val U = ref 1
		open Atom
  in fun newVid () = let val n = !U
					 in U := 1+n; VID (ID ("C#"^stringOfInt n))
					 end
  end


  (* lift/optimize atom cases when building cps structures *)

  val cps'case =
	  fn (atom,CMATCH'Atom am) => CPS'Atom (ATOM'Case (atom,am))
	   | (atom,cm) => CPS'Case (atom,cm)

  val cps'let =
	  fn (CDEC'Atom adec,CPS'Atom atom) => CPS'Atom (ATOM'Let (adec,atom))
	   | (cdec,cps) => CPS'Let (cdec,cps)

  val cmatch'rule =
	  fn (pat,CPS'Atom atom) => CMATCH'Atom (AMATCH'Rule (pat,atom))
	   | (pat,cps) => CMATCH'Rule (pat,cps)

  val cmatch'alt =
	  fn (CMATCH'Atom am1, CMATCH'Atom am2) => CMATCH'Atom (AMATCH'Alt (am1,am2))
	   | (cm1,cm2) => CMATCH'Alt (cm2,cm2)

  val cdec'seq = 
	  fn (CDEC'Atom am1, CDEC'Atom am2) => CDEC'Atom (ADEC'Seq (am1,am2))
	   | (cd1,cd2) => CDEC'Seq (cd1,cd2)

  val cdec'local = 
	  fn (CDEC'Atom ad1, CDEC'Atom ad2) => CDEC'Atom (ADEC'Local (ad1,ad2))
	   | (cd1,cd2) => CDEC'Local (cd1,cd2)

  val cdec'val = 
	  fn (pat,CPS'Atom atom) => CDEC'Atom (ADEC'Val (pat, atom))
	   | (pat,cps) => CDEC'Val (pat,cps)


  type schemes = {
                  C     : exp -> cps,
                  A     : exp -> (atom -> cps) -> cps,
                  As    : exp list -> (atom list -> cps) -> cps,
                  F     : func -> afunc,
                  CM    : match -> cmatch,
                  AM    : match -> (amatch -> cps) -> cps,
                  CD    : dec -> cdec,
                  AD    : dec -> (adec -> cps) -> cps,
                  ADv2  : dec -> (adec -> cdec) -> cdec
				  }

  fun compile'expC ({C,CM,CD,A,...}:schemes) : (exp -> cps) =
	  fn exp => 
		 case exp
          of EXP'Let (dec,exp)          => cps'let (CD dec,C exp)
           | EXP'Case (exp,match)       => A exp (fn atom => cps'case (atom, CM match))
           | EXP'App (exp1,exp2)        => A exp1 (fn atom1 => A exp2 (fn atom2 => CPS'App (atom1,atom2)))
		   | EXP'Handle (exp,match)     => CPS'Handle (C exp, CM match)
           | _ => 
			 (* EXP'Value, EXP'Var, EXP'Fn, EXP'Tuple, EXP'Raise *)
             A exp (fn a => CPS'Atom a)

  fun compile'expA ({C,A,As,F,AM,AD,...}:schemes) : (exp -> (atom -> cps) -> cps) =
	  fn exp => 
		 fn c =>
			case exp 
             of EXP'Value v             => c (ATOM'Value v)
              | EXP'Var x               => c (ATOM'Var x)
              | EXP'Fn func             => c (ATOM'Fn (F func))
              | EXP'Tuple exps          => As exps (fn atoms => c (ATOM'Tuple atoms))
              | EXP'Raise exp           => A exp (fn atom => c (ATOM'Raise atom))
              | EXP'Case (exp,match)    => A exp (fn atom => AM match (fn amatch => c (ATOM'Case (atom,amatch))))
              | EXP'Let (dec,exp)       => AD dec (fn adec => A exp (fn atom => c (ATOM'Let (adec,atom))))
			  | _ => 
				(* EXP'Handle, EXP'App *)
				case (C exp)
				 of CPS'Atom atom => c(atom)
				  | cps => let val x = newVid()
						   in CPS'Let (CDEC'Val (PAT'V x,cps),
									   c(ATOM'Var x))
						   end

  fun compile'expsA ({A,As,...}:schemes) : (exp list -> (atom list -> cps) -> cps) =
	  fn exps =>
		 fn c =>
			case exps of
				[] => c []
			  | exp::exps => A exp (fn atom => As exps (fn atoms => c (atom :: atoms)))

  fun compile'func ({CM,...}:schemes): (func -> afunc) =
	  fn FUNC match => AFUNC (CM match)


  fun compile'matchC ({C,CM,...}:schemes) : (match -> cmatch) =
	  fn match =>
		 case match
          of MATCH'Alt (m1,m2)			=> cmatch'alt (CM m1, CM m2)
           | MATCH'Rule (pat,exp)       => cmatch'rule (pat, C exp)

  
  fun compile'matchA ({A,AM,...}:schemes) : (match -> (amatch -> cps) -> cps) =
	  fn match =>
		 fn c =>
			case match
             of MATCH'Alt (m1,m2)       => AM m1 (fn am1 => AM m2 (fn am2 => c (AMATCH'Alt (am1,am2))))
              | MATCH'Rule (pat,exp)    => A exp (fn atom => c (AMATCH'Rule (pat,atom)))

  
  fun compile'decC ({C,CD,ADv2,...}:schemes) : (dec -> cdec) =
	  fn dec =>
		 case dec
          of DEC'Seq (dec1,dec2)        => cdec'seq (CD dec1, CD dec2) 
           | DEC'Local (dec1,dec2)      => cdec'local (CD dec1, CD dec2) 
           | DEC'Val (pat,exp)          => cdec'val (pat, C exp)
           | _ => 
             (* DEC'Valrec, DEC'ExGen, DEC'ExCopy *)
			 ADv2 dec (fn adec => CDEC'Atom adec)


  fun compile'decA ({A,AD,F,...}:schemes) : (dec -> (adec -> cps) -> cps) =
	  fn dec =>
		 fn c =>
			case dec
             of DEC'Seq (dec1,dec2)     => AD dec1 (fn ad1 => AD dec2 (fn ad2 => c (ADEC'Seq (ad1,ad2))))
              | DEC'Local (dec1,dec2)   => AD dec1 (fn ad1 => AD dec2 (fn ad2 => c (ADEC'Local (ad1,ad2))))
              | DEC'Val (pat,exp)       => A exp (fn atom => c (ADEC'Val (pat,atom)))
			  | DEC'Empty               => c (ADEC'Empty)
              | DEC'Valrec binds        => c (ADEC'Valrec (map (fn (x,f) => (x,F f)) binds))
              | DEC'ExGen x             => c (ADEC'ExGen x)
              | DEC'ExCopy (x,y)        => c (ADEC'ExCopy (x,y))


(*----------------------------------------------------------------------
INDEX: todo: fix cps schemes - need ADv2 ?
----------------------------------------------------------------------*)


(*----------------------------------------------------------------------
INDEX: uncompile : cps -> exp (for printing etc)
----------------------------------------------------------------------*)

  type schemes = {
                  C     : cps -> exp,
                  A     : atom -> exp,
                  CM    : cmatch -> match,
                  AM    : amatch -> match,
                  CD    : cdec -> dec,
                  AD    : adec -> dec
				  }

  fun uncompile'cps ({C,CM,CD,A,...}:schemes) : (cps -> exp) =
	  fn CPS'Let (cdec,cps)         => EXP'Let (CD cdec, C cps)
	   | CPS'Case (atom,cmatch)     => EXP'Case (A atom,CM cmatch)
	   | CPS'App (atom1,atom2)      => EXP'App (A atom1,A atom2)
	   | CPS'Handle (cps,cmatch)    => EXP'Handle (C cps,CM cmatch)
	   | CPS'Atom atom              => A atom

  fun uncompile'atom ({CM,A,AM,AD,...}:schemes) : (atom -> exp) =
      fn ATOM'Value v               => EXP'Value v
       | ATOM'Var x                 => EXP'Var x
       | ATOM'Tuple atoms           => EXP'Tuple (map A atoms)
       | ATOM'Let (adec,atom)       => EXP'Let (AD adec, A atom)
       | ATOM'Case (atom,amatch)    => EXP'Case (A atom, AM amatch)
       | ATOM'Raise atom            => EXP'Raise (A atom)
       | ATOM'Fn (AFUNC cm)         => EXP'Fn (FUNC (CM cm))

  fun uncompile'cmatch ({C,CM,AM,...}:schemes) : (cmatch -> match) =
      fn CMATCH'Alt (cm1,cm2)       => MATCH'Alt (CM cm1, CM cm2)
       | CMATCH'Rule (pat,cps)      => MATCH'Rule (pat, C cps)
       | CMATCH'Atom am				=> AM am

  fun uncompile'amatch ({A,AM,...}:schemes) : (amatch -> match) =
      fn AMATCH'Alt (am1,am2)       => MATCH'Alt (AM am1, AM am2)
       | AMATCH'Rule (pat,atom)     => MATCH'Rule (pat, A atom)

  fun uncompile'cdec ({C,CD,AD,...}:schemes) : (cdec -> dec) =
	  fn CDEC'Seq (cdec1,cdec2)     => DEC'Seq (CD cdec1, CD cdec2)
	   | CDEC'Local (cdec1,cdec2)   => DEC'Local (CD cdec1, CD cdec2)
	   | CDEC'Val (pat,cps)         => DEC'Val (pat, C cps)
	   | CDEC'Atom ad				=> AD ad

  fun uncompile'adec ({CM,A,AD,...}:schemes) : (adec -> dec) =
	  fn ADEC'Empty					=> DEC'Empty
	   | ADEC'Seq (adec1,adec2)     => DEC'Seq (AD adec1, AD adec2)
	   | ADEC'Local (adec1,adec2)   => DEC'Local (AD adec1, AD adec2)
	   | ADEC'Val (pat,atom)        => DEC'Val (pat, A atom)
	   | ADEC'ExGen x               => DEC'ExGen x
	   | ADEC'ExCopy (x,y)          => DEC'ExCopy (x,y)
	   | ADEC'Valrec binds			=> DEC'Valrec (map (fn (x,AFUNC cm) => (x,FUNC (CM cm))) binds)

(*----------------------------------------------------------------------
INDEX: todo: fixup uncompile
----------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: freevar calc...
----------------------------------------------------------------------*)

  open CPS
  structure FV = CPS_FV

  type schemes = {
                  C     : cps -> FV.cps,
                  A     : atom -> FV.atom,
                  CM    : cmatch -> FV.cmatch,
                  AM    : amatch -> FV.amatch,
                  CD    : cdec -> FV.cdec,
                  AD    : adec -> FV.adec,
                  F     : afunc -> FV.afunc
				  }
				 
  val fvs = FVS[]

  fun fvs'cps ({C,CM,CD,A,...}:schemes) =
	  fn CPS'Let (cdec,cps)         => FV.CPS'Let (CD cdec, fvs, C cps)
	   | CPS'Case (atom,cmatch)     => FV.CPS'Case (A atom,CM cmatch)
	   | CPS'App (atom1,atom2)      => FV.CPS'App (A atom1, A atom2)
	   | CPS'Handle (cps,cmatch)    => FV.CPS'Handle (C cps,fvs,CM cmatch)
	   | CPS'Atom atom              => FV.CPS'Atom (A atom)

  fun fvs'atom ({CM,A,AM,AD,F,...}:schemes) =
      fn ATOM'Value v               => FV.ATOM'Value v
       | ATOM'Var x                 => FV.ATOM'Var x
       | ATOM'Tuple atoms           => FV.ATOM'Tuple (map A atoms)
       | ATOM'Let (adec,atom)       => FV.ATOM'Let (AD adec, A atom)
       | ATOM'Case (atom,amatch)    => FV.ATOM'Case (A atom, AM amatch)
       | ATOM'Raise atom            => FV.ATOM'Raise (A atom)
       | ATOM'Fn func				=> FV.ATOM'Fn (F func)

  fun fvs'func ({CM,...}:schemes) =
	  fn AFUNC cm					=> FV.AFUNC (fvs, CM cm)

  fun fvs'cmatch ({C,CM,AM,...}:schemes) =
      fn CMATCH'Alt (cm1,cm2)       => FV.CMATCH'Alt (CM cm1, CM cm2)
       | CMATCH'Rule (pat,cps)      => FV.CMATCH'Rule (pat, C cps)
       | CMATCH'Atom am				=> FV.CMATCH'Atom (AM am)

  fun fvs'amatch ({A,AM,...}:schemes) =
      fn AMATCH'Alt (am1,am2)       => FV.AMATCH'Alt (AM am1, AM am2)
       | AMATCH'Rule (pat,atom)     => FV.AMATCH'Rule (pat, A atom)

  fun fvs'cdec ({C,CD,AD,...}:schemes) =
	  fn CDEC'Seq (cdec1,cdec2)     => FV.CDEC'Seq (CD cdec1, fvs, CD cdec2)
	   | CDEC'Local (cdec1,cdec2)   => FV.CDEC'Local (CD cdec1, fvs, CD cdec2)
	   | CDEC'Val (pat,cps)         => FV.CDEC'Val (fvs, pat, C cps)
	   | CDEC'Atom ad				=> FV.CDEC'Atom (AD ad)

  fun fvs'adec ({CM,A,AD,F,...}:schemes) =
	  fn ADEC'Empty					=> FV.ADEC'Empty
	   | ADEC'Seq (adec1,adec2)     => FV.ADEC'Seq (AD adec1, AD adec2)
	   | ADEC'Local (adec1,adec2)   => FV.ADEC'Local (AD adec1, AD adec2)
	   | ADEC'Val (pat,atom)        => FV.ADEC'Val (pat, A atom)
	   | ADEC'ExGen x               => FV.ADEC'ExGen x
	   | ADEC'ExCopy (x,y)          => FV.ADEC'ExCopy (x,y)
	   | ADEC'Valrec binds			=> FV.ADEC'Valrec (map (fn (x,f) => (x, F f)) binds)

