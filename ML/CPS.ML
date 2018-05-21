(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  21.    export
  86.    imports
  210.   functor
  241.   struct Cps - cps types (no free var info)
  288.   cps transform - Embed -> Cps
  431.   fixup cps schemes
  447.   Cps_Fv - cps types (with embedded free var info)
  491.   var sets
  519.   pat - bound/free
  543.   freevar calc - Cps -> Cps_Fv
  656.   fix-up fv-calc schemes
  684.   N_lay'* - patm,atom
  785.   full layout
  819.   depth restricted layout
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature CpsSig =
sig

  type value
  type prim
  type vid
  type uid
  type scon
  type pat

  datatype cps
    = CPS'Return    of atom
    | CPS'Raise     of atom
    | CPS'Let       of bind * cps
    | CPS'Case      of atom * cmatch
    | CPS'App       of atom * atom list
    | CPS'Handle    of cps * {save:uid list} * cmatch
    | CPS'LetC      of {save:uid list} * pat * cps * cps

  and atom
    = ATOM'Value    of value
    | ATOM'Var      of uid
    | ATOM'NewEx    of vid
    | ATOM'Tuple    of atom list
    | ATOM'PrimApp  of prim * atom
    | ATOM'Fn       of afunc
    | ATOM'Let      of bind * atom
    | ATOM'Case     of atom * amatch

  and bind
    = BIND'Val      of pat * atom
    | BIND'ValRec   of (uid * afunc) list

  and afunc = AFUNC'Def of {save:uid list} * uid list * cps

  and cmatch
    = CMATCH'Alt    of cmatch * cmatch
    | CMATCH'Rule   of pat * cps
    | CMATCH'Atom   of amatch

  and amatch
    = AMATCH'Alt    of amatch * amatch
    | AMATCH'Rule   of pat * atom


  type Embed_dec
  val cpsTransform : Embed_dec * {bound:uid list} -> cps

  type layout

  val lay'afunc     : afunc -> layout
  val lay'cps       : cps -> layout
  val lay'cmatch    : cmatch -> layout

  val N_lay'pat     : int -> pat -> layout
  val N_lay'atom    : {depth: int} -> atom -> layout
  val N_lay'cps_tail: {depth:int} -> cps -> layout


end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ../prelude/Prel.ML *)
signature PrelSig_Import =
sig
    val filter          : ('a -> bool) -> 'a list -> 'a list
    val fold            : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    val exists          : ('a -> bool) -> 'a list -> bool
    val forall          : ('a -> bool) -> 'a list -> bool
    val copy            : int -> 'a -> 'a list
    val zipPair         : 'a list -> 'b list -> ('a * 'b) list
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AtomSig ATOM.ML *)
signature AtomSig_Import =
sig
    type vid;
    type scon;
    val sof'vid : vid -> string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: EmbedSig EMBED.ML *)
signature EmbedSig_Import =
sig
  type exname
  type value
  type prim
  type vid
  type scon
  type con
  type uid
  val newuid        : unit -> uid
  val eq'uid        : uid -> uid -> bool
  val sof'uid       : uid -> string
  datatype pat
    = PAT'Wild
    | PAT'V         of uid
    | PAT'Layered   of uid * pat
    | PAT'Ref       of pat
    | PAT'C         of con * pat option
    | PAT'E         of exname * pat option
    | PAT'G         of uid * pat option
    | PAT'Scon      of scon
    | PAT'Tuple     of pat list
  datatype exp
    = EXP'Value     of value * {info : (vid,scon) either}
    | EXP'Var       of uid
    | EXP'NewEx     of vid
    | EXP'Tuple     of exp list
    | EXP'PrimApp   of prim * exp
    | EXP'Fn        of func
    | EXP'Raise     of exp
    | EXP'App       of exp * exp list
    | EXP'Handle    of exp * match
    | EXP'Let       of dec * exp
    | EXP'Case      of exp * match
  and func = FUNC'Def   of uid list * exp
  and match
    = MATCH'Alt     of match * match
    | MATCH'Rule    of pat * exp
  and dec
    = DEC'Empty
    | DEC'Seq       of dec * dec
    | DEC'Val       of pat * exp
    | DEC'Valrec    of (uid * func) list
  type Lang_dec
end;
(*IMPORTDONE:*)

(*IMPORTSIG: MisclaySig ../prelude/MISCLAY.ML *)
signature MisclaySig_Import =
sig
  type layout
  val Word                      : string -> layout
  val Left                      : string -> layout
  val Right                     : string -> layout
  val Blank                     : layout
  val //                        : layout * layout -> layout
  val @@                        : layout * layout -> layout
  val ++                        : layout * layout -> layout
  val ``                        : layout * layout -> layout
  val //``                      : layout * layout -> layout
  val layBracket                : layout -> layout
  val layBrace                  : layout -> layout
  val layCommaSep               : layout list -> layout
  val layVert                   : layout list -> layout
end;
(*IMPORTDONE:*)

(*IMPORTSIG: PrettySig PRETTY.ML *)
signature PrettySig_Import =
    sig
        type layout
        type scon
        type dec
        type match
        val lay'scon : scon -> layout
    end;
(*IMPORTDONE:*)

(*IMPORTSIG: ValueSig VALUE.ML *)
signature ValueSig_Import =
sig
    type vid
    type scon
    type exname
    type value
    type layout
    datatype effects = EFF'Pure | EFF'Impure | EFF'SideEffects | EFF'VarAlloc
    type con
    val sof'con         : con -> string
    type prim;
    val name'prim       : prim -> string
    val effects'prim    : prim -> effects
    val vid'exname      : exname -> vid
    val value'unit      : value
    val lay'value       : value -> layout
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor CPS
        (structure Prel : PrelSig_Import

         val right_to_left_tuples : bool ref
         val right_to_left_args : bool ref
         val bind_all_prim_app : bool ref
         val lift_lets : bool ref

         structure Atom : AtomSig_Import
         structure Embed : EmbedSig_Import
         structure Value : ValueSig_Import
         structure Misclay : MisclaySig_Import
         structure Pretty : PrettySig_Import
         sharing type Atom.scon = Embed.scon = Pretty.scon
         sharing type Atom.vid = Embed.vid = Value.vid
         sharing type Embed.con = Value.con
         sharing type Embed.exname = Value.exname
         sharing type Embed.prim = Value.prim
         sharing type Embed.value = Value.value
         sharing type Embed.Lang_dec = Pretty.dec
         sharing type Misclay.layout = Pretty.layout = Value.layout
        ) : CpsSig =
struct

  infix // @@ ++ `` //``
  open Prel Misclay Atom Embed Value

(*----------------------------------------------------------------------
INDEX: struct Cps - cps types (no free var info)
----------------------------------------------------------------------*)

structure Cps =
struct

  datatype cps
    = CPS'Return    of atom
    | CPS'Raise     of atom
    | CPS'Let       of bind * cps
    | CPS'Case      of atom * cmatch
    | CPS'App       of atom * atom list
    | CPS'Handle    of cps * cmatch
    | CPS'LetC      of pat * cps * cps

  and atom
    = ATOM'Value    of value
    | ATOM'Var      of uid
    | ATOM'NewEx    of vid
    | ATOM'Tuple    of atom list
    | ATOM'PrimApp  of prim * atom
    | ATOM'Fn       of afunc
    | ATOM'Let      of bind * atom
    | ATOM'Case     of atom * amatch

  and bind
    = BIND'Val      of pat * atom
    | BIND'ValRec   of (uid * afunc) list

  and afunc = AFUNC'Def of uid list * cps

  and cmatch
    = CMATCH'Alt    of cmatch * cmatch
    | CMATCH'Rule   of pat * cps
    | CMATCH'Atom   of amatch

  and amatch
    = AMATCH'Alt    of amatch * amatch
    | AMATCH'Rule   of pat * atom


  val atom'unit = ATOM'Value value'unit
  val atom'tuple = fn [] => atom'unit | [x] => x | xs => ATOM'Tuple xs

end

(*----------------------------------------------------------------------
INDEX: cps transform - Embed -> Cps
----------------------------------------------------------------------*)

  open Cps


  fun eq'pat_atom (PAT'V x1,ATOM'Var x2) = eq'uid x1 x2
    | eq'pat_atom (PAT'Tuple pats,ATOM'Tuple atoms) = (length pats = length atoms) andalso
                                                      forall eq'pat_atom (zipPair pats atoms)
    | eq'pat_atom _ = false

  fun eq'pat_cps (pat,CPS'Return atom) = eq'pat_atom (pat,atom)
    | eq'pat_cps _ = false



  (* lift/optimize atom cases when building cps structures *)

  val cps'case =
      fn (atom,CMATCH'Atom am) => CPS'Return (ATOM'Case (atom,am))
       | (atom,cm) => CPS'Case (atom,cm)

  val cmatch'rule =
      fn (pat,CPS'Return atom) => CMATCH'Atom (AMATCH'Rule (pat,atom))
       | (pat,cps) => CMATCH'Rule (pat,cps)

  val cmatch'alt =
      fn (CMATCH'Atom am1, CMATCH'Atom am2) => CMATCH'Atom (AMATCH'Alt (am1,am2))
       | (cm1,cm2) => CMATCH'Alt (cm1,cm2)

  val cps'let =
      fn (pat,CPS'Return atom1,CPS'Return atom2) => CPS'Return (ATOM'Let (BIND'Val (pat,atom1),atom2))
       | (pat,CPS'Return atom1,cps2) => CPS'Let (BIND'Val (pat,atom1),cps2)
       | (pat,cps1,cps2) => CPS'LetC (pat,cps1,cps2)


  fun transform_cps'let (pat,cps,body) =
      if (eq'pat_cps (pat,body)) then cps else cps'let (pat,cps,body)


  val cps'letrec =
      fn (binds,CPS'Return atom) => CPS'Return (ATOM'Let (BIND'ValRec binds,atom))
       | (binds,cps) => CPS'Let (BIND'ValRec binds,cps)



  val ContinuationLetBind : (cps -> (atom -> cps) -> cps) =
      fn cps =>
         fn c =>
            let val x = newuid()
            in CPS'LetC (PAT'V x, cps, c(ATOM'Var x))
            end

  val AtomicLetBind : (atom -> (atom -> cps) -> cps) =
      fn atom =>
         fn c =>
            let val x = newuid()
            in CPS'Let (BIND'Val (PAT'V x, atom), c(ATOM'Var x))
            end

  fun NoBind atom c = c atom

  fun MaybeBind'effects effects : (atom -> (atom -> cps) -> cps) =
      case effects of
          EFF'VarAlloc       => ContinuationLetBind o CPS'Return
        | EFF'SideEffects    => AtomicLetBind
        | EFF'Impure         => if (!bind_all_prim_app) then AtomicLetBind else NoBind
        | EFF'Pure           => if (!bind_all_prim_app) then AtomicLetBind else NoBind


  fun cpsSequence X xs c =
      case xs
       of [] => c []
        | x::xs => X x (fn y => cpsSequence X xs (fn ys => c (y::ys)))


  type C    = exp -> (cps -> cps) -> cps
  type CD   = dec -> cps -> cps
  type CM   = match -> cmatch
  type A    = exp -> (atom -> cps) -> cps


  type cps_schemes = {C:C, CD:CD, CM:CM, A:A}


  fun Identity x = x

  fun cps'Func (C:C) : (func -> afunc) =
      fn FUNC'Def (xs,exp) => AFUNC'Def (xs,C exp Identity)

  fun LL caller =
      (if (!lift_lets)
       then caller
       else (fn c => c (caller Identity)))


  (* Compile schemes C and A call each other - so every EXP form must be handled by one of them! *)


  fun cps_C ({C,CD,CM,A}:cps_schemes) : (exp -> (cps -> cps) -> cps) =
      fn exp => fn c =>
         case exp
          of EXP'Raise exp              => A exp (fn atom => c (CPS'Raise atom))
           | EXP'App (func,args)        => if (!right_to_left_args)
                                           then cpsSequence A (rev args) (fn args => A func (fn func => c (CPS'App (func,rev args))))
                                           else A func (fn func => cpsSequence A args (fn args => c (CPS'App (func,args))))
           | EXP'Handle (exp,match)     => c (CPS'Handle (C exp Identity, CM match))
           | EXP'Let (dec,exp)          => CD dec (C exp c)
           | EXP'Case (exp,match)       => A exp (fn atom => c (cps'case (atom, CM match)))
           | _                          => A exp (fn atom => c (CPS'Return atom))


  fun cps_CD ({C,CD,CM,A}:cps_schemes) : (dec -> cps -> cps) =
      fn dec => fn body =>
         case dec
          of DEC'Empty                  => body
           | DEC'Seq (dec1,dec2)        => CD dec1 (CD dec2 body)
           | DEC'Valrec binds           => cps'letrec (map (fn (x,f) => (x,cps'Func C f)) binds, body)
           | DEC'Val (pat,exp)          => LL (C exp) (fn cps => transform_cps'let (pat, cps, body))


  fun cps_CM ({C,CD,CM,A}:cps_schemes) : (match -> cmatch) =
      fn match =>
         case match
          of MATCH'Alt (m1,m2)          => cmatch'alt (CM m1, CM m2)
           | MATCH'Rule (pat,exp)       => cmatch'rule (pat, C exp Identity)


  fun cps_A ({C,CD,CM,A}:cps_schemes) : (exp -> (atom -> cps) -> cps) =
      fn exp => fn c =>
         case exp
          of EXP'Value (v,_)            => c (ATOM'Value v)
           | EXP'Var x                  => c (ATOM'Var x)
           | EXP'Fn func                => c (ATOM'Fn (cps'Func C func))
           | EXP'NewEx x                => c (ATOM'NewEx x)
           | EXP'Tuple exps             => if (!right_to_left_tuples)
                                           then cpsSequence A (rev exps) (fn atoms => c (atom'tuple (rev atoms)))
                                           else cpsSequence A exps (fn atoms => c (atom'tuple atoms))
           | EXP'PrimApp (prim,exp)     => A exp (fn atom => MaybeBind'effects (effects'prim prim) (ATOM'PrimApp (prim,atom)) c)
           | _                          => LL (C exp) (fn CPS'Return atom => c(atom)
                                                        | cps => ContinuationLetBind cps c)

(*----------------------------------------------------------------------
INDEX: fixup cps schemes
----------------------------------------------------------------------*)

  local
      fun makeSchemes () = {C=C,CD=CD,CM=CM,A=A}
      and C  x  = cps_C     (makeSchemes()) x
      and CD x  = cps_CD    (makeSchemes()) x
      and CM  x = cps_CM    (makeSchemes()) x
      and A   x = cps_A     (makeSchemes()) x
  in val transformToCps =
         let val {C,CD,CM,A} = makeSchemes()
         in CD
         end
  end

(*----------------------------------------------------------------------
INDEX: Cps_Fv - cps types (with embedded free var info)
----------------------------------------------------------------------*)


structure Cps_Fv =
struct

  datatype cps
    = CPS'Return    of atom
    | CPS'Raise     of atom
    | CPS'Let       of bind * cps
    | CPS'Case      of atom * cmatch
    | CPS'App       of atom * atom list
    | CPS'Handle    of cps * {save:uid list} * cmatch
    | CPS'LetC      of {save:uid list} * pat * cps * cps

  and atom
    = ATOM'Value    of value
    | ATOM'Var      of uid
    | ATOM'NewEx    of vid
    | ATOM'Tuple    of atom list
    | ATOM'PrimApp  of prim * atom
    | ATOM'Fn       of afunc
    | ATOM'Let      of bind * atom
    | ATOM'Case     of atom * amatch

  and bind
    = BIND'Val      of pat * atom
    | BIND'ValRec   of (uid * afunc) list

  and afunc = AFUNC'Def of {save:uid list} * uid list * cps

  and cmatch
    = CMATCH'Alt    of cmatch * cmatch
    | CMATCH'Rule   of pat * cps
    | CMATCH'Atom   of amatch

  and amatch
    = AMATCH'Alt    of amatch * amatch
    | AMATCH'Rule   of pat * atom

end

(*----------------------------------------------------------------------
INDEX: var sets
----------------------------------------------------------------------*)

  datatype varset = VARSET of uid list (* invariant - no dups *)

  fun list'varset (VARSET xs) = xs

  fun varset'push (varset as VARSET xs) x = if (exists (eq'uid x) xs)
                                            then varset
                                            else VARSET (x::xs)

  fun varset'remove (varset as VARSET xs) x = if (exists (eq'uid x) xs)
                                              then VARSET (filter (not o eq'uid x) xs)
                                              else varset

  val varset'empty = VARSET []
  fun varset'single x = VARSET [x]

  fun varset'list xs = fold varset'push varset'empty (rev xs)
  fun varset'union (VARSET xs, varset) = fold varset'push varset (rev xs)

  fun varset'restrict (varset, VARSET xs) = fold varset'remove varset xs

  infix && --
  val op && = varset'union
  val op -- = varset'restrict

(*----------------------------------------------------------------------
INDEX: pat - bound/free
----------------------------------------------------------------------*)

  local
      fun BOUND x {bound,free} = {bound = varset'push bound x, free = free}
      fun FREE x {bound,free} = {bound = bound, free = varset'push free x}
      fun P acc pat =
          case pat of
              PAT'Wild              => acc
            | PAT'V x               => BOUND x acc
            | PAT'Layered (x,pat)   => P (BOUND x acc) pat
            | PAT'Ref pat           => P acc pat
            | PAT'C (_,None)        => acc
            | PAT'C (_,Some pat)    => P acc pat
            | PAT'E (_,None)        => acc
            | PAT'E (_,Some pat)    => P acc pat
            | PAT'G (x,None)        => FREE x acc
            | PAT'G (x,Some pat)    => P (FREE x acc) pat
            | PAT'Scon _            => acc
            | PAT'Tuple pats        => fold P acc (rev pats)
  in val boundAndFree'pat = P {bound = varset'empty, free = varset'empty}
  end

(*----------------------------------------------------------------------
INDEX: freevar calc - Cps -> Cps_Fv
----------------------------------------------------------------------*)

  open Cps
  structure FV = Cps_Fv

  type fv_schemes = {P     : pat       -> {bound: varset, free: varset} * pat,
                     B     : bind      -> {bound: varset, free: varset} * FV.bind,
                     C     : cps       -> {               free: varset} * FV.cps,
                     A     : atom      -> {               free: varset} * FV.atom,
                     F     : afunc     -> {               free: varset} * FV.afunc,
                     CM    : cmatch    -> {               free: varset} * FV.cmatch,
                     AM    : amatch    -> {               free: varset} * FV.amatch}

  fun fv_map F (I,x) = (I,F x)

  fun fv_union P (({free=free1},x),({free=free2},y)) =
      ({free = free1 && free2}, P (x,y))

  fun fv_unionList xs =
      case (fold (fn (free1,atoms) =>
                     fn ({free=free2},atom) =>
                        (free1 && free2, atom::atoms)) (varset'empty,[]) xs)
       of (free,atoms) =>
          ({free=free}, rev atoms)

  fun fb_unionList xs =
      case (fold (fn (free1,bound1,atoms) =>
                     fn ({free=free2,bound=bound2},atom) =>
                        (free1 && free2, bound1 && bound2, atom::atoms)) (varset'empty,varset'empty,[]) xs)
       of (free,bound,atoms) =>
          ({free=free,bound=bound}, rev atoms)

  fun fv_bind P (({bound,free=free1},x),({free=free2},y)) =
      ({free = free1 && (free2 -- bound)}, P (x,y))

  fun fv_CPS'Handle (({free=free1},cps),({free=free2},cmatch)) =
      ({free = free1 && free2}, FV.CPS'Handle (cps,{save = list'varset free2},cmatch))

  fun fv_AFUNC'Def xs ({free},cps) =
      let val free = free -- varset'list xs
      in ({free=free}, FV.AFUNC'Def ({save=list'varset free},xs,cps))
      end

  fun fv_binds binds =
      case (fold (fn (xs,free1,binds) =>
                     fn (x,({free=free2},func)) =>
                        (x::xs, free1 && free2, (x,func)::binds)) ([],varset'empty,[]) binds)
       of (xs,free,binds) =>
          let val bound = varset'list xs
          in ({bound = bound, free = free -- bound}, binds)
          end

  fun fv_C ({P,C,A,B,F,CM,AM}:fv_schemes) =
      fn CPS'Return atom            => fv_map FV.CPS'Return (A atom)
       | CPS'Raise atom             => fv_map FV.CPS'Raise (A atom)
       | CPS'Handle (cps,cmatch)    => fv_CPS'Handle (C cps, CM cmatch)
       | CPS'Case (atom,cmatch)     => fv_union FV.CPS'Case (A atom,CM cmatch)
       | CPS'App (func,args)        => fv_union FV.CPS'App (A func, fv_unionList (map A args))
       | CPS'LetC (pat,cps1,cps2) =>
         let val ({free=freeP,bound=boundP},pat) = P pat
             val ({free=free1},cps1) = C cps1
             val ({free=free2},cps2) = C cps2
             val save = freeP && (free2 -- boundP)
             val cps = FV.CPS'LetC ({save=list'varset save},pat,cps1,cps2)
             val free = free1 && save
         in ({free = free},cps)
         end
       | CPS'Let (bind,cps) =>
         let val ({free=free1,bound=bound1},bind) = B bind
             val ({free=free2},cps) = C cps
             val free = free1 && (free2 -- bound1)
         in ({free = free},FV.CPS'Let (bind,cps))
         end

  fun fv_A ({P,C,A,B,F,CM,AM}:fv_schemes) =
      fn ATOM'Value v               => ({free=varset'empty}, FV.ATOM'Value v)
       | ATOM'Var x                 => ({free=varset'single x}, FV.ATOM'Var x)
       | ATOM'Tuple atoms           => fv_map FV.ATOM'Tuple (fv_unionList (map A atoms))
       | ATOM'Case (atom,amatch)    => fv_union FV.ATOM'Case (A atom, AM amatch)
       | ATOM'Fn func               => fv_map FV.ATOM'Fn (F func)
       | ATOM'NewEx x               => ({free=varset'empty}, FV.ATOM'NewEx x)
       | ATOM'PrimApp (prim,atom)   => fv_map (fn a => FV.ATOM'PrimApp (prim,a)) (A atom)
       | ATOM'Let (bind,atom) =>
         let val ({free=free1,bound=bound1},bind) = B bind
             val ({free=free2},atom) = A atom
             val free = free1 && (free2 -- bound1)
         in ({free = free},FV.ATOM'Let (bind,atom))
         end

  fun fv_B ({P,C,A,B,F,CM,AM}:fv_schemes) =
      fn BIND'Val (pat,atom) => let val ({free=freeP,bound=bound},pat) = P pat
                                    val ({free=free1},atom) = A atom
                                    val free = free1 && freeP
                                in ({free = free, bound = bound},FV.BIND'Val (pat,atom))
                                end
       | BIND'ValRec binds => let val ({bound,free},binds) = fv_binds (map (fn (x,f) => (x, F f)) binds)
                              in ({free = free, bound = bound},FV.BIND'ValRec binds)
                              end

  fun fv_F ({P,C,A,B,F,CM,AM}:fv_schemes) =
      fn AFUNC'Def (xs,cps)         => fv_AFUNC'Def xs (C cps)

  fun fv_CM ({P,C,A,B,F,CM,AM}:fv_schemes) =
      fn CMATCH'Alt (cm1,cm2)       => fv_union FV.CMATCH'Alt (CM cm1, CM cm2)
       | CMATCH'Rule (pat,cps)     => fv_bind FV.CMATCH'Rule (P pat, C cps)
       | CMATCH'Atom am             => fv_map FV.CMATCH'Atom (AM am)

  fun fv_AM ({P,C,A,B,F,CM,AM}:fv_schemes) =
      fn AMATCH'Alt (am1,am2)       => fv_union FV.AMATCH'Alt (AM am1, AM am2)
       | AMATCH'Rule (pat,atom)     => fv_bind FV.AMATCH'Rule (P pat, A atom)

(*----------------------------------------------------------------------
INDEX: fix-up fv-calc schemes
----------------------------------------------------------------------*)

  local
      fun P pat = (boundAndFree'pat pat, pat)
      fun makeSchemes () = {P=P,C=C,A=A,B=B,F=F,CM=CM,AM=AM}
      and C   x = fv_C  (makeSchemes()) x
      and A   x = fv_A  (makeSchemes()) x
      and B   x = fv_B  (makeSchemes()) x
      and F   x = fv_F  (makeSchemes()) x
      and CM  x = fv_CM (makeSchemes()) x
      and AM  x = fv_AM (makeSchemes()) x
  in val fv_schemes = makeSchemes()
  end

  open Cps_Fv

  type Embed_dec = Embed.dec

  fun cpsTransform (embed_dec,{bound=xs}) =
      let val body = Cps.CPS'Return (Cps.atom'tuple (map Cps.ATOM'Var xs))
          val cps_nofv = transformToCps embed_dec body
          val {C=calculateFreeVars,P,A,B,F,CM,AM} = fv_schemes
          val (_,cps) = calculateFreeVars cps_nofv
      in cps
      end

(*----------------------------------------------------------------------
INDEX: N_lay'* - patm,atom
----------------------------------------------------------------------*)

  fun lay'vid x = Word (sof'vid x)
  fun lay'uid x = Word (sof'uid x)
  fun lay'con x = Word (sof'con x)

  fun lay_dots _ = Word "..."

  fun lay_commas [] = Blank
    | lay_commas [x] = Word "."
    | lay_commas (xs as _::_::_) = Word (implode (copy (length xs-1) #","))

  fun U_lay'pat PS P =
      fn PAT'Wild               => Word "_"
       | PAT'V x                => lay'uid x
       | PAT'Layered (x,pat)    => lay'uid x ++ Word "as" ++ P pat
       | PAT'Ref pat            => Word "ref" ++ P pat
       | PAT'C (x,None)         => lay'con x
       | PAT'C (x,Some pat)     => lay'con x ++ P pat
       | PAT'E (ex,None)        => lay'vid (vid'exname ex)
       | PAT'E (ex,Some pat)    => lay'vid (vid'exname ex) ++ P pat
       | PAT'G (x,None)         => lay'uid x
       | PAT'G (x,Some pat)     => lay'uid x ++ P pat
       | PAT'Scon scon          => Pretty.lay'scon scon
       | PAT'Tuple pats         => layBracket (PS pats)


  fun N_lay'pat 0 pat = Word "<pat>"
    | N_lay'pat 1 pat = U_lay'pat lay_dots lay_dots pat
    | N_lay'pat n pat = U_lay'pat (layCommaSep o map (N_lay'pat (n-1))) (N_lay'pat (n-1)) pat


  fun lay'save xs = layBrace (layCommaSep (map lay'uid xs))

  type lay_schemes = {P     : pat -> layout,
                      C     : cps -> layout,
                      A     : atom -> layout,
                      B     : bind -> layout,
                      F     : afunc -> layout,
                      AS    : atom list -> layout,
                      CM    : layout -> cmatch -> layout,
                      AM    : layout -> amatch -> layout}

  fun lay'let1 key p e = key `` p @@ Word "=" //`` e @@ Word "in"

  fun lay'letrec F [] = Word "letrec[]in"
    | lay'letrec F [(x1,f1)] = (Word "letrec" `` lay'uid x1 @@ Word "=" )
                               //`` F f1 @@ Word "in"
    | lay'letrec F binds = (Word "letrec" )
                           //`` (layVert (map (fn (x,f) => lay'uid x @@ Word "=" `` F f) binds)) @@ Word "in"

  fun lay'rule p e = p @@ Word "=>" `` e

  fun layAngle x = Left "<" @@ x ++ Right ">"

  fun layArgs lay args = layAngle (layCommaSep (map lay args))


  fun lay_AM ({P,C,A,B,F,AS,CM,AM}:lay_schemes) left_context =
      fn AMATCH'Alt (am1,am2)           => AM (AM left_context am1 // Right " |") am2
       | AMATCH'Rule (pat,atom)         => (lay'rule (left_context `` P pat) (A atom))

  fun lay_CM ({P,C,A,B,F,AS,CM,AM}:lay_schemes) left_context =
      fn CMATCH'Alt (cm1,cm2)           => CM (CM left_context cm1 // Right " |") cm2
       | CMATCH'Rule (pat,cps)          => (lay'rule (left_context `` P pat) (C cps))
       | CMATCH'Atom am                 => AM left_context am

  fun lay'case key e M m = M (key `` e // Word "of") m

  fun lay_C ({P,C,A,B,F,AS,CM,AM}:lay_schemes) =
      fn CPS'Return atom                => Word "return" `` A atom
       | CPS'Raise atom                 => Word "raise" `` A atom
       | CPS'Case (atom,cmatch)         => lay'case (Word "case") (A atom) CM cmatch
       | CPS'App (func,args)            => A func `` layArgs A args
       | CPS'Handle (cps,{save},cmatch) => C cps ++ CM (Word "handle" `` lay'save save) cmatch
       | CPS'LetC ({save},pat,cps1,cps2) => lay'let1 (Word "let" `` lay'save save) (P pat) (C cps1) // (C cps2)
       | CPS'Let (bind,cps)             => B bind // C cps

  fun lay_A ({P,C,A,B,F,AS,CM,AM}:lay_schemes) =
      fn ATOM'Value v                   => lay'value v
       | ATOM'Var x                     => lay'uid x
       | ATOM'Tuple atoms               => layBracket (AS atoms)
       | ATOM'Case (atom,amatch)        => lay'case (Word "caseA") (A atom) AM amatch
       | ATOM'Fn afunc                  => F afunc
       | ATOM'NewEx x                   => Word "exception" `` lay'vid x
       | ATOM'PrimApp (prim,atom)       => Word ("prim-"^name'prim prim) @@ A atom
       | ATOM'Let (bind,atom)           => B bind // A atom

  fun lay_B ({P,C,A,B,F,AS,CM,AM}:lay_schemes) =
      fn BIND'Val (pat,atom)            => lay'let1 (Word "letA") (P pat) (A atom)
       | BIND'ValRec binds              => lay'letrec F binds

  fun lay_F ({P,C,A,B,F,AS,CM,AM}:lay_schemes) =
      fn AFUNC'Def ({save},xs,cps)      => lay'rule (Word "fn" `` lay'save save `` layArgs lay'uid xs) (C cps)

  fun lay_AS ({P,C,A,B,F,AS,CM,AM}:lay_schemes) =
      fn atoms =>
         layCommaSep (map A atoms)

(*----------------------------------------------------------------------
INDEX: full layout
----------------------------------------------------------------------*)

  fun lay'pat pat = U_lay'pat (layCommaSep o map lay'pat) lay'pat pat (* full *)

  local
      fun P   x = lay'pat x
      fun makeSchemes () = {P=P,C=C,A=A,B=B,F=F,AS=AS,CM=CM,AM=AM}
      and C   x = lay_C     (makeSchemes()) x
      and A   x = lay_A     (makeSchemes()) x
      and B   x = lay_B     (makeSchemes()) x
      and F   x = lay_F     (makeSchemes()) x
      and AS  x = lay_AS    (makeSchemes()) x
      and CM  x = lay_CM  (makeSchemes()) x
      and AM  x = lay_AM  (makeSchemes()) x
  in val lay_schemes = makeSchemes()
  end

  fun lay'cps x =
      let val {P,C,A,B,F,AS,CM,AM} = lay_schemes
      in C x
      end

  fun lay'cmatch x =
      let val {P,C,A,B,F,AS,CM,AM} = lay_schemes
      in CM Blank x
      end

  fun lay'afunc x =
      let val {P,C,A,B,F,AS,CM,AM} = lay_schemes
      in F x
      end

(*----------------------------------------------------------------------
INDEX: depth restricted layout
----------------------------------------------------------------------*)

  local
      fun P0 _ = Word "<pat>"
      fun A0 _ = Word "<atom>"
      fun B0 _ = Word "<bind>"
      fun C0 _ = Word "<cps>"
      fun F0 _ = Word "<afunc>"
      fun AS0 _ = Word "<atoms>"
      fun CM0 left_context _ = left_context ++ Word "<cmatchf>"
      fun AM0 left_context _ = left_context ++ Word "<amatchf>"
      val P = N_lay'pat
      fun schemes 0 = {P=P0,C=C0,A=A0,B=B0,F=F0,AS=AS0,CM=CM0,AM=AM0}
        | schemes n = {P=P n,C=C n,A=A n,B=B n, F=F n,AS=AS n,CM=CM n,AM=AM n}
      and C   n x = lay_C     (schemes(n-1)) x
      and A   n x = lay_A     (schemes(n-1)) x
      and B   n x = lay_B     (schemes(n-1)) x
      and F   n x = lay_F     (schemes(n-1)) x
      and AS  n x = lay_AS    (schemes(n-1)) x
      and CM  n x = lay_CM    (schemes(n-1)) x
      and AM  n x = lay_AM    (schemes(n-1)) x
  in fun N_lay_schemes {depth=n} = schemes n
  end

  fun N_lay'atom n x =
      let val {P,C,A,B,F,AS,CM,AM} = N_lay_schemes n
      in A x
      end

  fun N_lay'cmatch n x =
      let val {P,C,A,B,F,AS,CM,AM} = N_lay_schemes n
      in CM Blank x
      end

  fun N_lay'cps_tail n x =
      let val {P,C,A,B,F,AS,CM,AM} = N_lay_schemes n
          fun CT cps =
              case cps
               of CPS'LetC (_,_,_,cps)              => CT cps
                | CPS'Let (_,cps)                   => CT cps
                | CPS'Handle (cps,{save},cmatch)    => CT cps
                | _ => C cps
      in CT x
      end


end (* functor *)
