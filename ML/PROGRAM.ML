(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  14.    export
  26.    imports
  179.   functor
  215.   sig/spec
  265.   strexp/strdec
  303.   sigdec/fundec
  349.   topdec
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature ProgramSig =
sig
    type basis
    type topdec
    type exval
    val eval'program : basis -> topdec list -> (basis,exval) either
end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: EvalSig INTERPRETER.ML *)
signature EvalSig_Import =
sig
    type dec
    type env
    type exval
    val eval'dec : env -> dec -> (env,exval) either
end;
(*IMPORTDONE:*)

(*IMPORTSIG: PrelSig ../prelude/PREL.ML *)
signature PrelSig_Import =
sig
    val abort           : string -> 'a
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
    type ty;
    type dec;
    type mrule;
    type match;
    type exprow;
    type exp;
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
    val makeItDec : exp -> dec
end;
(*IMPORTDONE:*)

(*IMPORTSIG: ValueSig VALUE.ML *)
signature ValueSig_Import =
sig
    type vid
    type lab
    type scon
    type value
    type exval
end;
(*IMPORTDONE:*)

(*IMPORTSIG: BasisSig BASIS.ML *)
signature BasisSig_Import =
sig
    type 'a long
    type vid
    type tycon
    type strid
    type sigid
    type funid
    type value
    datatype idstatus = IDSTATUS'C | IDSTATUS'E | IDSTATUS'V
    type strenv
    type env

    val env'empty       : env
    val env'strenv      : strenv -> env
    val env'plus        : env -> env -> env
    val strenv'single   : strid * env -> strenv
    val strenv'list     : (strid * env) list -> strenv
    val look'longstrid  : string -> env -> strid long -> env
    type strinter
    type tyinter
    type valinter
    type inter
    val valinter'empty  : valinter
    val inter'strinter  : strinter -> inter
    val inter'tyinter   : tyinter -> inter
    val inter'valinter  : valinter -> inter
    val strinter'plusList  : strinter list -> strinter
    val tyinter'plusList  : tyinter list -> tyinter
    val valinter'plusList  : valinter list -> valinter

    val strinter'single : strid * inter -> strinter
    val tyinter'single  : tycon * valinter -> tyinter
    val valinter'single : vid * idstatus -> valinter
    val inter'empty     : inter
    val inter'plus      : inter -> inter -> inter
    val inter'plusList  : inter list -> inter
    val env'restrict    : env -> inter -> env

    type functorclosure
    val functorclosure'fn : (env -> env) -> functorclosure
    val apply'functorclosure : functorclosure -> env -> env
    type funenv
    type sigenv
    val funenv'plusList : funenv list -> funenv
    val sigenv'plusList : sigenv list -> sigenv
    val funenv'single   : funid * functorclosure -> funenv
    val sigenv'single   : sigid * inter -> sigenv
    type basis
    val basis'empty     : basis
    val basis'plus      : basis -> basis -> basis
    val basis'funenv    : funenv -> basis
    val basis'sigenv    : sigenv -> basis
    val basis'env       : env -> basis
    val look'funid      : basis -> funid -> functorclosure
    val look'sigid      : basis -> sigid -> inter
    val env'basis       : basis -> env

end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor PROGRAM
            (structure Prel : PrelSig_Import
             structure Lang : LangSig_Import
             structure Value : ValueSig_Import
             structure Basis : BasisSig_Import
             structure Eval : EvalSig_Import
             sharing type Lang.lab = Value.lab
             sharing type Lang.scon = Value.scon
             sharing type Lang.long = Basis.long
             sharing type Lang.vid = Value.vid = Basis.vid
             sharing type Lang.strid = Basis.strid
             sharing type Lang.tycon = Basis.tycon
             sharing type Lang.funid = Basis.funid
             sharing type Lang.sigid = Basis.sigid
             sharing type Value.value = Basis.value
             sharing type Value.exval = Eval.exval
             sharing type Basis.env = Eval.env
             sharing type Lang.dec = Eval.dec
                               ) : ProgramSig =
struct

  open Prel Lang Value Basis (*Eval*)

  fun notYet s = abort ("notYet: eval_module: "^s)

  exception ModPack of exval

  fun mayRaise_eval'dec DEC env dec =
      case (DEC env dec) of
          This env => env
        | That exval => raise (ModPack exval)

(*----------------------------------------------------------------------
INDEX: sig/spec
----------------------------------------------------------------------*)

  fun eval'sigexp_U SPEC SIG basis sigexp =
      case sigexp of
          SIG'Spec spec => SPEC spec
        | SIG'Id sigid => look'sigid basis sigid
        | SIG'TypeRealisation (sigexp,_) => SIG sigexp

  fun eval'spec_U SPEC SIG spec =
      case spec of
          SPEC'Val xs =>
          inter'valinter (valinter'plusList (map (fn (vid,_) =>
                                                     valinter'single (vid,IDSTATUS'V))
                                                 xs))
        | SPEC'Type xs =>
          inter'tyinter (tyinter'plusList (map (fn (_,tycon,_) =>
                                                   tyinter'single (tycon,valinter'empty))
                                               xs))
        | SPEC'Eqtype xs =>
          inter'tyinter (tyinter'plusList (map (fn (_,tycon) =>
                                                   tyinter'single (tycon,valinter'empty))
                                               xs))
        | SPEC'Datatype xs =>
          inter'plusList
          (map (fn (_,tycon,constructors) =>
                   let val vi = valinter'plusList (map (fn (vid,_) =>
                                                           valinter'single (vid,IDSTATUS'C))
                                                       constructors)
                   in
                       inter'plus (inter'valinter vi) (inter'tyinter (tyinter'single (tycon,vi)))
                   end) xs)
        | SPEC'DataReplicate x => notYet "SPEC'DataReplicate"
        | SPEC'Exception xs =>
          inter'valinter (valinter'plusList (map (fn (vid,_) =>
                                                     valinter'single (vid,IDSTATUS'E))
                                                 xs))
        | SPEC'Structure xs =>
          inter'strinter (strinter'plusList (map (fn (strid,sigexp) =>
                                                     strinter'single (strid,SIG sigexp))
                                                 xs))
        | SPEC'Include sigexp => SIG sigexp
        | SPEC'Empty => inter'empty
        | SPEC'Seq (spec1,spec2) => inter'plus (SPEC spec1) (SPEC spec2)
        | SPEC'Sharing (spec,_) => SPEC spec

  fun eval'sigexp basis x = eval'sigexp_U (eval'spec basis) (eval'sigexp basis) basis x
  and eval'spec basis x = eval'spec_U (eval'spec basis) (eval'sigexp basis) x

(*----------------------------------------------------------------------
INDEX: strexp/strdec
----------------------------------------------------------------------*)

  fun eval'strexp_U SE SD SIG basis x =
      case x of
          STREXP'Struct strdec => SD basis strdec
        | STREXP'Id stridlong => look'longstrid "" (env'basis basis) stridlong
        | STREXP'TransCon (strexp,sigexp) =>
          env'restrict (SE basis strexp) (SIG basis sigexp)
        | STREXP'OpaqueCon (strexp,sigexp) =>
          env'restrict (SE basis strexp) (SIG basis sigexp)
        | STREXP'FunctorApp (funid,strexp) =>
          apply'functorclosure (look'funid basis funid) (SE basis strexp)
        | STREXP'Let (strdec,strexp) =>
          let val env = SD basis strdec
          in SE (basis'plus basis (basis'env env)) strexp
          end

  fun eval'strdec_U DEC SE SD basis x =
      case x of
          STRDEC'Dec dec => mayRaise_eval'dec DEC (env'basis basis) dec
        | STRDEC'Structure xs =>
          env'strenv(strenv'list (map (fn (strid,strexp) => (strid, SE basis strexp))xs))
        | STRDEC'Local (sd1,sd2) =>
          let val basis1 = basis'env (SD basis sd1)
          in SD (basis'plus basis basis1) sd2
          end
        | STRDEC'Empty => env'empty
        | STRDEC'Seq (sd1,sd2) =>
          let val env1 = SD basis sd1
              val env2 = SD (basis'plus basis (basis'env env1)) sd2
          in env'plus env1 env2
          end

  fun eval'strexp DEC basis x = eval'strexp_U     (eval'strexp DEC) (eval'strdec DEC) eval'sigexp basis x
  and eval'strdec DEC basis x = eval'strdec_U DEC (eval'strexp DEC) (eval'strdec DEC)             basis x

(*----------------------------------------------------------------------
INDEX: sigdec/fundec
----------------------------------------------------------------------*)

  fun eval'sigdec basis x =
      case x of
          SIGDEC xs =>
          sigenv'plusList
              (map (fn (sigid,sigexp) =>
                       let val inter = eval'sigexp basis sigexp
                           val sigenv = sigenv'single (sigid,inter)
                       in sigenv
                       end) xs)

  fun eval'funimport DEC basis x strexp =
      case x of
          FUNIMPORT'Basic (strid,sigexp) =>
          let val inter = eval'sigexp basis sigexp
          in
              functorclosure'fn
              (fn env =>
                  let val env1 = env'strenv (strenv'single (strid, env'restrict env inter))
                      val basis1 = basis'plus basis (basis'env env1)
                  in eval'strexp DEC basis1 strexp
                  end)
          end
        | FUNIMPORT'Derived spec =>
          let val inter = eval'spec basis spec
          in
              functorclosure'fn
              (fn env =>
                  let val env1 = env'restrict env inter
                      val basis1 = basis'plus basis (basis'env env1)
                  in eval'strexp DEC basis1 strexp
                  end)
          end

  fun eval'fundec DEC basis x =
      case x of
          FUNDEC xs =>
          funenv'plusList
          (map (fn (funid,funimport,strexp) =>
                   let val functorclosure = eval'funimport DEC basis funimport strexp
                   in funenv'single (funid,functorclosure)
                   end) xs)

(*----------------------------------------------------------------------
INDEX: topdec
----------------------------------------------------------------------*)

  fun eval'topdec DEC basis x =
      case x of
          TOPDEC'Exp exp => basis'env (mayRaise_eval'dec DEC (env'basis basis) (makeItDec exp))
        | TOPDEC'Strdec x => basis'env (eval'strdec DEC basis x)
        | TOPDEC'Sigdec x => basis'sigenv (eval'sigdec basis x)
        | TOPDEC'Fundec x => basis'funenv (eval'fundec DEC basis x)

  fun eval'topdecs DEC basis [] = basis'empty
    | eval'topdecs DEC basis (x::xs) =
      let val basis1 = eval'topdec DEC basis x
          val basis2 = eval'topdecs DEC (basis'plus basis basis1) xs
      in basis'plus basis1 basis2
      end

  fun G_eval'program DEC basis xs =
      This (eval'topdecs DEC basis xs)
      handle ModPack exval => That exval


  val eval'program = G_eval'program Eval.eval'dec

end
