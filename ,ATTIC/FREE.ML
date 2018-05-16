(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  22.    export
  34.    imports
  53.    functor
  79.    undefs
  95.    pat
  257.   longvid
  263.   mrule, match, exp
  305.   source language generation -- support fvalbind clauses
  336.   dec
  438.   fix -- pat/exp/dec
  449.   module level
  453.   sig/spec
  503.   strexp/strdec
  541.   sigdec/fundec
  587.   topdec
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature FreeSig =
sig
    type basis
    type topdec
    type exval
    val free'program : basis -> topdec list -> (basis,exval) either
end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ../prelude/Prel.ML *)
(*IMPORTDONE:*)

(*IMPORTSIG: AtomSig ATOM.ML *)
(*IMPORTDONE:*)

(*IMPORTSIG: LangSig LANG.ML *)
(*IMPORTDONE:*)

(*IMPORTSIG: ValueSig VALUE.ML *)
(*IMPORTDONE:*)

(*IMPORTSIG: BasisSig BASIS.ML *)
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor FREE
	    (structure Prel : PrelSig_Import
	     structure Atom : AtomSig_Import
	     structure Lang : LangSig_Import
	     structure Value : ValueSig_Import
	     structure Basis : BasisSig_Import
	     sharing type Atom.lab = Lang.lab = Value.lab
	     sharing type Atom.scon = Lang.scon = Value.scon
	     sharing type Atom.long = Lang.long = Basis.long
	     sharing type Atom.vid = Lang.vid = Value.vid = Basis.vid
	     sharing type Atom.strid = Lang.strid = Basis.strid
	     sharing type Atom.tycon = Lang.tycon = Basis.tycon
	     sharing type Atom.funid = Lang.funid = Basis.funid
	     sharing type Atom.sigid = Lang.sigid = Basis.sigid
	     sharing type Value.value = Basis.value
			       ) : FreeSig =
struct

  open Prel Atom Lang Value Basis

  fun notYet s = abort ("notYet: free: "^s)

(*----------------------------------------------------------------------
INDEX: undefs
----------------------------------------------------------------------*)

  datatype undefkind = 
	   UDK'Strid of strid long |
	   UDK'Tycon of tycon long |
	   UDK'Vid of vid long |
	   UDK'Constructor of vid long

  datatype undef = UNDEF of undefkind list

  val undef'empty = UNDEF []
  fun undef'single x = UNDEF [x]
  fun undef'plus (UNDEF xs1) (UNDEF xs2) = UNDEF (xs1 @ xs2)
  fun undef'plusList xs = UNDEF (linkMap (fn (UNDEF ys) => ys) xs)

(*----------------------------------------------------------------------
INDEX: longvid
----------------------------------------------------------------------*)

  fun free'strid env context vid =
      case (lookup'strinter (strinter'inter env) vid) of
	  None => undef'single (UDK'Strid (LONG (rev context,vid)))
	| Some _ => undef'empty

  fun free'tycon env context vid =
      case (lookup'tyinter (tyinter'inter env) vid) of
	  None => undef'single (UDK'Tycon (LONG (rev context,vid)))
	| Some _ => undef'empty

  fun free'vid env context vid =
      case (lookup'valinter (valinter'inter env) vid) of
	  None => undef'single (UDK'Vid (LONG (rev context,vid)))
	| Some _ => undef'empty

  fun free'constructor env context vid =
      case (lookup'valinter (valinter'inter env) vid) of
	  None => undef'single (UDK'Vid (LONG (rev context,vid)))
	| Some ids => 
	  case ids of
	      IDSTATUS'V => undef'single (UDK'Constructor (LONG (rev context,vid)))
	    | IDSTATUS'C => undef'empty
	    | IDSTATUS'E => undef'empty

  fun free'long free'x env (LONG (strids,x)) =
      let
	  fun loop env context strids =
	      case strids of
		  [] => free'x env context x
		| strid::strinds =>
		  (case (lookup'strinter (strinter'inter env) strid) of
		       None => undef'single (UDK'Strid (LONG (rev context,strid)))
		     | Some env => loop env (strid::context) strids)
      in
	  loop env [] strids
      end
		    
  val free'longstrid = free'long free'strid
  val free'longtycon = free'long free'tycon
  val free'longvid = free'long free'vid
  val free'longconstructor = free'long free'constructor

(*----------------------------------------------------------------------
INDEX: pat
----------------------------------------------------------------------*)
			      
  fun maybeGetBindableVid'longvid env (LONG (_::_,vid)) = None
    | maybeGetBindableVid'longvid env (longvid as LONG ([],vid)) =
      if (isUnbound'vid env vid) then (Some vid)
      else let val (_,ids) = look'longvid env longvid
	   in case ids of
		  IDSTATUS'V => Some vid
		| IDSTATUS'C => None
		| IDSTATUS'E => None
	   end


  datatype status =
	   STATUS'V
	 | STATUS'Ref
	 | STATUS'C of vid
	 | STATUS'E of exname

  fun lookStatus'longvid env x = 
      let val (x_value,ids) = look'longvid env x
      in case ids of
	     IDSTATUS'V => STATUS'V
	   | IDSTATUS'C =>
	     let val either = ref_or_con0'value "patcons(C)" x_value
	     in case either of
		    This {Ref=()} => STATUS'Ref
		  | That {Con0=vid} => STATUS'C vid
	     end
	   | IDSTATUS'E =>
	     let val exname = excon0'value "parvar(E)" x_value
	     in STATUS'E exname
	     end
      end

      
  fun valenv'singlfreeue (k,(v,ids)) = valenv'single (k,(recval'value v,ids))
	       
  fun free'vidBinding vid recval = valenv'single (vid,(recval,IDSTATUS'V))
	  
  fun free'patvar env recval x =
      case (maybeGetBindableVid'longvid env x) of
	  Some vid => Some (free'vidBinding vid recval)
	| None =>
	  case (lookStatus'longvid env x) of
	      STATUS'V => error "patvar(V)"
	    | STATUS'Ref => error "patvar(Ref)"
	    | STATUS'C vid =>
	      let val (vid',vOpt) = con'value "patvar(C)" (value'recval recval)
	      in case vOpt of
		     Some _ => None
		   | None => if (eq'vid vid vid')
			     then (Some valenv'empty) else None
	      end
	    | STATUS'E exname =>
	      let val (exname',vOpt) = excon'value "parvar(E)" (value'recval recval)
	      in case vOpt of
		     Some _ => None
		   | None => if (eq'exname exname exname')
			     then (Some valenv'empty) else None
	      end

  fun free'patcons P env v x pat =
      case (lookStatus'longvid env x) of
	  STATUS'V => error "patcons(V)"
	| STATUS'Ref =>
	  let val v' = derefAddr'value "patcons(ref)" v
	  in P env v' pat
	  end
	| STATUS'C vid =>
	  let val (vid',vOpt) = con'value "patcons(C)" v
	  in case vOpt of
		 Some v' => if (eq'vid vid vid')
			    then (P env v' pat) else None
	       | None => None
	  end
	| STATUS'E exname =>
	  let val (exname',vOpt) = excon'value "parvar(E)" v
	  in case vOpt of
		 Some v' => if (eq'exname exname exname')
			    then (P env v' pat) else None
	       | None => None
	  end

  fun free'patrow P env record (PATROW (lps,{wildcard=true})) =
      notYet "patrow:wildcard"
    | free'patrow P env record (PATROW (lps,{wildcard=false})) =
      let
	fun loop accVe [] = Some accVe
	  | loop accVe ((lab,pat)::lps) =
	    let val v = look'record "patrow" record lab
	    in case (P env v pat) of
		 None => None
	       | Some ve => loop (valenv'plus accVe ve) lps
	    end
      in
	loop valenv'empty lps
      end
	  
  fun free'pattuple P env record pats =
      let fun loop accVe n [] = Some accVe
	    | loop accVe n (pat::pats) =
	      let val v = look'record "pattuple" record (LAB'Num n)
	      in case (P env v pat) of
		     None => None
		   | Some ve => loop (valenv'plus accVe ve) (n+1) pats
	      end
      in
	loop valenv'empty 1 pats
      end

  fun free'patlist P env v pats =
      case (pats,listCons'value "patlist" v) of
	  ([],None) => Some valenv'empty
	| (pat::pats,Some(v1,v2)) =>
	  (case (P env v1 pat) of
	       None => None
	     | Some ve1 => (case (free'patlist P env v2 pats) of
				None => None
			      | Some ve2 => Some (valenv'plus ve1 ve2)))
	| _ => None
	       
  fun inner_free'pat_U P env v pat =
      case pat of
	  PAT'Wild => Some valenv'empty
	| PAT'Scon scon =>
	  let val scon' = scon'value "free'pat" v
	  in if (eq'scon scon scon') then Some valenv'empty
	     else None
	  end
	| PAT'Var x => abort "inner_free'pat_U--Var"
	| PAT'Record pr =>
	  let val record = record'value "free'pat" v
	  in free'patrow P env record pr
	  end
	| PAT'Unit =>
	  let val U as () = unit'value "free'pat" v
	  in Some valenv'empty
	  end
	| PAT'Tuple pats =>
	  let val record = record'value "free'pat" v
	  in free'pattuple P env record pats
	  end
	| PAT'List pats => free'patlist P env v pats
	| PAT'Cons (x,pat) => free'patcons P env v x pat
	| PAT'Typed (pat,ty_IGNORED) => P env v pat
	| PAT'Layered (vid,tyo_IGNORED,pat) => abort "inner_free'pat_U--Layered"
					       
  fun free'pat_U P env recval pat =
      case pat of
	  PAT'Var x => free'patvar env recval  x 
	| PAT'Layered (vid,tyo_IGNORED,pat) =>
	  (case (P env recval pat) of
	       None => None
	     | Some ve => Some (valenv'plus (free'vidBinding vid recval) ve))
	| _ =>
	  let fun P' env v pat = P env (recval'value v) pat
	  in inner_free'pat_U P' env (value'recval recval) pat
	  end

(*----------------------------------------------------------------------
INDEX: mrule, match, exp
----------------------------------------------------------------------*)

  fun free'mrule P E env (MRULE (pat,exp)) = 
      let val (env',undef) = P env pat
      in undef'plus undef (E env' exp)
      end

  fun free'match P E env (MATCH xs) = undef'plusList (map (free'mrule P E env) xs)
    
  fun free'exprow E env exprow =
      case exprow of
	  EXPROW xs => undef'plusList (map (fn (lab,exp) => E env exp) xs)
      
  fun free'exp P E D env exp =
      case exp of
	  EXP'Scon scon => undef'empty
	| EXP'Var x => free'longvid env x
	| EXP'Record exprow => free'exprow E env exprow
	| EXP'Select lab => undef'empty
	| EXP'Unit => undef'empty
	| EXP'Tuple exps => undef'plusList (map (E env) exps)
	| EXP'List exps => undef'plusList (map (E env) exps)
	| EXP'Vector exps => undef'plusList (map (E env) exps)
	| EXP'Seq exps => undef'plusList (map (E env) exps)
	| EXP'Let (dec,exp) => E (D env dec) exp
	| EXP'App (e1,e2) => undef'plus (E env e1) (E env e2)
	| EXP'Typed (exp,ty) => E env exp
	| EXP'Andalso (e1,e2) => undef'plus (E env e1) (E env e2)
	| EXP'Orelse (e1,e2) => undef'plus (E env e1) (E env e2)
	| EXP'Handle (exp,match) => undef'plus (E env exp) (free'match P E env match)
	| EXP'Raise exp => E env exp
	| EXP'If (e1,e2,e3) => undef'plus (E env e1) (undef'plus (E env e2) (E env e3))
	| EXP'While (e1,e2) => notYet "while"
	| EXP'Case (exp,match) => undef'plus (E env exp) (free'match P E env match)
	| EXP'Fn match => free'match P E env match


(*----------------------------------------------------------------------
INDEX: source language generation -- support fvalbind clauses
----------------------------------------------------------------------*)

  fun exp'fnVid x body = EXP'Fn (MATCH [MRULE (PAT'Var (LONG ([],x)),body)])

  fun exp'fnVids [] body = body
    | exp'fnVids (x::xs) body = exp'fnVid x (exp'fnVids xs body)
			  
  local val U = ref 1
  in fun genVid () = let val n = !U
		     in U := 1+n; VID (ID ("G#"^stringOfInt n))
		     end
  end

  fun genVids n =
      if (n<0) then abort "genVids:n<0"
      else let fun loop acc n = if n=0 then acc
				else loop (genVid ()::acc) (n-1)
	   in loop [] n
	   end
  fun mrule'clause (pats,tyo_IGNORED,exp) = MRULE (PAT'Tuple pats,exp)
					    
  fun exp'clauses [] = abort "exp'clauses:[]"
    | exp'clauses (clauses as (pats,_,_)::_) =
      let val vids = genVids (length pats)
      in exp'fnVids vids
	 (EXP'Case (EXP'Tuple (map exp'vid vids),
		    MATCH (map mrule'clause clauses)))
      end

(*----------------------------------------------------------------------
INDEX: dec
----------------------------------------------------------------------*)
	       
  fun free'valbind P E env vb =
      case vb of	
	  VALBIND'Rec vb => valenv'Rec (free'valbind P E env vb)
	| VALBIND'Seq (pat,exp,vbo) =>
	  case (P env (E env exp) pat) of
	      None => raise (Pack exval'Bind)
	    | Some ve =>
	      (case vbo of
		   None => ve
		 | Some vb => valenv'plus ve (free'valbind P E env vb))

  fun free'fvalbind Eenv (FVALBIND fs) =
      valenv'Rec (valenv'plusList
		    (map (fn (vid,clauses) =>
			     free'vidBinding vid (Eenv (exp'clauses clauses))
			     ) fs))

  fun free'exbind1 env b =
      case b of
	  EXBIND1'New (vid,IGNORED_tyo) =>
	  let val en = gen'exname vid
	      val v = value'exval (exval'con0 en)
	  in valenv'singlfreeue (vid,(v,IDSTATUS'E))
	  end
	| EXBIND1'Copy (vid,longvid) =>
	  let val (v,ids) = look'longvid env longvid
	  in case ids of
		 IDSTATUS'V => error "exbind(copy):expected status E, got V"
	       | IDSTATUS'C => error "exbind(copy):expected status E, got C"
	       | IDSTATUS'E =>
		 let val exname = excon0'value "free'exbind1(E)" v
		 in valenv'singlfreeue (vid,(v,IDSTATUS'E))
		 end
	  end

  fun free'exbind env (EXBIND bs) =
      valenv'plusList (mapLR (free'exbind1 env) bs)

  fun free'typbind (TYPBIND xs) =
      tyenv'plusList (map (fn (_,tycon,_) =>
			      tyenv'single (tycon,valenv'empty)
			      ) xs)

  fun free'datbind (DATBIND xs) =
      let fun loop ve te [] = (ve,te)
	    | loop ve te ((IGNORED_tyvars,tycon,clauses)::xs) =
	      let val ve' = 
		      valenv'plusList
			(map (fn (vid,IGNORED_tyo) =>
				 let val v = value'con0 vid
				 in valenv'singlfreeue (vid,(v,IDSTATUS'C))
				 end) clauses)
		  val te' = tyenv'single (tycon,ve')
	      in loop (valenv'plus ve ve') (tyenv'plus te te') xs
	      end
      in loop valenv'empty tyenv'empty xs
      end

  fun free'optWithType tbo =
      case tbo of Some tb => free'typbind tb | None => tyenv'empty

  fun free'dec_U P E D env dec = 
      case dec of
	  DEC'Val vb => env'valenv (free'valbind P E env vb)
	| DEC'Fval fb => env'valenv (free'fvalbind (E env) fb)
	| DEC'Type typbind => env'tyenv (free'typbind typbind)
	| DEC'Datatype (db,tbo) =>
	  let val (ve,te) = free'datbind db
	      val env1 = env'plus (env'valenv ve) (env'tyenv te)
	      val env2 = env'tyenv (free'optWithType tbo)
	  in env'plus env1 env2
	  end
	| DEC'DataReplicate (tycon,tyconlong) =>
	  let val ve = look'longtycon env tyconlong
	      val env1 = env'valenv ve
	      val env2 = env'tyenv (tyenv'single (tycon,ve))
	  in env'plus env1 env2
	  end
	| DEC'Abstype (db,tbo,dec) =>
	  let val (ve,te_IGNORED) = free'datbind db
	      val env1 = env'valenv ve
	      val env2 = env'tyenv (free'optWithType tbo)
	      val env3 = D (env'plusList [env,env1,env2]) dec
	  in env'plus env2 env3
	  end
	| DEC'Exception exbind => env'valenv (free'exbind env exbind)
	| DEC'Local (dec1,dec2) =>
	  let val env1 = D env dec1
	  in D (env'plus env env1) dec2
	  end
	| DEC'Open xs => env'plusList (map (look'longstrid env) xs)
	| DEC'Empty => env'empty
	| DEC'Seq (dec1,dec2) =>
	  let val env1 = D env dec1
	      val env2 = D (env'plus env env1) dec2
	  in env'plus env1 env2
	  end

(*----------------------------------------------------------------------
INDEX: fix -- pat/exp/dec
----------------------------------------------------------------------*)

  fun free'patR x = free'pat_U free'patR x
  fun free'pat env v = free'patR env (recval'value v)

  fun free'exp env exp = value'recval (free'expR env exp)
  and free'expR x = free'exp_U free'pat  free'exp  free'dec x
  and free'dec x  = free'dec_U free'patR free'expR free'dec x

(*----------------------------------------------------------------------
INDEX: module level
----------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: sig/spec
----------------------------------------------------------------------*)

  fun free'sigexp_U SPEC SIG basis sigexp =
      case sigexp of
	  SIG'Spec spec => SPEC spec
	| SIG'Id sigid => look'sigid basis sigid
	| SIG'TypeRealisation (sigexp,_) => SIG sigexp

  fun free'spec_U SPEC SIG spec =
      case spec of 
	  SPEC'Val xs =>
	  inter'valinter (valinter'plusList (map (fn (vid,_) =>
						     valinter'single (vid,IDSTATUS'V))
						 xs))
	| SPEC'Type xs =>
	  inter'tyinter (tyinter'plusList (map (fn (_,tycon) =>
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
				   
  fun free'sigexp basis x = free'sigexp_U (free'spec basis) (free'sigexp basis) basis x
  and free'spec basis x = free'spec_U (free'spec basis) (free'sigexp basis) x

(*----------------------------------------------------------------------
INDEX: strexp/strdec
----------------------------------------------------------------------*)

  fun free'strexp_U SE SD SIG basis x =
      case x of
	  STREXP'Struct strdec => SD basis strdec
	| STREXP'Id stridlong => look'longstrid (env'basis basis) stridlong
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
      
  fun free'strdec_U SE SD basis x =
      case x of
	  STRDEC'Dec dec => free'dec (env'basis basis) dec
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

  fun free'strexp basis x = free'strexp_U free'strexp free'strdec free'sigexp basis x
  and free'strdec basis x = free'strdec_U free'strexp free'strdec	      basis x

(*----------------------------------------------------------------------
INDEX: sigdec/fundec
----------------------------------------------------------------------*)

  fun free'sigdec basis x =
      case x of
	  SIGDEC xs =>
	  sigenv'plusList
	      (map (fn (sigid,sigexp) =>
		       let val inter = free'sigexp basis sigexp
			   val sigenv = sigenv'single (sigid,inter)
		       in sigenv
		       end) xs)

  fun free'funimport basis x strexp =
      case x of
	  FUNIMPORT'Basic (strid,sigexp) =>
	  let val inter = free'sigexp basis sigexp
	  in
	      functorclosure'fn
	      (fn env => 
		  let val env1 = env'strenv (strenv'single (strid, env'restrict env inter))
		      val basis1 = basis'plus basis (basis'env env1)
		  in free'strexp basis1 strexp
		  end)
	  end
	| FUNIMPORT'Derived spec => 
	  let val inter = free'spec basis spec
	  in
	      functorclosure'fn
	      (fn env => 
		  let val env1 = env'restrict env inter
		      val basis1 = basis'plus basis (basis'env env1)
		  in free'strexp basis1 strexp
		  end)
	  end

  fun free'fundec basis x =
      case x of
	  FUNDEC xs =>
	  funenv'plusList
	  (map (fn (funid,funimport,strexp) =>
		   let val functorclosure = free'funimport basis funimport strexp
		   in funenv'single (funid,functorclosure)
		   end) xs)

(*----------------------------------------------------------------------
INDEX: topdec
----------------------------------------------------------------------*)
  
  fun free'topdec basis x =
      case x of
	  TOPDEC'Exp exp =>
	  let val v = free'exp (env'basis basis) exp
	      val ve = free'vidBinding vid'it (recval'value v)
	      val env = env'valenv ve
	      val basis = basis'env env
	  in basis
	  end
	| TOPDEC'Strdec x => basis'env (free'strdec basis x)
	| TOPDEC'Sigdec x => basis'sigenv (free'sigdec basis x)
	| TOPDEC'Fundec x => basis'funenv (free'fundec basis x)

      
  fun free'topdecs basis [] = basis'empty
    | free'topdecs basis (x::xs) =
      let val basis1 = free'topdec basis x
	  val basis2 = free'topdecs (basis'plus basis basis1) xs
      in basis'plus basis1 basis2
      end

  fun free'program basis xs =
      This (free'topdecs basis xs)
      handle Pack exval => That exval

end
