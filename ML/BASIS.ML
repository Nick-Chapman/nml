(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  15.	 export
  136.	 imports
  209.	 functor
  231.	 env
  314.	 inter
  360.	 env'restrict
  410.	 basis
  452.	 layout
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature BasisSig =
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
	type tyenv 
	type valenv 
	type env
		 
	val strenv'empty	: strenv
	val tyenv'empty		: tyenv
	val valenv'empty	: valenv
	val env'empty		: env

	val env'se_te_ve	: strenv * tyenv * valenv -> env
	val env'strenv		: strenv -> env
	val env'tyenv		: tyenv -> env
	val env'valenv		: valenv -> env

	val strenv'plus		: strenv -> strenv -> strenv
	val tyenv'plus		: tyenv -> tyenv -> tyenv
	val valenv'plus		: valenv -> valenv -> valenv
	val env'plus		: env -> env -> env
	val tyenv'plusList	: tyenv list -> tyenv
	val valenv'plusList : valenv list -> valenv
	val env'plusList	: env list -> env

	val strenv'env		: env -> strenv
	val tyenv'env		: env -> tyenv
	val valenv'env		: env -> valenv
								 
	val strenv'single	: strid * env -> strenv
	val tyenv'single	: tycon * valenv -> tyenv
	val valenv'single	: vid * (value * idstatus) -> valenv
	val strenv'list		: (strid * env) list -> strenv

	val isUnbound'vid	: env -> vid -> bool
	val look'longstrid	: string -> env -> strid long -> env
	val look'longtycon	: string -> env -> tycon long -> valenv
	val look'longvid	: string -> env -> vid long -> value * idstatus

	type ('a,'b) assoc
	val assoc'valenv	: valenv -> (vid, value * idstatus) assoc
	val valenv'assoc	: (vid, value * idstatus) assoc -> valenv

	(*  extra for CompEnv.. *)

	val look'strid		: string -> strenv -> strid -> env
	val look'tycon		: string -> tyenv -> tycon -> valenv
(*
	val look'vid		: string -> valenv -> vid -> value * idstatus
*)

	type strinter
	type tyinter
	type valinter
	type inter

	val strinter'empty	: strinter
	val tyinter'empty	: tyinter
	val valinter'empty	: valinter

	val inter'strinter	: strinter -> inter
	val inter'tyinter	: tyinter -> inter
	val inter'valinter	: valinter -> inter

	val strinter'plusList  : strinter list -> strinter
	val tyinter'plusList  : tyinter list -> tyinter
	val valinter'plusList  : valinter list -> valinter
									  
	val strinter'single : strid * inter -> strinter
	val tyinter'single	: tycon * valinter -> tyinter
	val valinter'single : vid * idstatus -> valinter

	val tyinter'list	: (tycon * valinter) list -> tyinter

	val inter'empty		: inter
	val inter'plus		: inter -> inter -> inter
	val inter'plusList	: inter list -> inter


	val strinter'inter	: inter -> strinter
	val tyinter'inter	: inter -> tyinter
	val valinter'inter	: inter -> valinter
				  
	val lookup'strinter : strinter -> strid -> inter option
	val lookup'tyinter	: tyinter -> tycon -> valinter option
	val lookup'valinter : valinter -> vid -> idstatus option


	val env'restrict	: env -> inter -> env

				
	type functorclosure
	val functorclosure'fn : (env -> env) -> functorclosure
	val apply'functorclosure : functorclosure -> env -> env

	type funenv
	type sigenv
	val funenv'empty	: funenv
	val sigenv'empty	: sigenv
	val funenv'plus		: funenv -> funenv -> funenv
	val sigenv'plus		: sigenv -> sigenv -> sigenv
	val funenv'plusList : funenv list -> funenv
	val sigenv'plusList : sigenv list -> sigenv
	val funenv'single	: funid * functorclosure -> funenv
	val sigenv'single	: sigid * inter -> sigenv

	type basis
	val basis'empty		: basis
	val basis'plus		: basis -> basis -> basis
	val basis'funenv	: funenv -> basis
	val basis'sigenv	: sigenv -> basis
	val basis'env		: env -> basis
	val look'funid		: basis -> funid -> functorclosure
	val look'sigid		: basis -> sigid -> inter
	val env'basis		: basis -> env
								   
	type layout
	val lay'basis		: basis -> layout
end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ../prelude/Prel.ML *)
signature PrelSig_Import =
sig
	val abort			: string -> 'a
	val fold			: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
	val foldR			: ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AssocSig ../prelude/ASSOC.ML *)
signature AssocSig_Import =
sig
	type ('a,'b) assoc
	val assoc'empty     : ('a,'b) assoc
	val assoc'single    : 'a * 'b -> ('a,'b) assoc
	val assoc'list      : ('a * 'b) list -> ('a,'b) assoc
	val assoc'plus      : ('a,'b) assoc -> ('a,'b) assoc -> ('a,'b) assoc
															
	val list'assoc      : ('a,'b) assoc -> ('a * 'b) list
										   
	val lookup'assoc    : (''a,'b) assoc -> ''a -> 'b option
	val look'assoc      : string -> (''a -> string) -> (''a,'b) assoc -> ''a -> 'b
end;
(*IMPORTDONE:*)

(*IMPORTSIG: MisclaySig ../prelude/MISCLAY.ML *)
signature MisclaySig_Import =
sig
  type layout
  val Word                      : string -> layout
  val Left                      : string -> layout
  val Right                     : string -> layout
  val //                        : layout * layout -> layout
  val @@                        : layout * layout -> layout
  val ++                        : layout * layout -> layout
  val ``                        : layout * layout -> layout
  val //``                      : layout * layout -> layout
  val layVert                   : layout list -> layout
  val layTuple                  : layout list -> layout
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AtomSig ATOM.ML *)
signature AtomSig_Import =
sig
	datatype id = ID of string
	datatype vid = VID of id
	datatype tycon = TYCON of id
	datatype strid = STRID of id
	datatype sigid = SIGID of id
	datatype funid = FUNID of id
	datatype 'a long = LONG of strid list * 'a
	val sof'vid : vid -> string
	val sof'tycon : tycon -> string
	val sof'strid : strid -> string
	val sof'sigid : sigid -> string
	val sof'funid : funid -> string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: ValueSig VALUE.ML *)
signature ValueSig_Import =
sig
	type vid
	type value
	type layout
	val lay'value		: value -> layout
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor BASIS(structure Prel : PrelSig_Import
			  structure Assoc : AssocSig_Import
			  structure Misclay : MisclaySig_Import
			  structure Atom : AtomSig_Import
			  structure Value : ValueSig_Import
			  sharing type Misclay.layout = Value.layout
			  sharing type Atom.vid = Value.vid
								 ) : BasisSig =
struct
			  
  infix // @@ ++ `` //``
  open Prel Assoc Misclay Atom Value

  (* force import as datatype -- avoid eq-type prob *)
  val U = (ID,VID,STRID,TYCON,SIGID,FUNID)

  datatype idstatus = IDSTATUS'C | IDSTATUS'E | IDSTATUS'V

(*----------------------------------------------------------------------
INDEX: env
----------------------------------------------------------------------*)

  datatype valenv = VALENV of (vid,value*idstatus) assoc
  datatype tyenv = TYENV of (tycon,valenv) assoc
  datatype strenv = STRENV of (strid,env) assoc
  and env = ENV of strenv * tyenv * valenv
																
  val strenv'empty = STRENV assoc'empty
  val tyenv'empty = TYENV assoc'empty
  val valenv'empty = VALENV assoc'empty
  val env'empty = ENV (strenv'empty,tyenv'empty,valenv'empty)
				  
  val env'se_te_ve = ENV
  fun env'strenv se = ENV(se,tyenv'empty,valenv'empty)
  fun env'tyenv te = ENV(strenv'empty,te,valenv'empty)
  fun env'valenv ve = ENV(strenv'empty,tyenv'empty,ve)
				  
  fun strenv'plus (STRENV a1) (STRENV a2) = STRENV (assoc'plus a1 a2)
  fun tyenv'plus (TYENV a1) (TYENV a2) = TYENV (assoc'plus a1 a2)
  fun valenv'plus (VALENV a1) (VALENV a2) = VALENV (assoc'plus a1 a2)
	  
  fun env'plus (ENV (se1,te1,ve1)) (ENV (se2,te2,ve2)) =
	  let val se = strenv'plus se1 se2
		  val te = tyenv'plus te1 te2
		  val ve = valenv'plus ve1 ve2
	  in ENV (se,te,ve)
	  end

  (* Use foldR, because assoc'plus takes time proportional to #arg2 *)
  val tyenv'plusList = foldR tyenv'plus tyenv'empty
  val valenv'plusList = foldR valenv'plus valenv'empty
  val env'plusList = foldR env'plus env'empty

  fun strenv'env (ENV (se,_,_)) = se
  fun tyenv'env (ENV (_,te,_)) = te
  fun valenv'env (ENV (_,_,ve)) = ve
				  
  fun strenv'single kv = STRENV (assoc'single kv)
  fun tyenv'single kv = TYENV (assoc'single kv)
  fun valenv'single kv = VALENV (assoc'single kv)
  fun strenv'list kvs = STRENV (assoc'list kvs)

  fun assoc'valenv (VALENV x) = x
  (*fun valenv'assoc a = valenv'plusList (map valenv'single (rev (list'assoc a)))*)
  val valenv'assoc = VALENV

  fun isUnbound'vid (ENV (_,_,VALENV assoc)) vid =
	  case (lookup'assoc assoc vid) of None => true | Some _ => false

  fun my_look'assoc who dotpath sofk a k =
	case (lookup'assoc a k) of
	    Some x => x
	  | None => abort (who^":"^dotpath^sofk k)

  fun look'strid dotpath (STRENV a) = my_look'assoc "look'strid" dotpath sof'strid a
  fun look'tycon dotpath (TYENV a) = my_look'assoc "look'tycon" dotpath sof'tycon a
  fun look'vid dotpath (VALENV a) k = my_look'assoc "look'vid" dotpath sof'vid a k
					
  fun look'long dotpath look component (env as ENV (se,_,_)) (LONG (strids,x)) =
	  case strids of
		  [] => look dotpath (component env) x
		| strid::strids =>
			  let val env = look'strid dotpath se strid 
				  val dotpath = dotpath ^ sof'strid strid ^ "."
			  in look'long dotpath look component env (LONG (strids,x))
			  end
				  
  fun look'longstrid dotpath = look'long dotpath look'strid strenv'env
  fun look'longtycon dotpath = look'long dotpath look'tycon tyenv'env
  fun look'longvid dotpath = look'long dotpath look'vid valenv'env

(*----------------------------------------------------------------------
INDEX: inter
----------------------------------------------------------------------*)

  datatype strinter = STRINTER of (strid,inter) assoc
  and tyinter = TYINTER of (tycon,valinter) assoc
  and valinter = VALINTER of (vid,idstatus) assoc
  and inter = INTER of strinter * tyinter * valinter

  val strinter'empty = STRINTER assoc'empty
  val tyinter'empty = TYINTER assoc'empty
  val valinter'empty = VALINTER assoc'empty

  fun inter'strinter strinter = INTER (strinter,tyinter'empty,valinter'empty)
  fun inter'tyinter tyinter = INTER (strinter'empty,tyinter,valinter'empty)
  fun inter'valinter valinter = INTER (strinter'empty,tyinter'empty,valinter)
					
  fun strinter'plus (STRINTER a1) (STRINTER a2) = STRINTER (assoc'plus a1 a2)
  fun tyinter'plus (TYINTER a1) (TYINTER a2) = TYINTER (assoc'plus a1 a2)
  fun valinter'plus (VALINTER a1) (VALINTER a2) = VALINTER (assoc'plus a1 a2)

  val strinter'plusList = foldR strinter'plus strinter'empty
  val tyinter'plusList = foldR tyinter'plus tyinter'empty
  val valinter'plusList = foldR valinter'plus valinter'empty

  fun strinter'single kv = STRINTER (assoc'single kv)
  fun tyinter'single kv = TYINTER (assoc'single kv)
  fun valinter'single kv = VALINTER (assoc'single kv)

  fun tyinter'list kvs = TYINTER (assoc'list kvs)

  val inter'empty = INTER (strinter'empty,tyinter'empty,valinter'empty)

  fun inter'plus (INTER (si1,ti1,vi1)) (INTER (si2,ti2,vi2)) =
	  let val si = strinter'plus si1 si2
		  val ti = tyinter'plus ti1 ti2
		  val vi = valinter'plus vi1 vi2
	  in INTER (si,ti,vi)
	  end

  val inter'plusList = foldR inter'plus inter'empty

  fun lookup'strinter (STRINTER a) = lookup'assoc a
  fun lookup'tyinter (TYINTER a) = lookup'assoc a
  fun lookup'valinter (VALINTER a) = lookup'assoc a

  fun strinter'inter (INTER (s,_,_)) = s
  fun tyinter'inter	 (INTER (_,t,_)) = t
  fun valinter'inter (INTER (_,_,v)) = v


(*----------------------------------------------------------------------
INDEX: env'restrict
----------------------------------------------------------------------*)

  fun revList'assoc a = rev (list'assoc a)

  fun mapFilter'assoc F a =
	  assoc'list (fold (fn acc => fn kv as (k,_) =>
									 case (F kv) of
										 None => acc
									   | Some v' => (k,v') :: acc
													) [] (revList'assoc a))

  fun ids'restrict oldIds newIds =
	  case (oldIds,newIds) of
		  (IDSTATUS'V,IDSTATUS'C) => abort "assertIdsRestriction:V->C"
		| (IDSTATUS'V,IDSTATUS'E) => abort "assertIdsRestriction:V->E"
		| (IDSTATUS'E,IDSTATUS'C) => abort "assertIdsRestriction:E->C"
		| (IDSTATUS'C,IDSTATUS'E) => abort "assertIdsRestriction:C->E"
		| _ => newIds
			   
  fun valenv'restrict (VALENV a) vi =
	  VALENV (mapFilter'assoc
				  (fn (vid,(value,oldIds)) =>
					  case (lookup'valinter vi vid) of
						  None => None
						| Some newIds => Some (value, ids'restrict oldIds newIds))
				  a)
	  
  fun tyenv'restrict (TYENV a) ti =
	  TYENV (mapFilter'assoc
				 (fn (tycon,ve) =>
					 case (lookup'tyinter ti tycon) of
						 None => None
					   | Some vi => Some (valenv'restrict ve vi))
				 a)

  fun strenv'restrict env'restrict (STRENV a) si =
	  STRENV (mapFilter'assoc
				  (fn (strid,env) =>
					  case (lookup'strinter si strid) of
						  None => None
						| Some inter => Some (env'restrict env inter))
				  a)

  fun env'restrict (ENV (se,te,ve)) (INTER (si,ti,vi)) =
	  let val se' = strenv'restrict env'restrict se si
		  val te' = tyenv'restrict te ti
		  val ve' = valenv'restrict ve vi
	  in ENV (se',te',ve')
	  end

(*----------------------------------------------------------------------
INDEX: basis
----------------------------------------------------------------------*)

  (*datatype functorclosure = FUNCTORCLOSURE of sigid * inter * strexp * basis*)
  datatype functorclosure = FUNCTORCLOSURE of env -> env
  val functorclosure'fn = FUNCTORCLOSURE
  fun apply'functorclosure (FUNCTORCLOSURE f) env = f env

  datatype sigenv = SIGENV of (sigid,inter) assoc
  datatype funenv = FUNENV of (funid,functorclosure) assoc
							
  val funenv'empty = FUNENV assoc'empty
  val sigenv'empty = SIGENV assoc'empty
					 
  fun funenv'plus (FUNENV a1) (FUNENV a2) = FUNENV (assoc'plus a1 a2)
  fun sigenv'plus (SIGENV a1) (SIGENV a2) = SIGENV (assoc'plus a1 a2)

  val funenv'plusList = foldR funenv'plus funenv'empty
  val sigenv'plusList = foldR sigenv'plus sigenv'empty

  fun funenv'single kv = FUNENV (assoc'single kv)
  fun sigenv'single kv = SIGENV (assoc'single kv)

  datatype basis = BASIS of funenv*sigenv*env
  val basis'empty = BASIS (funenv'empty,sigenv'empty,env'empty)
												  
  fun basis'plus (BASIS (fe1,ge1,env1)) (BASIS (fe2,ge2,env2)) =
	  let val fe = funenv'plus fe1 fe2
		  val ge = sigenv'plus ge1 ge2
		  val env = env'plus env1 env2
	  in BASIS (fe,ge,env)
	  end

  fun basis'funenv funenv = BASIS (funenv,sigenv'empty,env'empty)
  fun basis'sigenv sigenv = BASIS (funenv'empty,sigenv,env'empty)
  fun basis'env env = BASIS (funenv'empty,sigenv'empty,env)
					
  fun look'sigid (BASIS (_,SIGENV a,_)) = look'assoc "look'sigid" sof'sigid a
  fun look'funid (BASIS (FUNENV a,_,_)) = look'assoc "look'funid" sof'funid a
  fun env'basis (BASIS (fe,ge,env)) = env

(*----------------------------------------------------------------------
INDEX: layout
----------------------------------------------------------------------*)

  fun lay'notYet s = Left "<..." @@ Word s @@ Right "...>"

  fun lay'vid (VID (ID s)) = (*Word "op" ++ *)Word s
  fun lay'strid (STRID (ID s)) = Word s
  fun lay'tycon (TYCON (ID s)) = Word s
  fun lay'funid (FUNID (ID s)) = Word s
  fun lay'sigid (SIGID (ID s)) = Word s

  fun lay'strenv lay'env (STRENV assoc) =
	  layVert
	  (map (fn (strid,env) =>
			   Word "structure" ++ lay'strid strid ++ Word "=" 
					//`` (Word "struct" //`` lay'env env // Word "end")
			   ) (revList'assoc assoc))
							 
  fun lay'constructors (VALENV assoc) =
	  layTuple (map (fn (vid,_) => lay'vid vid) (revList'assoc assoc))

  fun lay'tyenv (TYENV assoc) =
	  layVert
	  (map (fn (tycon,valenv) =>
			   Word "type" ++ lay'tycon tycon ++ Word "=" ++ lay'constructors valenv
			   ) (revList'assoc assoc))
							 
  fun lay'valenv (VALENV assoc) =
	  layVert
	  (map (fn (vid,(value,ids)) =>
			   case ids of
				   IDSTATUS'V => Word "val" ++ lay'vid vid ++ Word "=" ++ lay'value value
				 | IDSTATUS'C => Word "con" ++ lay'vid vid
				 | IDSTATUS'E => Word "excon" ++ lay'vid vid
			   ) (revList'assoc assoc))
					
  fun lay'env (ENV (s,t,v)) =
	  lay'strenv lay'env s // lay'tyenv t // lay'valenv v
	  
  fun lay'funenv (FUNENV assoc) =
	  layVert
	  (map (fn (funid,fc) =>
			   Word "functor" ++ lay'funid funid
			   ) (revList'assoc assoc))
	   
  fun lay'strinter lay'inter (STRINTER assoc) =
	  layVert
	  (map (fn (strid,inter) =>
			   Word "structure" ++ lay'strid strid ++ Word "="
					//`` (Word "struct" //`` lay'inter inter // Word "end")
			   ) (revList'assoc assoc))

  fun lay'constructors (VALINTER assoc) =
	  layTuple (map (fn (vid,_) => lay'vid vid) (revList'assoc assoc))
							 
  fun lay'tyinter (TYINTER assoc) =
	  layVert
	  (map (fn (tycon,valinter) =>
			   Word "type" ++ lay'tycon tycon ++ Word "=" ++ lay'constructors valinter
			   ) (revList'assoc assoc))
							 
  fun lay'valinter (VALINTER assoc) =
	  layVert
	  (map (fn (vid,ids) =>
			   case ids of
				   IDSTATUS'V => Word "val" ++ lay'vid vid
				 | IDSTATUS'C => Word "con" ++ lay'vid vid
				 | IDSTATUS'E => Word "excon" ++ lay'vid vid
			   ) (revList'assoc assoc))
					
  fun lay'inter (INTER (s,t,v)) =
	  lay'strinter lay'inter s // lay'tyinter t // lay'valinter v

  fun lay'sigenv (SIGENV assoc) =
	  layVert
	  (map (fn (sigid,inter) =>
			   Word "signature" ++ lay'sigid sigid ++ Word "="
					//`` (Word "sig" //`` lay'inter inter // Word "end")
			   ) (revList'assoc assoc))
	  
  fun lay'basis (BASIS (fe,ge,env)) =
	  lay'sigenv ge // lay'funenv fe // lay'env env (* sigs before functors *)
									  
end
