

signature EvalSig = sig type basis
                        type topdec
                        type exval
                        val eval'program :
                            (basis -> (topdec list -> (basis,exval) either))
                    end
signature PrelSig_Import = sig val abort : (string -> 'a)
                               val stringOfInt : (int -> string)
                           end
signature AtomSig_Import = sig datatype id = ID of string
                               datatype vid = VID of id
                               type tyvar
                               type tycon
                               type strid
                               type sigid
                               type funid
                               datatype 'a long = LONG of (strid list * 'a)
                               datatype lab = LAB'Id of id | LAB'Num of int
                               type scon
                               val eq'vid : (vid -> (vid -> bool))
                           end
signature LangSig_Import =
            sig type vid
                type tyvar
                type tycon
                type strid
                type sigid
                type funid
                type 'a long
                type lab
                type scon
                type ty
                datatype patrow = PATROW of ((lab * pat) list *
                                             {wildcard : bool})
                and pat = PAT'Wild | PAT'Scon of scon
                          | PAT'Var of vid long
                          | PAT'Record of patrow | PAT'Unit
                          | PAT'Tuple of pat list
                          | PAT'List of pat list
                          | PAT'Cons of (vid long * pat)
                          | PAT'Typed of (pat * ty)
                          | PAT'Layered of (vid * ty option * pat
                                           )
                datatype typbind = TYPBIND of (tyvar list * tycon * ty) list
                datatype datbind = DATBIND of (tyvar list * tycon *
                                               (vid * ty option) list) list
                datatype exbind1 = EXBIND1'New of (vid * ty option)
                                   | EXBIND1'Copy of (vid * vid long)
                datatype exbind = EXBIND of exbind1 list
                datatype valbind = VALBIND'Seq of (pat * exp * valbind option)
                                   | VALBIND'Rec of valbind
                and fvalbind = FVALBIND of (vid * (pat list * ty option * exp)
                                                  list) list
                and dec = DEC'Val of valbind | DEC'Fval of fvalbind
                          | DEC'Type of typbind
                          | DEC'Datatype of (datbind * typbind option)
                          | DEC'DataReplicate of (tycon * tycon long)
                          | DEC'Abstype of (datbind * typbind option * dec)
                          | DEC'Exception of exbind | DEC'Local of (dec * dec)
                          | DEC'Open of strid long list | DEC'Empty
                          | DEC'Seq of (dec * dec)
                and mrule = MRULE of (pat * exp)
                and match = MATCH of mrule list
                and exprow = EXPROW of (lab * exp) list
                and exp = EXP'Scon of scon | EXP'Var of vid long
                          | EXP'Record of exprow | EXP'Select of lab | EXP'Unit
                          | EXP'Tuple of exp list | EXP'List of exp list
                          | EXP'Vector of exp list | EXP'Seq of exp list
                          | EXP'Let of (dec * exp) | EXP'App of (exp * exp)
                          | EXP'Typed of (exp * ty) | EXP'Andalso of (exp * exp)
                          | EXP'Orelse of (exp * exp)
                          | EXP'Handle of (exp * match) | EXP'Raise of exp
                          | EXP'If of (exp * exp * exp)
                          | EXP'While of (exp * exp) | EXP'Case of (exp * match)
                          | EXP'Fn of match
                datatype sigexp = SIG'Spec of spec | SIG'Id of sigid
                                  | SIG'TypeRealisation
                                    of (sigexp * (tyvar list * tycon long * ty))
                and spec = SPEC'Val of (vid * ty) list
                           | SPEC'Type of (tyvar list * tycon) list
                           | SPEC'Eqtype of (tyvar list * tycon) list
                           | SPEC'Datatype of (tyvar list * tycon *
                                               (vid * ty option) list) list
                           | SPEC'DataReplicate of (tycon * tycon long)
                           | SPEC'Exception of (vid * ty option) list
                           | SPEC'Structure of (strid * sigexp) list
                           | SPEC'Include of sigexp | SPEC'Empty
                           | SPEC'Seq of (spec * spec)
                           | SPEC'Sharing of (spec * tycon long list)
                datatype strexp = STREXP'Struct of strdec
                                  | STREXP'Id of strid long
                                  | STREXP'TransCon of (strexp * sigexp)
                                  | STREXP'OpaqueCon of (strexp * sigexp)
                                  | STREXP'FunctorApp of (funid * strexp)
                                  | STREXP'Let of (strdec * strexp)
                and strdec = STRDEC'Dec of dec
                             | STRDEC'Structure of (strid * strexp) list
                             | STRDEC'Local of (strdec * strdec) | STRDEC'Empty
                             | STRDEC'Seq of (strdec * strdec)
                datatype sigdec = SIGDEC of (sigid * sigexp) list
                datatype funimport = FUNIMPORT'Basic of (strid * sigexp)
                                     | FUNIMPORT'Derived of spec
                datatype fundec = FUNDEC of (funid * funimport * strexp) list
                datatype topdec = TOPDEC'Exp of exp | TOPDEC'Strdec of strdec
                                  | TOPDEC'Sigdec of sigdec
                                  | TOPDEC'Fundec of fundec
                val exp'vid : (vid -> exp)
            end
signature ValueSig_Import =
            sig type vid
                type lab
                type scon
                type exname
                type value
                type record
                type closure
                type exval
                val apply'value : (value -> (value -> value))
                val value'scon : (scon -> value)
                val value'con0 : (vid -> value)
                val value'exval : (exval -> value)
                val value'closure : (closure -> value)
                val value'record : (record -> value)
                val value'vector : (value vector -> value)
                val gen'exname : (vid -> exname)
                val closure'fn : ((value -> value) -> closure)
                val record'empty : record
                val record'valrow : ((lab * value) list ->
                                     record)
                val record'values : (value list -> record)
                val look'record : (string -> (record -> (lab -> value)))
                val vid'it : vid
                val exval'con0 : (exname -> exval)
                val exval'Match : exval
                val exval'Bind : exval
                val value'true : value
                val value'false : value
                val value'list : (value list -> value)
                val exval'value : (string -> (value -> exval))
                val scon'value : (string -> (value -> scon))
                val excon0'value : (string -> (value -> exname))
                val ref_or_con0'value :
                    (string -> (value -> ({Ref : unit},{Con0 : vid}) either))
                val con'value : (string -> (value -> (vid * value option)))
                val excon'value : (string -> (value -> (exname * value option)))
                val derefAddr'value : (string -> (value -> value))
                val bool'value : (string -> (value -> bool))
                val record'value : (string -> (value -> record))
                val unit'value : (string -> (value -> unit))
                val listCons'value : (string -> (value -> (value * value) option
                                                ))
                val eq'scon : (scon -> (scon -> bool))
                val eq'exname : (exname -> (exname -> bool))
            end
signature BasisSig_Import =
            sig type 'a long
                type vid
                type tycon
                type strid
                type sigid
                type funid
                type value
                datatype idstatus = IDSTATUS'C | IDSTATUS'E
                                    | IDSTATUS'V
                type recval
                type strenv
                type tyenv
                type valenv
                type env
                val tyenv'empty : tyenv
                val valenv'empty : valenv
                val env'empty : env
                val env'strenv : (strenv -> env)
                val env'tyenv : (tyenv -> env)
                val env'valenv : (valenv -> env)
                val tyenv'plus : (tyenv -> (tyenv -> tyenv))
                val valenv'plus : (valenv -> (valenv -> valenv))
                val env'plus : (env -> (env -> env))
                val tyenv'plusList : (tyenv list -> tyenv)
                val valenv'plusList : (valenv list -> valenv)
                val env'plusList : (env list -> env)
                val strenv'single : ((strid * env) -> strenv)
                val tyenv'single : ((tycon * valenv) -> tyenv)
                val valenv'single : ((vid * (recval * idstatus))
                                     -> valenv)
                val strenv'list : ((strid * env) list -> strenv)
                val recval'value : (value -> recval)
                val recval'fn : ((valenv -> value) -> recval)
                val value'recval : (recval -> value)
                val valenv'Rec : (valenv -> valenv)
                val isUnbound'vid : (env -> (vid -> bool))
                val look'longstrid : (env -> (strid long -> env)
                                     )
                val look'longtycon :
                    (env -> (tycon long -> valenv))
                val look'longvid : (env -> (vid long -> (value * idstatus)))
                type strinter
                type tyinter
                type valinter
                type inter
                val valinter'empty : valinter
                val inter'strinter : (strinter -> inter)
                val inter'tyinter : (tyinter -> inter)
                val inter'valinter : (valinter -> inter)
                val strinter'plusList : (strinter list -> strinter)
                val tyinter'plusList : (tyinter list -> tyinter)
                val valinter'plusList : (valinter list -> valinter)
                val strinter'single : ((strid * inter) -> strinter)
                val tyinter'single : ((tycon * valinter) -> tyinter)
                val valinter'single : ((vid * idstatus) -> valinter)
                val inter'empty : inter
                val inter'plus : (inter -> (inter -> inter))
                val inter'plusList : (inter list -> inter)
                val env'restrict : (env -> (inter -> env))
                type functorclosure
                val functorclosure'fn : ((env -> env) -> functorclosure)
                val apply'functorclosure : (functorclosure -> (env -> env))
                type funenv
                type sigenv
                val funenv'plusList : (funenv list -> funenv)
                val sigenv'plusList : (sigenv list -> sigenv)
                val funenv'single : ((funid * functorclosure) -> funenv)
                val sigenv'single : ((sigid * inter) -> sigenv)
                type basis
                val basis'empty : basis
                val basis'plus : (basis -> (basis -> basis))
                val basis'funenv : (funenv -> basis)
                val basis'sigenv : (sigenv -> basis)
                val basis'env : (env -> basis)
                val look'funid : (basis -> (funid -> functorclosure))
                val look'sigid : (basis -> (sigid -> inter))
                val env'basis : (basis -> env)
            end
functor EVAL (structure Prel : PrelSig_Import
              structure Atom : AtomSig_Import
              structure Lang : LangSig_Import
              structure Value : ValueSig_Import
              structure Basis : BasisSig_Import sharing type
              Atom.lab = Lang.lab = Value.lab sharing type
              Atom.scon = Lang.scon = Value.scon sharing type
              Atom.long = Lang.long = Basis.long sharing type
              Atom.vid = Lang.vid = Value.vid = Basis.vid sharing type
              Atom.strid = Lang.strid = Basis.strid sharing type
              Atom.tycon = Lang.tycon = Basis.tycon sharing type
              Atom.funid = Lang.funid = Basis.funid sharing type
              Atom.sigid = Lang.sigid = Basis.sigid sharing type
              Value.value = Basis.value) =
        struct
        open Prel Atom Lang Value Basis
        fun error s = abort (op ^ {1 = "error: eval: ",2 = s})
        fun notYet s = abort (op ^ {1 = "notYet: eval: ",2 = s})
        fun mapLR E xs = let fun loop acc [] = rev acc
                               | loop acc (op :: {1 = x,2 = xs}) =
                               (loop (op :: {1 = E x,2 = acc})) xs
                         in (loop []) xs
                         end
        exception Pack of exval
        fun maybeGetBindableVid'longvid env (LONG (op :: {1 = _,2 = _},vid)) =
          None
          | maybeGetBindableVid'longvid env (longvid as LONG ([],vid)) =
          if (isUnbound'vid env) vid then Some vid
          else let val (_,ids) = (look'longvid env) longvid
               in case ids of
                    IDSTATUS'V => (Some vid)
                  | IDSTATUS'C => None
                  | IDSTATUS'E => None
               end
        datatype status = STATUS'V | STATUS'Ref | STATUS'C of vid
                          | STATUS'E of exname
        fun lookStatus'longvid env x =
          let val (x_value,ids) = (look'longvid env) x
          in case ids of
               IDSTATUS'V => STATUS'V
             | IDSTATUS'C => let val either = (ref_or_con0'value "patcons(C)")
                                              x_value
                             in case either of
                                  This {Ref = ()} => STATUS'Ref
                                | That {Con0 = vid} => (STATUS'C vid)
                             end
             | IDSTATUS'E => let val exname = (excon0'value "parvar(E)") x_value
                             in STATUS'E exname
                             end
          end
        fun valenv'singleValue (k,(v,ids)) =
          valenv'single (k,(recval'value v,ids))
        fun eval'vidBinding vid recval = valenv'single (vid,(recval,IDSTATUS'V))
        fun eval'patvar env recval x =
          case (maybeGetBindableVid'longvid env) x
          of
            Some vid => (Some ((eval'vidBinding vid) recval))
          | None => (case (lookStatus'longvid env) x of
                       STATUS'V => (error "patvar(V)")
                     | STATUS'Ref => (error "patvar(Ref)")
                     | STATUS'C vid =>
                         let val (vid',vOpt) = (con'value "patvar(C)")
                                               (value'recval recval)
                         in case vOpt of
                              Some _ => None
                            | None => (if (eq'vid vid) vid'
                                       then Some valenv'empty else None)
                         end
                     | STATUS'E exname =>
                         let val (exname',vOpt) = (excon'value "parvar(E)")
                                                  (value'recval recval)
                         in case vOpt of
                              Some _ => None
                            | None => (if (eq'exname exname) exname'
                                       then Some valenv'empty else None)
                         end)
        fun eval'patcons P env v x pat =
          case (lookStatus'longvid env) x of
            STATUS'V => (error "patcons(V)")
          | STATUS'Ref => let val v' = (derefAddr'value "patcons(ref)") v
                          in ((P env) v') pat
                          end
          | STATUS'C vid => let val (vid',vOpt) = (con'value "patcons(C)") v
                            in case vOpt of
                                 Some v' => (if (eq'vid vid) vid'
                                             then ((P env) v') pat else None)
                               | None => None
                            end
          | STATUS'E exname => let val (exname',vOpt) = (excon'value "parvar(E)"
                                                        ) v
                               in case vOpt of
                                    Some v' => (if (eq'exname exname) exname'
                                                then ((P env) v') pat else None)
                                  | None => None
                               end
        fun eval'patrow P env record (PATROW (lps,{wildcard = true})) =
          notYet "patrow:wildcard"
          | eval'patrow P env record (PATROW (lps,{wildcard = false})) =
          let fun loop accVe [] = Some accVe
                | loop accVe (op :: {1 = (lab,pat),2 = lps}) =
                let val v = ((look'record "patrow") record) lab
                in case ((P env) v) pat of
                     None => None
                   | Some ve => ((loop ((valenv'plus accVe) ve)) lps)
                end
          in (loop valenv'empty) lps
          end
        fun eval'pattuple P env record pats =
          let fun loop accVe n [] = Some accVe
                | loop accVe n (op :: {1 = pat,2 = pats}) =
                let val v = ((look'record "pattuple") record) (LAB'Num n)
                in case ((P env) v) pat of
                     None => None
                   | Some ve => (((loop ((valenv'plus accVe) ve))
                                  (op + {1 = n,2 = 1})) pats)
                end
          in ((loop valenv'empty) 1) pats
          end
        fun eval'patlist P env v pats =
          case (pats,(listCons'value "patlist") v)
          of
            ([],None) => (Some valenv'empty)
          | (op :: {1 = pat,2 = pats},Some (v1,v2)
            ) => (case ((P env) v1) pat of
                    None => None
                  | Some ve1 => (case (((eval'patlist P) env) v2) pats of
                                   None => None
                                 | Some ve2 => (Some ((valenv'plus ve1) ve2))))
          | _ => None
        fun inner_eval'pat_U P env v pat =
          case pat of
            PAT'Wild => (Some valenv'empty)
          | PAT'Scon scon => let val scon' = (scon'value "eval'pat") v
                             in if (eq'scon scon) scon' then Some valenv'empty
                                else None
                             end
          | PAT'Var x => (abort "inner_eval'pat_U--Var")
          | PAT'Record pr => let val record = (record'value "eval'pat") v
                             in (((eval'patrow P) env) record) pr
                             end
          | PAT'Unit => let val U as () = (unit'value "eval'pat") v
                        in Some valenv'empty
                        end
          | PAT'Tuple pats => let val record = (record'value "eval'pat") v
                              in (((eval'pattuple P) env) record) pats
                              end
          | PAT'List pats => ((((eval'patlist P) env) v) pats)
          | PAT'Cons (x,pat) => (((((eval'patcons P) env) v) x) pat)
          | PAT'Typed (pat,ty_IGNORED) => (((P env) v) pat)
          | PAT'Layered (vid,tyo_IGNORED,pat) =>
              (abort "inner_eval'pat_U--Layered")
        fun eval'pat_U P env recval pat =
          case pat of
            PAT'Var x => (((eval'patvar env) recval) x)
          | PAT'Layered (vid,tyo_IGNORED,pat) =>
              (case ((P env) recval) pat of
                 None => None
               | Some ve => (Some ((valenv'plus ((eval'vidBinding vid) recval))
                                   ve)))
          | _ => let fun P' env v pat = ((P env) (recval'value v)) pat
                 in (((inner_eval'pat_U P') env) (value'recval recval)) pat
                 end
        fun eval'mrule P E env value (MRULE (pat,exp)) =
          case ((P env) value) pat of
            None => None
          | Some ve => let val env = (env'plus env) (env'valenv ve)
                       in Some ((E env) exp)
                       end
        fun eval'match P E env value (MATCH xs) =
          let fun loop [] = None
                | loop (op :: {1 = x,2 = xs}) =
                case ((((eval'mrule P) E) env) value) x of
                  Some res => (Some res)
                | None => (loop xs)
          in loop xs
          end
        fun makeEvalClosure P E (match,env,ve) =
          closure'fn (fn vArg => let val env = (env'plus env)
                                               (env'valenv (valenv'Rec ve))
                                 in case ((((eval'match P) E) env) vArg) match
                                    of
                                      Some res => res
                                    | None => (raise Pack exval'Match)
                                 end)
        fun eval'exprow Eenv exprow =
          case exprow of
            EXPROW xs => ((mapLR (fn (lab,exp) => (lab,Eenv exp))) xs)
        fun eval'seq Eenv [] = abort "eval'seq"
          | eval'seq Eenv (op :: {1 = exp,2 = exps}) =
          let fun loop v [] = v
                | loop _ (op :: {1 = exp,2 = exps}) = (loop (Eenv exp)) exps
          in (loop (Eenv exp)) exps
          end
        fun inner_eval'exp_U P E D env exp =
          case exp of
            EXP'Scon scon => (value'scon scon)
          | EXP'Var x => let val (v,is) = (look'longvid env) x
                         in v
                         end
          | EXP'Record exprow =>
              (value'record (record'valrow ((eval'exprow (E env)) exprow)))
          | EXP'Select lab => (notYet "exp:select")
          | EXP'Unit => (value'record record'empty)
          | EXP'Tuple exps => (value'record (record'values ((mapLR (E env)) exps
                                                           )))
          | EXP'List exps => (value'list ((mapLR (E env)) exps))
          | EXP'Vector exps =>
              (value'vector (Vector.fromList ((mapLR (E env)) exps)))
          | EXP'Seq exps => ((eval'seq (E env)) exps)
          | EXP'Let (dec,exp) => let val env' = (D env) dec
                                 in (E ((env'plus env) env')) exp
                                 end
          | EXP'App (e1,e2) => ((apply'value ((E env) e1)) ((E env) e2))
          | EXP'Typed (exp,ty) => ((E env) exp)
          | EXP'Andalso (e1,e2) => (if (bool'value "andalso") ((E env) e1)
                                    then (E env) e2 else value'false)
          | EXP'Orelse (e1,e2) => (if (bool'value "orele") ((E env) e1)
                                   then value'true else (E env) e2)
          | EXP'Handle (exp,match) =>
              ((E env) exp handle Pack exval => case ((((eval'match P) E) env)
                                                      (value'exval exval)) match
                                                of
                                                  None => (raise Pack exval)
                                                | Some res => res)
          | EXP'Raise exp => let val exval = (exval'value "eval'exp")
                                             ((E env) exp)
                             in raise Pack exval
                             end
          | EXP'If (e1,e2,e3) => (if (bool'value "if") ((E env) e1)
                                  then (E env) e2 else (E env) e3)
          | EXP'While (e1,e2) => (notYet "while")
          | EXP'Case (exp,match) => (case ((((eval'match P) E) env)
                                           ((E env) exp)) match of
                                       Some res => res
                                     | None => (raise Pack exval'Match))
          | EXP'Fn match => (abort "inner_eval'exp_U--Fn")
        fun eval'exp_U P E D env exp =
          case exp of
            EXP'Fn match =>
              (recval'fn (fn ve => value'closure (((makeEvalClosure P) E)
                                                  (match,env,ve))))
          | _ => (recval'value (((((inner_eval'exp_U P) E) D) env) exp))
        fun exp'fnVid x body = EXP'Fn (MATCH [MRULE (PAT'Var (LONG ([],x)),body)
                                             ])
        fun exp'fnVids [] body = body
          | exp'fnVids (op :: {1 = x,2 = xs}) body =
          (exp'fnVid x) ((exp'fnVids xs) body)
        local val U = ref 1
        in fun genVid () = let val n = ! U
                           in (op := {1 = U,2 = op + {1 = 1,2 = n}} ;
                               VID (ID (op ^ {1 = "G#",2 = stringOfInt n})))
                           end
        end
        fun genVids n = if op < {1 = n,2 = 0} then abort "genVids:n<0"
                        else let fun loop acc n =
                                   if op = {1 = n,2 = 0} then acc
                                   else (loop (op :: {1 = genVid (),2 = acc}))
                                        (op - {1 = n,2 = 1})
                             in (loop []) n
                             end
        fun mrule'clause (pats,tyo_IGNORED,exp) = MRULE (PAT'Tuple pats,exp)
        fun exp'clauses [] = abort "exp'clauses:[]"
          | exp'clauses (clauses as op :: {1 = (pats,_,_),2 = _}) =
          let val vids = genVids (length pats)
          in (exp'fnVids vids) (EXP'Case (EXP'Tuple ((map exp'vid) vids),
                                          MATCH ((map mrule'clause) clauses)))
          end
        fun eval'valbind P E env vb =
          case vb of
            VALBIND'Rec vb => (valenv'Rec ((((eval'valbind P) E) env) vb))
          | VALBIND'Seq (pat,exp,vbo) =>
              (case ((P env) ((E env) exp)) pat of
                 None => (raise Pack exval'Bind)
               | Some ve => (case vbo of
                               None => ve
                             | Some vb => ((valenv'plus ve)
                                           ((((eval'valbind P) E) env) vb))))
        fun eval'fvalbind Eenv (FVALBIND fs) =
          valenv'Rec (valenv'plusList
                      ((map (fn (vid,clauses) => (eval'vidBinding vid)
                                                 (Eenv (exp'clauses clauses))))
                       fs))
        fun eval'exbind1 env b =
          case b of
            EXBIND1'New (vid,IGNORED_tyo) =>
              let val en = gen'exname vid
                  val v = value'exval (exval'con0 en)
              in valenv'singleValue (vid,(v,IDSTATUS'E))
              end
          | EXBIND1'Copy (vid,longvid) =>
              let val (v,ids) = (look'longvid env) longvid
              in case ids of
                   IDSTATUS'V => (error "exbind(copy):expected status E, got V")
                 | IDSTATUS'C => (error "exbind(copy):expected status E, got C")
                 | IDSTATUS'E => let val exname = (excon0'value
                                                   "eval'exbind1(E)") v
                                 in valenv'singleValue (vid,(v,IDSTATUS'E))
                                 end
              end
        fun eval'exbind env (EXBIND bs) =
          valenv'plusList ((mapLR (eval'exbind1 env)) bs)
        fun eval'typbind (TYPBIND xs) =
          tyenv'plusList
          ((map (fn (_,tycon,_) => tyenv'single (tycon,valenv'empty))) xs)
        fun eval'datbind (DATBIND xs) =
          let fun loop ve te [] = (ve,te)
                | loop ve te (op :: {1 = (IGNORED_tyvars,tycon,clauses),2 = xs})
              = let val ve' = valenv'plusList
                              ((map (fn (vid,IGNORED_tyo) =>
                                          let val v = value'con0 vid
                                          in valenv'singleValue
                                             (vid,(v,IDSTATUS'C))
                                          end)) clauses)
                    val te' = tyenv'single (tycon,ve')
                in ((loop ((valenv'plus ve) ve')) ((tyenv'plus te) te')) xs
                end
          in ((loop valenv'empty) tyenv'empty) xs
          end
        fun eval'optWithType tbo = case tbo of
                                     Some tb => (eval'typbind tb)
                                   | None => tyenv'empty
        fun eval'dec_U P E D env dec =
          case dec of
            DEC'Val vb => (env'valenv ((((eval'valbind P) E) env) vb))
          | DEC'Fval fb => (env'valenv ((eval'fvalbind (E env)) fb))
          | DEC'Type typbind => (env'tyenv (eval'typbind typbind))
          | DEC'Datatype (db,tbo) =>
              let val (ve,te) = eval'datbind db
                  val env1 = (env'plus (env'valenv ve))
                             (env'tyenv te)
                  val env2 = env'tyenv (eval'optWithType tbo)
              in (env'plus env1) env2
              end
          | DEC'DataReplicate (tycon,tyconlong) =>
              let val ve = (look'longtycon env) tyconlong
                  val env1 = env'valenv ve
                  val env2 = env'tyenv (tyenv'single (tycon,ve))
              in (env'plus env1) env2
              end
          | DEC'Abstype (db,tbo,dec) =>
              let val (ve,te_IGNORED) = eval'datbind db
                  val env1 = env'valenv ve
                  val env2 = env'tyenv (eval'optWithType tbo)
                  val env3 = (D (env'plusList [env,env1,env2])) dec
              in (env'plus env2) env3
              end
          | DEC'Exception exbind => (env'valenv ((eval'exbind env) exbind))
          | DEC'Local (dec1,dec2) => let val env1 = (D env) dec1
                                     in (D ((env'plus env) env1)) dec2
                                     end
          | DEC'Open xs => (env'plusList ((map (look'longstrid env)) xs))
          | DEC'Empty => env'empty
          | DEC'Seq (dec1,dec2) => let val env1 = (D env) dec1
                                       val env2 = (D ((env'plus env) env1)) dec2
                                   in (env'plus env1) env2
                                   end
        fun eval'patR x = (eval'pat_U eval'patR) x
        fun eval'pat env v = (eval'patR env) (recval'value v)
        fun eval'exp env exp = value'recval ((eval'expR env) exp)
        and eval'expR x = (((eval'exp_U eval'pat) eval'exp) eval'dec) x
        and eval'dec x = (((eval'dec_U eval'patR) eval'expR) eval'dec) x
        fun eval'sigexp_U SPEC SIG basis sigexp =
          case sigexp of
            SIG'Spec spec => (SPEC spec)
          | SIG'Id sigid => ((look'sigid basis) sigid)
          | SIG'TypeRealisation (sigexp,_) => (SIG sigexp)
        fun eval'spec_U SPEC SIG spec =
          case spec of
            SPEC'Val xs => (inter'valinter
                            (valinter'plusList
                             ((map (fn (vid,_) => valinter'single
                                                  (vid,IDSTATUS'V))) xs)))
          | SPEC'Type xs =>
              (inter'tyinter (tyinter'plusList
                              ((map (fn (_,tycon) => tyinter'single
                                                     (tycon,valinter'empty))) xs
                              )))
          | SPEC'Eqtype xs =>
              (inter'tyinter (tyinter'plusList
                              ((map (fn (_,tycon) => tyinter'single
                                                     (tycon,valinter'empty))) xs
                              )))
          | SPEC'Datatype xs =>
              (inter'plusList
               ((map (fn (_,tycon,constructors)
                         => let val vi = valinter'plusList
                                         ((map (fn (vid,_) => valinter'single
                                                              (vid,IDSTATUS'C)))
                                          constructors)
                            in (inter'plus (inter'valinter vi))
                               (inter'tyinter (tyinter'single (tycon,vi)))
                            end)) xs))
          | SPEC'DataReplicate x => (notYet "SPEC'DataReplicate")
          | SPEC'Exception xs =>
              (inter'valinter (valinter'plusList
                               ((map (fn (vid,_) => valinter'single
                                                    (vid,IDSTATUS'E))) xs)))
          | SPEC'Structure xs =>
              (inter'strinter (strinter'plusList
                               ((map (fn (strid,sigexp) =>
                                           strinter'single (strid,SIG sigexp)))
                                xs)))
          | SPEC'Include sigexp => (SIG sigexp)
          | SPEC'Empty => inter'empty
          | SPEC'Seq (spec1,spec2) => ((inter'plus (SPEC spec1)) (SPEC spec2))
          | SPEC'Sharing (spec,_) => (SPEC spec)
        fun eval'sigexp basis x = (((eval'sigexp_U (eval'spec basis))
                                    (eval'sigexp basis)) basis) x
        and eval'spec basis x = ((eval'spec_U (eval'spec basis))
                                 (eval'sigexp basis)) x
        fun eval'strexp_U SE SD SIG basis x =
          case x of
            STREXP'Struct strdec => ((SD basis) strdec)
          | STREXP'Id stridlong => ((look'longstrid (env'basis basis)) stridlong
                                   )
          | STREXP'TransCon (strexp,sigexp) =>
              ((env'restrict ((SE basis) strexp)) ((SIG basis) sigexp))
          | STREXP'OpaqueCon (strexp,sigexp) =>
              ((env'restrict ((SE basis) strexp)) ((SIG basis) sigexp))
          | STREXP'FunctorApp (funid,strexp) =>
              ((apply'functorclosure ((look'funid basis) funid))
               ((SE basis) strexp))
          | STREXP'Let (strdec,strexp) =>
              let val env = (SD basis) strdec
              in (SE ((basis'plus basis) (basis'env env))) strexp
              end
        fun eval'strdec_U SE SD basis x =
          case x of
            STRDEC'Dec dec => ((eval'dec (env'basis basis)) dec)
          | STRDEC'Structure xs =>
              (env'strenv (strenv'list ((map (fn (strid,strexp) =>
                                                   (strid,(SE basis) strexp)))
                                        xs)))
          | STRDEC'Local (sd1,sd2) =>
              let val basis1 = basis'env ((SD basis) sd1)
              in (SD ((basis'plus basis) basis1)) sd2
              end
          | STRDEC'Empty => env'empty
          | STRDEC'Seq (sd1,sd2) =>
              let val env1 = (SD basis) sd1
                  val env2 = (SD ((basis'plus basis) (basis'env env1))) sd2
              in (env'plus env1) env2
              end
        fun eval'strexp basis x = ((((eval'strexp_U eval'strexp) eval'strdec)
                                    eval'sigexp) basis) x
        and eval'strdec basis x = (((eval'strdec_U eval'strexp) eval'strdec)
                                   basis) x
        fun eval'sigdec basis x =
          case x of
            SIGDEC xs => (sigenv'plusList
                          ((map (fn (sigid,sigexp) =>
                                      let val inter = (eval'sigexp basis) sigexp
                                          val sigenv = sigenv'single
                                                       (sigid,inter)
                                      in sigenv
                                      end)) xs))
        fun eval'funimport basis x strexp =
          case x of
            FUNIMPORT'Basic (strid,sigexp) =>
              let val inter = (eval'sigexp basis) sigexp
              in functorclosure'fn
                 (fn env => let val env1 = env'strenv (strenv'single
                                                       (strid,(env'restrict env)
                                                              inter))
                                val basis1 = (basis'plus basis) (basis'env env1)
                            in (eval'strexp basis1) strexp
                            end)
              end
          | FUNIMPORT'Derived spec =>
              let val inter = (eval'spec basis) spec
              in functorclosure'fn
                 (fn env => let val env1 = (env'restrict env)
                                           inter
                                val basis1 = (basis'plus basis) (basis'env env1)
                            in (eval'strexp basis1) strexp
                            end)
              end
        fun eval'fundec basis x =
          case x of
            FUNDEC xs => (funenv'plusList
                          ((map (fn (funid,funimport,strexp) =>
                                      let val functorclosure =
                                              ((eval'funimport basis) funimport)
                                              strexp
                                      in funenv'single (funid,functorclosure)
                                      end)) xs))
        fun eval'topdec basis x =
          case x of
            TOPDEC'Exp exp => let val v = (eval'exp (env'basis basis)) exp
                                  val ve = (eval'vidBinding vid'it)
                                           (recval'value v)
                                  val env = env'valenv ve
                                  val basis = basis'env env
                              in basis
                              end
          | TOPDEC'Strdec x => (basis'env ((eval'strdec basis) x))
          | TOPDEC'Sigdec x => (basis'sigenv ((eval'sigdec basis) x))
          | TOPDEC'Fundec x => (basis'funenv ((eval'fundec basis) x))
        fun eval'topdecs basis [] = basis'empty
          | eval'topdecs basis (op :: {1 = x,2 = xs}) =
          let val basis1 = (eval'topdec basis) x
              val basis2 = (eval'topdecs ((basis'plus basis) basis1)) xs
          in (basis'plus basis1) basis2
          end
        fun eval'program basis xs = This ((eval'topdecs basis) xs)
                                    handle Pack exval => That exval
        end : EvalSig
;

