

signature BuiltinSig = sig type basis
                           val basis'initial : basis
                       end
signature PrelSig_Import = sig
                           end
signature AtomSig_Import = sig datatype id = ID of string
                               datatype vid = VID of id
                               datatype tycon = TYCON of id
                               datatype strid = STRID of id
                           end
signature ValueSig_Import =
            sig type vid
                type value
                val value'assignOp : value
                val value'basval : ((string * (value -> value))
                                    -> value)
                val value'ref : value
                val value'con0 : (vid -> value)
                val value'vector : (value vector -> value)
                val value'outstream : (TextIO.outstream -> value
                                      )
                val value'instream : (TextIO.instream -> value)
                val value'unit : value
                val value'char : (char -> value)
                val value'int : (int -> value)
                val value'word : (word -> value)
                val value'string : (string -> value)
                val value'bool : (bool -> value)
                val value'pair : ((value * value) -> value)
                val value'list : (value list -> value)
                val char'value : (string -> (value -> char))
                val int'value : (string -> (value -> int))
                val word'value : (string -> (value -> word))
                val string'value : (string -> (value -> string))
                val bool'value : (string -> (value -> bool))
                val pair'value : (string -> (value -> (value * value)))
                val list'value : (string -> (value -> value list))
                val vector'value : (string -> (value -> value vector))
                val outstream'value : (string -> (value -> TextIO.outstream))
                val instream'value : (string -> (value -> TextIO.instream))
                val eq'value : (value -> (value -> bool))
            end
signature BasisSig_Import = sig type vid
                                type tycon
                                type strid
                                type value
                                datatype idstatus = IDSTATUS'C | IDSTATUS'E
                                                    | IDSTATUS'V
                                type recval
                                type strenv
                                type tyenv
                                type valenv
                                type env
                                val env'strenv : (strenv -> env)
                                val env'tyenv : (tyenv -> env)
                                val env'valenv : (valenv -> env)
                                val env'plus : (env -> (env -> env))
                                val valenv'plusList : (valenv list -> valenv)
                                val env'plusList : (env list -> env)
                                val strenv'single : ((strid * env) -> strenv)
                                val tyenv'single : ((tycon * valenv) -> tyenv)
                                val valenv'single : ((vid * (recval * idstatus))
                                                     -> valenv)
                                val recval'value : (value -> recval)
                                type basis
                                val basis'env : (env -> basis)
                            end
functor BUILTIN (val prefixNML : string
                 structure Prel : PrelSig_Import
                 structure Atom : AtomSig_Import
                 structure Value : ValueSig_Import
                 structure Basis : BasisSig_Import sharing type
                 Atom.vid = Value.vid = Basis.vid sharing type
                 Atom.strid = Basis.strid sharing type Atom.tycon = Basis.tycon
                 sharing type Value.value = Basis.value) =
        struct
        open Prel Atom Value Basis
        fun valenv'singleValue (k,(v,ids)) =
          valenv'single (k,(recval'value v,ids))
        datatype 'a conv = CONV of {from : ('a -> value),
                                    back : (string -> (value -> 'a))}
        fun mkConv (from,back) = CONV {from = from,back = back}
        fun from (CONV {from = from ,...}) = from
        fun back (CONV {back = back ,...}) = back
        fun from (CONV {from = from,back = back}) = from
        fun back (CONV {from = from,back = back}) = back
        fun identity x = x
        val V = mkConv (identity,fn _ => identity)
        val C = mkConv (value'char,char'value)
        val S = mkConv (value'string,string'value)
        val B = mkConv (value'bool,bool'value)
        val I = mkConv (value'int,int'value)
        val W = mkConv (value'word,word'value)
        val UNIT = mkConv (fn _ => value'unit,fn _ => fn _ => ())
        val OUTSTREAM = mkConv (value'outstream,outstream'value)
        val INSTREAM = mkConv (value'instream,instream'value)
        fun PAIR (c1,c2) =
          mkConv (fn (x1,x2) => value'pair ((from c1) x1,(from c2) x2),
                  fn who => fn v => let val who1 = op ^ {1 = who,2 = "[arg1]"}
                                        val who2 = op ^ {1 = who,2 = "[arg2]"}
                                    in let val (v1,v2) = (pair'value who) v
                                       in (((back c1) who1) v1,
                                           ((back c2) who2) v2)
                                       end
                                    end)
        fun LIST c = mkConv (fn xs => value'list ((map (from c)) xs),
                             fn who => fn v => (map ((back c) who))
                                               ((list'value who) v))
        fun VECTOR c = mkConv (fn xs => value'vector ((Vector.map (from c)) xs),
                               fn who => fn v => (Vector.map ((back c) who))
                                                 ((vector'value who) v))
        fun Basis xs = basis'env (env'plusList xs)
        val SpecialRefConstr =
            env'valenv (valenv'singleValue (VID (ID "ref"),(value'ref,IDSTATUS'C
                                                           )))
        fun Constr name = let val vid = VID (ID name)
                          in valenv'singleValue (vid,(value'con0 vid,IDSTATUS'C)
                                                )
                          end
        fun Datatype (name,xs) =
          let val ve = valenv'plusList ((map Constr) xs)
          in (env'plus (env'tyenv (tyenv'single (TYCON (ID name),ve))))
             (env'valenv ve)
          end
        fun Types names = env'plusList ((map (fn name => Datatype (name,[])))
                                        names)
        fun Value (name,conv,x) =
          env'valenv (valenv'singleValue
                      (VID (ID name),((from conv) x,IDSTATUS'V)))
        fun Function (name,arg,return,f) =
          let val value = value'basval (name,fn x => (from return)
                                                     (f (((back arg) name) x)))
          in env'valenv (valenv'singleValue (VID (ID name),(value,IDSTATUS'V)))
          end
        fun Struct (name,xs) =
          env'strenv (strenv'single (STRID (ID name),env'plusList xs))
        val basis'initial =
            Basis [SpecialRefConstr,
                   Datatype ("bool",["true","false"]),
                   Datatype ("list",["::","nil"]),
                   Types ["unit","int","char","string"],
                   Value (":=",V,value'assignOp),
                   Function ("~",I,I,~),
                   Function ("-",PAIR (I,I),I,op -),
                   Function ("+",PAIR (I,I),I,op +),
                   Function ("*",PAIR (I,I),I,op * ),
                   Function ("div",PAIR (I,I),I,op div),
                   Function ("mod",PAIR (I,I),I,op mod),
                   Function ("<",PAIR (I,I),B,op <),
                   Function ("<=",PAIR (I,I),B,op <=),
                   Function (">",PAIR (I,I),B,op >),
                   Function (">=",PAIR (I,I),B,op >=),
                   Function ("^",PAIR (S,S),S,op ^),
                   Function ("size",S,I,size),
                   Function ("chr",I,C,chr),
                   Function ("ord",C,I,ord),
                   Function ("implode",LIST C,S,implode),
                   Function ("explode",S,LIST C,explode),
                   Function ("=",PAIR (V,V),B,
                             fn (v1,v2) => (eq'value v1) v2),
                   Function ("print",S,UNIT,fn s => print s),
                   Struct ("Vector",[Types ["vector"],
                                     Function ("sub",PAIR (VECTOR V,I),V,
                                               Vector.sub)]),
                   Struct ("Word",[Types ["word"],Function ("+",PAIR (W,W),W,
                                                            Word.+),
                                   Function ("-",PAIR (W,W),W,Word.-),
                                   Function ("mod",PAIR (W,W),W,Word.mod),
                                   Function (">>",PAIR (W,W),W,Word.>>),
                                   Function ("<<",PAIR (W,W),W,Word.<<),
                                   Function ("orb",PAIR (W,W),W,Word.orb),
                                   Function ("andb",PAIR (W,W),W,Word.andb),
                                   Function ("notb",W,W,Word.notb),
                                   Function ("toInt",W,I,Word.toInt),
                                   Function ("fromInt",I,W,Word.fromInt)]),
                   Struct ("TextIO",[Types ["instream","outstream"],
                                     Value ("stdOut",OUTSTREAM,TextIO.stdOut),
                                     Function ("output",PAIR (OUTSTREAM,S),UNIT,
                                               TextIO.output),
                                     Function ("openIn",S,INSTREAM,TextIO.openIn
                                              ),Function ("closeIn",INSTREAM,
                                                          UNIT,TextIO.closeIn),
                                     Function ("inputN",PAIR (INSTREAM,I),S,
                                               TextIO.inputN)]),
                   Struct ("String",[Function ("sub",PAIR (S,I),C,String.sub)]),
                   Struct ("Char",[Function ("<",PAIR (C,C),B,Char.<),
                                   Function ("<=",PAIR (C,C),B,Char.<=),
                                   Function (">",PAIR (C,C),B,Char.>),
                                   Function (">=",PAIR (C,C),B,Char.>=)])]
        end : BuiltinSig
;

