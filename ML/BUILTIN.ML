(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  10.    export
  20.    imports
  97.    functor
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature BuiltinSig =
sig
    type basis
    val basis'initial : basis
end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ~/project/prelude/PREL.ML *)
signature PrelSig_Import =
sig
    val abort           : string -> 'a
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AtomSig ATOM.ML *)
signature AtomSig_Import =
sig
    datatype id = ID of string
    datatype vid = VID of id
    datatype tycon = TYCON of id
    datatype strid = STRID of id
end;
(*IMPORTDONE:*)

(*IMPORTSIG: ValueSig VALUE.ML *)
signature ValueSig_Import =
sig
    type vid
    type value
    datatype effects = EFF'Pure | EFF'Impure | EFF'SideEffects | EFF'VarAlloc
    type builtin
    val builtin'fn      : string * effects * int * (value -> value) -> builtin
    val value'builtin   : builtin -> value
    type con

    val makeCons        : (vid * int) list -> con list
    val vid'con         : con -> vid
    datatype instream_descriptor = ISD'stdIn | ISD'File of string
    datatype outstream_descriptor = OSD'stdOut | OSD'stdErr | OSD'File of string
    val value'instream  : instream_descriptor * TextIO.instream -> value
    val instream'value  : string -> value -> TextIO.instream
    val value'outstream : outstream_descriptor * TextIO.outstream -> value
    val outstream'value : string -> value -> TextIO.outstream
    val value'assignOp  : value
    val value'ref       : value
    val value'con0      : con -> value
    val value'vector    : value vector -> value
    val value'array     : value array -> value
    val value'unit      : value
    val value'char      : char -> value
    val value'int       : int -> value
    val value'word      : word -> value
    val value'string    : string -> value
    val value'bool      : bool -> value
    val value'pair      : value * value -> value
    val value'trip      : value * value * value -> value
    val value'list      : value list -> value
    val char'value      : string -> value -> char
    val int'value       : string -> value -> int
    val word'value      : string -> value -> word
    val string'value    : string -> value -> string
    val bool'value      : string -> value -> bool
    val pair'value      : string -> value -> value * value
    val trip'value      : string -> value -> value * value * value
    val list'value      : string -> value -> value list
    val vector'value    : string -> value -> value vector
    val array'value     : string -> value -> value array
    val lessNumTxt      : value * value -> bool
    val eq'value        : value -> value -> bool
    val decompile       : value -> unit


end;
(*IMPORTDONE:*)

(*IMPORTSIG: BasisSig BASIS.ML *)
signature BasisSig_Import =
sig
    type vid
    type tycon
    type strid
    type value
    datatype idstatus = IDSTATUS'C | IDSTATUS'E | IDSTATUS'V
    type strenv
    type tyenv
    type valenv
    type env

    val env'strenv      : strenv -> env
    val env'tyenv       : tyenv -> env
    val env'valenv      : valenv -> env
    val env'plus        : env -> env -> env
    val valenv'plusList : valenv list -> valenv
    val env'plusList    : env list -> env
    val strenv'single   : strid * env -> strenv
    val tyenv'single    : tycon * valenv -> tyenv
    val valenv'single   : vid * (value * idstatus) -> valenv
    type basis
    val basis'env       : env -> basis
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor BUILTIN(val prefixNML : string
                structure Prel : PrelSig_Import
                structure Atom : AtomSig_Import
                structure Value : ValueSig_Import
                structure Basis : BasisSig_Import
                sharing type Atom.vid = Value.vid = Basis.vid
                sharing type Atom.strid = Basis.strid
                sharing type Atom.tycon = Basis.tycon
                sharing type Value.value = Basis.value
                                  ) : BuiltinSig =
struct

  open Prel Atom Value Basis

  fun notYet s = abort ("notYet: BUILTIN: "^s)

  datatype 'a conv = CONV of {arity: int, from: string -> 'a -> value, back: string -> value -> 'a}

  fun mkConvN (arity,from,back) = CONV{arity=arity,from=fn _ =>from,back=back}
  fun mkConv (from,back)        = CONV{arity=1,from=fn _ =>from,back=back}
  fun mkConvD (fromD,back)      = CONV{arity=1,from=fromD,back=back}

  fun arity (CONV{arity,from,back}) = arity
  fun fromD (CONV{arity,from,back}) = from
  fun back (CONV{arity,back,from}) = back

  fun from conv = fromD conv "<no-description>"

  fun identity x = x

  val V = mkConv (identity, fn _ => identity)
  val C = mkConv (value'char,char'value)
  val S = mkConv (value'string,string'value)
  val B = mkConv (value'bool,bool'value)
  val I = mkConv (value'int,int'value)
  val W = mkConv (value'word,word'value)

  val UNIT = mkConv (fn _ => value'unit, fn _ => fn _ => ())

  fun OUTSTREAM_desc desc = mkConv (fn v => value'outstream (desc,v),outstream'value)
  fun INSTREAM_desc desc = mkConv (fn v => value'instream (desc,v),instream'value)

  val OUTSTREAM = mkConv (fn v => abort "OUTSTREAM",outstream'value)
  (*val INSTREAM = mkConv (fn v => abort "INSTREAM",instream'value)*)

  val INSTREAM = mkConvD (fn s => fn v => value'instream (ISD'File s,v),instream'value)


  fun PAIR(c1,c2) =
      mkConvN (2,
               fn (x1,x2) => value'pair (from c1 x1, from c2 x2),
               fn who => fn v =>
                            let val who1 = who^"[arg1]"
                                val who2 = who^"[arg2]"
                            in
                                let val (v1,v2) = pair'value who v
                                in (back c1 who1 v1,
                                    back c2 who2 v2)
                                end
                            end)

  fun TRIP(c1,c2,c3) =
      mkConvN (3,
               fn (x1,x2,x3) => value'trip (from c1 x1, from c2 x2, from c3 x3),
               fn who => fn v =>
                            let val who1 = who^"[arg1]"
                                val who2 = who^"[arg2]"
                                val who3 = who^"[arg3]"
                            in
                                let val (v1,v2,v3) = trip'value who v
                                in (back c1 who1 v1,
                                    back c2 who2 v2,
                                    back c3 who3 v3)
                                end
                            end)

  fun LIST(c) =
      mkConv (fn xs => value'list (map (from c) xs),
              fn who => fn v => map (back c who) (list'value who v))

  val VECTOR_V = mkConv (value'vector,vector'value)
  val ARRAY_V = mkConv (value'array,array'value)

  fun Basis xs =
      basis'env (env'plusList (map (fn x => x "") xs))

  fun SpecialRefConstr _ =
      env'valenv (valenv'single (VID (ID "ref"), (value'ref, IDSTATUS'C)))

  fun Constr (con) _ =
      valenv'single (vid'con con , (value'con0 con, IDSTATUS'C))

  fun Datatype (name,xs) _ =
      let val cons = makeCons (map (fn (x,arity) => (VID (ID x), arity)) xs)
          val ve = valenv'plusList (map (fn c => Constr c "") cons)
      in env'plus (env'tyenv (tyenv'single (TYCON (ID name), ve))) (env'valenv ve)
      end

  fun Types names _ = env'plusList (map (fn name => Datatype (name,[]) "") names)

  fun Value (name,conv,x) _ =
      env'valenv (valenv'single (VID (ID name), (from conv x, IDSTATUS'V)))

  fun Function'effects (name,arg,return,f) effects path =
      let val value = value'builtin (builtin'fn (path^name, effects, arity arg, fn x => from return (f (back arg name x))))
      in env'valenv (valenv'single (VID (ID name), (value,IDSTATUS'V)))
      end

  fun FunctionP q = Function'effects q EFF'Pure
  (*fun FunctionI q = Function'effects q EFF'Impure*)
  fun FunctionE q = Function'effects q EFF'SideEffects
  fun FunctionV q = Function'effects q EFF'VarAlloc

  fun Function'effects_D (name,arg,return,f) effects path =
      let val value = value'builtin (builtin'fn (path^name, effects, arity arg, fn x => let val s = back arg name x
                                                                                        in fromD return s (f s)
                                                                                        end))
      in env'valenv (valenv'single (VID (ID name), (value,IDSTATUS'V)))
      end


  fun Struct (name,xs) path =
      env'strenv (strenv'single (STRID (ID name), env'plusList (map (fn x => x (path^name^".")) xs)))

  fun flip (x,y) = (y,x)

  val basis'initial =
      Basis [
             SpecialRefConstr,

             Datatype ("bool",[("true",0),("false",0)]),
             Datatype ("list",[("::",2),("nil",0)]),

             Types ["unit", "int", "char", "string"],

             Value  (":=", V, value'assignOp),

             FunctionP ("~"     , I             , I     , op ~       ),
             FunctionP ("-"     , PAIR(I,I)     , I     , op -       ),
             FunctionP ("+"     , PAIR(I,I)     , I     , op +       ),
             FunctionP ("*"     , PAIR(I,I)     , I     , op *       ),
             FunctionP ("div"   , PAIR(I,I)     , I     , op div     ),
             FunctionP ("mod"   , PAIR(I,I)     , I     , op mod     ),
             FunctionP ("<"     , PAIR(V,V)     , B     , Value.lessNumTxt),
             FunctionP ("<="    , PAIR(V,V)     , B     , not o Value.lessNumTxt o flip),
             FunctionP (">"     , PAIR(V,V)     , B     , Value.lessNumTxt o flip),
             FunctionP (">="    , PAIR(V,V)     , B     , not o Value.lessNumTxt),
             FunctionP ("^"     , PAIR(S,S)     , S     , op ^       ),
             FunctionP ("size"  , S             , I     , size       ),
             FunctionP ("chr"   , I             , C     , chr        ),
             FunctionP ("ord"   , C             , I     , ord        ),
             FunctionV ("implode", LIST(C)      , S     , implode    ),
             FunctionV ("explode", S            , LIST(C),explode    ),
             FunctionP ("="     , PAIR(V,V)     , B     , fn (v1,v2) => eq'value v1 v2),
             FunctionE ("print" , S             , UNIT  , fn s => print ((*prefixNML ^*) s)),
             FunctionE ("decompile" , V         , UNIT  , Value.decompile),
             (*FunctionE ("exportC" , V         , UNIT  , Value.exportC),
             FunctionE ("exportC'"  , PAIR(S,V) , UNIT  , Value.exportC'),*)

             Struct("Vector",
                    [Types ["vector"],
                     FunctionP ("sub"   , PAIR(VECTOR_V,I), V   , Vector.sub),
                     FunctionV ("fromList",LIST(V),VECTOR_V     , Vector.fromList)
                     ]),

             Struct("Array",
                    [Types ["array"],
                     FunctionV ("array"  , PAIR(I,V)         , ARRAY_V   , Array.array),
                     FunctionP ("sub"    , PAIR(ARRAY_V,I)   , V         , Array.sub),
                     FunctionE ("update" , TRIP(ARRAY_V,I,V) , UNIT      , Array.update),
                     FunctionP ("length" , ARRAY_V           , I         , Array.length)
                     ]),

             Struct("Word",
                    [Types ["word"],
                     FunctionP ("+"     , PAIR(W,W)    , W     , Word.+        ),
                     FunctionP ("-"     , PAIR(W,W)    , W     , Word.-        ),
                     FunctionP ("mod"   , PAIR(W,W)    , W     , Word.mod      ),
                     FunctionP (">>"    , PAIR(W,W)    , W     , Word.>>       ),
                     FunctionP ("<<"    , PAIR(W,W)    , W     , Word.<<       ),
                     FunctionP ("orb"   , PAIR(W,W)    , W     , Word.orb      ),
                     FunctionP ("andb"  , PAIR(W,W)    , W     , Word.andb     ),
                     FunctionP ("notb"  , W            , W     , Word.notb     ),
                     FunctionP ("toInt" , W            , I     , Word.toInt    ),
                     FunctionP ("fromInt", I           , W     , Word.fromInt  ),
                     FunctionV ("toString",W           , S     , Word.toString )
                     ]),

             Struct("TextIO",
                    [Types ["instream", "outstream"],
                     Value ("stdOut", OUTSTREAM_desc Value.OSD'stdOut, TextIO.stdOut),
                     FunctionE ("output", PAIR(OUTSTREAM,S), UNIT, TextIO.output),
                     FunctionE ("flushOut", OUTSTREAM, UNIT, TextIO.flushOut),
                     FunctionE ("closeOut", OUTSTREAM, UNIT, TextIO.closeOut),
                     Function'effects_D ("openIn", S, INSTREAM, TextIO.openIn) EFF'VarAlloc,
                     FunctionV ("openOut", S, OUTSTREAM, TextIO.openOut),
                     FunctionE ("closeIn", INSTREAM, UNIT, TextIO.closeIn),
                     FunctionE ("inputN", PAIR(INSTREAM,I), S, TextIO.inputN)
                     ]),

             Struct("String",
                    [
                     FunctionP ("sub", PAIR(S,I), C, String.sub)
                     ]),

             Struct("Char",
                    [
                     FunctionP ("<"     , PAIR(C,C)     , B     , Char.<       ),
                     FunctionP ("<="    , PAIR(C,C)     , B     , Char.<=      ),
                     FunctionP (">"     , PAIR(C,C)     , B     , Char.>       ),
                     FunctionP (">="    , PAIR(C,C)     , B     , Char.>=      ),
                     FunctionP ("toString", C       , S     , Char.toString    )
                     ])
             ]

end
