

signature ValueSig = sig type vid
                         type lab
                         type scon
                         type exname
                         type value
                         type record
                         type closure
                         type exval
                         val apply'value : (value -> (value -> value))
                         val value'assignOp : value
                         val value'scon : (scon -> value)
                         val value'basval : ((string * (value -> value)) ->
                                             value)
                         val value'ref : value
                         val value'con0 : (vid -> value)
                         val value'con1 : ((vid * value) -> value)
                         val value'exval : (exval -> value)
                         val value'closure : (closure -> value)
                         val value'record : (record -> value)
                         val value'addr : (value ref -> value)
                         val value'vector : (value vector -> value)
                         val value'outstream : (TextIO.outstream -> value)
                         val value'instream : (TextIO.instream -> value)
                         val gen'exname : (vid -> exname)
                         val closure'fn : ((value -> value) -> closure)
                         val record'empty : record
                         val record'valrow : ((lab * value) list -> record)
                         val record'values : (value list -> record)
                         val size'record : (record -> int)
                         val look'record : (string -> (record -> (lab -> value))
                                           )
                         val isEmpty'record : (record -> bool)
                         val vid'it : vid
                         val vid'nil : vid
                         val vid'cons : vid
                         val exval'con0 : (exname -> exval)
                         val exval'Match : exval
                         val exval'Bind : exval
                         val value'unit : value
                         val value'char : (char -> value)
                         val value'int : (int -> value)
                         val value'word : (word -> value)
                         val value'string : (string -> value)
                         val value'true : value
                         val value'false : value
                         val value'bool : (bool -> value)
                         val value'pair : ((value * value) -> value)
                         val value'list : (value list -> value)
                         val exval'value : (string -> (value -> exval))
                         val scon'value : (string -> (value -> scon))
                         val ctag'value : (string -> (value -> vid))
                         val xtag'value : (string -> (value -> exname))
                         val cxbody'value : (string -> (value -> value))
                         val con0'value : (string -> (value -> vid))
                         val excon0'value : (string -> (value -> exname))
                         val ref_or_con0'value :
                             (string -> (value -> ({Ref : unit},{Con0 : vid})
                                                  either))
                         val con'value :
                             (string -> (value -> (vid * value option)))
                         val excon'value :
                             (string -> (value -> (exname * value option)))
                         val derefAddr'value : (string -> (value -> value))
                         val char'value : (string -> (value -> char))
                         val int'value : (string -> (value -> int))
                         val word'value : (string -> (value -> word))
                         val string'value : (string -> (value -> string))
                         val bool'value : (string -> (value -> bool))
                         val record'value : (string -> (value -> record))
                         val unit'value : (string -> (value -> unit))
                         val pair'value : (string -> (value -> (value * value)))
                         val listCons'value :
                             (string -> (value -> (value * value) option))
                         val list'value : (string -> (value -> value list))
                         val vector'value : (string -> (value -> value vector))
                         val outstream'value :
                             (string -> (value -> TextIO.outstream))
                         val instream'value :
                             (string -> (value -> TextIO.instream))
                         val eq'scon : (scon -> (scon -> bool))
                         val eq'exname : (exname -> (exname -> bool))
                         val eq'value : (value -> (value -> bool))
                         type layout
                         val lay'value : (value -> layout)
                     end
signature PrelSig_Import = sig val abort : (string -> 'a)
                               val sofChar : (char -> string)
                               val sofStringLit : (string -> string)
                           end
signature SortSig_Import = sig val sort : ((('a * 'a) -> bool) ->
                                           ('a list -> 'a list))
                           end
signature AssocSig_Import = sig type ('a,'b) assoc
                                val assoc'empty : ('a,'b) assoc
                                val assoc'list : (('a * 'b) list ->
                                                  ('a,'b) assoc)
                                val list'assoc : (('a,'b) assoc ->
                                                  ('a * 'b) list)
                                val look'assoc : (string -> ((''a -> string) ->
                                                             ((''a,'b) assoc ->
                                                              (''a -> 'b))))
                            end
signature MisclaySig_Import = sig type layout
                                  val Word : (string -> layout)
                                  val Left : (string -> layout)
                                  val Right : (string -> layout)
                                  val op // : ((layout * layout) -> layout)
                                  val op @@ : ((layout * layout) -> layout)
                                  val op ++ : ((layout * layout) -> layout)
                                  val op `` : ((layout * layout) -> layout)
                                  val op //`` : ((layout * layout) -> layout)
                                  val layWord : (word -> layout)
                                  val layInt : (int -> layout)
                                  val layBracket : (layout -> layout)
                                  val layBrace : (layout -> layout)
                                  val laySqBracket : (layout -> layout)
                                  val layCommaSep : (layout list -> layout)
                              end
signature AtomSig_Import = sig datatype id = ID of string
                               datatype vid = VID of id
                               datatype lab = LAB'Id of id | LAB'Num of int
                               datatype scon = SCON'Num of int
                                               | SCON'Word of word
                                               | SCON'Real of real
                                               | SCON'Char of char
                                               | SCON'String of string
                               val sof'lab : (lab -> string)
                               val eq'vid : (vid -> (vid -> bool))
                               val eq'lab : (lab -> (lab -> bool))
                           end
functor
VALUE (structure Prel : PrelSig_Import
       structure Sort : SortSig_Import
       structure Assoc : AssocSig_Import
       structure Misclay : MisclaySig_Import
       structure Atom : AtomSig_Import) =
struct
open Prel Sort Assoc Misclay Atom
fun error s = abort (op ^ {1 = "error: eval: ",2 = s})
datatype exname = EXNAME of {base : vid,unique : int}
datatype value = VALUE'ColonEq | VALUE'Special of scon
                 | VALUE'Basval of (string * (value -> value))
                 | VALUE'Ref | VALUE'Con0 of vid
                 | VALUE'Con1 of (vid * value) | VALUE'Exval of exval
                 | VALUE'Record of record | VALUE'Addr of value ref
                 | VALUE'Closure of closure
                 | VALUE'Vector of value vector
                 | VALUE'Outstream of TextIO.outstream
                 | VALUE'Instream of TextIO.instream
and record = RECORD of (lab,value) assoc
and exval = EXVAL'Con0 of exname | EXVAL'Con1 of (exname * value)
and closure = CLOSURE of (value -> value)
val value'assignOp = VALUE'ColonEq
val value'scon = VALUE'Special
val value'basval = VALUE'Basval
val value'ref = VALUE'Ref
val value'con0 = VALUE'Con0
val value'con1 = VALUE'Con1
val value'exval = VALUE'Exval
val value'record = VALUE'Record
val value'addr = VALUE'Addr
val value'closure = VALUE'Closure
val value'vector = VALUE'Vector
val value'outstream = VALUE'Outstream
val value'instream = VALUE'Instream
val closure'fn = CLOSURE
local val U = ref 1
in fun gen'exname base = let val n = ! U
                         in (op := {1 = U,2 = op + {1 = 1,2 = n}} ;
                             EXNAME {base = base,unique = n})
                         end
end
val record'empty = RECORD assoc'empty
fun record'valrow xs = RECORD (assoc'list xs)
fun record'values vs =
  record'valrow let fun loop acc n [] = acc
                      | loop acc n (op :: {1 = v,2 = vs}) =
                      ((loop (op :: {1 = (LAB'Num n,v),2 = acc}))
                       (op + {1 = n,2 = 1})) vs
                in ((loop []) 1) vs
                end
fun size'record (RECORD a) = length (list'assoc a)
fun look'record who (RECORD a) =
  ((look'assoc (op ^ {1 = who,2 = ":look'record"})) sof'lab) a
fun isEmpty'record (RECORD assoc) = case list'assoc assoc of
                                      [] => true
                                    | _ => false
val vid'it = VID (ID "it")
val vid'nil = VID (ID "nil")
val vid'cons = VID (ID "::")
val exval'con0 = EXVAL'Con0
val exval'Match = EXVAL'Con0 (gen'exname (VID (ID "Match")))
val exval'Bind = EXVAL'Con0 (gen'exname (VID (ID "Bind")))
val value'unit = VALUE'Record record'empty
fun value'char x = VALUE'Special (SCON'Char x)
fun value'int x = VALUE'Special (SCON'Num x)
fun value'word x = VALUE'Special (SCON'Word x)
fun value'string x = VALUE'Special (SCON'String x)
val value'true = VALUE'Con0 (VID (ID "true"))
val value'false = VALUE'Con0 (VID (ID "false"))
fun value'bool true = value'true
  | value'bool false = value'false
fun value'pair (v1,v2) = VALUE'Record (record'values [v1,v2])
fun value'list vs =
  case vs of
    [] => (VALUE'Con0 vid'nil)
  | op :: {1 = v,2 = vs} =>
      (VALUE'Con1 (vid'cons,value'pair (v,value'list vs)))
fun exval'value who v = case v of
                          VALUE'Exval exval => exval
                        | _ => (error (op ^ {1 = who,2 = ":exval'value"}
                                      ))
fun scon'value who v = case v of
                         VALUE'Special scon => scon
                       | _ => (error (op ^ {1 = who,2 = ":scon'value"}))
fun ctag'value who v = case v of
                         VALUE'Con0 vid => vid
                       | VALUE'Con1 (vid,_) => vid
                       | _ => (error (op ^ {1 = who,2 = ":ctag'value"}))
fun xtag'value who v = case v of
                         VALUE'Exval (EXVAL'Con0 exname) => exname
                       | VALUE'Exval (EXVAL'Con1 (exname,_)) => exname
                       | _ => (error (op ^ {1 = who,2 = ":xtag'value"}))
fun cxbody'value who v =
  case v of
    VALUE'Con1 (v_,v) => v
  | VALUE'Exval (EXVAL'Con1 (_,v)) => v
  | _ => (error (op ^ {1 = who,2 = ":cxbody'value"}))
fun con0'value who v = case v of
                         VALUE'Con0 x => x
                       | _ => (error (op ^ {1 = who,2 = ":con0'value"}))
fun excon0'value who v =
  case v of
    VALUE'Exval (EXVAL'Con0 exname) => exname
  | _ => (error (op ^ {1 = who,2 = ":excon0'value"}))
fun ref_or_con0'value who v =
  case v of
    VALUE'Ref => (This {Ref = ()})
  | VALUE'Con0 x => (That {Con0 = x})
  | _ => (error (op ^ {1 = who,2 = ":ref_or_con0'value"}))
fun con'value who v = case v of
                        VALUE'Con0 vid => (vid,None)
                      | VALUE'Con1 (vid,v) => (vid,Some v)
                      | _ => (error (op ^ {1 = who,2 = ":con'value"}))
fun excon'value who v = case v of
                          VALUE'Exval (EXVAL'Con0 exname) =>
                            (exname,None)
                        | VALUE'Exval (EXVAL'Con1 (exname,v)) =>
                            (exname,Some v)
                        | _ => (error (op ^ {1 = who,2 = ":excon'value"}
                                      ))
fun derefAddr'value who v =
  case v of
    VALUE'Addr (ref v) => v
  | _ => (error (op ^ {1 = who,2 = ":derefAddr'value"}))
fun char'value who v = case v of
                         VALUE'Special (SCON'Char x) => x
                       | _ => (error (op ^ {1 = who,2 = ":char'value"}))
fun int'value who v = case v of
                        VALUE'Special (SCON'Num x) => x
                      | _ => (error (op ^ {1 = who,2 = ":int'value"}))
fun word'value who v = case v of
                         VALUE'Special (SCON'Word x) => x
                       | _ => (error (op ^ {1 = who,2 = ":word'value"}))
fun string'value who v =
  case v of
    VALUE'Special (SCON'String x) => x
  | _ => (error (op ^ {1 = who,2 = ":string'value"}))
fun bool'value who v = case v of
                         VALUE'Con0 (VID (ID "true")) => true
                       | VALUE'Con0 (VID (ID "false")) => false
                       | _ => (error (op ^ {1 = who,2 = ":bool'value"}))
fun record'value who v =
  case v of
    VALUE'Record record => record
  | _ => (error (op ^ {1 = who,2 = ":record'value"}))
fun unit'value who v =
  let val who = op ^ {1 = who,2 = ":unit'value"}
      val record = (record'value who) v
  in if isEmpty'record record then ()
     else error (op ^ {1 = who,2 = ":non-empty-record"})
  end
fun pair'value who v = let val who = op ^ {1 = who,2 = ":pair'value"}
                           val record = (record'value who) v
                       in if op <> {1 = size'record record,2 = 2}
                          then error (op ^ {1 = who,2 = "[size<>2]"})
                          else let val v1 = ((look'record who) record)
                                            (LAB'Num 1)
                                   val v2 = ((look'record who) record)
                                            (LAB'Num 2)
                               in (v1,v2)
                               end
                       end
fun isList'value v = case v of
                       VALUE'Con0 vid => ((eq'vid vid) vid'nil)
                     | VALUE'Con1 (vid,_) => ((eq'vid vid) vid'cons)
                     | _ => false
fun listCons'value who v =
  let val (vid',vOpt) = (con'value (op ^ {1 = who,2 = ": expected con"})
                        ) v
  in if (eq'vid vid') vid'nil
     then case vOpt of
            None => None
          | Some _ => (error (op ^ {1 = who,2 = "nil: unexpected value"}
                             ))
     else if (eq'vid vid') vid'cons
          then case vOpt of
                 None => (error (op ^ {1 = who,2 = "cons: expected value"}))
               | Some v => (Some ((pair'value (op ^ {1 = who,2 = ":cons"})) v))
          else error (op ^ {1 = who,2 = ": expected nil/cons"})
  end
fun list'value who v = case (listCons'value who) v of
                         None => []
                       | Some (v1,v2) => (op :: {1 = v1,2 = (list'value who) v2}
                                         )
fun vector'value who v = case v of
                           VALUE'Vector x => x
                         | _ => (error (op ^ {1 = who,2 = ":vector'value:"}))
fun outstream'value who v = case v of
                              VALUE'Outstream x => x
                            | _ => (error (op ^ {1 = who,2 = ":outstream'value:"
                                                }))
fun instream'value who v = case v of
                             VALUE'Instream x => x
                           | _ => (error (op ^ {1 = who,2 = ":instream'value:"})
                                  )
fun lt'id (ID string1) (ID string2) = op < {1 = string1,2 = string2}
fun lt'lab lab1 lab2 = case (lab1,lab2) of
                         (LAB'Num n1,LAB'Num n2) => (op < {1 = n1,2 = n2})
                       | (LAB'Num _,LAB'Id _) => true
                       | (LAB'Id _,LAB'Num _) => false
                       | (LAB'Id x1,LAB'Id x2) => ((lt'id x1) x2)
fun eq'scon scon1 scon2 = case (scon1,scon2) of
                            (SCON'Num n1,SCON'Num n2) => (op = {1 = n1,2 = n2})
                          | (SCON'Word w1,SCON'Word w2) =>
                              (op = {1 = w1,2 = w2})
                          | (SCON'Real r1,SCON'Real r2) =>
                              (error "eq'scon:real/real")
                          | (SCON'Char c1,SCON'Char c2) =>
                              (op = {1 = c1,2 = c2})
                          | (SCON'String s1,SCON'String s2) =>
                              (op = {1 = s1,2 = s2})
                          | _ => (error "eq'scon")
fun sortPairList'record (RECORD assoc) =
  (sort (fn ((lab1,_),(lab2,_)) => (lt'lab lab1) lab2)) (list'assoc assoc)
fun eq'record eq'value r1 r2 =
  let fun loop [] [] = true
        | loop [] _ = error "eq'record:arg2 has extra elems"
        | loop _ [] = error "eq'record:arg1 has extra elems"
        | loop (op :: {1 = (lab1,v1),2 = lvs1}) (op :: {1 = (lab2,v2),2 = lvs2})
      = if not ((eq'lab lab1) lab2) then error "eq'record:diff labs"
        else ((eq'value v1) v2) andalso ((loop lvs1) lvs2)
  in (loop (sortPairList'record r1)) (sortPairList'record r2)
  end
fun eq'exname (x1 : exname) x2 = op = {1 = x1,2 = x2}
fun eq'exval eq'value exval1 exval2 =
  case (exval1,exval2) of
    (EXVAL'Con0 vid1,EXVAL'Con0 vid2) => ((eq'exname vid1) vid2)
  | (EXVAL'Con1 (vid1,v1),EXVAL'Con1 (vid2,v2)) =>
      (((eq'exname vid1) vid2) andalso ((eq'value v1) v2))
  | _ => false
fun eq'value v1 v2 = case (v1,v2) of
                       (VALUE'Special scon1,VALUE'Special scon2) =>
                         ((eq'scon scon1) scon2)
                     | (VALUE'Con0 vid1,VALUE'Con0 vid2) => ((eq'vid vid1) vid2)
                     | (VALUE'Con1 (vid1,v1),VALUE'Con1 (vid2,v2)) =>
                         (((eq'vid vid1) vid2) andalso ((eq'value v1) v2))
                     | (VALUE'Exval exval1,VALUE'Exval exval2) =>
                         (((eq'exval eq'value) exval1) exval2)
                     | (VALUE'Record r1,VALUE'Record r2) =>
                         (((eq'record eq'value) r1) r2)
                     | (VALUE'Addr a1,VALUE'Addr a2) => (op = {1 = a1,2 = a2})
                     | (VALUE'ColonEq,_) => (error "eq'value: ColonEq")
                     | (_,VALUE'ColonEq) => (error "eq'value: ColonEq")
                     | (VALUE'Basval _,_) => (error "eq'value: Basval")
                     | (_,VALUE'Basval _) => (error "eq'value: Basval")
                     | (VALUE'Ref,_) => (error "eq'value: Ref")
                     | (_,VALUE'Ref) => (error "eq'value: Ref")
                     | (VALUE'Closure _,_) => (error "eq'value: Closure")
                     | (_,VALUE'Closure _) => (error "eq'value: Closure")
                     | (VALUE'Vector _,_) => (error "eq'value: Vector")
                     | (_,VALUE'Vector _) => (error "eq'value: Vector")
                     | (VALUE'Outstream _,_) => (error "eq'value: Outstream")
                     | (_,VALUE'Outstream _) => (error "eq'value: Outstream")
                     | (VALUE'Instream _,_) => (error "eq'value: Instream")
                     | (_,VALUE'Instream _) => (error "eq'value: Instream")
                     | _ => false
fun apply'coloneq vArg =
  case vArg of
    VALUE'Record record =>
      let val v1 = ((look'record ":=") record) (LAB'Num 1)
          val v2 = ((look'record ":=") record) (LAB'Num 2)
          val U : unit = case v1 of
                           VALUE'Addr addr => (op := {1 = addr,2 = v2})
                         | _ => (error "apply:(:=)[arg1 not an address]")
      in value'unit
      end
  | _ => (error "apply:(:=)[to non record]")
fun apply'closure (CLOSURE f) vArg = f vArg
fun apply'value vFun vArg =
  case vFun of
    VALUE'ColonEq => (apply'coloneq vArg)
  | VALUE'Special _ => (error "apply:scon")
  | VALUE'Basval (_,f) => (f vArg)
  | VALUE'Ref => (VALUE'Addr (ref vArg))
  | VALUE'Con0 vid => (VALUE'Con1 (vid,vArg))
  | VALUE'Con1 _ => (error "apply:Con1")
  | VALUE'Exval (EXVAL'Con0 en) =>
      (VALUE'Exval (EXVAL'Con1 (en,vArg)))
  | VALUE'Exval (EXVAL'Con1 _) =>
      (error "apply:Exval(Con1)")
  | VALUE'Record _ => (error "apply:Record")
  | VALUE'Addr _ => (error "apply:Addr")
  | VALUE'Vector _ => (error "apply:Vector")
  | VALUE'Outstream _ => (error "apply:Outstream")
  | VALUE'Instream _ => (error "apply:Instream")
  | VALUE'Closure closure => ((apply'closure closure) vArg)
fun notYet s = op @@ {1 = op @@ {1 = Left "<...",2 = Word s},2 = Right "...>"}
fun lay'scon scon = case scon of
                      SCON'Num n => (layInt n)
                    | SCON'Word w => (layWord w)
                    | SCON'Real r => (notYet "real")
                    | SCON'Char c => (Word (sofChar c))
                    | SCON'String s => (Word (sofStringLit s))
fun lay'vid (VID (ID s)) = Word s
fun lay'exname (EXNAME {base = vid ,...}) = lay'vid vid
fun lay'id (ID x) = Word x
fun lay'lab lab = case lab of
                    LAB'Id id => (lay'id id)
                  | LAB'Num n => (layInt n)
fun needAtomic isAtomic layAtomic x =
  (if isAtomic x then fn x => x else layBracket) (layAtomic x)
fun isAtomic'exval exval = case exval of
                             EXVAL'Con0 _ => true
                           | EXVAL'Con1 _ => false
fun isAtomic'value value = case value of
                             VALUE'ColonEq => true
                           | VALUE'Special scon => true
                           | VALUE'Basval (s,_) => true
                           | VALUE'Ref => true
                           | VALUE'Con0 _ => true
                           | VALUE'Con1 _ => false
                           | VALUE'Exval exval => (isAtomic'exval exval)
                           | VALUE'Record record => true
                           | VALUE'Addr vr => false
                           | VALUE'Vector _ => true
                           | VALUE'Outstream _ => true
                           | VALUE'Instream _ => true
                           | VALUE'Closure closure => true
val atomic'value = needAtomic isAtomic'value
fun lay'exval V exval =
  case exval of
    EXVAL'Con0 exname => (lay'exname exname)
  | EXVAL'Con1 (exname,value) =>
      (op `` {1 = lay'exname exname,2 = layBracket (V value)})
fun lay'record V (RECORD assoc) =
  layBrace (layCommaSep ((map (fn (lab,v) => op ++ {1 = op ++ {1 = lay'lab lab,
                                                               2 = Word "="},
                                                    2 = V v}))
                         (rev (list'assoc assoc))))
fun raw_lay'value_U V v =
  case v of
    VALUE'ColonEq => (Word ":=")
  | VALUE'Special scon => (lay'scon scon)
  | VALUE'Basval (s,_) => (Word (op ^ {1 = op ^ {1 = "<builtin:",2 = s},2 = ">"}
                                ))
  | VALUE'Ref => (Word "ref")
  | VALUE'Con0 vid => (lay'vid vid)
  | VALUE'Con1 (vid,value) => (op `` {1 = lay'vid vid,2 = (atomic'value V) value
                                     })
  | VALUE'Exval exval => ((lay'exval V) exval)
  | VALUE'Record record => ((lay'record V) record)
  | VALUE'Addr vr => (op `` {1 = Word "ref",2 = (atomic'value V) (! vr)})
  | VALUE'Vector _ => (Word "#[...]")
  | VALUE'Outstream _ => (Word "<outstream>")
  | VALUE'Instream _ => (Word "<instream>")
  | VALUE'Closure closure => (Word "fn")
fun lay'value_U V v =
  if isList'value v
  then laySqBracket (layCommaSep ((map V) ((list'value "lay'value") v)))
  else (raw_lay'value_U V) v
fun lay'value v = (lay'value_U lay'value) v
end : ValueSig
;

