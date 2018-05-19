

val trace_tailcalls = ref false
signature MachineSig = sig type value
                           type builtin
                           type exname
                           type exval
                           type vid
                           type uid
                           type con
                           type scon
                           type pat
                           type atom
                           type cps
                           type cmatch
                           type afunc
                           datatype info = INFO'Uid of uid | INFO'Pat of pat | INFO'Atom of atom
                                           | INFO'String of string
                           datatype q = SLOC of (int * info)
                           datatype r = LOC'Stack of q | LOC'Frame of (int * info)
                                        | LOC'Arg of (int * info) | LOC'Cret | LOC'Xret
                                        | LOC'Value of value
                           datatype im = IM'MatchScon of (r * scon) | IM'MatchC of (r * con)
                                         | IM'MatchE of (r * exname) | IM'MatchG of (r * r)
                           datatype label = LABEL of int
                           datatype code = CODE'Seq of (instruction * code) | CODE'Return of r
                                           | CODE'TailCall of (r * r list) | CODE'Raise of r
                                           | CODE'If of (im * code * code)
                                           | CODE'Where of (code * label * code)
                                           | CODE'Goto of label
                           and instruction = INS'NewTuple of (q * {tuple_size : int})
                                             | INS'NewException of (q * vid)
                                             | INS'NewFn of (q * {frame_size : int,num_args : int} *
                                                             code * {name : string,info : afunc})
                                             | INS'NewRef of (q * r) | INS'NewCon of (q * con * r)
                                             | INS'Copy of (q * r) | INS'DeCon of (q * r)
                                             | INS'DeExcon of (q * r) | INS'DeRef of (q * r)
                                             | INS'DeTuple of (q * r * int)
                                             | INS'BuiltinApp of (q * builtin * r list)
                                             | INS'PushContinuation of ({frame_size : int} * code *
                                                                        {info : (cps * uid list)})
                                             | INS'PushHandler of ({frame_size : int} * code *
                                                                   {info : (cmatch * uid list)})
                                             | INS'SetTupleElement of (r * int * r)
                                             | INS'SetFrameElement of (r * int * r)
                                             | INS'SetContFrameElem of (int * r)
                                             | INS'SetXcontFrameElem of (int * r)
                           type layout
                           val lay'code : (code -> layout)
                           val eval'code : (code -> (value,exval) either)
                       end
signature PrelSig_Import = sig val echo : (string -> unit)
                               val abort : (string -> 'a)
                               val filter : (('a -> bool) -> ('a list -> 'a list))
                               val stringOfInt : (int -> string)
                               val exists : (('a -> bool) -> ('a list -> bool))
                               val enumerateFrom : (int -> ('a list -> ('a * int) list))
                               val spaces : (int -> string)
                               val max : (int -> (int -> int))
                               val upto : ((int * int) -> int list)
                           end
signature AssocSig_Import = sig type ('a,'b) assoc
                                val assoc'empty : ('a,'b) assoc
                                val extend'assoc : (('a,'b) assoc -> (('a * 'b) -> ('a,'b) assoc))
                                val look'assoc : (string -> ((''a -> string) ->
                                                             ((''a,'b) assoc -> (''a -> 'b))))
                            end
signature PosSig_Import = sig
                          end
signature AtomSig_Import = sig type vid
                               type lab
                               datatype scon = SCON'Num of int | SCON'Word of word
                                               | SCON'Real of real | SCON'Char of char
                                               | SCON'String of string
                               val sof'vid : (vid -> string)
                           end
signature EmbedSig_Import = sig type exname
                                type value
                                type vid
                                type scon
                                type con
                                type uid
                                val sof'uid : (uid -> string)
                                datatype pat = PAT'Wild | PAT'V of uid | PAT'Layered of (uid * pat)
                                               | PAT'Ref of pat | PAT'C of (con * pat option)
                                               | PAT'E of (exname * pat option)
                                               | PAT'G of (uid * pat option) | PAT'Scon of scon
                                               | PAT'Tuple of pat list
                                type exp
                                type func
                                type match
                                type dec
                                type env
                            end
signature ValueSig_Import = sig type vid
                                type lab
                                type scon
                                type exname
                                type value
                                type closure
                                type exval
                                type layout
                                type cid
                                type cexp
                                type cact
                                type cdef
                                type builtin
                                val name'builtin : (builtin -> string)
                                val apply'builtin : (builtin -> (value list -> value))
                                type con
                                val eq'con : (con -> (con -> bool))
                                val sof'con : (con -> string)
                                val n'con : (con -> int)
                                val value'null : (string -> value)
                                val value'exn : ((string * exn * (unit -> layout) *
                                                  (unit -> (cid * (((value -> cexp) *
                                                                    (cdef -> unit) * (cact -> unit))
                                                                   -> cexp))) *
                                                  (value -> (value,exval) either)) -> value)
                                val exn'value : (value -> exn option)
                                val value'newTuple : (string -> (int -> value))
                                val setTuple'value : (string -> ((value * int * value) -> unit))
                                val value'getTuple : (string -> ((value * int) -> value))
                                val apply'value : (value -> (value -> (value,exval) either))
                                val funcName'value : (value -> string)
                                val value'con1 : ((con * value) -> value)
                                val value'exval : (exval -> value)
                                val value'addr : (value ref -> value)
                                val gen'exname : (vid -> exname)
                                val vid'exname : (exname -> vid)
                                val n'exname : (exname -> int)
                                val exval'con0 : (exname -> exval)
                                val exval'value : (string -> (value -> exval))
                                val scon'value : (string -> (value -> scon))
                                val con'value : (string -> (value -> (con * value option)))
                                val excon'value : (string -> (value -> (exname * value option)))
                                val derefAddr'value : (string -> (value -> value))
                                val eq'scon : (scon -> (scon -> bool))
                                val eq'exname : (exname -> (exname -> bool))
                                val lay'value : (value -> layout)
                            end
signature MisclaySig_Import = sig type layout
                                  val Word : (string -> layout)
                                  val Left : (string -> layout)
                                  val Right : (string -> layout)
                                  val Sep : (string -> layout)
                                  val Blank : layout
                                  val squash_to : (int -> (layout -> layout))
                                  val prefixed_output_layout : (string -> (layout -> unit))
                                  val // : ((layout * layout) -> layout)
                                  val @@ : ((layout * layout) -> layout)
                                  val ++ : ((layout * layout) -> layout)
                                  val `` : ((layout * layout) -> layout)
                                  val //`` : ((layout * layout) -> layout)
                                  val layInt : (int -> layout)
                                  val layBracket : (layout -> layout)
                                  val layBrace : (layout -> layout)
                                  val layListSep : (layout -> (layout list -> layout))
                                  val layCommaSep : (layout list -> layout)
                                  val layVert : (layout list -> layout)
                              end
signature PrettySig_Import = sig type layout
                                 type scon
                                 type dec
                                 val lay'scon : (scon -> layout)
                             end
signature CpsSig_Import = sig type value
                              type vid
                              type uid
                              type scon
                              type pat
                              type cps
                              type atom
                              type bind
                              type afunc
                              type cmatch
                              type amatch
                              type layout
                              val lay'afunc : (afunc -> layout)
                              val lay'cmatch : (cmatch -> layout)
                              val N_lay'pat : (int -> (pat -> layout))
                              val N_lay'atom : ({depth : int} -> (atom -> layout))
                              val N_lay'cps_tail : ({depth : int} -> (cps -> layout))
                          end
signature CcodeSig_Import = sig type cid
                                type clab
                                type ctype
                                type ccall
                                type cexp
                                type cact
                                type cstat
                                type cdef
                                val makeCid : (string -> cid)
                                val clab's : (string -> clab)
                                val ccall : ((cid * cexp list) -> ccall)
                                val cdef'FuncDef : ((ctype * cid * (ctype * cid) list * cstat) ->
                                                    cdef)
                                val cdef'VarDef : ((ctype * cid * cexp) -> cdef)
                                val cexp'FunCall : (ccall -> cexp)
                                val cexp'Id : (cid -> cexp)
                                val cact'Assign : ((cid * cexp) -> cact)
                                val cact'Call : (ccall -> cact)
                                val cstat'DecListThen : ((ctype * cid list * cstat) -> cstat)
                                val cstat'ActThen : ((cact * cstat) -> cstat)
                                val cstat'Goto : (clab -> cstat)
                                val cstat'If : ((cexp * cstat * cstat) -> cstat)
                                val cstat'Where : ((cstat * clab * cstat) -> cstat)
                                val newCid : (string -> cid)
                                val ctype'Nword : ctype
                                val ctype'Ncode : ctype
                                val cexp'NewTup : (int -> cexp)
                                val cexp'NewException : (string -> cexp)
                                val cexp'NewFn : ({frame_size : int,num_args : int} ->
                                                  (cexp -> cexp))
                                val cexp'NewRef : (cexp -> cexp)
                                val cexp'NewCon : (int -> (cexp -> cexp))
                                val cexp'Copy : (cexp -> cexp)
                                val cexp'DeCon : (cexp -> cexp)
                                val cexp'DeExcon : (cexp -> cexp)
                                val cexp'DeRef : (cexp -> cexp)
                                val cexp'DeTuple : (cexp -> (int -> cexp))
                                val cexp'Builtin : (string -> (cexp list -> cexp))
                                val ccall'PushContinuation : (int -> (cexp -> ccall))
                                val ccall'PushHandler : (int -> (cexp -> ccall))
                                val ccall'SetTupleElement : (cexp -> (int -> (cexp -> ccall)))
                                val ccall'SetFrameElement : (cexp -> (int -> (cexp -> ccall)))
                                val ccall'SetContFrameElem : (int -> (cexp -> ccall))
                                val ccall'SetXcontFrameElem : (int -> (cexp -> ccall))
                                val cexp'MatchNum : (cexp -> (int -> cexp))
                                val cexp'MatchWord : (cexp -> (word -> cexp))
                                val cexp'MatchChar : (cexp -> (char -> cexp))
                                val cexp'MatchString : (cexp -> (string -> cexp))
                                val cexp'MatchC : (cexp -> (int -> cexp))
                                val cexp'MatchE : (cexp -> (int -> cexp))
                                val cexp'MatchG : (cexp -> (cexp -> cexp))
                                val cstat'ReturnWith : (cexp -> cstat)
                                val cstat'Tail : (cexp -> (cexp list -> cstat))
                                val cstat'Raise : (cexp -> cstat)
                                val cexp'Frame : (int -> cexp)
                                val cexp'Arg : (int -> cexp)
                                val cexp'Cret : cexp
                                val cexp'Xret : cexp
                                val cstat'pap : (cexp -> ({num_early : int,num_late : int} ->
                                                          (cexp list -> cstat)))
                                type layout
                            end
functor
MACHINE (val prefixNML : string
         structure Prel : PrelSig_Import
         structure Assoc : AssocSig_Import
         structure Pos : PosSig_Import
         structure Atom : AtomSig_Import
         structure Embed : EmbedSig_Import
         structure Value : ValueSig_Import
         structure Misclay : MisclaySig_Import
         structure Pretty : PrettySig_Import
         structure Cps : CpsSig_Import
         structure Ccode : CcodeSig_Import
         val wrapCol : int
         val display_counts : bool ref sharing type Atom.lab = Value.lab sharing type
         Atom.vid = Value.vid = Cps.vid = Embed.vid sharing type Cps.pat = Embed.pat sharing
         type Atom.scon = Value.scon = Pretty.scon = Embed.scon sharing type
         Misclay.layout = Value.layout = Cps.layout sharing type
         Misclay.layout = Pretty.layout = Ccode.layout sharing type Embed.con = Value.con
         sharing type Embed.exname = Value.exname sharing type Embed.value = Value.value
         sharing type Ccode.cid = Value.cid sharing type Ccode.cexp = Value.cexp sharing
         type Ccode.cact = Value.cact sharing type Ccode.cdef = Value.cdef) =
struct
open Cps Prel Assoc Pos Atom Value Misclay Pretty Embed Ccode
fun list'array array = (map (fn n => Array.sub (array,n)))
                       (upto (0,op - {1 = Array.length array,2 = 1}))
fun member eq ys x = (exists (fn y => (eq x) y)) ys
fun removeDups _ [] = []
  | removeDups eq (op :: {1 = x,2 = xs}) =
  op :: {1 = x,2 = (removeDups eq) (if ((member eq) xs) x
                                    then (filter (fn y => not ((eq x) y))) xs else xs)}
datatype info = INFO'Uid of uid | INFO'Pat of pat | INFO'Atom of atom
                | INFO'String of string
val name'pat = fn PAT'V x => op ^ {1 = sof'uid x,2 = "_"}
               | PAT'Layered (x,pat) => op ^ {1 = sof'uid x,2 = "_AS_"}
               | _ => ""
val name'info = fn INFO'Uid x => op ^ {1 = sof'uid x,2 = "_"}
                | INFO'Pat pat => name'pat pat
                | INFO'Atom atom => ""
                | INFO'String _ => ""
fun lay'vid x = Word (sof'vid x)
fun lay'uid x = Word (sof'uid x)
fun lay'con x = Word (sof'con x)
fun N_lay'info {depth = n} info =
  case info of
    INFO'Uid x => (lay'uid x)
  | INFO'Pat pat => ((N_lay'pat n) pat)
  | INFO'Atom atom => ((N_lay'atom {depth = n}) atom)
  | INFO'String s => (Word (op ^ {1 = op ^ {1 = "<",2 = s},2 = ">"}))
datatype q = SLOC of (int * info)
datatype r = LOC'Stack of q | LOC'Frame of (int * info) | LOC'Arg of (int * info) | LOC'Cret
             | LOC'Xret | LOC'Value of value
fun info'q (SLOC (_,info)) = info
fun eq'q (SLOC (n1,_)) (SLOC (n2,_)) = op = {1 = n1,2 = n2}
datatype im = IM'MatchScon of (r * scon) | IM'MatchC of (r * con)
              | IM'MatchE of (r * exname) | IM'MatchG of (r * r)
datatype label = LABEL of int
datatype code = CODE'Seq of (instruction * code) | CODE'Return of r
                | CODE'TailCall of (r * r list) | CODE'Raise of r
                | CODE'If of (im * code * code) | CODE'Where of (code * label * code)
                | CODE'Goto of label
and instruction = INS'NewTuple of (q * {tuple_size : int}) | INS'NewException of (q * vid)
                  | INS'NewFn of (q * {frame_size : int,num_args : int} * code *
                                  {name : string,info : afunc}) | INS'NewRef of (q * r)
                  | INS'NewCon of (q * con * r) | INS'Copy of (q * r) | INS'DeCon of (q * r)
                  | INS'DeExcon of (q * r) | INS'DeRef of (q * r)
                  | INS'DeTuple of (q * r * int) | INS'BuiltinApp of (q * builtin * r list)
                  | INS'PushContinuation of ({frame_size : int} * code *
                                             {info : (cps * uid list)})
                  | INS'PushHandler of ({frame_size : int} * code *
                                        {info : (cmatch * uid list)})
                  | INS'SetTupleElement of (r * int * r)
                  | INS'SetFrameElement of (r * int * r) | INS'SetContFrameElem of (int * r)
                  | INS'SetXcontFrameElem of (int * r)
datatype icounts = Icounts of {NT : int ref,NF : int ref,NR : int ref,NC : int ref,
                               TAIL : int ref,NX : int ref,CP : int ref,BI : int ref,
                               DC : int ref,DE : int ref,DR : int ref,DT : int ref,
                               PC : int ref,PCX : int ref,PH : int ref,ST : int ref,
                               SF : int ref,SC : int ref,SX : int ref,MS : int ref,
                               MC : int ref,ME : int ref,MG : int ref,IF : int ref,
                               CW : int ref,GO : int ref,PAP : int ref,REAPP : int ref,
                               RET : int ref,RAISE : int ref,SKIP : int ref}
fun new'icounts () = Icounts {NT = ref 0,NF = ref 0,NR = ref 0,NC = ref 0,TAIL = ref 0,
                              NX = ref 0,CP = ref 0,BI = ref 0,DC = ref 0,DE = ref 0,
                              DR = ref 0,DT = ref 0,PC = ref 0,PCX = ref 0,PH = ref 0,
                              ST = ref 0,SF = ref 0,SC = ref 0,SX = ref 0,MS = ref 0,
                              MC = ref 0,ME = ref 0,MG = ref 0,IF = ref 0,CW = ref 0,
                              GO = ref 0,PAP = ref 0,REAPP = ref 0,RET = ref 0,
                              RAISE = ref 0,SKIP = ref 0}
fun += {1 = r as ref x,2 = n} = op := {1 = r,2 = op + {1 = n,2 = x}}
fun inc (r as ref x) = op := {1 = r,2 = op + {1 = x,2 = 1}}
fun dec (r as ref x) = op := {1 = r,2 = op - {1 = x,2 = 1}}
fun count'im (Icounts {NT = NT,NX = NX,NF = NF,NR = NR,NC = NC,CP = CP,BI = BI,DC = DC,
                       DE = DE,DR = DR,DT = DT,PC = PC,PCX = PCX,PH = PH,ST = ST,SF = SF,
                       SC = SC,SX = SX,MS = MS,MC = MC,ME = ME,MG = MG,IF = IF,CW = CW,
                       GO = GO,RET = RET,RAISE = RAISE,SKIP = SKIP,TAIL = TAIL,PAP = PAP,
                       REAPP = REAPP}) = fn IM'MatchScon (r,scon) => inc MS
                                         | IM'MatchC (r,con) => inc MC
                                         | IM'MatchE (r,exname) => inc ME
                                         | IM'MatchG (r1,r2) => inc MG
fun count'instruction (Icounts {NT = NT,NX = NX,NF = NF,NR = NR,NC = NC,CP = CP,BI = BI,
                                DC = DC,DE = DE,DR = DR,DT = DT,PC = PC,PCX = PCX,PH = PH,
                                ST = ST,SF = SF,SC = SC,SX = SX,MS = MS,MC = MC,ME = ME,
                                MG = MG,IF = IF,CW = CW,GO = GO,RET = RET,RAISE = RAISE,
                                SKIP = SKIP,TAIL = TAIL,PAP = PAP,REAPP = REAPP}) =
  fn INS'NewTuple (q,{tuple_size = tuple_size}) => inc NT
  | INS'NewException (q,vid) => inc NX
  | INS'NewFn (q,{frame_size = frame_size,num_args = num_args},code,info) => inc NF
  | INS'NewRef (q,r) => inc NR
  | INS'NewCon (q,con,r) => inc NC
  | INS'Copy (q,r) => inc CP
  | INS'DeCon (q,r) => inc DC
  | INS'DeExcon (q,r) => inc DE
  | INS'DeRef (q,r) => inc DR
  | INS'DeTuple (q,r,n) => inc DT
  | INS'BuiltinApp (q,bi,rs) => inc BI
  | INS'PushContinuation ({frame_size = frame_size},code,_) => inc PC
  | INS'PushHandler ({frame_size = frame_size},code,_) => inc PH
  | INS'SetTupleElement (r1,n,r2) => inc ST
  | INS'SetFrameElement (r1,n,r2) => inc SF
  | INS'SetContFrameElem (n,r2) => inc SC
  | INS'SetXcontFrameElem (n,r2) => inc SX
fun max (a,b) = if op > {1 = a,2 = b} then a else b
fun count'return depth (Icounts {NT = NT,NX = NX,NF = NF,NR = NR,NC = NC,CP = CP,BI = BI,
                                 DC = DC,DE = DE,DR = DR,DT = DT,PC = PC,PCX = PCX,PH = PH,
                                 ST = ST,SF = SF,SC = SC,SX = SX,MS = MS,MC = MC,ME = ME,
                                 MG = MG,IF = IF,CW = CW,GO = GO,RET = RET,RAISE = RAISE,
                                 SKIP = SKIP,TAIL = TAIL,PAP = PAP,REAPP = REAPP}) =
  (inc RET ;op := {1 = PCX,2 = max (! PCX,depth)})
fun count'raise skip (Icounts {NT = NT,NX = NX,NF = NF,NR = NR,NC = NC,CP = CP,BI = BI,
                               DC = DC,DE = DE,DR = DR,DT = DT,PC = PC,PCX = PCX,PH = PH,
                               ST = ST,SF = SF,SC = SC,SX = SX,MS = MS,MC = MC,ME = ME,
                               MG = MG,IF = IF,CW = CW,GO = GO,RET = RET,RAISE = RAISE,
                               SKIP = SKIP,TAIL = TAIL,PAP = PAP,REAPP = REAPP}) =
  (inc RAISE ;+= {1 = SKIP,2 = skip})
fun count'pap (Icounts {NT = NT,NX = NX,NF = NF,NR = NR,NC = NC,CP = CP,BI = BI,DC = DC,
                        DE = DE,DR = DR,DT = DT,PC = PC,PCX = PCX,PH = PH,ST = ST,SF = SF,
                        SC = SC,SX = SX,MS = MS,MC = MC,ME = ME,MG = MG,IF = IF,CW = CW,
                        GO = GO,RET = RET,RAISE = RAISE,SKIP = SKIP,TAIL = TAIL,PAP = PAP,
                        REAPP = REAPP}) = inc PAP
fun count'reapp (Icounts {NT = NT,NX = NX,NF = NF,NR = NR,NC = NC,CP = CP,BI = BI,DC = DC,
                          DE = DE,DR = DR,DT = DT,PC = PC,PCX = PCX,PH = PH,ST = ST,SF = SF,
                          SC = SC,SX = SX,MS = MS,MC = MC,ME = ME,MG = MG,IF = IF,CW = CW,
                          GO = GO,RET = RET,RAISE = RAISE,SKIP = SKIP,TAIL = TAIL,PAP = PAP,
                          REAPP = REAPP}) = inc REAPP
fun count'tail (Icounts {NT = NT,NX = NX,NF = NF,NR = NR,NC = NC,CP = CP,BI = BI,DC = DC,
                         DE = DE,DR = DR,DT = DT,PC = PC,PCX = PCX,PH = PH,ST = ST,SF = SF,
                         SC = SC,SX = SX,MS = MS,MC = MC,ME = ME,MG = MG,IF = IF,CW = CW,
                         GO = GO,RET = RET,RAISE = RAISE,SKIP = SKIP,TAIL = TAIL,PAP = PAP,
                         REAPP = REAPP}) = fn num_args => inc TAIL
fun count'code (Icounts {NT = NT,NX = NX,NF = NF,NR = NR,NC = NC,CP = CP,BI = BI,DC = DC,
                         DE = DE,DR = DR,DT = DT,PC = PC,PCX = PCX,PH = PH,ST = ST,SF = SF,
                         SC = SC,SX = SX,MS = MS,MC = MC,ME = ME,MG = MG,IF = IF,CW = CW,
                         GO = GO,RET = RET,RAISE = RAISE,SKIP = SKIP,TAIL = TAIL,PAP = PAP,
                         REAPP = REAPP}) = fn CODE'Seq (ins,code) => ()
                                           | CODE'Return r => ()
                                           | CODE'TailCall (func,args) => ()
                                           | CODE'Raise r => ()
                                           | CODE'If (im,code1,code2) => inc IF
                                           | CODE'Where (code1,lab,code2) => inc CW
                                           | CODE'Goto lab => inc GO
fun lay'count tag (ref x) = op @@ {1 = op @@ {1 = Word tag,2 = Sep "="},2 = layInt x}
val layCommaSep = layListSep (Right ",")
fun lay'icounts (Icounts {NT = NT,NX = NX,NF = NF,NR = NR,NC = NC,CP = CP,BI = BI,DC = DC,
                          DE = DE,DR = DR,DT = DT,PC = PC,PCX = PCX,PH = PH,ST = ST,SF = SF,
                          SC = SC,SX = SX,MS = MS,MC = MC,ME = ME,MG = MG,IF = IF,CW = CW,
                          GO = GO,RET = RET,RAISE = RAISE,SKIP = SKIP,TAIL = TAIL,PAP = PAP,
                          REAPP = REAPP}) =
  op @@ {1 = Word "icounts: ",
         2 = layVert [op @@ {1 = Word "Control:",2 = layCommaSep [(lay'count "Call") TAIL,
                                                                  (lay'count "Push") PC,
                                                                  (lay'count "Handle") PH,
                                                                  (lay'count "Reapp") REAPP,
                                                                  (lay'count "Return") RET,
                                                                  (lay'count "Raise") RAISE,
                                                                  (lay'count "Skip") SKIP]},
                      op @@ {1 = Word "FrameSet:",
                             2 = layCommaSep [(lay'count "Function")
                                              SF,(lay'count "Continuation") SC,
                                              (lay'count "Handler") SX]},
                      op @@ {1 = Word "Branch:",2 = layCommaSep [(lay'count "If") IF,
                                                                 (lay'count "CodeWhere") CW,
                                                                 (lay'count "Goto") GO]},
                      op @@ {1 = Word "Match:",2 = layCommaSep [(lay'count "Scon") MS,
                                                                (lay'count "Con") MC,
                                                                (lay'count "Excon") ME,
                                                                (lay'count "GenEx") MG]},
                      op @@ {1 = Word "New:",2 = layCommaSep [(lay'count "Closure") NF,
                                                              (lay'count "Pap") PAP,
                                                              (lay'count "Tuple") NT,
                                                              (lay'count "TupleElem") ST,
                                                              (lay'count "Con") NC,
                                                              (lay'count "Ref") NR,
                                                              (lay'count "Exception") NX]},
                      op @@ {1 = Word "Destruct",2 = layCommaSep [(lay'count "TupleElem")
                                                                  DT,(lay'count "Con") DC,
                                                                  (lay'count "Excon") DE,
                                                                  (lay'count "Ref") DR]},
                      op @@ {1 = Word "Builtin:",2 = layCommaSep [(lay'count "App") BI,
                                                                  (lay'count "Copy") CP]},
                      (lay'count "Max-Control-Stack-Depth") PCX,
                      if op <> {1 = op - {1 = op + {1 = op + {1 = op + {1 = 1,2 = ! PC},
                                                              2 = ! PH},2 = ! REAPP},
                                          2 = op + {1 = ! RET,2 = ! SKIP}},2 = 0}
                      then Word "*** Invariant Failed: (1+Push+ReApp+PushHandler)-(Return+Skip) = 0"
                      else Blank]}
fun lay'q (SLOC (n,info)) = Word (op ^ {1 = op ^ {1 = name'info info,2 = "q"},2 = stringOfInt n})
fun lay'r location = case location of
                       LOC'Stack q => (lay'q q)
                     | LOC'Frame (n,info) => (Word (op ^ {1 = op ^ {1 = name'info info,2 = "FRAME_"
                                                                   },2 = stringOfInt n}))
                     | LOC'Arg (n,info) => (Word (op ^ {1 = op ^ {1 = name'info info,2 = "ARG_"},
                                                        2 = stringOfInt n}))
                     | LOC'Cret => (Word "CRET")
                     | LOC'Xret => (Word "XRET")
                     | LOC'Value v => (op @@ {1 = op @@ {1 = Left "value<",2 = lay'value v},
                                              2 = Right ">"})
fun lay'exname exname = lay'vid (vid'exname exname)
fun layComment lay = op @@ {1 = Word "//",2 = lay}
fun layBind' q lay = op @@ {1 = op @@ {1 = lay'q q,2 = Word "="},2 = lay}
fun layBind q lay = op @@ {1 = (layBind' q) lay,2 = layComment ((N_lay'info {depth = 4}) (info'q q))
                          }
fun layIndexed lay1 n = op @@ {1 = op @@ {1 = op @@ {1 = lay1,2 = Sep "["},2 = layInt n},
                               2 = Right "]"}
fun layIndexBind lay1 n lay2 = op @@ {1 = op @@ {1 = (layIndexed lay1) n,2 = Word "="},2 = lay2}
fun lay'op name lay = op @@ {1 = Word name,2 = layBracket lay}
fun lay'match r lay = op @@ {1 = op @@ {1 = lay'r r,2 = Word "=="},2 = lay}
fun layNestedCode head body = op // {1 = op //`` {1 = op @@ {1 = head,2 = Word "{"},2 = body},
                                     2 = Word "}"}
fun lay'save xs = layBrace (layCommaSep ((map lay'uid) xs))
fun layContinuationInfo (cps,save) =
  op // {1 = layComment (lay'save save),
         2 = layComment (op @@ {1 = Word "CRET=",2 = (N_lay'cps_tail {depth = 4}) cps})}
fun layHandleInfo (cmatch,save) =
  op // {1 = layComment (lay'save save),
         2 = layComment (op @@ {1 = Word "handler=",2 = lay'cmatch cmatch})}
fun layAngle x = op ++ {1 = op @@ {1 = Left "<",2 = x},2 = Right ">"}
fun layArgs lay args = layAngle (layCommaSep ((map lay) args))
fun lay'NewFn ({frame_size = frame_size,num_args = num_args},{name = name,info = afunc},C) =
  (layNestedCode (Left (op ^ {1 = "NewFn:",2 = name})))
  (op // {1 = layComment (op @@ {1 = Word (op ^ {1 = "#frame=",2 = stringOfInt frame_size}),
                                 2 = Word (op ^ {1 = "#args=",2 = stringOfInt num_args})}),2 = C})
fun lay'im im = case im of
                  IM'MatchScon (r,scon) =>
                    (op @@ {1 = Word "MatchScon:",2 = (lay'match r) (lay'scon scon)})
                | IM'MatchC (r,con) => (op @@ {1 = Word "MatchC:",2 = (lay'match r) (lay'con con)})
                | IM'MatchE (r,exname) =>
                    (op @@ {1 = Word "MatchE:",2 = (lay'match r) (lay'exname exname)})
                | IM'MatchG (r1,r2) => (op @@ {1 = Word "MatchG:",2 = (lay'match r1) (lay'r r2)})
fun layIfElse im lay1 lay2 =
  op // {1 = (layNestedCode (op @@ {1 = Word "if",2 = layBracket (lay'im im)})) lay1,
         2 = (layNestedCode (Word "else")) lay2}
fun sof'label (LABEL n) = op ^ {1 = "L_",2 = stringOfInt n}
fun lay'label lab = Word (sof'label lab)
fun lay'instruction ins =
  case ins of
    INS'NewTuple (q,{tuple_size = tuple_size}) =>
      ((layBind q) (op @@ {1 = Left "NewTuple",2 = layInt tuple_size}))
  | INS'NewException (q,vid) =>
      ((layBind q) ((lay'op "NewException") (lay'vid vid)))
  | INS'NewFn (q,sizes,code,info) =>
      ((layBind' q) (lay'NewFn (sizes,info,lay'code code)))
  | INS'NewRef (q,r) => ((layBind q) ((lay'op "Ref") (lay'r r)))
  | INS'NewCon (q,con,r) => ((layBind q) (op @@ {1 = (lay'op "Con") (lay'con con),
                                                 2 = layBracket (lay'r r)}))
  | INS'Copy (q,r) => ((layBind q) ((lay'op "Copy") (lay'r r)))
  | INS'DeCon (q,r) => ((layBind q) ((lay'op "DeCon") (lay'r r)))
  | INS'DeExcon (q,r) => ((layBind q) ((lay'op "DeExCopy") (lay'r r)))
  | INS'DeRef (q,r) => ((layBind q) ((lay'op "DeRef") (lay'r r)))
  | INS'DeTuple (q,r,n) => ((layBind q) ((layIndexed (lay'r r)) n))
  | INS'BuiltinApp (q,bi,rs) => ((layBind q) ((lay'op (op ^ {1 = "BuiltinApp:",2 = name'builtin bi})
                                              ) ((layArgs lay'r) rs)))
  | INS'PushContinuation ({frame_size = frame_size},code,{info = info}) =>
      ((layNestedCode (op @@ {1 = Left "PushContinuation",2 = layBracket (layInt frame_size)}))
       (op // {1 = layContinuationInfo info,2 = lay'code code}))
  | INS'PushHandler ({frame_size = frame_size},code,{info = info}) =>
      ((layNestedCode (op @@ {1 = Left "PushHandler",2 = layBracket (layInt frame_size)}))
       (op // {1 = layHandleInfo info,2 = lay'code code}))
  | INS'SetTupleElement (r1,n,r2) =>
      (op @@ {1 = Word "SetTuple:",2 = ((layIndexBind (lay'r r1)) n) (lay'r r2)})
  | INS'SetFrameElement (r1,n,r2) =>
      (op @@ {1 = Word "SetFrame:",2 = ((layIndexBind (lay'r r1)) n) (lay'r r2)})
  | INS'SetContFrameElem (n,r2) =>
      (op @@ {1 = Word "SetContFrame:",2 = ((layIndexBind (Word "cont")) n) (lay'r r2)})
  | INS'SetXcontFrameElem (n,r2) =>
      (op @@ {1 = Word "SetXcontFrame:",2 = ((layIndexBind (Word "xcont")) n) (lay'r r2)})
and lay'code code =
  case code of
    CODE'Seq (ins,code) => (op // {1 = lay'instruction ins,2 = lay'code code})
  | CODE'Return r => (op @@ {1 = Word "Return:",2 = lay'r r})
  | CODE'TailCall (func,args) =>
      (op @@ {1 = op @@ {1 = Word "Call:",2 = lay'r func},2 = (layArgs lay'r) args
             })
  | CODE'Raise r => (op @@ {1 = Word "Raise:",2 = lay'r r})
  | CODE'If (im,code1,code2) => (((layIfElse im) (lay'code code1))
                                 (lay'code code2))
  | CODE'Where (code1,lab,code2) =>
      (layAngle (op // {1 = lay'code code1,
                        2 = (layNestedCode (op @@ {1 = op @@ {1 = Word "where",2 = lay'label lab},
                                                   2 = Word "="})) (lay'code code2)}))
  | CODE'Goto lab => (op @@ {1 = Word "goto",2 = lay'label lab})
fun cid'q (SLOC (n,info)) = makeCid (op ^ {1 = op ^ {1 = name'info info,2 = "q"},2 = stringOfInt n})
fun cexp'matchScon e scon = case scon of
                              SCON'Num n => ((cexp'MatchNum e) n)
                            | SCON'Word w => ((cexp'MatchWord e) w)
                            | SCON'Real r => (abort "cexp'matchScon/real")
                            | SCON'Char c => ((cexp'MatchChar e) c)
                            | SCON'String s => ((cexp'MatchString e) s)
fun cexp'im R im = case im of
                     IM'MatchScon (r,scon) => ((cexp'matchScon (R r)) scon)
                   | IM'MatchC (r,con) => ((cexp'MatchC (R r)) (n'con con))
                   | IM'MatchE (r,exname) => ((cexp'MatchE (R r)) (n'exname exname))
                   | IM'MatchG (r1,r2) => ((cexp'MatchG (R r1)) (R r2))
fun clab'label label = clab's (sof'label label)
fun cc'bind (q,cexp) next = cstat'ActThen (cact'Assign (cid'q q,cexp),next)
fun cc'call call next = cstat'ActThen (cact'Call call,next)
fun cstat'instruction Emit R ins =
  case ins of
    INS'NewTuple (q,{tuple_size = tuple_size}) =>
      (cc'bind (q,cexp'NewTup tuple_size))
  | INS'NewException (q,vid) => (cc'bind (q,cexp'NewException (sof'vid vid)))
  | INS'NewFn (q,sizes,code,{info = info,name = name}) =>
      (cc'bind (q,(cexp'NewFn sizes) ((Emit name) code)))
  | INS'NewRef (q,r) => (cc'bind (q,cexp'NewRef (R r)))
  | INS'NewCon (q,con,r) => (cc'bind (q,(cexp'NewCon (n'con con)) (R r)))
  | INS'Copy (q,r) => (cc'bind (q,cexp'Copy (R r)))
  | INS'DeCon (q,r) => (cc'bind (q,cexp'DeCon (R r)))
  | INS'DeExcon (q,r) => (cc'bind (q,cexp'DeExcon (R r)))
  | INS'DeRef (q,r) => (cc'bind (q,cexp'DeRef (R r)))
  | INS'DeTuple (q,r,n) => (cc'bind (q,(cexp'DeTuple (R r)) n))
  | INS'BuiltinApp (q,bi,rs) => (cc'bind (q,(cexp'Builtin (name'builtin bi)) ((map R) rs)))
  | INS'PushContinuation ({frame_size = frame_size},code,_) =>
      (cc'call ((ccall'PushContinuation frame_size) ((Emit "cont") code)))
  | INS'PushHandler ({frame_size = frame_size},code,_) =>
      (cc'call ((ccall'PushHandler frame_size) ((Emit "handler") code)))
  | INS'SetTupleElement (r1,n,r2) => (cc'call (((ccall'SetTupleElement (R r1)) n) (R r2)))
  | INS'SetFrameElement (r1,n,r2) => (cc'call (((ccall'SetFrameElement (R r1)) n) (R r2)))
  | INS'SetContFrameElem (n,r) => (cc'call ((ccall'SetContFrameElem n) (R r)))
  | INS'SetXcontFrameElem (n,r) => (cc'call ((ccall'SetXcontFrameElem n) (R r)))
fun cstat'code Emit R code =
  case code of
    CODE'Seq (ins,code) => ((((cstat'instruction Emit) R) ins)
                            (((cstat'code Emit) R) code))
  | CODE'Return r => (cstat'ReturnWith (R r))
  | CODE'TailCall (func,args) => ((cstat'Tail (R func)) ((map R) args))
  | CODE'Raise r => (cstat'Raise (R r))
  | CODE'If (im,code1,code2) => (cstat'If ((cexp'im R) im,((cstat'code Emit) R) code1,
                                           ((cstat'code Emit) R) code2))
  | CODE'Where (code1,lab,code2) => (cstat'Where (((cstat'code Emit) R) code1,clab'label lab,
                                                  ((cstat'code Emit) R) code2))
  | CODE'Goto lab => (cstat'Goto (clab'label lab))
fun cexp'r V location = case location of
                          LOC'Stack q => (cexp'Id (cid'q q))
                        | LOC'Frame (n,info) => (cexp'Frame n)
                        | LOC'Arg (n,info) => (cexp'Arg n)
                        | LOC'Cret => cexp'Cret
                        | LOC'Xret => cexp'Xret
                        | LOC'Value v => (V v)
fun qs'instruction acc ins = case ins of
                               INS'NewTuple (q,{tuple_size = tuple_size}) => (op :: {1 = q,2 = acc})
                             | INS'NewException (q,vid) => (op :: {1 = q,2 = acc})
                             | INS'NewFn (q,sizes,code,{info = info,name = name}) =>
                                 (op :: {1 = q,2 = acc})
                             | INS'NewRef (q,r) => (op :: {1 = q,2 = acc})
                             | INS'NewCon (q,con,r) => (op :: {1 = q,2 = acc})
                             | INS'Copy (q,r) => (op :: {1 = q,2 = acc})
                             | INS'DeCon (q,r) => (op :: {1 = q,2 = acc})
                             | INS'DeExcon (q,r) => (op :: {1 = q,2 = acc})
                             | INS'DeRef (q,r) => (op :: {1 = q,2 = acc})
                             | INS'DeTuple (q,r,n) => (op :: {1 = q,2 = acc})
                             | INS'BuiltinApp (q,bi,rs) => (op :: {1 = q,2 = acc})
                             | INS'PushContinuation ({frame_size = frame_size},code,_) => acc
                             | INS'PushHandler ({frame_size = frame_size},code,_) => acc
                             | INS'SetTupleElement (r1,n,r2) => acc
                             | INS'SetFrameElement (r1,n,r2) => acc
                             | INS'SetContFrameElem (n,r) => acc
                             | INS'SetXcontFrameElem (n,r) => acc
fun qs'code acc code = case code of
                         CODE'Seq (ins,code) => ((qs'instruction ((qs'code acc) code)) ins)
                       | CODE'Return r => acc
                       | CODE'TailCall (func,args) => acc
                       | CODE'Raise r => acc
                       | CODE'If (im,code1,code2) => ((qs'code ((qs'code acc) code2)) code1)
                       | CODE'Where (code1,lab,code2) => ((qs'code ((qs'code acc) code2)) code1)
                       | CODE'Goto lab => acc
fun NEW_cexp'code cexp'V EmitDef name code =
  let val R = cexp'r cexp'V
      fun Emit name code = let val x = newCid name
                               val xs = (removeDups eq'q) ((qs'code []) code)
                               val body = ((cstat'code Emit) R) code
                               val body = case xs of
                                            [] => body
                                          | op :: {1 = _,2 = _} =>
                                              (cstat'DecListThen (ctype'Nword,(map cid'q) xs,body))
                               val def = cdef'FuncDef (ctype'Ncode,x,[],body)
                               val e = cexp'Id x
                               val () = EmitDef def
                           in e
                           end
  in (Emit name) code
  end
val NewTuple : (int -> value) = fn N => (value'newTuple "NewTuple") N
val NewException : (vid -> value) = fn vid => value'exval (exval'con0 (gen'exname vid))
val NewRef : (value -> value) = fn v => value'addr (ref v)
val NewCon : ((con * value) -> value) = fn (x,v) => value'con1 (x,v)
val SetTupleElement : ((value * int * value) -> unit) =
    fn (tup,n,v) => (setTuple'value "SetTupleElement") (tup,n,v)
val DeTuple : ((value * int) -> value) = fn (tup,n) => (value'getTuple "DeTuple") (tup,n)
val BuiltinApp : ((builtin * value list) -> value) = fn (bi,vs) => (apply'builtin bi) vs
val DeCon : (value -> value) = fn v1 => case (con'value "DeCon") v1 of
                                          (_,None) => (abort "Decon:None")
                                        | (_,Some v2) => v2
val DeExcon : (value -> value) = fn v1 => case (excon'value "DeCon") v1 of
                                            (_,None) => (abort "Decon:None")
                                          | (_,Some v2) => v2
val DeRef : (value -> value) = fn v => (derefAddr'value "DeRef") v
val MatchScon : ((scon * value) -> bool) =
    fn (scon,v) => (eq'scon scon) ((scon'value "MatchScon") v)
val MatchC : ((con * value) -> bool) =
    fn (con,v) => (eq'con con) let val (con,_) = (con'value "MatchC") v
                               in con
                               end
val MatchE : ((exname * value) -> bool) =
    fn (exname,v) => (eq'exname exname) let val (exname,_) = (excon'value "MatchE") v
                                        in exname
                                        end
val MatchG : ((value * value) -> bool) =
    fn (v1,v2) => (eq'exname let val (exname,_) = (excon'value "MatchG/1") v1
                             in exname
                             end) let val (exname,_) = (excon'value "MatchG/2") v2
                                  in exname
                                  end
datatype frame = FRAME of value array
fun NewFrame (N : int) = FRAME (Array.array (N,value'null "NewFrame"))
fun SetFrame (FRAME A,n,v) = Array.update (A,n,v)
fun GetFrame (FRAME A,n) = Array.sub (A,n)
fun lay'frame (FRAME A) = layVert ((map (fn n => op @@ {1 = op @@ {1 = layInt n,2 = Word "="},
                                                        2 = lay'value (Array.sub (A,n))}))
                                   (upto (0,op - {1 = Array.length A,2 = 1})))
fun MkFrame args = let val frame = NewFrame (length args)
                       val _ : unit list = (map (fn (arg,n) => SetFrame (frame,n,arg)))
                                           ((enumerateFrom 0) args)
                   in frame
                   end
datatype stack = STACK of (int,value) assoc ref
fun EmptyStack () = STACK (ref assoc'empty)
fun GetStack (STACK A,n) = (((look'assoc "GetStack") stringOfInt) (! A)) n
fun SetStack (STACK A,n,v) = op := {1 = A,2 = (extend'assoc (! A)) (n,v)}
datatype env = ENV of (stack * frame * {args : frame})
fun NewEnv (frame,{args = args}) = ENV (EmptyStack (),frame,{args = MkFrame args})
val GetLoc : ((env * r) -> value) = fn (ENV (stack,frame,{args = args}),r) =>
                                         case r of
                                           LOC'Stack (SLOC (n,_)) => (GetStack (stack,n))
                                         | LOC'Frame (n,_) => (GetFrame (frame,n))
                                         | LOC'Arg (n,_) => (GetFrame (args,n))
                                         | LOC'Cret => (GetFrame (args,0))
                                         | LOC'Xret => (GetFrame (args,0))
                                         | LOC'Value v => v
val GetLocs : ((env * r list) -> value list) =
    fn (env,args) => (map (fn arg => GetLoc (env,arg))) args
val SetStackLoc : ((env * q * value) -> unit) =
    fn (ENV (stack,_,_),SLOC (n,_),v) => SetStack (stack,n,v)
datatype closure_code_info = CCI of (code * string * afunc * {frame_size : int,num_args : int})
fun lay'cci (CCI (code,name,afunc,{frame_size = frame_size,num_args = num_args})) =
  layVert [op @@ {1 = Word "Name:",2 = Word name},op @@ {1 = Word "#Args:",2 = layInt num_args},
           op @@ {1 = Word "#Frame:",2 = layInt frame_size},
           op @@ {1 = Word "Source:",2 = lay'afunc afunc},
           op //`` {1 = Word "Code:",2 = lay'code code}]
datatype closure = Closure of (closure_code_info * frame)
fun num_args'closure (Closure (CCI (code,name,afunc,{frame_size = frame_size,num_args = num_args}),
                               frame)) = num_args
fun cacts'closure clo es =
  let val acts = (map (fn (e,n) => cact'Call (((ccall'SetFrameElement (cexp'Id clo)) n) e)))
                 ((enumerateFrom 0) es)
  in acts
  end
fun cexp'closure_bits V EmitDef (EmitAct : (cact -> unit)) name cid sizes code (FRAME A) =
  let val C = (((NEW_cexp'code V) EmitDef) name) code
      val es = (map V) (list'array A)
      val acts = (cacts'closure cid) es
      val _ : unit list = (map EmitAct) acts
      val exp = (cexp'NewFn sizes) C
      val def = cdef'VarDef (ctype'Nword,cid,exp)
      val () = EmitDef def
  in cexp'Id cid
  end
fun cexp'closure V EmitDef EmitAct cid (Closure (CCI (code,name,afunc,sizes),frame)) =
  (((((((cexp'closure_bits V) EmitDef) EmitAct) name) cid) sizes) code) frame
fun cexp'pap V EmitDef EmitAct name cid closure early =
  let val es = (map V) early
      val func = ((((cexp'closure V) EmitDef) EmitAct) (newCid (op ^ {1 = "paps_closure_",2 = name})
                                                       )) closure
      val num_early = length early
      val num_late = op - {1 = num_args'closure closure,2 = num_early}
      val pap_sizes = {num_early = num_early,num_late = num_late}
      val body = ((cstat'pap func) pap_sizes) es
      val def = cdef'FuncDef (ctype'Nword,cid,[],body)
      val () = EmitDef def
  in cexp'FunCall (ccall (cid,[]))
  end
type res = (value,exval) either
type interpret = ((closure * value list) -> res)
fun lay'closure (Closure (cci,frame)) = layVert [op @@ {1 = Word "Frame:",2 = lay'frame frame},
                                                 lay'cci cci]
exception V_Closure of closure
fun NewFn (Interpret,sizes as {frame_size = frame_size,num_args = num_args},code,
           {name = name,info = afunc}) =
  let val frame = NewFrame frame_size
      val cci = CCI (code,name,afunc,sizes)
      val closure = Closure (cci,frame)
      fun Decompile () = lay'closure closure
      fun NewExportStage2 cid (V,EmitDef,EmitAct) =
        (((((((cexp'closure_bits V) EmitDef) EmitAct) name)
           cid) sizes) code) frame
      fun NewExportC () = let val cid = newCid (op ^ {1 = "closure_",2 = name})
                          in (cid,NewExportStage2 cid)
                          end
      fun Apply v = Interpret (closure,[v])
  in value'exn (op ^ {1 = "closure:",2 = name},V_Closure closure,Decompile,NewExportC,Apply)
  end
val SetFrameElement : ((value * int * value) -> unit) =
    fn (func,n,v) => case exn'value func of
                       Some (V_Closure (Closure (_,frame))) => (SetFrame (frame,n,v))
                     | _ => (abort "SetFrameElement")
exception V_Pap of (closure * value list)
fun MakePap (Interpret,closure,early) =
  let val Closure (CCI (_,name,_,{frame_size = _,num_args = N}
                       ),_) = closure
      val tag = op ^ {1 = op ^ {1 = op ^ {1 = op ^ {1 = name,2 = ":"},2 = stringOfInt (length early)
                                         },2 = "/"},2 = stringOfInt N}
      fun Decompile () = layVert [op @@ {1 = Word "Pap:",2 = Word tag},
                                  op @@ {1 = Word "Closure:",2 = lay'closure closure},
                                  op @@ {1 = Word "Early:",2 = (layArgs lay'value) early}]
      fun NewExportStage2 cid (V,EmitDef,EmitAct) = ((((((cexp'pap V) EmitDef) EmitAct) name) cid)
                                                     closure) early
      fun NewExportC () = let val cid = newCid (op ^ {1 = "pap_",2 = name})
                          in (cid,NewExportStage2 cid)
                          end
      fun Apply v = Interpret (closure,op @ {1 = early,2 = [v]})
  in value'exn (op ^ {1 = "pap:",2 = tag},V_Pap (closure,early),Decompile,NewExportC,Apply)
  end
local fun loop xs 0 ys = (rev xs,ys)
        | loop xs N [] = abort "splitArgs"
        | loop xs N (op :: {1 = y,2 = ys}) = ((loop (op :: {1 = y,2 = xs})) (op - {1 = N,2 = 1})) ys
in fun splitArgs N ys = ((loop []) N) ys
end
datatype continue = CONTINUE of (code * frame) | OVERAPP of value list
local datatype cont = CONT of continue | HANDLE of (code * frame * int)
      datatype control = CONTROL of {D : int ref,C : cont list ref,H : int ref}
in type control = control
   type jump = ((control * code * env) -> res)
   fun newControl () = CONTROL {D = ref 1,C = ref [],H = ref 0}
   fun depth'control (CONTROL {D = D,C = C,H = H}) = ! D
   val PushContinue : ((control * continue) -> unit) =
       fn (CONTROL {D = D,C = C,H = H},continue) =>
            (inc D ;op := {1 = C,2 = op :: {1 = CONT continue,2 = ! C}})
   val PushHandler : ((control * int * code) -> unit) =
       fn (CONTROL {D = D,C = C,H = H},N,code) =>
            (op := {1 = C,2 = op :: {1 = HANDLE (code,NewFrame N,! H),2 = ! C}} ;
             op := {1 = H,2 = ! D} ;inc D)
   val SetContFrameElement : ((control * int * value) -> unit) =
       fn (CONTROL {D = D,C = C,H = H},n,v) => case ! C of
                                                 op :: {1 = CONT (CONTINUE (_,frame)),2 = _} =>
                                                   (SetFrame (frame,n,v))
                                               | _ => (abort "SetContFrameElement")
   val SetXcontFrameElement : ((control * int * value) -> unit) =
       fn (CONTROL {D = D,C = C,H = H},n,v) => case ! C of
                                                 op :: {1 = HANDLE (_,frame,_),2 = _} =>
                                                   (SetFrame (frame,n,v))
                                               | _ => (abort "SetXcontFrameElement")
   fun PopCont (0,_) = abort "PopCont/0"
     | PopCont (_,[]) = abort "PopCont/FINISH"
     | PopCont (1,op :: {1 = HANDLE p,2 = xs}) = (xs,p)
     | PopCont (_,op :: {1 = HANDLE _,2 = _}) = abort "PopCont/POPH/!1"
     | PopCont (n,op :: {1 = CONT _,2 = xs}) = PopCont (op - {1 = n,2 = 1},xs)
   val Raise : ((icounts * jump * control * value) -> res) =
       fn (icounts,Jump,control as CONTROL {D = D,C = C,H = H},v) =>
            if op = {1 = ! H,2 = 0} then That ((exval'value "Raise") v)
            else let val skip = op - {1 = ! D,2 = ! H}
                     val () = if op < {1 = skip,2 = 0} then abort "Raise:skip<0" else ()
                     val () = (count'raise skip) icounts
                     val (conts,(code,frame,h)) = PopCont (skip,! C)
                     val d = ! H
                 in (op := {1 = H,2 = h} ;op := {1 = D,2 = d} ;op := {1 = C,2 = conts} ;
                     Jump (control,code,NewEnv (frame,{args = [v]})))
                 end
   val rec ReturnContinue : ((icounts * control) -> continue option) =
           fn (icounts,control as CONTROL {D = D,C = C,H = H}) =>
                ((count'return (! D)) icounts ;
                 case ! C of
                   op :: {1 = CONT x,2 = conts} => (dec D ;op := {1 = C,2 = conts} ;Some x)
                 | op :: {1 = HANDLE (_,_,h),2 = conts} =>
                     (op := {1 = H,2 = h} ;dec D ;op := {1 = C,2 = conts} ;
                      ReturnContinue (icounts,control))
                 | [] => None)
end
val PushContinuation : ((control * int * code) -> unit) =
    fn (control,N,code) => PushContinue (control,CONTINUE (code,NewFrame N))
val PushReApp : ((control * value list) -> unit) =
    fn (control,args) => PushContinue (control,OVERAPP args)
val rec Return : ((icounts * jump * control * value) -> res) =
        fn (icounts,Jump,control,v) =>
             case ReturnContinue (icounts,control) of
               None => (This v)
             | Some x => (case x of
                            CONTINUE (code,frame) => (Jump (control,code,NewEnv (frame,{args = [v]})
                                                           ))
                          | OVERAPP args => (TailCall (icounts,Jump,control,v,args)))
        and Interpret : ((icounts * jump) -> ((closure * value list) -> res)) =
            fn (icounts,Jump) => fn (closure,args) => Enter (icounts,Jump,newControl (),closure,args
                                                            )
            and Enter : ((icounts * jump * control * closure * value list) -> res) =
                fn (icounts,Jump,control,
                    closure as Closure (CCI (code,name,_,{frame_size = _,num_args = N}),frame),args)
                   => let val () = if ! trace_tailcalls
                                   then echo (op ^ {1 = op ^ {1 = prefixNML,
                                                              2 = spaces (depth'control control)},
                                                    2 = name}) else ()
                          val n_supplied = length args
                      in if op < {1 = n_supplied,2 = N}
                         then let val () = count'pap icounts
                                  val pap = MakePap (Interpret (icounts,Jump),closure,args)
                              in Return (icounts,Jump,control,pap)
                              end else let val args = if op = {1 = n_supplied,2 = N} then args
                                                      else let val (args1,args2) =
                                                                   (splitArgs N) args
                                                               val () = count'reapp icounts
                                                               val () = PushReApp (control,args2)
                                                           in args1
                                                           end
                                       in Jump (control,code,NewEnv (frame,{args = args}))
                                       end
                      end
                and TailCall : ((icounts * jump * control * value * value list) -> res) =
                    fn (icounts,Jump,control,func,args) =>
                         ((count'tail icounts) (length args) ;
                          case exn'value func of
                            Some (V_Closure closure) =>
                              (Enter (icounts,Jump,control,closure,args))
                          | Some (V_Pap (closure,early)) =>
                              (Enter (icounts,Jump,control,closure,
                                      op @ {1 = early,2 = args}))
                          | _ => let val () =
                                         if ! trace_tailcalls
                                         then echo (op ^ {1 = op ^ {1 = op ^ {1 = prefixNML,
                                                                              2 = spaces
                                                                                  (depth'control
                                                                                   control)},
                                                                    2 = "**funcValue:"},
                                                          2 = funcName'value func}) else ()
                                     fun loop v [] = Return (icounts,Jump,control,v)
                                       | loop f (op :: {1 = x,2 = xs}) =
                                       case (apply'value f) x of
                                         This f => ((loop f) xs)
                                       | That exval => (Raise (icounts,Jump,control,
                                                               value'exval exval))
                                 in (loop func) args
                                 end)
val executeIm : ((icounts * env * im) -> bool) =
    fn (icounts,env,im) => ((count'im icounts) im ;
                            case im of
                              IM'MatchScon (r,scon) => (MatchScon (scon,GetLoc (env,r)))
                            | IM'MatchC (r,con) => (MatchC (con,GetLoc (env,r)))
                            | IM'MatchE (r,exname) => (MatchE (exname,GetLoc (env,r)))
                            | IM'MatchG (r1,r2) => (MatchG (GetLoc (env,r1),GetLoc (env,r2))))
val executeInstruction : ((icounts * interpret * control * env * instruction) -> unit) =
    fn (icounts,I,control,env,instruction) =>
         ((count'instruction icounts) instruction ;
          case instruction of
            INS'NewTuple (q,{tuple_size = tuple_size}) =>
              (SetStackLoc (env,q,NewTuple tuple_size))
          | INS'NewException (q,vid) => (SetStackLoc (env,q,NewException vid))
          | INS'NewFn (q,sizes,code,info) => (SetStackLoc (env,q,NewFn (I,sizes,code,info)))
          | INS'NewRef (q,r) => (SetStackLoc (env,q,NewRef (GetLoc (env,r))))
          | INS'NewCon (q,con,r) => (SetStackLoc (env,q,NewCon (con,GetLoc (env,r))))
          | INS'Copy (q,r) => (SetStackLoc (env,q,GetLoc (env,r)))
          | INS'DeCon (q,r) => (SetStackLoc (env,q,DeCon (GetLoc (env,r))))
          | INS'DeExcon (q,r) => (SetStackLoc (env,q,DeExcon (GetLoc (env,r))))
          | INS'DeRef (q,r) => (SetStackLoc (env,q,DeRef (GetLoc (env,r))))
          | INS'DeTuple (q,r,n) => (SetStackLoc (env,q,DeTuple (GetLoc (env,r),n)))
          | INS'BuiltinApp (q,bi,rs) => (SetStackLoc (env,q,BuiltinApp (bi,GetLocs (env,rs))))
          | INS'PushContinuation ({frame_size = frame_size},code,_) =>
              (PushContinuation (control,frame_size,code))
          | INS'PushHandler ({frame_size = frame_size},code,_) =>
              (PushHandler (control,frame_size,code))
          | INS'SetTupleElement (r1,n,r2) => (SetTupleElement (GetLoc (env,r1),n,GetLoc (env,r2)))
          | INS'SetFrameElement (r1,n,r2) => (SetFrameElement (GetLoc (env,r1),n,GetLoc (env,r2)))
          | INS'SetContFrameElem (n,r2) => (SetContFrameElement (control,n,GetLoc (env,r2)))
          | INS'SetXcontFrameElem (n,r2) => (SetXcontFrameElement (control,n,GetLoc (env,r2))))
val executeCode : ((icounts * jump * control * env * code) -> res) =
    fn (icounts,J,control,env,code) =>
         let fun doCode (code,goto) =
               ((count'code icounts) code ;
                case code of
                  CODE'Seq (ins,code) => (executeInstruction (icounts,Interpret (icounts,J),control,
                                                              env,ins) ;doCode (code,goto))
                | CODE'Return r => (Return (icounts,J,control,GetLoc (env,r)))
                | CODE'TailCall (func,args) => (TailCall (icounts,J,control,GetLoc (env,func),
                                                          GetLocs (env,args)))
                | CODE'Raise r => (Raise (icounts,J,control,GetLoc (env,r)))
                | CODE'If (im,code1,code2) => (if executeIm (icounts,env,im)
                                               then doCode (code1,goto) else doCode (code2,goto))
                | CODE'Where (code1,L,code2) =>
                    let fun goto' lab = if op = {1 = lab,2 = L} then doCode (code2,goto)
                                        else goto lab
                    in doCode (code1,goto')
                    end
                | CODE'Goto lab => (goto lab))
             fun goto lab = abort (op ^ {1 = "goto/",2 = sof'label lab})
         in doCode (code,goto)
         end
fun echo'lay lay x = ((prefixed_output_layout prefixNML) ((squash_to wrapCol) (lay x)) ;echo "")
fun eval'code code = let val icounts = new'icounts ()
                         fun Jump (control,code,env) = executeCode (icounts,Jump,control,env,code)
                         val control = newControl ()
                         val env = NewEnv (NewFrame 0,{args = []})
                         val res = Jump (control,code,env)
                         val () = if ! display_counts then (echo'lay lay'icounts) icounts else ()
                     in res
                     end
end : MachineSig
;

