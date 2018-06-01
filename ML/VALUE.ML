(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  31.    export
  187.   imports
  306.   functor
  334.   con - type for datatype constructor, which know their siblings
  373.   outstream_descriptor
  386.   value -- types
  426.   unique int built into each value - to break cycles, discover sharing
  439.   value -- builders
  465.   mutable tuples
  484.   unit
  493.   pair, trip
  527.   builtin
  554.   ColonEq (:=)
  570.   prim
  598.   value -- builders
  656.   value -- layout (raw)
  739.   value -- conversions from (for old MACHINE)
  768.   value -- conversion from LIST
  798.   value -- layout
  833.   value -- conversions from
  932.   value -- eq
  1021.  lessNumTxt
  1037.  value -- apply
  1083.  export -> ccode
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature ValueSig =
sig
    type vid
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

    datatype effects = EFF'Pure | EFF'Impure | EFF'SideEffects | EFF'VarAlloc

    type builtin

    val builtin'fn      : string * effects * int * (value -> value) -> builtin
    val name'builtin    : builtin -> string
    val arity'builtin   : builtin -> int
    val apply'builtin   : builtin -> value list -> value
    val value'builtin   : builtin -> value
    val builtin'value   : value -> builtin option

    type con
    (*val makeCons      : vid list -> con list*)
    val makeCons        : (vid * int) list -> con list
    val vid'con         : con -> vid
    val eq'con          : con -> con -> bool
    val siblings'con    : con -> con list
    val sof'con         : con -> string
    val n'con           : con -> int

    datatype prim
      = PRIM'Builtin    of builtin
      | PRIM'Con0       of con
      | PRIM'Ref

    val name'prim       : prim -> string
    val effects'prim    : prim -> effects
    val prim'value      : value -> prim option

    datatype instream_descriptor = ISD'stdIn | ISD'File of string
    datatype outstream_descriptor = OSD'stdOut | OSD'stdErr | OSD'File of string

    val value'instream  : instream_descriptor * TextIO.instream -> value
    val instream'value  : string -> value -> TextIO.instream

    val value'outstream : outstream_descriptor * TextIO.outstream -> value
    val outstream'value : string -> value -> TextIO.outstream


    val value'null      : string -> value

    val value'exn       : string * exn
                          * (unit -> layout)
                          * (unit -> cid * ((value -> cexp) * (cdef -> unit) * (cact -> unit) -> cexp))
                          * (value -> (value,exval) either) -> value

    val exn'value       : value -> exn option

    val value'newTuple  : string -> int -> value
    val setTuple'value  : string -> value * int * value -> unit
    val value'getTuple  : string -> value * int -> value

    val apply'value     : value -> value -> (value,exval) either
    val funcName'value  : value -> string

    val value'assignOp  : value
    val value'scon      : scon -> value
    val value'ref       : value
    val value'con0      : con -> value
    val value'con1      : con * value -> value
    val value'exval     : exval -> value
    val value'closure   : closure -> value
    val value'addr      : value ref -> value
    val value'vector    : value vector -> value
    val value'array     : value array -> value

    val gen'exname      : vid -> exname
    val vid'exname      : exname -> vid
    val n'exname        : exname -> int
    val closure'fn      : (value -> (value,exval) either) -> closure

    val vid'it          : vid
    val vid'nil         : vid
    val vid'cons        : vid

    val exval'con0      : exname -> exval
    val exval'Match     : exval
    val exval'Match1    : value -> exval
    val exval'Bind      : exval

    val value'unit      : value
    val value'char      : char -> value
    val value'int       : int -> value
    val value'word      : word -> value
    val value'string    : string -> value
    val value'true      : value
    val value'false     : value
    val value'bool      : bool -> value
    val value'pair      : value * value -> value
    val value'trip      : value * value * value -> value
    val value'list      : value list -> value

    val exval'value     : string -> value -> exval
    val scon'value      : string -> value -> scon

    val con0'value      : string -> value -> con
    val excon0'value    : string -> value -> exname
    val ref_or_con0'value : string -> value -> ({Ref:unit},{Con0:con}) either

    val con'value       : string -> value -> (con * value option)
    val excon'value     : string -> value -> (exname * value option)
    val derefAddr'value : string -> value -> value

    val char'value      : string -> value -> char
    val int'value       : string -> value -> int
    val word'value      : string -> value -> word
    val string'value    : string -> value -> string
    val bool'value      : string -> value -> bool
    val unit'value      : string -> value -> unit
    val pair'value      : string -> value -> value * value
    val trip'value      : string -> value -> value * value * value
    val listCons'value  : string -> value -> (value * value) option
    val list'value      : string -> value -> value list
    val vector'value    : string -> value -> value vector
    val array'value     : string -> value -> value array

    val lessNumTxt      : value * value -> bool (* overloaded for int,word,char,string *)

    val eq'scon         : scon -> scon -> bool
    val eq'exname       : exname -> exname -> bool
    val eq'value        : value -> value -> bool

    val lay'value       : value -> layout

    val decompile       : value -> unit

    (*val exportC           : value -> unit
    val exportC'        : string * value -> unit (* writeto file *)*)

    (*val cc'value      : cdef list -> value -> cdef list * cexp*)

    val exportC         : TextIO.outstream -> value -> unit

end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ~/project/prelude/PREL.ML *)
signature PrelSig_Import =
sig
    val echo            : string -> unit
    val abort           : string -> 'a
    val fold            : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    val enumerateFrom   : int -> 'a list -> ('a * int) list
    val upto            : int * int -> int list
    val sofStringLit    : string -> string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: MisclaySig ~/project/prelude/MISCLAY.ML *)
signature MisclaySig_Import =
sig
  type layout
  val Word                      : string -> layout
  val Left                      : string -> layout
  val Right                     : string -> layout
  val squash_to                 : int -> layout -> layout
  val show_layout               : layout -> string
  val stream_layout             : TextIO.outstream -> layout -> unit
  val prefixed_output_layout    : string -> layout -> unit
  val //                        : layout * layout -> layout
  val @@                        : layout * layout -> layout
  val ++                        : layout * layout -> layout
  val ``                        : layout * layout -> layout
  val //``                      : layout * layout -> layout
  val layWord                   : word -> layout
  val layInt                    : int -> layout
  val layBracket                : layout -> layout
  val laySqBracket              : layout -> layout
  val layCommaSep               : layout list -> layout
  val BlankLine                 : layout
end;
(*IMPORTDONE:*)

(*IMPORTSIG: ImpHashSig ~/project/prelude/IMP_HASH.ML *)
signature ImpHashSig_Import =
sig
    type layout
    type ('k,'v) hashTable
    val mkEmptyHT   : {eq   : 'k -> 'k -> bool,
                       hash : 'k -> int} -> ('k,'v) hashTable
    val delHT       : ('k,'v) hashTable -> 'k -> {hit : bool}
    val updateHT        : ('a,'b) hashTable -> 'a * 'b -> unit
    val lookHT          : ('a, 'b) hashTable -> 'a -> 'b option
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AtomSig ATOM.ML *)
signature AtomSig_Import =
sig
    datatype id = ID of string
    datatype vid = VID of id
    datatype lab = LAB'Id of id | LAB'Num of int
    datatype scon =
        SCON'Num of int
      | SCON'Word of word
      | SCON'Real of real
      | SCON'Char of char
      | SCON'String of string
    val sof'vid : vid -> string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: CcodeSig CCODE.ML *)
signature CcodeSig_Import =
sig
    type cid
    type ctype
    type ccall
    type cexp
    type cact
    type cstat
    type cdef
    val makeCid                     : string -> cid
    val ccall                       : cid * cexp list -> ccall
    val cdef'FuncDef                : ctype * cid * (ctype * cid) list * cstat -> cdef
    val cdef'VarDec                 : ctype * cid -> cdef
    val cdef'VarDef                 : ctype * cid * cexp -> cdef
    val cexp'FunCall                : ccall -> cexp
    val cexp'Id                     : cid -> cexp
    val cact'Call                   : ccall -> cact
    val cstat'DefThen               : ctype * cid * cexp * cstat -> cstat
    val cstat'ActThen               : cact * cstat -> cstat
    val cstat'Return                : cexp -> cstat
    val newCid                      : string -> cid
    val ctype'Nword                 : ctype
    val cexp'null                   : cexp
    val cexp'unit                   : cexp
    val cexp'colonEq                : cexp
    val cexp'ref0                   : cexp
    val cexp'con0                   : {tag:int, arity:int} -> cexp
    val cexp'UnappliedBuiltin       : int * string -> cexp
    val cexp'num                    : int -> cexp
    val cexp'word                   : word -> cexp
    val cexp'char                   : char -> cexp
    val cexp'string                 : string -> cexp
    val cexp'exname0                : string -> int -> cexp
    val cexp'exname1                : string -> int -> cexp -> cexp
    val cexp'NewArray               : int -> cexp
    val cexp'NewTup                 : int -> cexp
    val cexp'EmptyRef               : cexp
    val ccall'FixupRef              : cexp -> cexp -> ccall
    val cexp'NewCon                 : int -> cexp -> cexp
    val ccall'SetArrayElement       : cexp -> int -> cexp -> ccall
    val ccall'SetTupleElement       : cexp -> int -> cexp -> ccall
    val cexp'stdOut                   : cexp
    val cexp'stdErr                   : cexp

    type layout
    val layCdefs        : cdef list -> layout
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor VALUE(val prefixNML : string
              structure Prel : PrelSig_Import
              (*structure Sort : SortSig_Import
              structure Assoc : AssocSig_Import*)
              structure Misclay : MisclaySig_Import
              structure Ccode : CcodeSig_Import
              structure ImpHash : ImpHashSig_Import
              val wrapCol : int
              structure Atom : AtomSig_Import
              sharing type Misclay.layout = Ccode.layout = ImpHash.layout
                                 ) : ValueSig =
struct

  infix // @@ ++ `` //``
  open Prel (*Sort Assoc*) Misclay Atom Ccode ImpHash
  fun error s = abort ("error: VALUE: "^s)

  fun nth _ [] = abort "nth/[]"
    | nth n (x::xs) = if (n=0) then x else nth (n-1) xs (* interesting for opt: xs only used in one arm! *)


  fun list'array array =
      map (fn n => Array.sub (array,n)) (upto(0,Array.length array - 1))

(*----------------------------------------------------------------------
INDEX: con - type for datatype constructor, which know their siblings
----------------------------------------------------------------------*)
(*
 (*dummy implementation*)
  datatype con = CON of vid
  fun makeCons xs = map CON xs
  val vid'con = fn CON x => x
  fun eq'con (CON x1) (CON x2) = eq'vid x1 x2
  fun siblings'con con = abort "siblings'con"
*)

(*
  datatype con = CON of int * vid list
  fun vid'con (CON(i,names)) = nth i names
  fun makeCons names = map (fn i => CON (i,names)) (upto (0,length names - 1))
  fun eq'con (CON (n1,_)) (CON (n2,_)) = (n1=n2)
  fun siblings'con (CON(_,names)) = makeCons names
*)
(*
  fun n'con con =
      case (find'assoc (assoc'list (enumerateFrom 0 (siblings'con con))) (eq'con con))
       of None => abort "n'con"
        | Some n => n
*)

  datatype con = CON of {tag:int} * {arity:int} * vid * (vid*int) list
  fun vid'con (CON({tag},{arity},vid,names)) = vid
  fun makeCons nnames = map (fn ((vid,arity),tag) => CON ({tag=tag},{arity=arity},vid,nnames)) (enumerateFrom 0 nnames)
  fun eq'con (CON ({tag=n1},_,_,_)) (CON ({tag=n2},_,_,_)) = (n1=n2)
  fun siblings'con (CON(_,_,_,nnames)) = makeCons nnames

  fun arity'con (CON(_,{arity},_,_)) = arity
  fun n'con (CON({tag},_,_,_)) = tag


  fun sof'con con = sof'vid (vid'con con)
  (*fun span'con con = length (siblings'con con)*)

(*----------------------------------------------------------------------
INDEX: outstream_descriptor
----------------------------------------------------------------------*)

  datatype instream_descriptor = ISD'stdIn | ISD'File of string
  datatype outstream_descriptor = OSD'stdOut | OSD'stdErr | OSD'File of string

  fun cc'outstream_descriptor desc =
      case desc
       of OSD'stdOut => cexp'stdOut
        | OSD'stdErr => cexp'stdErr
        | OSD'File s => abort ("cc/OSD'File/"^s)

(*----------------------------------------------------------------------
INDEX: value -- types
----------------------------------------------------------------------*)

  datatype exname = EXNAME of {base : vid, unique : int}

  datatype effects = EFF'Pure | EFF'Impure | EFF'SideEffects | EFF'VarAlloc

  datatype builtin = BUILTIN of string * effects * int * (value -> value)

  and value = VALUE of int * valueform

  and valueform
    = VALUE'Unit
    | VALUE'ColonEq
    | VALUE'Ref         (* ref *)
    | VALUE'Null        of string
    | VALUE'Exn         of string * exn
                           * (unit -> layout)
                           * (unit -> cid * ((value -> cexp) * (cdef -> unit) * (cact -> unit) -> cexp))
                           * (value -> (value,exval) either)
    | VALUE'Tuple       of value array
    | VALUE'Special     of scon
    | VALUE'Builtin     of builtin
    | VALUE'Con0        of con
    | VALUE'Con1        of con * value
    | VALUE'Exval       of exval
    | VALUE'Addr        of value ref
    | VALUE'Closure     of closure
    | VALUE'Vector      of value vector
    | VALUE'Array       of value array
    | VALUE'Outstream   of outstream_descriptor * TextIO.outstream
    | VALUE'Instream    of instream_descriptor * TextIO.instream

  and exval =
      EXVAL'Con0 of exname
    | EXVAL'Con1 of exname * value

  and closure = CLOSURE of value -> (value,exval) either

(*----------------------------------------------------------------------
INDEX: unique int built into each value - to break cycles, discover sharing
----------------------------------------------------------------------*)

  local val U = ref 1
  in fun newValue form = let val n = !U
                         in U := 1+n; VALUE (n, form)
                         end
  end

  fun unique'value (VALUE (n,_)) = n
  fun form'value (VALUE (_,form)) = form

(*----------------------------------------------------------------------
INDEX: value -- builders
----------------------------------------------------------------------*)

  val value'assignOp    = newValue VALUE'ColonEq
  val value'unit        = newValue VALUE'Unit
  val value'ref         = newValue VALUE'Ref

  val value'null        = newValue o VALUE'Null
  val value'exn         = newValue o VALUE'Exn
  val value'tuple       = newValue o VALUE'Tuple
  val value'scon        = newValue o VALUE'Special
  val value'builtin     = newValue o VALUE'Builtin
  val value'con0        = newValue o VALUE'Con0
  val value'con1        = newValue o VALUE'Con1
  val value'exval       = newValue o VALUE'Exval
  val value'addr        = newValue o VALUE'Addr
  val value'closure     = newValue o VALUE'Closure
  val value'vector      = newValue o VALUE'Vector
  val value'array       = newValue o VALUE'Array
  val value'outstream   = newValue o VALUE'Outstream
  val value'instream    = newValue o VALUE'Instream


  val closure'fn = CLOSURE

(*----------------------------------------------------------------------
INDEX: builtin
----------------------------------------------------------------------*)

  val builtin'fn = BUILTIN

  fun name'builtin (BUILTIN (name,_,_,_)) = name
  fun arity'builtin (BUILTIN (_,_,arity,_)) = arity
  fun effects'builtin (BUILTIN (_,eff,_,_)) = eff

  fun builtin'value v =
      case (form'value v)
       of VALUE'Builtin p => Some p
        | _ => None

(*----------------------------------------------------------------------
INDEX: value -- layout (raw)
----------------------------------------------------------------------*)

  fun notYet s = Left "<..." @@ Word s @@ Right "...>"

  fun sofCharLit c = "#" ^ sofStringLit (implode [c])

  fun lay'scon scon = (* duplicated from PRETTY *)
      case scon of
          SCON'Num n => layInt n
        | SCON'Word w => layWord w
        | SCON'Real r => notYet "real"
        | SCON'Char c => Word (sofCharLit c)
        | SCON'String s => Word (sofStringLit s)

  fun lay'vid (VID (ID s)) = (*Word "op" ++ *) Word s

  fun lay'con con = Word (sof'con con)

  fun lay'exname (EXNAME {base=vid,unique=_}) = lay'vid vid
  (*fun lay'exname (EXNAME {base=vid,unique}) = lay'vid vid*) (* avoid wildcard *)


  fun needAtomic isAtomic layAtomic x =
      (if (isAtomic x) then (fn x => x) else layBracket)
      (layAtomic x)

  fun isAtomic'exval exval =
      case exval of
          EXVAL'Con0 _ => true
        | EXVAL'Con1 _ => false

  fun isAtomic'value value =
      case (form'value value) of
          VALUE'Null _                  => true
        | VALUE'Unit                    => true
        | VALUE'Exn _                   => true
        | VALUE'Tuple _                 => true
        | VALUE'ColonEq                 => true
        | VALUE'Special scon            => true
        | VALUE'Builtin bi              => true
        | VALUE'Ref                     => true
        | VALUE'Con0 _                  => true
        | VALUE'Con1 _                  => false
        | VALUE'Exval exval             => isAtomic'exval exval
        | VALUE'Addr vr                 => false
        | VALUE'Vector _                => true
        | VALUE'Array _                 => true
        | VALUE'Outstream _             => true
        | VALUE'Instream _              => true
        | VALUE'Closure closure         => true

  val atomic'value = needAtomic isAtomic'value

  fun lay'exval V exval =
      case exval of
          EXVAL'Con0 exname => lay'exname exname
        | EXVAL'Con1 (exname,value) => lay'exname exname `` layBracket (V value)

  fun lay'tuple V array =
      layBracket (layCommaSep (map V (list'array array)))

  fun U_raw_lay'value X V v =
      case (form'value v)
       of VALUE'Null who                => Word ("<null:"^who^">")
        | VALUE'Unit                    => Word "()"
        | VALUE'Exn x                   => X x
        | VALUE'Tuple array             => lay'tuple V array
        | VALUE'ColonEq                 => Word ":="
        | VALUE'Special scon            => lay'scon scon
        | VALUE'Builtin bi              => Word ("<builtin:"^name'builtin bi^">")
        | VALUE'Ref                     => Word "ref"
        | VALUE'Con0 con                => lay'con con
        | VALUE'Con1 (con,value)        => lay'con con `` atomic'value V value
        | VALUE'Exval exval             => lay'exval V exval
        | VALUE'Addr vr                 => Word "ref" `` atomic'value V (!vr)
        | VALUE'Vector _                => Word "<vector>"
        | VALUE'Array _                 => Word "<array>"
        | VALUE'Outstream _             => Word "<outstream>"
        | VALUE'Instream _              => Word "<instream>"
        | VALUE'Closure closure         => Word "<interpreted-closure>"


  fun X_raw_lay'value X v = U_raw_lay'value X (X_raw_lay'value X) v

  val raw_lay'value = X_raw_lay'value (fn (name,_,_,_,_) => Word ("<"^name^">"))

  fun raw_sof'value v = show_layout (raw_lay'value v)

  fun raw_errorV v s = error (s ^ " - " ^ raw_sof'value v)

(*----------------------------------------------------------------------
INDEX: mutable tuples
----------------------------------------------------------------------*)

  fun value'newTuple who 0 = error (who^":value'newTuple:0")
(*  | value'newTuple 1 = error "value'newTuple:1"*)
    | value'newTuple who N =
      value'tuple (Array.array(N,value'null (who^":value'newTuple")))

  fun setTuple'value who (tup,n,v) =
      case (form'value tup)
       of VALUE'Tuple A => Array.update(A,n,v)
        | _ => error (who^":setTuple'value")

  fun value'getTuple who (tup,n) =
      case (form'value tup)
       of VALUE'Tuple A => Array.sub(A,n)
        | _ => raw_errorV tup (who^":value'getTuple")

(*----------------------------------------------------------------------
INDEX: unit
----------------------------------------------------------------------*)

  fun unit'value who v =
      case (form'value v) of
          VALUE'Unit => ()
        | _ => error "unit'value"

(*----------------------------------------------------------------------
INDEX: pair, trip
----------------------------------------------------------------------*)

  fun value'pair (v0,v1) =
      let val T = value'newTuple "value'pair" 2
          val () = setTuple'value "value'pair:0" (T,0,v0)
          val () = setTuple'value "value'pair:1" (T,1,v1)
      in T
      end

  fun pair'value who v =
      case (form'value v)
       of VALUE'Tuple A => if Array.length(A) <> 2
                           then error (who^"pair'value:[size<>2]")
                           else (Array.sub(A,0),Array.sub(A,1))
        | _ => error (who^"pair'value")


  fun value'trip (v0,v1,v2) =
      let val T = value'newTuple "value'trip" 3
          val () = setTuple'value "value'trip:0" (T,0,v0)
          val () = setTuple'value "value'trip:1" (T,1,v1)
          val () = setTuple'value "value'trip:2" (T,2,v2)
      in T
      end

  fun trip'value who v =
      case (form'value v)
       of VALUE'Tuple A => if Array.length(A) <> 3
                           then error (who^"trip'value:[size<>3]")
                           else (Array.sub(A,0),Array.sub(A,1),Array.sub(A,2))
        | _ => error (who^"trip'value")

(*----------------------------------------------------------------------
INDEX: builtin / apply
----------------------------------------------------------------------*)

  fun value'tuple [] = value'unit
    | value'tuple [v] = abort "value'tuple/1"
    | value'tuple vs = let val tup = value'newTuple "value'tuple" (length vs)
                           val _:unit list = map (fn (v,n) => setTuple'value "value'tuple" (tup,n,v)) (enumerateFrom 0 vs)
                       in tup
                       end

  fun apply'builtin1 (BUILTIN (_,_,_,f)) v = f v (* no arity checking *)

  fun apply'builtin (BUILTIN (_,_,1,f)) [v] = f v
    | apply'builtin (BUILTIN (_,_,n,f)) vs = if (length vs <> n) then abort "apply'builtin/arity-mismatch" else f (value'tuple vs)

(*----------------------------------------------------------------------
INDEX: ColonEq (:=)
----------------------------------------------------------------------*)

  fun apply'coloneq vArg =
      let
          val (v1,v2) = pair'value "apply/:=" vArg
          val (U:unit) = case (form'value v1) of
                             VALUE'Addr addr => (addr := v2)
                           | _ => error "apply:(:=)[arg1 not an address]"
      in value'unit
      end

    (*val Builtin'ColonEq = builtin'fn ("(:=)",EFF'SideEffects,2,apply'coloneq)*)
    val Builtin'ColonEq = builtin'fn (":=",EFF'SideEffects,2,apply'coloneq)

(*----------------------------------------------------------------------
INDEX: prim
----------------------------------------------------------------------*)

    datatype prim
      = PRIM'Builtin        of builtin
      | PRIM'Con0           of con
      | PRIM'Ref

    fun prim'value v =
        case (form'value v)
         of VALUE'Builtin bi => Some (PRIM'Builtin bi)
          | VALUE'Con0 con   => Some (PRIM'Con0 con)
          | VALUE'Ref        => Some PRIM'Ref
          | VALUE'ColonEq    => Some (PRIM'Builtin Builtin'ColonEq)
          | _                => None

    val name'prim =
        fn PRIM'Builtin b   => name'builtin b
         | PRIM'Con0 con    => sof'con con
         | PRIM'Ref         => "ref"


    val effects'prim =
        fn PRIM'Builtin b   => effects'builtin b
         | PRIM'Con0 con    => EFF'Pure
         | PRIM'Ref         => EFF'Impure

(*----------------------------------------------------------------------
INDEX: value -- builders
----------------------------------------------------------------------*)

  local val U = ref 1 (* would a better implementation use a ref ? *)
  in fun gen'exname base = let val n = !U
                           in (U:=1+n; EXNAME {base=base,unique=n})
                           end
  end

  fun vid'exname (EXNAME {base,unique=_}) = base
  fun n'exname (EXNAME {base=vid,unique=u}) = u


  val vid'it = VID (ID "it")
  val vid'nil = VID (ID "nil")
  val vid'cons = VID (ID "::")
  val vid'true = VID (ID "true")
  val vid'false = VID (ID "false")


  fun isTrue'con con = sof'con con = "true"
  fun isFalse'con con = sof'con con = "false"
  fun isCons'con con = sof'con con = "::"
  fun isNil'con con = sof'con con = "nil"


  val exval'con0 = EXVAL'Con0

  val exval'Match = EXVAL'Con0 (gen'exname (VID (ID "Match")))
  val exval'Bind = EXVAL'Con0 (gen'exname (VID (ID "Bind")))

  val exval'Match1 = let val exname = gen'exname (VID (ID "Match1"))
                     in fn v => EXVAL'Con1 (exname,v)
                     end


  fun value'char x = value'scon (SCON'Char x)
  fun value'int x = value'scon (SCON'Num x)
  fun value'word x = value'scon (SCON'Word x)
  fun value'string x = value'scon (SCON'String x)


  val (con'true,con'false) = case (makeCons [(vid'true,0),(vid'false,0)]) of [a,b] => (a,b) | _ => abort "makeCons/true,false"
  val (con'cons,con'nil) = case (makeCons [(vid'cons,2),(vid'nil,0)]) of [a,b] => (a,b) | _ => abort "makeCons/cons,nil"


  val value'true = value'con0 con'true
  val value'false = value'con0 con'false

  fun value'bool true = value'true
    | value'bool false = value'false

  fun value'list vs =
      case vs of
          [] => value'con0 con'nil
        | v::vs => value'con1 (con'cons, value'pair (v,value'list vs))

(*----------------------------------------------------------------------
INDEX: value -- conversions from (for old MACHINE)
----------------------------------------------------------------------*)

(*
(* for old MACHINE *)
    val ctag'value      : string -> value -> vid
    val xtag'value      : string -> value -> exname
    val cxbody'value    : string -> value -> value

  fun ctag'value who v =
      case (form'value v) of
          VALUE'Con0 vid => vid
        | VALUE'Con1 (vid,_) => vid
        | _ => error (who^":ctag'value")

  fun xtag'value who v =
      case (form'value v) of
          VALUE'Exval (EXVAL'Con0 exname) => exname
        | VALUE'Exval (EXVAL'Con1 (exname,_)) => exname
        | _ => error (who^":xtag'value")

  fun cxbody'value who v =
      case (form'value v) of
          VALUE'Con1 (v_,v) => v
        | VALUE'Exval (EXVAL'Con1 (_,v)) => v
        | _ => error (who^":cxbody'value")
*)

(*----------------------------------------------------------------------
INDEX: value -- conversion from LIST
----------------------------------------------------------------------*)

  fun con'value who v =
      case (form'value v) of
          VALUE'Con0 con => (con,None)
        | VALUE'Con1 (con,v) => (con,Some v)
        | _ => error (*V v*) (who^":con'value")

  fun isList'value v = (* just checks the first constructor *)
      case (form'value v) of
          VALUE'Con0 con => isNil'con con
        | VALUE'Con1 (con,_) => isCons'con con
        | _ => false

  fun listCons'value who v =
      let val (con',vOpt) = con'value (who^": expected con") v
      in
          if (isNil'con con') then
              case vOpt of
                  None => None
                | Some _ => error (who^"nil: unexpected value")
          else if (isCons'con con') then
              case vOpt of
                  None => error (who^"cons: expected value")
                | Some v => Some (pair'value (who^":cons") v)
          else
              error (who^": expected nil/cons")
      end

  fun list'value who v =
      case (listCons'value who v) of
          None => []
        | Some (v1,v2) => v1 :: list'value who v2

(*----------------------------------------------------------------------
INDEX: value -- layout
----------------------------------------------------------------------*)

  fun U_lay'value X V v =
      if (isList'value v)
      then laySqBracket (layCommaSep (map V (list'value "lay'value" v)))
      else U_raw_lay'value X V v


  fun X_lay'value X v = U_lay'value X (X_lay'value X) v



  val decompiling_lay'value = X_lay'value (fn (_,_,LAY,_,_) => LAY())

  val lay'value = X_lay'value (fn (name,_,_,_,_) => Word ("<"^name^">"))

  fun sof'value v = show_layout (lay'value v)

  (*fun raw_lay'value v = U_raw_lay'value raw_lay'value v*)
  fun errorV v s = error (s ^ " - " ^ sof'value v)


  fun echo'layout layout =
      (prefixed_output_layout prefixNML (squash_to wrapCol layout);
       echo "")

  fun echo'lay lay x = echo'layout (lay x)


  fun layTag s lay x = Word s //`` (lay x)

  val decompile = echo'lay (layTag "DECOMPILE..." decompiling_lay'value)

(*----------------------------------------------------------------------
INDEX: value -- conversions from
----------------------------------------------------------------------*)

  fun exn'value v =
      case (form'value v) of VALUE'Exn (_,exn,_,_,_) => Some exn | _ => None

  fun exval'value who v =
      case (form'value v) of
          VALUE'Exval exval => exval
        | _ => error (who^":exval'value")

  fun scon'value who v =
      case (form'value v) of
          VALUE'Special scon => scon
        | _ => error (who^":scon'value")


  fun con0'value who v =
      case (form'value v) of
          VALUE'Con0 con => con
        | _ => error (who^":con0'value")

  fun excon0'value who v =
      case (form'value v) of
          VALUE'Exval (EXVAL'Con0 exname) => exname
        | _ => error (who^":excon0'value")

  fun ref_or_con0'value who v =
      case (form'value v) of
          VALUE'Ref => This {Ref=()}
        | VALUE'Con0 con => That {Con0=con}
        | _ => error (who^":ref_or_con0'value")

  fun excon'value who v =
      case (form'value v) of
          VALUE'Exval (EXVAL'Con0 exname) => (exname,None)
        | VALUE'Exval (EXVAL'Con1 (exname,v)) => (exname,Some v)
        | _ => error (who^":excon'value")

  fun derefAddr'value who v =
      case (form'value v) of
          VALUE'Addr (ref v) => v
        | _ => error (who^":derefAddr'value")


  fun char'value who v =
      case (form'value v) of
          VALUE'Special (SCON'Char x) => x
        | _ => error (who^":char'value")

  fun int'value who v =
      case (form'value v) of
          VALUE'Special (SCON'Num x) => x
        | _ => errorV v (who^":int'value")

  fun word'value who v =
      case (form'value v) of
          VALUE'Special (SCON'Word x) => x
        | _ => error (who^":word'value")

  fun string'value who v =
      case (form'value v) of
          VALUE'Special (SCON'String x) => x
        | _ => error (who^":string'value")

  fun bool'value who v =
      case (form'value v)
       of VALUE'Con0 con => if (isTrue'con con) then true else
                            if (isFalse'con con) then false else
                            abort (who^":bool'value/1")
        | _ => error (who^":bool'value/2")

  fun vector'value who v =
      case (form'value v) of
          VALUE'Vector x => x
        | _ => error (who^":vector'value:")

  fun array'value who v =
      case (form'value v) of
          VALUE'Array x => x
        | _ => error (who^":array'value:")

  fun outstream'value who v =
      case (form'value v) of
          VALUE'Outstream (_,x) => x
        | _ => error (who^":outstream'value:")

  fun instream'value who v =
      case (form'value v) of
          VALUE'Instream (_,x) => x
        | _ => error (who^":instream'value:")

(*----------------------------------------------------------------------
INDEX: value -- eq
----------------------------------------------------------------------*)

  fun lt'id (ID string1) (ID string2) = (string1 < string2)

  fun lt'lab lab1 lab2 =
      case (lab1,lab2) of
          (LAB'Num n1, LAB'Num n2) => (n1<n2)
        | (LAB'Num _, LAB'Id _) => true
        | (LAB'Id _, LAB'Num _) => false
        | (LAB'Id x1, LAB'Id x2) => lt'id x1 x2

  (*fun eq'scon (x1:scon) x2 = (x1=x2)*)
  fun eq'scon scon1 scon2 =
      case (scon1,scon2) of
          (SCON'Num n1,  SCON'Num n2)  => (n1=n2)
        | (SCON'Word w1, SCON'Word w2) => (w1=w2)
        | (SCON'Real r1, SCON'Real r2) => error "eq'scon:real/real"
        | (SCON'Char c1, SCON'Char c2) => (c1=c2)
        | (SCON'String s1, SCON'String s2) => (s1=s2)
        | _ => error "eq'scon"

  fun eq'exname (x1:exname) x2 = (x1=x2)

  fun eq'exval eq'value exval1 exval2 =
      case (exval1,exval2) of
          (EXVAL'Con0 vid1, EXVAL'Con0 vid2) => eq'exname vid1 vid2
        | (EXVAL'Con1 (vid1,v1), EXVAL'Con1 (vid2,v2)) =>
              (eq'exname vid1 vid2) andalso (eq'value v1 v2)
        | _ => false


  fun eq'tuple eq a1 a2 =
      let val n1 = Array.length(a1)
          val n2 = Array.length(a2)
      in if n1 <> n2 then error "eq'tuple/unequal size" else
         let fun loop n = if n=n1 then true else
                          eq (Array.sub(a1,n)) (Array.sub(a2,n))
                          andalso loop (n+1)
         in loop 0
         end
      end

  fun eq'value v1 v2 =
      case (form'value v1,form'value v2) of
          (VALUE'Special scon1, VALUE'Special scon2)    => eq'scon scon1 scon2
        | (VALUE'Con0 con1, VALUE'Con0 con2)            => eq'con con1 con2
        | (VALUE'Con1 (con1,v1), VALUE'Con1 (con2,v2))  => (eq'con con1 con2) andalso (eq'value v1 v2)
        | (VALUE'Exval exval1, VALUE'Exval exval2)      => eq'exval eq'value exval1 exval2
        | (VALUE'Addr a1, VALUE'Addr a2)                => (a1=a2)
        | (VALUE'Unit,VALUE'Unit)                       => true
        | (VALUE'Tuple a1,VALUE'Tuple a2)               => eq'tuple eq'value a1 a2
        (* any of the following cases indicate a type error *)
        | (VALUE'Unit,_)                                => error "eq'value: Unit"
        | (_,VALUE'Unit)                                => error "eq'value: Unit"
        | (VALUE'Null _,_)                              => error "eq'value: Null"
        | (_,VALUE'Null _)                              => error "eq'value: Null"
        | (VALUE'Tuple _,_)                             => error "eq'value: Tuple"
        | (_,VALUE'Tuple _)                             => error "eq'value: Tuple"
        | (VALUE'Exn _,_)                               => error "eq'value: Exn"
        | (_,VALUE'Exn _)                               => error "eq'value: Exn"
        | (VALUE'ColonEq,_)                             => error "eq'value: ColonEq"
        | (_,VALUE'ColonEq)                             => error "eq'value: ColonEq"
        | (VALUE'Builtin _,_)                           => error "eq'value: Builtin"
        | (_,VALUE'Builtin _)                           => error "eq'value: Builtin"
        | (VALUE'Ref,_)                                 => error "eq'value: Ref"
        | (_,VALUE'Ref)                                 => error "eq'value: Ref"
        | (VALUE'Closure _,_)                           => error "eq'value: Closure"
        | (_,VALUE'Closure _)                           => error "eq'value: Closure"
        | (VALUE'Vector _,_)                            => error "eq'value: Vector"
        | (_,VALUE'Vector _)                            => error "eq'value: Vector"
        | (VALUE'Array _,_)                             => error "eq'value: Array"
        | (_,VALUE'Array _)                             => error "eq'value: Array"
        | (VALUE'Outstream _,_)                         => error "eq'value: Outstream"
        | (_,VALUE'Outstream _)                         => error "eq'value: Outstream"
        | (VALUE'Instream _,_)                          => error "eq'value: Instream"
        | (_,VALUE'Instream _)                          => error "eq'value: Instream"
        (* some catch-all cases also indicate type errors,
           but the evaluator is not obliged to catch them. *)
        | _ => false

  local
      val hash'value = unique'value
      fun eq'value v1 v2 = unique'value v1 = unique'value v2
  in
      val eh'value = {eq = eq'value, hash = hash'value}
  end

(*----------------------------------------------------------------------
INDEX: lessNumTxt
----------------------------------------------------------------------*)

  fun lessNumTxt (v1,v2) =
      case (form'value v1,form'value v2) of
          (VALUE'Special scon1, VALUE'Special scon2)
          => (case (scon1,scon2) of
                  (SCON'Num n1,  SCON'Num n2)  => (n1<n2)
                | (SCON'Word w1, SCON'Word w2) => (w1<w2)
                | (SCON'Real r1, SCON'Real r2) => (r1<r2)
                | (SCON'Char c1, SCON'Char c2) => (c1<c2)
                | (SCON'String s1, SCON'String s2) => (s1<s2)
                | _ => error "lessNumTxt")
        | _ => error "lessNumTxt"

(*----------------------------------------------------------------------
INDEX: value -- apply
----------------------------------------------------------------------*)

  fun apply'value vFun vArg =
      case (form'value vFun) of
          VALUE'Null _                    => error "apply:null"
        | VALUE'Unit                      => error "apply:unit"
        | VALUE'Exn (_,_,_,_,APP)         => APP vArg
        | VALUE'Tuple _                   => error "apply:tuple"
        | VALUE'ColonEq                   => This (apply'coloneq vArg)
        | VALUE'Special _                 => error "apply:scon"
        | VALUE'Builtin bi                => This (apply'builtin1 bi vArg)
        | VALUE'Ref                       => This (value'addr (ref vArg))
        | VALUE'Con0 con                  => This (value'con1 (con,vArg))
        | VALUE'Con1 _                    => error ("apply:Con1 - " ^ sof'value vFun ^ " @ " ^ sof'value vArg)
        | VALUE'Exval (EXVAL'Con0 en)     => This (value'exval (EXVAL'Con1 (en,vArg)))
        | VALUE'Exval (EXVAL'Con1 _)      => error "apply:Exval(Con1)"
        | VALUE'Addr _                    => error "apply:Addr"
        | VALUE'Vector _                  => error "apply:Vector"
        | VALUE'Array _                   => error "apply:Array"
        | VALUE'Outstream _               => error "apply:Outstream"
        | VALUE'Instream _                => error "apply:Instream"
        | VALUE'Closure (CLOSURE f)       => f vArg

  fun funcName'value vFun =
      case (form'value vFun) of
          VALUE'Null _                    => error "funcName:null"
        | VALUE'Unit                      => error "funcName:unit"
        | VALUE'Exn (name,_,_,_,APP)      => "exn-"^name
        | VALUE'Tuple _                   => error "funcName:tuple"
        | VALUE'ColonEq                   => ":="
        | VALUE'Special _                 => error "funcName:scon"
        | VALUE'Builtin p                     => "builtin-"^name'builtin p
        | VALUE'Ref                       => "ref"
        | VALUE'Con0 con                  => "con0-"^sof'con con
        | VALUE'Con1 _                    => error "funcName:Con1"
        | VALUE'Exval (EXVAL'Con0 en)     => "excon0-"^sof'vid (vid'exname en)
        | VALUE'Exval (EXVAL'Con1 _)      => error "funcName:Exval(Con1)"
        | VALUE'Addr _                    => error "funcName:Addr"
        | VALUE'Vector _                  => error "funcName:Vector"
        | VALUE'Array _                   => error "funcName:Array"
        | VALUE'Outstream _               => error "funcName:Outstream"
        | VALUE'Instream _                => error "funcName:Instream"
        | VALUE'Closure (CLOSURE f)       => "closure"

(*----------------------------------------------------------------------
INDEX: export -> ccode
----------------------------------------------------------------------*)

  fun cexp'scon scon =
      case scon of
          SCON'Num n                => cexp'num n
        | SCON'Word w               => cexp'word w
        | SCON'Real r               => abort "cexp'scon/real"
        | SCON'Char c               => cexp'char c
        | SCON'String s             => cexp'string s

(*
local
  fun breakCyclesFB (forwardDef,breakExp) Pdoing cc'value =
      let
          fun cc'value_breakCycle defs v =
              if (Pdoing v) then (forwardDef::defs, breakExp) else cc'value defs v
      in cc'value_breakCycle
      end
  fun breakCyclesFunc name =
      let
          val forwardDef = cdef'FuncDec (ctype'Nword,name,[])
          val breakExp = cexp'FunCall (ccall (name,[]))
      in
           breakCyclesFB (forwardDef,breakExp)
      end
  fun breakCyclesVar name =
      let
          val forwardDef = cdef'VarDec (ctype'Nword,name)
          val breakExp = cexp'Id name
      in
           breakCyclesFB (forwardDef,breakExp)
      end
  fun nameStat name (defs,stat) =
      let val def = cdef'FuncDef (ctype'Nword,name,[],stat)
          val callExp = cexp'FunCall (ccall (name,[]))
      in (def::defs, callExp)
      end
  (*fun nameExp name (defs,e) = nameStat name (defs, cstat'Return e)*)
  fun nameExp name (defs,e) =
      let val def = cdef'VarDef (ctype'Nword,name,e)
      in (def::defs, cexp'Id name)
      end
  fun cexp'value ccV EmitDef v =
      let val (defs,e) = ccV v
          val (_:unit list ) = map EmitDef (rev defs)
      in e
      end
  fun cc'tuple cc'value defs array =
      let val (defs,es) =
              fold (fn (defs,es) => fn v => let val (defs,e) = cc'value defs v
                                            in (defs,e::es)
                                            end) (defs,[]) (rev (list'array array))
          val body = cstat'tuple es
      in (defs,body)
      end
  fun cc'exval cc'value defs exval =
      case exval
       of EXVAL'Con0 (EXNAME {base=vid,unique=u})   => (defs, cexp'exname0 (sof'vid vid) u)
        | EXVAL'Con1 (EXNAME {base=vid,unique=u},v) => let val (defs,e) = (cc'value defs v)
                                                       in (defs, cexp'exname1 (sof'vid vid) u e)
                                                       end
in
  fun OLD_cc'form doingP cc'value_mayCycle defs vform =
      case vform
       of VALUE'Null _              => nameExp (newCid "null") (defs, cexp'null)
        | VALUE'Unit                => (*sconnameExp (newCid "unit")*) (defs, cexp'unit)
        | VALUE'Exn (_,_,_,CC,_)    => let (*val name = newCid "v"*)
                                           val (name,Stage2) = CC()
                                           val cc'value = breakCyclesFunc name doingP cc'value_mayCycle
                                           val emitted = ref []
                                           fun EmitDef def =  (emitted := def :: !emitted)
                                           fun V v = cexp'value (cc'value []) EmitDef v
                                           val e = Stage2 (V,EmitDef)
                                           val defs1 = !emitted
                                       in (*nameExp name*) (defs1 @ defs, e)
                                       end
        | VALUE'Tuple array         => let val name = newCid "tuple"
                                           val cc'value = breakCyclesFunc name doingP cc'value_mayCycle
                                       in nameStat name (cc'tuple cc'value defs array)
                                       end
        | VALUE'ColonEq             => (*nameExp (newCid "v")*) (defs, cexp'colonEq)
        | VALUE'Special scon        => nameExp (newCid "scon") (defs, cexp'scon scon)
        | VALUE'Builtin bi          => (*nameExp (newCid "v")*) (defs, cexp'UnappliedBuiltin (arity'builtin bi,name'builtin bi))
        | VALUE'Ref                 => (*nameExp (newCid "v")*) (defs, cexp'ref0)
        | VALUE'Con0 con            => nameExp (newCid ("con_"^sof'con con)) (defs, cexp'con0 {tag = n'con con,
                                                                                               arity = arity'con con})
        | VALUE'Con1 (con,v)        => let val name = newCid "constructed"
                                           val cc'value = breakCyclesFunc name doingP cc'value_mayCycle
                                           val (defs,e) = (cc'value defs v)
                                       in nameExp name (defs, cexp'NewCon (n'con con) e)
                                       end
        | VALUE'Exval exval         => let val name = newCid "exval"
                                           val cc'value = breakCyclesFunc name doingP cc'value_mayCycle
                                           val (defs,e) = (cc'exval cc'value defs exval)
                                       in nameExp name (defs, e)
                                       end
        | VALUE'Addr (ref v)        => let val name = newCid "addr"
                                           val cc'value = breakCyclesVar name doingP cc'value_mayCycle
                                           val (defs,e) = (cc'value defs v)
                                       in nameExp name (defs, cexp'NewRef e)
                                       end
        | VALUE'Vector _            => abort "cc'form"
        | VALUE'Array _             => abort "cc'form"
        | VALUE'Outstream (desc,_)  => (defs, cc'outstream_descriptor desc)
        | VALUE'Instream _          => abort "cc'form/instream"
        | VALUE'Closure closure     => abort "cc'form/closure"
end
*)

  fun NEW_cc'exval V exval =
      case exval
       of EXVAL'Con0 (EXNAME {base=vid,unique=u})   => cexp'exname0 (sof'vid vid) u
        | EXVAL'Con1 (EXNAME {base=vid,unique=u},v) => cexp'exname1 (sof'vid vid) u (V v)


(*
local
  fun breakCyclesFB (forwardDef,breakExp) EmitDef Pdoing V =
      let
          fun V_breakCycle v =
              if (Pdoing v) then (EmitDef forwardDef; breakExp) else V v
      in V_breakCycle
      end
  fun breakCyclesFunc name =
      let
          val forwardDef = cdef'FuncDec (ctype'Nword,name,[])
          val breakExp = cexp'FunCall (ccall (name,[]))
      in
           breakCyclesFB (forwardDef,breakExp)
      end
  fun breakCyclesVar name =
      let
          val forwardDef = cdef'VarDec (ctype'Nword,name)
          val breakExp = cexp'Id name
      in
           breakCyclesFB (forwardDef,breakExp)
      end
  fun nameStat EmitDef name stat =
      let val def = cdef'FuncDef (ctype'Nword,name,[],stat)
          val callExp = cexp'FunCall (ccall (name,[]))
          val () = EmitDef def
      in callExp
      end
  fun nameExp EmitDef name e =
      let val def = cdef'VarDef (ctype'Nword,name,e)
          val () = EmitDef def
      in cexp'Id name
      end
in
  fun OLD_cc'form EmitDef doingP V_mayCycle vform =
      case vform
       of VALUE'Null _              => nameExp EmitDef (newCid "null") (cexp'null)
        | VALUE'Unit                => (*sconnameExp (newCid "unit")*) (cexp'unit)
        | VALUE'Exn (_,_,_,CC,_)    => let (*val name = newCid "v"*)
                                           val (name,Stage2) = CC()
                                           val V = breakCyclesFunc name EmitDef doingP V_mayCycle
                                           val e = Stage2 (V,EmitDef)
                                       in (*nameExp EmitDef name*) e
                                       end
        | VALUE'Tuple array         => let val name = newCid "tuple"
                                           val V = breakCyclesFunc name EmitDef doingP V_mayCycle
                                           val body = cstat'tuple (map V (list'array array))
                                       in nameStat EmitDef name body
                                       end
        | VALUE'ColonEq             => (*nameExp EmitDef (newCid "v")*) (cexp'colonEq)
        | VALUE'Special scon        => nameExp EmitDef (newCid "scon") (cexp'scon scon)
        | VALUE'Builtin bi          => (*nameExp EmitDef (newCid "v")*) (cexp'UnappliedBuiltin (arity'builtin bi,name'builtin bi))
        | VALUE'Ref                 => (*nameExp EmitDef (newCid "v")*) (cexp'ref0)
        | VALUE'Con0 con            => nameExp EmitDef (newCid ("con_"^sof'con con)) (cexp'con0 {tag = n'con con,
                                                                                               arity = arity'con con})
        | VALUE'Con1 (con,v)        => let val name = newCid "constructed"
                                           val V = breakCyclesFunc name EmitDef doingP V_mayCycle
                                       in nameExp EmitDef name (cexp'NewCon (n'con con) (V v))
                                       end
        | VALUE'Exval exval         => let val name = newCid "exval"
                                           val V = breakCyclesFunc name EmitDef doingP V_mayCycle
                                           val e = NEW_cc'exval V exval
                                       in nameExp EmitDef name e
                                       end
        | VALUE'Addr (ref v)        => let val name = newCid "addr"
                                           val V = breakCyclesVar name EmitDef doingP V_mayCycle
                                       in nameExp EmitDef name (cexp'NewRef (V v))
                                       end
        | VALUE'Vector _            => abort "cc'form"
        | VALUE'Array _             => abort "cc'form"
        | VALUE'Outstream (desc,_)  => cc'outstream_descriptor desc
        | VALUE'Instream _          => abort "cc'form/instream"
        | VALUE'Closure closure     => abort "cc'form/closure"
end

  fun OLD_cc'value value =
      let
          val emitted = ref []
          fun EmitDef def =  (emitted := def :: !emitted)
          val memo = mkEmptyHT eh'value
          val doing = mkEmptyHS eh'value
          fun V v =
              case (lookHT memo v) of
                  Some e => e
                | None => let val () = addHS doing v
                              val e = OLD_cc'form EmitDef (memHS doing) V (form'value v)
                              val {hit} = delHS doing v
                              val () = if hit then () else abort "cc'value/!hit!"
                              val () = updateHT memo (v,e)
                          in e
                          end
          val e = V value
          val defs = !emitted
      in (defs,e)
      end
*)


  fun stageExp e = (None, fn _ => e)

  fun nameExp name e =
      let fun Stage2 (_,EmitDef,EmitAct) =
              let val def = cdef'VarDef (ctype'Nword,name,e)
                  val () = EmitDef def
              in cexp'Id name
              end
      in (None, Stage2)
      end

  fun nameExpV name e'V =
      let val breakExp = cexp'Id name
          fun Stage2 (V,EmitDef,EmitAct) =
              let val e = e'V V
                  val def = cdef'VarDef (ctype'Nword,name,e)
                  val () = EmitDef def
              in breakExp
              end
          val forwardDef = cdef'VarDec (ctype'Nword,name)
      in (Some (forwardDef,breakExp), Stage2)
      end


(*
  fun cstat'tuple es =
      let val tup = makeCid "MyTuple"
          val sets = map (fn (e,n) => ccall'SetTupleElement (cexp'Id tup) n e) (enumerateFrom 0 es)
          val return = cstat'Return (cexp'Id tup)
          val body = cstat'DefThen (ctype'Nword, tup, cexp'NewTup (length sets),
                                    fold (fn stat => fn set => cstat'ActThen (cact'Call set,stat)) return (rev sets))
      in body
      end
*)
(*
  fun cstat'tuple es =
      let val tup = makeCid "MyTuple"
          val acts = map (fn (e,n) => cact'Call (ccall'SetTupleElement (cexp'Id tup) n e)) (enumerateFrom 0 es)
          val return = cstat'Return (cexp'Id tup)
          val body = cstat'DefThen (ctype'Nword, tup, cexp'NewTup (length es),
                                    fold (fn stat => fn act => cstat'ActThen (act,stat)) return (rev acts))
      in body
      end

  fun cexp'tuple EmitDef name es =
      let val breakExp = cexp'FunCall (ccall (name,[]))
          val body = cstat'tuple es
          val def = cdef'FuncDef (ctype'Nword,name,[],body)
          val () = EmitDef def
      in breakExp
      end
*)


  fun OLD_cexp'tuple EmitDef name es =
      let val breakExp = cexp'FunCall (ccall (name,[]))
          val tup = makeCid "MyTuple"
          val acts = map (fn (e,n) => cact'Call (ccall'SetTupleElement (cexp'Id tup) n e)) (enumerateFrom 0 es)
          val return = cstat'Return (cexp'Id tup)
          val body = cstat'DefThen (ctype'Nword, tup, cexp'NewTup (length es),
                                    fold (fn stat => fn act => cstat'ActThen (act,stat)) return (rev acts))
          val def = cdef'FuncDef (ctype'Nword,name,[],body)
          val () = EmitDef def
      in breakExp
      end


  fun cexp'tuple EmitDef EmitAct cid es =
      let val acts = map (fn (e,n) => cact'Call (ccall'SetTupleElement (cexp'Id cid) n e)) (enumerateFrom 0 es)
          val (_:unit list) = map EmitAct acts
          val exp = cexp'NewTup (length es)
          val def = cdef'VarDef (ctype'Nword,cid,exp)
          val () = EmitDef def
      in cexp'Id cid
      end

  fun cexp'array EmitDef EmitAct cid es =
      let val acts = map (fn (e,n) => cact'Call (ccall'SetArrayElement (cexp'Id cid) n e)) (enumerateFrom 0 es)
          val (_:unit list) = map EmitAct acts
          val exp = cexp'NewArray (length es)
          val def = cdef'VarDef (ctype'Nword,cid,exp)
          val () = EmitDef def
      in cexp'Id cid
      end


  fun cc'form vform =
      case vform
       of VALUE'Null _              => nameExp (newCid "null") cexp'null
        | VALUE'Unit                => stageExp cexp'unit
(*
        | VALUE'Tuple array         => let val name = newCid "tuple"
                                           val breakExp = cexp'FunCall (ccall (name,[]))
                                           fun Stage2 (V,EmitDef,EmitAct) =
                                               let val body = cstat'tuple (map V (list'array array))
                                                   val def = cdef'FuncDef (ctype'Nword,name,[],body)
                                                   val () = EmitDef def
                                               in breakExp
                                               end
                                           val forwardDef = cdef'FuncDec (ctype'Nword,name,[])
                                       in (Some (forwardDef,breakExp),Stage2)
                                       end
*)
        | VALUE'Tuple array         => let val name = newCid "tuple"
                                           fun Stage2 (V,EmitDef,EmitAct) =
                                               let val es = map V (list'array array)
                                               in cexp'tuple EmitDef EmitAct name es
                                               end
                                           (*val breakExp = cexp'FunCall (ccall (name,[]))
                                           val forwardDef = cdef'FuncDec (ctype'Nword,name,[])*)
                                           val forwardDef = cdef'VarDec (ctype'Nword,name)
                                           val breakExp = cexp'Id name
                                       in (Some (forwardDef,breakExp),Stage2)
                                       end
        | VALUE'Array array         => let val name = newCid "array"
                                           fun Stage2 (V,EmitDef,EmitAct) =
                                               let val es = map V (list'array array)
                                               in cexp'array EmitDef EmitAct name es
                                               end
                                           val forwardDef = cdef'VarDec (ctype'Nword,name)
                                           val breakExp = cexp'Id name
                                       in (Some (forwardDef,breakExp),Stage2)
                                       end
        | VALUE'Exn (_,_,_,CC,_)    => let val (name,Stage2) = CC()
                                           (*val forwardDef = cdef'FuncDec (ctype'Nword,name,[])
                                           val breakExp = cexp'FunCall (ccall (name,[]))*)
                                           val forwardDef = cdef'VarDec (ctype'Nword,name)
                                           val breakExp = cexp'Id name
                                       in (Some (forwardDef,breakExp), Stage2)
                                       end
        | VALUE'ColonEq             => stageExp cexp'colonEq
        | VALUE'Special scon        => nameExp (newCid "scon") (cexp'scon scon)
        | VALUE'Builtin bi          => stageExp (cexp'UnappliedBuiltin (arity'builtin bi,name'builtin bi))
        | VALUE'Ref                 => stageExp cexp'ref0
        | VALUE'Con0 con            => nameExp (newCid ("con_"^sof'con con)) (cexp'con0 {tag = n'con con,
                                                                                         arity = arity'con con})
        | VALUE'Con1 (con,v)        => let val name = newCid "constructed"
                                       in nameExpV name (fn V => cexp'NewCon (n'con con) (V v))
                                       end
        | VALUE'Exval exval         => let val name = newCid "exval"
                                       in nameExpV name (fn V => NEW_cc'exval V exval)
                                       end
(*
        | VALUE'Addr (ref v)        => let val name = newCid "addr"
                                       in nameExpV name (fn V => cexp'NewRef (V v))
                                       end
*)
        | VALUE'Addr (ref v)        => let val name = newCid "addr"
                                           val breakExp = cexp'Id name
                                           fun Stage2 (V,EmitDef,EmitAct) =
                                               let val e = cexp'EmptyRef
                                                   (*val e = cexp'NewRef (V v)*)
                                                   val def = cdef'VarDef (ctype'Nword,name,e)
                                                   val () = EmitDef def
                                                   val act = cact'Call (ccall'FixupRef breakExp (V v))
                                                   val () = EmitAct act
                                               in breakExp
                                               end
                                           val forwardDef = cdef'VarDec (ctype'Nword,name)
                                       in (Some (forwardDef,breakExp), Stage2)
                                       end
        | VALUE'Vector _            => abort "cc'form"
        | VALUE'Outstream (desc,_)  => stageExp (cc'outstream_descriptor desc)
        | VALUE'Instream _          => abort "cc'form/instream"
        | VALUE'Closure closure     => abort "cc'form/closure"


  fun makeInitDef name acts =
      let val return = cstat'Return cexp'unit (* should be void return !*)
          val body = fold (fn stat => fn act => cstat'ActThen (act,stat)) return (rev acts)
          val initDef = cdef'FuncDef (ctype'Nword,name,[],body)
      in initDef
      end

  fun cc'value value =
      let
          val emitted = ref []
          fun EmitDef def =  (emitted := def :: !emitted)
          val acts = ref []
          fun EmitAct act = (acts := act :: !acts)
          val memo = mkEmptyHT eh'value
          val doing = mkEmptyHT eh'value
          fun V v =
              case (lookHT memo v) of
                  Some e => e
                | None => case (lookHT doing v)
                           of Some (forwardDef,breakExp) => let val () = EmitDef forwardDef
                                                                (*val () = updateHT memo (v,breakExp) - avoid dup forwards!*)
                                                            in breakExp
                                                            end
                            | None =>
                              let val (fbo,Stage2) = cc'form (form'value v)
                                  val () = case fbo of None => () | Some fb => updateHT doing (v,fb)
                                  val e = Stage2 (V,EmitDef,EmitAct)
                                  val {hit} = delHT doing v
                                  (*val () = if hit then () else abort "cc'value/!hit!"*)
                                  val () = updateHT memo (v,e)
                              in e
                              end
          val e = V value
          val () = EmitDef (makeInitDef (makeCid "Init") (!acts))
          val defs = !emitted
      in (defs,e)
      end

(*
  fun exportC value =
      let val (defs,e) = cc'value value
          val layout = (layTag "cdefs..." layCdefs (rev defs)) // (layTag "cexp..." layCexp e)
      in echo'layout layout
      end

  fun exportC (filename,value) =
      let val (defs,e) = cc'value value
          val prog = cdef'VarDef (ctype'Nword,makeCid "TheProgram",e)
          val defs = prog :: defs
          val layout =
              Word "#include \"nml_runtime.h\""
              // (layCdefs (rev defs))
              (*// Word "SetProg" ++ layBracket (layCexp e)*)
              // BlankLine
          val os = TextIO.openOut filename
          val () = stream_layout os layout
          val () = TextIO.closeOut os
      in ()
      end
*)

  fun exportC os value =
      let val (defs,e) = cc'value value
          val prog = cdef'VarDef (ctype'Nword,makeCid "TheProgram",e)
          val defs = prog :: defs
          val layout =
              Word "#include \"nml_runtime.C\""
              // (layCdefs (rev defs))
              (*// Word "SetProg" ++ layBracket (layCexp e)*)
              // BlankLine
          val () = stream_layout os layout
      in ()
      end

end
