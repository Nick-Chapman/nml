(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  15.    export
  143.   imports
  175.   functor
  186.   c-code types
  223.   prim build
  256.   convert names to legal C identfifiers
  291.   build
  391.   lay c-code
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature CcodeSig =
sig
    type cid
    type clab
    type ctype
    type ccall
    type cexp
    type cact
    type cstat
    type cdef

    val makeCid                     : string -> cid
    val clab's                      : string -> clab
    val ctype's                     : string -> ctype
    val ccall                       : cid * cexp list -> ccall
    val cdef'FuncDec                : ctype * cid * (ctype * cid) list -> cdef
    val cdef'FuncDef                : ctype * cid * (ctype * cid) list * cstat -> cdef
    val cdef'VarDec                 : ctype * cid -> cdef
    val cdef'VarDef                 : ctype * cid * cexp -> cdef
    val cexp'FunCall                : ccall -> cexp
    val cexp'Id                     : cid -> cexp
    val cexp'IntLit                 : int -> cexp
    val cexp'StringLit              : string -> cexp

    val cact'Assign                 : cid * cexp -> cact
    val cact'Call                   : ccall -> cact

    val cstat'StaticThen            : ctype * cid * cstat -> cstat
    val cstat'DecListThen           : ctype * cid list * cstat -> cstat
    val cstat'DefThen               : ctype * cid * cexp * cstat -> cstat

    val cstat'ActThen               : cact * cstat -> cstat

    val cstat'Goto                  : clab -> cstat
    val cstat'If                    : cexp * cstat * cstat -> cstat
    val cstat'Return                : cexp -> cstat
    val cstat'Where                 : cstat * clab * cstat -> cstat

    val newCid                      : string -> cid
    val ctype'Nword                 : ctype
    val ctype'Ncode                 : ctype
    val ccall's                     : string -> cexp list -> ccall
    val cexp'call                   : string -> cexp list -> cexp
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
    val cexp'NewException           : string -> cexp

    (*val cexp'NewFn                  : {frame_size:int, num_args:int} -> cexp -> cexp*)
    val cexp'MakeSiFn               : {frame_size:int, num_args:int} -> cexp -> cexp
    val cexp'MakeFn                 : cexp -> cexp


    val cexp'NewPap                 : cexp -> {num_early:int, num_late:int} -> cexp
    val cexp'NewRef                 : cexp -> cexp
    val cexp'EmptyRef               : cexp
    val ccall'FixupRef              : cexp -> cexp -> ccall

    val cexp'NewCon                 : int -> cexp -> cexp
    val cexp'Copy                   : cexp -> cexp
    val cexp'DeCon                  : cexp -> cexp
    val cexp'DeExcon                : cexp -> cexp
    val cexp'DeRef                  : cexp -> cexp
    val cexp'DeTuple                : cexp -> int -> cexp
    val cexp'Builtin                : string -> cexp list -> cexp

    val cexp'MakeSiCont             : int -> cid -> cexp
    val cexp'MakeSiHandle           : int -> cid -> cexp
    val ccall'PushContinuation      : cexp -> ccall
    val ccall'PushHandler           : cexp -> ccall

    val ccall'SetArrayElement       : cexp -> int -> cexp -> ccall
    val ccall'SetTupleElement       : cexp -> int -> cexp -> ccall
    val ccall'SetFrameElement       : cexp -> int -> cexp -> ccall
    val ccall'SetContFrameElem      : int -> cexp -> ccall
    val ccall'SetXcontFrameElem     : int -> cexp -> ccall

    val cexp'MatchNum               : cexp -> int -> cexp
    val cexp'MatchWord              : cexp -> word -> cexp
    val cexp'MatchChar              : cexp -> char -> cexp
    val cexp'MatchString            : cexp -> string -> cexp
    val cexp'MatchC0                : cexp -> int -> cexp
    val cexp'MatchC1                : cexp -> int -> cexp
    val cexp'MatchE                 : cexp -> int -> cexp
    val cexp'MatchG                 : cexp -> cexp -> cexp

    val cstat'ReturnWith            : cexp -> cstat
    val cstat'Tail                  : cexp -> cexp list -> cstat
    val cstat'Raise                 : cexp -> cstat

    val cexp'Frame                  : int -> cexp
    val cexp'Arg                    : int -> cexp
    val cexp'Cret                   : cexp
    val cexp'Xret                   : cexp

    val cexp'stdOut                   : cexp
    val cexp'stdErr                   : cexp

    type layout
    val layCexp         : cexp -> layout
    val layCdefs        : cdef list -> layout
end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ../prelude/PREL.ML *)
signature PrelSig_Import =
sig
    val fold            : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    val stringOfInt     : int -> string
    val enumerateFrom   : int -> 'a list -> ('a * int) list
    val sofStringLit    : string -> string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: MisclaySig ../prelude/MISCLAY.ML *)
signature MisclaySig_Import =
sig
  type layout
  val Word                      : string -> layout
  val Right                     : string -> layout
  val //                        : layout * layout -> layout
  val @@                        : layout * layout -> layout
  val ++                        : layout * layout -> layout
  val ``                        : layout * layout -> layout
  val //``                      : layout * layout -> layout
  val layInt                    : int -> layout
  val layBracket                : layout -> layout
  val layCommaSep               : layout list -> layout
  val layVert                   : layout list -> layout
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor CCODE(structure Prel : PrelSig_Import
              structure Misclay : MisclaySig_Import) : CcodeSig =
struct

  infix // @@ ++ `` //``
  open Prel Misclay

(*----------------------------------------------------------------------
INDEX: c-code types
----------------------------------------------------------------------*)

  datatype cid = CID of string
  datatype clab = CLAB of string
  datatype ctype = CTYPE of string

  datatype cexp
    = CEXP'Id           of cid
    | CEXP'IntLit       of int
    | CEXP'StringLit    of string
    (*| CEXP'Amp          of cexp*)
    | CEXP'FunCall      of ccall
  and ccall
    = CCALL             of cid * cexp list

  datatype cact
    = CACT'Assign       of cid * cexp
    | CACT'Call         of ccall

  datatype cstat
    = CSTAT'Return      of cexp
    | CSTAT'StaticThen  of ctype * cid * cstat
    | CSTAT'DecListThen of ctype * cid list * cstat
    | CSTAT'DefThen     of ctype * cid * cexp * cstat
    | CSTAT'ActThen     of cact * cstat
    | CSTAT'If          of cexp * cstat * cstat
    | CSTAT'Goto        of clab
    | CSTAT'Where       of cstat * clab * cstat

  datatype cdef
    = CDEF'FuncDec of ctype * cid * (ctype * cid) list
    | CDEF'FuncDef of ctype * cid * (ctype * cid) list * cstat
    | CDEF'VarDec of ctype * cid
    | CDEF'VarDef of ctype * cid * cexp

(*----------------------------------------------------------------------
INDEX: prim build
----------------------------------------------------------------------*)

  val cid's             = CID
  val clab's            = CLAB
  val ctype's           = CTYPE

  val cact'Assign       = CACT'Assign
  val cact'Call         = CACT'Call

  val cexp'Id           = CEXP'Id
  val cexp'IntLit       = CEXP'IntLit
  val cexp'StringLit    = CEXP'StringLit
  (*val cexp'Amp          = CEXP'Amp*)
  val cexp'FunCall      = CEXP'FunCall

  val ccall             = CCALL

  val cstat'Return      = CSTAT'Return
  val cstat'StaticThen  = CSTAT'StaticThen
  val cstat'DecListThen = CSTAT'DecListThen
  val cstat'DefThen     = CSTAT'DefThen
  val cstat'ActThen     = CSTAT'ActThen
  val cstat'If          = CSTAT'If
  val cstat'Goto        = CSTAT'Goto
  val cstat'Where       = CSTAT'Where

  val cdef'FuncDec      = CDEF'FuncDec
  val cdef'FuncDef      = CDEF'FuncDef
  val cdef'VarDef       = CDEF'VarDef
  val cdef'VarDec       = CDEF'VarDec

(*----------------------------------------------------------------------
INDEX: convert names to legal C identfifiers
----------------------------------------------------------------------*)

  (*fun isAlpha c =
      (isUpper c) orelse (isLower c) orelse (isDigit c) orelse (isUnderscore c)*)

  val Cname'char = fn
      #"!"  => "Bang"
    | #"%"  => "Percent"
    | #"&"  => "Amp"
    | #"$"  => "Dollar"
    | #"#"  => "Hash"
    | #"+"  => "Plus"
    | #"-"  => "Dash"
    | #"/"  => "Slash"
    | #":"  => "Colon"
    | #"<"  => "Less"
    | #"="  => "Equal"
    | #">"  => "Greater"
    | #"?"  => "Query"
    | #"@"  => "At"
    | #"\\" => "Backslash"
    | #"~"  => "Tilda"
    | #"`"  => "BackTick"
    | #"^"  => "Hat"
    | #"|"  => "Bar"
    | #"*"  => "Star"
    | #"'"  => "Tick"
    | #"."  => "_" (* builtins are named using the enclosing Struct dot-path *)
    | c => (*if (not (isAlpha c)) then abort ("Cname'char: "^implode [c]) else*) implode [c]


  fun Cname'string s = concat (map Cname'char (explode s))

(*----------------------------------------------------------------------
INDEX: build
----------------------------------------------------------------------*)

  fun makeCid string = cid's (Cname'string string)

  local val U = ref 1
  in fun newCid string = let val n = !U
                         in U := 1+n; cid's ("n" ^ stringOfInt n ^ "_" ^ Cname'string string)
                         end
  end

  val ctype'Nword = ctype's "Nword"
  val ctype'Ncode = ctype's "Ncode"

  fun ccall's s args =  ccall (cid's s, args)
  fun cexp'call s args =  cexp'FunCall (ccall's s args)


  val cexp'null         = cexp'call "g_null" []
  val cexp'unit         = cexp'call "g_unit" []
  val cexp'colonEq      = cexp'call "g_colonEq" []
  val cexp'ref0         = cexp'call "g_ref0" []

  fun cexp'con0 {tag,arity}     = cexp'call "g_con0" [cexp'IntLit tag, cexp'IntLit arity]

  fun cexp'closeBuiltin n e = cexp'call ("m_CloseBuiltin_"^stringOfInt n) [e]

  fun cexp'UnappliedBuiltin (n,s) = cexp'closeBuiltin n (cexp'Id (cid's ("builtin_" ^ (Cname'string s))))

  fun cexp'num n = cexp'call "g_mkNum" [cexp'IntLit n] (* what about big ints? *)
  fun cexp'word w = cexp'call "g_mkWord" [cexp'IntLit (Word.toInt w)]
  fun cexp'char c = cexp'call "g_mkChar" [cexp'IntLit (ord c)]
  fun cexp'string s = cexp'call "g_mkString" [cexp'StringLit s]


  fun cexp'exname0 s u =
      cexp'call "g_mkExname" [cexp'StringLit s, cexp'IntLit u]

  fun cexp'exname1 s u e =
      cexp'call "g_mkExname" [cexp'StringLit s, cexp'IntLit u, e]


  fun cexp'MakeSiFn {frame_size,num_args} C = cexp'call "m_MakeSiFn" [cexp'IntLit frame_size,
                                                                      cexp'IntLit num_args,
                                                                      C]

  fun cexp'MakeFn si                        = cexp'call "g_MakeFn" [si]

  fun cexp'NewArray size                    = cexp'call "g_MakeArray" [cexp'IntLit size]
  fun cexp'NewTup size                      = cexp'call "g_MakeTuple" [cexp'IntLit size]
  fun cexp'NewException s                   = cexp'call "g_MakeException" [cexp'StringLit s]
  fun cexp'NewRef e                         = cexp'call "g_MakeRef" [e]

  val cexp'EmptyRef                        = cexp'call "g_EmptyRef" []
  fun ccall'FixupRef e1 e2                 = ccall's "g_FixupRef" [e1,e2]


  fun cexp'NewCon n e                       = cexp'call "g_MakeCon" [cexp'IntLit n,e]
  fun cexp'Copy e                           = cexp'call "g_Copy" [e]
  fun cexp'DeCon e                          = cexp'call "g_DeCon" [e]
  fun cexp'DeExcon e                        = cexp'call "g_DeExcon" [e]
  fun cexp'DeRef e                          = cexp'call "g_DeRef" [e]
  fun cexp'DeTuple e n                      = cexp'call "g_DeTuple" [e, cexp'IntLit n]
  fun cexp'Builtin s es                     = cexp'call ("builtin_" ^ Cname'string s) es

  fun cexp'MakeSiCont frame_size c          = cexp'call "m_MakeSiCont" [cexp'IntLit frame_size, cexp'Id c]
  fun cexp'MakeSiHandle frame_size c        = cexp'call "m_MakeSiHandle" [cexp'IntLit frame_size, cexp'Id c]
  fun ccall'PushContinuation si             = ccall's "g_PushContinuation" [si]
  fun ccall'PushHandler si                  = ccall's "g_PushHandler" [si]

  fun ccall'SetArrayElement r1 n r2         = ccall's "g_SetArrayElement" [r1, cexp'IntLit n, r2]
  fun ccall'SetTupleElement r1 n r2         = ccall's "g_SetTupleElement" [r1, cexp'IntLit n, r2]
  fun ccall'SetFrameElement r1 n r2         = ccall's "g_SetFrameElement" [r1, cexp'IntLit n, r2]
  fun ccall'SetContFrameElem n r            = ccall's "g_SetContFrameElem" [cexp'IntLit n, r]
  fun ccall'SetXcontFrameElem n r           = ccall's "g_SetXcontFrameElem" [cexp'IntLit n, r]

  fun cexp'MatchNum e n         = cexp'call "g_matchNum" [e, cexp'IntLit n] (* what about big ints? *)
  fun cexp'MatchWord e w        = cexp'call "g_matchWord" [e, cexp'IntLit (Word.toInt w)]
  fun cexp'MatchChar e c        = cexp'call "g_matchChar" [e, cexp'IntLit (ord c)]
  fun cexp'MatchString e s      = cexp'call "g_matchString" [e, cexp'StringLit s]
  fun cexp'MatchC0 e n          = cexp'call "g_matchC0" [e, cexp'IntLit n]
  fun cexp'MatchC1 e n          = cexp'call "g_matchC1" [e, cexp'IntLit n]
  fun cexp'MatchE e n           = cexp'call "g_matchE" [e, cexp'IntLit n]
  fun cexp'MatchG e e2          = cexp'call "g_matchG" [e, e2]

  fun cstat'ReturnWith e          = cstat'Return (cexp'call "g_returnWith" [e])
  fun cstat'Tail f args         = cstat'Return (cexp'call ("g_call_"^stringOfInt(length args)) (f::args))
  fun cstat'Raise e             = cstat'Return (cexp'call "g_raise" [e])

  fun cexp'Frame n              = cexp'call "FRAME" [cexp'IntLit n]
  fun cexp'Arg n                = cexp'call "ARG" [cexp'IntLit n]
  val cexp'Cret                 = cexp'Id (cid's "CRET")
  val cexp'Xret                 = cexp'Id (cid's "XRET")

  fun cexp'NewPap F {num_early,num_late} = cexp'call "g_MakePap" [F, cexp'IntLit num_early, cexp'IntLit num_late]


  val cexp'stdOut = cexp'call "g_stdOut" []
  val cexp'stdErr = cexp'call "g_stdErr" []

(*----------------------------------------------------------------------
INDEX: lay c-code
----------------------------------------------------------------------*)

  fun layCid (CID x) = Word x
  fun layClab (CLAB x) = Word x
  fun layCtype (CTYPE x) = Word x

  fun layIntForC x = Word (if (x<0) then "-"^stringOfInt(~x) else stringOfInt x)

  fun layCexp cexp =
      case cexp
       of CEXP'Id cid       => layCid cid
        | CEXP'IntLit n     => layIntForC n
        | CEXP'StringLit s  => Word (sofStringLit s)
        (*| CEXP'Amp e        => Left "&" ++ layCexp e*)
        | CEXP'FunCall call => layCcall call

  and layCcall (CCALL (x,args)) =
      layCid x @@ layBracket (layCommaSep (map layCexp args))


  fun layBraced head body =
      (head @@ Word "{") //`` body // Word "}"

  fun layCact act =
      case act
       of CACT'Assign (x,e) => layCid x ++ Word "=" ++ layCexp e @@ Right ";"
        | CACT'Call call => layCcall call @@ Right ";"

  fun layCstat x =
      case x
       of CSTAT'Return exp              => Word "return" ++ layCexp exp @@ Right ";"
        | CSTAT'StaticThen (typ,x,stat) => Word "static" ++ layCtype typ ++ layCid x @@ Right ";" // layCstat stat
        | CSTAT'DecListThen (typ,xs,stat) => layCtype typ ++ layCommaSep (map layCid xs) @@ Right ";" // layCstat stat
        | CSTAT'DefThen (typ,x,e,stat)  => layCtype typ ++ layCid x ++ Word "=" ++ layCexp e @@ Right ";" // layCstat stat
        | CSTAT'ActThen (act,stat)      => layCact act // layCstat stat
        | CSTAT'If (i,t,e)              => layBraced (layBraced (Word "if" ++ layBracket (layCexp i)) (layCstat t)
                                                                @@ (Word "else")) (layCstat e)
        | CSTAT'Goto lab                => Word "goto" ++ layClab lab @@ Right ";"
        | CSTAT'Where (s1,lab,s2)       => layCstat s1 // (layClab lab @@ Right ":") // (layCstat s2)

  fun layCdef (CDEF'FuncDec (typ,name,formals)) =
      (layCtype typ)
      ++ layCid name
      ++ layBracket (layCommaSep (map (fn (typ,x) => layCtype typ ++ layCid x) formals)) @@ Right ";//forward"
    | layCdef (CDEF'FuncDef (typ,name,formals,body)) =
      layBraced ((layCtype typ)
                 ++ layCid name
                 ++ layBracket (layCommaSep (map (fn (typ,x) => layCtype typ ++ layCid x) formals)))
                (layCstat body)
    | layCdef (CDEF'VarDec (typ,name)) =
      Word "extern" ++ (layCtype typ) ++ layCid name @@ Right ";"
    | layCdef (CDEF'VarDef (typ,name,exp)) =
      (layCtype typ) ++ layCid name ++ Word "=" ++ (layCexp exp) @@ Right ";"


  fun layCdefs defs = layVert (map layCdef defs)




end
