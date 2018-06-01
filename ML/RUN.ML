(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  14.    export
  45.    imports
  189.   functor
  228.   test lexer
  254.   test parser
  291.   test parser/pp
  345.   eval
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature RunSig =
    sig
        val tl : {IN:string, OUT:string} -> unit
        val echo_tl : {IN:string} -> unit

        type pos
        type topdec
        val tpFile : {path:string} -> pos * {eof:bool} * topdec list option
        val tpString : string -> pos * {eof:bool} * topdec list option

        type infixMap
        val theIM : infixMap

        val tpp : string -> unit
        val tpp' : string -> unit
        val tpp'' : string -> unit

        val Nexec : string -> unit
        val Nuse : string -> unit

        (*type value
        val Neval : string -> value*)

        val Nexport : string -> string -> unit
        val Nexport'  : string -> unit (* to stdOut *)

        val nux  : string list -> unit


        type basis
        val resetInitialBasis : unit -> unit
        val extendTheBasis : basis -> unit
        val getTheBasis : unit -> basis

    end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ../prelude/PREL.ML *)
signature PrelSig_Import =
sig
    val echo            : string -> unit
end;
(*IMPORTDONE:*)

(*IMPORTSIG: PosSig POS.ML *)
signature PosSig_Import =
sig
    type pos
    val makePos : {path: string, row: int, col: int} -> pos
    val sofPos : pos -> string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: QLayoutSig ../prelude/QLAYOUT.ML *)
signature QLayoutSig_Import = sig
  type layout
  val Word          : string -> layout
  val Sep           : string -> layout
  val Blank         : layout
  val Above         : layout -> layout -> layout
  val squash_to     : int -> layout -> layout
  val stream_layout : TextIO.outstream -> layout -> unit
  val prefixed_output_layout : string -> layout -> unit
end;
(*IMPORTDONE:*)

(*IMPORTSIG: TokSig TOK.ML *)
signature TokSig_Import =
sig
    type tok;
    val sofTok : tok -> string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: LexSig LEX.ML *)
signature LexSig_Import =
    sig
        type 'a llist
        val iterateLL : ('a -> ('b * 'a) option) -> 'a -> 'b llist
        val appLL : ('a -> unit) -> 'a llist -> unit
        val lazyFile : {path:string} -> string llist
        val lazyString : string -> string llist
        type pos
        type lexState
        type tok
        val initLS : pos -> string llist -> lexState
        val stepLS : string -> lexState -> (tok * lexState) option
        val posLS : lexState -> pos
    end;
(*IMPORTDONE:*)

(*IMPORTSIG: ParSig ../prelude/PAR1.ML *)
signature ParSig_Import =
sig
    type 'a par
    type pos
    type tok
    type lexState
    val execP       : 'a par -> lexState -> lexState * 'a option
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AtomSig ATOM.ML *)
signature AtomSig_Import =
sig
    datatype id = ID of string
    datatype vid = VID of id
    type strid;
    datatype 'a long = LONG of strid list * 'a
end;
(*IMPORTDONE:*)

(*IMPORTSIG: ParserSig PARSER.ML *)
signature ParserSig_Import =
sig
    datatype id = ID of string
    datatype vid = VID of id
    datatype leftOrRight = LR'Left | LR'Right
    datatype pa = PA of {assoc:leftOrRight, prec:int}
    type infixMap
    val makeIM : (vid * pa) list -> infixMap
    type topdec
    type 'a par
    val prog : infixMap -> topdec list par
end;
(*IMPORTDONE:*)

(*IMPORTSIG: PrettySig PRETTY.ML *)
signature PrettySig_Import =
    sig
        type layout
        type infixMap
        type topdec
        val lay'topdecs : infixMap -> topdec list -> layout
    end;
(*IMPORTDONE:*)

(*IMPORTSIG: BasisSig BASIS.ML *)
signature BasisSig_Import =
sig
    type 'a long
    type vid
    type value
    type idstatus;
    type env

    val look'longvid    : string -> env -> vid long -> value * idstatus
    type ('a,'b) assoc
    type basis
    val basis'plus      : basis -> basis -> basis
    val env'basis       : basis -> env

    type layout
    val lay'basis       : basis -> layout
end;
(*IMPORTDONE:*)

(*IMPORTSIG: BuiltinSig BUILTIN.ML *)
signature BuiltinSig_Import =
sig
    type basis
    val basis'initial : basis
end;
(*IMPORTDONE:*)

(*IMPORTSIG: ProgramSig PROGRAM.ML *)
signature ProgramSig_Import =
sig
    type basis
    type topdec
    type exval
    val eval'program : basis -> topdec list -> (basis,exval) either
end;
(*IMPORTDONE:*)

(*IMPORTSIG: ValueSig VALUE.ML *)
signature ValueSig_Import =
sig
    type vid
    type value
    type exval
    type layout
    val value'exval     : exval -> value
    val lay'value       : value -> layout
    val exportC         : TextIO.outstream -> value -> unit
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor RUN(val prefixNML : string
            structure Prel : PrelSig_Import
            val quiet : bool ref
            structure Pos : PosSig_Import
            structure QLayout : QLayoutSig_Import
            structure Tok : TokSig_Import
            structure Lex : LexSig_Import
            structure Par : ParSig_Import
            structure Atom : AtomSig_Import
            structure Parser : ParserSig_Import
            structure Pretty : PrettySig_Import
            structure Builtin : BuiltinSig_Import
            structure Basis : BasisSig_Import
            structure Program : ProgramSig_Import
            structure Value : ValueSig_Import
            val wrapCol : int
            sharing type Tok.tok = Lex.tok
            sharing type Pos.pos = Lex.pos = Par.pos
            sharing type Lex.lexState = Par.lexState
            sharing type Par.par = Parser.par
            sharing type Value.value = Basis.value
            sharing type Atom.vid = Parser.vid = Basis.vid
            sharing type Atom.long = Basis.long
            sharing type Parser.topdec = Pretty.topdec = Program.topdec
            sharing type Parser.infixMap = Pretty.infixMap
            sharing type QLayout.layout = Pretty.layout = Basis.layout = Value.layout
            sharing type Basis.basis = Builtin.basis = Program.basis
            sharing type Value.exval = Program.exval
                         ) : RunSig =
struct

    open Prel QLayout Atom Value Basis Builtin Pos Lex Par Parser Pretty Program

    fun takeWhile p [] = []
      | takeWhile p (x::xs) = if (p x) then x :: takeWhile p xs else []

    fun basename path =
        implode (rev (takeWhile (fn x => x <> #"/") (rev (explode path))))

(*----------------------------------------------------------------------
INDEX: test lexer
----------------------------------------------------------------------*)

    fun printPosTok os (p,tok) =
        TextIO.output (os,"(*"^sofPos p^"--*)"^Tok.sofTok tok^"\n")

    fun stepLSP ls =
        case (Lex.stepLS "stepLSP" ls) of
            Some (tok,ls') => Some ((Lex.posLS ls', tok), ls')
          | None => None

    fun tl_stream {IN,OUT=os} =
        let val ll_string = lazyFile {path=IN}
            val ls = Lex.initLS (makePos{path=IN,row=1,col=0}) ll_string
            val ll_posToken = iterateLL stepLSP ls
        in appLL (printPosTok os) ll_posToken
        end;

    fun tl {IN,OUT} =
        let val os = TextIO.openOut OUT
        in (tl_stream {IN=IN,OUT=os}; TextIO.closeOut os)
        end

    fun echo_tl {IN} = tl_stream {IN=IN,OUT=TextIO.stdOut}

(*----------------------------------------------------------------------
INDEX: test parser
----------------------------------------------------------------------*)

    val theIM = makeIM [
                        (* initial basis... *)
                        (VID (ID "::"), PA {prec=5, assoc=LR'Right}),
                        (VID (ID "="),  PA {prec=4, assoc=LR'Left}),
                        (VID (ID ":="), PA {prec=3, assoc=LR'Left}),
                        (* other (adhoc) *)
                        (VID (ID "^"), PA {prec=0, assoc=LR'Left}),
                        (VID (ID "*"), PA {prec=7, assoc=LR'Left}),
                        (VID (ID "+"), PA {prec=6, assoc=LR'Left}),
                        (VID (ID "-"), PA {prec=6, assoc=LR'Left}),
                        (VID (ID "<>"), PA {prec=0, assoc=LR'Left}),
                        (VID (ID "<"), PA {prec=0, assoc=LR'Left}),
                        (VID (ID ">"), PA {prec=0, assoc=LR'Left}),
                        (VID (ID "<="), PA {prec=0, assoc=LR'Left}),
                        (VID (ID ">="), PA {prec=0, assoc=LR'Left}),
                        (VID (ID "o"), PA {prec=0, assoc=LR'Left}),
                        (VID (ID "div"), PA {prec=7, assoc=LR'Left}),
                        (VID (ID "mod"), PA {prec=7, assoc=LR'Left}),
                        (VID (ID "@"), PA {prec=0, assoc=LR'Right})
                        ]

    fun tp_ll path ll =
        let val par = prog theIM
            val ls = Lex.initLS (makePos{path=path,row=1,col=0}) ll
            val (ls',xOpt) = Par.execP par ls
            val eof = case (stepLS "tp_ll" ls') of None => true | Some _ => false
        val posOpt = Lex.posLS ls'
        in (posOpt,{eof=eof},xOpt)
        end

    fun tpFile {path} = tp_ll path (lazyFile {path=path})
    fun tpString string = tp_ll "<string>" (lazyString string)

(*----------------------------------------------------------------------
INDEX: test parser/pp
----------------------------------------------------------------------*)

    val istrings = ["@","o","^","*","+","-","<>",">","<","<=",">=","div","mod",
                    "@@","++","//","``","//``",
                    "**","##",">>",">>>","-$$","$$-"]
    val theIM_forPP =
        makeIM ([
                 (* initial basis... *)
                 (VID (ID "::"), PA {prec=5, assoc=LR'Right}),
                 (VID (ID "="),  PA {prec=4, assoc=LR'Left}),
                 (VID (ID ":="), PA {prec=3, assoc=LR'Left})
                 (* other (adhoc) *)
                 ] @ map (fn x =>
                          (VID (ID x), PA {prec=0, assoc=LR'Left})
                          ) istrings)

    infix // fun (lay1 // lay2)  = Above lay1 lay2
    val BlankLine = Sep ""
    fun layVert [] = Blank (* duplicate def *)
      | layVert [x] = x
      | layVert (x::xs) = Above x (layVert xs)
    fun lay'top im xs =
        layVert [BlankLine,
                 squash_to wrapCol (lay'topdecs im xs),
                 Word ";",
                 BlankLine]

    fun echo_and_Word s = (echo s; Word s)

    fun tpp_streamOut {IN,OUT=os} =
        let val im = theIM_forPP
            val (pos,{eof},tdso) = tpFile {path=IN}
            val layout =
                BlankLine
                //(if eof then Blank else
                   echo_and_Word ("***parser did not reach eof, pos="^sofPos pos))
                // (case tdso of
                        Some xs => lay'top im xs
                      | _ => echo_and_Word ("***parse-error, pos="^sofPos pos))
                // BlankLine
        in stream_layout os layout
        end

    fun tpp_fileOut {IN,OUT} =
        let val os = TextIO.openOut OUT
        in (tpp_streamOut {IN=IN,OUT=os}; TextIO.closeOut os)
        end

    fun tpp path = tpp_streamOut {IN=path,OUT=TextIO.stdOut}
    fun tpp' path = tpp_fileOut {IN=path, OUT=",g/"^basename path}
    fun tpp'' path = tpp_fileOut {IN=",g/"^path, OUT=",g/2-"^basename path}

(*----------------------------------------------------------------------
INDEX: eval
----------------------------------------------------------------------*)

    fun NMLecho s = if (!quiet) then () else echo (prefixNML^s)

    fun echo'lay lay x =
        if (!quiet) then () else
        (prefixed_output_layout prefixNML (squash_to wrapCol (lay x));
         echo "")

    val echoBasis = echo'lay lay'basis
    val echoValue = echo'lay lay'value

    local
        val THE_basis = ref basis'initial
    in
        fun resetInitialBasis () = (THE_basis := basis'initial)
        fun extendTheBasis basis1 = (THE_basis := basis'plus (!THE_basis) basis1)
        fun getTheBasis () = !THE_basis

        fun G_useREADER PROG READER =
            let
                val U as () = NMLecho ("***parsing...")
                val (pos,{eof},tdso) = READER()
                val U =
                    if eof then () else
                    NMLecho ("***parser did not reach eof, pos="^sofPos pos)
            in
                case tdso of
                    None => NMLecho ("***parse-error, pos="^sofPos pos)
                  | Some tds =>
                    (
                     NMLecho ("***evaluation...");
                     case (PROG (!THE_basis) tds) of
                         This basis1 =>
                         (echoBasis basis1;
                          extendTheBasis basis1)
                       | That exval =>
                         (NMLecho ("***uncaught exception");
                          echoValue (value'exval exval)))
            end

    end

    fun G_Nexec PROG string = G_useREADER PROG (fn () => tpString string)
    fun G_Nuse PROG path = G_useREADER PROG (fn () => tpFile {path=path})

    val Nexec = G_Nexec eval'program
    val Nuse = G_Nuse eval'program

    fun getIt() = let val (v,_) = look'longvid "" (env'basis (getTheBasis())) (LONG ([],VID (ID "it")))
                  in v
                  end

(*
    fun Neval s = (Nexec s; getIt())
    fun Nexport filename s =
        let val () = NMLecho ("***export-> "^filename^"...");
            val os = TextIO.openOut filename
            val () = Value.exportC os (Neval s)
            val () = TextIO.closeOut os
        in ()
        end
    fun Nexport' s = Value.exportC TextIO.stdOut (Neval s)
*)

    fun Nexport_it filename =
        let val () = NMLecho ("***export-> "^filename^"...");
            val os = TextIO.openOut filename
            val () = Value.exportC os (getIt())
            val () = TextIO.closeOut os
        in ()
        end

    fun Nexport filename s =
        let val () = Nexec s
        in Nexport_it filename
        end

    fun Nexport' s =
        let val () = Nexec s
        in Value.exportC TextIO.stdOut  (getIt())
        end

    fun nux [] = ()
      | nux ("-x"::x::xs)           = (NMLecho "nux/exec"      ; Nexec x      ; nux xs)
      | nux ("--export"::x::xs)     = (NMLecho "nux/export"    ; Nexport_it x ; nux xs)
      | nux (x::xs)                 = (NMLecho ("nux/use:"^x)  ; Nuse x       ; nux xs)

end
