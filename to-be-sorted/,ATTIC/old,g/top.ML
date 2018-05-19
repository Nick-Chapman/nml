

val preludeDir = "/home/nic/project/prelude/"
fun useP s = use (op ^ {1 = preludeDir,2 = s})
val it = useP "option.ML"
val it = useP "either.ML"
val it = useP "Prel.ML"
val it = useP "Sort.ML"
val it = useP "ASSOC.ML"
val it = useP "QLAYOUT.ML"
val it = useP "MISCLAY.ML"
val it = use "ATOM.ML"
val it = use "VALUE.ML"
val it = use "BASIS.ML"
val it = use "BUILTIN.ML"
val it = use "LANG.ML"
val it = use "EVAL.ML"
val it = use "TOK.ML"
val it = use "LEX.ML"
val it = use "PFV.ML"
val it = use "PARSER.ML"
val it = use "PRETTY.ML"
val it = use "RUN.ML"
val prefixNML = "NML: "
val it = use "bind.ML"
val it = use "test.ML"
val it = runTests tests
signature BMARK = sig val doit : (unit -> unit)
                      val testit : (TextIO.outstream -> unit)
                  end
val it = ((use "/home/nic/sml-resources/BENCHMARKS/SML97/programs/life/life.sml"
          ) Main.testit) TextIO.stdOut
signature BMARK = sig type generation
                      val show : ((string -> unit) -> (generation -> unit))
                      val gun : generation
                      val nthgen : (generation -> (int -> generation))
                  end
val it =
((((use "/home/nic/sml-resources/BENCHMARKS/SML97/programs/life/life.sml")
   let open Main
   in (show print) gun
   end) let open Main
        in (show print) ((nthgen gun) 1)
        end) use) "BUILTIN.ML"
val it = use "bind.ML"
val it = Run.Nuse "nml_NonPrim.ML"
val it = Run.Nexec "open NonPrim"
val it = Run.Nexec
         "signature BMARK = sig val testit : TextIO.outstream -> unit end;"
val it =
Run.Nuse "/home/nic/sml-resources/BENCHMARKS/SML97/programs/life/life.sml"
val it = Run.Nexec "Main.testit TextIO.stdOut;"
val it = Run.Nuse "nml_NonPrim.ML"
val it = Run.Nexec "open NonPrim"
val it =
Run.Nexec
"signature BMARK = sig type generation; val show : (string -> unit) -> generation -> unit; val gun : generation val nthgen : generation -> int -> generation end"
val it =
Run.Nuse "/home/nic/sml-resources/BENCHMARKS/SML97/programs/life/life.sml"
val it = Run.Nexec "let open Main in show print gun end"
val it = Run.Nexec "let open Main in show print (nthgen gun 1) end"
val it = use "bind.ML"
val it = Run.Nuse "nml_NonPrim.ML"
val it = Run.Nexec "open NonPrim"
val it = Run.Nuse (op ^ {1 = preludeDir,2 = "option.ML"})
val it = Run.Nuse (op ^ {1 = preludeDir,2 = "either.ML"})
val it = Run.Nuse (op ^ {1 = preludeDir,2 = "Prel.ML"})
val it = Run.Nuse (op ^ {1 = preludeDir,2 = "Sort.ML"})
val it = Run.Nuse (op ^ {1 = preludeDir,2 = "ASSOC.ML"})
val it = Run.Nuse (op ^ {1 = preludeDir,2 = "QLAYOUT.ML"})
val it = Run.Nuse (op ^ {1 = preludeDir,2 = "MISCLAY.ML"})
val it = Run.Nuse "ATOM.ML"
val it = Run.Nuse "VALUE.ML"
val it = Run.Nuse "BASIS.ML"
val it = Run.Nuse "BUILTIN.ML"
val it = Run.Nuse "LANG.ML"
val it = Run.Nuse "EVAL.ML"
val it = Run.Nuse "TOK.ML"
val it = Run.Nuse "LEX.ML"
val it = Run.Nuse "PFV.ML"
val it = Run.Nuse "PARSER.ML"
val it = Run.Nuse "PRETTY.ML"
val it = Run.Nuse "RUN.ML"
val it = Run.Nexec "val prefixNML = \"NML-inner: \";"
val it = Run.Nuse "bind.ML"
val it = Run.Nexec "(fn f => f 3)(fn x => x + 1)"
val it = Run.Nexec "Run.Nexec \"(fn f => f 3)(fn x => x + 1)\""
val it = Run.Nuse "nml_NonPrim.ML"
val it = Run.Nexec "open NonPrim"
val it = Run.Nexec "map (fn x => x * x) [1,2,3,4,5]"
val it =
Run.Nexec
"Run.Nuse (\"nml_NonPrim.ML\"); Run.Nexec (\"open NonPrim\"); Run.Nexec \"map (fn x => x * x) [1,2,3,4,5]\";"
val it = Run.Nexec "Run.Nexec \"map (fn x => x * x) [1,2,3,4,5]\";"
val it = Run.Nuse "nml_NonPrim.ML"
val it = Run.Nexec "open NonPrim;"
val it = Run.Nexec "val r = ref 11 ; (!r, (r := 22; !r))"
val it = Run.Nexec "[1,2,3] @ [4,5,6]"
val it = Run.Nexec "map (fn x => x * x) [1,2,3,4,5]"
val it = Run.Nexec "rev [1,2,3,4,5]"
val it = Run.Nexec "(1 <> 1, 2 <> 3)"
val it = ((Run.Nexec "length [11,12,13]") Run.Nexec) "TextIO.stdOut"
val it = Run.Nexec "exception foo of string;"
val it = Run.Nuse "test.ML"
val it = ((Run.Nuse "regs/append-example.ML") Run.Nuse)
         (op ^ {1 = preludeDir,2 = "Prel.ML"})
val it = Run.Nexec "Prel.link [[1,2,3],[4,5,6],[],[7],[8,9]]"
val it = Run.Nuse "../misc-ml/thrice.ML"
val it = Run.Nexec "let open XXX in echo(stringOfInt(1+result)) end"
val it = ((((use "../misc-ml/try-assign-op.ML") Run.Nuse)
           "../misc-ml/try-assign-op.ML") Run.Nuse) "../misc-ml/nqueens.ML"
val it = Run.Nexec "nqueens 3"
val it = Run.Nexec "nqueens 4"
val it = Run.Nexec "nqueens 5"
val it = Run.tpp' "VALUE.ML"
val it = use ",g/VALUE.ML"
val it = Run.tpp' "BUILTIN.ML"
val it = use ",g/BUILTIN.ML"
val it = Run.tpp' "EVAL.ML"
val it = use ",g/EVAL.ML"
val it = Run.tpp' "../misc-ml/nqueens.ML"
val it = use ",g/nqueens.ML"
val it = nqueens 4
val it = nqueens 5
val it = Run.tpp' "top.ML"
val it = Run.tpp' "PFV.ML"
val it = Run.tpp'' "PFV.ML"
val it = Run.tpp' "../misc-ml/thrice.ML"
val it = Run.tpp' "../misc-ml/nqueens.ML"
val it = Run.tpp' "../prelude/Prel.ML"
val it = Run.tpp' "../prelude/ASSOC.ML"
val it = Run.tpp' "TOK.ML"
val it = Run.tpp' "LEX.ML"
val it = Run.tpp' "LANG.ML"
val it = Run.tpp' "PARSER.ML"
val it = Run.tpp "../xxx.ML"
val it = Run.echo_tl {IN = op ^ {1 = preludeDir,2 = "option.ML"}}
val it = Run.tpp (op ^ {1 = preludeDir,2 = "option.ML"})
;

