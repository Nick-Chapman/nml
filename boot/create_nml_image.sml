(*Control.Print.out := {say = fn _ => (), flush = fn () => ()};*)
fun prefixPrelude s = "prelude/"^s
;use "prelude/pervasives.ML";
val nml_sources = map (fn s => "ML/"^s)
    [
     "CCODE.ML",
     "MACHINE.ML",
     "COMPILE3.ML",
     "CPS.ML",
     "EMBED.ML",
     "EVAL3.ML",
(* pre-compiler *)
     "ATOM.ML",
     "VALUE.ML",
     "BASIS.ML",
     "BUILTIN.ML",
     "LANG.ML",
     "TOK.ML",
     "POS.ML",
     "LEX.ML",
     "PARSER.ML",
     "PRETTY.ML",
     "RUN.ML",
     "PROGRAM.ML",
     "INTERPRETER.ML"
     ] @ map prefixPrelude [
     "PAR1.ML",
     "PAR2.ML",
     "PAR3.ML",
     "PREL.ML",
     "SORT.ML",
     "ASSOC.ML",
     "QLAYOUT.ML",
     "MISCLAY.ML",
     "IMP_HASH.ML"
     ]
;map use nml_sources;
val prefixNML = "NML: ";
use "bind.ML";
SMLofNJ.exportML "boot/nml.image";
