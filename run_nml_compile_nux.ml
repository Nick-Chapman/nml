
val preludeDir = "prelude/"
fun prefixPrelude s = preludeDir^s
val pervasives = map prefixPrelude ["pervasives.ML"]; (*["option.ML","either.ML"];*)
;map use pervasives;
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
	
(* load the code into NJ... *)
;map use nml_sources;
val prefixNML = "NML: ";
use "bind.ML";

(*----------------------------------------------------------------------*)
(* export nux... *)

Run.Nuse "predefined/nml_NonPrim.ML";
Run.Nexec "open NonPrim";
map Run.Nuse pervasives;
map Run.Nuse nml_sources;
Run.Nexec "val prefixNML = \"NML-inner: \";"; 
Run.Nuse "bind.ML";
Run.Nexport ",C/nux.C" "Run.nux";
