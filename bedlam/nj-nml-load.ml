(*create bedlam.C using nml loaded in nj*)
quiet := true;
val () = Run.Nuse "../predefined/nml_NonPrim.ML"
val () = Run.Nexec "open NonPrim"
val () = Run.Nuse "../prelude/pervasives.ML"
val () = Run.Nuse "../prelude/PREL.ML"
val () = Run.Nexec "structure Prel = PREL() open Prel"
(*val () = print_lang := true*)
val () = Run.Nuse "bedlam.ml"
val () = Run.Nexport "bedlam.C" "fn _ => run()"
