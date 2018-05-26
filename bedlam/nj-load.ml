(*run bedlam in nj*)
Control.Print.out := {say = fn _ => (), flush = fn () => ()};
use "prelude/pervasives.ML";
use "prelude/PREL.ML";
structure Prel = PREL();
open Prel;
use "bedlam/bedlam.ml";
run();
