

    
    
structure Parser_ORIG = Parser;
structure Pretty_ORIG = Pretty;
open Parser_ORIG Pretty_ORIG;

structure Parser : ParserSig =
    struct
	val new_prog = prog
	open Parser_ORIG
	val prog = new_prog
    end;
structure Pretty : PrettySig =
    struct
	val new_lay'topdecs = lay'topdecs
	open Pretty_ORIG
	val lay'topdecs = new_lay'topdecs
    end;





Test.echo_tpp {IN=",samples/prec.ML"};
Test.echo_tpp {IN=",samples/fplusg.ML"};
Test.echo_tpp {IN=",samples/fog.ML"};




open Ptest;

tp "/home/chapman/per/nml/front/PRETTY.ML"
    
tp ",samples/functor-twice-example.ML"

echo_tpp ",samples/functor-twice-example.ML"
use ",samples/functor-twice-example.ML"




tpp {IN="/home/chapman/per/nml/front/,ATTIC/pp.ML",OUT=",g/pp.ML"};

use ",g/pp.ML";


tp "/home/chapman/per/nml/front/PRETTY.ML"
echo_tpp "/home/chapman/per/nml/front/PRETTY.ML"

tpp {IN="/home/chapman/per/nml/front/PRETTY.ML",OUT=",g/PRETTY.ML"};



open QLayout;
open Prel;
use "../ML/gen-lay.ML";
use ",ATTIC/pp.ML";


use "test-curlypat.ML";
echo_tpp "test-curlypat.ML";


open Ptest;
tp "test-curlypat.ML";
open Lang    
val x = it;
val (_,_,Some[td,_]) = x;
val (TOPDEC'Exp (EXP'Fn (MATCH [mrule]))) = td
val (MRULE (PAT'Record (PATROW (lpats,w)),_)) = mrule

    
echo_tpp "itest.ML";
echo_tpp "/home/chapman/per/nml/thrice.ML";
tpp {IN="/home/chapman/per/nml/thrice.ML",OUT=",g/thrice.ML"};
use ",g/thrice.ML";




    
open Ptest;
Test.tp ",samples/test-curlypat.ML";
Test.tp "/home/chapman/per/nml/thrice.ML";
Test.tp "/home/chapman/per/nml/misc-ml/nqueens.ML"; (* has symbolic idents *)
Test.tp "/home/chapman/per/Perseus/ml/prel.ML"; (* infix pat *)
Test.tp "top.ML"; (* this file *)
Test.tp "Prel.ML";
Test.tp "ASSOC.ML";
Test.tp "TOK.ML";
Test.tp "LEX.ML";
Test.tp "PFV.ML";
Test.tp "LANG.ML";
Test.tp "PARSER.ML";



open Ltest;
tl {IN="top.ML", OUT=",g/top.ML"};
tl {IN="/home/chapman/per/nml/misc-ml/nqueens.ML", OUT= ",g/nqueens.ML"};
nonfix >>;
use ",g/nqueens.ML";
nqueens 4;
tl {IN="PFV.ML",OUT=",g/PFV.ML"};
use ",g/PFV.ML";
tl {IN="LEX.ML", OUT=",g/LEX.ML"};
use ",g/LEX.ML";
tl {IN="PARSER.ML", OUT= ",g/PARSER.ML"};
use ",g/PARSER.ML"


(* dumped her -- Fri May  5 16:15:32 2000 *)
Test.tpp {IN="../thrice.ML",OUT=",g/thrice.ML"};
use ",g/thrice.ML";
Test.tpp {IN="PARSER.ML", OUT= ",g/PARSER.ML"};
use ",g/PARSER.ML"
Test.tpp {IN="PRETTY.ML", OUT= ",g/PRETTY.ML"};
use ",g/PRETTY.ML"
Test.echo_tpp {IN="PFV.ML"}; (* no semi after signature & derived functor *)
use "../thrice.ML";
Test.echo_tpp {IN="../thrice.ML"};
Test.tpp {IN="../thrice.ML",OUT=",g/thrice.ML"};
use ",g/thrice.ML";
