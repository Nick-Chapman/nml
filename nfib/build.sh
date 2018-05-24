#!/bin/bash
exe=$1
ml=$2
genC=$3

$exe \
predefined/nml_NonPrim.ML \
-x 'open NonPrim' \
prelude/pervasives.ML \
prelude/PREL.ML \
-x 'structure Prel = PREL();' \
$ml \
-x nfib_top \
--export $genC
