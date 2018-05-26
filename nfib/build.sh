#!/bin/bash
genC=$1

boot/nux.exe \
predefined/nml_NonPrim.ML \
-x 'open NonPrim' \
prelude/pervasives.ML \
prelude/PREL.ML \
-x 'structure Prel = PREL();' \
nfib/nfib.ml \
-x nfib_top \
--export $genC
