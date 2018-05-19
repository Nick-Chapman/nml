#!/bin/bash
cd $(dirname $0)
../gen2/nux.exe \
../predefined/nml_NonPrim.ML \
-x 'open NonPrim' \
../prelude/pervasives.ML \
../prelude/PREL.ML \
-x 'structure Prel = PREL();' \
nfib.ml \
-x nfib_top \
--export nfib.C
