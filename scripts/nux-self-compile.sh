#!/bin/bash
exe=$1
prefix=$2
generatedC=$3
mkdir -p $(dirname $generatedC)

$exe \
predefined/nml_NonPrim.ML \
-x 'open NonPrim' \
prelude/pervasives.ML \
ML/CCODE.ML \
ML/MACHINE.ML \
ML/COMPILE3.ML \
ML/CPS.ML \
ML/EMBED.ML \
ML/EVAL3.ML \
ML/ATOM.ML \
ML/VALUE.ML \
ML/BASIS.ML \
ML/BUILTIN.ML \
ML/LANG.ML \
ML/TOK.ML \
ML/POS.ML \
ML/LEX.ML \
ML/PARSER.ML \
ML/PRETTY.ML \
ML/RUN.ML \
ML/PROGRAM.ML \
ML/INTERPRETER.ML \
prelude/PAR1.ML \
prelude/PAR2.ML \
prelude/PAR3.ML \
prelude/PREL.ML \
prelude/SORT.ML \
prelude/ASSOC.ML \
prelude/QLAYOUT.ML \
prelude/MISCLAY.ML \
prelude/IMP_HASH.ML \
-x 'val prefixNML = "'$prefix': ";' \
bind.ML \
-x Run.nux \
--export $generatedC