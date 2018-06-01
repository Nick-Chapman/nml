#!/bin/bash
boot/nux.exe\
    predefined/nml_NonPrim.ML\
    -x 'open NonPrim' \
    life/life.ml\
    -x 'fn _ => Main.testit TextIO.stdOut' \
    --export $1
