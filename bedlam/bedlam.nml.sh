#!/bin/bash
boot/nux.exe\
    predefined/nml_NonPrim.ML -x 'open NonPrim' \
    prelude/pervasives.ML\
    prelude/PREL.ML\
    -x 'structure Prel = PREL()'\
    -x 'open Prel'\
    bedlam/bedlam.ml\
    -x 'fn _ => run()'\
    --export $1
