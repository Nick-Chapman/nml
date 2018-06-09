#!/bin/bash
boot/nux.exe\
    predefined/Predefined{Sig,}.ml -x 'open Predefined' \
    prelude/pervasives.ML\
    prelude/PREL.ML\
    -x 'structure Prel = PREL()'\
    -x 'open Prel'\
    bedlam/bedlam.ml\
    -x 'fn _ => run()'\
    --export $1
