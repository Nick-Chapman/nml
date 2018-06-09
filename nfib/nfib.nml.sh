#!/bin/bash
boot/nux.exe \
    predefined/Predefined{Sig,}.ml -x 'open Predefined' \
    prelude/pervasives.ML \
    prelude/PREL.ML \
    -x 'structure Prel = PREL();' \
    nfib/nfib.ml \
    -x nfib_top \
    --export $1
