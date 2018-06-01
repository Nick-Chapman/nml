#!/bin/bash
boot/nux.exe \
    prelude/pervasives.ML \
    prelude/PREL.ML \
    -x 'structure Prel = PREL();' \
    nfib/nfib.ml \
    -x nfib_top \
    --export $1
