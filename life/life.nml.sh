#!/bin/bash
boot/nux.exe\
    predefined/Predefined{Sig,}.ml -x 'open Predefined' \
    life/life.ml\
    -x 'fn _ => Main.testit TextIO.stdOut' \
    --export $1
