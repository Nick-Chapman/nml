#!/bin/bash
boot/nux.exe\
    life/life.ml\
    -x 'fn _ => Main.testit TextIO.stdOut' \
    --export $1
