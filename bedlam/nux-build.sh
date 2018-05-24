#create bedlam.C using nux
../boot/nux.exe\
	../predefined/nml_NonPrim.ML\
	-x 'open NonPrim' \
    ../prelude/pervasives.ML\
    ../prelude/PREL.ML\
    -x 'structure Prel = PREL()'\
    -x 'open Prel'\
	bedlam.ml\
    -x 'fn _ => run()'\
    --export $1
