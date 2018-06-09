(*create nux.C using nml loaded in nj*)
quiet:=true;
Run.Nuse "predefined/PredefinedSig.ml";
Run.Nuse "predefined/Predefined.ml";
Run.Nexec "open Predefined";
Run.Nuse "prelude/pervasives.ML";
map Run.Nuse nml_sources;
Run.Nexec "val prefixNML = \"NML-boot: \";";
Run.Nuse "bind.ML";
Run.Nexec "quiet := false";
quiet:=false;
Run.Nexport "boot/nux.C.gen" "Run.nux";
