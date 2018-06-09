(*create nux.C using nml loaded in nj*)
quiet:=true;
Run.Nuse "predefined/nml_NonPrim.ML";
Run.Nexec "open NonPrim";
Run.Nuse "prelude/pervasives.ML";
map Run.Nuse nml_sources;
Run.Nexec "val prefixNML = \"NML-boot: \";";
Run.Nuse "bind.ML";
Run.Nexec "quiet := false";
quiet:=false;
Run.Nexec "Run.Nuse \"predefined/nml_NonPrim.ML\"";
Run.Nexec "Run.Nexec \"open NonPrim\"";
Run.Nexport "boot/nux.C.gen" "Run.nux";
