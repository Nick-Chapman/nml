(*create nux.C using nml loaded in nj*)
quiet:=true;
Run.Nuse "predefined/nml_NonPrim.ML";
Run.Nexec "open NonPrim";
map Run.Nuse pervasives;
map Run.Nuse nml_sources;
Run.Nexec "val prefixNML = \"NML-boot: \";";
Run.Nuse "bind.ML";
Run.Nexec "quiet := true";
Run.Nexport "boot/nux.C" "Run.nux";
