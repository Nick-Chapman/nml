# example of running a nml compiled program
mkdir -p ,C ,obj go
echo 'use "run_nml_compile_nfib.ml";' | sml
make go/nfib
go/nfib 20
