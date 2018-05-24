
start: nfib.test


CXXFLAGS = -g -pipe -Wall -Wno-write-strings -Wno-format -Iruntime

nfib.test: nfib/nfib.exe
	nfib/nfib.exe 25


nfib/nfib.exe : runtime/nml_runtime.o nfib/nfib.o
	g++ $^ -o $@

nfib/nfib.C: nfib/build.sh nfib/nfib.ml gen1/nux.exe
	$<


gen1/nux.exe : runtime/nml_runtime.o gen1/nux.o
	g++ $^ -o $@

gen1/nux.C: scripts/nux-self-compile.sh boot/nux.exe
	$^ "NML-gen1" $@


boot/nux.exe : runtime/nml_runtime.o boot/nux.o
	g++ $^ -o $@

boot/nux.C: boot/nux.ml
	echo 'use "boot/nux.ml";' | sml #sml/nj

%.exe : runtime/nml_runtime.o %.o
	g++ $^ -o $@
