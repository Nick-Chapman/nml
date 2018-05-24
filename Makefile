
start: nfib.run

NJ = sml -Ccm.verbose=false

CXXFLAGS = -g -pipe -Wall -Wno-write-strings -Wno-format -Iruntime

# Link all executables with the nml runtime
%.exe : runtime/nml_runtime.o %.o
	g++ $^ -o $@


# Create boot/nux using NJ in two steps
boot/nux.C: boot/create_nml_image.sml boot/nux.ml
	cat boot/create_nml_image.sml | $(NJ)
	cat boot/nux.ml | $(NJ) @SMLload=boot/nml.image


#Create gen1/nux using boot/nux
gen1/nux.C: scripts/nux-self-compile.sh boot/nux.exe
	$^ "NML-gen1" $@


# nfib example: build using gen1/nux
nfib/nfib.C: nfib/build.sh boot/nux.exe nfib/nfib.ml 
	$^ $@

nfib.run: nfib/nfib.exe
	nfib/nfib.exe 25
