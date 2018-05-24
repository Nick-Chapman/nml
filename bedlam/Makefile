
all: nj.run nml.run

# TODO: avoid dupication with stuff in ../Makefile

NJ = sml -Ccm.verbose=false
NJ_ARCH=x86-linux

CXXFLAGS = -g -pipe -Wall -Wno-write-strings -Wno-format -I../runtime

%.exe : ../runtime/nml_runtime.o %.o
	g++ $^ -o $@


# compile bedlam using nml in nj
bedlam.C: ../boot/nml.image.$(NJ_ARCH) nj-nml-load.ml bedlam.ml
	cat nj-nml-load.ml | sml @SMLload=../boot/nml.image

# compile bedlam using nux (alternative)
bedlam2.C: nux-build.sh ../boot/nux.exe bedlam.ml
	./$< $@


# run bedlam with nj (~1sec)
nj.run: nj-load.ml bedlam.ml
	@echo '==================================================[nj]'
	cat nj-load.ml | time $(NJ)


# run bedlam with nml (~67sec) -- time to profile!
nml.run: bedlam.exe
	@echo '==================================================[nml]'
	time ./bedlam.exe
