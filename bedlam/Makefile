
top: nj.run nml.run bedlam.gprof.out

NJ = sml -Ccm.verbose=false

# run bedlam with nj (~1sec)
nj.run: nj-load.ml bedlam.ml
	@echo '==================================================[nj]'
	cat nj-load.ml | time $(NJ)

RUN = ../runtime

CXXFLAGS = -Wall -Wno-write-strings -Wno-format

OPT = -O3

# Generate bedlam.C

# compile bedlam using nml in nj
#NJ_ARCH=x86-linux
#bedlam.C: ../boot/nml.image.$(NJ_ARCH) nj-nml-load.ml bedlam.ml
#	cat nj-nml-load.ml | sml @SMLload=../boot/nml.image

# compile bedlam using nux (works even with nj installed)
bedlam.C: nux-build.sh ../boot/nux.exe bedlam.ml
	./$< $@


# Normal build & run (no profiling)

%.o : %.C
	g++ $(OPT) -c $(CXXFLAGS) -I$(RUN) $< -o $@

%.exe : %.o
	g++ $^ -o $@

# run bedlam with nml (~67sec) -- time to profile!
nml.run: bedlam.exe
	@echo '==================================================[nml]'
	time ./bedlam.exe


# Build and run with profiling

%.pg.o : %.C
	g++ -pg $(OPT) -c $(CXXFLAGS) -I$(RUN) $< -o $@

%.pg.exe : %.pg.o
	g++ -pg -static $^ -o $@

bedlam.gmon.out: bedlam.pg.exe
	rm -f gmon.out
	./$^
	mv gmon.out $@

bedlam.gprof.out: bedlam.pg.exe bedlam.gmon.out
	rm -f $@
	gprof $^ > $@
