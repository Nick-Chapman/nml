
start: boot gen1 nfib bedlam life

boot: boot/nux.exe
gen1: gen1.cmp
nfib: nfib.run
bedlam: bedlam.run bedlam/bedlam.gprof.out
life: life.cmp life/life.gprof.out

clean:
	git clean -Xf
	rm -f boot/nux.C

PREL = prelude/ASSOC.ML prelude/IMP_HASH.ML prelude/MISCLAY.ML prelude/PAR1.ML prelude/PAR2.ML prelude/PAR3.ML prelude/pervasives.ML prelude/PREL.ML prelude/QLAYOUT.ML prelude/SORT.ML

NML = bind.ML ML/ATOM.ML ML/BASIS.ML ML/BUILTIN.ML ML/CCODE.ML ML/COMPILE3.ML ML/CPS.ML ML/EMBED.ML ML/EVAL3.ML ML/INTERPRETER.ML ML/LANG.ML ML/LEX.ML ML/MACHINE.ML ML/PARSER.ML ML/POS.ML ML/PRETTY.ML ML/PROGRAM.ML ML/RUN.ML ML/tc.ML ML/TOK.ML ML/VALUE.ML

PREDEF = predefined/nml_NonPrim.ML

RUNTIME = runtime/nml_runtime.C runtime/nml_runtime.h

NJ = sml -Ccm.verbose=false
ARCH = x86-linux

RUN = runtime
OPT = -O3 -DNDEBUG
CXXFLAGS = $(OPT) --param inline-unit-growth=100 -Winline -Wall -Wno-write-strings -Wno-format -I$(RUN)

%.nml.C: %.nml.sh boot/nux.exe
	time ./$< $@

%.o : %.nml.C $(RUNTIME)
	time g++ $(CXXFLAGS) -c $< -o $@

%.exe : %.o
	g++ $^ -o $@

%.pg.o : %.nml.C $(RUNTIME)
	time g++ -pg $(CXXFLAGS) -c $< -o $@

%.pg.exe : %.pg.o
	g++ -pg -static $^ -o $@

%.gmon.out: %.pg.exe
	rm -f gmon.out
	time $<
	mv gmon.out $@

%.gprof.out: %.pg.exe %.gmon.out
	rm -f $@
	gprof $^ > $@

# boot

boot/nml.image.$(ARCH): boot/create_nml_image.sml $(PREL) $(NML)
	cat $< | time $(NJ)

boot/nux.C: boot/nux.ml boot/nml.image.$(ARCH) $(PREDEF) $(PREL) $(NML)
	cat $< | time $(NJ) @SMLload=boot/nml.image

boot/nux.o: OPT = # avoid optimize compiler...so slow!
boot/nux.o: $(RUNTIME)

# gen1

gen1.cmp: boot/nux.C gen1/nux.nml.C
	sed s/NML-boot/NML-gen1/ boot/nux.C | cmp - gen1/nux.nml.C

# nfib

nfib/nfib.nml.C : nfib/nfib.ml

nfib.run: nfib/nfib.exe
	nfib/nfib.exe 25

# bedlam

bedlam/bedlam.nml.C : bedlam/bedlam.ml

#bedlam/bedlam.o: OPT = -O3 -DNDEBUG

#bedlam/bedlam.pg.o: OPT = -O3 -DNDEBUG

bedlam.run: bedlam.nj-run bedlam.nml-run

bedlam.nj-run: bedlam/nj-load.ml bedlam/bedlam.ml
	@echo '==================================================[nj]'
	cat $< | time $(NJ)

bedlam.nml-run: bedlam/bedlam.exe
	@echo '==================================================[nml]'
	time $<

# life

life/life.nml.C : life/life.ml

#life/life.o: OPT = -O3 -DNDEBUG

life/life.out : life/life.exe
	time ./$< | tee $@

life.cmp: life/life.out.expected life/life.out
	cmp $^
