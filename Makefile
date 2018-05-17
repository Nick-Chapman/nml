
# directories for generated .C and .o
genC = ,C
obj = ,obj

.PRECIOUS: ,obj/%.o

CC = g++


#NODEBUG = -DNDEBUG
#PROFILE = -pg

CXXFLAGS= $(PROFILE) $(NODEBUG) -g -pipe -Wall -Wno-write-strings -Wno-format -Iruntime 

regs =\
	ids1\
	ids2\
	ids3\
	ids4\
	data-rep\
	basis-types\
	append-example\
	val-named-ref\
	test-while\
	eval-order-func-arg\
	eval-order-arg-func\
	local-scope\
	assign-order\
	gen-exception\
	gen-exception1\
	raise-order\
	ref-equality\
	excon1\


# just small examples
small = fact even thrice idchain papchain tok tpp nqueens life xstrip ff lpp


all_examples = $(regs) $(small) nux

all_objects = $(addsuffix .o, $(addprefix $(obj)/, $(all_examples)))

top: go/lpp
#top: lpp.test
#top: ff.test
#top: nfib.test
#top: nux.fact
#top: xstrip.test
#top: life.test
#top: nqueens.test
#top: tpp.test
#top: all
#top: go/nux
#top: tok.test
#top: fact.test
#top: tests
#top: regs



nfib.test: go/nfib
	time go/nfib -debug $(XARGS) 30


all: $(addprefix go/, $(all_examples))

regs: $(addprefix go/, $(regs))
	scripts/run.sh $^

tests: $(addsuffix .test, $(small))

ff.test: go/ff
	#go/ff d1d2 w
	#go/ff "d1d2 e1a8 f1h8" "w"
	#go/ff d1 c1 h5 h6 d8 c8 a5 a6 b6 c7 b5 c6 c5 d5   ffw
	#go/ff d1 c1 h5 h6 d8 c8 a5 a6 b6 c7 b5 c6 c5 d5 a4 b4	fw -debug
	go/ff D1 d2 E1 pfw -10meg -debug

xstrip.test: go/xstrip
	go/xstrip "zero(one(two)three(four(five)six seven eight) nine"

nux.fact: go/nux
	go/nux $(XARGS) -1meg -debug predefined/nml_NonPrim.ML  -x 'open NonPrim' ~/project/prelude/{pervasives.ML,PREL.ML} -x 'structure Prel = PREL()' examples/fact.ML -x 'fact_input' --export ,C/fact2.C

nux.factx: go/nux
	go/nux $(XARGS) -debug predefined/nml_NonPrim.ML -x 'open NonPrim' ~/project/prelude/{pervasives.ML,PREL.ML} -x 'structure Prel = PREL()' examples/fact.ML -x 'fact 5'

nux.nux: go/nux
	time go/nux $(XARGS) -debug predefined/nml_NonPrim.ML  -x 'open NonPrim' ~/project/prelude/{pervasives.ML,[A-Z]*.ML} ML/*.ML -x 'val prefixNML = "NML-squared: ";' bind.ML -x Run.nux --export nux2.C


life.test: go/life
	go/life -debug 5

nqueens.test: go/nqueens
	go/nqueens 5

tpp.test: go/tpp
	go/tpp -debug examples/sample.ML 

tok.test: go/tok
	go/tok -debug regs/assign-order.ML 

even.test: go/even
	go/even 2

fact.test: go/fact
	go/fact 6

%.test : go/%
	$<

# dependancy on nml_runtime.h header
$(all_objects) runtime/nml_runtime.o : runtime/nml_runtime.h


# Build the object from the generated C file
$(obj)/%.o : $(genC)/%.C
	$(CXX) -c $(CPPFLAGS) $(CXXFLAGS) $< -o $@


# Build the executable from the obj + runtime.o
go/% : runtime/nml_runtime.o $(obj)/%.o 
	g++ $(PROFILE) $^ -o $@ 


cleano:
	rm runtime/nml_runtime.o $(obj)/* go/*

cleanC: #cleano
	rm $(genC)/*
