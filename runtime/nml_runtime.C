/*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  96.    Counter
  118.   Some counters
  160.   leaky_alloc
  171.   SiCont -- "si" means "static info"
  196.   SiClosure
  223.   TheArg*, CRET, XRET
  249.   TheFramePointer
  266.   Static Frame Data
  286.   TheStack
  297.   Hob
  320.   BrokenHeart
  346.   HeapSpace
  363.   TheInitHeap
  376.   low-level tagging
  407.   Nword
  448.   equalWord - ML polymorphic equality
  468.   what
  489.   evacuate
  503.   Copy/Evacutate Words
  515.   Closure
  573.   HeapPointer
  579.   heap_alloc
  594.   garbage collection
  666.   stack_alloc
  681.   Jump, Enter
  716.   unit
  742.   Char
  774.   Word
  801.   BoxInt
  838.   String
  865.   lessNumTxt - overloaded - ought to be resolved at compile time!
  897.   istream
  918.   ostream
  941.   Con0, Con1
  1009.  ExCon0, ExCon1
  1080.  unapplied Con0, ExCon0
  1113.  Tuple
  1185.  Value_VectorN
  1222.  ref
  1246.  Value_ArrayN
  1283.  Stack allocated Continuations / Handlers
  1384.  ScavangeStack
  1404.  SetArgsFromFrame_upto
  1422.  Pap
  1481.  Unapplied builtin, of 1 arg
  1519.  Unapplied builtin, of 2 args
  1558.  Unapplied builtin, of 3 args
  1598.  OverApp
  1630.  debug
  1646.  callFunc (arg count check) - variable num_actual_args
  1680.  g_mk*
  1700.  g_match*
  1732.  g_* - Con/ExCon - build/destruct
  1780.  g_*
  1882.  util builders
  1906.  builtin (top-level)
  1980.  builtin_Vector*
  2005.  builtin_Array*
  2036.  BitsOf
  2059.  builtin_Word*
  2145.  builtin_TextIO*
  2201.  builtin_String*
  2212.  builtin_Char*
  2229.  print_stats
  2303.  StopExecution / CatchAny
  2325.  main
 CONTENTS-END-LINE:
 ------------------------------------------------------------------------------*/

#include "nml_runtime.h"
#include <fstream>
#include <cassert>
#include <iostream>
#include <cstdlib>

using namespace std;

bool debug = false;

bool show_progress = false;
bool show_alloc_progress = false;

#define ABORT { assert(0); exit(1); }
#define NOT_YET { assert(0); exit(1); }
#define TYPE_ERROR { assert(0); exit(1); }

void print_stats(char* tag); //forward

const unsigned OneK = 1000;
const unsigned OneMeg = 1000000;

Ncode ReturnWith(Nword res); // forward

//----------------------------------------------------------------------
//INDEX: Counter
//----------------------------------------------------------------------

class Counter {
public:
  //enum { scale = 1000 };
	enum { scale = 1 };
	std::string name;
	unsigned count;
public:
	Counter(std::string name_) : name(name_), count(0) {
	  //printf("create: Counter(%s)\n",name_.c_str());
	}
	void inc(unsigned n) { count += n; }
private:
	Counter(Counter&);
};

ostream& operator<< (ostream& os, const Counter& c) {
	os << c.name << "=" << c.count / Counter::scale;
	return os;
}

//----------------------------------------------------------------------
//INDEX: Some counters
//----------------------------------------------------------------------

Counter LeakyAllocation("L");
Counter HeapAllocation("H");
//Counter StackAllocation("S");

Counter EvacuationAllocation("GC");


//Level StackDepth("D");

Counter si_allocation("SI");

//Counter control_allocation("C");
//Counter handle_allocation("H");
//Counter overapp_allocation("X"); // X for extra args

Counter fn_allocation("F"); // F for fn (was: L for Lambda!)
Counter tuple_allocation("T");

//Counter data_allocation("D");

Counter unit_data_allocation("Du");
Counter char_data_allocation("Dc");
Counter word_data_allocation("Dw");
Counter num_data_allocation("Dn");

Counter string_data_allocation("Ds");

Counter io_data_allocation("Dio");
Counter con0_data_allocation("Dc0");
Counter con1_data_allocation("Dc1");
Counter excon0_data_allocation("Dx0");
Counter excon1_data_allocation("Dx1");
Counter vector_data_allocation("Dv");
Counter array_data_allocation("Da");

Counter ref_allocation("R");
Counter pap_allocation("P");
Counter unappliedB_allocation("UB"); //unapplied builtins
Counter unappliedC_allocation("UC"); //unapplied data constructors

//----------------------------------------------------------------------
//INDEX: leaky_alloc
//----------------------------------------------------------------------

void* leaky_alloc(Counter& counter, size_t size) {
	assert(size%4==0);
	counter.inc(size);
	LeakyAllocation.inc(size);
	return new char[size];
}

//----------------------------------------------------------------------
//INDEX: SiCont -- "si" means "static info"
//----------------------------------------------------------------------

class SiCont { 
public:
	char* name;
	Ncode code;
	unsigned frame_size;
public:
	SiCont(char* name_, Ncode code_, unsigned frame_size_) :
		name(name_),
		code(code_),
		frame_size(frame_size_) {
	}
	static void* operator new(size_t size, Counter& counter) {
		return leaky_alloc(counter,size);
	}
};

SiCont* makeSiCont(//Counter& counter, 
				   char* name, Ncode code, unsigned frame_size) {
	return new (si_allocation) SiCont(name,code,frame_size);
}

//----------------------------------------------------------------------
//INDEX: SiClosure
//----------------------------------------------------------------------

class SiClosure {
public:
	char* name;
	Ncode code;
	unsigned frame_size;
	unsigned num_args;
public:
	SiClosure(char* name_, Ncode code_, unsigned frame_size_, unsigned num_args_)
		: name(name_), code(code_), frame_size(frame_size_), num_args(num_args_) {}
	static void* operator new(size_t size, Counter& counter) {
		return leaky_alloc(counter,size);
	}
	string what() {
		static char buf[20]; 
		sprintf(buf,"F=%d,A=%d",frame_size,num_args);
		return string(name) + "/" + string(buf);
	}
};

SiClosure* makeSiClosure(Counter& counter, char* name, Ncode code, unsigned frame_size, unsigned num_args) {
	return new (counter) SiClosure(name,code,frame_size,num_args);
}

//----------------------------------------------------------------------
//INDEX: TheArg*, CRET, XRET
//----------------------------------------------------------------------

Nword TheArg0;
Nword TheArg1;
Nword TheArg2;
Nword TheArg3;
Nword TheArg4;
Nword TheArg5;
Nword TheArg6;
Nword TheArg7;

Nword& GetArgVar(unsigned n) {
	if (n==0) { return TheArg0; }
	if (n==1) { return TheArg1; }
	if (n==2) { return TheArg2; }
	if (n==3) { return TheArg3; }
	if (n==4) { return TheArg4; }
	cout << "**GetArgVar(not yet supported): " << n << endl;
	NOT_YET;
}

Nword CRET;
Nword XRET;

//----------------------------------------------------------------------
//INDEX: TheFramePointer
//----------------------------------------------------------------------

unsigned TheFrameSize = 0;
Nword* TheFramePointer;

void SetFrameReference(unsigned size, Nword* words) {
	TheFrameSize = size;
	TheFramePointer = words;
}

Nword FRAME(unsigned i) {
	assert(i < TheFrameSize);
	return TheFramePointer[i];
}

//----------------------------------------------------------------------
//INDEX: Static Frame Data
//----------------------------------------------------------------------

const unsigned MaxFrameSize = 100;
Nword StaticFrameWords[MaxFrameSize];

unsigned maxRequiredStaticFrameSize = 0;

Counter CopiedFrameWords("CopiedFrameWords");

void CopyTheFrame(unsigned size, Nword* words) {
	if (size>maxRequiredStaticFrameSize) { maxRequiredStaticFrameSize = size; } // collect info
	CopiedFrameWords.inc(size);
	for (unsigned i = 0; i<size; ++i) {
		StaticFrameWords[i] = words[i];
	}
	SetFrameReference(size,StaticFrameWords);
}

//----------------------------------------------------------------------
//INDEX: TheStack
//----------------------------------------------------------------------

//const unsigned MaxStackSize = 2000;
const unsigned MaxStackSize = OneMeg;
char TheStack[MaxStackSize];
char* const StackTop = &TheStack[MaxStackSize];
char* StackPointer = StackTop; //stack grows downwards
char* StackLowWater = StackTop;

//----------------------------------------------------------------------
//INDEX: Hob
//----------------------------------------------------------------------

void ScavangeStack(); //forward

void* heap_alloc(Counter& counter, size_t size); //forard

class Hob {
public:
	virtual ~Hob() {}
	virtual std::string what() =0;
	virtual bool equalTo(Nword) =0;
	static void* operator new(size_t size, Counter& counter) {
		return heap_alloc(counter,size);
	}
	virtual unsigned bytes() =0;
	virtual Hob* evacuate() =0;
	virtual void scavenge() =0;
};

#define HobBytes(N) (sizeof(*this) + N * sizeof(Nword))

//----------------------------------------------------------------------
//INDEX: BrokenHeart
//----------------------------------------------------------------------

class BrokenHeart : public Hob {
	Hob* m_forwardingPointer;
public:
	BrokenHeart(Hob* hob) : m_forwardingPointer(hob) {}
	static void* operator new(size_t size, void* p) {
		assert(size == sizeof(BrokenHeart));
		return p;
	}
	std::string what() { return "BrokenHeart"; }
	bool equalTo(Nword v) { ABORT; }
	unsigned bytes() { ABORT; }
	Hob* evacuate() { return m_forwardingPointer; }
	void scavenge() { ABORT; }
	//bool is_broken_heart() { return true; }
	//unsigned bytes() { return HobBytes(0); } //NO - need to account for teh size of teh onject over-writtem
	//unsigned bytes() { return m_forwardingPointer->bytes(); } //needed for invalidate loop
};

void SetBrokenHeart(Hob* before, Hob* after) {
	new (before) BrokenHeart(after); //placement new - to overwrite old object with BrokenHeart
}

//----------------------------------------------------------------------
//INDEX: HeapSpace
//----------------------------------------------------------------------

class HeapSpace {
public:
	char* base;
	char* top;
public:
    HeapSpace() : base(0), top(0) {}
	HeapSpace(char* base_, char* top_) : base(base_), top(top_) {
	  //printf("HeapSpace(%x,%x)\n",base,top);
	}
	bool inSpace(Hob* hob) {
		char* hp = reinterpret_cast<char*>(hob);
		return (base <= hp) && (hp < top);
	}
};

//----------------------------------------------------------------------
//INDEX: TheInitHeap
//----------------------------------------------------------------------

//const unsigned InitHeapSize = 100*OneK;
const unsigned InitHeapSize = OneMeg;
char TheInitHeap[InitHeapSize];
char* const InitHeapTop = &TheInitHeap[InitHeapSize];

HeapSpace initSpace(TheInitHeap,TheInitHeap+InitHeapSize);

HeapSpace allocSpace = initSpace;
HeapSpace currentSpace;

//----------------------------------------------------------------------
//INDEX: low-level tagging
//----------------------------------------------------------------------

bool isTaggedPointer(xint raw) { return (raw & ~255) && !(raw & 3); } //two LSM bits are zero & value>255; property of pointer
xint TagPointer(Hob* pointer) {
	xint raw = reinterpret_cast<xint>(pointer); //unchanged
	assert(isTaggedPointer(raw));
	return raw;
}

xint TagInt(xint i) {
	assert ((static_cast<unsigned>(i) >> 30 == 0) //two MSB bit are the same (i.e. we have a 31bit int)
			|| (static_cast<unsigned>(i) >> 30 == 3));
	return (i << 1) | 1; // shift & tag
}

xint TagUnsigned(unsigned u) {
	assert(!(u>>30)); //two MSB bit are zero (i.e. we have a 30bit unsigned)
	return (u << 1) | 1; // shift & tag
}

xint TagChar(char c) {
	return static_cast<xint>(c); //unchanged
}

Hob* UnTagPointer(xint raw) { return reinterpret_cast<Hob*>(raw); }
int UnTagInt(xint raw) { return raw >> 1; }
unsigned UnTagUnsigned(xint raw) { return raw >> 1; }
char UnTagChar(xint raw) { assert(raw<=255); return static_cast<char>(raw); }

//----------------------------------------------------------------------
//INDEX: Nword
//----------------------------------------------------------------------

Nword::Nword(Hob* hob) : _raw(TagPointer(hob)) {}

bool isPointer(Nword w) { return isTaggedPointer(w._raw); }

Hob* getPointer(Nword w) {
	assert(isPointer(w));
	Hob* hob = UnTagPointer(w._raw); 
	assert(hob);
	assert(initSpace.inSpace(hob) || currentSpace.inSpace(hob));
	return hob;
}

Nword Nword::fromRawUnboxed(xint raw) { return Nword(raw); }
Nword Nword::fromInt(int i) { return Nword(TagInt(i)); }
Nword Nword::fromUnsigned(unsigned u) { return Nword(TagUnsigned(u)); }
Nword Nword::fromChar(char c) { return Nword(TagChar(c)); }

xint Nword::getRawUnboxed(Nword w) { 
	assert(!isPointer(w));
	return w._raw;
}

int Nword::getInt(Nword w) {
	assert(!isPointer(w));
	return UnTagInt(w._raw);
}

unsigned Nword::getUnsigned(Nword w) {
	assert(!isPointer(w));
	return UnTagUnsigned(w._raw);
}

char Nword::getChar(Nword w) {
	assert(!isPointer(w));
	return UnTagChar(w._raw);
}

//----------------------------------------------------------------------
//INDEX: equalWord - ML polymorphic equality
//----------------------------------------------------------------------

bool equalWord (Nword w1, Nword w2) {
	if (!isPointer(w1)) {
		if (!isPointer(w2)) {
			return (Nword::getRawUnboxed(w1) == Nword::getRawUnboxed(w2));
		} else {
			return false;
		}
	} else {
		if (!isPointer(w2)) {
			return false;
		} else {
			return getPointer(w1)->equalTo(w2);
		}
	}
}

//----------------------------------------------------------------------
//INDEX: what
//----------------------------------------------------------------------

string what(Nword w) { 
	if (isPointer(w)) {
		return getPointer(w)->what(); 
	} else {
		char buf[20];
		sprintf(buf,"%d",Nword::getRawUnboxed(w));
		return "Unboxed<" + string(buf) + ">";
	}
}

ostream& operator<< (ostream& os, Nword w) {
	if (isPointer(w)) {
		os << "<Hob:" << getPointer(w) << " = " << getPointer(w)->what() << ">";
	} else {
		os << "<Unboxed:" << Nword::getRawUnboxed(w) << ">";
	}
	return os;
}

//----------------------------------------------------------------------
//INDEX: evacuate
//----------------------------------------------------------------------

void top_evacuate(Nword& w) {
	if (!isPointer(w)) { return; }
	Hob* before = getPointer(w);
	assert(before);
	if (initSpace.inSpace(before)) { return; }
	Hob* after = before->evacuate();
	SetBrokenHeart(before,after); // ought to avoid repeatedly setting this broken heart
	w = after;
}

//----------------------------------------------------------------------
//INDEX: Copy/Evacutate Words
//----------------------------------------------------------------------

void CopyWords(unsigned size, Nword* from, Nword* to) {
	for (unsigned i=0; i<size; ++i) { to[i] = from[i]; }
}

void EvacuateWords(unsigned size, Nword* words) {
	for (unsigned i = 0; i < size; ++i) { top_evacuate(words[i]); }
}

//----------------------------------------------------------------------
//INDEX: Closure
//----------------------------------------------------------------------

class Closure;
Closure* makeClosure(Counter& counter, SiClosure* si); //forward

class Closure : public Hob {
public:
	SiClosure* si;
private:
	Nword _words[];
public:
	Closure(SiClosure* si_) : si(si_) {}
	static void* operator new(size_t size, Counter& counter, unsigned extra_words) {
		return heap_alloc(counter,size+extra_words*sizeof(Nword));
	}
	std::string what() { return "Closure/"+si->what(); }
	Nword& frameElem(unsigned i) {
		assert(i<si->frame_size);
		return _words[i];
	}
	void SetFrameReference() {
		::SetFrameReference(si->frame_size, _words);
		//::CopyTheFrame(si->frame_size, _words);
	}
	bool equalTo(Nword w) { TYPE_ERROR; }
public:
	unsigned bytes() { return HobBytes(si->frame_size); }
	Hob* evacuate() {
		Closure* res = makeClosure(EvacuationAllocation,si);
		CopyWords(si->frame_size,_words,res->_words);
		return res;
	}
	void scavenge() {
		EvacuateWords(si->frame_size,_words);
	}
};

Closure* makeClosure(Counter& counter, SiClosure* si) {
	return new (counter,si->frame_size) Closure(si);
}

Closure* getClosure(Nword w) {
	//assert(w);
	if (Closure* x = dynamic_cast<Closure*>(getPointer(w))) { return x; }
	cout << "ERROR/getClosure: " << what(w) << " -- " << getPointer(w) << endl;
	TYPE_ERROR;
}

void SetClosureFrameElem(Nword func, unsigned n, Nword v) {
	getClosure(func)->frameElem(n) = v;
}

Nword TheCurrentFunction; // = 0;

#define TheCurrentSiClosure (getClosure(TheCurrentFunction)->si)

//----------------------------------------------------------------------
//INDEX: HeapPointer
//----------------------------------------------------------------------

char* HeapPointer = TheInitHeap; //where allocation occurs. The heap grows upwards.

//----------------------------------------------------------------------
//INDEX: heap_alloc
//----------------------------------------------------------------------

void* heap_alloc(Counter& counter, size_t size) {
	// printf("heap_alloc(%s,%d)\n",counter.name.c_str(),size);
	// printf("HeapPointer=%x\n",HeapPointer);
	// printf("base=%x\n",allocSpace.base);
	// printf("top=%x\n",allocSpace.top);

	assert(size%4==0);
	counter.inc(size);
	HeapAllocation.inc(size);
	void* result = HeapPointer;
	assert(allocSpace.base <= HeapPointer);
	HeapPointer += size;
	assert(HeapPointer < allocSpace.top);
	return result;
}

//----------------------------------------------------------------------
//INDEX: garbage collection
//----------------------------------------------------------------------

HeapSpace HeapSpaceA;
HeapSpace HeapSpaceB;

unsigned gc_count = 0;

void gc() {

	++ gc_count;

	cout << "***GC(" << gc_count << ")... ";
	flush(cout);

	//unsigned unusedSpaceAtEndOfHeap = currentSpace.top - HeapPointer;

	static bool inA = true; // flip between two spaces

	allocSpace = inA ? HeapSpaceA : HeapSpaceB;
  	inA = !inA;
	HeapPointer = allocSpace.base;

	// evacuate roots; roots depend on on whether we are in closure/continuation/handler
	bool inContinuation = isPointer(CRET);
	bool inHandler = isPointer(XRET);

	//cout << "-gc: evacuate roots\n";
	if (inContinuation || inHandler) {
		if (inContinuation) { 
			top_evacuate(CRET);  //comment-out for a BUG
		} else { 
			top_evacuate(XRET); 
		}
		for (unsigned i = 0; i < TheFrameSize; ++i) {
			top_evacuate(TheFramePointer[i]);
		}
	} else {
		unsigned N = TheCurrentSiClosure->num_args;
		for (unsigned i = 0; i < N ; ++i) {
			top_evacuate(GetArgVar(i));
		}
		top_evacuate(TheCurrentFunction);
	}
	
	//cout << "- gc: scavange stack\n";
	ScavangeStack();

	//cout << "- gc: scavange init heap\n";
	for (char* hp = initSpace.base; hp < initSpace.top; ) {
		Hob* hob = reinterpret_cast<Hob*>(hp);
		hob->scavenge();
		hp += hob->bytes();
	}

	//cout << "- gc: main scavange loop\n";
	for (char* hp = allocSpace.base; hp < HeapPointer; ) {
		Hob* hob = reinterpret_cast<Hob*>(hp);
		hob->scavenge();
		hp += hob->bytes();
	}

	unsigned Q = currentSpace.top - currentSpace.base;
	unsigned L = HeapPointer - allocSpace.base;

	//cout << "***GC(" << gc_count << ")... ";
	cout << Q << " -> " << L << " [" << (100*L/Q) << "%]" << endl;

	currentSpace = allocSpace;
}

//----------------------------------------------------------------------
//INDEX: stack_alloc
//----------------------------------------------------------------------

void* stack_alloc(//Counter& counter, 
				  size_t size) {
	assert(size%4==0);
	//counter.inc(size);
	//StackAllocation.inc(size);
	StackPointer -= size;
	assert(StackPointer>=TheStack); //stack-overflow - allocate another stack chunck!
	if (StackPointer < StackLowWater) { StackLowWater = StackPointer; } // just for stats
	return StackPointer;
}

//----------------------------------------------------------------------
//INDEX: Jump, Enter
//----------------------------------------------------------------------

const unsigned MaxAlloctaionWhichMightOccurBeforeNextJump = 2000;

void  maybe_gc() {
	// We only dare do GC when we are jumping from one code sequence to
	// another because it is the only time we can find all the roots.
	bool need2gc = HeapPointer + MaxAlloctaionWhichMightOccurBeforeNextJump > currentSpace.top;
	if (need2gc) { gc(); }
}

Ncode Jump(SiCont* si, Nword* words) {
	if (show_progress) { 
		cout << "**Jump: " << si->name << endl; 
	}
	CopyTheFrame (si->frame_size, words);
	maybe_gc();
	return si->code;
}

Ncode Enter(Closure* closure) {
	if (show_progress) {
		char* name = closure->si->name;
		cout << "**Enter: " << name << endl;
	}
	TheCurrentFunction = closure;
	CRET = Nword::fromRawUnboxed(0); XRET = Nword::fromRawUnboxed(0);
	Ncode code = closure->si->code;
	maybe_gc();
	getClosure(TheCurrentFunction)->SetFrameReference(); // must be after any gc!
	return code;
}

//----------------------------------------------------------------------
//INDEX: unit
//----------------------------------------------------------------------

Hob* get_unit();

class Value_unit : public Hob {
public:
	Value_unit() {}
	std::string what() { return "unit"; }
	bool equalTo(Nword w) { return true; } // because of type safety
	unsigned bytes() { return HobBytes(0); }
	Hob* evacuate() { return get_unit(); }
	void scavenge() {}
};

//Hob* the_unit = new (unit_data_allocation) Value_unit();
Hob* get_unit() {
  static Hob* the_unit = new (unit_data_allocation) Value_unit();

	return the_unit;
}

//  Nword get_unit() {
//  	static Nword the_unit = new (unit_data_allocation) Value_unit();
//  	return the_unit;
//  }

//----------------------------------------------------------------------
//INDEX: Char
//----------------------------------------------------------------------

char getChar(Nword w); //forward

//  class Value_Char : public Hob {
//  public:
//  	char _c;
//  	Value_Char(char c) : _c(c) {}
//  	std::string what() { return "Char"; }
//  	bool equalTo(Nword v) { return _c == getChar(v); }
//  	unsigned bytes() { return HobBytes(0); }
//  	Hob* evacuate() { return new (EvacuationAllocation) Value_Char(_c); }
//  	void scavenge() {}
//  };

char getChar(Nword w) {
	//if (Value_Char* x = dynamic_cast<Value_Char*>(getPointer(w))) { return x->_c; }
	//if (!isPointer(w)) { return getUnsigned(w); }
	//if (!isPointer(w)) { return getChar(w); }
	return Nword::getChar(w);
	//cout << "ERROR/getChar: " << what(w) << endl;
	//TYPE_ERROR;
}

Nword makeChar(char c) { 
	//return new (char_data_allocation) Value_Char(c);
	//return Nword::fromUnsigned(c);
	return Nword::fromChar(c);
}

//----------------------------------------------------------------------
//INDEX: Word
//----------------------------------------------------------------------

unsigned getWord(Nword w); //forward

class Value_Word : public Hob {
public:
	unsigned _n;
	Value_Word(unsigned n) : _n(n) {}
	std::string what() { return "Word"; }
	bool equalTo(Nword v) { return _n == getWord(v); }
	unsigned bytes() { return HobBytes(0); }
	Hob* evacuate() { return new (EvacuationAllocation) Value_Word(_n); }
	void scavenge() {}
};

unsigned getWord(Nword w) {
	if (Value_Word* x = dynamic_cast<Value_Word*>(getPointer(w))) { return x->_n; }
	cout << "ERROR/getWord: " << what(w) << endl;
	TYPE_ERROR;
}

Nword makeWord(unsigned n) {
	return new (word_data_allocation) Value_Word(n);
}

//----------------------------------------------------------------------
//INDEX: BoxInt
//----------------------------------------------------------------------

int getInt(Nword w); //forward

//  class Value_BoxInt : public Hob {
//  public:
//  	int _n;
//  	Value_BoxInt(int n) : _n(n) {}
//  	std::string what() { return "BoxInt"; }
//  	bool equalTo(Nword v) { return _n == getInt(v); }
//  	unsigned bytes() { return HobBytes(0); }
//  	Hob* evacuate() { return new (EvacuationAllocation) Value_BoxInt(_n); }
//  	void scavenge() {}
//  };

int getInt(Nword w) {
	//if (Value_BoxInt* x = dynamic_cast<Value_BoxInt*>(getPointer(w))) { return x->_n; }
	//cout << "ERROR/getInt: " << what(w) << endl;
	//TYPE_ERROR;
	assert(!isPointer(w));
//  	unsigned u = Nword::getUnsigned(w);
//  	//int i = static_cast<int>(u) - (1<<30);
//  	int i = static_cast<int>(u);
//  	return i;
	return Nword::getInt(w);
}

Nword makeInt(int i) {
	//return new (num_data_allocation) Value_BoxInt(i); //boxed
	//unsigned u = static_cast<unsigned>(i+(1<<30));
	//unsigned u = static_cast<unsigned>(i);
	//return Nword::fromUnsigned(u);
	return Nword::fromInt(i);
}

//----------------------------------------------------------------------
//INDEX: String
//----------------------------------------------------------------------

std::string getString(Nword w); //forward

class Value_String : public Hob {
public:
	std::string _s;
	Value_String(std::string s) : _s(s) {}
	std::string what() { return "String"; }
	bool equalTo(Nword v) { return _s == getString(v); }
	unsigned bytes() { return HobBytes(0); }
	Hob* evacuate() { return new (EvacuationAllocation) Value_String(_s); }
	void scavenge() {}
};

std::string getString(Nword w) {
	if (Value_String* x = dynamic_cast<Value_String*>(getPointer(w))) { return x->_s; }
	cout << "ERROR/getString: " << what(w) << endl;
	TYPE_ERROR;
}

Nword makeString(std::string s) {
	return new (string_data_allocation) Value_String(s);
}

//----------------------------------------------------------------------
//INDEX: lessNumTxt - overloaded - ought to be resolved at compile time!
//----------------------------------------------------------------------

// Would this be better recoded to dispatch?
bool lessNumTxt(Nword w1, Nword w2) {
	if (!isPointer(w1)) {
		return Nword::getRawUnboxed(w1) < Nword::getRawUnboxed(w2);
	}
	if (Value_Word* x1 = dynamic_cast<Value_Word*>(getPointer(w1))) {
		if (Value_Word* x2 = dynamic_cast<Value_Word*>(getPointer(w2))) {
			return (x1->_n < x2->_n);
		}
	}
	else if (Value_String* x1 = dynamic_cast<Value_String*>(getPointer(w1))) {
		if (Value_String* x2 = dynamic_cast<Value_String*>(getPointer(w2))) {
			return (x1->_s < x2->_s);
		}
	}
//  	else if (Value_Char* x1 = dynamic_cast<Value_Char*>(getPointer(w1))) {
//  		if (Value_Char* x2 = dynamic_cast<Value_Char*>(getPointer(w2))) {
//  			return (x1->_c < x2->_c);
//  		}
//  	}
//  	else if (Value_BoxInt* x1 = dynamic_cast<Value_BoxInt*>(getPointer(w1))) {
//  		if (Value_BoxInt* x2 = dynamic_cast<Value_BoxInt*>(getPointer(w2))) {
//  			return (x1->_n < x2->_n);
//  		}
//  	}
	TYPE_ERROR;
}

//----------------------------------------------------------------------
//INDEX: istream
//----------------------------------------------------------------------

class Value_instream : public Hob {
public:
	std::istream& _is;
	Value_instream(std::istream& is) : _is(is) {}
	std::string what() { return "instream"; }
	bool equalTo(Nword w) { TYPE_ERROR; }
	unsigned bytes() { return HobBytes(0); }
	Hob* evacuate() { return new (EvacuationAllocation) Value_instream(_is); }
	void scavenge() {}
};

std::istream& getInstream(Nword w) {
	if (Value_instream* x = dynamic_cast<Value_instream*>(getPointer(w))) { return x->_is; }
	cout << "ERROR/getInstream: " << what(w) << endl;
	TYPE_ERROR;
}

//----------------------------------------------------------------------
//INDEX: ostream
//----------------------------------------------------------------------

class Value_outstream : public Hob {
public:
	std::ostream& _os;
	Value_outstream(std::ostream& os) : _os(os) {}
	std::string what() { return "outstream"; }
	bool equalTo(Nword w) { TYPE_ERROR; }
	unsigned bytes() { return HobBytes(0); }
	Hob* evacuate() { return new (EvacuationAllocation) Value_outstream(_os); }
	void scavenge() {}
};

std::ostream& getOutstream(Nword w) {
	if (Value_outstream* x = dynamic_cast<Value_outstream*>(getPointer(w))) { return x->_os; }
	cout << "ERROR/getOutstream: " << what(w) << endl;
	TYPE_ERROR;
}

Nword the_stdOut = new (io_data_allocation) Value_outstream(cout);

//----------------------------------------------------------------------
//INDEX: Con0, Con1
//----------------------------------------------------------------------

unsigned getTag(Nword); //forward
Nword getCon(Nword w); //forward

class Value_Con0 : public Hob {
public:
	unsigned tag;
	Value_Con0(unsigned tag_) : tag(tag_) {}
	std::string what() { return "Con0"; }
	bool equalTo(Nword v) { return tag == getTag(v); }
	unsigned bytes() { return HobBytes(0); }
	Hob* evacuate() { return new (EvacuationAllocation) Value_Con0(tag); } 
	void scavenge() {}
};

class Value_Con1 : public Hob {
public:
	Nword word;
	Value_Con1(Nword word_) : word(word_) {}
	std::string what() { return "Con1<"+::what(word)+">"; }
	virtual unsigned tag() = 0;
	//bool equalTo(Nword v) { return tag() == getTag(v) && getPointer(word)->equalTo(getCon(v)); }
	bool equalTo(Nword v) { return tag() == getTag(v) && equalWord(word,getCon(v)); }
	unsigned bytes() { return HobBytes(0); }
	void scavenge() { ::top_evacuate(word); }
};

template<unsigned TAG>
class Value_Con1_TAG : public Value_Con1 {
public:
	Value_Con1_TAG(Nword word_) : Value_Con1(word_) {}
	unsigned tag() { return TAG; }
	Hob* evacuate() { return new (EvacuationAllocation) Value_Con1_TAG<TAG>(word); }
};

unsigned getTag(Nword w) {
	if (!isPointer(w)) { return Nword::getUnsigned(w); }
	if (Value_Con0* x = dynamic_cast<Value_Con0*>(getPointer(w))) { return x->tag; }
	if (Value_Con1* x = dynamic_cast<Value_Con1*>(getPointer(w))) { return x->tag(); }
	cout << "ERROR/getTag: " << what(w) << endl;
	TYPE_ERROR;
}

Nword getCon(Nword w) {
	if (Value_Con1* x = dynamic_cast<Value_Con1*>(getPointer(w))) { return x->word; }
	cout << "ERROR/getCon: " << what(w) << endl;
	TYPE_ERROR;
}

Nword makeCon0(unsigned tag) {
	//return new (con0_data_allocation) Value_Con0(tag); //boxed
	return Nword::fromUnsigned(tag); //unboxed
}

Nword makeCon1(unsigned tag, Nword w) {
	switch(tag) {
#define TAG(T) case T: { return new (con1_data_allocation) Value_Con1_TAG<T>(w); }
	TAG (0); TAG (1); TAG (2); TAG (3); TAG (4); TAG (5); TAG (6); TAG (7); TAG (8); TAG(9);
	TAG(10); TAG(11); TAG(12); TAG(13); TAG(14); TAG(15); TAG(16); TAG(17); TAG(18); TAG(19);
	}
	cout << "**makeCon1(not yet supported): " << tag << endl;
	NOT_YET;
	//return new (con1_data_allocation) Value_Con1(tag,w);
}

//----------------------------------------------------------------------
//INDEX: ExCon0, ExCon1
//----------------------------------------------------------------------

unsigned getExTag(Nword w);
Nword getExCon(Nword w);

class Value_ExCon0 : public Hob {
public:
	std::string name;
	unsigned tag;
	Value_ExCon0(std::string name_, unsigned tag_) : name(name_), tag(tag_) {}
	std::string what() { return "ExCon0-"+name; }
	bool equalTo(Nword v) { return tag == getExTag(v); }
	unsigned bytes() { return HobBytes(0); }
	Hob* evacuate() { return new (EvacuationAllocation) Value_ExCon0(name,tag); }
	void scavenge() {}
};

class Value_ExCon1 : public Hob {
public:
	std::string name;
	unsigned tag;
	Nword word;
	Value_ExCon1(std::string name_, unsigned tag_, Nword word_) : name(name_), tag(tag_), word(word_) {}
	std::string what() { return "ExCon1-"+name; }
	//bool equalTo(Nword v) { return tag == getExTag(v) && getPointer(word)->equalTo(getExCon(v)); }
	bool equalTo(Nword v) { return tag == getExTag(v) && equalWord(word,getExCon(v)); }
	unsigned bytes() { return HobBytes(0); }
	Hob* evacuate() { return new (EvacuationAllocation) Value_ExCon1(name,tag,word); }
	void scavenge() { ::top_evacuate(word); }
};

std::string getExName(Nword w) {
	if (Value_ExCon0* x = dynamic_cast<Value_ExCon0*>(getPointer(w))) { return x->name; }
	if (Value_ExCon1* x = dynamic_cast<Value_ExCon1*>(getPointer(w))) { return x->name; }
	if (Closure* c = dynamic_cast<Closure*>(getPointer(w))) { 
		if (Value_ExCon0* x = dynamic_cast<Value_ExCon0*>(getPointer(c->frameElem(0)))) { 
			return x->name; 
		}
	}
	cout << "ERROR/getExName: " << what(w) << endl;
	TYPE_ERROR;
}

unsigned getExTag(Nword w) {
	if (Value_ExCon0* x = dynamic_cast<Value_ExCon0*>(getPointer(w))) { return x->tag; }
	if (Value_ExCon1* x = dynamic_cast<Value_ExCon1*>(getPointer(w))) { return x->tag; }
	if (Closure* c = dynamic_cast<Closure*>(getPointer(w))) { 
		if (Value_ExCon0* x = dynamic_cast<Value_ExCon0*>(getPointer(c->frameElem(0)))) { 
			return x->tag; 
		}
	}
	cout << "ERROR/getExName: " << what(w) << endl;
	TYPE_ERROR;
}

Nword getExCon(Nword w) {
	if (Value_ExCon1* x = dynamic_cast<Value_ExCon1*>(getPointer(w))) { return x->word; }
	cout << "ERROR/getExCon: " << what(w) << endl;
	TYPE_ERROR;
}

Nword makeExCon0(std::string name, unsigned tag) {
	return new (excon0_data_allocation) Value_ExCon0(name,tag);
}

Nword makeExCon1(std::string name, unsigned tag, Nword w) {
	return new (excon1_data_allocation) Value_ExCon1(name,tag,w);
}

//----------------------------------------------------------------------
//INDEX: unapplied Con0, ExCon0
//----------------------------------------------------------------------

Ncode ApplyCon0 () {
	unsigned tag = getTag(FRAME(0));
	Nword res = makeCon1(tag,ARG(0));
	return ReturnWith(res);
}

SiClosure SiApplyCon0 = SiClosure("<ApplyCon0>",Ncode(ApplyCon0),1,1);

Closure* makeCon0_closure(unsigned tag) {
	Closure* clo = makeClosure(unappliedC_allocation,&SiApplyCon0);
	clo->frameElem(0) = makeCon0(tag);
	return clo;
}

Ncode ApplyExCon0 () {
	unsigned tag = getExTag(FRAME(0));
	std::string name = getExName(FRAME(0));
	Nword res = makeExCon1(name,tag,ARG(0));
	return ReturnWith(res);
}

SiClosure SiApplyExCon0 = SiClosure("<ApplyExCon0>",Ncode(ApplyExCon0),1,1);

Closure* makeExCon0_closure(std::string name, unsigned tag) {
	Closure* clo = makeClosure(unappliedC_allocation,&SiApplyExCon0);
	clo->frameElem(0) = makeExCon0(name,tag);
	return clo;
}

//----------------------------------------------------------------------
//INDEX: Tuple
//----------------------------------------------------------------------

class Value_Tuple;

Value_Tuple* getTuple(Nword);

class Value_Tuple : public Hob {
protected:
	Nword _words[];
public:
	std::string what() { return "Tuple"; }
	bool equalTo(Nword v) {
		Value_Tuple* t = getTuple(v);
		for (unsigned i=0; i<size(); ++i) {
			if (!equalWord(tupleElem(i),t->tupleElem(i))) return false;
		}
		return true;
	}
	virtual unsigned size() =0;
	Nword& tupleElem(unsigned n) {
		assert(n<size());
		return _words[n];
	}
};

template<unsigned N>
class Value_Tuple_N : public Value_Tuple {
public:
	Value_Tuple_N() {}
	static void* operator new(size_t size, Counter& counter, unsigned extra_words) {
		return heap_alloc(counter,size+extra_words*sizeof(Nword));
	}
	unsigned size() { return N; }
	unsigned bytes() { 
		return HobBytes(N);
	}
	Hob* evacuate() {
		Value_Tuple_N<N>* res = new (EvacuationAllocation,N) Value_Tuple_N<N>();
		CopyWords(N,_words,res->_words);
		return res;
	}
	void scavenge() {
		EvacuateWords(N,_words);
	}
};

Nword makeTuple(unsigned n) {
#define SIZE(n) case n: { return new (tuple_allocation,n) Value_Tuple_N<n>(); }
	switch(n) {
	SIZE(2) SIZE(3) SIZE(4) SIZE(5) SIZE(6) SIZE(7) SIZE(8) SIZE(9) SIZE(31)
	}
	cout << "**makeTuple(not yet supported): " << n << endl;
	NOT_YET;
	//return new (tuple_allocation,n) Value_Tuple_VarN(n);
}

Value_Tuple* getTuple(Nword w) {
	if (Value_Tuple* x = dynamic_cast<Value_Tuple*>(getPointer(w))) { return x; }
	cout << "ERROR/getTuple: " << what(w) << endl;
	TYPE_ERROR;
}

Nword getTupleElem(Nword w,unsigned n) {
	return getTuple(w)->tupleElem(n);
}

void setTupleElem(Nword w,unsigned n,Nword v) {
	getTuple(w)->tupleElem(n) = v;
}

//----------------------------------------------------------------------
//INDEX: Value_VectorN
//----------------------------------------------------------------------

class Value_Vector : public Hob {
	unsigned _n;
	Nword* _words;
public:
	Value_Vector(unsigned n): _n(n), _words(new Nword[_n]) {} // AGGH - where is this allocated??
	std::string what() { return "vector"; }
	bool equalTo(Nword w) { NOT_YET; } // is vector an eqtype?
	unsigned size() { return _n; }
	Nword& elem(unsigned i) {
		assert(i<_n);
		return _words[i];
	}
	unsigned bytes() { return HobBytes(0); }
	Hob* evacuate() {
		Value_Vector* res = new (EvacuationAllocation) Value_Vector(_n);
		CopyWords(_n,_words,res->_words);
		return res;
	}
	void scavenge() {
		EvacuateWords(_n,_words);
	}
};

Value_Vector* getVector(Nword w) {
	if (Value_Vector* x = dynamic_cast<Value_Vector*>(getPointer(w))) { return x; }
	cout << "ERROR/getVector: " << what(w) << endl;
	TYPE_ERROR;
}

Value_Vector* makeVector(unsigned n) {
	return new (vector_data_allocation) Value_Vector(n);
}

//----------------------------------------------------------------------
//INDEX: ref
//----------------------------------------------------------------------

Nword& getRef(Nword); //forward

class Value_Ref : public Hob {
public:
	Nword _w;
	Value_Ref(Nword w) : _w(w) {}
	std::string what() { return "ref"; }
	//bool equalTo(Nword w2) { return equalWord(_w,getRef(w2)); } //wrong!
	bool equalTo(Nword w2) { return (this == getPointer(w2)); } // must do pointer-equality for refs.
	unsigned bytes() { return HobBytes(0); }
	Hob* evacuate() { return new (EvacuationAllocation) Value_Ref(_w); }
	void scavenge() { ::top_evacuate(_w); }
};

Nword& getRef(Nword w) {
	if (Value_Ref* x = dynamic_cast<Value_Ref*>(getPointer(w))) { return x->_w; }
	cout << "ERROR/getRef: " << what(w) << endl;
	TYPE_ERROR;
}

//----------------------------------------------------------------------
//INDEX: Value_ArrayN
//----------------------------------------------------------------------

class Value_Array : public Hob {
	unsigned _n;
	Nword* _words;
public:
	Value_Array(unsigned n): _n(n), _words(new Nword[_n]) {} // AGGH - where is this allocated??
	std::string what() { return "array"; }
	bool equalTo(Nword w) { NOT_YET; } // is array an eqtype?
	unsigned size() { return _n; }
	Nword& elem(unsigned i) {
		assert(i<_n);
		return _words[i];
	}
	unsigned bytes() { return HobBytes(0); }
	Hob* evacuate() {
		Value_Array* res = new (EvacuationAllocation) Value_Array(_n);
		CopyWords(_n,_words,res->_words);
		return res;
	}
	void scavenge() {
		EvacuateWords(_n,_words);
	}
};

Value_Array* getArray(Nword w) {
	if (Value_Array* x = dynamic_cast<Value_Array*>(getPointer(w))) { return x; }
	cout << "ERROR/getArray: " << what(w) << endl;
	TYPE_ERROR;
}

Value_Array* makeArray(unsigned n) {
	return new (array_data_allocation) Value_Array(n);
}

//----------------------------------------------------------------------
//INDEX: Stack allocated Continuations / Handlers
//----------------------------------------------------------------------

class Continuation {
public:
	SiCont* si;
	Nword words[];
public:
	Continuation(SiCont* si_) : si(si_) {}
	static void* operator new(size_t size, //Counter& counter, 
							  unsigned extra_words) {
		return stack_alloc(//counter, 
						   size+extra_words*sizeof(Nword));
	}
	Nword& frameElem(unsigned i) {
		assert(i<si->frame_size);
		return words[i];
	}
	unsigned bytes() { return HobBytes(si->frame_size); }
	void scavenge() { EvacuateWords(si->frame_size,words); }
};

class Handler {
public:
	SiCont* si;
	Handler* handler;
	Nword words[];
public:
	Handler(SiCont* si_, Handler* handler_) : si(si_), handler(handler_) {}
	static void* operator new(size_t size, //Counter& counter, 
							  unsigned extra_words) {
		return stack_alloc(//counter, 
			size+extra_words*sizeof(Nword));
	}
	Nword& frameElem(unsigned i) {
		assert(i<si->frame_size);
		return words[i];
	}
	unsigned bytes() { return HobBytes(si->frame_size); }
	void scavenge() { EvacuateWords(si->frame_size,words); }
};

Handler* TheCurrentHandler = 0;

void SetContFrameElem(unsigned n,Nword v) {
	Continuation* CC = reinterpret_cast<Continuation*>(StackPointer);
	CC->frameElem(n) = v;
}

void SetXcontFrameElem(unsigned n,Nword v) {
	TheCurrentHandler->frameElem(n) = v;
}

Ncode ReturnWith(Nword res) {
	Continuation* CC = reinterpret_cast<Continuation*>(StackPointer);
	if (show_progress) { cout << "**Return: " << CC->si->name << endl; }
	CRET = res; XRET = Nword::fromRawUnboxed(0);
	unsigned size = CC->bytes();
	StackPointer += size;
  	return Jump(CC->si, CC->words);
}

Ncode RaiseWith(Nword res) {
	Handler* CH = TheCurrentHandler;
	if (show_progress) { 
		cout << "**Raise: " << CH->si->name << endl;
		cout << "**Sliding: " << (reinterpret_cast<char*>(CH) - StackPointer) << endl;
	}
	XRET = res; CRET = Nword::fromRawUnboxed(0);
	unsigned size = CH->bytes();
	StackPointer = reinterpret_cast<char*>(CH) + size;
	TheCurrentHandler = CH->handler;
  	return Jump(CH->si, CH->words);
}

void PushContinuation(//Counter& counter, 
					  SiCont* si) {	
	if (show_progress) { cout << "**PushContinuation: " << si->name << endl; }
	new (//counter,
		 si->frame_size) Continuation(si);
}

Ncode PopHandler () {
	unsigned size = TheCurrentHandler->bytes();
	StackPointer += size;
	TheCurrentHandler = TheCurrentHandler->handler;
	return ReturnWith(CRET);
}

SiCont SiPopHandler = SiCont("<pop-handler>",Ncode(PopHandler),0);

void PushHandler(//Counter& counter, 
				 SiCont* si) {
	if (show_progress) { cout << "**Pushhandler: " << si->name << endl; }
	TheCurrentHandler = new (//counter,
							 si->frame_size) Handler(si,TheCurrentHandler);
	PushContinuation(//counter,
					 &SiPopHandler);
}

//----------------------------------------------------------------------
//INDEX: ScavangeStack
//----------------------------------------------------------------------

void ScavangeStack() {
	char* sp = StackPointer;
	Handler* H = TheCurrentHandler;
	while (sp < StackTop) {
		if (reinterpret_cast<Handler*>(sp) == H) {
			H->scavenge();
			sp += H->bytes();
			H = H->handler;
		} else {
			Continuation* C = reinterpret_cast<Continuation*>(sp);
			C->scavenge();
			sp += C->bytes();
		}
	}
}

//----------------------------------------------------------------------
//INDEX: SetArgsFromFrame_upto
//----------------------------------------------------------------------

void SetArgsFromFrame_upto(unsigned n) {
	if (n>0) {
		SetArgsFromFrame_upto(n-1);
		GetArgVar(n-1) = FRAME(n-1);
	}
}

void ShiftUpArgs_upto(unsigned shift, unsigned n) {
	if (n>0) {
		GetArgVar(shift+n-1) = GetArgVar(n-1); //start with rightmost, to avoid over-writing
		ShiftUpArgs_upto(shift,n-1);
	}
}

//----------------------------------------------------------------------
//INDEX: Pap
//----------------------------------------------------------------------

Ncode ApplyPapCode () {
	unsigned frame_size = TheFrameSize;
	unsigned num_early_args = frame_size - 1;
	Nword func = FRAME(num_early_args);
	Closure* closure = getClosure(func);
	unsigned num_formal_args = closure->si->num_args;
	if (show_progress) {
		char* name = closure->si->name;
		cout << "**ApplyPapCode: " << name << ", " << num_early_args << "/" << num_formal_args << endl;
	}
	unsigned num_remaining_args = num_formal_args - num_early_args;
	ShiftUpArgs_upto(num_early_args,num_remaining_args);
	SetArgsFromFrame_upto(num_early_args);
	return Enter(closure);
}

SiClosure SiPap_1_1("<Pap:1,1>",Ncode(ApplyPapCode),2,1); //should also use optimized code.
SiClosure SiPap_1_2("<Pap:1,2>",Ncode(ApplyPapCode),2,2);
SiClosure SiPap_1_3("<Pap:1,4>",Ncode(ApplyPapCode),2,3);
SiClosure SiPap_1_4("<Pap:1,4>",Ncode(ApplyPapCode),2,4);
SiClosure SiPap_2_1("<Pap:2,1>",Ncode(ApplyPapCode),3,1);
SiClosure SiPap_2_2("<Pap:2,2>",Ncode(ApplyPapCode),3,2);
SiClosure SiPap_3_1("<Pap:3,1>",Ncode(ApplyPapCode),4,1);
SiClosure SiPap_3_2("<Pap:3,2>",Ncode(ApplyPapCode),4,2);
SiClosure SiPap_4_1("<Pap:4,1>",Ncode(ApplyPapCode),5,1);

SiClosure* getSiPap(unsigned num_early_args, unsigned num_remaining_args) {
	if (num_early_args==1 && num_remaining_args==1) { return &SiPap_1_1; }
	if (num_early_args==1 && num_remaining_args==2) { return &SiPap_1_2; }
	if (num_early_args==1 && num_remaining_args==3) { return &SiPap_1_3; }
	if (num_early_args==1 && num_remaining_args==4) { return &SiPap_1_4; }
	if (num_early_args==2 && num_remaining_args==1) { return &SiPap_2_1; }
	if (num_early_args==2 && num_remaining_args==2) { return &SiPap_2_2; }
	if (num_early_args==3 && num_remaining_args==1) { return &SiPap_3_1; }
	if (num_early_args==3 && num_remaining_args==2) { return &SiPap_3_2; }
	if (num_early_args==4 && num_remaining_args==1) { return &SiPap_4_1; }
	cout << "**getSiPap(not yet supported): " << num_early_args << "," << num_remaining_args << endl; NOT_YET;
	return makeSiClosure(pap_allocation,"<Pap>",Ncode(ApplyPapCode),num_early_args+1,num_remaining_args);
}

Nword makePap(Nword func, unsigned num_early_args, unsigned num_remaining_args) {
	//cout << "**makePap - " << num_early_args << "," << num_remaining_args << endl;
	SiClosure* si = getSiPap(num_early_args,num_remaining_args);
	Closure* clo = makeClosure(pap_allocation,si);
	clo->frameElem(num_early_args) = func;
	return clo;
}

void SetClosureFrameFromArgs_upto(Nword w, unsigned n) {
	if (n>0) {
		SetClosureFrameFromArgs_upto(w,n-1);
		SetClosureFrameElem(w,n-1,GetArgVar(n-1));
	}
}

//----------------------------------------------------------------------
//INDEX: Unapplied builtin, of 1 arg
//----------------------------------------------------------------------

class Value_Op1 : public Hob {
public:
	NwordOp1 _op;
	Value_Op1(NwordOp1 op) : _op(op) {}
	std::string what() { return "op1"; }
	bool equalTo(Nword w1) { TYPE_ERROR; }
	unsigned bytes() { return HobBytes(0); }
	Hob* evacuate() { return new (EvacuationAllocation) Value_Op1(_op); }
	void scavenge() {}
};

NwordOp1 getOp1(Nword w) {
	if (Value_Op1* x = dynamic_cast<Value_Op1*>(getPointer(w))) { return x->_op; }
	cout << "ERROR/getOp1: " << what(w) << endl;
	TYPE_ERROR;
}

Ncode ApplyBuiltin1 () {
	//cout << "**ApplyBuiltin1" << endl;
	NwordOp1 op = getOp1(FRAME(0));
	Nword w = ARG(0);
	Nword res = op(w);
	return ReturnWith(res);
}

SiClosure SiApplyBuiltin1 = SiClosure("ApplyBuiltin1",Ncode(ApplyBuiltin1),1,1);

Nword g_CloseBuiltin_1(char* name, NwordOp1 op) {
	//cout << "**g_CloseBuiltin_1: " << name << endl;
	Closure* clo = makeClosure(unappliedB_allocation,&SiApplyBuiltin1);
	clo->frameElem(0) = new (unappliedB_allocation) Value_Op1(op);
	return clo;
}

//----------------------------------------------------------------------
//INDEX: Unapplied builtin, of 2 args
//----------------------------------------------------------------------

class Value_Op2 : public Hob {
public:
	NwordOp2 _op;
	Value_Op2(NwordOp2 op) : _op(op) {}
	std::string what() { return "op2"; }
	bool equalTo(Nword w2) { TYPE_ERROR; }
	unsigned bytes() { return HobBytes(0); }
	Hob* evacuate() { return new (EvacuationAllocation) Value_Op2(_op); }
	void scavenge() {}
};

NwordOp2 getOp2(Nword w) {
	if (Value_Op2* x = dynamic_cast<Value_Op2*>(getPointer(w))) { return x->_op; }
	cout << "ERROR/getOp2: " << what(w) << endl;
	TYPE_ERROR;
}

Ncode ApplyBuiltin2 () {
	//cout << "**ApplyBuiltin2" << endl;
	NwordOp2 op = getOp2(FRAME(0));
	Nword w0 = getTupleElem(ARG(0),0);
	Nword w1 = getTupleElem(ARG(0),1);
	Nword res = op(w0,w1);
	return ReturnWith(res);
}

SiClosure SiApplyBuiltin2 = SiClosure("ApplyBuiltin2",Ncode(ApplyBuiltin2),1,1);

Nword g_CloseBuiltin_2(char* name, NwordOp2 op) {
	//cout << "**g_CloseBuiltin_2: " << name << endl;
	Closure* clo = makeClosure(unappliedB_allocation,&SiApplyBuiltin2);
	clo->frameElem(0) = new (unappliedB_allocation) Value_Op2(op);
	return clo;
}

//----------------------------------------------------------------------
//INDEX: Unapplied builtin, of 3 args
//----------------------------------------------------------------------

class Value_Op3 : public Hob {
public:
	NwordOp3 _op;
	Value_Op3(NwordOp3 op) : _op(op) {}
	std::string what() { return "op3"; }
	bool equalTo(Nword w3) { TYPE_ERROR; }
	unsigned bytes() { return HobBytes(0); }
	Hob* evacuate() { return new (EvacuationAllocation) Value_Op3(_op); }
	void scavenge() {}
};

NwordOp3 getOp3(Nword w) {
	if (Value_Op3* x = dynamic_cast<Value_Op3*>(getPointer(w))) { return x->_op; }
	cout << "ERROR/getOp3: " << what(w) << endl;
	TYPE_ERROR;
}

Ncode ApplyBuiltin3 () {
	//cout << "**ApplyBuiltin3" << endl;
	NwordOp3 op = getOp3(FRAME(0));
	Nword w0 = getTupleElem(ARG(0),0);
	Nword w1 = getTupleElem(ARG(0),1);
	Nword w2 = getTupleElem(ARG(0),2);
	Nword res = op(w0,w1,w2);
	return ReturnWith(res);
}

SiClosure SiApplyBuiltin3 = SiClosure("ApplyBuiltin3",Ncode(ApplyBuiltin3),1,1);

Nword g_CloseBuiltin_3(char* name, NwordOp3 op) {
	//cout << "**g_CloseBuiltin_3: " << name << endl;
	Closure* clo = makeClosure(unappliedB_allocation,&SiApplyBuiltin3);
	clo->frameElem(0) = new (unappliedB_allocation) Value_Op3(op);
	return clo;
}

//----------------------------------------------------------------------
//INDEX: OverApp
//----------------------------------------------------------------------

Ncode ApplyOverApp (); //forward

SiCont SiOverApp1 ("<OverApp1>",Ncode(ApplyOverApp),1); //could optimize ApplyOverApp, as frame_size is known!
SiCont SiOverApp2 ("<OverApp2>",Ncode(ApplyOverApp),2);
SiCont SiOverApp3 ("<OverApp3>",Ncode(ApplyOverApp),3);

SiCont* getSiOverApp(unsigned frame_size) {
	if (frame_size==1) { return &SiOverApp1; }
	if (frame_size==2) { return &SiOverApp2; }
	if (frame_size==3) { return &SiOverApp3; }
	cout << "**getSiOverApp(not yet supported): " << frame_size << endl; NOT_YET;
	return makeSiCont(//overapp_allocation,
					  "<OverApp>",Ncode(ApplyOverApp),frame_size);
}

void PushOverApp(unsigned frame_size) {
	//cout << "**PushOverApp: " << frame_size << endl;
	SiCont* si = getSiOverApp(frame_size);
	PushContinuation(//overapp_allocation,
					 si);
}

void SetContFrameFromArgs_upto(unsigned shift_num_formal_args,unsigned n) {
	if (n>0) {
		SetContFrameFromArgs_upto(shift_num_formal_args,n-1);
		SetContFrameElem(n-1,GetArgVar(n-1+shift_num_formal_args));
	}
}
//----------------------------------------------------------------------
//INDEX: debug
//----------------------------------------------------------------------

string spaces(unsigned n) {
	string res = "";
	for (unsigned u = 0; u<n; ++u) {
		res += " ";
	}
	return res;
}

unsigned stackDepth() {
	unsigned depth = 0;
	char* sp = StackPointer;
	Handler* H = TheCurrentHandler;
	while (sp < StackTop) {
		++depth;
		if (reinterpret_cast<Handler*>(sp) == H) {
			sp += H->bytes();
			H = H->handler;
		} else {
			Continuation* C = reinterpret_cast<Continuation*>(sp);
			sp += C->bytes();
		}
	}
	return depth;
}


//----------------------------------------------------------------------
//INDEX: callFunc (arg count check) - variable num_actual_args
//----------------------------------------------------------------------

Ncode callFunc(unsigned num_actual_args, Nword func) {
	//cout << "**callFunc: " << num_actual_args << endl;
	Closure* closure = getClosure(func);
	//cout << "NML: " << spaces(stackDepth()) << closure->si->name << endl;
	unsigned num_formal_args = closure->si->num_args;
	if (show_progress) {
		char* name = closure->si->name;
		cout << "**callFunc: " << name << ", " << num_actual_args << "/" << num_formal_args << endl;
	}
	if (num_actual_args < num_formal_args) { // pap
		unsigned num_remaining_args = num_formal_args - num_actual_args;
		Nword pap = makePap(func,num_actual_args,num_remaining_args);
		SetClosureFrameFromArgs_upto(pap,num_actual_args);
		return ReturnWith(pap);
	}
	if (num_actual_args > num_formal_args) { // overapp
		unsigned num_extra_args = num_actual_args - num_formal_args;
		PushOverApp(num_extra_args);
		SetContFrameFromArgs_upto(num_formal_args, num_extra_args);
	}
	return Enter(closure);
}

Ncode ApplyOverApp () {
	unsigned frame_size = TheFrameSize;
	//cout << "**ApplyOverApp: " << frame_size << endl;
	SetArgsFromFrame_upto(frame_size);
	return callFunc(frame_size,CRET);
}

//----------------------------------------------------------------------
//INDEX: g_mk*
//----------------------------------------------------------------------

Nword g_mkString(char* cp) {
	return makeString(std::string(cp));
}

Nword g_mkNum(int n) {
	return makeInt(n);
}

Nword g_mkWord(unsigned n) {
	return makeWord(n);
}

Nword g_mkChar(char c) {
	return makeChar(c);
}

//----------------------------------------------------------------------
//INDEX: g_match*
//----------------------------------------------------------------------

bool g_matchNum(Nword w,int n) {
	return (getInt(w) == n);
}

bool g_matchChar(Nword w,char n) {
	return (getChar(w) == n);
}

bool g_matchString(Nword w,char* cp) {
	std::string s1 = getString(w);
	std::string s2 = std::string(cp);
	bool res = (s1 == s2);
	//cout << "**g_matchstring: " << s1 << " ~ " << s2 << " -> " << res << endl;
	return res;
}

bool g_matchC(Nword w,unsigned n) {
	return (getTag(w) == n);
}

bool g_matchE(Nword w,unsigned n) {
	return (getExTag(w) == n);
}

bool g_matchG(Nword w1,Nword w2) {
	return (getExTag(w1) == getExTag(w2));
}

//----------------------------------------------------------------------
//INDEX: g_* - Con/ExCon - build/destruct
//----------------------------------------------------------------------

Nword g_con0(unsigned tag, unsigned arity) { //arity ignored!!
	//cout << "--g_con0" << "(" <<tag<<","<<arity<<")" << endl;
	//return makeCon0_arity(tag,arity);
	assert((arity==0) || (arity==1));
	if (arity) {
		return makeCon0_closure(tag);
	} else {
		return makeCon0(tag);
	}
}

Nword g_MakeCon(unsigned tag, Nword v) {
	//cout << "**g_MakeCon" << endl;
	return makeCon1(tag,v);
}

Nword g_DeCon(Nword w) {
	return getCon(w);
}

Nword g_mkExname(char* name,unsigned tag) {
	//cout << "**g_mkExname: " << name << "," << tag << endl;
	//Nword w = makeExCon0(name,tag);
	Nword w = makeExCon0_closure(name,tag);
	//cout << "**g_mkExname: " << name << "," << tag << " -> " << w << endl;
	return w;
}

Nword g_mkExname(char* name,unsigned tag,Nword value) { //yuck, overloaded
	//cout << "**g_mkExname(3args): " << name << "," << tag << "," << value << endl;
	Nword w = makeExCon1(name,tag,value);
	//cout << "**g_mkExname(3args): " << name << "," << tag << "," << value << " -> " << w << endl;
	return w;
}

Nword g_MakeException(char* name) {
	static unsigned N = 100; // HACK - to be fixed!
	return g_mkExname(name,++N);
}

Nword g_DeExcon(Nword w) {
	return getExCon(w);
}

//----------------------------------------------------------------------
//INDEX: g_*
//----------------------------------------------------------------------

Nword g_MakePap(Nword func, unsigned num_early_args, unsigned num_remaining_args) {
	return makePap(func,num_early_args,num_remaining_args);
}

Nword g_MakeTuple(unsigned n) {
	return makeTuple(n);
}

SiCont* g_MakeSiCont(char* name, Ncode code, unsigned frame_size) {
	return makeSiCont(//control_allocation,
					  name,code,frame_size);
}

SiCont* g_MakeSiHandle(char* name, Ncode code, unsigned frame_size) {
	return makeSiCont(//handle_allocation,
					  name,code,frame_size);
}

SiClosure* g_MakeSiFn(char* name, Ncode code, unsigned frame_size, unsigned num_args) {
	return makeSiClosure(fn_allocation,name,code,frame_size,num_args);
}

void g_PushContinuation(SiCont* si) {
	PushContinuation(//control_allocation,
					 si);
}

void g_PushHandler(SiCont* si) {
	PushHandler(//handle_allocation,
				si);
}

Nword g_MakeFn(SiClosure* si) {
	return makeClosure(fn_allocation,si);
}

void g_SetTupleElement(Nword w,unsigned n,Nword v) {
	setTupleElem(w,n,v);
}

void g_SetFrameElement(Nword w,unsigned n,Nword v) {
	SetClosureFrameElem(w,n,v);
}

void g_SetContFrameElem(unsigned n,Nword v) {
	SetContFrameElem(n,v);
}

void g_SetXcontFrameElem(unsigned n,Nword v) {
	SetXcontFrameElem(n,v);
}

Nword g_Copy(Nword w) {
	return w;
}

Nword g_DeTuple(Nword w,unsigned n) {
	return getTupleElem(w,n);
}

Ncode g_returnWith(Nword res) {
	return ReturnWith(res);
}

Ncode g_raise(Nword w) {
	return RaiseWith(w);
}

Nword g_stdOut() {
	return the_stdOut;
}

Nword g_unit () {
	return get_unit();
}

Nword g_MakeRef (Nword w) {
	//assert (w);
	//cout << "**g_MakeRef" << endl;
	return new (ref_allocation) Value_Ref(w);
}

Nword g_DeRef (Nword w) {
	//cout << "**g_DeRef" << endl;
	//return w->deRef();
	return getRef(w);
}

Nword g_EmptyRef () {
	//cout << "**g_EmptyRef" << endl;
	return new (ref_allocation) Value_Ref(Nword::fromRawUnboxed(0));
}

void g_FixupRef (Nword w1, Nword w2) {
	//cout << "**g_FixupRef" << endl;
	getRef(w1) = w2;
}

//----------------------------------------------------------------------
//INDEX: util builders
//----------------------------------------------------------------------

Nword makeBool(bool b) {
	return makeCon0(b?0:1); //true=0, false=1 -doh!
}

const unsigned the_nil_tag = 1;
const unsigned the_cons_tag = 0;

Nword the_nil = makeCon0(the_nil_tag);

Nword makePair(Nword w0, Nword w1) {
	Nword res = makeTuple(2);
	setTupleElem(res,0,w0);
	setTupleElem(res,1,w1);
	return res;
}

Nword makeListCons(Nword w0, Nword w1) {
	return makeCon1 (the_cons_tag, makePair(w0,w1));
}

//----------------------------------------------------------------------
//INDEX: builtin (top-level)
//----------------------------------------------------------------------

Nword builtin_ColonEqual(Nword w1,Nword w2) {
	getRef(w1) = w2;
	return get_unit();
}

Nword builtin_Tilda(Nword w) { return makeInt(- getInt(w)); }

Nword builtin_Dash(Nword w1,Nword w2) { return makeInt(getInt(w1) - getInt(w2)); }
Nword builtin_Plus(Nword w1,Nword w2) { return makeInt(getInt(w1) + getInt(w2)); }
Nword builtin_Star(Nword w1,Nword w2) { return makeInt(getInt(w1) * getInt(w2)); }
Nword builtin_div(Nword w1,Nword w2) { return makeInt(getInt(w1) / getInt(w2)); }
Nword builtin_mod(Nword w1,Nword w2) { return makeInt(getInt(w1) % getInt(w2)); }

Nword builtin_Less(Nword w1,Nword w2)           { return makeBool (  lessNumTxt(w1,w2)); }
Nword builtin_LessEqual(Nword w1,Nword w2)      { return makeBool (! lessNumTxt(w2,w1)); }
Nword builtin_Greater(Nword w1,Nword w2)        { return makeBool (  lessNumTxt(w2,w1)); }
Nword builtin_GreaterEqual(Nword w1,Nword w2)   { return makeBool (! lessNumTxt(w1,w2)); }

Nword builtin_Hat(Nword w1,Nword w2) {
    return makeString(getString(w1) + getString(w2));
}

Nword builtin_size(Nword w) {
	return makeInt(getString(w).size());
}

Nword builtin_chr(Nword w) {
	int n = getInt(w);
	assert(0<=n && n<=255);
	char c = n;
	return makeChar(c);
}

Nword builtin_ord(Nword w) {
	char c = getChar(w);
	return makeInt(c);
}

Nword builtin_implode(Nword w) {
	//cout << "**builtin_implode" << endl;
	string res;
	for (Nword xs = w; getTag(xs) != the_nil_tag; xs = getTupleElem (getCon(xs),1)) {
		Nword x = getTupleElem(getCon(xs),0);
		res += getChar(x);
	}
	//cout << "**builtin_implode ->" << res << endl;
	return makeString(res);
}

Nword builtin_explode(Nword w) {
	string s = getString(w);
	unsigned n = s.size();
	Nword res = the_nil;
	for (unsigned i = n; i > 0; ) {
		char c = s[--i];
		res = makeListCons(makeChar(c),res);
	}
	return res;
}

Nword builtin_Equal(Nword w1,Nword w2) {
	//return equalWord(w1,w2);  //missing makeBool - how can I cathc this error?
	bool b = equalWord(w1,w2);
 	//cout << "**builtin_Equal: " << w1 << "," << w2 << " -> " << (b?"1":"0") << endl;
	return makeBool(b);
}

Nword builtin_print(Nword w) {
	cout << getString(w) ;
	return get_unit();
}

//----------------------------------------------------------------------
//INDEX: builtin_Vector*
//----------------------------------------------------------------------

Nword builtin_Vector_sub(Nword w1,Nword w2) {
	//cout << "**builtin_Vector_sub: " << endl;
	return getVector(w1)->elem(getInt(w2));
}

Nword builtin_Vector_fromList(Nword w) {
	//cout << "**builtin_Vector_fromList" << endl;
	unsigned length = 0;
	for (Nword xs = w; getTag(xs) != the_nil_tag; xs = getTupleElem (getCon(xs),1)) { ++length; }
	//cout << "**builtin_Vector_fromList, length=" << length << endl;
	Value_Vector* vec = makeVector(length);
	unsigned i = 0;
	for (Nword xs = w; getTag(xs) != the_nil_tag; xs = getTupleElem (getCon(xs),1)) {
		Nword x = getTupleElem(getCon(xs),0);
		//cout << "**builtin_Vector_fromList, set: " << i << endl;
		vec->elem(i) = x;
		++i;
	}
	return vec;
}

//----------------------------------------------------------------------
//INDEX: builtin_Array*
//----------------------------------------------------------------------

Nword builtin_Array_array(Nword w1,Nword w2) {
	unsigned n = getInt(w1);
	//cout << "**builtin_Array_array: " << n << endl;
	Value_Array* array = makeArray(n);
	for (unsigned i = 0; i<n; ++i) {
		//cout << "**builtin_Array_array, set: " << i << endl;
		array->elem(i) = w2;
	}
	return array;
}

Nword builtin_Array_sub(Nword w1,Nword w2) {
	//cout << "**builtin_Array_sub: " << endl;
	return getArray(w1)->elem(getInt(w2));
}

Nword builtin_Array_length(Nword w) {
	//cout << "**builtin_Array_length: " << endl;
	return makeInt (getArray(w)->size());
}

Nword builtin_Array_update(Nword w1,Nword w2,Nword w3) {
	//cout << "**builtin_Array_update: " << endl;
	getArray(w1)->elem(getInt(w2)) = w3;
	return get_unit();
}

//----------------------------------------------------------------------
//INDEX: BitsOf
//----------------------------------------------------------------------

struct BitsOf {
public:
	unsigned u;
	BitsOf (unsigned u_) :u(u_) {}
};

ostream& operator<< (ostream& os, const BitsOf& x) {
  	char buf[20];
  	sprintf(buf,"%x",x.u);
  	os << buf;
//  	os << "<";
//  	for (unsigned n = 32; n>0; ) { 
//  		--n;
//  		os << ((x.u >> n) & 1);
//  	}
//  	os << ">";
	return os;
}

//----------------------------------------------------------------------
//INDEX: builtin_Word*
//----------------------------------------------------------------------

Nword builtin_Word_fromInt(Nword w) {
	int i = getInt(w);
	assert(i>=0);
	unsigned u = i;
	//cout << "**builtin_Word_fromInt: " << i << " -> " << BitsOf(u) << endl;
	return makeWord(u);
}

Nword builtin_Word_toString(Nword w) {
	unsigned n = getWord(w);
	char buf[20];
	sprintf(buf,"%x",n);
	return makeString(buf);
}

Nword builtin_Word_Plus(Nword w1,Nword w2) {
	unsigned n1 = getWord(w1);
	unsigned n2 = getWord(w2);
	unsigned n3 = n1 + n2;
	//cout << "**builtin_Word_Plus: " << BitsOf(n1) << "," << BitsOf(n2) << " -> " << BitsOf(n3) << endl;
	return makeWord(n3);
}

Nword builtin_Word_Dash(Nword w1,Nword w2) {
	unsigned n1 = getWord(w1);
	unsigned n2 = getWord(w2);
	unsigned n3 = n1 - n2;
	//cout << "**builtin_Word_Dash: " << BitsOf(n1) << "," << BitsOf(n2) << " -> " << BitsOf(n3) << endl;
	return makeWord(n3);
}

Nword builtin_Word_mod(Nword w1,Nword w2) {
	unsigned n1 = getWord(w1);
	unsigned n2 = getWord(w2);
	unsigned n3 = n1 % n2;
	//cout << "**builtin_Word_mod: " << BitsOf(n1) << "," << BitsOf(n2) << " -> " << BitsOf(n3) << endl;
	return makeWord(n3);
}

Nword builtin_Word_GreaterGreater(Nword w1,Nword w2) {
	unsigned n1 = getWord(w1);
	unsigned n2 = getWord(w2);
	unsigned n3 = n1 >> n2;
	//cout << "**builtin_Word_GreaterGreater: " << BitsOf(n1) << "," << BitsOf(n2) << " -> " << BitsOf(n3) << endl;
	return makeWord(n3);
}

Nword builtin_Word_LessLess(Nword w1,Nword w2) {
	unsigned n1 = getWord(w1);
	unsigned n2 = getWord(w2);
	unsigned n3 = n1 << n2;
	//cout << "**builtin_Word_LessLess: " << BitsOf(n1) << "," << BitsOf(n2) << " -> " << BitsOf(n3) << endl;
	return makeWord(n3);
}

Nword builtin_Word_orb(Nword w1,Nword w2) {
	unsigned n1 = getWord(w1);
	unsigned n2 = getWord(w2);
	unsigned n3 = n1 | n2;
	//cout << "**builtin_Word_orb: " << BitsOf(n1) << "," << BitsOf(n2) << " -> " << BitsOf(n3) << endl;
	return makeWord(n3);
}

Nword builtin_Word_andb(Nword w1,Nword w2) {
	unsigned n1 = getWord(w1);
	unsigned n2 = getWord(w2);
	unsigned n3 = n1 & n2;
	//cout << "**builtin_Word_andb: " << BitsOf(n1) << "," << BitsOf(n2) << " -> " << BitsOf(n3) << endl;
	return makeWord(n3);
}

Nword builtin_Word_notb(Nword w) {
	unsigned n = getWord(w);
	unsigned res = ~ n;
	//cout << "**builtin_Word_notb: " << BitsOf(n) << " -> " << BitsOf(res) << endl;
	return makeWord(res);
}

Nword builtin_Word_toInt(Nword w) {
	int u = getWord(w);
	int i = u;
	//cout << "**builtin_Word_toInt: " << u << " -> " << i << endl;
	return makeInt(i);
}

//----------------------------------------------------------------------
//INDEX: builtin_TextIO*
//----------------------------------------------------------------------

Nword builtin_TextIO_output(Nword w1,Nword w2) {
	ostream& os = getOutstream(w1);
	string s = getString(w2);
	os << s;
	return get_unit();
}

Nword builtin_TextIO_flushOut(Nword) {
	NOT_YET;
}

Nword builtin_TextIO_closeOut(Nword w) {
	//cout << "**builtin_closeOut: " << "??" << endl;
	ostream& os = getOutstream(w);
	ofstream& ofs = dynamic_cast<ofstream&>(os); //not all ostream are ofstream!
	ofs.close();
	return get_unit();
}

Nword builtin_TextIO_openIn(Nword w) {
	std::string s = getString(w);
	//cout << "**builtin_openIn: " << s << endl;
	std::ifstream* ifsp = new std::ifstream(s.c_str());
	assert(ifsp && ifsp->is_open()); //should really throw an exception if cant open file
	return new (io_data_allocation) Value_instream(*ifsp);
}

Nword builtin_TextIO_openOut(Nword w) {
	std::string s = getString(w);
	//cout << "**builtin_openOut: " << s << endl;
	std::ofstream* ofsp = new std::ofstream(s.c_str());
	return new (io_data_allocation) Value_outstream(*ofsp);
}

Nword builtin_TextIO_closeIn(Nword w) {
	//NOT DONE YET
	//std::istream& is = w->getInstream();
	//is.close(); //only for ifstream
	return get_unit();
}

Nword builtin_TextIO_inputN(Nword w1,Nword w2) {
	std::istream& is = getInstream(w1);
	int n =	 getInt(w2);
	//cout << "**builtin_inputN: " << n << endl;
	assert(n>=0);
	char c;
	string res;
	while (n && is.get(c)) {
		res += c;
	}
	//cout << "**builtin_inputN: " << n << " -> " << res.size() << endl;
	return makeString(res);
}

//----------------------------------------------------------------------
//INDEX: builtin_String*
//----------------------------------------------------------------------

Nword builtin_String_sub(Nword w1,Nword w2) {
	std::string s = getString(w1);
	int n = getInt(w2);
	//cout << "**builtin_sub: " << s << ", " << n << endl;
	return makeChar(s[n]);
}

//----------------------------------------------------------------------
//INDEX: builtin_Char*
//----------------------------------------------------------------------

Nword builtin_Char_LessEqual(Nword w1,Nword w2) { return makeBool(getChar(w1) <= getChar(w2));}
Nword builtin_Char_GreaterEqual(Nword w1,Nword w2) { return makeBool(getChar(w1) >= getChar(w2));}

Nword builtin_Char_Less (Nword,Nword) { NOT_YET; }
Nword builtin_Char_Greater (Nword,Nword) { NOT_YET; }

Nword builtin_Char_toString(Nword w) {
	static char* cp = new char[2];
	cp[0] = getChar(w);
	cp[1] = '\0';
	return makeString(std::string(cp));
}

//----------------------------------------------------------------------
//INDEX: print_stats
//----------------------------------------------------------------------

void print_stats(char* tag) {

	Counter data_allocation("D");
	data_allocation.inc (unit_data_allocation.count +
						 char_data_allocation.count +
						 word_data_allocation.count +
						 num_data_allocation.count +
						 string_data_allocation.count +
						 io_data_allocation.count +
						 con0_data_allocation.count +
						 con1_data_allocation.count +
						 excon0_data_allocation.count +
						 excon1_data_allocation.count +
						 vector_data_allocation.count +
						 array_data_allocation.count);
	
	cout << "**Stats(" << tag << ") "
		 << "[" /*<< "scale="*/ << Counter::scale << "] "
		 << LeakyAllocation << ", "
		 << HeapAllocation << " (" 
		 << EvacuationAllocation << ") ("
		//<< StackAllocation << ", "
		//<< control_allocation << ","
		//<< handle_allocation << ","
		//<< overapp_allocation << ","
		 << si_allocation << ","
		 << fn_allocation << ","
		 << tuple_allocation << ","
		 << data_allocation << ","
		 << ref_allocation << ","
		 << pap_allocation << ","
		 << unappliedB_allocation << ","
		 << unappliedC_allocation
		 << ")" << endl;

	assert(HeapAllocation.count //+ StackAllocation.count 
		   + LeakyAllocation.count
		   == (//control_allocation.count +
			   //handle_allocation.count +
			   //overapp_allocation.count +
			   si_allocation.count +
			   fn_allocation.count +
			   tuple_allocation.count +
			   data_allocation.count +
			   ref_allocation.count +
			   pap_allocation.count +
			   unappliedB_allocation.count +
			   unappliedC_allocation.count + 
			   EvacuationAllocation.count
			   ));


	cout << "**Data(" << tag << ") "
		 << data_allocation << " ("
		 << con0_data_allocation << ","
		 << con1_data_allocation << ","
		 << num_data_allocation << ","
		 << char_data_allocation << ","
		 << string_data_allocation << ","
		 << array_data_allocation << ","
		 << vector_data_allocation << ","
		 << word_data_allocation << ","
		 << io_data_allocation << ","
		 << excon0_data_allocation << ","
		 << excon1_data_allocation << ","
		 << unit_data_allocation << ")"
		 << endl;

}

//----------------------------------------------------------------------
//INDEX: StopExecution / CatchAny
//----------------------------------------------------------------------

Ncode StopExecution () {
//  	if (debug) {
//  		cout << "**Stop: CRET = " << CRET << endl;
//  	}
	return Ncode(0);
}

SiCont SiStopExection = SiCont("<Stop>",Ncode(StopExecution),0);

Ncode CatchAll () {
	cout << "** Unhandled exception: " << getExName(XRET) << endl;
	return Ncode(0);
}

SiCont SiCatchAll = SiCont("<CatchAll>",Ncode(CatchAll),0);

Handler CatchAllHandler(&SiCatchAll,0);

//----------------------------------------------------------------------
//INDEX: main
//----------------------------------------------------------------------

#define AssertWords(N,T) { \
	if (N != (sizeof(T)/sizeof(Nword))) { \
		printf("Words (%s) = %d, expected %d\n",#T,sizeof(T)/sizeof(Nword),N); \
		ABORT; \
	} \
}

int main(int argc, char* argv[]) {

  	if (debug) { 
  		print_stats("main"); 
  	}

	AssertWords(1,Continuation);
	AssertWords(2,Handler);

	const unsigned H = 1; // should be just 1, for the vtable

	//AssertWords(1,string);

	AssertWords(H,Hob);
	AssertWords(1+H,Value_outstream);
	AssertWords(1+H,Value_instream);
	AssertWords(2+H,Value_Array);
	AssertWords(2+H,Value_Vector);
	AssertWords(H,Value_unit);
	//AssertWords(1+H,Value_String);
	//AssertWords(1+H,Value_BoxInt);
	AssertWords(1+H,Value_Word);
	//AssertWords(1+H,Value_Char);
	AssertWords(1+H,Value_Ref);
	AssertWords(H,Value_Tuple);
	//AssertWords(1+H,Value_Tuple_VarN);
	//AssertWords(2+H,Value_Tuple_N<2>);
	//AssertWords(3+H,Value_Tuple_N<3>);
	AssertWords(H,Value_Tuple);
	AssertWords(H,Value_Tuple_N<2>); //+ var elems
	//AssertWords(1+H,Value_Con0);
	AssertWords(1+H,Value_Con1);

	AssertWords(3,SiCont);
	AssertWords(3,SiClosure);
	AssertWords(1+H,Closure);
	AssertWords(1+H,Value_Op1);
	AssertWords(1+H,Value_Op2);
	AssertWords(1+H,Value_Op3);


	//const unsigned HeapSpaceSize = 10 * OneMeg;
	unsigned HeapSpaceSize = 100 * OneMeg;

	Nword command_args = the_nil;
	for (unsigned i = argc; i>1; ) { //reverse order
		--i;
		std::string s(argv[i]);
		//cout << "**main:command_args: " << cp << endl;
		if (s == "-debug") {
			debug = true;
		} else if (s == "-progress") {
			show_progress = true;
		} else if (s == "-alloc-progress") {
			show_alloc_progress = true;
		} else if (s == "-1meg") {
			HeapSpaceSize = OneMeg;
		} else if (s == "-10meg") {
			HeapSpaceSize = 10 * OneMeg;
		} else {
			command_args = makeListCons(makeString(s),command_args);
		}
	}

	//char SpaceA[HeapSpaceSize];
	//char SpaceB[HeapSpaceSize];
	char* SpaceA = new char[HeapSpaceSize];
	char* SpaceB = new char[HeapSpaceSize];

	HeapSpaceA = HeapSpace (SpaceA, SpaceA + HeapSpaceSize);
	HeapSpaceB = HeapSpace (SpaceB, SpaceB + HeapSpaceSize);

	Init();
	initSpace.top = HeapPointer;

  	allocSpace = HeapSpaceB;
	currentSpace = allocSpace;
	HeapPointer = allocSpace.base;

  	if (debug) { 
  		print_stats("init"); 
  	}

	SetArg(0,command_args);
	PushContinuation(//control_allocation,
					 &SiStopExection);
	TheCurrentHandler = &CatchAllHandler;
	Closure* closure = getClosure(TheProgram);

 	TheCurrentFunction = closure;
	closure->SetFrameReference();
	CRET = Nword::fromRawUnboxed(0); XRET = Nword::fromRawUnboxed(0);

	NcodeFP fp = closure->si->code._fp;
	while(fp) {
		Ncode code = (*fp)();
		fp = code._fp;
	}

	if (debug) { 
		print_stats("final"); 
		//cout << "** maxRequiredStaticFrameSize = " << maxRequiredStaticFrameSize << endl;
		//cout << "** maxRequiredStackSize = " << StackTop - StackLowWater << endl;
		//cout << "** copiedFrameWords = " << CopiedFrameWords.count  << endl;
		cout << "**number GCs = " << gc_count  << endl;
	}

	exit(0);
}
