

class Frame_0 : public Frame_rep {
public:
	Frame_0() {}
	unsigned frameSize() { return 0; }
	Nword& frameElem(unsigned n) { assert(0); }
};

template<unsigned N> 
class Frame_N : public Frame_rep {
	Nword _w[N];
public:
	Frame_N() {}
	unsigned frameSize() { return N; }
	Nword& frameElem(unsigned n) {
		assert(n<N);
		return _w[n];
	}
};


template<unsigned n>
Frame makeFrame(unsigned* counter) {
	//std::cout << "**makeFrame: " << n << std::endl;
	return new (counter) Frame_N<n>();
}

template<>
inline
Frame makeFrame<0>(unsigned* counter) {
	//std::cout << "**makeFrame<0>" << std::endl;
	return new (counter) Frame_0();
}

template<unsigned frame_size>
Nword makeClosure_N(char* name, unsigned num_args, Ncode code) {
	Frame frame = makeFrame<frame_size>(&closure_allocation);
	return makeClosure(name,num_args,code,frame);
}	

#define g_NewFn(frame_size,num_args,code) (makeClosure_N<frame_size>(#code,num_args,Ncode(code)))



Ncode ApplyPapCode ();
template<unsigned num_early_args>
Nword makePap_N(Nword func, unsigned num_remaining_args) {
	//cout << "**makePap_N - " << num_early_args << "," << num_remaining_args << endl;
	Frame frame = makeFrame<num_early_args+1>(&pap_allocation);
	frame->frameElem(num_early_args) = func;
	return makeClosure("<pap>",num_remaining_args,Ncode(ApplyPapCode),frame);
}	
#define g_NewPap(func,num_early_args,num_remaining_args) (makePap_N<num_early_args>(func,num_remaining_args))



#define g_PushContinuation(frame_size,code) (gg_PushContinuation(#code,Ncode(code),makeFrame<frame_size>(&control_allocation)))

#define g_PushHandler(frame_size,code) (gg_PushHandler(#code,Ncode(code),makeFrame<frame_size>(&control_allocation)))


extern unsigned control_allocation;
extern unsigned closure_allocation;

Frame makeFrameVarSize(unsigned* counter, unsigned frame_size);

Nword makeClosure(char* name, unsigned num_args, Ncode code, Frame frame);

#define g_NewFn(frame_size,num_args,code) (makeClosure(#code,num_args,Ncode(code),makeFrameVarSize(&closure_allocation,frame_size)))

Nword makePapVarSize(Nword func, unsigned num_early_args, unsigned num_remaining_args);
#define g_NewPap(func,num_early_args,num_remaining_args) (makePapVarSize(func,num_early_args,num_remaining_args))

void gg_PushContinuation(char*,Ncode,Frame);
#define g_PushContinuation(frame_size,code) (gg_PushContinuation(#code,Ncode(code),makeFrameVarSize(&control_allocation,frame_size)))

void gg_PushHandler(char*,Ncode,Frame);
#define g_PushHandler(frame_size,code) (gg_PushHandler(#code,Ncode(code),makeFrameVarSize(&control_allocation,frame_size)))


void gg_PushContinuation(char* name, Ncode code, Frame frame) {
	//cout << "**gg_PushContinuation: " << name << endl;
	TheCurrentContinuation = makeContinuation(name,code,frame,TheCurrentContinuation);
}

void gg_PushHandler(char* name, Ncode code, Frame frame) {
	//cout << "**gg_PushHandler: " << name << endl;
	TheCurrentHandler = makeHandler(name,code,frame,TheCurrentHandler,TheCurrentContinuation);
	TheCurrentContinuation = makeContinuation("<pop-handler>",Ncode(PopHandler),frame,TheCurrentContinuation);
}


//----------------------------------------------------------------------
//INDEX: callFunc_N (arg count check)
//----------------------------------------------------------------------

template<unsigned num_actual_args>
Ncode callFunc_N(Nword func) {
	//cout << "**callFunc_N: " << num_actual_args << endl;
	Closure closure = getClosure(func);
	char* name = closure->getClosureName();
	unsigned num_formal_args = closure->getNumArgs();
	if (showProgress) {
		cout << "**callFunc_N: " << name << ", " << num_actual_args << "/" << num_formal_args << endl;
	}
	if (num_actual_args < num_formal_args) { // pap
		unsigned num_remaining_args = num_formal_args - num_actual_args;
		Nword pap = makePap_N<num_actual_args>(func,num_remaining_args); //fixed-size-pap!
		SetClosureFrameFromArgs_upto(pap,num_actual_args);
		return ReturnWith(pap);
	}
	if (num_actual_args > num_formal_args) { // overapp
		unsigned num_extra_args = num_actual_args - num_formal_args;
		PushOverApp(num_extra_args);
		SetContFrameFromArgs_upto(num_formal_args, num_extra_args);
	}
	TheFramePointer = closure->getClosureFrame();
	return Jump(name, closure->getClosureCode());
}


class Continuation_rep : public TopAlloc {
public:
	char* _name;
	Ncode _code;
	Continuation _cont;
	Frame _frame;
public:
	Continuation_rep(char* name, Ncode code, Frame frame, Continuation cont) :
		_name(name),
		_code(code),
		_cont(cont),
		_frame(frame) {
	}
	void SetTheFramePointer() {
		TheFramePointer = FramePointer(_frame->frameSize(), _frame->words());
	}
	void setContFrameElem(unsigned n,Nword v) {
		_frame->frameElem(n) = v;
	}
};


Continuation makeContinuation(char* name, Ncode code, Frame frame, Continuation cont) {
	return new ("continuation",&control_allocation) Continuation_rep(name,code,frame,cont);
}

Continuation makeContinuationVarSize(char* name, Ncode code, unsigned frame_size, Continuation cont) {
	Frame frame = makeFrameVarSize(&control_allocation,frame_size);
	return makeContinuation(name,code,frame,cont);
}


void SetTheFramePointer(Frame frame) {
	//TheFramePointer = FramePointer(frame);
	TheFramePointer = FramePointer(frame->frameSize(), frame->words());
}


class Value_Tuple_VarN : public Value_Tuple {
	unsigned _n;
	//Nword _w[N];
	Nword* _w;
public:
	Value_Tuple_VarN(unsigned n) : _n(n), _w(new Nword[n]) {}
	virtual unsigned size() { return _n; }
	virtual Nword& tupleElem(unsigned i) {
		assert(i<_n);
		return _w[i];
	}
};


class Frame_rep : public TopAlloc {
public:
	Frame_rep() {}
	virtual unsigned frameSize() =0;
	virtual Nword& frameElem(unsigned n) =0;
};

class Frame_VarN : public Frame_rep {
	unsigned _n;
	Nword* _w;
public:
	Frame_VarN(unsigned n) : _n(n), _w(new Nword[_n]) {}
	unsigned frameSize() { return _n; }
	Nword& frameElem(unsigned i) {
		assert(i<_n);
		return _w[i];
	}
};


class Closure_rep : public Nword_rep {
	char* _name;
	unsigned _num_args;
	Ncode _code;
	Frame _frame;
public:
	Closure_rep(char* name, unsigned num_args, Ncode code, Frame frame)
		: _name(name), _num_args(num_args), _code(code), _frame(frame) {}
	virtual std::string what() { return "Closure"; }
	char* getClosureName() {
		return _name;
	}
	unsigned getNumArgs() {
		return _num_args;
	}
	Ncode getClosureCode() {
		return _code;
	}
	Nword& frameElem(unsigned n) {
		return _frame->frameElem(n);
	}
	void SetTheFramePointer() {
		TheFramePointer = FramePointer(_frame->frameSize(), _frame->words());
	}
	bool equalTo(Nword w) { TYPE_ERROR; }
};

Closure makeClosure(unsigned* counter, char* name, unsigned num_args, Ncode code, Frame frame) {
	return new ("closure",counter) Closure_rep(name,num_args,code,frame);
}

Closure makeClosureVS(unsigned* counter,char* name, unsigned num_args, Ncode code, unsigned frame_size) {
	return makeClosure(counter,name,num_args,code,makeFrameVarSize(counter,frame_size));
}


//----------------------------------------------------------------------
//INDEX: Tue Nov 21 15:28:02 2006
//----------------------------------------------------------------------

class Nword_rep : public TopAlloc {
public:
	virtual ~Nword_rep() {}
	virtual std::string what() =0;
	virtual bool equalTo(Nword) =0;
	virtual int getNum() { assert(0); }
	virtual char getChar() { assert(0); }
	virtual std::string getString() { assert(0); }
	virtual Nword deCon() { assert(0); }
	virtual unsigned getTag() { assert(0); }
	virtual Nword M_getTupleElem(unsigned) { assert(0); }
	virtual void setTupleElem(unsigned,Nword) { assert(0); }
	virtual Closure getClosure() { assert(0); }
	virtual std::istream& getIstream() { assert(0); }
	virtual Nword deRef() { assert(0); }
	virtual void setRef(Nword v) { assert(0); }
};

class Continuation_rep;
typedef Continuation_rep* Continuation;

class Continuation_rep {
public:
	char* _name;
	Ncode _code;
	Continuation _cont;
	unsigned _frame_size;
	Nword _words[];
public:
	Continuation_rep(char* name, Ncode code, Continuation cont, unsigned frame_size) :
		_name(name),
		_code(code),
		_cont(cont),
		_frame_size(frame_size) {
	}
	static void* operator new(size_t size, char* tag, unsigned* counter, unsigned extra_words) {
		return my_alloc(size+extra_words*sizeof(Nword),tag,counter);
	}
	void setContFrameElem(unsigned i,Nword v) {
		assert(i<_frame_size);
		_words[i] = v;
	}
	void SetTheFramePointer() {
		TheFramePointer = FramePointer(_frame_size, _words);
	}
};

Continuation makeContinuationVarSize(char* name, Ncode code, unsigned frame_size, Continuation cont) {
	char buf[20];
	sprintf(buf,"continuation:%d",frame_size);
	return new (buf,&control_allocation,frame_size) Continuation_rep(name,code,cont,frame_size);
}

//----------------------------------------------------------------------
//INDEX: Wed Nov 22 10:02:55 2006
//----------------------------------------------------------------------

set<string> tags;
	if (debug && tags.find(tag) == tags.end()) {
		cout << "**Allocation: #words("<<tag<<") -> " << size/4 << endl;
		tags.insert(tag);
	}

	char buf[20] ;
	sprintf(buf,"frame-var:%d",frame_size);


class Closure : public Nword_rep {
	char* _name;
	unsigned _num_args;
	Ncode _code;
	unsigned _frame_size;
	Nword _words[];
public:
	Closure(char* name, unsigned num_args, Ncode code, unsigned frame_size)
		: _name(name), _num_args(num_args), _code(code), _frame_size(frame_size) {}
	virtual std::string what() { return "Closure"; }
	char* getClosureName() {
		return _name;
	}
	unsigned getNumArgs() {
		return _num_args;
	}
	Ncode getClosureCode() {
		return _code;
	}
	Nword& frameElem(unsigned i) {
		assert(i<_frame_size);
		return _words[i];
	}
	void SetTheFramePointer() {
		TheFramePointer = FramePointer(_frame_size, _words);
	}
	bool equalTo(Nword w) { TYPE_ERROR; }
};

	Closure* clo = makeClosureB(unappliedC_allocation,"<con0>",1,Ncode(ApplyCon0),1);


//----------------------------------------------------------------------
//INDEX: Frame
//----------------------------------------------------------------------

class Frame_rep {
	unsigned _n;
	Nword _w[];
public:
	Frame_rep(unsigned n) : _n(n) {}
	static void* operator new(size_t size, Counter& counter, unsigned extra_words) {
		return my_alloc(size+extra_words*sizeof(Nword),counter);
	}
	unsigned frameSize() { return _n; }
	Nword& frameElem(unsigned i) {
		assert(i<_n);
		return _w[i];
	}
	Nword* words() {
		return _w;
	}
};


class Handler_rep : public TopAlloc {
public:
	char* _name;
	Ncode _code;
	Frame _frame;
	Handler _handler;
	Continuation* _cont;
public:
	Handler_rep(char* name, Ncode code, Frame frame, Handler handler, Continuation* cont) :
		_name(name),
		_code(code),
		_frame(frame),
		_handler(handler),
		_cont(cont) {
	}
	void SetTheFramePointer() {
		TheFramePointer = FramePointer(_frame->frameSize(), _frame->words());
	}
	Nword& frameElem(unsigned n) {
		return _frame->frameElem(n);
	}
};

Handler makeHandler(char* name, Ncode code, Frame frame, Handler handler, Continuation* cont) {
	return new (control_allocation) Handler_rep(name,code,frame,handler,cont);
}

//  Handler makeHandler_Frame(Counter& counter, char* name, Ncode code, Frame frame, Handler handler, Continuation* cont) {
//  	return new (counter) Handler_rep(name,code,frame,handler,cont);
//  }

Handler makeHandler(Counter& counter, char* name, Ncode code, unsigned frame_size, Handler handler, Continuation* cont) {
	//Frame frame = new (counter,frame_size) Frame_rep(frame_size);
  	//areturn makeHandler_Frame(counter,name,code,frame,handler,cont);
	return new (counter) Handler_rep(name,code,frame_size,handler,cont);
}


//----------------------------------------------------------------------
//INDEX: Handler
//----------------------------------------------------------------------

CLASS Handler {
public:
	SiCont* _si;
	Handler* _handler;
	Continuation* _cont;
	Nword _words[];
public:
	Handler(SiCont* si, Handler* handler, Continuation* cont) :
		_si(si),
		_handler(handler),
		_cont(cont) {
	}
	static void* operator new(size_t size, Counter& counter, unsigned extra_words) {
		return my_alloc(size+extra_words*sizeof(Nword),counter);
	}
	void SetTheFramePointer() {
		TheFramePointer = FramePointer(_si->frame_size, _words);
	}
	Nword& frameElem(unsigned i) {
		assert(i<_si->frame_size);
		return _words[i];
	}
};

Handler* makeHandler(Counter& counter, SiCont* si, Handler* handler, Continuation* cont) {
	return new (counter,si->frame_size) Handler(si,handler,cont);
}

static Handler* TheCurrentHandler;

Ncode PopHandler () {
  	//cout << "**PopHandler: " << endl;
	TheCurrentHandler = TheCurrentHandler->_handler;
	return ReturnWith(CRET);
}

SiCont SiPopHandler = SiCont("<pop-handler>",Ncode(PopHandler),0);

void pushHandler(Counter& counter, SiCont* si) {
	//cout << "**pushHandler: " << frame_size << endl;
  	TheCurrentHandler = makeHandler(counter,si,TheCurrentHandler,TheCurrentContinuation);
  	pushContinuation(counter,&SiPopHandler);
}

void SetXcontFrameElem(unsigned n,Nword v) {
	TheCurrentHandler->frameElem(n) = v;
}


Counter PushC ("PushC");
Counter PopC ("PopC");
Counter PushH ("PushH");
Counter RaiseH ("RaiseH");

	static void operator delete(void*,size_t) {}
private:
	// private & undefined; to ensure pool-new is used
	static void* operator new(size_t size);
	static void* operator new[](size_t size);
	static void operator delete[](void* p,size_t size);

//----------------------------------------------------------------------
//INDEX: TopAlloc
//----------------------------------------------------------------------

class TopAlloc {
public:
	virtual ~TopAlloc() {}
	static void* operator new(size_t size, Counter& counter) {
		return my_alloc(counter,size);
	}
	static void* operator new(size_t size, Counter& counter, unsigned extra_words) {
		return my_alloc(counter,size+extra_words*sizeof(Nword));
	}
	static void operator delete(void*,size_t) {}
private:
	// private & undefined; to ensure pool-new is used
	static void* operator new(size_t size);
	static void* operator new[](size_t size);
	static void operator delete[](void* p,size_t size);
};


class Nword_rep : public TopAlloc {
public:
	virtual ~Nword_rep() {}
	virtual std::string what() =0;
	virtual bool equalTo(Nword) =0;
};


//  class FramePointer {
//  private:
//  	unsigned _n;
//  	Nword* _words;
//  public:
//  	FramePointer(unsigned n, Nword* words) : _n(n), _words(words) {
//  	}
//  	Nword& framePointerElem(unsigned i) {
//  		assert(i<_n);
//  		return _words[i];
//  	}
//  	unsigned frameSize() {
//  		return _n;
//  	}
//  };

//  FramePointer TheFramePointer(0,0);

//  Nword FRAME(unsigned i) {
//  	return TheFramePointer.framePointerElem(i);
//  }

//  unsigned TheFrameSize = 0;

//  void SetTheFrame(unsigned size, Nword* words) {
//  	TheFrameSize = size;
//  	TheFramePointer = FramePointer(size,words);
//  }



//----------------------------------------------------------------------
//INDEX: Level
//----------------------------------------------------------------------

class Level {
public:
	std::string name;
	unsigned count;
	unsigned max;
public:
	Level(std::string name_) : name(name_), count(0), max(0) {}
	void inc(unsigned n) { count += n; if (count>max) max = count; }
	void dec(unsigned n) { count -= n; }
private:
	Level(Level&);
};

ostream& operator<< (ostream& os, const Level& c) {
	return os << c.name << "=" << c.count << "[" /*<< "max="*/ << c.max << "]";
}


//----------------------------------------------------------------------
//INDEX: Heap allocated continuations
//----------------------------------------------------------------------

class HeapContinuation {
public:
	SiCont* _si;
	HeapContinuation* _cont;
private:
	Nword _words[];
public:
	HeapContinuation(SiCont* si, HeapContinuation* cont) :
		_si(si),
		_cont(cont) {
	}
	static void* operator new(size_t base_size, Counter& counter, unsigned extra_words) {
		const unsigned size = base_size + extra_words*sizeof(Nword);
		return my_alloc(counter,size);
	}
	void SetTheFrame() {
		::SetTheFrame(_si->frame_size, _words);
	}
	Nword& frameElem(unsigned i) {
		assert(i<_si->frame_size);
		return _words[i];
	}
};

HeapContinuation* makeHeapContinuation(Counter& counter, SiCont* si, HeapContinuation* cont) {
	return new (counter,si->frame_size) HeapContinuation(si,cont);
}

Ncode PopHeapHandler (); //forward
SiCont SiPopHeapHandler = SiCont("<pop-handler>",Ncode(PopHeapHandler),0);

class HeapHandler : public HeapContinuation {
public:
	SiCont* _si;
	HeapHandler* _handler;
private:
	Nword _words[];
public:
	HeapHandler(SiCont* si, HeapHandler* handler, HeapContinuation* cont) :
		HeapContinuation(&SiPopHeapHandler,cont),
		_si(si),
		_handler(handler) {
	}
	void SetTheFrame() {
		::SetTheFrame(_si->frame_size, _words);
	}
	Nword& frameElem(unsigned i) {
		assert(i<_si->frame_size);
		return _words[i];
	}
};

HeapHandler* makeHeapHandler(Counter& counter, SiCont* si, HeapHandler* handler, HeapContinuation* cont) {
	return new (counter,si->frame_size) HeapHandler(si,handler,cont);
}

HeapContinuation* TheCurrentHeapContinuation = 0;
HeapHandler* TheCurrentHeapHandler = 0;

Ncode HeapReturnWith(Nword res); //forward
Ncode PopHeapHandler () {
	TheCurrentHeapHandler = TheCurrentHeapHandler->_handler;
	return HeapReturnWith(CRET);
}

void HeapSetContFrameElem(unsigned n,Nword v) {
	TheCurrentHeapContinuation->frameElem(n) = v;
}

void HeapSetXcontFrameElem(unsigned n,Nword v) {
	TheCurrentHeapHandler->frameElem(n) = v;
}

void PushHeapContinuation(Counter& counter, SiCont* si) {
	TheCurrentHeapContinuation = makeHeapContinuation(counter,si,TheCurrentHeapContinuation);
}

void PushHeapHandler(Counter& counter, SiCont* si) {
	HeapHandler* H = makeHeapHandler(counter,si,TheCurrentHeapHandler,TheCurrentHeapContinuation);
	TheCurrentHeapHandler = H;
	TheCurrentHeapContinuation = H;
}

Ncode HeapReturnWith(Nword res) {
	assert (res);
	CRET = res;
	if (!TheCurrentHeapContinuation) { return Ncode(0); }
	HeapContinuation* C = TheCurrentHeapContinuation;
	if (showProgress) { cout << "**Return: " << C->_si->name << endl; }
	TheCurrentHeapContinuation = C->_cont;
	C->SetTheFrame();
	char* name = C->_si->name;
	Ncode code = C->_si->code;
	return Jump(name,code);
}

Ncode HeapRaiseWith(Nword res) {
	assert (res);
	XRET = res;
	if (!TheCurrentHeapHandler) {
		cout << "** uncaught exception at top level" << endl;
		return Ncode(0);
	}
	HeapHandler* H = TheCurrentHeapHandler;
	if (showProgress) { cout << "**Raise: " << H->_si->name << endl; }
	TheCurrentHeapContinuation = H->_cont;
	TheCurrentHeapHandler = H->_handler;
	H->SetTheFrame();
	return Jump(H->_si->name, H->_si->code);
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
	SetTheFrame(size,StaticFrameWords);
}

//----------------------------------------------------------------------
//INDEX: Stack allocated continuations
//----------------------------------------------------------------------

const unsigned MaxStackSize = 10000;
char TheStack[MaxStackSize];
char*const TheStackTop = &TheStack[MaxStackSize];
char* TheStackPointer = TheStackTop;

unsigned maxRequiredStackSize = 0;

class StackContinuation {
public:
	SiCont* _si;
	//StackContinuation* _cont;
	Nword _words[];
public:
	StackContinuation(SiCont* si
					  //, StackContinuation* cont
		) :
		_si(si) { //, _cont(cont) {
	}
	static void* operator new(size_t base_size, Counter& counter, unsigned extra_words) {
		const unsigned size = base_size + extra_words*sizeof(Nword);
		//return my_alloc(counter,size);
		counter.inc(size);
		StackAllocation.inc(size);
		assert(TheStackPointer<=TheStackTop);
		//StackDepth.inc(size);
		TheStackPointer -= size;
		unsigned currentStackSize = TheStackTop-TheStackPointer;
		if (currentStackSize > maxRequiredStackSize) { maxRequiredStackSize = currentStackSize; }
		//assert(currentStackSize == StackDepth.count);
		assert(TheStackPointer>=TheStack); //stack-overflow - possible, means we need a bigger stack.
		return TheStackPointer;
	}
	Nword& frameElem(unsigned i) {
		assert(i<_si->frame_size);
		return _words[i];
	}
};

StackContinuation* makeStackContinuation(Counter& counter, SiCont* si
										 //, StackContinuation* cont) 
	){
	return new (counter,si->frame_size) StackContinuation(si
														  //,cont
		);
}

Ncode PopStackHandler (); //forward
SiCont SiPopStackHandler = SiCont("<pop-handler>",Ncode(PopStackHandler),0);

class StackHandler : public StackContinuation {
public:
	SiCont* _si;
	//unsigned _handlerStackDepth;
	StackHandler* _handler;
	Nword _words[];
public:
	StackHandler(SiCont* si, StackHandler* handler
				 //, StackContinuation* cont
		) :
		StackContinuation(&SiPopStackHandler
						  //,cont
			),
		_si(si),
		//_handlerStackDepth(StackDepth.count),
		_handler(handler) {
	}
	Nword& frameElem(unsigned i) {
		assert(i<_si->frame_size);
		return _words[i];
	}
};

StackHandler* makeStackHandler(Counter& counter, SiCont* si, StackHandler* handler
							   //, StackContinuation* cont
	) {
	return new (counter,si->frame_size) StackHandler(si,handler
													 //,cont
		);
}

//StackContinuation* old_TheCurrentStackContinuation = 0;
StackHandler* TheCurrentStackHandler = 0;

StackContinuation* TheCurrentStackContinuation() {
	//assert((!old_TheCurrentStackContinuation && (TheStackPointer == TheStackTop)) || (char*)old_TheCurrentStackContinuation == TheStackPointer);
	//return old_TheCurrentStackContinuation;
	//return (StackContinuation*)TheStackPointer;
	return (TheStackPointer == TheStackTop) ? 0 : (StackContinuation*)TheStackPointer;
}

Ncode StackReturnWith(Nword res); //forward
Ncode PopStackHandler () {

	unsigned size = sizeof(StackHandler) - sizeof(StackContinuation) + TheCurrentStackHandler->_si->frame_size*sizeof(Nword);
	//StackDepth.dec(size);
	TheStackPointer += size;

	TheCurrentStackHandler = TheCurrentStackHandler->_handler;
	return StackReturnWith(CRET);
}

void StackSetContFrameElem(unsigned n,Nword v) {
	TheCurrentStackContinuation()->frameElem(n) = v;
}

void StackSetXcontFrameElem(unsigned n,Nword v) {
	TheCurrentStackHandler->frameElem(n) = v;
}

void PushStackContinuation(Counter& counter, SiCont* si) {
	//old_TheCurrentStackContinuation = 
	makeStackContinuation(counter,si
						  //,TheCurrentStackContinuation()
		);
	//assert((char*)TheCurrentStackContinuation == TheStackPointer);
}

void PushStackHandler(Counter& counter, SiCont* si) {
	StackHandler* H = makeStackHandler(counter,si,TheCurrentStackHandler
									   //,TheCurrentStackContinuation()
		);
	TheCurrentStackHandler = H;

	//old_TheCurrentStackContinuation = H;
	//assert((char*)TheCurrentStackContinuation == TheStackPointer);
}

Ncode StackReturnWith(Nword res) {
	assert (res);
	CRET = res;
	if (!TheCurrentStackContinuation()) { return Ncode(0); }
	if (showProgress) { cout << "**Return: " << TheCurrentStackContinuation()->_si->name << endl; }
	//assert((char*)TheCurrentStackContinuation == TheStackPointer);

	StackContinuation* C = TheCurrentStackContinuation();

	unsigned size = sizeof(StackContinuation) + TheCurrentStackContinuation()->_si->frame_size*sizeof(Nword);
	//StackDepth.dec(size);
	TheStackPointer += size;

	//old_TheCurrentStackContinuation = C->_cont;


	CopyTheFrame (C->_si->frame_size, C->_words);
	return Jump(C->_si->name, C->_si->code);
}

Ncode StackRaiseWith(Nword res) {
	assert (res);
	XRET = res;
	if (!TheCurrentStackHandler) {
		cout << "** uncaught exception at top level" << endl;
		return Ncode(0);
	}
	if (showProgress) { cout << "**Raise: " << TheCurrentStackHandler->_si->name << endl; }

	//assert((char*)TheCurrentStackContinuation == TheStackPointer);


	unsigned size1 = sizeof(StackHandler) + TheCurrentStackHandler->_si->frame_size*sizeof(Nword);
	//unsigned old_size2 = (StackDepth.count - TheCurrentStackHandler->_handlerStackDepth);
	unsigned size2 = ((char*)TheCurrentStackHandler) - ((char*)TheCurrentStackContinuation());
	//assert (old_size2 == size2);
	unsigned size = size1 + size2;

	//StackDepth.dec(size);
	TheStackPointer += size;

	StackHandler* H = TheCurrentStackHandler;
	
	//old_TheCurrentStackContinuation = H->_cont;

	TheCurrentStackHandler = H->_handler;
	CopyTheFrame (H->_si->frame_size, H->_words);
	return Jump(H->_si->name, H->_si->code);
}

//----------------------------------------------------------------------
//INDEX: Select Heap/Stack continuations...
//----------------------------------------------------------------------

bool UseHeapAllocatedContinuation = false;

void SetContFrameElem(unsigned n,Nword v) {
	if (UseHeapAllocatedContinuation) {
		HeapSetContFrameElem(n,v);
	} else {
		StackSetContFrameElem(n,v);
	}
}

void SetXcontFrameElem(unsigned n,Nword v) {
	if (UseHeapAllocatedContinuation) {
		HeapSetXcontFrameElem(n,v);
	} else {
		StackSetXcontFrameElem(n,v);
	}
}

void PushContinuation(Counter& counter, SiCont* si) {
	if (UseHeapAllocatedContinuation) {
		PushHeapContinuation(counter,si);
	} else {
		PushStackContinuation(counter,si);
	}
}

void PushHandler(Counter& counter, SiCont* si) {
	if (UseHeapAllocatedContinuation) {
		PushHeapHandler(counter,si);
	} else {
		PushStackHandler(counter,si);
	}
}

Ncode ReturnWith(Nword res) {
	if (UseHeapAllocatedContinuation) {
		return HeapReturnWith(res);
	} else {
		return StackReturnWith(res);
	}
}

Ncode RaiseWith(Nword res) {
	if (UseHeapAllocatedContinuation) {
		return HeapRaiseWith(res);
	} else {
		return StackRaiseWith(res);
	}
}

	for (unsigned i = argc; i>1; ) { //reverse order
		--i;
		std::string s(argv[i]);
		//cout << "**main:command_args: " << cp << endl;
		if (s == "-debug") {
			debug = true;
		} else if (s == "-heap") {
			UseHeapAllocatedContinuation = true;
		} else {
			command_args = makeListCons(makeString(s),command_args);
		}
	}

	if (UseHeapAllocatedContinuation) {
		AssertWords(2,HeapContinuation);
		AssertWords(4,HeapHandler);
	} else {
		AssertWords(1,StackContinuation);
		AssertWords(3,StackHandler);
	}



	unsigned size = sizeof(Continuation) + si->frame_size*sizeof(Nword);
	new (stack_alloc(counter,size)) Continuation(si);

	unsigned size = sizeof(Handler) + si->frame_size*sizeof(Nword);
	Handler* H = new (stack_alloc(counter,size)) Handler(si,TheCurrentHandler);


Ncode PopHandler () {
	//assert(TheStackPointer-sizeof(Continuation) == (char*)TheCurrentHandler);
	//unsigned size = sizeof(Handler) - sizeof(Continuation) + TheCurrentHandler->si->frame_size*sizeof(Nword);
	assert(TheStackPointer == (char*)TheCurrentHandler);
	unsigned size = sizeof(Handler) + TheCurrentHandler->si->frame_size*sizeof(Nword);
	TheStackPointer += size;
	TheCurrentHandler = TheCurrentHandler->handler;
	return ReturnWith(CRET);
}

	//if (StackPointer == StackTop) { return Ncode(0); }
//  	if (!TheCurrentHandler) {
//  		cout << "** uncaught exception at top level" << endl;
//  		return Ncode(0);
//  	}

//----------------------------------------------------------------------
//INDEX: Fri Nov 24 21:15:16 2006
//----------------------------------------------------------------------

//  Nword makeCon0_arity(unsigned tag, unsigned arity) {
//  	if (arity==0) { return makeCon0(tag); }
//  	return makeCon0_closure(tag);
//  }



	//return makeCon0_arity(tag,0); //hack - need arity
	//Nword w = makeCon0(tag);

//  Nword g_MakeException(char* name) {
//  	cout << "**g_MakeException: " << name << endl;
//  	static unsigned N = 100;
//  	//Nword w = makeCon0(++N);
//  	Nword w = makeExCon0(name,++N);
//  	cout << "**g_MakeException: " << name << " -> " << w << endl;
//  	return w;
//  }


//  Value_ExCon0* getEx0(Nword w) {
//  	if (Value_ExCon0* x = dynamic_cast<Value_ExCon0*>(w)) { return x; }
//  	if (Value_ExCon1* x = dynamic_cast<Value_ExCon1*>(w)) { return x; }
//  	cout << "ERROR/getEx0: " << w->what() << endl;
//  	TYPE_ERROR;
//  }

//  std::string getExName(Nword w) { return getEx0(w)->_name; }
//  unsigned getExTag(Nword w) { return getEx0(w)->_n; }


//----------------------------------------------------------------------
//INDEX: Wed Nov 29 15:49:10 2006
//----------------------------------------------------------------------

//----------------------------------------------------------------------
//INDEX: heap_alloc - FAKE - just leaks
//----------------------------------------------------------------------

void* heap_alloc(Counter& counter, size_t size) {
	static unsigned total_megs = 0;
	assert(size%4==0);
	counter.inc(size);
	HeapAllocation.inc(size);
	unsigned new_total_megs = HeapAllocation.count/1000000;
	if (debug && new_total_megs > total_megs) {
		total_megs = new_total_megs;
		print_stats("on-meg");
	}
	return new char[size];
}


	//static unsigned total_megs = 0;
	unsigned new_total_megs = HeapAllocation.count/OneMeg;
	if (debug && new_total_megs > total_megs) {
		total_megs = new_total_megs;
		print_stats("on-meg");
	}

	
	if (byteNum(HeapPointer) >= 5775000) {
		progress = true;
	}
	if (byteNum(HeapPointer) >= 5790000) {
		progress = false;
	}


	unsigned size = 2000; // big enough I hope!
	unsigned size = 0; // irelevant for current unlimited heap
	static bool first = true;
	if (first && HeapPointer-TheHeap +size> (int)1*OneMeg) {
		first = false;
		gc(); 
	}
	static bool second = true;
	if (second && HeapPointer-TheHeap +size> (int)2*OneMeg) {
		second = false;
		gc(); 
	}

	static bool first = true;
	if (first && (HeapPointer-HeapValidityPointer > (int)OneMeg)) {
		//first = false;
		gc(); 
	}
	static bool second = true;
	if (!first && second && (HeapPointer-HeapValidityPointer > (int)OneMeg)) {
		second = false;
		gc(); 
	}


void gc() {

	static unsigned gc_count = 0;
	++ gc_count;
	
	//cout << "***GC(start): hp=" << byteNum(HeapPointer) << endl;

	char* oldHP = HeapPointer;
	char* chasePointer = HeapPointer;

	// evacuate roots...
	for (Nword** root = all_roots; *root; ++root) {
		//cout << "**evacuate root(args): " << *root << endl;
		evacuate(**root);
	}
	for (unsigned i = 0; i < TheFrameSize; ++i) {
		//cout << "**evacuate root(frame): " << i << endl;
		evacuate(TheFramePointer[i]);
	}
	// evacuate roots on stack..

	//cout << "**ScavangeStack... depth=" << StackPointer-StackTop << endl;
	ScavangeStack();
	//cout << "**ScavangeStack...done" << endl;

	//cout << "**Scavange Init Heap..." << endl;
	for (char* hp = TheHeap; hp < HeapAfterInit; ) {
		Hob* hob = reinterpret_cast<Hob*>(hp);
		//cout << "*scavange(init heap): " << byteNum(hp) << " / " << byteNum(HeapPointer) << " - " << hob->what() << endl;
		hob->scavenge();
		hp += hob->bytes();
	}
	//cout << "**Scavange Init Heap...done" << endl;

	//cout << "**main Scavange loop..." << endl;
	while (chasePointer < HeapPointer) {
		Hob* hob = reinterpret_cast<Hob*>(chasePointer);
		//cout << "*scavange: " << byteNum(chasePointer) << " / " << byteNum(HeapPointer) << " - " << hob->what() << endl;
		hob->scavenge();
		chasePointer += hob->bytes();
	}
	//cout << "**main Scavange loop...done" << endl;

	unsigned A = byteNum(HeapValidityPointer);
	unsigned B = byteNum(oldHP);
	unsigned C = byteNum(HeapPointer);

	unsigned Q = B - A;
	unsigned L = C- B;

//  	cout << "***GC(end)  : hp=" << byteNum(HeapPointer) 
//  		 << " - liveCount = " << liveCount << " [" << (100*liveCount/byteNum(oldHP)) << "%]" << endl;

	cout << "***GC(" << gc_count << ")  - "
		 << A << " / " << B << " / " << C << " : " << Q << " -> " << L
		 << " [" << (100*L/Q) << "%]" << endl;

	//cout << "***gc() - invalidate..." <<endl;
	for (char* invalidatePointer = HeapValidityPointer; invalidatePointer < oldHP; ) {
		Hob* hob = reinterpret_cast<Hob*>(invalidatePointer);
		//cout << "*invalidate: " << byteNum(hob) << " - " << hob->what() << endl;
		//cout << "*invalidate: " << byteNum(hob) ; flush(cout); cout << " - " << hob->what() << endl;
		hob->m_invalidated = true; //SET INVALIDATED
		invalidatePointer += hob->bytes();
	}
	//cout << "***gc() - invalidate...done" <<endl;

	HeapValidityPointer = oldHP;
}


Nword TheCurrentFunction;

 	//&TheCurrentFunction,




//... also: frame, stack, static roots in compiled program 
Nword* all_roots[] = { 
	&CRET, //commented out - bug on purpose - check heap validity checking!
	&XRET, 
	&TheArg0,
	&TheArg1, &TheArg2, &TheArg3, &TheArg4,
	&TheArg5, &TheArg6, &TheArg7,
	0
}; 

	for (Nword** root = all_roots; *root; ++root) {
		evacuate(**root);
	}


//  	// evacuate roots; roots depend on on whether we are in closure/continuation/handler
//  	if (inContinuation) { top_evacuate(CRET); } 
//  	if (inHandler) { top_evacuate(XRET); }
	
//  	if (!(inContinuation || inHandler)) { // in closure
//  		unsigned N = TheCurrentSiClosure->num_args;
//  		for (unsigned i = 0; i < N ; ++i) {
//  			top_evacuate(GetArgVar(i));
//  		}
//  	}
//  	if (inContinuation || inHandler) {
//  		for (unsigned i = 0; i < TheFrameSize; ++i) {
//  			top_evacuate(TheFramePointer[i]);
//  		}
//  	} else {
//  		top_evacuate(TheCurrentFunction);
//  	}



//  	// Scavenge loop..
//  	while (chasePointer < HeapPointer) {
//  		Hob* hob = reinterpret_cast<Hob*>(chasePointer);
//  		//cout << "*scavange: " << byteNum(chasePointer) << " / " << byteNum(HeapPointer) << " - " << hob->what() << endl;
//  		hob->scavenge();
//  		chasePointer += hob->bytes();
//  	}


//  	unsigned a = byteNum(getHob(before));
//  	unsigned b = byteNum(getHob(after));
//  //  	bool c= (a==32092116) || (a==32090956)
//  //  		||  (b==32092116) || (b==32090956)
//  //  		;
//  	bool c= ((a>=32090000) && (a<=32092000))
//  		||  ((b>=32090000) && (b<=32092000))
//  		;


//  		unsigned a = byteNum(hob);
//  		//bool c= (a==32092116) || (a==32090956);
//  		bool c= (a>=32090000) && (a<=32092000);
//  		if (c) {
//  		}
		


//  	if (gc_count >= 32 ) {
//  		progress = true;
//  	}

//  	cout << "***GC(" << gc_count << ")... " 
//  		 << TheCurrentSiClosure->name
//  		 << "/"
//  		 << TheCurrentSiClosure->num_args
//  		 << ", CRET?="<<inContinuation
//  		 << ", XRET?="<<inHandler
//  		 << endl;



Ncode Jump(char* name, Ncode code) {

//  	unsigned allocSinceLastJump = HeapPointer - HP_onLastJump;
//  	if (allocSinceLastJump > maxAllocBetweenJumps) { maxAllocBetweenJumps = allocSinceLastJump; }
//  	//cout << "**Jump: " << name << "allocSinceLastJump = " << allocSinceLastJump << endl;
//  	// We only dare do GC when we arejumping from one code sequence to another
//  	// because is the only time we can find all the roots.
//  	if (HeapPointer-HeapValidityPointer > (int)(1*OneMeg)) {
//  		gc(); 
//  	}
//  	HP_onLastJump = HeapPointer;

	//cout << "**Jump: " << name << endl;
	maybe_gc();
	return code;
}



Ncode Enter(Closure* closure) {
	//char* name = closure->getClosureName();
	//cout << "**Enter: " << name << endl;

	//TheCurrentSiClosure = closure->si;
	TheCurrentFunction = closure;
	//closure->SetFrameReference();
	CRET = 0; XRET = 0;

// cant use Jump for closures - because we must set the FramePointer (reference)
// after GC, if it is done.

	//return Jump(name, closure->getClosureCode());
	
	Ncode code = closure->getClosureCode();
	maybe_gc();
	//closure->SetFrameReference();
	getClosure_raw(TheCurrentFunction)->SetFrameReference();

	return code;
}

//  	TheCurrentSiClosure = closure->si;
//  	TheCurrentFunction = closure;
//  	closure->SetFrameReference();
//  	CRET = 0; XRET = 0;
//  	return Jump(name, closure->getClosureCode());
	return Enter(closure);


//  Ncode Jump(char* name, Ncode code) {
//  	if (progress) { cout << "**Jump: " << name << endl; }
//  	maybe_gc();
//  	return code;
//  }


//  	CopyTheFrame (CC->si->frame_size, CC->words);
//  	return Jump(CC->si->name, CC->si->code);

//  	CopyTheFrame (CH->si->frame_size, CH->words);
//  	return Jump(CH->si->name, CH->si->code);

//Continuation*& TheCurrentContinuation = (Continuation*)StackPointer;

	//unsigned size = sizeof(Continuation) + CC->si->frame_size*sizeof(Nword);
	//unsigned size = sizeof(Handler) + TheCurrentHandler->si->frame_size*sizeof(Nword);
	//unsigned size = sizeof(Handler) + TheCurrentHandler->si->frame_size*sizeof(Nword);

//  	unsigned getNumArgs() {
//  		return si->num_args;
//  	}

	//unsigned num_formal_args = closure->getNumArgs();

	Ncode getClosureCode() {
		return si->code;
	}
	Ncode code = closure->getClosureCode();

	char* getClosureName() {
		return si->name;
	}


	virtual unsigned bytes() {
  		cout << "*bytes (dummy) : " << what() << endl;
		assert(0);
	}
	virtual Nword evacuate() {
  		cout << "**evacuate (dummy) : " << what() << endl;
		assert(0);
  	}

	virtual void scavenge() {
  		cout << "*scanvenge (dummy) : " << what() << endl;
		assert(0);
  	}

//----------------------------------------------------------------------
//INDEX: Fri Dec  1 18:44:09 2006
//----------------------------------------------------------------------


//const unsigned MaxHeapSize = 100*OneMeg;
//const unsigned MaxHeapSize = 11*OneMeg;
//const unsigned InitHeapSize = 10*OneMeg;
//const unsigned InitHeapSize = 1*OneMeg; //only for init-heap-data


//char* HeapAfterInit; //assigned later - used as base for invalidating

//char* HeapValidityPointer = 0;

//char* OldHP = 0;

//const unsigned MaxAlloctaionWhichMightOccurBeforeNextJump = 20; //too small

//const unsigned HeapSpaceSize = OneMeg + MaxAlloctaionWhichMightOccurBeforeNextJump;

//const unsigned HeapSpaceSize = 10 * OneMeg;


 //where object are read; where aobject are allocated during non-GC time
//HeapSpace currentSpace(0,0);
//HeapSpace currentSpace(TheInitHeap,TheInitHeap+HeapSpaceSize);

//HeapSpace currentSpace = initSpace;


bool IsWordInInitHeap(Hob* hob) {
//  	assert(TheInitHeap == initSpace.base);
//  	assert(HeapAfterInit == initSpace.top);
	char* hp = reinterpret_cast<char*>(hob);
	//return (TheInitHeap <= hp) && (hp < HeapAfterInit);
	return (initSpace.base <= hp) && (hp < initSpace.top);
}

bool IsWordInValidHeap(Hob* hob) {
	//assert(HeapValidityPointer == currentSpace.base);
	char* hp = reinterpret_cast<char*>(hob);
	//return (HeapValidityPointer <= hp) && (hp < HeapPointer);
	//return (currentSpace.base <= hp) && (hp < HeapPointer); //tighter, but maybe wrong when we alternate heaps
	return (currentSpace.base <= hp) && (hp < currentSpace.top);
}


bool IsWordInInitHeap(Hob* hob) {
//  	char* hp = reinterpret_cast<char*>(hob);
//  	return (initSpace.base <= hp) && (hp < initSpace.top);
	return initSpace.inSpace(hob);
}

bool IsWordInValidHeap(Hob* hob) {
//  	char* hp = reinterpret_cast<char*>(hob);
//  	return (currentSpace.base <= hp) && (hp < currentSpace.top);
	return currentSpace.inSpace(hob);
}

Hob* getHob(Nword w) {
	if (Hob* hob = w._hob) {
		bool invalid1 = 0; //hob->m_invalidated;
		bool invalid2 = 0; //hob->is_invalid();
		bool invalid3 = !(IsWordInInitHeap(hob) || IsWordInValidHeap(hob));
		if (invalid1 || invalid2 || invalid3) {
			cout << "ERROR/getHob/invalid: " << hob->what() << " - " << byteNum(hob) << " - "
				 << (invalid1?"1":"0") << ","
				 << (invalid2?"1":"0") << ","
				 << (invalid3?"1":"0") << endl;
			assert(0);
		}
		return hob;
	}
	return 0; 
	//assert(0); //maybe dont ever allow this!
}

//  	char* evacee = reinterpret_cast<char*>(getHob(before));
//  	bool newlyAllocated = (OldHP <= evacee) && (evacee < HeapPointer);
//  	if (newlyAllocated) {
//  		cout << "**OldHP =  " << byteNum(OldHP) << "..." << endl;
//  		cout << "**HeapPointer =  " << byteNum(HeapPointer) << "..." << endl;
//  		cout << "**BAD evacuate: " << byteNum(getHob(before)) << "..." << endl;
//  		assert(0);
//  	}

//  	bool already_evacuated_1 = getHob(before->m_evacuated);
//  	bool already_evacuated_2 = before->is_broken_heart();
//  	assert(already_evacuated_1 == already_evacuated_2);
//  	bool already_evacuated = already_evacuated_1;

//  	if (already_evacuated) {
//  		extra_info = " (already-moved)"; 
//  		after = before->m_evacuated;
//  	} 
//  	else

		//if (getHob(after) == getHob(before)) { extra_info = " *Not-Moved*"; }
		//assert(getHob_unchecked(after) != getHob(before)); //ensure is moved

		//before->m_evacuated = after;



void gc() {

	static unsigned gc_count = 0;
	++ gc_count;
	cout << "***GC(" << gc_count << ")... " << endl;

	bool inContinuation = isHob(CRET);
	bool inHandler = isHob(XRET);

	assert(TheCurrentSiClosure);
	
	//OldHP = HeapPointer;
	
	unsigned unusedSpaceAtEndOfHeap = currentSpace.top - HeapPointer;

	//HeapPointer += unusedSpaceAtEndOfHeap; //bumo up HeapPointer, so heap section are not consec

	//HeapPointer = new char[HeapSpaceSize]; // just magic up a brand new heap space - just a dev stage, on way to two heaps

	// really try flipping between two spaces for the first time..
	static bool inA = true;
	HeapPointer = inA ? SpaceA : SpaceB;
	inA = !inA;

	HeapSpace nextSpace; //where objects are allocated during GC

	nextSpace.base = HeapPointer;
	nextSpace.top = nextSpace.base + HeapSpaceSize;

	//assert(nextSpace.top < InitHeapTop);


	allocSpace = nextSpace;
	
	//char* chasePointer = HeapPointer;

	// evacuate roots; roots depend on on whether we are in closure/continuation/handler
	cout << "-gc: evacuate roots\n";
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
	
	cout << "- gc: scavange stack\n";
	ScavangeStack();

	cout << "- gc: scavange init heap\n";
	//for (char* hp = TheInitHeap; hp < HeapAfterInit; )
	for (char* hp = initSpace.base; hp < initSpace.top; )
	{
		Hob* hob = reinterpret_cast<Hob*>(hp);
		//cout << "*scavange(init heap): " << byteNum(hp) << " / " << byteNum(HeapPointer) << " - " << hob->what() << endl;
		hob->scavenge();
		hp += hob->bytes();
	}

	cout << "- gc: main scavange loop\n";

	//assert(OldHP == chasePointer);

	// Scavenge loop..
	//assert(nextSpace.base == OldHP);

	for (char* hp = nextSpace.base; hp < HeapPointer; ) {
		Hob* hob = reinterpret_cast<Hob*>(hp);

		if (progress) {
			cout << "*scavange: " << byteNum(hp) << " / " << byteNum(HeapPointer) << " - " << hob->what() << endl;
		}

		hob->scavenge();
		//hob->m_scavenged = true;

		hp += hob->bytes();
	}

	//assert(HeapValidityPointer == currentSpace.base);

	//assert(nextSpace.base == OldHP);

	// invalidate old heap - expensive! - just for dev.
	cout << "- gc: invalidate\n";

	//assert(HeapValidityPointer == currentSpace.base);


	//unsigned spaceOverlap = currentSpace.top - nextSpace.base ;
	//assert (spaceOverlap == unusedSpaceAtEndOfHeap);
	//cout << "** space overlap - " << spaceOverlap << endl;

//  	assert(nextSpace.base == OldHP);
//  	assert(currentSpace.top == OldHP + spaceOverlap);
//  	assert(currentSpace.top - spaceOverlap == OldHP);

	for (char* hp = currentSpace.base; hp < currentSpace.top - unusedSpaceAtEndOfHeap; ) {
		Hob* hob = reinterpret_cast<Hob*>(hp);
		unsigned B = hob->bytes();
		Invalidate(getHob(hob));
		//hob->m_invalidated = true; //SET INVALIDATED
		hp += B;
	}
	//cout << "- gc: invalidate..done\n";

	
//  	unsigned A = byteNum(currentSpace.base);
//  	unsigned B = byteNum(nextSpace.base);
//  	unsigned C = byteNum(HeapPointer);
//  	unsigned Q = B - A;
//  	unsigned L = C- B;

	unsigned Q = currentSpace.top - currentSpace.base;
	unsigned L = HeapPointer - nextSpace.base;

	cout << "***GC(" << gc_count << ")  - "
		//<< A << " / " << B << " / " << C << " : " 
		 << Q << " -> " << L
		 << " [" << (100*L/Q) << "%]" 
		 << endl;

	//assert(nextSpace.base == OldHP);

	//HeapValidityPointer = OldHP;
//  	currentSpace.base = OldHP;
//  	currentSpace.top = OldHP + HeapSpaceSize;

	currentSpace = nextSpace;

//  	assert (currentSpace.base == nextSpace.base);
//  	assert (currentSpace.top == nextSpace.top);
}

//char* HP_onLastJump = TheInitHeap;


unsigned maxAllocBetweenJumps = 0;

//  	unsigned allocSinceLastJump = HeapPointer - HP_onLastJump;
//  	if (allocSinceLastJump > maxAllocBetweenJumps) { maxAllocBetweenJumps = allocSinceLastJump; }

	//bool need2gc = (HeapPointer-HeapValidityPointer > (int)(1*OneMeg));

//  	char* CurrentHeapT = HeapValidityPointer + HeapSpaceSize;
//  	assert (CurrentHeapT == currentSpace.top);

	//bool need2gc = HeapPointer + MaxAlloctaionWhichMightOccurBeforeNextJump > CurrentHeapT;
	//if (HeapPointer-HeapValidityPointer > (int)(1*OneMeg)) { gc(); }

	//HP_onLastJump = HeapPointer;
		cout << "** maxAllocBetweenJumps = " << maxAllocBetweenJumps << endl;
	//for (char* hp = TheInitHeap; hp < HeapAfterInit; )

	//assert(HeapValidityPointer == currentSpace.base);

	//assert(nextSpace.base == OldHP);

	// invalidate old heap - expensive! - just for dev.

	//assert(HeapValidityPointer == currentSpace.base);


	//unsigned spaceOverlap = currentSpace.top - nextSpace.base ;
	//assert (spaceOverlap == unusedSpaceAtEndOfHeap);
	//cout << "** space overlap - " << spaceOverlap << endl;

//  	assert(nextSpace.base == OldHP);
//  	assert(currentSpace.top == OldHP + spaceOverlap);
//  	assert(currentSpace.top - spaceOverlap == OldHP);

		//hob->m_invalidated = true; //SET INVALIDATED	
//  	unsigned A = byteNum(currentSpace.base);
//  	unsigned B = byteNum(nextSpace.base);
//  	unsigned C = byteNum(HeapPointer);
//  	unsigned Q = B - A;
//  	unsigned L = C- B;

	unsigned Q = currentSpace.top - currentSpace.base;
	unsigned L = HeapPointer - nextSpace.base;

	cout << "***GC(" << gc_count << ")  - "
		//<< A << " / " << B << " / " << C << " : " 
		 << Q << " -> " << L
		 << " [" << (100*L/Q) << "%]" 
		 << endl;


	//assert(nextSpace.base == OldHP);

	//HeapValidityPointer = OldHP;
//  	currentSpace.base = OldHP;
//  	currentSpace.top = OldHP + HeapSpaceSize;

	currentSpace = nextSpace;

//  	assert (currentSpace.base == nextSpace.base);
//  	assert (currentSpace.top == nextSpace.top);
}


class Hob {
public:
	virtual ~Hob() {}
	virtual std::string what() =0;
	virtual bool equalTo(Nword) =0;
	static void* operator new(size_t size, Counter& counter) {
		return heap_alloc(counter,size);
	}
	static void* operator new(size_t size, Counter& counter, unsigned extra_words) {
		return heap_alloc(counter,size+extra_words*sizeof(Nword));
	}
	virtual unsigned bytes() =0;
	virtual Nword evacuate() =0;
//  	void scavenge_top() {
//  		//assert(!m_scavenged);
//  		scavenge();
//    	}
//  	virtual bool is_invalid() { return false; } //HERE
//  	virtual bool is_broken_heart() { return false; }
public:
	virtual void scavenge() =0;
public:
	// temp hack - location where object has been evacuated to. really should use broken-heart
	//Nword m_evacuated;
	//bool m_invalidated :1;
	//bool m_scavenged :1;
};


//  	HeapSpace nextSpace; //where objects are allocated during GC
//  	nextSpace.base = HeapPointer;
//  	nextSpace.top = nextSpace.base + HeapSpaceSize;

  	allocSpace = HeapSpace(HeapPointer,HeapPointer + HeapSpaceSize);

	//allocSpace = nextSpace;


//  	HeapPointer = inA ? SpaceA : SpaceB;
//  	inA = !inA;
//    	allocSpace = HeapSpace(HeapPointer,HeapPointer + HeapSpaceSize);


	//cout << "main(): HP=" << HeapPointer-TheInitHeap << endl;
	//cout << "after args: HP=" << HeapPointer-TheInitHeap << endl;
	//cout << "after init: HP=" << HeapPointer-TheInitHeap << endl;

//  	HeapPointer = SpaceB;
//  	currentSpace.base = HeapPointer;
//  	currentSpace.top = HeapPointer + HeapSpaceSize;
//  	allocSpace = currentSpace;


//----------------------------------------------------------------------
//INDEX: Mon Dec  4 23:05:53 2006
//----------------------------------------------------------------------

class Value_Tuple;

Value_Tuple* getTuple(Nword);

class Value_Tuple : public Hob {
public:
	//std::string what() { return "Tuple"; }
	bool equalTo(Nword v) {
		for (unsigned i=0; i<size(); ++i) {
			if (!tupleElem(i)->equalTo(getTuple(v)->tupleElem(i))) return false;
		}
		return true;
	}
	virtual unsigned size() =0;
	virtual Nword& tupleElem(unsigned) { assert(0); }
};


class Value_Tuple_VarN : public Value_Tuple {
	unsigned _n;
	Nword _words[];
public:
	Value_Tuple_VarN(unsigned n) : _n(n) {}
	std::string what() { return "Tuple_VarN"; }
	virtual unsigned size() { return _n; }
	virtual Nword& tupleElem(unsigned i) {
		assert(i<_n);
		return _words[i];
	}
	unsigned bytes() { return HobBytes(_n); }
	Nword evacuate() {
		Value_Tuple_VarN* res = new (EvacuationAllocation,_n) Value_Tuple_VarN(_n);
		CopyWords(_n,_words,res->_words);
		return res;
	}
	void scavenge() {
		EvacuateWords(_n,_words);
	}
};

template<unsigned N>
class Value_Tuple_N : public Value_Tuple {
	Nword _words[N];
public:
	Value_Tuple_N() {
		if (show_alloc_progress) { cout << "**Value_Tuple_N: " << N << " -> " << byteNum(this) << endl; }
	}
	std::string what() { return "Tuple_N"; }
	virtual unsigned size() { return N; }
	virtual Nword& tupleElem(unsigned n) {
		assert(n<N);
		return _words[n];
	}
	unsigned bytes() { 
		//unsigned B = HobBytes(0); // **not**  HobBytes(N);
		//cout << "**bytes : Value_Tuple_N<" << N << ">" << " -> " << B << endl;
		//return B;
		return HobBytes(0); // **not**  HobBytes(N);
	}
	Nword evacuate() {
		Value_Tuple_N<N>* res = new (EvacuationAllocation) Value_Tuple_N<N>();
		CopyWords(N,_words,res->_words);
		return res;
	}
	void scavenge() {
		EvacuateWords(N,_words);
	}
};


Counter count_tuples("#T");
Counter count_pairs("#T2");
Counter count_trips("#T3");
Counter count_quads("#T4");
Counter count_quins("#T5");
Counter count_hexs("#T6");
Counter count_heps_plus("#T7+");

Nword makeTuple(unsigned n) {
	count_tuples.inc(1); 
	//std::cout << "**makeTuple: " << n << std::endl;
	if (n==2) { count_pairs.inc(1); return new (tuple_allocation) Value_Tuple_N<2>(); }
	if (n==3) { count_trips.inc(1); return new (tuple_allocation) Value_Tuple_N<3>(); }
	if (n==4) { count_quads.inc(1); return new (tuple_allocation) Value_Tuple_N<4>(); }
	if (n==5) { count_quins.inc(1); return new (tuple_allocation) Value_Tuple_N<5>(); }
	if (n==6) { count_hexs.inc(1);  return new (tuple_allocation) Value_Tuple_N<6>(); }
	count_heps_plus.inc(1); 
	return new (tuple_allocation,n) Value_Tuple_VarN(n);
	//cout << "**makeTuple(not yet supported): " << n << endl;
	//NOT_YET;
	//return new (tuple_allocation) Value_Tuple_N<n>();
}

Value_Tuple* getTuple(Nword w) {
	if (Value_Tuple* x = dynamic_cast<Value_Tuple*>(getHob(w))) { return x; }
	cout << "ERROR/getTuple: " << w->what() << endl;
	TYPE_ERROR;
}

Nword getTupleElem(Nword w,unsigned n) {
	return getTuple(w)->tupleElem(n);
}

void setTupleElem(Nword w,unsigned n,Nword v) {
	getTuple(w)->tupleElem(n) = v;
}



class Value_Con1 : public Hob {
public:
	unsigned tag;
	Nword word;
	Value_Con1(unsigned tag_, Nword word_) : tag(tag_), word(word_) {
		//if (show_alloc_progress) { cout << "**Value_Con1: " << tag << " -> " << byteNum(this) << endl; }
	}
	std::string what() { return "Con1"; }
	bool equalTo(Nword v) { return tag == getTag(v) && word->equalTo(getCon(v)); }
	unsigned bytes() { return HobBytes(0); }
	Nword evacuate() { return new (EvacuationAllocation) Value_Con1(tag,word); }
	void scavenge() { ::top_evacuate(word); }
};

unsigned getTag(Nword w) {
	if (Value_Con0* x = dynamic_cast<Value_Con0*>(getHob(w))) { return x->tag; }
	if (Value_Con1* x = dynamic_cast<Value_Con1*>(getHob(w))) { return x->tag; }
	cout << "ERROR/getTag: " << w->what() << endl;
	TYPE_ERROR;
}


Counter count_tuples("#T");
Counter count_pairs("#T2");
Counter count_trips("#T3");
Counter count_quads("#T4");
Counter count_quins("#T5");
Counter count_hexs("#T6");
Counter count_heps_plus("#T7+");

Nword makeTuple(unsigned n) {
	count_tuples.inc(1); 
	//std::cout << "**makeTuple: " << n << std::endl;
	if (n==2) { count_pairs.inc(1); return new (tuple_allocation,2) Value_Tuple_N<2>(); }
	if (n==3) { count_trips.inc(1); return new (tuple_allocation,3) Value_Tuple_N<3>(); }
	if (n==4) { count_quads.inc(1); return new (tuple_allocation,4) Value_Tuple_N<4>(); }
	if (n==5) { count_quins.inc(1); return new (tuple_allocation,5) Value_Tuple_N<5>(); }
	if (n==6) { count_hexs.inc(1);  return new (tuple_allocation,6) Value_Tuple_N<6>(); }
	count_heps_plus.inc(1); 
	if (n==7) { count_hexs.inc(1);  return new (tuple_allocation,7) Value_Tuple_N<7>(); }
	if (n==8) { count_hexs.inc(1);  return new (tuple_allocation,8) Value_Tuple_N<8>(); }
	if (n==31) { count_hexs.inc(1);  return new (tuple_allocation,31) Value_Tuple_N<31>(); }
	cout << "**makeTuple(not yet supported): " << n << endl;
	NOT_YET;
	//return new (tuple_allocation,n) Value_Tuple_VarN(n);
}


//  	cout << "**Tuples(" << tag << ") "
//  		 << count_tuples << " (" 
//  		 << count_pairs << "," 
//  		 << count_trips << "," 
//  		 << count_quads << "," 
//  		 << count_quins << "," 
//  		 << count_hexs << "," 
//  		 << count_heps_plus << ")" 
//  		 << endl;

//----------------------------------------------------------------------
//INDEX: Tue Dec  5 01:33:19 2006
//----------------------------------------------------------------------

//  	static void* operator new(size_t size, Counter& counter, unsigned extra_words) {
//  		return heap_alloc(counter,size+extra_words*sizeof(Nword));
//  	}


//----------------------------------------------------------------------
//INDEX: Invalid Heap Object
//----------------------------------------------------------------------

//  class Hob_Invalid : public Hob {
//  public:
//  	Hob_Invalid() {}
//  	static void* operator new(size_t size, void* p) {
//  		assert(size == sizeof(Hob_Invalid));
//  		return p;
//  	}
//  	std::string what() { return "Invalid-Heap-Object"; }
//  	bool is_invalid() { return true; }
//  	bool equalTo(Nword v) { assert(0); }
//  	unsigned bytes() { assert(0); }
//  	Nword evacuate() { assert(0); }
//  	void scavenge() { assert(0); }
//  };

//  void Invalidate(Hob* w) {
//  	if (show_gc_progress) { cout << "**Invalidate: " << w << endl; }
//  	new (w) Hob_Invalid(); //placement new - to overwrite old object with Invalid Object
//  }


unsigned byteNum(void* p) {
	assert(p);
	//int res = reinterpret_cast<char*>(p) - TheInitHeap;
	int res = reinterpret_cast<char*>(p) - (char*)0;
	assert (res>=0);
	return res;
}

Hob* getHob_unchecked(Nword w) {
	return *(reinterpret_cast<Hob**>(&w));
}

Hob* getHob(Nword w) {
	if (Hob* hob = w._hob) {
		bool invalid = !(initSpace.inSpace(hob) || currentSpace.inSpace(hob));
		if (invalid) {
			cout << "ERROR/getHob/invalid: " << hob->what() << " - " << byteNum(hob) << endl;
			assert(0);
		}
		return hob;
	}
	return 0; 
	//assert(0); //maybe dont ever allow this!
}

bool isHob(Nword w) { return !!(getHob(w)); }


		} else if (s == "-gc-progress") {
			show_gc_progress = true;

bool show_gc_progress = false;
	if (show_gc_progress) { cout << "**BrokenHeart: " << after << " -> " << before << endl; }

	if (show_gc_progress) {
		cout << "**evacuate: " 
			 << byteNum(getHob(before)) << " --> " << byteNum(getHob_unchecked(after))
			 << " [" << before->what() << "]"
			 << extra_info
			 << endl;
	}


void top_evacuate(Nword& before) {

	if (!getHob(before)) {
		cout << "**evacuate: <null>" << endl;
		return;
	}

	Hob* hob = getHob(before);
	assert(hob);

	Nword after; // = 0;
	string extra_info = "";

	if (initSpace.inSpace(hob)) {
		extra_info = " (init-heap, so unmoved)"; 
		after = before;
	} else {
		extra_info = " *move*"; 
		after = before->evacuate();
		// would like to avoid repeatedly setting this broken heart
		SetBrokenHeart(getHob(before),getHob_unchecked(after));
	}

	before = after;
}


	//cout << "- gc: main scavange loop\n";
	for (char* hp = allocSpace.base; hp < HeapPointer; ) {
		Hob* hob = reinterpret_cast<Hob*>(hp);
		if (show_gc_progress) {
			cout << "*scavange: " << byteNum(hp) << " / " << byteNum(HeapPointer) << " - " << hob->what() << endl;
		}
		hob->scavenge();
		hp += hob->bytes();
	}

//  	//cout << "- gc: invalidate\n";
//  	for (char* hp = currentSpace.base; hp < currentSpace.top - unusedSpaceAtEndOfHeap; ) {
//  		Hob* hob = reinterpret_cast<Hob*>(hp);
//  		unsigned B = hob->bytes();
//  		Invalidate(getHob(hob));
//  		hp += B;
//  	}


	//cout << "- gc: scavange init heap\n";
	for (char* hp = initSpace.base; hp < initSpace.top; ) {
		Hob* hob = reinterpret_cast<Hob*>(hp);
		//cout << "*scavange(init heap): " << byteNum(hp) << " / " << byteNum(HeapPointer) << " - " << hob->what() << endl;
		hob->scavenge();
		hp += hob->bytes();
	}


Nword makeBoxInt(int n) {
	Hob* res = new (num_data_allocation) Value_BoxInt(n);
	if (show_alloc_progress) { cout << "**makeBoxInt: " << n << " -> " << byteNum(res) << endl; }
	return res;
}


//----------------------------------------------------------------------
//INDEX: Tue Dec  5 12:13:02 2006
//----------------------------------------------------------------------
void top_evacuate(Nword& before) {

	if (!before.isPointer()) { return; }
//  	if (!getHob(before)) {
//  		cout << "**evacuate: <null>" << endl;
//  		return;
//  	}

	Hob* hob = getHob(before);
	assert(hob);

	Nword after; // = 0;
	string extra_info = "";

	if (initSpace.inSpace(hob)) {
		extra_info = " (init-heap, so unmoved)"; 
		after = before;
	} else {
		extra_info = " *move*"; 
		after = before->evacuate();
		// would like to avoid repeatedly setting this broken heart
		SetBrokenHeart(getHob(before),getHob_unchecked(after));
	}

	before = after;
}

Hob* getPointer_unchecked(Nword w) {
	return reinterpret_cast<Hob*>(*(reinterpret_cast<unsigned*>(&w)) & ~3);
}

	//SetBrokenHeart(before,getPointer_unchecked(after)); // ought to avoid repeatedly setting this broken heart


//----------------------------------------------------------------------
//INDEX: Wed Dec  6 16:41:49 2006
//----------------------------------------------------------------------

//----------------------------------------------------------------------
//INDEX: low-level tagging (scheme 1)
//----------------------------------------------------------------------

//  bool isTaggedPointer(unsigned raw) { return   raw & 1;  } // pointer is tagged,  LSB is 1
//  bool isTaggedUnboxed(unsigned raw) { return !(raw & 1); } // unboxed is shifted, LSM is 0
//  unsigned TagPointer(Hob* pointer) {
//  	assert(!(reinterpret_cast<unsigned>(pointer) & 3)); //two LSM bits are zero; property of pointer
//  	return reinterpret_cast<unsigned>(pointer) | 1; //tag
//  }
//  unsigned TagUnboxed(unsigned unboxed) {
//  	assert(!(unboxed & (1U<<31))); //MSB bit is not set
//  	return unboxed << 1; //shift
//  }
//  Hob* UnTagPointer(unsigned raw) { return reinterpret_cast<Hob*>(raw & ~3); }
//  unsigned UnTagUnboxed(unsigned raw) { return raw >> 1; }

//----------------------------------------------------------------------
//INDEX: low-level tagging (scheme 2)
//----------------------------------------------------------------------

bool isTaggedPointer(unsigned raw) { return !(raw & 1U); } // pointer is untouched, LSB is 0
bool isTaggedUnboxed(unsigned raw) { return  (raw & 1U); } // unboxed is shifted & tagged, LSB is 1
unsigned TagPointer(Hob* pointer) {
	assert(!(reinterpret_cast<unsigned>(pointer) & 3)); //two LSM bits are zero; property of pointer
	return reinterpret_cast<unsigned>(pointer); //unchanged
}
unsigned TagUnboxed(unsigned unboxed) {
	assert(!(unboxed & (1U<<31))); //MSB bit is zero
	return (unboxed << 1) | 1U; // shift & tag
}
Hob* UnTagPointer(unsigned raw) { return reinterpret_cast<Hob*>(raw); }
unsigned UnTagUnboxed(unsigned raw) { return raw >> 1; }

//----------------------------------------------------------------------
//INDEX: Nword
//----------------------------------------------------------------------

Nword::Nword() : _raw(TagUnboxed(0)) {}
Nword::Nword(Hob* hob) : _raw(TagPointer(hob)) {}
Nword::Nword(unsigned unboxed) : _raw(TagUnboxed(unboxed)) {}
bool isPointer(Nword w) { return isTaggedPointer(w._raw); }
bool isUnboxed(Nword w) { return isTaggedUnboxed(w._raw); }
Hob* getPointer(Nword w) {
	assert(isPointer(w));
	Hob* hob = UnTagPointer(w._raw); 
	assert(hob);
	assert(initSpace.inSpace(hob) || currentSpace.inSpace(hob));
	return hob;
}
unsigned getUnboxed(Nword w) {
	assert(isUnboxed(w));
	return UnTagUnboxed(w._raw);
}


//----------------------------------------------------------------------
//INDEX: Wed Dec  6 23:48:05 2006
//----------------------------------------------------------------------

Closure* getClosure(Nword w) {
//  	if (!isPointer(w)) {
//  		return makeCon0_closure(getUnsigned(w));
//  	}
//  	if (Value_Con0* x = dynamic_cast<Value_Con0*>(getPointer(w))) {
//  		return makeCon0_closure(x->tag);
//  	}
//  	if (Value_ExCon0* x = dynamic_cast<Value_ExCon0*>(getPointer(w))) {
//  		return makeExCon0_closure(x->name, x->tag);
//  	}
	return getClosure_raw(w);
}

//----------------------------------------------------------------------
//INDEX: Thu Dec  7 11:42:06 2006
//----------------------------------------------------------------------

// getClosure may allocate - and so if called twice wil get two objects - need to fix this!
	if (closure != getClosure(func)) {
		cout << "closure has moved : " << func << endl;
		assert(0);
	}
