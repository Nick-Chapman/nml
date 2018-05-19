
//----------------------------------------------------------------------
//INDEX: TheFramePointer
//----------------------------------------------------------------------

const unsigned MaxFrameSize = 17;

Nword TheFrameWords[MaxFrameSize];

//unsigned maxframesize = 0;

class FramePointer {
private:
	unsigned _n;
	//Nword* _words;
public:
	//FramePointer(unsigned n, Nword* words) : _n(n), _words(words) {}
	//FramePointer(unsigned n, Nword* words) : _n(n), _words(new Nword[n]) {
	FramePointer(unsigned n, Nword* words) : _n(n) {
		//if (n>maxframesize) { maxframesize = n; }
		assert(n <= MaxFrameSize);
		//cout << "**FramePoint: copying #words - " << n << " -> " << _words << endl;
		for (unsigned i = 0; i<n; ++i) {
			//_words[i] = words[i];
			TheFrameWords[i] = words[i];
		}
	}
	Nword& framePointerElem(unsigned i) {
		assert(i<_n);
		//return _words[i];
		return TheFrameWords[i];
	}
	unsigned frameSize() {
		return _n;
	}
};

FramePointer TheFramePointer(0,0);

Nword FRAME(unsigned n) {
	return TheFramePointer.framePointerElem(n);
}

//----------------------------------------------------------------------
//INDEX: Stack allocated Continuation
//----------------------------------------------------------------------


Counter NumPushes("P");
Level DepthC("D");
Level StackDepth("S");


//const unsigned MaxStackSize = 1300;
const unsigned MaxStackSize = 10000;

//char* TheStack = new char[MaxStackSize];
char TheStack[MaxStackSize];

char*const TheStackTop = &TheStack[MaxStackSize];
char* TheStackPointer = TheStackTop;


class Continuation {
public:
	SiCont* _si;
	Continuation* _cont;
private:
	Nword _words[];
public:
	Continuation(SiCont* si, Continuation* cont) :
		_si(si),
		_cont(cont) {
	}
	static void* operator new(size_t base_size, Counter& counter, unsigned extra_words) {
		const unsigned size = base_size + extra_words*sizeof(Nword);
		NumPushes.inc(1);
		DepthC.inc(1);
		StackDepth.inc(size);
		//return my_alloc(counter,size);
		//assert(TheStackPointer<=TheStackTop);
		//cout << "TheStackPointer(alloc):" << (void*)TheStackPointer <<"->" << (void*)(TheStackPointer-size) << endl;
		TheStackPointer -= size;
		unsigned currentStackSize = TheStackTop-TheStackPointer;
		assert(currentStackSize == StackDepth.count);
		assert(TheStackPointer>=TheStack); //stack-overflow - possible, means we need a bigger stack.
		for (unsigned i=0; i<size; ++i) { TheStackPointer[i] = 0; }
		return TheStackPointer;
	}
	void SetTheFrame() {
		TheFramePointer = FramePointer(_si->frame_size, _words);
	}
	Nword& frameElem(unsigned i) {
		assert(i<_si->frame_size);
		return _words[i];
	}
};

Continuation* makeContinuation(Counter& counter, SiCont* si, Continuation* cont) {
	return new (counter,si->frame_size) Continuation(si,cont);
}

//----------------------------------------------------------------------
//INDEX: TheCurrentContinuation
//----------------------------------------------------------------------

Continuation* TheCurrentContinuation = 0;

void SetContFrameElem(unsigned n,Nword v) {
	TheCurrentContinuation->frameElem(n) = v;
}

void pushContinuation(Counter& counter, SiCont* si) {
	TheCurrentContinuation = makeContinuation(counter,si,TheCurrentContinuation);
}

Nword CRET;

Ncode ReturnWith(Nword res) {
	assert (res);
	CRET = res;
	if (!TheCurrentContinuation) { return Ncode(0); }
	Continuation* C = TheCurrentContinuation;
	if (showProgress) { cout << "**Return: " << C->_si->name << endl; }
	TheCurrentContinuation = C->_cont;
	unsigned size = sizeof(Continuation) + C->_si->frame_size*sizeof(Nword);
	DepthC.dec(1);
	StackDepth.dec(size);
	C->SetTheFrame();

	char* name = C->_si->name;
	Ncode code = C->_si->code;

	//cout << "TheStackPointer(free):" << (void*)TheStackPointer <<"->" << (void*)(TheStackPointer+size) << endl;
	//for (unsigned i=0; i<size; ++i) { TheStackPointer[i] = 0; }
	TheStackPointer += size;

	//return Jump(C->_si->name, C->_si->code);
	return Jump(name,code);
}

//----------------------------------------------------------------------
//INDEX: Handler
//----------------------------------------------------------------------

Ncode PopHandler (); //forward

SiCont SiPopHandler = SiCont("<pop-handler>",Ncode(PopHandler),0);

class Handler : public Continuation {
public:
	SiCont* _si; // for handler
	unsigned _handlerDepth;
	unsigned _handlerStackDepth;
	Handler* _handler; // prev handler
private:
	Nword _words[]; // for handler code (shadow Continuation member)
public:
	Handler(SiCont* si, Handler* handler, Continuation* cont) :
		Continuation(&SiPopHandler,cont),
		_si(si),
		_handlerDepth(DepthC.count),
		_handlerStackDepth(StackDepth.count),
		_handler(handler) {
	}
	void SetTheFrame() {
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

//----------------------------------------------------------------------
//INDEX: TheCurrentHandler
//----------------------------------------------------------------------

Handler* TheCurrentHandler = 0;

ostream& operator<< (ostream& os, const Handler* h) {
	if (!h) { return os << "H-null"; }
	return os << "H-" << (void*)h << "=" << h->_si->name << "," << h->_si->frame_size;
}

void pushHandler(Counter& counter, SiCont* si) {
	//cout << "**pushHandler: " << si->name << ", " << si->frame_size << ", " << TheCurrentHandler << endl;
  	Handler* H = makeHandler(counter,si,TheCurrentHandler,TheCurrentContinuation);
	//cout << "**pushHandler: " << TheCurrentHandler << "->" << H << endl;
	TheCurrentHandler = H;
	TheCurrentContinuation = H;
}

void SetXcontFrameElem(unsigned n,Nword v) {
	TheCurrentHandler->frameElem(n) = v;
}

Ncode PopHandler () {
  	//cout << "**PopHandler: " << TheCurrentHandler << endl;
	//cout << "**PopHandler: " << TheCurrentHandler << "->" << TheCurrentHandler->_handler << endl;
	unsigned size = sizeof(Handler) - sizeof(Continuation) + TheCurrentHandler->_si->frame_size*sizeof(Nword);
	StackDepth.dec(size);
	TheStackPointer += size;
	TheCurrentHandler = TheCurrentHandler->_handler;
	return ReturnWith(CRET);
//  	if (!TheCurrentContinuation) { return Ncode(0); }
//  	Continuation* C = TheCurrentContinuation;
//  	if (showProgress) { cout << "**Return: " << C->_si->name << endl; }
//  	TheCurrentContinuation = C->_cont;
//  	DepthC.dec(1);
//  	unsigned size2 = sizeof(Continuation) + C->_si->frame_size*sizeof(Nword);
//  	StackDepth.dec(size2);
//  	C->SetTheFrame();
//  	return Jump(C->_si->name, C->_si->code);
}

Nword XRET;

Ncode RaiseWith(Nword res) {
	assert (res);
	XRET = res;
	if (!TheCurrentHandler) {
		cout << "** uncaught exception at top level" << endl;
		return Ncode(0);
	}
	Handler* H = TheCurrentHandler;
	if (showProgress) { cout << "**Raise: " << H->_si->name << endl; }
	unsigned num_conts = (1 + DepthC.count - H->_handlerDepth);
	DepthC.dec(num_conts);

	unsigned size1 = sizeof(Handler) + H->_si->frame_size*sizeof(Nword);
	unsigned size2 = (StackDepth.count - H->_handlerStackDepth);
	unsigned size = size1 + size2;
	StackDepth.dec(size);

	//cout << "**Raise: #conts=" << num_conts << ", size=" << size << endl;

	TheStackPointer += size;

	TheCurrentContinuation = H->_cont;
	TheCurrentHandler = H->_handler;
	H->SetTheFrame();
	return Jump(H->_si->name, H->_si->code);
}
