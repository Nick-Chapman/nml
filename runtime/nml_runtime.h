/*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  114.   args
  142.   g_call_*
  151.   g_call_* - should be selected by compiler
  179.   builtin
 CONTENTS-END-LINE:
 ------------------------------------------------------------------------------*/

#define xint long

class SiCont;
class SiClosure;
class Hob;

class Nword {
private:
    //unsigned _raw;
    xint _raw;
    Nword(xint raw) : _raw(raw) {}
public:
    Nword(Hob* hob); // implicit conversion from Hob* pointer to Nword
    Nword() {}; // default constructor - distinct from pointer
public:
    friend bool isPointer(Nword);
    friend Hob* getPointer(Nword);
public:
    static Nword fromRawUnboxed(xint); // unboxed values, distinct from pointer
    static Nword fromInt(int);
    static Nword fromUnsigned(unsigned);
    static Nword fromChar(char);
public:
    static xint getRawUnboxed(Nword); //careful
    static int getInt(Nword);
    static unsigned getUnsigned(Nword);
    static char getChar(Nword);
};

class Ncode;
typedef Ncode (*NcodeFP) (void);
struct Ncode {
    NcodeFP _fp;
    explicit Ncode(NcodeFP fp) : _fp(fp) {}
};

extern Nword TheProgram;
Nword Init ();

extern Nword CRET;
extern Nword XRET;

Nword ARG(unsigned);
Nword FRAME(unsigned);

bool g_matchNum             (Nword,int);
bool g_matchChar            (Nword,char);
bool g_matchString          (Nword,char*);
bool g_matchC0              (Nword,unsigned);
bool g_matchC1              (Nword,unsigned);
bool g_matchE               (Nword,unsigned);
bool g_matchG               (Nword,Nword);

Nword g_DeCon               (Nword);
Nword g_DeExcon             (Nword);
Nword g_DeRef               (Nword);
Nword g_DeTuple             (Nword,unsigned);

Nword g_mkNum               (int);
Nword g_mkWord              (unsigned);
Nword g_mkChar              (char);
Nword g_mkString            (char*);
Nword g_mkExname            (char*,unsigned);
Nword g_mkExname            (char*,unsigned,Nword); // yuck, overloaded

Nword g_Copy                (Nword);
Nword g_MakeCon              (unsigned tag,Nword);
Nword g_MakeRef              (Nword);
Nword g_con0                (unsigned tag,unsigned arity);
Nword g_unit                ();
Nword g_stdOut              ();
Nword g_MakeException        (char*);
Nword g_EmptyRef            ();

void g_FixupRef             (Nword, Nword);
void g_SetTupleElement      (Nword,unsigned,Nword);
void g_SetFrameElement      (Nword,unsigned,Nword); //for closures ("fn")
void g_SetContFrameElem     (unsigned,Nword);
void g_SetXcontFrameElem    (unsigned,Nword);

Ncode g_returnWith          (Nword);
Ncode g_raise               (Nword);

Nword g_MakePap             (Nword func, unsigned num_early_args, unsigned num_remaining_args);

SiCont* g_MakeSiCont        (char* name, Ncode code, unsigned frame_size);
SiCont* g_MakeSiHandle      (char* name, Ncode code, unsigned frame_size); //identical except for allocation stats attribution
SiClosure* g_MakeSiFn       (char* name, Ncode code, unsigned frame_size, unsigned num_args);

Nword g_MakeTuple           (unsigned n);
Nword g_MakeFn              (SiClosure* si);
void g_PushContinuation     (SiCont* si);
void g_PushHandler          (SiCont* si);

#define m_MakeSiCont(frame_size,code)           (g_MakeSiCont(#code,Ncode(code),frame_size))
#define m_MakeSiHandle(frame_size,code)         (g_MakeSiHandle(#code,Ncode(code),frame_size))
#define m_MakeSiFn(frame_size,num_args,code)    (g_MakeSiFn(#code,Ncode(code),frame_size,num_args))

typedef Nword               (*NwordOp1) (Nword);
typedef Nword               (*NwordOp2) (Nword,Nword);
typedef Nword               (*NwordOp3) (Nword,Nword,Nword);

Nword g_CloseBuiltin_1      (char*,NwordOp1);
Nword g_CloseBuiltin_2      (char*,NwordOp2);
Nword g_CloseBuiltin_3      (char*,NwordOp3);

#define m_CloseBuiltin_1(op) (g_CloseBuiltin_1(#op,op))
#define m_CloseBuiltin_2(op) (g_CloseBuiltin_2(#op,op))
#define m_CloseBuiltin_3(op) (g_CloseBuiltin_3(#op,op))

//----------------------------------------------------------------------
//INDEX: args
//----------------------------------------------------------------------

template<unsigned n> Nword& GetArg();

extern Nword TheArg0;
extern Nword TheArg1;
extern Nword TheArg2;
extern Nword TheArg3;
extern Nword TheArg4;
extern Nword TheArg5;
extern Nword TheArg6;
extern Nword TheArg7;

template<> inline Nword& GetArg<0>() { return TheArg0; }
template<> inline Nword& GetArg<1>() { return TheArg1; }
template<> inline Nword& GetArg<2>() { return TheArg2; }
template<> inline Nword& GetArg<3>() { return TheArg3; }
template<> inline Nword& GetArg<4>() { return TheArg4; }
template<> inline Nword& GetArg<5>() { return TheArg5; }
template<> inline Nword& GetArg<6>() { return TheArg6; }
template<> inline Nword& GetArg<7>() { return TheArg7; }

#define ARG(n) (GetArg<n>())

#define SetArg(n,v) ( GetArg<n>() = (v) )

//----------------------------------------------------------------------
//INDEX: g_call_*
//----------------------------------------------------------------------

Ncode callFunc(unsigned num_actual_args, Nword func);

//#define g_call(num_actual_args,func) (callFunc_N<num_actual_args>(func)) //would be nice
#define g_call(num_actual_args,func) (callFunc(num_actual_args,func))

//----------------------------------------------------------------------
//INDEX: g_call_* - should be selected by compiler
//----------------------------------------------------------------------

// Assumes args have been moved to stack to avoid over-write probs...

#define g_call_1(func,x0)               ( SetArg(0,x0),                                                         g_call(1,func) )
#define g_call_2(func,x0,x1)            ( SetArg(0,x0), SetArg(1,x1),                                           g_call(2,func) )
#define g_call_3(func,x0,x1,x2)         ( SetArg(0,x0), SetArg(1,x1), SetArg(2,x2),                             g_call(3,func) )
#define g_call_4(func,x0,x1,x2,x3)      ( SetArg(0,x0), SetArg(1,x1), SetArg(2,x2), SetArg(3,x3),               g_call(4,func) )
#define g_call_5(func,x0,x1,x2,x3,x4)   ( SetArg(0,x0), SetArg(1,x1), SetArg(2,x2), SetArg(3,x3), SetArg(4,x4), g_call(5,func) )


#define g_call_6(func,x0,x1,x2,x3,x4,x5) ( \
    SetArg(0,x0), SetArg(1,x1), SetArg(2,x2), SetArg(3,x3), SetArg(4,x4), SetArg(5,x5), \
    g_call(6,func) \
)

#define g_call_7(func,x0,x1,x2,x3,x4,x5,x6) ( \
    SetArg(0,x0), SetArg(1,x1), SetArg(2,x2), SetArg(3,x3), SetArg(4,x4), SetArg(5,x5), SetArg(6,x6), \
    g_call(7,func) \
)

#define g_call_8(func,x0,x1,x2,x3,x4,x5,x6,x7) ( \
    SetArg(0,x0), SetArg(1,x1), SetArg(2,x2), SetArg(3,x3), SetArg(4,x4), SetArg(5,x5), SetArg(6,x6), SetArg(7,x7), \
    g_call(8,func) \
)

//----------------------------------------------------------------------
//INDEX: builtin
//----------------------------------------------------------------------

Nword builtin_ColonEqual            (Nword,Nword);

Nword builtin_Tilda                 (Nword);
Nword builtin_Dash                  (Nword,Nword);
Nword builtin_Plus                  (Nword,Nword);
Nword builtin_Star                  (Nword,Nword);
Nword builtin_div                   (Nword,Nword);
Nword builtin_mod                   (Nword,Nword);
Nword builtin_Less                  (Nword,Nword);
Nword builtin_LessEqual             (Nword,Nword);
Nword builtin_Greater               (Nword,Nword);
Nword builtin_GreaterEqual          (Nword,Nword);
Nword builtin_Hat                   (Nword,Nword);
Nword builtin_size                  (Nword);
Nword builtin_chr                   (Nword);
Nword builtin_ord                   (Nword);
Nword builtin_implode               (Nword);
Nword builtin_explode               (Nword);
Nword builtin_Equal                 (Nword,Nword);
Nword builtin_print                 (Nword);

Nword builtin_Vector_sub            (Nword,Nword);
Nword builtin_Vector_fromList       (Nword);

Nword builtin_Array_array           (Nword,Nword);
Nword builtin_Array_sub             (Nword,Nword);
Nword builtin_Array_length          (Nword);
Nword builtin_Array_update          (Nword,Nword,Nword);

Nword builtin_Word_Plus             (Nword,Nword);
Nword builtin_Word_Dash             (Nword,Nword);
Nword builtin_Word_mod              (Nword,Nword);
Nword builtin_Word_GreaterGreater   (Nword,Nword);
Nword builtin_Word_LessLess         (Nword,Nword);
Nword builtin_Word_orb              (Nword,Nword);
Nword builtin_Word_andb             (Nword,Nword);
Nword builtin_Word_notb             (Nword);
Nword builtin_Word_toInt            (Nword);
Nword builtin_Word_fromInt          (Nword);
Nword builtin_Word_toString         (Nword);

Nword builtin_TextIO_output         (Nword,Nword);
Nword builtin_TextIO_flushOut       (Nword);
Nword builtin_TextIO_closeOut       (Nword);
Nword builtin_TextIO_openIn         (Nword);
Nword builtin_TextIO_openOut        (Nword);
Nword builtin_TextIO_closeIn        (Nword);
Nword builtin_TextIO_inputN         (Nword,Nword);

Nword builtin_String_sub            (Nword,Nword);

Nword builtin_Char_Less             (Nword,Nword);
Nword builtin_Char_LessEqual        (Nword,Nword);
Nword builtin_Char_Greater          (Nword,Nword);
Nword builtin_Char_GreaterEqual     (Nword,Nword);
Nword builtin_Char_toString         (Nword);
