(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  17.    value manip
  46.    mutable tuples
  67.    frame
  76.    stack - need to know max stack size to use arrays; use assoc for now
  85.    env
  107.   control
  146.   value embedded closure
  169.   execute
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

  open Prel Assoc Atom Value Machine

(*----------------------------------------------------------------------
INDEX: value manip
----------------------------------------------------------------------*)

  val NewException : (vid -> value) =
	  fn vid => value'exval (exval'con0 (gen'exname vid))

  val DeCon : (value -> value) =
	  fn v1 => case (con'value "DeCon" v1) of (_,None) => abort "Decon:None" | (_,Some v2) => v2

  val DeExcon : (value -> value) =
	  fn v1 => case (excon'value "DeCon" v1) of (_,None) => abort "Decon:None" | (_,Some v2) => v2

  val DeRef : (value -> value) =
	  fn v => derefAddr'value "DeRef" v

  val MatchScon : (scon * value -> bool) = 
	  fn (scon,v) => eq'scon scon (scon'value "MatchScon" v)

  val MatchC : (vid * value -> bool) = 
	  fn (vid,v) => eq'vid vid (let val (vid,_) = con'value "MatchC" v in vid end)

  val MatchE : (exname * value -> bool) = 
	  fn (exname,v) => eq'exname exname (let val (exname,_) = excon'value "MatchE" v in exname end)

  val MatchG : (value * value -> bool) = 
	  fn (v1,v2) => eq'exname (let val (exname,_) = excon'value "MatchG/1" v1 in exname end)
							  (let val (exname,_) = excon'value "MatchG/2" v2 in exname end)

(*----------------------------------------------------------------------
INDEX: mutable tuples
----------------------------------------------------------------------*)

  exception V_MutTuple of value array

  fun NewTuple (N:int) = 
	  value'exn (V_MutTuple (Array.array(N,value'null)))

  val SetTupleElement : (value * int * value -> unit) =
	  fn (tup,n,v) =>
		 case (exn'value tup) 
		  of Some (V_MutTuple A) => Array.update(A,n,v)
		   | _ => abort "SetTupleElement"

  val DeTuple : (value * int -> value) =
	  fn (tup,n) =>
		 case (exn'value tup) 
		  of Some (V_MutTuple A) => Array.sub(A,n)
		   | _ => abort "DeTuple"

(*----------------------------------------------------------------------
INDEX: frame
----------------------------------------------------------------------*)

  datatype frame = FRAME of value array
  fun NewFrame (N:int) = FRAME (Array.array(N,value'null))
  fun SetFrame (FRAME A,n,v) = Array.update(A,n,v)
  fun GetFrame (FRAME A,n) = Array.sub(A,n)

(*----------------------------------------------------------------------
INDEX: stack - need to know max stack size to use arrays; use assoc for now
----------------------------------------------------------------------*)

  datatype stack = STACK of (int,value) assoc ref
  fun EmptyStack () = STACK (ref assoc'empty)
  fun GetStack (STACK A,n) = look'assoc "GetStack" stringOfInt (!A) n
  fun SetStack (STACK A,n,v) = (A := extend'assoc (!A) (n,v))

(*-------------------------------------------------------------
INDEX: env
----------------------------------------------------------------------*)

  datatype env = ENV of stack * frame * {arg:value}

  fun NewEnv (frame,{arg}) = ENV (EmptyStack(),frame,{arg=arg})

  val GetLoc : (env * location -> value) =
	  fn (ENV (stack,frame,{arg}),r) =>
		 case r 
          of LOC'Stack (SLOC(n,_))  => GetStack (stack,n)
           | LOC'Frame (FLOC(n,_))  => GetFrame (frame,n)
           | LOC'FuncArg            => arg
           | LOC'Cret               => arg
           | LOC'Xret               => arg
           | LOC'Value v            => v
           | LOC'Null               => abort "GetLoc/null"

  val SetStackLoc : (env * stack_location * value -> unit)=
      fn (ENV(stack,_,_),SLOC (n,_),v) => SetStack(stack,n,v)

(*----------------------------------------------------------------------
INDEX: control
----------------------------------------------------------------------*)

  datatype cont	= CONTINUE of cont * code * frame | FINISH
  datatype control = CONTROL of {C: cont ref, H: (cont * code * frame) list ref}
		  
  val PushContinuation : (control * int * code -> unit) =
	  fn (CONTROL{C,H},N,code) => C := CONTINUE (!C,code,NewFrame N)

  val PushHandler : (control * int * code -> unit) =
	  fn (CONTROL{C,H},N,code) => H := (!C,code,NewFrame N) :: !H

  val PopHandler : (control -> unit) =
	  fn CONTROL{C,H} => H := (case !H of [] => abort "PopHandler" | _::handlers => handlers)

  val SetContFrameElement : (control * int * value -> unit) =
	  fn (CONTROL{C,H},n,v) =>
		 case (!C) of FINISH => abort "SetContFrameElement"
					| CONTINUE(_,_,frame) => SetFrame (frame,n,v)

  val SetXcontFrameElement : (control * int * value -> unit) =
	  fn (CONTROL{C,H},n,v) =>
		 case (!H) of [] => abort "SetXcontFrameElement"
					| (_,_,frame)::_ => SetFrame (frame,n,v)

  type res	= (value,exval) either
  type jump = (code * env -> res)
					
  val Return : (jump * control * value -> res) =
	  fn (Jump,CONTROL{C,H},v) =>
		 case (!C) of FINISH => This v
					| CONTINUE (cont,code,frame) => (C:=cont; Jump (code, NewEnv (frame,{arg=v})))
												 
  val Raise : (jump * control * value -> res) =
	  fn (Jump,CONTROL{C,H},v) =>
		 case (!H) of [] => That (exval'value "Raise" v)
					| (cont,code,frame)::handlers => (H:=handlers; C:=cont; Jump (code, NewEnv (frame,{arg=v})))

(*----------------------------------------------------------------------
INDEX: value embedded closure
----------------------------------------------------------------------*)

  exception V_Closure of code * frame
						 
  fun NewFn (N,code) = 
	  value'exn (V_Closure (code,NewFrame N))

  val SetFrameElement : (value * int * value -> unit) =
	  fn (func,n,v) => 
		 case (exn'value func) 
		  of Some (V_Closure (_,frame)) => SetFrame (frame,n,v)
		   | _ => abort "SetFrameElement"

  val TailCall : (jump * control * value * value -> res) =
	  fn (Jump,control,func,arg) => 
		 case (exn'value func) 
		  of Some (V_Closure (code,frame)) => Jump (code, NewEnv (frame,{arg=arg}))
		   | _ => let val v = apply'value func arg (* interpreted functions, builtins, :=/ref/con0/excon0 application *)
				  in Return (Jump,control,v)
				  end (* handle Pack => Raise !? *)

(*----------------------------------------------------------------------
INDEX: execute
----------------------------------------------------------------------*)

  type step = (unit -> res)
  type nf	= {next: step, fail: step}

  val executeInstruction : (control * env * instruction * nf -> res) =
	  fn (control,env,instruction,{next,fail}) =>
		 case instruction
          of INS'NewTuple (q,{tuple_size})             => (SetStackLoc (env,q,NewTuple tuple_size)          ; next())
           | INS'NewFn (q,{frame_size},code)           => (SetStackLoc (env,q,NewFn (frame_size,code))      ; next())
           | INS'NewException (q,vid)                  => (SetStackLoc (env,q,NewException vid)             ; next())
           | INS'Copy (q,r)                            => (SetStackLoc (env,q,GetLoc (env,r))               ; next())
           | INS'DeCon (q,r)                           => (SetStackLoc (env,q,DeCon (GetLoc (env,r)))       ; next())
           | INS'DeExcon(q,r)                          => (SetStackLoc (env,q,DeExcon (GetLoc (env,r)))     ; next())
           | INS'DeRef (q,r)                           => (SetStackLoc (env,q,DeRef (GetLoc (env,r)))       ; next())
           | INS'DeTuple (q,r,n)                       => (SetStackLoc (env,q,DeTuple (GetLoc (env,r),n))   ; next())
           | INS'PushContinuation ({frame_size},code)  => (PushContinuation (control,frame_size,code)       ; next())
           | INS'PushHandle ({frame_size},code)        => (PushHandler (control,frame_size,code)            ; next())
           | INS'PopHandle                             => (PopHandler control                               ; next())
           | INS'SetTupleElement (r1,n,r2)             => (SetTupleElement(GetLoc(env,r1),n,GetLoc(env,r2)) ; next())
           | INS'SetFrameElement (r1,n,r2)             => (SetFrameElement(GetLoc(env,r1),n,GetLoc(env,r2)) ; next())
           | INS'SetContFrameElem (n,r2)               => (SetContFrameElement(control,n,GetLoc(env,r2))    ; next())
           | INS'SetXcontFrameElem (n,r2)              => (SetXcontFrameElement(control,n,GetLoc(env,r2))   ; next())
           | INS'MatchScon (r,scon)                    => if (MatchScon (scon,            GetLoc (env,r)))  then next() else fail()
           | INS'MatchC (r,vid)                        => if (MatchC    (vid,             GetLoc (env,r)))  then next() else fail()
           | INS'MatchE (r,exname)                     => if (MatchE    (exname,          GetLoc (env,r)))  then next() else fail()
           | INS'MatchG (r1,r2)                        => if (MatchG    (GetLoc (env,r1), GetLoc (env,r2))) then next() else fail()


  val rec executeSeq : (jump * control * env * seq * nf -> res) =
	  fn (J,control,env,seq,{next,fail}) =>
		 case seq
          of SEQ'Empty                  => next()
           | SEQ'Seq (seq1,seq2)        => executeSeq (J,control,env,seq1, 
													   {next = fn()=> executeSeq (J,control,env,seq2,{next=next,fail=fail}),
														fail = fail})
           | SEQ'FatBar (seq1,seq2)     => executeSeq (J,control,env,seq1, 
													   {next = next,
														fail = fn()=> executeSeq (J,control,env,seq2,{next=next,fail=fail})})
           | SEQ'Raise r                => Raise (J,control,GetLoc (env,r))
           | SEQ'I instruction          => executeInstruction (control,env,instruction,{next=next,fail=fail})
              

  val rec executeCode : (jump * control * env * code * {fail:step} -> res) =
	  fn (J,control,env,code,{fail}) =>
		 case code
          of CODE'Seq (seq,code)        => executeSeq (J,control,env,seq,
													   {next = fn()=> executeCode (J,control,env,code,{fail=fail}),
														fail = fail})
           | CODE'FatBar (code1,code2)  => executeCode (J,control,env,code1,
														{fail = fn()=> executeCode (J,control,env,code2, {fail=fail})})
           | CODE'Return r              => Return (J,control,GetLoc(env,r))
           | CODE'TailCall (func,arg)   => TailCall (J,control,GetLoc(env,func),GetLoc(env,arg))
           | CODE'Raise r               => Raise (J,control,GetLoc(env,r))
                            
							
  fun evalCode code =
	  let val control = CONTROL {C = ref FINISH, H = ref []}
		  val fail = (fn () => abort "evalCode:fail")
		  fun Jump (code,env) = executeCode (Jump,control,env,code,{fail=fail})
		  val env = NewEnv (NewFrame 0,{arg=value'null})
	  in Jump (code,env)
	  end
