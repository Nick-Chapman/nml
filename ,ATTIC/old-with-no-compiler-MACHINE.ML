(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  16.	 machine -- registers
  22.	 machine -- code
  62.	 machine -- components
  86.	 machine -- eval
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

open Prel Atom Value
nonfix o

fun notYet s = abort s

(*----------------------------------------------------------------------
INDEX: machine -- registers
----------------------------------------------------------------------*)
							  
datatype reg = REG of int

(*----------------------------------------------------------------------
INDEX: machine -- code
----------------------------------------------------------------------*)

datatype ipush = (* produces a value, given just temps *)
	IPUSH'GenExname		of vid
  | IPUSH'Const			of value
  | IPUSH'Scon			of scon
  | IPUSH'Ref			of reg
  | IPUSH'Con0			of vid
  | IPUSH'Con1			of vid * reg
  | IPUSH'Exname0		of reg
  | IPUSH'Exname1		of reg * reg
  | IPUSH'Record		of reg list (* not-fixed length instruction *)
  | IPUSH'SelectRecord	of reg * int
  | IPUSH'DeRef			of reg
  | IPUSH'DeCon			of reg (* con1/ex1 *)
  | IPUSH'Assign		of reg * reg
  | IPUSH'Eq			of reg * reg

datatype imatch = 
	IMATCH'MatchScon	 of scon
  | IMATCH'MatchCon		 of vid (* con0/con1 *)
  | IMATCH'MatchExname	 of reg (* ex0/ex1 *)

datatype code =
	CODE'Return			of reg
  | CODE'Call			of reg * reg
  | CODE'Raise			of reg
  | CODE'Instr			of instr * code
and instr =
	INSTR'Push			of ipush
  | INSTR'Match			of reg * imatch
  | INSTR'OnFail		of code
  | INSTR'SelectFrame	of int
  | INSTR'PushClosure	of code * reg list
  | INSTR'PushCont		of code * reg list
  | INSTR'PushHandler	of code * reg list
  | INSTR'PopHandler

(*----------------------------------------------------------------------
INDEX: machine -- components
----------------------------------------------------------------------*)

datatype frame = FRAME of value list
datatype dump = DUMP of (code * frame * dump) option
datatype handler = HANDLER of (code * frame * dump * handler) option
datatype onfail = ONFAIL of code option
abstype temps = TEMPS of value list * int (* length-of-list*)
with
   val temps'empty = TEMPS ([],0)
   fun pushValue v (TEMPS (vs,n)) = TEMPS (v::vs,n+1)
   fun getReg (TEMPS (vs,n)) (REG i) = (* index reverse-list from zero *)
	   if (i>=n) then abort "select'temps" else elem (n-i-1) vs
end
	
datatype machine = MACHINE of (code * frame * dump * handler * onfail * temps)
(* MACHINE(c,f,d,h,o,t) *)

fun popHandler (HANDLER (Some (_,_,_,h))) = h
  | popHandler (HANDLER None) = abort "popHandler"

fun code'onfail (ONFAIL None) = abort "code'onfail:None"
  | code'onfail (ONFAIL (Some c)) = c

fun selectFrame n (FRAME vs) = elem n vs

fun getRegs t regs = FRAME (map (getReg t) regs)

(*----------------------------------------------------------------------
INDEX: machine -- eval
----------------------------------------------------------------------*)

fun meval'ipush ipush t =
	case ipush of
		IPUSH'GenExname vid => value'exval (exval'con0 (gen'exname vid))
	  | IPUSH'Const v => v
	  | IPUSH'Ref r => value'addr (ref (getReg t r))

fun meval'imatch imatch t v =
	case imatch of
		IMATCH'MatchScon scon => let val scon' = scon'value "IMATCH'MatchScon" v
								 in (eq'scon scon scon')
								 end
	  | IMATCH'MatchCon vid => let val vid' = ctag'value "IMATCH'MatchCon" v
							   in (eq'vid vid vid')
							   end
	  | IMATCH'MatchExname r =>
		let val exname = xtag'value "IMATCH'MatchExname(1)" (getReg t r)
			val exname' = xtag'value "IMATCH'MatchExname(2)" v
		in (eq'exname exname exname')
		end

fun meval'return M d h v =
	case d of
		DUMP None => This v
	  | DUMP (Some (c,f,d)) => let val o = ONFAIL None
								   val t = pushValue v temps'empty
							   in M (MACHINE(c,f,d,h,o,t))
							   end

fun meval'call M (vFun,vArg) =
	notYet "meval'call"

fun meval'raise M h v =
	case h of
		HANDLER None => That (exval'value "meval'raise" v)
	  | HANDLER (Some (c,f,d,h)) => let val o = ONFAIL None
										val t = pushValue v temps'empty
									in M (MACHINE (c,f,d,h,o,t))
									end
										
fun makeMachineClosure M (code,frame) =
	closure'fn (fn vArg =>
				   let val d = DUMP None
					   val h = HANDLER None
					   val o  = ONFAIL None
					   val t = pushValue vArg temps'empty
				   in case (M (MACHINE (code,frame,d,h,o,t))) of
						  This value => value
						| That exval => notYet "????"
				   end)
							   
fun meval'machine_U M (MACHINE (currentCode,f,d,h,o,t)) =
	case currentCode of
		CODE'Return r => meval'return M d h (getReg t r)
	  | CODE'Call (rFun,rArg) => meval'call M (getReg t rFun, getReg t rArg)
	  | CODE'Raise r => meval'raise M h (getReg t r)
	  | CODE'Instr (instr,nextCode) =>
		case instr of
			INSTR'Push ipush =>
			let val v = meval'ipush ipush t
				val t = pushValue v t
			in M (MACHINE (nextCode,f,d,h,o,t))
			end
		  | INSTR'Match (r,imatch) =>
			let val v = getReg t r
				val branchcode = if (meval'imatch imatch t v)
								 then nextCode
								 else code'onfail o
			in M (MACHINE (branchcode,f,d,h,o,t))
			end
		  | INSTR'OnFail code =>
			let val o = ONFAIL (Some code)
			in M (MACHINE (nextCode,f,d,h,o,t))
			end
		  | INSTR'SelectFrame n =>
			let val v = selectFrame n f
				val t = pushValue v t
			in M (MACHINE (nextCode,f,d,h,o,t))
			end
		  | INSTR'PushClosure (code,regs) =>
			let val v = value'closure (makeMachineClosure M (code, getRegs t regs))
				val t = pushValue v t
			in M (MACHINE (nextCode,f,d,h,o,t))
			end
		  | INSTR'PushCont (code,regs) =>
			let val d = DUMP (Some (code,getRegs t regs,d))
			in M (MACHINE (nextCode,f,d,h,o,t))
			end
		  | INSTR'PushHandler (code,regs) =>
			let val h = HANDLER (Some (code,getRegs t regs,d,h))
			in M (MACHINE (nextCode,f,d,h,o,t))
			end
		  | INSTR'PopHandler =>
			let val h = popHandler h
			in M (MACHINE (nextCode,f,d,h,o,t))
			end

fun meval'machine m = meval'machine_U meval'machine m
