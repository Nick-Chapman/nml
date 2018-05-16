
(*----------------------------------------------------------------------
INDEX: uni abstraction
----------------------------------------------------------------------*)
  datatype 'a uni = Uni of sub -> (sub * 'a) err
  fun deUni (Uni f) = f

  fun resultUni a = Uni (fn sub => Okay (sub,a))

  fun bindUni (Uni f, M) = 
	  Uni (fn sub => 
			  case (f sub) 
			   of Err s => Err s
				| Okay (sub,a) => deUni (M a) sub)
	  
  fun execUni (Uni f) = case (f empty'sub) of Err s => Err s | Okay (_,a) => Okay a

  fun failUni s = Uni (fn _ => Err s)

  fun subUni sub1 =
	  Uni (fn sub => Okay (compose'sub (sub1,sub), ()))

  fun withSubUni f =
	  Uni (fn sub => deUni (f sub) sub)

  infix >> >>> -$$ $$- **

  val op >> = bindUni
  fun u >>> f = u >> (fn x => resultUni (f x))
  fun (u1 -$$ u2) = u1 >> (fn () => u2)
  fun (u1 $$- u2) = u1 >> (fn x => u2 -$$ resultUni x)

  fun (u1 ** u2) = u1 >> (fn x1 => u2 >>> (fn x2 => (x1,x2)))
  (*fun joinUni (u1,u2) = bindUni (u1, fn () => bindUni (u2, fn () => resultUni ()))*)

  val joinListUni = fold (curry op -$$) (resultUni ())

  fun linkUni [] = resultUni []
	| linkUni (u::us) = u >> (fn x => linkUni us >> (fn xs => resultUni (x::xs)))


(*----------------------------------------------------------------------
INDEX: unification of types
----------------------------------------------------------------------*)

  fun applySubUni tor = withSubUni (fn sub => resultUni (apply'sub sub tor))

  fun unifyTT (tor1,tor2) =
	  applySubUni tor1 >> (fn tor1 =>
	  applySubUni tor2 >> (fn tor2 =>
		  case (tor1,tor2)
		   of (TOR'Tv tv1,TOR'Tv tv2)   => if (tv1=tv2) then resultUni () else subUni (unit'sub (tv1,TOR'Tv tv2))
			| (TOR'Tv tv,tor)           => if (occurs tv tor) then failUni "v/occ" else subUni (unit'sub (tv,tor))
			| (tor,TOR'Tv tv)           => if (occurs tv tor) then failUni "occ/v" else subUni (unit'sub (tv,tor))
			| (TOR'Bool,TOR'Bool)		=> resultUni ()
			| (TOR'Arrow (a1,b1),TOR'Arrow (a2,b2)) => unifyTT (a1,a2) -$$ unifyTT (b1,b2)
			| (TOR'Row row1, TOR'Row row2) => (case (zip'rows (row1,row2))
												of Err s => failUni ("row:"^s)
												 | Okay zipped => joinListUni (map (fn (_,tt) => unifyTT tt) zipped))
			(* unification failures *)
			| (TOR'Bool,TOR'Arrow _)    => failUni "bool/arrow"
			| (TOR'Bool,TOR'Row _)      => failUni "bool/row"
			| (TOR'Arrow _,TOR'Bool)    => failUni "arrow/bool"
			| (TOR'Arrow _,TOR'Row _)   => failUni "arrow/row"
			| (TOR'Row _,TOR'Bool)      => failUni "row/bool"
			| (TOR'Row _,TOR'Arrow _)   => failUni "row/arrow"
			))

(*----------------------------------------------------------------------
INDEX: tc schemes
----------------------------------------------------------------------*)

  type E = (context * exp -> tor uni)

(*----------------------------------------------------------------------
INDEX: match
----------------------------------------------------------------------*)
  fun U_tc'match (E:E) (context,match) =
	  case match
	   of MATCH (pos,[MRULE (pat,exp)]) => let val (tor1,context) = tc'pat (context,pat)
										   in E (context,exp) >>> (fn tor2 => TOR'Arrow (tor1,tor2))
										   end
		| _ => notYet "constraints'match"

(*----------------------------------------------------------------------
INDEX: exprow
----------------------------------------------------------------------*)
  fun U_tc'exprow (E:E) context (EXPROW xs) = 
	  linkUni (map (fn (lab,exp) => E (context,exp) >>> (fn tor => (lab,tor))) xs)
	  >>> TOR'Row

(*----------------------------------------------------------------------
INDEX: exp
----------------------------------------------------------------------*)
  fun U_tc'exp (E:E) : E = fn (context,exp) =>
	  case exp of
          EXP'Scon scon             => notYet "scon"
        | EXP'Var (LONG([],x),pos)	=> resultUni (TOR'Tv (app'context context x))
        | EXP'Var (x,pos)  			=> notYet "long-var"
        | EXP'Record exprow         => U_tc'exprow E context exprow
        | EXP'Select (pos,lab)      => notYet "select"
        | EXP'Unit                  => notYet "unit"
        | EXP'Tuple exps            => U_tc'exprow E context (exprow'tuple exps)
        | EXP'List exps             => notYet "list"
        | EXP'Vector exps           => notYet "vector"
        | EXP'Seq exps              => notYet "seq"
        | EXP'Let (dec,exp)         => notYet "let"
        | EXP'App (func,arg)        => let val tor = TOR'Tv (genTv "@")
									   in  E (context,func) >> (fn tor1 =>
										   E (context,arg) >> (fn tor2 =>
										   unifyTT (tor1, TOR'Arrow (tor2,tor)) -$$ resultUni tor))
									   end
        | EXP'Typed (exp,ty)        => notYet "typed"
        | EXP'Andalso (e1,e2)       => notYet "andalso"
        | EXP'Orelse (e1,e2)        => notYet "orelse"
        | EXP'Handle (exp,match)    => notYet "handle"
        | EXP'Raise exp             => notYet "raise"
        | EXP'If (e1,e2,e3)         => E (context,e1) >> (fn tor1 =>
									   unifyTT (tor1,TOR'Bool) -$$
									   E (context,e2) >> (fn tor2 =>
									   E (context,e3) >> (fn tor3 =>
									   unifyTT (tor2,tor3) -$$
									   resultUni tor2)))
        | EXP'While (e1,e2)         => notYet "while"
        | EXP'Case (exp,match)      => notYet "case"
        | EXP'Fn match              => U_tc'match E (context,match)

  local fun E x  = U_tc'exp E x
  in val tc'exp = E
  end

(*
  fun new_tc'exp context exp =
	  execUni ((tc'exp (context,exp) ** withSubUni resultUni)
			   >>> (fn (tor,sub) => (apply'sub sub tor)))


  fun new_tc'exp context exp =
	  execUni (tc'exp (context,exp) >> applySubUni)	  
*)

(*----------------------------------------------------------------------
INDEX: runTc
----------------------------------------------------------------------*)
  fun runTc exp =
	  let val context = empty'context
	  in case (execUni (tc'exp (context,exp) >> applySubUni))
		  of Err s => abort ("TcErr:"^s)
		   | Okay tor => 
			 let (*val () = see (Word "raw =" @@ raw_lay'tor tor)*)
				 val () = see (Word "(old)result =" @@ lay'tor tor)
			 in ()
			 end
	  end
