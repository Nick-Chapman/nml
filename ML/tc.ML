(*------------------------------------------------------------------------------
  CONTENTS-START-LINE: HERE=2 SEP=1
   122.   source language generation - fvalbind (dup EMBED)
   163.   err -  like option, but with a string
   168.   zip'rows
   183.   optTupleRow
   194.   lang - general fold ops
   283.   ut - unknown types / unifiable type vars
   293.   tname - type names (what datatype generate)
   303.   tor - computed types
   402.   scon
   413.   sub
   430.   unification abstraction
   468.   unification of types
   496.   tscheme
   532.   idstatus
   537.   valenv
   565.   typefun
   573.   tystr
   582.   tyenv
   599.   strenv, env
   635.   tvscoped
   644.   context
   672.   bool,list - fromcontext
   688.   tyrow, ty
   711.   tc schemes
   719.   patrow
   734.   pat
   790.   mrule
   801.   match
   812.   exprow
   819.   exp
   872.   valbind
   895.   datbind
   929.   exbind
   944.   dec
   972.   fix: T,P, E/D
   983.   basis
   1034.  sigexp, spec
   1088.  strexp, strdec
   1123.  topdec(s)
   1140.  runTc
   1158.  predefined context
  CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

  open Prel Assoc Misclay Pos Atom Lang
  infix ++ @@ // //``


  fun lay'vid   (VID   (ID s)) = Word s
  fun lay'strid (STRID (ID s)) = Word s
  fun lay'tycon (TYCON (ID s)) = Word s
  fun lay'funid (FUNID (ID s)) = Word s
  fun lay'sigid (SIGID (ID s)) = Word s


  fun lay'exp exp = Pretty.lay'exp_noIM exp

  fun sof'tyvar (TYVAR (ID s)) = "'" ^ s

  fun sof'rigid x = "#" ^ sof'tyvar x

  fun eq'tyvar (x1:tyvar) x2 = (x1=x2)

  fun revList'assoc a = rev (list'assoc a)

  fun mapV'assoc F a = map'assoc (fn (k,v) => (k,F v)) a

  infix -- &&
  fun op -- (xs,ys) = filter (fn x => not (exists (curry op = x) ys)) xs
  fun op && (xs,ys) = xs @ (ys -- xs)

	  
  fun parseThen PROG READER =
	  let (*val U as () = echo ("***parsing...")*)
		  val (pos,{eof},tdso) = READER()
		  val U = if eof then () else abort ("***parser did not reach eof, pos="^sofPos pos)
	  in case tdso of
			 None => abort ("***parse-error, pos="^sofPos pos)
		   | Some tds => PROG tds
	  end
		  
  fun notYet s = abort ("notYet: tc: "^s)

  fun lay'id (ID x) = Word (case x of "*" => "* " | _ => x)
  fun lay'lab lab =
	  case lab of
		  LAB'Id id => lay'id id
		| LAB'Num n => layInt n

  (* dup from EMBED.ML *)
  fun leq'id (ID x1, ID x2) = (x1 <= x2)
  val leq'lab =
	  fn (LAB'Num x1, LAB'Num x2) => (x1<=x2)
	   | (LAB'Num _, LAB'Id _) => true
	   | (LAB'Id _, LAB'Num _) => false
	   | (LAB'Id x1, LAB'Id x2) => leq'id (x1,x2)
  fun leq'keyed leq ((k1,v1),(k2,v2)) = leq (k1,k2)

  fun tyrow'tuple tys = TYROW (map (fn (ty,n) => (LAB'Num n, ty)) (enumerateFrom 1 tys))
  fun exprow'tuple exps = EXPROW (map (fn (exp,n) => (LAB'Num n, exp)) (enumerateFrom 1 exps))
  fun patrow'tuple pats = PATROW (map (fn (pat,n) => (LAB'Num n, pat)) (enumerateFrom 1 pats), {wildcard=None})

  (* dup from EMBED.ML *)
  fun vid'pat pat =
	  case pat of
		  PAT'Typed (pat,_)  => vid'pat pat
		| PAT'Var (LONG([],vid)) => vid
		| PAT'Var (LONG(_::_,vid)) => abort "vid'pat/long"
		| _ => abort "vid'pat"
  val match'exp =
	  fn EXP'Fn match => match
	   | _ => abort "match'exo"
  fun xfs'valbind vb =
	  case vb of		
		  VALBIND'Rec vb => xfs'valbind vb (* 2nd inner rec ignored *)
		| VALBIND'Seq (pat,exp,None) => [(vid'pat pat,match'exp exp)]
		| VALBIND'Seq (pat,exp,Some vb) => (vid'pat pat,match'exp exp) :: xfs'valbind vb

(*----------------------------------------------------------------------
INDEX: source language generation - fvalbind (dup EMBED)
----------------------------------------------------------------------*)

  fun exp'fnVid pos x body = EXP'Fn (MATCH (pos,[MRULE (PAT'Var (LONG ([],x)),body)]))

  fun exp'fnVids pos [] body = body
	| exp'fnVids pos (x::xs) body = exp'fnVid pos x (exp'fnVids pos xs body)

  fun mrule'clause (pats,tyo_IGNORED,exp) = MRULE (PAT'Tuple pats,exp)


  fun exp'fnPat pos pat body = EXP'Fn (MATCH (pos,[MRULE (pat,body)]))
  fun exp'fnPats pos [] body = body
	| exp'fnPats pos (x::xs) body = exp'fnPat pos x (exp'fnPats pos xs body)
									
  fun exp'clauses prefix pos [] = abort "exp'clauses:[]"
(*de-optimization!!
	(* special case for 1 clause *)
	| exp'clauses prefix pos [(pats,_,exp)] = exp'fnPats pos pats exp
*)
	(* general cases for 2 or more clauses*)
	| exp'clauses prefix pos (clauses as (pats,_,_)::_) =
	  let (*val vids = genVids prefix (length pats)*)
		  val vids = map (fn n => VID (ID (prefix^"-"^stringOfInt n))) (upto (1,length pats))
	  in exp'fnVids pos vids
		 (EXP'Case (EXP'Tuple (map (exp'vid pos) vids),
					MATCH (pos,map mrule'clause clauses)))
	  end

  fun valbind'pes [] = abort "valbind'pes[]"
	| valbind'pes [(p,e)] = VALBIND'Seq (p,e,None)
	| valbind'pes ((p,e)::pes) = VALBIND'Seq (p,e,Some(valbind'pes pes))

  fun valbind'fvalbind (FVALBIND fs) =
	  VALBIND'Rec 
	  (valbind'pes
	   (map (fn (pos,vid,clauses) => let val exp = exp'clauses (sof'vid vid) pos clauses
									 in (PAT'Var (LONG ([],vid)), exp)
									 end) fs))

(*----------------------------------------------------------------------
INDEX: err -  like option, but with a string
----------------------------------------------------------------------*)
  datatype 'a err = Okay of 'a | Err of string

(*----------------------------------------------------------------------
INDEX: zip'rows
----------------------------------------------------------------------*)
  val zip'rows : (lab*'a) list * (lab*'b) list -> (lab*('a*'b)) list err
	= fn (row1,row2) =>
		 let fun loop acc [] [] = Okay (rev acc)
			   | loop acc ((lab,x)::row1) ((lab2,y)::row2) = if (eq'lab lab lab2) then loop ((lab,(x,y))::acc) row1 row2 
															 else Err (sof'lab lab ^ "/" ^ sof'lab lab2)
			   | loop _ [] (_::_) = Err "[]/::"
			   | loop _ (_::_) [] = Err "::/[]"
			 val row1' = Sort.sort (leq'keyed leq'lab) row1
			 val row2' = Sort.sort (leq'keyed leq'lab) row2
		 in loop [] row1' row2'
		 end

(*----------------------------------------------------------------------
INDEX: optTupleRow
----------------------------------------------------------------------*)
  val optTupleRow : (lab*'a) list -> 'a list option
	= fn row =>
		 let fun loop acc [] = Some (rev acc)
			   | loop acc (((lab,x),n)::row) = if (eq'lab lab (LAB'Num n)) then loop (x::acc) row
											   else None
		 in loop [] (enumerateFrom 1 (Sort.sort (leq'keyed leq'lab) row))
		 end

(*----------------------------------------------------------------------
INDEX: lang - general fold ops
----------------------------------------------------------------------*)

  fun fold'ty {V,T} acc ty =
	  case ty
       of TY'Var x                  => V acc x
        | TY'Record (TYROW xs)      => fold (fn acc => fn (lab,exp) => T acc exp) acc xs
        | TY'Con (tys,ltc)          => fold T acc tys
        | TY'Tuple tys              => fold T acc tys
        | TY'Func (ty1,ty2)         => T (T acc ty1) ty2

  fun fold'pat {T,P} acc pat =
      case pat 
       of PAT'Wild                      => acc
        | PAT'Scon scon                 => acc
        | PAT'Var x                     => acc
        | PAT'Record (PATROW (xs,_))    => fold (fn acc => fn (lab,pat) => P acc pat) acc xs
        | PAT'Unit                      => acc
        | PAT'Tuple pats                => fold P acc pats
        | PAT'List pats                 => fold P acc pats
        | PAT'Cons (x,pat)              => P acc pat
        | PAT'Typed (pat,ty)            => T (P acc pat) ty
        | PAT'Layered (vid,None,pat)    => P acc pat
        | PAT'Layered (vid,Some ty,pat) => P (T acc ty) pat

  fun fold'match P E acc =
        fn MATCH (pos,mrules) => fold (fn acc => fn MRULE (pat,exp) => E (P acc pat) exp) acc mrules

  fun fold'exp {T,P,E,D} acc exp =
      case exp 
	   of EXP'Scon _				=> acc
        | EXP'Var (x,pos)           => acc
        | EXP'Record (EXPROW xs)    => fold (fn acc => fn (lab,exp) => E acc exp) acc xs
        | EXP'Select (pos,lab)      => acc
        | EXP'Unit                  => acc
        | EXP'Tuple exps            => fold E acc exps
        | EXP'List exps             => fold E acc exps
        | EXP'Vector exps           => fold E acc exps
        | EXP'Seq exps              => fold E acc exps
        | EXP'Let (dec,exp)         => E (D acc dec) exp
        | EXP'App (func,arg)        => E (E acc func) arg
        | EXP'Typed (exp,ty)        => T (E acc exp) ty
        | EXP'Andalso (e1,e2)       => E (E acc e1) e2
        | EXP'Orelse (e1,e2)        => E (E acc e1) e2
        | EXP'Handle (exp,match)    => fold'match P E (E acc exp) match
        | EXP'Raise exp             => E acc exp
        | EXP'If (e1,e2,e3)         => E (E (E acc e1) e2) e3
        | EXP'While (e1,e2)         => E (E acc e1) e2
        | EXP'Case (exp,match)      => fold'match P E (E acc exp) match
        | EXP'Fn match              => fold'match P E acc match

  fun fold'valbind P E acc vb =
      case vb
	   of VALBIND'Seq (pat,exp,None)    => E (P acc pat) exp
        | VALBIND'Seq (pat,exp,Some vb) => fold'valbind P E (E (P acc pat) exp) vb
        | VALBIND'Rec vb                => fold'valbind P E acc vb

  fun fold'exbind1 T acc b =
	  case b 
       of EXBIND1'New (vid,Some ty) => T acc ty
        | EXBIND1'New (vid,None)    => acc
        | EXBIND1'Copy (vid,longY)  => acc

  fun fold'dec {T,D} acc dec =
      case dec
       of DEC'Val (tyvars,vb)       => acc
        | DEC'Fval fb               => acc
        | DEC'Type typbind          => acc
        | DEC'Datatype (db,tbo)     => acc
        | DEC'DataReplicate (a,b)	=> acc
        | DEC'Abstype (db,tbo,dec)  => D acc dec
        | DEC'Exception (EXBIND bs) => fold (fold'exbind1 T) acc bs
        | DEC'Local (dec1,dec2)     => D (D acc dec1) dec2
        | DEC'Open xs               => acc
        | DEC'Empty                 => acc
        | DEC'Seq (dec1,dec2)       => D (D acc dec1) dec2

	  local
          fun V acc x   = if exists (eq'tyvar x) acc then acc else x :: acc
          fun T acc ty  = fold'ty {T = T, V = V} acc ty
          fun P acc pat = fold'pat {T=T,P=P} acc pat
          fun D acc dec = fold'dec {T=T,D=D} acc dec
          fun E acc exp = fold'exp {T=T,P=P,E=E,D=D} acc exp
      in
	  val unguarded'vb = fold'valbind P E []
	  val tyvars'ty = T []
      end

(*----------------------------------------------------------------------
INDEX: ut - unknown types / unifiable type vars
----------------------------------------------------------------------*)
  datatype ut = Ut of int
  fun sof'ut (Ut n) = "#" ^ stringOfInt n
  fun eq'ut (Ut x1) (Ut x2) = (x1=x2)
  local val U = ref 1
  in fun genUt() = let val n = !U in U := 1+n; Ut(n) end
  end

(*----------------------------------------------------------------------
INDEX: tname - type names (what datatype generate)
----------------------------------------------------------------------*)
  datatype tname = TNAME of int * tycon
  fun sof'tname (TNAME (n,tycon)) = sof'tycon tycon
  fun eq'tname (TNAME (x1,_)) (TNAME (x2,_)) = (x1=x2)
  local val U = ref 1
  in fun genTname tycon = let val n = !U in U := 1+n; TNAME(n,tycon) end
  end

(*----------------------------------------------------------------------
INDEX: tor - computed types
----------------------------------------------------------------------*)

  datatype tor
	= TOR'Ut	of ut
	| TOR'Rigid of tyvar (* ascribed tyvar *)
	| TOR'Arrow of tor * tor
	| TOR'Row	of (lab * tor) list
	| TOR'Cons  of tname * tor list
				  
  val tor'ut    = TOR'Ut
  val tor'rigid = TOR'Rigid
  val tor'arrow = TOR'Arrow

  (*val tor'row   = fn [_] => abort "tor'row" | xs => TOR'Row xs*)
  val tor'row   = TOR'Row

  val tor'unit  = TOR'Row []
  val tor'cons  = TOR'Cons


  fun genTor name = TOR'Cons (genTname (TYCON (ID name)),[])

  val tor'int       = genTor "int"
  val tor'char      = genTor "char"
  val tor'string    = genTor "string"
  (*val tor'bool      = genTor "bool"*)
  val tor'exn       = genTor "exn"


  val brief_sof'tor =
      fn TOR'Ut _       => "var"
       | TOR'Rigid x    => ("rigid:"^sof'rigid x)
       | TOR'Arrow _    => "arrow"
       | TOR'Row xs     => "row"
       | TOR'Cons (x,_) => ("cons:"^sof'tname x)


  fun mapUtRigid'tor UT RIGID =
      let val rec TOR = fn TOR'Ut ut            => UT ut
                         | TOR'Rigid tyvar      => RIGID tyvar
                         | TOR'Arrow (tor1,tor2)=> TOR'Arrow (TOR tor1, TOR tor2)
                         | TOR'Row xs           => TOR'Row (map (fn (lab,tor) => (lab,TOR tor)) xs)
                         | TOR'Cons (tname,tors)=> TOR'Cons (tname,map TOR tors)
      in TOR
	  end

  fun mapUt'tor UT = mapUtRigid'tor UT TOR'Rigid

  fun optMapUt'tor O_UT = (* chance for optimization here - dont build new structure when None *)
	  mapUt'tor (fn ut => case (O_UT ut) of Some tor => tor | None => tor'ut ut)

  fun mapRigid'tor RIGID = mapUtRigid'tor tor'ut RIGID

  fun optMapRigid'tor O_RIGID = (* chance for optimization here - dont build new structure when None *)
	  mapRigid'tor (fn rigid => case (O_RIGID rigid) of Some tor => tor | None => TOR'Rigid rigid)

  fun existsUt'tor UT =
      let val rec TOR = fn TOR'Ut ut            => UT ut
                         | TOR'Rigid _          => false
                         | TOR'Arrow (tor1,tor2)=> (TOR tor1) orelse (TOR tor2)
                         | TOR'Row xs           => exists (fn (_,tor) => TOR tor) xs
                         | TOR'Cons (_,tors)    => exists TOR tors
      in TOR
	  end

  fun lay'tor_UT UT tor =
	  let fun Bracket0 b x = if (b>0) then layBracket x else x
		  fun Bracket1 b x = if (b>1) then layBracket x else x
		  fun TOR b =
              fn TOR'Ut ut              => UT ut
               | TOR'Rigid tyvar        => Word (sof'rigid tyvar)
               | TOR'Arrow (tor1,tor2)  => Bracket0 b (TOR 1 tor1 ++ Word "->" ++ TOR 0 tor2)
               | TOR'Row xs             => (case (optTupleRow xs)
											 of Some [] => Word "unit"
											  | Some tors => Bracket1 b (layListSep (Word "*") (map (TOR 2) tors))
											  | None => layBrace (layListSep
																  (Right ",")
																  (map (fn (lab,tor) => lay'lab lab ++ Right ":" ++ TOR 0 tor)
																	   xs)))
               | TOR'Cons (tname,tors)  => (case tors of [] => Blank
													   | [tor] => TOR 2 tor
													   | _ => layTuple (map (TOR 0) tors))
										   ++ Word (sof'tname tname)
      in TOR 0 tor
	  end

  local fun collect acc tor =
			case tor
             of TOR'Ut ut               => if exists (eq'ut ut) acc then acc else ut :: acc
              | TOR'Rigid _             => acc
              | TOR'Arrow (tor1,tor2)   => collect (collect acc tor1) tor2
              | TOR'Row xs              => fold (fn acc => fn (lab,tor) => collect acc tor) acc xs
			  | TOR'Cons (_,tors)		=> fold collect acc tors
  in fun utsOf'tor tor = rev (collect [] tor)
	 fun utsOf'tors tors = rev (fold collect [] tors)
  end

(*----------------------------------------------------------------------
INDEX: scon
----------------------------------------------------------------------*)
  fun tor'scon scon =
	  case scon of
		  SCON'Num n	=> tor'int
		| SCON'Word w	=> notYet "scon/word"
		| SCON'Real r	=> notYet "scon/real"
		| SCON'Char c	=> tor'char
		| SCON'String s => tor'string

(*----------------------------------------------------------------------
INDEX: sub
----------------------------------------------------------------------*)
  datatype sub = Sub of ut -> tor
  fun deSub (Sub f) = f
  val empty'sub = Sub (fn v => tor'ut v)
  fun unit'sub (v,tor) = Sub (fn v2 => if (v=v2) then tor else tor'ut v2)
  fun apply'sub (Sub UT) = mapUt'tor UT
  fun compose'sub (sub1,sub2) = Sub (fn v => apply'sub sub1 (deSub sub2 v))

  (*fun sub'listPairs pairs = fold (curry compose'sub) empty'sub (map unit'sub pairs) - NO - must be simultaneous *)
  fun sub'listPairs pairs =
	  fold (fn sub => 
			   fn (v,tor) => 
				  Sub (fn v2 => if (v=v2) then tor else deSub sub v2)
				  ) empty'sub pairs

(*----------------------------------------------------------------------
INDEX: unification abstraction
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

  fun makeSubUni sub1 =
	  Uni (fn sub => Okay (compose'sub (sub1,sub), ()))

  val theSubUni = Uni (fn sub => Okay (sub,sub))

  infix >> >>> -$$ $$- **

  val op >> = bindUni
  fun u >>> f = u >> (fn x => resultUni (f x))
  fun (u1 -$$ u2) = u1 >> (fn () => u2)
  fun (u1 $$- u2) = u1 >> (fn x => u2 -$$ resultUni x)
  fun (u1 ** u2) = u1 >> (fn x1 => u2 >>> (fn x2 => (x1,x2)))

  val joinListUni = fold (curry op -$$) (resultUni ())

  fun linkUni [] = resultUni []
	| linkUni (u::us) = u >> (fn x => linkUni us >> (fn xs => resultUni (x::xs)))

  val err2uni = fn Okay x => resultUni x | Err s => failUni s

(*----------------------------------------------------------------------
INDEX: unification of types
----------------------------------------------------------------------*)

  val refineTor = apply'sub

  fun refineTorUni tor = theSubUni >>> (fn sub => refineTor sub tor)

  fun occurs v = existsUt'tor (fn v' =>v=v')

  fun unifyTT (tor1,tor2) =
	  refineTorUni tor1 >> (fn tor1 =>
	  refineTorUni tor2 >> (fn tor2 =>
		  case (tor1,tor2)
		   of (TOR'Ut ut1,TOR'Ut ut2)	=> if (ut1=ut2) then resultUni () else makeSubUni (unit'sub (ut1,tor'ut ut2))
			| (TOR'Ut ut,tor)			=> if (occurs ut tor) then failUni "v/occ" else makeSubUni (unit'sub (ut,tor))
			| (tor,TOR'Ut ut)			=> if (occurs ut tor) then failUni "occ/v" else makeSubUni (unit'sub (ut,tor))
			| (TOR'Cons (n1,ts1),TOR'Cons (n2,ts2)) => if (eq'tname n1 n2) 
													   then joinListUni (map unifyTT (zipPair ts1 ts2))
													   else failUni ("tname:"^sof'tname n1^"/"^sof'tname n2)
			| (TOR'Arrow (a1,b1),TOR'Arrow (a2,b2)) => unifyTT (a1,a2) -$$ unifyTT (b1,b2)
			| (TOR'Row row1, TOR'Row row2) => (case (zip'rows (row1,row2))
												of Err s => failUni ("row:"^s)
												 | Okay zipped => joinListUni (map (fn (_,tt) => unifyTT tt) zipped))
			| (TOR'Rigid s1, TOR'Rigid s2)=> if (s1=s2) then resultUni () else failUni ("rigid:"^sof'rigid s1^"/"^sof'rigid s2)
			| _ => 
			  failUni (brief_sof'tor tor1 ^ "/" ^ brief_sof'tor tor2)))

(*----------------------------------------------------------------------
INDEX: tscheme
----------------------------------------------------------------------*)
  datatype tscheme = Tscheme of ut list * tor
  fun tscheme'cons uts tor = Tscheme (uts,tor)

  fun sof'UtInt n = "'" ^
					(if n<26 then implode [chr (ord #"a" + n)]
					 else "ut_" ^ stringOfInt n)

  fun lay'tscheme (Tscheme (uts,tor)) =
	  let val assoc = assoc'list (enumerateFrom 0 uts)
		  fun UT ut = Word (case (lookup'assoc assoc ut) of Some n => sof'UtInt n | None => sof'ut ut)
	  in (*laySqBracket (layCommaSep (map UT uts)) ++ *) lay'tor_UT UT tor
	  end

  fun tor'tscheme (Tscheme (_::_,tor)) = abort "tor'tscheme/::"
    | tor'tscheme (Tscheme ([],tor)) = tor

  fun utsOf'tscheme (Tscheme (uts,tor)) = utsOf'tor tor -- uts

  fun tscheme'close sub scoped contextUts tor =
      let
		  val tor = refineTor sub tor
		  val a = assoc'list (map (fn rigid => (rigid, tor'ut (genUt()))) scoped)
		  val tor = optMapRigid'tor (fn rigid => find'assoc a (eq'tyvar rigid)) tor
          (*val uts = utsOf'tor tor -- contextUts*)
          val uts = utsOf'tor tor -- utsOf'tors (map (refineTor sub o TOR'Ut) contextUts)
      in Tscheme (uts,tor)
      end

  fun instance'tscheme (Tscheme (uts,tor)) =
	  let val a = assoc'list (map (fn ut => (ut, tor'ut (genUt()))) uts)
	  in optMapUt'tor (fn ut => find'assoc a (eq'ut ut)) tor
      end

(*----------------------------------------------------------------------
INDEX: idstatus
----------------------------------------------------------------------*)
  datatype ids = IDS'C | IDS'V | IDS'E

(*----------------------------------------------------------------------
INDEX: valenv
----------------------------------------------------------------------*)
  datatype valenv = VALENV of (vid,tscheme * ids) assoc
  val valenv'empty = VALENV assoc'empty
  fun valenv'plus (VALENV a1) (VALENV a2) = VALENV (assoc'plus a1 a2)
  val valenv'plusList = foldR valenv'plus valenv'empty
  fun valenv'single kv = VALENV (assoc'single kv)

  fun lookup'vid (VALENV a) k = lookup'assoc a k
  fun look'vid (VALENV a) k = look'assoc "tc/look'vid" sof'vid a k


  fun utsOf'valenv (VALENV a) = (* dups ok *)
      linkMap (fn (_,(tscheme,_)) => utsOf'tscheme tscheme) (list'assoc a)

  fun valenv'close sub scoped contextUts (VALENV a) = 
      VALENV (mapV'assoc (fn (tscheme,ids) => (tscheme'close sub scoped contextUts (tor'tscheme tscheme),ids)) a)

  fun lay'valenv (VALENV assoc) =
	  layVert
	  (map (fn (vid,(tscheme,IGNORED_ids)) =>
			   case (*ids*) Basis.IDSTATUS'V
				of Basis.IDSTATUS'V => Word "val" ++ lay'vid vid ++ Word ":" ++ lay'tscheme tscheme
				 | Basis.IDSTATUS'C => Word "con" ++ lay'vid vid
				 | Basis.IDSTATUS'E => Word "excon" ++ lay'vid vid
			   ) (revList'assoc assoc))

(*----------------------------------------------------------------------
INDEX: typefun
----------------------------------------------------------------------*)
  datatype typefun = TYPEFUN of tor list -> tor err
  val typefun'fn = TYPEFUN
  fun de'typefun (TYPEFUN f) =f
  val apply'typefun = de'typefun

(*----------------------------------------------------------------------
INDEX: tystr
----------------------------------------------------------------------*)
  datatype tystr = TYSTR of typefun * valenv
  val tystr'cons = TYSTR
  fun typefun'tystr (TYSTR (typefun,_)) = typefun

  fun lay'tystr (TYSTR _) = Word "<tystr>"

(*----------------------------------------------------------------------
INDEX: tyenv
----------------------------------------------------------------------*)
  datatype tyenv = TYENV of (tycon,tystr) assoc
  val tyenv'empty = TYENV assoc'empty
  fun tyenv'plus (TYENV a1) (TYENV a2) = TYENV (assoc'plus a1 a2)
  val tyenv'plusList = foldR tyenv'plus tyenv'empty
  fun tyenv'single kv = TYENV (assoc'single kv)
  fun look'tycon (TYENV a) k = look'assoc "tc/look'tycon" sof'tycon a k


  fun lay'tyenv (TYENV assoc) =
	  layVert
	  (map (fn (tycon,tystr) => 
			   Word "type" ++ lay'tycon tycon ++ Word "=" ++ lay'tystr tystr
			   ) (revList'assoc assoc))

(*----------------------------------------------------------------------
INDEX: strenv, env
----------------------------------------------------------------------*)
  datatype strenv = STRENV of (strid,env) assoc
  and env = ENV of strenv * tyenv * valenv

  val strenv'empty = STRENV assoc'empty
  fun strenv'plus (STRENV a1) (STRENV a2) = STRENV (assoc'plus a1 a2)
  val strenv'plusList = foldR strenv'plus strenv'empty
  fun strenv'single kv = STRENV (assoc'single kv)

  fun look'strid (STRENV a) k = look'assoc "tc/look'strid" sof'strid a k

  fun env'strenv se = ENV (se,tyenv'empty,valenv'empty)
  fun env'tyenv te = ENV (strenv'empty,te,valenv'empty)
  fun env'valenv ve = ENV (strenv'empty,tyenv'empty,ve)

  fun strenv'env (ENV (x,_,_)) = x
  fun tyenv'env (ENV (_,x,_)) = x
  fun valenv'env (ENV (_,_,x)) = x

  val env'empty = ENV (strenv'empty,tyenv'empty,valenv'empty)
  fun env'plus (ENV (se1,te1,ve1)) (ENV (se2,te2,ve2)) =
	  let val se = strenv'plus se1 se2
		  val te = tyenv'plus te1 te2
		  val ve = valenv'plus ve1 ve2
	  in ENV (se,te,ve)
	  end

  fun lay'strenv lay'env (STRENV assoc) =
	  layVert
	  (map (fn (strid,env) =>
			   Word "structure" ++ lay'strid strid ++ Word "=" 
					//`` (Word "struct" //`` lay'env env // Word "end")
			   ) (revList'assoc assoc))

  fun lay'env (ENV (s,t,v)) =
	  lay'strenv lay'env s // lay'tyenv t // lay'valenv v

(*----------------------------------------------------------------------
INDEX: tvscoped
----------------------------------------------------------------------*)
  datatype tvscoped = TVSCOPED of tyvar list
  val list'tvscoped = fn TVSCOPED xs => xs
  val tvscoped'list = TVSCOPED
  val tvscoped'empty = TVSCOPED []
  fun tvscoped'plus (TVSCOPED xs1) (TVSCOPED xs2) = TVSCOPED (xs1 @ xs2)

(*----------------------------------------------------------------------
INDEX: context
----------------------------------------------------------------------*)
  datatype context = CONTEXT of env * tvscoped

  fun context'env env = CONTEXT (env,tvscoped'empty)
  fun env'context (CONTEXT (env,_)) = env

  fun context'tvscoped tvscoped = CONTEXT (env'empty,tvscoped)
  fun tvscoped'context (CONTEXT (_,tvscoped)) = tvscoped

  fun context'plus (CONTEXT (env1,tvs1)) (CONTEXT (env2,tvs2)) = 
	  let val env = env'plus env1 env2
		  val tvs = tvscoped'plus tvs1 tvs2
	  in CONTEXT (env,tvs)
	  end

  val strenv'context = strenv'env o env'context
  val tyenv'context = tyenv'env o env'context
  val valenv'context = valenv'env o env'context
  val context'valenv = context'env o env'valenv
  val context'tyenv = context'env o env'tyenv

  val context'empty = context'env env'empty

  fun utsOf'context context = utsOf'valenv (valenv'context context)

  (*val lay'context = lay'env o env'context*)

(*----------------------------------------------------------------------
INDEX: bool,list - fromcontext
----------------------------------------------------------------------*)

  fun tor'bool context =
	  let val tc = TYCON (ID "bool") (* maybe better to find this is special structure *)
		  val tf = typefun'tystr (look'tycon (tyenv'context context) tc)
	  in err2uni (apply'typefun tf [])
	  end

  fun tor'list context =
	  let val tc = TYCON (ID "list") (* maybe better to find this is special structure *)
		  val tf = typefun'tystr (look'tycon (tyenv'context context) tc)
	  in fn t => err2uni (apply'typefun tf [t])
	  end

(*----------------------------------------------------------------------
INDEX: tyrow, ty
----------------------------------------------------------------------*)
  fun tc'tyrow T tyrow =
	  case tyrow of
		  TYROW xs => linkUni (map (fn (lab,ty) => T ty >>> (fn tor => (lab, tor))) xs) >>> tor'row


  fun tc'ty TV (tyenv,ty) =
	  let val rec T = 
			  fn TY'Var tyvar              => err2uni (TV tyvar)
			   | TY'Record tyrow           => tc'tyrow T tyrow
			   | TY'Con (tys,LONG([],tc))  => linkUni (map T tys) >>
													  (fn tors =>
														  case (apply'typefun (typefun'tystr (look'tycon tyenv tc)) tors)
														   of Err s => failUni s
															| Okay tor => resultUni tor)
			   | TY'Con (tys,ltc)          => notYet "ty/long-tc"
			   | TY'Tuple tys              => tc'tyrow T (tyrow'tuple tys)
			   | TY'Func (ty1,ty2)         => (T ty1 ** T ty2) >>> tor'arrow
	  in T ty
	  end

(*----------------------------------------------------------------------
INDEX: tc schemes
----------------------------------------------------------------------*)
  type T = ((tyvar -> tor err) -> tyenv * ty -> tor uni)
  type P = (context * pat -> (tor * valenv) uni)
  type E = (context * exp -> tor uni)
  type D = (context * dec -> env uni)

(*----------------------------------------------------------------------
INDEX: patrow
----------------------------------------------------------------------*)

  fun tc'patrow (P:P) (context,PATROW (xs,{wildcard=Some pos})) = notYet ("tc'patrow/wildcard: "^Pos.sofPos pos)
    | tc'patrow (P:P) (context,PATROW (xs,{wildcard=None})) =
	  linkUni (map (fn (lab,pat) => P (context,pat) >>> (fn (tor,ve) => ((lab,tor),ve))) xs)
	  >>> unzip
	  >>> (fn (ys,ves) => (tor'row ys, valenv'plusList ves))

  fun tc'pats (P:P) (context,pats) =
	  linkUni (map (fn pat => P (context,pat)) pats)
	  >>> unzip
	  >>> (fn (ys,ves) => (ys, valenv'plusList ves))

(*----------------------------------------------------------------------
INDEX: pat
----------------------------------------------------------------------*)

  fun tc'var context x =
	  case (case (lookup'vid (valenv'context context) x)
			 of None => None
			  | Some (_,IDS'V) => None
			  | Some (tscheme,_) => Some tscheme)
	   of None => let val tor = tor'ut (genUt())
					  val valenv = valenv'single (x,(tscheme'cons [] tor,IDS'V))
				  in (tor,valenv)
				  end
		| Some tscheme => let val tor = instance'tscheme tscheme
						  in (tor,valenv'empty)
						  end

  fun tc'cons context x =
	  case (lookup'vid (valenv'context context) x)
	   of None => failUni ("cons/unbound/"^sof'vid x)
		| Some (_,IDS'V) => failUni ("cons/V/"^sof'vid x)
		| Some (tscheme,_) => let val tor = instance'tscheme tscheme
							  in resultUni (tor,valenv'empty)
							  end


  val TV = (Okay o TOR'Rigid)

  fun U_tc'pat {T:T,P:P} : P = fn (context,pat) =>
      case pat of
          PAT'Wild                  => let val tor = tor'ut (genUt())
                                       in resultUni (tor,valenv'empty)
                                       end
        | PAT'Scon scon             => resultUni (tor'scon scon,valenv'empty)
        | PAT'Var (LONG ([],x))     => resultUni (tc'var context x)
        | PAT'Var x                 => notYet "pat/var/long"
        | PAT'Record patrow         => tc'patrow P (context,patrow)
        | PAT'Unit                  => resultUni (tor'unit,valenv'empty)
        | PAT'Tuple pats            => tc'patrow P (context,patrow'tuple pats)
        | PAT'List pats             => let val t1 = tor'ut (genUt())
									   in tc'pats P (context,pats) >> (fn (tors,valenv) =>
										   joinListUni (map (fn t2 => unifyTT (t1,t2)) tors) -$$
										   tor'list context t1 >>> (fn list_t1 => (list_t1, valenv)))
									   end
        | PAT'Cons (LONG ([],x),pat)=> let val tor = tor'ut (genUt())
                                       in  tc'cons context x >> (fn (tor1,valenv1) =>
                                           P (context,pat) >> (fn (tor2,valenv2) =>
                                           unifyTT (tor1, tor'arrow (tor2,tor)) 
										   -$$ resultUni (tor, valenv'plus valenv1 valenv2)))
                                       end
        | PAT'Cons (x,pat)          => notYet "pat/cons/long"
        | PAT'Typed (pat,ty)        => P (context,pat) >> (fn res as (t1,valenv) =>
									   T TV (tyenv'context context, ty) >> (fn t2 =>
									   unifyTT (t1,t2) -$$ resultUni res))
        | PAT'Layered (vid,tyo,pat) => notYet "pat/layered"

(*----------------------------------------------------------------------
INDEX: mrule
----------------------------------------------------------------------*)
  fun tc'mrule (P:P) (E:E) (context,mrule) =
      case mrule
       of MRULE (pat,exp) => 
		  P (context,pat) >> (fn (tor1,valenv1) =>
		  let val context = context'plus context (context'valenv valenv1)
		  in E (context,exp) >>> (fn tor2 => tor'arrow (tor1,tor2))
		  end)

(*----------------------------------------------------------------------
INDEX: match
----------------------------------------------------------------------*)
  fun tc'match (P:P) (E:E) (context,match) =
      case match
       of MATCH (pos,[]) => abort "tc'match/[]"
        | MATCH (pos,mrule1::mrules) => 
		  tc'mrule P E (context,mrule1) >> (fn t1 =>
		  joinListUni (map (fn mrule => tc'mrule P E (context,mrule) >> (fn t2 => unifyTT (t1,t2))) mrules) -$$
		  resultUni t1)

(*----------------------------------------------------------------------
INDEX: exprow
----------------------------------------------------------------------*)
  fun tc'exprow (E:E) (context,EXPROW xs) = 
      linkUni (map (fn (lab,exp) => E (context,exp) >>> (fn tor => (lab,tor))) xs)
      >>> tor'row

(*----------------------------------------------------------------------
INDEX: exp
----------------------------------------------------------------------*)
  fun U_tc'exp {T:T,P:P,E:E,D:D} : E = fn (context,exp) =>
      case exp of
          EXP'Scon scon             => resultUni (tor'scon scon)
        | EXP'Var (LONG([],x),pos)  => let val (tscheme,notYetIds) = look'vid (valenv'context context) x
                                       in resultUni (instance'tscheme tscheme)
                                       end
        | EXP'Var (x,pos)           => notYet "long-var"
        | EXP'Record exprow         => tc'exprow E (context,exprow)
        | EXP'Select (pos,lab)      => notYet "select"
        | EXP'Unit                  => resultUni tor'unit
        | EXP'Tuple exps            => tc'exprow E (context,exprow'tuple exps)
        | EXP'List exps             => let val t1 = tor'ut (genUt())
                                       in joinListUni (map (fn exp => E (context,exp) >> (fn t2 => unifyTT (t1,t2))) exps) -$$
										   tor'list context t1
									   end
        | EXP'Vector exps           => notYet "vector"
        | EXP'Seq exps              => linkUni (map (fn exp => E (context,exp)) exps) >>> rev >>> (fn [] => abort "seq/[]" | t::_ => t)
        | EXP'Let (dec,exp)         => D (context,dec) >> (fn env => 
                                       E (context'plus context (context'env env),exp))
        | EXP'App (func,arg)        => let val tor = tor'ut (genUt())
                                       in  E (context,func) >> (fn tor1 =>
                                           E (context,arg) >> (fn tor2 =>
                                           unifyTT (tor1, tor'arrow (tor2,tor)) -$$ resultUni tor))
                                       end
        | EXP'Typed (exp,ty)        => E (context,exp) >> (fn t1 =>
									   T TV (tyenv'context context,ty) >> (fn t2 =>
									   unifyTT (t1,t2) -$$ resultUni t1))
        | EXP'Andalso (e1,e2)       =>
									   tor'bool context >> (fn tor_bool =>
									   E (context,e1) >> (fn tor1 => unifyTT (tor1,tor_bool) -$$
                                       E (context,e2) >> (fn tor2 => unifyTT (tor2,tor_bool) -$$
									   resultUni tor_bool)))
        | EXP'Orelse (e1,e2)        => 
									   tor'bool context >> (fn tor_bool =>
									   E (context,e1) >> (fn tor1 => unifyTT (tor1,tor_bool) -$$
                                       E (context,e2) >> (fn tor2 => unifyTT (tor2,tor_bool) -$$
									   resultUni tor_bool)))
        | EXP'Handle (exp,match)    => notYet "handle"
        | EXP'Raise exp             => E (context,exp) >> (fn tor => unifyTT (tor, tor'exn))
										 -$$ resultUni (tor'ut (genUt()))
        | EXP'If (e1,e2,e3)         => E (context,e1) >> (fn tor1 =>
									   tor'bool context >> (fn tor_bool => unifyTT (tor1,tor_bool) -$$
                                       E (context,e2) >> (fn tor2 =>
                                       E (context,e3) >> (fn tor3 =>
                                       unifyTT (tor2,tor3) -$$
                                       resultUni tor2))))
        | EXP'While (e1,e2)         => notYet "while"
        | EXP'Case (exp,match)      => E (context, EXP'App (EXP'Fn match, exp))
        | EXP'Fn match              => tc'match P E (context,match)

(*----------------------------------------------------------------------
INDEX: valbind
----------------------------------------------------------------------*)
  fun tc'valbind (P:P) (E:E) (context,vb) =
      case vb of
          VALBIND'Seq (pat,exp,vbo)     => P (context,pat) >> (fn (tor1,valenv1) =>
                                           E (context,exp) >> (fn tor2 => 
										   unifyTT (tor1,tor2) -$$
										   (case vbo of None => resultUni valenv1 
													  | Some vb => 
														tc'valbind P E (context,vb) >>> (fn valenv2 =>
														valenv'plus valenv1 valenv2))))
        | VALBIND'Rec vb =>
		  let val xfs = xfs'valbind vb
			  val pairs = map (fn (x,_) => (x,tor'ut (genUt()))) xfs
			  val valenv  = valenv'plusList (map (fn (x,tor) => valenv'single (x,(tscheme'cons [] tor,IDS'V))) pairs)
			  val context = context'plus context (context'valenv valenv)
		  in 
			  tc'valbind P E (context,vb) >> (fn valenv =>
			  joinListUni (map (fn (x,tor) => unifyTT (tor, case (look'vid valenv x) of (tscheme,_) => tor'tscheme tscheme)) pairs)
			  -$$ resultUni valenv)
		  end

(*----------------------------------------------------------------------
INDEX: datbind
----------------------------------------------------------------------*)

  fun makeTypefun tname N =
	  typefun'fn (fn tors =>
					 if (length tors <> N)
					 then Err (sof'tname tname ^ "/arity")
					 else Okay (tor'cons (tname,tors)))
	  

  fun tc'datbind (T:T) context (DATBIND xs) =
	  case xs
	   of [(tyvars,tycon,clauses)] =>
		  let val tname = genTname tycon
			  val typefun = makeTypefun tname (length tyvars)
			  val pairs = map (fn tyvar => (tyvar,genUt())) tyvars
			  val uts = map #2 pairs
			  val tor = tor'cons (tname,map TOR'Ut uts)
			  val a = assoc'list pairs
			  fun TV tv = case (lookup'assoc a tv) of Some ut => Okay (TOR'Ut ut) | None => Err (sof'tyvar tv)
			  fun tyenv'valenv valenv = tyenv'single (tycon, tystr'cons (typefun,valenv))
			  val tyenv = tyenv'plus (tyenv'context context) (tyenv'valenv valenv'empty)
		  in
			  linkUni (map (fn (vid,tyopt) => 
							   (case tyopt
								 of Some ty => T TV (tyenv,ty) >>> (fn tor' => tor'arrow (tor',tor))
								  | None => resultUni tor) 
							   >>> (fn tor => valenv'single (vid,(tscheme'cons uts tor,IDS'C))))
						   clauses)
					  >>> valenv'plusList >>> (fn valenv => (tyenv'valenv valenv, valenv))
		  end
		| _ => notYet "datbind/2+"		  

(*----------------------------------------------------------------------
INDEX: exbind
----------------------------------------------------------------------*)

  fun tc'exbind1 (T:T) context b =
	  case b 
       of EXBIND1'New (vid,tyo) => (case tyo 
									 of None => resultUni (tor'exn)
									  | Some ty => T TV (tyenv'context context,ty) >>> (fn tor => tor'arrow (tor,tor'exn)))
								   >>> (fn tor => valenv'single (vid, (tscheme'cons [] tor, IDS'E)))
        | EXBIND1'Copy (vid,longY)  => notYet "exbind1/copy"

  fun tc'exbind T context (EXBIND bs) = 
	  linkUni (map (tc'exbind1 T context) bs) >>> valenv'plusList

(*----------------------------------------------------------------------
INDEX: dec
----------------------------------------------------------------------*)
  fun U_tc'dec {T:T,P,E,D:D} : D = fn (context,dec) =>
      case dec
       of DEC'Val (tyvars,vb)		=> let val X = unguarded'vb vb
										   val Y = list'tvscoped (tvscoped'context context)
										   val scoped = tyvars && (X -- Y) 
										   val context = context'plus context (context'tvscoped (tvscoped'list scoped))
									   in 
										   tc'valbind P E (context,vb) >> (fn valenv => 
										   theSubUni >>> (fn sub =>
										   env'valenv (valenv'close sub scoped (utsOf'context context) valenv)))
									   end
        | DEC'Fval fb               => D (context, DEC'Val ([], valbind'fvalbind fb))
        | DEC'Type typbind          => notYet "type"
        | DEC'Datatype (db,None)    => tc'datbind T context db >>> (fn (te,ve) => env'plus (env'valenv ve) (env'tyenv te))
        | DEC'Datatype (db,tbo)     => notYet "datatype/Some tbo"
        | DEC'DataReplicate (tycon,tyconlong)=> notYet "data-replicate"
        | DEC'Abstype (db,tbo,dec)  => notYet "abstype"
        | DEC'Exception exbind      => tc'exbind T context exbind >>> env'valenv
        | DEC'Local (dec1,dec2)     => notYet "local"
        | DEC'Open [LONG([],strid)] => resultUni (look'strid (strenv'context context) strid)
        | DEC'Open xs               => notYet "open"
        | DEC'Empty                 => resultUni env'empty
        | DEC'Seq (dec1,dec2)       => D (context,dec1) >> (fn env1 => 
                                       D (context'plus context (context'env env1),dec2) >>> (fn env2 =>
                                       env'plus env1 env2))

(*----------------------------------------------------------------------
INDEX: fix: T,P, E/D
----------------------------------------------------------------------*)
  local fun T x = tc'ty x
		fun P x = U_tc'pat {T=T,P=P} x
        fun E x = U_tc'exp {T=T,P=P,E=E,D=D} x
        and D x = U_tc'dec {T=T,P=P,E=E,D=D} x
  in val tc'exp = E
	 val tc'dec = D
  end

(*----------------------------------------------------------------------
INDEX: basis
----------------------------------------------------------------------*)

(*
  datatype sig = SIG of env (* & tyname set *)
  val sig'con = SIG
*)

(*
  datatype functorclosure = FUNCTORCLOSURE of env -> env
  val functorclosure'fn = FUNCTORCLOSURE
  fun apply'functorclosure (FUNCTORCLOSURE f) env = f env
*)
  datatype functorclosure = FUNCTORCLOSURE

  datatype sigenv = SIGENV of (sigid,env) assoc
  datatype funenv = FUNENV of (funid,functorclosure) assoc
							
  val funenv'empty = FUNENV assoc'empty
  val sigenv'empty = SIGENV assoc'empty
					 
  fun funenv'plus (FUNENV a1) (FUNENV a2) = FUNENV (assoc'plus a1 a2)
  fun sigenv'plus (SIGENV a1) (SIGENV a2) = SIGENV (assoc'plus a1 a2)

  val funenv'plusList = foldR funenv'plus funenv'empty
  val sigenv'plusList = foldR sigenv'plus sigenv'empty

  fun funenv'single kv = FUNENV (assoc'single kv)
  fun sigenv'single kv = SIGENV (assoc'single kv)

  datatype basis = BASIS of funenv*sigenv*env
  val basis'empty = BASIS (funenv'empty,sigenv'empty,env'empty)
												  
  fun basis'plus (BASIS (fe1,ge1,env1)) (BASIS (fe2,ge2,env2)) =
	  let val fe = funenv'plus fe1 fe2
		  val ge = sigenv'plus ge1 ge2
		  val env = env'plus env1 env2
	  in BASIS (fe,ge,env)
	  end

  fun basis'funenv funenv = BASIS (funenv,sigenv'empty,env'empty)
  fun basis'sigenv sigenv = BASIS (funenv'empty,sigenv,env'empty)
  fun basis'env env = BASIS (funenv'empty,sigenv'empty,env)
					
  fun look'sigid (BASIS (_,SIGENV a,_)) = look'assoc "look'sigid" sof'sigid a
  fun look'funid (BASIS (FUNENV a,_,_)) = look'assoc "look'funid" sof'funid a
  fun env'basis (BASIS (fe,ge,env)) = env

  val context'basis = context'env o env'basis


  fun lay'sigenv (SIGENV assoc) =
	  layVert
	  (map (fn (sigid,env) =>
			   Word "signature" ++ lay'sigid sigid ++ Word "="
					//`` (Word "sig" //`` lay'env env // Word "end")
			   ) (revList'assoc assoc))

  fun lay'funenv (FUNENV assoc) = Blank
	  
  fun lay'basis (BASIS (fe,ge,env)) =
	  lay'sigenv ge // lay'funenv fe // lay'env env (* sigs before functors *)

(*----------------------------------------------------------------------
INDEX: sigexp, spec
----------------------------------------------------------------------*)

  type TY = (ty -> tscheme)
  type SPEC = (spec -> env)
  type SIG = (sigexp -> env)

  fun U_tc'sigexp (SPEC:SPEC) : SIG = fn sigexp =>
	  case sigexp
       of SIG'Spec spec         => SPEC spec
        | SIG'Id sigid          => notYet "sig/id" 
        | SIG'TypeRealisation _ => notYet "sig/TypeRealisation" 

  fun U_tc'spec (TY:TY) (SPEC:SPEC) : SPEC = fn spec =>
	  case spec 
       of SPEC'Val xs           => env'valenv (valenv'plusList (map (fn (vid,ty) =>
																		valenv'single (vid,(TY ty, IDS'V)))
																	xs))
        | SPEC'Type xs          => notYet "spec/type"
        | SPEC'Eqtype xs        => notYet "spec/eqtype"
        | SPEC'Datatype xs      => notYet "spec/datatype"
        | SPEC'DataReplicate x  => notYet "spec/data-rep"
        | SPEC'Exception xs     => notYet "spec/exception"
        | SPEC'Structure xs     => notYet "spec/structure"
        | SPEC'Include sigexp   => notYet "spec/include" 
        | SPEC'Empty            => env'empty
        | SPEC'Seq (spec1,spec2)=> env'plus (SPEC spec1) (SPEC spec2)
        | SPEC'Sharing (spec,_) => notYet "spec/sharing" 


  fun tscheme'ty (basis,ty) =
	  case (execUni (tc'ty (Okay o TOR'Rigid) (tyenv'context (context'basis basis), ty)
						   >>> tscheme'close empty'sub (tyvars'ty ty) []))
	   of Err s => abort ("tscheme'ty - "^s)
		| Okay x => x


  local 
	  fun TY basis x = tscheme'ty (basis,x)
	  fun SPEC basis x = U_tc'spec (TY basis) (SPEC basis) x
	  fun SIG basis x  = U_tc'sigexp (SPEC basis) x
  in
	 val tc'sigexp = SIG
  end

  fun tc'sigdec basis x =
	  case x of
		  SIGDEC xs =>
		  sigenv'plusList
			  (map (fn (sigid,sigexp) =>
					   let val env = tc'sigexp basis sigexp
						   val sigenv = sigenv'single (sigid,env)
					   in sigenv
					   end) xs)

(*----------------------------------------------------------------------
INDEX: strexp, strdec
----------------------------------------------------------------------*)

  type SE = (basis * strexp -> env uni)
  type SD = (basis * strdec -> env uni)

  fun U_tc'strexp (SE:SE) (SD:SD) : SE = fn (basis,x) =>
	  case x 
	   of STREXP'Struct strdec				=> SD (basis,strdec)
        | STREXP'Id stridlong               => notYet "strexp/id"
        | STREXP'TransCon (strexp,sigexp)   => notYet "strexp/TransCon"
        | STREXP'OpaqueCon (strexp,sigexp)  => notYet "strexp/OpaqueCon"
        | STREXP'FunctorApp (funid,strexp)  => notYet "strexp/FunctoApp" 
        | STREXP'Let (strdec,strexp)        => notYet "strexp/Let"

  fun U_tc'strdec (SE:SE) (SD:SD) : SD = fn (basis,x) =>
	  case x 
       of STRDEC'Dec dec            => tc'dec (context'basis basis,dec)
        | STRDEC'Structure xs       => linkUni (map (fn (strid,strexp) => SE (basis,strexp) >>> (fn env => strenv'single (strid,env))) xs)
										   >>> strenv'plusList >>> env'strenv
        | STRDEC'Local (sd1,sd2)    => SD (basis,sd1) >> (fn env1 => 
                                       SD (basis'plus basis (basis'env env1),sd2))
        | STRDEC'Empty              => resultUni env'empty
        | STRDEC'Seq (sd1,sd2)      => SD (basis,sd1) >> (fn env1 => 
                                       SD (basis'plus basis (basis'env env1),sd2) >>> (fn env2 =>
                                       env'plus env1 env2))

  local
	  fun SE x = U_tc'strexp SE SD x
	  and SD x = U_tc'strdec SE SD x
  in
	val tc'strdec = SD
  end

(*----------------------------------------------------------------------
INDEX: topdec(s)
----------------------------------------------------------------------*)
  fun tc'topdec (basis,td) =
	  case td 
       of TOPDEC'Exp exp    => tc'dec (context'basis basis,makeItDec exp) >>> basis'env
        | TOPDEC'Strdec x   => tc'strdec (basis,x) >>> basis'env
        | TOPDEC'Sigdec x   => resultUni (tc'sigdec basis x) >>> basis'sigenv
        | TOPDEC'Fundec x   => notYet "fundec"

  fun tc'topdecs (basis,tds) =
	  case tds
       of []                => resultUni basis'empty
        | td::tds           => tc'topdec (basis,td) >> (fn basis1 => 
							   tc'topdecs (basis'plus basis basis1,tds) >>> (fn basis2 =>
                               basis'plus basis1 basis2))

(*----------------------------------------------------------------------
INDEX: runTc
----------------------------------------------------------------------*)
  val THE_basis = ref basis'empty (* RESETS TO EMPTY *)
  fun extendTheBasis basis = THE_basis := basis'plus (!THE_basis) basis

  fun runTc tds =
      let val basis = ! THE_basis
      in case (execUni (tc'topdecs (basis,tds)))
          of Err s => see (Word ("TcErr:"^s))
           | Okay basis => let val () = see (Word "TC :" @@ lay'basis basis)
							   val () = extendTheBasis basis
						   in ()
						   end
      end

  fun tc string = parseThen runTc (fn () => Run.tpString string)
  fun tc' path = parseThen runTc (fn () => Run.tpFile {path=path})

(*----------------------------------------------------------------------
INDEX: predefined context
----------------------------------------------------------------------*)
  val tyenv'initial =
	  (TYENV o assoc'list o map (fn (s,f) => (TYCON (ID s), tystr'cons (typefun'fn f,valenv'empty))))
	  [
       ("unit"  , fn []  => Okay tor'unit       | _ => Err "unit/arity"),
       ("int"   , fn []  => Okay tor'int        | _ => Err "int/arity"),
       ("char"  , fn []  => Okay tor'char       | _ => Err "char/arity"),
       ("string", fn []  => Okay tor'string     | _ => Err "string/arity")
      ]

