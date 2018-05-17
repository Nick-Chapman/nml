(*------------------------------------------------------------------------------
CONTENTS-START-LINE: HERE=2 SEP=1
 15.    Comment from Anstract days
 62.    Export: ImpHashSig
 128.   imports
 155.   functor
 167.   hashTables...
 374.   hashSets...
 396.   pairing & tripling  hash & eqaulity Functions...
 455.   misc...
CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*------------------------------------------------------------------------------
INDEX: Comment from Anstract days
------------------------------------------------------------------------------*)

(* Imperative (expanding) hash tables... *)

(***
 * $Id: IMP_HASH.ML,v 1.8 1998/01/26 12:47:50 mick Exp $
 *
 * $Log: IMP_HASH.ML,v $
 * Revision 1.8  1998/01/26 12:47:50  mick
 * Removed delKeyInBucket. Explicit isMem tests now at call points, and accDel
 * used directly. This avoids creating the Some objects.
 *
 * Revision 1.7  1998/01/22  13:33:10  mick
 * Added delAllHT.
 * Added explicit argument (bucket) to lookKeyInBucket to take advantage of
 * Poly/ML optimisation - no closure created now when called with all args.
 *
 * Revision 1.6  1998/01/09  16:04:06  mick
 * Took local functions out of delKeyInBucket and lookKeyInBucket by giving
 * them extra parameter. This prevents closures from being created for each
 * call to the functions.
 *
 * Revision 1.5  1997/11/12  16:33:41  nic
 * Removed: insertHT, insertHS
 * setHS now returns: {hit:bool}
 * Moved histFuncH her from MISC4.
 * setHS now avoids doing the setHT if the element is already in the set.
 *
 * Revision 1.4  1997/11/12  11:57:28  nic
 * Switch to curried equality functions.
 *
 * Revision 1.3  1997/09/24  07:04:18  nic
 * Added functions: delHT & setHT.
 * Switched off run-time assertions.
 * Reimplemented insertHT in terms of getHT & setHT.
 * Reimplemented insertHS & firstSetHT in terms of setHT.
 *
 * Revision 1.2  1997/09/10  08:27:00  nic
 * Removed dependence on Counter module.
 *
 * Revision 1.1  1997/05/20  09:45:22  nic
 * Initial revision
 *
 ***)

(*------------------------------------------------------------------------------
INDEX: Export: ImpHashSig
------------------------------------------------------------------------------*)

signature ImpHashSig =
sig
    type layout
    type ('k,'v) hashTable
    type 'a hashSet

    val mkEmptyHT   : {eq   : 'k -> 'k -> bool,
					   hash : 'k -> int} -> ('k,'v) hashTable
    val mkEmptyOfHT : ('k,'v) hashTable -> ('k,'v) hashTable
    val sizeHT	    : ('a, 'b) hashTable -> int
    val layHT       : ({key : 'k, value : 'v} -> layout) -> ('k,'v) hashTable -> layout
    val foldHT	    : ('a -> {key: 'b, value: 'c} -> 'a) -> 'a -> ('b, 'c) hashTable -> 'a
    val listHT	    : ('a, 'b) hashTable -> {key: 'a, value: 'b} list
    val getHT	    : ('k,'v) hashTable -> 'k -> {key   : 'k,
						     value : 'v} option
    val delHT       : ('k,'v) hashTable -> 'k -> {hit : bool}
    val delAllHT    : ('k,'v) hashTable -> unit
    val setHT	    : ('k,'v) hashTable -> {key : 'k,
											value : 'v} -> {hit : bool}
        
    val mkEmptyHS   : {eq: 'a -> 'a -> bool, hash: 'a -> int} -> 'a hashSet
    val mkEmptyOfHS : 'a hashSet -> 'a hashSet
    val sizeHS      : 'a hashSet -> int
    val layHS       : ('a -> layout) -> 'a hashSet -> layout
    val foldHS      : ('a -> 'b -> 'a) -> 'a -> 'b hashSet -> 'a
    val listHS      : 'a hashSet -> 'a list
    val memHS       : 'a hashSet -> 'a -> bool
    val delHS       : 'a hashSet -> 'a -> {hit: bool}
    val setHS       : 'a hashSet -> 'a -> {hit: bool}

    val eqPair        : ('a -> 'a -> bool) * ('b -> 'b -> bool) -> ('a * 'b) -> ('a * 'b) -> bool
    val eqTrip        : ('a -> 'a -> bool) * ('b -> 'b -> bool) * ('c -> 'c -> bool)
						-> ('a * 'b * 'c) -> ('a * 'b * 'c) -> bool
    val eqList        : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
    val hashPair      : ('a -> int) * ('b -> int) -> 'a * 'b -> int
    val hashTrip      : ('a -> int) * ('b -> int) * ('c -> int) -> 'a * 'b * 'c -> int
    val hashList      : ('a -> int) -> 'a list -> int
    val eqBool        : bool -> bool -> bool
    val hashBool      : bool -> int
	
    val firstSetHT      : ('a, 'b) hashTable -> 'a -> 'b -> unit
    val updateHT		: ('a,'b) hashTable -> 'a * 'b -> unit
    val lookHT          : ('a, 'b) hashTable -> 'a -> 'b option
    val lookHT'         : ('a, 'b) hashTable -> 'a -> 'b

    val addHS	        : 'a hashSet -> 'a -> unit
    val mkHS            : {eq: 'a -> 'a -> bool, hash: 'a -> int} -> 'a list -> 'a hashSet

    val memberH         : {eq: 'a -> 'a -> bool, hash: 'a -> int} -> 'a list -> 'a -> bool
    val duplicatesH     : {eq: 'a -> 'a -> bool, hash: 'a -> int} -> 'a list -> 'a list
    val noRepeatsH      : {eq: 'a -> 'a -> bool, hash: 'a -> int} -> 'a list -> 'a list
    val pairs2func      : {eq: 'a -> 'a -> bool, hash: 'a -> int} -> ('a * 'b) list -> 'a -> 'b option
    val pairs2func'     : {eq: 'a -> 'a -> bool, hash: 'a -> int} -> ('a * 'b) list -> 'a -> 'b
    val pairs2funcInv   : {eq: 'a -> 'a -> bool, hash: 'a -> int} -> ('b * 'a) list -> 'a -> 'b option

    val histFuncH :
		{eq: 'a -> 'a -> bool, hash: 'a -> int} ->
		('a * 'b) list ->
		{func: 'a -> 'b list, keys: 'a list}
		
end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig Prel.ML *)
signature PrelSig_Import =
sig
    val echo            : string -> unit
    val abort           : string -> 'a
    val fold            : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
end;
(*IMPORTDONE:*)

(*IMPORTSIG: MisclaySig MISCLAY.ML *)
signature MisclaySig_Import =
sig
  type layout
  val Word              : string -> layout
  val Right             : string -> layout
  val Blank             : layout
  val @@                : layout * layout -> layout
  val layInt            : int -> layout
  val layListSep        : layout -> layout list -> layout
  val layVert           : layout list -> layout
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor IMP_HASH(structure Prel : PrelSig_Import
		 structure Misclay : MisclaySig_Import
				     ) : ImpHashSig =
struct

  open Prel Misclay
  infix @@

(*------------------------------------------------------------------------------
INDEX: hashTables...
------------------------------------------------------------------------------*)
	
    datatype ('k,'v) hashTable =
        HT of {eq               : 'k -> 'k -> bool,
               hash             : 'k -> int, 
               arrayRef         : {key : 'k, value : 'v}  list Array.array ref,
               objectCountRef   : int ref}

    fun invariantHT (HT{arrayRef,objectCountRef,...}) =
	let
	    val objectCount = !objectCountRef
	    val array = !arrayRef
	    val capacity = Array.length array
	in
	    capacity >= 1
	    andalso (objectCount <= capacity)
	end

    exception BadHT
    val assertHT = fn (_: ('k,'v) hashTable) => ()
    (*val assertHT = (echo "**using assertHT"; fn ht => if (invariantHT ht) then () else raise BadHT)*)


    fun mkEmptyHT {eq,hash} =
	let
	    (*val X = see (Word "mkEmptyHT:1")*)
	    val ht = HT {eq = eq, hash = hash,
			 arrayRef = ref (Array.array (1,[])),
			 objectCountRef = ref 0}
	in
	    (assertHT ht; ht)
	end

    fun mkEmptyOfHT (HT{eq,hash,...}) = mkEmptyHT {eq=eq,hash=hash}

    fun sizeHT (HT{objectCountRef,...}) = !objectCountRef

    fun layList lay xs = layListSep (Right ",") (map lay xs)

    fun layHT layKV (HT{arrayRef,objectCountRef,...}) =
	let
	    val array = !arrayRef
	    fun loop acc slot =
		let val bucket = Array.sub (array,slot)
		    val acc = (layInt slot @@ Word ":" @@ layList layKV bucket) :: acc
		in if slot = 0 then acc else loop acc (slot-1)
		end
	    val capacity = Array.length array
	in layVert [Blank,
		    Word "HT_array=" @@ layVert (loop [] (capacity - 1)),
		    Word "HT_objectCount=" @@ layInt (!objectCountRef)]
	end

    
    fun foldHT f acc (ht as HT{arrayRef,...})  =
	let
	    val array = !arrayRef
	    fun foldArrayBefore acc slot  =
		let
		    val bucket = Array.sub (array,slot)
		    val acc = fold f acc bucket
		in
		    if slot = 0 then acc
				else foldArrayBefore acc (slot-1)
		end
	    val capacity = Array.length array
	in
	    foldArrayBefore acc (capacity - 1)
	end

    fun listHT ht = foldHT (fn xs => fn x => x :: xs) [] ht

    local
	fun expand hash oldArray =
	    let
		val oldCapacity = Array.length oldArray
		val newCapacity = 2 * oldCapacity
		val newArray = Array.array (newCapacity, [])
		fun copyBucket oldBucket =
		    case oldBucket of
			[] => ()
		      | (kv as {key,value})::oldBucket =>
			    let
				val newSlot = (hash key) mod newCapacity
				val newBucket = Array.sub (newArray, newSlot)
				val newBucket = kv :: newBucket
			    in
				(Array.update (newArray, newSlot, newBucket);
				 copyBucket oldBucket)
			    end
		fun copyArrayBefore oldSlot =
		    let
			val oldBucket = Array.sub (oldArray,oldSlot)
		    in
			(copyBucket oldBucket;
			 if oldSlot = 0 then () else copyArrayBefore (oldSlot-1))
		    end
	    in
		(copyArrayBefore (oldCapacity - 1);
		 newArray)
	    end

	local
	  fun look eq_searchKey bucket =
	    case bucket of
	      [] => None
	    | (kv as {key,value=_})::bucket => if (eq_searchKey key) then Some kv
								     else look eq_searchKey bucket
	in
	  fun lookKeyInBucket (eq: 'a -> 'a -> bool) searchKey bucket =
	  (******
	   * Having bucket argument prevents closure being created in Poly/ML, since it is
	   * optimised out for local functions where all arguments are given.
	   ******)
	      look (eq searchKey) bucket
	end

	local
	  fun revOnto [] ys = ys
	    | revOnto (x::xs) ys = revOnto xs (x::ys)
        in
	  fun accDel eq_searchKey acc bucket =
	      case bucket of
		  [] => abort "accDel"
		| (kv as {key,value=_})::bucket =>
		      if (eq_searchKey key) then revOnto acc bucket
					    else accDel eq_searchKey (kv::acc) bucket
        end

	fun isMem eq_searchKey bucket =
	    case bucket of
		[] => false
	      | (kv as {key,value=_})::bucket => if (eq_searchKey key) then true
								       else isMem eq_searchKey bucket
    in
	fun getHT (HT{eq,hash,arrayRef,objectCountRef}) key =
	    let
		val array = !arrayRef
		val capacity = Array.length array
		val slot = (hash key) mod capacity
		val bucket = Array.sub (array,slot)
	    in
		lookKeyInBucket eq key bucket
	    end

	fun delHT (ht as HT{eq,hash,arrayRef,objectCountRef}) key =
	    let
		val array = !arrayRef
		val capacity = Array.length array
		val slot = (hash key) mod capacity
		val bucket = Array.sub (array,slot)
		val eqKey  = eq key
	    in
		if isMem eqKey bucket then
		  let
		      val bucket = accDel eqKey [] bucket
		      val (EFFECT:unit) = Array.update (array, slot, bucket)
		      val objectCount = !objectCountRef
		      (* Should reduce array here if objectCount < capacity/2  *)
		      val (EFFECT:unit) = (objectCountRef := objectCount - 1)
		  in
		      (assertHT ht; {hit=true})
		  end
		else
		  {hit=false}
	    end
	
	fun delAllHT (ht as HT{eq,hash,arrayRef,objectCountRef}) =
	(******
	 * This is useful to allow garbage to be collected once the HT is no longer needed,
	 * but is still referenced.
	 ******)
	(
	  arrayRef := Array.array (1,[]);
	  objectCountRef := 0
	);


	fun setHT (ht as HT{eq,hash,arrayRef,objectCountRef}) (kv as {key,value=_}) =
	    let
		val array = !arrayRef
		val capacity = Array.length array
		val slot =  (hash key) mod capacity
		val bucket = Array.sub (array,slot)
		val eqKey  = eq key
	    in
		if isMem eqKey bucket then
		  let val bucket = accDel eqKey [] bucket
		      val (EFFECT:unit) = Array.update (array, slot, kv :: bucket)
		  in {hit=true}
		  end
		else
		  let
		      val objectCount = !objectCountRef
		      val (EFFECT:unit) = Array.update (array, slot, kv :: bucket)
		      val (EFFECT:unit) =
			  if (objectCount = capacity)
			  then (arrayRef := expand hash array)
			  else ()
		      val (EFFECT:unit) = (objectCountRef := 1 + objectCount)
		  in (assertHT ht; {hit=false})
		  end
	    end
    end    

(*------------------------------------------------------------------------------
INDEX: hashSets...
------------------------------------------------------------------------------*)

    abstype 'a hashSet = HS of ('a,unit) hashTable
    with
	fun mkEmptyHS R = HS (mkEmptyHT R)
	fun mkEmptyOfHS (HS ht) = HS (mkEmptyOfHT ht)
	fun sizeHS (HS ht) = sizeHT ht
	fun layHS lay (HS ht) = layHT (fn {key,...} => lay key) ht
	fun foldHS f acc (HS ht) = foldHT (fn acc => fn {key,value=()} => f acc key) acc ht
	fun memHS (HS ht) key = case (getHT ht key) of None => false | Some _ => true
	(*fun setHS (HS ht) key = setHT ht {key=key,value=()}*)
	fun setHS (HS ht) key =
	    case (getHT ht key) of
		Some _ => {hit=true}
	      | None => setHT ht {key=key,value=()}
	fun delHS (HS ht) key = delHT ht key
    end

    fun listHS hs = foldHS (fn xs => fn x => x :: xs) [] hs

(*------------------------------------------------------------------------------
INDEX: pairing & tripling  hash & eqaulity Functions...
------------------------------------------------------------------------------*)
 
    fun eqPair (eqA,eqB) (a:'a,b:'b) (a2:'a,b2:'b) = eqA a a2 andalso eqB b b2
    fun eqTrip (eqA,eqB,eqC) (a:'a,b:'b,c:'c) (a2:'a,b2:'b,c2:'c) =
	eqA a a2 andalso eqB b b2 andalso eqC c c2

    fun eqList (eq: 'a -> 'a -> bool) xs ys =
	case (xs,ys) of
	    ([],[]) => true
	  | (x::xs,y::ys) => eqList eq xs ys andalso eq x y
	  | _ => false
		


(* Limited experimental evidence shows that the simpler version
 below is a better hash function...
    local
	fun tri x = x * (x+1) div 2
	fun pyramid x = x * (x+1) * (x+2) div 6
	val (ASSERT:unit) = assert "tri" (map tri [0,1,2,3,4,5] = [0,1,3,6,10,15])
	val (ASSERT:unit) = assert "pyramid" (map pyramid [0,1,2,3,4,5] = [0,1,4,10,20,35])
	fun pairHashed (a,b) = tri (a + b) + a
	fun tripHashed (a,b,c) = pyramid (a + b + c) + tri (a + b) + a
	val (ASSERT:unit) = assert "pairHashed"
	    (map pairHashed [(0,0),
			     (0,1),(1,0),
			     (0,2),(1,1),(2,0),
			     (0,3),(1,2),(2,1),(3,0),
			     (0,4) (*...*)]
	     = [0,1,2,3,4,5,6,7,8,9,10]) 
	val (ASSERT:unit) = assert "tripHashed"
	    (map tripHashed [(0,0,0),
			     (0,0,1),(0,1,0),(1,0,0),
			     (0,0,2),(0,1,1),(1,0,1),(0,2,0),(1,1,0),(2,0,0),
			     (0,0,3) (*...*)]
	     = [0,1,2,3,4,5,6,7,8,9,10])
    in
	fun hashPair (hashA,hashB) (a,b) = pairHashed (hashA a, hashB b)
	fun hashTrip (hashA,hashB,hashC) (a,b,c) = tripHashed (hashA a, hashB b, hashC c)
    end
...*)	


    local
	fun pairHashed_SIMPLER (a,b) = 3 * a + b
	fun tripHashed_SIMPLER (a,b,c) = 3 * (3 * a + b) + c
    in
	fun hashPair (hashA,hashB) (a,b) = pairHashed_SIMPLER (hashA a, hashB b)
	fun hashTrip (hashA,hashB,hashC) (a,b,c) = tripHashed_SIMPLER (hashA a, hashB b, hashC c)
	fun hashList hash = fold (fn acc => fn x => pairHashed_SIMPLER (acc,hash x)) 0
    end

    

    fun eqBool (b : bool) b'= (b=b')
    fun hashBool b = (if b then 1 else 0)

(*------------------------------------------------------------------------------
INDEX: misc...
------------------------------------------------------------------------------*)

    fun impMap f [] = ()
      | impMap f (x::xs) = (f x : unit; impMap f xs)


    fun firstSetHT ht key value =
	let val kv = {key = key, value = value}
	    val {hit} = setHT ht kv
	    val (ASSERT:unit) = if hit then abort "firstSetHT:not first" else ()
	in ()
	end
		
    fun updateHT ht (k,v) = let val {hit=_} = setHT ht {key=k,value=v} in () end

    fun lookHT ht k = 
	case (getHT ht k) of
	    None => None
	  | Some {key=_,value=v} => Some v

    fun lookHT' ht key =
	case (getHT ht key) of
	    None => abort "lookHT'"
	  | Some {value,...} => value


    fun addHS set x = let val {hit=_} = setHS set x in () end

    fun mkHS eh xs =
	let val hs = mkEmptyHS eh
	    val (EFFECT:unit) = impMap (fn x => let val {hit=U} = setHS hs x
						in ()
						end) xs
	in hs
	end

    fun memberH eh xs =
	let val set = mkHS eh xs
	    fun mem x = memHS set x
	in mem
	end

    fun duplicatesH eh xs =
	let val hs = mkEmptyHS eh
	in rev (fold (fn dups => fn x => 
		      let val {hit} = setHS hs x
		      in if hit then (x :: dups) else dups
		      end) [] xs)
	end

    fun noRepeatsH eh xs = listHS (mkHS eh xs)

(*    
val ehInt = {eq = (op =), hash = (fn x => x)}    
duplicatesH ehInt [1,7,4,7,2,2,3,3,6,7]
*)

    fun pairs2func eh pairs = (* partial function *)
	let val ht = mkEmptyHT eh
	    val (EFFECT:unit) = impMap (fn (k,v) => firstSetHT ht k v) pairs
	in lookHT ht
	end

    fun pairs2func' eh xs = (* total function *)
	let val f = pairs2func eh xs
	in fn k =>
	      case (f k) of
		  None => abort "pairs2func"
		| Some v => v
	end
    
    fun pairs2funcInv eh pairs =
	let val ht = mkEmptyHT eh
	    val (EFFECT:unit) = impMap (fn (v,k) => firstSetHT ht k v) pairs
	in lookHT ht
	end


    fun histFuncH eh pairs =
	let val ht = mkEmptyHT eh
	    fun lookDefNil k = case (getHT ht k) of Some {key=_,value=vs} => vs | None => []
	    val (U:unit) = impMap (fn (k,v) =>
				   let val vs = lookDefNil k
				       val {hit=U} = setHT ht {key = k, value = v::vs}
				   in ()
				   end) pairs
	    val keys = foldHT (fn keys => fn {key,value=_} => key::keys) [] ht
	in {func = lookDefNil,
	    keys = keys}
	end
	
    
end; (* struct *)

