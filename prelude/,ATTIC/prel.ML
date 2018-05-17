
use "/home/nic/project/prelude/Prel.ML";
use "/home/nic/project/prelude/Sort.ML";
use "/home/nic/project/prelude/option.ML";
use "/home/nic/project/prelude/QLAYOUT.ML";
use "/home/nic/project/prelude/MISCLAY.ML";
use "/home/nic/project/prelude/IMP_HASH.ML";
use "/home/nic/project/nml/PFV.ML";
structure QLayout = QLAYOUT(structure Prel = Prel)
structure Misclay = MISCLAY(structure Prel = Prel
							structure QLayout = QLayout)
structure ImpHash = IMP_HASH(structure Prel = Prel
							 structure Misclay = Misclay)
open Prel
open Sort
open Misclay
open ImpHash
infix @@
infix ++
infix ``
infix //
infix //``


fun (lay1 //`` lay2) = AboveTab 4(*2*) lay1 lay2

fun see' layout = output_layout layout
fun see layout = output_layout (layVert [layout,BlankLine])


fun write n filename layout =
	let val os = TextIO.openOut filename
		val SQUASH = (if n=0 then (fn(x)=>x) else squash_to n)
		val () = stream_layout os (SQUASH (layVert [layout,BlankLine]))
		val () = TextIO.closeOut os
	in ()
	end


fun member ys x = exists (fn y => x=y) ys


fun removeDups [] = []
  | removeDups (x::xs) = x :: removeDups (if (member xs x)
										  then filter (fn y => x<>y) xs 
										  else xs)

fun uncurry f (a,b) = f a b


fun filterSome [] = []
  | filterSome (None::xs) = filterSome xs
  | filterSome (Some x::xs) = x :: filterSome xs


fun fix F x = F (fix F) x


fun removeAdjacentDups [] = []
  | removeAdjacentDups [x] = [x]
  | removeAdjacentDups (x1::x2::xs) = if (x1=x2) then removeAdjacentDups (x2::xs)
									  else x1 :: removeAdjacentDups (x2::xs)

(*removeAdjacentDups [1,2,3,3,4,4,4,5,6,7,6,6,5,5,5,4,3]*)

fun layOption lay = fn Some x => lay x | None => Word "*None*"



local
	fun loop acc y [] = (rev acc, y)
	  | loop acc y (x::xs) = loop (y::acc) x xs
in
fun splitLast [] = abort "splitLast"
  | splitLast (x::xs) = loop [] x xs
end


fun splitP f xs =
	let val n = ref 0
	in split (fn x => (n:=1+(!n);
					   if (!n mod 10 = 0) then echo' ("."^stringOfInt (!n)) else (); 
					   f x)) xs
	end



infix --
fun ([] -- ys) = []
  | ((x::xs) -- ys) = if (member ys x) then (xs--ys) else x :: (xs--ys)



fun mapOption f = fn None => None | Some x => Some (f x)


fun layVertBL [] = Blank
  | layVertBL [x] = x
  | layVertBL (x::xs) = Above (Above x (Sep "")) (layVertBL xs)



fun lookup' xs k =
	let fun look [] = None
		  | look ((k',v)::xs) = if (k=k') then Some v else look xs
	in look xs
	end

fun lookupV sofk xs k =
	case (lookup' xs k) of
		None => abort ("lookup:"^sofk k)
	  | Some v => v

fun lookupS xs = lookupV (fn k => k) xs
(*fun lookup xs = lookupV (fn k => "<cant-print-key>") xs*)

fun lookupL lay xs = lookupV (fn k => show_layout(lay k)) xs


fun pair (a,b) = (a,b)
fun zip xs ys = zipWith "zip" pair xs ys


fun takeWhile P [] = []
  | takeWhile P (x::xs) = if (P x) then (x :: takeWhile P xs) else []


fun take n [] = []
  | take n (x::xs) = if (n > 0) then (x :: take (n-1) xs) else []

fun basename path = 
	implode (rev (takeWhile (fn x => x<>(#"/")) (rev (explode path))))



fun isPrintable c = (c >= #" ") andalso (c <= #"~")

fun quoteChar c =
	case c of
		#"\"" => "\\\""
	  | #"\\" => "\\\\"
	  | #"\t" => "\\t"
	  | #"\n" => "\\n"
	  | _ => 
		if (isPrintable c) then	implode [c] 
		else let val n = ord c
			 in "\\"^(if (n<10) then "00" else if (n<100) then "0" else "")^stringOfInt n
			 end


fun quoteString s = "\"" ^ old_implode (map quoteChar (explode s)) ^ "\""

fun quoteCharForRegExp c =
	case c of
		#"\"" => "\\\""
	  | #"\\" => "\\\\"
	  | #"\t" => "\\t"
	  | #"\n" => "\\n"
	  | #"[" => "\\["
	  | #"]" => "\\]"
	  | #"(" => "\\("
	  | #")" => "\\)"
	  | #"|" => "\\|"
	  | #"*" => "\\*"
	  | #"+" => "\\+"
	  | #"?" => "\\?"
	  | #"." => "\\."
	  | #"^" => "\\^"
	  | #"$" => "\\$"
	  | _ => implode [c]

fun layBraceC {leftContext} x = leftContext @@ Word "{" `` x ++ Word "}"

fun layBraceCV {leftContext} x = 
		(leftContext @@ Word "{")
		//`` x
		// Word "}"


fun mergeSortedAssoc {less} {combine} =
	let fun merge [] ys = ys
		  | merge xs [] = xs
		  | merge (xs as ((x as (k1,v1))::xs')) (ys as ((y as (k2,v2))::ys')) =
			if (k1=k2) 
			then (k1, combine (v1,v2)) :: merge xs' ys'
			else if less(k1,k2) then x :: merge xs' ys
			else y :: merge xs ys'
	in merge
	end

fun mergeSortedAssocOverlap {less} {combine} =
	let fun merge [] ys = []
		  | merge xs [] = []
		  | merge (xs as ((x as (k1,v1))::xs')) (ys as ((y as (k2,v2))::ys')) =
			if (k1=k2) 
			then (k1, combine (v1,v2)) :: merge xs' ys'
			else if less(k1,k2) then merge xs' ys
			else merge xs ys'
	in merge
	end



fun layList LAY xs = 
	laySqBracket (layCommaSep (map LAY xs))

fun layListV LAY xs = 
	laySqBracket (layVertCommaSep (map LAY xs))


fun see_unsquashed layout = output_layout (layVert [layout,BlankLine])
fun see layout = output_layout (squash_to 98 (layVert [layout,BlankLine]))



(* filterMap -- filter, then map... *)

(*fun filterMap1 F P xs = map F (filter P xs)*)
(*
fun filterMap FP xs = 
	map ((fn x => case (FP x) of Some y => y | None => abort "filterMap")
		 ) (filter (fn x => case (FP x) of Some _ => true | None => false) xs)
*)
fun filterMap FP [] = []
  | filterMap FP (x::xs) = (case (FP x) of 
							   None => filterMap FP xs
							 | Some y => y :: filterMap FP xs)


fun layBind (layA,layB) (a,b) = layA a @@ Word "=" ++ layB b
fun layTag name lay x = layBind (Word,lay) (name,x)
fun layLabel name lay x = Word name @@ Right ":" //`` lay x

fun flip f a b = f b a

fun old_explode s = map (fn x => implode [x]) (explode s)

fun sofStringList xs = show_layout (layList Word xs)



local
	fun histAcc (x,n) [] = [(x,n)]
	  | histAcc (x,n) (y::ys) = if (x=y) then histAcc (x,n+1) ys else (x,n)::histAcc (y,1) ys
in
fun hist less xs =
	case (sort less xs) of
		[] => []
	  | x::xs => histAcc (x,1) xs
end

fun histEq [] = []
  | histEq (x::xs) =
	let val (yes,no) = split (fn x1 => x1=x) xs
	in (x,1+length yes) :: histEq no
	end

fun lessKey less p = (fn ((k1:'a,v1:'b),(k2:'a,v2:'b)) => less(k1,k2)) p : bool

local
	fun collateAcc eq (k,vs) [] = [(k,rev vs)]
	  | collateAcc eq (k,vs) ((k1,v)::xs) = if eq(k,k1) 
											then collateAcc eq (k,v::vs) xs
											else (k,rev vs)::collateAcc eq (k1,[v]) xs
in
fun collate {eq,less} xs =
	case (sort (lessKey less) xs) of
		[] => []
	  | (k,v)::xs => collateAcc eq (k,[v]) xs
fun collateAdjacentEq {eq} xs =
	case (xs) of
		[] => []
	  | (k,v)::xs => collateAcc eq (k,[v]) xs
end

fun collateEq {eq} [] = []
  | collateEq {eq} ((k,v)::xs) =
	let val (yes,no) = split (fn (k1,_) => eq(k1,k)) xs
	in (k,v::map #2 yes) :: collateEq {eq=eq} no
	end

infix et
fun (P1 et P2) x = P1 x andalso P2 x


fun duplicates xs = map #1 (filter (fn (_,x) => x>1) (histEq xs))


fun eqList eq [] [] = true
  | eqList eq (x::xs) (y::ys) = eq (x,y) andalso eqList eq xs ys
  | eqList _ _ _ = false


fun lessList {eq,less} =
	let fun LL _ [] = false
		  | LL [] (_::_) = true
		  | LL (x::xs) (y::ys) = less (x,y) orelse (eq (x,y) andalso LL xs ys)
	in LL
	end

fun layPairF (layA,layB) (a,b) = layPair (layA a, layB b)


fun layComment s = Word "//" @@ Word s
fun layCommentC s = Word "/*" @@ Word s @@ Word "*/"
