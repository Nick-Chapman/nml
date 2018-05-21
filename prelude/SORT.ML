
(* Wed May  3 10:50:31 2000 -- Taken from Poly/ML prelude... *)

signature SortSig =
sig
    val sort : ('a * 'a -> bool) -> 'a list -> 'a list
end;

functor SORT() : SortSig =
struct

    fun echo s = print (s^"\n")
    exception Abort of string
    fun abort s = (echo ("Abort: "^s); raise Abort s)

    fun revOnto acc [] = acc
      | revOnto acc (h::t) = revOnto (h::acc) t

    local fun chopAcc acc xs 0 = (rev acc, xs)
        | chopAcc _   [] _ = abort "chop:list too short"
        | chopAcc acc (x::xs) n = chopAcc (x::acc) xs (n-1)
    in fun chop xs n = if n < 0 then abort "chop:n<0" else chopAcc [] xs n
    end

    fun stableMergeSort lt =
    (******
     * Sort by splitting list into 2, sorting each list recursively, then merging
     * the sorted lists.
     *
     * Note that in order to be stable, if revMerge finds equal elements at the two
     * heads, it must put the 2nd one first, so when the result is rev'd, the element
     * from the first list goes first. This means it must cons the element from the
     * first list onto the accumulator first.
     *
     * The only reason for having revMerge rather than merge is to be tail-recursive.
     ******)
    let
        fun revMerge acc xs [] = revOnto acc xs
          | revMerge acc [] ys = revOnto acc ys
          | revMerge acc (xs as x::xs') (ys as y::ys') =
        if lt (y,x)
        then revMerge (y::acc) xs ys'
        else revMerge (x::acc) xs' ys
        fun sort [] = []
          | sort (sing as [_]) = sing
          | sort (pair as [x,y]) = if lt(y,x) then [y,x] else pair (*Optimisation*)
          | sort xs =
        let val (xs1,xs2) = chop xs (length xs div 2)
        in rev (revMerge [] (sort xs1) (sort xs2))
        end
    in sort
    end

    val sort = stableMergeSort

end