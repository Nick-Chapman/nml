(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  10.    export
  31.    import
  43.    functor
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature AssocSig =
sig
    type ('a,'b) assoc
    val assoc'empty     : ('a,'b) assoc
    val assoc'single    : 'a * 'b -> ('a,'b) assoc
    val assoc'list      : ('a * 'b) list -> ('a,'b) assoc
    val assoc'plus      : ('a,'b) assoc -> ('a,'b) assoc -> ('a,'b) assoc
    val extend'assoc    : ('a,'b) assoc -> 'a * 'b -> ('a,'b) assoc
    val delete'assoc    : (''a,'b) assoc -> ''a -> (''a,'b) assoc
    val map'assoc       : ('a * 'b -> 'c * 'd) -> ('a,'b) assoc -> ('c,'d) assoc
    val list'assoc      : ('a,'b) assoc -> ('a * 'b) list
    val find'assoc      : ('a,'b) assoc -> ('a -> bool) -> 'b option
    val lookup'assoc    : (''a,'b) assoc -> ''a -> 'b option
    val look'assoc      : string -> (''a -> string) -> (''a,'b) assoc -> ''a -> 'b
end;

(*----------------------------------------------------------------------
INDEX: import
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig Prel.ML *)
signature PrelSig_Import =
sig
    val abort           : string -> 'a
    val filter          : ('a -> bool) -> 'a list -> 'a list
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor ASSOC(structure Prel : PrelSig_Import) : AssocSig =
struct

    open Prel

    abstype ('a,'b) assoc = ASSOC of ('a * 'b) list
    with
    val assoc'empty = ASSOC []
    fun assoc'single x = ASSOC [x]
    fun assoc'list xs = ASSOC xs
    (* assoc'plus -- arg2 overides arg1.  Takes time proportional to arg2. *)
    fun assoc'plus (ASSOC xs1) (ASSOC xs2) = ASSOC (xs2 @ xs1)
    fun extend'assoc (ASSOC xs) x = ASSOC (x::xs)
    fun delete'assoc (ASSOC xs) k = ASSOC (filter (fn (k',_) => k<>k') xs)
    fun map'assoc f (ASSOC xs) = ASSOC (map f xs)
    fun list'assoc (ASSOC xs) = xs

    fun find'assoc (ASSOC xs) eq_k =
        let fun look [] = None
          | look ((k',v)::xs) = if (eq_k k') then Some v else look xs
        in look xs
        end
    end
(*
    fun lookup'assoc (ASSOC xs) k =
        let fun look [] = None
          | look ((k',v)::xs) = if (k=k') then Some v else look xs
        in look xs
        end
    end
*)
    fun lookup'assoc ass k = find'assoc ass (fn k' => k=k')


    (* look'assoc -- lookup, aborting if the key does not exist *)
    fun look'assoc who sofk a k =
    case (lookup'assoc a k) of
        Some x => x
      | None => abort (who^":"^sofk k)

end
