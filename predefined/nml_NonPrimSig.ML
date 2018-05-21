
signature NonPrimSig =
sig
    val @       : 'a list * 'a list -> 'a list
    val !       : 'a ref -> 'a
    val o       : ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
    val map     : ('a -> 'b) -> 'a list -> 'b list
    val rev     : 'a list -> 'a list
    val not     : bool -> bool
    val <>      : ''a * ''a -> bool (* must be eq types *)
    val length  : 'a list -> int
    val app     : ('a -> 'b) -> 'a list -> unit
    val concat  : string list -> string
    val foldl   : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
end
