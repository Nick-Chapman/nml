
signature PrimSig =
sig

    val =       : 'a * 'a -> bool

    val ~       : int -> int
    val +       : int * int -> int
    val -       : int * int -> int
    val *       : int * int -> int
    val mod     : int * int -> int
    val div     : int * int -> int

    val <       : int * int -> bool
    val <=      : int * int -> bool
    val >       : int * int -> bool
    val >=      : int * int -> bool

    val chr     : int -> char
    val ord     : char -> int

    val implode : char list -> string
    val explode : string -> char list
    val ^       : string * string -> string
    val print   : string -> unit

end
