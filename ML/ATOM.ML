(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  12.    export
  164.   imports
  175.   functor
  276.   non-prim builders
  292.   module language
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature AtomSig =
sig

    datatype id = ID of string

    datatype vid = VID of id
    datatype tyvar = TYVAR of id
    datatype tycon = TYCON of id
    datatype strid = STRID of id
    datatype sigid = SIGID of id
    datatype funid = FUNID of id

    datatype 'a long = LONG of strid list * 'a

    datatype lab = LAB'Id of id | LAB'Num of int

    datatype scon =
        SCON'Num of int
      | SCON'Word of word
      | SCON'Real of real (* not parsed *)
      | SCON'Char of char
      | SCON'String of string

    val long'x : 'a -> 'a long

    val sof'vid : vid -> string
    val sof'tycon : tycon -> string
    val sof'strid : strid -> string
    val sof'sigid : sigid -> string
    val sof'funid : funid -> string
    val sof'lab : lab -> string

    val sof'longvid : vid long -> string

    val eq'vid : vid -> vid -> bool
    val eq'lab : lab -> lab -> bool

end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ../prelude/Prel.ML *)
signature PrelSig_Import =
sig
    val link            : 'a list list -> 'a list
    val stringOfInt     : int -> string
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor ATOM(structure Prel : PrelSig_Import) : AtomSig =
struct

    open Prel;

    datatype id = ID of string

    datatype vid = VID of id
    datatype tyvar = TYVAR of id
    datatype tycon = TYCON of id
    datatype strid = STRID of id
    datatype sigid = SIGID of id
    datatype funid = FUNID of id

    datatype 'a long = LONG of strid list * 'a

    datatype lab = LAB'Id of id | LAB'Num of int

    datatype scon =
        SCON'Num of int
      | SCON'Word of word
      | SCON'Real of real (* not parsed *)
      | SCON'Char of char
      | SCON'String of string

(*----------------------------------------------------------------------
INDEX: non-prim builders
----------------------------------------------------------------------*)

    fun long'x x = LONG ([],x)

(*----------------------------------------------------------------------
INDEX: sof/eq
----------------------------------------------------------------------*)

  fun sof'vid (VID (ID s)) = s
  fun sof'tycon (TYCON (ID s)) = s
  fun sof'strid (STRID (ID s)) = s
  fun sof'sigid (SIGID (ID s)) = s
  fun sof'funid (FUNID (ID s)) = s

  fun sof'lab lab =
      "#" ^ (case lab of
                 LAB'Id (ID string) => string
               | LAB'Num n => stringOfInt n)

  fun sof'longvid (LONG(xs,vid)) =
      implode (link (map (fn x => explode (sof'strid x^".")) xs)) ^  sof'vid vid

  fun eq'vid (x1:vid) x2 = (x1=x2)
  fun eq'lab (x1:lab) x2 = (x1=x2)

end;
