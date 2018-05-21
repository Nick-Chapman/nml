(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  10.    imports
  42.    export
  89.    functor
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig Prel.ML *)
signature PrelSig_Import =
sig
    val stringOfInt     : int -> string
end;
(*IMPORTDONE:*)

(*IMPORTSIG: QLayoutSig QLAYOUT.ML *)
signature QLayoutSig_Import = sig
  type layout
  val Word          : string -> layout
  val Left          : string -> layout
  val Right         : string -> layout
  val Sep           : string -> layout
  val Blank         : layout
  val Then      : int -> layout -> layout -> layout
  val Glue      : layout -> layout -> layout
  val Above         : layout -> layout -> layout
  val AboveTab      : int -> layout -> layout -> layout
  val squash_to     : int -> layout -> layout
  val show_layout   : layout -> string
  val output_layout : layout -> unit
  val stream_layout : TextIO.outstream -> layout -> unit
  val prefixed_output_layout : string -> layout -> unit
  val prefixed_stream_layout : string -> TextIO.outstream -> layout -> unit
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature MisclaySig =
sig
  type layout

  val Word                      : string -> layout
  val Left                      : string -> layout
  val Right                     : string -> layout
  val Sep                       : string -> layout
  val Blank                     : layout
  val Then                      : int -> layout -> layout -> layout
  val Glue                      : layout -> layout -> layout
  val Above                     : layout -> layout -> layout
  val AboveTab                  : int -> layout -> layout -> layout
  val squash_to                 : int -> layout -> layout
  val show_layout               : layout -> string
  val output_layout             : layout -> unit
  val stream_layout             : TextIO.outstream -> layout -> unit
  val prefixed_output_layout    : string -> layout -> unit
  val prefixed_stream_layout    : string -> TextIO.outstream -> layout -> unit

  val //                        : layout * layout -> layout
  val @@                        : layout * layout -> layout
  val ++                        : layout * layout -> layout
  val ``                        : layout * layout -> layout
  val //``                      : layout * layout -> layout
  val layWord                   : word -> layout
  val layInt                    : int -> layout
  val layBracket                : layout -> layout
  val layBrace                  : layout -> layout
  val laySqBracket              : layout -> layout
  val layHashSqBracket          : layout -> layout
  val layListSep                : layout -> layout list -> layout
  val layCommaSep               : layout list -> layout
  val laySequence               : layout list -> layout
  val layVert                   : layout list -> layout
  val layVertSep                : layout -> layout list -> layout
  val layVertCommaSep           : layout list -> layout
  val layTuple                  : layout list -> layout
  val layPair                   : layout * layout -> layout
  val BlankLine                 : layout
  val see                       : layout -> unit
end;

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor MISCLAY(structure Prel : PrelSig_Import
        structure QLayout : QLayoutSig_Import
                    ) : MisclaySig =
struct

  infix // @@ ++ `` //``
  open Prel QLayout

  fun (lay1 // lay2)  = Above lay1 lay2
  fun (lay1 @@ lay2)  = Glue lay1 lay2
  fun (lay1 ++ lay2)  = Then 0 lay1 lay2
  fun (lay1 `` lay2)  = Then 2 lay1 lay2
  fun (lay1 //`` lay2)  = Above lay1 (Glue (Sep"  ") lay2)

  fun layWord w   = Word ("0wx"^Word.toString w)
  fun layInt int   = Word (stringOfInt int)
  fun layBracket x = Left "(" @@ x ++ Right ")"
  fun layBrace x   = Left "{" @@ x ++ Right "}"
  fun laySqBracket x = Left "[" @@ x ++ Right "]"
  fun layHashSqBracket x = Left "#[" @@ x ++ Right "]"

  fun layListSep sep [] = Blank
    | layListSep sep [x] = x
    | layListSep sep (x::xs) =
      let fun loop acc [] = acc (* never matches *)
            | loop acc [x] = acc ++ x
            | loop acc (x::xs) = loop (acc ++ (x @@ sep)) xs
      in loop (x @@ sep) xs
      end

  val layCommaSep = layListSep (Sep ",")

  fun laySequence [] = Blank
    | laySequence [x] = x
    | laySequence (x::xs) =
      let fun loop acc [] = acc (* never matches *)
            | loop acc [x] = acc ++ x
            | loop acc (x::xs) = loop (acc ++ x) xs
      in loop x xs
      end

  fun layVert [] = Blank
    | layVert [x] = x
    | layVert (x::xs) = Above x (layVert xs)

  fun layVertSep sep [] = Blank
    | layVertSep sep [x] = x
    | layVertSep sep  (x::xs) = Above (x @@ sep) (layVertSep sep xs)

  val layVertCommaSep = layVertSep (Sep ",")

  fun layTuple es = layBracket (layCommaSep es)

  fun layPair (a,b) = layTuple [a,b]

  val BlankLine = Sep ""

  fun see layout = output_layout (squash_to 78 (layVert [layout,BlankLine]))

end
