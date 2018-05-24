
fun link_forall [] P = []
  | link_forall (x::xs) P = P x @ link_forall xs P

fun upto (a,b) = if a>b then [] else a :: upto (a+1,b)

fun exists p [] = false
  | exists p (x::xs) = p x orelse exists p xs

fun member ys x = exists (fn y => x=y) ys

exception Abort of string
fun abort s = raise (Abort s)

fun filter p [] = []
  | filter p (x::xs) = if (p x) then x :: filter p xs
                       else filter p xs

fun removeDups eq [] = []
  | removeDups eq (x::xs) = x :: removeDups eq (if (exists (eq x) xs)
                                                then filter (not o eq x) xs
                                                else xs)
(*
12 . . 15  ...  60 . . 63
 .     .
 .     .
 0 1 2 3   ...  48 . . 51
*)

datatype cell = CELL of int
fun eqCell (CELL a) (CELL b) = (a=b)
fun not0123 a = a<0 orelse a>3

fun mkCell (x,y,z) =
    if (not0123 x) then abort "mkCell:x" else
    if (not0123 y) then abort "mkCell:y" else
    if (not0123 z) then abort "mkCell:z" else
    CELL (x + 4*y + 16 * z)

val id          = fn (a,b,c) => (a,b,c)
val quarterXY   = fn (a,b,c) => (b,3-a,c)
val halfXY      = fn (a,b,c) => (3-a,3-b,c)
val quarterXZ   = fn (a,b,c) => (c,b,3-a)
val clock       = fn (a,b,c) => (b,c,a)
val anti        = fn (a,b,c) => (c,a,b)

val orientations =
    link_forall [id,quarterXY] (fn f1 =>
    link_forall [id,halfXY]    (fn f2 =>
    link_forall [id,quarterXZ] (fn f3 =>
    link_forall [id,clock,anti](fn f4 =>
    [f1 o f2 o f3 o f4]))))

fun shifts (X,Y,Z) =
    link_forall (upto (0,X-1)) (fn x =>
    link_forall (upto (0,Y-1)) (fn y =>
    link_forall (upto (0,Z-1)) (fn z =>
    [fn (a,b,c) => (a+x,b+y,c+z)])))

datatype piece = PIECE of string * cell list list

fun eqCellList xs ys =
    (length xs = length ys) andalso not (exists (not o member ys) xs)

fun mkPiece (name,XYZ,offsets) =
    PIECE (name, removeDups eqCellList
           (link_forall orientations (fn F =>
           link_forall (shifts XYZ) (fn G =>
           [map (mkCell o F o G) offsets]))))

fun thePieces() =
    map mkPiece
    [
    (* red pieces *)
     ("A",(2,3,3),[(0,0,0),(1,0,0),(2,0,0),(0,1,0),(0,0,1)]), (* base-girder *)
     ("B",(2,3,3),[(0,0,0),(1,0,0),(1,1,0),(2,1,0),(2,1,1)]), (* spiral-staircase *)
     ("C",(2,3,3),[(0,0,0),(1,0,0),(1,1,0),(2,1,0),(1,1,1)]), (* fork *)
     ("D",(2,2,4),[(1,0,0),(1,1,0),(1,2,0),(0,1,0),(2,1,0)]), (* flat-cross *)
     (* blue *)
     ("e",(2,3,3),[(0,0,0),(1,0,0),(2,0,0),(1,1,0),(1,1,1)]), (* modern-art *)
     ("f",(2,3,3),[(0,0,0),(1,0,0),(2,0,0),(1,1,0),(0,0,1)]), (* bent-f-piece *)
     ("g",(2,3,3),[(0,0,0),(1,0,0),(1,1,0),(1,1,1),(2,1,1)]), (* s-piece *)
     ("h",(2,2,4),[(1,0,0),(0,1,0),(1,1,0),(2,1,0),(0,2,0)]), (* flat-tree *)
     (* yellow *)
     ("v",(2,3,3),[(0,0,0),(1,0,0),(2,0,0),(2,0,1),(2,1,1)]), (* L-sign-post *)
     ("w",(2,3,3),[(0,0,0),(1,0,0),(2,0,0),(2,1,0),(0,0,1)]), (* hug *)
     ("x",(2,3,3),[(0,0,0),(1,0,0),(2,0,0),(1,1,0),(1,0,1)]), (* middle-girder *)
     ("y",(2,2,4),[(0,0,0),(1,0,0),(1,1,0),(2,1,0),(2,2,0)]), (* simple-stairs *)
     ("z",(3,3,3),[(0,0,0),(1,0,0),(1,1,0),(1,1,1)]) (* small-one *)
     ]

datatype ('a,'c) nd = ND of (unit -> 'c) -> ((unit -> 'c) -> 'a -> 'c) -> 'c
fun deND (ND x) = x
fun resultND x = ND (fn f => fn s => s f x)
fun bindND (ND m,F) = ND (fn f => fn s => m f (fn f => fn x => deND (F x) f s))
val failND = ND (fn f => fn s => f ())
fun altND (ND m1, F) = ND (fn f => fn s => m1 (fn () => deND (F()) f s) s)
fun execND (ND m1) = m1 (fn () => []) (fn f => fn x => [x])

fun try_forall [] F = failND
  | try_forall (x::xs) F = altND (F x, fn () => try_forall xs F)


datatype cube = CUBE of (string*cell) list
val emptyCube = CUBE []

fun isFilled (CUBE xs) cell = exists (fn (_,cell') => eqCell cell cell') xs

val effort = ref 0

fun tick() =
    let val n = 1 + !effort
        (*val h = n div 100*)
    in (if n mod 100 = 0 then print ("."(*^stringOfInt h*)) else ());
    effort := n
    end

fun placePiece (CUBE xs) name cells =
    (tick();
    CUBE (map (fn cell => (name,cell)) cells @ xs))

fun lookCube (CUBE xs) cell =
    let fun look [] = "."
          | look ((name,cell')::xs) =
            if (eqCell cell cell') then name else look xs
    in look xs
    end

fun stringOfCube cube =
    "\n(effort="^stringOfInt(!effort)^")\n"^
    concat (link_forall [3,2,1,0] (fn y =>
    link_forall [0,1,2,3] (fn z =>
    ["   "^ concat (link_forall [0,1,2,3] (fn x =>
    [lookCube cube (mkCell (x,y,z))]))]) @ ["\n"]))

fun find_placements thePieces cube cells placedNames =
    (
    (*print ("find_placements : " ^ concat placedNames^"\n");*)
    case cells
     of [] => resultND cube
      | cellToFill::cells =>
        if (isFilled cube cellToFill)
        then find_placements thePieces cube cells placedNames else
        try_forall thePieces (fn PIECE (name,placements) =>
        if (member placedNames name) then failND else
        try_forall placements (fn cells_covered =>
        if not (exists (eqCell cellToFill) cells_covered) then failND else
        if (exists (isFilled cube) cells_covered) then failND else
        find_placements thePieces (placePiece cube name cells_covered) cells (name::placedNames)))
)

val show = (map (print o stringOfCube) o execND)

fun show num nd =
    let val n = ref num
    in
        deND nd (fn () => print "stop\n")
             (fn f => fn x =>
                         (print (stringOfCube x) ;
                          n := !n - 1;
                          (if !n = 0 then () else f ())
                          ))
    end

fun go num pieces cells =
    show num (find_placements pieces emptyCube cells [])

fun run() =
    let
        val num = 1
        val () = print "generating pieces\n"
        val pieces = thePieces()
    in print "searching\n";
        go num pieces (map CELL (upto (0,63)))
    end

(*
(effort=6025)
   eeev   Dvvv   wwzz   fggz
   DeBv   DeCh   Dwgg   ffgz
   ACBB   DCCh   fwCh   fwxh
   AAAB   AyyB   yyxh   yxxx
...
*)
