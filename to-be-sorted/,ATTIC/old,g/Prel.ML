

signature PrelSig = sig exception Abort of string
                        val echo : (string -> unit)
                        val echo' : (string -> unit)
                        val abort : (string -> 'a)
                        val link : ('a list list -> 'a list)
                        val linkMap : (('a -> 'b list) -> ('a list -> 'b list))
                        val old_implode : (string list -> string)
                        val filter : (('a -> bool) -> ('a list -> 'a list))
                        val fold : (('a -> ('b -> 'a)) ->
                                    ('a -> ('b list -> 'a)))
                        val foldR : (('a -> ('b -> 'b)) ->
                                     ('b -> ('a list -> 'b)))
                        val stringOfInt : (int -> string)
                        val exists : (('a -> bool) -> ('a list -> bool))
                        val forall : (('a -> bool) -> ('a list -> bool))
                        val elem : (int -> ('a list -> 'a))
                        val enumerateFrom : (int -> ('a list -> ('a * int) list)
                                            )
                        val copy : (int -> ('a -> 'a list))
                        val spaces : (int -> string)
                        val max : (int -> (int -> int))
                        val maximum : (int -> (int list -> int))
                        val upto : ((int * int) -> int list)
                        val curry : ((('a * 'b) -> 'c) -> ('a -> ('b -> 'c)))
                        val zipWith : (string -> ((('a * 'b) -> 'c) ->
                                                  ('a list ->
                                                   ('b list -> 'c list))))
                        val zipPair : ('a list -> ('b list -> ('a * 'b) list))
                        val unzip : (('a * 'b) list -> ('a list * 'b list))
                        val sofChar : (char -> string)
                        val sofStringLit : (string -> string)
                        val split : (('a -> bool) ->
                                     ('a list -> ('a list * 'a list)))
                    end
structure
Prel =
struct
fun echo s = print (op ^ {1 = s,2 = "\n"})
fun echo' s = print (s : string)
exception Abort of string
fun abort s = (echo (op ^ {1 = "Abort: ",2 = s}) ;raise Abort s
              )
fun link [] = []
  | link (op :: {1 = xs,2 = xss}) = @ {1 = xs,2 = link xss}
fun linkMap f [] = []
  | linkMap f (op :: {1 = x,2 = xs}) =
  @ {1 = f x,2 = (linkMap f) xs}
fun old_implode xs = implode (link ((map explode) xs))
fun filter p [] = []
  | filter p (op :: {1 = x,2 = xs}) =
  if p x then op :: {1 = x,2 = (filter p) xs}
  else (filter p) xs
fun fold f acc [] = acc
  | fold f acc (op :: {1 = x,2 = xs}) =
  ((fold f) ((f acc) x)) xs
fun foldR f base [] = base
  | foldR f base (op :: {1 = x,2 = xs}) =
  (f x) (((foldR f) base) xs)
local fun stringOfPos 0 = ""
        | stringOfPos n =
        let val low = op mod {1 = n,2 = 10}
            val high = op div {1 = n,2 = 10}
        in op ^ {1 = stringOfPos high,
                 2 = implode [chr (op + {1 = low,2 = 48})]}
        end
in fun stringOfInt n =
     if op = {1 = n,2 = 0} then "0"
     else if op < {1 = n,2 = 0}
          then op ^ {1 = "~",2 = stringOfPos (~ n)}
          else stringOfPos n
end
fun exists p [] = false
  | exists p (op :: {1 = x,2 = xs}) =
  (p x) orelse ((exists p) xs)
fun forall p [] = true
  | forall p (op :: {1 = x,2 = xs}) =
  (p x) andalso ((forall p) xs)
fun elem n [] = abort "elem:[]"
  | elem n (op :: {1 = x,2 = xs}) =
  if op = {1 = n,2 = 0} then x
  else (elem (op - {1 = n,2 = 1})) xs
fun enumerateFrom n xs =
  let fun loop acc n [] = rev acc
        | loop acc n (op :: {1 = x,2 = xs}) =
        ((loop (op :: {1 = (x,n),2 = acc}))
         (op + {1 = 1,2 = n})) xs
  in ((loop []) n) xs
  end
local fun copyAcc acc n x =
        if op <= {1 = n,2 = 0} then acc
        else ((copyAcc (op :: {1 = x,2 = acc}))
              (op - {1 = n,2 = 1})) x
in fun copy n x = if op < {1 = n,2 = 0} then abort "copy:n<0"
                  else ((copyAcc []) n) x
end
fun spaces n = implode ((copy n)  )
fun max a b = if op > {1 = a,2 = b} then a else b : int
fun maximum n ns = ((fold max) n) ns
fun upto (a,b) =
  if op > {1 = a,2 = b} then []
  else op :: {1 = a,2 = upto (op + {1 = a,2 = 1},b)}
fun curry f x y = f (x,y)
fun zipWith who _ [] [] = []
  | zipWith who _ [] _ =
  abort (op ^ {1 = who,2 = ":zipWith:[],_"})
  | zipWith who _ _ [] =
  abort (op ^ {1 = who,2 = ":zipWith:_,[]"})
  | zipWith who f (op :: {1 = x,2 = xs}) (op :: {1 = y,2 = ys})
= op :: {1 = f (x,y),2 = (((zipWith who) f) xs) ys}
fun zipPair xs ys = case (xs,ys) of
                      ([],[]) => []
                    | (op :: {1 = x,2 = xs},
                       op :: {1 = y,2 = ys}) =>
                        (op :: {1 = (x,y),2 = (zipPair xs) ys})
                    | _ => (abort "zipPair")
local fun unzipAcc xs ys [] = (rev xs,rev ys)
        | unzipAcc xs ys (op :: {1 = (x,y),2 = xys}) =
        ((unzipAcc (op :: {1 = x,2 = xs}))
         (op :: {1 = y,2 = ys})) xys
in fun unzip xys = ((unzipAcc []) []) xys
end
fun isPrintable c = let val ordc = ord c
                    in (op >= {1 = ordc,2 = 32}) andalso
                       (op <= {1 = ordc,2 = 126})
                    end
fun sofChar c =
  if op = {1 = c,2 = \t} then "\\t"
  else if op = {1 = c,2 = \n} then "\\n"
       else if op = {1 = c,2 = \"} then "\\\""
            else if op = {1 = c,2 = \\}
                 then "\\\\"
                 else if isPrintable c
                      then implode [c]
                      else op ^ {1 = "\\",
                                 2 = let val ordc = ord c
                                     in op ^ {1 = if op < {1 = ordc,
                                                           2 = 10}
                                                  then "00"
                                                  else if op < {1 = ordc,
                                                                2 = 100}
                                                       then "0" else "",
                                              2 = stringOfInt ordc}
                                     end}
fun sofStringLit string =
  implode
  (op :: {1 = \",2 = ((fold (fn acc => fn c => @ {1 = explode (sofChar c),
                                                  2 = acc})) [\"])
                     (rev (explode string))})
local fun splitAcc yes no p [] = (rev yes,rev no)
        | splitAcc yes no p (op :: {1 = x,2 = xs}) =
        if p x then (((splitAcc (op :: {1 = x,2 = yes})) no) p) xs
        else (((splitAcc yes) (op :: {1 = x,2 = no})) p) xs
in fun split p = ((splitAcc []) []) p
end
end : PrelSig
;

