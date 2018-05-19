
signature LazySig =
    sig
	type 'a lazy
	val lazy : 'a -> 'a lazy
	val delay : (unit -> 'a) -> 'a lazy
	val force :'a lazy -> 'a
    end;


structure Lazy : LazySig =
    struct
	datatype 'a lazyVal = Thunk of unit -> 'a | Value of 'a
	datatype 'a lazy = Lazy of 'a lazyVal ref
	fun lazy v = Lazy (ref (Value v))
	fun delay thunk = Lazy (ref (Thunk thunk))
	fun force (Lazy r) =
	    case (!r) of
		Thunk thunk => let val v = thunk()
			       in r := Value v; v
			       end
	      | Value v => v
    end;

open Lazy
datatype 'a llist = LNil | LCons of 'a * 'a llist lazy


fun takeLL n ll =
    if n <= 0 then []
	      else case ll of
		       LNil => []
		     | LCons (x,ll) => x :: takeLL (n-1) (force ll)

    
val rec iterateU : (unit -> 'a option) -> 'a llist
    = fn f =>
    case f() of
	None => LNil
      | Some x => LCons (x, delay (fn () => iterateU f))

val rec iterate : ('a -> ('b * 'a) option) -> 'a -> 'b llist
    = fn f => fn a =>
    case f a of
	None => LNil
      | Some (b,a) => LCons (b, delay (fn () => iterate f a))



fun min a b = if a < b then a else b

val lazyString : string -> string llist
    = fn s =>
    let val CHUNK = 5
	val z = size s
	val r = ref 0
    in iterateU (fn () =>
		let val n = !r
		in if n > z then None else
		    (r := n+CHUNK; Some (substring(s,n,min CHUNK (z-n))))
		end)
    end

val FILE_CHUNK = 1000
fun lazyFile {path} =
    let open TextIO
	val instream = openIn path
	fun get() =
	    let val s : string = inputN (instream,FILE_CHUNK)
	    in if s = "" then (closeIn instream; LNil)
			 else LCons (s, delay get)
	    end
    in
	get()
    end

val rec appLL : ('a -> unit) -> 'a llist -> unit
    = fn f => fn ll =>
    case ll of
	LNil => ()
      | LCons (x,ll) => (f x; appLL f (force ll))
