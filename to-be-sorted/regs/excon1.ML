
(* example of compile time constructed excon1s - was missing from exported C support *)

exception Foo of int
fun deFoo x = (raise x) handle Foo n => n
val a = Foo 1
val b = Foo 2
;EXPECT (fn () => (deFoo a = 1) andalso (deFoo b = 2))
