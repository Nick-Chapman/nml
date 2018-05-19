
structure Test = 
struct

structure Inner =
struct
datatype outstream = C1 | C2
open TextIO (* makes outstream be a tcon which maps to an empty valenv *)
end

(* NJ complains about the following data-rep, but if it were excepted, it should
not introduce the constructors C1 and C2 *)
datatype rep = datatype Inner.outstream

fun res() = case (Inner.C2,999) 
	   of (C1,999) => 1  (* hence C1 has status V and should match *)
	    | (C2,_) => 2

val X = EXPECT(f() => res() = 1)

end;
val res = Test.res