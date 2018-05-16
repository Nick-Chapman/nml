
fun intOfString s = case (Prel.readInt s) of Some i => i | None => Prel.abort "intOfString"

fun printInt i = print (Prel.stringOfInt i^"\n")

fun run_life init max =
	let open Main
		fun loop x i = if i>max then () 
					   else (printInt i; Main.show print x; loop (Main.nthgen x 1) (i+1))
	in loop init 0
	end

fun life_top [arg] = run_life Main.gun (intOfString arg)
  | life_top _ = Prel.abort "life_top"
