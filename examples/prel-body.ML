
	fun echo s = print (s^"\n")
	fun echo' s = print (s : string)

	exception Abort of string
	fun abort s = (echo ("Abort: "^s); raise Abort s)
		  
	fun link [] = []
	  | link (xs::xss) = xs @ link xss

	(*fun linkMap f xs = link (map f xs)*)
	fun linkMap f [] = []
	  | linkMap f (x::xs) = f x @ linkMap f xs

	fun old_implode xs = implode (link (map explode xs))
	
	fun filter p [] = []
	  | filter p (x::xs) = if (p x) then x :: filter p xs
					else filter p xs

	fun fold f acc [] = acc
	  | fold f acc (x::xs) = fold f (f acc x) xs

	fun foldR f base [] = base
	  | foldR f base (x::xs) = f x (foldR f base xs)

	local
	fun stringOfPos 0 = ""
	  | stringOfPos n = let val low = n mod 10
				val high = n div 10
				in stringOfPos high ^ implode [chr (low + 48)]
				end
	in
	fun stringOfInt n =
		if n = 0 then "0" else
		if n < 0 then "~" ^ stringOfPos (~n)
			 else stringOfPos n
	end
			
	fun exists p [] = false
	  | exists p (x::xs) = p x orelse exists p xs

	fun forall p [] = true
	  | forall p (x::xs) = p x andalso forall p xs

	fun elem n [] = abort "elem:[]"
	  | elem n (x::xs) = if n=0 then x else elem (n-1) xs
					
	fun enumerateFrom n xs =
	let fun loop acc n [] = rev acc
		  | loop acc n (x::xs) = loop ((x,n)::acc) (1+n) xs
	in loop [] n xs
	end

	local fun copyAcc acc n x = if n<=0 then acc else copyAcc (x::acc) (n-1) x 
	in fun copy n x = if n < 0 then abort "copy:n<0" else copyAcc [] n x
	end
	
	fun spaces n = implode (copy n #" ")
	fun max a b = if (a > b) then a else b : int
	fun maximum n ns = fold max n ns
	fun upto (a,b) = if a>b then [] else a :: upto (a+1,b)
	fun curry f x y = f (x,y)

	fun zipWith who _ [] [] = []
	  | zipWith who _ [] _= abort (who^":zipWith:[],_")
	  | zipWith who _ _ []= abort (who^":zipWith:_,[]")
	  | zipWith who f (x::xs) (y::ys) = f (x,y) :: zipWith who f xs ys

	fun zipPair xs ys =
	case (xs,ys) of
		([],[]) => []
	  | (x::xs,y::ys) => (x,y) :: zipPair xs ys
	  | _ => abort "zipPair"

	local fun unzipAcc xs ys [] = (rev xs, rev ys)
		| unzipAcc xs ys ((x,y)::xys) =	 unzipAcc (x::xs) (y::ys) xys
	in (*val unzip = unzipAcc [] [] -- FAILS value restriction *)
	fun unzip xys = unzipAcc [] [] xys
	end
			   
  fun isPrintable c =
	  let val ordc = ord c
	  in ordc >= 32 andalso ordc <= 126
	  end
	  
  fun sofChar c =
	  if c = #"\t" then "\\t" else
	  if c = #"\n" then "\\n" else
	  if c = #"\"" then "\\\"" else
	  if c = #"\\" then "\\\\" else
	  if isPrintable c then implode[c] else
	  "\\" ^ let val ordc = ord c
		 in (if ordc < 10 then "00" else if ordc < 100 then "0" else "")
		 ^stringOfInt ordc
		 end

  fun sofStringLit string =
	  implode (#"\"" ::
		   fold (fn acc => fn c => explode (sofChar c) @ acc
			 ) [#"\""] (rev (explode string)))


  local	   
	  fun splitAcc yes no p [] = (rev yes, rev no)
		| splitAcc yes no p (x::xs) = if (p x) then splitAcc (x::yes) no p xs
									  else			splitAcc yes (x::no) p xs
  in
  fun split p = splitAcc [] [] p
  end


  fun readInt s = 
	  let fun loop acc [] = Some acc
			| loop acc (c::cs) = let val d = ord c - ord #"0"
								 in if (0<=d andalso d<=9) then loop (10*acc+d) cs
									else None
								 end
	  in loop 0 (explode s)
	  end

