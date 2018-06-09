
structure Predefined : PredefinedSig =
struct

  fun [] @ ys = ys
    | (x::xs) @ ys = x :: (xs @ ys)

  fun ! (ref x) = x

  fun (f o g) x = f (g x)

  fun map f [] = []
    | map f (x::xs) = f x :: map f xs

  local fun revOnto acc [] = acc
          | revOnto acc (x::xs) = revOnto (x::acc) xs
  in val rev = revOnto []
  end

  val not = fn true => false | false => true

  fun (x <> y) = not (x = y)

  local fun lengthAcc acc [] = acc
          | lengthAcc acc (_::xs) = lengthAcc (1+acc) xs
  in val length = lengthAcc 0
  end

  fun app f [] = ()
    | app f (x::xs) = (f x; app f xs)

  fun concat xs =
      let fun loop acc [] = implode acc
            | loop acc (x::xs) = loop (explode x @ acc) xs
      in loop [] (rev xs)
      end

  fun foldl f acc [] = acc
    | foldl f acc (x::xs) = foldl f (f (x,acc)) xs

end
