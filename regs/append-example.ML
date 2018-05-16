
let
    infix @;
    fun [] @ ys = ys
      | (x::xs) @ ys = x :: (xs @ ys)
    fun res() = [1,2,3] @ [4,5,6]
in
    EXPECT (fn()=> res() = [1,2,3,4,5,6])
end
