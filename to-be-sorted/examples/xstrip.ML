
(* Strip a string of bracketed sections - write code using strange exception style *)
(* multiple nested handlers are set up - and dynamically generated exceptions are used *)

exception UnmatchedRightBracket

fun xstrip _ [] = []
  | xstrip js (c::cs) = case c of
							#"(" => let exception j of char list
									in c :: (xstrip (j::js) cs handle j v => v)
									end
						  | #")" => (case js of [] => raise UnmatchedRightBracket
											  | j::js => raise j (c :: xstrip js cs))
						  | _ => c :: xstrip js cs

fun xstrip_top [s] = print (implode (xstrip [] (explode s))^"\n")

