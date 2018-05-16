

structure XXX = struct
                fun echo s = print (op ^ {1 = s,2 = "\n"})
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
                fun twice f x = f (f x)
                fun thrice f x = f (f (f x))
                fun subFrom a b = op - {1 = b,2 = a}
                val dec = subFrom 1
                val result = ((thrice thrice) dec) 100
                val xxx = echo (op ^ {1 = "result=",2 = stringOfInt result})
                end
;

