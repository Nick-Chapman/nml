

fun nqueens n =
  let open Word
      val z = 0wx1
      val a = op - {1 = << (z,fromInt n),2 = z}
      fun b (c,d,e,f,g) =
        if op = {1 = e,2 = a}
        then op :: {1 = f,2 = g}
        else let fun h (x,g) =
                   if op = {1 = andb (op + {1 = x,2 = z},a),2 = 0wx0
                           } then g
                   else let val y = andb (op + {1 = x,2 = z},notb x)
                        in h (op + {1 = x,2 = y},
                              b (<< (op + {1 = c,2 = y},z),
                                 op >> (op + {1 = d,2 = y},z),
                                 op + {1 = e,2 = y},
                                 op :: {1 = Vector.sub
                                            (#[0,1,2,27,3,24,28,33,4,
                                               17,25,31,29,12,34,14,
                                               5,8,18,36,26,23,32,16,
                                               30,11,13,7,35,22,15,
                                               10,6,21,9,20,19],
                                             toInt (op mod {1 = y,2 = 0wx25})),
                                        2 = f},g))
                        end
             in h (orb (orb (c,d),e),g)
             end
  in b (0wx0,0wx0,0wx0,[],[])
  end
;

