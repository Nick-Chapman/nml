

functor FFF () = struct
                 val xxx = 42
                 end
structure sss = FFF (struct
                     end)
structure sss2 = FFF (struct
                      end)
;

