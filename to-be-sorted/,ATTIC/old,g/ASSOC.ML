

signature AssocSig = sig type ('a,'b) assoc
                         val assoc'empty : ('a,'b) assoc
                         val assoc'single : (('a * 'b) -> ('a,'b) assoc)
                         val assoc'list : (('a * 'b) list -> ('a,'b) assoc)
                         val assoc'plus : (('a,'b) assoc ->
                                           (('a,'b) assoc -> ('a,'b) assoc))
                         val extend'assoc : (('a,'b) assoc ->
                                             (('a * 'b) -> ('a,'b) assoc))
                         val delete'assoc : ((''a,'b) assoc ->
                                             (''a -> (''a,'b) assoc))
                         val map'assoc : ((('a * 'b) -> ('c * 'd)) ->
                                          (('a,'b) assoc -> ('c,'d) assoc))
                         val list'assoc : (('a,'b) assoc -> ('a * 'b) list)
                         val lookup'assoc : ((''a,'b) assoc ->
                                             (''a -> 'b option))
                         val look'assoc : (string -> ((''a -> string) ->
                                                      ((''a,'b) assoc ->
                                                       (''a -> 'b))))
                     end
signature PrelSig_Import = sig val abort : (string -> 'a)
                               val filter : (('a -> bool) -> ('a list -> 'a list
                                                             ))
                           end
functor ASSOC (structure Prel : PrelSig_Import) =
        struct
        open Prel
        abstype ('a,'b) assoc = ASSOC of ('a * 'b) list
        with val assoc'empty = ASSOC []
             fun assoc'single x = ASSOC [x]
             fun assoc'list xs = ASSOC xs
             fun assoc'plus (ASSOC xs1) (ASSOC xs2) =
               ASSOC (@ {1 = xs2,2 = xs1})
             fun extend'assoc (ASSOC xs) x = ASSOC (op :: {1 = x,2 = xs})
             fun delete'assoc (ASSOC xs) k =
               ASSOC ((filter (fn (k',_) => op <> {1 = k,2 = k'})) xs)
             fun map'assoc f (ASSOC xs) = ASSOC ((map f) xs)
             fun list'assoc (ASSOC xs) = xs
             fun lookup'assoc (ASSOC xs) k =
               let fun look [] = None
                     | look (op :: {1 = (k',v),2 = xs}) =
                     if op = {1 = k,2 = k'} then Some v else look xs
               in look xs
               end
        end
        fun look'assoc who sofk a k =
          case (lookup'assoc a) k of
            Some x => x
          | None => (abort (op ^ {1 = op ^ {1 = who,2 = ":"},2 = sofk k}))
        end : AssocSig
;

