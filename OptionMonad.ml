module rec OptionMonad : sig 
    include Monad.Monad
    val access  : ('a t) -> 'a option
    val make    : 'a option -> ('a t)
end = struct
    type 'a t = 'a option

    let (>>=) ma f = match ma with
                        Some a  ->  f a
                      | None    ->  None
    
    let return a = Some a
    let fail _ = None

    let access x = x
    let make x = x

    include Monad.Generic (OptionMonad)
end
