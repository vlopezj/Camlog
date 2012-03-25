module rec OptionMonad : sig 
    include Monad.Monad
    val ad    : ('a t) -> 'a option
    val ab    : 'a option -> ('a t)
end = struct
    type 'a t = 'a option

    let (>>=) ma f = match ma with
                        Some a  ->  f a
                      | None    ->  None
    
    let return a = Some a
    let fail _ = None

    let ab x = x
    let ad x = x

    include Monad.Generic (OptionMonad)
end
