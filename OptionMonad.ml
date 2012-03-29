module rec OptionMonad : Monad.Monad with type 'a t = 'a option
= struct
    type 'a t = 'a option

    let (>>=) ma f = match ma with
                        Some a  ->  f a
                      | None    ->  None
    
    let return a = Some a
    let fail _ = None

    include Monad.Generic (OptionMonad)
end
