module OptionMonad : Monad.Monad with
    type 'a t = 'a option
