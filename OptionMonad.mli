module rec OptionMonad :
  sig
    type 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
    val fail : string -> 'a t
    val ( >>| ) : 'a t -> 'b t -> 'b t
    val ad : 'a t -> 'a option
    val ab : 'a option -> 'a t
  end
