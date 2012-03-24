module type Monad =
  sig
    type 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
    val fail : string -> 'a t
    val ( >>| ) : 'a t -> 'b t -> 'b t
  end
module Generic :
  functor (M : Monad) ->
    sig
      val ( >>| ) : 'a M.t -> 'b M.t -> 'b M.t
      val map : ('a -> 'b) -> 'a M.t -> 'b M.t
      val collapse : 'a M.t M.t -> 'a M.t
      val fold_l : ('a -> 'b -> 'a M.t) -> 'a -> 'b list -> 'a M.t
      val fold_l2 :
        ('a -> 'b -> 'c -> 'a M.t) -> 'a -> 'b list -> 'c list -> 'a M.t
    end
