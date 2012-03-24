module type Monad = sig 
    type 'a t

    val (>>=) : 'a t -> ('a -> 'b t)  -> 'b t
    val return : 'a -> 'a t
    val fail : string -> 'a t

    (* Self-derivable *)
    val (>>|) : 'a t -> 'b t -> 'b t
end

module Generic (M : Monad) : sig
    val (>>|) : 'a M.t -> 'b M.t -> 'b M.t 
    val map   : ('a -> 'b) -> 'a M.t -> 'b M.t
    val collapse : ('a M.t) M.t -> 'a M.t
    val fold_l : ('b -> 'a -> 'b M.t) -> 'b -> 'a list -> 'b M.t
    val fold_l2 : ('b -> 'a -> 'c -> 'b M.t) -> 'b -> 'a list -> 'c list -> 'b M.t
end = struct
    let (>>=) = M.(>>=)

    let (>>|) ma mb = ma >>= (fun _ -> mb)

    let map f ma = ma >>= (fun a -> M.return (f a))
    let collapse mma = mma >>= (fun ma -> ma)
    let rec fold_l f i = function
                        []      -> M.return i
                      | x :: xs -> f i x >>= (fun y -> fold_l f y xs)
    let rec fold_l2 f i l1 l2 = try fold_l (fun x (a,b) -> f x a b) i 
                                        (List.combine l1 l2)
                                with Invalid_argument s -> M.fail s
end



    
