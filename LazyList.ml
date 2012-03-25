type 'a t = Empty | Cons of ('a * (('a t) Lazy.t))

let empty = Empty
let cons x l = Cons (x, (Lazy.lazy_from_val l))
let single x = cons x empty

let rec from_list = function
    x :: xs -> Cons (x, (lazy (from_list xs)))
  | []      -> Empty

let rec to_list = function
    Cons (x, it) -> x :: (to_list (Lazy.force it))
  | Empty        -> []

let rec map f = function
    Cons (x, it) -> Cons ((f x), (lazy (map f (Lazy.force it))))
  | Empty        -> Empty

let rec concat = function
    Cons (it, itt)    -> (match it with
            Cons (x, it') -> Cons (x, lazy (concat 
                                (Cons (Lazy.force it', itt))))
          | Empty         -> concat (Lazy.force itt))
  | Empty             -> Empty


let (>>=) ma f = concat (map f ma)

let append x y = concat (from_list [x;y])

let rec filter f it = it >>= (fun x -> if (f x) then (single x) else empty)

let rec option_map f it = it >>= (fun x -> match (f x) with 
                                    Some x -> (single x)
                                  | None -> empty)





