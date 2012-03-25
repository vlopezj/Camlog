type 'a t = Empty | Cons of ('a * (('a t) Lazy.t))

val empty : 'a t
val cons : 'a -> 'a t -> 'a t
val single : 'a -> 'a t

val from_list : 'a list -> 'a t
val to_list   : 'a t -> 'a list

val map : ('a -> 'b) -> 'a t -> 'b t
val filter : ('a -> bool) -> 'a t -> 'a t
val option_map : ('a -> 'b option) -> 'a t -> 'b t

val (>>=)    : 'a t -> ('a -> 'b t) -> 'b t


val concat  : ('a t) t -> 'a t
val append  : ('a t) -> ('a t) -> ('a t)

