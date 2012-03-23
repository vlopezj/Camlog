(* Interfaz genérica para módulos de sustitución *)

type ('a,'b) t = ('a * 'b) list
type ('a,'b) fmap = ('a -> 'b option) -> 'b -> 'b

val identity    : ('a,'b) t
val compose     : ('a,'b) fmap -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t

val apply       : ('a,'b) fmap -> ('a,'b) t -> 'b -> 'b
val find        : ('a,'b) t -> 'a -> 'b option
val make        : 'a -> 'b -> ('a,'b) t
val add         : ('a,'b) fmap -> ('a,'b) t -> 'a -> 'b -> ('a,'b) t

