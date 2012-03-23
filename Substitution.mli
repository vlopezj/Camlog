(* Interfaz genérica para módulos de sustitución *)

type ('a,'b) t
type ('a,'b) fmap = ('a -> 'b option) -> 'b -> 'b

val identity    : ('a,'b) t
val find        : ('a,'b) t -> 'a -> 'b option
val compose     : ('a,'b) fmap -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
val make        : 'a -> 'b -> ('a,'b) t
val add         : ('a,'b) fmap -> ('a,'b) t -> 'a -> 'b -> ('a,'b) t

