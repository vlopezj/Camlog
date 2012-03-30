(* Generic interface for composable substitutions *)
module type OrderedType = Map.OrderedType

module type Interpolable = sig
    type 'a t

    val sure_interpolate : ('a -> 'b t) -> 'a t -> 'b t
    val interpolate :   ('a -> 'a t option) -> 'a t -> 'a t
end

module type SubstitutionSig = sig
    type key
    type value

    type t

    val identity : t 
    val singleton : key -> value -> t

    val compose : t -> t -> t

    val apply : t -> value -> value
    val find : t -> key -> value option
    val add : t -> key -> value -> t
    val filter : (key -> bool) -> t -> t
end


