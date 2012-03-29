open Substitution

module SubstitutionList : sig
    include SubstitutionSig
    val ad : ('a * 'b) list -> ('a, 'b) t 
    val ab : ('a, 'b) t -> ('a * 'b) list
end with
    type ('a, 'b) t = ('a * 'b) list

