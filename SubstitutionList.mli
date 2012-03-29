open Substitution

module SubstitutionList : SubstitutionSig with
    type ('a, 'b) t = ('a * 'b) list

