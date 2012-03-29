open Substitution

module SubstitutionList : SubstitutionSig with
    (* FIXME: Only for debugging purposes *)
    type ('a, 'b) t = ('a * 'b) list

