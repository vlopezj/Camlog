module Make (K : Substitution.OrderedType) (V : Substitution.Interpolable) :
    Substitution.SubstitutionSig 
    with type key = K.t
    with type value = K.t V.t
    (* FIXME: Only for debugging purposes *)
    with type t = (K.t * (K.t V.t)) list

