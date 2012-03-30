(* Módulo de sustitución *)
module Make (K : Substitution.OrderedType) (V : Substitution.Interpolable) : 
    Substitution.SubstitutionSig 
    with type key = K.t
    with type value = K.t V.t
    (* FIXME: Only for debugging purposes *)
    with type t = (K.t * (K.t V.t)) list
= struct
    type key = K.t
    type value = K.t V.t
    type t = (key * value) list

    let identity : t = []
    let singleton i v = [(i,v)]

    let find b i = try Some (snd (List.find 
                                (fun (j,_) -> (i = j)) b))
                   with
                        Not_found -> None


    let apply b e = V.interpolate (find b) e

    let compose b1 b2 = List.append b1 (List.map 
                                (fun (j,e) -> (j, apply b1 e))
                                b2)

    let add b i v = compose (singleton i v) b

    let filter f b = List.filter (fun (i,_) -> f i) b
end

