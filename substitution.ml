module type InterpolableSig = sig
    type t
    type key

    val interpolate : key -> t -> t
end


module Substitution (K : Set.OrderedType, I : InterpolableSig) : SubstitutionSig  = struct
    type s = (K.t * I(K).t) list

    let identity = []
    let find b i = try Some (snd (List.find (fun (j,_) -> (K.compare i j = 0)) b)) with
                        Not_found -> None
    let subst b p = List.fold_left (fun (p',(i,v)) -> I.interpolate p' i v) p b
    let compose b1 b2 = List.append b1 (List.map (fun (j,e) -> (j, subst b1 e))
    b2)

    let make i v = [(i,v)]
    let add b i v = compose (make i v) b
end 
