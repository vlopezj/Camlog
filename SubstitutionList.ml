open Substitution

(* Módulo de sustitución *)
module rec SubstitutionList : SubstitutionSig with
    type ('a,'b) t = ('a * 'b) list 
= struct
    type ('a,'b) t = ('a * 'b) list
    type ('a,'b) fmap = ('a -> 'b option) -> 'b -> 'b

    let identity : ('a,'b) t = []
    let find b i = try Some (snd (List.find 
                                (fun (j,_) -> (i = j)) b))
                   with
                        Not_found -> None


    let apply (ip : ('a,'b) fmap) b e = ip (find b) e

    let compose ip b1 b2 = List.append b1 (List.map 
                                (fun (j,e) -> (j, apply ip b1 e))
                                b2)

    let make i v = [(i,v)]
    let add ip b i v = compose ip (make i v) b

    let filter f b = List.filter (fun (i,_) -> f i) b
end

