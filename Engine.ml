(* Definiciones *)

module Subst = Substitution

(* Tipos *)
type variable_id = int
type predicate = { name: string; args: expression list }
        and term = Pred of predicate | Num of int
        and expression = 
            Term of term
          | Var of variable_id

type rule = { csq: predicate; cnd: predicate list }
type bindings = (variable_id,expression) Subst.t
type answer = bindings option 
type rule_base = rule list

(* Manejo de bindings *)
let bind_if_possible f expr = match expr with
                             Var i -> (match f i with
                                                Some e -> e
                                              | None   -> expr)
                            | _    -> expr 

let rec interpolate f expr  = match expr with
                             Term Pred {name=n; args=a} -> Term (Pred {name = n;
                                       args = List.map (interpolate f) a})
                           | _          -> bind_if_possible f expr


(* Funciones *)

let arity p = List.length p.args


let add_binding = Subst.add interpolate
let apply_bindings = Subst.apply interpolate

let unify p1 p2 =
    let rec unify' bnd p1 p2 =
        (* Si son variables, las sustituimos por sus valores ya en la lista *)
        let p1' = bind_if_possible (Subst.find bnd) p1 in
        let p2' = bind_if_possible (Subst.find bnd) p2 in
            match p1', p2' with
                Var i, (Term _ as e)         
              | (Term _ as e), Var i          -> Some (add_binding bnd i 
                                                (apply_bindings bnd e))
              | Var i as u, (Var j as v) -> 
                                          if i < j then
                                                Some (add_binding bnd j u)
                                          else if i > j then
                                                Some (add_binding bnd i v)
                                          else
                                                Some bnd
              | Term Pred {name=n;args=a1}, 
                Term Pred {name=m;args=a2} -> if n = m then 
                                                unify_mult bnd a1 a2
                                              else
                                                None
              | Term Num k, Term Num l  -> if k = l then Some bnd else None
              | _,_                     -> None
         and unify_mult bnd a b = match a, b with
                                [],[] -> Some bnd
                              | x :: xs, y :: ys -> (match unify' bnd x y with
                                    Some bnd' -> unify_mult bnd' xs ys
                                  | None      -> None)
                              | _    -> None
in
    unify' Subst.identity p1 p2



(* Examples *)

(* Algunos atajos *)
let upred n a = (Pred { name=n; args = a})
let pred n a = Term (upred n a)
let ulit n = upred n []
let lit n = pred n []
let num n = Term (Num n)


let rdb_belongs = [{csq = { name = "pertenece"; 
                            args = [Var 1; 
                                Term (Pred {name = "|"; 
                                           args = [Var 1; Var 3]})]};
                    cnd = []};
                   {csq = { name = "pertenece";
                            args = [Var 1;
                               Term (Pred {name = "|";
                                          args = [Var 2; Var 3]})]};
                    cnd = [{ name = "pertenece";
                             args = [Var 1; Var 3] }]}]



let pred_horizontal = pred "recta" [pred "punto" [Var 1;Var 2];
                                    pred "punto" [Var 3;Var 2]]

let pred_vertical   = pred "recta" [pred "punto" [Var 1;Var 2];
                                    pred "punto" [Var 1;Var 3]]


