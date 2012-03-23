(* Definiciones *)

module Subst Substitution

type variable_id = int
type predicate = { name: string; args: expression list }
        and term = Pred of predicate | Num of int
        and expression = 
            Term of term
          | Var of variable_id

let bind_if_possible f expr = match expr with
                             Var i -> (match f i with
                                                Some e -> e
                                              | None   -> expr)
                            | _    -> expr 

let rec interpolate f expr  = match expr with
                             Term Pred {name=n; args=a} -> Term (Pred {name = n;
                                       args = List.map (interpolate f) a})
                           | _          -> bind_if_possible f expr

type rule = { csq: predicate; cnd: predicate list }

type bindings = (variable_id,expression) Subst.t

type answer = bindings option 

type rule_base = rule list

(* Funciones *)

let arity p = List.length p.args


let add_binding = Subst.add interpolate
let subst_bindings = Subst.subst interpolate

let unify p1 p2 =
    let rec unify' bnd p1 p2 =
        let p1' = bind_if_possible (Subst.find bnd) p1 in
        let p2' = bind_if_possible (Subst.find bnd) p2 in
            match p1', p2' with
                Var i, (Term _ as e)         
              | (Term _ as e), Var i          -> Some (add_binding bnd i 
                                                (subst_bindings bnd e))
              | Var i as u, (Var j as v) -> (* Always substitute with the smaller
                                             variable *)
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


let pred n a = Term (Pred { name=n; args = a})
let lit n = pred n []


(* Examples *)

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















