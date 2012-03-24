(* Definiciones *)

(* Select a Substitution implementation. May change *)
module Subst = SubstitutionList.SubstitutionList

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

let offset_variable o = function
        Var i   -> Var (i + o)
      | e       -> e

module O = OptionMonad.OptionMonad
module GO = Monad.Generic (O)

let unify ?(off1=0) p1 ?(off2=0) p2 =
    let rec unify' bnd p1 p2 =
        (* Si son variables, las sustituimos por sus valores ya en la lista *)
        let p1' = bind_if_possible (Subst.find bnd) (offset_variable off1 p1) in
        let p2' = bind_if_possible (Subst.find bnd) (offset_variable off2 p2) in
            match p1', p2' with
                Var i, (Term _ as e)         
              | (Term _ as e), Var i          -> O.return (add_binding bnd i 
                                                (apply_bindings bnd e))
              | Var i as u, (Var j as v) -> 
                                          if i < j then
                                                O.return (add_binding bnd j u)
                                          else if i > j then
                                                O.return (add_binding bnd i v)
                                          else
                                                O.return bnd
              | Term Pred {name=n;args=a1}, 
                Term Pred {name=m;args=a2} -> if n = m then 
                                                GO.fold_l2 unify' bnd a1 a2
                                              else
                                                O.fail ""
              | Term k, Term l             -> if k = l then O.return bnd else
                                                O.fail ""
in
    O.access (unify' Subst.identity p1 p2)


(* Examples *)

(* Syntactic sugar *)
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


