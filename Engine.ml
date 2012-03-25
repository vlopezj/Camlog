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

(* Funciones de expresión *)
let mkpred name args = {name = name; args = args}

let arity p = List.length p.args

let max a b = if a > b then a else b

let rec variable_span = function
            Var i   -> i
          | Term Pred {args=a} -> variable_span_all a
          | _       -> 0
and variable_span_all l = List.fold_left (fun acc e -> max acc (variable_span
                            e)) 0 l


(* Reglas *)
type rule = { csq: predicate; cnd: predicate list }
type bindings = (variable_id,expression) Subst.t
type answer = bindings option 

type rule_base = {user: rule list}


(* Manejo de bindings *)
let bind_if_possible f expr = match expr with
                             Var i -> (match f i with
                                                Some e -> e
                                              | None   -> expr)
                            | _    -> expr 

let rec interpolate f expr  = match expr with
                             Term Pred {name=n; args=a} -> Term (Pred 
                                        (mkpred n (List.map (interpolate f) a)))
                            | _          -> bind_if_possible f expr


(* Funciones *)

(* Para predicados *)

let add_binding = Subst.add interpolate

let apply_bindings = Subst.apply interpolate

let apply_offset off expr = interpolate (fun i -> Some (Var (i + off))) expr


let offset_variable o = function
        Var i   -> Var (i + o)
      | e       -> e


(* El motor utiliza renombramiento y sustitución retardados *)

module O = OptionMonad.OptionMonad
module GO = Monad.Generic (O)

let unify ?(bnd=Subst.identity) ?(off1=0) p1 ?(off2=0) p2 =
    let rec unify' bnd p1 p2 =
        (* Si son variables, las sustituimos por sus valores ya en la lista *)
        let p1' = bind_if_possible (Subst.find bnd) (offset_variable off1 p1) in
        let p2' = bind_if_possible (Subst.find bnd) (offset_variable off2 p2) in
            match p1', p2', off1, off2 with
                Var i, (Term _ as e), _, off
              | (Term _ as e), Var i, off, _  -> O.return (add_binding bnd i 
                                                 (apply_bindings bnd
                                                 (apply_offset off e)))
              | Var i as u, (Var j as v), _, _ -> 
                                          if i < j then
                                                O.return (add_binding bnd j u)
                                          else if i > j then
                                                O.return (add_binding bnd i v)
                                          else
                                                O.return bnd
              | Term Pred {name=n;args=a1}, 
                Term Pred {name=m;args=a2}, _, _ -> if n = m then 
                                                GO.fold_l2 unify' bnd a1 a2
                                              else
                                                O.fail ""
              | Term k, Term l, _, _      -> if k = l then O.return bnd else
                                                O.fail ""
in
    O.ad (unify' bnd p1 p2)

type rule_result = (int * bindings * (predicate list))

let (>>=) = LazyList.>>=

let busca_reglas db ?(off=0) ?(bnd=Subst.identity) pred =
    let it_reglas = LazyList.from_list db in
    let rule_off = off + (variable_span pred) in
    LazyList.option_map (fun
        { csq=csq; cnd=cnd } ->
            (match unify ~bnd=bnd ~off1:off pred ~off2:rule_off (Term (Pred csq)) with
                Some bnd    ->    Some (rule_off, bnd, cnd)
              | None        ->    None)) it_reglas

let rec prove db ?(bnd=Subst.identity) ?(off=0) pred =
    let reglas = busca_reglas db off pred in
    reglas >>= (fun (roff, bnd, cnd) -> prove_all db ~bnd:bnd ~roff:roff cnd) 
and prove_all db ?(bnd=Subst.identity) ?(off=0) predl = 
    let poff = roff + (variable_span_all predl) in
        let prove_all' bnd roff = function
            []  ->  LazyList.Single bnd
            x :: xs -> (prove db ~bnd:bnd ~off:poff x) >>= (fun b ->
                            prove_all' b 
    in
        prove_all' bnd predl

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


