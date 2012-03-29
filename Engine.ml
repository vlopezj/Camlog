(* Definiciones *)

(* Select a Substitution implementation. May change *)
module Subst = SubstitutionList.SubstitutionList

(* Tipos *)
type variable_id = int
type pred_name = Str of string
               | Num of int
               | Cons
               | Empty

type predicate = { name: pred_name; args: expression list }
        and expression = 
            Term of predicate
          | Var of variable_id



(* Funciones de expresión *)
let mkpred name args = {name = name; args = args}

let arity p = List.length p.args

let max a b = if a > b then a else b

let rec variable_span = function
            Var i   -> i
          | Term {args=a} -> variable_span_all a

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
                             Term {name=n; args=a} -> Term 
                                        (mkpred n (List.map (interpolate f) a))
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
              | Term {name=n;args=a1}, 
                Term {name=m;args=a2}, _, _ -> if n = m then 
                                                GO.fold_l2 unify' bnd a1 a2
                                              else
                                                O.fail ""
in
    O.ad (unify' bnd p1 p2)

type partial_proof = Partial of (int * bindings * predicate list)
type var_count_proof = Counted of (int * bindings)

let (>>=) = LazyList.(>>=)

let busca_reglas db var_count ?(bnd=Subst.identity) ?(off=0) pred =
    let it_reglas = LazyList.from_list db in
    LazyList.option_map (fun
        { csq=csq; cnd=cnd } ->
            (match unify ~bnd:bnd ~off1:off pred ~off2:var_count (Term csq) with
                Some bnd    ->    Some (Partial (var_count, bnd, cnd))
              | None        ->    None)) it_reglas


let rec prove db ?(var_count=0) ?(bnd=Subst.identity) ?(off=0) pred =
    let pred_span = off + (variable_span pred) in
    let var_count' = max var_count pred_span in
    let reglas = busca_reglas db var_count' ~bnd:bnd ~off:off pred in
    reglas >>= (fun (Partial (off, bnd, cnd)) -> 
        let rule_span = off + variable_span_all (List.map (fun x -> Term x) cnd) in
        LazyList.map
            (fun (Counted (vc,bnd))-> 
                    Counted (vc, Subst.filter (fun i -> (i <= pred_span)) bnd))
        (prove_all db ~var_count:(max var_count rule_span) ~bnd:bnd ~ploff:off cnd))                             

and prove_all db ?(var_count=0) ?(bnd=Subst.identity) ?(ploff=0) predl = 
    let rec prove_all' bnd var_count = function
            []  ->  LazyList.single (Counted (var_count, bnd))
          | x :: xs -> (prove db ~var_count:var_count ~bnd:bnd ~off:ploff (Term x)) 
                        >>= (fun (Counted (var_count, b)) -> prove_all' b var_count xs)
    in
        prove_all' bnd var_count predl

(* Todo: limpiar *)

(* Examples *)

(* Syntactic sugar *)
let upred n a = ({ name=(Str n); args = a})
let pred n a = Term (upred n a)
let ulit n = upred n []
let lit n = pred n []
let num n = Term {name=(Num n); args = []}
let pcons p q = Term {name=Cons; args = [p;q]}
let rec plist = function
    []      -> Term {name=Empty; args=[]}
  | p :: ps -> pcons p (plist ps)

let (<<-) csq cnd = {csq = csq; cnd = cnd}
let (|-) = pcons


let rdb_belongs = [upred "pertenece" [Var 1; (Var 1 |- Var 2)] <<- [];

                   upred "pertenece" [Var 1; (Var 2 |- Var 3)] <<- 
                       [upred "pertenece" [Var 1; Var 3]]]



let pred_horizontal = pred "recta" [pred "punto" [Var 1;Var 2];
                                    pred "punto" [Var 3;Var 2]]

let pred_vertical   = pred "recta" [pred "punto" [Var 1;Var 2];
                                    pred "punto" [Var 1;Var 3]]


