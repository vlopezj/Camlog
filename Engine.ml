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

(* Deferred predicate renaming *)
type lazy_pred = LazyPred of (int * predicate)
type lazy_pred_list = LazyPredList of (int * predicate list)

let lazy_of_pred p = LazyPred (0, p);;
let expr_of_lazy (LazyPred (off, p)) = apply_offset off (Term p)

let lazy_pred_span (LazyPred (off, p)) = off + (variable_span (Term p))
let lazy_pred_list_span (LazyPredList (off, pl)) = List.fold_left (fun acc p ->
        max acc (lazy_pred_span (LazyPred (off, p)))) 0 pl

let list_of_lazy_pred_list (LazyPredList(off, pl)) = List.map (fun p ->
    LazyPred(off, p)) pl

(* Unification ~ the core of the engine *)
let unify ?(bnd=Subst.identity) lp1 lp2 =
    let rec unify' bnd e1 e2 =
        (* Si son variables, las sustituimos por sus valores ya en la lista *)
        let e1' = bind_if_possible (Subst.find bnd) e1 in
        let e2' = bind_if_possible (Subst.find bnd) e2 in
            match e1', e2' with
                Var i, (Term _ as e)
              | (Term _ as e), Var i        -> Some (add_binding bnd i
                                            (apply_bindings bnd e))
              | Var i as u, (Var j as v)    ->  if i < j then
                                                    Some (add_binding bnd j u)
                                                else if i > j then
                                                    Some (add_binding bnd i v)
                                                else
                                                    Some bnd
              | Term {name=n;args=a1}, 
                Term {name=m;args=a2}    -> if n = m then 
                                                GO.fold_l2 unify' bnd a1 a2
                                              else
                                                None
in
    unify' bnd (expr_of_lazy lp1) (expr_of_lazy lp2)

(* Proof tree generation *)
type var_count_proof = Counted of (int * bindings)
type partial_proof = Partial of (var_count_proof * lazy_pred_list)

let (>>=) = LazyList.(>>=)

let busca_reglas db var_count ?(bnd=Subst.identity) lp =
    let it_reglas = LazyList.from_list db in
    LazyList.option_map (fun
        { csq=csq; cnd=cnd } ->
            (* Rename variables *)
            let lcsq = LazyPred (var_count, csq) in
            let lcnd = LazyPredList (var_count, cnd) in
            (match unify ~bnd:bnd lp lcsq with
                Some bnd    ->    Some (Partial ((Counted (max (lazy_pred_span lcsq)
                                                        (lazy_pred_list_span
                                                        lcnd),
                                        bnd)), lcnd))
              | None        ->    None)) 
        it_reglas

let clean_bindings span bnd = Subst.filter (fun i -> (i <= span)) bnd

let clean_counted span (Counted (vc, bnd)) = (Counted (vc, clean_bindings span
bnd))

let rec prove db ?(cb=Counted (0, Subst.identity)) lp =
    let pred_span = lazy_pred_span lp in
    let Counted(var_count, bnd) = cb in
    let var_count = max var_count pred_span in
        let reglas = busca_reglas db var_count ~bnd:bnd lp in
        reglas >>= (fun (Partial (cb, lpl)) -> 
            LazyList.map (clean_counted pred_span)
                (prove_all db ~cb:cb lpl))                             

and prove_all db ?(cb=Counted (0, Subst.identity)) lpl = 
    let rec prove_all' cb = function
            []  ->  LazyList.single cb
          | lp :: lps -> (prove db ~cb:cb lp) 
                        >>= (fun cb -> prove_all' cb lps)
    in
        prove_all' cb (list_of_lazy_pred_list lpl)


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


