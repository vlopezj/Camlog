(* Definiciones *)

(* FIXME: Make implementation private *)

(* Select a Substitution implementation. Should change to somenthing
 * more efficient (Tree, or even an impure HashMap) *)
module Subst = SubstitutionList.SubstitutionList

(* Tipos *)
type variable_id = int
type special_pred = Is | Cut | BinaryNum of (int -> int -> bool)
type pred_name = PredName of string
               | Num of int
               | Cons
               | Empty
               | Special of special_pred

type 'a gen_predicate = { name: pred_name; args: 'a gen_expression list }
        and 'a gen_expression = 
            Term of 'a gen_predicate
          | Var of 'a

type 'a gen_bindings = ('a, 'a gen_expression) Subst.t

(* Inside the engine, variables are encoded as integers *)
type predicate = variable_id gen_predicate
type expression = variable_id gen_expression
type bindings = variable_id gen_bindings

(* Expression manipulation functions *)
let mkpred name args = {name = name; args = args}
let arity p = List.length p.args


let rec variable_span = function
            Var i   -> i
          | Term {args=a} -> variable_span_all a

and variable_span_all l = List.fold_left (fun acc e -> max acc (variable_span
                            e)) 0 l


(* Rules *)
type rule = { csq: predicate; cnd: predicate list }
type rule_base = {user: rule list}

(* Manejo de bindings *)
let bind_if_possible f expr = match expr with
                             Var i -> (match f i with
                                                Some e -> e
                                              | None   -> expr)
                            | _    -> expr 

let rec sure_interpolate f = function
        Term {name=n; args=a} -> 
            Term (mkpred n (List.map (sure_interpolate f) a))
          | Var i  -> f i


let interpolate f = sure_interpolate (fun i -> match f i with
                                                     Some v -> v
                                                   | None  -> Var i)

let add_binding = Subst.add interpolate
let apply_bindings = Subst.apply interpolate

let apply_offset off expr = interpolate (fun i -> Some (Var (i + off))) expr


(* Funciones *)
(* El motor utiliza renombramiento y sustitución retardados *)

module O = OptionMonad.OptionMonad
module GO = Monad.Generic (O)

(* Deferred predicate renaming *)
type lazy_pred = LazyPred of (int * predicate)
type lazy_pred_list = LazyPredList of (int * predicate list)

let lazy_of_pred p = LazyPred (0, p);;
let expr_of_lazy (LazyPred (off, p)) = apply_offset off (Term p)

(* Knowing the maximum variable present inside an expression
 * is necessary for variable allocation *)
let lazy_pred_span (LazyPred (off, p)) = off + (variable_span (Term p))
let lazy_pred_list_span (LazyPredList (off, pl)) = List.fold_left (fun acc p ->
        max acc (lazy_pred_span (LazyPred (off, p)))) 0 pl

let list_of_lazy_pred_list (LazyPredList(off, pl)) = List.map (fun p ->
    LazyPred(off, p)) pl

(* Unification ~ the core of the engine
 * The central idea is generating a series of simple substitutions
 * (var --> expr), and composing them to generate a Most General
 * Unifier. 
 * *)
let unify ?(bnd=Subst.identity) lp1 lp2 =
    let rec unify' bnd e1 e2 =
        (* We replace variables with the values they already have *)
        let e1' = bind_if_possible (Subst.find bnd) e1 in
        let e2' = bind_if_possible (Subst.find bnd) e2 in
            match e1', e2' with
                Var i, (Term _ as e)
              | (Term _ as e), Var i        -> Some (add_binding bnd i
                           (* We won't descend any further, so we apply all
                            * possible bindings *)
                                                    (apply_bindings bnd e))
              (* We try to substitute large variables with small variables *)
              | Var i as u, (Var j as v)    ->  if i < j then
                                                    Some (add_binding bnd j u)
                                                else if i > j then
                                                    Some (add_binding bnd i v)
                                                else
                                                    Some bnd
              | Term {name=n;args=a1}, 
                Term {name=m;args=a2}    -> (* Built-in equality should be
                                             * enough *)
                                            if n = m then 
                                                GO.fold_l2 unify' bnd a1 a2
                                            else
                                                None
in
    (* We obtain a list of bindings which, when applied simultaneously
     * to both predicates, should yield the same result *)
    unify' bnd (expr_of_lazy lp1) (expr_of_lazy lp2)

(*
 * Proof tree generation 
 *)

(* Stores a proof context -- the largest variable yet allocated, and
 * the current bindings *)
type var_count_proof = Counted of (int * bindings)

(* Stores a proof context and a list of necessary clauses to prove
 * for the context to be valid *)
type partial_proof = Partial of (var_count_proof * lazy_pred_list)

let (>>=) = LazyList.(>>=)

let busca_reglas db var_count ?(bnd=Subst.identity) lp =
    (* TODO: Make more efficient... maybe filter by name and arity *)
    let it_reglas = LazyList.from_list db in
    LazyList.option_map (fun
        { csq=csq; cnd=cnd } ->
            (* Rename variables *)
            let lcsq = LazyPred (var_count, csq) in
            let lcnd = LazyPredList (var_count, cnd) in
            (* Try to unify with consequent *)
            (match unify ~bnd:bnd lp lcsq with
                Some bnd    ->    Some (Partial (
                                    (* Allocate space for variables in rule *)
                                    (Counted (max (lazy_pred_span lcsq)
                                                  (lazy_pred_list_span lcnd),
                                        bnd)), lcnd))
              | None        ->    None)) 
        it_reglas

(* Removes unnecessary bindings *)
let clean_bindings span bnd = Subst.filter (fun i -> (i <= span)) bnd
let clean_counted span (Counted (vc, bnd)) = (Counted (vc, clean_bindings span
bnd))

let rec prove db ?(cb=Counted (0, Subst.identity)) lp =
    (* Allocate space for variables in predicate *)
    let Counted(var_count, bnd) = cb in
    let pred_span = lazy_pred_span lp in
    let var_count = max var_count pred_span in
        let reglas = busca_reglas db var_count ~bnd:bnd lp in
        (* Try each rule non-determistically *)
        reglas >>= (fun (Partial (cb, lpl)) -> 
            LazyList.map (clean_counted pred_span)
                (prove_all db ~cb:cb lpl))                             

and prove_all db ?(cb=Counted (0, Subst.identity)) lpl = 
    (* Fold-left over the conditions *)
    let rec prove_all' cb = function
            []  ->  LazyList.single cb
          | lp :: lps -> (prove db ~cb:cb lp) 
                        >>= (fun cb -> prove_all' cb lps)
    in
        prove_all' cb (list_of_lazy_pred_list lpl)


(* User *)
(* From the user point of view, variables are strings or anonymous *)
type u_variable_id = Anonymous | VarName of string
type user_predicate = u_variable_id gen_predicate
type user_expression = u_variable_id gen_expression

module IntMap = Map.Make(struct
    type t = int
    let compare = compare
end)

module StringMap = Map.Make(struct
    type t = string
    let compare = compare
end)

(* TODO: Already pure. Make functional? *)
let expression_from_user ?(tbls=(ref IntMap.empty, ref
        StringMap.empty)) ue =
    let var_count = ref 0 in
    let (table, rev_table) = tbls in
    let allocate = (fun var -> var_count := !var_count + 1;
                               table := IntMap.add !var_count var !table;
                               Var !var_count)
    in
    let new_expr = sure_interpolate (function 
                      Anonymous -> allocate Anonymous
                    | VarName str as uvar -> try StringMap.find str !rev_table with
                        Not_found -> let v = allocate uvar in
                            rev_table := StringMap.add str v !rev_table;
                                         v) ue in
        (new_expr, !table)

let pred_wrapper f p = match f (Term p) with
    Term p' -> p'
  | _       -> assert false

let pred_wrapper2 f p = match f (Term p) with
    (Term p',x) -> (p',x)
  | _           -> assert false


let predicate_from_user ?(tbls=(ref IntMap.empty, ref StringMap.empty))
    = pred_wrapper2 (expression_from_user ~tbls:tbls)

(* TODO:
    Finish improving data input and output mechanisms
    Make test bank
    Cuts
    Evaluation
   *)

(* Examples *)

(* Syntactic sugar *)
let upred n a = ({ name=(PredName n); args = a})
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


