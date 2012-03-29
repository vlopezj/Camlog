module Subst :
  sig
    type ('a, 'b) t = ('a * 'b) list
    type ('a, 'b) fmap = ('a -> 'b option) -> 'b -> 'b
    val identity : ('a, 'b) t
    val compose : ('a, 'b) fmap -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
    val apply : ('a, 'b) fmap -> ('a, 'b) t -> 'b -> 'b
    val find : ('a, 'b) t -> 'a -> 'b option
    val make : 'a -> 'b -> ('a, 'b) t
    val add : ('a, 'b) fmap -> ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
    val filter : ('a -> bool) -> ('a, 'b) t -> ('a, 'b) t
  end
type variable_id = int
type pred_name = Str of string | Num of int | Cons | Empty
type predicate = { name : pred_name; args : expression list; }
and expression = Term of predicate | Var of variable_id
val mkpred : pred_name -> expression list -> predicate
val arity : predicate -> int
val max : 'a -> 'a -> 'a
val variable_span : expression -> variable_id
val variable_span_all : expression list -> variable_id
type rule = { csq : predicate; cnd : predicate list; }
type bindings = (variable_id, expression) Subst.t
type answer = bindings option
type rule_base = { user : rule list; }
val bind_if_possible :
  (variable_id -> expression option) -> expression -> expression
val interpolate :
  (variable_id -> expression option) -> expression -> expression
val add_binding :
  (variable_id, expression) Subst.t ->
  variable_id -> expression -> (variable_id, expression) Subst.t
val apply_bindings :
  (variable_id, expression) Subst.t -> expression -> expression
val apply_offset : int -> expression -> expression
val offset_variable : int -> expression -> expression
module O :
  sig
    type 'a t = 'a option
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
    val fail : string -> 'a t
    val ( >>| ) : 'a t -> 'b t -> 'b t
  end
module GO :
  sig
    val ( >>| ) : 'a O.t -> 'b O.t -> 'b O.t
    val map : ('a -> 'b) -> 'a O.t -> 'b O.t
    val collapse : 'a O.t O.t -> 'a O.t
    val fold_l : ('a -> 'b -> 'a O.t) -> 'a -> 'b list -> 'a O.t
    val fold_l2 :
      ('a -> 'b -> 'c -> 'a O.t) -> 'a -> 'b list -> 'c list -> 'a O.t
  end
type lazy_pred = LazyPred of (int * predicate)
type lazy_pred_list = LazyPredList of (int * predicate list)
val lazy_of_pred : predicate -> lazy_pred
val expr_of_lazy : lazy_pred -> expression
val lazy_pred_span : lazy_pred -> int
val lazy_pred_list_span : lazy_pred_list -> int
val list_of_lazy_pred_list : lazy_pred_list -> lazy_pred list
val unify :
  ?bnd:(variable_id, expression) Subst.t ->
  lazy_pred -> lazy_pred -> (variable_id, expression) Subst.t O.t
type var_count_proof = Counted of (int * bindings)
type partial_proof = Partial of (var_count_proof * lazy_pred_list)
val ( >>= ) : 'a LazyList.t -> ('a -> 'b LazyList.t) -> 'b LazyList.t
val busca_reglas :
  rule list ->
  int ->
  ?bnd:(variable_id, expression) Subst.t ->
  lazy_pred -> partial_proof LazyList.t
val clean_bindings : 'a -> ('a, 'b) Subst.t -> ('a, 'b) Subst.t
val clean_counted : variable_id -> var_count_proof -> var_count_proof
val prove :
  rule list -> ?cb:var_count_proof -> lazy_pred -> var_count_proof LazyList.t
val prove_all :
  rule list ->
  ?cb:var_count_proof -> lazy_pred_list -> var_count_proof LazyList.t
val upred : string -> expression list -> predicate
val pred : string -> expression list -> expression
val ulit : string -> predicate
val lit : string -> expression
val num : int -> expression
val pcons : expression -> expression -> expression
val plist : expression list -> expression
val ( <<- ) : predicate -> predicate list -> rule
val ( |- ) : expression -> expression -> expression
val rdb_belongs : rule list
val pred_horizontal : expression
val pred_vertical : expression
