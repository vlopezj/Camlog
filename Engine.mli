type variable_id = int
type special_pred = Is | Cut | BinaryNum of (int -> int -> bool)
type pred_name =
    PredName of string
  | Num of int
  | Cons
  | Empty
  | Special of special_pred
type 'a gen_predicate = { name : pred_name; args : 'a gen_expression list; }
and 'a gen_expression = Term of 'a gen_predicate | Var of 'a
type predicate = variable_id gen_predicate
type expression = variable_id gen_expression
val pred_wrapper :
  ('a gen_expression -> 'b gen_expression) ->
  'a gen_predicate -> 'b gen_predicate
val pred_wrapper2 :
  ('a gen_expression -> 'b gen_expression * 'c) ->
  'a gen_predicate -> 'b gen_predicate * 'c
val mkpred : pred_name -> 'a gen_expression list -> 'a gen_predicate
val arity : 'a gen_predicate -> int
module ExpressionInterpolable :
  sig
    type 'a t = 'a gen_expression
    val sure_interpolate : ('a -> 'b t) -> 'a t -> 'b t
    val interpolate : ('a -> 'a t option) -> 'a t -> 'a t
  end
module SubstId :
  sig
    type key = variable_id
    type value = variable_id ExpressionInterpolable.t
    type t = (variable_id * variable_id ExpressionInterpolable.t) list
    val identity : t
    val singleton : key -> value -> t
    val compose : t -> t -> t
    val apply : t -> value -> value
    val find : t -> key -> value option
    val add : t -> key -> value -> t
    val filter : (key -> bool) -> t -> t
  end
module EI :
  sig
    type 'a t = 'a gen_expression
    val sure_interpolate : ('a -> 'b t) -> 'a t -> 'b t
    val interpolate : ('a -> 'a t option) -> 'a t -> 'a t
  end
type bindings = SubstId.t
val variable_span : int gen_expression -> int
val variable_span_all : int gen_expression list -> int
type 'a rule = { csq : 'a gen_predicate; cnd : 'a gen_predicate list; }
type rule_base = { user : variable_id rule list; }
val bind_if_possible :
  ('a -> 'a gen_expression option) -> 'a gen_expression -> 'a gen_expression
val add_binding : SubstId.t -> SubstId.key -> SubstId.value -> SubstId.t
val apply_bindings : SubstId.t -> SubstId.value -> SubstId.value
val apply_offset : int -> int EI.t -> int EI.t
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
val expr_of_lazy : lazy_pred -> variable_id EI.t
val lazy_pred_span : lazy_pred -> int
val lazy_pred_list_span : lazy_pred_list -> int
val list_of_lazy_pred_list : lazy_pred_list -> lazy_pred list
val unify : ?bnd:SubstId.t -> lazy_pred -> lazy_pred -> SubstId.t O.t
type var_count_proof = Counted of (int * bindings)
type partial_proof = Partial of (var_count_proof * lazy_pred_list)
val ( >>= ) : 'a LazyList.t -> ('a -> 'b LazyList.t) -> 'b LazyList.t
val busca_reglas :
  variable_id rule list ->
  int -> ?bnd:SubstId.t -> lazy_pred -> partial_proof LazyList.t
val clean_bindings : SubstId.key -> SubstId.t -> SubstId.t
val clean_counted : SubstId.key -> var_count_proof -> var_count_proof
val prove :
  variable_id rule list ->
  ?cb:var_count_proof -> lazy_pred -> var_count_proof LazyList.t
val prove_all :
  variable_id rule list ->
  ?cb:var_count_proof -> lazy_pred_list -> var_count_proof LazyList.t
type u_variable_id = Anonymous | VarName of string
type user_predicate = u_variable_id gen_predicate
type user_expression = u_variable_id gen_expression
module IntMap :
  sig
    type key = int
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
module StringMap :
  sig
    type key = string
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
val expression_from_user :
  ?tbls:u_variable_id IntMap.t ref * IntMap.key EI.t StringMap.t ref ->
  u_variable_id EI.t -> IntMap.key EI.t * u_variable_id IntMap.t
val predicate_from_user :
  ?tbls:u_variable_id IntMap.t ref * IntMap.key EI.t StringMap.t ref ->
  u_variable_id gen_predicate ->
  IntMap.key gen_predicate * u_variable_id IntMap.t
val upred : string -> 'a gen_expression list -> 'a gen_predicate
val pred : string -> 'a gen_expression list -> 'a gen_expression
val ulit : string -> 'a gen_predicate
val lit : string -> 'a gen_expression
val num : int -> 'a gen_expression
val pcons : 'a gen_expression -> 'a gen_expression -> 'a gen_expression
val plist : 'a gen_expression list -> 'a gen_expression
val ( <<- ) : 'a gen_predicate -> 'a gen_predicate list -> 'a rule
val ( |- ) : 'a gen_expression -> 'a gen_expression -> 'a gen_expression
val rdb_belongs : int rule list
val rdb_path : int rule list
val pred_horizontal : int gen_expression
val pred_vertical : int gen_expression
