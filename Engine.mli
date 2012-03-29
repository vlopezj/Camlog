module Subst :
  sig
    type ('a, 'b) t = ('a, 'b) SubstitutionList.SubstitutionList.t
    type ('a, 'b) fmap = ('a -> 'b option) -> 'b -> 'b
    val identity : ('a, 'b) t
    val compose : ('a, 'b) fmap -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
    val apply : ('a, 'b) fmap -> ('a, 'b) t -> 'b -> 'b
    val find : ('a, 'b) t -> 'a -> 'b option
    val make : 'a -> 'b -> ('a, 'b) t
    val add : ('a, 'b) fmap -> ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
    val filter : ('a -> bool) -> ('a, 'b) t -> ('a, 'b) t
    val ad : ('a * 'b) list -> ('a, 'b) t
    val ab : ('a, 'b) t -> ('a * 'b) list
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
    type 'a t = 'a OptionMonad.OptionMonad.t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
    val fail : string -> 'a t
    val ( >>| ) : 'a t -> 'b t -> 'b t
    val ad : 'a t -> 'a option
    val ab : 'a option -> 'a t
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
val unify :
  ?bnd:(variable_id, expression) Subst.t ->
  ?off1:int ->
  expression ->
  ?off2:int -> expression -> (variable_id, expression) Subst.t option
type partial_proof = Partial of (int * bindings * predicate list)
type var_count_proof = Counted of (int * bindings)
val ( >>= ) : 'a LazyList.t -> ('a -> 'b LazyList.t) -> 'b LazyList.t
val busca_reglas :
  rule list ->
  int ->
  ?bnd:(variable_id, expression) Subst.t ->
  ?off:int -> expression -> partial_proof LazyList.t
val prove :
  rule list ->
  int ->
  ?bnd:(variable_id, expression) Subst.t ->
  ?off:int -> expression -> var_count_proof LazyList.t
val prove_all :
  rule list ->
  int ->
  ?bnd:bindings -> ?ploff:int -> predicate list -> var_count_proof LazyList.t
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
