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
    val ad : ('a * 'b) list -> ('a, 'b) SubstitutionList.SubstitutionList.t
    val ab : ('a, 'b) SubstitutionList.SubstitutionList.t -> ('a * 'b) list
  end
type variable_id = int
type predicate = { name : string; args : expression list; }
and term = Pred of predicate | Num of int
and expression = Term of term | Var of variable_id
type rule = { csq : predicate; cnd : predicate list; }
type bindings = (variable_id, expression) Subst.t
type answer = bindings option
type rule_base = rule list
val bind_if_possible :
  (variable_id -> expression option) -> expression -> expression
val interpolate :
  (variable_id -> expression option) -> expression -> expression
val arity : predicate -> int
val add_binding :
  (variable_id, expression) Subst.t ->
  variable_id -> expression -> (variable_id, expression) Subst.t
val apply_bindings :
  (variable_id, expression) Subst.t -> expression -> expression
val offset_variable : int -> expression -> expression
module O :
  sig
    type 'a t = 'a OptionMonad.OptionMonad.t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
    val fail : string -> 'a t
    val ( >>| ) : 'a t -> 'b t -> 'b t
    val access : 'a t -> 'a option
    val make : 'a option -> 'a t
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
  ?off1:int ->
  expression ->
  ?off2:int -> expression -> (variable_id, expression) Subst.t option
val upred : string -> expression list -> term
val pred : string -> expression list -> expression
val ulit : string -> term
val lit : string -> expression
val num : int -> expression
val rdb_belongs : rule list
val pred_horizontal : expression
val pred_vertical : expression
