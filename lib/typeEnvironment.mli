(* 'scope is a phantom type *)

type 'scope type_name = private {type_name : string} [@@unboxed]

type 'scope var_id = private {var_id : int} [@@unboxed]

type level

type 'scope t

type 'scope expression_type =
  | Const of 'scope type_name
  | Operator of 'scope expression_type list
  | Var of 'scope var_ref

and 'scope var_ref

and 'scope var =
  | Unbound of 'scope var_id * level
  | Bound of 'scope expression_type
  | Generic of 'scope var_id

type 'a computation = {computation : 'scope. 'scope t -> 'a} [@@unboxed]

val const : string -> 'scope expression_type

val arrow :
  'scope expression_type -> 'scope expression_type -> 'scope expression_type

val new_unbound_var : 'scope t -> 'scope var_ref

val new_generic_var : 'scope t -> 'scope var

val assign : 'scope var_ref -> 'scope var -> unit

val deref : 'scope var_ref -> 'scope var

val with_next_level : 'scope t -> ('scope t -> unit) -> unit

val run : 'a computation -> 'a
