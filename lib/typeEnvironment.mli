(* 'env is a phantom type *)

type 'env var_id = private {var_id : int} [@@unboxed]

type 'env t

type level = {level : int} [@@unboxed]

type 'env ty =
  | Constant of Core.type_name
  | Operator of Core.type_name * 'env ty list
  | Variable of 'env var ref

and 'env var =
  | Unbound of 'env var_id * level
  | Bound of 'env ty
  | Generic of 'env var_id

type 'a computation = {computation : 'env. 'env t -> 'a} [@@unboxed]

val next_var_id : 'env t -> 'env var_id

val run : 'a computation -> 'a
