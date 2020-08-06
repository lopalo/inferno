(* 'env is a phantom type *)

type 'env var_id = private {var_id : int} [@@unboxed]

type 'env t

type level = {level : int} [@@unboxed]

type 'env ty =
  | Type of Core.type_name * 'env ty list
  | Variable of 'env var ref

and 'env var =
  | Bound of 'env ty
  | Unbound of 'env var_id * level
  | Generic of 'env var_id
  | Generic' of 'env var_id * level

type 'a computation = {computation : 'env. 'env t -> 'a} [@@unboxed]

val next_var_id : 'env t -> 'env var_id

val run : 'a computation -> 'a
