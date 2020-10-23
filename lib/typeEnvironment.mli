(* 'env is a phantom type *)

type 'env type_id = private {type_id : int} [@@unboxed]

type 'env t

type level = {level : int} [@@unboxed]

type 'env ty =
  | Type of Core.type_name * 'env ty list
  | Generic of 'env type_id
  | Generic' of 'env type_id * level
  | Variable of 'env var ref

and 'env var =
  | Bound of 'env ty
  | Unbound of 'env type_id * level

type 'a computation = {computation : 'env. 'env t -> 'a} [@@unboxed]

val next_type_id : 'env t -> 'env type_id

val run : 'a computation -> 'a
