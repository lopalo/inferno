(* 'env is a phantom type *)

type 'env type_id = {type_id : int} [@@unboxed]

type 'env t = 'env type_id ref

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

let next_type_id env =
  let id = !env in
  env := {type_id = id.type_id + 1};
  id

let run {computation} =
  (* This is a type system trick to make the mutable values produced in *)
  (* different instantiations of the environment incompatible in *)
  (* terms of their types. That's achieved through a phantom type called 'env. *)
  let env = ref {type_id = 0} in
  computation env
