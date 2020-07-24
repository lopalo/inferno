(* 'env is a phantom type *)

type 'env var_id = {var_id : int} [@@unboxed]

type 'env t = 'env var_id ref

type level = {level : int} [@@unboxed]

type 'env ty =
  | Constant of Core.type_name
  | Operator of Core.type_name * 'env ty list
  | Variable of 'env var ref

and 'env var =
  | Unbound of 'env var_id * level
  | Bound of 'env ty

(* | Generic of 'env var_id *)

type 'a computation = {computation : 'env. 'env t -> 'a} [@@unboxed]

let next_var_id env =
  let id = !env in
  env := {var_id = id.var_id + 1};
  id

let run {computation} =
  (* This is a type system trick to make the mutable values produced in *)
  (* different instantiations of the environment incompatible in *)
  (* terms of their types. That's achieved through a phantom type called 'env. *)
  let env = ref {var_id = 0} in
  computation env
