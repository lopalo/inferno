(* 'scope is a phantom type *)

type 'scope type_name = {type_name : string} [@@unboxed]

type 'scope var_id = {var_id : int} [@@unboxed]

type level = {level : int} [@@unboxed]

type 'scope t =
  { mutable current_var_id : 'scope var_id;
    mutable current_level : level }

type 'scope expression_type =
  | Const of 'scope type_name
  | Operator of 'scope expression_type list
  | Var of 'scope var_ref

and 'scope var_ref = 'scope var ref

and 'scope var =
  | Unbound of 'scope var_id * level
  | Bound of 'scope expression_type
  | Generic of 'scope var_id

type 'a computation = {computation : 'scope. 'scope t -> 'a} [@@unboxed]

let const type_name = Const {type_name}

let arrow parameter result = Operator [const "->"; parameter; result]

let new_unbound_var env =
  let id = env.current_var_id in
  let var = Unbound (id, env.current_level) in
  env.current_var_id <- {var_id = id.var_id + 1};
  ref var

let new_generic_var env =
  let id = env.current_var_id in
  let var = Generic id in
  env.current_var_id <- {var_id = id.var_id + 1};
  var

let assign var_ref var = var_ref := var

let deref var_ref = !var_ref

let with_next_level env f =
  let level = env.current_level in
  env.current_level <- {level = succ level.level};
  f env;
  env.current_level <- {level = level.level + 1}

let run {computation} =
  (* This is a type system trick to make the values produced in  *)
  (* different instantiations of the environment incompatible in  *)
  (* terms of their types. That's achieved through a phantom type called 'scope. *)
  let env = {current_var_id = {var_id = 0}; current_level = {level = 0}} in
  computation env
