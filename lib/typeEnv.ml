type 'scope t =
  { mutable current_var_id : int;
    mutable current_level : int }

type 'scope var = {id : int}

type 'a computation = {computation : 'scope. 'scope t -> 'a} [@@unboxed]

let new_var env =
  let var = {id = env.current_var_id} in
  env.current_var_id <- env.current_var_id + 1;
  var

let run {computation} =
  (* This is a type system trick to prevent  *)
  (* a mutable state from leaking outside the function *)
  let env = {current_var_id = 0; current_level = 0} in
  computation env
