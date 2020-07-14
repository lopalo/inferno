type 'scope t

type 'scope var = {id : int} (* TODO: hide constructor *)

type 'a computation = {computation : 'scope. 'scope t -> 'a} [@@unboxed]

val new_var : 'scope t -> 'scope var

val run : 'a computation -> 'a
