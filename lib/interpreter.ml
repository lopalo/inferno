module E = Expression
module Scope = Core.Scope

type obj =
  | Unit
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | Function of func

and func =
  | Lambda of
      { scope : scope;
        parameter : Core.name;
        result : E.typed }
  | Function1 of (obj -> obj)
  | Function2 of (obj -> obj -> obj)

and scope = obj Scope.t

exception TypeError of string

let type_error name = raise (TypeError name)

let rec execute scope ({expr; _} : E.typed) =
  match expr with
  | E.Value v -> execute_value v
  | Name n -> Scope.find n scope
  | Lambda {free_names; parameter; result} ->
      let scope = Scope.filter (fun n _ -> E.Names.mem n free_names) scope in
      Function (Lambda {scope; parameter; result})
  | Application (func, argument) -> (
      let func = execute scope func in
      let argument = execute scope argument in
      match func with
      | Function f -> execute_application argument f
      | _ -> type_error "application")
  | Let {name; rhs; body} ->
      let rhs = execute scope rhs in
      let scope = Scope.add name rhs scope in
      execute scope body
  | TypeDefinition _ -> Unit
  | Packing (_, content) -> execute scope content
  | Unpacking {name; rhs; body; _} ->
      let rhs = execute scope rhs in
      let scope = Scope.add name rhs scope in
      execute scope body

and execute_value = function
  | E.Value.Unit -> Unit
  | Int i -> Int i
  | Float f -> Float f
  | Bool b -> Bool b
  | Str s -> Str s

and execute_application argument = function
  | Lambda {scope; parameter; result} ->
      let scope = Scope.add parameter argument scope in
      execute scope result
  | Function1 f -> f argument
  | Function2 f -> Function (Function1 (f argument))

module Definitions = struct
  let fn1 f = Function (Function1 f)

  let fn2 f = Function (Function2 f)

  let int_plus i i' =
    match (i, i') with
    | Int i, Int i' -> Int (i + i')
    | _ -> type_error "int plus"

  let int_minus i i' =
    match (i, i') with
    | Int i, Int i' -> Int (i - i')
    | _ -> type_error "int minus"

  let int_to_string = function
    | Int i -> Str (string_of_int i)
    | _ -> type_error "int to string"

  let write = function
    | Str s -> print_string s; Unit
    | _ -> type_error "write"

  let items =
    [ ("i.+", fn2 int_plus);
      ("i.-", fn2 int_minus);
      ("i.toString", fn1 int_to_string);
      ("write", fn1 write) ]

  let function_arity = function
    | Function f -> (
      match f with
      | Lambda _ -> 1
      | Function1 _ -> 1
      | Function2 _ -> 2)
    | _ -> 0

  let rec function_type_arity =
    let open Core.TypeTag in
    function
    | Type (name, [_; result]) when name = arrow ->
        succ (function_type_arity result)
    | _ -> 0

  let _match_arity obj type_tag =
    function_arity obj <> function_type_arity type_tag

  let scope _type_definitions =
    let scope =
      List.fold_left
        (fun scope (name, obj) -> Scope.add {name} obj scope)
        Scope.empty items
    in
    (* TODO: *)
    (* List.iter *)
    (*   (fun (name, type_tag) -> *)
    (*     match Scope.find_opt {name} scope with *)
    (*     | Some obj when match_arity obj type_tag -> () *)
    (*     | _ -> type_error name) *)
    (*   type_definitions; *)
    scope
end

let execute_expression type_definitions expression =
  let scope = Definitions.scope type_definitions in
  execute scope expression |> ignore
