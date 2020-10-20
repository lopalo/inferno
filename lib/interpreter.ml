module E = Expression
module Scope = Core.Scope

type obj =
  | Unit
  | Int of int
  | Float of float
  | Bool of bool
  | Str of string
  | Function of func
  | Option of obj option
  | And of obj * obj
  | Or of (obj, obj) result
  | List of obj list
  | Tree of tree

and func =
  | Lambda of
      { scope : scope;
        parameter : Core.name;
        result : E.typed }
  | Function1 of (obj -> obj)
  | Function2 of (obj -> obj -> obj)
  | Function3 of (obj -> obj -> obj -> obj)

and scope = obj Scope.t

and tree =
  | Empty
  | Node of tree * obj * tree

exception TypeError of string

let type_error name = raise (TypeError name)

let rec execute scope ({expr; _} : E.typed) =
  match expr with
  | E.Value v -> execute_value v
  | Name n -> Scope.find n scope
  | Lambda {free_names; parameter; result} ->
      let scope = Scope.filter (fun n _ -> E.Names.mem n free_names) scope in
      Function (Lambda {scope; parameter; result})
  | Application (func, argument) ->
      let func = execute scope func in
      let argument = execute scope argument in
      execute_application func argument
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

and execute_application func argument =
  match func with
  | Function func -> (
    match func with
    | Lambda {scope; parameter; result} ->
        let scope = Scope.add parameter argument scope in
        execute scope result
    | Function1 f -> f argument
    | Function2 f -> Function (Function1 (f argument))
    | Function3 f -> Function (Function2 (f argument)))
  | _ -> type_error "application"

module Definitions = struct
  let fn1 f = Function (Function1 f)

  let fn2 f = Function (Function2 f)

  let fn3 f = Function (Function3 f)

  let rec fix func argument =
    let func' = fix func |> fn1 in
    execute_application (execute_application func func') argument

  let rec loop func x =
    match execute_application func x with
    | Or (Error x) -> loop func x
    | Or (Ok res) -> res
    | _ -> type_error "loop"

  let true_value = Bool true

  let false_value = Bool false

  let match_bool condition true_func false_func =
    match condition with
    | Bool true -> execute_application true_func Unit
    | Bool false -> execute_application false_func Unit
    | _ -> type_error "match bool"

  let none = Option None

  let some x = Option (Some x)

  let match_option option_val none_func some_func =
    match option_val with
    | Option None -> execute_application none_func Unit
    | Option (Some x) -> execute_application some_func x
    | _ -> type_error "match option"

  let and_ x y = And (x, y)

  let first = function
    | And (a, _) -> a
    | _ -> type_error "first"

  let second = function
    | And (_, b) -> b
    | _ -> type_error "second"

  let left x = Or (Error x)

  let right x = Or (Ok x)

  let match_or or_val left_func right_func =
    match or_val with
    | Or (Error x) -> execute_application left_func x
    | Or (Ok x) -> execute_application right_func x
    | _ -> type_error "match or"

  let null = List []

  let cons x = function
    | List l -> List (x :: l)
    | _ -> type_error "cons"

  let match_list list_val null_func list_func =
    match list_val with
    | List [] -> execute_application null_func Unit
    | List (x :: tail) ->
        execute_application (execute_application list_func x) (List tail)
    | _ -> type_error "match list"

  let empty = Tree Empty

  let node left element right =
    match (left, right) with
    | Tree left, Tree right -> Tree (Node (left, element, right))
    | _ -> type_error "node"

  let match_tree tree_val empty_func tree_func =
    match tree_val with
    | Tree Empty -> execute_application empty_func Unit
    | Tree (Node (left, element, right)) ->
        execute_application
          (execute_application
             (execute_application tree_func (Tree left))
             element)
          (Tree right)
    | _ -> type_error "match tree"

  let int_eq i i' =
    match (i, i') with
    | Int i, Int i' -> Bool (i = i')
    | _ -> type_error "int equals"

  let int_gt i i' =
    match (i, i') with
    | Int i, Int i' -> Bool (i > i')
    | _ -> type_error "int greater than"

  let int_plus i i' =
    match (i, i') with
    | Int i, Int i' -> Int (i + i')
    | _ -> type_error "int plus"

  let int_minus i i' =
    match (i, i') with
    | Int i, Int i' -> Int (i - i')
    | _ -> type_error "int minus"

  let int_mul i i' =
    match (i, i') with
    | Int i, Int i' -> Int (i * i')
    | _ -> type_error "int mul"

  let int_div i i' =
    match (i, i') with
    | Int _, Int 0 -> Int 0
    | Int i, Int i' -> Int (i / i')
    | _ -> type_error "int div"

  let int_to_string = function
    | Int i -> Str (string_of_int i)
    | _ -> type_error "int to string"

  let int_of_string = function
    | Str s -> Option (int_of_string_opt s |> Option.map (fun i -> Int i))
    | _ -> type_error "int of string"

  let int_to_float = function
    | Int i -> Float (float_of_int i)
    | _ -> type_error "int to float"

  let float_eq f f' =
    match (f, f') with
    | Float f, Float f' -> Bool (f = f')
    | _ -> type_error "float equals"

  let float_gt f f' =
    match (f, f') with
    | Float f, Float f' -> Bool (f > f')
    | _ -> type_error "float greater than"

  let float_plus f f' =
    match (f, f') with
    | Float f, Float f' -> Float (f +. f')
    | _ -> type_error "float plus"

  let float_minus f f' =
    match (f, f') with
    | Float f, Float f' -> Float (f -. f')
    | _ -> type_error "float minus"

  let float_mul f f' =
    match (f, f') with
    | Float f, Float f' -> Float (f *. f')
    | _ -> type_error "float mul"

  let float_div f f' =
    match (f, f') with
    | Float _, Float 0.0 -> Float 0.0
    | Float f, Float f' -> Float (f /. f')
    | _ -> type_error "float div"

  let float_to_string = function
    | Float f -> Str (string_of_float f)
    | _ -> type_error "float to string"

  let float_of_string = function
    | Str s -> Option (float_of_string_opt s |> Option.map (fun f -> Float f))
    | _ -> type_error "float of string"

  let float_to_int = function
    | Float f -> Int (int_of_float f)
    | _ -> type_error "float to int"

  let string_eq s s' =
    match (s, s') with
    | Str s, Str s' -> Bool (String.equal s s')
    | _ -> type_error "String equals"

  let string_gt s s' =
    match (s, s') with
    | Str s, Str s' -> Bool (s > s')
    | _ -> type_error "string greater than"

  let length = function
    | Str s -> Int (String.length s)
    | _ -> type_error "string length"

  let concat s s' =
    match (s, s') with
    | Str s, Str s' -> Str (s ^ s')
    | _ -> type_error "concat"

  let split s s' =
    match (s, s') with
    | Str s, Str s' ->
        List (Str.(split (regexp s) s') |> List.map (fun s -> Str s))
    | _ -> type_error "split"

  let substr s offset length =
    match (s, offset, length) with
    | Str s, Int offset, Int length ->
        let str_len = String.length s in
        let offset = min str_len offset |> max 0 in
        let length = min (str_len - offset) length |> max 0 in
        Str (String.sub s offset length)
    | _ -> type_error "substr"

  let readline = function
    | Unit -> Str (read_line ())
    | _ -> type_error "read line"

  let write = function
    | Str s -> print_string s; Unit
    | _ -> type_error "write"

  let items =
    [ ("fix", fn2 fix);
      ("loop", fn2 loop);
      ("true", true_value);
      ("false", false_value);
      ("@bool", fn3 match_bool);
      ("none", none);
      ("some", fn1 some);
      ("@option", fn3 match_option);
      ("&", fn2 and_);
      ("$1", fn1 first);
      ("$2", fn1 second);
      ("left", fn1 left);
      ("right", fn1 right);
      ("@or", fn3 match_or);
      ("null", null);
      ("cons", fn2 cons);
      ("@list", fn3 match_list);
      ("empty", empty);
      ("node", fn3 node);
      ("@tree", fn3 match_tree);
      ("i.=", fn2 int_eq);
      ("i.>", fn2 int_gt);
      ("i.+", fn2 int_plus);
      ("i.-", fn2 int_minus);
      ("i.*", fn2 int_mul);
      ("i./", fn2 int_div);
      ("i.toString", fn1 int_to_string);
      ("i.ofString", fn1 int_of_string);
      ("i.toFloat", fn1 int_to_float);
      ("f.=", fn2 float_eq);
      ("f.>", fn2 float_gt);
      ("f.+", fn2 float_plus);
      ("f.-", fn2 float_minus);
      ("f.*", fn2 float_mul);
      ("f./", fn2 float_div);
      ("f.toString", fn1 float_to_string);
      ("f.ofString", fn1 float_of_string);
      ("f.toInt", fn1 float_to_int);
      ("string.=", fn2 string_eq);
      ("string.>", fn2 string_gt);
      ("length", fn1 length);
      ("concat", fn2 concat);
      ("split", fn2 split);
      ("substr", fn3 substr);
      ("readLine", fn1 readline);
      ("write", fn1 write) ]

  let function_arity = function
    | Function f -> (
      match f with
      | Lambda _ -> 1
      | Function1 _ -> 1
      | Function2 _ -> 2
      | Function3 _ -> 3)
    | _ -> 0

  let rec function_type_arity =
    let open Core.TypeTag in
    function
    | Type (name, [_; result]) when name = arrow ->
        succ (function_type_arity result)
    | _ -> 0

  let match_arity obj type_tag =
    function_arity obj = function_type_arity type_tag

  let scope type_definitions =
    let scope =
      List.fold_left
        (fun scope (name, obj) -> Scope.add {name} obj scope)
        Scope.empty items
    in
    List.iter
      (fun (name, type_tag) ->
        match Scope.find_opt {name} scope with
        | Some obj when match_arity obj type_tag -> ()
        | _ -> type_error name)
      type_definitions;
    scope
end

let execute_expression type_definitions expression =
  let scope = Definitions.scope type_definitions in
  execute scope expression |> ignore
