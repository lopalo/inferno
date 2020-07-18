type name = {name : string} [@@unboxed]

type 'a t =
  | Name of name
  | TypeName of name
  | Lambda of name * 'a
  | Application of 'a * 'a
  | Unit
  | Int of int
  | Float of float
  | Bool of bool

type untyped =
  { expr : untyped t;
    source_pos : Lexing.position }

type type_tag =
  | Const of string
  | Operator of type_tag list
  | Generic of int

type typed =
  { expr : typed t;
    source_pos : Lexing.position;
    tag : type_tag }

let fmt = Printf.sprintf

let flatten_application expression =
  let rec f ({expr; _} as e : untyped) =
    match expr with
    | Application (func, arg) -> arg :: f func
    | _ -> [e]
  in
  f expression |> List.rev

let rec to_string ?(with_type = false) ({expr; _} as e : untyped) =
  (* TODO: use with_type flag  *)
  match expr with
  | Name {name} -> name
  | TypeName {name} -> name
  | Lambda (parameter, result) ->
      fmt "(\\%s -> %s)" parameter.name (to_string ~with_type result)
  | Application _ ->
      let exps = flatten_application e |> List.map (to_string ~with_type) in
      "(" ^ String.concat " " exps ^ ")"
  | Unit -> "()"
  | Int i -> Int.to_string i
  | Float f -> Float.to_string f
  | Bool b -> Bool.to_string b
