type name = {name : string} [@@unboxed]

type 'a t =
  | Value of Core.Value.t
  | Name of name
  | TypeName of name
  | Lambda of name * 'a
  | Application of 'a * 'a

type untyped =
  { expr : untyped t;
    source_pos : Lexing.position }

type typed =
  { expr : typed t;
    source_pos : Lexing.position;
    tag : Core.type_tag }

let flatten_application expression =
  let rec f ({expr; _} as e : untyped) =
    match expr with
    | Application (func, arg) -> arg :: f func
    | _ -> [e]
  in
  f expression |> List.rev

let flatten_lambda expression =
  let rec f params ({expr; _} as e : untyped) =
    match expr with
    | Lambda (param, res) -> f (param :: params) res
    | _ -> (List.rev params, e)
  in
  f [] expression

let rec to_string ?(with_type = false) ({expr; _} as e : untyped) =
  (* TODO: use with_type flag  *)
  let fmt = Printf.sprintf in
  match expr with
  | Value v -> Core.Value.to_string v
  | Name {name} -> name
  | TypeName {name} -> name
  | Lambda _ ->
      let params, result = flatten_lambda e in
      let params_str = List.map (fun p -> p.name) params |> String.concat " " in
      fmt "(\\%s -> %s)" params_str (to_string ~with_type result)
  | Application _ ->
      let exps = flatten_application e |> List.map (to_string ~with_type) in
      "(" ^ String.concat " " exps ^ ")"
