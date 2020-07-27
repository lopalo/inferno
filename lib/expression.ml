type 'a t =
  | Value of Core.Value.t
  | Name of Core.name
  (* TODO *)
  (* | TypeName of Core.name *)
  | Lambda of Core.name * 'a
  | Application of 'a * 'a

type parsed =
  { expr : parsed t;
    source_pos : Lexing.position }

type typed =
  { expr : typed t;
    tag : Core.TypeTag.t }

let flatten_application expression =
  let rec f ({expr; _} as e : typed) =
    match expr with
    | Application (func, arg) -> arg :: f func
    | _ -> [e]
  in
  f expression |> List.rev

let flatten_lambda expression =
  let rec f params ({expr; _} as e : typed) =
    match expr with
    | Lambda (param, res) -> f (param :: params) res
    | _ -> (List.rev params, e)
  in
  f [] expression

let rec to_string ?(with_type = false) ({expr; tag} as e) =
  let fmt = Printf.sprintf in
  let str =
    match expr with
    | Value v -> Core.Value.to_string v
    | Name {name} -> name
    | Lambda _ ->
        let params, result = flatten_lambda e in
        let name p = p.Core.name in
        let params_str = List.map name params |> String.concat " " in
        fmt "(\\%s -> %s)" params_str (to_string ~with_type result)
    | Application _ ->
        let exps = flatten_application e |> List.map (to_string ~with_type) in
        "(" ^ String.concat " " exps ^ ")"
  in
  if with_type then fmt "[%s : %s]" str (Core.TypeTag.to_string tag) else str
