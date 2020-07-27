type name = {name : string} [@@unboxed]

type type_name = {type_name : string} [@@unboxed]

module TypeTag = struct
  type t =
    | Constant of type_name
    | Operator of type_name * t list
    | Generic of string

  let arrow = {type_name = "->"}

  let fmt = Printf.sprintf

  let is_arrow = function
    | Operator (name, [_; _]) when name = arrow -> true
    | _ -> false

  let rec to_string = function
    | Constant {type_name} -> type_name
    | Operator (name, [parameter; result]) when name = arrow ->
        let f =
          if is_arrow parameter then fmt "(%s) -> %s" else fmt "%s -> %s"
        in
        f (to_string parameter) (to_string result)
    | Operator ({type_name}, types) ->
        let ts = List.map to_string types |> String.concat " " in
        fmt "(%s %s)" type_name ts
    | Generic id -> id
end

module Value = struct
  type t =
    | Unit
    | Int of int
    | Float of float
    | Bool of bool
    | Str of string

  let fmt = Printf.sprintf

  let to_string = function
    | Unit -> "()"
    | Int i -> Int.to_string i
    | Float f -> Float.to_string f
    | Bool b -> Bool.to_string b
    | Str s -> fmt "\"%s\"" (String.escaped s)

  let type_tag value =
    let open TypeTag in
    let type_name =
      match value with
      | Unit -> "Unit"
      | Int _ -> "Int"
      | Float _ -> "Float"
      | Bool _ -> "Bool"
      | Str _ -> "String"
    in
    Constant {type_name}
end

module Definitions : sig
  val items : (string * TypeTag.t) list
end = struct
  let c type_name = TypeTag.Constant {type_name}

  let ( @> ) parameter result = TypeTag.(Operator (arrow, [parameter; result]))

  let u = c "Unit"

  let i = c "Int"

  let f = c "Float"

  let b = c "Bool"

  let s = c "String"

  let g var_id = TypeTag.Generic var_id

  let match_bool =
    let result = g "a" in
    let branch = u @> result in
    b @> branch @> branch @> result

  let items =
    [ ("true", b);
      ("false", b);
      ("matchBool", match_bool);
      ("plus", i @> i @> i);
      ("minus", i @> i @> i);
      ("plusFloat", f @> f @> f);
      ("minusFloat", f @> f @> f);
      ("concat", s @> s @> s) ]
end

module TypeScope = Map.Make (struct
  type t = name

  let compare x y = String.compare x.name y.name
end)

let type_scope =
  let open TypeScope in
  List.fold_left
    (fun scope (name, type_tag) -> add {name} type_tag scope)
    empty Definitions.items
