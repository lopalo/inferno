type name = {name : string} [@@unboxed]

type type_name = {type_name : string} [@@unboxed]

module TypeTag = struct
  type t =
    | Constant of type_name
    | Operator of type_name * t list
    | Generic of string

  let arrow = {type_name = "->"}

  let is_arrow = function
    | Operator (name, [_; _]) when name = arrow -> true
    | _ -> false

  let rec pp ppf =
    let open Fmt in
    function
    | Constant {type_name} -> string ppf type_name
    | Operator (name, [parameter; result]) when name = arrow ->
        (if is_arrow parameter then parens pp else pp) ppf parameter;
        sp ppf ();
        string ppf "->";
        sp ppf ();
        pp ppf result
    | Operator ({type_name}, types) ->
        (list ~sep:sp pp |> pair ~sep:sp string |> parens) ppf (type_name, types)
    | Generic id -> string ppf id

  let boxed_pp = Util.surround_pp "<" ">" pp |> Fmt.box ~indent:1
end

module Value = struct
  type t =
    | Unit
    | Int of int
    | Float of float
    | Bool of bool
    | Str of string

  let pp ppf =
    let open Fmt in
    function
    | Unit -> string ppf "()"
    | Int i -> int ppf i
    | Float f -> float ppf f
    | Bool b -> bool ppf b
    | Str s -> quote string ppf @@ String.escaped s

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
