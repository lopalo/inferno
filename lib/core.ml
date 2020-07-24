type name = {name : string} [@@unboxed]

type type_name = {type_name : string} [@@unboxed]

module TypeTag = struct
  type t =
    | Constant of type_name
    | Operator of type_name * t list

  let arrow = {type_name = "->"}

  (* TODO *)
  let to_string _ = "X"
end

(* | Generic of string *)

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

  (* TODO *)
  (* let g var_id = Generic var_id *)

  let items =
    (* TODO: polymorphic signature for "matchBool" *)
    [ ("matchBool", b @> (u @> f) @> u @> f);
      ("plus", i @> i @> i);
      ("minus", i @> i @> i);
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
