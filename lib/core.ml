type type_tag =
  | Const of string
  | Operator of type_tag list
  | Generic of int

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
end
