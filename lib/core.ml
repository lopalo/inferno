type name = {name : string} [@@unboxed]

type type_name = {type_name : string} [@@unboxed]

type type_parameter = {type_parameter : string} [@@unboxed]

module TypeTag : sig
  type t =
    | Type of type_name * t list
    | Generic of type_parameter

  val arrow : type_name

  val unit : type_name

  val pp : t Fmt.t

  val boxed_pp : t Fmt.t
end = struct
  type t =
    | Type of type_name * t list
    | Generic of type_parameter

  let unit = {type_name = "Unit"}

  let arrow = {type_name = "->"}

  let is_arrow = function
    | Type (name, [_; _]) when name = arrow -> true
    | _ -> false

  let rec pp ppf =
    let open Fmt in
    function
    | Type ({type_name}, []) -> string ppf type_name
    | Type (name, [parameter; result]) when name = arrow ->
        parameter_pp ppf parameter;
        sp ppf ();
        string ppf "->";
        sp ppf ();
        pp ppf result
    | Type ({type_name}, types) ->
        (list ~sep:sp parameter_pp |> pair ~sep:sp string |> parens)
          ppf (type_name, types)
    | Generic {type_parameter} -> string ppf type_parameter

  and parameter_pp ppf ty = (if is_arrow ty then Fmt.parens pp else pp) ppf ty

  let boxed_pp = Util.surround_pp "<" ">" pp |> Fmt.box ~indent:1
end

module Scope = Map.Make (struct
  type t = name

  let compare x y = String.compare x.name y.name
end)
