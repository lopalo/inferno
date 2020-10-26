type name = {name : string} [@@unboxed]

type type_name = {type_name : string} [@@unboxed]

type type_parameter = {type_parameter : string} [@@unboxed]

module TypeTag : sig
  type t =
    | Constant of type_name
    | Compound of t list
    | Generic of type_parameter

  val unit : type_name

  val arrow : type_name

  val iter_type_names : (type_name -> unit) -> t -> unit

  val pp : t Fmt.t

  val boxed_pp : t Fmt.t
end = struct
  type t =
    | Constant of type_name
    | Compound of t list
    | Generic of type_parameter

  let unit = {type_name = "Unit"}

  let arrow = {type_name = "->"}

  let is_arrow = function
    | Compound [Constant name; _; _] when name = arrow -> true
    | _ -> false

  let rec iter_type_names f = function
    | Constant type_name -> f type_name
    | Compound types -> List.iter (iter_type_names f) types
    | Generic _ -> ()

  let rec pp ppf =
    let open Fmt in
    function
    | Constant {type_name} -> string ppf type_name
    | Compound [Constant name; parameter; result] when name = arrow ->
        parameter_pp ppf parameter;
        sp ppf ();
        string ppf "->";
        sp ppf ();
        pp ppf result
    | Compound [Constant name; parameter] when name = arrow ->
        let partial_arrow_pp ppf () =
          parameter_pp ppf parameter; sp ppf (); string ppf "->"
        in
        (parens partial_arrow_pp) ppf ()
    | Compound types -> (list ~sep:sp parameter_pp |> parens) ppf types
    | Generic {type_parameter} -> string ppf type_parameter

  and parameter_pp ppf ty = (if is_arrow ty then Fmt.parens pp else pp) ppf ty

  let boxed_pp = Util.surround_pp "<" ">" pp |> Fmt.box ~indent:1
end

module Scope = Map.Make (struct
  type t = name

  let compare x y = String.compare x.name y.name
end)
