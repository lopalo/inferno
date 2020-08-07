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
    let open Core.TypeTag in
    let type_name =
      match value with
      | Unit -> "Unit"
      | Int _ -> "Int"
      | Float _ -> "Float"
      | Bool _ -> "Bool"
      | Str _ -> "String"
    in
    Type ({type_name}, [])
end

type type_constructor =
  { name : Core.type_name;
    parameters : Core.type_parameter list;
    content : Core.TypeTag.t }

type 'a t =
  | Value of Value.t
  | Name of Core.name
  | Lambda of Core.name * 'a
  | Application of 'a * 'a
  | Let of
      { name : Core.name;
        rhs : 'a;
        body : 'a }
  | TypeDefinition of type_constructor
  | Packing of Core.type_name * 'a
  | Unpacking of
      { type_name : Core.type_name;
        name : Core.name;
        rhs : 'a;
        body : 'a }

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

let flatten_lambda =
  let rec f params ({expr; tag} as e) =
    match (expr, tag) with
    | Lambda (param, res), Type (_, tag :: _) -> f ((param, tag) :: params) res
    | _ -> (List.rev params, e)
  in
  f []

let space_pp = Fmt.(const char ' ')

let rec pp ?(with_type = false) ppf ({expr; tag} as e) =
  let open Fmt in
  let open Core in
  match expr with
  | Value v -> Value.pp ppf v
  | Name {name} -> string ppf name
  | Lambda _ -> (lambda_pp ~with_type |> hvbox ~indent:1) ppf (flatten_lambda e)
  | Application _ ->
      let app_pp =
        application_pp ~with_type
        |> (if with_type then annotation_pp else using fst)
        |> hvbox ~indent:1
      in
      app_pp ppf (flatten_application e, tag)
  | Let {name; rhs; body} -> let_pp ~with_type ppf (name, rhs, body)
  | TypeDefinition constructor -> type_definition_pp ppf constructor
  | Packing ({type_name}, content) ->
      (pp ~with_type |> pair ~sep:sp string |> parens) ppf (type_name, content)
  | Unpacking {type_name; name; rhs; body} ->
      unpacking_pp ~with_type ppf (type_name, name, rhs, body)

and annotation_pp : 'a. 'a Fmt.t -> ('a * _) Fmt.t =
 fun pp_value -> Fmt.(Core.TypeTag.boxed_pp |> pair ~sep:space_pp pp_value)

and parameter_pp ~with_type =
  let open Fmt in
  using (fun param -> param.Core.name) string
  |> if with_type then annotation_pp else using fst

and lambda_pp ~with_type ppf (params, res) =
  let open Fmt in
  string ppf "(\\";
  (parameter_pp ~with_type |> list ~sep:sp |> hvbox) ppf params;
  sp ppf ();
  string ppf "->";
  sp ppf ();
  pp ~with_type ppf res;
  string ppf ")"

and application_pp ~with_type =
  Fmt.(pp ~with_type |> list ~sep:sp |> Util.surround_pp "(" ")")

and let_pp ~with_type ppf ({name}, rhs, body) =
  let open Fmt in
  string ppf "let ";
  (string |> if with_type then annotation_pp else using fst) ppf (name, rhs.tag);
  string ppf " = ";
  (pp ~with_type |> box) ppf rhs;
  string ppf " in";
  sp ppf ();
  pp ~with_type ppf body

and type_definition_pp ppf {name; parameters; content} =
  let open Fmt in
  string ppf "define ";
  string ppf name.type_name;
  space_pp ppf ();
  (match parameters with
  | [] -> ()
  | params ->
      List.map (fun {Core.type_parameter} -> type_parameter) params
      |> list ~sep:space_pp string ppf;
      space_pp ppf ());
  string ppf "of ";
  (Core.TypeTag.pp |> box) ppf content

and unpacking_pp ~with_type ppf ({type_name}, {name}, rhs, body) =
  let open Fmt in
  string ppf "unpack ";
  string ppf type_name;
  space_pp ppf ();
  (string |> if with_type then annotation_pp else using fst) ppf (name, rhs.tag);
  string ppf " = ";
  (pp ~with_type |> box) ppf rhs;
  string ppf " in";
  sp ppf ();
  pp ~with_type ppf body
