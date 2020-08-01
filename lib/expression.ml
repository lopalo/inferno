type 'a t =
  | Value of Core.Value.t
  | Name of Core.name
  (* TODO *)
  (* | TypeName of Core.name *)
  | Lambda of Core.name * 'a
  | Application of 'a * 'a
  | Let of
      { name : Core.name;
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

let flatten_lambda expression =
  let rec f params ({expr; tag} as e) =
    match (expr, tag) with
    | Lambda (param, res), Operator (_, tag :: _) ->
        f ((param, tag) :: params) res
    | _ -> (List.rev params, e)
  in
  f [] expression

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
  | Let {name; rhs; body} -> (let_pp ~with_type |> hvbox) ppf (name, rhs, body)

and annotation_pp : 'a. 'a Fmt.t -> ('a * _) Fmt.t =
 fun pp_value ->
  Fmt.(Core.TypeTag.boxed_pp |> pair ~sep:(const char ' ') pp_value)

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
  sp ppf ();
  string ppf "=";
  sp ppf ();
  pp ~with_type ppf rhs;
  sp ppf ();
  string ppf "in";
  sp ppf ();
  pp ~with_type ppf body
