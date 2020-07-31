module E = Expression
module TE = TypeEnvironment
module TS = Core.TypeScope

type 'env expression =
  { expr : 'env expression E.t;
    source_pos : Lexing.position;
    ty : 'env TE.ty }

type 'env context =
  { env : 'env TE.t;
    scope : 'env TE.ty TS.t;
    level : TE.level }

exception Error of (Lexing.position * string)

let error source_pos message = raise (Error (source_pos, message))

let var_type v = TE.Variable (ref v)

let new_unbound_var {env; level; _} =
  TE.(var_type (Unbound (next_var_id env, level)))

let rec tag_to_type var = function
  (* TODO: local scope for replacing Core.Generic *)
  (* TODO: replace (Core.TypeTag.Generic id) with val from the scope or *)
  (* (new_generic_var env) and add it to the scope  *)
  | Core.TypeTag.Constant type_name -> TE.Constant type_name
  | Operator (op_name, type_tags) ->
      Operator (op_name, List.map (tag_to_type var) type_tags)
  | Generic _ -> var

let rec type_tag = function
  | TE.Constant type_name -> Core.TypeTag.Constant type_name
  | Operator (op_name, types) -> Operator (op_name, List.map type_tag types)
  | Variable {contents = Bound ty} -> type_tag ty
  | Variable {contents = Unbound ({var_id}, _)} ->
      Generic (Util.name_of_id var_id)

let check_occurrence source_pos var_id ty =
  let rec f = function
    | TE.Constant _ -> ()
    | Operator (_, types) -> List.iter f types
    | Variable {contents = Bound ty} -> f ty
    | Variable {contents = Unbound (id, _)} ->
        if var_id = id then error source_pos "recursive type"
  in
  f ty

let unify source_pos ty ty' =
  let rec f ty ty' =
    if ty == ty' then ()
    else
      match (ty, ty') with
      | TE.Constant name, TE.Constant name' when name = name' -> ()
      | Operator (name, types), Operator (name', types')
        when name = name' && List.length types = List.length types' ->
          List.iter2 f types types'
      | Variable {contents = Bound ty}, ty'
      | ty, Variable {contents = Bound ty'} ->
          f ty ty'
      | Variable ({contents = Unbound (id, _)} as var), ty
      | ty, Variable ({contents = Unbound (id, _)} as var) ->
          check_occurrence source_pos id ty;
          var := Bound ty
      | ty, ty' ->
          let ty_pp ppf ty = type_tag ty |> Core.TypeTag.boxed_pp ppf in
          Fmt.str "cannot unify types@ %a@ and@ %a" ty_pp ty ty_pp ty'
          |> error source_pos
  in
  f ty ty'

let arrow parameter result =
  TE.Operator (Core.TypeTag.arrow, [parameter; result])

let rec infer ({scope; _} as ctx) {E.expr; source_pos} =
  let expr, ty =
    match expr with
    | Value v as e ->
        (e, Core.Value.type_tag v |> tag_to_type (new_unbound_var ctx))
    | Name n as e -> (
        ( e,
          match TS.find_opt n scope with
          | Some ty -> ty
          | None ->
              Printf.sprintf "undefined name \"%s\"" n.name |> error source_pos
        ))
    | Lambda (parameter, result) ->
        let param_type = new_unbound_var ctx in
        let scope = TS.add parameter param_type scope in
        let result = infer {ctx with scope} result in
        let ty = arrow param_type result.ty in
        (Lambda (parameter, result), ty)
    | Application (func, argument) ->
        let func = infer ctx func in
        let argument = infer ctx argument in
        let ty = new_unbound_var ctx in
        unify source_pos func.ty (arrow argument.ty ty);
        (Application (func, argument), ty)
  in
  {expr; source_pos; ty}

let rec annotate {expr; ty; _} =
  let expr =
    match expr with
    | (Value _ | Name _) as e -> e
    | Lambda (parameter, result) -> Lambda (parameter, annotate result)
    | Application (func, argument) ->
        Application (annotate func, annotate argument)
  in
  {E.expr; tag = type_tag ty}

let infer_expression expression =
  let computation env =
    let level = {TE.level = 0} in
    let context = {env; scope = TS.empty; level} in
    let scope =
      TS.map (fun t -> tag_to_type (new_unbound_var context) t) Core.type_scope
    in
    let context = {context with scope} in
    let expr = infer context expression in
    annotate expr
  in
  TE.run {computation}
