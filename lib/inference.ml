module E = Expression
module Env = TypeEnvironment
module Tag = Core.TypeTag

module Scope = Map.Make (struct
  open Core

  type t = name

  let compare x y = String.compare x.name y.name
end)

module TypeNames = Set.Make (struct
  open Core

  type t = type_name

  let compare x y = String.compare x.type_name y.type_name
end)

type 'env expression =
  { expr : 'env expression E.t;
    source_pos : Lexing.position;
    ty : 'env Env.ty }

type 'env context =
  { env : 'env Env.t;
    scope : 'env Env.ty Scope.t;
    level : Env.level;
    defined_types : TypeNames.t }

exception Error of (Lexing.position * string)

let error source_pos message = raise (Error (source_pos, message))

let var_type v = Env.Variable (ref v)

let new_unbound_var {env; level; _} =
  Env.(var_type (Unbound (next_var_id env, level)))

let new_generic_var {env; _} = Env.(var_type (Generic (next_var_id env)))

let tag_to_type ctx ty =
  let vars = Hashtbl.create 10 in
  let rec f = function
    | Tag.Type (op_name, type_tags) -> Env.Type (op_name, List.map f type_tags)
    | Generic id -> (
      match Hashtbl.find_opt vars id with
      | Some ty -> ty
      | None ->
          let ty = new_generic_var ctx in
          Hashtbl.add vars id ty; ty)
  in
  f ty

let rec type_tag = function
  | Env.Type (op_name, types) -> Tag.Type (op_name, List.map type_tag types)
  | Variable {contents = Bound ty} -> type_tag ty
  | Variable {contents = Unbound ({var_id}, _)}
  | Variable {contents = Generic {var_id}} ->
      Generic (Util.name_of_id var_id)

let check_occurrence source_pos var_id ty =
  let rec f = function
    | Env.Type (_, types) -> List.iter f types
    | Variable {contents = Bound ty} -> f ty
    | Variable {contents = Unbound (id, _)} ->
        if var_id = id then error source_pos "recursive type"
    | Variable {contents = Generic _} -> assert false
  in
  f ty

let adjust_levels (level : Env.level) ty =
  let rec f = function
    | Env.Type (_, types) -> List.iter f types
    | Variable {contents = Bound ty} -> f ty
    | Variable ({contents = Unbound (id, level')} as var) ->
        var := Unbound (id, {level = min level.level level'.level})
    | Variable {contents = Generic _} -> assert false
  in
  f ty

let unify source_pos ty ty' =
  let rec f ty ty' =
    if ty == ty' then ()
    else
      match (ty, ty') with
      | Env.Type (name, types), Env.Type (name', types')
        when name = name' && List.length types = List.length types' ->
          List.iter2 f types types'
      | Variable {contents = Bound ty}, ty'
      | ty, Variable {contents = Bound ty'} ->
          f ty ty'
      | Variable ({contents = Unbound (id, level)} as var), ty
      | ty, Variable ({contents = Unbound (id, level)} as var) ->
          check_occurrence source_pos id ty;
          adjust_levels level ty;
          var := Bound ty
      | ty, ty' ->
          let ty_pp ppf ty = type_tag ty |> Tag.boxed_pp ppf in
          Fmt.str "cannot unify types@ %a@ and@ %a" ty_pp ty ty_pp ty'
          |> error source_pos
  in
  f ty ty'

let generalize (level : Env.level) ty =
  let rec f = function
    | Env.Type (name, types) -> Env.Type (name, List.map f types)
    | Variable {contents = Bound ty} -> f ty
    | Variable {contents = Unbound (id, level')} as ty ->
        if level.level < level'.level then Variable (ref (Env.Generic id))
        else ty
    | Variable {contents = Generic _} as ty -> ty
  in
  f ty

let instantiate ctx ty =
  let vars = Hashtbl.create 10 in
  let rec f = function
    | Env.Type (name, types) -> Env.Type (name, List.map f types)
    | Variable {contents = Bound ty} -> f ty
    | Variable {contents = Unbound _} as ty -> ty
    | Variable {contents = Generic id} -> (
      match Hashtbl.find_opt vars id with
      | Some ty -> ty
      | None ->
          let ty = new_unbound_var ctx in
          Hashtbl.add vars id ty; ty)
  in
  f ty

let arrow parameter result = Env.Type (Tag.arrow, [parameter; result])

let rec infer ({scope; _} as ctx) {E.expr; source_pos} =
  let expr, ty =
    match expr with
    | Value v as e -> (e, Core.Value.type_tag v |> tag_to_type ctx)
    | Name n as e -> (
        ( e,
          match Scope.find_opt n scope with
          | Some ty -> instantiate ctx ty
          | None ->
              Printf.sprintf "undefined name \"%s\"" n.name |> error source_pos
        ))
    | Lambda (parameter, result) ->
        let param_type = new_unbound_var ctx in
        let scope = Scope.add parameter param_type scope in
        let result = infer {ctx with scope} result in
        let ty = arrow param_type result.ty in
        (Lambda (parameter, result), ty)
    | Application (func, argument) ->
        let func = infer ctx func in
        let argument = infer ctx argument in
        let ty = new_unbound_var ctx in
        unify source_pos func.ty (arrow argument.ty ty);
        (Application (func, argument), ty)
    | Let {name; rhs; body} ->
        let lvl = ctx.level in
        let rhs = infer {ctx with level = {level = lvl.level + 1}} rhs in
        let rhs = {rhs with ty = generalize lvl rhs.ty} in
        let scope = Scope.add name rhs.ty scope in
        let body = infer {ctx with scope} body in
        (Let {name; rhs; body}, body.ty)
  in
  {expr; source_pos; ty}

let rec annotate {expr; ty; _} =
  let expr =
    match expr with
    | (Value _ | Name _) as e -> e
    | Lambda (parameter, result) -> Lambda (parameter, annotate result)
    | Application (func, argument) ->
        Application (annotate func, annotate argument)
    | Let {name; rhs; body} ->
        let rhs, body = (annotate rhs, annotate body) in
        Let {name; rhs; body}
  in
  {E.expr; tag = type_tag ty}

let infer_expression definitions expression =
  let computation env =
    let level = {Env.level = 0} in
    (* TODO: populate defined_types with types from Core and Definitions *)
    let context =
      {env; scope = Scope.empty; level; defined_types = TypeNames.empty}
    in
    let scope =
      List.fold_left
        (fun scope (name, type_tag) ->
          Scope.add {name} (tag_to_type context type_tag) scope)
        Scope.empty definitions
    in
    let context = {context with scope} in
    let expr = infer context expression in
    annotate expr
  in
  Env.run {computation}
