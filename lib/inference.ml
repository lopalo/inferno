module E = Expression
module Env = TypeEnvironment
module Tag = Core.TypeTag
module Scope = Core.Scope

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
    core_types : TypeNames.t;
    constructors : (Core.type_name, E.type_constructor) Hashtbl.t }

exception Error of (Lexing.position * string)

let error source_pos message = raise (Error (source_pos, message))

let var_type v = Env.Variable (ref v)

let new_unbound_var {env; level; _} =
  Env.(var_type (Unbound (next_type_id env, level)))

let new_generic_type {env; _} = Env.(Generic (next_type_id env))

let new_generic_type' {env; level; _} = Env.(Generic' (next_type_id env, level))

let next_level ctx = {ctx with level = {level = ctx.level.level + 1}}

let tag_to_type ctx tag =
  let open Hashtbl in
  let vars = create 10 in
  let rec f = function
    | Tag.Constant name -> Env.Constant name
    | Compound type_tags -> Compound (List.map f type_tags)
    | Generic id -> (
      match find_opt vars id with
      | Some ty -> ty
      | None ->
          let ty = new_generic_type ctx in
          add vars id ty; ty)
  in
  f tag

let rec type_tag = function
  | Env.Constant name -> Tag.Constant name
  | Compound types -> Compound (List.map type_tag types)
  | Generic {type_id} -> Generic {type_parameter = "'" ^ Util.name_of_id type_id}
  | Generic' ({type_id}, _) ->
      Generic {type_parameter = "''" ^ Util.name_of_id type_id}
  | Variable {contents = Bound ty} -> type_tag ty
  | Variable {contents = Unbound ({type_id}, _)} ->
      Generic {type_parameter = Util.name_of_id type_id}

let check_occurrence source_pos type_id =
  let rec f = function
    | Env.Constant _ -> ()
    | Compound types -> List.iter f types
    | Generic _ -> assert false
    | Generic' _ -> ()
    | Variable {contents = Bound ty} -> f ty
    | Variable {contents = Unbound (id, _)} ->
        if type_id = id then error source_pos "Recursive type"
  in
  f

let adjust_levels (level : Env.level) =
  let rec f = function
    | Env.Constant _ -> ()
    | Compound types -> List.iter f types
    | Generic _ -> assert false
    | Generic' _ -> ()
    | Variable {contents = Bound ty} -> f ty
    | Variable ({contents = Unbound (id, var_level)} as var) ->
        var := Unbound (id, {level = min level.level var_level.level})
  in
  f

let generic_type'_escapes (level : Env.level) =
  let rec f = function
    | Env.Constant _ -> false
    | Compound types -> List.exists f types
    | Generic _ -> assert false
    | Generic' (_, gen_level) -> level.level <= gen_level.level
    | Variable {contents = Bound ty} -> f ty
    | Variable {contents = Unbound _} -> false
  in
  f

let unify source_pos root_ty root_ty' =
  let rec f ty ty' =
    if ty == ty' then ()
    else
      let unification_error details =
        let ty_pp ppf ty = type_tag ty |> Tag.boxed_pp ppf in
        Fmt.str
          "Cannot unify types@[<hv 1> %a and@ %a@].@ Types@[<hv 1> %a and@ \
           %a@] are not compatible.@ %s"
          ty_pp root_ty ty_pp root_ty' ty_pp ty ty_pp ty' details
        |> error source_pos
      in
      match (ty, ty') with
      | Env.Constant name, Env.Constant name' when name = name' -> ()
      | Compound types, Compound types'
        when List.length types = List.length types' ->
          List.iter2 f types types'
      | Variable {contents = Bound ty}, ty'
      | ty, Variable {contents = Bound ty'} ->
          f ty ty'
      | Variable ({contents = Unbound (id, level)} as var), ty
      | ty, Variable ({contents = Unbound (id, level)} as var) ->
          check_occurrence source_pos id ty;
          if generic_type'_escapes level ty then
            unification_error
              "Some generic type of rank-2 would escape its scope";
          adjust_levels level ty;
          var := Bound ty
      | _ -> unification_error ""
  in
  f root_ty root_ty'

let generalize (level : Env.level) =
  let rec f = function
    | Env.Constant _ as ty -> ty
    | Compound types -> Compound (List.map f types)
    | Generic _ as ty -> ty
    | Generic' _ as ty -> ty
    | Variable {contents = Bound ty} -> f ty
    | Variable {contents = Unbound (id, var_level)} as ty ->
        if level.level < var_level.level then Env.Generic id else ty
  in
  f

let instantiate ctx ty =
  let open Hashtbl in
  let vars = create 10 in
  let rec f = function
    | Env.Constant _ as ty -> ty
    | Compound types -> Compound (List.map f types)
    | Generic id -> (
      match find_opt vars id with
      | Some ty -> ty
      | None ->
          let ty = new_unbound_var ctx in
          add vars id ty; ty)
    | Generic' _ as ty -> ty
    | Variable {contents = Bound ty} -> f ty
    | Variable {contents = Unbound _} as ty -> ty
  in
  f ty

let instantiate_type_constructor ctx make_generic_type {E.parameters; content; _}
    =
  let open Hashtbl in
  let vars = create 10 in
  let parameters =
    List.map
      (fun param ->
        let ty = new_unbound_var ctx in
        replace vars param ty; ty)
      parameters
  in
  let rec f = function
    | Tag.Constant op_name -> Env.Constant op_name
    | Compound type_tags -> Compound (List.map f type_tags)
    | Generic id -> (
      match find_opt vars id with
      | Some ty -> ty
      | None ->
          let ty = make_generic_type ctx in
          add vars id ty; ty)
  in
  (parameters, f content)

let arrow parameter result =
  Env.(Compound [Constant Tag.arrow; parameter; result])

let rec infer ({scope; core_types; constructors; _} as ctx) {E.expr; source_pos}
    =
  let fmt = Printf.sprintf in
  let expr, ty =
    match expr with
    | Value v as e -> (e, E.Value.type_tag v |> tag_to_type ctx)
    | Name n as e -> (
        ( e,
          match Scope.find_opt n scope with
          | Some ty -> instantiate ctx ty
          | None -> fmt "Undefined name \"%s\"" n.name |> error source_pos ))
    | Lambda ({parameter; result; _} as l) ->
        let param_type = new_unbound_var ctx in
        let scope = Scope.add parameter param_type scope in
        let result = infer {ctx with scope} result in
        let ty = arrow param_type result.ty in
        (Lambda {l with parameter; result}, ty)
    | Application (func, argument) ->
        let func = infer ctx func in
        let argument = infer ctx argument in
        let ty = new_unbound_var ctx in
        unify source_pos func.ty (arrow argument.ty ty);
        (Application (func, argument), ty)
    | Let {name; rhs; body} ->
        let lvl = ctx.level in
        let rhs = infer (next_level ctx) rhs in
        let rhs = {rhs with ty = generalize lvl rhs.ty} in
        let scope = Scope.add name rhs.ty scope in
        let body = infer {ctx with scope} body in
        (Let {name; rhs; body}, body.ty)
    | TypeDefinition ({name; content; _} as constructor) ->
        let is_defined type_name =
          TypeNames.mem type_name core_types
          || Hashtbl.mem constructors type_name
        in
        if is_defined name then
          fmt "Type \"%s\" is already defined" name.type_name
          |> error source_pos;
        Hashtbl.add constructors name constructor;
        Tag.iter_type_names
          (fun name ->
            if not (is_defined name) then
              fmt "Type \"%s\" is not defined" name.type_name
              |> error source_pos)
          content;
        let ty = Env.Constant Tag.unit in
        (TypeDefinition constructor, ty)
    | Packing (type_name, content) -> (
      match Hashtbl.find_opt constructors type_name with
      | None ->
          fmt "Type \"%s\" is not defined" type_name.type_name
          |> error source_pos
      | Some constructor ->
          let type_params, content_ty =
            instantiate_type_constructor ctx new_generic_type' constructor
          in
          let content = infer (next_level ctx) content in
          unify source_pos content_ty content.ty;
          let ty = Env.Compound (Constant type_name :: type_params) in
          (Packing (type_name, content), ty))
    | Unpacking {type_name; name; rhs; body} -> (
      match Hashtbl.find_opt constructors type_name with
      | None ->
          fmt "Type \"%s\" is not defined" type_name.type_name
          |> error source_pos
      | Some constructor ->
          let type_params, content_ty =
            instantiate_type_constructor ctx new_generic_type constructor
          in
          let rhs_ty = Env.Compound (Constant type_name :: type_params) in
          let rhs = infer ctx rhs in
          unify source_pos rhs_ty rhs.ty;
          let scope = Scope.add name content_ty scope in
          let body = infer {ctx with scope} body in
          (Unpacking {type_name; name; rhs; body}, body.ty))
  in
  {expr; source_pos; ty}

let rec annotate {expr; ty; _} =
  let expr =
    match expr with
    | (Value _ | Name _) as e -> e
    | Lambda ({parameter; result; _} as l) ->
        Lambda {l with parameter; result = annotate result}
    | Application (func, argument) ->
        Application (annotate func, annotate argument)
    | Let {name; rhs; body} ->
        let rhs, body = (annotate rhs, annotate body) in
        Let {name; rhs; body}
    | TypeDefinition constructor -> TypeDefinition constructor
    | Packing (type_name, content) ->
        let content = annotate content in
        Packing (type_name, content)
    | Unpacking {type_name; name; rhs; body} ->
        let rhs = annotate rhs in
        let body = annotate body in
        Unpacking {type_name; name; rhs; body}
  in
  {E.expr; tag = type_tag ty}

let collect_type_names =
  let open TypeNames in
  let rec collect = function
    | Tag.Constant type_name -> singleton type_name
    | Compound type_tags -> fold empty type_tags
    | Generic _ -> empty
  and fold type_names type_tags =
    List.(map collect type_tags |> fold_left union type_names)
  in
  fold empty

let infer_expression definitions expression =
  let computation env =
    let level = {Env.level = 0} in
    let core_types = List.map snd definitions |> collect_type_names in
    let constructors = Hashtbl.create 100 in
    let context = {env; scope = Scope.empty; level; core_types; constructors} in
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
