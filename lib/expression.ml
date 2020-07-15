type name = Name of string [@@unboxed]

module type Meta = sig
  type 'a t
end

module Expression (Meta : Meta) = struct
  type t = expr Meta.t

  and expr =
    | Name of name
    | Lambda of t * t
    | Application of t * t
    | Int of int
    | Bool of int
end

type source_location =
  { file_name : string;
    line_num : int;
    column_nub : int }

module UntypedMeta = struct
  type 'a t =
    { expr : 'a;
      source_loc : source_location }
end

module TypedMeta = struct
  type type_tag =
    | Const of string
    | Operator of type_tag list
    | Generic of int

  type 'a t =
    { expr : 'a;
      source_loc : source_location;
      tag : type_tag }
end

module Untyped = Expression (UntypedMeta)
module Typed = Expression (TypedMeta)
