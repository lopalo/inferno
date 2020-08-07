module Value : sig
  type t =
    | Unit
    | Int of int
    | Float of float
    | Bool of bool
    | Str of string

  val type_tag : t -> Core.TypeTag.t
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

val pp : ?with_type:bool -> typed Fmt.t
