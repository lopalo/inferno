exception Error of (Lexing.position * string)

val infer_expression : Definitions.t -> Expression.parsed -> Expression.typed
