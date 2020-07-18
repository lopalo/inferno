{
open Parser

module L = Lexing

exception Error of string
}

let whitespace = [' ' '\t']+

let newline = '\r' | '\n' | "\r\n"

let unit = '(' whitespace* ')'

let symbol = ['a'-'z' 'A'-'Z' '0'-'9' '_']

let name = ['a'-'z' '_'] symbol*

let type_name = ['A'-'Z' '_'] symbol*

let digit = ['0'-'9']

let sign = ['-' '+']

let integer = sign? digit+

let float = sign? digit+ '.' digit+

rule read =
  parse
  | whitespace {read lexbuf}
  | newline {L.new_line lexbuf; read lexbuf}
  | unit {UNIT}
  | name {NAME (L.lexeme lexbuf)}
  | type_name {TYPE_NAME (L.lexeme lexbuf)}
  | integer {INTEGER (int_of_string (L.lexeme lexbuf))}
  | float {FLOAT (float_of_string (L.lexeme lexbuf))}
  | "true" {BOOLEAN true}
  | "false" {BOOLEAN false}
  | '\\' {LAMBDA}
  | "->" {ARROW}
  | "|>" {PIPE}
  | '(' {LEFT_BRACKET}
  | ')' {RIGHT_BRACKET}
  | eof {EOF}
  | _ {raise (Error ("Unexpected char: " ^ Lexing.lexeme lexbuf))}

