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
  | integer {INTEGER (int_of_string (L.lexeme lexbuf))}
  | float {FLOAT (float_of_string (L.lexeme lexbuf))}
  | '"' {read_string (Buffer.create 16) lexbuf}
  | '\\' {LAMBDA}
  | "->" {ARROW}
  | "let" {LET}
  | "=" {EQUALS}
  | "in" {IN}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | "|>" {PIPE}
  | '(' {LEFT_BRACKET}
  | ')' {RIGHT_BRACKET}
  | name {NAME (L.lexeme lexbuf)}
  | type_name {TYPE_NAME (L.lexeme lexbuf)}
  | eof {EOF}
  | _ {raise (Error ("Unexpected char: " ^ Lexing.lexeme lexbuf))}

and read_string buffer =
  parse
  | '\\' '\\' { Buffer.add_char buffer '\\';
                read_string buffer lexbuf }
  | '\\' '"' { Buffer.add_char buffer '"';
               read_string buffer lexbuf }
  | '\\' 'n' { Buffer.add_char buffer '\n';
               read_string buffer lexbuf }
  | '\\' 'r' { Buffer.add_char buffer '\r';
               read_string buffer lexbuf }
  | '\\' 't' { Buffer.add_char buffer '\t';
               read_string buffer lexbuf }
  | [^ '"' '\\']+ { Buffer.add_string buffer (Lexing.lexeme lexbuf);
                    read_string buffer lexbuf}
  | '"' {STRING (Buffer.contents buffer)}
  | eof {raise (Error "String is not terminated")}
  | _ {raise (Error ("Illegal string character: " ^ Lexing.lexeme lexbuf))}

