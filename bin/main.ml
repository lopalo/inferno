open Inferno

let fmt = Printf.sprintf

let string_of_position (pos : Lexing.position) =
  fmt "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let position (lexbuf : Lexing.lexbuf) = string_of_position lexbuf.lex_curr_p

let parse_and_print file_name channel ~show_types =
  let lexbuf = Lexing.from_channel channel in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file_name};
  try
    match Parser.main Lexer.read lexbuf with
    | Some expression ->
        Inference.infer_expression expression
        |> Expression.to_string ~with_type:show_types
        |> print_endline
    | None -> ()
  with
  | Lexer.Error msg -> fmt "%s: %s" (position lexbuf) msg |> print_endline
  | Parser.Error -> fmt "%s: syntax error" (position lexbuf) |> print_endline
  | Inference.Error (pos, msg) ->
      fmt "%s: %s" (string_of_position pos) msg |> print_endline

let () =
  let file_name = ref None in
  (* TODO: "execute" mode *)
  let show_types = ref false in
  let execute = ref false in
  let handler fname =
    match !file_name with
    | None -> file_name := Some fname
    | Some _ -> raise (Arg.Bad "accepts exactly one SOURCE_FILE")
  in
  let specs =
    [ ("-types", Arg.Set show_types, "Annotate expressions with infered types");
      ("-exec", Arg.Set execute, "Execute code") ]
  in
  let usage = "inferno SOURCE_FILE" in
  Arg.parse specs handler usage;
  match !file_name with
  | None -> Arg.usage specs usage
  | Some fname ->
      let ch = open_in fname in
      parse_and_print fname ch ~show_types:!show_types;
      close_in ch
