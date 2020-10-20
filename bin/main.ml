open Inferno

let fmt = Printf.sprintf

let string_of_position (pos : Lexing.position) =
  fmt "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let position (lexbuf : Lexing.lexbuf) = string_of_position lexbuf.lex_curr_p

let parse_and_print file_name channel ~margin ~show_types ~execute =
  let lexbuf = Lexing.from_channel channel in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file_name};
  let formatter = Format.std_formatter in
  Format.pp_set_margin formatter margin;
  try
    match Parser.main_expression Lexer.read lexbuf with
    | Some expression ->
        let definitions = Definitions.items in
        let expression, _ =
          Inference.infer_expression definitions expression
          |> Expression.collect_free_names
        in
        if execute then Interpreter.execute_expression definitions expression
        else expression |> Expression.pp ~with_type:show_types formatter;
        Format.pp_print_newline formatter ()
    | None -> ()
  with
  | Lexer.Error msg -> fmt "%s: %s" (position lexbuf) msg |> print_endline
  | Parser.Error -> fmt "%s: Syntax error" (position lexbuf) |> print_endline
  | Inference.Error (pos, msg) ->
      fmt "%s:\n%s" (string_of_position pos) msg |> print_endline

let () =
  let file_name = ref None in
  let margin = ref 80 in
  let show_types = ref false in
  let execute = ref false in
  let handler fname =
    match !file_name with
    | None -> file_name := Some fname
    | Some _ -> raise (Arg.Bad "accepts exactly one SOURCE_FILE")
  in
  let specs =
    [ ("-margin", Arg.Set_int margin, "Right margin for pretty-printing");
      ("-types", Arg.Set show_types, "Annotate expressions with infered types");
      ("-exec", Arg.Set execute, "Execute code") ]
  in
  let usage = "inferno SOURCE_FILE" in
  Arg.parse specs handler usage;
  match !file_name with
  | None -> Arg.usage specs usage
  | Some fname ->
      let ch = open_in fname in
      parse_and_print fname ch ~margin:!margin ~show_types:!show_types
        ~execute:!execute;
      close_in ch
