open Inferno

type action =
  | Show
  | Execute

let fmt = Printf.sprintf

let string_of_position (pos : Lexing.position) =
  fmt "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let position (lexbuf : Lexing.lexbuf) = string_of_position lexbuf.lex_curr_p

let parse_and_check_types lexbuf definitions =
  try
    match Parser.main_expression Lexer.read lexbuf with
    | Some expression ->
        let expression, _ =
          Inference.infer_expression definitions expression
          |> Expression.collect_free_names
        in
        Some expression
    | None -> None
  with
  | Lexer.Error msg ->
      fmt "%s: %s" (position lexbuf) msg |> print_endline;
      None
  | Parser.Error ->
      fmt "%s: Syntax error" (position lexbuf) |> print_endline;
      None
  | Inference.Error (pos, msg) ->
      fmt "%s:\n%s" (string_of_position pos) msg |> print_endline;
      None

let () =
  let action = ref None in
  let file_name = ref None in
  let margin = ref 80 in
  let show_types = ref false in
  let show_spec =
    [ ("-margin", Arg.Set_int margin, "Right margin for pretty-printing");
      ("-types", Arg.Set show_types, "Annotate expressions with infered types")
    ]
  in
  let exec_spec = [] in
  let specs =
    ref
      [ ("show", Arg.Unit Fun.id, "Display parsed expression");
        ("exec", Arg.Unit Fun.id, "Execute expression") ]
  in
  let handler = function
    | "show" ->
        action := Some Show;
        specs := show_spec
    | "exec" ->
        action := Some Execute;
        specs := exec_spec
    | fname -> (
      match !file_name with
      | None -> file_name := Some fname
      | Some _ -> raise (Arg.Bad "accepts exactly one <path>"))
  in
  let usage = "inferno <action> [<options>] <path>" in
  Arg.parse_dynamic specs handler usage;
  match (!action, !file_name) with
  | None, _
  | _, None ->
      Arg.usage !specs usage
  | Some act, Some fname -> (
      let channel = open_in fname in
      let lexbuf = Lexing.from_channel channel in
      lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = fname};
      let definitions = Definitions.items in
      let res = parse_and_check_types lexbuf definitions in
      close_in channel;
      match res with
      | Some expression -> (
        match act with
        | Show ->
            let formatter = Format.std_formatter in
            Format.pp_set_margin formatter !margin;
            expression |> Expression.pp ~with_type:!show_types formatter;
            Format.pp_print_newline formatter ()
        | Execute -> Interpreter.execute_expression definitions expression)
      | None -> ())
