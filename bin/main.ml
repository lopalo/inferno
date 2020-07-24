open Inferno

let fmt = Printf.sprintf

let string_of_position (pos : Lexing.position) =
  fmt "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let position (lexbuf : Lexing.lexbuf) = string_of_position lexbuf.lex_curr_p

let parse_and_print file_name channel =
  let lexbuf = Lexing.from_channel channel in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file_name};
  try
    match Parser.main Lexer.read lexbuf with
    | Some expression -> Expression.to_string expression |> print_endline
    | None -> ()
  with
  | Lexer.Error msg -> fmt "%s: %s" (position lexbuf) msg |> print_endline
  | Parser.Error -> fmt "%s: syntax error" (position lexbuf) |> print_endline

let () =
  let file_name = ref None in
  (* TODO: use these flags *)
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
      parse_and_print fname ch; close_in ch

let type_escape_test () =
  (* TODO: delete  *)
  let open Inferno.TypeEnvironment in
  let u {var_id} = var_id in
  let foo e = e |> next_var_id |> u in
  run {computation = (fun e -> e |> foo |> succ)} |> print_int;
  print_newline ();
  (* Harmful functions *)
  (* run {computation = (fun e -> e)} |> ignore; *)
  (* run {computation = (fun e -> e |> next_var_id)} |> ignore; *)
  (* run *)
  (*   { computation = *)
  (*       (fun e -> *)
  (*         let vid = next_var_id e in *)
  (*         run {computation = (fun e' -> e' |> next_var_id |> ( = ) vid)}) } *)
  (* |> ignore; *)
  (* let j = run {computation = (fun _e _e' -> ())} in *)
  (* run {computation = (fun e -> j e)}; *)
  (* let box = ref [] in *)
  (* run *)
  (*   { computation = *)
  (*       (fun e -> e |> next_var_id |> (fun x -> [x]) |> ( := ) box) }; *)
  (* ^^^ Harmful functions ^^^ *)
  let k = run {computation = (fun _e _e' -> ())} in
  run {computation = (fun _e -> k ())};
  run {computation = (fun e -> e |> foo |> ( + ) 100 |> string_of_int)}
  |> print_string;
  print_newline ();
  print_endline "ZZZZ!!!"
