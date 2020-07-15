let main () =
  (* TODO: delete  *)
  let open Inferno.TypeEnvironment in
  let u = function
    | Unbound (vid, _) -> vid.var_id
    | _ -> 100000
  in
  let foo e = e |> new_unbound_var |> deref |> u in
  run {computation = (fun e -> e |> foo |> succ)} |> print_int;
  print_newline ();
  (* Harmful functions *)
  (* run {computation = (fun e -> e)} |> ignore; *)
  (* run {computation = (fun e -> e |> new_unbound_var)} |> ignore; *)
  (* run *)
  (*   { computation = *)
  (*       (fun e -> *)
  (*         let v = new_unbound_var e in *)
  (*         run *)
  (*           { computation = *)
  (*               (fun e' -> e' |> new_unbound_var |> deref |> assign v) }) }; *)
  (* let j = run {computation = (fun _e _e' -> ())} in *)
  (* run {computation = (fun e -> j e)}; *)
  (* let box = ref [] in *)
  (* run *)
  (*   { computation = *)
  (*       (fun e -> e |> new_unbound_var |> (fun x -> [x]) |> ( := ) box) }; *)
  (* ^^^ Harmful functions ^^^ *)
  let k = run {computation = (fun _e _e' -> ())} in
  run {computation = (fun _e -> k ())};
  run {computation = (fun e -> e |> foo |> ( + ) 100 |> string_of_int)}
  |> print_string;
  print_newline ();
  print_endline "ZZZZ!!!"

let () = main ()
