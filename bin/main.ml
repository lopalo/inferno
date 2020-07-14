module TEnv = Inferno.TypeEnv

let main () =
  let open TEnv in
  let foo e = e |> new_var |> fun v -> v.id in
  run {computation = (fun e -> e |> foo |> succ)} |> print_int;
  print_newline ();
  (* Harmful functions *)
  (* let box = ref {id = 0} in *)
  (* run {computation = (fun e -> e)} |> ignore; *)
  (* run {computation = (fun e -> e |> new_var)} |> ignore; *)
  (* run {computation = (fun e -> e |> new_var |> ( := ) box)} |> ignore; *)
  (* ^^^ Harmful functions ^^^ *)
  run {computation = (fun e -> e |> foo |> ( + ) 100 |> string_of_int)}
  |> print_string;
  print_newline ();
  print_endline "ZZZZ!!!"

let () = main ()
