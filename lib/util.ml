let name_of_id id =
  let open Char in
  let letter = 97 + (id mod 26) |> chr |> escaped in
  let digit = if id >= 26 then string_of_int (id / 26) else "" in
  letter ^ digit

let surround_pp s s' pp_v ppf v = Fmt.(string ppf s; pp_v ppf v; string ppf s')
