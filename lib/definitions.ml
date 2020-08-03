open Core

let c type_name = TypeTag.Type ({type_name}, [])

let ( @> ) parameter result = TypeTag.(Type (arrow, [parameter; result]))

let u = c "Unit"

let i = c "Int"

let f = c "Float"

let b = c "Bool"

let s = c "String"

let g var_id = TypeTag.Generic var_id

let match_bool =
  let result = g "a" in
  let branch = u @> result in
  b @> branch @> branch @> result

let items =
  [ ("true", b);
    ("false", b);
    ("matchBool", match_bool);
    ("plus", i @> i @> i);
    ("minus", i @> i @> i);
    ("plusFloat", f @> f @> f);
    ("minusFloat", f @> f @> f);
    ("concat", s @> s @> s) ]
