let ty type_string =
  Parser.main_type_spec Lexer.read (Lexing.from_string type_string)

let items =
  [ ("true", ty "Bool");
    ("false", ty "Bool");
    ("matchBool", ty "Bool -> (Unit -> a) -> (Unit -> a) -> a");
    ("plus", ty "Int -> Int -> Int");
    ("minus", ty "Int -> Int -> Int");
    ("plusFloat", ty "Float -> Float -> Float");
    ("minusFloat", ty "Float -> Float -> Float");
    ("concat", ty "String -> String -> String") ]
