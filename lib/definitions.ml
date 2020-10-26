type t = (string * Core.TypeTag.t) list

let ty type_string =
  Parser.main_type_spec Lexer.read (Lexing.from_string type_string)

let items =
  [ ("fix", ty "((a -> b) -> a -> b) -> a -> b");
    ("loop", ty "(a -> (Or a b)) -> a -> b");
    ("true", ty "Bool");
    ("false", ty "Bool");
    ("@bool", ty "Bool -> (Unit -> a) -> (Unit -> a) -> a");
    ("&", ty "a -> b -> (And a b)");
    ("$1", ty "(And a b) -> a");
    ("$2", ty "(And a b) -> b");
    ("left", ty "a -> (Or a b)");
    ("right", ty "b -> (Or a b)");
    ("@or", ty "(Or a b) -> (a -> c) -> (b -> c) -> c");
    ("null", ty "(List a)");
    (":", ty "a -> (List a) -> (List a)");
    ("@list", ty "(List a) -> (Unit -> b) -> (a -> (List a) -> b) -> b");
    (* Int *)
    ("i.=", ty "Int -> Int -> Bool");
    ("i.>", ty "Int -> Int -> Bool");
    ("i.+", ty "Int -> Int -> Int");
    ("i.-", ty "Int -> Int -> Int");
    ("i.*", ty "Int -> Int -> Int");
    ("i./", ty "Int -> Int -> Int");
    ("i.toString", ty "Int -> String");
    ("i.ofString", ty "String -> (Or Unit Int)");
    ("i.toFloat", ty "Int -> Float");
    (* Float *)
    ("f.=", ty "Float -> Float -> Bool");
    ("f.>", ty "Float -> Float -> Bool");
    ("f.+", ty "Float -> Float -> Float");
    ("f.-", ty "Float -> Float -> Float");
    ("f.*", ty "Float -> Float -> Float");
    ("f./", ty "Float -> Float -> Float");
    ("f.toString", ty "Float -> String");
    ("f.ofString", ty "String -> (Or Unit Float)");
    ("f.toInt", ty "Float -> Int");
    (* String *)
    ("string.=", ty "String -> String -> Bool");
    ("string.>", ty "String -> String -> Bool");
    ("length", ty "String -> Int");
    ("concat", ty "String -> String -> String");
    ("split", ty "String -> String -> (List String)");
    ("substr", ty "String -> Int -> Int -> String");
    (* I/O *)
    ("readLine", ty "Unit -> String");
    ("write", ty "String -> Unit");
    (* Type conversion *)
    ("@<", ty "(a b c) -> ((a b) c)");
    ("@>", ty "((a b) c) -> (a b c)");
    ("@@<", ty "(a b c d) -> ((a b) c d)");
    ("@@>", ty "((a b) c d) -> (a b c d)");
    ("@@@<", ty "(a b c d e) -> ((a b) c d e)");
    ("@@@>", ty "((a b) c d e) -> (a b c d e)") ]
