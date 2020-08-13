type t = (string * Core.TypeTag.t) list

let ty type_string =
  Parser.main_type_spec Lexer.read (Lexing.from_string type_string)

let items =
  [ ("fix", ty "((a -> b) -> a -> b) -> a -> b");
    ("loop", ty "(a -> (Either a b)) -> a -> b");
    ("true", ty "Bool");
    ("false", ty "Bool");
    ("@bool", ty "Bool -> (Unit -> a) -> (Unit -> a) -> a");
    ("none", ty "(Option a)");
    ("some", ty "a -> (Option a)");
    ("@option", ty "(Option a) -> (Unit -> b) -> (a -> b) -> b");
    ("&", ty "a -> b -> (Both a b)");
    ("$1", ty "(Both a b) -> a");
    ("$2", ty "(Both a b) -> b");
    ("left", ty "a -> (Either a b)");
    ("right", ty "b -> (Either a b)");
    ("@either", ty "(Either a b) -> (a -> c) -> (b -> c) -> c");
    ("null", ty "(List a)");
    ("cons", ty "a -> (List a) -> (List a)");
    ("@list", ty "(List a) -> (Unit -> b) -> (a -> (List a) -> b) -> b");
    ("emptyNode", ty "(Tree a)");
    ("node", ty "(Tree a) -> a -> (Tree a) -> (Tree a)");
    ( "@tree",
      ty "(Tree a) -> (Unit -> b) -> ((Tree a) -> a -> (Tree a) -> b) -> b" );
    (* Int *)
    ("i.=", ty "Int -> Int -> Bool");
    ("i.>", ty "Int -> Int -> Bool");
    ("i.+", ty "Int -> Int -> Int");
    ("i.-", ty "Int -> Int -> Int");
    ("i.*", ty "Int -> Int -> Int");
    ("i./", ty "Int -> Int -> Int");
    ("i.toString", ty "Int -> String");
    ("i.ofString", ty "String -> (Option Int)");
    ("i.toFloat", ty "Int -> Float");
    (* Float *)
    ("f.=", ty "Float -> Float -> Bool");
    ("f.>", ty "Float -> Float -> Bool");
    ("f.+", ty "Float -> Float -> Float");
    ("f.-", ty "Float -> Float -> Float");
    ("f.*", ty "Float -> Float -> Float");
    ("f./", ty "Float -> Float -> Float");
    ("f.toString", ty "Float -> String");
    ("f.ofString", ty "String -> (Option Float)");
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
    ("writeNewline", ty "Unit -> Unit") ]
