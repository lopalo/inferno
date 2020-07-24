%{
open Core.Value
open Expression

let wrap expression position =
  {expr=expression; source_pos=position}
%}

%token UNIT
%token <string> NAME
%token <string> TYPE_NAME
%token <int> INTEGER
%token <float> FLOAT
%token <string> STRING
%token LAMBDA ARROW
%token IF THEN ELSE
%token PIPE
%token LEFT_BRACKET RIGHT_BRACKET
%token EOF

%right LAMBDA ARROW
%nonassoc IF
%nonassoc ELSE
%left PIPE
%nonassoc UNIT NAME TYPE_NAME INTEGER FLOAT STRING
%nonassoc LEFT_BRACKET
%left APPLICATION

%start <parsed option> main

%%

main:
  | EOF {None}
  | e = expression EOF {Some e}

expression:
  e = expr {wrap e $startpos}

expr:
  | v = value {Value v}
  | LEFT_BRACKET; e = expr; RIGHT_BRACKET {e}
  | n = NAME {Name {name=n}}
  | tn = TYPE_NAME {Name {name=tn}}
  | LAMBDA; parameter = NAME; parameters = NAME*; ARROW; result = expression
    { let rec lambda = function
        | param :: params ->
            wrap (Lambda ({name=param}, lambda params)) $startpos
        | [] -> result
      in
      Lambda ({name=parameter}, lambda parameters) }
  | func = expression; arg = expression %prec APPLICATION
      {Application (func, arg)}
  | arg = expression; PIPE; func = expression
      {Application (func, arg)}
  | e = if_expr {e}

value:
  | UNIT {Unit}
  | i = INTEGER {Int i}
  | f = FLOAT {Float f}
  | s = STRING {Str s}

%inline if_expr:
  IF; cond = expression;
  THEN; true_branch = expression;
  ELSE; false_branch = expression
  { let pos = $startpos in
    let func = wrap (Name {name="matchBool"}) $startpos in
    let true_fun = wrap (Lambda ({name="_"}, true_branch)) pos in
    let false_fun = wrap (Lambda ({name="_"}, false_branch)) pos in
    let app func arg = wrap (Application (func, arg)) pos in
    Application (app (app func cond) true_fun, false_fun) }
