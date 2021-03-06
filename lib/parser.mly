%{
open Core
open Expression

let wrap expression ({Lexing.pos_cnum; _} as position) =
  {expr = expression; source_pos = {position with pos_cnum = succ pos_cnum}}

let make_lambda parameter result =
  Lambda {parameter = {name = parameter}; result; free_names = Names.empty}
%}

%token UNIT
%token <string> NAME
%token <string> TYPE_NAME
%token <int> INTEGER
%token <float> FLOAT
%token <string> STRING
%token LAMBDA ARROW
%token LET EQUALS IN
%token DEFINE OF
%token IF THEN ELSE
%token PIPE
%token LEFT_BRACKET RIGHT_BRACKET
%token EOF

%nonassoc LET IN DEFINE
%right LAMBDA ARROW
%nonassoc IF ELSE
%left PIPE
%nonassoc UNIT NAME TYPE_NAME INTEGER FLOAT STRING
%nonassoc LEFT_BRACKET
%left APPLICATION

%start <parsed option> main_expression

%start <TypeTag.t> main_type_spec

%%

main_expression:
  | EOF {None}
  | e = expression EOF {Some e}

main_type_spec:
  | ty = type_spec EOF {ty}

expression:
  e = expr {wrap e $startpos}

expr:
  | v = value {Value v}
  | LEFT_BRACKET; e = expr; RIGHT_BRACKET {e}
  | name = NAME {Name {name}}
  | LAMBDA; parameter = NAME; parameters = NAME*; ARROW; result = expression
    { let rec lambda = function
        | param :: params ->
            wrap (make_lambda param (lambda params)) $startpos
        | [] -> result
      in
      make_lambda parameter (lambda parameters) }
  | func = expression; arg = expression %prec APPLICATION
      {Application (func, arg)}
  | arg = expression; PIPE; func = expression
      {Application (func, arg)}
  | LET; name = NAME; EQUALS; rhs = expression; IN; body = expression
      {Let {name = {name}; rhs; body}}
  | DEFINE; type_name = TYPE_NAME; parameters = type_parameter*;
    OF; content = type_spec
      {TypeDefinition {name = {type_name}; parameters; content}}
  | type_name = TYPE_NAME; content = expression
      {Packing ({type_name}, content)}
  | LET; type_name = TYPE_NAME; name = NAME;
    EQUALS; rhs = expression; IN; body = expression
      {Unpacking {type_name = {type_name}; name = {name}; rhs; body}}
  | e = if_expr {e}

value:
  | UNIT {Value.Unit}
  | i = INTEGER {Value.Int i}
  | f = FLOAT {Value.Float f}
  | s = STRING {Value.Str s}

type_parameter:
  type_parameter = NAME
  {{type_parameter}}

type_spec:
  | type_name = TYPE_NAME
    {TypeTag.Constant {type_name}}
  | parameter = type_spec; ARROW; result = type_spec
    {TypeTag.(Compound [Constant arrow; parameter; result])}
  | LEFT_BRACKET; type_header = type_spec; types = type_spec+; RIGHT_BRACKET
    {TypeTag.Compound (type_header :: types)}
  | name = type_parameter
    {TypeTag.Generic name}
  | LEFT_BRACKET; ty = type_spec; RIGHT_BRACKET {ty}

%inline if_expr:
  IF; cond = expression;
  THEN; true_branch = expression;
  ELSE; false_branch = expression
  { let pos = $startpos in
    let func = wrap (Name {name = "@bool"}) pos in
    let branch_fun branch = wrap (make_lambda "_" branch) in
    let true_fun = branch_fun true_branch pos in
    let false_fun = branch_fun false_branch pos in
    let app func arg = wrap (Application (func, arg)) pos in
    Application (app (app func cond) true_fun, false_fun) }

