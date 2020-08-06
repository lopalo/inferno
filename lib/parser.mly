%{
open Core
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
%token LET EQUALS IN
%token DEFINE OF UNPACK
%token IF THEN ELSE
%token PIPE
%token LEFT_BRACKET RIGHT_BRACKET
%token EOF

%nonassoc LET IN UNPACK
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
            wrap (Lambda ({name=param}, lambda params)) $startpos
        | [] -> result
      in
      Lambda ({name=parameter}, lambda parameters) }
  | func = expression; arg = expression %prec APPLICATION
      {Application (func, arg)}
  | arg = expression; PIPE; func = expression
      {Application (func, arg)}
  | LET; name = NAME; EQUALS; rhs = expression; IN; body = expression
      {Let {name={name}; rhs; body}}
  | DEFINE; type_name = TYPE_NAME; parameters = type_parameter*;
    OF; content = type_spec
      {TypeDefinition {name={type_name}; parameters; content}}
  | type_name = TYPE_NAME; content = expression
      {Packing ({type_name}, content)}
  | UNPACK; type_name = TYPE_NAME; name = NAME;
    EQUALS; rhs = expression; IN; body = expression
      {Unpacking {type_name={type_name}; name={name}; rhs; body}}
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
    {TypeTag.Type ({type_name}, [])}
  | parameter = type_spec; ARROW; result = type_spec
    {TypeTag.(Type (arrow, [parameter; result]))}
  | LEFT_BRACKET; type_name = TYPE_NAME; types = type_spec+; RIGHT_BRACKET
    {TypeTag.Type ({type_name}, types)}
  | name = type_parameter
    {TypeTag.Generic name}
  | LEFT_BRACKET; ty = type_spec; RIGHT_BRACKET {ty}

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

