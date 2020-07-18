%{
open Expression

let annotate expression source_position =
  {expr=expression; source_pos=source_position}
%}


%token UNIT
%token <string> NAME
%token <string> TYPE_NAME
%token <int> INTEGER
%token <float> FLOAT
%token <bool> BOOLEAN
%token LAMBDA ARROW
%token PIPE
%token LEFT_BRACKET RIGHT_BRACKET
%token EOF

%right LAMBDA ARROW
%left PIPE
%nonassoc UNIT NAME TYPE_NAME INTEGER FLOAT BOOLEAN
%nonassoc LEFT_BRACKET
%left APPLICATION

%start <untyped option> main

%%

main:
  | EOF {None}
  | e = expression EOF {Some e}

expression:
  | LEFT_BRACKET; e = expression; RIGHT_BRACKET {e}
  | UNIT {annotate Unit $startpos}
  | n = NAME {annotate (Name {name=n}) $startpos}
  | tn = TYPE_NAME {annotate (TypeName {name=tn}) $startpos}
  | i = INTEGER {annotate (Int i) $startpos}
  | f = FLOAT {annotate (Float f) $startpos}
  | b = BOOLEAN {annotate (Bool b) $startpos}
  | LAMBDA; parameter = NAME; ARROW; result = expression
      {annotate (Lambda ({name=parameter}, result)) $startpos}
  | func = expression; arg = expression %prec APPLICATION
      {annotate (Application (func, arg)) $startpos}
  | arg = expression; PIPE; func = expression
      {annotate (Application (func, arg)) $startpos}





