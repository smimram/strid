%{
    open Lang

    let parse_error s =
        Lang.error ("Parse error: " ^ s)
%}

%token NEWCOL NEWLINE LBRACK RBRACK LPAR RPAR EQ COMMA EOF LACC RACC
%token LET MATRIX INCLUDE
%token <string> STRING

%start defs
%type <((string * string) list) * Lang.ir_matrix> defs
%%

defs:
    | LET STRING EQ STRING defs { let env, mat = $5 in ($2,$4)::env, mat }
    | matrix { [], $1 }

matrix:
    | MATRIX LACC matrix_lines RACC EOF { $3 }
;

matrix_lines:
    | line matrix_lines { $1::$2 }
    | { [] }
;

line:
    | box NEWCOL line { (Some $1)::$3 }
    | NEWCOL line { None::$2 }
    | box NEWLINE { [Some $1] }
    | NEWLINE { [None] }
;

box:
    | STRING LPAR directions RPAR options { new box $1 $3 $5 }
;

directions:
    | dir COMMA directions { $1::$3 }
    | dir { [$1] }
;

dir:
    | STRING { reldir_of_string $1 }
    | { reldir_of_string "" }
;

options:
    | LBRACK STRING option_val RBRACK options { ($2, $3)::$5 }
    | { [] }
;

option_val:
    | COMMA STRING EQ STRING option_val { ($2, $4)::$5 }
    | { [] }
;
