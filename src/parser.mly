%{
    open Lang
%}

%token NEWCOL NEWLINE LBRACK RBRACK LPAR RPAR EQ COMMA EOF
%token <string> STRING COMMENT

%start matrix
%type <Lang.ir_matrix> matrix
%%

matrix:
    | line matrix { $1::$2 }
    | EOF { [] }
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
