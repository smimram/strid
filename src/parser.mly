%{
    open Lang
%}

%token NEWCOL NEWLINE LBRACK RBRACK COMMA EOF
%token <string> BOX STRING

%start matrix
%type <Lang.ir_matrix> matrix
%%

matrix:
    | line matrix { $1::$2 }
    | EOF | { [] }
;

line:
    | box NEWCOL line { (Some $1)::$3 }
    | NEWCOL line { None::$2 }
    | box NEWLINE { [Some $1] }
    | NEWLINE { [None] }
;

box:
    | BOX LBRACK directions RBRACK { new box $1 (List.hd $3) (List.tl $3) }
;

directions:
    | STRING COMMA directions { (reldir_of_string $1)::$3 }
    | STRING { [reldir_of_string $1] }
;
