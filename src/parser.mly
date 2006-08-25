%{
    open Lang
%}

%token NEWCOL NEWLINE LBRACK RBRACK COMMA
%token <string> BOX STRING

%start matrix
%type <Lang.matrix> matrix
%%

matrix:
    | line matrix { $1::$2 }
    | { [] }
;

line:
    | box NEWCOL line { (Some $1)::$3 }
    | NEWCOL line { None::$2 }
    | box NEWLINE { [Some $1] }
    | NEWLINE { [None] }
;

box:
    | BOX LBRACK directions RBRACK {
        {
            box_name = $1;
            box_dir = List.hd $3;
            box_connexions = List.tl $3;
        }
    }
;

directions:
    | STRING COMMA directions { (reldir_of_string $1)::$3 }
    | STRING { [reldir_of_string $1] }
;
