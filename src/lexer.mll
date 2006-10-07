{
    open Lexing
    open Parser
}

let space = ' ' | '\t' | '\r'

rule token = parse
  | "&" { NEWCOL}
  | "\\\\" { NEWLINE }
  | '#'([^'#']* as tex)'#' { STRING tex }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | '(' { LPAR }
  | ')' { RPAR }
  | ',' { COMMA }
  | '=' { EQ }
  | (['a'-'z']+ as str) { STRING str }
  | (space|'\n')+ { token lexbuf }
  | eof { EOF }
