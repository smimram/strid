{
    open Lexing
    open Parser
}

let space = ' ' | '\t' | '\r'

rule token = parse
  | "let" { LET }
  | "include" { INCLUDE }
  | "matrix" { MATRIX }
  | "%"_* { token lexbuf }
  | "&" { NEWCOL}
  | "\\\\" { NEWLINE }
  | '#'([^'#']* as tex)'#' { STRING tex }
  | '{' { LACC }
  | '}' { RACC }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | '(' { LPAR }
  | ')' { RPAR }
  | ',' { COMMA }
  | '=' { EQ }
  | (['a'-'z''0'-'9''.']+ as str) { STRING str }
  | (space|'\n')+ { token lexbuf }
  | eof { EOF }
