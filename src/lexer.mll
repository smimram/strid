{
    open Lexing
    open Parser
}

let space = ' ' | '\t' | '\r'

rule token = parse
  | "&" { NEWCOL}
  | "\\\\" { NEWLINE }
  | '\\'(['a'-'z']+ as name) { BOX name }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | ',' { COMMA }
  | (['a'-'z']+ as str) { STRING str }
  | (space|'\n')+ { token lexbuf }
  | eof { EOF }
