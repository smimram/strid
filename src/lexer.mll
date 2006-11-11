{
(*
 * Copyright (C) 2006 Samuel Mimram
 *
 * This file is part of strid.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

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
