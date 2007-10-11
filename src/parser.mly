%{
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

    open Lang
%}

%token NEWCOL NEWLINE LBRACK RBRACK LPAR RPAR EQ COMMA EOF LACC RACC
%token LET MATRIX INCLUDE
%token <string> STRING

%start defs
%type <Lang.ir_matrix> defs
%%

defs:
    | matrix { $1 }

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
