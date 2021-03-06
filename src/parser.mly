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
    | MATRIX matrix_options LACC matrix_lines RACC EOF { { ir_options = $2; ir_lines = $4 } }
;

matrix_options:
    | LBRACK matrix_opts RBRACK { $2 }
    | LBRACK RBRACK { [] }
    | { [] }

matrix_opts:
    | matrix_opt { [$1] }
    | matrix_opt COMMA matrix_opts { $1::$3 }

matrix_opt:
    | STRING { $1 }
    | STRING EQ STRING { $1 ^ "=" ^ $3 }

matrix_lines:
    | line matrix_lines { $1::$2 }
    | { [] }
;

line:
    | boxes NEWCOL line { $1::$3 }
    | boxes NEWLINE { [$1] }
;

boxes:
    | box boxes { $1::$2 }
    | { [] }
;

box:
    | STRING LPAR directions RPAR options { make_box $1 $3 $5 }
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
    | LBRACK RBRACK { [] }
    | { [] }
;

option_val:
    | COMMA STRING EQ STRING option_val { ($2, $4)::$5 }
    | { [] }
;
